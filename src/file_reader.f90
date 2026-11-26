module file_reader_m
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    use strings_m, only: string_t
    use fparse_tools_m, only: fparse_tools
    use error_handling_m, only: error_handling
    implicit none

    private

    public :: file_reader_t

    ! nick: i think have 5 deep nested set of include files is more than enough
    integer, parameter :: max_include_depth = 5

    type :: source_line_t
        character(len=:), allocatable :: content, source
        integer :: number = 0
    end type

    type :: file_reader_t
        private
        type(source_line_t), allocatable :: lines(:)
        type(string_t) :: stack(max_include_depth) ! this is a stack of include files specified using the @ directive (see example)
    contains
        procedure :: load
        procedure :: count
        procedure :: get_line
        procedure, private :: read_file
        procedure, nopass, private :: read_line
        procedure, private :: join_continuations
    end type

contains

    subroutine load(self, filename)
        class(file_reader_t), intent(out) :: self
        character(len=*), intent(in) :: filename

        allocate(self%lines(0))
        call self%read_file(fparse_tools%resolve_path(filename), 1)
        call self%join_continuations()

    end subroutine load


    recursive subroutine read_file(self, filename, depth)
        class(file_reader_t), intent(inout) :: self
        character(len=*), intent(in) :: filename
        integer, intent(in) :: depth
        !
        character(len=:), allocatable :: content, token, rest, include_file
        integer :: unit, status, number, comment, i

        if (depth > max_include_depth) then
            call error_handling%abort_routine(0, filename, 'include depth exceeded')
        end if
        do i = 1, depth-1
            if (self%stack(i)%s == filename) then
                call error_handling%abort_routine(0, filename, 'circular include statements detected')
            end if
        end do
        self%stack(depth) = string_t(filename)
        open(newunit=unit, file=filename, status='old', action='read', iostat=status)
        if (status /= 0) call error_handling%abort_routine(0, filename, 'cannot open file')

        number = 0
        do
            call self%read_line(unit, content, status)
            if (status == iostat_end) exit
            if (status /= 0) call error_handling%abort_routine(number+1, filename, 'cannot read file')
            number = number + 1
            comment = index(content, '#')
            if (comment > 0) content = content(:comment-1)
            call fparse_tools%split_first_token(content, token, rest)
            if (token == '') cycle
            select case (fparse_tools%normalise_name(token))
            case ('@INCLUDE', 'INCLUDE')
                if (rest == '') call error_handling%abort_routine(number, filename, 'missing include filename')
                include_file = fparse_tools%resolve_path(rest, filename)
                call self%read_file(include_file, depth+1)
            case default
                self%lines = [self%lines, source_line_t(trim(adjustl(content)), filename, number)]
            end select
        end do
        close(unit)

    end subroutine read_file


    subroutine read_line(unit, content, status)
        integer, intent(in) :: unit
        character(len=:), allocatable, intent(out) :: content
        integer, intent(out) :: status
        !
        character(len=256) :: chunk
        integer :: n

        content = ''
        do
            read(unit, '(a)', advance='no', size=n, iostat=status) chunk
            content = content // chunk(:n)
            if (status == iostat_eor) then
                status = 0
                return
            end if
            if (status /= 0) return
        end do

    end subroutine read_line


    subroutine join_continuations(self)
        class(file_reader_t), intent(inout) :: self
        !
        type(source_line_t), allocatable :: joined(:)
        type(source_line_t) :: line
        integer :: i, n

        allocate(joined(0))
        i = 1
        do while (i <= size(self%lines))
            line = self%lines(i)
            do
                n = verify(line%content, ' ' // achar(9), back=.true.)
                if (n == 0) exit
                if (line%content(n:n) /= achar(92)) exit ! achar(92) is backslash (what we deem a continuation character)
                line%content = trim(line%content(:n-1))
                i = i + 1
                if (i > size(self%lines)) then
                    call error_handling%abort_routine(line%number, line%source, 'line continuation missing')
                end if
                line%content = line%content // ' ' // self%lines(i)%content
            end do
            joined = [joined, line]
            i = i + 1
        end do
        call move_alloc(joined, self%lines)

    end subroutine join_continuations


    integer function count(self)
        class(file_reader_t), intent(in) :: self

        count = 0
        if (allocated(self%lines)) count = size(self%lines)

    end function count


    subroutine get_line(self, i, content, source, number)
        class(file_reader_t), intent(in) :: self
        integer, intent(in) :: i
        character(len=:), allocatable, intent(out) :: content, source
        integer, intent(out) :: number

        content = self%lines(i)%content
        source = self%lines(i)%source
        number = self%lines(i)%number

    end subroutine get_line

end module file_reader_m
