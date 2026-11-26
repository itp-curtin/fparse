module fparse_io
    use fparse_util, only: strip_comment, &
                           is_blank, &
                           split_first_token, &
                           normalise_name, &
                           is_absolute_path, &
                           get_dirname, &
                           join_path
    implicit none

    ! maximum depth of "include" files. don't think this will be used but could be handy maybe
    integer, parameter :: max_include_depth = 5

contains

    !--------------------------------------------------------------------------
    ! load_file_lines
    !
    ! purpose: load lines from a file and expand @include directives.
    !
    ! inputs:
    !          1. filename
    !          2. ounit
    !
    ! outputs:
    !          1. lines
    !          2. file_sources
    !          3. line_nums
    !--------------------------------------------------------------------------
    subroutine load_file_lines(filename, lines, file_sources, line_nums, ounit)
        implicit none
        character(len=*), intent(in) :: filename
        character(len=:), allocatable, intent(out) :: lines(:)
        character(len=:), allocatable, intent(out) :: file_sources(:)
        integer, allocatable, intent(out) :: line_nums(:)
        integer, intent(in) :: ounit
        !
        character(len=:), allocatable :: file_stack(:)
        integer :: depth

        allocate(character(len=1) :: lines(0))
        allocate(character(len=1) :: file_sources(0))
        allocate(line_nums(0))
        allocate(character(len=1) :: file_stack(0))

        depth = 0

        call load_file_recursive(filename, lines, file_sources, line_nums, file_stack, depth, ounit)

    end subroutine load_file_lines


    !--------------------------------------------------------------------------
    ! load_file_recursive
    !
    ! purpose: recursive include expansion for input files.
    !
    ! inputs:
    !          1. filename
    !          2. ounit
    !
    ! outputs:
    !          1. lines
    !          2. file_sources
    !          3. line_nums
    !--------------------------------------------------------------------------
    recursive subroutine load_file_recursive(filename, lines, file_sources, line_nums, file_stack, depth, ounit)
        implicit none
        character(len=*), intent(in) :: filename
        character(len=:), allocatable, intent(inout) :: lines(:)
        character(len=:), allocatable, intent(inout) :: file_sources(:)
        integer, allocatable, intent(inout) :: line_nums(:)
        character(len=:), allocatable, intent(inout) :: file_stack(:)
        integer, intent(inout) :: depth
        integer, intent(in) :: ounit
        !
        integer :: unit, io, line_num
        character(len=1024) :: raw_line
        character(len=:), allocatable :: content
        character(len=:), allocatable :: token
        character(len=:), allocatable :: rest
        character(len=:), allocatable :: include_file
        character(len=:), allocatable :: dirname

        if (depth >= max_include_depth) then
            write(ounit,*) 'error: include depth exceeded for ', trim(filename)
            stop
        endif

        ! this ensures there is no circular include statements in the input files
        if (is_in_file_stack(filename, file_stack)) then
            write(ounit,*) 'error: circular include statements detected in ', trim(filename)
            stop
        endif

        call push_file_stack(file_stack, filename)
        depth = depth + 1

        open(newunit=unit, file=trim(filename), status='old', iostat=io)
        if (io /= 0) then
            write(ounit,*) 'error: cannot open file ', trim(filename)
            stop
        endif

        dirname = get_dirname(trim(filename))
        io = 0
        line_num = 0
        do while (io == 0)
            read(unit,'(A)', iostat=io) raw_line
            if (io /= 0) exit
            line_num = line_num + 1

            call strip_comment(raw_line, content)
            if (is_blank(content)) cycle

            ! if formatted in the intended way this will extract the first token which should be @include or include
            call split_first_token(content, token, rest)

            if (normalise_name(token) == '@INCLUDE' .or. normalise_name(token) == 'INCLUDE') then

                if (len_trim(rest) == 0) then
                    write(ounit,*) 'error: missing include filename at ', trim(filename), ':', line_num
                    stop
                endif

                if (is_absolute_path(rest)) then
                    include_file = trim(rest)
                else
                    include_file = join_path(dirname, trim(rest))
                endif

                call load_file_recursive(include_file, lines, file_sources, line_nums, file_stack, depth, ounit)

            else
                call append_line(lines, file_sources, line_nums, trim(content), trim(filename), line_num)
            endif
        enddo

        ! close the file and pop the file stack
        close(unit)
        depth = depth - 1
        call pop_file_stack(file_stack)

    end subroutine load_file_recursive


    !--------------------------------------------------------------------------
    ! append_line
    !
    ! purpose: append a line and metadata to arrays.
    !
    ! inputs:
    !          1. line
    !          2. source
    !          3. line_num
    !
    ! outputs:
    !          1. lines
    !          2. file_sources
    !          3. line_nums
    !--------------------------------------------------------------------------
    subroutine append_line(lines, file_sources, line_nums, line, source, line_num)
        implicit none
        character(len=:), allocatable, intent(inout) :: lines(:)
        character(len=:), allocatable, intent(inout) :: file_sources(:)
        integer, allocatable, intent(inout) :: line_nums(:)
        character(len=*), intent(in) :: line
        character(len=*), intent(in) :: source
        integer, intent(in) :: line_num
        !
        character(len=:), allocatable :: tmp_lines(:)
        character(len=:), allocatable :: tmp_sources(:)
        integer, allocatable :: tmp_nums(:)
        integer :: n
        integer :: len_lines, len_sources
        integer :: new_len_lines, new_len_sources

        n = size(lines)
        len_lines = 0
        len_sources = 0
        if (n > 0) then
            len_lines = len(lines(1))
            len_sources = len(file_sources(1))
        endif
        new_len_lines = max(1, max(len_lines, len_trim(line)))
        new_len_sources = max(1, max(len_sources, len_trim(source)))
        allocate(character(len=new_len_lines) :: tmp_lines(n+1))
        allocate(character(len=new_len_sources) :: tmp_sources(n+1))
        allocate(tmp_nums(n+1))

        if (n > 0) then
            tmp_lines(1:n) = lines
            tmp_sources(1:n) = file_sources
            tmp_nums(1:n) = line_nums
        endif
        tmp_lines(n+1) = line
        tmp_sources(n+1) = source
        tmp_nums(n+1) = line_num

        call move_alloc(tmp_lines, lines)
        call move_alloc(tmp_sources, file_sources)
        call move_alloc(tmp_nums, line_nums)

    end subroutine append_line


    !--------------------------------------------------------------------------
    ! push_file_stack
    !
    ! purpose: push a filename onto the include file stack.
    !
    ! inputs:
    !          1. file_stack
    !          2. filename
    !
    ! outputs:
    !          1. file_stack
    !--------------------------------------------------------------------------
    subroutine push_file_stack(file_stack, filename)
        implicit none
        character(len=:), allocatable, intent(inout) :: file_stack(:)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: tmp(:)
        integer :: n_include_depth

        n_include_depth = size(file_stack)

        if (n_include_depth > 0) then
            allocate(character(len=max(1, max(len(file_stack(1)), len_trim(filename)))) :: tmp(n_include_depth+1))
        else
            allocate(character(len=max(1, len_trim(filename))) :: tmp(n_include_depth+1))
        endif

        if (n_include_depth > 0) then
            tmp(1:n_include_depth) = file_stack
        endif
        tmp(n_include_depth+1) = trim(filename)
        call move_alloc(tmp, file_stack)

    end subroutine push_file_stack


    !--------------------------------------------------------------------------
    ! pop_file_stack
    !
    ! purpose: pop the top of the include file stack.
    !
    ! inputs:
    !          1. file_stack
    !
    ! outputs:
    !          1. file_stack
    !--------------------------------------------------------------------------
    subroutine pop_file_stack(file_stack)
        implicit none
        character(len=:), allocatable, intent(inout) :: file_stack(:)
        !
        character(len=:), allocatable :: tmp(:)
        integer :: n_include_depth

        n_include_depth = size(file_stack)

        if (n_include_depth <= 1) then
            allocate(character(len=1) :: tmp(0))
        else
            allocate(character(len=len(file_stack(1))) :: tmp(n_include_depth-1))
            tmp = file_stack(1:n_include_depth-1)
        endif

        call move_alloc(tmp, file_stack)

    end subroutine pop_file_stack


    !--------------------------------------------------------------------------
    ! is_in_file_stack
    !
    ! purpose: check if a filename is in the include file stack.
    !
    ! inputs:
    !          1. filename
    !          2. file_stack
    !
    ! outputs:
    !          1. is_in_file_stack
    !--------------------------------------------------------------------------
    logical function is_in_file_stack(filename, file_stack)
        implicit none
        character(len=*), intent(in) :: filename
        character(len=:), allocatable, intent(in) :: file_stack(:)
        !
        integer :: i

        is_in_file_stack = .false.
        do i = 1, size(file_stack)
            if (trim(file_stack(i)) == trim(filename)) then
                is_in_file_stack = .true.
                return
            endif
        enddo

    end function is_in_file_stack

end module fparse_io
