module fparse_tools_m
    use strings_m, only: string_t
    implicit none
    
    private

    public :: fparse_tools

    type :: fparse_tools_t
    contains
        procedure, nopass :: normalise_name
        procedure, nopass :: normalise_key
        procedure, nopass :: split_first_token
        procedure, nopass :: split_tokens
        procedure, nopass :: resolve_path
    end type fparse_tools_t

    type(fparse_tools_t), parameter :: fparse_tools = fparse_tools_t()

contains

    function normalise_name(value) result(name)
        character(len=*), intent(in) :: value
        character(len=:), allocatable :: name
        !
        type(string_t) :: work
        integer :: first, last

        first = verify(value, ' ' // achar(9))
        last = verify(value, ' ' // achar(9), back=.true.)
        name = ''
        if (first == 0) return
        work = string_t(value(first:last))
        call work%make_upper()
        name = work%s

    end function normalise_name


    function normalise_key(value) result(key)
        character(len=*), intent(in) :: value
        !
        character(len=:), allocatable :: key
        type(string_t) :: work

        work = string_t(value)
        call work%remove('[]')
        key = fparse_tools%normalise_name(work%s)

    end function normalise_key


    subroutine split_first_token(value, first, rest)
        character(len=*), intent(in) :: value
        character(len=:), allocatable, intent(out) :: first, rest
        !
        integer :: start, finish, n

        n = len_trim(value)
        start = verify(value(:n), ' ' // achar(9))
        if (start == 0) then
            first = ''
            rest = ''
            return
        endif

        finish = scan(value(start:n), ' ' // achar(9))
        if (finish == 0) then
            first = value(start:n)
            rest = ''
        else
            finish = start + finish - 1
            first = value(start:finish-1)
            start = verify(value(finish:n), ' ' // achar(9))
            rest = ''
            if (start > 0) rest = value(finish+start-1:n)
        endif

    end subroutine split_first_token


    subroutine split_tokens(value, tokens)
        character(len=*), intent(in) :: value
        character(len=:), allocatable, intent(out) :: tokens(:)
        character(len=:), allocatable :: rest, token, tail
        integer :: n, i

        rest = value
        n = 0
        do
            call fparse_tools%split_first_token(rest, token, tail)
            if (len(token) == 0) exit
            n = n + 1
            rest = tail
        enddo

        allocate(character(len=max(1, len_trim(value))) :: tokens(n))

        rest = value

        do i = 1, n
            call fparse_tools%split_first_token(rest, token, tail)
            tokens(i) = token
            rest = tail
        enddo

    end subroutine split_tokens


    function resolve_path(filename, containing_file) result(path)
        character(len=*), intent(in) :: filename
        character(len=*), intent(in), optional :: containing_file
        character(len=:), allocatable :: path, work, part
        type(string_t), allocatable :: parts(:)
        integer :: slash, start, finish, n, i
        logical :: absolute

        work = trim(filename)
        if (len(work) == 0) then
            path = ''
            return
        endif

        if (present(containing_file)) then
            if (work(1:1) /= '/') then
                slash = index(containing_file, '/', back=.true.)
                work = containing_file(:slash) // work
            endif
        endif

        absolute = work(1:1) == '/'
        allocate(parts(len(work)))
        n = 0
        start = 1
        do while (start <= len(work))
            finish = index(work(start:), '/')
            if (finish == 0) finish = len(work) - start + 2
            part = work(start:start+finish-2)
            start = start + finish

            if (part == '' .or. part == '.') cycle

            if (part == '..' .and. n > 0) then
                if (parts(n)%s /= '..') then
                    n = n - 1
                    cycle
                endif
            endif

            if (part == '..' .and. absolute .and. n == 0) cycle
            n = n + 1
            parts(n) = string_t(part)
        enddo

        path = ''
        if (absolute) path = '/'
        do i = 1, n
            if (i > 1) path = path // '/'
            path = path // parts(i)%s
        enddo

        if (path == '') path = '.'

    end function resolve_path

end module fparse_tools_m
