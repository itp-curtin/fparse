module fparse_util
    implicit none
    character(len=1), parameter :: backslash_char = achar(92)

contains

    !--------------------------------------------------------------------------
    ! normalise_name
    !
    ! purpose: return trimmed, left-adjusted, uppercase name.
    !
    ! inputs:
    !          1. input
    !
    ! outputs:
    !          1. output
    !--------------------------------------------------------------------------
    function normalise_name(input) result(output)
        implicit none
        character(len=*), intent(in) :: input
        !
        character(len=:), allocatable :: output
        character(len=:), allocatable :: tmp
        integer :: i

        tmp = adjustl(trim(input))
        allocate(character(len=len(tmp)) :: output)
        output = tmp
        do i = 1, len(output)
            if (output(i:i) >= 'a' .and. output(i:i) <= 'z') then
                output(i:i) = achar(iachar(output(i:i)) - 32)
            endif
        enddo

    end function normalise_name


    !--------------------------------------------------------------------------
    ! normalise_key
    !
    ! purpose: remove brackets if present and normalise string to uppercase.
    !
    ! inputs:
    !          1. input
    !
    ! outputs:
    !          1. output
    !--------------------------------------------------------------------------
    function normalise_key(input) result(output)
        implicit none
        character(len=*), intent(in) :: input
        !
        character(len=:), allocatable :: output
        character(len=:), allocatable :: tmp
        integer :: n_key_chars

        tmp = adjustl(trim(input))
        n_key_chars = len(tmp)
        if (n_key_chars >= 2) then
            if (tmp(1:1) == '[' .and. tmp(n_key_chars:n_key_chars) == ']') then
                tmp = tmp(2:n_key_chars-1)
            endif
        endif
        output = normalise_name(tmp)

    end function normalise_key


    !--------------------------------------------------------------------------
    ! strip_comment
    !
    ! purpose: remove comment starting with # and trim.
    !
    ! inputs:
    !          1. line
    !
    ! outputs:
    !          1. content
    !--------------------------------------------------------------------------
    subroutine strip_comment(line, content)
        implicit none
        character(len=*), intent(in) :: line
        character(len=:), allocatable, intent(out) :: content
        !
        integer :: pos

        pos = index(line, '#')
        if (pos > 0) then
            content = trim(line(1:pos-1))
        else
            content = trim(line)
        endif

    end subroutine strip_comment


    !--------------------------------------------------------------------------
    ! is_blank
    !
    ! purpose: check if a string is empty after trimming.
    !
    ! inputs:
    !          1. line
    !
    ! outputs:
    !          1. is_blank
    !--------------------------------------------------------------------------
    logical function is_blank(line)
        implicit none
        character(len=*), intent(in) :: line

        is_blank = (len_trim(line) == 0)

    end function is_blank


    !--------------------------------------------------------------------------
    ! ends_with_backslash
    !
    ! purpose: check if a trimmed line ends with a backslash.
    !
    ! inputs:
    !          1. line
    !
    ! outputs:
    !          1. ends_with_backslash
    !--------------------------------------------------------------------------
    logical function ends_with_backslash(line)
        implicit none
        character(len=*), intent(in) :: line
        !
        integer :: n_line_chars

        n_line_chars = len_trim(line)
        if (n_line_chars <= 0) then
            ends_with_backslash = .false.
        else
            ends_with_backslash = (line(n_line_chars:n_line_chars) == backslash_char)
        endif

    end function ends_with_backslash


    !--------------------------------------------------------------------------
    ! remove_trailing_backslash
    !
    ! purpose: remove a trailing backslash from a trimmed line.
    !
    ! inputs:
    !          1. line
    !
    ! outputs:
    !          1. output
    !--------------------------------------------------------------------------
    function remove_trailing_backslash(line) result(output)
        implicit none
        character(len=*), intent(in) :: line
        character(len=:), allocatable :: output
        !
        integer :: n_line_chars

        n_line_chars = len_trim(line)
        if (n_line_chars <= 0) then
            output = ''
        else
            if (line(n_line_chars:n_line_chars) == backslash_char) then
                output = trim(line(1:n_line_chars-1))
            else
                output = trim(line)
            endif
        endif

    end function remove_trailing_backslash


    !--------------------------------------------------------------------------
    ! split_first_token
    !
    ! purpose: split a line into first token and remainder.
    !
    ! inputs:
    !          1. line
    !
    ! outputs:
    !          1. token
    !          2. rest
    !--------------------------------------------------------------------------
    subroutine split_first_token(line, token, rest)
        implicit none
        character(len=*), intent(in) :: line
        character(len=:), allocatable, intent(out) :: token
        character(len=:), allocatable, intent(out) :: rest
        !
        integer :: pos
        character(len=:), allocatable :: tmp

        tmp = adjustl(trim(line))
        pos = index(tmp, ' ') ! by default 'index' returns the index of the first instance of the substring
        if (pos == 0) then
            token = tmp
            rest = ''
        else
            token = tmp(1:pos-1)
            rest = adjustl(trim(tmp(pos+1:)))
        endif

    end subroutine split_first_token


    !--------------------------------------------------------------------------
    ! split_tokens
    !
    ! purpose: split a string into space-separated tokens.
    !
    ! inputs:
    !          1. input
    !
    ! outputs:
    !          1. tokens
    !          2. ntokens
    !--------------------------------------------------------------------------
    subroutine split_tokens(input, tokens, ntokens)
        implicit none
        character(len=*), intent(in) :: input
        character(len=:), allocatable, intent(out) :: tokens(:)
        integer, intent(out) :: ntokens
        !
        integer :: i, n_input_chars, start, finish
        character(len=:), allocatable :: trimmed_input

        trimmed_input = adjustl(trim(input))
        n_input_chars = len_trim(trimmed_input)
        if (n_input_chars == 0) then
            allocate(character(len=1) :: tokens(0))
            ntokens = 0
            return
        endif

        ntokens = 0
        start = 1
        do while (start <= n_input_chars)
            finish = index(trimmed_input(start:), ' ')
            if (finish == 0) then
                ntokens = ntokens + 1
                exit
            else
                ntokens = ntokens + 1
                start = start + finish
                do while (start <= n_input_chars .and. trimmed_input(start:start) == ' ')
                    start = start + 1
                enddo
            endif
        enddo

        allocate(character(len=len(trimmed_input)) :: tokens(ntokens))
        ntokens = 0
        start = 1
        do while (start <= n_input_chars)
            finish = index(trimmed_input(start:), ' ')
            if (finish == 0) then
                ntokens = ntokens + 1
                tokens(ntokens) = trimmed_input(start:n_input_chars)
                exit
            else
                ntokens = ntokens + 1
                tokens(ntokens) = trimmed_input(start:start+finish-2)
                start = start + finish
                do while (start <= n_input_chars .and. trimmed_input(start:start) == ' ')
                    start = start + 1
                enddo
            endif
        enddo

    end subroutine split_tokens


    !--------------------------------------------------------------------------
    ! is_absolute_path
    !
    ! purpose: check if a path is absolute.
    !
    ! inputs:
    !          1. path
    !
    ! outputs:
    !          1. is_absolute_path
    !--------------------------------------------------------------------------
    logical function is_absolute_path(path)
        implicit none
        character(len=*), intent(in) :: path

        if (len_trim(path) == 0) then
            is_absolute_path = .false.
        else
            is_absolute_path = (path(1:1) == '/')
        endif

    end function is_absolute_path


    !--------------------------------------------------------------------------
    ! get_dirname
    !
    ! purpose: return directory of a file path.
    !
    ! inputs:
    !          1. path
    !
    ! outputs:
    !          1. dirname
    !--------------------------------------------------------------------------
    function get_dirname(path) result(dirname)
        implicit none
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: dirname
        integer :: i

        i = len_trim(path)
        do while (i > 0)
            if (path(i:i) == '/') exit ! at this point we know where the last forward slash is, so exit search loop
            i = i - 1
        enddo

        if (i <= 0) then ! if this is true we assume file is in the current directory, so we return '.' as the dirname
            dirname = '.'
        else
            dirname = path(1:i-1) ! otherwise store everything up to the last forward slash as the dirname
            if (len_trim(dirname) == 0) then
                dirname = '/'
            endif
        endif

    end function get_dirname


    !--------------------------------------------------------------------------
    ! join_path
    !
    ! purpose: join directory and filename into a path.
    !
    ! inputs:
    !          1. dirname
    !          2. filename
    !
    ! outputs:
    !          1. path
    !--------------------------------------------------------------------------
    function join_path(dirname, filename) result(path)
        implicit none
        character(len=*), intent(in) :: dirname
        character(len=*), intent(in) :: filename
        !
        character(len=:), allocatable :: path
        character(len=:), allocatable :: d

        d = trim(dirname)
        if (d == '/' .or. d == '') then
            path = trim(d) // trim(filename)
        else
            path = trim(d) // '/' // trim(filename)
        endif

    end function join_path

end module fparse_util
