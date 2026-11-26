module input_m
    use, intrinsic :: iso_fortran_env, only: real64
    use strings_m, only: string_t
    use schema_m, only: schema_t
    use block_data_m, only: block_data_t
    use fparse_tools_m, only: fparse_tools
    use error_handling_m, only: error_handling
    use file_reader_m, only: file_reader_t
    implicit none

    private

    public :: input_t

    type :: input_t
        type(schema_t) :: schema
        character(len=:), allocatable, private :: main_input_file
        type(block_data_t), allocatable, private :: block_data(:)
        logical, private :: parsed = .false.
    contains
        procedure :: initialise
        procedure :: set_main_input_file
        procedure :: parse_input
        procedure :: has_block
        procedure :: has_key
        procedure :: read_string
        procedure :: read_integer
        procedure :: read_real
        procedure :: read_logical
        procedure :: read_integer_list
        procedure :: read_real_list
        procedure :: read_string_list
        procedure, private :: parse_lines
        procedure, private :: validate_required_inputs
        procedure, private :: find_block
        procedure, private :: append_block
        procedure, private :: get_raw_value
        procedure, private :: numeric_tokens
        procedure, private :: fparse_error
    end type

    interface input_t
        module procedure new_input
    end interface

contains

    function new_input(main_input_file, schema) result(input)
        character(len=*), intent(in), optional :: main_input_file
        type(schema_t), intent(in), optional :: schema
        !
        type(input_t) :: input

        call input%initialise(main_input_file, schema)

    end function new_input


    subroutine initialise(self, main_input_file, schema)
        class(input_t), intent(out) :: self
        character(len=*), intent(in), optional :: main_input_file
        type(schema_t), intent(in), optional :: schema

        self%schema = schema_t()

        if (present(schema)) then
            self%schema = schema
        end if

        allocate(self%block_data(0))

        if (present(main_input_file)) then
            call self%set_main_input_file(main_input_file)
        end if

    end subroutine initialise


    subroutine set_main_input_file(self, file)
        class(input_t), intent(inout) :: self
        character(len=*), intent(in) :: file

        self%main_input_file = trim(file)
        self%parsed = .false.

        if (allocated(self%block_data)) then
            deallocate(self%block_data)
        end if

        allocate(self%block_data(0))

    end subroutine set_main_input_file


    subroutine parse_input(self)
        class(input_t), intent(inout) :: self
        !
        type(file_reader_t) :: reader

        if (.not. allocated(self%main_input_file)) then
            call error_handling%abort_routine(0, 'fparse', 'main input file has not been set')
        end if

        if (len_trim(self%main_input_file) == 0) then
            call error_handling%abort_routine(0, 'fparse', 'main input file is empty')
        end if

        self%parsed = .false.
        call reader%load(self%main_input_file)
        call self%parse_lines(reader)
        call self%validate_required_inputs()
        self%parsed = .true.

    end subroutine parse_input


    subroutine parse_lines(self, reader)
        class(input_t), intent(inout) :: self
        type(file_reader_t), intent(in) :: reader
        !
        type(block_data_t) :: current_block
        character(len=:), allocatable :: line, source, token, rest, block_name, key_name
        integer :: i, number
        logical :: in_block

        if (allocated(self%block_data)) deallocate(self%block_data)
        allocate(self%block_data(0))
        in_block = .false.

        do i = 1, reader%count()
            call reader%get_line(i, line, source, number)
            if (len_trim(line) == 0) cycle

            if (.not. in_block) then

                if (fparse_tools%normalise_name(line) == 'END') then
                    call error_handling%abort_routine(number, source, 'unexpected end without block')
                end if

                block_name = fparse_tools%normalise_name(line)

                if (.not. self%schema%has_block(block_name)) then
                    call error_handling%abort_routine(number, source, 'unknown block: ' // line)
                end if

                if (self%has_block(block_name)) then
                    call error_handling%abort_routine(number, source, 'duplicate block: ' // line)
                end if

                current_block = block_data_t(block_name, source, number)
                in_block = .true.

            else if (fparse_tools%normalise_name(line) == 'END') then
                call current_block%finish(number)
                call self%append_block(current_block)
                in_block = .false.
            else
                if (self%schema%has_block(line)) then
                    call error_handling%abort_routine(number, source, 'block not closed before new block')
                end if

                call fparse_tools%split_first_token(line, token, rest)

                if (rest == '') then
                    call error_handling%abort_routine(number, source, 'missing value for key')
                end if

                key_name = fparse_tools%normalise_key(token)

                if (.not. self%schema%has_key(block_name, key_name)) then
                    call error_handling%abort_routine(number, source, 'unknown key: ' // token)
                end if

                call current_block%add_pair(token, rest, source, number)
            end if
        end do

        if (in_block) then
            call current_block%get_location(source, number)
            call error_handling%abort_routine(number, source, 'block not closed: ' // current_block%get_name())
        end if

    end subroutine parse_lines


    subroutine validate_required_inputs(self)
        class(input_t), intent(in) :: self
        !
        type(string_t), allocatable :: names(:)
        character(len=:), allocatable :: block_name, source
        integer :: i, j, number

        names = self%schema%required_blocks()

        do i = 1, size(names)
            if (.not. self%has_block(names(i)%s)) then
                call error_handling%abort_routine(0, self%main_input_file, 'required block missing: ' // names(i)%s)
            end if
        end do

        ! required keys apply only when their containing block is present.
        ! could possibly be a case where if a block is missing, defaults are
        ! fine but when it is present the user really should include all the keys
        do i = 1, size(self%block_data)

            block_name = self%block_data(i)%get_name()
            names = self%schema%required_keys(block_name)

            do j = 1, size(names)
                if (self%block_data(i)%has_key(names(j)%s)) cycle
                call self%block_data(i)%get_location(source, number)
                call error_handling%abort_routine(number, source, 'required key missing: ' // block_name // ' ' // names(j)%s)
            end do

        end do

    end subroutine validate_required_inputs


    integer function find_block(self, name) result(iblock)
        class(input_t), intent(in) :: self
        character(len=*), intent(in) :: name
        !
        character(len=:), allocatable :: normalised
        integer :: i

        iblock = 0
        if (.not. allocated(self%block_data)) return
        normalised = fparse_tools%normalise_name(name)
        do i = 1, size(self%block_data)
            if (self%block_data(i)%get_name() /= normalised) cycle
            iblock = i
            return
        end do

    end function find_block


    subroutine append_block(self, block)
        class(input_t), intent(inout) :: self
        type(block_data_t), intent(in) :: block

        self%block_data = [self%block_data, block]

    end subroutine append_block


    logical function has_block(self, block_name)
        class(input_t), intent(in) :: self
        character(len=*), intent(in) :: block_name

        has_block = self%find_block(block_name) > 0

    end function has_block


    logical function has_key(self, block_name, key_name)
        class(input_t), intent(in) :: self
        character(len=*), intent(in) :: block_name, key_name
        !
        integer :: iblock

        has_key = .false.
        iblock = self%find_block(block_name)
        if (iblock > 0) has_key = self%block_data(iblock)%has_key(key_name)

    end function has_key


    subroutine get_raw_value(self, block_name, key_name, raw, found)
        class(input_t), intent(in) :: self
        character(len=*), intent(in) :: block_name, key_name
        character(len=:), allocatable, intent(out) :: raw
        logical, intent(out) :: found
        !
        integer :: iblock

        if (.not. self%parsed) then
            call error_handling%abort_routine(0, 'fparse', 'input has not been parsed yet')
        end if

        if (.not. self%schema%has_block(block_name)) then
            call self%fparse_error(block_name, key_name, 'block not in schema')
        end if

        if (.not. self%schema%has_key(block_name, key_name)) then
            call self%fparse_error(block_name, key_name, 'key not in schema')
        end if

        iblock = self%find_block(block_name)

        if (iblock > 0) then

            call self%block_data(iblock)%read_value(key_name, raw, found)

            if (found) then
                return
            else
                if (self%schema%is_required(block_name, key_name)) then
                    call self%fparse_error(block_name, key_name, 'required key missing')
                end if
            end if

        else if (self%schema%is_required(block_name)) then
            call self%fparse_error(block_name, key_name, 'required block missing')
        end if

        ! if we reached here, the block/key is not present in the input, but
        ! it is not required, so we can return the default value specified from the schema
        call self%schema%get_default(block_name, key_name, raw, found)

    end subroutine get_raw_value


    subroutine fparse_error(self, block_name, key_name, message)
        class(input_t), intent(in) :: self
        character(len=*), intent(in) :: block_name, key_name, message
        !
        character(len=:), allocatable :: source
        integer :: iblock, number

        iblock = self%find_block(block_name)
        if (iblock > 0) then
            if (self%block_data(iblock)%has_key(key_name)) then
                call self%block_data(iblock)%get_location(source, number, key_name)
                call error_handling%abort_routine(number, source, message // ': ' // block_name // ' ' // key_name)
                return
            end if
        end if

        call error_handling%abort_routine(0, 'schema/default', message // ': ' // block_name // ' ' // key_name)

    end subroutine fparse_error


    subroutine numeric_tokens(self, block_name, key_name, tokens, found, scalar)
        class(input_t), intent(in) :: self
        character(len=*), intent(in) :: block_name, key_name
        character(len=:), allocatable, intent(out) :: tokens(:)
        logical, intent(out) :: found
        logical, intent(in), optional :: scalar
        !
        character(len=:), allocatable :: raw

        call self%get_raw_value(block_name, key_name, raw, found)

        call fparse_tools%split_tokens(raw, tokens)

        if (.not. found) return

        if (size(tokens) == 0 .or. scan(raw, ',;/*') > 0) then
            call self%fparse_error(block_name, key_name, 'invalid numeric value')
        end if

        if (present(scalar)) then
            if (scalar .and. size(tokens) /= 1) then
                call self%fparse_error(block_name, key_name, 'expected one numeric value')
            end if
        end if

    end subroutine numeric_tokens


    subroutine read_string(self, block_name, key_name, value, found)
        class(input_t), intent(in) :: self
        character(len=*), intent(in) :: block_name
        character(len=*), intent(in) :: key_name
        character(len=:), allocatable, intent(out) :: value
        logical, intent(out), optional :: found
        !
        character(len=:), allocatable :: raw
        logical :: is_found

        call self%get_raw_value(block_name, key_name, raw, is_found)

        if (is_found) then
            value = trim(raw)
        else
            value = ''
        end if

        if (present(found)) then
            found = is_found
        end if

    end subroutine read_string


    subroutine read_integer(self, block_name, key_name, value, found)
        class(input_t), intent(in) :: self
        character(len=*), intent(in) :: block_name
        character(len=*), intent(in) :: key_name
        integer, intent(out) :: value
        logical, intent(out), optional :: found
        !
        logical :: is_found
        character(len=:), allocatable :: tokens(:)
        integer :: io

        call self%numeric_tokens(block_name, key_name, tokens, is_found, scalar=.true.)

        if (is_found) then
            read(tokens(1), *, iostat=io) value
            if (io /= 0) then
                call self%fparse_error(block_name, key_name, 'invalid integer value')
            end if
        else
            value = 0
        end if

        if (present(found)) then
            found = is_found
        end if

    end subroutine read_integer


    subroutine read_real(self, block_name, key_name, value, found)
        class(input_t), intent(in) :: self
        character(len=*), intent(in) :: block_name
        character(len=*), intent(in) :: key_name
        real(real64), intent(out) :: value
        logical, intent(out), optional :: found
        !
        logical :: is_found
        character(len=:), allocatable :: tokens(:)
        integer :: io

        call self%numeric_tokens(block_name, key_name, tokens, is_found, scalar=.true.)

        if (is_found) then
            read(tokens(1), *, iostat=io) value
            if (io /= 0) then
                call self%fparse_error(block_name, key_name, 'invalid real value')
            end if
        else
            value = 0.0_real64
        end if

        if (present(found)) then
            found = is_found
        end if

    end subroutine read_real


    subroutine read_logical(self, block_name, key_name, value, found)
        class(input_t), intent(in) :: self
        character(len=*), intent(in) :: block_name
        character(len=*), intent(in) :: key_name
        logical, intent(out) :: value
        logical, intent(out), optional :: found
        !
        character(len=:), allocatable :: raw
        logical :: is_found
        character(len=:), allocatable :: nraw

        call self%get_raw_value(block_name, key_name, raw, is_found)

        if (is_found) then
            nraw = fparse_tools%normalise_name(raw)
            select case (nraw)
            case ('ON','TRUE','YES','1','.TRUE.')
                value = .true.
            case ('OFF','FALSE','NO','0','.FALSE.')
                value = .false.
            case default
                call self%fparse_error(block_name, key_name, &
                    'invalid logical value')
            end select
        else
            value = .false.
        end if
        if (present(found)) then
            found = is_found
        end if

    end subroutine read_logical


    subroutine read_integer_list(self, block_name, key_name, values, nvalues, found)
        class(input_t), intent(in) :: self
        character(len=*), intent(in) :: block_name
        character(len=*), intent(in) :: key_name
        integer, allocatable, intent(out) :: values(:)
        integer, intent(out) :: nvalues
        logical, intent(out), optional :: found
        !
        logical :: is_found
        character(len=:), allocatable :: tokens(:)
        integer :: i, io

        call self%numeric_tokens(block_name, key_name, tokens, is_found)

        if (is_found) then
            nvalues = size(tokens)
            allocate(values(nvalues))
            do i = 1, nvalues
                read(tokens(i), *, iostat=io) values(i)
                if (io /= 0) then
                    call self%fparse_error(block_name, key_name, 'invalid integer list')
                end if
            end do
        else
            allocate(values(0))
            nvalues = 0
        end if

        if (present(found)) then
            found = is_found
        end if

    end subroutine read_integer_list


    subroutine read_real_list(self, block_name, key_name, values, nvalues, found)
        class(input_t), intent(in) :: self
        character(len=*), intent(in) :: block_name
        character(len=*), intent(in) :: key_name
        real(real64), allocatable, intent(out) :: values(:)
        integer, intent(out) :: nvalues
        logical, intent(out), optional :: found
        !
        logical :: is_found
        character(len=:), allocatable :: tokens(:)
        integer :: i, io

        call self%numeric_tokens(block_name, key_name, tokens, is_found)

        if (is_found) then
            nvalues = size(tokens)
            allocate(values(nvalues))
            do i = 1, nvalues
                read(tokens(i), *, iostat=io) values(i)
                if (io /= 0) then
                    call self%fparse_error(block_name, key_name, 'invalid real list')
                end if
            end do
        else
            allocate(values(0))
            nvalues = 0
        end if

        if (present(found)) then
            found = is_found
        end if

    end subroutine read_real_list


    subroutine read_string_list(self, block_name, key_name, values, nvalues, found)
        class(input_t), intent(in) :: self
        character(len=*), intent(in) :: block_name
        character(len=*), intent(in) :: key_name
        character(len=:), allocatable, intent(out) :: values(:)
        integer, intent(out) :: nvalues
        logical, intent(out), optional :: found
        !
        character(len=:), allocatable :: raw
        logical :: is_found

        call self%get_raw_value(block_name, key_name, raw, is_found)

        if (is_found) then
            call fparse_tools%split_tokens(raw, values)
            nvalues = size(values)
        else
            allocate(character(len=1) :: values(0))
            nvalues = 0
        end if

        if (present(found)) then
            found = is_found
        end if

    end subroutine read_string_list


end module input_m
