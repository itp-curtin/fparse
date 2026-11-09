module fparse_getters
    use fparse_types
    use fparse_schema, only: find_block_index, find_key_index
    use fparse_util, only: normalise_name, normalise_key, split_tokens
    implicit none

contains

    !--------------------------------------------------------------------------
    ! has_block
    !
    ! purpose: check if a block exists in the parsed input tree.
    !
    ! inputs:
    !          1. tree
    !          2. block_name
    !
    ! outputs:
    !          1. has_block
    !--------------------------------------------------------------------------
    logical function has_block(tree, block_name)
        implicit none
        type(input_tree_type), intent(in) :: tree
        character(len=*), intent(in) :: block_name
        integer :: i
        character(len=:), allocatable :: nname

        nname = normalise_name(block_name)
        has_block = .false.
        do i = 1, size(tree%blocks)
            if (tree%blocks(i)%name == nname) then
                has_block = .true.
                return
            endif
        enddo

    end function has_block


    !--------------------------------------------------------------------------
    ! has_key
    !
    ! purpose: check if a key exists in a block.
    !
    ! inputs:
    !          1. tree
    !          2. block_name
    !          3. key_name
    !
    ! outputs:
    !          1. has_key
    !--------------------------------------------------------------------------
    logical function has_key(tree, block_name, key_name)
        implicit none
        type(input_tree_type), intent(in) :: tree
        character(len=*), intent(in) :: block_name
        character(len=*), intent(in) :: key_name
        !
        integer :: iblock, ikey

        iblock = find_block_data_index(tree, block_name)
        if (iblock <= 0) then
            has_key = .false.
            return
        endif
        ikey = find_pair_index(tree%blocks(iblock), key_name)
        has_key = (ikey > 0)

    end function has_key


    !--------------------------------------------------------------------------
    ! get_string
    !
    ! purpose: get a string value from the input tree.
    !
    ! inputs:
    !          1. schema
    !          2. tree
    !          3. block_name
    !          4. key_name
    !          5. ounit
    !
    ! outputs:
    !          1. value
    !          2. found
    !--------------------------------------------------------------------------
    subroutine get_string(schema, tree, block_name, key_name, value, ounit, found)
        implicit none
        type(schema_type), intent(in) :: schema
        type(input_tree_type), intent(in) :: tree
        character(len=*), intent(in) :: block_name
        character(len=*), intent(in) :: key_name
        character(len=:), allocatable, intent(out) :: value
        integer, intent(in) :: ounit
        logical, intent(out), optional :: found
        !
        character(len=:), allocatable :: raw
        logical :: is_found

        call get_raw_value(schema, tree, block_name, key_name, raw, ounit, is_found)

        if (is_found) then
            value = trim(raw)
        else
            value = ''
        endif

        if (present(found)) then
            found = is_found
        endif

    end subroutine get_string


    !--------------------------------------------------------------------------
    ! get_integer
    !
    ! purpose: get an integer value from the input tree.
    !
    ! inputs:
    !          1. schema
    !          2. tree
    !          3. block_name
    !          4. key_name
    !          5. ounit
    !
    ! outputs:
    !          1. value
    !          2. found
    !--------------------------------------------------------------------------
    subroutine get_integer(schema, tree, block_name, key_name, value, ounit, found)
        implicit none
        type(schema_type), intent(in) :: schema
        type(input_tree_type), intent(in) :: tree
        character(len=*), intent(in) :: block_name
        character(len=*), intent(in) :: key_name
        integer, intent(out) :: value
        integer, intent(in) :: ounit
        logical, intent(out), optional :: found
        !
        character(len=:), allocatable :: raw
        logical :: is_found
        integer :: io

        call get_raw_value(schema, tree, block_name, key_name, raw, ounit, is_found)

        if (is_found) then
            read(raw, *, iostat=io) value
            if (io /= 0) then
                call stop_error(ounit, block_name, key_name, 'invalid integer value')
            endif
        else
            value = 0
        endif

        if (present(found)) then
            found = is_found
        endif

    end subroutine get_integer


    !--------------------------------------------------------------------------
    ! get_real
    !
    ! purpose: get a real value from the input tree.
    !
    ! inputs:
    !          1. schema
    !          2. tree
    !          3. block_name
    !          4. key_name
    !          5. ounit
    !
    ! outputs:
    !          1. value
    !          2. found
    !--------------------------------------------------------------------------
    subroutine get_real(schema, tree, block_name, key_name, value, ounit, found)
        implicit none
        type(schema_type), intent(in) :: schema
        type(input_tree_type), intent(in) :: tree
        character(len=*), intent(in) :: block_name
        character(len=*), intent(in) :: key_name
        real(kind=8), intent(out) :: value
        integer, intent(in) :: ounit
        logical, intent(out), optional :: found
        !
        character(len=:), allocatable :: raw
        logical :: is_found
        integer :: io

        call get_raw_value(schema, tree, block_name, key_name, raw, ounit, is_found)

        if (is_found) then
            read(raw, *, iostat=io) value
            if (io /= 0) then
                call stop_error(ounit, block_name, key_name, 'invalid real value')
            endif
        else
            value = 0.0d0
        endif

        if (present(found)) then
            found = is_found
        endif

    end subroutine get_real


    !---------------------------------------------------------------------------
    ! get_logical
    !
    ! purpose: get a logical value from the input tree.
    !
    ! inputs:
    !          1. schema
    !          2. tree
    !          3. block_name
    !          4. key_name
    !          5. ounit
    !
    ! outputs:
    !          1. value
    !          2. found
    !---------------------------------------------------------------------------
    subroutine get_logical(schema, tree, block_name, key_name, value, ounit, found)
        implicit none
        type(schema_type), intent(in) :: schema
        type(input_tree_type), intent(in) :: tree
        character(len=*), intent(in) :: block_name
        character(len=*), intent(in) :: key_name
        logical, intent(out) :: value
        integer, intent(in) :: ounit
        logical, intent(out), optional :: found
        !
        character(len=:), allocatable :: raw
        logical :: is_found
        character(len=:), allocatable :: nraw

        call get_raw_value(schema, tree, block_name, key_name, raw, ounit, is_found)

        if (is_found) then
            nraw = normalise_name(raw)
            select case (nraw)
            case ('ON','TRUE','YES','1')
                value = .true.
            case ('OFF','FALSE','NO','0')
                value = .false.
            case default
                call stop_error(ounit, block_name, key_name, &
                    'invalid logical value')
            end select
        else
            value = .false.
        endif
        if (present(found)) then
            found = is_found
        endif

    end subroutine get_logical


    !--------------------------------------------------------------------------
    ! get_integer_list
    !
    ! purpose: get a list of integers from the input tree.
    !
    ! inputs:
    !          1. schema
    !          2. tree
    !          3. block_name
    !          4. key_name
    !          5. ounit
    !
    ! outputs:
    !          1. values
    !          2. nvalues
    !          3. found
    !--------------------------------------------------------------------------
    subroutine get_integer_list(schema, tree, block_name, key_name, values, nvalues, ounit, found)
        implicit none
        type(schema_type), intent(in) :: schema
        type(input_tree_type), intent(in) :: tree
        character(len=*), intent(in) :: block_name
        character(len=*), intent(in) :: key_name
        integer, allocatable, intent(out) :: values(:)
        integer, intent(out) :: nvalues
        integer, intent(in) :: ounit
        logical, intent(out), optional :: found
        !
        character(len=:), allocatable :: raw
        logical :: is_found
        character(len=:), allocatable :: tokens(:)
        integer :: i, io

        call get_raw_value(schema, tree, block_name, key_name, raw, ounit, is_found)

        if (is_found) then
            call split_tokens(raw, tokens, nvalues)
            allocate(values(nvalues))
            do i = 1, nvalues
                read(tokens(i), *, iostat=io) values(i)
                if (io /= 0) then
                    call stop_error(ounit, block_name, key_name, 'invalid integer list')
                endif
            enddo
        else
            allocate(values(0))
            nvalues = 0
        endif

        if (present(found)) then
            found = is_found
        endif

    end subroutine get_integer_list


    !--------------------------------------------------------------------------
    ! get_real_list
    !
    ! purpose: get a list of reals from the input tree.
    !
    ! inputs:
    !          1. schema
    !          2. tree
    !          3. block_name
    !          4. key_name
    !          5. ounit
    !
    ! outputs:
    !          1. values
    !          2. nvalues
    !          3. found
    !--------------------------------------------------------------------------
    subroutine get_real_list(schema, tree, block_name, key_name, values, nvalues, ounit, found)
        implicit none
        type(schema_type), intent(in) :: schema
        type(input_tree_type), intent(in) :: tree
        character(len=*), intent(in) :: block_name
        character(len=*), intent(in) :: key_name
        real(kind=8), allocatable, intent(out) :: values(:)
        integer, intent(out) :: nvalues
        integer, intent(in) :: ounit
        logical, intent(out), optional :: found
        !
        character(len=:), allocatable :: raw
        logical :: is_found
        character(len=:), allocatable :: tokens(:)
        integer :: i, io

        call get_raw_value(schema, tree, block_name, key_name, raw, ounit, is_found)

        if (is_found) then
            call split_tokens(raw, tokens, nvalues)
            allocate(values(nvalues))
            do i = 1, nvalues
                read(tokens(i), *, iostat=io) values(i)
                if (io /= 0) then
                    call stop_error(ounit, block_name, key_name, 'invalid real list')
                endif
            enddo
        else
            allocate(values(0))
            nvalues = 0
        endif

        if (present(found)) then
            found = is_found
        endif

    end subroutine get_real_list


    !--------------------------------------------------------------------------
    ! get_string_list
    !
    ! purpose: get a list of strings from the input tree.
    !
    ! inputs:
    !          1. schema
    !          2. tree
    !          3. block_name
    !          4. key_name
    !          5. ounit
    !
    ! outputs:
    !          1. values
    !          2. nvalues
    !          3. found
    !--------------------------------------------------------------------------
    subroutine get_string_list(schema, tree, block_name, key_name, values, nvalues, ounit, found)
        implicit none
        type(schema_type), intent(in) :: schema
        type(input_tree_type), intent(in) :: tree
        character(len=*), intent(in) :: block_name
        character(len=*), intent(in) :: key_name
        character(len=:), allocatable, intent(out) :: values(:)
        integer, intent(out) :: nvalues
        integer, intent(in) :: ounit
        logical, intent(out), optional :: found
        character(len=:), allocatable :: raw
        logical :: is_found

        call get_raw_value(schema, tree, block_name, key_name, raw, ounit, is_found)

        if (is_found) then
            call split_tokens(raw, values, nvalues)
        else
            allocate(character(len=1) :: values(0))
            nvalues = 0
        endif

        if (present(found)) then
            found = is_found
        endif

    end subroutine get_string_list


    !--------------------------------------------------------------------------
    ! get_raw_value
    !
    ! purpose: get raw string value with defaults and required checks.
    !
    ! inputs:
    !          1. schema
    !          2. tree
    !          3. block_name
    !          4. key_name
    !          5. ounit
    !
    ! outputs:
    !          1. raw
    !          2. found
    !--------------------------------------------------------------------------
    subroutine get_raw_value(schema, tree, block_name, key_name, raw, ounit, found)
        implicit none
        type(schema_type), intent(in) :: schema
        type(input_tree_type), intent(in) :: tree
        character(len=*), intent(in) :: block_name
        character(len=*), intent(in) :: key_name
        character(len=:), allocatable, intent(out) :: raw
        integer, intent(in) :: ounit
        logical, intent(out) :: found
        !
        integer :: iblock_def, ikey_def
        integer :: iblock_data, ikey_data

        iblock_def = find_block_index(schema, block_name)

        if (iblock_def <= 0) then
            call stop_error(ounit, block_name, key_name, 'block not in schema')
        endif

        ikey_def = find_key_index(schema%blocks(iblock_def), key_name)
        if (ikey_def <= 0) then
            call stop_error(ounit, block_name, key_name, 'key not in schema')
        endif

        iblock_data = find_block_data_index(tree, block_name)
        if (iblock_data <= 0) then
            if (schema%blocks(iblock_def)%required) then
                call stop_error(ounit, block_name, key_name, 'required block missing')
            else
                found = .false.
                raw = ''
                return
            endif
        endif

        ikey_data = find_pair_index(tree%blocks(iblock_data), key_name)
        if (ikey_data > 0) then
            raw = tree%blocks(iblock_data)%pairs(ikey_data)%value
            found = .true.
            return
        endif

        if (schema%blocks(iblock_def)%keys(ikey_def)%required) then
            call stop_error(ounit, block_name, key_name, 'required key missing')
        endif

        if (schema%blocks(iblock_def)%keys(ikey_def)%has_default) then
            raw = schema%blocks(iblock_def)%keys(ikey_def)%default_value
            found = .true.
            return
        endif

        raw = ''
        found = .false.

    end subroutine get_raw_value


    !--------------------------------------------------------------------------
    ! find_block_data_index
    !
    ! purpose: find a block in the parsed tree by name.
    !
    ! inputs:
    !          1. tree
    !          2. name
    !
    ! outputs:
    !          1. find_block_data_index
    !--------------------------------------------------------------------------
    integer function find_block_data_index(tree, name)
        implicit none
        type(input_tree_type), intent(in) :: tree
        character(len=*), intent(in) :: name
        !
        character(len=:), allocatable :: nname
        integer :: i

        nname = normalise_name(name)
        find_block_data_index = 0
        do i = 1, size(tree%blocks)
            if (tree%blocks(i)%name == nname) then
                find_block_data_index = i
                return
            endif
        enddo

    end function find_block_data_index


    !--------------------------------------------------------------------------
    ! find_pair_index
    !
    ! purpose: find a key/value pair in a block by key name.
    !
    ! inputs:
    !          1. block
    !          2. key
    !
    ! outputs:
    !          1. find_pair_index
    !--------------------------------------------------------------------------
    integer function find_pair_index(block, key)
        implicit none
        type(block_data_type), intent(in) :: block
        character(len=*), intent(in) :: key
        !
        character(len=:), allocatable :: nkey
        integer :: i

        nkey = normalise_key(key)
        find_pair_index = 0
        do i = 1, size(block%pairs)
            if (block%pairs(i)%key == nkey) then
                find_pair_index = i
                return
            endif
        enddo

    end function find_pair_index


    !--------------------------------------------------------------------------
    ! stop_error
    !
    ! purpose: stop with a formatted error message.
    !
    ! inputs:
    !          1. ounit
    !          2. block_name
    !          3. key_name
    !          4. message
    !--------------------------------------------------------------------------
    subroutine stop_error(ounit, block_name, key_name, message)
        implicit none
        integer, intent(in) :: ounit
        character(len=*), intent(in) :: block_name
        character(len=*), intent(in) :: key_name
        character(len=*), intent(in) :: message

        write(ounit,*) 'error: ', trim(message)
        write(ounit,*) 'block: ', trim(block_name)
        write(ounit,*) 'key:   ', trim(key_name)
        stop

    end subroutine stop_error

end module fparse_getters
