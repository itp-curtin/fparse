module fparse_parser
    use fparse_types
    use fparse_schema, only: find_block_index, find_key_index
    use fparse_util, only: normalise_name, &
                           normalise_key, &
                           split_first_token, &
                           ends_with_backslash, &
                           remove_trailing_backslash
    use fparse_io, only: load_file_lines
    implicit none

contains

    !--------------------------------------------------------------------------
    ! parse_input
    !
    ! purpose: parse an input file into a tree using a schema.
    !
    ! inputs:
    !          1. schema
    !          2. filename
    !          3. ounit
    !
    ! outputs:
    !          1. tree
    !--------------------------------------------------------------------------
    subroutine parse_input(schema, filename, tree, ounit)
        implicit none
        type(schema_type), intent(in) :: schema
        character(len=*), intent(in) :: filename
        type(input_tree_type), intent(out) :: tree
        integer, intent(in) :: ounit
        !
        character(len=:), allocatable :: lines(:)
        character(len=:), allocatable :: file_sources(:)
        integer, allocatable :: line_nums(:)
        character(len=:), allocatable :: logical_lines(:)
        character(len=:), allocatable :: logical_file_sources(:)
        integer, allocatable :: logical_line_nums(:)

        call load_file_lines(filename, lines, file_sources, line_nums, ounit)

        call build_logical_lines(lines, file_sources, line_nums, logical_lines, logical_file_sources, logical_line_nums, ounit)

        call parse_lines(schema, logical_lines, logical_file_sources, logical_line_nums, tree, ounit)

        call validate_required(schema, tree, ounit)

    end subroutine parse_input


    !--------------------------------------------------------------------------
    ! build_logical_lines
    !
    ! purpose: join continued lines ending with backslash. this then compresses
    !          the original "lines" array into a new "logical_lines" array, with
    !          the multi-line inputs joined into a single line.
    !
    !
    ! inputs:
    !          1. lines
    !          2. file_sources
    !          3. line_nums
    !
    ! outputs:
    !          1. logical_lines
    !          2. logical_file_sources
    !          3. logical_line_nums
    !--------------------------------------------------------------------------
    subroutine build_logical_lines(lines, file_sources, line_nums, logical_lines, logical_file_sources, logical_line_nums, ounit)
        implicit none
        character(len=:), allocatable, intent(in) :: lines(:)
        character(len=:), allocatable, intent(in) :: file_sources(:)
        integer, allocatable, intent(in) :: line_nums(:)
        character(len=:), allocatable, intent(out) :: logical_lines(:)
        character(len=:), allocatable, intent(out) :: logical_file_sources(:)
        integer, allocatable, intent(out) :: logical_line_nums(:)
        integer, intent(in) :: ounit
        !
        integer :: i
        integer :: n_lines
        integer :: start_line
        character(len=:), allocatable :: current
        character(len=:), allocatable :: combined
        character(len=:), allocatable :: start_source

        allocate(character(len=1) :: logical_lines(0))
        allocate(character(len=1) :: logical_file_sources(0))
        allocate(logical_line_nums(0))

        i = 1
        n_lines = size(lines)
        do while (i <= n_lines)
            current = lines(i)
            combined = current
            start_source = file_sources(i)
            start_line = line_nums(i)

            do while (ends_with_backslash(combined))
                combined = remove_trailing_backslash(combined)
                i = i + 1
                if (i > n_lines) then
                    call stop_error(ounit, start_source, start_line, 'line continuation missing')
                endif
                ! put a space between the inputs across the two lines
                combined = trim(combined) // ' ' // trim(lines(i))
            enddo

            call append_logical(logical_lines, logical_file_sources, logical_line_nums, combined, start_source, start_line)
            i = i + 1

        enddo

    end subroutine build_logical_lines


    !--------------------------------------------------------------------------
    ! append_logical
    !
    ! purpose: append a logical line and metadata.
    !
    ! inputs:
    !          1. line
    !          2. source
    !          3. line_num
    !
    ! outputs:
    !          1. logical_lines
    !          2. logical_file_sources
    !          3. logical_line_nums
    !--------------------------------------------------------------------------
    subroutine append_logical(logical_lines, logical_file_sources, logical_line_nums, line, source, line_num)
        implicit none
        character(len=:), allocatable, intent(inout) :: logical_lines(:)
        character(len=:), allocatable, intent(inout) :: logical_file_sources(:)
        integer, allocatable, intent(inout) :: logical_line_nums(:)
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

        n = size(logical_lines)
        len_lines = 0
        len_sources = 0

        if (n > 0) then
            len_lines = len(logical_lines(1))
            len_sources = len(logical_file_sources(1))
        endif

        new_len_lines = max(1, max(len_lines, len_trim(line)))
        new_len_sources = max(1, max(len_sources, len_trim(source)))

        allocate(character(len=new_len_lines) :: tmp_lines(n+1))
        allocate(character(len=new_len_sources) :: tmp_sources(n+1))
        allocate(tmp_nums(n+1))

        if (n > 0) then
            tmp_lines(1:n) = logical_lines
            tmp_sources(1:n) = logical_file_sources
            tmp_nums(1:n) = logical_line_nums
        endif
        tmp_lines(n+1) = trim(line)
        tmp_sources(n+1) = source
        tmp_nums(n+1) = line_num

        call move_alloc(tmp_lines, logical_lines)
        call move_alloc(tmp_sources, logical_file_sources)
        call move_alloc(tmp_nums, logical_line_nums)

    end subroutine append_logical


    !--------------------------------------------------------------------------
    ! parse_lines
    !
    ! purpose: parse the logical lines into the input tree
    !
    ! inputs:
    !          1. schema
    !          2. logical_lines
    !          3. logical_file_sources
    !          4. logical_line_nums
    !          5. ounit
    !
    ! outputs:
    !          1. tree
    !--------------------------------------------------------------------------
    subroutine parse_lines(schema, logical_lines, logical_file_sources, logical_line_nums, tree, ounit)
        implicit none
        type(schema_type), intent(in) :: schema
        character(len=:), allocatable, intent(in) :: logical_lines(:)
        character(len=:), allocatable, intent(in) :: logical_file_sources(:)
        integer, allocatable, intent(in) :: logical_line_nums(:)
        type(input_tree_type), intent(out) :: tree
        integer, intent(in) :: ounit
        !
        integer :: i
        integer :: iblock_def
        logical :: in_block
        type(block_data_type) :: current_block
        character(len=:), allocatable :: line
        character(len=:), allocatable :: token
        character(len=:), allocatable :: rest
        character(len=:), allocatable :: block_name
        character(len=:), allocatable :: key_name

        allocate(tree%blocks(0))
        in_block = .false.

        do i = 1, size(logical_lines)

            line = logical_lines(i)
            if (len_trim(line) == 0) cycle

            if (.not. in_block) then

                if (normalise_name(line) == 'END') then
                    call stop_error(ounit, logical_file_sources(i), logical_line_nums(i), 'unexpected end without block')
                endif

                block_name = normalise_name(line)
                iblock_def = find_block_index(schema, block_name)

                if (iblock_def == 0) then
                    call stop_error(ounit, logical_file_sources(i), logical_line_nums(i), 'unknown block: ' // trim(line))
                endif

                ! this if statement checks that a block name is not already registered in the input tree
                if (find_block_data_index(tree, block_name) > 0) then
                    call stop_error(ounit, logical_file_sources(i), logical_line_nums(i), 'duplicate block: ' // trim(line))
                endif

                ! if reach this point then we must have entered a new block and are at its name line so we can initialise it
                in_block = .true.
                call init_block(current_block, block_name, logical_file_sources(i), logical_line_nums(i))

            else
                if (normalise_name(line) == 'END') then
                    current_block%end_line = logical_line_nums(i)
                    ! at this point we have collected all the data for the current block so we can append it to the input tree
                    call append_block(tree, current_block)
                    in_block = .false.
                else
                    if (find_block_index(schema, normalise_name(line)) > 0) then
                        call stop_error(ounit, logical_file_sources(i), logical_line_nums(i), 'block not closed before new block')
                    endif

                    call split_first_token(line, token, rest)

                    if (len_trim(rest) == 0) then
                        call stop_error(ounit, logical_file_sources(i), logical_line_nums(i), 'missing value for key')
                    endif

                    key_name = normalise_key(token)

                    if (find_key_index(schema%blocks(iblock_def), key_name) == 0) &
                        then
                        call stop_error(ounit, logical_file_sources(i), logical_line_nums(i), 'unknown key: ' // trim(token))
                    endif

                    if (find_pair_index(current_block, key_name) > 0) then
                        call stop_error(ounit, logical_file_sources(i), logical_line_nums(i), 'duplicate key: ' // trim(token))
                    endif

                    call append_pair(current_block, key_name, rest, logical_file_sources(i), logical_line_nums(i))
                endif
            endif
        enddo

        if (in_block) then
            call stop_error(ounit, current_block%source, current_block%start_line, 'block not closed: ' // trim(current_block%name))
        endif

    end subroutine parse_lines


    !--------------------------------------------------------------------------
    ! validate_required
    !
    ! purpose: check required blocks and keys are present by comparing what is
    !          in the schema with what is in the parsed input tree.
    !
    ! inputs:
    !          1. schema
    !          2. tree
    !          3. ounit
    !--------------------------------------------------------------------------
    subroutine validate_required(schema, tree, ounit)
        implicit none
        type(schema_type), intent(in) :: schema
        type(input_tree_type), intent(in) :: tree
        integer, intent(in) :: ounit
        !
        integer :: i, j, iblock

        do i = 1, size(schema%blocks)
            if (schema%blocks(i)%required) then
                if (find_block_data_index(tree, schema%blocks(i)%name) == 0) &
                    then
                    write(ounit,*) 'error: required block missing: ', trim(schema%blocks(i)%name)
                    stop
                endif
            endif
        enddo

        do i = 1, size(tree%blocks)
            iblock = find_block_index(schema, tree%blocks(i)%name)
            if (iblock == 0) cycle
            do j = 1, size(schema%blocks(iblock)%keys)
                if (schema%blocks(iblock)%keys(j)%required) then
                    if (find_pair_index(tree%blocks(i), &
                        schema%blocks(iblock)%keys(j)%name) == 0) then
                        write(ounit,*) 'error: required key missing: ', &
                            trim(schema%blocks(iblock)%name), ' ', &
                            trim(schema%blocks(iblock)%keys(j)%name)
                        stop
                    endif
                endif
            enddo
        enddo

    end subroutine validate_required


    !--------------------------------------------------------------------------
    ! init_block
    !
    ! purpose: initialise a block data object.
    !
    ! inputs:
    !          1. name
    !          2. source
    !          3. line
    !
    ! outputs:
    !          1. block
    !--------------------------------------------------------------------------
    subroutine init_block(block, name, source, line)
        implicit none
        type(block_data_type), intent(out) :: block
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: source
        integer, intent(in) :: line

        block%name = name
        block%source = source
        block%start_line = line
        block%end_line = -1
        allocate(block%pairs(0))

    end subroutine init_block


    !--------------------------------------------------------------------------
    ! append_block
    !
    ! purpose: append a block to the tree.
    !
    ! inputs:
    !          1. block
    !
    ! outputs:
    !          1. tree
    !--------------------------------------------------------------------------
    subroutine append_block(tree, block)
        implicit none
        type(input_tree_type), intent(inout) :: tree
        type(block_data_type), intent(in) :: block
        type(block_data_type), allocatable :: tmp(:)
        integer :: n

        n = size(tree%blocks)
        allocate(tmp(n+1))
        if (n > 0) then
            tmp(1:n) = tree%blocks
        endif
        tmp(n+1) = block
        call move_alloc(tmp, tree%blocks)

    end subroutine append_block


    !--------------------------------------------------------------------------
    ! append_pair
    !
    ! purpose: append a key/value pair to a block.
    !
    ! inputs:
    !          1. key
    !          2. value
    !          3. source
    !          4. line
    !
    ! outputs:
    !          1. block
    !--------------------------------------------------------------------------
    subroutine append_pair(block, key, value, source, line)
        implicit none
        type(block_data_type), intent(inout) :: block
        character(len=*), intent(in) :: key
        character(len=*), intent(in) :: value
        character(len=*), intent(in) :: source
        integer, intent(in) :: line
        !
        type(key_value_pair_type), allocatable :: tmp(:)
        integer :: n

        n = size(block%pairs)
        allocate(tmp(n+1))
        if (n > 0) then
            tmp(1:n) = block%pairs
        endif
        tmp(n+1)%key = key
        tmp(n+1)%value = trim(value)
        tmp(n+1)%source = source
        tmp(n+1)%line = line
        call move_alloc(tmp, block%pairs)

    end subroutine append_pair


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
    !          2. source
    !          3. line
    !          4. message
    !--------------------------------------------------------------------------
    subroutine stop_error(ounit, source, line, message)
        implicit none
        integer, intent(in) :: ounit
        character(len=*), intent(in) :: source
        integer, intent(in) :: line
        character(len=*), intent(in) :: message

        write(ounit,*) 'error: ', trim(message)
        write(ounit,*) 'source: ', trim(source), ' line ', line
        stop

    end subroutine stop_error

end module fparse_parser
