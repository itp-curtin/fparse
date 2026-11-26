module fparse_schema
    use fparse_types
    use fparse_util, only: normalise_name, normalise_key
    implicit none

contains

    !--------------------------------------------------------------------------
    ! init_schema
    !
    ! purpose: initialise an empty schema.
    !
    ! inputs:
    !          1. schema
    !
    ! outputs:
    !          1. schema
    !--------------------------------------------------------------------------
    subroutine init_schema(schema)
        implicit none
        type(schema_type), intent(out) :: schema

        allocate(schema%blocks(0))

    end subroutine init_schema


    !--------------------------------------------------------------------------
    ! add_block
    !
    ! purpose: add a block definition to the schema.
    !
    ! inputs:
    !          1. schema
    !          2. name
    !          3. required
    !
    ! outputs:
    !          1. schema
    !--------------------------------------------------------------------------
    subroutine add_block(schema, name, required)
        implicit none
        type(schema_type), intent(inout) :: schema
        character(len=*), intent(in) :: name
        logical, intent(in) :: required
        !
        type(block_def_type), allocatable :: tmp(:)
        integer :: n_curr_existing_blocks

        n_curr_existing_blocks = size(schema%blocks)
        allocate(tmp(n_curr_existing_blocks+1))

        if (n_curr_existing_blocks > 0) then
            tmp(1:n_curr_existing_blocks) = schema%blocks
        endif

        ! after this call, the allocation of tmp is transferred to schema%blocks, and tmp is deallocated
        call move_alloc(tmp, schema%blocks)

        schema%blocks(n_curr_existing_blocks+1)%name = normalise_name(name)
        schema%blocks(n_curr_existing_blocks+1)%required = required

        ! initialise the number of keys in the block to 0
        allocate(schema%blocks(n_curr_existing_blocks+1)%keys(0))

    end subroutine add_block


    !--------------------------------------------------------------------------
    ! add_key
    !
    ! purpose: add a key definition to a block in the schema.
    !
    ! inputs:
    !          1. schema
    !          2. block_name
    !          3. key_name
    !          4. required
    !          5. has_default
    !          6. default_value
    !
    ! outputs:
    !          1. schema
    !--------------------------------------------------------------------------
    subroutine add_key(schema, block_name, key_name, required, has_default, default_value)
        implicit none
        type(schema_type), intent(inout) :: schema
        character(len=*), intent(in) :: block_name
        character(len=*), intent(in) :: key_name
        logical, intent(in) :: required
        logical, intent(in) :: has_default
        character(len=*), intent(in) :: default_value
        !
        integer :: iblock
        type(key_def_type), allocatable :: tmp(:)
        integer :: n_curr_existing_keys

        iblock = find_block_index(schema, block_name)
        if (iblock <= 0) then
            write(*,*) 'error: block not found in schema: ', trim(block_name)
            stop
        endif

        n_curr_existing_keys = size(schema%blocks(iblock)%keys)
        allocate(tmp(n_curr_existing_keys+1))

        if (n_curr_existing_keys > 0) then
            tmp(1:n_curr_existing_keys) = schema%blocks(iblock)%keys
        endif

        call move_alloc(tmp, schema%blocks(iblock)%keys)

        schema%blocks(iblock)%keys(n_curr_existing_keys+1)%name = normalise_key(key_name)
        schema%blocks(iblock)%keys(n_curr_existing_keys+1)%required = required
        schema%blocks(iblock)%keys(n_curr_existing_keys+1)%has_default = has_default

        if (has_default) then
            schema%blocks(iblock)%keys(n_curr_existing_keys+1)%default_value = trim(default_value)
        endif

    end subroutine add_key


    !--------------------------------------------------------------------------
    ! find_block_index
    !
    ! purpose: find a block index by name in the schema.
    !
    ! inputs:
    !          1. schema
    !          2. name
    !
    ! outputs:
    !          1. find_block_index
    !--------------------------------------------------------------------------
    integer function find_block_index(schema, name)
        implicit none
        type(schema_type), intent(in) :: schema
        character(len=*), intent(in) :: name
        !
        character(len=:), allocatable :: nname
        integer :: i

        nname = normalise_name(name)

        find_block_index = 0
        do i = 1, size(schema%blocks)
            if (schema%blocks(i)%name == nname) then
                find_block_index = i
                return
            endif
        enddo

    end function find_block_index


    !--------------------------------------------------------------------------
    ! find_key_index
    !
    ! purpose: find a key index by name in a block definition.
    !
    ! inputs:
    !          1. block
    !          2. name
    !
    ! outputs:
    !          1. find_key_index
    !--------------------------------------------------------------------------
    integer function find_key_index(block, name)
        implicit none
        type(block_def_type), intent(in) :: block
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: nname
        integer :: i

        nname = normalise_key(name)

        find_key_index = 0
        do i = 1, size(block%keys)
            if (block%keys(i)%name == nname) then
                find_key_index = i
                return
            endif
        enddo

    end function find_key_index

end module fparse_schema
