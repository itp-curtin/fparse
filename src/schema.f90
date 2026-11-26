module schema_m
    use strings_m, only: string_t
    use block_def_m, only: block_def_t
    use fparse_tools_m, only: fparse_tools
    use error_handling_m, only: error_handling
    implicit none

    private

    public :: schema_t

    type :: schema_t
        private
        type(block_def_t), allocatable :: blocks(:)
    contains
        procedure :: add_block
        procedure :: add_key
        procedure :: has_block
        procedure :: has_key
        procedure :: is_required
        procedure :: get_default
        procedure :: required_blocks
        procedure :: required_keys
        procedure, private :: find_block
        procedure, private :: get_block_idx
    end type schema_t

    interface schema_t
        module procedure new_schema
    end interface schema_t

contains

    function new_schema() result(schema)
        type(schema_t) :: schema

        allocate(schema%blocks(0))

    end function new_schema


    subroutine add_block(self, name, required)
        class(schema_t), intent(inout) :: self
        character(len=*), intent(in) :: name
        logical, intent(in), optional :: required
        type(block_def_t) :: block

        block = block_def_t(name, required)

        if (self%has_block(block%get_name())) then
            call error_handling%abort_routine(0, 'fparse', 'duplicate schema block: ' // block%get_name())
        end if

        if (.not. allocated(self%blocks)) allocate(self%blocks(0))
        self%blocks = [self%blocks, block]

    end subroutine add_block


    subroutine add_key(self, block_name, key_name, required, has_default, default_value)
        class(schema_t), intent(inout) :: self
        character(len=*), intent(in) :: block_name, key_name
        logical, intent(in), optional :: required, has_default
        character(len=*), intent(in), optional :: default_value
        !
        logical :: use_default
        integer :: iblock

        iblock = self%get_block_idx(block_name)

        use_default = present(default_value)
        if (present(has_default)) use_default = has_default

        if (use_default) then
            if (.not. present(default_value)) then
                call error_handling%abort_routine(0, 'fparse', 'default value not supplied: ' // key_name)
            end if

            call self%blocks(iblock)%add_key(key_name, required, default_value)
        else
            call self%blocks(iblock)%add_key(key_name, required)
        end if

    end subroutine add_key


    integer function find_block(self, name) result(iblock)
        class(schema_t), intent(in) :: self
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: normalised
        integer :: i

        ! if the block with name "name" is not found function returns 0
        iblock = 0

        if (.not. allocated(self%blocks)) return
        normalised = fparse_tools%normalise_name(name)

        do i = 1, size(self%blocks)
            if (self%blocks(i)%get_name() /= normalised) cycle
            iblock = i
            return
        end do

    end function find_block


    integer function get_block_idx(self, name) result(iblock)
        class(schema_t), intent(in) :: self
        character(len=*), intent(in) :: name

        iblock = self%find_block(name)
        if (iblock == 0) then
            call error_handling%abort_routine(0, 'fparse', 'block not in schema: ' // name)
        end if

    end function get_block_idx


    logical function has_block(self, block_name)
        class(schema_t), intent(in) :: self
        character(len=*), intent(in) :: block_name

        has_block = self%find_block(block_name) > 0

    end function has_block


    logical function has_key(self, block_name, key_name)
        class(schema_t), intent(in) :: self
        character(len=*), intent(in) :: block_name, key_name
        !
        integer :: iblock

        has_key = .false.
        iblock = self%find_block(block_name)
        if (iblock > 0) has_key = self%blocks(iblock)%has_key(key_name)

    end function has_key


    logical function is_required(self, block_name, key_name)
        class(schema_t), intent(in) :: self
        character(len=*), intent(in) :: block_name
        character(len=*), intent(in), optional :: key_name
        !
        integer :: iblock

        iblock = self%get_block_idx(block_name)
        if (present(key_name)) then
            is_required = self%blocks(iblock)%key_is_required(key_name)
        else
            is_required = self%blocks(iblock)%is_required()
        end if

    end function is_required


    subroutine get_default(self, block_name, key_name, value, found)
        class(schema_t), intent(in) :: self
        character(len=*), intent(in) :: block_name, key_name
        character(len=:), allocatable, intent(out) :: value
        logical, intent(out) :: found
        !
        integer :: iblock

        iblock = self%get_block_idx(block_name)
        call self%blocks(iblock)%get_default(key_name, value, found)

    end subroutine get_default


    function required_blocks(self) result(names)
        class(schema_t), intent(in) :: self
        type(string_t), allocatable :: names(:)
        !
        integer :: i

        allocate(names(0))
        if (.not. allocated(self%blocks)) return

        do i = 1, size(self%blocks)
            if (self%blocks(i)%is_required()) then
                names = [names, string_t(self%blocks(i)%get_name())]
            end if
        end do

    end function required_blocks


    function required_keys(self, block_name) result(names)
        class(schema_t), intent(in) :: self
        character(len=*), intent(in) :: block_name
        !
        type(string_t), allocatable :: names(:)
        integer :: iblock

        iblock = self%get_block_idx(block_name)

        names = self%blocks(iblock)%required_keys()

    end function required_keys

end module schema_m
