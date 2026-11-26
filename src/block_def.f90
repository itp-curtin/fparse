module block_def_m
    use strings_m, only: string_t
    use key_def_m, only: key_def_t
    use fparse_tools_m, only: fparse_tools
    use error_handling_m, only: error_handling
    implicit none

    private

    public :: block_def_t

    type :: block_def_t
        private
        character(len=:), allocatable :: name
        logical :: required = .false.
        type(key_def_t), allocatable :: keys(:)
    contains
        procedure :: get_name
        procedure :: is_required
        procedure :: add_key
        procedure :: has_key
        procedure :: key_is_required
        procedure :: get_default
        procedure :: required_keys
        procedure, private :: find_key
        procedure, private :: get_key_idx
    end type block_def_t

    interface block_def_t
        module procedure new_block
    end interface block_def_t

contains

    function new_block(name, required) result(block)
        character(len=*), intent(in) :: name
        logical, intent(in), optional :: required
        !
        type(block_def_t) :: block

        block%name = fparse_tools%normalise_name(name)

        if (block%name == '' .or. block%name == 'END') then
            call error_handling%abort_routine(0, 'fparse', 'invalid schema block name: ' // name)
        end if

        if (present(required)) block%required = required
        allocate(block%keys(0))

    end function new_block


    function get_name(self) result(name)
        class(block_def_t), intent(in) :: self
        character(len=:), allocatable :: name

        name = ''
        if (allocated(self%name)) name = self%name

    end function get_name


    logical function is_required(self)
        class(block_def_t), intent(in) :: self

        is_required = self%required
    end function is_required


    subroutine add_key(self, name, required, default_value)
        class(block_def_t), intent(inout) :: self
        character(len=*), intent(in) :: name
        logical, intent(in), optional :: required
        character(len=*), intent(in), optional :: default_value
        !
        type(key_def_t) :: key

        key = key_def_t(name, required, default_value)
        if (self%has_key(key%get_name())) then
            call error_handling%abort_routine(0, 'fparse', 'duplicate schema key: ' // self%get_name() // ' ' // key%get_name())
        end if

        if (.not. allocated(self%keys)) allocate(self%keys(0))
        self%keys = [self%keys, key]

    end subroutine add_key


    logical function has_key(self, name)
        class(block_def_t), intent(in) :: self
        character(len=*), intent(in) :: name

        has_key = self%find_key(name) > 0

    end function has_key


    integer function find_key(self, name) result(ikey)
        class(block_def_t), intent(in) :: self
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: normalised
        integer :: i

        ikey = 0
        if (.not. allocated(self%keys)) return
        normalised = fparse_tools%normalise_key(name)

        do i = 1, size(self%keys)
            if (self%keys(i)%get_name() /= normalised) cycle
            ikey = i
            return
        end do
    end function find_key


    integer function get_key_idx(self, name) result(ikey)
        class(block_def_t), intent(in) :: self
        character(len=*), intent(in) :: name

        ikey = self%find_key(name)

        if (ikey == 0) then
            call error_handling%abort_routine(0, 'fparse', 'key not in schema: ' // self%get_name() // ' ' // name)
        end if

    end function get_key_idx


    logical function key_is_required(self, name)
        class(block_def_t), intent(in) :: self
        character(len=*), intent(in) :: name
        integer :: ikey

        ikey = self%get_key_idx(name)
        key_is_required = self%keys(ikey)%is_required()
    end function key_is_required


    subroutine get_default(self, name, value, found)
        class(block_def_t), intent(in) :: self
        character(len=*), intent(in) :: name
        character(len=:), allocatable, intent(out) :: value
        logical, intent(out) :: found
        integer :: ikey

        ikey = self%get_key_idx(name)
        call self%keys(ikey)%get_default_value(value, found)

    end subroutine get_default


    function required_keys(self) result(names)
        class(block_def_t), intent(in) :: self
        type(string_t), allocatable :: names(:)
        integer :: i

        allocate(names(0))
        if (.not. allocated(self%keys)) return

        do i = 1, size(self%keys)
            if (self%keys(i)%is_required()) then
                names = [names, string_t(self%keys(i)%get_name())]
            end if
        end do

    end function required_keys

end module block_def_m
