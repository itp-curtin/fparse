module block_data_m
    use fparse_tools_m, only: fparse_tools
    use error_handling_m, only: error_handling
    implicit none

    private

    public :: block_data_t

    type :: key_value_pair_type
        private
        character(len=:), allocatable :: key, value, source
        integer :: line = 0
    end type key_value_pair_type

    type :: block_data_t
        private
        character(len=:), allocatable :: name, source
        integer :: start_line = 0, end_line = 0
        type(key_value_pair_type), allocatable :: key_value_pairs(:)
    contains
        procedure :: get_name
        procedure :: get_location
        procedure :: finish
        procedure :: add_pair
        procedure :: has_key
        procedure :: read_value
        procedure, private :: find_key
    end type block_data_t

    interface block_data_t
        module procedure new_block_data
    end interface block_data_t

contains

    function new_block_data(name, source, number) result(block)
        character(len=*), intent(in) :: name, source
        integer, intent(in) :: number
        !
        type(block_data_t) :: block

        block%name = fparse_tools%normalise_name(name)
        block%source = source
        block%start_line = number
        allocate(block%key_value_pairs(0))

    end function new_block_data


    function get_name(self) result(name)
        class(block_data_t), intent(in) :: self
        !
        character(len=:), allocatable :: name

        name = ''
        if (allocated(self%name)) name = self%name

    end function get_name


    subroutine get_location(self, source, number, key)
        class(block_data_t), intent(in) :: self
        character(len=:), allocatable, intent(out) :: source
        integer, intent(out) :: number
        character(len=*), intent(in), optional :: key
        !
        integer :: ikey

        source = ''
        if (allocated(self%source)) source = self%source
        number = self%start_line

        if (present(key)) then
            ikey = self%find_key(key)
            if (ikey > 0) then
                source = self%key_value_pairs(ikey)%source
                number = self%key_value_pairs(ikey)%line
            end if
        end if

    end subroutine get_location


    subroutine finish(self, number)
        class(block_data_t), intent(inout) :: self
        integer, intent(in) :: number

        self%end_line = number

    end subroutine finish


    subroutine add_pair(self, key, value, source, number)
        class(block_data_t), intent(inout) :: self
        character(len=*), intent(in) :: key, value, source
        integer, intent(in) :: number
        !
        character(len=:), allocatable :: normalised

        normalised = fparse_tools%normalise_key(key)
        if (self%has_key(normalised)) then
            call error_handling%abort_routine(number, source, 'duplicate key: ' // key)
        end if

        if (.not. allocated(self%key_value_pairs)) allocate(self%key_value_pairs(0))
        self%key_value_pairs = [self%key_value_pairs, key_value_pair_type(normalised, trim(value), source, number)]

    end subroutine add_pair


    logical function has_key(self, key)
        class(block_data_t), intent(in) :: self
        character(len=*), intent(in) :: key

        has_key = self%find_key(key) > 0

    end function has_key


    subroutine read_value(self, key, value, found)
        class(block_data_t), intent(in) :: self
        character(len=*), intent(in) :: key
        character(len=:), allocatable, intent(out) :: value
        logical, intent(out) :: found
        !
        integer :: ikey

        ikey = self%find_key(key)
        found = ikey > 0
        value = ''
        if (found) value = self%key_value_pairs(ikey)%value

    end subroutine read_value


    integer function find_key(self, key) result(ikey)
        class(block_data_t), intent(in) :: self
        character(len=*), intent(in) :: key
        !
        character(len=:), allocatable :: normalised
        integer :: i

        ikey = 0
        if (.not. allocated(self%key_value_pairs)) return
        normalised = fparse_tools%normalise_key(key)

        do i = 1, size(self%key_value_pairs)
            if (self%key_value_pairs(i)%key /= normalised) cycle
            ikey = i
            return
        end do

    end function find_key

end module block_data_m
