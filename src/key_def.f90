module key_def_m
    use fparse_tools_m, only: fparse_tools
    use error_handling_m, only: error_handling
    implicit none

    private

    public :: key_def_t

    type :: key_def_t
        private
        character(len=:), allocatable :: name
        logical :: required = .false.
        character(len=:), allocatable :: default_value
    contains
        procedure :: get_name
        procedure :: is_required
        procedure :: get_default_value
    end type key_def_t

    interface key_def_t
        module procedure new_key
    end interface key_def_t

contains

    function new_key(name, required, default_value) result(key)
        character(len=*), intent(in) :: name
        logical, intent(in), optional :: required
        character(len=*), intent(in), optional :: default_value
        !
        type(key_def_t) :: key

        key%name = fparse_tools%normalise_key(name)

        if (key%name == '') then
            call error_handling%abort_routine(0, 'fparse', 'empty schema key name')
        end if

        if (present(required)) key%required = required

        if (present(default_value)) then
            if (key%required) then
                call error_handling%abort_routine(0, 'fparse', 'required key cannot have a default: ' // key%name)
            end if
            key%default_value = trim(default_value)
        end if

    end function new_key


    function get_name(self) result(name)
        class(key_def_t), intent(in) :: self
        !
        character(len=:), allocatable :: name

        name = ''
        if (allocated(self%name)) name = self%name

    end function get_name


    logical function is_required(self)
        class(key_def_t), intent(in) :: self

        is_required = self%required

    end function is_required


    subroutine get_default_value(self, value, found)
        class(key_def_t), intent(in) :: self
        character(len=:), allocatable, intent(out) :: value
        logical, intent(out) :: found

        found = allocated(self%default_value)
        value = ''
        if (found) value = self%default_value

    end subroutine get_default_value

end module key_def_m
