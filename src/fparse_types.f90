module fparse_types
    implicit none

    type key_def_type
        character(len=:), allocatable :: name
        logical :: required
        logical :: has_default
        character(len=:), allocatable :: default_value
    end type key_def_type

    type block_def_type
        character(len=:), allocatable :: name
        logical :: required
        type(key_def_type), allocatable :: keys(:)
    end type block_def_type

    type schema_type
        type(block_def_type), allocatable :: blocks(:)
    end type schema_type

    type key_value_pair_type
        character(len=:), allocatable :: key
        character(len=:), allocatable :: value
        integer :: line
        character(len=:), allocatable :: source
    end type key_value_pair_type

    type block_data_type
        character(len=:), allocatable :: name
        integer :: start_line
        integer :: end_line
        character(len=:), allocatable :: source
        type(key_value_pair_type), allocatable :: pairs(:)
    end type block_data_type

    type input_tree_type
        type(block_data_type), allocatable :: blocks(:)
    end type input_tree_type

end module fparse_types
