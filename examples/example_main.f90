program example_main
    use fparse
    implicit none
    type(schema_type) :: schema
    type(input_tree_type) :: tree
    character(len=:), allocatable :: system_name
    integer :: nelecs
    real(kind=8) :: dr, rmax
    integer :: n_rpoints
    character(len=:), allocatable :: mode
    character(len=:), allocatable :: flags(:)
    character(len=:), allocatable :: terms(:)
    integer :: nflags
    integer :: nterms
    logical :: mode_from_input
    real(kind=8) :: Z
    real(kind=8) :: cutoff
    integer, allocatable :: levels(:)
    integer :: nlevels
    logical :: use_cache
    integer :: iterm

    call init_schema(schema)
    call build_schema(schema)

    ! assumes user is running the example executable from the examples/ directory
    call parse_input(schema, 'example_input.in', tree, 6)

    ! process data from the TARGET block
    call get_string(schema, tree, 'target', 'system', system_name, 6)
    call get_integer(schema, tree, 'target', 'nelecs', nelecs, 6)
    call get_real(schema, tree, 'target', 'Z', Z, 6)
    call get_string_list(schema, tree, 'target', 'terms', terms, nterms, 6)

    ! process data from the GRID block
    call get_real(schema, tree, 'grid', 'dr', dr, 6)
    call get_real(schema, tree, 'grid', 'rmax', rmax, 6)
    call get_integer(schema, tree, 'grid', 'n_rpoints', n_rpoints, 6)

    ! this is using the has_key function a part of the fparse package
    mode_from_input = has_key(tree, 'options', 'mode')

    ! process data from the OPTIONS block
    call get_string(schema, tree, 'options', 'mode', mode, 6)
    call get_logical(schema, tree, 'options', 'use_cache', use_cache, 6)
    call get_string_list(schema, tree, 'options', 'flags', flags, nflags, 6)

    ! process data from the SETTINGS block
    call get_real(schema, tree, 'settings', 'cutoff', cutoff, 6)
    call get_integer_list(schema, tree, 'settings', 'levels', levels, nlevels, 6)

    ! print out the data parsed from the input file 
    write(6,*) 'system: ', trim(system_name)
    write(6,*) 'nelecs: ', nelecs
    write(6,*) 'Z: ', Z
    write(6,*) 'dr: ', dr
    write(6,*) 'rmax: ', rmax
    write(6,*) 'n_rpoints: ', n_rpoints
    if (mode_from_input) then
        write(6,*) 'mode: ', trim(mode), ' (input)'
    else
        write(6,*) 'mode: ', trim(mode), ' (default)'
    endif
    write(6,*) 'use_cache: ', use_cache
    write(6,*) 'flags: ', nflags
    write(6,*) 'cutoff: ', cutoff
    write(6,*) 'levels: ', nlevels
    write(6,*) 'terms: ', nterms
    write(6,*) '----------------------------------------'
    write(6,*) ' requested term symbols:'
    if (nterms > 0) then
        do iterm = 1, nterms
            write(6,*) '  ', trim(terms(iterm))
        end do
    else
        write(6,*) '  none'
    endif

contains

    !--------------------------------------------------------------------------
    ! build_schema
    !
    ! purpose: define the schema for the example input file.
    !
    ! inputs:
    !          1. schema
    !
    ! outputs:
    !          1. schema
    !--------------------------------------------------------------------------
    subroutine build_schema(schema)
        implicit none
        type(schema_type), intent(inout) :: schema

                             ! blockname, required
        call add_block(schema, 'TARGET',  .true.)

                           ! blockname, keyname, required, has_default, default_value
        call add_key(schema, 'TARGET', 'system', .true.,   .false.,     '')
        call add_key(schema, 'TARGET', 'nelecs', .true., .false., '')
        call add_key(schema, 'TARGET', 'z', .false., .true., '0.0')
        call add_key(schema, 'TARGET', 'terms', .false., .false., '')

        call add_block(schema, 'GRID', .true.)
        call add_key(schema, 'GRID', 'dr', .true., .false., '')
        call add_key(schema, 'GRID', 'rmax', .true., .false., '')
        call add_key(schema, 'GRID', 'n_rpoints', .true., .false., '')

        call add_block(schema, 'OPTIONS', .false.)
        call add_key(schema, 'OPTIONS', 'mode', .false., .true., 'standard')
        call add_key(schema, 'OPTIONS', 'flags', .false., .false., '')
        call add_key(schema, 'OPTIONS', 'use_cache', .false., .true., 'off')

        call add_block(schema, 'SETTINGS', .false.)
        call add_key(schema, 'SETTINGS', 'cutoff', .false., .true., '1.0e-6')
        call add_key(schema, 'SETTINGS', 'levels', .false., .false., '')

    end subroutine build_schema

end program example_main
