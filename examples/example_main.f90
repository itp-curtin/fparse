program example_main
    use, intrinsic :: iso_fortran_env, only: real64
    use fparse_m, only: input_t
    implicit none
    type(input_t) :: input
    character(len=:), allocatable :: system_name, mode, flags(:), terms(:)
    integer :: nelecs, n_rpoints, nflags, nterms, nlevels, iterm
    integer, allocatable :: levels(:)
    real(real64) :: dr, rmax, z, cutoff
    logical :: use_cache

    input = input_t('example_input.in')
    ! other ways to initialise the input object:
    ! call input%initialise()
    ! call input%set_main_input_file('example_input.in')


    ! here we define the input schema, i.e. the expected structure of the input file
    call input%schema%add_block('TARGET', required=.true.)
    call input%schema%add_key('TARGET', 'system', required=.true.)
    call input%schema%add_key('TARGET', 'nelecs', required=.true.)
    call input%schema%add_key('TARGET', 'z', default_value='0.0')
    call input%schema%add_key('TARGET', 'terms')

    call input%schema%add_block('GRID', required=.true.)
    call input%schema%add_key('GRID', 'dr', required=.true.)
    call input%schema%add_key('GRID', 'rmax', required=.true.)
    call input%schema%add_key('GRID', 'n_rpoints', required=.true.)

    call input%schema%add_block('OPTIONS')
    call input%schema%add_key('OPTIONS', 'mode', default_value='standard')
    call input%schema%add_key('OPTIONS', 'flags')
    call input%schema%add_key('OPTIONS', 'use_cache', default_value='off')

    call input%schema%add_block('SETTINGS')
    call input%schema%add_key('SETTINGS', 'cutoff', default_value='1.0e-6')
    call input%schema%add_key('SETTINGS', 'levels')

    ! at this point we parse the input file and check that it follows the schema defined above
    call input%parse_input()

    ! now we can retrieve the values from the input file, using the read_* bindings
    call input%read_string('target', 'system', system_name)
    call input%read_integer('target', 'nelecs', nelecs)
    call input%read_real('target', 'z', z)
    call input%read_string_list('target', 'terms', terms, nterms)
    call input%read_real('grid', 'dr', dr)
    call input%read_real('grid', 'rmax', rmax)
    call input%read_integer('grid', 'n_rpoints', n_rpoints)
    call input%read_string('options', 'mode', mode)
    ! read_logical accepts the following inputs which correspond to true and false:
    ! true:  on,  yes, true,  .true.,  1
    ! false: off, no,  false, .false., 0
    call input%read_logical('options', 'use_cache', use_cache)
    call input%read_string_list('options', 'flags', flags, nflags)
    call input%read_real('settings', 'cutoff', cutoff)
    call input%read_integer_list('settings', 'levels', levels, nlevels)

    print *, 'system: ', system_name
    print *, 'nelecs: ', nelecs
    print *, 'Z: ', z
    print *, 'dr: ', dr
    print *, 'rmax: ', rmax
    print *, 'n_rpoints: ', n_rpoints
    if (input%has_key('options', 'mode')) then
        print *, 'mode: ', mode, ' (input)'
    else
        print *, 'mode: ', mode, ' (default)'
    end if
    print *, 'use_cache: ', use_cache
    print *, 'flags: ', nflags
    print *, 'cutoff: ', cutoff
    print *, 'levels: ', nlevels
    print *, 'requested term symbols:'
    do iterm = 1, nterms
        print *, '  ', trim(terms(iterm))
    end do

end program example_main
