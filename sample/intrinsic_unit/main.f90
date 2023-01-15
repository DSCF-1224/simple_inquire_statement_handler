program test

    ! required MODULE(s)
    use ,     intrinsic :: iso_fortran_env
    use , non_intrinsic :: simple_inquire_statement_handler

    ! require all variables to be explicitly declared
    implicit none

    call exec_test( INPUT_UNIT                   , 'INPUT_UNIT'                   )
    call exec_test( OUTPUT_UNIT                  , 'OUTPUT_UNIT'                  )
    call exec_test( ERROR_UNIT                   , 'ERROR_UNIT'                   )
    call exec_test( IOSTAT_INQUIRE_INTERNAL_UNIT , 'IOSTAT_INQUIRE_INTERNAL_UNIT' )

    contains

    subroutine exec_test ( unit_number, unit_name )

        ! argument(s) for this SUBROUTINE
        integer              , intent(in) :: unit_number
        character ( len= * ) , intent(in) :: unit_name

        ! declaration: variable(s) for this SUBROUTINE
        type(type_inquire_statement_handler) :: handler

        call handler%fetch_data_all(unit_number)

        print '(A,1X,A,/)' , '###' , unit_name

        print '(A)' , '```'
        print '(A)' , '[' // unit_name // ': all]'

        call handler%write_data_all(OUTPUT_UNIT)

        print '(/,A)' , '[' // unit_name // ': each]'

        call handler%write_data_access      (OUTPUT_UNIT)
        call handler%write_data_action      (OUTPUT_UNIT)
        call handler%write_data_blank       (OUTPUT_UNIT)
        call handler%write_data_delim       (OUTPUT_UNIT)
        call handler%write_data_direct      (OUTPUT_UNIT)
        call handler%write_data_exist       (OUTPUT_UNIT)
        call handler%write_data_form        (OUTPUT_UNIT)
        call handler%write_data_formatted   (OUTPUT_UNIT)
        call handler%write_data_name        (OUTPUT_UNIT)
        call handler%write_data_named       (OUTPUT_UNIT)
        call handler%write_data_nextrec     (OUTPUT_UNIT)
        call handler%write_data_number      (OUTPUT_UNIT)
        call handler%write_data_opened      (OUTPUT_UNIT)
        call handler%write_data_pad         (OUTPUT_UNIT)
        call handler%write_data_position    (OUTPUT_UNIT)
        call handler%write_data_read        (OUTPUT_UNIT)
        call handler%write_data_readwrite   (OUTPUT_UNIT)
        call handler%write_data_recl        (OUTPUT_UNIT)
        call handler%write_data_sequential  (OUTPUT_UNIT)
        call handler%write_data_unformatted (OUTPUT_UNIT)
        call handler%write_data_write       (OUTPUT_UNIT)

        print '(A,/)' , '```'

    end subroutine exec_test

end program test
