program test

    ! required MODULE(s)
    use ,     intrinsic :: iso_fortran_env
    use , non_intrinsic :: simple_inquire_statement_handler

    ! require all variables to be explicitly declared
    implicit none

    call exec_test( OUTPUT_UNIT , 'OUTPUT_UNIT' )
    call exec_test( ERROR_UNIT  , 'ERROR_UNIT'  )

    contains

    subroutine exec_test ( unit_number, unit_name )

        ! argument(s) for this SUBROUTINE
        integer              , intent(in) :: unit_number
        character ( len= * ) , intent(in) :: unit_name

        ! declaration: variable(s) for this SUBROUTINE
        type(type_inquire_statement_handler) :: handler

        call handler%fetch_data_all(unit_number)

        print '(/,A,A,A)' , '[', unit_name, ': all]'

        call handler%write_data_all(unit_number)

        print '(/,A,A,A)' , '[', unit_name, ': each]'

        call handler%write_data_access      (unit_number)
        call handler%write_data_action      (unit_number)
        call handler%write_data_blank       (unit_number)
        call handler%write_data_delim       (unit_number)
        call handler%write_data_direct      (unit_number)
        call handler%write_data_exist       (unit_number)
        call handler%write_data_form        (unit_number)
        call handler%write_data_formatted   (unit_number)
        call handler%write_data_name        (unit_number)
        call handler%write_data_named       (unit_number)
        call handler%write_data_nextrec     (unit_number)
        call handler%write_data_number      (unit_number)
        call handler%write_data_opened      (unit_number)
        call handler%write_data_pad         (unit_number)
        call handler%write_data_position    (unit_number)
        call handler%write_data_read        (unit_number)
        call handler%write_data_readwrite   (unit_number)
        call handler%write_data_recl        (unit_number)
        call handler%write_data_sequential  (unit_number)
        call handler%write_data_unformatted (unit_number)
        call handler%write_data_write       (unit_number)

    end subroutine exec_test

end program test
