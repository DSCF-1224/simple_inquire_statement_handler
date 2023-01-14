module simple_inquire_statement_handler

    ! required MODULE(s)
    use , intrinsic :: iso_fortran_env



    ! require all variables to be explicitly declared
    implicit none



    ! accessibility setting
    private
    public  :: type_inquire_statement_handler



    ! definition: PARAMETER
    integer , parameter :: LEN_STR_FIELD_ACCESS      = len( 'SEQUENTIAL'  )
    integer , parameter :: LEN_STR_FIELD_ACTION      = len( 'READWRITE'   ) ! .eq. len('UNDEFINED')
    integer , parameter :: LEN_STR_FIELD_BLANK       = len( 'UNDEFINED'   )
    integer , parameter :: LEN_STR_FIELD_DELIM       = len( 'APOSTROPHE'  )
    integer , parameter :: LEN_STR_FIELD_DIRECT      = len( 'UNKNOWN'     )
    integer , parameter :: LEN_STR_FIELD_FORM        = len( 'UNFORMATTED' )
    integer , parameter :: LEN_STR_FIELD_FORMATTED   = len( 'UNKNOWN'     )
    integer , parameter :: LEN_STR_FIELD_NAME        = 512
    integer , parameter :: LEN_STR_FIELD_PAD         = len( 'YES'         )
    integer , parameter :: LEN_STR_FIELD_POSITION    = len( 'UNDEFINED'   )
    integer , parameter :: LEN_STR_FIELD_READ        = len( 'UNKNOWN'     )
    integer , parameter :: LEN_STR_FIELD_READWRITE   = len( 'UNKNOWN'     )
    integer , parameter :: LEN_STR_FIELD_SEQUENTIAL  = len( 'UNKNOWN'     )
    integer , parameter :: LEN_STR_FIELD_UNFORMATTED = len( 'UNKNOWN'     )
    integer , parameter :: LEN_STR_FIELD_WRITE       = len( 'UNKNOWN'     )



    ! definition: PARAMETER
    character( len= * ) , parameter :: ASCII_CHR_SPACE = ' '



    ! definition: PARAMETER
    logical , parameter :: DEFAULT_LGC_FIELD_EXIST  = .false.
    logical , parameter :: DEFAULT_LGC_FIELD_NAMED  = .false.
    logical , parameter :: DEFAULT_LGC_FIELD_OPENED = .false.



    ! definition: PARAMETER
    integer , parameter :: DEFAULT_INT_FIELD_NEXTREC =  1
    integer , parameter :: DEFAULT_INT_FIELD_NUMBER  = -1
    integer , parameter :: DEFAULT_INT_FIELD_RECL    =  0



    ! definition: PARAMETER
    character( len= * ) , parameter :: DEFAULT_STR_FIELD_ACCESS      = repeat( ASCII_CHR_SPACE , LEN_STR_FIELD_ACCESS      )
    character( len= * ) , parameter :: DEFAULT_STR_FIELD_ACTION      = repeat( ASCII_CHR_SPACE , LEN_STR_FIELD_ACTION      )
    character( len= * ) , parameter :: DEFAULT_STR_FIELD_BLANK       = repeat( ASCII_CHR_SPACE , LEN_STR_FIELD_BLANK       )
    character( len= * ) , parameter :: DEFAULT_STR_FIELD_DELIM       = repeat( ASCII_CHR_SPACE , LEN_STR_FIELD_DELIM       )
    character( len= * ) , parameter :: DEFAULT_STR_FIELD_DIRECT      = repeat( ASCII_CHR_SPACE , LEN_STR_FIELD_DIRECT      )
    character( len= * ) , parameter :: DEFAULT_STR_FIELD_FORM        = repeat( ASCII_CHR_SPACE , LEN_STR_FIELD_FORM        )
    character( len= * ) , parameter :: DEFAULT_STR_FIELD_FORMATTED   = repeat( ASCII_CHR_SPACE , LEN_STR_FIELD_FORMATTED   )
    character( len= * ) , parameter :: DEFAULT_STR_FIELD_NAME        = repeat( ASCII_CHR_SPACE , LEN_STR_FIELD_NAME        )
    character( len= * ) , parameter :: DEFAULT_STR_FIELD_PAD         = repeat( ASCII_CHR_SPACE , LEN_STR_FIELD_PAD         )
    character( len= * ) , parameter :: DEFAULT_STR_FIELD_POSITION    = repeat( ASCII_CHR_SPACE , LEN_STR_FIELD_POSITION    )
    character( len= * ) , parameter :: DEFAULT_STR_FIELD_READ        = repeat( ASCII_CHR_SPACE , LEN_STR_FIELD_READ        )
    character( len= * ) , parameter :: DEFAULT_STR_FIELD_READWRITE   = repeat( ASCII_CHR_SPACE , LEN_STR_FIELD_READWRITE   )
    character( len= * ) , parameter :: DEFAULT_STR_FIELD_SEQUENTIAL  = repeat( ASCII_CHR_SPACE , LEN_STR_FIELD_SEQUENTIAL  )
    character( len= * ) , parameter :: DEFAULT_STR_FIELD_UNFORMATTED = repeat( ASCII_CHR_SPACE , LEN_STR_FIELD_UNFORMATTED )
    character( len= * ) , parameter :: DEFAULT_STR_FIELD_WRITE       = repeat( ASCII_CHR_SPACE , LEN_STR_FIELD_WRITE       )



    ! definition: PARAMETER
    character( len= 11 ) , parameter :: FIELD_NAME_ACCESS      = 'ACCESS'
    character( len= 11 ) , parameter :: FIELD_NAME_ACTION      = 'ACTION'
    character( len= 11 ) , parameter :: FIELD_NAME_BLANK       = 'BLANK'
    character( len= 11 ) , parameter :: FIELD_NAME_DELIM       = 'DELIM'
    character( len= 11 ) , parameter :: FIELD_NAME_DIRECT      = 'DIRECT'
    character( len= 11 ) , parameter :: FIELD_NAME_EXIST       = 'EXIST'
    character( len= 11 ) , parameter :: FIELD_NAME_FORM        = 'FORM'
    character( len= 11 ) , parameter :: FIELD_NAME_FORMATTED   = 'FORMATTED'
    character( len= 11 ) , parameter :: FIELD_NAME_NAME        = 'NAME'
    character( len= 11 ) , parameter :: FIELD_NAME_NAMED       = 'NAMED'
    character( len= 11 ) , parameter :: FIELD_NAME_NEXTREC     = 'NEXTREC'
    character( len= 11 ) , parameter :: FIELD_NAME_NUMBER      = 'NUMBER'
    character( len= 11 ) , parameter :: FIELD_NAME_OPENED      = 'OPENED'
    character( len= 11 ) , parameter :: FIELD_NAME_PAD         = 'PAD'
    character( len= 11 ) , parameter :: FIELD_NAME_POSITION    = 'POSITION'
    character( len= 11 ) , parameter :: FIELD_NAME_READ        = 'READ'
    character( len= 11 ) , parameter :: FIELD_NAME_READWRITE   = 'READWRITE'
    character( len= 11 ) , parameter :: FIELD_NAME_RECL        = 'RECL'
    character( len= 11 ) , parameter :: FIELD_NAME_SEQUENTIAL  = 'SEQUENTIAL'
    character( len= 11 ) , parameter :: FIELD_NAME_UNFORMATTED = 'UNFORMATTED'
    character( len= 11 ) , parameter :: FIELD_NAME_WRITE       = 'WRITE'



    ! definition: PARAMETER
    character( len= * ) , parameter :: FMT_WRITE_INT = '(A,1X,"->",1X,I0)'
    character( len= * ) , parameter :: FMT_WRITE_LGC = '(A,1X,"->",1X,L1)'
    character( len= * ) , parameter :: FMT_WRITE_STR = '(A,1X,"->",1X,A)'



    ! definition: user-defined TYPE
    type type_inquire_statement_handler

        ! field(s) of this TYPE
        character ( len= LEN_STR_FIELD_ACCESS      ) , private :: access      = DEFAULT_STR_FIELD_ACCESS
        character ( len= LEN_STR_FIELD_ACTION      ) , private :: action      = DEFAULT_STR_FIELD_ACTION
        character ( len= LEN_STR_FIELD_BLANK       ) , private :: blank       = DEFAULT_STR_FIELD_BLANK
        character ( len= LEN_STR_FIELD_DELIM       ) , private :: delim       = DEFAULT_STR_FIELD_DELIM
        character ( len= LEN_STR_FIELD_DIRECT      ) , private :: direct      = DEFAULT_STR_FIELD_DIRECT
        logical                                      , private :: exist       = DEFAULT_LGC_FIELD_EXIST
        character ( len= LEN_STR_FIELD_FORM        ) , private :: form        = DEFAULT_STR_FIELD_FORM
        character ( len= LEN_STR_FIELD_FORMATTED   ) , private :: formatted   = DEFAULT_STR_FIELD_FORMATTED
        character ( len= LEN_STR_FIELD_NAME        ) , private :: name        = DEFAULT_STR_FIELD_NAME
        logical                                      , private :: named       = DEFAULT_LGC_FIELD_NAMED
        integer                                      , private :: nextrec     = DEFAULT_INT_FIELD_NEXTREC
        integer                                      , private :: number      = DEFAULT_INT_FIELD_NUMBER
        logical                                      , private :: opened      = DEFAULT_LGC_FIELD_OPENED
        character ( len= LEN_STR_FIELD_PAD         ) , private :: pad         = DEFAULT_STR_FIELD_PAD
        character ( len= LEN_STR_FIELD_POSITION    ) , private :: position    = DEFAULT_STR_FIELD_POSITION
        character ( len= LEN_STR_FIELD_READ        ) , private :: read        = DEFAULT_STR_FIELD_READ
        character ( len= LEN_STR_FIELD_READWRITE   ) , private :: readwrite   = DEFAULT_STR_FIELD_READWRITE
        integer                                      , private :: recl        = DEFAULT_INT_FIELD_RECL
        character ( len= LEN_STR_FIELD_SEQUENTIAL  ) , private :: sequential  = DEFAULT_STR_FIELD_SEQUENTIAL
        character ( len= LEN_STR_FIELD_UNFORMATTED ) , private :: unformatted = DEFAULT_STR_FIELD_UNFORMATTED
        character ( len= LEN_STR_FIELD_WRITE       ) , private :: write       = DEFAULT_STR_FIELD_WRITE

        contains

        ! kind: SUBROUTINE
        procedure , pass , private :: fetch_data_with_iostat_access
        procedure , pass , private :: fetch_data_with_iostat_action
        procedure , pass , private :: fetch_data_with_iostat_all
        procedure , pass , private :: fetch_data_with_iostat_blank
        procedure , pass , private :: fetch_data_with_iostat_delim
        procedure , pass , private :: fetch_data_with_iostat_direct
        procedure , pass , private :: fetch_data_with_iostat_exist
        procedure , pass , private :: fetch_data_with_iostat_form
        procedure , pass , private :: fetch_data_with_iostat_formatted
        procedure , pass , private :: fetch_data_with_iostat_name
        procedure , pass , private :: fetch_data_with_iostat_named
        procedure , pass , private :: fetch_data_with_iostat_nextrec
        procedure , pass , private :: fetch_data_with_iostat_number
        procedure , pass , private :: fetch_data_with_iostat_opened
        procedure , pass , private :: fetch_data_with_iostat_pad
        procedure , pass , private :: fetch_data_with_iostat_position
        procedure , pass , private :: fetch_data_with_iostat_read
        procedure , pass , private :: fetch_data_with_iostat_readwrite
        procedure , pass , private :: fetch_data_with_iostat_recl
        procedure , pass , private :: fetch_data_with_iostat_sequential
        procedure , pass , private :: fetch_data_with_iostat_unformatted
        procedure , pass , private :: fetch_data_with_iostat_write

        ! kind: SUBROUTINE
        procedure , pass , private :: fetch_data_without_iostat_access
        procedure , pass , private :: fetch_data_without_iostat_action
        procedure , pass , private :: fetch_data_without_iostat_all
        procedure , pass , private :: fetch_data_without_iostat_blank
        procedure , pass , private :: fetch_data_without_iostat_delim
        procedure , pass , private :: fetch_data_without_iostat_direct
        procedure , pass , private :: fetch_data_without_iostat_exist
        procedure , pass , private :: fetch_data_without_iostat_form
        procedure , pass , private :: fetch_data_without_iostat_formatted
        procedure , pass , private :: fetch_data_without_iostat_name
        procedure , pass , private :: fetch_data_without_iostat_named
        procedure , pass , private :: fetch_data_without_iostat_nextrec
        procedure , pass , private :: fetch_data_without_iostat_number
        procedure , pass , private :: fetch_data_without_iostat_opened
        procedure , pass , private :: fetch_data_without_iostat_pad
        procedure , pass , private :: fetch_data_without_iostat_position
        procedure , pass , private :: fetch_data_without_iostat_read
        procedure , pass , private :: fetch_data_without_iostat_readwrite
        procedure , pass , private :: fetch_data_without_iostat_recl
        procedure , pass , private :: fetch_data_without_iostat_sequential
        procedure , pass , private :: fetch_data_without_iostat_unformatted
        procedure , pass , private :: fetch_data_without_iostat_write

        ! kind: SUBROUTINE
        procedure , pass , private :: init_field_access
        procedure , pass , private :: init_field_action
        procedure , pass , private :: init_field_all
        procedure , pass , private :: init_field_blank
        procedure , pass , private :: init_field_delim
        procedure , pass , private :: init_field_direct
        procedure , pass , private :: init_field_exist
        procedure , pass , private :: init_field_form
        procedure , pass , private :: init_field_formatted
        procedure , pass , private :: init_field_name
        procedure , pass , private :: init_field_named
        procedure , pass , private :: init_field_nextrec
        procedure , pass , private :: init_field_number
        procedure , pass , private :: init_field_opened
        procedure , pass , private :: init_field_pad
        procedure , pass , private :: init_field_position
        procedure , pass , private :: init_field_read
        procedure , pass , private :: init_field_readwrite
        procedure , pass , private :: init_field_recl
        procedure , pass , private :: init_field_sequential
        procedure , pass , private :: init_field_unformatted
        procedure , pass , private :: init_field_write

        ! kind: SUBROUTINE
        procedure , pass , private :: write_data_with_iostat_access
        procedure , pass , private :: write_data_with_iostat_action
        procedure , pass , private :: write_data_with_iostat_all
        procedure , pass , private :: write_data_with_iostat_blank
        procedure , pass , private :: write_data_with_iostat_delim
        procedure , pass , private :: write_data_with_iostat_direct
        procedure , pass , private :: write_data_with_iostat_exist
        procedure , pass , private :: write_data_with_iostat_form
        procedure , pass , private :: write_data_with_iostat_formatted
        procedure , pass , private :: write_data_with_iostat_name
        procedure , pass , private :: write_data_with_iostat_named
        procedure , pass , private :: write_data_with_iostat_nextrec
        procedure , pass , private :: write_data_with_iostat_number
        procedure , pass , private :: write_data_with_iostat_opened
        procedure , pass , private :: write_data_with_iostat_pad
        procedure , pass , private :: write_data_with_iostat_position
        procedure , pass , private :: write_data_with_iostat_read
        procedure , pass , private :: write_data_with_iostat_readwrite
        procedure , pass , private :: write_data_with_iostat_recl
        procedure , pass , private :: write_data_with_iostat_sequential
        procedure , pass , private :: write_data_with_iostat_unformatted
        procedure , pass , private :: write_data_with_iostat_write

        ! kind: SUBROUTINE
        procedure , pass , private :: write_data_without_iostat_access
        procedure , pass , private :: write_data_without_iostat_action
        procedure , pass , private :: write_data_without_iostat_all
        procedure , pass , private :: write_data_without_iostat_blank
        procedure , pass , private :: write_data_without_iostat_delim
        procedure , pass , private :: write_data_without_iostat_direct
        procedure , pass , private :: write_data_without_iostat_exist
        procedure , pass , private :: write_data_without_iostat_form
        procedure , pass , private :: write_data_without_iostat_formatted
        procedure , pass , private :: write_data_without_iostat_name
        procedure , pass , private :: write_data_without_iostat_named
        procedure , pass , private :: write_data_without_iostat_nextrec
        procedure , pass , private :: write_data_without_iostat_number
        procedure , pass , private :: write_data_without_iostat_opened
        procedure , pass , private :: write_data_without_iostat_pad
        procedure , pass , private :: write_data_without_iostat_position
        procedure , pass , private :: write_data_without_iostat_read
        procedure , pass , private :: write_data_without_iostat_readwrite
        procedure , pass , private :: write_data_without_iostat_recl
        procedure , pass , private :: write_data_without_iostat_sequential
        procedure , pass , private :: write_data_without_iostat_unformatted
        procedure , pass , private :: write_data_without_iostat_write

        ! kind: INTERFACE
        generic , public :: fetch_data_access      => fetch_data_with_iostat_access
        generic , public :: fetch_data_access      => fetch_data_without_iostat_access
        generic , public :: fetch_data_action      => fetch_data_with_iostat_action
        generic , public :: fetch_data_action      => fetch_data_without_iostat_action
        generic , public :: fetch_data_all         => fetch_data_with_iostat_all
        generic , public :: fetch_data_all         => fetch_data_without_iostat_all
        generic , public :: fetch_data_blank       => fetch_data_with_iostat_blank
        generic , public :: fetch_data_blank       => fetch_data_without_iostat_blank
        generic , public :: fetch_data_delim       => fetch_data_with_iostat_delim
        generic , public :: fetch_data_delim       => fetch_data_without_iostat_delim
        generic , public :: fetch_data_direct      => fetch_data_with_iostat_direct
        generic , public :: fetch_data_direct      => fetch_data_without_iostat_direct
        generic , public :: fetch_data_exist       => fetch_data_with_iostat_exist
        generic , public :: fetch_data_exist       => fetch_data_without_iostat_exist
        generic , public :: fetch_data_form        => fetch_data_with_iostat_form
        generic , public :: fetch_data_form        => fetch_data_without_iostat_form
        generic , public :: fetch_data_formatted   => fetch_data_with_iostat_formatted
        generic , public :: fetch_data_formatted   => fetch_data_without_iostat_formatted
        generic , public :: fetch_data_name        => fetch_data_with_iostat_name
        generic , public :: fetch_data_name        => fetch_data_without_iostat_name
        generic , public :: fetch_data_named       => fetch_data_with_iostat_named
        generic , public :: fetch_data_named       => fetch_data_without_iostat_named
        generic , public :: fetch_data_nextrec     => fetch_data_with_iostat_nextrec
        generic , public :: fetch_data_nextrec     => fetch_data_without_iostat_nextrec
        generic , public :: fetch_data_number      => fetch_data_with_iostat_number
        generic , public :: fetch_data_number      => fetch_data_without_iostat_number
        generic , public :: fetch_data_opened      => fetch_data_with_iostat_opened
        generic , public :: fetch_data_opened      => fetch_data_without_iostat_opened
        generic , public :: fetch_data_pad         => fetch_data_with_iostat_pad
        generic , public :: fetch_data_pad         => fetch_data_without_iostat_pad
        generic , public :: fetch_data_position    => fetch_data_with_iostat_position
        generic , public :: fetch_data_position    => fetch_data_without_iostat_position
        generic , public :: fetch_data_read        => fetch_data_with_iostat_read
        generic , public :: fetch_data_read        => fetch_data_without_iostat_read
        generic , public :: fetch_data_readwrite   => fetch_data_with_iostat_readwrite
        generic , public :: fetch_data_readwrite   => fetch_data_without_iostat_readwrite
        generic , public :: fetch_data_recl        => fetch_data_with_iostat_recl
        generic , public :: fetch_data_recl        => fetch_data_without_iostat_recl
        generic , public :: fetch_data_sequential  => fetch_data_with_iostat_sequential
        generic , public :: fetch_data_sequential  => fetch_data_without_iostat_sequential
        generic , public :: fetch_data_unformatted => fetch_data_with_iostat_unformatted
        generic , public :: fetch_data_unformatted => fetch_data_without_iostat_unformatted
        generic , public :: fetch_data_write       => fetch_data_with_iostat_write
        generic , public :: fetch_data_write       => fetch_data_without_iostat_write

        ! kind: INTERFACE
        generic , public :: write_data_access      => write_data_with_iostat_access
        generic , public :: write_data_access      => write_data_without_iostat_access
        generic , public :: write_data_action      => write_data_with_iostat_action
        generic , public :: write_data_action      => write_data_without_iostat_action
        generic , public :: write_data_all         => write_data_with_iostat_all
        generic , public :: write_data_all         => write_data_without_iostat_all
        generic , public :: write_data_blank       => write_data_with_iostat_blank
        generic , public :: write_data_blank       => write_data_without_iostat_blank
        generic , public :: write_data_delim       => write_data_with_iostat_delim
        generic , public :: write_data_delim       => write_data_without_iostat_delim
        generic , public :: write_data_direct      => write_data_with_iostat_direct
        generic , public :: write_data_direct      => write_data_without_iostat_direct
        generic , public :: write_data_exist       => write_data_with_iostat_exist
        generic , public :: write_data_exist       => write_data_without_iostat_exist
        generic , public :: write_data_form        => write_data_with_iostat_form
        generic , public :: write_data_form        => write_data_without_iostat_form
        generic , public :: write_data_formatted   => write_data_with_iostat_formatted
        generic , public :: write_data_formatted   => write_data_without_iostat_formatted
        generic , public :: write_data_name        => write_data_with_iostat_name
        generic , public :: write_data_name        => write_data_without_iostat_name
        generic , public :: write_data_named       => write_data_with_iostat_named
        generic , public :: write_data_named       => write_data_without_iostat_named
        generic , public :: write_data_nextrec     => write_data_with_iostat_nextrec
        generic , public :: write_data_nextrec     => write_data_without_iostat_nextrec
        generic , public :: write_data_number      => write_data_with_iostat_number
        generic , public :: write_data_number      => write_data_without_iostat_number
        generic , public :: write_data_opened      => write_data_with_iostat_opened
        generic , public :: write_data_opened      => write_data_without_iostat_opened
        generic , public :: write_data_pad         => write_data_with_iostat_pad
        generic , public :: write_data_pad         => write_data_without_iostat_pad
        generic , public :: write_data_position    => write_data_with_iostat_position
        generic , public :: write_data_position    => write_data_without_iostat_position
        generic , public :: write_data_read        => write_data_with_iostat_read
        generic , public :: write_data_read        => write_data_without_iostat_read
        generic , public :: write_data_readwrite   => write_data_with_iostat_readwrite
        generic , public :: write_data_readwrite   => write_data_without_iostat_readwrite
        generic , public :: write_data_recl        => write_data_with_iostat_recl
        generic , public :: write_data_recl        => write_data_without_iostat_recl
        generic , public :: write_data_sequential  => write_data_with_iostat_sequential
        generic , public :: write_data_sequential  => write_data_without_iostat_sequential
        generic , public :: write_data_unformatted => write_data_with_iostat_unformatted
        generic , public :: write_data_unformatted => write_data_without_iostat_unformatted
        generic , public :: write_data_write       => write_data_with_iostat_write
        generic , public :: write_data_write       => write_data_without_iostat_write

    end type type_inquire_statement_handler



    ! definition: user-defined INTERFACE
    interface ! fetch_data

        module subroutine fetch_data_with_iostat_access ( handler, unit, iostat, iomsg )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer              , intent( in    ) :: unit
            integer              , intent( inout ) :: iostat
            character ( len= * ) , intent( inout ) :: iomsg

        end subroutine fetch_data_with_iostat_access



        module subroutine fetch_data_with_iostat_action ( handler, unit, iostat, iomsg )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer              , intent( in    ) :: unit
            integer              , intent( inout ) :: iostat
            character ( len= * ) , intent( inout ) :: iomsg

        end subroutine fetch_data_with_iostat_action



        module subroutine fetch_data_with_iostat_all ( handler, unit, iostat, iomsg )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer              , intent( in    ) :: unit
            integer              , intent( inout ) :: iostat
            character ( len= * ) , intent( inout ) :: iomsg

        end subroutine fetch_data_with_iostat_all



        module subroutine fetch_data_with_iostat_blank ( handler, unit, iostat, iomsg )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer              , intent( in    ) :: unit
            integer              , intent( inout ) :: iostat
            character ( len= * ) , intent( inout ) :: iomsg

        end subroutine fetch_data_with_iostat_blank



        module subroutine fetch_data_with_iostat_delim ( handler, unit, iostat, iomsg )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer              , intent( in    ) :: unit
            integer              , intent( inout ) :: iostat
            character ( len= * ) , intent( inout ) :: iomsg

        end subroutine fetch_data_with_iostat_delim



        module subroutine fetch_data_with_iostat_direct ( handler, unit, iostat, iomsg )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer              , intent( in    ) :: unit
            integer              , intent( inout ) :: iostat
            character ( len= * ) , intent( inout ) :: iomsg

        end subroutine fetch_data_with_iostat_direct



        module subroutine fetch_data_with_iostat_exist ( handler, unit, iostat, iomsg )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer              , intent( in    ) :: unit
            integer              , intent( inout ) :: iostat
            character ( len= * ) , intent( inout ) :: iomsg

        end subroutine fetch_data_with_iostat_exist



        module subroutine fetch_data_with_iostat_form ( handler, unit, iostat, iomsg )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer              , intent( in    ) :: unit
            integer              , intent( inout ) :: iostat
            character ( len= * ) , intent( inout ) :: iomsg

        end subroutine fetch_data_with_iostat_form



        module subroutine fetch_data_with_iostat_formatted ( handler, unit, iostat, iomsg )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer              , intent( in    ) :: unit
            integer              , intent( inout ) :: iostat
            character ( len= * ) , intent( inout ) :: iomsg

        end subroutine fetch_data_with_iostat_formatted



        module subroutine fetch_data_with_iostat_name ( handler, unit, iostat, iomsg )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer              , intent( in    ) :: unit
            integer              , intent( inout ) :: iostat
            character ( len= * ) , intent( inout ) :: iomsg

        end subroutine fetch_data_with_iostat_name



        module subroutine fetch_data_with_iostat_named ( handler, unit, iostat, iomsg )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer              , intent( in    ) :: unit
            integer              , intent( inout ) :: iostat
            character ( len= * ) , intent( inout ) :: iomsg

        end subroutine fetch_data_with_iostat_named



        module subroutine fetch_data_with_iostat_nextrec ( handler, unit, iostat, iomsg )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer              , intent( in    ) :: unit
            integer              , intent( inout ) :: iostat
            character ( len= * ) , intent( inout ) :: iomsg

        end subroutine fetch_data_with_iostat_nextrec



        module subroutine fetch_data_with_iostat_number ( handler, unit, iostat, iomsg )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer              , intent( in    ) :: unit
            integer              , intent( inout ) :: iostat
            character ( len= * ) , intent( inout ) :: iomsg

        end subroutine fetch_data_with_iostat_number



        module subroutine fetch_data_with_iostat_opened ( handler, unit, iostat, iomsg )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer              , intent( in    ) :: unit
            integer              , intent( inout ) :: iostat
            character ( len= * ) , intent( inout ) :: iomsg

        end subroutine fetch_data_with_iostat_opened



        module subroutine fetch_data_with_iostat_pad ( handler, unit, iostat, iomsg )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer              , intent( in    ) :: unit
            integer              , intent( inout ) :: iostat
            character ( len= * ) , intent( inout ) :: iomsg

        end subroutine fetch_data_with_iostat_pad



        module subroutine fetch_data_with_iostat_position ( handler, unit, iostat, iomsg )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer              , intent( in    ) :: unit
            integer              , intent( inout ) :: iostat
            character ( len= * ) , intent( inout ) :: iomsg

        end subroutine fetch_data_with_iostat_position



        module subroutine fetch_data_with_iostat_read ( handler, unit, iostat, iomsg )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer              , intent( in    ) :: unit
            integer              , intent( inout ) :: iostat
            character ( len= * ) , intent( inout ) :: iomsg

        end subroutine fetch_data_with_iostat_read



        module subroutine fetch_data_with_iostat_readwrite ( handler, unit, iostat, iomsg )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer              , intent( in    ) :: unit
            integer              , intent( inout ) :: iostat
            character ( len= * ) , intent( inout ) :: iomsg

        end subroutine fetch_data_with_iostat_readwrite



        module subroutine fetch_data_with_iostat_recl ( handler, unit, iostat, iomsg )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer              , intent( in    ) :: unit
            integer              , intent( inout ) :: iostat
            character ( len= * ) , intent( inout ) :: iomsg

        end subroutine fetch_data_with_iostat_recl



        module subroutine fetch_data_with_iostat_sequential ( handler, unit, iostat, iomsg )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer              , intent( in    ) :: unit
            integer              , intent( inout ) :: iostat
            character ( len= * ) , intent( inout ) :: iomsg

        end subroutine fetch_data_with_iostat_sequential



        module subroutine fetch_data_with_iostat_unformatted ( handler, unit, iostat, iomsg )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer              , intent( in    ) :: unit
            integer              , intent( inout ) :: iostat
            character ( len= * ) , intent( inout ) :: iomsg

        end subroutine fetch_data_with_iostat_unformatted



        module subroutine fetch_data_with_iostat_write ( handler, unit, iostat, iomsg )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer              , intent( in    ) :: unit
            integer              , intent( inout ) :: iostat
            character ( len= * ) , intent( inout ) :: iomsg

        end subroutine fetch_data_with_iostat_write



        module subroutine fetch_data_without_iostat_access ( handler, unit )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer , intent(in) :: unit

        end subroutine fetch_data_without_iostat_access



        module subroutine fetch_data_without_iostat_action ( handler, unit )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer , intent(in) :: unit

        end subroutine fetch_data_without_iostat_action



        module subroutine fetch_data_without_iostat_all ( handler, unit )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer , intent(in) :: unit

        end subroutine fetch_data_without_iostat_all



        module subroutine fetch_data_without_iostat_blank ( handler, unit )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer , intent(in) :: unit

        end subroutine fetch_data_without_iostat_blank



        module subroutine fetch_data_without_iostat_delim ( handler, unit )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer , intent(in) :: unit

        end subroutine fetch_data_without_iostat_delim



        module subroutine fetch_data_without_iostat_direct ( handler, unit )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer , intent(in) :: unit

        end subroutine fetch_data_without_iostat_direct



        module subroutine fetch_data_without_iostat_exist ( handler, unit )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer , intent(in) :: unit

        end subroutine fetch_data_without_iostat_exist



        module subroutine fetch_data_without_iostat_form ( handler, unit )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer , intent(in) :: unit

        end subroutine fetch_data_without_iostat_form



        module subroutine fetch_data_without_iostat_formatted ( handler, unit )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer , intent(in) :: unit

        end subroutine fetch_data_without_iostat_formatted



        module subroutine fetch_data_without_iostat_name ( handler, unit )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer , intent(in) :: unit

        end subroutine fetch_data_without_iostat_name



        module subroutine fetch_data_without_iostat_named ( handler, unit )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer , intent(in) :: unit

        end subroutine fetch_data_without_iostat_named



        module subroutine fetch_data_without_iostat_nextrec ( handler, unit )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer , intent(in) :: unit

        end subroutine fetch_data_without_iostat_nextrec



        module subroutine fetch_data_without_iostat_number ( handler, unit )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer , intent(in) :: unit

        end subroutine fetch_data_without_iostat_number



        module subroutine fetch_data_without_iostat_opened ( handler, unit )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer , intent(in) :: unit

        end subroutine fetch_data_without_iostat_opened



        module subroutine fetch_data_without_iostat_pad ( handler, unit )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer , intent(in) :: unit

        end subroutine fetch_data_without_iostat_pad



        module subroutine fetch_data_without_iostat_position ( handler, unit )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer , intent(in) :: unit

        end subroutine fetch_data_without_iostat_position



        module subroutine fetch_data_without_iostat_read ( handler, unit )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer , intent(in) :: unit

        end subroutine fetch_data_without_iostat_read



        module subroutine fetch_data_without_iostat_readwrite ( handler, unit )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer , intent(in) :: unit

        end subroutine fetch_data_without_iostat_readwrite



        module subroutine fetch_data_without_iostat_recl ( handler, unit )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer , intent(in) :: unit

        end subroutine fetch_data_without_iostat_recl



        module subroutine fetch_data_without_iostat_sequential ( handler, unit )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer , intent(in) :: unit

        end subroutine fetch_data_without_iostat_sequential



        module subroutine fetch_data_without_iostat_unformatted ( handler, unit )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer , intent(in) :: unit

        end subroutine fetch_data_without_iostat_unformatted



        module subroutine fetch_data_without_iostat_write ( handler, unit )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer , intent(in) :: unit

        end subroutine fetch_data_without_iostat_write

    end interface ! fetch_data



    ! definition: user-defined INTERFACE
    interface ! init_field

        module subroutine init_field_access (handler)

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

        end subroutine init_field_access



        module subroutine init_field_action (handler)

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

        end subroutine init_field_action



        module subroutine init_field_all (handler)

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

        end subroutine init_field_all



        module subroutine init_field_blank (handler)

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

        end subroutine init_field_blank



        module subroutine init_field_delim (handler)

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

        end subroutine init_field_delim



        module subroutine init_field_direct (handler)

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

        end subroutine init_field_direct



        module subroutine init_field_exist (handler)

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

        end subroutine init_field_exist



        module subroutine init_field_form (handler)

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

        end subroutine init_field_form



        module subroutine init_field_formatted (handler)

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

        end subroutine init_field_formatted



        module subroutine init_field_name (handler)

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

        end subroutine init_field_name



        module subroutine init_field_named (handler)

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

        end subroutine init_field_named



        module subroutine init_field_nextrec (handler)

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

        end subroutine init_field_nextrec



        module subroutine init_field_number (handler)

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

        end subroutine init_field_number



        module subroutine init_field_opened (handler)

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

        end subroutine init_field_opened



        module subroutine init_field_pad (handler)

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

        end subroutine init_field_pad



        module subroutine init_field_position (handler)

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

        end subroutine init_field_position



        module subroutine init_field_read (handler)

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

        end subroutine init_field_read



        module subroutine init_field_readwrite (handler)

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

        end subroutine init_field_readwrite



        module subroutine init_field_recl (handler)

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

        end subroutine init_field_recl



        module subroutine init_field_sequential (handler)

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

        end subroutine init_field_sequential



        module subroutine init_field_unformatted (handler)

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

        end subroutine init_field_unformatted



        module subroutine init_field_write (handler)

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

        end subroutine init_field_write

    end interface ! init_field



    ! definition: user-defined INTERFACE
    interface ! write_data

        module subroutine write_data_with_iostat_access ( handler, unit, iostat, iomsg )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer              , intent( in    ) :: unit
            integer              , intent( inout ) :: iostat
            character ( len= * ) , intent( inout ) :: iomsg

        end subroutine write_data_with_iostat_access



        module subroutine write_data_with_iostat_action ( handler, unit, iostat, iomsg )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer              , intent( in    ) :: unit
            integer              , intent( inout ) :: iostat
            character ( len= * ) , intent( inout ) :: iomsg

        end subroutine write_data_with_iostat_action



        module subroutine write_data_with_iostat_all ( handler, unit, iostat, iomsg )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer              , intent( in    ) :: unit
            integer              , intent( inout ) :: iostat
            character ( len= * ) , intent( inout ) :: iomsg

        end subroutine write_data_with_iostat_all



        module subroutine write_data_with_iostat_blank ( handler, unit, iostat, iomsg )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer              , intent( in    ) :: unit
            integer              , intent( inout ) :: iostat
            character ( len= * ) , intent( inout ) :: iomsg

        end subroutine write_data_with_iostat_blank



        module subroutine write_data_with_iostat_delim ( handler, unit, iostat, iomsg )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer              , intent( in    ) :: unit
            integer              , intent( inout ) :: iostat
            character ( len= * ) , intent( inout ) :: iomsg

        end subroutine write_data_with_iostat_delim



        module subroutine write_data_with_iostat_direct ( handler, unit, iostat, iomsg )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer              , intent( in    ) :: unit
            integer              , intent( inout ) :: iostat
            character ( len= * ) , intent( inout ) :: iomsg

        end subroutine write_data_with_iostat_direct



        module subroutine write_data_with_iostat_exist ( handler, unit, iostat, iomsg )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer              , intent( in    ) :: unit
            integer              , intent( inout ) :: iostat
            character ( len= * ) , intent( inout ) :: iomsg

        end subroutine write_data_with_iostat_exist



        module subroutine write_data_with_iostat_form ( handler, unit, iostat, iomsg )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer              , intent( in    ) :: unit
            integer              , intent( inout ) :: iostat
            character ( len= * ) , intent( inout ) :: iomsg

        end subroutine write_data_with_iostat_form



        module subroutine write_data_with_iostat_formatted ( handler, unit, iostat, iomsg )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer              , intent( in    ) :: unit
            integer              , intent( inout ) :: iostat
            character ( len= * ) , intent( inout ) :: iomsg

        end subroutine write_data_with_iostat_formatted



        module subroutine write_data_with_iostat_name ( handler, unit, iostat, iomsg )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer              , intent( in    ) :: unit
            integer              , intent( inout ) :: iostat
            character ( len= * ) , intent( inout ) :: iomsg

        end subroutine write_data_with_iostat_name



        module subroutine write_data_with_iostat_named ( handler, unit, iostat, iomsg )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer              , intent( in    ) :: unit
            integer              , intent( inout ) :: iostat
            character ( len= * ) , intent( inout ) :: iomsg

        end subroutine write_data_with_iostat_named



        module subroutine write_data_with_iostat_nextrec ( handler, unit, iostat, iomsg )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer              , intent( in    ) :: unit
            integer              , intent( inout ) :: iostat
            character ( len= * ) , intent( inout ) :: iomsg

        end subroutine write_data_with_iostat_nextrec



        module subroutine write_data_with_iostat_number ( handler, unit, iostat, iomsg )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer              , intent( in    ) :: unit
            integer              , intent( inout ) :: iostat
            character ( len= * ) , intent( inout ) :: iomsg

        end subroutine write_data_with_iostat_number



        module subroutine write_data_with_iostat_opened ( handler, unit, iostat, iomsg )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer              , intent( in    ) :: unit
            integer              , intent( inout ) :: iostat
            character ( len= * ) , intent( inout ) :: iomsg

        end subroutine write_data_with_iostat_opened



        module subroutine write_data_with_iostat_pad ( handler, unit, iostat, iomsg )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer              , intent( in    ) :: unit
            integer              , intent( inout ) :: iostat
            character ( len= * ) , intent( inout ) :: iomsg

        end subroutine write_data_with_iostat_pad



        module subroutine write_data_with_iostat_position ( handler, unit, iostat, iomsg )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer              , intent( in    ) :: unit
            integer              , intent( inout ) :: iostat
            character ( len= * ) , intent( inout ) :: iomsg

        end subroutine write_data_with_iostat_position



        module subroutine write_data_with_iostat_read ( handler, unit, iostat, iomsg )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer              , intent( in    ) :: unit
            integer              , intent( inout ) :: iostat
            character ( len= * ) , intent( inout ) :: iomsg

        end subroutine write_data_with_iostat_read



        module subroutine write_data_with_iostat_readwrite ( handler, unit, iostat, iomsg )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer              , intent( in    ) :: unit
            integer              , intent( inout ) :: iostat
            character ( len= * ) , intent( inout ) :: iomsg

        end subroutine write_data_with_iostat_readwrite



        module subroutine write_data_with_iostat_recl ( handler, unit, iostat, iomsg )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer              , intent( in    ) :: unit
            integer              , intent( inout ) :: iostat
            character ( len= * ) , intent( inout ) :: iomsg

        end subroutine write_data_with_iostat_recl



        module subroutine write_data_with_iostat_sequential ( handler, unit, iostat, iomsg )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer              , intent( in    ) :: unit
            integer              , intent( inout ) :: iostat
            character ( len= * ) , intent( inout ) :: iomsg

        end subroutine write_data_with_iostat_sequential



        module subroutine write_data_with_iostat_unformatted ( handler, unit, iostat, iomsg )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer              , intent( in    ) :: unit
            integer              , intent( inout ) :: iostat
            character ( len= * ) , intent( inout ) :: iomsg

        end subroutine write_data_with_iostat_unformatted



        module subroutine write_data_with_iostat_write ( handler, unit, iostat, iomsg )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer              , intent( in    ) :: unit
            integer              , intent( inout ) :: iostat
            character ( len= * ) , intent( inout ) :: iomsg

        end subroutine write_data_with_iostat_write



        module subroutine write_data_without_iostat_access ( handler, unit )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer , intent(in) :: unit

        end subroutine write_data_without_iostat_access



        module subroutine write_data_without_iostat_action ( handler, unit )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer , intent(in) :: unit

        end subroutine write_data_without_iostat_action



        module subroutine write_data_without_iostat_all ( handler, unit )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer , intent(in) :: unit

        end subroutine write_data_without_iostat_all



        module subroutine write_data_without_iostat_blank ( handler, unit )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer , intent(in) :: unit

        end subroutine write_data_without_iostat_blank



        module subroutine write_data_without_iostat_delim ( handler, unit )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer , intent(in) :: unit

        end subroutine write_data_without_iostat_delim



        module subroutine write_data_without_iostat_direct ( handler, unit )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer , intent(in) :: unit

        end subroutine write_data_without_iostat_direct



        module subroutine write_data_without_iostat_exist ( handler, unit )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer , intent(in) :: unit

        end subroutine write_data_without_iostat_exist



        module subroutine write_data_without_iostat_form ( handler, unit )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer , intent(in) :: unit

        end subroutine write_data_without_iostat_form



        module subroutine write_data_without_iostat_formatted ( handler, unit )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer , intent(in) :: unit

        end subroutine write_data_without_iostat_formatted



        module subroutine write_data_without_iostat_name ( handler, unit )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer , intent(in) :: unit

        end subroutine write_data_without_iostat_name



        module subroutine write_data_without_iostat_named ( handler, unit )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer , intent(in) :: unit

        end subroutine write_data_without_iostat_named



        module subroutine write_data_without_iostat_nextrec ( handler, unit )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer , intent(in) :: unit

        end subroutine write_data_without_iostat_nextrec



        module subroutine write_data_without_iostat_number ( handler, unit )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer , intent(in) :: unit

        end subroutine write_data_without_iostat_number



        module subroutine write_data_without_iostat_opened ( handler, unit )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer , intent(in) :: unit

        end subroutine write_data_without_iostat_opened



        module subroutine write_data_without_iostat_pad ( handler, unit )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer , intent(in) :: unit

        end subroutine write_data_without_iostat_pad



        module subroutine write_data_without_iostat_position ( handler, unit )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer , intent(in) :: unit

        end subroutine write_data_without_iostat_position



        module subroutine write_data_without_iostat_read ( handler, unit )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer , intent(in) :: unit

        end subroutine write_data_without_iostat_read



        module subroutine write_data_without_iostat_readwrite ( handler, unit )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer , intent(in) :: unit

        end subroutine write_data_without_iostat_readwrite



        module subroutine write_data_without_iostat_recl ( handler, unit )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer , intent(in) :: unit

        end subroutine write_data_without_iostat_recl



        module subroutine write_data_without_iostat_sequential ( handler, unit )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer , intent(in) :: unit

        end subroutine write_data_without_iostat_sequential



        module subroutine write_data_without_iostat_unformatted ( handler, unit )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer , intent(in) :: unit

        end subroutine write_data_without_iostat_unformatted



        module subroutine write_data_without_iostat_write ( handler, unit )

            ! argument(s) for this SUBROUTINE
            class(type_inquire_statement_handler) , intent(inout) :: handler

            ! argument(s) for this SUBROUTINE
            integer , intent(in) :: unit

        end subroutine write_data_without_iostat_write

    end interface ! write_data



    ! definition: user-defined INTERFACE
    interface

        module pure elemental function invalidity_iostat (iostat) result(invalidity)

            ! argument(s) for this FUNCTION
            integer , intent(in) :: iostat

            ! return value of this FUNCTION
            logical :: invalidity

        end function invalidity_iostat

    end interface

end module simple_inquire_statement_handler



submodule (simple_inquire_statement_handler) imp_fetch_data

    ! require all variables to be explicitly declared
    implicit none

    contains

    module procedure fetch_data_with_iostat_access         ; call handler%init_field_access      ; inquire( unit = unit , access      = handler%access      , iostat = iostat , iomsg = iomsg ) ; end procedure
    module procedure fetch_data_with_iostat_action         ; call handler%init_field_action      ; inquire( unit = unit , action      = handler%action      , iostat = iostat , iomsg = iomsg ) ; end procedure
    module procedure fetch_data_with_iostat_blank          ; call handler%init_field_blank       ; inquire( unit = unit , blank       = handler%blank       , iostat = iostat , iomsg = iomsg ) ; end procedure
    module procedure fetch_data_with_iostat_delim          ; call handler%init_field_delim       ; inquire( unit = unit , delim       = handler%delim       , iostat = iostat , iomsg = iomsg ) ; end procedure
    module procedure fetch_data_with_iostat_direct         ; call handler%init_field_direct      ; inquire( unit = unit , direct      = handler%direct      , iostat = iostat , iomsg = iomsg ) ; end procedure
    module procedure fetch_data_with_iostat_exist          ; call handler%init_field_exist       ; inquire( unit = unit , exist       = handler%exist       , iostat = iostat , iomsg = iomsg ) ; end procedure
    module procedure fetch_data_with_iostat_form           ; call handler%init_field_form        ; inquire( unit = unit , form        = handler%form        , iostat = iostat , iomsg = iomsg ) ; end procedure
    module procedure fetch_data_with_iostat_formatted      ; call handler%init_field_formatted   ; inquire( unit = unit , formatted   = handler%formatted   , iostat = iostat , iomsg = iomsg ) ; end procedure
    module procedure fetch_data_with_iostat_name           ; call handler%init_field_name        ; inquire( unit = unit , name        = handler%name        , iostat = iostat , iomsg = iomsg ) ; end procedure
    module procedure fetch_data_with_iostat_named          ; call handler%init_field_named       ; inquire( unit = unit , named       = handler%named       , iostat = iostat , iomsg = iomsg ) ; end procedure
    module procedure fetch_data_with_iostat_nextrec        ; call handler%init_field_nextrec     ; inquire( unit = unit , nextrec     = handler%nextrec     , iostat = iostat , iomsg = iomsg ) ; end procedure
    module procedure fetch_data_with_iostat_number         ; call handler%init_field_number      ; inquire( unit = unit , number      = handler%number      , iostat = iostat , iomsg = iomsg ) ; end procedure
    module procedure fetch_data_with_iostat_opened         ; call handler%init_field_opened      ; inquire( unit = unit , opened      = handler%opened      , iostat = iostat , iomsg = iomsg ) ; end procedure
    module procedure fetch_data_with_iostat_pad            ; call handler%init_field_pad         ; inquire( unit = unit , pad         = handler%pad         , iostat = iostat , iomsg = iomsg ) ; end procedure
    module procedure fetch_data_with_iostat_position       ; call handler%init_field_position    ; inquire( unit = unit , position    = handler%position    , iostat = iostat , iomsg = iomsg ) ; end procedure
    module procedure fetch_data_with_iostat_read           ; call handler%init_field_read        ; inquire( unit = unit , read        = handler%read        , iostat = iostat , iomsg = iomsg ) ; end procedure
    module procedure fetch_data_with_iostat_readwrite      ; call handler%init_field_readwrite   ; inquire( unit = unit , readwrite   = handler%readwrite   , iostat = iostat , iomsg = iomsg ) ; end procedure
    module procedure fetch_data_with_iostat_recl           ; call handler%init_field_recl        ; inquire( unit = unit , recl        = handler%recl        , iostat = iostat , iomsg = iomsg ) ; end procedure
    module procedure fetch_data_with_iostat_sequential     ; call handler%init_field_sequential  ; inquire( unit = unit , sequential  = handler%sequential  , iostat = iostat , iomsg = iomsg ) ; end procedure
    module procedure fetch_data_with_iostat_unformatted    ; call handler%init_field_unformatted ; inquire( unit = unit , unformatted = handler%unformatted , iostat = iostat , iomsg = iomsg ) ; end procedure
    module procedure fetch_data_with_iostat_write          ; call handler%init_field_write       ; inquire( unit = unit , write       = handler%write       , iostat = iostat , iomsg = iomsg ) ; end procedure

    module procedure fetch_data_without_iostat_access      ; call handler%init_field_access      ; inquire( unit = unit , access      = handler%access      ) ; end procedure
    module procedure fetch_data_without_iostat_action      ; call handler%init_field_action      ; inquire( unit = unit , action      = handler%action      ) ; end procedure
    module procedure fetch_data_without_iostat_blank       ; call handler%init_field_blank       ; inquire( unit = unit , blank       = handler%blank       ) ; end procedure
    module procedure fetch_data_without_iostat_delim       ; call handler%init_field_delim       ; inquire( unit = unit , delim       = handler%delim       ) ; end procedure
    module procedure fetch_data_without_iostat_direct      ; call handler%init_field_direct      ; inquire( unit = unit , direct      = handler%direct      ) ; end procedure
    module procedure fetch_data_without_iostat_exist       ; call handler%init_field_exist       ; inquire( unit = unit , exist       = handler%exist       ) ; end procedure
    module procedure fetch_data_without_iostat_form        ; call handler%init_field_form        ; inquire( unit = unit , form        = handler%form        ) ; end procedure
    module procedure fetch_data_without_iostat_formatted   ; call handler%init_field_formatted   ; inquire( unit = unit , formatted   = handler%formatted   ) ; end procedure
    module procedure fetch_data_without_iostat_name        ; call handler%init_field_name        ; inquire( unit = unit , name        = handler%name        ) ; end procedure
    module procedure fetch_data_without_iostat_named       ; call handler%init_field_named       ; inquire( unit = unit , named       = handler%named       ) ; end procedure
    module procedure fetch_data_without_iostat_nextrec     ; call handler%init_field_nextrec     ; inquire( unit = unit , nextrec     = handler%nextrec     ) ; end procedure
    module procedure fetch_data_without_iostat_number      ; call handler%init_field_number      ; inquire( unit = unit , number      = handler%number      ) ; end procedure
    module procedure fetch_data_without_iostat_opened      ; call handler%init_field_opened      ; inquire( unit = unit , opened      = handler%opened      ) ; end procedure
    module procedure fetch_data_without_iostat_pad         ; call handler%init_field_pad         ; inquire( unit = unit , pad         = handler%pad         ) ; end procedure
    module procedure fetch_data_without_iostat_position    ; call handler%init_field_position    ; inquire( unit = unit , position    = handler%position    ) ; end procedure
    module procedure fetch_data_without_iostat_read        ; call handler%init_field_read        ; inquire( unit = unit , read        = handler%read        ) ; end procedure
    module procedure fetch_data_without_iostat_readwrite   ; call handler%init_field_readwrite   ; inquire( unit = unit , readwrite   = handler%readwrite   ) ; end procedure
    module procedure fetch_data_without_iostat_recl        ; call handler%init_field_recl        ; inquire( unit = unit , recl        = handler%recl        ) ; end procedure
    module procedure fetch_data_without_iostat_sequential  ; call handler%init_field_sequential  ; inquire( unit = unit , sequential  = handler%sequential  ) ; end procedure
    module procedure fetch_data_without_iostat_unformatted ; call handler%init_field_unformatted ; inquire( unit = unit , unformatted = handler%unformatted ) ; end procedure
    module procedure fetch_data_without_iostat_write       ; call handler%init_field_write       ; inquire( unit = unit , write       = handler%write       ) ; end procedure



    module procedure fetch_data_with_iostat_all

        call handler%fetch_data_access      ( unit= unit, iostat= iostat, iomsg= iomsg ) ; if ( invalidity_iostat(iostat) ) return
        call handler%fetch_data_action      ( unit= unit, iostat= iostat, iomsg= iomsg ) ; if ( invalidity_iostat(iostat) ) return
        call handler%fetch_data_blank       ( unit= unit, iostat= iostat, iomsg= iomsg ) ; if ( invalidity_iostat(iostat) ) return
        call handler%fetch_data_delim       ( unit= unit, iostat= iostat, iomsg= iomsg ) ; if ( invalidity_iostat(iostat) ) return
        call handler%fetch_data_direct      ( unit= unit, iostat= iostat, iomsg= iomsg ) ; if ( invalidity_iostat(iostat) ) return
        call handler%fetch_data_exist       ( unit= unit, iostat= iostat, iomsg= iomsg ) ; if ( invalidity_iostat(iostat) ) return
        call handler%fetch_data_form        ( unit= unit, iostat= iostat, iomsg= iomsg ) ; if ( invalidity_iostat(iostat) ) return
        call handler%fetch_data_formatted   ( unit= unit, iostat= iostat, iomsg= iomsg ) ; if ( invalidity_iostat(iostat) ) return
        call handler%fetch_data_name        ( unit= unit, iostat= iostat, iomsg= iomsg ) ; if ( invalidity_iostat(iostat) ) return
        call handler%fetch_data_named       ( unit= unit, iostat= iostat, iomsg= iomsg ) ; if ( invalidity_iostat(iostat) ) return
        call handler%fetch_data_nextrec     ( unit= unit, iostat= iostat, iomsg= iomsg ) ; if ( invalidity_iostat(iostat) ) return
        call handler%fetch_data_number      ( unit= unit, iostat= iostat, iomsg= iomsg ) ; if ( invalidity_iostat(iostat) ) return
        call handler%fetch_data_opened      ( unit= unit, iostat= iostat, iomsg= iomsg ) ; if ( invalidity_iostat(iostat) ) return
        call handler%fetch_data_pad         ( unit= unit, iostat= iostat, iomsg= iomsg ) ; if ( invalidity_iostat(iostat) ) return
        call handler%fetch_data_position    ( unit= unit, iostat= iostat, iomsg= iomsg ) ; if ( invalidity_iostat(iostat) ) return
        call handler%fetch_data_read        ( unit= unit, iostat= iostat, iomsg= iomsg ) ; if ( invalidity_iostat(iostat) ) return
        call handler%fetch_data_readwrite   ( unit= unit, iostat= iostat, iomsg= iomsg ) ; if ( invalidity_iostat(iostat) ) return
        call handler%fetch_data_recl        ( unit= unit, iostat= iostat, iomsg= iomsg ) ; if ( invalidity_iostat(iostat) ) return
        call handler%fetch_data_sequential  ( unit= unit, iostat= iostat, iomsg= iomsg ) ; if ( invalidity_iostat(iostat) ) return
        call handler%fetch_data_unformatted ( unit= unit, iostat= iostat, iomsg= iomsg ) ; if ( invalidity_iostat(iostat) ) return
        call handler%fetch_data_write       ( unit= unit, iostat= iostat, iomsg= iomsg ) ; if ( invalidity_iostat(iostat) ) return

    end procedure fetch_data_with_iostat_all



    module procedure fetch_data_without_iostat_all

        call handler%fetch_data_access      ( unit )
        call handler%fetch_data_action      ( unit )
        call handler%fetch_data_blank       ( unit )
        call handler%fetch_data_delim       ( unit )
        call handler%fetch_data_direct      ( unit )
        call handler%fetch_data_exist       ( unit )
        call handler%fetch_data_form        ( unit )
        call handler%fetch_data_formatted   ( unit )
        call handler%fetch_data_name        ( unit )
        call handler%fetch_data_named       ( unit )
        call handler%fetch_data_nextrec     ( unit )
        call handler%fetch_data_number      ( unit )
        call handler%fetch_data_opened      ( unit )
        call handler%fetch_data_pad         ( unit )
        call handler%fetch_data_position    ( unit )
        call handler%fetch_data_read        ( unit )
        call handler%fetch_data_readwrite   ( unit )
        call handler%fetch_data_recl        ( unit )
        call handler%fetch_data_sequential  ( unit )
        call handler%fetch_data_unformatted ( unit )
        call handler%fetch_data_write       ( unit )

    end procedure fetch_data_without_iostat_all

end submodule imp_fetch_data



submodule (simple_inquire_statement_handler) imp_init_field

    ! require all variables to be explicitly declared
    implicit none

    contains

    module procedure init_field_access      ; handler%access      = DEFAULT_STR_FIELD_ACCESS      ; end procedure
    module procedure init_field_action      ; handler%action      = DEFAULT_STR_FIELD_ACTION      ; end procedure
    module procedure init_field_blank       ; handler%blank       = DEFAULT_STR_FIELD_BLANK       ; end procedure
    module procedure init_field_delim       ; handler%delim       = DEFAULT_STR_FIELD_DELIM       ; end procedure
    module procedure init_field_direct      ; handler%direct      = DEFAULT_STR_FIELD_DIRECT      ; end procedure
    module procedure init_field_exist       ; handler%exist       = DEFAULT_LGC_FIELD_EXIST       ; end procedure
    module procedure init_field_form        ; handler%form        = DEFAULT_STR_FIELD_FORM        ; end procedure
    module procedure init_field_formatted   ; handler%formatted   = DEFAULT_STR_FIELD_FORMATTED   ; end procedure
    module procedure init_field_name        ; handler%name        = DEFAULT_STR_FIELD_NAME        ; end procedure
    module procedure init_field_named       ; handler%named       = DEFAULT_LGC_FIELD_NAMED       ; end procedure
    module procedure init_field_nextrec     ; handler%nextrec     = DEFAULT_INT_FIELD_NEXTREC     ; end procedure
    module procedure init_field_number      ; handler%number      = DEFAULT_INT_FIELD_NUMBER      ; end procedure
    module procedure init_field_opened      ; handler%opened      = DEFAULT_LGC_FIELD_OPENED      ; end procedure
    module procedure init_field_pad         ; handler%pad         = DEFAULT_STR_FIELD_PAD         ; end procedure
    module procedure init_field_position    ; handler%position    = DEFAULT_STR_FIELD_POSITION    ; end procedure
    module procedure init_field_read        ; handler%read        = DEFAULT_STR_FIELD_READ        ; end procedure
    module procedure init_field_readwrite   ; handler%readwrite   = DEFAULT_STR_FIELD_READWRITE   ; end procedure
    module procedure init_field_sequential  ; handler%sequential  = DEFAULT_STR_FIELD_SEQUENTIAL  ; end procedure
    module procedure init_field_unformatted ; handler%unformatted = DEFAULT_STR_FIELD_UNFORMATTED ; end procedure
    module procedure init_field_recl        ; handler%recl        = DEFAULT_INT_FIELD_RECL        ; end procedure
    module procedure init_field_write       ; handler%write       = DEFAULT_STR_FIELD_WRITE       ; end procedure



    module procedure init_field_all

        call handler%init_field_access
        call handler%init_field_action
        call handler%init_field_blank
        call handler%init_field_delim
        call handler%init_field_direct
        call handler%init_field_exist
        call handler%init_field_form
        call handler%init_field_formatted
        call handler%init_field_name
        call handler%init_field_named
        call handler%init_field_nextrec
        call handler%init_field_number
        call handler%init_field_opened
        call handler%init_field_pad
        call handler%init_field_position
        call handler%init_field_read
        call handler%init_field_readwrite
        call handler%init_field_recl
        call handler%init_field_sequential
        call handler%init_field_unformatted
        call handler%init_field_write

    end procedure init_field_all

end submodule imp_init_field



submodule (simple_inquire_statement_handler) imp_write_data

    ! require all variables to be explicitly declared
    implicit none

    contains

    module procedure write_data_with_iostat_access         ; write( unit = unit , fmt = FMT_WRITE_STR , iostat = iostat , iomsg = iomsg ) trim( FIELD_NAME_ACCESS      ) , trim( handler%access      ) ; end procedure
    module procedure write_data_with_iostat_action         ; write( unit = unit , fmt = FMT_WRITE_STR , iostat = iostat , iomsg = iomsg ) trim( FIELD_NAME_ACTION      ) , trim( handler%action      ) ; end procedure
    module procedure write_data_with_iostat_blank          ; write( unit = unit , fmt = FMT_WRITE_STR , iostat = iostat , iomsg = iomsg ) trim( FIELD_NAME_BLANK       ) , trim( handler%blank       ) ; end procedure
    module procedure write_data_with_iostat_delim          ; write( unit = unit , fmt = FMT_WRITE_STR , iostat = iostat , iomsg = iomsg ) trim( FIELD_NAME_DELIM       ) , trim( handler%delim       ) ; end procedure
    module procedure write_data_with_iostat_direct         ; write( unit = unit , fmt = FMT_WRITE_STR , iostat = iostat , iomsg = iomsg ) trim( FIELD_NAME_DIRECT      ) , trim( handler%direct      ) ; end procedure
    module procedure write_data_with_iostat_exist          ; write( unit = unit , fmt = FMT_WRITE_LGC , iostat = iostat , iomsg = iomsg ) trim( FIELD_NAME_EXIST       ) ,       handler%exist         ; end procedure
    module procedure write_data_with_iostat_form           ; write( unit = unit , fmt = FMT_WRITE_STR , iostat = iostat , iomsg = iomsg ) trim( FIELD_NAME_FORM        ) , trim( handler%form        ) ; end procedure
    module procedure write_data_with_iostat_formatted      ; write( unit = unit , fmt = FMT_WRITE_STR , iostat = iostat , iomsg = iomsg ) trim( FIELD_NAME_FORMATTED   ) , trim( handler%formatted   ) ; end procedure
    module procedure write_data_with_iostat_name           ; write( unit = unit , fmt = FMT_WRITE_STR , iostat = iostat , iomsg = iomsg ) trim( FIELD_NAME_NAME        ) , trim( handler%name        ) ; end procedure
    module procedure write_data_with_iostat_named          ; write( unit = unit , fmt = FMT_WRITE_LGC , iostat = iostat , iomsg = iomsg ) trim( FIELD_NAME_NAMED       ) ,       handler%named         ; end procedure
    module procedure write_data_with_iostat_nextrec        ; write( unit = unit , fmt = FMT_WRITE_INT , iostat = iostat , iomsg = iomsg ) trim( FIELD_NAME_NEXTREC     ) ,       handler%nextrec       ; end procedure
    module procedure write_data_with_iostat_number         ; write( unit = unit , fmt = FMT_WRITE_INT , iostat = iostat , iomsg = iomsg ) trim( FIELD_NAME_NUMBER      ) ,       handler%number        ; end procedure
    module procedure write_data_with_iostat_opened         ; write( unit = unit , fmt = FMT_WRITE_LGC , iostat = iostat , iomsg = iomsg ) trim( FIELD_NAME_OPENED      ) ,       handler%opened        ; end procedure
    module procedure write_data_with_iostat_pad            ; write( unit = unit , fmt = FMT_WRITE_STR , iostat = iostat , iomsg = iomsg ) trim( FIELD_NAME_PAD         ) , trim( handler%pad         ) ; end procedure
    module procedure write_data_with_iostat_position       ; write( unit = unit , fmt = FMT_WRITE_STR , iostat = iostat , iomsg = iomsg ) trim( FIELD_NAME_POSITION    ) , trim( handler%position    ) ; end procedure
    module procedure write_data_with_iostat_read           ; write( unit = unit , fmt = FMT_WRITE_STR , iostat = iostat , iomsg = iomsg ) trim( FIELD_NAME_READ        ) , trim( handler%read        ) ; end procedure
    module procedure write_data_with_iostat_readwrite      ; write( unit = unit , fmt = FMT_WRITE_STR , iostat = iostat , iomsg = iomsg ) trim( FIELD_NAME_READWRITE   ) , trim( handler%readwrite   ) ; end procedure
    module procedure write_data_with_iostat_recl           ; write( unit = unit , fmt = FMT_WRITE_INT , iostat = iostat , iomsg = iomsg ) trim( FIELD_NAME_RECL        ) ,       handler%recl          ; end procedure
    module procedure write_data_with_iostat_sequential     ; write( unit = unit , fmt = FMT_WRITE_STR , iostat = iostat , iomsg = iomsg ) trim( FIELD_NAME_SEQUENTIAL  ) , trim( handler%sequential  ) ; end procedure
    module procedure write_data_with_iostat_unformatted    ; write( unit = unit , fmt = FMT_WRITE_STR , iostat = iostat , iomsg = iomsg ) trim( FIELD_NAME_UNFORMATTED ) , trim( handler%unformatted ) ; end procedure
    module procedure write_data_with_iostat_write          ; write( unit = unit , fmt = FMT_WRITE_STR , iostat = iostat , iomsg = iomsg ) trim( FIELD_NAME_WRITE       ) , trim( handler%write       ) ; end procedure

    module procedure write_data_without_iostat_access      ; write( unit = unit , fmt = FMT_WRITE_STR )                                   trim( FIELD_NAME_ACCESS      ) , trim( handler%access      ) ; end procedure
    module procedure write_data_without_iostat_action      ; write( unit = unit , fmt = FMT_WRITE_STR )                                   trim( FIELD_NAME_ACTION      ) , trim( handler%action      ) ; end procedure
    module procedure write_data_without_iostat_blank       ; write( unit = unit , fmt = FMT_WRITE_STR )                                   trim( FIELD_NAME_BLANK       ) , trim( handler%blank       ) ; end procedure
    module procedure write_data_without_iostat_delim       ; write( unit = unit , fmt = FMT_WRITE_STR )                                   trim( FIELD_NAME_DELIM       ) , trim( handler%delim       ) ; end procedure
    module procedure write_data_without_iostat_direct      ; write( unit = unit , fmt = FMT_WRITE_STR )                                   trim( FIELD_NAME_DIRECT      ) , trim( handler%direct      ) ; end procedure
    module procedure write_data_without_iostat_exist       ; write( unit = unit , fmt = FMT_WRITE_LGC )                                   trim( FIELD_NAME_EXIST       ) ,       handler%exist         ; end procedure
    module procedure write_data_without_iostat_form        ; write( unit = unit , fmt = FMT_WRITE_STR )                                   trim( FIELD_NAME_FORM        ) , trim( handler%form        ) ; end procedure
    module procedure write_data_without_iostat_formatted   ; write( unit = unit , fmt = FMT_WRITE_STR )                                   trim( FIELD_NAME_FORMATTED   ) , trim( handler%formatted   ) ; end procedure
    module procedure write_data_without_iostat_name        ; write( unit = unit , fmt = FMT_WRITE_STR )                                   trim( FIELD_NAME_NAME        ) , trim( handler%name        ) ; end procedure
    module procedure write_data_without_iostat_named       ; write( unit = unit , fmt = FMT_WRITE_LGC )                                   trim( FIELD_NAME_NAMED       ) ,       handler%named         ; end procedure
    module procedure write_data_without_iostat_nextrec     ; write( unit = unit , fmt = FMT_WRITE_INT )                                   trim( FIELD_NAME_NEXTREC     ) ,       handler%nextrec       ; end procedure
    module procedure write_data_without_iostat_number      ; write( unit = unit , fmt = FMT_WRITE_INT )                                   trim( FIELD_NAME_NUMBER      ) ,       handler%number        ; end procedure
    module procedure write_data_without_iostat_opened      ; write( unit = unit , fmt = FMT_WRITE_LGC )                                   trim( FIELD_NAME_OPENED      ) ,       handler%opened        ; end procedure
    module procedure write_data_without_iostat_pad         ; write( unit = unit , fmt = FMT_WRITE_STR )                                   trim( FIELD_NAME_PAD         ) , trim( handler%pad         ) ; end procedure
    module procedure write_data_without_iostat_position    ; write( unit = unit , fmt = FMT_WRITE_STR )                                   trim( FIELD_NAME_POSITION    ) , trim( handler%position    ) ; end procedure
    module procedure write_data_without_iostat_read        ; write( unit = unit , fmt = FMT_WRITE_STR )                                   trim( FIELD_NAME_READ        ) , trim( handler%read        ) ; end procedure
    module procedure write_data_without_iostat_readwrite   ; write( unit = unit , fmt = FMT_WRITE_STR )                                   trim( FIELD_NAME_READWRITE   ) , trim( handler%readwrite   ) ; end procedure
    module procedure write_data_without_iostat_recl        ; write( unit = unit , fmt = FMT_WRITE_INT )                                   trim( FIELD_NAME_RECL        ) ,       handler%recl          ; end procedure
    module procedure write_data_without_iostat_sequential  ; write( unit = unit , fmt = FMT_WRITE_STR )                                   trim( FIELD_NAME_SEQUENTIAL  ) , trim( handler%sequential  ) ; end procedure
    module procedure write_data_without_iostat_unformatted ; write( unit = unit , fmt = FMT_WRITE_STR )                                   trim( FIELD_NAME_UNFORMATTED ) , trim( handler%unformatted ) ; end procedure
    module procedure write_data_without_iostat_write       ; write( unit = unit , fmt = FMT_WRITE_STR )                                   trim( FIELD_NAME_WRITE       ) , trim( handler%write       ) ; end procedure



    module procedure write_data_with_iostat_all

        write( unit = unit , fmt = FMT_WRITE_STR , iostat = iostat , iomsg = iomsg ) FIELD_NAME_ACCESS      , trim( handler%access      ) ; if ( invalidity_iostat(iostat) ) return
        write( unit = unit , fmt = FMT_WRITE_STR , iostat = iostat , iomsg = iomsg ) FIELD_NAME_ACTION      , trim( handler%action      ) ; if ( invalidity_iostat(iostat) ) return
        write( unit = unit , fmt = FMT_WRITE_STR , iostat = iostat , iomsg = iomsg ) FIELD_NAME_BLANK       , trim( handler%blank       ) ; if ( invalidity_iostat(iostat) ) return
        write( unit = unit , fmt = FMT_WRITE_STR , iostat = iostat , iomsg = iomsg ) FIELD_NAME_DELIM       , trim( handler%delim       ) ; if ( invalidity_iostat(iostat) ) return
        write( unit = unit , fmt = FMT_WRITE_STR , iostat = iostat , iomsg = iomsg ) FIELD_NAME_DIRECT      , trim( handler%direct      ) ; if ( invalidity_iostat(iostat) ) return
        write( unit = unit , fmt = FMT_WRITE_LGC , iostat = iostat , iomsg = iomsg ) FIELD_NAME_EXIST       ,       handler%exist         ; if ( invalidity_iostat(iostat) ) return
        write( unit = unit , fmt = FMT_WRITE_STR , iostat = iostat , iomsg = iomsg ) FIELD_NAME_FORM        , trim( handler%form        ) ; if ( invalidity_iostat(iostat) ) return
        write( unit = unit , fmt = FMT_WRITE_STR , iostat = iostat , iomsg = iomsg ) FIELD_NAME_FORMATTED   , trim( handler%formatted   ) ; if ( invalidity_iostat(iostat) ) return
        write( unit = unit , fmt = FMT_WRITE_STR , iostat = iostat , iomsg = iomsg ) FIELD_NAME_NAME        , trim( handler%name        ) ; if ( invalidity_iostat(iostat) ) return
        write( unit = unit , fmt = FMT_WRITE_LGC , iostat = iostat , iomsg = iomsg ) FIELD_NAME_NAMED       ,       handler%named         ; if ( invalidity_iostat(iostat) ) return
        write( unit = unit , fmt = FMT_WRITE_INT , iostat = iostat , iomsg = iomsg ) FIELD_NAME_NEXTREC     ,       handler%nextrec       ; if ( invalidity_iostat(iostat) ) return
        write( unit = unit , fmt = FMT_WRITE_INT , iostat = iostat , iomsg = iomsg ) FIELD_NAME_NUMBER      ,       handler%number        ; if ( invalidity_iostat(iostat) ) return
        write( unit = unit , fmt = FMT_WRITE_LGC , iostat = iostat , iomsg = iomsg ) FIELD_NAME_OPENED      ,       handler%opened        ; if ( invalidity_iostat(iostat) ) return
        write( unit = unit , fmt = FMT_WRITE_STR , iostat = iostat , iomsg = iomsg ) FIELD_NAME_PAD         , trim( handler%pad         ) ; if ( invalidity_iostat(iostat) ) return
        write( unit = unit , fmt = FMT_WRITE_STR , iostat = iostat , iomsg = iomsg ) FIELD_NAME_POSITION    , trim( handler%position    ) ; if ( invalidity_iostat(iostat) ) return
        write( unit = unit , fmt = FMT_WRITE_STR , iostat = iostat , iomsg = iomsg ) FIELD_NAME_READ        , trim( handler%read        ) ; if ( invalidity_iostat(iostat) ) return
        write( unit = unit , fmt = FMT_WRITE_STR , iostat = iostat , iomsg = iomsg ) FIELD_NAME_READWRITE   , trim( handler%readwrite   ) ; if ( invalidity_iostat(iostat) ) return
        write( unit = unit , fmt = FMT_WRITE_INT , iostat = iostat , iomsg = iomsg ) FIELD_NAME_RECL        ,       handler%recl          ; if ( invalidity_iostat(iostat) ) return
        write( unit = unit , fmt = FMT_WRITE_STR , iostat = iostat , iomsg = iomsg ) FIELD_NAME_SEQUENTIAL  , trim( handler%sequential  ) ; if ( invalidity_iostat(iostat) ) return
        write( unit = unit , fmt = FMT_WRITE_STR , iostat = iostat , iomsg = iomsg ) FIELD_NAME_UNFORMATTED , trim( handler%unformatted ) ; if ( invalidity_iostat(iostat) ) return
        write( unit = unit , fmt = FMT_WRITE_STR , iostat = iostat , iomsg = iomsg ) FIELD_NAME_WRITE       , trim( handler%write       ) ; if ( invalidity_iostat(iostat) ) return

    end procedure write_data_with_iostat_all



    module procedure write_data_without_iostat_all

        write( unit = unit , fmt = FMT_WRITE_STR ) FIELD_NAME_ACCESS      , trim( handler%access      ) ;!
        write( unit = unit , fmt = FMT_WRITE_STR ) FIELD_NAME_ACTION      , trim( handler%action      ) ;!
        write( unit = unit , fmt = FMT_WRITE_STR ) FIELD_NAME_BLANK       , trim( handler%blank       ) ;!
        write( unit = unit , fmt = FMT_WRITE_STR ) FIELD_NAME_DELIM       , trim( handler%delim       ) ;!
        write( unit = unit , fmt = FMT_WRITE_STR ) FIELD_NAME_DIRECT      , trim( handler%direct      ) ;!
        write( unit = unit , fmt = FMT_WRITE_LGC ) FIELD_NAME_EXIST       ,       handler%exist         ;!
        write( unit = unit , fmt = FMT_WRITE_STR ) FIELD_NAME_FORM        , trim( handler%form        ) ;!
        write( unit = unit , fmt = FMT_WRITE_STR ) FIELD_NAME_FORMATTED   , trim( handler%formatted   ) ;!
        write( unit = unit , fmt = FMT_WRITE_STR ) FIELD_NAME_NAME        , trim( handler%name        ) ;!
        write( unit = unit , fmt = FMT_WRITE_LGC ) FIELD_NAME_NAMED       ,       handler%named         ;!
        write( unit = unit , fmt = FMT_WRITE_INT ) FIELD_NAME_NEXTREC     ,       handler%nextrec       ;!
        write( unit = unit , fmt = FMT_WRITE_INT ) FIELD_NAME_NUMBER      ,       handler%number        ;!
        write( unit = unit , fmt = FMT_WRITE_LGC ) FIELD_NAME_OPENED      ,       handler%opened        ;!
        write( unit = unit , fmt = FMT_WRITE_STR ) FIELD_NAME_PAD         , trim( handler%pad         ) ;!
        write( unit = unit , fmt = FMT_WRITE_STR ) FIELD_NAME_POSITION    , trim( handler%position    ) ;!
        write( unit = unit , fmt = FMT_WRITE_STR ) FIELD_NAME_READ        , trim( handler%read        ) ;!
        write( unit = unit , fmt = FMT_WRITE_STR ) FIELD_NAME_READWRITE   , trim( handler%readwrite   ) ;!
        write( unit = unit , fmt = FMT_WRITE_INT ) FIELD_NAME_RECL        ,       handler%recl          ;!
        write( unit = unit , fmt = FMT_WRITE_STR ) FIELD_NAME_SEQUENTIAL  , trim( handler%sequential  ) ;!
        write( unit = unit , fmt = FMT_WRITE_STR ) FIELD_NAME_UNFORMATTED , trim( handler%unformatted ) ;!
        write( unit = unit , fmt = FMT_WRITE_STR ) FIELD_NAME_WRITE       , trim( handler%write       ) ;!

    end procedure write_data_without_iostat_all

end submodule imp_write_data




submodule (simple_inquire_statement_handler) imp_utility

    ! require all variables to be explicitly declared
    implicit none

    contains

    module procedure invalidity_iostat

        invalidity = (iostat .ne. 0)

    end procedure invalidity_iostat

end submodule imp_utility
