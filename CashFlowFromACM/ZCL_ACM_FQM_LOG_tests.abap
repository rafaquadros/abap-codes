*"* use this source file for your ABAP unit test classes
**********************************************************************
* Test classes
**********************************************************************
CLASS lct_temporal_arguments DEFINITION FOR TESTING
                             DURATION SHORT
                             RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    DATA
        mr_cut TYPE REF TO zcl_acm_fqm_log.
    METHODS:
      setup,
      tst_greater_initial_date FOR TESTING,     "Initial date greater then final date
      tst_greater_initial_time_dti FOR TESTING, "Only initial date and initial time greater then final time
      tst_greater_initial_time_dtf FOR TESTING, "Only final date and initial time greater then final time
      tst_tmstp_range_dti FOR TESTING,          "Timestamp range for only initial date and period of time
      tst_tmstp_range_dtf FOR TESTING,          "Timestamp range for only final date and period of time
      tst_tmstp_range_both FOR TESTING.         "Timestamp range for both dates and period of time
ENDCLASS.

CLASS lct_temporal_arguments IMPLEMENTATION.
  METHOD setup.
    me->mr_cut = NEW zcl_acm_fqm_log(
      i_contract_number = '0000000001'
      i_event_type      = 'C'
*          i_guid            =
*          i_fail_indicator  =
    ).
  ENDMETHOD.

  METHOD tst_greater_initial_date.
    "Given a initial date greater then the final date
    DATA: l_initial_date TYPE dats VALUE '20210518',
          l_final_date   TYPE dats VALUE '15000422',
          l_initial_time TYPE tims VALUE '235959', "any time
          l_final_time   TYPE tims VALUE '000000'. "any time
    "When these time arguments are checked
    TRY.
        mr_cut->check_temporal_arguments(
          EXPORTING
            i_initial_date = l_initial_date
            i_final_date   = l_final_date
            i_initial_time = l_initial_time
            i_final_time   = l_final_time
*          IMPORTING
*            eg_moment      =
        ).
*        CATCH cx_bapi_error.
      CATCH cx_bapi_error INTO DATA(lx_error).
    ENDTRY.
    "Then message E012(ZFQM) must be found
    IF line_exists( lx_error->status[ id = 'ZFQM' type = 'E' number = 012 ] ).
      DATA(l_boolean) = abap_true.
    ENDIF.
    cl_abap_unit_assert=>assert_true(
      EXPORTING
        act              = l_boolean
*           msg              =
*           level            = if_abap_unit_constant=>severity-medium
*           quit             = if_abap_unit_constant=>quit-test
*         RECEIVING
*           assertion_failed =
    ).
  ENDMETHOD.

  METHOD tst_greater_initial_time_dtf.
    "Given a final date and a initial time greater then the final time
    DATA: l_initial_date TYPE dats VALUE '00000000',
          l_final_date   TYPE dats,
          l_initial_time TYPE tims VALUE '235959',
          l_final_time   TYPE tims VALUE '000001'.

    l_final_date = sy-datlo.
    "When these time arguments are checked
    TRY.
        mr_cut->check_temporal_arguments(
          EXPORTING
            i_initial_date = l_initial_date
            i_final_date   = l_final_date
            i_initial_time = l_initial_time
            i_final_time   = l_final_time
*          IMPORTING
*            eg_moment      =
        ).
*        CATCH cx_bapi_error.
      CATCH cx_bapi_error INTO DATA(lx_error).
    ENDTRY.
    "Then message E013(ZFQM) must be found
    IF line_exists( lx_error->status[ id = 'ZFQM' type = 'E' number = 013 ] ).
      DATA(l_boolean) = abap_true.
    ENDIF.
    cl_abap_unit_assert=>assert_true(
      EXPORTING
        act              = l_boolean
*           msg              =
*           level            = if_abap_unit_constant=>severity-medium
*           quit             = if_abap_unit_constant=>quit-test
*         RECEIVING
*           assertion_failed =
    ).

  ENDMETHOD.

  METHOD tst_greater_initial_time_dti.
    "Given a initial date and a initial time greater then the final time
    DATA: l_initial_date TYPE dats,
          l_final_date   TYPE dats VALUE '00000000',
          l_initial_time TYPE tims VALUE '235959',
          l_final_time   TYPE tims VALUE '000001'.

    l_initial_date = sy-datlo.
    "When these time arguments are checked
    TRY.
        mr_cut->check_temporal_arguments(
          EXPORTING
            i_initial_date = l_initial_date
            i_final_date   = l_final_date
            i_initial_time = l_initial_time
            i_final_time   = l_final_time
*          IMPORTING
*            eg_moment      =
        ).
*        CATCH cx_bapi_error.
      CATCH cx_bapi_error INTO DATA(lx_error).
    ENDTRY.
    "Then message E013(ZFQM) must be found
    IF line_exists( lx_error->status[ id = 'ZFQM' type = 'E' number = 013 ] ).
      DATA(l_boolean) = abap_true.
    ENDIF.
    cl_abap_unit_assert=>assert_true(
      EXPORTING
        act              = l_boolean
*           msg              =
*           level            = if_abap_unit_constant=>severity-medium
*           quit             = if_abap_unit_constant=>quit-test
*         RECEIVING
*           assertion_failed =
    ).
  ENDMETHOD.

  METHOD tst_tmstp_range_both.
    "Given valid initial date and final date
    DATA(l_final_date) = sy-datlo.
    DATA(l_initial_date) = CONV dats( l_final_date - 2 ).
    DATA(l_final_time)   = sy-timlo.
    DATA(l_initial_time) = CONV tims( l_final_time - 1 ).
    "When these time arguments are checked
    TRY.
        mr_cut->check_temporal_arguments(
          EXPORTING
            i_initial_date = l_initial_date
            i_final_date   = l_final_date
            i_initial_time = l_initial_time
            i_final_time   = l_final_time
          IMPORTING
            eg_moment      = DATA(lg_timestamp)
        ).
*        CATCH cx_bapi_error.
      CATCH cx_bapi_error.
        cl_abap_unit_assert=>fail(
*          EXPORTING
*            msg    =
*            level  = if_abap_unit_constant=>severity-medium
*            quit   = if_abap_unit_constant=>quit-test
*            detail =
        ).
    ENDTRY.
    "Then a date between the final and initial dates is found in the range
    DATA(l_test_date) = sy-datlo - 1.
    CONVERT DATE l_test_date TIME sy-timlo INTO TIME STAMP DATA(l_test_timestamp) TIME ZONE sy-zonlo.

    cl_abap_unit_assert=>assert_true(
      EXPORTING
        act              = COND #( WHEN l_test_timestamp IN lg_timestamp THEN abap_true ELSE abap_false )
*        msg              =
*        level            = if_abap_unit_constant=>severity-medium
*        quit             = if_abap_unit_constant=>quit-test
*      RECEIVING
*        assertion_failed =
    ).
  ENDMETHOD.

  METHOD tst_tmstp_range_dtf.
    "Given a valid final date
    DATA(l_final_date) = sy-datlo.
    DATA(l_initial_date) = CONV dats( 00000000 ).
    DATA(l_final_time)   = sy-timlo.
    DATA(l_initial_time) = CONV tims( l_final_time - 1 ).
    "When these time arguments are checked
    TRY.
        mr_cut->check_temporal_arguments(
          EXPORTING
            i_initial_date = l_initial_date
            i_final_date   = l_final_date
            i_initial_time = l_initial_time
            i_final_time   = l_final_time
          IMPORTING
            eg_moment      = DATA(lg_timestamp)
        ).
*        CATCH cx_bapi_error.
      CATCH cx_bapi_error.
        cl_abap_unit_assert=>fail(
*          EXPORTING
*            msg    =
*            level  = if_abap_unit_constant=>severity-medium
*            quit   = if_abap_unit_constant=>quit-test
*            detail =
        ).
    ENDTRY.
    "Then a date between the final and initial dates is found in the range
    DATA(l_test_date) = sy-datlo. SUBTRACT 1 FROM l_test_date.
    CONVERT DATE l_test_date TIME sy-timlo INTO TIME STAMP DATA(l_test_timestamp) TIME ZONE sy-zonlo.

    cl_abap_unit_assert=>assert_true(
      EXPORTING
        act              = COND #( WHEN l_test_timestamp IN lg_timestamp THEN abap_true ELSE abap_false )
*        msg              =
*        level            = if_abap_unit_constant=>severity-medium
*        quit             = if_abap_unit_constant=>quit-test
*      RECEIVING
*        assertion_failed =
    ).
  ENDMETHOD.

  METHOD tst_tmstp_range_dti.
    "Given a valid initial date
    DATA(l_final_date) = CONV dats( '00000000' ).
    DATA(l_initial_date) = sy-datlo.
    DATA(l_final_time)   = sy-timlo.
    DATA(l_initial_time) = CONV tims( l_final_time - 1 ).
    "When these time arguments are checked
    TRY.
        mr_cut->check_temporal_arguments(
          EXPORTING
            i_initial_date = l_initial_date
            i_final_date   = l_final_date
            i_initial_time = l_initial_time
            i_final_time   = l_final_time
          IMPORTING
            eg_moment      = DATA(lg_timestamp)
        ).
*        CATCH cx_bapi_error.
      CATCH cx_bapi_error.
        cl_abap_unit_assert=>fail(
*          EXPORTING
*            msg    =
*            level  = if_abap_unit_constant=>severity-medium
*            quit   = if_abap_unit_constant=>quit-test
*            detail =
        ).
    ENDTRY.
    "Then the current date is found in the range
    DATA(l_test_date) = sy-datlo.
    CONVERT DATE l_test_date TIME sy-timlo INTO TIME STAMP DATA(l_test_timestamp) TIME ZONE sy-zonlo.

    cl_abap_unit_assert=>assert_true(
      EXPORTING
        act              = COND #( WHEN l_test_timestamp IN lg_timestamp THEN abap_true ELSE abap_false )
*        msg              =
*        level            = if_abap_unit_constant=>severity-medium
*        quit             = if_abap_unit_constant=>quit-test
*      RECEIVING
*        assertion_failed =
    ).
  ENDMETHOD.

ENDCLASS.