*"* use this source file for your ABAP unit test classes
CLASS: lct_basic_tests       DEFINITION DEFERRED,
       lct_new_contract      DEFINITION DEFERRED,
       lct_existing_contract DEFINITION DEFERRED,
       zcl_acm_contract      DEFINITION LOCAL FRIENDS lct_basic_tests
                                                      lct_new_contract
                                                      lct_existing_contract.

**********************************************************************
* Help Classes
**********************************************************************
CLASS lcl_api_new_ctr DEFINITION INHERITING FROM lcl_api.
  PUBLIC SECTION.
    METHODS
      wb2_read_docu_header_single REDEFINITION.
ENDCLASS.

CLASS lcl_api_new_ctr IMPLEMENTATION.

  METHOD wb2_read_docu_header_single.

    RAISE EXCEPTION TYPE cx_bapi_error.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_api_exist_ctr DEFINITION INHERITING FROM lcl_api.
  PUBLIC SECTION.
    METHODS
      wb2_read_docu_header_single REDEFINITION.
ENDCLASS.


CLASS lcl_api_exist_ctr IMPLEMENTATION.

  METHOD wb2_read_docu_header_single.
    es_header_data-tkonn   = i_contract_number.
    es_header_data-tctyp   = 'ZCR1'.
  ENDMETHOD.

ENDCLASS.

**********************************************************************
* Test classes
**********************************************************************
CLASS lct_common DEFINITION ABSTRACT.
  PROTECTED SECTION.
    CONSTANTS:
      mc_cl         TYPE sibfcatid VALUE 'CL',
      mc_0000000001 TYPE tkonn VALUE '0000000001'.
    DATA
        mr_cut TYPE REF TO zcl_acm_contract.
ENDCLASS.

CLASS lct_basic_tests DEFINITION INHERITING FROM lct_common FOR TESTING
                      DURATION SHORT
                      RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS:
      setup,
      tst_find_by_lpor FOR TESTING,
      tst_lpor         FOR TESTING.
ENDCLASS.


CLASS lct_basic_tests IMPLEMENTATION.
  METHOD setup.

    mr_cut = NEW zcl_acm_contract( i_contract_number = mc_0000000001 ).
  ENDMETHOD.
  METHOD tst_find_by_lpor.
    DATA: ls_lpor TYPE sibflpor.
    "Given an contract number and the related Local Persistent Object Reference (LPOR)
    CLEAR mr_cut.
    ls_lpor-catid = mc_cl.
    ls_lpor-typeid = zcl_acm_contract=>mc_this_template.
    ls_lpor-instid = mc_0000000001.
    "When the related object instance is required
    me->mr_cut ?= zcl_acm_contract=>bi_persistent~find_by_lpor( lpor = ls_lpor ).
    "Then the respective contract number becomes the key attribute of the instance provided
    IF cl_abap_unit_assert=>assert_bound(
      EXPORTING
        act              = me->mr_cut
*        msg              =
*        level            = if_abap_unit_constant=>severity-medium
*        quit             = if_abap_unit_constant=>quit-test
*      RECEIVING
*        assertion_failed =
    ) = abap_true.
      cl_abap_unit_assert=>assert_equals(
        EXPORTING
          act                  = me->mr_cut->m_contract_number
          exp                  = mc_0000000001
*            ignore_hash_sequence = abap_false
*            tol                  =
*            msg                  =
*            level                = if_abap_unit_constant=>severity-medium
*            quit                 = if_abap_unit_constant=>quit-test
*          RECEIVING
*            assertion_failed     =
      ).
    ENDIF.
  ENDMETHOD.

  METHOD tst_lpor.

    "Given the code under test is bound
    cl_abap_unit_assert=>assert_bound(
          EXPORTING
            act              = me->mr_cut
*                msg              =
*                level            = if_abap_unit_constant=>severity-medium
*                quit             = if_abap_unit_constant=>quit-test
*              RECEIVING
*                assertion_failed =
        ).
    "When the related LPOR  is required
    DATA(ls_lpor) = me->mr_cut->bi_persistent~lpor( ).
    "Then it is the same as the LPOR kept internally in the instance
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = ls_lpor
        exp                  = me->mr_cut->ms_lpor
*            ignore_hash_sequence = abap_false
*            tol                  =
*            msg                  =
*            level                = if_abap_unit_constant=>severity-medium
*            quit                 = if_abap_unit_constant=>quit-test
*          RECEIVING
*            assertion_failed     =
    ).
  ENDMETHOD.

ENDCLASS.

CLASS lct_new_contract DEFINITION INHERITING FROM lct_common FOR TESTING
                       DURATION SHORT
                       RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS:
      setup,
      tst_is_new FOR TESTING.
ENDCLASS.

CLASS lct_new_contract IMPLEMENTATION.

  METHOD setup.

    me->mr_cut = NEW zcl_acm_contract( i_contract_number = mc_0000000001 ).
    me->mr_cut->mr_api = NEW lcl_api_new_ctr( ).
  ENDMETHOD.

  METHOD tst_is_new.
    "Given a non-existing contract
    "When this condition is tested
    DATA(l_result) = mr_cut->is_new( ).
    "Then the result is FALSE
    cl_abap_unit_assert=>assert_true(
      EXPORTING
        act              = l_result
*        msg              =
*        level            = if_abap_unit_constant=>severity-medium
*        quit             = if_abap_unit_constant=>quit-test
*      RECEIVING
*        assertion_failed =
    ).
  ENDMETHOD.

ENDCLASS.

CLASS lct_existing_contract DEFINITION INHERITING FROM lct_common FOR TESTING
                            DURATION SHORT
                            RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS:
      setup,
      tst_is_new FOR TESTING.
ENDCLASS.

CLASS lct_existing_contract IMPLEMENTATION.

  METHOD setup.

    me->mr_cut = NEW zcl_acm_contract( i_contract_number = mc_0000000001 ).
    me->mr_cut->mr_api = NEW lcl_api_exist_ctr( ).
  ENDMETHOD.

  METHOD tst_is_new.
    "Given an existing contract
    "When this condition is tested
    DATA(l_result) = mr_cut->is_new( ).
    "Then the result is TRUE
    cl_abap_unit_assert=>assert_false(
      EXPORTING
        act              = l_result
*        msg              =
*        level            = if_abap_unit_constant=>severity-medium
*        quit             = if_abap_unit_constant=>quit-test
*      RECEIVING
*        assertion_failed =
    ).
  ENDMETHOD.

ENDCLASS.