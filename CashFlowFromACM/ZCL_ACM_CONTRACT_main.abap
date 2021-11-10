CLASS zcl_acm_contract DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES bi_object .
    INTERFACES bi_persistent .
    INTERFACES if_workflow .

    CONSTANTS mc_this_template TYPE seoclsname VALUE 'ZCL_ACM_CONTRACT' ##NO_TEXT.
    DATA: m_contract_number TYPE tkonn READ-ONLY,
          m_main_contract   TYPE tkonn READ-ONLY,
          m_ref_doc_type    TYPE wlf_shadow_doc_type  READ-ONLY.

    EVENTS: created,
            modified.

    CLASS-METHODS class_constructor .
    METHODS:
      constructor IMPORTING i_contract_number TYPE tkonn,
      is_new      RETURNING VALUE(r_boolean)  TYPE boolean,
      get_available_amount IMPORTING is_item                   TYPE komwbhi
                           RETURNING VALUE(r_available_amount) TYPE bwert
                           RAISING   cx_bapi_error,
      raise_created,
      raise_modified,
      send_new_2fqm             IMPORTING VALUE(i_agent)     TYPE swp_agent OPTIONAL
                                RETURNING VALUE(rt_messages) TYPE bapiret2_t,
      send_changes_2fqm         IMPORTING VALUE(i_agent)     TYPE swp_agent OPTIONAL
                                RETURNING VALUE(rt_messages) TYPE bapiret2_t,
      send_changes_in_main_2fqm IMPORTING VALUE(i_agent)     TYPE swp_agent OPTIONAL
                                RETURNING VALUE(rt_messages) TYPE bapiret2_t.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA:
      mr_api TYPE REF TO lcl_api,
      mr_db  TYPE REF TO lcl_db.

    CLASS-METHODS
      get_reversed_amount IMPORTING i_contract_number        TYPE tkonn
                                    i_item                   TYPE tposn
                          RETURNING VALUE(r_reversed_amount) TYPE bwert.

    DATA:
      ms_lpor                  TYPE sibflpor,
      ms_header                TYPE komwbhk,
      ms_header_main           TYPE komwbhk,
      mt_items                 TYPE lcl_api=>yt_items,
      mt_items_main            TYPE lcl_api=>yt_items,
      mt_business_data         TYPE wb2_komwbhd_tty,
      mt_business_data_main    TYPE wb2_komwbhd_tty,
      mt_predef_comm_data      TYPE ztt_fqm_001,
      mt_predef_comm_data_main TYPE ztt_fqm_001.

    METHODS:
      set_attributes RAISING cx_bapi_error,
      set_attributes_4main RAISING cx_bapi_error,
      set_header     RAISING cx_bapi_error,
      set_header_4main     RAISING cx_bapi_error,
      set_items      RAISING cx_bapi_error,
      set_items_4main RAISING cx_bapi_error,
      set_business_data RAISING cx_bapi_error,
      set_business_data_4main RAISING cx_bapi_error,
      set_predef_comm_data RAISING cx_bapi_error,
      set_predef_comm_data_4main RAISING cx_bapi_error,
      get_available_amount_4main IMPORTING is_item                   TYPE komwbhi
                                 RETURNING VALUE(r_available_amount) TYPE bwert
                                 RAISING   cx_bapi_error,
      get_liquidity_item
        IMPORTING
                  i_material      TYPE fqm_flow_rfc1-material
        RETURNING
                  VALUE(r_result) TYPE fqm_flow_rfc1-liquidity_item
        RAISING   cx_bapi_error,
      get_liquidity_item_4main
        IMPORTING
                  i_material      TYPE fqm_flow_rfc1-material
        RETURNING
                  VALUE(r_result) TYPE fqm_flow_rfc1-liquidity_item
        RAISING   cx_bapi_error,
      get_planning_level
        IMPORTING
                  i_material      TYPE fqm_flow_rfc1-material
        RETURNING
                  VALUE(r_result) TYPE fqm_flow_rfc1-planning_level
        RAISING   cx_bapi_error,
      get_planning_level_4main
        IMPORTING
                  i_material      TYPE fqm_flow_rfc1-material
        RETURNING
                  VALUE(r_result) TYPE fqm_flow_rfc1-planning_level
        RAISING   cx_bapi_error,
      get_planning_group IMPORTING i_partner       TYPE bu_partner
                         RETURNING VALUE(r_result) TYPE fqm_flow_rfc1-planning_group,
      get_planning_group_sd IMPORTING i_partner       TYPE bu_partner
                            RETURNING VALUE(r_result) TYPE fqm_flow_rfc1-planning_group,
      get_certainty_level
        IMPORTING
                  i_material      TYPE fqm_flow_rfc1-material
        RETURNING
                  VALUE(r_result) TYPE fqm_flow_rfc1-certainty_level
        RAISING   cx_bapi_error,
      get_certainty_level_4main
        IMPORTING
                  i_material      TYPE fqm_flow_rfc1-material
        RETURNING
                  VALUE(r_result) TYPE fqm_flow_rfc1-certainty_level
        RAISING   cx_bapi_error,
      get_opened_quantity IMPORTING i_contract_number TYPE tkonn
                                    i_item            TYPE tposn
                                    i_sub_item        TYPE tposn_sub
                          EXPORTING e_open_quantity   TYPE menge_d
                                    e_unit            TYPE meins,
      fill_fmq_flow
        RETURNING
          VALUE(rt_flows) TYPE if_fqmc_types=>ty_t_flow
        RAISING
          cx_bapi_error,
      fill_fmq_flow_4main RETURNING VALUE(rt_flows) TYPE if_fqmc_types=>ty_t_flow
                          RAISING   cx_bapi_error,
      raisewf_created  RAISING cx_bapi_error,
      raisewf_modified RAISING cx_bapi_error,
      raise_created_as_rfctask,
      raise_modified_as_rfctask,
      raise_created_directly,
      raise_modified_directly,
      log_error_messages
        IMPORTING
          ix_error     TYPE REF TO cx_bapi_error
          i_event_type TYPE zde_acm2fqm_event_type
          i_agent      TYPE swp_agent
        RAISING
          cx_bapi_error,
      log_success_messages
        IMPORTING
          is_success_message TYPE bapiret2
          i_event_type       TYPE zde_acm2fqm_event_type
          i_agent            TYPE swp_agent
        RAISING
          cx_bapi_error,
      log_error_messages_4main
        IMPORTING
          ix_error     TYPE REF TO cx_bapi_error
          i_event_type TYPE zde_acm2fqm_event_type
          i_agent      TYPE swp_agent
        RAISING
          cx_bapi_error,
      log_success_messages_4main
        IMPORTING
          is_success_message TYPE bapiret2
          i_event_type       TYPE zde_acm2fqm_event_type
          i_agent            TYPE swp_agent
        RAISING
          cx_bapi_error,
      check_call_off IMPORTING i_agent         TYPE swp_agent OPTIONAL
                     RETURNING
                               VALUE(r_result) TYPE abap_bool,
      set_main RAISING cx_bapi_error.
ENDCLASS.



CLASS zcl_acm_contract IMPLEMENTATION.


  METHOD bi_object~default_attribute_value.

  ENDMETHOD.


  METHOD bi_object~execute_default_method.

  ENDMETHOD.


  METHOD bi_object~release.

  ENDMETHOD.


  METHOD bi_persistent~find_by_lpor.

    result = NEW zcl_acm_contract( i_contract_number = CONV #( lpor-instid ) ).
  ENDMETHOD.


  METHOD bi_persistent~lpor.

    result = me->ms_lpor.
  ENDMETHOD.


  METHOD bi_persistent~refresh.

  ENDMETHOD.


  METHOD class_constructor.
    mr_api = NEW lcl_api( ).
    mr_db  = NEW lcl_db( ).
  ENDMETHOD.


  METHOD constructor.

    me->m_contract_number = i_contract_number.
    me->ms_lpor-catid = 'CL'.
    me->ms_lpor-typeid = me->mc_this_template.
    me->ms_lpor-instid = i_contract_number.
  ENDMETHOD.


  METHOD fill_fmq_flow.

    IF me->ms_header IS INITIAL OR me->mt_items IS INITIAL OR mt_business_data IS INITIAL OR mt_predef_comm_data IS INITIAL.
      TRY.
          me->set_attributes( ).
*          CATCH cx_bapi_error.
        CATCH cx_bapi_error INTO DATA(lx_attr_error).
          RAISE EXCEPTION lx_attr_error.
      ENDTRY.
    ENDIF.

    TRY.
        rt_flows  = VALUE if_fqmc_types=>ty_t_flow( FOR ls_item IN me->mt_items
                                                     (    company_code     = me->ms_header-company_code
                                                          transaction_date = me->ms_header-zzdata_pagto
                                                          origin_system    = me->ms_header-logsys
                                                          material         = ls_item-matnr
                                                          business_area    = ls_item-werks "N/A ls_item-gsber
                                                          profit_center    = ls_item-prctr
                                                          cost_center      = ls_item-kostl
                                                          "segment          = N/A
                                                          "bank_account_id  = ??
                                                          amount           = me->get_available_amount( is_item = ls_item )
*                                                                             CATCH cx_bapi_error.
                                                           currency        = me->ms_header-tkwaers
                                                           "base_amount     = ??
                                                           "base_currency   = ??
                                                           "fi_account      = N/A
                                                           origin_document_id = me->m_contract_number
                                                           origin_transaction_id = cl_abap_syst=>get_transaction_code( )
                                                           "flow_id               = ??
                                                           "origin_trans_qualifier = ??
                                                           customer_number      = me->ms_header-kunnr
                                                           vendor_number        = mt_business_data[ tposn = '000000'
                                                                                                    tposn_sub = '000000' ]-elifn
                                                           "partner              = ??
                                                           "project              = ??
                                                           "trading_partner      = ??
                                                           "assigned_company_code = ??
                                                           fi_fiscal_year       = ls_item-erdat(4)
                                                           origin_application    = if_fqm_origin_application_c=>gc_remote-code
                                                           owner                 = 'RTE'
                                                       ) ).

        DATA(lt_release_criteria) = me->mr_api->fqm_release_cfg_read( ).

        LOOP AT rt_flows ASSIGNING FIELD-SYMBOL(<ls_flow>).
          <ls_flow>-certainty_level = me->get_certainty_level( <ls_flow>-material ).
          <ls_flow>-flow_type = COND #(  WHEN <ls_flow>-Amount < 0 THEN '900001'
                                         ELSE  '900000' ).
          <ls_flow>-liquidity_item = me->get_liquidity_item( i_material = <ls_flow>-material ).
          <ls_flow>-planning_level = me->get_planning_level( <ls_flow>-material ).
          <ls_flow>-planning_group = COND #( WHEN mt_business_data[ tposn = '000000'
                                                                    tposn_sub = '000000' ]-elifn IS NOT INITIAL
                                              THEN me->get_planning_group( EXPORTING i_partner = mt_business_data[ tposn = '000000'
                                                                                                                   tposn_sub = '000000' ]-elifn )
                                             ELSE me->get_planning_group_sd( i_partner = me->ms_header-kunnr ) ).
          CLEAR <ls_flow>-business_area.
          <ls_flow>-rel_status = COND #( WHEN line_exists( lt_release_criteria[ origin_system  = me->ms_header-logsys
                                                                                company_code   = me->ms_header-company_code
                                                                                planning_level = <ls_flow>-planning_level   ] )
                                          THEN 'U' ).
        ENDLOOP.
      CATCH cx_sy_itab_line_not_found
            cx_bapi_error.
        MESSAGE e001(zacm02) INTO DATA(l_message).
        RAISE EXCEPTION TYPE cx_bapi_error
          EXPORTING
            status = VALUE #(  (    id         = sy-msgid
                                    type       = sy-msgty
                                    number     = sy-msgno
                                    message    = l_message
                                    message_v1 = sy-msgv1
                                    message_v2 = sy-msgv2
                                    message_v3 = sy-msgv3
                                    message_v4 = sy-msgv4 ) ).
    ENDTRY.

  ENDMETHOD.


  METHOD fill_fmq_flow_4main.
    IF me->ms_header_main IS INITIAL OR me->mt_items_main IS INITIAL OR mt_business_data_main IS INITIAL OR
       mt_predef_comm_data_main IS INITIAL.
      TRY.
          me->set_attributes_4main( ).
*          CATCH cx_bapi_error.
        CATCH cx_bapi_error INTO DATA(lx_attr_error).
          RAISE EXCEPTION lx_attr_error.
      ENDTRY.
    ENDIF.

    TRY.
        rt_flows  = VALUE if_fqmc_types=>ty_t_flow( FOR ls_item IN me->mt_items_main
                                                     (    company_code     = me->ms_header_main-company_code
                                                          transaction_date = me->ms_header_main-zzdata_pagto
                                                          origin_system    = me->ms_header_main-logsys
                                                          material         = ls_item-matnr
                                                          business_area    = ls_item-werks "N/A ls_item-gsber
                                                          profit_center    = ls_item-prctr
                                                          cost_center      = ls_item-kostl
                                                          "segment          = N/A
                                                          "bank_account_id  = ??
                                                          amount           = me->get_available_amount_4main( is_item = ls_item  )
*                                                                             CATCH cx_bapi_error.
                                                           currency        = me->ms_header_main-tkwaers
                                                           "base_amount     = ??
                                                           "base_currency   = ??
                                                           "fi_account      = N/A
                                                           origin_document_id = me->m_main_contract
                                                           origin_transaction_id = cl_abap_syst=>get_transaction_code( )
                                                           "flow_id               = ??
                                                           "origin_trans_qualifier = ??
                                                           customer_number      = me->ms_header_main-kunnr
                                                           vendor_number        = mt_business_data_main[ tposn = '000000'
                                                                                                    tposn_sub = '000000' ]-elifn
                                                           "partner              = ??
                                                           "project              = ??
                                                           "trading_partner      = ??
                                                           "assigned_company_code = ??
                                                           fi_fiscal_year       = ls_item-erdat(4)
                                                           origin_application    = if_fqm_origin_application_c=>gc_remote-code
                                                           owner                 = 'RTE'
                                                       ) ).

        DATA(lt_release_criteria) = me->mr_api->fqm_release_cfg_read( ).

        LOOP AT rt_flows ASSIGNING FIELD-SYMBOL(<ls_flow>).
          <ls_flow>-certainty_level = me->get_certainty_level_4main( <ls_flow>-material ).
          <ls_flow>-flow_type = COND #(  WHEN <ls_flow>-Amount < 0 THEN '900001'
                                         ELSE  '900000' ).
          <ls_flow>-liquidity_item = me->get_liquidity_item_4main( i_material = <ls_flow>-material ).
          <ls_flow>-planning_level = me->get_planning_level_4main( <ls_flow>-material ).
          <ls_flow>-planning_group = COND #( WHEN mt_business_data_main[ tposn = '000000'
                                                                         tposn_sub = '000000' ]-elifn IS NOT INITIAL
                                              THEN me->get_planning_group( EXPORTING i_partner = mt_business_data_main[ tposn = '000000'
                                                                                                                        tposn_sub = '000000' ]-elifn )
                                             ELSE '' ).
          CLEAR <ls_flow>-business_area.
          <ls_flow>-rel_status = COND #( WHEN line_exists( lt_release_criteria[ origin_system  = me->ms_header_main-logsys
                                                                                company_code   = me->ms_header_main-company_code
                                                                                planning_level = <ls_flow>-planning_level   ] )
                                          THEN 'U' ).
        ENDLOOP.
      CATCH cx_sy_itab_line_not_found
            cx_bapi_error.
        MESSAGE e001(zacm02) INTO DATA(l_message).
        RAISE EXCEPTION TYPE cx_bapi_error
          EXPORTING
            status = VALUE #(  (    id         = sy-msgid
                                    type       = sy-msgty
                                    number     = sy-msgno
                                    message    = l_message
                                    message_v1 = sy-msgv1
                                    message_v2 = sy-msgv2
                                    message_v3 = sy-msgv3
                                    message_v4 = sy-msgv4 ) ).
    ENDTRY.
  ENDMETHOD.


  METHOD get_certainty_level.

    CLEAR r_result.
    IF me->mt_predef_comm_data IS INITIAL.
      TRY.
          me->set_predef_comm_data( ).
*          CATCH cx_bapi_error.
        CATCH cx_bapi_error INTO DATA(lx_error_ctrty).
          RAISE EXCEPTION lx_error_ctrty.
      ENDTRY.
    ENDIF.

    TRY.
        r_result = me->mt_predef_comm_data[ material = i_material ]-certainty_level.
      CATCH cx_sy_itab_line_not_found.
        MESSAGE e004(zfqm) WITH i_material INTO DATA(l_message).
        RAISE EXCEPTION TYPE cx_bapi_error
          EXPORTING
            status = VALUE #( ( id = sy-msgid
                                type = sy-msgty
                                number = sy-msgno
                                message = l_message
                                message_v1 = sy-msgv1
                                message_v2 = sy-msgv2
                                message_v3 = sy-msgv3
                                message_v4 = sy-msgv4 ) ).
    ENDTRY.

  ENDMETHOD.


  METHOD get_certainty_level_4main.

    CLEAR r_result.
    IF me->mt_predef_comm_data_main IS INITIAL.
      TRY.
          me->set_predef_comm_data_4main( ).
*          CATCH cx_bapi_error.
        CATCH cx_bapi_error INTO DATA(lx_error_ctrty).
          RAISE EXCEPTION lx_error_ctrty.
      ENDTRY.
    ENDIF.

    TRY.
        r_result = me->mt_predef_comm_data_main[ material = i_material ]-certainty_level.
      CATCH cx_sy_itab_line_not_found.
        MESSAGE e004(zfqm) WITH i_material INTO DATA(l_message).
        RAISE EXCEPTION TYPE cx_bapi_error
          EXPORTING
            status = VALUE #( ( id = sy-msgid
                                type = sy-msgty
                                number = sy-msgno
                                message = l_message
                                message_v1 = sy-msgv1
                                message_v2 = sy-msgv2
                                message_v3 = sy-msgv3
                                message_v4 = sy-msgv4 ) ).
    ENDTRY.
  ENDMETHOD.


  METHOD get_liquidity_item.

    CLEAR r_result.
    IF me->mt_predef_comm_data IS INITIAL.
      TRY.
          me->set_predef_comm_data( ).
*          CATCH cx_bapi_error.
        CATCH cx_bapi_error INTO DATA(lx_error_ctrty).
          RAISE EXCEPTION lx_error_ctrty.
      ENDTRY.
    ENDIF.

    TRY.
        r_result = me->mt_predef_comm_data[ material = i_material ]-liquidity_item.
      CATCH cx_sy_itab_line_not_found ##NO_HANDLER.
        MESSAGE e004(zfqm) WITH i_material INTO DATA(l_message).
        RAISE EXCEPTION TYPE cx_bapi_error
          EXPORTING
            status = VALUE #( ( id = sy-msgid
                                type = sy-msgty
                                number = sy-msgno
                                message = l_message
                                message_v1 = sy-msgv1
                                message_v2 = sy-msgv2
                                message_v3 = sy-msgv3
                                message_v4 = sy-msgv4 ) ).
    ENDTRY.
  ENDMETHOD.


  METHOD get_liquidity_item_4main.

    CLEAR r_result.
    IF me->mt_predef_comm_data_main IS INITIAL.
      TRY.
          me->set_predef_comm_data_4main( ).
*          CATCH cx_bapi_error.
        CATCH cx_bapi_error INTO DATA(lx_error_ctrty).
          RAISE EXCEPTION lx_error_ctrty.
      ENDTRY.
    ENDIF.

    TRY.
        r_result = me->mt_predef_comm_data_main[ material = i_material ]-liquidity_item.
      CATCH cx_sy_itab_line_not_found ##NO_HANDLER.
        MESSAGE e004(zfqm) WITH i_material INTO DATA(l_message).
        RAISE EXCEPTION TYPE cx_bapi_error
          EXPORTING
            status = VALUE #( ( id = sy-msgid
                                type = sy-msgty
                                number = sy-msgno
                                message = l_message
                                message_v1 = sy-msgv1
                                message_v2 = sy-msgv2
                                message_v3 = sy-msgv3
                                message_v4 = sy-msgv4 ) ).
    ENDTRY.
  ENDMETHOD.


  METHOD get_planning_group.

    CLEAR r_result.
    TRY.
        r_result = me->mr_db->query_fdgrv_in_lfb1( i_vendor_id = i_partner ).
*                   CATCH cx_sy_sql_error.
      CATCH cx_sy_sql_error ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.


  METHOD get_planning_group_sd.

    CLEAR r_result.
    TRY.
        r_result = me->mr_db->query_fdgrv_in_KNB1( i_customer_id = i_partner ).
*                   CATCH cx_sy_sql_error.
      CATCH cx_sy_sql_error ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.


  METHOD get_planning_level.

    CLEAR r_result.
    IF me->mt_predef_comm_data IS INITIAL.
      TRY.
          me->set_predef_comm_data( ).
*          CATCH cx_bapi_error.
        CATCH cx_bapi_error INTO DATA(lx_error_ctrty).
          RAISE EXCEPTION lx_error_ctrty.
      ENDTRY.
    ENDIF.

    TRY.
        r_result = me->mt_predef_comm_data[ material = i_material ]-planning_level.
      CATCH cx_sy_itab_line_not_found.
        MESSAGE e004(zfqm) WITH i_material INTO DATA(l_message).
        RAISE EXCEPTION TYPE cx_bapi_error
          EXPORTING
            status = VALUE #( ( id = sy-msgid
                                type = sy-msgty
                                number = sy-msgno
                                message = l_message
                                message_v1 = sy-msgv1
                                message_v2 = sy-msgv2
                                message_v3 = sy-msgv3
                                message_v4 = sy-msgv4 ) ).
    ENDTRY.
  ENDMETHOD.


  METHOD get_planning_level_4main.

    CLEAR r_result.
    IF me->mt_predef_comm_data_main IS INITIAL.
      TRY.
          me->set_predef_comm_data_4main( ).
*          CATCH cx_bapi_error.
        CATCH cx_bapi_error INTO DATA(lx_error_ctrty).
          RAISE EXCEPTION lx_error_ctrty.
      ENDTRY.
    ENDIF.

    TRY.
        r_result = me->mt_predef_comm_data_main[ material = i_material ]-planning_level.
      CATCH cx_sy_itab_line_not_found.
        MESSAGE e004(zfqm) WITH i_material INTO DATA(l_message).
        RAISE EXCEPTION TYPE cx_bapi_error
          EXPORTING
            status = VALUE #( ( id = sy-msgid
                                type = sy-msgty
                                number = sy-msgno
                                message = l_message
                                message_v1 = sy-msgv1
                                message_v2 = sy-msgv2
                                message_v3 = sy-msgv3
                                message_v4 = sy-msgv4 ) ).
    ENDTRY.
  ENDMETHOD.


  METHOD get_opened_quantity.

    CLEAR: e_open_quantity, e_unit.
* /ACCGO/CL_CMN_TC_UTILITY.get_tot_consm_qty
*-->  Get Quantity Type instance
*----------------------------------------------------------------------*
    /accgo/cl_cck_quantity_types=>get_instance( IMPORTING ex_o_object = DATA(lr_quantity_types) ).
*----------------------------------------------------------------------*
    IF lr_quantity_types IS BOUND.
      DATA(lt_quan_types) = VALUE /accgo/tt_cmn_qty( ( quan_type = /accgo/if_cmn_constants=>gc_available_to_calloff ) ).
      lr_quantity_types->get_quantity_data_dynamically(
        EXPORTING
          is_wbhi_key             = VALUE #( tkonn = i_contract_number tposn = i_item  tposn_sub = i_sub_item )
          it_quan_type            = lt_quan_types
*          iv_dec_qty_not_required =
        IMPORTING
          et_quantity             = DATA(et_quantity)
      ).
    ENDIF.

    TRY.
        e_open_quantity = et_quantity[ quantity_type = /accgo/if_cmn_constants=>gc_available_to_calloff ]-quantity.
        e_unit          = et_quantity[ quantity_type = /accgo/if_cmn_constants=>gc_available_to_calloff ]-unit.
      CATCH cx_sy_itab_line_not_found ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.


  METHOD is_new.

    CLEAR r_boolean.
    IF me->ms_header IS INITIAL.
      TRY.
          me->set_header( ).
*          CATCH cx_bapi_error.
        CATCH cx_bapi_error ##NO_HANDLER.
      ENDTRY.
    ENDIF.

    r_boolean = COND #( WHEN me->ms_header IS INITIAL THEN abap_true ).
  ENDMETHOD.


  METHOD get_available_amount.

    CLEAR r_available_amount.
    IF me->ms_header IS INITIAL OR me->mt_items IS INITIAL OR mt_business_data IS INITIAL OR
       mt_predef_comm_data IS INITIAL.
      TRY.
          me->set_attributes( ).
*          CATCH cx_bapi_error.
        CATCH cx_bapi_error INTO DATA(lx_attr_error).
          RAISE EXCEPTION lx_attr_error.
      ENDTRY.
    ENDIF.


    "Getting directly used amount
    TRY.
        r_available_amount           = COND #( WHEN me->m_ref_doc_type = 'C'
                                                THEN me->mr_db->query_used_value_from_po(
                                                                                          i_contract_number = me->m_contract_number
                                                                                          i_contract_item   = is_item-tposn
                                                                                        ) - is_item-netwrt_mm
*                                                                                                   CATCH cx_sy_sql_error.
                                           WHEN me->m_ref_doc_type = 'A'
                                                 THEN is_item-netwrt_sd - me->mr_db->query_used_value_from_so(
                                                                                             i_contract_number = me->m_contract_number
                                                                                             i_contract_item   = is_item-tposn
                                                                                       )
*                                                                                                   CATCH cx_sy_sql_error.
                                         ).
        r_available_amount = r_available_amount + get_reversed_amount( EXPORTING i_contract_number = me->m_contract_number
                                                                                 i_item = is_item-tposn ).
      CATCH cx_sy_sql_error.
        MESSAGE e002 INTO DATA(l_message).
        RAISE EXCEPTION TYPE cx_bapi_error
          EXPORTING
            status = VALUE #( ( id = sy-msgid
                                type = sy-msgty
                                number = sy-msgno
                                message = l_message
                                message_v1 = sy-msgv1
                                message_v2 = sy-msgv2
                                message_v3 = sy-msgv3
                                message_v4 = sy-msgv4 ) ).
      CATCH cx_bapi_error INTO DATA(lx_error).
        RAISE EXCEPTION lx_error.
    ENDTRY.

    IF abs( r_available_amount ) < 1. "ToDo: Evaluate the possibility of to assign this control to a variant and whether the currency shall play a role in it
      DO 100 TIMES. "ToDo: Evaluate the possibility of to assign this control to a variant
        "If the contract is fully used there is no available amount
        me->get_opened_quantity(
          EXPORTING
            i_contract_number = me->m_contract_number
            i_item            = is_item-tposn
            i_sub_item        = is_item-tposn_sub
          IMPORTING
            e_open_quantity   = DATA(l_open_quantity)
*            e_unit            =
        ).
        IF l_open_quantity <= 0.
          r_available_amount = 0.
          RETURN.
        ENDIF.
      ENDDO.
    ENDIF.

  ENDMETHOD.


  METHOD get_available_amount_4main.

    CLEAR r_available_amount.
    IF me->ms_header_main IS INITIAL OR me->mt_items_main IS INITIAL OR mt_business_data_main IS INITIAL OR
       mt_predef_comm_data_main IS INITIAL.
      TRY.
          me->set_attributes_4main( ).
*          CATCH cx_bapi_error.
        CATCH cx_bapi_error INTO DATA(lx_attr_error).
          RAISE EXCEPTION lx_attr_error.
      ENDTRY.
    ENDIF.

    "Getting directly used amount
    TRY.
        r_available_amount           = COND #( WHEN me->m_ref_doc_type = 'C'
                                                THEN me->mr_db->query_used_value_from_po(
                                                                                          i_contract_number = me->m_main_contract
                                                                                          i_contract_item   = is_item-tposn
                                                                                        ) - is_item-netwrt_mm
*                                                                                                   CATCH cx_sy_sql_error.
                                           WHEN me->m_ref_doc_type = 'A'
                                                 THEN is_item-netwrt_sd - me->mr_db->query_used_value_from_so(
                                                                                             i_contract_number = me->m_main_contract
                                                                                             i_contract_item   = is_item-tposn
                                                                                       )
*                                                                                                   CATCH cx_sy_sql_error.
                                         ).
        r_available_amount = r_available_amount + get_reversed_amount( EXPORTING i_contract_number = me->m_main_contract
                                                                                 i_item            = is_item-tposn ).
      CATCH cx_sy_sql_error.
        MESSAGE e002 INTO DATA(l_message).
        RAISE EXCEPTION TYPE cx_bapi_error
          EXPORTING
            status = VALUE #( ( id = sy-msgid
                                type = sy-msgty
                                number = sy-msgno
                                message = l_message
                                message_v1 = sy-msgv1
                                message_v2 = sy-msgv2
                                message_v3 = sy-msgv3
                                message_v4 = sy-msgv4 ) ).
    ENDTRY.

    IF abs( r_available_amount ) < 1. "ToDo: Evaluate the possibility of to assign this control to a variant and whether the currency shall play a role in it
      DO 100 TIMES. "ToDo: Evaluate the possibility of to assign this control to a variant
        "If the contract is fully used there is no available amount
        me->get_opened_quantity(
          EXPORTING
            i_contract_number = me->m_contract_number
            i_item            = is_item-tposn
            i_sub_item        = is_item-tposn_sub
          IMPORTING
            e_open_quantity   = DATA(l_open_quantity)
*            e_unit            =
        ).
        IF l_open_quantity <= 0.
          r_available_amount = 0.
          RETURN.
        ENDIF.
      ENDDO.
    ENDIF.
  ENDMETHOD.


  METHOD raise_created.

    IF 1 = 2.
      me->raise_created_directly( ).
    ELSEIF 2 = 3.
      me->raise_created_as_rfctask(  ).
    ELSE.
      TRY.
          me->raisewf_created( ).
*        CATCH cx_bapi_error.
        CATCH cx_bapi_error INTO DATA(lx_error).
          TRY.
              log_error_messages( EXPORTING ix_error = lx_error i_event_type = 'C'
                                            i_agent  = 'US' && cl_abap_syst=>get_user_name( ) ).
            CATCH cx_bapi_error INTO lx_error.
              IF lx_error->status IS NOT INITIAL.
                MESSAGE lx_error->status[ 1 ]-message TYPE 'E'.
              ELSE.
                MESSAGE e001.
              ENDIF.
          ENDTRY.
      ENDTRY.
    ENDIF.
  ENDMETHOD.


  METHOD raise_modified.

    IF 1 = 2.
      me->raise_modified_directly( ).
    ELSEIF 2 = 3.
      me->raise_modified_as_rfctask(  ).
    ELSE.
      TRY.
          me->raisewf_modified( ).
*        CATCH cx_bapi_error.
        CATCH cx_bapi_error INTO DATA(lx_error).
          TRY.
              log_error_messages( EXPORTING ix_error = lx_error i_event_type = 'M'
                                            i_agent  = 'US' && cl_abap_syst=>get_user_name( ) ).
            CATCH cx_bapi_error INTO lx_error.
              IF lx_error->status IS NOT INITIAL.
                MESSAGE lx_error->status[ 1 ]-message TYPE 'E'.
              ELSE.
                MESSAGE e001.
              ENDIF.
          ENDTRY.
      ENDTRY.
    ENDIF.
  ENDMETHOD.


  METHOD raisewf_created.

    TRY.
        cl_swf_evt_event=>raise(
          EXPORTING
            im_objcateg        = cl_swf_evt_event=>mc_objcateg_cl
            im_objtype         = mc_this_template
            im_event           = 'CREATED'
            im_objkey          = me->m_contract_number
*               im_event_container =
        ).
*           CATCH cx_swf_evt_invalid_objtype.
*           CATCH cx_swf_evt_invalid_event.
      CATCH cx_swf_evt_invalid_objtype ##NO_HANDLER
            cx_swf_evt_invalid_event.
        MESSAGE e001 INTO DATA(l_message).
        RAISE EXCEPTION TYPE cx_bapi_error
          EXPORTING
            status = VALUE #(  (    id         = sy-msgid
                                    type       = sy-msgty
                                    number     = sy-msgno
                                    message    = l_message
                                    message_v1 = sy-msgv1
                                    message_v2 = sy-msgv2
                                    message_v3 = sy-msgv3
                                    message_v4 = sy-msgv4 ) ).
    ENDTRY.
  ENDMETHOD.


  METHOD raisewf_modified.

    TRY.
        cl_swf_evt_event=>raise(
          EXPORTING
            im_objcateg        = cl_swf_evt_event=>mc_objcateg_cl
            im_objtype         = mc_this_template
            im_event           = 'MODIFIED'
            im_objkey          = me->m_contract_number
*               im_event_container =
        ).
*           CATCH cx_swf_evt_invalid_objtype.
*           CATCH cx_swf_evt_invalid_event.
      CATCH cx_swf_evt_invalid_objtype ##NO_HANDLER
            cx_swf_evt_invalid_event.
        MESSAGE e001 INTO DATA(l_message).
        RAISE EXCEPTION TYPE cx_bapi_error
          EXPORTING
            status = VALUE #(  (    id         = sy-msgid
                                    type       = sy-msgty
                                    number     = sy-msgno
                                    message    = l_message
                                    message_v1 = sy-msgv1
                                    message_v2 = sy-msgv2
                                    message_v3 = sy-msgv3
                                    message_v4 = sy-msgv4 ) ).
    ENDTRY.
  ENDMETHOD.


  METHOD raise_created_as_rfctask.

    me->mr_api->zfqm_send_created( i_contract_number = me->m_contract_number  ).
  ENDMETHOD.


  METHOD raise_modified_as_rfctask.

    me->mr_api->zfqm_send_changes( i_contract_number = me->m_contract_number ).
  ENDMETHOD.


  METHOD raise_created_directly.

    me->send_new_2fqm( ).
  ENDMETHOD.


  METHOD raise_modified_directly.

    me->send_changes_2fqm( ).
  ENDMETHOD.


  METHOD send_changes_2fqm.

    CLEAR rt_messages.
    TRY.
        IF me->check_call_off( i_agent = i_agent ) = abap_true.
          RETURN.
        ENDIF.

        DATA(lt_flows) = fill_fmq_flow( ).

        me->mr_api->fqm_aif_remote_update(
          EXPORTING
            i_operation = 'UPD'
            it_flows    = lt_flows
        ).
*        CATCH cx_bapi_error.
        MESSAGE i007(zfqm) WITH me->m_contract_number INTO DATA(l_success_message).
        log_success_messages( EXPORTING is_success_message = VALUE #( id = sy-msgid
                                                                      type = sy-msgty
                                                                      number = sy-msgno
                                                                      message = l_success_message
                                                                      message_v1 = sy-msgv1
                                                                      message_v2 = sy-msgv2
                                                                      message_v3 = sy-msgv3
                                                                      message_v4 = sy-msgv4 )
                                         i_event_type       = 'M'
                                         i_agent            = i_agent  ).
      CATCH cx_bapi_error INTO DATA(lx_error).
        MESSAGE e008(zfqm) WITH me->m_contract_number INTO DATA(l_error_message).
        lx_error->status = VALUE #( BASE lx_error->status
                                    ( id = sy-msgid
                                      type = sy-msgty
                                      number = sy-msgno
                                      message = l_success_message
                                      message_v1 = sy-msgv1
                                      message_v2 = sy-msgv2
                                      message_v3 = sy-msgv3
                                      message_v4 = sy-msgv4 ) ).
        TRY.
            me->log_error_messages(
              EXPORTING
                ix_error     = lx_error
                i_event_type = 'M'
                i_agent      = i_agent
            ).
          CATCH cx_bapi_error INTO lx_error.
            IF lx_error->status IS NOT INITIAL.
              MESSAGE lx_error->status[ 1 ]-message TYPE 'E'.
            ELSE.
              MESSAGE e001.
            ENDIF.
        ENDTRY.
        rt_messages = lx_error->status.
    ENDTRY.
  ENDMETHOD.


  METHOD send_changes_in_main_2fqm.

    CLEAR rt_messages.
    TRY.
        DATA(lt_flows) = fill_fmq_flow_4main( ).

        DATA(l_operation) = VALUE char3(  ).
        LOOP AT lt_flows TRANSPORTING NO FIELDS WHERE amount <> 0.
          l_operation = 'UPD'.
          EXIT.
        ENDLOOP.
        l_operation = COND #( WHEN l_operation IS INITIAL THEN 'DEL' ELSE 'UPD' ).

        me->mr_api->fqm_aif_remote_update(
          EXPORTING
            i_operation = l_operation
            it_flows    = lt_flows
        ).
*        CATCH cx_bapi_error.
        MESSAGE i007(zfqm) WITH me->m_main_contract INTO DATA(l_success_message).
        log_success_messages_4main( EXPORTING is_success_message = VALUE #( id = sy-msgid
                                                                      type = sy-msgty
                                                                      number = sy-msgno
                                                                      message = l_success_message
                                                                      message_v1 = sy-msgv1
                                                                      message_v2 = sy-msgv2
                                                                      message_v3 = sy-msgv3
                                                                      message_v4 = sy-msgv4 )
                                         i_event_type       = 'M'
                                         i_agent            = i_agent  ).
      CATCH cx_bapi_error INTO DATA(lx_error).
        MESSAGE e008(zfqm) WITH me->m_contract_number INTO DATA(l_error_message).
        lx_error->status = VALUE #( BASE lx_error->status
                                    ( id = sy-msgid
                                      type = sy-msgty
                                      number = sy-msgno
                                      message = l_success_message
                                      message_v1 = sy-msgv1
                                      message_v2 = sy-msgv2
                                      message_v3 = sy-msgv3
                                      message_v4 = sy-msgv4 ) ).
        TRY.
            me->log_error_messages_4main(
              EXPORTING
                ix_error     = lx_error
                i_event_type = 'M'
                i_agent      = i_agent
            ).
          CATCH cx_bapi_error INTO lx_error.
            IF lx_error->status IS NOT INITIAL.
              MESSAGE lx_error->status[ 1 ]-message TYPE 'E'.
            ELSE.
              MESSAGE e001.
            ENDIF.
        ENDTRY.
        rt_messages = lx_error->status.
    ENDTRY.
  ENDMETHOD.


  METHOD send_new_2fqm.

    CLEAR rt_messages.
    TRY.
        IF me->check_call_off( i_agent = i_agent ) = abap_true.
          RETURN.
        ENDIF.

        me->mr_api->fqm_aif_remote_update(
          EXPORTING
            i_operation = 'INS'
            it_flows    = fill_fmq_flow( )
        ).
*        CATCH cx_bapi_error.
        MESSAGE i005(zfqm) WITH me->m_contract_number INTO DATA(l_success_message).
        log_success_messages( EXPORTING is_success_message = VALUE #( id = sy-msgid
                                                                      type = sy-msgty
                                                                      number = sy-msgno
                                                                      message = l_success_message
                                                                      message_v1 = sy-msgv1
                                                                      message_v2 = sy-msgv2
                                                                      message_v3 = sy-msgv3
                                                                      message_v4 = sy-msgv4 )
                                         i_event_type       = 'C'
                                         i_agent            = i_agent  ).
      CATCH cx_bapi_error INTO DATA(lx_error).
        MESSAGE e006(zfqm) WITH me->m_contract_number INTO DATA(l_error_message).
        lx_error->status = VALUE #( BASE lx_error->status
                                    ( id = sy-msgid
                                      type = sy-msgty
                                      number = sy-msgno
                                      message = l_success_message
                                      message_v1 = sy-msgv1
                                      message_v2 = sy-msgv2
                                      message_v3 = sy-msgv3
                                      message_v4 = sy-msgv4 ) ).
        TRY.
            me->log_error_messages(
              EXPORTING
                ix_error     = lx_error
                i_event_type = 'C'
                i_agent      = i_agent
            ).
          CATCH cx_bapi_error INTO lx_error.
            IF lx_error->status IS NOT INITIAL.
              MESSAGE lx_error->status[ 1 ]-message TYPE 'E'.
            ELSE.
              MESSAGE e001.
            ENDIF.
        ENDTRY.
        rt_messages = lx_error->status.
    ENDTRY.
  ENDMETHOD.


  METHOD set_attributes.

    TRY.
        me->set_header( ).
*        CATCH cx_bapi_error.
        me->set_items( ).
*        CATCH cx_bapi_error.
        me->set_business_data( ).
*        CATCH cx_bapi_error.
        me->set_predef_comm_data(  ).
      CATCH cx_bapi_error INTO DATA(lx_error).
        RAISE EXCEPTION lx_error.
    ENDTRY.
  ENDMETHOD.


  METHOD set_attributes_4main.

    TRY.
        me->set_header_4main( ).
*        CATCH cx_bapi_error.
        me->set_items_4main( ).
*        CATCH cx_bapi_error.
        me->set_business_data_4main( ).
*        CATCH cx_bapi_error.
        me->set_predef_comm_data_4main(  ).
      CATCH cx_bapi_error INTO DATA(lx_error).
        RAISE EXCEPTION lx_error.
    ENDTRY.
  ENDMETHOD.


  METHOD set_business_data.

    CLEAR me->mt_business_data.
    TRY.
        me->mr_api->wb2_read_busi_data_with_tkonn(
          EXPORTING
            i_contract_number    = me->m_contract_number
          IMPORTING
            et_business_data = me->mt_business_data
        ).
*        CATCH cx_bapi_error.
      CATCH cx_bapi_error INTO DATA(lx_error).
        RAISE EXCEPTION lx_error.
    ENDTRY.
  ENDMETHOD.


  METHOD set_business_data_4main.

    CLEAR me->mt_business_data_main.
    TRY.
        me->mr_api->wb2_read_busi_data_with_tkonn(
          EXPORTING
            i_contract_number    = me->m_main_contract
          IMPORTING
            et_business_data = me->mt_business_data_main
        ).
*        CATCH cx_bapi_error.
      CATCH cx_bapi_error INTO DATA(lx_error).
        RAISE EXCEPTION lx_error.
    ENDTRY.
  ENDMETHOD.


  METHOD set_header.

    TRY.
        me->mr_api->wb2_read_docu_header_single(
          EXPORTING
            i_contract_number  = me->m_contract_number
          IMPORTING
            es_header_data = me->ms_header
        ).
*        CATCH cx_bapi_error.
        IF me->m_ref_doc_type IS INITIAL.
          me->m_ref_doc_type = COND #( WHEN me->ms_header-tctyp+1(1) = 'C' THEN 'C'
                                       WHEN me->ms_header-tctyp+1(1) = 'V' THEN 'A' ).
          IF me->m_ref_doc_type IS INITIAL.
            MESSAGE e010 WITH me->m_contract_number INTO DATA(l_message).
            RAISE EXCEPTION TYPE cx_bapi_error
              EXPORTING
                status = VALUE #( ( id = sy-msgid
                                    type = sy-msgty
                                    number = sy-msgno
                                    message = l_message
                                    message_v1 = sy-msgv1
                                    message_v2 = sy-msgv2
                                    message_v3 = sy-msgv3
                                    message_v4 = sy-msgv4 ) ).
          ENDIF.
        ENDIF.
      CATCH cx_bapi_error INTO DATA(lx_error).
        RAISE EXCEPTION lx_error.
    ENDTRY.
  ENDMETHOD.


  METHOD set_header_4main.

    TRY.
        me->set_main(  ).
      CATCH cx_bapi_error INTO DATA(lx_error).
        RAISE EXCEPTION lx_error.
    ENDTRY.


    TRY.
        me->mr_api->wb2_read_docu_header_single(
          EXPORTING
            i_contract_number  = me->m_main_contract
          IMPORTING
            es_header_data = me->ms_header_main
        ).
*        CATCH cx_bapi_error.
      CATCH cx_bapi_error INTO lx_error.
        RAISE EXCEPTION lx_error.
    ENDTRY.
  ENDMETHOD.


  METHOD set_items.

    CLEAR me->mt_items.
    TRY.
        me->mr_api->wb2_read_docu_item_with_tkonn(
          EXPORTING
            i_contract_number = me->m_contract_number
          IMPORTING
            et_items      = me->mt_items
        ).
*        CATCH cx_bapi_error.
      CATCH cx_bapi_error INTO DATA(lx_error).
        RAISE EXCEPTION lx_error.
    ENDTRY.
  ENDMETHOD.


  METHOD set_items_4main.

    CLEAR me->mt_items_main.
    TRY.
        me->mr_api->wb2_read_docu_item_with_tkonn(
          EXPORTING
            i_contract_number = me->m_main_contract
          IMPORTING
            et_items      = me->mt_items_main
        ).
*        CATCH cx_bapi_error.
      CATCH cx_bapi_error INTO DATA(lx_error).
        RAISE EXCEPTION lx_error.
    ENDTRY.
  ENDMETHOD.


  METHOD set_predef_comm_data.

    CLEAR me->mt_predef_comm_data.
    IF me->ms_header IS INITIAL.
      TRY.
          me->mr_api->wb2_read_docu_header_single(
            EXPORTING
              i_contract_number  = me->m_contract_number
            IMPORTING
              es_header_data = me->ms_header
          ).
*          CATCH cx_bapi_error.
        CATCH cx_bapi_error INTO DATA(lx_error).
          RAISE EXCEPTION lx_error.
      ENDTRY.
    ENDIF.

    TRY.
        me->mt_predef_comm_data = me->mr_db->query_ztb_fqm_001( i_contract_type = me->ms_header-tctyp ).
*                                  CATCH cx_sy_sql_error.
      CATCH cx_sy_sql_error.
        MESSAGE e003(zfqm) WITH me->ms_header-tctyp INTO DATA(l_message).
        RAISE EXCEPTION TYPE cx_bapi_error
          EXPORTING
            status = VALUE #( ( id = sy-msgid
                                type = sy-msgty
                                number = sy-msgno
                                message = l_message
                                message_v1 = sy-msgv1
                                message_v2 = sy-msgv2
                                message_v3 = sy-msgv3
                                message_v4 = sy-msgv4 ) ).
    ENDTRY.
  ENDMETHOD.


  METHOD set_predef_comm_data_4main.

    CLEAR me->mt_predef_comm_data_main.
    IF me->ms_header_main IS INITIAL.
      TRY.
          me->mr_api->wb2_read_docu_header_single(
            EXPORTING
              i_contract_number  = me->m_main_contract
            IMPORTING
              es_header_data = me->ms_header_main
          ).
*          CATCH cx_bapi_error.
        CATCH cx_bapi_error INTO DATA(lx_error).
          RAISE EXCEPTION lx_error.
      ENDTRY.
    ENDIF.

    TRY.
        me->mt_predef_comm_data_main = me->mr_db->query_ztb_fqm_001( i_contract_type = me->ms_header_main-tctyp ).
*                                  CATCH cx_sy_sql_error.
      CATCH cx_sy_sql_error.
        MESSAGE e003(zfqm) WITH me->ms_header_main-tctyp INTO DATA(l_message).
        RAISE EXCEPTION TYPE cx_bapi_error
          EXPORTING
            status = VALUE #( ( id = sy-msgid
                                type = sy-msgty
                                number = sy-msgno
                                message = l_message
                                message_v1 = sy-msgv1
                                message_v2 = sy-msgv2
                                message_v3 = sy-msgv3
                                message_v4 = sy-msgv4 ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD log_error_messages.

    DATA(lr_log) = NEW zcl_acm_fqm_log(
      i_contract_number = me->m_contract_number
      i_event_type      = i_event_type
      i_agent           = i_agent
*           i_guid            =
      i_fail_indicator  = abap_true
    ).

    TRY.
        lr_log->add_messages( it_messages = ix_error->status ).
*              CATCH cx_bapi_error.
      CATCH cx_bapi_error INTO DATA(lx_error).
        RAISE EXCEPTION lx_error.
    ENDTRY.

  ENDMETHOD.


  METHOD log_error_messages_4main.

    DATA(lr_log) = NEW zcl_acm_fqm_log(
      i_contract_number = COND #( WHEN me->m_main_contract IS NOT INITIAL THEN me->m_main_contract
                                  ELSE me->m_contract_number )
      i_event_type      = i_event_type
      i_agent           = i_agent
*           i_guid            =
      i_fail_indicator  = abap_true
    ).

    TRY.
        lr_log->add_messages( it_messages = ix_error->status ).
*              CATCH cx_bapi_error.
      CATCH cx_bapi_error INTO DATA(lx_error).
        RAISE EXCEPTION lx_error.
    ENDTRY.

  ENDMETHOD.


  METHOD log_success_messages.

    DATA(lr_log) = NEW zcl_acm_fqm_log(
      i_contract_number = me->m_contract_number
      i_event_type      = i_event_type
      i_agent           = i_agent
*         i_guid            =
    ).

    TRY.
        lr_log->add_messages( it_messages = VALUE #( ( is_success_message ) )  ).
*       CATCH cx_bapi_error.
      CATCH cx_bapi_error INTO DATA(lx_error).
        RAISE EXCEPTION lx_error.
    ENDTRY.

  ENDMETHOD.


  METHOD log_success_messages_4main.

    DATA(lr_log) = NEW zcl_acm_fqm_log(
      i_contract_number = COND #( WHEN me->m_main_contract IS NOT INITIAL THEN me->m_main_contract
                                  ELSE me->m_contract_number )
      i_event_type      = i_event_type
      i_agent           = i_agent
*         i_guid            =
    ).

    TRY.
        lr_log->add_messages( it_messages = VALUE #( ( is_success_message ) )  ).
*       CATCH cx_bapi_error.
      CATCH cx_bapi_error INTO DATA(lx_error).
        RAISE EXCEPTION lx_error.
    ENDTRY.

  ENDMETHOD.



  METHOD check_call_off.

    CLEAR r_result.
    IF me->ms_header-tctyp IS INITIAL.
      TRY.
          me->set_header( ).
*          CATCH cx_bapi_error.
        CATCH cx_bapi_error.
          RETURN.
      ENDTRY.
    ENDIF.

    IF me->ms_header-tctyp(1) <> 'Y'. "Call-off Contract
      RETURN.
    ELSE.
      r_result = abap_true.
      DATA(lt_return) = me->send_changes_in_main_2fqm( i_agent = i_agent ).
    ENDIF.
  ENDMETHOD.


  METHOD set_main.
    DATA  l_count TYPE i.

    TRY.
        me->mr_db->query_doc_flow(
          EXPORTING
            i_call_off      = me->m_contract_number
          IMPORTING
            e_main_contract = me->m_main_contract
*            e_ref_doc       =
            e_ref_doc_type  = me->m_ref_doc_type
        ).
*        CATCH cx_sy_sql_error.
        EXIT.
      CATCH cx_sy_sql_error.
        IF l_count >= 1000.
          MESSAGE e014 INTO DATA(l_message).
          m_raise_bapi_error.
        ELSE.
          ADD 1 TO l_count.
          RETRY.
        ENDIF.
    ENDTRY.
  ENDMETHOD.


  METHOD get_reversed_amount.

    CLEAR r_reversed_amount.
    TRY.
        mr_db->query_reversals( EXPORTING i_contract_number = i_contract_number
                                          i_contract_item   = i_item
                                 IMPORTING et_reversals = DATA(lt_reversals) ).
      CATCH cx_sy_sql_error.
        RETURN.
    ENDTRY.

    LOOP AT lt_reversals ASSIGNING FIELD-SYMBOL(<ls_reversal>).
      DATA(lt_doc_flow) = zcl_acm_application=>get_document_flow(
                            i_application     = <ls_reversal>-application
                            i_application_itm = <ls_reversal>-application_itm
                          ).
      TRY.
          r_reversed_amount = r_reversed_amount + lt_doc_flow[ doc_typewb = 'Q' ]-valuewb +
                                                  lt_doc_flow[ doc_typewb = 'Q' ]-tax_valwb.
        CATCH cx_sy_itab_line_not_found ##NO_HANDLER.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.