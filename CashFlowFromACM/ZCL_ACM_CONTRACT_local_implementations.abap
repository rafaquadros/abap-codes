*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
CLASS lcl_api IMPLEMENTATION.

 METHOD wb2_read_docu_header_single.

    CLEAR es_header_data.
    CALL FUNCTION 'WB2_READ_DOCU_HEADER_SINGLE'
      EXPORTING
        i_tkonn            = i_contract_number
        i_bypassing_buffer = abap_true
*       i_refresh_buffer   =
      IMPORTING
        e_komwbhk          = es_header_data
      EXCEPTIONS
        no_record_found    = 1
        OTHERS             = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
        INTO DATA(l_message).

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
    ENDIF.
  ENDMETHOD.

  METHOD fqm_release_cfg_read.

    CLEAR rt_release_criterial.
    CALL FUNCTION 'FQM_RELEASE_CFG_READ'
      IMPORTING
        et_result = rt_release_criterial.
  ENDMETHOD.

  METHOD fqm_aif_remote_update.

    DATA(lt_return) = VALUE bapiret2_t(  ).
    CALL FUNCTION 'FQM_AIF_REMOTE_UPDATE'
      EXPORTING
*       testrun    =
        operation  = i_operation
        flows      = it_flows
      TABLES
        return_tab = lt_return.
    IF line_exists( lt_return[ type = 'A' ] ) OR line_exists( lt_return[ type = 'E' ] ).
      RAISE EXCEPTION TYPE cx_bapi_error
        EXPORTING
          status = lt_return.
    ENDIF.
  ENDMETHOD.

  METHOD wb2_read_docu_item_with_tkonn.

    CLEAR et_items.
    CALL FUNCTION 'WB2_READ_DOCU_ITEM_WITH_TKONN'
      EXPORTING
        i_tkonn            = i_contract_number
        i_bypassing_buffer = abap_true
*       i_refresh_buffer   =
      TABLES
        t_komwbhi          = et_items
      EXCEPTIONS
        no_records_found   = 1
        OTHERS             = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
        INTO DATA(l_message).

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
    ENDIF.
  ENDMETHOD.

  METHOD wb2_read_busi_data_with_tkonn.

    CLEAR et_business_data.
    CALL FUNCTION 'WB2_READ_BUSI_DATA_WITH_TKONN'
      EXPORTING
        i_tkonn          = i_contract_number
*       i_bypassing_buffer =
*       i_refresh_buffer =
      TABLES
        t_komwbhd        = et_business_data
      EXCEPTIONS
        no_records_found = 1
        OTHERS           = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
        INTO DATA(l_message).
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
    ENDIF.
  ENDMETHOD.

  METHOD zfqm_send_changes.

    CALL FUNCTION 'ZFQM_SEND_CHANGES'
      IN BACKGROUND TASK AS SEPARATE UNIT
      EXPORTING
        i_contract_number = i_contract_number.
  ENDMETHOD.

  METHOD zfqm_send_created.

    CALL FUNCTION 'ZFQM_SEND_CREATED'
      IN BACKGROUND TASK AS SEPARATE UNIT
      EXPORTING
        i_contract_number = i_contract_number.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_db IMPLEMENTATION.

  METHOD query_prctr_in_marc.

    CLEAR r_profit_center.
    SELECT prctr UP TO 1 ROWS
      FROM marc
      INTO r_profit_center
     WHERE matnr = i_material
       AND werks = i_plant
     ORDER BY PRIMARY KEY.
    ENDSELECT.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_sql_error.
    ENDIF.
  ENDMETHOD.

  METHOD query_ztb_fqm_001.

    CLEAR rt_ztbfqm_001.
    SELECT contract_type, material, certainty_level, planning_level, liquidity_item
      FROM ztbfqm_001
      INTO TABLE @rt_ztbfqm_001
     WHERE contract_type = @i_contract_type.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_sql_error.
    ENDIF.
  ENDMETHOD.

  METHOD query_fdgrv_in_lfb1.

    CLEAR r_planing_group.
    SELECT fdgrv UP TO 1 ROWS
      FROM lfb1
      INTO @r_planing_group
     WHERE lifnr = @i_vendor_id
     ORDER BY PRIMARY KEY.
    ENDSELECT.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_sql_error.
    ENDIF.
  ENDMETHOD.

  METHOD query_FDGRV_in_KNB1.
    CLEAR r_planing_group.
    SELECT fdgrv UP TO 1 ROWS
      FROM knb1
      INTO @r_planing_group
     WHERE kunnr = @i_customer_id
     ORDER BY PRIMARY KEY.
    ENDSELECT.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_sql_error.
    ENDIF.
  ENDMETHOD.

  METHOD query_doc_flow.

    CLEAR: e_main_contract, e_ref_doc, e_ref_doc_type.
    SELECT contract_num, tkonn_to, refdoc_type UP TO 1 ROWS
      FROM wbhf AS flow
     INNER JOIN /accgo/t_appdata AS aplk
        ON aplk~refdoc = flow~tkonn_to
       "AND aplk~refdoc_item = flow~tposn_to
      INTO (@e_main_contract, @e_ref_doc, @e_ref_doc_type)
     WHERE flow~tkonn_from = @i_call_off.
    ENDSELECT.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_sql_error.
    ENDIF.
  ENDMETHOD.

  METHOD query_used_value_from_po.

    CLEAR r_used_amount.
    SELECT SUM( netwr )
      FROM /accgo/t_appdata AS aplk
     INNER JOIN ekpo AS poit
        ON poit~ebeln = aplk~refdoc
       AND concat( '00000', poit~ebelp ) = aplk~refdoc_item
     WHERE contract_num = @i_contract_number
       AND contract_item = @i_contract_item
       INTO @r_used_amount.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_sql_error.
    ENDIF.
  ENDMETHOD.

  METHOD query_used_value_from_so.

    CLEAR r_used_amount.
    SELECT SUM( netwr + mwsbp )
      FROM /accgo/t_appdata AS aplk
     INNER JOIN vbap AS soit
        ON soit~vbeln = aplk~refdoc
       AND concat( '0000', soit~posnr ) = aplk~refdoc_item
     WHERE contract_num = @i_contract_number
       AND contract_item = @i_contract_item
      INTO @r_used_amount.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_sql_error.
    ENDIF.
  ENDMETHOD.

  METHOD query_reversals.

    CLEAR et_reversals.
    SELECT refdoc, refdoc_item
      FROM /accgo/t_appdata
     WHERE refdoc_type   = 'ZA'
       AND contract_num  = @i_contract_number
       AND contract_item = @i_contract_item
       AND appl_status   = '9'
      INTO TABLE @et_reversals.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_sql_error.
    ENDIF.

  ENDMETHOD.

ENDCLASS.