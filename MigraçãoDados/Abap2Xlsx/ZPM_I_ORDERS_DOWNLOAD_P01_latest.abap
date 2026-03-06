*&---------------------------------------------------------------------*
*& Include zpm_i_orders_download_p01
*&---------------------------------------------------------------------*

CLASS lcl_media IMPLEMENTATION.
  METHOD confirm_filepathname.

  ENDMETHOD.

  METHOD free.

  ENDMETHOD.

  METHOD get_file_name.

  ENDMETHOD.

  METHOD open_file.

  ENDMETHOD.

  METHOD set_xlsx_file.

    me->mr_xlsx_file = ir_xlsx_file.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_windows IMPLEMENTATION.
  METHOD close_file.

  ENDMETHOD.

  METHOD constructor.

    super->constructor(  ).

    me->m_filename = i_filename.

  ENDMETHOD.

  METHOD open.

  ENDMETHOD.

  METHOD save_xlsx_file.

    DATA(lr_writer) = NEW zcl_excel_writer_2007(  ).
    DATA(l_xdata) = lr_writer->zif_excel_writer~write_file( io_excel = me->mr_xlsx_file ).
    DATA(lt_rawdata) = cl_bcs_convert=>xstring_to_solix( iv_xstring  = l_xdata ).
    DATA(l_bytecount) = xstrlen( l_xdata ).

    cl_gui_frontend_services=>gui_download( EXPORTING bin_filesize = l_bytecount
                                                      filename     = me->m_filename
                                                      filetype     = 'BIN'
                                             CHANGING data_tab     = lt_rawdata ).
  ENDMETHOD.

ENDCLASS.

CLASS lcl_pm_order IMPLEMENTATION.
  METHOD constructor.

    me->m_order_id = i_order_id.

    TRY.
        me->mr_log = NEW cl_cacs_bal(
*            i_handle              =
            i_log_name            = CONV #( me->m_order_id )
            i_object              = lcl_report=>mc_log-main_object
            i_subobject           = mc_log-sub_object
        ).
*          CATCH cx_cacs_bal_ex.  "
      CATCH cx_cacs_bal_ex ##NO_HANDLER.
    ENDTRY.

    me->mr_sap_api = NEW #(  ).
  ENDMETHOD.

  METHOD set_attributes.

    me->mr_sap_api->bapi_alm_order_get_detail(
      EXPORTING
        i_order_id                 = me->m_order_id
      IMPORTING
        es_header                  = me->ms_header
        es_service                 = me->ms_service
        es_ref_order_item          = me->ms_ref_order_item
        es_join_venture_accounting = me->ms_join_venture_accounting
        et_partners                = me->mt_partners
        et_operations              = me->mt_operations
        et_components              = me->mt_components
        et_settlement_rules        = me->mt_settlement_rules
        et_costs_sum               = me->mt_costs_sum
        et_costs_details           = me->mt_costs_details
    ).
*    CATCH cx_bapi_error.
  ENDMETHOD.

  METHOD rtti_structure_2header_values.

    CLEAR: et_header, et_values, et_fields.

    DATA(lr_sd) = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data( is_data ) ).
    DATA(lt_comp) = lr_sd->get_components( ).

    FIELD-SYMBOLS <f> TYPE any.

    LOOP AT lt_comp ASSIGNING FIELD-SYMBOL(<c>).

      "--- Header (tenta texto DDIC; senão usa o nome do componente)
      DATA(lv_head) = |{ <c>-name }|.

      TRY.
          DATA(lr_ed) = CAST cl_abap_elemdescr( <c>-type ).
          DATA(ls_dfies) = VALUE dfies(  ).
          lr_ed->get_ddic_field( RECEIVING p_flddescr = ls_dfies ).
          IF ls_dfies-scrtext_l IS NOT INITIAL.
            lv_head = ls_dfies-scrtext_l.
          ELSEIF ls_dfies-fieldtext IS NOT INITIAL.
            lv_head = ls_dfies-fieldtext.
          ENDIF.
        CATCH cx_sy_move_cast_error cx_sy_dyn_call_illegal_method.
          " mantém o nome do componente
      ENDTRY.

      APPEND lv_head TO et_header.
      APPEND |{ <c>-name }| TO et_fields.

      "--- Value
      ASSIGN COMPONENT <c>-name OF STRUCTURE is_data TO <f>.
      IF sy-subrc = 0.
        APPEND |{ <f> }| TO et_values.
      ELSE.
        APPEND `` TO et_values.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD rtti_table_to_header_rows.

    CLEAR: et_header, et_rows, et_fields.

    DATA(lr_td) = CAST cl_abap_tabledescr( cl_abap_typedescr=>describe_by_data( it_data ) ).
    DATA(lr_line) = lr_td->get_table_line_type( ).

    " Assume line type estruturado (é o seu caso: BAPI_*_T/ET)
    DATA(lr_sd) = CAST cl_abap_structdescr( lr_line ).
    DATA(lt_comp) = lr_sd->get_components( ).

    " Header
    LOOP AT lt_comp ASSIGNING FIELD-SYMBOL(<c>).

      DATA(lv_head) = |{ <c>-name }|.

      TRY.
          DATA(lr_ed) = CAST cl_abap_elemdescr( <c>-type ).
          DATA(ls_dfies) = VALUE dfies( ).
          lr_ed->get_ddic_field( RECEIVING p_flddescr = ls_dfies ).
          IF ls_dfies-scrtext_l IS NOT INITIAL.
            lv_head = ls_dfies-scrtext_l.
          ELSEIF ls_dfies-fieldtext IS NOT INITIAL.
            lv_head = ls_dfies-fieldtext.
          ENDIF.
        CATCH cx_sy_move_cast_error cx_sy_dyn_call_illegal_method.
      ENDTRY.

      APPEND lv_head TO et_header.
      APPEND |{ <c>-name }| TO et_fields.

    ENDLOOP.

    " Rows
    FIELD-SYMBOLS <ls_line> TYPE any.
    FIELD-SYMBOLS <f>       TYPE any.

    LOOP AT it_data ASSIGNING <ls_line>.
      DATA(lt_row) = VALUE string_table( ).

      LOOP AT lt_comp ASSIGNING <c>.
        ASSIGN COMPONENT <c>-name OF STRUCTURE <ls_line> TO <f>.
        IF sy-subrc = 0.
          APPEND |{ <f> }| TO lt_row.
        ELSE.
          APPEND `` TO lt_row.
        ENDIF.
      ENDLOOP.

      APPEND lt_row TO et_rows.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_header_data.
    rtti_structure_2header_values(
      EXPORTING
        is_data   = me->ms_header
      IMPORTING
        et_header = et_header
        et_values = et_values
        et_fields = et_fields
    ).
    " ORDERID sempre como 1ª coluna
    INSERT `ORDERID` INTO et_header INDEX 1.
    INSERT `ORDERID` INTO et_fields INDEX 1.
    INSERT |{ me->m_order_id }| INTO et_values INDEX 1.
  ENDMETHOD.


  METHOD get_service_data.
    rtti_structure_2header_values(
      EXPORTING is_data   = me->ms_service
      IMPORTING et_header = et_header
                et_values = et_values
                et_fields = et_fields
    ).
    INSERT `ORDERID` INTO et_header INDEX 1.
    INSERT `ORDERID` INTO et_fields INDEX 1.
    INSERT |{ me->m_order_id }| INTO et_values INDEX 1.
  ENDMETHOD.


  METHOD get_ref_order_item_data.
    rtti_structure_2header_values(
      EXPORTING is_data   = me->ms_ref_order_item
      IMPORTING et_header = et_header
                et_values = et_values
                et_fields = et_fields
    ).
    INSERT `ORDERID` INTO et_header INDEX 1.
    INSERT `ORDERID` INTO et_fields INDEX 1.
    INSERT |{ me->m_order_id }| INTO et_values INDEX 1.
  ENDMETHOD.


  METHOD get_jva_data.
    rtti_structure_2header_values(
      EXPORTING is_data   = me->ms_join_venture_accounting
      IMPORTING et_header = et_header
                et_values = et_values
                et_fields = et_fields
    ).
    INSERT `ORDERID` INTO et_header INDEX 1.
    INSERT `ORDERID` INTO et_fields INDEX 1.
    INSERT |{ me->m_order_id }| INTO et_values INDEX 1.
  ENDMETHOD.


  METHOD get_partners_data.
    rtti_table_to_header_rows(
      EXPORTING it_data   = me->mt_partners
      IMPORTING et_header = et_header
                et_rows   = et_rows
                et_fields = et_fields
    ).
    INSERT `ORDERID` INTO et_header INDEX 1.
    INSERT `ORDERID` INTO et_fields INDEX 1.
    LOOP AT et_rows ASSIGNING FIELD-SYMBOL(<r>).
      INSERT |{ me->m_order_id }| INTO <r> INDEX 1.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_operations_data.
    rtti_table_to_header_rows(
      EXPORTING it_data   = me->mt_operations
      IMPORTING et_header = et_header
                et_rows   = et_rows
                et_fields = et_fields
    ).
    INSERT `ORDERID` INTO et_header INDEX 1.
    INSERT `ORDERID` INTO et_fields INDEX 1.
    LOOP AT et_rows ASSIGNING FIELD-SYMBOL(<r>).
      INSERT |{ me->m_order_id }| INTO <r> INDEX 1.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_components_data.
    rtti_table_to_header_rows(
      EXPORTING it_data   = me->mt_components
      IMPORTING et_header = et_header
                et_rows   = et_rows
                et_fields = et_fields
    ).
    INSERT `ORDERID` INTO et_header INDEX 1.
    INSERT `ORDERID` INTO et_fields INDEX 1.
    LOOP AT et_rows ASSIGNING FIELD-SYMBOL(<r>).
      INSERT |{ me->m_order_id }| INTO <r> INDEX 1.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_settlement_rules_data.
    rtti_table_to_header_rows(
      EXPORTING it_data   = me->mt_settlement_rules
      IMPORTING et_header = et_header
                et_rows   = et_rows
                et_fields = et_fields
    ).
    INSERT `ORDERID` INTO et_header INDEX 1.
    INSERT `ORDERID` INTO et_fields INDEX 1.
    LOOP AT et_rows ASSIGNING FIELD-SYMBOL(<r>).
      INSERT |{ me->m_order_id }| INTO <r> INDEX 1.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_costs_sum_data.
    rtti_table_to_header_rows(
      EXPORTING it_data   = me->mt_costs_sum
      IMPORTING et_header = et_header
                et_rows   = et_rows
                et_fields = et_fields
    ).
    INSERT `ORDERID` INTO et_header INDEX 1.
    INSERT `ORDERID` INTO et_fields INDEX 1.
    LOOP AT et_rows ASSIGNING FIELD-SYMBOL(<r>).
      INSERT |{ me->m_order_id }| INTO <r> INDEX 1.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_costs_details_data.
    rtti_table_to_header_rows(
      EXPORTING it_data   = me->mt_costs_details
      IMPORTING et_header = et_header
                et_rows   = et_rows
                et_fields = et_fields
    ).
    INSERT `ORDERID` INTO et_header INDEX 1.
    INSERT `ORDERID` INTO et_fields INDEX 1.
    LOOP AT et_rows ASSIGNING FIELD-SYMBOL(<r>).
      INSERT |{ me->m_order_id }| INTO <r> INDEX 1.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_sap_api IMPLEMENTATION.
  METHOD bapi_alm_order_get_detail.

    DATA(lt_return) = VALUE bapiret2_t(  ).
    CALL FUNCTION 'BAPI_ALM_ORDER_GET_DETAIL'
      EXPORTING
        number           = i_order_id
      IMPORTING
        es_header        = es_header
        es_srvdata       = es_service
        es_reforder_item = es_ref_order_item
        es_jva_data      = es_join_venture_accounting
      TABLES
        et_partner       = et_partners
        et_operations    = et_operations
        et_components    = et_components
*       et_relations     =
        et_srules        = et_settlement_rules
*       et_olist         =
*       et_oprol         =
*       et_texts         =
*       et_text_lines    =
*       et_prts          =
        et_costs_sum     = et_costs_sum
        et_costs_details = et_costs_details
        return           = lt_return
*       extension_in     =
*       extension_out    =
*       et_reforder_serno_olist  =
*       et_serviceoutline        =
*       et_servicelines  =
*       et_servicelimit  =
*       et_servicecontractlimits =
*       et_permit        =
*       et_permit_issue  =
*       et_additional_texts      =
      .
    IF line_exists( lt_return[ type = 'E' ] ) OR line_exists( lt_return[ type = 'A' ] ).
      RAISE EXCEPTION TYPE CX_bapi_error
        EXPORTING
          status = lt_return.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_report IMPLEMENTATION.
  METHOD constructor.

    TRY.
        me->mr_log = NEW cl_cacs_bal(
*            i_handle              =
            i_log_name            = CONV #( sy-repid && sy-datum && sy-uzeit )
            i_object              = mc_log-main_object
            i_subobject           = mc_log-sub_object
        ).
*          CATCH cx_cacs_bal_ex.  "
      CATCH cx_cacs_bal_ex ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

  METHOD write_header_if_needed.

    IF cv_hdr_written = abap_true.
      RETURN.
    ENDIF.

    DATA(lv_col) = 1.

    " Linha 1: descri絥s
    LOOP AT it_header ASSIGNING FIELD-SYMBOL(<h>).
      ir_ws->set_cell(
        ip_column = zcl_excel_common=>convert_column2alpha( lv_col )
        ip_row    = 1
        ip_value  = <h>
      ).
      lv_col += 1.
    ENDLOOP.

    " Linha 2: nomes t飮icos (se informado)
    IF it_fields IS NOT INITIAL.
      lv_col = 1.
      LOOP AT it_fields ASSIGNING FIELD-SYMBOL(<f>).
        ir_ws->set_cell(
          ip_column = zcl_excel_common=>convert_column2alpha( lv_col )
          ip_row    = 2
          ip_value  = <f>
        ).
        lv_col += 1.
      ENDLOOP.
    ENDIF.

    cv_hdr_written = abap_true.

  ENDMETHOD.


  METHOD write_rows.

    FIELD-SYMBOLS <row> TYPE string_table.
    FIELD-SYMBOLS <val> TYPE string.

    LOOP AT it_rows ASSIGNING <row>.
      DATA(lv_col) = 1.

      LOOP AT <row> ASSIGNING <val>.
        ir_ws->set_cell(
          ip_column = zcl_excel_common=>convert_column2alpha( lv_col )
          ip_row    = cv_next_row
          ip_value  = <val>
        ).
        lv_col += 1.
      ENDLOOP.

      cv_next_row += 1.
    ENDLOOP.

  ENDMETHOD.

  METHOD convert_to_xlsx.
    DATA lt_tfields TYPE string_table.

    DATA(lr_excel) = NEW zcl_excel( ).
    TRY.
        set_worksheets( lr_excel ).
      CATCH zcx_excel INTO DATA(lx_error).
        MESSAGE lx_error->if_message~get_text( ) TYPE 'E' DISPLAY LIKE 'I'.
        RETURN.
    ENDTRY.

    " Guardar no media (para o DOWNLOAD_XLSX usar depois)
    IF me->mr_media IS BOUND.
      me->mr_media->set_xlsx_file( ir_xlsx_file = lr_excel ).
    ENDIF.

    " Worksheets
    DATA(lr_ws_header)    = lr_excel->get_worksheet_by_name( 'Header' ).
    DATA(lr_ws_service)   = lr_excel->get_worksheet_by_name( 'Service Spec' ).
    DATA(lr_ws_refitem)   = lr_excel->get_worksheet_by_name( 'Ref. Ord. Item' ).
    DATA(lr_ws_jva)       = lr_excel->get_worksheet_by_name( 'JVA' ).
    DATA(lr_ws_partners)  = lr_excel->get_worksheet_by_name( 'Partners' ).
    DATA(lr_ws_oper)      = lr_excel->get_worksheet_by_name( 'Operations' ).
    DATA(lr_ws_comp)      = lr_excel->get_worksheet_by_name( 'Components' ).
    DATA(lr_ws_settl)     = lr_excel->get_worksheet_by_name( 'Settlement Rules' ).
    DATA(lr_ws_csum)      = lr_excel->get_worksheet_by_name( 'Costs Sum' ).
    DATA(lr_ws_cdet)      = lr_excel->get_worksheet_by_name( 'Costs Details' ).

    " Controle de cabeçalho + linha atual por aba
    DATA: lv_h_header  TYPE abap_bool VALUE abap_false,
          lv_h_service TYPE abap_bool VALUE abap_false,
          lv_h_refitem TYPE abap_bool VALUE abap_false,
          lv_h_jva     TYPE abap_bool VALUE abap_false,
          lv_h_part    TYPE abap_bool VALUE abap_false,
          lv_h_oper    TYPE abap_bool VALUE abap_false,
          lv_h_comp    TYPE abap_bool VALUE abap_false,
          lv_h_settl   TYPE abap_bool VALUE abap_false,
          lv_h_csum    TYPE abap_bool VALUE abap_false,
          lv_h_cdet    TYPE abap_bool VALUE abap_false.

    DATA: lv_r_header  TYPE i VALUE 3,
          lv_r_service TYPE i VALUE 3,
          lv_r_refitem TYPE i VALUE 3,
          lv_r_jva     TYPE i VALUE 3,
          lv_r_part    TYPE i VALUE 3,
          lv_r_oper    TYPE i VALUE 3,
          lv_r_comp    TYPE i VALUE 3,
          lv_r_settl   TYPE i VALUE 3,
          lv_r_csum    TYPE i VALUE 3,
          lv_r_cdet    TYPE i VALUE 3.

    LOOP AT me->mt_orders ASSIGNING FIELD-SYMBOL(<ls_order>).

      " Buscar dados SAP
      TRY.
          <ls_order>-instance->set_attributes( ).
        CATCH cx_bapi_error INTO DATA(lx_bapi_error).
          IF me->mr_log IS BOUND.
            LOOP AT lx_bapi_error->status ASSIGNING FIELD-SYMBOL(<ls_error>).
              TRY.
                  me->mr_log->add_message(
                    is_balmsg = VALUE #(
                      msgid = <ls_error>-id
                      msgty = <ls_error>-type
                      msgno = <ls_error>-number
                      msgv1 = <ls_error>-message_v1
                      msgv2 = <ls_error>-message_v2
                      msgv3 = <ls_error>-message_v3
                      msgv4 = <ls_error>-message_v4
                    )
                  ).
                CATCH cx_cacs_bal_ex.
                  CONTINUE.
              ENDTRY.
            ENDLOOP.
          ENDIF.
          CONTINUE.
      ENDTRY.

      "========================
      " Estruturas (1 linha por ordem)
      "========================
      DATA lt_head   TYPE string_table.
      DATA lt_fields TYPE string_table.
      DATA lt_vals   TYPE string_table.

      " Header
      CLEAR: lt_head, lt_fields, lt_vals.
      <ls_order>-instance->get_header_data( IMPORTING et_header = lt_head et_values = lt_vals et_fields = lt_fields ).
      write_header_if_needed( EXPORTING ir_ws = lr_ws_header it_header = lt_head it_fields = lt_fields
                             CHANGING  cv_hdr_written = lv_h_header ).
      write_rows( EXPORTING ir_ws = lr_ws_header it_rows = VALUE lcl_pm_order=>ty_rows( ( lt_vals ) )
                 CHANGING  cv_next_row = lv_r_header ).

      " Service Spec
      CLEAR: lt_head, lt_fields, lt_vals.
      <ls_order>-instance->get_service_data( IMPORTING et_header = lt_head et_values = lt_vals et_fields = lt_fields ).
      write_header_if_needed( EXPORTING ir_ws = lr_ws_service it_header = lt_head it_fields = lt_fields
                             CHANGING  cv_hdr_written = lv_h_service ).
      write_rows( EXPORTING ir_ws = lr_ws_service it_rows = VALUE lcl_pm_order=>ty_rows( ( lt_vals ) )
                 CHANGING  cv_next_row = lv_r_service ).

      " Ref. Ord. Item
      CLEAR: lt_head, lt_fields, lt_vals.
      <ls_order>-instance->get_ref_order_item_data( IMPORTING et_header = lt_head et_values = lt_vals et_fields = lt_fields ).
      write_header_if_needed( EXPORTING ir_ws = lr_ws_refitem it_header = lt_head it_fields = lt_fields
                             CHANGING  cv_hdr_written = lv_h_refitem ).
      write_rows( EXPORTING ir_ws = lr_ws_refitem it_rows = VALUE lcl_pm_order=>ty_rows( ( lt_vals ) )
                 CHANGING  cv_next_row = lv_r_refitem ).

      " JVA
      CLEAR: lt_head, lt_fields, lt_vals.
      <ls_order>-instance->get_jva_data( IMPORTING et_header = lt_head et_values = lt_vals et_fields = lt_fields ).
      write_header_if_needed( EXPORTING ir_ws = lr_ws_jva it_header = lt_head it_fields = lt_fields
                             CHANGING  cv_hdr_written = lv_h_jva ).
      write_rows( EXPORTING ir_ws = lr_ws_jva it_rows = VALUE lcl_pm_order=>ty_rows( ( lt_vals ) )
                 CHANGING  cv_next_row = lv_r_jva ).

      "========================
      " Tabelas (N linhas por ordem)
      "========================
      DATA lt_thead TYPE string_table.
      DATA lt_rows  TYPE lcl_pm_order=>ty_rows.

      " Partners
      CLEAR: lt_thead, lt_rows, lt_tfields.
      <ls_order>-instance->get_partners_data(
        IMPORTING
          et_header = lt_thead
          et_rows   = lt_rows
          et_fields = lt_tfields
      ).
      write_header_if_needed(
        EXPORTING
          ir_ws     = lr_ws_partners
          it_header = lt_thead
          it_fields = lt_tfields
        CHANGING
          cv_hdr_written = lv_h_part
      ).
      write_rows( EXPORTING ir_ws = lr_ws_partners it_rows = lt_rows
                 CHANGING  cv_next_row = lv_r_part ).

      " Operations
      CLEAR: lt_thead, lt_rows, lt_tfields.
      <ls_order>-instance->get_operations_data(
        IMPORTING et_header = lt_thead
                  et_rows   = lt_rows
                  et_fields = lt_tfields
      ).
      write_header_if_needed(
        EXPORTING ir_ws     = lr_ws_oper
                  it_header = lt_thead
                  it_fields = lt_tfields
        CHANGING  cv_hdr_written = lv_h_oper
      ).
      write_rows( EXPORTING ir_ws = lr_ws_oper it_rows = lt_rows
                 CHANGING  cv_next_row = lv_r_oper ).

      " Components
      CLEAR: lt_thead, lt_rows, lt_tfields.
      <ls_order>-instance->get_components_data(
       IMPORTING et_header = lt_thead
                 et_rows   = lt_rows
                 et_fields = lt_tfields
      ).
      write_header_if_needed(
        EXPORTING ir_ws     = lr_ws_comp
                  it_header = lt_thead
                  it_fields = lt_tfields
        CHANGING  cv_hdr_written = lv_h_comp
      ).
      write_rows( EXPORTING ir_ws = lr_ws_comp it_rows = lt_rows
                 CHANGING  cv_next_row = lv_r_comp ).

      " Settlement Rules
      CLEAR: lt_thead, lt_rows, lt_tfields.
      <ls_order>-instance->get_settlement_rules_data(
       IMPORTING et_header = lt_thead
                 et_rows   = lt_rows
                 et_fields = lt_tfields
      ).
      write_header_if_needed(
        EXPORTING ir_ws     =  lr_ws_settl
                  it_header = lt_thead
                  it_fields = lt_tfields
        CHANGING  cv_hdr_written = lv_h_settl
      ).
      write_rows( EXPORTING ir_ws = lr_ws_settl it_rows = lt_rows
                 CHANGING  cv_next_row = lv_r_settl ).

      " Costs Sum
      CLEAR: lt_thead, lt_rows, lt_tfields.
      <ls_order>-instance->get_costs_sum_data(
       IMPORTING et_header = lt_thead
                 et_rows   = lt_rows
                 et_fields = lt_tfields
      ).
      write_header_if_needed(
        EXPORTING ir_ws     = lr_ws_csum
                  it_header = lt_thead
                  it_fields = lt_tfields
        CHANGING  cv_hdr_written = lv_h_csum
      ).
      write_rows( EXPORTING ir_ws = lr_ws_csum it_rows = lt_rows
                 CHANGING  cv_next_row = lv_r_csum ).

      CLEAR: lt_thead, lt_rows, lt_tfields.
      <ls_order>-instance->get_costs_details_data(
       IMPORTING et_header = lt_thead
                 et_rows   = lt_rows
                 et_fields = lt_tfields
      ).
      write_header_if_needed(
        EXPORTING ir_ws     = lr_ws_cdet
                  it_header = lt_thead
                  it_fields = lt_tfields
        CHANGING  cv_hdr_written = lv_h_cdet
      ).
      write_rows( EXPORTING ir_ws = lr_ws_cdet it_rows = lt_rows
                 CHANGING  cv_next_row = lv_r_cdet ).

    ENDLOOP.

  ENDMETHOD.

  METHOD download_xlsx.

    TRY.
        me->mr_media->save_xlsx_file( ).
*    CATCH cx_bapi_error.
      CATCH cx_bapi_error INTO DATA(lx_error).
    ENDTRY.
  ENDMETHOD.

  METHOD get_orders.

    SELECT FROM aufk
      FIELDS aufnr
     WHERE aufnr IN @s_order[]
       AND auart IN @s_otype[]
       AND werks IN @s_mplan[]
     ORDER BY PRIMARY KEY
      INTO TABLE @DATA(lt_orders).

    LOOP AT lt_orders ASSIGNING FIELD-SYMBOL(<l_order>).
      APPEND VALUE #( instance = NEW lcl_pm_order( i_order_id = CONV #( <l_order> ) ) ) TO me->mt_orders.
    ENDLOOP.
  ENDMETHOD.

  METHOD run.

    me->get_orders( ).
    TRY.
        me->convert_to_xlsx( ).
      CATCH zcx_excel INTO DATA(lx_error).
        MESSAGE lx_error->if_message~get_text( ) TYPE 'E' DISPLAY LIKE 'I'.
        LEAVE TO CURRENT TRANSACTION.
    ENDTRY.
    me->download_xlsx( ).
  ENDMETHOD.


  METHOD set_worksheets.

    ir_excel->add_new_worksheet( 'Header').
*    CATCH zcx_excel.
    ir_excel->add_new_worksheet( 'Service Spec').
*    CATCH zcx_excel.
    ir_excel->add_new_worksheet( 'Ref. Ord. Item').
*    CATCH zcx_excel.
    ir_excel->add_new_worksheet( 'JVA').
*    CATCH zcx_excel.
    ir_excel->add_new_worksheet( 'Partners').
*    CATCH zcx_excel.
    ir_excel->add_new_worksheet( 'Operations').
*    CATCH zcx_excel.
    ir_excel->add_new_worksheet( 'Components').
*    CATCH zcx_excel.
    ir_excel->add_new_worksheet( 'Settlement Rules').
*    CATCH zcx_excel.
    ir_excel->add_new_worksheet( 'Costs Sum').
*    CATCH zcx_excel.
    ir_excel->add_new_worksheet( 'Costs Details').
*    CATCH zcx_excel.
  ENDMETHOD.

  METHOD get_field_value.
    DATA  lt_dynpfields TYPE dynpread_tabtype.
    FIELD-SYMBOLS  <ls_dynpfield>  TYPE dynpread.

    APPEND  i_fieldname TO lt_dynpfields.

    CALL FUNCTION 'DYNP_VALUES_READ'
      EXPORTING
        dyname               = sy-repid
        dynumb               = sy-dynnr
      TABLES
        dynpfields           = lt_dynpfields
      EXCEPTIONS
        invalid_abapworkarea = 1
        invalid_dynprofield  = 2
        invalid_dynproname   = 3
        invalid_dynpronummer = 4
        invalid_request      = 5
        no_fielddescription  = 6
        invalid_parameter    = 7
        undefind_error       = 8
        double_conversion    = 9
        stepl_not_found      = 10
        OTHERS               = 11.
    IF sy-subrc <> 0.
*      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      RETURN.
    ENDIF.

    READ TABLE lt_dynpfields ASSIGNING <ls_dynpfield>
     WITH KEY fieldname = i_fieldname.
    IF sy-subrc = 0.
      r_fvalue = <ls_dynpfield>-fieldvalue.
    ENDIF.
  ENDMETHOD.

ENDCLASS.