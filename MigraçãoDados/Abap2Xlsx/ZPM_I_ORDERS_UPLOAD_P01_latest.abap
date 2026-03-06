*&---------------------------------------------------------------------*
*& Include zpm_i_orders_upload_p01
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

ENDCLASS.

CLASS lcl_windows IMPLEMENTATION.
  METHOD close_file.

  ENDMETHOD.

  METHOD constructor.
    DATA: lt_filetable TYPE filetable,
          l_rc         TYPE i.
    FIELD-SYMBOLS  <ls_fileline>  TYPE file_table.

    super->constructor( ).

    IF i_filename IS INITIAL.
      CALL METHOD cl_gui_frontend_services=>file_open_dialog
        CHANGING
          file_table              = lt_filetable
          rc                      = l_rc
        EXCEPTIONS
          file_open_dialog_failed = 1
          cntl_error              = 2
          error_no_gui            = 3
          not_supported_by_gui    = 4
          OTHERS                  = 5.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
      READ TABLE lt_filetable INDEX 1 ASSIGNING <ls_fileline>.
      IF sy-subrc = 0.
        me->m_filename = <ls_fileline>-filename.
      ENDIF.
    ELSE.
      m_filename = i_filename.
    ENDIF.
  ENDMETHOD.

  METHOD open_file.

    me->open( ).
*    CATCH cx_bapi_error.
  ENDMETHOD.

  METHOD open.

    DATA(lr_xlsx_reader) = CAST zif_excel_reader( NEW zcl_excel_reader_2007(  ) ).

    me->mr_xlsx_file = lr_xlsx_reader->load_file(
                         i_filename             = me->m_filename
*                         i_use_alternate_zip    = space
*                         i_from_applserver      = SY-BATCH
*                         iv_zcl_excel_classname =
                       ).
  ENDMETHOD.

ENDCLASS.

CLASS lcl_sap_api IMPLEMENTATION.
  METHOD bapi_alm_order_maintain.

    DATA(lt_return) = VALUE bapiret2_t(  ).
    DATA(lt_methods) = it_methods.
    DATA(lt_headers) = it_headers.
    DATA(lt_headers_upd) = it_headers_upd.
    DATA(lt_services) = it_services.
    DATA(lt_services_upd) = it_services_upd.
    DATA(lt_partners) = it_partners.
    DATA(lt_partners_upd) = it_partners_upd.
    DATA(lt_operations) = it_operations.
    DATA(lt_operations_upd) = it_operations_upd.
    DATA(lt_components) = it_components.
    DATA(lt_components_upd) = it_components_upd.
    DATA(lt_settlement_rules) = it_settlement_rules.
    DATA(lt_settlement_rules_upd) = it_settlement_rules_upd.
    DATA(lt_reforder_items) = it_reforder_items.
    DATA(lt_reforder_items_upd) = it_reforder_items_upd.
    DATA(lt_jva) = it_jva.
    DATA(lt_jva_upd) = it_jva_upd.

    CALL FUNCTION 'BAPI_ALM_ORDER_MAINTAIN'
*      EXPORTING
*        iv_mmsrv_external_maintenace =
      TABLES
        it_methods          = lt_methods
        it_header           = lt_headers
        it_header_up        = lt_headers_upd
        it_header_srv       = lt_services
        it_header_srv_up    = lt_services_upd
*       it_userstatus       =
        it_partner          = lt_partners
        it_partner_up       = lt_partners_upd
        it_operation        = lt_operations
        it_operation_up     = lt_operations_upd
*       it_relation         =
*       it_relation_up      =
        it_component        = lt_components
        it_component_up     = lt_components_upd
*       it_objectlist       =
*       it_objectlist_up    =
*       it_olist_relation   =
*       it_text             =
*       it_text_lines       =
        it_srule            = lt_settlement_rules
        it_srule_up         = lt_settlement_rules_upd
*       it_tasklists        =
*       extension_in        =
        return              = lt_return
        et_numbers          = rt_orders
        it_reforder_item    = lt_reforder_items
        it_reforder_item_up = lt_reforder_items_upd
*       it_reforder_serno_olist_ins  =
*       it_reforder_serno_olist_del  =
*       it_prt              =
*       it_prt_up           =
*       it_reforder_operation        =
*       it_serviceoutline   =
*       it_serviceoutline_up         =
*       it_servicelines     =
*       it_servicelines_up  =
*       it_servicelimit     =
*       it_servicelimit_up  =
*       it_servicecontractlimits     =
*       it_servicecontractlimits_up  =
*       et_notification_numbers      =
*       it_permit           =
*       it_permit_up        =
*       it_permit_issue     =
*       it_estimated_costs  =
        it_header_jva       = lt_jva
        it_header_jva_up    = lt_jva_upd
*       it_additional_text  =
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

  METHOD run.
    me->upload_xlsx( ).
    me->parse_xlsx( ).
    me->create_pm_orders( ).

    me->mr_log->display(
*      i_single            =
*      i_popup             =
*      i_grid              =
*      i_toolbar           = 'X'
*      i_bydetlevel        =
*      i_treesize          = 10
*      i_sort_by_probclass = abap_false
*      i_start_col         =
*      i_start_row         =
*      i_end_col           =
*      i_end_row           =
*      i_title             =
    ).
*    CATCH cx_cacs_bal_ex.
  ENDMETHOD.

  METHOD upload_xlsx.

    me->mr_media->open_file( ).
*    CATCH cx_bapi_error.
  ENDMETHOD.


  METHOD create_pm_orders.

    TRY.
        DATA(lt_orders) = NEW lcl_sap_api( )->bapi_alm_order_maintain(
          it_methods              = me->mt_methods
          it_headers              = me->mt_headers
          it_headers_upd          = me->mt_headers_upd
          it_services             = me->mt_services
          it_services_upd         = me->mt_services_upd
          it_reforder_items       = me->mt_reforder_items
          it_reforder_items_upd   = me->mt_reforder_items_upd
          it_jva                  = me->mt_jva
          it_jva_upd              = me->mt_jva_upd
          it_partners             = me->mt_partners
          it_partners_upd         = me->mt_partners_upd
          it_operations           = me->mt_operations
          it_operations_upd       = me->mt_operations_upd
          it_components           = me->mt_components
          it_components_upd       = me->mt_components_upd
          it_settlement_rules     = me->mt_settlement_rules
          it_settlement_rules_upd = me->mt_settlement_rules_upd
        ).
*    CATCH cx_bapi_error.
        LOOP AT lt_orders ASSIGNING FIELD-SYMBOL(<ls_order>).
          MESSAGE i019 WITH <ls_order>-aufnr_new <ls_order>-aufnr_in INTO DATA(l_message).
          me->mr_log->add_message( is_balmsg = CORRESPONDING #( syst ) ).
*          CATCH cx_cacs_bal_ex.
        ENDLOOP.
      CATCH cx_bapi_error INTO DATA(lx_error).
        LOOP AT lx_error->status ASSIGNING FIELD-SYMBOL(<ls_return>).
          me->mr_log->add_message( is_balmsg = VALUE #( msgid = <ls_return>-id
                                                        msgty = <ls_return>-type
                                                        msgno = <ls_return>-number
                                                        msgv1 = <ls_return>-message_v1
                                                        msgv2 = <ls_return>-message_v2
                                                        msgv3 = <ls_return>-message_v3
                                                        msgv4 = <ls_return>-message_v4 ) ).
*          CATCH cx_cacs_bal_ex.
        ENDLOOP.
    ENDTRY.
  ENDMETHOD.


  METHOD parse_xlsx.

    DATA(lr_xlsx_file) = me->mr_media->mr_xlsx_file.
    IF lr_xlsx_file IS INITIAL.
      RETURN.
    ENDIF.

    " Cada aba do XLSX tem:
    "  - Linha 1: descrições (texto)
    "  - Linha 2: nomes técnicos (componentes DDIC)
    "  - Linha 3..n: dados

    TYPES: BEGIN OF ty_sheet_map,
             sheet_name TYPE string,
             data_ref   TYPE REF TO data,
             upd_ref    TYPE REF TO data,
           END OF ty_sheet_map,
           tyt_sheet_maps TYPE STANDARD TABLE OF ty_sheet_map WITH EMPTY KEY.

    DATA(lt_map) = VALUE tyt_sheet_maps(
      ( sheet_name = `Header`
        data_ref   = REF #( me->mt_headers )
        upd_ref    = REF #( me->mt_headers_upd ) )
      ( sheet_name = `Service Spec`
        data_ref   = REF #( me->mt_services )
        upd_ref    = REF #( me->mt_services_upd ) )
      ( sheet_name = `Ref. Ord. Item`
        data_ref   = REF #( me->mt_reforder_items )
        upd_ref    = REF #( me->mt_reforder_items_upd ) )
      ( sheet_name = `JVA`
        data_ref   = REF #( me->mt_jva )
        upd_ref    = REF #( me->mt_jva_upd ) )
      ( sheet_name = `Partners`
        data_ref   = REF #( me->mt_partners )
        upd_ref    = REF #( me->mt_partners_upd ) )
      ( sheet_name = `Operations`
        data_ref   = REF #( me->mt_operations )
        upd_ref    = REF #( me->mt_operations_upd ) )
      ( sheet_name = `Components`
        data_ref   = REF #( me->mt_components )
        upd_ref    = REF #( me->mt_components_upd ) )
      ( sheet_name = `Settlement Rules`
        data_ref   = REF #( me->mt_settlement_rules )
        upd_ref    = REF #( me->mt_settlement_rules_upd ) )
    ).

    FIELD-SYMBOLS: <gt_data> TYPE STANDARD TABLE,
                   <gt_upd>  TYPE STANDARD TABLE,
                   <ls_data> TYPE any,
                   <ls_upd>  TYPE any,
                   <f_data>  TYPE any,
                   <f_upd>   TYPE any.

    DATA: lr_ws TYPE REF TO zcl_excel_worksheet.
    DATA: lt_fields TYPE string_table.
    DATA: lv_col_alpha TYPE  zexcel_cell_column_alpha.
    DATA: lv_cell_value TYPE string.
    DATA: lv_row TYPE i.
    DATA: lv_col TYPE i.
    DATA: lv_field TYPE string.
    DATA: lv_rollname TYPE rollname.

    LOOP AT lt_map ASSIGNING FIELD-SYMBOL(<ls_map>).
      ASSIGN <ls_map>-data_ref->* TO <gt_data>.
      ASSIGN <ls_map>-upd_ref->*  TO <gt_upd>.
      IF <gt_data> IS NOT ASSIGNED OR <gt_upd> IS NOT ASSIGNED.
        CONTINUE.
      ENDIF.

      CLEAR: <gt_data>, <gt_upd>.

      TRY.
          lr_ws = lr_xlsx_file->get_worksheet_by_name( CONV #(  <ls_map>-sheet_name ) ).
        CATCH cx_root.
          CONTINUE.
      ENDTRY.
      IF lr_ws IS INITIAL.
        CONTINUE.
      ENDIF.

      " Lê a linha 2 com os nomes técnicos dos campos
      CLEAR lt_fields.
      lv_col = 1.
      WHILE abap_true = abap_true.
        lv_col_alpha = zcl_excel_common=>convert_column2alpha( lv_col ).
        CLEAR lv_cell_value.
        TRY.
            lr_ws->get_cell(
              EXPORTING
                ip_column  = lv_col_alpha
                ip_row     = 2
              IMPORTING
                ep_value   = lv_cell_value
*                ep_rc      =
*                ep_style   =
*                ep_guid    =
*                ep_formula =
            ).
*            CATCH zcx_excel.
          CATCH cx_root.
            CLEAR lv_cell_value.
        ENDTRY.
        CONDENSE lv_cell_value.
        IF lv_cell_value IS INITIAL.
          EXIT.
        ENDIF.
        APPEND lv_cell_value TO lt_fields.
        lv_col = lv_col + 1.
      ENDWHILE.
      IF lt_fields IS INITIAL.
        CONTINUE.
      ENDIF.

      " RTTI do UPD para decidir onde marcar 'X'
      DATA(lr_upd_tab) = CAST cl_abap_tabledescr( cl_abap_typedescr=>describe_by_data( <gt_upd> ) ).
      DATA(lr_upd_line) = CAST cl_abap_structdescr( lr_upd_tab->get_table_line_type( ) ).
      DATA(lt_upd_comp) = lr_upd_line->get_components( ).

      " Percorre os dados a partir da linha 3
      lv_row = 3.
      WHILE abap_true = abap_true.
        " critério de parada: ORDERID (coluna 1) vazio
        lv_col_alpha = zcl_excel_common=>convert_column2alpha( 1 ).
        CLEAR lv_cell_value.
        TRY.
            lr_ws->get_cell(
              EXPORTING
                ip_column  = lv_col_alpha
                ip_row     = lv_row
              IMPORTING
                ep_value   = lv_cell_value
*                ep_rc      =
*                ep_style   =
*                ep_guid    =
*                ep_formula =
            ).
*            CATCH zcx_excel.
          CATCH cx_root.
            CLEAR lv_cell_value.
        ENDTRY.
        CONDENSE lv_cell_value.
        IF lv_cell_value IS INITIAL.
          EXIT.
        ENDIF.

        APPEND INITIAL LINE TO <gt_data> ASSIGNING <ls_data>.
        APPEND INITIAL LINE TO <gt_upd>  ASSIGNING <ls_upd>.

        " Preenche a linha de dados a partir dos campos da linha 2
        DO lines( lt_fields ) TIMES.
          lv_col = sy-index.
          lv_field = lt_fields[ lv_col ].
          lv_col_alpha = zcl_excel_common=>convert_column2alpha( lv_col ).
          CLEAR lv_cell_value.
          TRY.
              lr_ws->get_cell(
                EXPORTING
                  ip_column  = lv_col_alpha
                  ip_row     = lv_row
                IMPORTING
                  ep_value   = lv_cell_value
*                  ep_rc      =
*                  ep_style   =
*                  ep_guid    =
*                  ep_formula =
              ).
*              CATCH zcx_excel.
            CATCH cx_root.
              CLEAR lv_cell_value.
          ENDTRY.
          IF lv_cell_value IS INITIAL.
            CONTINUE.
          ENDIF.

          ASSIGN COMPONENT lv_field OF STRUCTURE <ls_data> TO <f_data>.
          IF sy-subrc = 0.
            TRY.
                <f_data> = lv_cell_value.
              CATCH cx_root.
                CLEAR <f_data>.
            ENDTRY.
          ENDIF.
        ENDDO.

        " Preenche a linha UPD: copia chaves / campos não-bapiupdate e marca 'X' onde houver valor
        LOOP AT lt_upd_comp ASSIGNING FIELD-SYMBOL(<ls_uc>).
          ASSIGN COMPONENT <ls_uc>-name OF STRUCTURE <ls_upd> TO <f_upd>.
          IF sy-subrc <> 0.
            CONTINUE.
          ENDIF.

          " Descobre se é um campo do tipo BAPIUPDATE (flag)
          CLEAR lv_rollname.
          TRY.
              DATA(lr_ed) = CAST cl_abap_elemdescr( <ls_uc>-type ).
              DATA(ls_dfies) = VALUE dfies( ).
              lr_ed->get_ddic_field( RECEIVING p_flddescr = ls_dfies ).
              lv_rollname = ls_dfies-rollname.
            CATCH cx_root.
              CLEAR lv_rollname.
          ENDTRY.

          IF lv_rollname = 'BAPIUPDATE'.
            ASSIGN COMPONENT <ls_uc>-name OF STRUCTURE <ls_data> TO <f_data>.
            IF sy-subrc = 0 AND <f_data> IS NOT INITIAL.
              <f_upd> = 'X'.
            ELSE.
              CLEAR <f_upd>.
            ENDIF.
          ELSE.
            " chaves/campos de identificação
            ASSIGN COMPONENT <ls_uc>-name OF STRUCTURE <ls_data> TO <f_data>.
            IF sy-subrc = 0.
              TRY.
                  <f_upd> = <f_data>.
                CATCH cx_root.
                  CLEAR <f_upd>.
              ENDTRY.
            ENDIF.
          ENDIF.
        ENDLOOP.

        lv_row = lv_row + 1.
      ENDWHILE.
    ENDLOOP.

    " Tenta preencher MT_METHODS com um conteúdo mínimo para criação.
    CLEAR me->mt_methods.
    IF me->mt_headers IS NOT INITIAL.
      DATA(lv_refnum) = VALUE ifrefnum(  ).
      LOOP AT me->mt_headers ASSIGNING FIELD-SYMBOL(<ls_hdr>).
        lv_refnum = sy-tabix.
        APPEND VALUE bapi_alm_order_method(
          refnumber  = lv_refnum
          objecttype = 'HEADER'
          method     = 'CREATE'
          objectkey  = CONV objidext( <ls_hdr>-orderid )
        ) TO me->mt_methods.
      ENDLOOP.
      " Registro final de SAVE (placeholder)
      APPEND VALUE bapi_alm_order_method(
        refnumber  = '000000'
        objecttype = ''
        method     = 'SAVE'
        objectkey  = ''
      ) TO me->mt_methods.
    ENDIF.

  ENDMETHOD.

ENDCLASS.