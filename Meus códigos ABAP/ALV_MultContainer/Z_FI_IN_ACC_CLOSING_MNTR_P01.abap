CLASS lcl_api IMPLEMENTATION.
  METHOD reuse_alv_variant_f4.

    CLEAR rs_vrnt.
    CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
      EXPORTING
        is_variant    = is_variant    " Variant information
*       i_tabname_header    =     " Header Table Name
*       i_tabname_item      =     " Item Table Name
*       it_default_fieldcat =     " Field catalog with field descriptions
*       i_save        = SPACE    " Variants can be saved
*       i_display_via_grid  = SPACE
      IMPORTING
*       e_exit        =     " Dialog cancelled by user
        es_variant    = rs_vrnt    " Variant information
      EXCEPTIONS
        not_found     = 1
        program_error = 2
        OTHERS        = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                 INTO DATA(l_message).
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
  ENDMETHOD.
ENDCLASS.


CLASS lcl_controller IMPLEMENTATION.
  METHOD constructor.

    me->mr_api   = NEW lcl_api(  ).
    me->mr_model = NEW zcl_fi_acc_closing_aut_model(  ).
  ENDMETHOD.

  METHOD if_salv_csqt_content_manager~fill_container_content.

    DATA(lr_splitter) = NEW cl_gui_splitter_container(
*      link_dynnr              =
*      link_repid              =
*      shellstyle              =
*      left                    =
*      top                     =
*      width                   =
*      height                  =
*      metric                  = cntl_metric_dynpro
*      align                   = 15
    parent                  = r_container
    rows                    = 2
    columns                 = 1
*      no_autodef_progid_dynnr =
*      name                    =
  ).

    me->mr_top_container = lr_splitter->get_container(
                             row    = 1
                             column = 1
                           ).

    lr_splitter->set_row_height(
      EXPORTING
        id                = 1
        height            = 10
*      IMPORTING
*        result            =
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3
    ).
    IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*       WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.


    DATA(lr_bottom_container) = lr_splitter->get_container(
                              row    = 2
                              column = 1
                            ).

    DATA(lr_bottom_splitter) = NEW cl_gui_splitter_container(
*      link_dynnr              =
*      link_repid              =
*      shellstyle              =
*      left                    =
*      top                     =
*      width                   =
*      height                  =
*      metric                  = cntl_metric_dynpro
*      align                   = 15
    parent                  = lr_bottom_container
    rows                    = 1
    columns                 = 2
*      no_autodef_progid_dynnr =
*      name                    =
  ).

    me->mr_main_container = lr_bottom_splitter->get_container(
                                row    = 1
                                column = 1
                              ).

    me->mr_main_container->set_width(
      EXPORTING
        width      = 72    " Current Width of Control
      EXCEPTIONS
        cntl_error = 1
        OTHERS     = 2
    ).
    IF sy-subrc <> 0.
*     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    lr_bottom_splitter->set_column_width(
      EXPORTING
        id                = 1    " Column ID
        width             = 72   " NPlWidth
*      IMPORTING
*        result            =     " Result Code
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3
    ).
    IF sy-subrc <> 0.
*     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    DATA(lr_lateral_container) = lr_bottom_splitter->get_container(
                                row    = 1
                                column = 2
                              ).
    DATA(lr_lateral_splitter) = NEW cl_gui_splitter_container(
*      link_dynnr              =
*      link_repid              =
*      shellstyle              =
*      left                    =
*      top                     =
*      width                   =
*      height                  =
*      metric                  = cntl_metric_dynpro
*      align                   = 15
    parent                  = lr_lateral_container
    rows                    = 2
    columns                 = 1
*      no_autodef_progid_dynnr =
*      name                    =
  ).

    me->mr_right_top_cntnr = lr_lateral_splitter->get_container(
                              row       = 1
                              column    = 1
                          ).
    me->mr_right_top_cntnr->set_height(
      EXPORTING
        height     = 05    " Current Height of Control
*      EXCEPTIONS
*        cntl_error = 1
*        others     = 2
    ).
    IF sy-subrc <> 0.
*     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    me->mr_right_bottom_cntnr = lr_lateral_splitter->get_container(
                              row       = 2
                              column    = 1
                          ).

    TRY .
        me->display_main( ).
        me->display_right_top( ).
        me->display_right_bottom(  ).
      CATCH cx_bapi_error.
    ENDTRY.
  ENDMETHOD.


  METHOD display_main.

    TRY.
        cl_salv_table=>factory(
          EXPORTING
*            list_display   = list_display    " ALV Displayed in List Mode
            r_container    = me->mr_main_container    " Abstract Container for GUI Controls
*            container_name = container_name
          IMPORTING
            r_salv_table   = me->mr_main_view   " Basis Class Simple ALV Tables
          CHANGING
            t_table        = me->mt_output
        ).
*          CATCH cx_salv_msg.    "
      CATCH cx_salv_msg INTO DATA(lx_salv_msg).
        DATA(l_message) = lx_salv_msg->get_text( ).
        MESSAGE l_message TYPE 'E'.
    ENDTRY.

    set_view( ).

    mr_main_view->display( ).
  ENDMETHOD.


  METHOD display_right_top.

    TRY.
        cl_salv_table=>factory(
          EXPORTING
*            list_display   = list_display    " ALV Displayed in List Mode
            r_container    = me->mr_right_top_cntnr    " Abstract Container for GUI Controls
*            container_name = container_name
          IMPORTING
            r_salv_table   = me->mr_right_top_view   " Basis Class Simple ALV Tables
          CHANGING
            t_table        = mt_right_top_output
        ).
*          CATCH cx_salv_msg.    "
      CATCH cx_salv_msg INTO DATA(lx_salv_msg).
        DATA(l_message) = lx_salv_msg->get_text( ).
        MESSAGE l_message TYPE 'E'.
    ENDTRY.

    set_right_top_view( ).

    mr_right_top_view->display( ).
  ENDMETHOD.


  METHOD display_right_bottom.

    TRY.
        cl_salv_table=>factory(
          EXPORTING
*            list_display   = list_display    " ALV Displayed in List Mode
            r_container    = me->mr_right_bottom_cntnr    " Abstract Container for GUI Controls
*            container_name = container_name
          IMPORTING
            r_salv_table   = me->mr_right_bottom_view   " Basis Class Simple ALV Tables
          CHANGING
            t_table        = mt_right_bottom_output
        ).
*          CATCH cx_salv_msg.    "
      CATCH cx_salv_msg INTO DATA(lx_salv_msg).
        DATA(l_message) = lx_salv_msg->get_text( ).
        MESSAGE l_message TYPE 'E'.
    ENDTRY.

    set_right_bottom_view( ).

    mr_right_bottom_view->display( ).
  ENDMETHOD.


  METHOD on_link_click.

    TRY.
        CASE column.
          WHEN 'ACC_DOCUMENT_ID'.
            zcl_fi_acc_closing_aut_model=>navigate_fb03( EXPORTING  i_company_code = mt_output[ row ]-company_code
                                                            i_fiscal_year  = mt_output[ row ]-fiscal_year
                                                            i_document_id  = mt_output[ row ]-acc_document_id ).
        ENDCASE.
      CATCH cx_sy_itab_line_not_found.
        RETURN.
    ENDTRY.
  ENDMETHOD.


  METHOD on_user_command.

    DATA(lt_selected_rows) = me->mr_main_view->get_selections( )->get_selected_rows( ).
    DATA(lt_orders) = VALUE zcl_fi_acc_closing_aut_model=>yt_orders( FOR l_row IN lt_selected_rows
                                                                                 ( mt_output[ l_row ]-order_id ) ).
    TRY.
        CASE e_salv_function.
          WHEN zif_fi_acc_clsng_aut_constants=>mc_function-messages.
            IF lines( lt_selected_rows ) > 1.
              RETURN.
            ENDIF.

            zcl_fi_acc_closing_aut_model=>show_messages( i_log_handle = me->mt_output[ lt_selected_rows[ 1 ] ]-log_handle ).
          WHEN zif_fi_acc_clsng_aut_constants=>mc_function-provision_gen.
            TRY.
                me->mr_model->post_provision( it_orders = lt_orders )->display(
                  EXPORTING
*                    i_single            =     " Single Display
                    i_popup             =  abap_true   " Dialog Box
*                    i_grid              =     " Grid
*                    i_toolbar           = 'X'    " Boolean Variable (X=True, -=False, Space=Unknown)
*                    i_bydetlevel        =     " Display Message Hierarachy
*                    i_treesize          = 10    " Size of Tree in Display
*                    i_sort_by_probclass = ABAP_FALSE    " Sort By PROBCLASS (X=true, space=false)
*                    i_start_col         =     " Application Log: Dialog box coordinates
*                    i_start_row         =     " Application Log: Dialog box coordinates
*                    i_end_col           =     " Application Log: Dialog box coordinates
*                    i_end_row           =     " Application Log: Dialog box coordinates
*                    i_title             =     " Application Log: Screen Title
                ).
*                  CATCH cx_cacs_bal_ex.    "
              CATCH cx_sy_itab_line_not_found ##NO_HANDLER
                    cx_cacs_bal_ex.
            ENDTRY.

            me->refresh( ).
          WHEN zif_fi_acc_clsng_aut_constants=>mc_function-provision_selection.
            TRY.
                me->mr_model->select_4provision( it_orders = lt_orders ).
*                  CATCH cx_bapi_error.    "
                CLEAR: me->mt_output, me->mt_right_top_output, me->mt_right_bottom_output.
                me->refresh( ).
              CATCH cx_bapi_error ##NO_HANDLER.
            ENDTRY.
          WHEN zif_fi_acc_clsng_aut_constants=>mc_function-provision_unselection.
            TRY.
                me->mr_model->unselect_4provision( it_orders = lt_orders ).
*              CATCH cx_bapi_error.    "
                me->refresh( ).
              CATCH cx_bapi_error ##NO_HANDLER.
            ENDTRY.
          WHEN zif_fi_acc_clsng_aut_constants=>mc_function-provision_show.
            me->mr_model->show_provisions( ).
            CLEAR: me->mt_output, me->mt_right_top_output, me->mt_right_bottom_output.
            me->refresh( ).
        ENDCASE.
      CATCH cx_sy_itab_line_not_found ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.


  METHOD get_data.
    DEFINE lm_set_range.
      IF s_&1 IS NOT INITIAL.
        APPEND s_&1 TO s_&1[].
        SORT s_&1[] BY table_line.
        DELETE ADJACENT DUPLICATES FROM s_&1[].
      ENDIF.
      DATA(lg_&1) = VALUE zcl_fi_acc_closing_aut_model=>yg_&2(  ).
      lg_&1 = CORRESPONDING #( s_&1[] ).
    END-OF-DEFINITION.

    lm_set_range: cdte creation_dates,
                  scdt sap_creation_dates,
                  ordd order_ids,
                  orst order_statuses,
                  acqr acquirers,
                  sttn situations,
                  dcid acc_document_ids.

    TRY.
        IF lg_dcid IS INITIAL.
          me->mt_output = me->mr_model->get_data(
                    EXPORTING
                      ig_creation_dates     = lg_cdte
                      ig_sap_creation_dates = lg_scdt
                      ig_order_ids          = lg_ordd
                      ig_order_statuses     = lg_orst
                      ig_acquirers          = lg_acqr
                      ig_situations         = lg_sttn
                      ig_acc_document_ids   = lg_dcid
*                    i_fiscal_year         =
                    IMPORTING
                      et_situation_sum      = me->mt_right_top_output
                      et_acquirer_sum       = me->mt_right_bottom_output
                  ).
*                  CATCH cx_bapi_error.    "
        ELSE.
          me->mt_output = me->mr_model->get_data(
                EXPORTING
                  ig_creation_dates     = lg_cdte
                  ig_sap_creation_dates = lg_scdt
                  ig_order_ids          = lg_ordd
                  ig_order_statuses     = lg_orst
                  ig_acquirers          = lg_acqr
                  ig_situations         = lg_sttn
                  ig_acc_document_ids   = lg_dcid
                  i_fiscal_year         = p_fscy
                IMPORTING
                  et_situation_sum      = me->mt_right_top_output
                  et_acquirer_sum       = me->mt_right_bottom_output
              ).
*                  CATCH cx_bapi_error.    "
        ENDIF.
      CATCH cx_bapi_error INTO DATA(lx_error).
        LOOP AT lx_error->status ASSIGNING FIELD-SYMBOL(<ls_return>).
          MESSAGE <ls_return>-message TYPE 'I' DISPLAY LIKE <ls_return>-type.
        ENDLOOP.
        LEAVE TO CURRENT TRANSACTION.
    ENDTRY.

  ENDMETHOD.


  METHOD run.
    me->get_data( ).

    DATA(l_title) = CONV string( 'FI - Monitoramento de vendas'(t01) ).
    CALL FUNCTION 'SALV_CSQT_CREATE_CONTAINER'
      EXPORTING
        r_content_manager = me    " Parameter display
        title             = l_title.
  ENDMETHOD.


  METHOD set_columns_orders.

    DEFINE lm_set_position.
      lr_columns->set_column_position(
        EXPORTING
          columnname = &1    " ALV Control: Field Name of Internal Table Field
          position   = &2
      ).
    END-OF-DEFINITION.

    TRY.
        DATA(lr_columns) = me->mr_main_view->get_columns(  ).
      CATCH cx_salv_not_found.
        RETURN.
    ENDTRY.

    lm_set_position: 'PROVISION_STATUS'       1,
                     'ORDER_ID'               2,
                     'CREATION_DATE_SC'       3,
                     'ORDER_STATUS'           4,
                     'NET_AMOUNT'             5,
                     'TRANSACTION_AMOUNT'     6,
                     'ACQUIRER'               7,
                     'ACC_DOCUMENT_ID'        8,
                     'SAP_POSTING_DATE'       9,
                     'SAP_AMOUNT'            10,
                     'DIFFERENCE'            11,
                     'SITUATION_TEXT'        12.

  ENDMETHOD.


  METHOD set_columns_styles.
    DEFINE lm_set_technical.
      try .
          lr_columns->get_column( &1 )->set_technical( if_salv_c_bool_sap=>true ).
        catch cx_salv_not_found
              cx_salv_data_error ##NO_HANDLER.
      endtry.
    END-OF-DEFINITION.
    DEFINE lm_set_sign.
      try .
          lr_columns->get_column( &1 )->set_sign( if_salv_c_bool_sap=>true ).
        catch cx_salv_not_found
              cx_salv_data_error ##NO_HANDLER.
      endtry.
    END-OF-DEFINITION.
    DEFINE lm_set_alignment.
      try .
          lr_columns->get_column( &1 )->SET_ALIGNMENT( &2 ).
        catch cx_salv_not_found
              cx_salv_data_error ##NO_HANDLER.
      endtry.
    END-OF-DEFINITION.
    DEFINE lm_set_hidden.
      try .
        lr_columns->get_column( columnname = &1 )->set_visible(
            value = if_salv_c_bool_sap=>false
        ).
*          CATCH cx_salv_not_found.    "
            catch cx_salv_not_found ##NO_HANDLER.
     endtry.
    END-OF-DEFINITION.
    DEFINE lm_set_hotspot.
      TRY .
          CAST cl_salv_column_list( lr_columns->get_column( &1 ) )->set_cell_type( if_salv_c_cell_type=>hotspot ).
        CATCH cx_salv_not_found
              cx_salv_data_error ##NO_HANDLER.
      ENDTRY.
    END-OF-DEFINITION.

    TRY.
        DATA(lr_columns) = me->mr_main_view->get_columns(  ).
      CATCH cx_salv_not_found.
        RETURN.
    ENDTRY.

    DATA(lr_layout) = me->mr_main_view->get_layout( ).
    lr_layout->set_key( value = VALUE #( report = sy-repid )  ).
    lr_layout->set_save_restriction(
*            value = IF_SALV_C_LAYOUT=>RESTRICT_NONE
    ).


    me->mr_main_view->get_selections( )->set_selection_mode(
        value = if_salv_c_selection_mode=>multiple
    ).

    lm_set_technical: 'LOG_HANDLE', 'SITUATION'.

    lm_set_hidden: 'CURRENCY',         'COMPANY_CODE',     'FISCAL_YEAR'.

    "Setting hotspot
    lm_set_hotspot  'ACC_DOCUMENT_ID'.

  ENDMETHOD.


  METHOD set_right_top_columns_styles.

    TRY.
        me->mr_right_top_view->get_columns(  )->get_column( columnname = 'CURRENCY' )->set_visible(
            value = if_salv_c_bool_sap=>false
        ).
*          CATCH cx_salv_not_found.    " .
      CATCH cx_salv_not_found ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.


  METHOD set_right_bottom_clmns_styles.

    TRY.
        me->mr_right_bottom_view->get_columns(  )->get_column( columnname = 'CURRENCY' )->set_visible(
            value = if_salv_c_bool_sap=>false
        ).
*          CATCH cx_salv_not_found.    " .
      CATCH cx_salv_not_found ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.


  METHOD set_custom_functions.
    DEFINE lm_set_function.
      TRY.
          ir_functions->add_function(
            EXPORTING
              name               = &1    " ALV Function
              icon               = conv #( &2 )
*            text               =
              tooltip            = CONV #( &3 )
              position           = if_salv_c_function_position=>right_of_salv_functions    " Positioning Function
          ).
*          CATCH cx_salv_existing.    "
*          CATCH cx_salv_wrong_call.    "
        CATCH cx_salv_existing ##NO_HANDLER
              cx_salv_not_found
              cx_salv_wrong_call.
      ENDTRY.
    END-OF-DEFINITION.

    TRY.
        me->mr_model->check_auth_execute( ).
*          CATCH cx_bapi_error.    "
        lm_set_function
            zif_fi_acc_clsng_aut_constants=>mc_function-provision_gen  zif_fi_acc_clsng_aut_constants=>mc_icon-provision_gen 'Gerar provisão'(tt5).
      CATCH cx_bapi_error ##NO_HANDLER.
    ENDTRY.

    TRY.
        me->mr_model->check_auth_select( ).
*          CATCH cx_bapi_error.    "

        lm_set_function:
            zif_fi_acc_clsng_aut_constants=>mc_function-provision_selection zif_fi_acc_clsng_aut_constants=>mc_icon-provision_sel 'Marcar para provisionamento'(tt6),
            zif_fi_acc_clsng_aut_constants=>mc_function-provision_unselection zif_fi_acc_clsng_aut_constants=>mc_icon-remove_sel 'Desmarcar para provisionamento'(tt7).
      CATCH cx_bapi_error ##NO_HANDLER.
    ENDTRY.

    lm_set_function:
       zif_fi_acc_clsng_aut_constants=>mc_function-messages       zif_fi_acc_clsng_aut_constants=>mc_icon-messages 'Mensagens de processamento'(tt4),
       zif_fi_acc_clsng_aut_constants=>mc_function-provision_show zif_fi_acc_clsng_aut_constants=>mc_icon-provision_show 'Exibir provisões'(tt1).
  ENDMETHOD.


  METHOD set_events.

    DATA(lr_event) = me->mr_main_view->get_event( ).
    SET HANDLER:
                 me->on_link_click    FOR lr_event,
                 me->on_user_command  FOR lr_event.
*                 me->on_data_changed  FOR ALL INSTANCES
*                                      ACTIVATION abap_true,
  ENDMETHOD.

  METHOD set_functions.

    DATA(lr_functions) = mr_main_view->get_functions( ).
    lr_functions->set_all(
*      value = if_salv_c_bool_sap=>true
    ).

    lr_functions->set_export_localfile(
*      value = if_salv_c_bool_sap=>true
    ).

    me->set_custom_functions( ir_functions = lr_functions ).

  ENDMETHOD.


  METHOD set_sort.
    DEFINE lm_add_sort.
      TRY.
          lr_sort->add_sort(
            EXPORTING
              columnname         =  &1    " ALV Control: Field Name of Internal Table Field
              position           =  &2
*                sequence           = sequence    " Sort Sequence
*                subtotal           = if_salv_c_bool_sap=>true    " Boolean Variable (X=True, Space=False)
*                group              = group    " Control Break
*                obligatory         = obligatory    " Boolean Variable (X=True, Space=False)
*              RECEIVING
*                value              = value    " ALV Sort Settings
          ).
*              CATCH cx_salv_not_found.    "
*              CATCH cx_salv_existing.    "
*              CATCH cx_salv_data_error.    "
*              CATCH cx_salv_data_error.    "
        CATCH cx_salv_not_found ##NO_HANDLER
              cx_salv_existing
              cx_salv_data_error.
      ENDTRY.
    END-OF-DEFINITION.

    TRY.
        DATA(lr_sort) = me->mr_main_view->get_sorts(  ).
      CATCH cx_salv_not_found.
        RETURN.
    ENDTRY.

*    lm_add_sort: 'VENDOR_CODE'         1,
*                 'ACC_DOCUMENT'        2,
*                 'ITEM_ID'             3.
  ENDMETHOD.


  METHOD set_right_top_aggr.

    TRY.
        me->mr_right_top_view->get_aggregations(  )->add_aggregation(
          EXPORTING
            columnname         ='TOTAL_AMOUNT'     " ALV Control: Field Name of Internal Table Field
*            aggregation        = IF_SALV_C_AGGREGATION=>TOTAL    " Aggregation
*          RECEIVING
*            value              =     " ALV: Aggregations
        ).
*          CATCH cx_salv_data_error.    "
*          CATCH cx_salv_not_found.    "
*          CATCH cx_salv_existing.    " .
      CATCH cx_salv_not_found ##NO_HANDLER
            cx_salv_data_error
            cx_salv_existing.
    ENDTRY.
  ENDMETHOD.


  METHOD set_right_bottom_aggr.

    TRY.
        me->mr_right_bottom_view->get_aggregations(  )->add_aggregation(
          EXPORTING
            columnname         ='TOTAL_AMOUNT'     " ALV Control: Field Name of Internal Table Field
*            aggregation        = IF_SALV_C_AGGREGATION=>TOTAL    " Aggregation
*          RECEIVING
*            value              =     " ALV: Aggregations
        ).
*          CATCH cx_salv_data_error.    "
*          CATCH cx_salv_not_found.    "
*          CATCH cx_salv_existing.    " .
      CATCH cx_salv_not_found ##NO_HANDLER
            cx_salv_data_error
            cx_salv_existing.
    ENDTRY.
  ENDMETHOD.


  METHOD set_texts.
    DATA:
          lr_column TYPE REF TO cl_salv_column,
          l_text    TYPE string,
          l_length  TYPE lvc_outlen.
    DEFINE lm_set_text.
      l_text = conv string( &2 ).
      l_length = strlen( l_text ).
      TRY.
          lr_column = lr_columns->get_column( columnname = &1 ).
*                    CATCH cx_salv_not_found.  " .
          lr_column->set_medium_text( value = conv #( l_text ) ).
          lr_column->set_long_text( value = conv #( l_text ) ).
          lr_column->set_output_length( value = l_length ).
        CATCH cx_salv_not_found ##NO_HANDLER.
      ENDTRY.
    END-OF-DEFINITION.

    TRY.
        DATA(lr_columns) = me->mr_main_view->get_columns(  ).
      CATCH cx_salv_not_found.
        RETURN.
    ENDTRY.

    lm_set_text
                 'SITUATION_TEXT'       'Situação'(h01).

  ENDMETHOD.

  METHOD set_right_top_texts.
    DATA:
          lr_column TYPE REF TO cl_salv_column,
          l_text    TYPE string,
          l_length  TYPE lvc_outlen.
    DEFINE lm_set_text.
      l_text = conv string( &2 ).
      l_length = strlen( l_text ).
      TRY.
          lr_column = lr_columns->get_column( columnname = &1 ).
*                    CATCH cx_salv_not_found.  " .
          lr_column->set_medium_text( value = conv #( l_text ) ).
          lr_column->set_long_text( value = conv #( l_text ) ).
          lr_column->set_output_length( value = l_length ).
        CATCH cx_salv_not_found ##NO_HANDLER.
      ENDTRY.
    END-OF-DEFINITION.

    TRY.
        DATA(lr_columns) = me->mr_right_top_view->get_columns(  ).
      CATCH cx_salv_not_found.
        RETURN.
    ENDTRY.

    lm_set_text
                 'SITUATION_TEXT'       'Situação'(h01).
  ENDMETHOD.


  METHOD set_top_page.
    DATA l_items TYPE c LENGTH 20.

    IF i_reuse_control IS INITIAL.
      me->mr_top = NEW cl_dd_document(
*      style            =
*      background_color =
*      bds_stylesheet   =
*      no_margins       =
      ).
    ELSE.
      me->mr_top->initialize_document(
*        EXPORTING
*          first_time       =
*          style            =
*          background_color =
*          bds_stylesheet   =
*          no_margins       =
      ).
    ENDIF.

    WRITE lines( me->mt_output ) TO l_items.

    me->mr_top->add_text(
      EXPORTING
        text          = |({ l_items } ) { 'Itens'(l01) }|
*        text_table    =
*        fix_lines     =
        sap_style     = cl_dd_document=>heading
*        sap_color     =
*        sap_fontsize  =
*        sap_fontstyle =
*        sap_emphasis  =
*        style_class   =
*        a11y_tooltip  =
*      CHANGING
*        document      =
    ).

    me->mr_top->display_document(
      EXPORTING
        reuse_control      = i_reuse_control
*        reuse_registration =
*        container          =
        parent             = me->mr_top_container
      EXCEPTIONS
        html_display_error = 1
        OTHERS             = 2
    ).
    IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*       WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ENDMETHOD.

  METHOD set_view.

    me->set_sort( ).

    me->set_columns_styles( ).

    me->set_functions( ).

    me->set_events( ).

    me->set_texts( ).

    me->set_top_page( i_reuse_control = abap_false ).

    me->set_columns_orders( ).
  ENDMETHOD.

  METHOD set_right_top_view.

    me->set_right_top_aggr( ).

    me->set_right_top_columns_styles( ).

    me->set_right_top_texts( ).
  ENDMETHOD.

  METHOD set_right_bottom_view.

    me->set_right_bottom_aggr( ).

    me->set_right_bottom_clmns_styles( ).
  ENDMETHOD.


  METHOD refresh.

    me->get_data( ).

    me->mr_main_view->refresh(
*                  EXPORTING
*                    s_stable     =     " ALV Control: Refresh Stability
*                    refresh_mode = IF_SALV_C_REFRESH=>SOFT    " ALV: Data Element for Constants
    ).
    me->mr_right_top_view->refresh(
*                  EXPORTING
*                    s_stable     =     " ALV Control: Refresh Stability
*                    refresh_mode = IF_SALV_C_REFRESH=>SOFT    " ALV: Data Element for Constants
    ).
    me->mr_right_bottom_view->refresh(
*                  EXPORTING
*                    s_stable     =     " ALV Control: Refresh Stability
*                    refresh_mode = IF_SALV_C_REFRESH=>SOFT    " ALV: Data Element for Constants
    ).

  ENDMETHOD.

ENDCLASS.