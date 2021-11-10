*&---------------------------------------------------------------------*
*& Include zrfqm_log_acm_2_fqm_p01
*&---------------------------------------------------------------------*

CLASS lcl_report IMPLEMENTATION.
  METHOD get_data.

    TRY.
        zcl_acm_fqm_log=>check_temporal_arguments(
          EXPORTING
            i_initial_date = pdati
            i_final_date   = pdatf
            i_initial_time = ptimi
            i_final_time   = ptimf
          IMPORTING
            eg_moment      = DATA(lg_moment)
        ).
*            CATCH cx_bapi_error.

        IF sctrn IS NOT INITIAL.
          APPEND sctrn TO sctrn[].
          SORT sctrn[].
          DELETE ADJACENT DUPLICATES FROM sctrn[].
        ENDIF.

        IF sevty IS NOT INITIAL.
          APPEND sevty TO sevty[].
          SORT sevty[].
          DELETE ADJACENT DUPLICATES FROM sevty[].
        ENDIF.

        mt_log_events = zcl_acm_fqm_log=>get_list_of_entries(
                         ig_contract_number = sctrn[]
                         ig_event_type      = sevty[]
                         ig_moment          = lg_moment
*                             i_initial_date     =
*                             i_final_date       =
*                             i_initial_time     =
*                             i_final_time       =
                       ).
*                           CATCH cx_bapi_error.
      CATCH cx_bapi_error INTO DATA(lx_error).
        MESSAGE lx_error->status[ 1 ]-message TYPE 'I' DISPLAY LIKE 'E'.
        LEAVE.
    ENDTRY.
  ENDMETHOD.

  METHOD show_data.

    me->prepare_data( ).
    CALL FUNCTION 'SALV_CSQT_CREATE_CONTAINER'
      EXPORTING
        r_content_manager = me
        title             = 'Log of data migration from ACM to Cash Flow'(t01).

  ENDMETHOD.

  METHOD prepare_data.

    LOOP AT me->mt_log_events ASSIGNING FIELD-SYMBOL(<lr_log_event>).
      APPEND INITIAL LINE TO me->mt_data ASSIGNING FIELD-SYMBOL(<ls_data>).
      <ls_data>-zresult = COND #( WHEN <lr_log_event>->m_fail IS INITIAL THEN '@0V@'
                                  ELSE '@03@' ).
      <ls_data>-guid = <lr_log_event>->get_guid(  ).
      <ls_data>-contract_number = <lr_log_event>->m_contract_number.
      <ls_data>-event_type = <lr_log_event>->m_event_type.
      <ls_data>-fail  = <Lr_log_event>->m_fail.
      <ls_data>-zdate =  <lr_log_event>->m_date.
      <ls_data>-ztime = <lr_log_event>->m_time.
      <ls_data>-agent = <lr_log_event>->m_agent.
    ENDLOOP.

    SORT me->mt_data BY contract_number zdate ztime.
  ENDMETHOD.

  METHOD if_salv_csqt_content_manager~fill_container_content.

    TRY .
        me->display( ir_container = me->mr_container ).
      CATCH cx_bapi_error ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

  METHOD display.

    TRY.
        cl_salv_table=>factory(
          EXPORTING
*            list_display   = if_salv_c_bool_sap=&gt;false &quot; ALV Displayed in List Mode
            r_container    =  ir_container                         "Abstract Container for GUI Controls
*            container_name =
            IMPORTING
              r_salv_table   =  mr_view                            "Basis Class Simple ALV Tables
            CHANGING
              t_table        = mt_data
        ).
*      CATCH cx_salv_msg. &quot; ALV: General Error Class with Message
      CATCH cx_salv_msg INTO DATA(lx_error).
        DATA(l_message) = lx_error->get_text( ).
        MESSAGE l_message TYPE 'E'.
    ENDTRY.

    me->set_view( ).

    me->mr_view->display( ).

  ENDMETHOD.

  METHOD on_user_command.

    DATA(lt_selected) = mr_view->get_selections( )->get_selected_rows( ).

    CASE e_salv_function.
      WHEN 'ZREDO'.
        IF me->mt_data[ lt_selected[ 1 ] ]-fail IS NOT INITIAL.
          TRY.
              NEW zcl_acm_contract( i_contract_number = me->mt_data[ lt_selected[ 1 ] ]-contract_number )->send_new_2fqm( ).
              CLEAR: me->mt_data, me->mt_log_events.
              me->get_data( ).
              me->prepare_data( ).
              me->mr_view->refresh(
*                EXPORTING
*                  s_stable     =
*                  refresh_mode = if_salv_c_refresh=>soft
              ).
            CATCH cx_sy_itab_line_not_found ##NO_HANDLER.
          ENDTRY.
        ELSE.
          MESSAGE i015.
        ENDIF.
      WHEN 'ZSHMG'.
        LOOP AT me->mt_log_events ASSIGNING FIELD-SYMBOL(<lr_log_event>).
          IF <lr_log_event>->get_guid(  ) = me->mt_data[ lt_selected[ 1 ] ]-guid.
            TRY.
                <lr_log_event>->show_messages( ).
*                CATCH cx_bapi_error.
                EXIT.
              CATCH cx_bapi_error.
                EXIT.
            ENDTRY.
          ENDIF.
        ENDLOOP.
    ENDCASE.
  ENDMETHOD.


  METHOD set_view.

    me->set_selection_multiple( ).

    me->set_columns_styles( ).

    me->set_functions( ).

    me->set_events( ).

    me->set_sort(  ).
  ENDMETHOD.


  METHOD set_selection_multiple.

    DATA(lr_selections) = mr_view->get_selections( ).

    lr_selections->set_selection_mode( if_salv_c_selection_mode=>single ).

*    lr_selections->set_selection_mode( if_salv_c_selection_mode=>row_column ).
  ENDMETHOD.


  METHOD set_columns_styles.
    DATA lr_column_table TYPE REF TO cl_salv_column_table.

    DATA(lr_columns) = me->mr_view->get_columns( ).
    IF lr_columns IS NOT BOUND.
      RETURN.
    ENDIF.

    lr_columns->set_optimize( value = if_salv_c_bool_sap=>true ).
    TRY .
        lr_column_table ?= lr_columns->get_column( 'CONTRACT_NUMBER' ).
      CATCH cx_salv_not_found.
        RETURN.
    ENDTRY.

    IF lr_column_table IS NOT BOUND.
      RETURN.
    ENDIF.

    lr_column_table->set_cell_type( if_salv_c_cell_type=>hotspot ).

    TRY.
        DATA(lr_column) = lr_columns->get_column( 'FAIL' ).
        lr_column->set_technical(
*            value = if_salv_c_bool_sap=>true
        ).
      CATCH cx_salv_not_found ##NO_HANDLER.
    ENDTRY.

    TRY.
        lr_column = lr_columns->get_column( 'GUID' ).
        lr_column->set_technical(
*            value = if_salv_c_bool_sap=>true
        ).
      CATCH cx_salv_not_found ##NO_HANDLER.
    ENDTRY.

    TRY.
        lr_column = lr_columns->get_column( 'LOG_HANDLE' ).
        lr_column->set_technical(
*            value = if_salv_c_bool_sap=>true
        ).
      CATCH cx_salv_not_found ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.


  METHOD set_functions.

    DATA(lr_functions) = mr_view->get_functions( ).
    lr_functions->set_all(
*      value = if_salv_c_bool_sap=>true
    ).
    lr_functions->set_export_localfile(
*      value = if_salv_c_bool_sap=>true
    ).

    me->set_custom_functions( lr_functions ).
  ENDMETHOD.


  METHOD set_events.

    DATA(lr_events) = mr_view->get_event( ).

    SET HANDLER: me->on_user_command FOR lr_events,
                 me->on_link_click   FOR lr_events.
  ENDMETHOD.


  METHOD set_sort.

    DATA(lr_sort) = mr_view->get_sorts( ).
    TRY.
        lr_sort->add_sort(
          EXPORTING
            columnname = 'CONTRACT_NUMBER'
*        position   =
*        sequence   = if_salv_c_sort=>sort_up
*        subtotal   = if_salv_c_bool_sap=>false
*        group      = if_salv_c_sort=>group_none
*        obligatory = if_salv_c_bool_sap=>false
*      RECEIVING
*        value      =
        ).
      CATCH cx_salv_not_found ##NO_HANDLER
            cx_salv_existing
            cx_salv_data_error.
    ENDTRY.
  ENDMETHOD.

  METHOD on_link_click.

    TRY.
        DATA(l_contract_number) = me->mt_data[ row ]-contract_number.
        SET PARAMETER ID 'WKN' FIELD l_contract_number.
        CALL TRANSACTION 'WB23' AND SKIP FIRST SCREEN.
      CATCH cx_sy_itab_line_not_found ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

  METHOD set_custom_functions.

    set_redo( ir_functions ).

    set_show_messages(  ir_functions ).
  ENDMETHOD.


  METHOD set_redo.

    DATA(l_tooltip) = CONV string(  'Retry'(p01)  ).
    DATA(l_icon) = CONV string( '@4R@' ). "'ICON_SYSTEM_REDO'.
    TRY.
        ir_functions->add_function(
          EXPORTING
            name     =  'ZREDO'               "ALV Function
            icon     = l_icon
*            text     =
            tooltip  = l_tooltip
            position = if_salv_c_function_position=>left_of_salv_functions                 "Positioning Function
        ).
*        CATCH cx_salv_existing.   &quot; ALV: General Error Class (Checked During Syntax Check)
*        CATCH cx_salv_wrong_call. &quot; ALV: General Error Class (Checked During Syntax Check)
      CATCH cx_salv_existing
            cx_salv_not_found
            cx_salv_wrong_call.
    ENDTRY.

  ENDMETHOD.


  METHOD set_show_messages.

    DATA(l_tooltip) = CONV string(  'Show messages'(p02)  ).
    DATA(l_icon) = CONV string( '@AL@' ). "'ICON_REPORT'.
    TRY.
        ir_functions->add_function(
          EXPORTING
            name     =  'ZSHMG'               "ALV Function
            icon     = l_icon
*            text     =
            tooltip  = l_tooltip
            position = if_salv_c_function_position=>left_of_salv_functions                 "Positioning Function
        ).
*        CATCH cx_salv_existing.   &quot; ALV: General Error Class (Checked During Syntax Check)
*        CATCH cx_salv_wrong_call. &quot; ALV: General Error Class (Checked During Syntax Check)
      CATCH cx_salv_existing
            cx_salv_not_found
            cx_salv_wrong_call.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.