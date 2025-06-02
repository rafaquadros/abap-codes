CLASS lcl_api DEFINITION.
  PUBLIC SECTION.
    METHODS
        reuse_alv_variant_f4      IMPORTING is_variant     TYPE disvariant
                                  RETURNING VALUE(rs_vrnt) TYPE disvariant
                                  RAISING cx_bapi_error.
ENDCLASS.

CLASS lcl_controller DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES if_salv_csqt_content_manager.

    DATA:
          mt_output              TYPE zcl_fi_acc_closing_aut_model=>yt_output,
          mt_right_top_output    TYPE zcl_fi_acc_closing_aut_model=>yt_situation_sum,
          mt_right_bottom_output TYPE zcl_fi_acc_closing_aut_model=>yt_acquirer_sum.
    METHODS:
             constructor,
             get_data,
             run.
  PRIVATE SECTION.
    DATA:
          mr_api                TYPE REF TO lcl_api,
          mr_model              TYPE REF TO zcl_fi_acc_closing_aut_model,
          mr_right_top_view     TYPE REF TO cl_salv_table,
          mr_right_bottom_view  TYPE REF TO cl_salv_table,
          mr_main_view          TYPE REF TO cl_salv_table,
          mr_right_top_cntnr    TYPE REF TO cl_gui_container,
          mr_right_bottom_cntnr TYPE REF TO cl_gui_container,
          mr_top_container      TYPE REF TO cl_gui_container,
          mr_main_container     TYPE REF TO cl_gui_container,
          mr_top                TYPE REF TO cl_dd_document.
    METHODS:
             display_main,
             display_right_top,
             display_right_bottom,
             on_link_click        FOR EVENT link_click OF  cl_salv_events_table
                                  IMPORTING row
                                            column,
             on_user_command      FOR EVENT added_function OF cl_salv_events
                                  IMPORTING e_salv_function,
             set_columns_orders,
             set_columns_styles,
             set_right_top_columns_styles,
             set_right_bottom_clmns_styles,
             set_custom_functions IMPORTING ir_functions TYPE REF TO cl_salv_functions_list,
             set_events,
             set_functions,
             set_sort,
             set_right_top_aggr,
             set_right_bottom_aggr,
             set_texts,
             set_right_top_texts,
             set_top_page         IMPORTING i_reuse_control TYPE  sdydo_flag,
             set_view,
             set_right_top_view,
             set_right_bottom_view,
             refresh.
ENDCLASS.