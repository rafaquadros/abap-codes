*&---------------------------------------------------------------------*
*& Include zrfqm_log_acm_2_d01
*&---------------------------------------------------------------------*

CLASS lcl_report DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES if_salv_csqt_content_manager.
    METHODS:
      get_data,
      show_data,
      on_user_command FOR EVENT added_function OF cl_salv_events
        IMPORTING e_salv_function,
      on_link_click   FOR EVENT link_click OF cl_salv_events_table
        IMPORTING row
                  column.

  PRIVATE SECTION.
    DATA:
      mr_container  TYPE REF TO cl_gui_custom_container,
      mr_view       TYPE REF TO cl_salv_table,
      mt_log_events TYPE ztt_fqm_004,
      mt_data       TYPE ztt_fqm_003.

    METHODS:
      display IMPORTING ir_container TYPE REF TO cl_gui_custom_container
              RAISING   cx_bapi_error,
      prepare_data,
      set_view,
      set_selection_multiple,
      set_columns_styles,
      set_functions,
      set_custom_functions IMPORTING ir_functions TYPE REF TO cl_salv_functions_list,
      set_events,
      set_sort,
      set_redo
        IMPORTING
          ir_functions TYPE REF TO cl_salv_functions_list,
      set_show_messages
        IMPORTING
          ir_functions TYPE REF TO cl_salv_functions_list.
ENDCLASS.