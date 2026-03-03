*&---------------------------------------------------------------------*
*& Report zpm_orders_download
*&---------------------------------------------------------------------*
*&Change     Developer    Date        Description
* DA1K940048 AVV_QUARAF   26-FEB-2026 Initial development
*&---------------------------------------------------------------------*

INCLUDE: zpm_i_orders_download_top,"Global data
         zpm_i_orders_download_p01."Local classes implementations

INITIALIZATION.
  r_report = NEW lcl_report(  ).

AT SELECTION-SCREEN ON p_filepn.
  IF r_report->mr_media IS INITIAL.
    FREE r_desktop.
    p_filepn = r_report->get_field_value( 'P_FILEPN' ).


    IF p_filepn IS NOT INITIAL OR p_filepn CO space.
      r_desktop = NEW #( p_filepn ).
      r_report->mr_media ?= r_desktop.

      p_filepn = r_report->mr_media->m_filename.
    ELSE.
      IF p_filepn IS INITIAL.
        MESSAGE 'Please provide the file path and name.'(m01) TYPE 'E' DISPLAY LIKE 'I'.
        LEAVE TO CURRENT TRANSACTION.
      ENDIF.
    ENDIF.


  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_filepn.
  DATA: t_file_table TYPE filetable,
        rc           TYPE i.
  FIELD-SYMBOLS  <s_file_table> TYPE file_table.

  CLEAR: t_file_table, rc.
  cl_gui_frontend_services=>file_open_dialog(
    EXPORTING
*      window_title            = window_title
*      default_extension       = default_extension
*      default_filename        = default_filename
*      file_filter             = file_filter
*      with_encoding           = with_encoding
*      initial_directory       = initial_directory
      multiselection          = abap_false
    CHANGING
      file_table              = t_file_table
      rc                      = rc
*      user_action             = user_action
*      file_encoding           = file_encoding
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
         ).
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  READ TABLE t_file_table ASSIGNING <s_file_table> INDEX 1.
  IF sy-subrc = 0.
    p_filepn = <s_file_table>-filename.
  ENDIF.

START-OF-SELECTION.
  r_report->run( ).