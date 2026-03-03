*&---------------------------------------------------------------------*
*& Include zpm_i_orders_download_top
*&---------------------------------------------------------------------*

REPORT zpm_r_orders_download MESSAGE-ID zpm.

INCLUDE zpm_i_orders_download_d01. "Local Classes Definitions

*----------------------------------------------------------------------*
* REFERENCES VARIABLES
*----------------------------------------------------------------------*
DATA: order             TYPE aufnr,
      order_type        TYPE aufart,
      maintenance_plant TYPE swerk.
*----------------------------------------------------------------------*
* SELECTION SCREEN
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_order FOR order,
                  s_otype FOR order_type,
                  s_mplan FOR maintenance_plant.
  PARAMETERS p_filepn  TYPE string obLIGATORY
             LOWER CASE.
  SELECTION-SCREEN COMMENT  /1(79) TEXT-002.
SELECTION-SCREEN END OF BLOCK b1.

*--------------------------------------------------------------------*
* GLOBAL ATOMIC VARIABLES
*--------------------------------------------------------------------*
DATA: r_desktop TYPE REF TO lcl_windows,
      r_report  TYPE REF TO lcl_report.
*--------------------------------------------------------------------*
* MACROS
*--------------------------------------------------------------------*
DEFINE m_raise_fm_exception.
  MESSAGE ID &1
          TYPE &2
          NUMBER &3
          WITH &4 &5 &6 &7
          INTO l_message.

  APPEND INITIAL LINE TO lt_return ASSIGNING <ls_return>.
  <ls_return>-id         = sy-msgid.
  <ls_return>-type       = sy-msgty.
  <ls_return>-number     = sy-msgno.
  <ls_return>-message    = l_message.
  <ls_return>-message_v1 = sy-msgv1.
  <ls_return>-message_v2 = sy-msgv2.DEFINE m_raise_message.
  READ TABLE lx_error->status WITH KEY type = 'E'
   ASSIGNING <ls_return>.
  IF sy-subrc = 0.
    MESSAGE
         ID <ls_return>-id
       TYPE <ls_return>-type
     NUMBER <ls_return>-number
       WITH <ls_return>-message_v1 <lw_return>-message_v2
            <ls_return>-message_v3 <lw_return>-message_v4.
  ENDIF.
END-OF-DEFINITION.
DEFINE m_raise_message.
  READ TABLE lx_error->status WITH KEY type = 'E'
   ASSIGNING <ls_return>.
  IF sy-subrc = 0.
    MESSAGE
         ID <ls_return>-id
       TYPE <ls_return>-type
     NUMBER <ls_return>-number
       WITH <ls_return>-message_v1<lw_return>-message_v2
            <ls_return>-message_v3<lw_return>-message_v4.
  ENDIF.
END-OF-DEFINITION.