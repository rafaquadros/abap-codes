*&---------------------------------------------------------------------*
*& Report zrfqm_log_acm_2_fqm
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*

INCLUDE:  zrfqm_log_acm_2_fqm_top, "global data
          zrfqm_log_acm_2_fqm_p01. "classes implementations

INITIALIZATION.
  pdati = pdatf = sy-datlo.
  ptimf = '235959'.

START-OF-SELECTION.
  DATA(r_report) = NEW lcl_report( ).
  r_report->get_data( ).

end-OF-SELECTION.
  r_report->show_data(  ).