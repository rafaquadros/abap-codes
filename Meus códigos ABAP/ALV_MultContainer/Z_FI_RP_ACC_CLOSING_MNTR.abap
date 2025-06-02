INCLUDE: z_fi_in_acc_closing_mntr_top, "Definições globais
         z_fi_in_acc_closing_mntr_p01. "Implementações de classes

INITIALIZATION.
  DATA(gr_controller) = NEW lcl_controller(  ).

AT SELECTION-SCREEN OUTPUT.
  SET TITLEBAR 'TTL_HEADER' WITH 'Monitor de vendas'(t01).

START-OF-SELECTION.
  gr_controller->run( ).