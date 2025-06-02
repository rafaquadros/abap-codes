REPORT z_fi_rp_acc_closing_mntr MESSAGE-ID zfi01.
INCLUDE z_fi_in_acc_closing_mntr_d01. "Definições de classes

**********************************************************************
* Variáveis de referência
**********************************************************************
DATA:
      creation_date     TYPE z_fi_ed_creation_date,
      sap_creation_date TYPE erdat,
      order_id          TYPE bstkd,
      order_status      TYPE z_fi_ed_sac_order_status,
      acquirer          TYPE z_fi_ed_acquirer,
      situation         TYPE z_fi_ed_sales_mntr_situation,
      acc_document_id   TYPE belnr_d.

**********************************************************************
* Tela de seleção
**********************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1.
SELECT-OPTIONS:
    s_cdte FOR creation_date,
    s_scdt FOR sap_creation_date,
    s_ordd FOR order_id,
    s_orst FOR order_status,
    s_acqr FOR acquirer,
    s_sttn FOR situation,
    s_dcid FOR acc_document_id.
PARAMETERS
    p_fscy TYPE gjahr DEFAULT sy-datlo(4).
SELECTION-SCREEN END OF BLOCK b1.