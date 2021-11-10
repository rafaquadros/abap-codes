*&---------------------------------------------------------------------*
*& Include zrfqm_log_acm_2_fqm_top
*&---------------------------------------------------------------------*
REPORT zrfqm_log_acm_2_fqm MESSAGE-ID zfqm.

INCLUDE zrfqm_log_acm_2_d01. "classes definitions

**********************************************************************
* REFERENCES VARIABLES
**********************************************************************
DATA:
  contract_number TYPE tkonn,
  event_type      TYPE zde_acm2fqm_event_type.

**********************************************************************
* SELECTION-SCREEN
**********************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS:
      sctrn FOR contract_number,
      sevty FOR event_type.
  SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
    SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT 1(15) TEXT-003 FOR FIELD pdati.
      PARAMETERS pdati TYPE dats.
      SELECTION-SCREEN COMMENT 30(13) TEXT-004 FOR FIELD pdatf.
      PARAMETERS pdatf TYPE dats.
    SELECTION-SCREEN END OF LINE.
    SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT 1(15) TEXT-005 FOR FIELD ptimi.
      PARAMETERS ptimi TYPE tims.
      SELECTION-SCREEN COMMENT 30(13) TEXT-006 FOR FIELD ptimf.
      PARAMETERS ptimf TYPE tims.
    SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN END OF BLOCK b2.
SELECTION-SCREEN END OF BLOCK b1.