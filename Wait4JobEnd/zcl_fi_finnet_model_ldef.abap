*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section
CLASS lcl_db DEFINITION DEFERRED.
CLASS zcl_fi_finnet_model DEFINITION LOCAL FRIENDS lcl_db.

CLASS lcl_api DEFINITION.
  PUBLIC SECTION.
    TYPES yt_joblist TYPE STANDARD TABLE OF tbtcjmoni WITH EMPTY KEY.
    METHODS:
        bp_job_checkstate   IMPORTING i_job_name        TYPE btcjob
                                      i_job_count       TYPE btcjobcnt
                            RETURNING VALUE(r_status)   TYPE btcstatus
                            RAISING cx_bapi_error,
        bp_job_moni         IMPORTING i_job_name        TYPE btcjob
                            RETURNING VALUE(rt_joblist) TYPE yt_joblist
                            RAISING cx_bapi_error,
        get_print_parameters EXPORTING es_out_params TYPE pri_params
                                       e_valid       TYPE c
                             RAISING cx_bapi_error,
        job_close           IMPORTING i_job_name        TYPE btcjob
                                      i_job_count       TYPE btcjobcnt
                            RAISING cx_bapi_error,
        job_open            IMPORTING i_job_name        TYPE btcjob
                            RETURNING VALUE(r_jobcount) TYPE btcjobcnt
                            RAISING cx_bapi_error,
        popup_to_confirm    IMPORTING i_main_text       TYPE string
                                      i_1st_bttn_txt    TYPE string
                                      i_2nd_bttn_txt    TYPE string
                            RETURNING VALUE(r_answer)   TYPE char1
                            RAISING cx_bapi_error,
        rs_create_variant   IMPORTING i_program         TYPE raldb_repo
                                      i_variant         TYPE raldb_vari
                                      is_descr          TYPE varid
                                      it_contents       TYPE rsparams_tt
                                      it_texts          TYPE diwps_varit_t
                            RAISING cx_bapi_error,
        rs_variant_contents IMPORTING i_program         TYPE raldb_repo
                                      i_variant         TYPE raldb_vari
                            RETURNING VALUE(rt_values)  TYPE rsparams_tt
                            RAISING   cx_bapi_error,
        rs_variant_delete   IMPORTING i_program         TYPE raldb_repo
                                      i_variant         TYPE raldb_vari
                            RAISING cx_bapi_error.
ENDCLASS.


CLASS lcl_db DEFINITION.
  PUBLIC SECTION.
    TYPES: BEGIN OF ys_paym_send,
            execution_date  TYPE laufd,
            execution_id    TYPE laufi,
            company_code    TYPE bukrs,
            fiscal_year     TYPE gjahr,
            document_id     TYPE belnr_d,
            item_id         TYPE buzei,
            due_date        TYPE z_fi_ed_due_date,
            van_return      TYPE z_fi_ed_van_return,
            sending_status  TYPE z_fi_ed_sending_status,
            payment_status  TYPE z_fi_ed_payment_status,
            ff5_short_key   TYPE z_fi_ed_ff5_short_key,
            created_at      TYPE z_fi_ed_createdat,
            created_by      TYPE z_fi_ed_createdby,
            last_changed_at TYPE z_fi_ed_modifiedat,
            modified_by     TYPE z_fi_ed_modifiedby,
           END OF   ys_paym_send,
           yt_payms_snds    TYPE STANDARD TABLE OF ys_paym_send WITH EMPTY KEY.
    METHODS:
        chk_reguh IMPORTING i_execution_date       TYPE laufd
                            i_execution_id         TYPE laufi
                  RAISING   cx_sy_sql_error,
        qry_bkpf  IMPORTING i_company_code              TYPE bukrs
                            i_fiscal_year               TYPE gjahr
                            i_document_id               TYPE belnr_d
                  RETURNING VALUE(rs_doc_header)    TYPE zcl_fi_finnet_model=>ys_document_header
                  RAISING cx_sy_sql_error,
        qry_bseg  IMPORTING i_company_code              TYPE bukrs
                            i_fiscal_year               TYPE gjahr
                            i_document_id               TYPE belnr_d
                            i_item_id                   TYPE buzei
                  RETURNING VALUE(rs_doc_item)    TYPE zcl_fi_finnet_model=>ys_document_item
                  RAISING cx_sy_sql_error,
        qry_bsik  IMPORTING i_company_code          TYPE bukrs
                            i_house_bank_key        TYPE hbkid
                            i_payment_method        TYPE dzlsch
                  RETURNING VALUE(rt_payments_keys) TYPE zcl_fi_finnet_model=>yt_payments_keys
                  RAISING cx_sy_sql_error,
        qry_febep IMPORTING i_short_key                 TYPE kukey_eb
                            i_payment_doc               TYPE belnr_d
                  RETURNING VALUE(rs_entry)             TYPE zcl_fi_finnet_model=>ys_bank_return
                  RAISING cx_sy_sql_error,
        qry_lfa1  IMPORTING i_vendor_code               TYPE lifnr
                  RETURNING VALUE(rs_vendor)            TYPE zcl_fi_finnet_model=>ys_vendor
                  RAISING cx_sy_sql_error,
        qry_regup_reguh IMPORTING i_execution_date            TYPE laufd
                            i_execution_id              TYPE laufi
                            i_company_code              TYPE bukrs
                            i_fiscal_year               TYPE gjahr
                            i_document_id               TYPE belnr_d
                            i_item_id                   TYPE buzei
                  RETURNING VALUE(rs_payment)           TYPE zcl_fi_finnet_model=>ys_payment
                  RAISING cx_sy_sql_error,
        qry_z_payms_snds IMPORTING    ig_execution_date TYPE zcl_fi_finnet_model=>yg_sending_date
                                      ig_due_date       TYPE zcl_fi_finnet_model=>yg_due_date
                                      ig_document_id    TYPE zcl_fi_finnet_model=>yg_document_id
                                      i_company_code    TYPE bukrs
                                      ig_fiscal_year    TYPE zcl_fi_finnet_model=>yg_fiscal_year
                         RETURNING VALUE(rt_payms_snds) TYPE yt_payms_snds
                         RAISING cx_sy_sql_error.
ENDCLASS.