*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section
CLASS lcl_api DEFINITION.
  PUBLIC SECTION.
    TYPES:
      yt_items   TYPE STANDARD TABLE OF komwbhi,
      yt_soitems TYPE STANDARD TABLE OF bapisditbos.
    METHODS:
      fqm_release_cfg_read          RETURNING VALUE(rt_release_criterial) TYPE fqmy_rel_cfg,
      fqm_aif_remote_update         IMPORTING i_operation TYPE char3
                                              it_flows    TYPE if_fqmc_types=>ty_t_flow
                                    RAISING   cx_bapi_error,
      wb2_read_docu_header_single   IMPORTING i_contract_number TYPE tkonn
                                    EXPORTING es_header_data    TYPE komwbhk
                                    RAISING   cx_bapi_error,
      wb2_read_docu_item_with_tkonn IMPORTING i_contract_number TYPE tkonn
                                    EXPORTING et_items          TYPE yt_items
                                    RAISING   cx_bapi_error,
      wb2_read_busi_data_with_tkonn IMPORTING i_contract_number TYPE tkonn
                                    EXPORTING et_business_data  TYPE wb2_komwbhd_tty
                                    RAISING   cx_bapi_error,
      zfqm_send_created             IMPORTING i_contract_number TYPE tkonn,
      zfqm_send_changes             IMPORTING i_contract_number TYPE tkonn.
ENDCLASS.


CLASS lcl_db DEFINITION.
  PUBLIC SECTION.
    TYPES:
      BEGIN OF ys_reversal,
        application     TYPE wb2_document_number,
        application_itm TYPE wb2_document_item,
      END OF ys_reversal,
      yt_reversals TYPE STANDARD TABLE OF ys_reversal.

    METHODS:
      query_prctr_in_marc IMPORTING i_material             TYPE matnr
                                    i_plant                TYPE werks_d
                          RETURNING VALUE(r_profit_center) TYPE prctr
                          RAISING   cx_sy_sql_error,
      query_FDGRV_in_lfb1 IMPORTING i_vendor_id            TYPE lifnr
                          RETURNING VALUE(r_planing_group) TYPE fdgrv
                          RAISING   cx_sy_sql_error,
      query_FDGRV_in_KNB1 IMPORTING i_customer_id          TYPE kunnr
                          RETURNING VALUE(r_planing_group) TYPE fdgrv
                          RAISING   cx_sy_sql_error,
      query_ztb_fqm_001 IMPORTING i_contract_type      TYPE tctyp
                        RETURNING VALUE(rt_ztbfqm_001) TYPE ztt_fqm_001
                        RAISING   cx_sy_sql_error,
      query_doc_flow    IMPORTING i_call_off             TYPE tkonn
                        EXPORTING VALUE(e_main_contract) TYPE tkonn
                                  VALUE(e_ref_doc)       TYPE /accgo/e_refdoc
                                  VALUE(e_ref_doc_type)  TYPE wlf_shadow_doc_type
                        RAISING   cx_sy_sql_error,
      query_used_value_from_po  IMPORTING i_contract_number    TYPE tkonn
                                          i_contract_item      TYPE tposn
                                RETURNING VALUE(r_used_amount) TYPE bwert
                                RAISING   cx_sy_sql_error,
      query_used_value_from_so  IMPORTING i_contract_number    TYPE tkonn
                                          i_contract_item      TYPE tposn
                                RETURNING VALUE(r_used_amount) TYPE bwert
                                RAISING   cx_sy_sql_error,
      query_reversals           IMPORTING i_contract_number TYPE tkonn
                                          i_contract_item   TYPE tposn
                                EXPORTING et_reversals      TYPE yt_reversals
                                RAISING   cx_sy_sql_error.
ENDCLASS.