*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section
CLASS lcl_db DEFINITION.
  PUBLIC SECTION.
    METHODS:
      query_ztbfqm_002 IMPORTING i_guid          TYPE guid
                       RETURNING VALUE(rs_entry) TYPE zst_fqm_002
                       RAISING   cx_SY_SQL_ERROR,
      query_ztbfqm_002_with_filter IMPORTING ig_contract_number TYPE zcl_acm_fqm_log=>yg_contract_number
                                             ig_event_type      TYPE zcl_acm_fqm_log=>yg_event_type
                                             ig_moment          TYPE zcl_acm_fqm_log=>yg_moment
                                   RETURNING VALUE(rt_entries)  TYPE ztt_fqm_002
                                   RAISING   cx_sy_sql_error,
      insert_ztbfqm_002 IMPORTING is_entry TYPE ztbfqm_002
                        RAISING   cx_sy_sql_error.
ENDCLASS.