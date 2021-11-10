*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


CLASS lcl_db IMPLEMENTATION.

  METHOD query_ztbfqm_002.

    CLEAR rs_entry.
    SELECT guid, contract_number, event_type, fail, log_handle, agent, moment
      UP TO 1 ROWS
     FROM ztbfqm_002
     INTO @rs_entry
    WHERE guid = @i_guid
    ORDER BY PRIMARY KEY.
    ENDSELECT.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_sql_error.
    ENDIF.
  ENDMETHOD.

  METHOD insert_ztbfqm_002.

    INSERT ztbfqm_002 FROM is_entry.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_sql_error.
    ENDIF.
  ENDMETHOD.

  METHOD query_ztbfqm_002_with_filter.

    CLEAR rt_entries.
    SELECT guid, contract_number, event_type, fail, log_handle, agent, moment
     FROM ztbfqm_002
     INTO TABLE @rt_entries
    WHERE contract_number IN @ig_contract_number
      AND event_type      IN @ig_event_type
      AND moment          IN @ig_moment.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_sql_error.
    ENDIF.
  ENDMETHOD.

ENDCLASS.