CLASS zcl_acm_fqm_log DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES:
      yg_contract_number TYPE RANGE OF tkonn,
      yg_event_type      TYPE RANGE OF zde_acm2fqm_event_type,
      yg_moment          TYPE RANGE OF zde_moment.

    CLASS-METHODS:
      class_constructor,
      check_temporal_arguments IMPORTING i_initial_date TYPE dats
                                         i_final_date   TYPE dats
                                         i_initial_time TYPE tims
                                         i_final_time   TYPE tims
                               EXPORTING eg_moment      TYPE yg_moment
                               RAISING   cx_bapi_error,
      get_list_of_entries IMPORTING ig_contract_number     TYPE yg_contract_number
                                    ig_event_type          TYPE yg_event_type
                                    ig_moment              TYPE yg_moment OPTIONAL
                                    i_initial_date         TYPE dats OPTIONAL
                                    i_final_date           TYPE dats OPTIONAL
                                    i_initial_time         TYPE tims OPTIONAL
                                    i_final_time           TYPE tims OPTIONAL
                          RETURNING VALUE(rt_entries_list) TYPE ztt_fqm_004
                          RAISING   cx_bapi_error.

    CONSTANTS:
      mc_ZACM_FQM   TYPE balobj_d  VALUE 'ZACM_FQM',
      mc_ZACM_FQM_C TYPE balsubobj VALUE 'ZACM_FQM_C',
      mc_ZACM_FQM_m TYPE balsubobj VALUE 'ZACM_FQM_M',
      mc_ZACM_FQM_r TYPE balsubobj value 'ZACM_FQM_R'.
    DATA:
      m_contract_number TYPE tkonn READ-ONLY,
      m_event_type      TYPE zde_acm2fqm_event_type READ-ONLY,
      m_agent           TYPE xubname READ-ONLY,
      m_date            TYPE dats READ-ONLY,
      m_time            TYPE tims READ-ONLY,
      m_fail            TYPE xfeld READ-ONLY.
    METHODS:
      constructor IMPORTING i_contract_number TYPE tkonn
                            i_event_type      TYPE zde_acm2fqm_event_type
                            i_agent           TYPE swp_agent OPTIONAL
                            i_guid            TYPE guid OPTIONAL
                            i_fail_indicator  TYPE xfeld OPTIONAL,
      add_messages IMPORTING it_messages TYPE bapiret2_t
                   RAISING   cx_bapi_error,
      get_guid     RETURNING VALUE(r_guid) TYPE guid,
      show_messages RAISING cx_bapi_error.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA
        mr_db TYPE REF TO lcl_db.
    DATA:
      mr_log TYPE REF TO cl_cacs_bal,
      m_guid TYPE guid.
    METHODS
      set_attributes RAISING cx_bapi_error
                             zcx_entry_not_found.
    METHODS set_log  IMPORTING i_loghandle TYPE  balloghndl OPTIONAL
                     RAISING   cx_bapi_error.
ENDCLASS.



CLASS zcl_acm_fqm_log IMPLEMENTATION.
  METHOD class_constructor.

    mr_db = NEW lcl_db( ).
  ENDMETHOD.

  METHOD constructor.

    me->m_contract_number = i_contract_number.
    me->m_event_type      = i_event_type.

    IF i_agent IS SUPPLIED.
      me->m_agent = i_agent+2.
    ENDIF.

    IF i_guid IS SUPPLIED.
      me->m_guid = i_guid.
    ENDIF.

    IF i_fail_indicator IS SUPPLIED.
      me->m_fail = i_fail_indicator.
    ENDIF.
  ENDMETHOD.

  METHOD set_attributes.

    IF me->m_guid IS INITIAL.
      TRY.
          me->m_guid = cl_uuid_factory=>create_system_uuid( )->create_uuid_c32( ).
*                                                        CATCH cx_uuid_error.
        CATCH cx_uuid_error.
          MESSAGE e001 INTO DATA(l_message).
          m_raise_bapi_error.
      ENDTRY.
    ENDIF.


    TRY.
        DATA(ls_entry) = me->mr_db->query_ztbfqm_002( i_guid = me->m_guid  ).
*                         CATCH cx_sy_sql_error.
        me->m_contract_number = ls_entry-contract_number.
        me->m_agent           = ls_entry-agent.
        CONVERT TIME STAMP ls_entry-moment TIME ZONE sy-zonlo INTO DATE me->m_date TIME me->m_time.
        TRY.
            me->set_log( ls_entry-log_handle ).
          CATCH cx_bapi_error INTO DATA(lx_error).
            RAISE EXCEPTION lx_error.
        ENDTRY.
      CATCH cx_sy_sql_error.
        me->m_date = sy-datlo.
        me->m_time = sy-timlo.

        TRY.
            me->set_log(
*        i_loghandle =
            ).

          CATCH cx_bapi_error INTO lx_error.
            RAISE EXCEPTION lx_error.
        ENDTRY.

        MESSAGE e002 INTO DATA(l_message1).
        RAISE EXCEPTION TYPE zcx_entry_not_found.
    ENDTRY.
  ENDMETHOD.

  METHOD add_messages.

    IF me->m_guid IS INITIAL.
      TRY.
          me->set_attributes( ).
*          CATCH cx_bapi_error.
*          CATCH zcx_entry_not_found.
        CATCH cx_bapi_error INTO DATA(lx_error).
          RAISE EXCEPTION lx_error.
        CATCH zcx_entry_not_found ##NO_HANDLER.
      ENDTRY.
    ENDIF.

    CONVERT DATE me->m_date TIME me->m_time INTO TIME STAMP DATA(l_moment) TIME ZONE sy-zonlo.
    DATA(ls_entry) = VALUE ztbfqm_002( mandt           = cl_abap_syst=>get_client( )
                                       guid            = me->m_guid
                                       contract_number = me->m_contract_number
                                       event_type      = me->m_event_type
                                       fail            = me->m_fail
                                       log_handle      = me->mr_log->get_handle( )
                                       agent           = COND #( WHEN me->m_agent IS NOT INITIAL THEN me->m_agent
                                                                 ELSE cl_abap_syst=>get_user_name( ) )
                                       moment          = l_moment ).
    TRY.
        me->mr_db->insert_ztbfqm_002( is_entry = ls_entry ).
*        CATCH cx_sy_sql_error.
      CATCH cx_sy_sql_error.
        MESSAGE e001 INTO DATA(l_message).
        m_raise_bapi_error.
    ENDTRY.

    TRY.
        LOOP AT it_messages ASSIGNING FIELD-SYMBOL(<ls_message>).

          me->mr_log->add_message( is_balmsg = VALUE #( msgty = <ls_message>-type
                                                        msgid = <ls_message>-id
                                                        msgno = <ls_message>-number
                                                        msgv1 = <ls_message>-message_v1
                                                        msgv2 = <ls_message>-message_v2
                                                        msgv3 = <ls_message>-message_v3
                                                        msgv4 = <ls_message>-message_v4 ) ).
*      CATCH cx_cacs_bal_ex.
        ENDLOOP.

        me->mr_log->save_messages(
*        i_updatetask = ' '
        ).
*    CATCH cx_cacs_bal_ex.
      CATCH cx_cacs_bal_ex.
        MESSAGE e001 INTO l_message.
        m_raise_bapi_error.
    ENDTRY.

    COMMIT WORK.
  ENDMETHOD.

  METHOD get_guid.

    r_guid = me->m_guid.
  ENDMETHOD.

  METHOD show_messages.

    IF me->mr_log IS NOT BOUND.
      TRY.
          me->set_attributes( ).
*          CATCH cx_bapi_error.
*          CATCH zcx_entry_not_found.
        CATCH cx_bapi_error
              zcx_entry_not_found.
          RETURN.
      ENDTRY.
    ENDIF.

    TRY.
        mr_log->display(
*        EXPORTING
*          i_single            =
            i_popup             = abap_true
            i_grid              = abap_true
*          i_toolbar           = 'X'
*          i_bydetlevel        =
*          i_treesize          = 10
*          i_sort_by_probclass = abap_false
*          i_start_col         =
*          i_start_row         =
*          i_end_col           =
*          i_end_row           =
*          i_title             =
        ).
      CATCH cx_cacs_bal_ex ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

  METHOD get_list_of_entries.
    DATA  lg_moment TYPE yg_moment.

    CLEAR rt_entries_list.
    IF ig_moment IS NOT INITIAL.
      lg_moment = ig_moment.
    ELSE.
      TRY.
          zcl_acm_fqm_log=>check_temporal_arguments(
            EXPORTING
              i_initial_date = i_initial_date
              i_final_date   = i_final_date
              i_initial_time = i_initial_time
              i_final_time   = i_final_time
            IMPORTING
              eg_moment      = lg_moment
          ).
*          CATCH cx_bapi_error.
        CATCH cx_bapi_error INTO DATA(lx_error).
          RAISE EXCEPTION lx_error.
      ENDTRY.
    ENDIF.

    TRY.
        DATA(lt_entries) = mr_db->query_ztbfqm_002_with_filter(
                             ig_contract_number = ig_contract_number
                             ig_event_type      = ig_event_type
                             ig_moment          = lg_moment
                           ).
*                           CATCH cx_sy_sql_error.
      CATCH cx_sy_sql_error.
        MESSAGE e011 INTO DATA(l_message).
        m_raise_bapi_error.
    ENDTRY.

    rt_entries_list = VALUE #( FOR ls_entry IN lt_entries ( NEW zcl_acm_fqm_log(
      i_contract_number = ls_entry-contract_number
      i_event_type      = ls_entry-event_type
      i_guid            = ls_entry-guid
      i_fail_indicator  = ls_entry-fail
    )  ) ).

    LOOP AT rt_entries_list ASSIGNING FIELD-SYMBOL(<lr_entry>).
      TRY.
          <lr_entry>->set_attributes(  ).
        CATCH zcx_entry_not_found ##NO_HANDLER.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.

  METHOD check_temporal_arguments.

    CLEAR eg_moment.
    IF i_initial_date > i_final_date AND i_final_date <> '00000000'.
      MESSAGE e012 INTO DATA(l_message).
      m_raise_bapi_error.
    ENDIF.

    IF i_final_date IS INITIAL OR i_initial_date IS INITIAL.
      IF i_initial_time > i_final_time AND i_final_time IS NOT INITIAL.
        MESSAGE e013 INTO l_message.
        m_raise_bapi_error.
      ENDIF.
    ENDIF.

    DATA(l_initial_date) = COND dats( WHEN i_initial_date IS NOT INITIAL THEN i_initial_date
                                      ELSE '00010101' ).
    DATA(l_final_date)   = COND dats( WHEN i_final_date IS NOT INITIAL THEN i_final_date
                                      WHEN i_initial_date IS NOT INITIAL THEN i_initial_date
                                      ELSE '30991231' ).
    DATA(l_final_time)   = COND tims( WHEN i_final_time IS NOT INITIAL THEN i_final_time
                                      ELSE '235959' ).

    CONVERT DATE l_initial_date TIME i_initial_time INTO TIME STAMP DATA(l_initial_timestamp) TIME ZONE sy-zonlo.
    CONVERT DATE l_final_date   TIME l_final_time   INTO TIME STAMP DATA(l_final_timestamp) TIME ZONE sy-zonlo.

    eg_moment = VALUE #( ( sign = 'I' option = 'BT' low = l_initial_timestamp high = l_final_timestamp ) ).
  ENDMETHOD.


  METHOD set_log.

    DATA l_message TYPE string.

    TRY.
        IF i_loghandle IS NOT INITIAL.
          mr_log = cl_cacs_bal=>create_instance(
                     id_handle             = i_loghandle
*                     id_log_name           =
*                     id_object             =
*                     id_subobject          =
*                     id_default_msgid      =
*                     id_getmsgfilter       =
*                     id_msgfilter          =
*                     ib_use_cacs_bufferlog =
*                     id_probclass_filter   =
                   ).
*                   CATCH cx_cacs_bal_ex.
        ELSE.
          mr_log = NEW cl_cacs_bal(
*            i_handle              =
                        i_log_name            = me->m_contract_number && me->m_date && me->m_time
                        i_object              = mc_zacm_fqm
                        i_subobject           = COND #( WHEN me->m_event_type = 'C' THEN mc_zacm_fqm_c
                                                        WHEN me->m_event_type = 'M' THEN mc_zacm_fqm_m
                                                        WHEN me->m_event_type = 'R' THEN mc_zacm_fqm_r )
*            i_default_msgid       =
*            i_getmsgfilter        =
*            i_msgfilter           =
*            ib_use_cacs_bufferlog =
*            id_probclass_filter   =
          ).
*          CATCH cx_cacs_bal_ex.
        ENDIF.
      CATCH cx_cacs_bal_ex.
        MESSAGE e001 INTO l_message.
        m_raise_bapi_error.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.