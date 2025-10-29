*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
CLASS lcl_api IMPLEMENTATION.
  METHOD bp_job_checkstate.

    CLEAR r_status.
    CALL FUNCTION 'BP_JOB_CHECKSTATE'
      EXPORTING
        dialog                       = zif_fi_finnet_constants=>mc_n    " Dialog yes/no
        jobcount                     = i_job_count    " Job count of a job
        jobname                      = i_job_name    " Job name
*       start_asap                   =
*       time_limit                   =
*       read_only_status             =
      IMPORTING
*       status_according_to_db       =
        actual_status                = r_status
      EXCEPTIONS
        checking_of_job_has_failed   = 1
        correcting_job_status_failed = 2
        invalid_dialog_type          = 3
        job_does_not_exist           = 4
        no_check_privilege_given     = 5
        ready_switch_too_dangerous   = 6
        OTHERS                       = 7.
    IF sy-subrc <> 0.
      IF sy-msgid IS NOT INITIAL.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                   INTO DATA(l_message).
      ELSE.
        MESSAGE e277(zfi01) INTO l_message.
      ENDIF.
      m_raise_bapi_error.
    ENDIF.
  ENDMETHOD.

  METHOD bp_job_moni.

    CLEAR rt_joblist.
    CALL FUNCTION 'BP_JOB_MONI'
      EXPORTING
        jobname         = i_job_name    " Background job name
*       enddate         = '        '    " Job start date
*       endtime         = '      '    " Batch job start time
      TABLES
        joblist         = rt_joblist    " Job Data Sent to Monitoring
      EXCEPTIONS
        jobname_missing = 1
        no_jobs_found   = 2
        other_error     = 3
        OTHERS          = 4.
    IF sy-subrc <> 0.
      IF sy-msgid IS NOT INITIAL.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                   INTO DATA(l_message).
      ELSE.
        MESSAGE e397(zfi01) WITH i_job_name INTO l_message.
      ENDIF.
      m_raise_bapi_error.
    ENDIF.
  ENDMETHOD.

  METHOD get_print_parameters.

    CLEAR: e_valid, es_out_params.
    CALL FUNCTION 'GET_PRINT_PARAMETERS'
      EXPORTING
*       archive_id             = C_CHAR_UNKNOWN    " Archive ID
*       archive_info           = C_CHAR_UNKNOWN    " Additional information for report names
*       archive_mode           = C_CHAR_UNKNOWN    " Control of the printing and archiving
*       archive_text           = C_CHAR_UNKNOWN    " Info text for the archive request
*       ar_object              = C_CHAR_UNKNOWN    " Archiving object
*       archive_report         = C_CHAR_UNKNOWN    " Archiving Report
*       authority              = C_CHAR_UNKNOWN    " Default for 'authorization'
*       copies                 = C_NUM3_UNKNOWN
*       cover_page             = C_CHAR_UNKNOWN    " Selection cover sheet output value
*       data_set               = C_CHAR_UNKNOWN    " Default for name of the spool data
*       department             = C_CHAR_UNKNOWN    " Default for 'department'
*       destination            = C_CHAR_UNKNOWN    " Default for 'output device'
*       expiration             = C_NUM1_UNKNOWN    " Default for 'retention period'
        immediately            = abap_false    " Default for 'Print immediately'
*       in_archive_parameters  = SPACE    " List of all archiving parameters
*       in_parameters          = SPACE    " List of all default parameters
*       layout                 = C_CHAR_UNKNOWN
*       line_count             = C_INT_UNKNOWN    " Default for 'Rows'
*       line_size              = C_INT_UNKNOWN    " Default for 'columns'
*       list_name              = C_CHAR_UNKNOWN    " Default for 'spool request name'
*       list_text              = C_CHAR_UNKNOWN    " Default for 'spool request text'
*       mode                   = SPACE
*       new_list_id            = C_CHAR_UNKNOWN    " Default for 'New spool request'
*       protect_list           = C_CHAR_UNKNOWN    " Cannot Append Any More to Generated Spool Request
        no_dialog              = abap_true    " Suppress dialog
*       receiver               = C_CHAR_UNKNOWN    " Default for 'recipient'
*       release                = C_CHAR_UNKNOWN
*       report                 = C_CHAR_UNKNOWN    " REPORT name
*       sap_cover_page         = C_CHAR_UNKNOWN    " Default for the output of the SAP cover sheet
*       host_cover_page        = C_CHAR_UNKNOWN    " Default for Output of Operating System Cover Sheet
*       priority               = C_NUM1_UNKNOWN    " Default for Priority
*       sap_object             = C_CHAR_UNKNOWN    " Name of the application for the archiving
*       type                   = C_CHAR_UNKNOWN
*       user                   = SY-UNAME
*       use_old_layout         = SPACE    " Use Old List Format
*       uc_display_mode        = C_CHAR_UNKNOWN    " Unicode Print Layout
*       draft                  = C_CHAR_UNKNOWN    " Print text only
*       abap_list              = SPACE    " Print ABAP List
*       use_archivename_def    = SPACE    " Note %_ARCHIVE-REPORT
*       default_spool_size     = C_CHAR_UNKNOWN    " Max. Width of Spool Request: 255 Characters
*       with_structure         = C_CHAR_UNKNOWN    " Print with Structure Information
*       suppress_shading       = C_CHAR_UNKNOWN    " Suppress Colors/Shading
*       po_fax_store           = SPACE    " Print Immediately (Print Parameter)
*       no_frames              = C_CHAR_UNKNOWN    " No Frame Characters
      IMPORTING
*       out_archive_parameters =     " List of the called up archiving parameters
        out_parameters         = es_out_params    " List of the called print parameter
        valid                  = e_valid
*       valid_for_spool_creation =     " Print parameters in OUT_PARAMETERS are valid for creating a
      EXCEPTIONS
        archive_info_not_found = 1
        invalid_print_params   = 2
        invalid_archive_params = 3
        OTHERS                 = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                 INTO DATA(l_message).
      RAISE EXCEPTION TYPE cx_bapi_error
        EXPORTING
          status = VALUE #( ( id = sy-msgid
                              type = sy-msgty
                              number = sy-msgno
                              message    = l_message
                              message_v1 = sy-msgv1
                              message_v2 = sy-msgv2
                              message_v3 = sy-msgv3
                              message_v4 = sy-msgv4 ) ).
    ENDIF.
  ENDMETHOD.

  METHOD job_close.

    CALL FUNCTION 'JOB_CLOSE'
      EXPORTING
*       at_opmode            = SPACE    " Job Start for Operation Mode: Operation Mode Name
*       at_opmode_periodic   = SPACE    " Job Start is Periodic According to Operation Mode
*       calendar_id          = SPACE    " Factory Calendar ID for Start Date Restriction
*       event_id             = SPACE    " Job Start After Event: Event Name
*       event_param          = SPACE    " Job Start After Event: Event Parameter
*       event_periodic       = SPACE    " Job Start is Periodic According to Event
        jobcount             = i_job_count    " Job number
        jobname              = i_job_name    " Job Name
*       laststrtdt           = NO_DATE    " No Start After: Date
*       laststrttm           = NO_TIME    " No Start After: Time
*       prddays              = 0    " Day Portion of Job Repetition Period
*       prdhours             = 0    " Minute Portion of Job Repetition Period
*       prdmins              = 0    " Hour Portion of Job Repetition Period
*       prdmonths            = 0    " Month Portion of Job Repetition Period
*       prdweeks             = 0    " Week Portion of Job Repetition Period
*       predjob_checkstat    = SPACE    " Start Job Only if Predecessor Runs Without Error
*       pred_jobcount        = SPACE    " Job Start After Predecessor Job: Predecessor Job Count
*       pred_jobname         = SPACE    " Job Start After Predecessor Job: Predecessor Job Name
*       sdlstrtdt            = NO_DATE    " Start Date of Background Job
*       sdlstrttm            = NO_TIME    " Time of Start Date of Background Job
*       startdate_restriction       = BTC_PROCESS_ALWAYS    " Restrictions for Start Date
        strtimmed            = abap_true    " Immediate Execution of Background Job
*       targetsystem         = SPACE    " Target System of Job
*       start_on_workday_not_before = SY-DATUM    " Earliest Start Date for Start 'On Workday'
*       start_on_workday_nr  = 0    " Number of Workday on Which Job is to Start
*       workday_count_direction     = 0    " Direction of Workday Count
*       recipient_obj        =     " The Mail Recipient of the Spool Lists
*       targetserver         = SPACE    " Target Application Server for Job
*       dont_release         = SPACE    " Job Not Released in Spite of Start Condition
*       targetgroup          = SPACE    " Target Application Server Group for Job
*       direct_start         =     " "X" - Start Without Converting to Time-Based Job
*      IMPORTING
*       job_was_released     =     " = 'X', if Job Was Released
*      CHANGING
*       ret                  =     " Special Additional Error Code
      EXCEPTIONS
        cant_start_immediate = 1
        invalid_startdate    = 2
        jobname_missing      = 3
        job_close_failed     = 4
        job_nosteps          = 5
        job_notex            = 6
        lock_failed          = 7
        invalid_target       = 8
        OTHERS               = 9.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                 INTO DATA(l_message).
      m_raise_bapi_error.
    ENDIF.
  ENDMETHOD.

  METHOD job_open.

    CLEAR r_jobcount.
    CALL FUNCTION 'JOB_OPEN'
      EXPORTING
*       delanfrep        = SPACE    " Do Not Use
*       jobgroup         = SPACE    " Group to Which the Job is to be Assigned
        jobname          = i_job_name    " Job Name
*       sdlstrtdt        = NO_DATE    " Do Not Use
*       sdlstrttm        = NO_TIME    " Do Not Use
*       jobclass         =     " Job classification
*       check_jobclass   =     " Reference type CHAR1 for background processing
      IMPORTING
        jobcount         = r_jobcount    " ID Number of Background Job
*       info             =
*      CHANGING
*       ret              =     " Special Additional Error Code
      EXCEPTIONS
        cant_create_job  = 1
        invalid_job_data = 2
        jobname_missing  = 3
        OTHERS           = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                 INTO DATA(l_message).
      m_raise_bapi_error.
    ENDIF.
  ENDMETHOD.

  METHOD popup_to_confirm.

    CLEAR r_answer.
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
*       titlebar       = SPACE    " Title of dialog box
*       diagnose_object       = SPACE    " Diagnosis text (maintain via SE61)
        text_question  = i_main_text    " Question text in dialog box
        text_button_1  = i_1st_bttn_txt    " Text on the first pushbutton
*       icon_button_1  = SPACE    " Icon on first pushbutton
        text_button_2  = i_2nd_bttn_txt    " Text on the second pushbutton
*       icon_button_2  = SPACE    " Icon on second pushbutton
*       default_button = '1'    " Cursor position
*       display_cancel_button = 'X'    " Button for displaying cancel pushbutton
*       userdefined_f1_help   = SPACE    " User-Defined F1 Help
*       start_column   = 25    " Column in which the POPUP begins
*       start_row      = 6    " Line in which the POPUP begins
*       popup_type     =     " Icon type
*       iv_quickinfo_button_1 = SPACE    " Quick Info on First Pushbutton
*       iv_quickinfo_button_2 = SPACE    " Quick Info on Second Pushbutton
      IMPORTING
        answer         = r_answer    " Return values: '1', '2', 'A'
*      TABLES
*       parameter      =     " Text transfer table for parameter in text
      EXCEPTIONS
        text_not_found = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                 INTO DATA(l_message).
      m_raise_bapi_error.
    ENDIF.
  ENDMETHOD.

  METHOD rs_create_variant.

    CALL FUNCTION 'RS_CREATE_VARIANT'
      EXPORTING
        curr_report               = i_program    " Report Name
        curr_variant              = i_variant    " Variant name
        vari_desc                 = is_descr    " Variant description (structure VARID)
      TABLES
        vari_contents             = it_contents    " Contents (STRUCTURE RSPARAMS)
        vari_text                 = it_texts    " Variant text (structure VARIT)
*       vscreens                  =
*       vari_contents_l           =     " Gen. Structure for Parameters and Select-Options (132 char.)
      EXCEPTIONS
        illegal_report_or_variant = 1
        illegal_variantname       = 2
        not_authorized            = 3
        not_executed              = 4
        report_not_existent       = 5
        report_not_supplied       = 6
        variant_exists            = 7
        variant_locked            = 8
        OTHERS                    = 9.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                 INTO DATA(l_message).
      m_raise_bapi_error.
    ENDIF.
  ENDMETHOD.

  METHOD rs_variant_contents.

    CLEAR rt_values.
    CALL FUNCTION 'RS_VARIANT_CONTENTS'
      EXPORTING
        report               = i_program    " Report name
        variant              = i_variant    " Variant name
        move_or_write        = 'M'    " 'W' external format, 'M' internal format
*       no_import            = SPACE
*       execute_direct       = SPACE
*          IMPORTING
*       sp                   =     " Search Help
      TABLES
*       l_params             =     " Table that contains variant parameters
*       l_params_nonv        =     " Tabelle that contains invisible parameters
*       l_selop              =     " Table that contains select options
*       l_selop_nonv         =     " Table that contains invisible select options
        valutab              = rt_values    " Table that contains the values (P + S)
*       valutabl             =
*       objects              =
*       free_selections_desc =
*       free_selections_value =
      EXCEPTIONS
        variant_non_existent = 1
        variant_obsolete     = 2
        OTHERS               = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                 INTO DATA(l_message).
      m_raise_bapi_error.
    ENDIF.
  ENDMETHOD.

  METHOD rs_variant_delete.

    CALL FUNCTION 'RS_VARIANT_DELETE'
      EXPORTING
        report               = i_program    " Name of report
        variant              = i_variant    " Name of variant to be deleted
        flag_confirmscreen   = zif_fi_finnet_constants=>mc_n    " Suppress Dialog Box
        flag_delallclient    = zif_fi_finnet_constants=>mc_n    " Suppress Dialog Box
*      IMPORTING
*       variant              =     " Name of variant to be deleted
      EXCEPTIONS
        not_authorized       = 1
        not_executed         = 2
        no_report            = 3
        report_not_existent  = 4
        report_not_supplied  = 5
        variant_locked       = 6
        variant_not_existent = 7
        no_corr_insert       = 8
        variant_protected    = 9
        OTHERS               = 10.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                 INTO DATA(l_message).
      m_raise_bapi_error.
    ENDIF.
  ENDMETHOD.

ENDCLASS.


CLASS lcl_db IMPLEMENTATION.
  METHOD chk_reguh.

    SELECT COUNT(*)
      FROM reguh
     WHERE laufd = i_execution_date
       AND laufi = i_execution_id.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_sql_error.
    ENDIF.
  ENDMETHOD.


  METHOD qry_bkpf.

    CLEAR rs_doc_header.
    SELECT bukrs gjahr belnr blart budat waers
        UP TO 1 ROWS
      FROM bkpf
      INTO rs_doc_header
     WHERE bukrs  = i_company_code
       AND gjahr  = i_fiscal_year
       AND belnr  = i_document_id
     ORDER BY PRIMARY KEY.
    ENDSELECT.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_sql_error.
    ENDIF.
  ENDMETHOD.


  METHOD qry_bseg.

    CLEAR rs_doc_item.
    SELECT bukrs gjahr belnr buzei augbl
           zuonr zlsch zlspr hkont hbkid
           dmbtr qbshb lifnr
        UP TO 1 ROWS
      FROM bseg
      INTO rs_doc_item
     WHERE bukrs = i_company_code
       AND gjahr = i_fiscal_year
       AND belnr = i_document_id
       AND buzei = i_item_id
     ORDER BY PRIMARY KEY.
    ENDSELECT.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_sql_error.
    ENDIF.
  ENDMETHOD.


  METHOD qry_bsik.

    CLEAR rt_payments_keys.
    SELECT gjahr belnr buzei zuonr zfbdt
           zbd1t
      FROM bsik
      INTO TABLE rt_payments_keys
     WHERE bukrs = i_company_code
       AND hbkid = i_house_bank_key
       AND zlsch = i_payment_method
       AND zlspr = space
       AND zuonr <> space
       AND shkzg = zif_fi_finnet_constants=>mc_posting_type-credit.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_sql_error.
    ENDIF.
  ENDMETHOD.


  METHOD qry_febep.

    CLEAR rs_entry.
    SELECT kukey gjahr nbbln vgint butxt
        UP TO 1 ROWS
      FROM febep
      INTO rs_entry
     WHERE kukey = i_short_key
       AND nbbln = i_payment_doc
     ORDER BY kukey DESCENDING nbbln DESCENDING.
    ENDSELECT.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_sql_error.
    ENDIF.
  ENDMETHOD.


  METHOD qry_lfa1.

    CLEAR rs_vendor.
    SELECT lifnr name1 stcd1
        UP TO 1 ROWS
      FROM lfa1
      INTO rs_vendor
     WHERE lifnr = i_vendor_code.
    ENDSELECT.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_sql_error.
    ENDIF.
  ENDMETHOD.


  METHOD qry_regup_reguh.

    CLEAR rs_payment.

    SELECT laufd laufi bukrs gjahr belnr
           buzei vblnr
        UP TO 1 ROWS
      FROM regup
      INTO rs_payment
     WHERE laufd = i_execution_date
       AND laufi = i_execution_id
       AND bukrs = i_company_code
       AND gjahr = i_fiscal_year
       AND belnr = i_document_id
       AND buzei = i_item_id
     ORDER BY PRIMARY KEY.
    ENDSELECT.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_sql_error.
    ELSE.
      SELECT zbnkn zbkon ubnkl ubknt ubkon
          UP TO 1 ROWS
        FROM reguh
        INTO (rs_payment-bank_account, rs_payment-bank_account_ctrl, rs_payment-own_bank,
              rs_payment-own_account,  rs_payment-own_account_ctrl)
        WHERE laufd = i_execution_date
          AND laufi = i_execution_id
        ORDER BY PRIMARY KEY.
      ENDSELECT.
    ENDIF.
  ENDMETHOD.


  METHOD qry_z_payms_snds.

    CLEAR rt_payms_snds.

    SELECT execution_date execution_id company_code   fiscal_year    document_id   item_id
           due_date       van_return   sending_status payment_status ff5_short_key
           created_at     created_by   last_changed_at modified_by                "Administrative data
      FROM zfi_snd_pym_2van AS sndhd
     INNER JOIN zfi_bnk_van_pym AS snddt
        ON snddt~sendind_id = sndhd~sending_id
      INTO TABLE rt_payms_snds
     WHERE execution_date IN ig_execution_date
       AND due_date       IN ig_due_date
       AND company_code   =  i_company_code
       AND fiscal_year    IN ig_fiscal_year
       AND document_id    IN ig_document_id
     ORDER BY execution_date execution_id company_code fiscal_year document_id item_id.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_sql_error.
    ENDIF.

  ENDMETHOD.

ENDCLASS.