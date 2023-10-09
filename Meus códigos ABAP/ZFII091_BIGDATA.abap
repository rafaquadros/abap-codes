*&---------------------------------------------------------------------*
*&  Include  ZFII091_BIGDATA
*&---------------------------------------------------------------------*
*Extração de dados BigData - Transação S_ALR_87011964 - Extração de PEP's para CNS43
*Grava Dados na Tabela Z
DATA:
  "tl_zfit024_2                TYPE STANDARD TABLE OF zfit024 WITH HEADER LINE, <<< META-rgq = Solution review
  o_tvarv_2                   TYPE REF TO zcl_stvarv, "Objeto da TVARV
  gt_structured_2             TYPE STANDARD TABLE OF zfit030, "Tabela CNS43
  v_variant                   TYPE zfit024-variant,
  vl_variant                  TYPE raldb_vari,
  v_max_commit_2              TYPE  sy-tabix,
  v_commit_2                  TYPE  sy-tabix,
  v_lines_tot_2               TYPE  sy-tabix,
  v_uname                     TYPE  sy-uname,
  v_debug_on_off              TYPE  abap_bool,
  v_eventid_2                 LIKE  tbtcm-eventid,
  v_eventparm_2               LIKE  tbtcm-eventparm,
  v_external_program_active_2 LIKE  tbtcm-xpgactive,
  v_jobcount_2                LIKE  tbtcm-jobcount,
  v_jobname_2                 LIKE  tbtcm-jobname,
  v_varv_jobname_2            LIKE  tbtcm-jobname,
  v_stepcount_2               LIKE  tbtcm-stepcount,
  v_len_2                     TYPE  i,
  vg_datum_2                  TYPE sy-datum,
  gr_guid_gen_2               TYPE REF TO if_system_uuid.

IF sy-batch = abap_on or sy-uname = 'PJBIGDATA2'.

  gr_guid_gen_2 = cl_uuid_factory=>create_system_uuid( ).
  vg_datum_2 = sy-datum.

  CALL FUNCTION 'GET_JOB_RUNTIME_INFO'
    IMPORTING
      eventid                 = v_eventid_2
      eventparm               = v_eventparm_2
      external_program_active = v_external_program_active_2
      jobcount                = v_jobcount_2
      jobname                 = v_jobname_2
      stepcount               = v_stepcount_2
    EXCEPTIONS
      no_runtime_info         = 1
      OTHERS                  = 2.
  IF sy-subrc IS INITIAL or sy-uname = 'PJBIGDATA2'.

    CREATE OBJECT o_tvarv_2
      EXPORTING
        prefix    = 'ZFIR091'
        separator = '-'.

    CALL METHOD o_tvarv_2->get_parameter
      EXPORTING
        suffix = 'JOB_NAME_1'
      IMPORTING
        value  = v_varv_jobname_2.

    CALL METHOD o_tvarv_2->get_parameter
      EXPORTING
        suffix = 'MAX_COMMIT'
      IMPORTING
        value  = v_max_commit_2.

    CALL METHOD o_tvarv_2->get_parameter
      EXPORTING
        suffix = 'DEBUG_USER'
      IMPORTING
        value  = v_uname.

    v_len_2 = strlen( v_varv_jobname_2 ).
    IF v_jobname_2(v_len_2) = v_varv_jobname_2 or sy-uname = 'PJBIGDATA2'.

      IF sy-uname = v_uname.
        SELECT SINGLE low
          FROM tvarvc
          INTO v_debug_on_off
         WHERE name = 'ZFIR091-DEBUG_ON_OFF'
           AND type = 'P'
           AND numb = '0000'.
        IF sy-subrc IS INITIAL.
          MESSAGE i137(zfi) WITH 'Modo Debug Ligado'.
          MESSAGE i137(zfi) WITH 'Verificar o parâmetro' 'ZFIR091-DEBUG_USER' 'para desligar'.
          MESSAGE i137(zfi) WITH 'Verificar o parâmetro' 'ZFIR091-DEBUG_ON_OFF' 'para desligar'.
          WHILE v_debug_on_off = abap_on.
            WAIT UP TO 5 SECONDS."Aguarda 5 segundos para próxima verificação

            SELECT SINGLE low
              FROM tvarvc
              INTO v_debug_on_off
             WHERE name = 'ZFIR091-DEBUG_ON_OFF'
               AND type = 'P'
               AND numb = '0000'.
          ENDWHILE.
        ENDIF.
      ENDIF.

      TRY.
          DATA(l_processamento_2) = gr_guid_gen_2->create_uuid_x16( ).
*                              CATCH cx_uuid_error.  "
        CATCH cx_uuid_error ##NO_HANDLER.
      ENDTRY.

      v_variant = srtvr.
*      SELECT * >>> META-rgq - Solution review
*        INTO TABLE tl_zfit024_2
*        FROM zfit024
*       WHERE main_programm = 'ZFIR091'
*         AND subm_programm = 'RABEST_ALV01'
*         AND variant       = v_variant.
*      IF sy-subrc IS INITIAL. <<< META-rgq = Solution review
*          Elimina processamento com a mesma Variante e Data
      BREAK-POINT 'PJBIGDATA2'.
      SELECT variante
        INTO vl_variant
        FROM zfit030
          UP TO 1 ROWS
       WHERE variante  = v_variant
         AND data_proc = vg_datum_2.
      ENDSELECT.
      IF sy-subrc IS INITIAL.
        IF sy-batch = abap_on.
          MESSAGE i137(zfi) WITH 'Já foi realizada extração data:' vg_datum_2 'Variante:' v_variant.
          MESSAGE i137(zfi) WITH 'Os dados serão eliminados,' 'e inseridos novamente' 'conforme a nova extração.'.
        ENDIF.

        DELETE FROM zfit030 WHERE variante  EQ v_variant
                              AND data_proc EQ vg_datum_2.
        IF sy-batch = abap_on.
          MESSAGE i137(zfi) WITH 'Tabela ZFIT030:' sy-dbcnt 'eliminados'.
          COMMIT WORK.
        ENDIF.
      ENDIF.

      "SORT tl_zfit024_2 BY id_sequencia. <<< META-rgq = Solution review
      CLEAR: v_commit_2.
* >>> META-rgq - Solution review
      DATA: lr_std_structure TYPE REF TO cl_abap_structdescr,
            lr_cst_structure TYPE REF TO cl_abap_structdescr.

      lr_cst_structure = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_name( p_name = 'ZFIT030' ) ).
      DATA(lt_cst_components) = lr_cst_structure->get_components( ).

      TRY.
        lr_cst_structure = CAST cl_abap_structdescr( lt_cst_components[ name = 'REGULARES' ]-TYPE ).
        lt_cst_components = lr_cst_structure->get_components( ).
      CATCH cx_sy_itab_line_not_found ##NO_HANDLER.
      ENDTRY.
* <<< META-rgq = Solution review
      LOOP AT <itab_data> ASSIGNING <itab_line>.
        ADD 1 TO v_commit_2.
        TRY.
            DATA(l_detalhe_2) = gr_guid_gen_2->create_uuid_x16( ).
*                          CATCH cx_uuid_error.  "
          CATCH cx_uuid_error ##NO_HANDLER.
        ENDTRY.

        APPEND INITIAL LINE TO gt_structured_2 ASSIGNING FIELD-SYMBOL(<ls_structure_2>).
        <ls_structure_2>-processamento = l_processamento_2.
        <ls_structure_2>-detalhe = l_detalhe_2.

        CONVERT DATE sy-datum TIME sy-uzeit INTO TIME STAMP <ls_structure_2>-criado_em TIME ZONE 'UTC'."sy-zonlo.
        <ls_structure_2>-modificado_em = <ls_structure_2>-criado_em.
        <ls_structure_2>-criado_por = <ls_structure_2>-modificado_por = sy-uname.
        <ls_structure_2>-data_proc = vg_datum_2.
        <ls_structure_2>-variante = v_variant.

*          LOOP AT tl_zfit024_2. >>> META-rgq - Solution review
*Origem
*            ASSIGN COMPONENT tl_zfit024_2-fieldname_de OF STRUCTURE <itab_line> TO FIELD-SYMBOL(<l_field_de_2>).
*Destino
*            ASSIGN COMPONENT tl_zfit024_2-fieldname_para OF STRUCTURE <ls_structure_2> TO FIELD-SYMBOL(<l_field_para_2>).
*            IF <l_field_de_2> IS ASSIGNED AND <l_field_para_2> IS ASSIGNED.
*              <l_field_para_2> = <l_field_de_2>.
*            ENDIF.
*            UNASSIGN: <l_field_de_2>, <l_field_para_2>.
*          ENDLOOP. <<< META-rgq = Solution review
* >>> META-rgq - Solution review
        lr_std_structure = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data( p_data = <itab_line> ) ).
        DATA(lt_std_components) = lr_std_structure->get_components(  ).

        LOOP AT lt_std_components ASSIGNING FIELD-SYMBOL(<ls_std_component>).
          ASSIGN COMPONENT <ls_std_component>-name OF STRUCTURE <itab_line> TO FIELD-SYMBOL(<l_field_de_2>).
          IF sy-subrc <> 0.
            CONTINUE.
          ENDIF.
          GET REFERENCE OF <l_field_de_2> INTO DATA(lr_field_de_2).
          DATA(lr_type_from) = cl_abap_typedescr=>describe_by_data_ref( lr_field_de_2 ).
          DATA(l_type_name_from) = lr_type_from->get_relative_name( ).
          LOOP AT lt_cst_components ASSIGNING FIELD-SYMBOL(<ls_cst_component>).
            ASSIGN COMPONENT <ls_cst_component>-name OF STRUCTURE <ls_structure_2> TO FIELD-SYMBOL(<l_field_para_2>).
            IF sy-subrc <> 0.
              EXIT.
            ENDIF.
            GET REFERENCE OF <l_field_para_2> INTO DATA(lr_field_para_2).
            DATA(lr_type_to) = cl_abap_typedescr=>describe_by_data_ref( lr_field_para_2 ).
            DATA(l_type_name_to) = lr_type_to->get_relative_name( ).
            IF l_type_name_to = l_type_name_from OR
               ( l_type_name_from = 'AM_POSID' AND
                 l_type_name_to   = 'ZDEFI_PEP' ) OR
               ( <ls_std_component>-name = 'BTR1' AND
                 l_type_name_from = 'REPBETRAG' AND
                 l_type_name_to = 'URWRT') OR
               ( <ls_std_component>-name = 'BTR2' AND
                 l_type_name_from = 'REPBETRAG' AND
                 l_type_name_to = 'KNAFA') OR
               ( <ls_std_component>-name = 'BTR3' AND
                 l_type_name_from = 'REPBETRAG' AND
                 l_type_name_to = 'ANBTR').
              <l_field_para_2> = <l_field_de_2>.
              UNASSIGN <l_field_para_2>.
              EXIT.
            ENDIF.
            UNASSIGN <l_field_para_2>.
          ENDLOOP.
          UNASSIGN <l_field_de_2>.
        ENDLOOP.
* <<< META-rgq = Solution review
        IF v_commit_2 >= v_max_commit_2.
          CLEAR v_commit_2.
          INSERT zfit030 FROM TABLE gt_structured_2.
          IF sy-subrc IS INITIAL.
            COMMIT WORK.
          ENDIF.
          DATA(v_lines_2) = lines( gt_structured_2 ).
          ADD v_lines_2 TO v_lines_tot_2.
          CLEAR gt_structured_2.
        ENDIF.
      ENDLOOP.
*      ELSE. >>> META-rgq - Solution review
*        MESSAGE i137(zfi) WITH 'Não foi encontrado' 'configuração na tabela ZFIT024'.
*        MESSAGE i137(zfi) WITH 'Programa Principal = ZFIR091' 'Programa Submit = RABEST_ALV01'
*                               'Variante = Z106'.
*      ENDIF. <<< META-rgq = Solution review

      IF NOT v_commit_2 IS INITIAL.
        INSERT zfit030 FROM TABLE gt_structured_2.
        IF sy-subrc IS INITIAL.
          COMMIT WORK.
        ENDIF.
        v_lines_2 = lines( gt_structured_2 ).
        ADD v_lines_2 TO v_lines_tot_2.
      ENDIF.

      MESSAGE i137(zfi) WITH 'Extração data:' vg_datum_2 'Variate:' v_variant.
      MESSAGE i137(zfi) WITH 'Total Linhas inseridas:' v_lines_tot_2.

    ENDIF.
  ENDIF.
ENDIF.