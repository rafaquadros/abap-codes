  METHOD reject.
    CONSTANTS: lc_marketing TYPE swfdname    VALUE 'MARKETINGDECISION',
               lc_quality   TYPE swfdname    VALUE 'QUALITYDECISION',
               lc_ssg       TYPE swfdname    VALUE 'SSGDECISION'.
    DATA l_element_name TYPE swfdname.

    "Find the top WorkItem
    DATA(lv_objkey) = CONV swo_typeid( me->m_config_id ).
    DATA(lt_top_wl) = me->mr_api->sap_wapi_workitems_to_object(
          is_lpor = CORRESPONDING #( me->ms_lpor )
          ).
    TRY.
        DATA(l_top_wi) = lt_top_wl[ 1 ]-wi_id.
      CATCH cx_sy_itab_line_not_found.
        RETURN.
    ENDTRY.

    "Get current notes
    TRY.
        DATA(lr_wf_context) =  cl_swf_run_workitem_context=>get_instance( im_wiid = l_top_wi ).
*                         CATCH cx_swf_run_wim.  "
        DATA(lr_wf_container) = CAST cl_swf_cnt_container( lr_wf_context->if_wapi_workitem_context~get_wi_container( ) ).
      CATCH cx_swf_run_wim.
        RETURN.
    ENDTRY.

    "Get notes for this decision
    DATA(lt_notes) = mr_api->txw_textnote_edit( ).

    "Set notes into the workflow container
    IF lt_notes IS NOT INITIAL.
      TRY.
          lr_wf_container->if_swf_cnt_element_access_1~element_set_value(
          EXPORTING
            name                          = 'NOTES'    " Unique Name of Element
*          qname                         = qname    " Qualified Name of Element (Unique, String)
            value                         = lt_notes    " Element Value
*          unit                          = unit    " Element Unit
*        IMPORTING
*          exception_return              = exception_return    " First Exception That Occurred (If Queried: No RAISE)
            ).
*        CATCH cx_swf_cnt_cont_access_denied.    "
*        CATCH cx_swf_cnt_elem_not_found.    "
*        CATCH cx_swf_cnt_elem_access_denied.    "
*        CATCH cx_swf_cnt_elem_type_conflict.    "
*        CATCH cx_swf_cnt_unit_type_conflict.    "
*        CATCH cx_swf_cnt_elem_def_invalid.    "
*        CATCH cx_swf_cnt_invalid_qname.    "
*        CATCH cx_swf_cnt_container.    "
        CATCH cx_swf_cnt_cont_access_denied
          cx_swf_cnt_elem_not_found
          cx_swf_cnt_elem_access_denied
          cx_swf_cnt_elem_type_conflict
          cx_swf_cnt_unit_type_conflict
          cx_swf_cnt_elem_def_invalid
          cx_swf_cnt_invalid_qname
          cx_swf_cnt_container.
          RETURN.
      ENDTRY.
    ENDIF.

    CASE iv_wf_tsk_fnc.
      WHEN zif_gpe_lt_c_wf=>mc_wf_tsk_fnc-marketing.
        l_element_name = lc_marketing.
      WHEN zif_gpe_lt_c_wf=>mc_wf_tsk_fnc-quality.
        l_element_name = lc_quality.
      WHEN zif_gpe_lt_c_wf=>mc_wf_tsk_fnc-ssg.
        l_element_name = lc_ssg.
      WHEN OTHERS.
        RETURN.
    ENDCASE.

    TRY.
        lr_wf_container->if_swf_cnt_element_access_1~element_set_value(
        EXPORTING
          name                          = l_element_name    " Unique Name of Element
*          qname                         = qname    " Qualified Name of Element (Unique, String)
          value                         = zif_gpe_lt_c_wf=>mc_decision_result-rejected    " Element Value
*          unit                          = unit    " Element Unit
*        IMPORTING
*          exception_return              = exception_return    " First Exception That Occurred (If Queried: No RAISE)
          ).
*        CATCH cx_swf_cnt_cont_access_denied.    "
*        CATCH cx_swf_cnt_elem_not_found.    "
*        CATCH cx_swf_cnt_elem_access_denied.    "
*        CATCH cx_swf_cnt_elem_type_conflict.    "
*        CATCH cx_swf_cnt_unit_type_conflict.    "
*        CATCH cx_swf_cnt_elem_def_invalid.    "
*        CATCH cx_swf_cnt_invalid_qname.    "
*        CATCH cx_swf_cnt_container.    "

        lr_wf_container->save_to_database( ).
*          CATCH cx_swf_cnt_invalid_por.    "
        lr_wf_context->publish( ).

        me->raise_action_executed( ).
*          CATCH cx_bapi_error.    "
      CATCH cx_swf_cnt_cont_access_denied
        cx_swf_cnt_elem_not_found
        cx_swf_cnt_elem_access_denied
        cx_swf_cnt_elem_type_conflict
        cx_swf_cnt_unit_type_conflict
        cx_swf_cnt_elem_def_invalid
        cx_swf_cnt_invalid_qname
        cx_swf_cnt_invalid_por
        cx_swf_cnt_container
        cx_bapi_error.
        RETURN.
    ENDTRY.

    COMMIT WORK.
  ENDMETHOD.
