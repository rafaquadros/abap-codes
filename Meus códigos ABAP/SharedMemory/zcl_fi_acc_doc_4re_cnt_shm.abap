CLASS zcl_fi_acc_doc_4re_cnt_shm DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC
  SHARED MEMORY ENABLED.

  PUBLIC SECTION.
    INTERFACES if_shm_build_instance.

    METHODS:
      get_request RETURNING VALUE(rs_request) TYPE zefi_ctr_post_request,
      get_result  RETURNING VALUE(rs_result)  TYPE zefi_ctr_post_result,
      set_request IMPORTING is_request TYPE zefi_ctr_post_request,
      set_result  IMPORTING is_result  TYPE zefi_ctr_post_result.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      ms_request TYPE zefi_ctr_post_request,
      ms_result  TYPE zefi_ctr_post_result.
ENDCLASS.



CLASS zcl_fi_acc_doc_4re_cnt_shm IMPLEMENTATION.
  METHOD if_shm_build_instance~build.
    DATA: lr_area TYPE REF TO zcl_fi_accd_4rec_shma_area,
          lr_root TYPE REF TO zcl_fi_acc_doc_4re_cnt_shm.

    TRY.
        lr_area = zcl_fi_accd_4rec_shma_area=>attach_for_write(
*                      inst_name   = cl_shm_area=>default_instance
*                      attach_mode = cl_shm_area=>attach_mode_default
*                      wait_time   = 0
                         ).
*                    CATCH cx_shm_exclusive_lock_active.
*                    CATCH cx_shm_version_limit_exceeded.
      CATCH cx_shm_error INTO DATA(lx_shm_error).
        RAISE EXCEPTION TYPE cx_shm_build_failed
          EXPORTING
            previous = lx_shm_error.
    ENDTRY.

*    lr_root = NEW #(  ).
    CREATE OBJECT lr_root AREA HANDLE lr_area.

    lr_root->set_request( is_request = VALUE #( ) ).
    lr_root->set_result( is_result = VALUE #(  ) ).

    lr_area->set_root( root = lr_root ).
*    CATCH cx_shm_initial_reference.
*    CATCH cx_shm_wrong_handle.

    TRY.
        lr_area->detach_commit( ).
*    CATCH cx_shm_wrong_handle.
*    CATCH cx_shm_already_detached.
*    CATCH cx_shm_secondary_commit.
*    CATCH cx_shm_event_execution_failed.
*    CATCH cx_shm_completion_error.
      CATCH cx_shm_wrong_handle ##NO_HANDLER
            cx_shm_already_detached
            cx_shm_secondary_commit
            cx_shm_event_execution_failed
            cx_shm_completion_error.
    ENDTRY.

  ENDMETHOD.

  METHOD get_request.

    CLEAR rs_request.
    rs_request = me->ms_request.
  ENDMETHOD.

  METHOD get_result.

    CLEAR rs_result.
    rs_result = me->ms_result.
  ENDMETHOD.

  METHOD set_request.

    me->ms_request = is_request.
  ENDMETHOD.

  METHOD set_result.

    me->ms_result = is_result.
  ENDMETHOD.

ENDCLASS.