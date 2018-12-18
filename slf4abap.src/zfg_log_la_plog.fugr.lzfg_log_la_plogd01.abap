*----------------------------------------------------------------------*
***INCLUDE LZFG_LOG_LA_PLOGD01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&       Class lcl_handle_salv_events
*&---------------------------------------------------------------------*
CLASS lcl_handle_salv_events DEFINITION.
  PUBLIC SECTION.
    METHODS:
      on_user_command FOR EVENT added_function OF cl_salv_events
        IMPORTING e_salv_function.
ENDCLASS.
*&---------------------------------------------------------------------*
*&       Class (Implementation)  lcl_handle_salv_events
*&---------------------------------------------------------------------*
CLASS lcl_handle_salv_events IMPLEMENTATION.
  METHOD on_user_command.
    PERFORM handle_salv_user_command USING e_salv_function.
  ENDMETHOD.
ENDCLASS.               "lcl_handle_salv_events
*&---------------------------------------------------------------------*
*&       Class LCL_HANDLE_TIMER_EVENTS
*&---------------------------------------------------------------------*
CLASS lcl_handle_timer_events DEFINITION.

  PUBLIC SECTION.
    METHODS timer_finished FOR EVENT finished OF cl_gui_timer.

ENDCLASS.
*&---------------------------------------------------------------------*
*&       Class (Implementation)  LCL_HANDLE_TIMER_EVENTS
*&---------------------------------------------------------------------*
CLASS lcl_handle_timer_events IMPLEMENTATION.

  METHOD timer_finished.

    DATA lt_log TYPE ztt_log_la_plog.
    DATA ls_msg TYPE bapi_msg.

    DATA lr_shared_object_root TYPE REF TO zcl_log_la_plog_root.

    TRY.

        DATA(lr_shared_object_area) = zcl_log_la_plog_area=>attach_for_update(
*                                  inst_name                    = CL_SHM_AREA=>DEFAULT_INSTANCE
                                  attach_mode                   = cl_shm_area=>attach_mode_wait
                                  wait_time                     = 200
                              ).

        lr_shared_object_area->root->set_last_read( ).

        IF NOT lr_shared_object_area->root->is_empty( ).
          lt_log = lr_shared_object_area->root->remove( ).
        ENDIF.

        lr_shared_object_area->detach_commit( ).

      CATCH cx_shm_version_limit_exceeded.
      CATCH cx_shm_pending_lock_removed.
      CATCH cx_shm_wrong_handle.
      CATCH cx_shm_already_detached.
      CATCH cx_shm_inconsistent.
      CATCH cx_shm_no_active_version.
      CATCH cx_shm_read_lock_active.
      CATCH cx_shm_exclusive_lock_active.
      CATCH cx_shm_parameter_error.
      CATCH cx_shm_change_lock_active.
      CATCH cx_shm_secondary_commit.
      CATCH cx_shm_event_execution_failed.
      CATCH cx_shm_completion_error.

    ENDTRY.

    IF lt_log IS NOT INITIAL.

      INSERT LINES OF lt_log INTO TABLE gt_log.

*-- filter log table
      PERFORM filter_log.

*-- set log buttons filtered
      PERFORM set_log_btn_filtered.

*-- refresh alv
      PERFORM refresh_salv.

    ENDIF.

*-- call timer again
    PERFORM build_timer.

  ENDMETHOD.

ENDCLASS.               "LCL_HANDLE_TIMER_EVENTS
