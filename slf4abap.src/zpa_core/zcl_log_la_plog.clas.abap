class ZCL_LOG_LA_PLOG definition
  public
  inheriting from ZCL_LOG_LA
  create public .

public section.
  type-pools ABAP .
  type-pools ICON .

  methods CONSTRUCTOR
    importing
      !IV_POPUP type ABAP_BOOL default ABAP_FALSE .

  methods INIT
    redefinition .
  methods SAVE
    redefinition .
protected section.
private section.

  data MV_POPUP type ABAP_BOOL .
  data MR_SHARED_OBJECT_AREA type ref to ZCL_LOG_LA_PLOG_AREA .

  methods SHOW_POPUP
    importing
      !IT_LOG type ZTT_LOG_LA_PLOG
      !IV_POPUP type ABAP_BOOL
      !IV_IMMEDIATE_FLUSH type ABAP_BOOL
    exporting
      !ER_LOG type ref to ZCL_LOG .
  methods MAP_LOG_TO_POPUP_TABLE
    importing
      !IT_LOG type TT_LOG
    exporting
      !ET_LOG type ZTT_LOG_LA_PLOG .
  methods MAP_TYPE_TO_ICON
    importing
      !IV_TYPE type CHAR01
    returning
      value(RV_TYPE_ICON) type ICON_D .
  methods WRITE_LOG_TO_SHARE_OBJECT
    importing
      !IT_LOG type ZTT_LOG_LA_PLOG
    exporting
      !EV_POPUP_ONLINE type ABAP_BOOL
      !ER_LOG type ref to ZCL_LOG .
  methods BUILD_SHARE_OBJECT
    exporting
      !ER_LOG type ref to ZCL_LOG .
ENDCLASS.



CLASS ZCL_LOG_LA_PLOG IMPLEMENTATION.


METHOD build_share_object.

  DATA lr_shared_object_root TYPE REF TO zcl_log_la_plog_root.
  DATA lr_shared_object_area TYPE REF TO zcl_log_la_plog_area.
  DATA lv_not_active TYPE abap_bool.

*-- init
  er_log = zcl_log=>get_instance( ).

  TRY.

      lr_shared_object_area = zcl_log_la_plog_area=>attach_for_read(
*                                  inst_name = CL_SHM_AREA=>DEFAULT_INSTANCE
                               ).
      lr_shared_object_area->detach( ).

    CATCH cx_shm_wrong_handle.
    CATCH cx_shm_already_detached.
    CATCH cx_shm_inconsistent.
    CATCH cx_shm_no_active_version.
      lv_not_active = abap_true.
    CATCH cx_shm_read_lock_active.
    CATCH cx_shm_exclusive_lock_active.
    CATCH cx_shm_parameter_error.
    CATCH cx_shm_change_lock_active.

  ENDTRY.

*-- build share object
  TRY.

      IF lv_not_active = abap_true.

        lr_shared_object_area = zcl_log_la_plog_area=>attach_for_write(
          inst_name   = cl_shm_area=>default_instance
          attach_mode = cl_shm_area=>attach_mode_wait
          wait_time   = 200 ).

        CREATE OBJECT lr_shared_object_root AREA HANDLE lr_shared_object_area.

        lr_shared_object_area->set_root( root = lr_shared_object_root ).

      ELSE.

        lr_shared_object_area = zcl_log_la_plog_area=>attach_for_update(
*                            inst_name                     = CL_SHM_AREA=>DEFAULT_INSTANCE
                             attach_mode                   = cl_shm_area=>attach_mode_wait
                             wait_time                     = 200
                              ).
      ENDIF.

      lr_shared_object_area->root->set_last_read( ).

      lr_shared_object_area->detach_commit( ).

    CATCH cx_shm_inconsistent INTO DATA(lr_shm_inconsistent).
      er_log->import_msg_from_cx_msg( ir_exception = lr_shm_inconsistent ).
      RETURN.

    CATCH cx_shm_no_active_version INTO DATA(lr_shm_no_active_version).
*      er_log->import_msg_from_cx_msg( ir_exception = lr_shm_no_active_version ).
      RETURN.

    CATCH cx_shm_exclusive_lock_active INTO DATA(lr_shm_exclusive_lock_active).
      er_log->import_msg_from_cx_msg( ir_exception = lr_shm_exclusive_lock_active ).
      RETURN.

    CATCH cx_shm_version_limit_exceeded INTO DATA(lr_shm_version_limit_exceeded).
      er_log->import_msg_from_cx_msg( ir_exception = lr_shm_version_limit_exceeded ).
      RETURN.

    CATCH cx_shm_change_lock_active INTO DATA(lr_shm_change_lock_active).
      er_log->import_msg_from_cx_msg( ir_exception = lr_shm_change_lock_active ).
      RETURN.

    CATCH cx_shm_parameter_error INTO DATA(lr_shm_parameter_error).
      er_log->import_msg_from_cx_msg( ir_exception = lr_shm_parameter_error ).
      RETURN.

    CATCH cx_shm_pending_lock_removed INTO DATA(lr_shm_pending_lock_removed).
      er_log->import_msg_from_cx_msg( ir_exception = lr_shm_pending_lock_removed ).
      RETURN.

    CATCH cx_shm_initial_reference INTO DATA(lr_shm_initial_reference).
      er_log->import_msg_from_cx_msg( ir_exception = lr_shm_initial_reference ).
      RETURN.

    CATCH cx_shm_wrong_handle INTO DATA(lr_shm_wrong_handle).
      er_log->import_msg_from_cx_msg( ir_exception = lr_shm_wrong_handle ).
      RETURN.

    CATCH cx_shm_already_detached INTO DATA(lr_shm_already_detached).
      er_log->import_msg_from_cx_msg( ir_exception = lr_shm_already_detached ).
      RETURN.

    CATCH cx_shm_secondary_commit INTO DATA(lr_shm_secondary_commit).
      er_log->import_msg_from_cx_msg( ir_exception = lr_shm_secondary_commit ).
      RETURN.

    CATCH cx_shm_event_execution_failed INTO DATA(lr_shm_event_execution_failed).
      er_log->import_msg_from_cx_msg( ir_exception = lr_shm_event_execution_failed ).
      RETURN.

    CATCH cx_shm_completion_error INTO DATA(lr_shm_completion_error).
      er_log->import_msg_from_cx_msg( ir_exception = lr_shm_completion_error ).
      RETURN.

  ENDTRY.

ENDMETHOD.


METHOD constructor.

*-- call super constructor
  super->constructor( ).

*-- init
  mv_popup = iv_popup.

ENDMETHOD.


METHOD init.

  DATA lr_log TYPE REF TO zcl_log.

*-- init
  er_log = zcl_log=>get_instance( ).

  CLEAR lr_log.

  super->init(
    EXPORTING
      iv_immediate_flush = iv_immediate_flush
    IMPORTING
      er_log             = lr_log
  ).

  er_log->import_msg_from_logger( ir_instance = lr_log ).

  IF lr_log->is_error( ).
    RETURN.
  ENDIF.

*-- init shared object
  IF is_immediate_flush( ).

    CLEAR lr_log.

    me->build_share_object(
      IMPORTING
        er_log = lr_log
           ).

    er_log->import_msg_from_logger( ir_instance = lr_log ).

    IF lr_log->is_error( ).
      RETURN.
    ENDIF.

  ENDIF.

ENDMETHOD.


METHOD map_log_to_popup_table.

  DATA lv_msg TYPE string.
  DATA lv_type_icon TYPE icon_d.

*-- init
  CLEAR et_log.

  IF it_log IS INITIAL.
    RETURN.
  ENDIF.

*-- map
  LOOP AT it_log INTO DATA(lr_log).

    CLEAR lv_msg.
    CLEAR lv_type_icon.

*- map type icon
    lv_type_icon = me->map_type_to_icon( iv_type = lr_log->ms_log-type ).

*- build message
    MESSAGE ID lr_log->ms_log-id
            TYPE 'S'
            NUMBER lr_log->ms_log-number
            WITH lr_log->ms_log-message_v1
                 lr_log->ms_log-message_v2
                 lr_log->ms_log-message_v3
                 lr_log->ms_log-message_v4
            INTO lv_msg.

*- map message
    INSERT VALUE #( type_icon = lv_type_icon
                    type = lr_log->ms_log-type
                    date = lr_log->ms_log-date
                    time = lr_log->ms_log-time
                    msg = lv_msg
                   ) INTO TABLE et_log.

  ENDLOOP.

ENDMETHOD.


METHOD map_type_to_icon.

*-- init
  CLEAR rv_type_icon.

*-- map type to icon
  CASE iv_type.

    WHEN zcl_log=>gc_type_debug.
      rv_type_icon = icon_location.

    WHEN zcl_log=>gc_type_error.
      rv_type_icon = icon_message_error_small.

    WHEN zcl_log=>gc_type_fatal.
      rv_type_icon = icon_message_critical_small.

    WHEN zcl_log=>gc_type_info.
      rv_type_icon = icon_message_information_small.

    WHEN zcl_log=>gc_type_warning.
      rv_type_icon = icon_message_warning_small.

    WHEN zcl_log=>gc_type_user.
      rv_type_icon = icon_position_hr.

    WHEN OTHERS.
      rv_type_icon = icon_space.

  ENDCASE.

ENDMETHOD.


METHOD save.

  DATA lr_log TYPE REF TO zcl_log.
  DATA lv_popup_online TYPE abap_bool.

*-- init
  er_log = zcl_log=>get_instance( ).

  IF it_log IS INITIAL.
    RETURN.
  ENDIF.

*-- map log to popup table
  me->map_log_to_popup_table(
    EXPORTING
      it_log = it_log
    IMPORTING
      et_log = DATA(lt_log)
         ).

*-- write log to share object
  IF is_immediate_flush( ).

    CLEAR lr_log.

    me->write_log_to_share_object(
      EXPORTING
        it_log              = lt_log
      IMPORTING
        ev_popup_online     = lv_popup_online
        er_log              = lr_log
           ).

    er_log->import_msg_from_logger( ir_instance = lr_log ).

    IF lr_log->is_error( ).
      RETURN.
    ENDIF.

    CLEAR lt_log.

  ELSE.
    lv_popup_online = abap_false.
  ENDIF.

*-- show popup
  IF lv_popup_online = abap_false.

    CLEAR lr_log.

    me->show_popup(
      EXPORTING
        it_log = lt_log
        iv_popup = mv_popup
        iv_immediate_flush = mv_immediate_flush
      IMPORTING
        er_log = lr_log
           ).

    er_log->import_msg_from_logger( ir_instance = lr_log ).

    IF lr_log->is_error( ).
      RETURN.
    ENDIF.

  ENDIF.

ENDMETHOD.


METHOD show_popup.

*-- init
  er_log = zcl_log=>get_instance( ).

*-- show popup
  CALL FUNCTION 'ZFM_LOG_LA_PLOG'
    STARTING NEW TASK 'TPLOG'
    DESTINATION 'NONE'
    EXPORTING
      it_log                = it_log
      iv_popup              = iv_popup
      iv_immediate_flush    = iv_immediate_flush
    EXCEPTIONS
      communication_failure = 1
      system_failure        = 2
      resource_failure      = 3
      OTHERS                = 4.

  IF sy-subrc <> 0.
    er_log->import_msg_from_symsg( ).
    RETURN.
  ENDIF.

ENDMETHOD.


METHOD write_log_to_share_object.

  DATA lr_log TYPE REF TO zcl_log.

*-- init
  er_log = zcl_log=>get_instance( ).

  CLEAR ev_popup_online.

  IF it_log IS INITIAL.
    RETURN.
  ENDIF.

*-- write log to shared object
  TRY.

*- attach for update incl. lock
      DATA(lr_shared_object_area) = zcl_log_la_plog_area=>attach_for_update(
*                                 inst_name                     = CL_SHM_AREA=>DEFAULT_INSTANCE
                                  attach_mode                   = cl_shm_area=>attach_mode_wait
                                  wait_time                     = 200
                        ).

*- add log to queue
      lr_shared_object_area->root->add( it_log = it_log ).

*- check if popup is online
      CLEAR lr_log.

      lr_shared_object_area->root->is_popup_online(
        IMPORTING
          ev_result = ev_popup_online
          er_log    = lr_log
             ).

      er_log->import_msg_from_logger( ir_instance = lr_log ).

      IF ev_popup_online = abap_false.
        lr_shared_object_area->root->set_popup_open( ).
        lr_shared_object_area->root->set_last_read( ).
      ENDIF.

*- unlock
      lr_shared_object_area->detach_commit( ).

    CATCH cx_shm_wrong_handle INTO DATA(lr_shm_wrong_handle).
      er_log->import_msg_from_cx_msg( ir_exception = lr_shm_wrong_handle ).
      RETURN.

    CATCH cx_shm_already_detached INTO DATA(lr_shm_already_detached).
      er_log->import_msg_from_cx_msg( ir_exception = lr_shm_already_detached ).
      RETURN.

    CATCH cx_shm_secondary_commit INTO DATA(lr_shm_secondary_commit).
      er_log->import_msg_from_cx_msg( ir_exception = lr_shm_secondary_commit ).
      RETURN.

    CATCH cx_shm_event_execution_failed INTO DATA(lr_shm_event_execution_failed).
      er_log->import_msg_from_cx_msg( ir_exception = lr_shm_event_execution_failed ).
      RETURN.

    CATCH cx_shm_completion_error INTO DATA(lr_shm_completion_error).
      er_log->import_msg_from_cx_msg( ir_exception = lr_shm_completion_error ).
      RETURN.

    CATCH cx_shm_inconsistent INTO DATA(lr_shm_inconsistent).
      er_log->import_msg_from_cx_msg( ir_exception = lr_shm_inconsistent ).
      RETURN.

    CATCH cx_shm_no_active_version INTO DATA(lr_shm_no_active_version).
*      er_log->import_msg_from_cx_msg( ir_exception = lr_shm_inconsistent ).
      RETURN.

    CATCH cx_shm_exclusive_lock_active INTO DATA(lr_shm_exclusive_lock_active).
      er_log->import_msg_from_cx_msg( ir_exception = lr_shm_exclusive_lock_active ).
      RETURN.

    CATCH cx_shm_version_limit_exceeded INTO DATA(lr_shm_version_limit_exceeded).
      er_log->import_msg_from_cx_msg( ir_exception = lr_shm_version_limit_exceeded ).
      RETURN.

    CATCH cx_shm_change_lock_active INTO DATA(lr_shm_change_lock_active).
      er_log->import_msg_from_cx_msg( ir_exception = lr_shm_change_lock_active ).
      RETURN.
    CATCH cx_shm_parameter_error INTO DATA(lr_shm_parameter_error).
      er_log->import_msg_from_cx_msg( ir_exception = lr_shm_parameter_error ).
      RETURN.

    CATCH cx_shm_pending_lock_removed INTO DATA(lr_shm_pending_lock_removed).
      er_log->import_msg_from_cx_msg( ir_exception = lr_shm_pending_lock_removed ).
      RETURN.

  ENDTRY.

ENDMETHOD.
ENDCLASS.
