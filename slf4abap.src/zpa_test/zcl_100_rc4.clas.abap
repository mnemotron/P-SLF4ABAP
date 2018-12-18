class ZCL_100_RC4 definition
  public
  final
  create public .

public section.

  class-methods DO_T001_LA_STATUS_BAR .
  class-methods DO_T002_LA_ALOG .
  class-methods DO_T003_LA_PLOG .
  class-methods DO_T004_LA_PLOG_I .
  class-methods DO_T005_LA_STATUS_BAR_I .
  class-methods DO_T006_LA_ALOG_I .
  class-methods DO_T007_LA_DLOG_I .
protected section.
private section.
ENDCLASS.



CLASS ZCL_100_RC4 IMPLEMENTATION.


METHOD do_t001_la_status_bar.

  DATA lv_msg TYPE bapi_msg.

*-- get log instance
  DATA(lr_log) = zcl_log=>get_instance( ).

*-- add log appender: status bar
  lr_log->add_log_appender( ir_la = NEW zcl_log_la_sbar( ) ).

*-- log message
  MESSAGE w000(zmc_test) WITH 'Thats a log message in the status bar.' INTO lv_msg.
  lr_log->import_msg_from_symsg( ).

*-- save/flush log
  lr_log->save( ).

  WAIT UP TO 1 SECONDS.

ENDMETHOD.


METHOD do_t002_la_alog.

*-- get log instance
  DATA(lr_log) = zcl_log=>get_instance( iv_log_object = 'ZZZIBM' ).

*-- add log appender: application log
  lr_log->add_log_appender( ir_la = NEW zcl_log_la_alog( iv_log_key = 'T002') ).

*-- log message
  lr_log->fatal(
    EXPORTING
      iv_id         = 'ZMC_TEST'
      iv_number     = '000'
      iv_message_v1 = 'Thats a fatal log message for the application log:'
      iv_message_v2 = ' T002'
      ).

  lr_log->user(
  EXPORTING
    iv_id         = 'ZMC_TEST'
    iv_number     = '000'
    iv_message_v1 = 'Thats a user log message for the application log:'
    iv_message_v2 = ' T002'
    ).

  lr_log->info(
  EXPORTING
    iv_id         = 'ZMC_TEST'
    iv_number     = '000'
    iv_message_v1 = 'Thats a info log message for the application log:'
    iv_message_v2 = ' T002'
    ).

  lr_log->debug(
EXPORTING
  iv_id         = 'ZMC_TEST'
  iv_number     = '000'
  iv_message_v1 = 'Thats a debug log message for the application log:'
  iv_message_v2 = ' T002'
  ).

  lr_log->warning(
EXPORTING
  iv_id         = 'ZMC_TEST'
  iv_number     = '000'
  iv_message_v1 = 'Thats a warn log message for the application log:'
  iv_message_v2 = ' T002'
  ).

*-- save/flush log
  lr_log->save( ).

  COMMIT WORK AND WAIT.

ENDMETHOD.


METHOD do_t003_la_plog.

*-- get log instance
  DATA(lr_log) = zcl_log=>get_instance( iv_log_object = 'ZZZIBM' ).

*-- set trace level
  lr_log->set_trace_level( iv_trace_level = zcl_log=>gc_trace_debug ).

*-- add log appender: application log
  lr_log->add_log_appender( ir_la = NEW zcl_log_la_plog( ) ).

*-- log message
  lr_log->fatal(
    EXPORTING
      iv_id         = 'ZMC_TEST'
      iv_number     = '000'
      iv_message_v1 = 'Thats a fatal log message for the application log:'
      iv_message_v2 = ' T002'
      ).

  lr_log->user(
  EXPORTING
    iv_id         = 'ZMC_TEST'
    iv_number     = '000'
    iv_message_v1 = 'Thats a user log message for the application log:'
    iv_message_v2 = ' T002'
    ).

  lr_log->info(
  EXPORTING
    iv_id         = 'ZMC_TEST'
    iv_number     = '000'
    iv_message_v1 = 'Thats a info log message for the application log:'
    iv_message_v2 = ' T002'
    ).

  lr_log->debug(
EXPORTING
  iv_id         = 'ZMC_TEST'
  iv_number     = '000'
  iv_message_v1 = 'Thats a debug log message for the application log:'
  iv_message_v2 = ' T002'
  ).

  lr_log->warning(
EXPORTING
  iv_id         = 'ZMC_TEST'
  iv_number     = '000'
  iv_message_v1 = 'Thats a warn log message for the application log:'
  iv_message_v2 = ' T002'
  ).

  lr_log->error(
EXPORTING
iv_id         = 'ZMC_TEST'
iv_number     = '000'
iv_message_v1 = 'Thats a error log message for the application log:'
iv_message_v2 = ' T002'
).

*-- save/flush log
  lr_log->save( ).

ENDMETHOD.


METHOD DO_T004_LA_PLOG_I.

*-- get log instance
  DATA(lr_log) = zcl_log=>get_instance(
*                   iv_trace_callstack = ABAP_FALSE
                    iv_trace_level     = zcl_log=>gc_trace_debug
                    iv_immediate_flush = abap_true
*                   iv_log_object =
*                   iv_log_subobject   =
               ).

*-- add log appender: application log
  lr_log->add_log_appender( ir_la = NEW zcl_log_la_plog( iv_popup = abap_true ) ).

*-- log message
  lr_log->fatal(
    EXPORTING
      iv_id         = 'ZMC_TEST'
      iv_number     = '000'
      iv_message_v1 = 'Thats a fatal log message for the application log:'
      iv_message_v2 = ' T002'
      ).

  lr_log->user(
  EXPORTING
    iv_id         = 'ZMC_TEST'
    iv_number     = '000'
    iv_message_v1 = 'Thats a user log message for the application log:'
    iv_message_v2 = ' T002'
    ).

  lr_log->info(
  EXPORTING
    iv_id         = 'ZMC_TEST'
    iv_number     = '000'
    iv_message_v1 = 'Thats a info log message for the application log:'
    iv_message_v2 = ' T002'
    ).

  lr_log->debug(
EXPORTING
  iv_id         = 'ZMC_TEST'
  iv_number     = '000'
  iv_message_v1 = 'Thats a debug log message for the application log:'
  iv_message_v2 = ' T002'
  ).

  WAIT UP TO 5 SECONDS.

  lr_log->warning(
EXPORTING
  iv_id         = 'ZMC_TEST'
  iv_number     = '000'
  iv_message_v1 = 'Thats a warn log message for the application log:'
  iv_message_v2 = ' T002'
  ).

  lr_log->error(
  EXPORTING
    iv_id         = 'ZMC_TEST'
    iv_number     = '000'
    iv_message_v1 = 'Thats a error log message for the application log:'
    iv_message_v2 = ' T002'
    ).

  WAIT UP TO 2 SECONDS.

  lr_log->error(
  EXPORTING
    iv_id         = 'ZMC_TEST'
    iv_number     = '000'
    iv_message_v1 = 'Thats a error log message for the application log:'
    iv_message_v2 = ' T002'
    ).

  WAIT UP TO 1 SECONDS.

  lr_log->error(
EXPORTING
  iv_id         = 'ZMC_TEST'
  iv_number     = '000'
  iv_message_v1 = 'Thats a error log message for the application log:'
  iv_message_v2 = ' T002'
  ).
ENDMETHOD.


METHOD DO_T005_LA_STATUS_BAR_I.

  DATA lv_msg TYPE bapi_msg.

*-- get log instance
  DATA(lr_log) = zcl_log=>get_instance(
*                   iv_trace_callstack = ABAP_FALSE
*                   iv_read_config     = ABAP_FALSE
*                   iv_trace_level     = GC_TRACE_FATAL
                    iv_immediate_flush = abap_true
*                   iv_log_object      =
*                   iv_log_subobject   =
               ).

*-- add log appender: status bar
  lr_log->add_log_appender( ir_la = NEW zcl_log_la_sbar( ) ).

*-- log message
  lr_log->fatal(
    EXPORTING
      iv_id         = 'ZMC_TEST'
      iv_number     = '000'
      iv_message_v1 = 'Thats a fatal log message for the application log:'
      iv_message_v2 = ' T005'
      ).

  lr_log->user(
  EXPORTING
    iv_id         = 'ZMC_TEST'
    iv_number     = '000'
    iv_message_v1 = 'Thats a user log message for the application log:'
    iv_message_v2 = ' T005'
    ).

  WAIT UP TO 2 SECONDS.

  lr_log->info(
  EXPORTING
    iv_id         = 'ZMC_TEST'
    iv_number     = '000'
    iv_message_v1 = 'Thats a info log message for the application log:'
    iv_message_v2 = ' T005'
    ).

ENDMETHOD.


METHOD do_t006_la_alog_i.

*-- get log instance
  DATA(lr_log) = zcl_log=>get_instance(
*                   iv_trace_callstack = ABAP_FALSE
*                   iv_read_config     = ABAP_FALSE
                    iv_trace_level     = ZCL_LOG=>gc_trace_debug
                    iv_immediate_flush = abap_true
                    iv_log_object = 'ZZZIBM'
*                   iv_log_subobject   =
               ).

*-- add log appender: application log
  lr_log->add_log_appender( ir_la = NEW zcl_log_la_alog( iv_log_key = 'T006') ).

*-- log message
  lr_log->fatal(
    EXPORTING
      iv_id         = 'ZMC_TEST'
      iv_number     = '000'
      iv_message_v1 = 'Thats a fatal log message for the application log:'
      iv_message_v2 = ' T006'
      ).

  lr_log->user(
  EXPORTING
    iv_id         = 'ZMC_TEST'
    iv_number     = '000'
    iv_message_v1 = 'Thats a user log message for the application log:'
    iv_message_v2 = ' T006'
    ).

  lr_log->info(
  EXPORTING
    iv_id         = 'ZMC_TEST'
    iv_number     = '000'
    iv_message_v1 = 'Thats a info log message for the application log:'
    iv_message_v2 = ' T006'
    ).

  lr_log->debug(
EXPORTING
  iv_id         = 'ZMC_TEST'
  iv_number     = '000'
  iv_message_v1 = 'Thats a debug log message for the application log:'
  iv_message_v2 = ' T006'
  ).

  lr_log->warning(
EXPORTING
  iv_id         = 'ZMC_TEST'
  iv_number     = '000'
  iv_message_v1 = 'Thats a warn log message for the application log:'
  iv_message_v2 = ' T006'
  ).

ENDMETHOD.


METHOD do_t007_la_dlog_i.

*-- get log instance
  DATA(lr_log) = zcl_log=>get_instance(
*                     iv_trace_callstack = ABAP_FALSE
*                     iv_read_config     = ABAP_FALSE
                      iv_trace_level     = zcl_log=>gc_trace_debug
                      iv_immediate_flush = abap_true
                      iv_log_object      = 'ZZZIBM'
*                     iv_log_subobject   =
                 ).

*-- add log appender: enhanced data log
  lr_log->add_log_appender( ir_la = NEW zcl_log_la_dlog( ) ).

*-- log message
  lr_log->fatal(
    EXPORTING
      iv_id         = 'ZMC_TEST'
      iv_number     = '000'
      iv_message_v1 = 'Thats a fatal log message:'
      iv_message_v2 = ' T007'
      ).

  DATA(lr_msg) = lr_log->debug(
      iv_id         = 'ZMC_TEST'
      iv_number     = '000'
      iv_message_v1 = 'Thats a debug log message:'
      iv_message_v2 = 'T007'
*     iv_message_v3              =
*     iv_message_v4              =
      iv_disable_immediate_flush = abap_true
               ).

  lr_msg->add_data_type(
    EXPORTING
      iv_name = 'lr_msg'
      ix_data = lr_msg->ms_log
  ).

*-- flush manually
  lr_log->save(
*    IMPORTING
*      er_log =
  ).

ENDMETHOD.
ENDCLASS.
