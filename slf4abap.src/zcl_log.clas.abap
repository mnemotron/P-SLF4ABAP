"!
"! Author: Franz Christoph
"! Creation Date: 2018
"! Version: 1.0.0
"! Since: 1.0.0
"!
class ZCL_LOG definition
  public
  final
  create private .

public section.
  type-pools ABAP .

  interfaces ZIF_LOG .

  aliases TT_LOG
    for ZIF_LOG~TT_LOG .

  constants GC_TYPE_ERROR type CHAR01 value 'E' ##NO_TEXT.
  constants GC_TYPE_DEBUG type CHAR01 value 'D' ##NO_TEXT.
  constants GC_TYPE_INFO type CHAR01 value 'I' ##NO_TEXT.
  constants GC_TYPE_USER type CHAR01 value 'U' ##NO_TEXT.
  constants GC_TYPE_WARNING type CHAR01 value 'W' ##NO_TEXT.
  constants GC_TYPE_FATAL type CHAR01 value 'F' ##NO_TEXT.
  constants GC_TRACE_DEBUG type CHAR01 value 'D' ##NO_TEXT.
  constants GC_TRACE_FATAL type CHAR01 value 'F' ##NO_TEXT.
  constants GC_TRACE_ERROR type CHAR01 value 'E' ##NO_TEXT.
  constants GC_TRACE_WARNING type CHAR01 value 'W' ##NO_TEXT.
  constants GC_TRACE_INFO type CHAR01 value 'I' ##NO_TEXT.
  constants GC_TRACE_USER type CHAR01 value 'U' ##NO_TEXT.
  constants GC_TRACE_OFF type CHAR01 value 'O' ##NO_TEXT.
  constants GC_SYMSG_TYPE_ERROR type SYST_MSGTY value 'E' ##NO_TEXT.
  constants GC_SYMSG_TYPE_WARNING type SYST_MSGTY value 'W' ##NO_TEXT.
  constants GC_SYMSG_TYPE_ABORT type SYST_MSGTY value 'A' ##NO_TEXT.
  constants GC_SYMSG_TYPE_EXIT type SYST_MSGTY value 'X' ##NO_TEXT.
  constants GC_SYMSG_TYPE_INFO type SYST_MSGTY value 'I' ##NO_TEXT.
  constants GC_SYMSG_TYPE_SUCCESS type SYST_MSGTY value 'S' ##NO_TEXT.
  data MT_LOG type TT_LOG read-only .

    "! FACTORY GET INSTANCE
    "!
    "! Level   Trace Level
    "! 6.      DEBUG
    "! 5.      FATAL
    "! 4.      ERROR
    "! 3.      WARNING
    "! 2.      INFO
    "! 1.      USER
    "!
    "! @parameter iv_trace_callstack  | Trace Callstack, default false
    "! @parameter iv_trace_level      | Trace Level, default trace level: FATAL
    "!
    "! @parameter rr_instance         | Returns a log instance
    "!
  class-methods GET_INSTANCE
    importing
      !IV_TRACE_CALLSTACK type ABAP_BOOL default ABAP_FALSE
      !IV_READ_CONFIG type ABAP_BOOL default ABAP_FALSE
      !IV_TRACE_LEVEL type CHAR01 default GC_TRACE_FATAL
      !IV_IMMEDIATE_FLUSH type ABAP_BOOL default ABAP_FALSE
      !IV_LOG_OBJECT type BAL_S_LOG-OBJECT optional
      !IV_LOG_SUBOBJECT type BAL_S_LOG-SUBOBJECT optional
    returning
      value(RR_INSTANCE) type ref to ZCL_LOG .
  methods EXPORT_MSG_TO_BAPIRET_TABLE
    exporting
      !ET_BAPIRET type BAPIRET2_T .
  methods DEBUG
    importing
      !IV_ID type SYMSGID
      !IV_NUMBER type SYMSGNO
      !IV_MESSAGE_V1 type SYMSGV optional
      !IV_MESSAGE_V2 type SYMSGV optional
      !IV_MESSAGE_V3 type SYMSGV optional
      !IV_MESSAGE_V4 type SYMSGV optional
      !IV_DISABLE_IMMEDIATE_FLUSH type ABAP_BOOL default ABAP_FALSE
    returning
      value(ER_LOGM) type ref to ZCL_LOG_MSG .
  methods FATAL
    importing
      !IV_ID type SYMSGID
      !IV_NUMBER type SYMSGNO
      !IV_MESSAGE_V1 type SYMSGV optional
      !IV_MESSAGE_V2 type SYMSGV optional
      !IV_MESSAGE_V3 type SYMSGV optional
      !IV_MESSAGE_V4 type SYMSGV optional
      !IV_DISABLE_IMMEDIATE_FLUSH type ABAP_BOOL default ABAP_FALSE
    returning
      value(ER_LOGM) type ref to ZCL_LOG_MSG .
  methods ERROR
    importing
      !IV_ID type SYMSGID
      !IV_NUMBER type SYMSGNO
      !IV_MESSAGE_V1 type SYMSGV optional
      !IV_MESSAGE_V2 type SYMSGV optional
      !IV_MESSAGE_V3 type SYMSGV optional
      !IV_MESSAGE_V4 type SYMSGV optional
      !IV_DISABLE_IMMEDIATE_FLUSH type ABAP_BOOL default ABAP_FALSE
    returning
      value(ER_LOGM) type ref to ZCL_LOG_MSG .
  methods WARNING
    importing
      !IV_ID type SYMSGID
      !IV_NUMBER type SYMSGNO
      !IV_MESSAGE_V1 type SYMSGV optional
      !IV_MESSAGE_V2 type SYMSGV optional
      !IV_MESSAGE_V3 type SYMSGV optional
      !IV_MESSAGE_V4 type SYMSGV optional
      !IV_DISABLE_IMMEDIATE_FLUSH type ABAP_BOOL default ABAP_FALSE
    returning
      value(ER_LOGM) type ref to ZCL_LOG_MSG .
  methods INFO
    importing
      !IV_ID type SYMSGID
      !IV_NUMBER type SYMSGNO
      !IV_MESSAGE_V1 type SYMSGV optional
      !IV_MESSAGE_V2 type SYMSGV optional
      !IV_MESSAGE_V3 type SYMSGV optional
      !IV_MESSAGE_V4 type SYMSGV optional
      !IV_DISABLE_IMMEDIATE_FLUSH type ABAP_BOOL default ABAP_FALSE
    returning
      value(ER_LOGM) type ref to ZCL_LOG_MSG .
  methods USER
    importing
      !IV_ID type SYMSGID
      !IV_NUMBER type SYMSGNO
      !IV_MESSAGE_V1 type SYMSGV optional
      !IV_MESSAGE_V2 type SYMSGV optional
      !IV_MESSAGE_V3 type SYMSGV optional
      !IV_MESSAGE_V4 type SYMSGV optional
      !IV_DISABLE_IMMEDIATE_FLUSH type ABAP_BOOL default ABAP_FALSE
    returning
      value(ER_LOGM) type ref to ZCL_LOG_MSG .
  methods IMPORT_MSG_FROM_BAPIRET_TABLE
    importing
      !IT_BAPIRET type BAPIRET2_T .
  methods IMPORT_MSG_FROM_BAPIRET
    importing
      !IS_BAPIRET type BAPIRET2
    returning
      value(ER_LOGM) type ref to ZCL_LOG_MSG .
  methods IMPORT_MSG_FROM_SYMSG
    returning
      value(ER_LOGM) type ref to ZCL_LOG_MSG .
  methods IMPORT_MSG_FROM_LOGGER
    importing
      !IR_INSTANCE type ref to ZCL_LOG .
  methods IMPORT_MSG_FROM_CX_MSG
    importing
      !IR_EXCEPTION type ref to IF_MESSAGE
    returning
      value(ER_LOGM) type ref to ZCL_LOG_MSG .
  methods IMPORT_MSG_FROM_CX_MSG_T100
    importing
      !IR_EXCEPTION type ref to IF_T100_MESSAGE
    returning
      value(ER_LOGM) type ref to ZCL_LOG_MSG .
  methods IS_TRACE_LEVEL_DEBUG
    returning
      value(RV_RESULT) type ABAP_BOOL .
  methods IS_TRACE_LEVEL_ERROR
    returning
      value(RV_RESULT) type ABAP_BOOL .
  methods IS_TRACE_LEVEL_FATAL
    returning
      value(RV_RESULT) type ABAP_BOOL .
  methods IS_TRACE_LEVEL_WARNING
    returning
      value(RV_RESULT) type ABAP_BOOL .
  methods IS_TRACE_LEVEL_INFO
    returning
      value(RV_RESULT) type ABAP_BOOL .
  methods IS_TRACE_LEVEL_USER
    returning
      value(RV_RESULT) type ABAP_BOOL .
  methods IS_TRACE_LEVEL_OFF
    returning
      value(RV_RESULT) type ABAP_BOOL .
  methods IS_TRACE_CALLSTACK
    returning
      value(RV_RESULT) type ABAP_BOOL .
  methods IS_ERROR
    returning
      value(RV_RESULT) type ABAP_BOOL .
  methods IS_WARNING
    returning
      value(RV_RESULT) type ABAP_BOOL .
  methods IS_DEBUG
    returning
      value(RV_RESULT) type ABAP_BOOL .
  methods IS_USER
    returning
      value(RV_RESULT) type ABAP_BOOL .
  methods IS_EMPTY
    returning
      value(RV_RESULT) type ABAP_BOOL .
  methods SAVE
    exporting
      !ER_LOG type ref to ZCL_LOG .
  methods IMMEDIATE_FLUSH
    exporting
      !ER_LOG type ref to ZCL_LOG .
  methods GET_MSG_BY_TRACE_LEVEL
    exporting
      !ET_LOG type ZIF_LOG~TT_LOG .
  methods SET_TRACE_LEVEL
    importing
      !IV_TRACE_LEVEL type CHAR01 .
  methods ADD_TAG
    importing
      !IV_TAG type STRING .
  methods ADD_LOG_APPENDER
    importing
      !IR_LA type ref to ZCL_LOG_LA
    returning
      value(RR_LOG) type ref to ZCL_LOG .
  methods IS_IMMEDIATE_FLUSH
    returning
      value(RV_RESULT) type ABAP_BOOL .
protected section.
"!
"! Author: Franz Christoph
"! Creation Date: 2018
"! Version: 1.0.0
"! Since: 1.0.0
"!
private section.
"!
"! Author: Franz Christoph
"! Creation Date: 2018
"! Version: 1.0.0
"! Since: 1.0.0
"!

  aliases TT_LOG_LA
    for ZIF_LOG~TT_LOG_LA .
  aliases TT_LOG_TAG
    for ZIF_LOG~TT_LOG_TAG .

  types:
    BEGIN OF ts_config,
      object          TYPE balobj_d,
      subobject       TYPE balsubobj,
      trace_level     TYPE char01,
      trace_callstack TYPE abap_bool,
      immediate_flush TYPE abap_bool,
    END OF ts_config .

  data MT_LOG_TAG type TT_LOG_TAG .
  data MT_LOG_LA type TT_LOG_LA .
  data MS_CONFIG type TS_CONFIG .

  class-methods GET_CONFIG
    importing
      !IV_OBJECT type BALOBJ_D
      !IV_SUBOBJECT type BALSUBOBJ
    changing
      value(CS_CONFIG) type TS_CONFIG .
  "! CONSTRUCTOR
  "!
  "! @parameter iv_trace_level      | Trace Level
  "! @parameter iv_trace_callstack  | Trace Callstack
  "!
  methods CONSTRUCTOR
    importing
      !IS_CONFIG type TS_CONFIG .
  methods ADD_MSG
    importing
      !IV_TYPE type CHAR01
      !IV_ID type SYMSGID
      !IV_NUMBER type SYMSGNO
      !IV_MESSAGE_V1 type SYMSGV optional
      !IV_MESSAGE_V2 type SYMSGV optional
      !IV_MESSAGE_V3 type SYMSGV optional
      !IV_MESSAGE_V4 type SYMSGV optional
      !IV_DISABLE_IMMEDIATE_FLUSH type ABAP_BOOL optional
    returning
      value(ER_LOGM) type ref to ZCL_LOG_MSG .
  methods GET_CALLSTACK
    exporting
      !ET_ABAP_CALLSTACK type ABAP_CALLSTACK .
  methods ADD_CALLSTACK_DATA
    changing
      !CS_LOG type ZIF_LOG~TS_LOG .
  methods GET_FROM_SYMSG
    importing
      !IV_LOGSYS type LOGSYS optional
      !IV_LANGU type SYLANGU default SY-LANGU
      !IV_MSGID type SY-MSGID default SY-MSGID
      !IV_MSGTY type SY-MSGTY default SY-MSGTY
      !IV_MSGNO type SY-MSGNO default SY-MSGNO
      !IV_MSGV1 type ANY default SY-MSGV1
      !IV_MSGV2 type ANY default SY-MSGV2
      !IV_MSGV3 type ANY default SY-MSGV3
      !IV_MSGV4 type ANY default SY-MSGV4
      !IV_MSG type BAPI_MSG optional
    exporting
      !ES_BAPIRET type BAPIRET2 .
  methods GET_NEXT_INDEX
    exporting
      !EV_INDEX type INT4 .
  methods ADD_TAG_TO_MSG_ALL .
  methods REFRESH_LOG .
  methods MAP_MTYPE_TO_SAPMTYPE
    importing
      !IV_TYPE type CHAR01
    exporting
      !EV_TYPE_SAP type SYMSGTY .
ENDCLASS.



CLASS ZCL_LOG IMPLEMENTATION.


METHOD ADD_CALLSTACK_DATA.

  DATA lt_abap_callstack TYPE abap_callstack.

*-- get abap callstack
  me->get_callstack(
    IMPORTING
      et_abap_callstack = lt_abap_callstack
         ).

*-- map stack peak to log
  READ TABLE lt_abap_callstack
    INDEX 5 "Caller Position(5) -> ERROR(4) -> ADD_MESSAGE(3) -> ADD_CALLSTACK_DATA(2) -> GET_CALLSTACK(1)
    REFERENCE INTO DATA(lr_abap_callstack).

  IF sy-subrc = 0.
    MOVE-CORRESPONDING lr_abap_callstack->* TO cs_log.
  ENDIF.

ENDMETHOD.


METHOD add_log_appender.

  DATA lr_log TYPE REF TO zcl_log.

*-- init
  rr_log = zcl_log=>get_instance( ).

  IF ir_la IS NOT BOUND.
    RETURN.
  ENDIF.

*-- add log appender
  IF NOT line_exists( mt_log_la[ table_line = ir_la ] ).

*- add
    INSERT ir_la INTO TABLE mt_log_la.

*- call init
    CLEAR lr_log.

    ir_la->init(
      EXPORTING
        iv_immediate_flush = ms_config-immediate_flush
      IMPORTING
        er_log             = lr_log
    ).

    rr_log->import_msg_from_logger( ir_instance = lr_log ).

    IF lr_log->is_error( ).
      RETURN.
    ENDIF.

  ENDIF.

ENDMETHOD.


METHOD add_msg.

  DATA ls_log TYPE zif_log~ts_log.
  DATA lv_guid TYPE sysuuid_c32.

*-- init
  CLEAR er_logm.

*-- get next index
  me->get_next_index(
    IMPORTING
      ev_index = DATA(lv_index)
         ).

  lv_guid = lv_index.

*-- map type to sap type
  me->map_mtype_to_sapmtype(
    EXPORTING
      iv_type     = iv_type
    IMPORTING
      ev_type_sap = DATA(lv_type_sap)
         ).

*-- add message
  ls_log-guid = lv_guid.
  ls_log-uname = sy-uname.
  ls_log-date = sy-datum.
  ls_log-time = sy-uzeit.
  ls_log-type = iv_type.
  ls_log-type_sap = lv_type_sap.
  ls_log-id = iv_id.
  ls_log-number = iv_number.
  ls_log-message_v1 = iv_message_v1.
  ls_log-message_v2 = iv_message_v2.
  ls_log-message_v3 = iv_message_v3.
  ls_log-message_v4 = iv_message_v4.

*-- add callstack data
  IF me->is_trace_callstack( ).
    me->add_callstack_data( CHANGING cs_log = ls_log ).
  ENDIF.

*-- create log message instance
  er_logm = zcl_log_msg=>get_instance( is_log = ls_log ).

  INSERT er_logm INTO TABLE mt_log.

*-- immediate flush?
  IF iv_disable_immediate_flush = abap_false AND me->is_immediate_flush( ).
    me->save(
*      IMPORTING
*        er_log =
           ).
  ENDIF.

ENDMETHOD.


METHOD add_tag.

  IF iv_tag IS INITIAL OR line_exists( mt_log_tag[ table_line = iv_tag ] ).
    RETURN.
  ENDIF.

  INSERT iv_tag INTO TABLE mt_log_tag.

ENDMETHOD.


METHOD add_tag_to_msg_all.

  LOOP AT mt_log INTO DATA(lr_log).
    LOOP AT mt_log_tag REFERENCE INTO DATA(lr_tag).
      lr_log->add_tag( iv_tag = lr_tag->* ).
    ENDLOOP.
  ENDLOOP.

ENDMETHOD.


METHOD constructor.

*-- init
  ms_config = is_config.

  CLEAR mt_log_la.
  CLEAR mt_log_tag.
  CLEAR mt_log.

ENDMETHOD.


METHOD debug.

*-- init
  CLEAR er_logm.

*-- add message
  er_logm = me->add_msg(
      iv_type       = me->gc_type_debug
      iv_id         = iv_id
      iv_number     = iv_number
      iv_message_v1 = iv_message_v1
      iv_message_v2 = iv_message_v2
      iv_message_v3 = iv_message_v3
      iv_message_v4 = iv_message_v4
      iv_disable_immediate_flush = iv_disable_immediate_flush
         ).

ENDMETHOD.


METHOD error.

*-- init
  CLEAR er_logm.

*-- add message
  er_logm = me->add_msg(
      iv_type       = me->gc_type_error
      iv_id         = iv_id
      iv_number     = iv_number
      iv_message_v1 = iv_message_v1
      iv_message_v2 = iv_message_v2
      iv_message_v3 = iv_message_v3
      iv_message_v4 = iv_message_v4
      iv_disable_immediate_flush = iv_disable_immediate_flush
         ).

ENDMETHOD.


METHOD export_msg_to_bapiret_table.

*-- init
  CLEAR et_bapiret.

*-- export messages to bapiret table
  LOOP AT mt_log INTO DATA(lr_log).

*- add message
    APPEND VALUE #( type = lr_log->ms_log-type_sap
                    id = lr_log->ms_log-id
                    number = lr_log->ms_log-number
                    message_v1 = lr_log->ms_log-message_v1
                    message_v2 = lr_log->ms_log-message_v2
                    message_v3 = lr_log->ms_log-message_v3
                    message_v4 = lr_log->ms_log-message_v4
                   ) TO et_bapiret.

  ENDLOOP.

ENDMETHOD.


METHOD fatal.

*-- init
  CLEAR er_logm.

*-- add message
  er_logm = me->add_msg(
      iv_type       = me->gc_type_fatal
      iv_id         = iv_id
      iv_number     = iv_number
      iv_message_v1 = iv_message_v1
      iv_message_v2 = iv_message_v2
      iv_message_v3 = iv_message_v3
      iv_message_v4 = iv_message_v4
      iv_disable_immediate_flush = iv_disable_immediate_flush
         ).

ENDMETHOD.


METHOD GET_CALLSTACK.

*-- init
  CLEAR et_abap_callstack.

*-- get system callstack
  CALL FUNCTION 'SYSTEM_CALLSTACK'
*   EXPORTING
*     MAX_LEVEL          = 0
    IMPORTING
      callstack = et_abap_callstack
*     ET_CALLSTACK       =
    .

ENDMETHOD.


METHOD get_config.

  DATA lt_config TYPE SORTED TABLE OF zta_logc WITH UNIQUE KEY object subobject.
  DATA lr_c TYPE REF TO zta_logc.

  IF iv_object IS INITIAL.
    RETURN.
  ENDIF.

*-- select config
  SELECT *
    FROM zta_logc
    INTO CORRESPONDING FIELDS OF TABLE lt_config
    WHERE object = iv_object.

  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

*-- object | subobject
  READ TABLE lt_config
    WITH TABLE KEY object = iv_object
                   subobject = iv_subobject
    REFERENCE INTO lr_c.

  IF sy-subrc = 0.
    MOVE-CORRESPONDING lr_c->* TO cs_config.
    RETURN.
  ENDIF.

*-- object | *
  READ TABLE lt_config
    WITH TABLE KEY object = iv_object
                   subobject = ''
    REFERENCE INTO lr_c.

  IF sy-subrc = 0.
    MOVE-CORRESPONDING lr_c->* TO cs_config.
    RETURN.
  ENDIF.

ENDMETHOD.


METHOD GET_FROM_SYMSG.

  DATA lv_msg_var1 TYPE balm-msgv1.
  DATA lv_msg_var2 TYPE balm-msgv2.
  DATA lv_msg_var3 TYPE balm-msgv3.
  DATA lv_msg_var4 TYPE balm-msgv4.
  DATA lv_msgno TYPE t100-msgnr.

*-- init
  CLEAR es_bapiret.

*-- map sy-msgxx to return
  MOVE iv_msgty  TO es_bapiret-type.
  MOVE iv_msgid  TO es_bapiret-id.
  MOVE iv_msgno  TO es_bapiret-number.
  MOVE iv_msgv1  TO es_bapiret-message_v1.
  MOVE iv_msgv2  TO es_bapiret-message_v2.
  MOVE iv_msgv3  TO es_bapiret-message_v3.
  MOVE iv_msgv4  TO es_bapiret-message_v4.
  MOVE iv_logsys TO es_bapiret-system.

  IF es_bapiret-system IS INITIAL.
    CALL FUNCTION 'OWN_LOGICAL_SYSTEM_GET'
      IMPORTING
        own_logical_system = es_bapiret-system
      EXCEPTIONS
        OTHERS             = 0.
  ENDIF.

*-- build message
  IF NOT iv_msg IS INITIAL.
    es_bapiret-message = iv_msg.
  ELSE.
    lv_msgno    = iv_msgno.
    lv_msg_var1 = iv_msgv1.
    lv_msg_var2 = iv_msgv2.
    lv_msg_var3 = iv_msgv3.
    lv_msg_var4 = iv_msgv4.

    CALL FUNCTION 'MESSAGE_PREPARE'
      EXPORTING
        language               = iv_langu
        msg_id                 = iv_msgid
        msg_no                 = lv_msgno
        msg_var1               = lv_msg_var1
        msg_var2               = lv_msg_var2
        msg_var3               = lv_msg_var3
        msg_var4               = lv_msg_var4
      IMPORTING
        msg_text               = es_bapiret-message
      EXCEPTIONS
        function_not_completed = 1
        message_not_found      = 2
        error_message          = 3
        OTHERS                 = 4.

    IF sy-subrc <> 0.

      MOVE sy-msgty  TO es_bapiret-type.
      MOVE sy-msgid  TO es_bapiret-id.
      MOVE sy-msgno  TO es_bapiret-number.
      MOVE sy-msgv1  TO es_bapiret-message_v1.
      MOVE sy-msgv2  TO es_bapiret-message_v2.
      MOVE sy-msgv3  TO es_bapiret-message_v3.
      MOVE sy-msgv4  TO es_bapiret-message_v4.

      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
             INTO es_bapiret-message.

    ENDIF.

  ENDIF.

ENDMETHOD.


METHOD get_instance.

  DATA ls_config TYPE ts_config.

*-- init
  CLEAR rr_instance.

*-- init config
  ls_config-object = iv_log_object.
  ls_config-subobject = iv_log_subobject.
  ls_config-immediate_flush = iv_immediate_flush.
  ls_config-trace_level = iv_trace_level.
  ls_config-trace_callstack = iv_trace_callstack.

*-- read config
  IF iv_read_config = abap_true.
    zcl_log=>get_config(
      EXPORTING
        iv_object    = iv_log_subobject
        iv_subobject = iv_log_subobject
      CHANGING
        cs_config    = ls_config
           ).
  ENDIF.

*-- create instance
  rr_instance = NEW zcl_log( is_config = ls_config ).

ENDMETHOD.


METHOD get_msg_by_trace_level.

*-- init
  CLEAR et_log.

  IF is_trace_level_off( ) = abap_true.
    RETURN.
  ENDIF.

  et_log[] = mt_log[].

  CASE ms_config-trace_level.

    WHEN gc_trace_debug.
      RETURN.

    WHEN gc_trace_fatal.
      DELETE et_log WHERE table_line->ms_log-type = gc_type_debug.
      RETURN.

    WHEN gc_trace_error.
      DELETE et_log WHERE table_line->ms_log-type = gc_type_debug.
      DELETE et_log WHERE table_line->ms_log-type = gc_type_fatal.
      RETURN.

    WHEN gc_trace_warning.
      DELETE et_log WHERE table_line->ms_log-type = gc_type_debug.
      DELETE et_log WHERE table_line->ms_log-type = gc_type_fatal.
      DELETE et_log WHERE table_line->ms_log-type = gc_type_error.
      RETURN.

    WHEN gc_trace_info.
      DELETE et_log WHERE table_line->ms_log-type = gc_type_debug.
      DELETE et_log WHERE table_line->ms_log-type = gc_type_fatal.
      DELETE et_log WHERE table_line->ms_log-type = gc_type_error.
      DELETE et_log WHERE table_line->ms_log-type = gc_type_warning.
      RETURN.

    WHEN gc_trace_user.
      DELETE et_log WHERE table_line->ms_log-type <> gc_type_user.
      RETURN.

  ENDCASE.

ENDMETHOD.


METHOD get_next_index.

  DATA lv_index TYPE int4.

*-- init
  CLEAR ev_index.

*-- get last index
  lv_index = lines( mt_log ).

*-- return next index
  ev_index = lv_index + 1.

ENDMETHOD.


METHOD immediate_flush.

  DATA lr_log TYPE REF TO zcl_log.
  DATA lv_immediate_flush_man TYPE abap_bool.

*-- init
  er_log = zcl_log=>get_instance( ).

*-- set immediate flush
  IF NOT me->is_immediate_flush( ).
    lv_immediate_flush_man = abap_true.
    me->ms_config-immediate_flush = abap_true.
  ENDIF.

*-- save
  CLEAR lr_log.

  me->save(
    IMPORTING
      er_log = lr_log
         ).

  er_log->import_msg_from_logger( ir_instance = lr_log ).

*-- remove immediate flush
  IF lv_immediate_flush_man = abap_true.
    me->ms_config-immediate_flush = abap_false.
  ENDIF.

ENDMETHOD.


METHOD import_msg_from_bapiret.

*-- init
  CLEAR er_logm.

*-- map sap message type to log type
  CASE is_bapiret-type.

    WHEN gc_symsg_type_error. "error
      er_logm = me->error(
        EXPORTING
          iv_id         = is_bapiret-id
          iv_number     = is_bapiret-number
          iv_message_v1 = is_bapiret-message_v1
          iv_message_v2 = is_bapiret-message_v2
          iv_message_v3 = is_bapiret-message_v3
          iv_message_v4 = is_bapiret-message_v4
          ).

    WHEN gc_symsg_type_abort OR "abort
         gc_symsg_type_exit.    "exit
      er_logm = me->fatal(
        EXPORTING
          iv_id         = is_bapiret-id
          iv_number     = is_bapiret-number
          iv_message_v1 = is_bapiret-message_v1
          iv_message_v2 = is_bapiret-message_v2
          iv_message_v3 = is_bapiret-message_v3
          iv_message_v4 = is_bapiret-message_v4
          ).

    WHEN gc_symsg_type_warning. "warning
      er_logm = me->warning(
        EXPORTING
          iv_id         = is_bapiret-id
          iv_number     = is_bapiret-number
          iv_message_v1 = is_bapiret-message_v1
          iv_message_v2 = is_bapiret-message_v2
          iv_message_v3 = is_bapiret-message_v3
          iv_message_v4 = is_bapiret-message_v4
          ).

    WHEN gc_symsg_type_info. "info
      er_logm = me->info(
        EXPORTING
          iv_id         = is_bapiret-id
          iv_number     = is_bapiret-number
          iv_message_v1 = is_bapiret-message_v1
          iv_message_v2 = is_bapiret-message_v2
          iv_message_v3 = is_bapiret-message_v3
          iv_message_v4 = is_bapiret-message_v4
          ).

    WHEN gc_symsg_type_success. "success
      er_logm = me->info(
        EXPORTING
          iv_id         = is_bapiret-id
          iv_number     = is_bapiret-number
          iv_message_v1 = is_bapiret-message_v1
          iv_message_v2 = is_bapiret-message_v2
          iv_message_v3 = is_bapiret-message_v3
          iv_message_v4 = is_bapiret-message_v4
          ).

    WHEN OTHERS.
      er_logm = zcl_log_msg=>get_instance( ).

  ENDCASE.

ENDMETHOD.


METHOD IMPORT_MSG_FROM_BAPIRET_TABLE.

*-- map sap message type to log type
  LOOP AT it_bapiret REFERENCE INTO DATA(lr_bapiret).
    me->import_msg_from_bapiret( is_bapiret = lr_bapiret->* ).
  ENDLOOP.

ENDMETHOD.


METHOD import_msg_from_cx_msg.

*-- init
  er_logm = zcl_log_msg=>get_instance( ).

  IF ir_exception IS NOT BOUND.
    RETURN.
  ENDIF.

*-- set sy-msgxx
  cl_message_helper=>set_msg_vars_for_if_msg( ir_exception ).

*-- add messages
  er_logm = me->import_msg_from_symsg( ).

ENDMETHOD.


METHOD import_msg_from_cx_msg_t100.

*-- init
  er_logm = zcl_log_msg=>get_instance( ).

  IF ir_exception IS NOT BOUND.
    RETURN.
  ENDIF.

*-- set sy-msgxx
  cl_message_helper=>set_msg_vars_for_if_t100_msg( ir_exception ).

*-- add messages
  er_logm = me->import_msg_from_symsg( ).

ENDMETHOD.


METHOD import_msg_from_logger.

  IF ir_instance IS NOT BOUND.
    RETURN.
  ENDIF.

  IF ir_instance->is_empty( ).
    RETURN.
  ENDIF.

*-- add general tags to all messages
  ir_instance->add_tag_to_msg_all( ).

*-- add messages
  INSERT LINES OF ir_instance->mt_log INTO TABLE mt_log.

ENDMETHOD.


METHOD import_msg_from_symsg.

*-- init
  CLEAR er_logm.

*-- get from sy-msgxx
  me->get_from_symsg(
*    EXPORTING
*      iv_logsys  =
*      iv_langu   = sy-langu
*      iv_msgid   = sy-msgid
*      iv_msgty   = sy-msgty
*      iv_msgno   = sy-msgno
*      iv_msgv1   = sy-msgv1
*      iv_msgv2   = sy-msgv2
*      iv_msgv3   = sy-msgv3
*      iv_msgv4   = sy-msgv4
*      iv_msg     =
    IMPORTING
      es_bapiret = DATA(ls_bapiret)
         ).

*-- add message to log
  er_logm = me->import_msg_from_bapiret( is_bapiret = ls_bapiret ).

ENDMETHOD.


METHOD info.

*-- init
  CLEAR er_logm.

*-- add message
  er_logm = me->add_msg(
      iv_type       = me->gc_type_info
      iv_id         = iv_id
      iv_number     = iv_number
      iv_message_v1 = iv_message_v1
      iv_message_v2 = iv_message_v2
      iv_message_v3 = iv_message_v3
      iv_message_v4 = iv_message_v4
      iv_disable_immediate_flush = iv_disable_immediate_flush
         ).

ENDMETHOD.


METHOD is_debug.

*-- init
  CLEAR rv_result.

*-- is debug?
  LOOP AT mt_log INTO DATA(lr_log)
    WHERE table_line->ms_log-type = gc_type_debug.
    rv_result = abap_true.
    RETURN.
  ENDLOOP.

ENDMETHOD.


METHOD is_empty.

  IF lines( mt_log ) <= 0.
    rv_result = abap_true.
  ELSE.
    rv_result = abap_false.
  ENDIF.

ENDMETHOD.


METHOD IS_ERROR.

*-- init
  CLEAR rv_result.

*-- is error?
  LOOP AT mt_log INTO DATA(lr_log)
    WHERE table_line->ms_log-type = gc_type_error OR
          table_line->ms_log-type = gc_type_fatal.
    rv_result = abap_true.
    RETURN.
  ENDLOOP.

ENDMETHOD.


METHOD is_immediate_flush.

  rv_result = ms_config-immediate_flush.

ENDMETHOD.


METHOD IS_TRACE_CALLSTACK.

  rv_result = ms_config-trace_callstack.

ENDMETHOD.


METHOD IS_TRACE_LEVEL_DEBUG.

  IF ms_config-trace_level = gc_trace_debug.
    rv_result = abap_true.
  ELSE.
    rv_result = abap_false.
  ENDIF.

ENDMETHOD.


METHOD IS_TRACE_LEVEL_ERROR.

  IF ms_config-trace_level = gc_trace_error.
    rv_result = abap_true.
  ELSE.
    rv_result = abap_false.
  ENDIF.

ENDMETHOD.


METHOD IS_TRACE_LEVEL_FATAL.

  IF ms_config-trace_level = gc_trace_fatal.
    rv_result = abap_true.
  ELSE.
    rv_result = abap_false.
  ENDIF.

ENDMETHOD.


METHOD IS_TRACE_LEVEL_INFO.

  IF ms_config-trace_level = gc_trace_info.
    rv_result = abap_true.
  ELSE.
    rv_result = abap_false.
  ENDIF.

ENDMETHOD.


METHOD IS_TRACE_LEVEL_OFF.

  IF ms_config-trace_level = gc_trace_off.
    rv_result = abap_true.
  ELSE.
    rv_result = abap_false.
  ENDIF.

ENDMETHOD.


METHOD IS_TRACE_LEVEL_USER.

  IF ms_config-trace_level = gc_trace_user.
    rv_result = abap_true.
  ELSE.
    rv_result = abap_false.
  ENDIF.

ENDMETHOD.


METHOD IS_TRACE_LEVEL_WARNING.

  IF ms_config-trace_level = gc_trace_warning.
    rv_result = abap_true.
  ELSE.
    rv_result = abap_false.
  ENDIF.

ENDMETHOD.


METHOD is_user.

*-- init
  CLEAR rv_result.

*-- is user?
  LOOP AT mt_log INTO DATA(lr_log)
    WHERE table_line->ms_log-type = gc_type_user.
    rv_result = abap_true.
    RETURN.
  ENDLOOP.

ENDMETHOD.


METHOD is_warning.

*-- init
  CLEAR rv_result.

*-- is warning?
  LOOP AT mt_log INTO DATA(lr_log)
    WHERE table_line->ms_log-type = gc_type_warning.
    rv_result = abap_true.
    RETURN.
  ENDLOOP.

ENDMETHOD.


METHOD map_mtype_to_sapmtype.

*-- init
  CLEAR ev_type_sap.

*- map type
  CASE iv_type.
    WHEN gc_type_debug.
      ev_type_sap = gc_symsg_type_info.
    WHEN gc_type_fatal.
      ev_type_sap = gc_symsg_type_abort.
    WHEN gc_type_error.
      ev_type_sap = gc_symsg_type_error.
    WHEN gc_type_warning.
      ev_type_sap = gc_symsg_type_warning.
    WHEN gc_type_info.
      ev_type_sap = gc_symsg_type_info.
    WHEN gc_type_user.
      ev_type_sap = gc_symsg_type_info.
    WHEN OTHERS.
      ev_type_sap = gc_symsg_type_success.
  ENDCASE.

ENDMETHOD.


METHOD refresh_log.

  CLEAR mt_log.
  CLEAR mt_log_tag.

ENDMETHOD.


METHOD save.

  DATA lr_log TYPE REF TO zcl_log.
  DATA lv_trace_level TYPE char01.

*-- init
  er_log = zcl_log=>get_instance( ).

*-- add general tags to all messages
  me->add_tag_to_msg_all( ).

*-- save only trace level messages
  me->get_msg_by_trace_level(
    IMPORTING
      et_log = DATA(lt_log)
         ).

  IF lt_log IS INITIAL.
    RETURN.
  ENDIF.

*-- call log appenders
  LOOP AT mt_log_la INTO DATA(lr_la).

    CLEAR lr_log.

    lr_la->save(
      EXPORTING
        iv_log_object    = ms_config-object
        iv_log_subobject = ms_config-subobject
        it_log           = lt_log
      IMPORTING
        er_log           = lr_log
    ).

    er_log->import_msg_from_logger( ir_instance = lr_log ).

  ENDLOOP.

*-- delete flushed log messages
  me->refresh_log( ).

ENDMETHOD.


METHOD SET_TRACE_LEVEL.

  ms_config-trace_level = iv_trace_level.

ENDMETHOD.


METHOD user.

*-- init
  CLEAR er_logm.

*-- add message
  er_logm = me->add_msg(
      iv_type       = me->gc_type_user
      iv_id         = iv_id
      iv_number     = iv_number
      iv_message_v1 = iv_message_v1
      iv_message_v2 = iv_message_v2
      iv_message_v3 = iv_message_v3
      iv_message_v4 = iv_message_v4
      iv_disable_immediate_flush = iv_disable_immediate_flush
         ).

ENDMETHOD.


METHOD warning.

*-- init
  CLEAR er_logm.

*-- add message
  er_logm = me->add_msg(
      iv_type       = me->gc_type_warning
      iv_id         = iv_id
      iv_number     = iv_number
      iv_message_v1 = iv_message_v1
      iv_message_v2 = iv_message_v2
      iv_message_v3 = iv_message_v3
      iv_message_v4 = iv_message_v4
      iv_disable_immediate_flush = iv_disable_immediate_flush
         ).

ENDMETHOD.
ENDCLASS.
