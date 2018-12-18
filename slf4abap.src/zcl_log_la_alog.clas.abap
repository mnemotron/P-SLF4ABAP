class ZCL_LOG_LA_ALOG definition
  public
  inheriting from ZCL_LOG_LA
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IV_LOG_KEY type STRING optional .
  methods THREAD_SAVE_FINISHED
    importing
      !P_TASK type CLIKE .

  methods GET_LOG_NUMBER
    redefinition .
  methods SAVE
    redefinition .
protected section.
private section.

  aliases TT_LOG_TAG
    for ZIF_LOG~TT_LOG_TAG .

  types:
    BEGIN OF ts_log_handle,
      key       TYPE string,
      object    TYPE bal_s_log-object,
      subobject	TYPE bal_s_log-subobject,
      loghandle TYPE balloghndl,
    END OF ts_log_handle .
  types:
    tt_log_handle TYPE SORTED TABLE OF ts_log_handle WITH UNIQUE KEY key object subobject .

  constants GC_PCLASS_VERY_IMPORTANT type BAL_S_MSG-PROBCLASS value '1' ##NO_TEXT.
  constants GC_PCLASS_IMPORTANT type BAL_S_MSG-PROBCLASS value '2' ##NO_TEXT.
  constants GC_PCLASS_MEDIUM type BAL_S_MSG-PROBCLASS value '3' ##NO_TEXT.
  constants GC_PCLASS_INFO type BAL_S_MSG-PROBCLASS value '4' ##NO_TEXT.
  data MV_LOG_KEY type STRING .
  data MT_LOG_HANDLE type TT_LOG_HANDLE .
  data MV_THREAD_FINISHED type ABAP_BOOL .

  methods CREATE
    importing
      !IV_KEY type ANY optional
      !IV_LOG_OBJECT type BAL_S_LOG-OBJECT
      !IV_LOG_SUBOBJECT type BAL_S_LOG-SUBOBJECT
    exporting
      !EV_LOG_HANDLE type BALLOGHNDL
      !ER_LOG type ref to ZCL_LOG .
  methods ADD_MESSAGE
    importing
      !IV_LOG_HANDLE type BALLOGHNDL
      !IS_LOG_MSG type BAL_S_MSG
    exporting
      !ER_LOG type ref to ZCL_LOG .
  methods ADD_MESSAGES
    importing
      !IT_LOG type TT_LOG
      !IV_LOG_HANDLE type BALLOGHNDL
    exporting
      !ER_LOG type ref to ZCL_LOG .
  methods SAVE_ALOG
    importing
      !IV_LOG_HANDLE type BALLOGHNDL
    exporting
      !ER_LOG type ref to ZCL_LOG .
  methods RUN_SAVE_IMMEDIATE
    importing
      !IV_LOG_OBJECT type BAL_S_LOG-OBJECT
      !IV_LOG_SUBOBJECT type BAL_S_LOG-SUBOBJECT
      !IV_LOG_KEY type STRING
      !IT_LOG type TT_LOG
    exporting
      !ER_LOG type ref to ZCL_LOG .
  methods RUN_SAVE
    importing
      !IV_LOG_OBJECT type BAL_S_LOG-OBJECT
      !IV_LOG_SUBOBJECT type BAL_S_LOG-SUBOBJECT
      !IT_LOG type TT_LOG
    exporting
      !ER_LOG type ref to ZCL_LOG .
  methods DO_SAVE
    importing
      !IV_LOG_OBJECT type BAL_S_LOG-OBJECT
      !IV_LOG_SUBOBJECT type BAL_S_LOG-SUBOBJECT
      !IT_LOG type TT_LOG
    exporting
      !ER_LOG type ref to ZCL_LOG .
  methods MAP_LOG_TO_ALOG_MSG
    importing
      !IS_LOG_MSG type TS_LOG
    exporting
      !ES_ALOG_MSG type BAL_S_MSG .
  methods GET_LOG_HANDLE
    importing
      !IV_KEY type STRING optional
      !IV_LOG_OBJECT type BAL_S_LOG-OBJECT
      !IV_LOG_SUBOBJECT type BAL_S_LOG-SUBOBJECT
    exporting
      !EV_LOG_HANDLE type BALLOGHNDL
      !ER_LOG type ref to ZCL_LOG .
  methods SET_THREAD_SAVE_FINISHED
    importing
      !IV_FINISHED type ABAP_BOOL .
  methods MAP_TAGS_TO_CONTEXT
    importing
      !IT_TAG type TT_LOG_TAG
    changing
      !CS_ALOG_MSG type BAL_S_MSG .
ENDCLASS.



CLASS ZCL_LOG_LA_ALOG IMPLEMENTATION.


METHOD ADD_MESSAGE.

*-- init
  er_log = zcl_log=>get_instance( ).

*-- add message
  CALL FUNCTION 'BAL_LOG_MSG_ADD'
    EXPORTING
      i_log_handle     = iv_log_handle
      i_s_msg          = is_log_msg
*   IMPORTING
*     E_S_MSG_HANDLE   =
*     E_MSG_WAS_LOGGED =
*     E_MSG_WAS_DISPLAYED       =
    EXCEPTIONS
      log_not_found    = 1
      msg_inconsistent = 2
      log_is_full      = 3
      error_message    = 4
      OTHERS           = 5.

  IF sy-subrc <> 0.
    er_log->import_msg_from_symsg( ).
    RETURN.
  ENDIF.

ENDMETHOD.


METHOD add_messages.

  DATA ls_log_msg TYPE bal_s_msg.
  DATA lr_log TYPE REF TO zcl_log.

*-- init
  er_log = zcl_log=>get_instance( ).

  IF it_log IS INITIAL.
    RETURN.
  ENDIF.

*-- add messages
  LOOP AT it_log INTO DATA(lr_msg).

    CLEAR ls_log_msg.

*-- map to application log message
    me->map_log_to_alog_msg(
      EXPORTING
        is_log_msg  = lr_msg->ms_log
      IMPORTING
        es_alog_msg = ls_log_msg
           ).

*-- add tags as context
    me->map_tags_to_context(
      EXPORTING
        it_tag      = lr_msg->mt_log_tag
      CHANGING
        cs_alog_msg = ls_log_msg
           ).

*- add message to application log
    CLEAR lr_log.

    me->add_message(
      EXPORTING
        iv_log_handle = iv_log_handle
        is_log_msg    = ls_log_msg
      IMPORTING
        er_log        = lr_log
           ).

    er_log->import_msg_from_logger( ir_instance = lr_log ).

    IF lr_log->is_error( ).
      CONTINUE.
    ENDIF.

  ENDLOOP.

ENDMETHOD.


METHOD constructor.

*-- call super constructor
  super->constructor( ).

*-- init
  mv_log_key = iv_log_key.
  mv_thread_finished = abap_false.

ENDMETHOD.


METHOD CREATE.

  DATA ls_alog_header TYPE bal_s_log.

*-- init
  CLEAR ev_log_handle.
  er_log = zcl_log=>get_instance( ).

*-- create application log
  CLEAR ls_alog_header.
  ls_alog_header-extnumber = iv_key.
  ls_alog_header-object = iv_log_object.
  ls_alog_header-subobject = iv_log_subobject.

  TRANSLATE ls_alog_header-object TO UPPER CASE.          "#EC SYNTCHAR
  TRANSLATE ls_alog_header-subobject TO UPPER CASE.       "#EC SYNTCHAR

  CALL FUNCTION 'BAL_LOG_CREATE'
    EXPORTING
      i_s_log                 = ls_alog_header
    IMPORTING
      e_log_handle            = ev_log_handle
    EXCEPTIONS
      log_header_inconsistent = 1
      OTHERS                  = 2.

  IF sy-subrc <> 0.
    er_log->import_msg_from_symsg( ).
    RETURN.
  ENDIF.

ENDMETHOD.


METHOD do_save.

  DATA lr_log TYPE REF TO zcl_log.

*-- init
  er_log = zcl_log=>get_instance( ).

  IF it_log IS INITIAL.
    RETURN.
  ENDIF.

*-- get log handle
  CLEAR lr_log.

  me->get_log_handle(
    EXPORTING
      iv_key           = mv_log_key
      iv_log_object    = iv_log_object
      iv_log_subobject = iv_log_subobject
    IMPORTING
      ev_log_handle    = DATA(lv_log_handle)
      er_log           = lr_log
         ).

  er_log->import_msg_from_logger( ir_instance = lr_log ).

  IF lr_log->is_error( ).
    RETURN.
  ENDIF.

*-- add messages
  CLEAR lr_log.

  me->add_messages(
    EXPORTING
      it_log        = it_log
      iv_log_handle = lv_log_handle
    IMPORTING
      er_log        = lr_log
         ).

  er_log->import_msg_from_logger( ir_instance = lr_log ).

  IF lr_log->is_error( ).
    RETURN.
  ENDIF.

*-- save application log
  CLEAR lr_log.

  me->save_alog(
    EXPORTING
      iv_log_handle = lv_log_handle
    IMPORTING
      er_log        = lr_log
         ).

  er_log->import_msg_from_logger( ir_instance = lr_log ).

  IF lr_log->is_error( ).
    RETURN.
  ENDIF.

ENDMETHOD.


METHOD get_log_handle.

  DATA lr_log TYPE REF TO zcl_log.

*-- init
  CLEAR ev_log_handle.
  er_log = zcl_log=>get_instance( ).

*-- check log handle buffer
  READ TABLE mt_log_handle
    WITH TABLE KEY key = iv_key
                   object = iv_log_object
                   subobject = iv_log_subobject
    REFERENCE INTO DATA(lr_log_handle).

  IF sy-subrc = 0.
    ev_log_handle = lr_log_handle->loghandle.
  ELSE.

*-- create application log
    CLEAR lr_log.

    me->create(
      EXPORTING
        iv_key           = iv_key
        iv_log_object    = iv_log_object
        iv_log_subobject = iv_log_subobject
      IMPORTING
        ev_log_handle    = ev_log_handle
        er_log           = lr_log
    ).

    er_log->import_msg_from_logger( ir_instance = lr_log ).

    IF lr_log->is_error( ).
      RETURN.
    ENDIF.

*-- add created log handle to buffer
    INSERT VALUE #( key = iv_key
                    object = iv_log_object
                    subobject = iv_log_subobject
                    loghandle = ev_log_handle
                  ) INTO TABLE mt_log_handle.

  ENDIF.

ENDMETHOD.


METHOD get_log_number.

*-- init
  CLEAR rv_log_number.

  READ TABLE mt_log_handle
    INDEX 1
    REFERENCE INTO DATA(lr_lh).

  IF sy-subrc = 0.
    rv_log_number = lr_lh->loghandle.
  ENDIF.

ENDMETHOD.


METHOD MAP_LOG_TO_ALOG_MSG.

  DATA lv_tzone TYPE timezone.

*-- init
  CLEAR es_alog_msg.

*-- map
  es_alog_msg-msgty = is_log_msg-type.
  es_alog_msg-msgno = is_log_msg-number.
  es_alog_msg-msgid = is_log_msg-id.
  es_alog_msg-msgv1 = is_log_msg-message_v1.
  es_alog_msg-msgv2 = is_log_msg-message_v2.
  es_alog_msg-msgv3 = is_log_msg-message_v3.
  es_alog_msg-msgv4 = is_log_msg-message_v4.

  CALL FUNCTION 'GET_SYSTEM_TIMEZONE'
    IMPORTING
      timezone = lv_tzone.

  CONVERT DATE is_log_msg-date TIME is_log_msg-time INTO TIME STAMP es_alog_msg-time_stmp TIME ZONE lv_tzone.

*-- map problem class
  CASE is_log_msg-type.

    WHEN zcl_log=>gc_type_debug.
      es_alog_msg-probclass = gc_pclass_info.

    WHEN zcl_log=>gc_type_fatal.
      es_alog_msg-probclass = gc_pclass_very_important.

    WHEN zcl_log=>gc_type_error.
      es_alog_msg-probclass = gc_pclass_important.

    WHEN zcl_log=>gc_type_warning.
      es_alog_msg-probclass = gc_pclass_medium.

    WHEN zcl_log=>gc_type_info.
      es_alog_msg-probclass = gc_pclass_info.

    WHEN zcl_log=>gc_type_user.
      es_alog_msg-probclass = gc_pclass_info.

  ENDCASE.

ENDMETHOD.


METHOD map_tags_to_context.

  DATA ls_msg_context TYPE bal_s_cont.
  DATA lv_tag_str TYPE string.
  DATA ls_context TYPE zst_context_tag.

  IF it_tag IS INITIAL.
    RETURN.
  ENDIF.

*-- concatenate tags
  LOOP AT it_tag REFERENCE INTO DATA(lr_tag).
    lv_tag_str = |{ lv_tag_str }, { lr_tag->* }|.
  ENDLOOP.

  ls_context-tag = lv_tag_str.

*-- set context
  ls_msg_context-tabname = 'ZST_CONTEXT_TAG'.
  ls_msg_context-value = ls_context.
  cs_alog_msg-context = ls_msg_context.

ENDMETHOD.


METHOD RUN_SAVE.

  DATA lr_log TYPE REF TO zcl_log.

*-- init
  er_log = zcl_log=>get_instance( ).

  IF it_log IS INITIAL.
    RETURN.
  ENDIF.

*-- save log
  me->do_save(
    EXPORTING
      iv_log_object    = iv_log_object
      iv_log_subobject = iv_log_subobject
      it_log           = it_log
    IMPORTING
      er_log           = DATA(lr_log_save)
         ).

  er_log->import_msg_from_logger( ir_instance = lr_log_save ).

*-- commit or rollback
  me->post(
     EXPORTING
       ir_log             = lr_log_save
*      iv_wait            = ABAP_TRUE
*      iv_rfc_destination =
*      iv_close_session   = ABAP_FALSE
     IMPORTING
       er_log             = DATA(lr_log_post)
          ).

  er_log->import_msg_from_logger( ir_instance = lr_log_post ).

ENDMETHOD.


METHOD run_save_immediate.

  DATA lt_log_save TYPE ztt_log.

*-- init
  er_log = zcl_log=>get_instance( ).

*-- map
  LOOP AT it_log INTO DATA(lr_l).
    INSERT lr_l->ms_log INTO TABLE lt_log_save.
  ENDLOOP.

*-- call function module
  me->set_thread_save_finished( iv_finished = abap_false ).

  CALL FUNCTION 'ZFM_LOG_LA_ALOG'
    STARTING NEW TASK 'TALOG'
    DESTINATION 'NONE'
    CALLING me->thread_save_finished ON END OF TASK
    EXPORTING
      iv_log_object         = iv_log_object
      iv_log_subobject      = iv_log_subobject
      iv_log_key            = iv_log_key
      it_log                = lt_log_save
    EXCEPTIONS
      communication_failure = 1
      system_failure        = 2
      resource_failure      = 3
      OTHERS                = 4.

  IF sy-subrc <> 0.
    er_log->import_msg_from_symsg( ).
    RETURN.
  ENDIF.

  WAIT UNTIL mv_thread_finished = abap_true.

ENDMETHOD.


METHOD save.

  DATA lr_log TYPE REF TO zcl_log.

*-- init
  er_log = zcl_log=>get_instance( ).

  IF it_log IS INITIAL.
    RETURN.
  ENDIF.

*-- run save
  CLEAR lr_log.

  IF is_immediate_flush( ).

    CLEAR lr_log.

    me->run_save_immediate(
      EXPORTING
        iv_log_object    = iv_log_object
        iv_log_subobject = iv_log_subobject
        iv_log_key       = mv_log_key
        it_log           = it_log
      IMPORTING
        er_log           = lr_log
           ).

    er_log->import_msg_from_logger( ir_instance = lr_log ).

    IF lr_log->is_error( ).
      RETURN.
    ENDIF.

  ELSE.

    CLEAR lr_log.

    me->run_save(
      EXPORTING
        iv_log_object = iv_log_object
        iv_log_subobject = iv_log_subobject
        it_log = it_log
      IMPORTING
        er_log = lr_log
           ).

    er_log->import_msg_from_logger( ir_instance = lr_log ).

    IF lr_log->is_error( ).
      RETURN.
    ENDIF.

  ENDIF.

ENDMETHOD.


METHOD SAVE_ALOG.

  DATA lt_log_handle TYPE bal_t_logh.

*-- init
  er_log = zcl_log=>get_instance( ).

*-- save application log
  APPEND iv_log_handle TO lt_log_handle.

  CALL FUNCTION 'BAL_DB_SAVE'
    EXPORTING
*     I_CLIENT         = SY-MANDT
*     I_IN_UPDATE_TASK = ' '
*     I_SAVE_ALL       = ' '
      i_t_log_handle   = lt_log_handle
*     I_2TH_CONNECTION = ' '
*     I_2TH_CONNECT_COMMIT       = ' '
*     I_LINK2JOB       = 'X'
*   IMPORTING
*     E_NEW_LOGNUMBERS =
*     E_SECOND_CONNECTION        =
    EXCEPTIONS
      log_not_found    = 1
      save_not_allowed = 2
      numbering_error  = 3
      OTHERS           = 4.

  IF sy-subrc <> 0.
    er_log->import_msg_from_symsg( ).
    RETURN.
  ENDIF.

ENDMETHOD.


METHOD set_thread_save_finished.

  mv_thread_finished = iv_finished.

ENDMETHOD.


METHOD thread_save_finished.

  me->set_thread_save_finished( iv_finished = abap_true ).

ENDMETHOD.
ENDCLASS.
