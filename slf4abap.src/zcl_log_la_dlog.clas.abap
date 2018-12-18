class ZCL_LOG_LA_DLOG definition
  public
  inheriting from ZCL_LOG_LA
  create public .

public section.

  methods CONSTRUCTOR .
  methods THREAD_SAVE_FINISHED
    importing
      !P_TASK type CLIKE .

  methods GET_LOG_NUMBER
    redefinition .
  methods SAVE
    redefinition .
protected section.
private section.

  aliases GC_LOG_MSG_CLASS_ID
    for ZIF_LOG~GC_LOG_MSG_CLASS_ID .

  types:
    BEGIN OF ts_index,
      index TYPE int4,
      guid  TYPE sysuuid_c32,
    END OF ts_index .
  types:
    tt_index TYPE SORTED TABLE OF ts_index WITH UNIQUE KEY index .

  data MT_INDEX type TT_INDEX .
  data MV_LOG_NUMBER type STRING .
  data MV_THREAD_FINISHED type ABAP_BOOL .

  methods SAVE_LOG
    importing
      !IV_LOG_OBJECT type BAL_S_LOG-OBJECT
      !IV_LOG_SUBOBJECT type BAL_S_LOG-SUBOBJECT
      !IT_LOG type TT_LOG
      !IV_LOG_NUMBER type STRING
    exporting
      !ET_LOG type TT_LOG
      !ER_LOG type ref to ZCL_LOG .
  methods CREATE_GUID
    exporting
      !EV_GUID type SYSUUID_C32
      !ER_LOG type ref to ZCL_LOG .
  methods CREATE_LOG_NUMBER
    exporting
      !EV_NUMBER type STRING .
  methods SAVE_TYPE
    exporting
      !ER_LOG type ref to ZCL_LOG
    changing
      !CT_LOG type TT_LOG .
  methods SAVE_TAG
    importing
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
  methods RUN_SAVE_IMMEDIATE
    importing
      !IV_LOG_OBJECT type BAL_S_LOG-OBJECT
      !IV_LOG_SUBOBJECT type BAL_S_LOG-SUBOBJECT
      !IT_LOG type TT_LOG
    exporting
      !ER_LOG type ref to ZCL_LOG .
  methods SET_THREAD_SAVE_FINISHED
    importing
      !IV_FINISHED type ABAP_BOOL .
ENDCLASS.



CLASS ZCL_LOG_LA_DLOG IMPLEMENTATION.


METHOD constructor.

*-- call super constructor
  super->constructor( ).

*-- init
  mv_thread_finished = abap_false.

ENDMETHOD.


METHOD CREATE_GUID.

*-- init
  CLEAR ev_guid.

  er_log = zcl_log=>get_instance( ).

  TRY.
      ev_guid = cl_system_uuid=>create_uuid_c32_static( ).

    CATCH cx_uuid_error INTO DATA(lr_cx_uiid_error).
      er_log->import_msg_from_cx_msg( ir_exception = lr_cx_uiid_error ).
      RETURN.
  ENDTRY.

ENDMETHOD.


METHOD CREATE_LOG_NUMBER.

  DATA lv_timestamp TYPE timestamp.
  DATA lv_random_int TYPE i.

*-- init
  CLEAR ev_number.

*-- get time stamp
  GET TIME STAMP FIELD lv_timestamp.

*-- random number
  CALL FUNCTION 'GENERAL_GET_RANDOM_INT'
    EXPORTING
      range  = 100
    IMPORTING
      random = lv_random_int.


*-- return log number
  ev_number = |{ lv_timestamp }{ lv_random_int }|.

ENDMETHOD.


METHOD do_save.

  DATA lr_log TYPE REF TO zcl_log.

*-- init
  er_log = zcl_log=>get_instance( ).

  IF it_log IS INITIAL.
    RETURN.
  ENDIF.

*-- create log number
  me->create_log_number(
    IMPORTING
      ev_number = mv_log_number
         ).

*-- save data: log
  CLEAR lr_log.

  me->save_log(
    EXPORTING
      iv_log_object = iv_log_object
      iv_log_subobject = iv_log_subobject
      it_log = it_log
      iv_log_number = mv_log_number
    IMPORTING
      et_log = DATA(lt_log_with_guid)
      er_log = lr_log
         ).

  er_log->import_msg_from_logger( ir_instance = lr_log ).

  IF lr_log->is_error( ).
    RETURN.
  ENDIF.

*-- save data: type
  CLEAR lr_log.

  me->save_type(
    IMPORTING
      er_log = lr_log
    CHANGING
      ct_log = lt_log_with_guid
      ).

  er_log->import_msg_from_logger( ir_instance = lr_log ).

  IF lr_log->is_error( ).
    RETURN.
  ENDIF.

*-- save data: tags
  CLEAR lr_log.

  me->save_tag(
    EXPORTING
      it_log = lt_log_with_guid
    IMPORTING
      er_log = lr_log
         ).

  er_log->import_msg_from_logger( ir_instance = lr_log ).

  IF lr_log->is_error( ).
    RETURN.
  ENDIF.

ENDMETHOD.


METHOD get_log_number.

*-- init
  CLEAR rv_log_number.

  rv_log_number = mv_log_number.

ENDMETHOD.


METHOD run_save.

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

  DATA lt_log_save TYPE ztt_log_la_dlog.
  DATA ls_log_save TYPE zst_log_la_dlog.

  DATA lt_log_data_save TYPE ztt_log_data.
  DATA lt_log_data_v_save TYPE ztt_log_data_v.
  DATA lt_log_data_s_save TYPE ztt_log_data_s.
  DATA lt_log_data_si_save TYPE ztt_log_data_si.
  DATA lt_log_data_t_save TYPE ztt_log_data_t.
  DATA lt_log_tag_save TYPE ztt_log_tag.

*-- init
  er_log = zcl_log=>get_instance( ).

*-- map
  LOOP AT it_log INTO DATA(lr_l).
    CLEAR ls_log_save.
    ls_log_save-slog = lr_l->ms_log.
    ls_log_save-tlog_data[] = lr_l->mt_log_data[].
    ls_log_save-tlog_data_v[] = lr_l->mt_log_data_v[].
    ls_log_save-tlog_data_s[] = lr_l->mt_log_data_s[].
    ls_log_save-tlog_data_si[] = lr_l->mt_log_data_si[].
*   ls_log_save-tlog_data_t[] = lr_l->mt_log_data_t[].
    ls_log_save-tlog_tag[] = lr_l->mt_log_tag[].
    APPEND ls_log_save TO lt_log_save.
  ENDLOOP.

*-- call function module
  me->set_thread_save_finished( iv_finished = abap_false ).

  CALL FUNCTION 'ZFM_LOG_LA_DLOG'
    STARTING NEW TASK 'TDLOG'
    DESTINATION 'NONE'
    CALLING me->thread_save_finished ON END OF TASK
    EXPORTING
      iv_log_object         = iv_log_object
      iv_log_subobject      = iv_log_subobject
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


METHOD save_log.

  DATA lt_log TYPE SORTED TABLE OF zta_log WITH UNIQUE KEY guid.
  DATA lr_log TYPE REF TO zcl_log.

*-- init
  CLEAR et_log.
  er_log = zcl_log=>get_instance( ).

  et_log[] = it_log[].

  LOOP AT et_log INTO DATA(lr_l).

*- create GUID
    CLEAR lr_log.

    me->create_guid(
      IMPORTING
         ev_guid = DATA(lv_guid)
         er_log  = lr_log
           ).

    er_log->import_msg_from_logger( ir_instance = lr_log ).

    IF er_log->is_error( ).
      RETURN.
    ENDIF.

*- update GUID
    lr_l->set_guid( iv_guid = lv_guid ).

*- map and collect
    INSERT VALUE #( mandt = sy-mandt
                    guid = lr_l->ms_log-guid
                    log_object = iv_log_object
                    log_subobject = iv_log_subobject
                    log_date = lr_l->ms_log-date
                    log_time = lr_l->ms_log-time
                    log_uname = lr_l->ms_log-uname
                    log_number = iv_log_number
                    msg_type = lr_l->ms_log-type
                    msg_id = lr_l->ms_log-id
                    msg_number = lr_l->ms_log-number
                    msg_v1 = lr_l->ms_log-message_v1
                    msg_v2 = lr_l->ms_log-message_v2
                    msg_v3 = lr_l->ms_log-message_v3
                    msg_v4 = lr_l->ms_log-message_v4
                  ) INTO TABLE lt_log.

    IF sy-subrc <> 0.
      "Log has not been saved.
      er_log->error( iv_id = gc_log_msg_class_id iv_number = '000').
      RETURN.
    ENDIF.

  ENDLOOP.

*-- save to DB
  IF lt_log IS NOT INITIAL.
    INSERT zta_log FROM TABLE lt_log.

    IF sy-subrc <> 0.
      "Log has not been saved.
      er_log->error( iv_id = gc_log_msg_class_id iv_number = '000').
      RETURN.
    ENDIF.
  ENDIF.

ENDMETHOD.


METHOD SAVE_TAG.

  DATA lt_logt TYPE STANDARD TABLE OF zta_logt WITH DEFAULT KEY.
  DATA lr_log TYPE REF TO zcl_log.

*-- init
  er_log = zcl_log=>get_instance( ).

*-- save tags
  LOOP AT it_log INTO DATA(lr_l).

    LOOP AT lr_l->mt_log_tag REFERENCE INTO DATA(lr_t).

*- create GUID
      CLEAR lr_log.

      me->create_guid(
        IMPORTING
           ev_guid = DATA(lv_guid)
           er_log  = lr_log
             ).

      er_log->import_msg_from_logger( ir_instance = lr_log ).

      IF er_log->is_error( ).
        RETURN.
      ENDIF.

      INSERT VALUE #( mandt = sy-mandt
                      guid = lv_guid
                      guidl = lr_l->ms_log-guid
                      tag = lr_t->*
                     ) INTO TABLE lt_logt.

      IF sy-subrc <> 0.
        "Log data tags has not been saved.
        er_log->error( iv_id = gc_log_msg_class_id iv_number = '006').
        RETURN.
      ENDIF.

    ENDLOOP.

  ENDLOOP.

*-- save to DB
  IF lt_logt IS NOT INITIAL.
    INSERT zta_logt FROM TABLE lt_logt.

    IF sy-subrc <> 0.
      "Log data tags has not been saved.
      er_log->error( iv_id = gc_log_msg_class_id iv_number = '006').
      RETURN.
    ENDIF.
  ENDIF.

ENDMETHOD.


METHOD SAVE_TYPE.

  DATA lr_log TYPE REF TO zcl_log.
  DATA lv_guid TYPE sysuuid_c32.
  DATA lt_logd TYPE SORTED TABLE OF zta_logd WITH UNIQUE KEY guidl guidx.
  DATA lt_logd_v TYPE SORTED TABLE OF zta_logv WITH UNIQUE KEY guid.
  DATA lt_logd_s TYPE SORTED TABLE OF zta_logs WITH UNIQUE KEY guid.
  DATA lt_logd_si TYPE SORTED TABLE OF zta_logsi WITH UNIQUE KEY guids guidx.

*-- init
  er_log = zcl_log=>get_instance( ).

*-- save log data
  LOOP AT ct_log INTO DATA(lr_l).

*- map header variable
    LOOP AT lr_l->mt_log_data_v REFERENCE INTO DATA(lr_ld_v).

*- create GUID
      CLEAR lr_log.
      CLEAR lv_guid.

      me->create_guid(
        IMPORTING
           ev_guid = lv_guid
           er_log  = lr_log
             ).

      er_log->import_msg_from_logger( ir_instance = lr_log ).

      IF er_log->is_error( ).
        RETURN.
      ENDIF.

*- collect index <-> GUID
      INSERT VALUE #( index = lr_ld_v->guid guid = lv_guid ) INTO TABLE mt_index.

      IF sy-subrc <> 0.
        "Save log data: Internal index not set.
        er_log->error( iv_id = gc_log_msg_class_id iv_number = '007').
        RETURN.
      ENDIF.

*- collect DB item

      INSERT VALUE #( mandt = sy-mandt
                      guid = lv_guid
                      name = lr_ld_v->name
                      value = lr_ld_v->value
                    ) INTO TABLE lt_logd_v.

      IF sy-subrc <> 0.
        "Log data variable types has not been saved.
        er_log->error( iv_id = gc_log_msg_class_id iv_number = '002').
        RETURN.
      ENDIF.

    ENDLOOP.

*-- map header structure
    LOOP AT lr_l->mt_log_data_s REFERENCE INTO DATA(lr_ld_s).

*- create GUID
      CLEAR lr_log.
      CLEAR lv_guid.

      me->create_guid(
        IMPORTING
           ev_guid = lv_guid
           er_log  = lr_log
             ).

      er_log->import_msg_from_logger( ir_instance = lr_log ).

      IF er_log->is_error( ).
        RETURN.
      ENDIF.

*- collect index <-> GUID
      INSERT VALUE #( index = lr_ld_s->guid guid = lv_guid ) INTO TABLE mt_index.

      IF sy-subrc <> 0.
        "Save log data: Internal index not set.
        er_log->error( iv_id = gc_log_msg_class_id iv_number = '007').
        RETURN.
      ENDIF.

*- collect DB item
      INSERT VALUE #( mandt = sy-mandt
                      guid = lv_guid
                      name = lr_ld_s->name
                    ) INTO TABLE lt_logd_s.

      IF sy-subrc <> 0.
        "Log data structure item types has not been saved.
        er_log->error( iv_id = gc_log_msg_class_id iv_number = '004').
        RETURN.
      ENDIF.

    ENDLOOP.

*-- map item structure
    LOOP AT lr_l->mt_log_data_si REFERENCE INTO DATA(lr_ld_si).

      READ TABLE mt_index
        WITH TABLE KEY index = lr_ld_si->guids
        REFERENCE INTO DATA(lr_si_guid).

      IF sy-subrc <> 0.
        "Save log data: Internal index not found.
        er_log->error( iv_id = gc_log_msg_class_id iv_number = '005').
        RETURN.
      ENDIF.

      READ TABLE mt_index
        WITH TABLE KEY index = lr_ld_si->guidx
        REFERENCE INTO DATA(lr_si_guidx).

      IF sy-subrc <> 0.
        "Save log data: Internal index not found.
        er_log->error( iv_id = gc_log_msg_class_id iv_number = '005').
        RETURN.
      ENDIF.

*- collect DB item
      INSERT VALUE #( mandt = sy-mandt
                      guids = lr_si_guid->guid
                      guidx = lr_si_guidx->guid
                      type = lr_ld_si->type
                    ) INTO TABLE lt_logd_si.

      IF sy-subrc <> 0.
        "Log data structure item types has not been saved.
        er_log->error( iv_id = gc_log_msg_class_id iv_number = '004').
        RETURN.
      ENDIF.

    ENDLOOP.

*- map message <-> types
    LOOP AT lr_l->mt_log_data REFERENCE INTO DATA(lr_ld).

*- map index
      READ TABLE mt_index
        WITH TABLE KEY index = lr_ld->guid
        REFERENCE INTO DATA(lr_ld_guid).

      IF sy-subrc <> 0.
        "Save log data: Internal index not found.
        er_log->error( iv_id = gc_log_msg_class_id iv_number = '005').
        RETURN.
      ENDIF.

*- collect DB item
      INSERT VALUE #( mandt = sy-mandt
                      guidl = lr_l->ms_log-guid
                      guidx = lr_ld_guid->guid
                      type = lr_ld->type
                    ) INTO TABLE lt_logd.

      IF sy-subrc <> 0.
        "Log data has not been saved.
        er_log->error( iv_id = gc_log_msg_class_id iv_number = '001').
        RETURN.
      ENDIF.

    ENDLOOP.

  ENDLOOP.

*-- save to DB
  IF lt_logd IS NOT INITIAL.
    INSERT zta_logd FROM TABLE lt_logd.

*- save: msg <-> types
    IF sy-subrc <> 0.
      "Log data has not been saved.
      er_log->error( iv_id = gc_log_msg_class_id iv_number = '001').
      RETURN.
    ENDIF.
  ENDIF.

*- save: variable
  IF lt_logd_v IS NOT INITIAL.
    INSERT zta_logv FROM TABLE lt_logd_v.

    IF sy-subrc <> 0.
      "Log data variable types has not been saved.
      er_log->error( iv_id = gc_log_msg_class_id iv_number = '002').
      RETURN.
    ENDIF.
  ENDIF.

*- save: structure header
  IF lt_logd_s IS NOT INITIAL.
    INSERT zta_logs FROM TABLE lt_logd_s.

    IF sy-subrc <> 0.
      "Log data structure types has not been saved.
      er_log->error( iv_id = gc_log_msg_class_id iv_number = '003').
      RETURN.
    ENDIF.
  ENDIF.

*- save: structure item
  IF lt_logd_si IS NOT INITIAL.
    INSERT zta_logsi FROM TABLE lt_logd_si.

    IF sy-subrc <> 0.
      "Log data structure item types has not been saved.
      er_log->error( iv_id = gc_log_msg_class_id iv_number = '004').
      RETURN.
    ENDIF.
  ENDIF.

ENDMETHOD.


METHOD SET_THREAD_SAVE_FINISHED.

  mv_thread_finished = iv_finished.

ENDMETHOD.


METHOD THREAD_SAVE_FINISHED.

  me->set_thread_save_finished( iv_finished = abap_true ).

ENDMETHOD.
ENDCLASS.
