FUNCTION zfm_log_la_alog.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_LOG_OBJECT) TYPE  BAL_S_LOG-OBJECT
*"     VALUE(IV_LOG_SUBOBJECT) TYPE  BAL_S_LOG-SUBOBJECT
*"     VALUE(IV_LOG_KEY) TYPE  STRING
*"     VALUE(IT_LOG) TYPE  ZTT_LOG
*"----------------------------------------------------------------------

  DATA lr_log TYPE REF TO zcl_log.
  DATA lt_log_save TYPE zif_log=>tt_log.

*-- init
  DATA(lr_alog) = NEW zcl_log_la_alog( iv_log_key = iv_log_key ).

  CLEAR lr_log.

  lr_alog->init(
    EXPORTING
      iv_immediate_flush = abap_false
    IMPORTING
      er_log             = lr_log
  ).

  IF lr_log->is_error( ).
    RETURN.
  ENDIF.

*-- map log
  LOOP AT it_log REFERENCE INTO DATA(lr_l).
    DATA(lr_logm) = zcl_log_msg=>get_instance( is_log = lr_l->* ).
    INSERT lr_logm INTO TABLE lt_log_save.
  ENDLOOP.

*-- save log
  CLEAR lr_log.

  lr_alog->save(
    EXPORTING
      iv_log_object    = iv_log_object
      iv_log_subobject = iv_log_subobject
      it_log           = lt_log_save
    IMPORTING
      er_log           = lr_log
  ).

  IF lr_log->is_error( ).
    RETURN.
  ENDIF.

ENDFUNCTION.
