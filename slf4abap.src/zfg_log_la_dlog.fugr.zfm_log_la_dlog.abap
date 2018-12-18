FUNCTION zfm_log_la_dlog.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_LOG_OBJECT) TYPE  BAL_S_LOG-OBJECT
*"     VALUE(IV_LOG_SUBOBJECT) TYPE  BAL_S_LOG-SUBOBJECT
*"     VALUE(IT_LOG) TYPE  ZTT_LOG_LA_DLOG
*"----------------------------------------------------------------------

  DATA lr_log TYPE REF TO zcl_log.
  DATA lt_log_save TYPE zif_log=>tt_log.

*-- init
  DATA(lr_dlog) = NEW zcl_log_la_dlog( ).

  CLEAR lr_log.

  lr_dlog->init(
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
    DATA(lr_logm) = zcl_log_msg=>get_instance( is_log = lr_l->slog ).
    lr_logm->mt_log_data[] = lr_l->tlog_data[].
    lr_logm->mt_log_data_v[] = lr_l->tlog_data_v[].
    lr_logm->mt_log_data_s[] = lr_l->tlog_data_s[].
    lr_logm->mt_log_data_si[] = lr_l->tlog_data_si[].
*   lr_logm->mt_log_data_t[] = lr_l->tlog_data_t[].
    lr_logm->mt_log_tag[] = lr_l->tlog_tag[].
    INSERT lr_logm INTO TABLE lt_log_save.
  ENDLOOP.

*-- save log
  CLEAR lr_log.

  lr_dlog->save(
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
