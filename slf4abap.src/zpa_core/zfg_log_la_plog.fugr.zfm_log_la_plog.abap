FUNCTION zfm_log_la_plog.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IT_LOG) TYPE  ZTT_LOG_LA_PLOG
*"     VALUE(IV_POPUP) TYPE  XFELD
*"     VALUE(IV_IMMEDIATE_FLUSH) TYPE  XFELD
*"----------------------------------------------------------------------

*-- set immediate flush
  gv_immediate_flush = iv_immediate_flush.

*-- set log table
  PERFORM set_log USING it_log.

*-- show log popup
  PERFORM call_log_popup
              USING
                 iv_popup.

ENDFUNCTION.
