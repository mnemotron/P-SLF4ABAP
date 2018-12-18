class ZCL_LOG_LA definition
  public
  abstract
  create public .

public section.

  interfaces ZIF_LOG .

  aliases TS_LOG
    for ZIF_LOG~TS_LOG .
  aliases TT_LOG
    for ZIF_LOG~TT_LOG .

  methods INIT
    importing
      !IV_IMMEDIATE_FLUSH type ABAP_BOOL
    exporting
      !ER_LOG type ref to ZCL_LOG .
  methods SAVE
    importing
      !IV_LOG_OBJECT type BAL_S_LOG-OBJECT optional
      !IV_LOG_SUBOBJECT type BAL_S_LOG-SUBOBJECT optional
      !IT_LOG type TT_LOG
    exporting
      !ER_LOG type ref to ZCL_LOG .
  methods GET_LOG_NUMBER
    returning
      value(RV_LOG_NUMBER) type STRING .
protected section.

  data MV_IMMEDIATE_FLUSH type ABAP_BOOL .

  methods IS_IMMEDIATE_FLUSH
    returning
      value(RV_RESULT) type ABAP_BOOL .
  methods POST
    importing
      !IR_LOG type ref to ZCL_LOG
      !IV_WAIT type ABAP_BOOL default ABAP_TRUE
      !IV_RFC_DESTINATION type RFCDEST optional
      !IV_CLOSE_SESSION type ABAP_BOOL default ABAP_FALSE
    exporting
      !ER_LOG type ref to ZCL_LOG .
private section.
ENDCLASS.



CLASS ZCL_LOG_LA IMPLEMENTATION.


  method GET_LOG_NUMBER.
  endmethod.


METHOD init.

*-- init
  er_log = zcl_log=>get_instance( ).

  mv_immediate_flush = iv_immediate_flush.

ENDMETHOD.


METHOD is_immediate_flush.

  rv_result = mv_immediate_flush.

ENDMETHOD.


METHOD post.

  DATA ls_return TYPE bapiret2.

*-- init
  er_log = zcl_log=>get_instance( ).

*-- commit or rollback based on the log
  IF ir_log IS BOUND AND ir_log->is_error( ).

*- rollback
    IF iv_rfc_destination IS INITIAL.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'. "#EC CI_USE_WANTED
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
        DESTINATION iv_rfc_destination. "#EC CI_USE_WANTED
    ENDIF.

  ELSE.

    IF iv_rfc_destination IS INITIAL.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait   = iv_wait
        IMPORTING
          return = ls_return.

      er_log->import_msg_from_bapiret( is_bapiret = ls_return ).

    ELSE.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        DESTINATION iv_rfc_destination
        EXPORTING
          wait                  = iv_wait
        IMPORTING
          return                = ls_return
        EXCEPTIONS
          system_failure        = 1
          communication_failure = 2. "#EC CI_USE_WANTED

      IF sy-subrc <> 0.
        er_log->import_msg_from_symsg( ).
      ENDIF.

      er_log->import_msg_from_bapiret( is_bapiret = ls_return ).

    ENDIF.

  ENDIF.

*-- close RFC session
  IF iv_rfc_destination IS NOT INITIAL AND iv_close_session = abap_true.

    CALL FUNCTION 'RFC_CONNECTION_CLOSE'
      EXPORTING
        destination          = iv_rfc_destination
*       taskname             =
      EXCEPTIONS
        destination_not_open = 1
        error_message        = 2
        OTHERS               = 3.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

  ENDIF.

ENDMETHOD.


  method SAVE.
  endmethod.
ENDCLASS.
