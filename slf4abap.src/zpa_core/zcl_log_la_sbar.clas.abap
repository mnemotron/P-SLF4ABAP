class ZCL_LOG_LA_SBAR definition
  public
  inheriting from ZCL_LOG_LA
  create public .

public section.

  methods SAVE
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_LOG_LA_SBAR IMPLEMENTATION.


METHOD save.

*-- init
  er_log = zcl_log=>get_instance( ).

  IF it_log IS INITIAL.
    RETURN.
  ENDIF.

*-- get the first message
  READ TABLE it_log
    INDEX 1
    INTO DATA(lr_log).

  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

*-- show message
  MESSAGE ID lr_log->ms_log-id
     TYPE 'S'
     NUMBER lr_log->ms_log-number
     WITH lr_log->ms_log-message_v1
          lr_log->ms_log-message_v2
          lr_log->ms_log-message_v3
          lr_log->ms_log-message_v4
     DISPLAY LIKE lr_log->ms_log-type_sap.

ENDMETHOD.
ENDCLASS.
