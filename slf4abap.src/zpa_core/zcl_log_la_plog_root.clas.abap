class ZCL_LOG_LA_PLOG_ROOT definition
  public
  final
  create public
  shared memory enabled .

public section.

  methods ADD
    importing
      !IT_LOG type ZTT_LOG_LA_PLOG .
  methods REMOVE
    returning
      value(RT_LOG) type ZTT_LOG_LA_PLOG .
  methods IS_EMPTY
    returning
      value(RV_RESULT) type ABAP_BOOL .
  methods IS_POPUP_ONLINE
    exporting
      !EV_RESULT type ABAP_BOOL
      !ER_LOG type ref to ZCL_LOG .
  methods SET_POPUP_OPEN .
  methods SET_POPUP_CLOSED .
  methods SET_LAST_READ .
protected section.
private section.

  data MT_LOG type ZTT_LOG_LA_PLOG .
  data MV_TIMESTAMP_LAST_READ type TIMESTAMPL .
  data MV_FLAG_LAST_READ type ABAP_BOOL .

  methods REFRESH .
ENDCLASS.



CLASS ZCL_LOG_LA_PLOG_ROOT IMPLEMENTATION.


METHOD add.

  INSERT LINES OF it_log INTO TABLE mt_log.

ENDMETHOD.


METHOD IS_EMPTY.

  IF mt_log IS INITIAL.
    rv_result = abap_true.
  ELSE.
    rv_result = abap_false.
  ENDIF.

ENDMETHOD.


METHOD is_popup_online.

  DATA lv_time_current TYPE timestampl.

*-- init
  er_log = zcl_log=>get_instance( ).

  CLEAR ev_result.

*-- check, is popup already online?
  IF mv_flag_last_read = abap_true.

    DATA(lr_timestamp) = NEW cl_abap_tstmp( ).

    GET TIME STAMP FIELD lv_time_current.

    TRY.

        lr_timestamp->subtractsecs(
          EXPORTING
            tstmp                      = lv_time_current
            secs                       = 4
          RECEIVING
            r_tstmp                    = lv_time_current
             ).

        DATA(lv_compare) = lr_timestamp->compare( tstmp1 = mv_timestamp_last_read tstmp2 = lv_time_current ).

      CATCH cx_parameter_invalid_type INTO DATA(lr_parameter_invalid_type).
        er_log->import_msg_from_cx_msg( ir_exception = lr_parameter_invalid_type ).
        RETURN.
      CATCH cx_parameter_invalid_range INTO DATA(lr_parameter_invalid_range).
        er_log->import_msg_from_cx_msg( ir_exception = lr_parameter_invalid_range ).
        RETURN.

    ENDTRY.

*-- result: Smaller (-1), Equal (0), Greater (1)
    IF lv_compare = -1 AND lv_compare <> 0.
      ev_result = abap_false.
    ELSE.
      ev_result = abap_true.
    ENDIF.

  ELSE.
    ev_result = abap_false.
  ENDIF.

ENDMETHOD.


METHOD REFRESH.

  CLEAR mt_log.

ENDMETHOD.


METHOD REMOVE.

*-- init
  CLEAR rt_log.

*-- return logs
  rt_log[] = mt_log[].

*-- refresh queue
  me->refresh( ).

ENDMETHOD.


METHOD set_last_read.

  GET TIME STAMP FIELD mv_timestamp_last_read.

ENDMETHOD.


METHOD set_popup_closed.

  mv_flag_last_read = abap_false.

ENDMETHOD.


METHOD set_popup_open.

  mv_flag_last_read = abap_true.

ENDMETHOD.
ENDCLASS.
