class ZCL_LOG_GUI_MOD definition
  public
  final
  create private .

public section.

  interfaces ZIF_LOG .

  class-methods GET_INSTANCE
    importing
      !IS_SEL type ZST_LOGGUI_SEL
    exporting
      !ER_INSTANCE type ref to ZCL_LOG_GUI_MOD
      !ER_LOG type ref to ZCL_LOG .
  methods SELECT .
protected section.
private section.

  aliases TT_LOG
    for ZIF_LOG~TT_LOG .

  data MS_SEL type ZST_LOGGUI_SEL .
  data MT_LOG type TT_LOG .

  methods CONSTRUCTOR
    importing
      !IS_SEL type ZST_LOGGUI_SEL .
  methods SELECT_LOG
    importing
      !IS_SEL type ZST_LOGGUI_SEL
    exporting
      !ET_LOG type ZTT_LOG .
ENDCLASS.



CLASS ZCL_LOG_GUI_MOD IMPLEMENTATION.


METHOD constructor.

*-- init
  ms_sel = is_sel.

ENDMETHOD.


METHOD get_instance.

*-- init
  er_log = zcl_log=>get_instance( ).

  CLEAR er_instance.

*-- create instance
  er_instance = NEW zcl_log_gui_mod( is_sel = is_sel ).

ENDMETHOD.


METHOD select.

*-- select log
  me->select_log(
    EXPORTING
      is_sel = ms_sel
    IMPORTING
      et_log = DATA(lt_log)
         ).

*-- build log instances


ENDMETHOD.


METHOD select_log.

*-- init
  CLEAR et_log.

*-- select
  SELECT *
    FROM zta_log
    INTO CORRESPONDING FIELDS OF TABLE et_log
    WHERE log_object IN is_sel-log_object_rng AND
          log_subobject IN is_sel-log_subobject_rng AND
*          log_date
*          log_time
          msg_type IN is_sel-msg_type_rng.

  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

ENDMETHOD.
ENDCLASS.
