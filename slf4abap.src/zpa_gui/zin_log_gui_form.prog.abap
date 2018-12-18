FORM open_gui .

*  lcl_app=>gui( )->go_home( ).

  CALL SELECTION-SCREEN 1001.

ENDFORM.

FORM exit .

ENDFORM.

FORM output .

  DATA lt_ucomm TYPE TABLE OF sy-ucomm.

  PERFORM set_pf_status IN PROGRAM rsdbrunt IF FOUND.

  APPEND 'CRET' TO lt_ucomm.  "button execute

  CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
    EXPORTING
      p_status  = sy-pfkey
    TABLES
      p_exclude = lt_ucomm.

ENDFORM.
