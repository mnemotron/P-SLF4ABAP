*----------------------------------------------------------------------*
***INCLUDE LZFG_LOG_LA_PLOGO01.
*----------------------------------------------------------------------*
MODULE status_1000 OUTPUT.

  SET PF-STATUS '1000'.
  SET TITLEBAR '1000'.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  INIT_1000  OUTPUT
*&---------------------------------------------------------------------*
MODULE init_1000 OUTPUT.

*-- build screen
  PERFORM build_screen.

*-- init timer
  PERFORM build_timer.

ENDMODULE.
