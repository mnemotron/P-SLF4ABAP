CLASS lcl_app DEFINITION FINAL CREATE PRIVATE.

  PUBLIC SECTION.

    CLASS-METHODS gui
      RETURNING
        VALUE(rr_instance) TYPE REF TO lcl_gui.

  PRIVATE SECTION.

    CLASS-DATA sr_gui TYPE REF TO lcl_gui.

ENDCLASS.

CLASS lcl_app IMPLEMENTATION.

  METHOD gui.

    IF sr_gui IS NOT BOUND.
      sr_gui = lcl_gui=>get_instance( ).
    ENDIF.

    rr_instance = sr_gui.

  ENDMETHOD.

ENDCLASS.
