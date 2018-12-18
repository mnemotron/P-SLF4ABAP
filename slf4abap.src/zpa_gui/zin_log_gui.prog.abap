CLASS lcl_gui DEFINITION FINAL CREATE PRIVATE.

  PUBLIC SECTION.

    CLASS-METHODS get_instance
      RETURNING
        VALUE(rr_instance) TYPE REF TO lcl_gui.

    METHODS on_event FOR EVENT sapevent OF cl_gui_html_viewer
      IMPORTING action frame getdata postdata query_table.  "#EC NEEDED

    METHODS go_home.

  PRIVATE SECTION.

    DATA mr_html_viewer TYPE REF TO cl_gui_html_viewer.

    METHODS constructor.

    METHODS init.

    METHODS handle_action
      IMPORTING action      TYPE c
                frame       TYPE c OPTIONAL
                getdata     TYPE c OPTIONAL
                postdata    TYPE cnht_post_data_tab OPTIONAL
                query_table TYPE cnht_query_table OPTIONAL.

ENDCLASS.

CLASS lcl_gui IMPLEMENTATION.

  METHOD constructor.

    init( ).

  ENDMETHOD.

  METHOD init.

    DATA lt_events TYPE cntl_simple_events.
    DATA ls_event LIKE LINE OF lt_events.

*-- create HTML viewer instance
    CREATE OBJECT mr_html_viewer
      EXPORTING
        query_table_disabled = abap_true
        parent               = cl_gui_container=>screen0.

*-- add events
    ls_event-eventid    = mr_html_viewer->m_id_sapevent.
    ls_event-appl_event = abap_true.
    APPEND ls_event TO lt_events.

    mr_html_viewer->set_registered_events( lt_events ).

    SET HANDLER me->on_event FOR mr_html_viewer.

  ENDMETHOD.

  METHOD on_event.

    me->handle_action(
      action      = action
      frame       = frame
      getdata     = getdata
      postdata    = postdata
      query_table = query_table ).

  ENDMETHOD.

  METHOD handle_action.

  ENDMETHOD.

  METHOD go_home.

*    on_event( action = |{ zif_abapgit_definitions=>gc_action-go_main }| ).

  ENDMETHOD.

  METHOD get_instance.

*-- init
    CLEAR rr_instance.

*-- create instance
    rr_instance = NEW lcl_gui( ).

  ENDMETHOD.

ENDCLASS.
