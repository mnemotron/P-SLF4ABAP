*----------------------------------------------------------------------*
***INCLUDE LZFG_LOG_LA_PLOGF01.
*----------------------------------------------------------------------*
FORM user_command_1000 USING gv_okcode TYPE sy-ucomm..

  DATA lv_count TYPE int1.

  gv_save_okcode = gv_okcode.

  CLEAR gv_okcode.

  CASE gv_save_okcode.
    WHEN 'EXIT'.
      lv_count = 5.
      PERFORM set_popup_closed CHANGING lv_count.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CALL_LOG_POPUP
*&---------------------------------------------------------------------*
FORM call_log_popup USING iv_popup TYPE abap_bool.

  IF iv_popup = abap_true.

    CALL SCREEN '1000'
      STARTING AT 10 1
      ENDING AT 130 15.

  ELSE.

    CALL SCREEN '1000'.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SET_LOG
*&---------------------------------------------------------------------*
FORM set_log USING it_log TYPE ztt_log_la_plog.

  gt_log[] = it_log[].

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SET_LOG_ALV
*&---------------------------------------------------------------------*
FORM set_log_alv.

*-- init
  CLEAR gt_log_alv.

  gt_log_alv[] = gt_log[].

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  BUILD_SCREEN
*&---------------------------------------------------------------------*
FORM build_timer.

  DATA lr_event TYPE REF TO lcl_handle_timer_events.

*-- time only if immediate flush is on
  IF gv_immediate_flush = abap_false.
    RETURN.
  ENDIF.

*-- build timer
  IF gr_timer IS INITIAL.

    CREATE OBJECT gr_timer.

    CREATE OBJECT lr_event.
    SET HANDLER lr_event->timer_finished FOR gr_timer.

*-- set interval in seconds
    gr_timer->interval = 2.

  ENDIF.

  gr_timer->run( ).

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  BUILD_SCREEN
*&---------------------------------------------------------------------*
FORM build_screen.

  DATA lr_salv_functions TYPE REF TO cl_salv_functions_list.
  DATA lr_salv_columns TYPE REF TO cl_salv_columns_table.
  DATA lr_salv_display TYPE REF TO cl_salv_display_settings.
  DATA lr_salv_select TYPE REF TO cl_salv_selections.
  DATA lr_salv_events TYPE REF TO cl_salv_events_table.
  DATA lr_salv_events_handle TYPE REF TO lcl_handle_salv_events.
  DATA lv_text_user TYPE string.
  DATA lv_text_info TYPE string.
  DATA lv_text_warning TYPE string.
  DATA lv_text_error TYPE string.
  DATA lv_text_fatal TYPE string.
  DATA lv_text_debug TYPE string.
  DATA lv_tooltip TYPE string.
  DATA lv_icon TYPE string.
  DATA lt_count_type TYPE tt_count_type.
  DATA lr_column TYPE REF TO cl_salv_column_table.

*-- create container
  IF gr_container IS INITIAL.

    CREATE OBJECT gr_container
      EXPORTING
*       parent                      =
        container_name              = 'GR_CONTAINER'
*       style                       =
*       lifetime                    = lifetime_default
*       repid                       =
*       dynnr                       =
*       no_autodef_progid_dynnr     =
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid
              TYPE sy-msgty
              NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.


  ENDIF.

*-- create table
  TRY.
      cl_salv_table=>factory(
        EXPORTING
          list_display   = if_salv_c_bool_sap=>false
          r_container    = gr_container
          container_name = 'GR_CONTAINER'
        IMPORTING
          r_salv_table   = gr_salv_table
        CHANGING
          t_table        = gt_log_alv
      ).

    CATCH cx_salv_msg INTO DATA(lr_salv_msg).
      MESSAGE ID lr_salv_msg->msgid
        TYPE lr_salv_msg->msgty
        NUMBER lr_salv_msg->msgno
        WITH lr_salv_msg->msgv1
             lr_salv_msg->msgv2
             lr_salv_msg->msgv3
             lr_salv_msg->msgv4.
  ENDTRY.

*- setup functions
  lr_salv_functions = gr_salv_table->get_functions( ).
  lr_salv_functions->set_all( if_salv_c_bool_sap=>true ).

  TRY.
      lv_icon = icon_position_hr.
      lv_tooltip = text-001.
      lr_salv_functions->add_function(
        name     = 'FILTER_USER'
        icon     = lv_icon
        text     = '[0]'
        tooltip  = lv_tooltip
        position = if_salv_c_function_position=>left_of_salv_functions ).

      lv_icon = icon_message_information_small.
      lv_tooltip = text-002.
      lr_salv_functions->add_function(
        name     = 'FILTER_INFO'
        icon     = lv_icon
        text     = '[0]'
        tooltip  = lv_tooltip
        position = if_salv_c_function_position=>left_of_salv_functions ).

      lv_icon = icon_message_warning_small.
      lv_tooltip = text-003.
      lr_salv_functions->add_function(
        name     = 'FILTER_WARNING'
        icon     = lv_icon
        text     = '[0]'
        tooltip  = lv_tooltip
        position = if_salv_c_function_position=>left_of_salv_functions ).

      lv_icon = icon_message_error_small.
      lv_tooltip = text-004.
      lr_salv_functions->add_function(
        name     = 'FILTER_ERROR'
        icon     = lv_icon
        text     = '[0]'
        tooltip  = lv_tooltip
        position = if_salv_c_function_position=>left_of_salv_functions ).

      lv_icon = icon_message_critical_small.
      lv_tooltip = text-005.
      lr_salv_functions->add_function(
        name     = 'FILTER_FATAL'
        icon     = lv_icon
        text     = '[0]'
        tooltip  = lv_tooltip
        position = if_salv_c_function_position=>left_of_salv_functions ).

      lv_icon = icon_location.
      lv_tooltip = text-006.
      lr_salv_functions->add_function(
        name     = 'FILTER_DEBUG'
        icon     = lv_icon
        text     = '[0]'
        tooltip  = lv_tooltip
        position = if_salv_c_function_position=>left_of_salv_functions ).

    CATCH cx_salv_wrong_call INTO DATA(lr_cx_salv_wrong_call).

      DATA(ls_msg) = lr_cx_salv_wrong_call->get_message( ).

      MESSAGE ID ls_msg-msgid
        TYPE ls_msg-msgty
        NUMBER ls_msg-msgno
        WITH ls_msg-msgv1
             ls_msg-msgv2
             ls_msg-msgv3
             ls_msg-msgv4.

    CATCH cx_salv_existing INTO DATA(lr_cx_salv_existing).

      DATA(ls_msg1) = lr_cx_salv_existing->get_message( ).

      MESSAGE ID ls_msg1-msgid
        TYPE ls_msg1-msgty
        NUMBER ls_msg1-msgno
        WITH ls_msg1-msgv1
             ls_msg1-msgv2
             ls_msg1-msgv3
             ls_msg1-msgv4.

  ENDTRY.

*- setup display settings
  lr_salv_display = gr_salv_table->get_display_settings( ).
  lr_salv_display->set_striped_pattern( if_salv_c_bool_sap=>true ). "activate striped pattern

*- setup selection setting
  lr_salv_select = gr_salv_table->get_selections( ).
  IF lr_salv_select IS BOUND.
    lr_salv_select->set_selection_mode( value = if_salv_c_selection_mode=>none ).
  ENDIF.

*- setup event settings
  lr_salv_events =  gr_salv_table->get_event( ).

  CREATE OBJECT lr_salv_events_handle.
  SET HANDLER lr_salv_events_handle->on_user_command FOR lr_salv_events.

*- setup columns
  lr_salv_columns = gr_salv_table->get_columns( ).

  IF lr_salv_columns IS BOUND.

    lr_salv_columns->set_optimize( if_salv_c_bool_sap=>true ).


    DATA(lt_salv_column_ref) = lr_salv_columns->get( ).

    LOOP AT lt_salv_column_ref REFERENCE INTO DATA(lr_cref).

      TRY.
          lr_column ?= lr_salv_columns->get_column( lr_cref->columnname ).
        CATCH cx_salv_not_found INTO DATA(lr_cx_salv_not_found).
          DATA(ls_msg2) = lr_cx_salv_not_found->get_message( ).

          MESSAGE ID ls_msg2-msgid
            TYPE ls_msg2-msgty
            NUMBER ls_msg2-msgno
            WITH ls_msg2-msgv1
                 ls_msg2-msgv2
                 ls_msg2-msgv3
                 ls_msg2-msgv4.
      ENDTRY.

      IF lr_column IS NOT BOUND.
        CONTINUE.
      ENDIF.

      CASE lr_cref->columnname.

        WHEN 'TYPE'.
          lr_column->set_visible( value = if_salv_c_bool_sap=>false ).

      ENDCASE.

    ENDLOOP.

  ENDIF.

*-- set alv log table
  PERFORM set_log_alv.

*-- set log button filtered
  PERFORM set_log_btn_filtered.

*- display table
  gr_salv_table->display( ).

ENDFORM.

FORM refresh_salv.

  TRY.

*-- optimize columns
      DATA(lr_salv_columns) = gr_salv_table->get_columns( ).

      IF lr_salv_columns IS BOUND.
        lr_salv_columns->set_optimize( if_salv_c_bool_sap=>true ).
      ENDIF.

*-- refresh
      gr_salv_table->refresh(
*        EXPORTING
*          s_stable     =
*          refresh_mode =
      ).

      gr_salv_table->display( ).

    CATCH cx_salv_no_new_data_allowed INTO DATA(lr_cx_salv_no_new_data_allowed).

      DATA(ls_msg) = lr_cx_salv_no_new_data_allowed->get_message( ).

      MESSAGE ID ls_msg-msgid
        TYPE ls_msg-msgty
        NUMBER ls_msg-msgno
        WITH ls_msg-msgv1
             ls_msg-msgv2
             ls_msg-msgv3
             ls_msg-msgv4.
  ENDTRY.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  HANDLE_SALV_USER_COMMAND
*&---------------------------------------------------------------------*
FORM handle_salv_user_command USING iv_ucomm.

  CASE iv_ucomm.

    WHEN 'FILTER_USER'.
      PERFORM add_log_filter
            USING
               zcl_log=>gc_type_user.

    WHEN 'FILTER_INFO'.
      PERFORM add_log_filter
            USING
               zcl_log=>gc_type_info.

    WHEN 'FILTER_WARNING'.
      PERFORM add_log_filter
            USING
               zcl_log=>gc_type_warning.

    WHEN 'FILTER_ERROR'.
      PERFORM add_log_filter
            USING
               zcl_log=>gc_type_error.

    WHEN 'FILTER_FATAL'.
      PERFORM add_log_filter
            USING
               zcl_log=>gc_type_fatal.

    WHEN 'FILTER_DEBUG'.
      PERFORM add_log_filter
            USING
               zcl_log=>gc_type_debug.

  ENDCASE.

*-- filter log
  PERFORM filter_log.

*-- set button filtered
  PERFORM set_log_btn_filtered.

*-- refresh table
  PERFORM refresh_salv.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  COUNT_MSG_TYPES
*&---------------------------------------------------------------------*
FORM count_msg_types CHANGING ct_count_type TYPE tt_count_type.

  DATA ls_count_type TYPE ts_count_type.

*-- init
  CLEAR ct_count_type.

*-- count
  LOOP AT gt_log REFERENCE INTO DATA(lr_log).

    CLEAR ls_count_type.
    ls_count_type-type = lr_log->type.
    ls_count_type-count = 1.

    COLLECT ls_count_type INTO ct_count_type.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  GET_LOG_FILTERED
*&---------------------------------------------------------------------*
FORM add_log_filter USING iv_type TYPE char01.

*-- add filter
  IF line_exists( gt_filter_btn[ type = iv_type ] ).
    DELETE gt_filter_btn WHERE type = iv_type.
  ELSEIF iv_type IS NOT INITIAL.
    INSERT VALUE #( type = iv_type ) INTO TABLE gt_filter_btn.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SET_LOG_BTN_FILTERED
*&---------------------------------------------------------------------*
FORM set_log_btn_filtered.

  DATA(lr_salv_functions) = gr_salv_table->get_functions( ).
  DATA(lt_salv_function_list) = lr_salv_functions->get_functions( ).
  DATA lv_tooltip TYPE string.
  DATA lv_icon TYPE string.
  DATA lt_count_type TYPE tt_count_type.
  DATA lr_ct TYPE REF TO ts_count_type.

*- count message types
  PERFORM count_msg_types
              CHANGING
                 lt_count_type.

*-- set button text
  LOOP AT lt_salv_function_list REFERENCE INTO DATA(lr_r).

    DATA(lv_text) = lr_r->r_function->get_text( ).

    IF lv_text IS INITIAL.
      CONTINUE.
    ENDIF.

    DATA(lv_l) = strlen( lv_text ).
    DATA(lv_offset) = lv_l - 1.

    IF lv_text+lv_offset(1) CS '*'.
      lv_text = lv_text+0(lv_offset).
    ENDIF.


    CASE lr_r->r_function->get_name( ).

      WHEN 'FILTER_USER'.

        READ TABLE lt_count_type
          WITH TABLE KEY type =  zcl_log=>gc_type_user
          REFERENCE INTO lr_ct.

        IF sy-subrc = 0.
          lv_text = |[{ lr_ct->count }]|.
        ENDIF.

        IF line_exists( gt_filter_btn[ type = zcl_log=>gc_type_user ] ).
          lv_text = |{ lv_text }*|.
        ENDIF.

        lr_r->r_function->set_text( value = lv_text ).

      WHEN 'FILTER_INFO'.

        READ TABLE lt_count_type
          WITH TABLE KEY type =  zcl_log=>gc_type_info
          REFERENCE INTO lr_ct.

        IF sy-subrc = 0.
          lv_text = |[{ lr_ct->count }]|.
        ENDIF.

        IF line_exists( gt_filter_btn[ type = zcl_log=>gc_type_info ] ).
          lv_text = |{ lv_text }*|.
        ENDIF.

        lr_r->r_function->set_text( value = lv_text ).

      WHEN 'FILTER_WARNING'.

        READ TABLE lt_count_type
          WITH TABLE KEY type =  zcl_log=>gc_type_warning
          REFERENCE INTO lr_ct.

        IF sy-subrc = 0.
          lv_text = |[{ lr_ct->count }]|.
        ENDIF.

        IF line_exists( gt_filter_btn[ type = zcl_log=>gc_type_warning ] ).
          lv_text = |{ lv_text }*|.
        ENDIF.

        lr_r->r_function->set_text( value = lv_text ).

      WHEN 'FILTER_ERROR'.

        READ TABLE lt_count_type
          WITH TABLE KEY type =  zcl_log=>gc_type_error
          REFERENCE INTO lr_ct.

        IF sy-subrc = 0.
          lv_text = |[{ lr_ct->count }]|.
        ENDIF.

        IF line_exists( gt_filter_btn[ type = zcl_log=>gc_type_error ] ).
          lv_text = |{ lv_text }*|.
        ENDIF.

        lr_r->r_function->set_text( value = lv_text ).

      WHEN 'FILTER_FATAL'.

        READ TABLE lt_count_type
          WITH TABLE KEY type =  zcl_log=>gc_type_fatal
          REFERENCE INTO lr_ct.

        IF sy-subrc = 0.
          lv_text = |[{ lr_ct->count }]|.
        ENDIF.

        IF line_exists( gt_filter_btn[ type = zcl_log=>gc_type_fatal ] ).
          lv_text = |{ lv_text }*|.
        ENDIF.

        lr_r->r_function->set_text( value = lv_text ).

      WHEN 'FILTER_DEBUG'.

        READ TABLE lt_count_type
          WITH TABLE KEY type =  zcl_log=>gc_type_debug
          REFERENCE INTO lr_ct.

        IF sy-subrc = 0.
          lv_text = |[{ lr_ct->count }]|.
        ENDIF.

        IF line_exists( gt_filter_btn[ type = zcl_log=>gc_type_debug ] ).
          lv_text = |{ lv_text }*|.
        ENDIF.

        lr_r->r_function->set_text( value = lv_text ).

    ENDCASE.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FILTER_LOG
*&---------------------------------------------------------------------*
FORM filter_log.

*-- init alv log table
  PERFORM set_log_alv.

*-- filter log table
  LOOP AT gt_filter_btn REFERENCE INTO DATA(lr_f).
    DELETE gt_log_alv WHERE type = lr_f->type.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SET_POPUP_CLOSED
*&---------------------------------------------------------------------*
FORM set_popup_closed CHANGING lv_count TYPE int1.

  IF lv_count < 1.
    RETURN.
  ENDIF.

  TRY.

*- attach for update incl. lock
      DATA(lr_shared_object_area) = zcl_log_la_plog_area=>attach_for_update(
*                                 inst_name                     = CL_SHM_AREA=>DEFAULT_INSTANCE
                                  attach_mode                   = cl_shm_area=>attach_mode_wait
                                  wait_time                     = 1000
                        ).

      lr_shared_object_area->root->set_popup_closed( ).

*- unlock
      lr_shared_object_area->detach_commit( ).

    CATCH cx_shm_wrong_handle INTO DATA(lr_shm_wrong_handle).
      RETURN.

    CATCH cx_shm_already_detached INTO DATA(lr_shm_already_detached).
      RETURN.

    CATCH cx_shm_secondary_commit INTO DATA(lr_shm_secondary_commit).
      RETURN.

    CATCH cx_shm_event_execution_failed INTO DATA(lr_shm_event_execution_failed).
      RETURN.

    CATCH cx_shm_completion_error INTO DATA(lr_shm_completion_error).
      RETURN.

    CATCH cx_shm_inconsistent INTO DATA(lr_shm_inconsistent).
      RETURN.

    CATCH cx_shm_no_active_version INTO DATA(lr_shm_no_active_version).
      RETURN.

    CATCH cx_shm_exclusive_lock_active INTO DATA(lr_shm_exclusive_lock_active).
      RETURN.

    CATCH cx_shm_version_limit_exceeded INTO DATA(lr_shm_version_limit_exceeded).
      RETURN.

    CATCH cx_shm_change_lock_active INTO DATA(lr_shm_change_lock_active).

      lv_count = lv_count - 1.

      PERFORM set_popup_closed
                  CHANGING
                     lv_count.

      RETURN.

    CATCH cx_shm_parameter_error INTO DATA(lr_shm_parameter_error).
      RETURN.

    CATCH cx_shm_pending_lock_removed INTO DATA(lr_shm_pending_lock_removed).
      RETURN.

  ENDTRY.

ENDFORM.
