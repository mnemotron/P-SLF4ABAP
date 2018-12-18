FUNCTION-POOL zfg_log_la_plog.              "MESSAGE-ID ..

TYPE-POOLS: abap.

*-- screen: 1000
TYPES: BEGIN OF ts_count_type,
         type  TYPE char01,
         count TYPE int4,
       END OF ts_count_type.

TYPES: BEGIN OF ts_filter_btn,
         type TYPE char01,
       END OF ts_filter_btn.

TYPES: tt_filter_btn TYPE SORTED TABLE OF ts_filter_btn WITH UNIQUE KEY type.
TYPES: tt_count_type TYPE SORTED TABLE OF ts_count_type WITH UNIQUE KEY type.

DATA gv_okcode TYPE sy-ucomm.
DATA gv_save_okcode TYPE sy-ucomm.
DATA gt_log TYPE ztt_log_la_plog.
DATA gt_log_alv TYPE TABLE OF zst_log_la_plog.
DATA gt_filter_btn TYPE tt_filter_btn.
DATA gv_immediate_flush type abap_bool.

DATA gr_container TYPE REF TO cl_gui_custom_container.
DATA gr_salv_table TYPE REF TO cl_salv_table.
DATA gr_timer TYPE REF TO cl_gui_timer.

INCLUDE lzfg_log_la_plogd01.
