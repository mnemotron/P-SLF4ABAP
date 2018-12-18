INTERFACE zif_log
  PUBLIC .

  TYPES:
    tt_log_la TYPE STANDARD TABLE OF REF TO zcl_log_la WITH NON-UNIQUE DEFAULT KEY.

  TYPES:
    tt_log_tag TYPE STANDARD TABLE OF string WITH NON-UNIQUE DEFAULT KEY .
  TYPES:
    BEGIN OF ts_log_data_v,
      guid  TYPE int4,
      name  TYPE string,
      value TYPE string,
    END OF ts_log_data_v .
  TYPES:
    tt_log_data_v TYPE SORTED TABLE OF ts_log_data_v WITH UNIQUE KEY guid .
  TYPES:
    BEGIN OF ts_log_data_si,
      guids TYPE int4,
      guidx TYPE int4,
      type  TYPE char01,
    END OF ts_log_data_si .
  TYPES:
    tt_log_data_si TYPE SORTED TABLE OF ts_log_data_si WITH UNIQUE KEY guids guidx .
  TYPES:
    BEGIN OF ts_log_data_s,
      guid TYPE int4,
      name TYPE string,
    END OF ts_log_data_s .
  TYPES:
    tt_log_data_s TYPE SORTED TABLE OF ts_log_data_s WITH UNIQUE KEY guid .
  TYPES:
    BEGIN OF ts_log_data_t,
      guid  TYPE int4,
      guids TYPE int4,
      name  TYPE string,
    END OF ts_log_data_t .
  TYPES:
    tt_log_data_t TYPE SORTED TABLE OF ts_log_data_t WITH UNIQUE KEY guid guids .
  TYPES:
    BEGIN OF ts_log_data,
      guid TYPE int4,
      type TYPE char01,
    END OF ts_log_data .
  TYPES:
    tt_log_data TYPE SORTED TABLE OF ts_log_data WITH UNIQUE KEY guid type .
  TYPES:
    BEGIN OF ts_log,
      guid        TYPE sysuuid_c32,
      date        TYPE sydatum,
      time        TYPE syuzeit,
      uname       TYPE syuname,
      type        TYPE char01,
      type_sap    type symsgty,
      id          TYPE symsgid,
      number      TYPE symsgno,
      message_v1  TYPE symsgv,
      message_v2  TYPE symsgv,
      message_v3  TYPE symsgv,
      message_v4  TYPE symsgv,
      mainprogram TYPE syrepid,
      include     TYPE include,
      line        TYPE int4,
      blocktype   TYPE char12,
      blockname   TYPE string,
    END OF ts_log .
  TYPES:
    tt_log TYPE STANDARD TABLE OF REF TO zcl_log_msg WITH NON-UNIQUE DEFAULT KEY.

  CONSTANTS gc_log_msg_class_id TYPE symsgid VALUE 'ZMC_LOG' ##NO_TEXT.
ENDINTERFACE.
