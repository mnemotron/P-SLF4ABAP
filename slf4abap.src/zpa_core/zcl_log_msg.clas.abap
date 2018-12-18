"!
"! Author: Franz Christoph
"! Creation Date: 2018
"! Version: 1.0.0
"! Since: 1.0.0
"!
class ZCL_LOG_MSG definition
  public
  final
  create private .

public section.

  interfaces ZIF_LOG .

  aliases TS_LOG
    for ZIF_LOG~TS_LOG .
  aliases TS_LOG_DATA
    for ZIF_LOG~TS_LOG_DATA .
  aliases TS_LOG_DATA_S
    for ZIF_LOG~TS_LOG_DATA_S .
  aliases TS_LOG_DATA_SI
    for ZIF_LOG~TS_LOG_DATA_SI .
  aliases TS_LOG_DATA_V
    for ZIF_LOG~TS_LOG_DATA_V .
  aliases TT_LOG_DATA
    for ZIF_LOG~TT_LOG_DATA .
  aliases TT_LOG_DATA_S
    for ZIF_LOG~TT_LOG_DATA_S .
  aliases TT_LOG_DATA_SI
    for ZIF_LOG~TT_LOG_DATA_SI .
  aliases TT_LOG_DATA_V
    for ZIF_LOG~TT_LOG_DATA_V .
  aliases TT_LOG_TAG
    for ZIF_LOG~TT_LOG_TAG .

  data MT_LOG_DATA type TT_LOG_DATA .
  data MT_LOG_DATA_V type TT_LOG_DATA_V .
  data MT_LOG_DATA_S type TT_LOG_DATA_S .
  data MT_LOG_DATA_SI type TT_LOG_DATA_SI .
  data MT_LOG_TAG type TT_LOG_TAG .
  data MS_LOG type TS_LOG .

  class-methods GET_INSTANCE
    importing
      !IS_LOG type TS_LOG optional
    returning
      value(RR_INSTANCE) type ref to ZCL_LOG_MSG .
  methods ADD_DATA_TYPE
    importing
      !IV_NAME type STRING
      !IX_DATA type ANY .
  methods ADD_TAG
    importing
      !IV_TAG type STRING .
  methods GET_MESSAGE
    returning
      value(RV_MESSAGE) type STRING .
  methods RAISE_MESSAGE .
  methods SET_GUID
    importing
      !IV_GUID type SYSUUID_C32 .
protected section.
"!
"! Author: Franz Christoph
"! Creation Date: 2018
"! Version: 1.0.0
"! Since: 1.0.0
"!
private section.
"!
"! Author: Franz Christoph
"! Creation Date: 2018
"! Version: 1.0.0
"! Since: 1.0.0
"!

  data MV_INDEX type INT4 .
  constants GC_TYPE_VARIABLE type CHAR01 value 'V' ##NO_TEXT.
  constants GC_TYPE_STRUCTURE type CHAR01 value 'S' ##NO_TEXT.
  constants GC_TYPE_TABLE type CHAR01 value 'T' ##NO_TEXT.

  methods CONSTRUCTOR
    importing
      !IS_LOG type TS_LOG optional .
  methods ADD_DATA_TYPE_GEN
    importing
      !IV_NAME type STRING
      !IX_DATA type ANY
    exporting
      !EV_TYPE type CHAR01
      !EV_GUID type SYSUUID_C32 .
  methods ADD_DATA_VARIABLE
    importing
      !IV_NAME type STRING
      !IV_DATA type ANY
    exporting
      !EV_GUID type SYSUUID_C32 .
  methods ADD_DATA_STRUCTURE
    importing
      !IV_NAME type STRING
      !IS_DATA type ANY
    exporting
      !EV_GUID type SYSUUID_C32 .
  methods ADD_DATA
    importing
      !IV_GUID type SYSUUID_C32
      !IV_TYPE type CHAR01 .
  methods GET_TYPE_KIND
    importing
      !IX_TYPE type DATA
    exporting
      !EV_TYPE_KIND type ABAP_TYPEKIND .
  methods GET_TYPE_DESCRIPTION
    importing
      !IX_DATA type ANY
    exporting
      !ER_TYPE_DESCRIPTION type ref to CL_ABAP_ELEMDESCR .
  methods GET_STRUCTURE_DESCRIPTION
    importing
      !IX_DATA type ANY
    exporting
      !ER_STRUCTURE_DESCRIPTION type ref to CL_ABAP_STRUCTDESCR .
  methods GET_NEXT_INDEX
    exporting
      !EV_INDEX type INT4 .
ENDCLASS.



CLASS ZCL_LOG_MSG IMPLEMENTATION.


METHOD ADD_DATA.

  IF iv_guid IS INITIAL.
    RETURN.
  ENDIF.

*-- add GUID
  INSERT VALUE #( guid = iv_guid
                  type = iv_type
                ) INTO TABLE mt_log_data.

ENDMETHOD.


METHOD ADD_DATA_STRUCTURE.

  DATA lv_name TYPE string.
  FIELD-SYMBOLS <fx_data> TYPE any.

*-- init
  CLEAR ev_guid.

*-- get next index
  me->get_next_index(
    IMPORTING
      ev_index = DATA(lv_index)
         ).

  ev_guid = lv_index.

*-- get structure description
  me->get_structure_description(
    EXPORTING
      ix_data                  = is_data
    IMPORTING
      er_structure_description = DATA(lr_structure_description)
         ).

*- add structure
  INSERT VALUE #( guid = ev_guid
                  name = iv_name
                 ) INTO TABLE mt_log_data_s.

*-- add each structure field
  LOOP AT lr_structure_description->components REFERENCE INTO DATA(lr_c).

    ASSIGN COMPONENT lr_c->name OF STRUCTURE is_data TO <fx_data>.

    lv_name = lr_c->name.

*- add type
    me->add_data_type_gen(
      EXPORTING
        iv_name = lv_name
        ix_data = <fx_data>
      IMPORTING
        ev_type = DATA(lv_type)
        ev_guid = DATA(lv_guid)
           ).

    INSERT VALUE #( guids = ev_guid
                    guidx = lv_guid
                    type = lv_type
                 ) INTO TABLE mt_log_data_si.

  ENDLOOP.

ENDMETHOD.


METHOD ADD_DATA_TYPE.

*-- add type
  me->add_data_type_gen(
    EXPORTING
      iv_name = iv_name
      ix_data = ix_data
    IMPORTING
      ev_type = DATA(lv_type)
      ev_guid = DATA(lv_guid)
         ).

*-- add GUID
  me->add_data( iv_guid = lv_guid iv_type = lv_type ).

ENDMETHOD.


METHOD ADD_DATA_TYPE_GEN.
*
* Numeric Data Type         | ID
* ------------------------------
* b                         | b
* s                         | s
* i                         | I
* p                         | P
* decfloat16                | a
* decfloat34                | e
* f                         | F
*
* Character-Like Data Type  | ID
* ------------------------------
* c                         | C
* string                    | g
* n                         | N
* d                         | D
* t                         | T
*
* Complex Type              | ID
* ------------------------------
* flat structure            | u
* deep structure            | v
* internal table            | h
*
* Byte Like Data Type       | ID
* ------------------------------
* x                         | X
* xstring                   | Y
*
* Reference Type            | ID
* ------------------------------
* data reference            | l
* object reference          | r
*


*-- init
  CLEAR ev_guid.
  CLEAR ev_type.

*-- determine type kind
  me->get_type_kind(
    EXPORTING
      ix_type      = ix_data
    IMPORTING
      ev_type_kind = DATA(lv_type_kind)
         ).

*-- execute type log
  CASE lv_type_kind.

*- numeric data type
    WHEN 'b' OR 's' OR 'I' OR 'P' OR 'a' OR 'e' OR 'F'.

      me->add_data_variable(
        EXPORTING
          iv_name = iv_name
          iv_data = ix_data
        IMPORTING
          ev_guid = ev_guid
             ).

      ev_type = gc_type_variable.

*- character like data type
    WHEN 'C' OR 'g' OR 'N' OR 'D' OR 'T'.

      me->add_data_variable(
        EXPORTING
          iv_name = iv_name
          iv_data = ix_data
        IMPORTING
          ev_guid = ev_guid
             ).

      ev_type = gc_type_variable.

*- complex type flat/deep structure
    WHEN 'u' OR 'v'.

      me->add_data_structure(
        EXPORTING
          iv_name = iv_name
          is_data = ix_data
        IMPORTING
          ev_guid = ev_guid
             ).

      ev_type = gc_type_structure.

*- internal table
    WHEN 'h'.
*      CL_ABAP_TABLEDESCR

      ev_type = gc_type_table.

*- byte like data type
    WHEN 'X' OR 'y'.

*- reference type
    WHEN 'l' OR 'r'.

  ENDCASE.

ENDMETHOD.


METHOD ADD_DATA_VARIABLE.

  DATA lv_value TYPE string.

*-- init
  CLEAR ev_guid.

*-- get next index
  me->get_next_index(
    IMPORTING
      ev_index = DATA(lv_index)
         ).

  ev_guid = lv_index.

*-- add variable
  lv_value = iv_data.

  INSERT VALUE #( guid = ev_guid
                  name = iv_name
                  value = lv_value
                 ) INTO TABLE mt_log_data_v.

ENDMETHOD.


METHOD add_tag.

  IF iv_tag IS INITIAL OR line_exists( mt_log_tag[ table_line = iv_tag ] ).
    RETURN.
  ENDIF.

  INSERT iv_tag INTO TABLE mt_log_tag.

ENDMETHOD.


METHOD CONSTRUCTOR.

*-- init
  ms_log = is_log.

ENDMETHOD.


METHOD GET_INSTANCE.

*-- init
  CLEAR rr_instance.

*-- create instance
  rr_instance = NEW zcl_log_msg( is_log = is_log ).

ENDMETHOD.


METHOD get_message.

*-- init
  CLEAR rv_message.

*-- return message
  MESSAGE ID ms_log-id
          TYPE ms_log-type_sap
          NUMBER ms_log-number
    WITH ms_log-message_v1
         ms_log-message_v2
         ms_log-message_v3
         ms_log-message_v4
    INTO rv_message.

ENDMETHOD.


METHOD GET_NEXT_INDEX.

*-- init
  CLEAR ev_index.

*-- next index
  mv_index = mv_index + 1.

*-- return next index
  ev_index = mv_index.

ENDMETHOD.


METHOD GET_STRUCTURE_DESCRIPTION.

*-- init
  CLEAR er_structure_description.

*-- get structure description
  er_structure_description ?= cl_abap_datadescr=>describe_by_data( p_data = ix_data ).

ENDMETHOD.


METHOD GET_TYPE_DESCRIPTION.

*-- init
  CLEAR er_type_description.

*-- get type description
  er_type_description ?= cl_abap_datadescr=>describe_by_data( p_data = ix_data ).

ENDMETHOD.


METHOD GET_TYPE_KIND.

*-- init
  CLEAR ev_type_kind.

*-- determine type kind
  ev_type_kind = cl_abap_datadescr=>get_data_type_kind( p_data = ix_type ).

ENDMETHOD.


METHOD raise_message.

  MESSAGE ID ms_log-id
    TYPE ms_log-type_sap
    NUMBER ms_log-number
    WITH ms_log-message_v1
         ms_log-message_v2
         ms_log-message_v3
         ms_log-message_v4.

ENDMETHOD.


METHOD set_guid.

  ms_log-guid = iv_guid.

ENDMETHOD.
ENDCLASS.
