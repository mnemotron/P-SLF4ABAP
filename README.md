# Simple Logging Facade for ABAP (SLF4ABAP)

## Features
+ <b>TRACE LEVEL</b><br>
  + message logging using trace level (DEBUG, FATAL, ERROR, WARNING, INFO, USER)

+ <b>PERSISTENCE</b><br>
  + log messages can be stored using several log appender i.e. SAP application log, enhanced SLF4ABAP log, status bar, popup list
  + messages can be saved persistently immediately at the time they are thrown (the database commit has no effect on the actual process)
  + the popup list log appender can display messages immediately when a message is thrown and runs independently (like a console)
  
+ <b>IMPORT/EXPORT</b><br>
  + import from default BAPIRET structure and table
  + import from default IF_T100_MESSAGE exceptions
  + import from default IF_MESSAGE exceptions
  + import from default SY message variables
  + export to BAPIRET table

+ <b>ENHANCED COCKPIT</b><br>
  + planned...

+ <b>ENHANCED LOGGING</b><br>
  + several variables with numeric, character or flat-structure data type can be assigned to a debug message (the entire variable content is logged)
  + ABAP callstack information can be logged

+ <b>TAGGING</b><br>
  + a log message can be tagged with several external keys

## Get Started

### Create Instance
```abap
" create SLF4ABAP log instance by factory
DATA(lr_log) = ZCL_LOG=>GET_INSTANCE( ).
```

### Add Log Appender
```abap
" add popup list log appender
  lr_log->add_log_appender( ir_la = NEW zcl_log_la_plog( iv_popup = abap_true ) ).
```

### Log Message
```abap
" add a debug message based on a message class
" <...> are placeholders
DATA(lr_msg_debug) = lr_log->debug( 
    iv_id = '<MESSAGE_CLASS>' 
    iv_number = '<MESSAGE_NUMBER>' 
    iv_message_v1 = '<MESSAGE_V1_VALUE>' ).
 
" add an user message
DATA(lr_msg_user) = lr_log->user( iv_id = '<MESSAGE_CLASS>' iv_number = '<MESSAGE_NUMBER>' ).
```

### Add Tags to a Log Message
```abap
" add an info message based on a message class
" <...> are placeholders
DATA(lr_msg_info) = lr_log->info( iv_id = '<MESSAGE_CLASS>' iv_number = '<MESSAGE_NUMBER>' ).

" add tags
" <...> are placeholders, the external key can be anything i.e. sales order number
lr_msg_info->add_tag( iv_tag = <EXTERNAL_KEY_1> ).
lr_msg_info->add_tag( iv_tag = <EXTERNAL_KEY_2> ).
```

### Assign Variable to a Debug Log Message 
```abap
" add a debug message based on a message class
" <...> are placeholders
DATA(lr_msg_debug) = lr_log->debug( iv_id = '<MESSAGE_CLASS>' iv_number = '<MESSAGE_NUMBER>' ).

" assign a variable
" iv_name: is the name of the passed variable
" ix_data: is the variable itself 
" (currently only flat structures, numeric and character data types are supported)
lr_msg_debug->add_data_type( iv_name = 'lv_char' ix_data = lv_char ).
```

## Import/Export
Use the abapGit client to import or export the repository.

URL: [abapGit](https://github.com/larshp/abapGit)
