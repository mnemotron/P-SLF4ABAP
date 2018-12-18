class ZCL_P definition
  public
  final
  create public .

public section.

  constants GC_PROCESS_PLOG type ZDA_PROCESS value 'PLOG' ##NO_TEXT.

  class-methods LOCK .
  class-methods UNLOCK .
protected section.
private section.
ENDCLASS.



CLASS ZCL_P IMPLEMENTATION.


  method LOCK.
  endmethod.


  method UNLOCK.
  endmethod.
ENDCLASS.
