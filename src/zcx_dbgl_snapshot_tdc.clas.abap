CLASS zcx_dbgl_snapshot_tdc DEFINITION
  PUBLIC
  INHERITING FROM zcx_dbgl_snapshot
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !textid   LIKE if_t100_message=>t100key OPTIONAL
        !previous LIKE previous OPTIONAL .
    METHODS if_message~get_longtext
         REDEFINITION .
    METHODS if_message~get_text
         REDEFINITION .
    CLASS-METHODS wrap
      IMPORTING
        previous TYPE REF TO cx_static_check
      RAISING
        zcx_dbgl_snapshot_tdc.
protected section.
private section.
ENDCLASS.



CLASS ZCX_DBGL_SNAPSHOT_TDC IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.


  METHOD if_message~get_longtext.

    IF previous IS BOUND.
      result = previous->if_message~get_longtext( preserve_newlines ).
    ELSE.
      result = super->if_message~get_longtext( preserve_newlines ).
    ENDIF.

  ENDMETHOD.


  METHOD if_message~get_text.

    IF previous IS BOUND.
      result = previous->if_message~get_text( ).
    ELSE.
      result = super->if_message~get_text( ).
    ENDIF.

  ENDMETHOD.


  METHOD wrap.

    RAISE EXCEPTION TYPE zcx_dbgl_snapshot_tdc
      EXPORTING
        previous = previous.

  ENDMETHOD.
ENDCLASS.
