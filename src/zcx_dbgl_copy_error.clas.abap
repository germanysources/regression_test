class ZCX_DBGL_COPY_ERROR definition
  public
  inheriting from CX_STATIC_CHECK
  final
  create private .

public section.

  methods CONSTRUCTOR
    importing
      !TEXTID like TEXTID optional
      !PREVIOUS like PREVIOUS optional .
  class-methods WRAP_FAILURE
    importing
      !FAILURE type ref to CX_STATIC_CHECK
    raising
      ZCX_DBGL_COPY_ERROR .

  methods IF_MESSAGE~GET_LONGTEXT
    redefinition .
  methods IF_MESSAGE~GET_TEXT
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCX_DBGL_COPY_ERROR IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
TEXTID = TEXTID
PREVIOUS = PREVIOUS
.
  endmethod.


  method IF_MESSAGE~GET_LONGTEXT.

    IF previous IS BOUND.
      result = previous->if_message~get_longtext( preserve_newlines ).
    ELSE.
      result = me->if_message~get_longtext( preserve_newlines ).
    ENDIF.

  endmethod.


  method IF_MESSAGE~GET_TEXT.

    IF previous IS BOUND.
      result = previous->if_message~get_text( ).
    ELSE.
      result = me->if_message~get_text( ).
    ENDIF.

  endmethod.


  method WRAP_FAILURE.

    RAISE EXCEPTION TYPE zcx_dbgl_copy_error
      EXPORTING
        previous = failure.

  endmethod.
ENDCLASS.
