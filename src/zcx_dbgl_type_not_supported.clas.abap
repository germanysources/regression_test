class ZCX_DBGL_TYPE_NOT_SUPPORTED definition
  public
  inheriting from CX_STATIC_CHECK
  final
  create public .

public section.

  constants ZCX_DBGL_TYPE_NOT_SUPPORTED type SOTR_CONC value '005056914D751EE9B7D4371EF906A7FD' ##NO_TEXT.
  data TYPE type STRING .

  methods CONSTRUCTOR
    importing
      !TEXTID like TEXTID optional
      !PREVIOUS like PREVIOUS optional
      !TYPE type STRING .
protected section.
private section.
ENDCLASS.



CLASS ZCX_DBGL_TYPE_NOT_SUPPORTED IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
TEXTID = TEXTID
PREVIOUS = PREVIOUS
.
 IF textid IS INITIAL.
   me->textid = ZCX_DBGL_TYPE_NOT_SUPPORTED .
 ENDIF.
me->TYPE = TYPE .
  endmethod.
ENDCLASS.
