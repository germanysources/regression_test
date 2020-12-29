class ZCX_DBGL_DICTIONARY_ACCESS definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

public section.

  constants CLASS_INTF type SOTR_CONC value '0050569AC5931EDB92B727A8E6BD20DA' ##NO_TEXT.

  methods CONSTRUCTOR
    importing
      !TEXTID like TEXTID optional
      !PREVIOUS like PREVIOUS optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_DBGL_DICTIONARY_ACCESS IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
TEXTID = TEXTID
PREVIOUS = PREVIOUS
.
  endmethod.
ENDCLASS.
