class ZDBGL_GET_GLOBALS definition
  public
  final
  create public
  inheriting from zdbgl_getter.

public section.

  class-methods FACTORY
    IMPORTING
      key_testcase TYPE zdbgl_key_testcases
      program TYPE progname
    RETURNING VALUE(instance) TYPE REF TO zdbgl_get_globals
    RAISING
      zcx_dbgl_testcase.
protected section.
private section.
ENDCLASS.



CLASS ZDBGL_GET_GLOBALS IMPLEMENTATION.


  method FACTORY.
    TYPES: BEGIN OF record,
      globals TYPE zdbgl_variables-globals,
    END OF record.
    DATA: full_record TYPE STANDARD TABLE OF record,
          globals_json_string TYPE string.
    FIELD-SYMBOLS: <record> TYPE record.

    SELECT globals FROM zdbgl_variables
      INTO CORRESPONDING FIELDS OF TABLE @full_record
      WHERE abap_program = @program AND key_testcase = @key_testcase.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_dbgl_testcase
        EXPORTING
          textid = zcx_dbgl_testcase=>testcase_not_found
          key_testcase = key_testcase
          program = program.
    ENDIF.

    LOOP AT full_record ASSIGNING <record>.
      globals_json_string = globals_json_string &&
        <record>-globals.
    ENDLOOP.

    CREATE OBJECT instance
      EXPORTING
        values = globals_json_string
        program = program.

  endmethod.
ENDCLASS.
