class ZDBGL_GET_LOCALS definition
  public
  INHERITING FROM zdbgl_getter
  final
  create public .

public section.
  TYPES: BEGIN OF _source_position,
    abap_program TYPE tpda_program,
    include TYPE tpda_include,
    line TYPE tpda_sc_line,
  END OF _source_position.

  class-methods FACTORY
    IMPORTING
      key_testcase TYPE zdbgl_key_testcases
      source_position TYPE _source_position
    RETURNING VALUE(instance) TYPE REF TO zdbgl_get_locals
    RAISING
      zcx_dbgl_testcase.
protected section.
private section.
ENDCLASS.



CLASS ZDBGL_GET_LOCALS IMPLEMENTATION.


  method FACTORY.
    TYPES: BEGIN OF _record,
      locals TYPE zdbgl_locals-locals,
    END OF _record.
    DATA: full_record TYPE STANDARD TABLE OF _record,
          locals_json_string TYPE string,
          record TYPE REF TO _record.

    SELECT locals FROM zdbgl_locals
      INTO CORRESPONDING FIELDS OF TABLE @full_record
      WHERE abap_program = @source_position-abap_program
      AND include = @source_position-include AND line = @source_position-line
      AND key_testcase = @key_testcase.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_dbgl_testcase
        EXPORTING
          textid = zcx_dbgl_testcase=>testcase_not_found
          key_testcase = key_testcase
          program = source_position-abap_program.
    ENDIF.

    LOOP AT full_record REFERENCE INTO record.
      locals_json_string = locals_json_string &&
        record->*-locals.
    ENDLOOP.

    CREATE OBJECT instance
      EXPORTING
        values = locals_json_string
        program = source_position-abap_program.

  endmethod.
ENDCLASS.
