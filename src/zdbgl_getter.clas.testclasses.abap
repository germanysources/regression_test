CLASS test_getter DEFINITION FOR TESTING
  DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.

  METHODS get_hex_value FOR TESTING
    RAISING
      cx_static_check.

ENDCLASS.

CLASS zdbgl_getter DEFINITION LOCAL FRIENDS test_getter.

CLASS test_getter IMPLEMENTATION.

  METHOD get_hex_value.
    DATA: _cut TYPE REF TO zdbgl_getter,
          values TYPE string,
          exp_base64_value TYPE string,
          act_base64_value TYPE string.

    exp_base64_value = 'IJ'.
    values = '{"before":"ABC","var":"IJ","after":"DD"}'.
    CREATE OBJECT _cut
      EXPORTING
        values = values
        program = 'test'.

    act_base64_value = _cut->get_hex_value( 'var' ).
    cl_abap_unit_assert=>assert_equals( exp = exp_base64_value
      act = act_base64_value msg = 'value not determined correctly' ).

  ENDMETHOD.

ENDCLASS.
