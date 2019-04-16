CLASS test_get_globals DEFINITION FOR TESTING
  DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
  DATA cut TYPE REF TO zdbgl_get_globals.

  METHODS get_hex_value FOR TESTING
    RAISING
      cx_static_check.

ENDCLASS.

CLASS zdbgl_get_globals DEFINITION LOCAL FRIENDS test_get_globals.

CLASS test_get_globals IMPLEMENTATION.

  METHOD get_hex_value.
    DATA: _cut TYPE REF TO zdbgl_get_globals,
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
