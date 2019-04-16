*&---------------------------------------------------------------------*
*& Report  ZDBGL_DEMO_REGRESSION_TEST
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZDBGL_DEMO_REGRESSION_TEST.
DATA: demo_itab TYPE STANDARD TABLE OF sflight.

START-OF-SELECTION.
  FIELD-SYMBOLS: <line> TYPE sflight.

  APPEND INITIAL LINE TO demo_itab ASSIGNING <line>.
  <line>-carrid = 'LH'.
  <line>-connid = '3445'.
  <line>-price = 500.
  BREAK-POINT.
  PERFORM to_verify.
  BREAK-POINT.


" subprogram should be verified.
" It changes the global variable demo_itab.
" Imagine this would be legacy code and you must changed this.
FORM to_verify.
  FIELD-SYMBOLS: <line> TYPE sflight.

  APPEND INITIAL LINE TO demo_itab ASSIGNING <line>.
  <line>-carrid = 'LH'.
  <line>-connid = '3444'.
  <line>-price = 400.

ENDFORM.

CLASS regression_test DEFINITION FOR TESTING
  DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    " set_globals is used to set the value of "demo_itab"
    " before the procedure to_verify is executed
    DATA: set_globals TYPE REF TO zdbgl_get_globals,
          verify TYPE REF TO zdbgl_get_globals.

    METHODS setup
      RAISING cx_static_check.

    METHODS verify_changed_itab FOR TESTING
      RAISING cx_static_check.

ENDCLASS.

CLASS regression_test IMPLEMENTATION.

  METHOD setup.

    set_globals = zdbgl_get_globals=>factory(
      EXPORTING program = sy-repid key_testcase = 'BEFORE' ).
    verify = zdbgl_get_globals=>factory(
      EXPORTING program = sy-repid key_testcase = 'AFTER' ).

  ENDMETHOD.

  METHOD verify_changed_itab.
    DATA: exp_demo_itab LIKE demo_itab.

    " set "demo_itab" to the value before the procedure was executed
    set_globals->get_table( EXPORTING name = 'DEMO_ITAB'
      IMPORTING value = demo_itab ).

    " verify give us the expected value
    verify->get_table( EXPORTING name = 'DEMO_ITAB'
      IMPORTING value = exp_demo_itab ).

    " execute procedure
     PERFORM to_verify.

     cl_abap_unit_assert=>assert_equals( exp = exp_demo_itab
      act = demo_itab msg = 'Procedure changed in an invalid way' ).

  ENDMETHOD.

ENDCLASS.
