CLASS test_store_globals DEFINITION FOR TESTING
  DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    DATA cut TYPE REF TO zdbgl_store_globals.

    METHODS setup
      RAISING cx_tpda.

    METHODS concat FOR TESTING.

ENDCLASS.

CLASS zdbgl_store_globals DEFINITION LOCAL FRIENDS test_store_globals.

CLASS test_store_globals IMPLEMENTATION.

  METHOD setup.

    CREATE OBJECT cut
      EXPORTING
        program = space
        in_unit_test = abap_true.

  ENDMETHOD.

  METHOD concat.
    DATA: exp_db_fragments TYPE zdbgl_store_globals=>_db_fragments,
          act_db_fragments TYPE zdbgl_store_globals=>_db_fragments.
    FIELD-SYMBOLS: <line> TYPE string,
                   <frag> TYPE zdbgl_variables.

    APPEND INITIAL LINE TO cut->json_fragments ASSIGNING <line>.
    DO 2499 TIMES.
      <line> = <line> && 'A'.
    ENDDO.
    APPEND INITIAL LINE TO cut->json_fragments ASSIGNING <line>.
    DO 50 TIMES.
      <line> = <line> && 'B'.
    ENDDO.

    APPEND INITIAL LINE TO exp_db_fragments ASSIGNING <frag>.
    <frag>-globals = '{'.
    DO 1199 TIMES.
      <frag>-globals = <frag>-globals && 'A'.
    ENDDO.
    APPEND INITIAL LINE TO exp_db_fragments ASSIGNING <frag>.
    DO 1200 TIMES.
      <frag>-globals = <frag>-globals && 'A'.
    ENDDO.

    APPEND INITIAL LINE TO exp_db_fragments ASSIGNING <frag>.
    DO 100 TIMES.
      <frag>-globals = <frag>-globals && 'A'.
    ENDDO.
    <frag>-globals = <frag>-globals && ','.
    DO 50 TIMES.
      <frag>-globals = <frag>-globals && 'B'.
    ENDDO.
    <frag>-globals = <frag>-globals && '}'.

    cut->concat_json_fragments_sstrings( IMPORTING db_fragments = act_db_fragments ).

    cl_abap_unit_assert=>assert_equals( exp = exp_db_fragments
      act = act_db_fragments msg = 'Splitted Table' ).

  ENDMETHOD.

ENDCLASS.
