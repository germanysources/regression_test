class ZDBGL_STORE_GLOBALS definition
  public
  inheriting from ZDBGL_ABSTRACT_STORAGE
  final
  create private .

public section.

  class-methods STORE
    importing
      !PROGRAM type PROGNAME
      !KEY_TESTCASE type ZDBGL_KEY_TESTCASES
      !FORCE type SAP_BOOL
    raising
      CX_STATIC_CHECK .
protected section.

  data GLOBALS type TPDA_SCR_GLOBALS_IT .
private section.

  types:
    _db_fragments TYPE STANDARD TABLE OF zdbgl_variables .

  methods CONSTRUCTOR
    importing
      !PROGRAM type PROGNAME
      !IN_UNIT_TEST type SAP_BOOL optional
    raising
      CX_TPDA .
  methods HANDLE
    raising
      CX_TPDA .
  methods CONCAT_JSON_FRAGMENTS_SSTRINGS
    exporting
      !DB_FRAGMENTS type _DB_FRAGMENTS .
  methods LOG_STORAGE
    importing
      !PROGRAM type PROGRAM
      !KEY_TESTCASE type ZDBGL_KEY_TESTCASES .
  methods GET_LENGTH_DB_FRAGMENT
    returning
      value(LENGTH) type I .
ENDCLASS.



CLASS ZDBGL_STORE_GLOBALS IMPLEMENTATION.


  METHOD CONCAT_JSON_FRAGMENTS_SSTRINGS.
    DATA: len            TYPE i,
          offset         TYPE i,
          json_as_string TYPE string.
    FIELD-SYMBOLS: <db_frag> TYPE zdbgl_variables.

    len = get_length_db_fragment( ).
    json_as_string  = concat_json_fragments_string( ).

    WHILE offset < strlen( json_as_string ).
      APPEND INITIAL LINE TO db_fragments ASSIGNING <db_frag>.
      TRY.
          <db_frag>-globals = substring( val = json_as_string
            off = offset len = len ).
          offset = offset + len.
        CATCH cx_sy_range_out_of_bounds.
          <db_frag>-globals = substring( val = json_as_string
          off = offset ).
          offset = strlen( json_as_string ).
      ENDTRY.
    ENDWHILE.

  ENDMETHOD.


  method CONSTRUCTOR.

    super->constructor( ).
    IF in_unit_test = abap_false.
      globals = cl_tpda_script_data_descr=>globals(
        EXPORTING p_program = program ).
    ENDIF.

  endmethod.


  method GET_LENGTH_DB_FRAGMENT.
    DATA: field_desc TYPE STANDARD TABLE OF dfies.

    CALL FUNCTION 'DDIF_FIELDINFO_GET'
      EXPORTING
        tabname = 'ZDBGL_VARIABLES'
        fieldname = 'GLOBALS'
      TABLES
        dfies_tab = field_desc.

    READ TABLE field_desc ASSIGNING FIELD-SYMBOL(<field>)
      INDEX 1.
    length = <field>-leng.

  endmethod.


  METHOD handle.
    FIELD-SYMBOLS: <variable> TYPE tpda_scr_globals.

    LOOP AT globals ASSIGNING <variable>.

      TRY.
          APPEND _handle( name = <variable>-name
            is_object = abap_true ) TO json_fragments.
          ##NO_HANDLER
        CATCH zcx_dbgl_type_not_supported.
      ENDTRY.

    ENDLOOP.

  ENDMETHOD.


  method LOG_STORAGE.
    DATA: subkey(200).

    subkey = program && '_' && key_testcase.
    LOG-POINT ID zdbgl_store_globals SUBKEY subkey
      FIELDS json_fragments.

  endmethod.


  METHOD store.
    DATA: parser          TYPE REF TO zdbgl_store_globals,
          " Fragment in db table zdbgl_variables
          db_fragments    TYPE _db_fragments,
          exception       TYPE REF TO cx_root,
          exc_program     TYPE syrepid,
          exc_include     TYPE syrepid,
          exc_source_line TYPE i.
    FIELD-SYMBOLS: <db_frag> TYPE zdbgl_variables.

    TRY.
        CREATE OBJECT parser
          EXPORTING
            program = program.

        parser->handle( ).

        parser->log_storage( program = program
          key_testcase = key_testcase ).
        parser->concat_json_fragments_sstrings( IMPORTING db_fragments
          = db_fragments ).

        LOOP AT db_fragments ASSIGNING <db_frag>.
          <db_frag>-abap_program = program.
          <db_frag>-key_testcase = key_testcase.
          <db_frag>-key_data = sy-tabix.
        ENDLOOP.

        IF force = abap_true.
          DELETE FROM zdbgl_variables WHERE abap_program = program AND
            key_testcase = key_testcase.
          INSERT zdbgl_variables FROM TABLE db_fragments.
        ELSE.
          TRY.
              INSERT zdbgl_variables FROM TABLE db_fragments.
            CATCH cx_sy_open_sql_db INTO DATA(fail).
              IF fail->textid = fail->duplicate_key.
                RAISE EXCEPTION TYPE zcx_dbgl_testcase
                  EXPORTING
                    key_testcase = key_testcase
                    textid       = zcx_dbgl_testcase=>duplicate_testcase.
              ELSE.
                RAISE EXCEPTION fail.
              ENDIF.
          ENDTRY.
        ENDIF.
        COMMIT WORK.

      ##CATCH_ALL
      CATCH cx_root INTO exception.
        " log exception and raise again
        exception->get_source_position(
          IMPORTING program_name = exc_program include_name = exc_include
            source_line = exc_source_line ).
        LOG-POINT ID zdbgl_store_globals FIELDS program
          exception->get_text( ) exc_program exc_include exc_source_line.
        RAISE EXCEPTION exception.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
