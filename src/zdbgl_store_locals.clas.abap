class ZDBGL_STORE_LOCALS definition
  public
  inheriting from ZDBGL_ABSTRACT_STORAGE
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IN_UNIT_TEST type SAP_BOOL optional .
  class-methods STORE
    IMPORTING
      source_position TYPE tpda_curr_source_pos
      key_testcase TYPE zdbgl_key_testcases
      force TYPE abap_bool
    RAISING
      cx_static_check.
protected section.
private section.

  types:
    _db_fragments TYPE STANDARD TABLE OF zdbgl_locals .

  data LOCALS type TPDA_SCR_LOCALS_IT .
  data:
    json_fragments TYPE STANDARD TABLE OF string .

  methods HANDLE
    raising
      CX_TPDA .
  methods CONCAT_JSON_FRAGMENTS
    exporting
      !DB_FRAGMENTS type _DB_FRAGMENTS .
  methods GET_LEN_DB_FRAGMENT
    returning
      value(LEN) type I .
  METHODS log_storage
    IMPORTING
      source_position TYPE tpda_curr_source_pos
      key_testcase TYPE zdbgl_key_testcases.
  CLASS-METHODS move_sp_to_db_fragment
    IMPORTING
      source_position TYPE tpda_curr_source_pos
    CHANGING
      fragment TYPE zdbgl_locals.
ENDCLASS.



CLASS ZDBGL_STORE_LOCALS IMPLEMENTATION.


  method CONCAT_JSON_FRAGMENTS.
    DATA: len            TYPE i VALUE 1200,
          offset         TYPE i,
          json_as_string TYPE string.
    FIELD-SYMBOLS: <db_frag> TYPE zdbgl_locals.

    len = get_len_db_fragment( ).
    json_as_string = '{'.
    LOOP AT json_fragments ASSIGNING FIELD-SYMBOL(<frag>).

      IF sy-tabix = 1.
        json_as_string = json_as_string && <frag>.
      ELSE.
        json_as_string = json_as_string && comma
          && <frag>.
      ENDIF.

    ENDLOOP.
    json_as_string = json_as_string && '}'.

    WHILE offset < strlen( json_as_string ).
      APPEND INITIAL LINE TO db_fragments ASSIGNING <db_frag>.
      TRY.
          <db_frag>-locals = substring( val = json_as_string
            off = offset len = len ).
          offset = offset + len.
        CATCH cx_sy_range_out_of_bounds.
          <db_frag>-locals = substring( val = json_as_string
          off = offset ).
          offset = strlen( json_as_string ).
      ENDTRY.
    ENDWHILE.

  endmethod.


  method CONSTRUCTOR.

    super->constructor( ).
    IF in_unit_test = abap_false.
      locals = cl_tpda_script_data_descr=>locals( ).
    ENDIF.

  endmethod.


  method GET_LEN_DB_FRAGMENT.
    DATA: field_desc TYPE STANDARD TABLE OF dfies.

    CALL FUNCTION 'DDIF_FIELDINFO_GET'
      EXPORTING
        tabname = 'ZDBGL_LOCALS'
        fieldname = 'LOCALS'
      TABLES
        dfies_tab = field_desc.

    READ TABLE field_desc ASSIGNING FIELD-SYMBOL(<field>)
      INDEX 1.
    len = <field>-leng.

  endmethod.


  METHOD handle.
    DATA: variable TYPE REF TO tpda_scr_localsline.

    LOOP AT locals REFERENCE INTO variable.

      TRY.
          APPEND _handle( name = variable->*-name
            is_object = abap_true ) TO json_fragments.
          ##NO_HANDLER
        CATCH zcx_dbgl_type_not_supported.
      ENDTRY.

    ENDLOOP.

  ENDMETHOD.


  method LOG_STORAGE.

    DATA: subkey(200).

    subkey = source_position-program && '_' && source_position-include &&
      '_' && key_testcase.
    LOG-POINT ID zdbgl_store_locals SUBKEY subkey
      FIELDS json_fragments.

  endmethod.


  METHOD move_sp_to_db_fragment.

    fragment-abap_program = source_position-program.
    ##ENH_OK
    MOVE-CORRESPONDING source_position TO fragment.

  ENDMETHOD.


  METHOD store.
    DATA: parser          TYPE REF TO zdbgl_store_locals,
          " Fragment in db table zdbgl_locals
          db_fragments    TYPE _db_fragments,
          exception       TYPE REF TO cx_root,
          exc_program     TYPE syrepid,
          exc_include     TYPE syrepid,
          exc_source_line TYPE i.
    FIELD-SYMBOLS: <db_frag> TYPE zdbgl_locals.

    TRY.
        CREATE OBJECT parser.

        parser->handle( ).

        parser->log_storage( source_position = source_position
          key_testcase = key_testcase ).
        parser->concat_json_fragments( IMPORTING db_fragments
            = db_fragments ).

        LOOP AT db_fragments ASSIGNING <db_frag>.
          <db_frag>-key_testcase = key_testcase.
          <db_frag>-key_data = sy-tabix.
          move_sp_to_db_fragment( EXPORTING source_position = source_position
            CHANGING fragment = <db_frag> ).
        ENDLOOP.

        IF force = abap_true.
          DELETE FROM zdbgl_locals WHERE abap_program = source_position-program
            AND include = source_position-include AND line = source_position-line
            AND key_testcase = key_testcase.
          INSERT zdbgl_locals FROM TABLE db_fragments.
        ELSE.
          TRY.
              INSERT zdbgl_locals FROM TABLE db_fragments.
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
        LOG-POINT ID zdbgl_store_locals FIELDS source_position-program
          exception->get_text( ) exc_program exc_include exc_source_line.
        RAISE EXCEPTION exception.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
