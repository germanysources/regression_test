class ZDBGL_STORE_GLOBALS definition
  public
  final
  create private .

public section.

  class-methods STORE
    importing
      !PROGRAM type PROGNAME
      !KEY_TESTCASE type ZDBGL_KEY_TESTCASES
      !FORCE type SAP_BOOL
    RAISING
      cx_static_check.
protected section.

  types:
    _fragments TYPE STANDARD TABLE OF string .
  types:
    _db_fragments TYPE STANDARD TABLE OF zdbgl_variables .

  constants:
    quote(1) value '"' ##NO_TEXT.
  constants:
    colon(1) value ':' ##NO_TEXT.
  constants:
    comma(1) value ',' ##NO_TEXT.
  data GLOBALS type TPDA_SCR_GLOBALS_IT .
  data JSON_FRAGMENTS type _FRAGMENTS .

  methods CONSTRUCTOR
    importing
      !PROGRAM type PROGNAME
      !IN_UNIT_TEST type SAP_BOOL optional
    raising
      CX_TPDA .
  methods HANDLE_TAB
    importing
      !IS_OBJECT type ABAP_BOOL
      !NAME type STRING
      !DESCR type ref to CL_TPDA_SCRIPT_DATA_DESCR
    returning
      value(FRAGMENT) type STRING
    raising
      CX_TPDA
      ZCX_DBGL_TYPE_NOT_SUPPORTED .
  methods HANDLE_OBJECT
    importing
      !NAME type STRING
      !DESCR type ref to CL_TPDA_SCRIPT_DATA_DESCR
    raising
      CX_TPDA
      ZCX_DBGL_TYPE_NOT_SUPPORTED .
  methods HANDLE_OBJREF
    importing
      !NAME type STRING
    raising
      CX_TPDA
      ZCX_DBGL_TYPE_NOT_SUPPORTED .
  methods HANDLE_STRING
    importing
      !IS_OBJECT type ABAP_BOOL
      !NAME type STRING
      !DESCR type ref to CL_TPDA_SCRIPT_DATA_DESCR
    returning
      value(FRAGMENT) type STRING
    raising
      CX_TPDA .
  methods HANDLE_SIMPLE
    importing
      !IS_OBJECT type ABAP_BOOL
      !NAME type STRING
      !DESCR type ref to CL_TPDA_SCRIPT_DATA_DESCR
    returning
      value(FRAGMENT) type STRING
    raising
      CX_TPDA .
  methods HANDLE_STRUCT
    importing
      !IS_OBJECT type ABAP_BOOL
      !NAME type STRING
      !DESCR type ref to CL_TPDA_SCRIPT_DATA_DESCR
    returning
      value(FRAGMENT) type STRING
    raising
      CX_TPDA .
  methods HANDLE_DATAREF
    importing
      !NAME type STRING
      !DESCR type ref to CL_TPDA_SCRIPT_DATA_DESCR
    raising
      CX_TPDA
      ZCX_DBGL_TYPE_NOT_SUPPORTED .
  methods HANDLE
    raising
      CX_TPDA .
  methods _HANDLE
    importing
      !NAME type STRING
      !IS_OBJECT type ABAP_BOOL
    returning
      value(FRAGMENT) type STRING
    raising
      CX_TPDA
      ZCX_DBGL_TYPE_NOT_SUPPORTED .
  methods CONCAT_JSON_FRAGMENTS
    exporting
      !DB_FRAGMENTS type _DB_FRAGMENTS .
  methods LOG_STORAGE
    importing
      !PROGRAM type PROGRAM
      !KEY_TESTCASE type ZDBGL_KEY_TESTCASES .
  methods GET_LEN_DB_FRAGMENT
    returning
      value(LEN) type I .
private section.
ENDCLASS.



CLASS ZDBGL_STORE_GLOBALS IMPLEMENTATION.


  METHOD concat_json_fragments.
    DATA: len            TYPE i VALUE 1200,
          end            TYPE i,
          offset         TYPE i,
          json_as_string TYPE string.
    FIELD-SYMBOLS: <db_frag> TYPE zdbgl_variables.

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

    IF in_unit_test = abap_false.
      globals = cl_tpda_script_data_descr=>globals(
        EXPORTING p_program = program ).
    ENDIF.

  endmethod.


  method GET_LEN_DB_FRAGMENT.
    DATA: field_desc TYPE STANDARD TABLE OF dfies.

    CALL FUNCTION 'DDIF_FIELDINFO_GET'
      EXPORTING
        tabname = 'ZDBGL_VARIABLES'
        fieldname = 'GLOBALS'
      TABLES
        dfies_tab = field_desc.

    READ TABLE field_desc ASSIGNING FIELD-SYMBOL(<field>)
      INDEX 1.
    len = <field>-leng.

  endmethod.


  METHOD handle.
    DATA: fragment TYPE string.
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


  method HANDLE_DATAREF.

    RAISE EXCEPTION TYPE zcx_dbgl_type_not_supported
      EXPORTING
        type = 'DATAREF'.

  endmethod.


  method HANDLE_OBJECT.

    RAISE EXCEPTION TYPE zcx_dbgl_type_not_supported
      EXPORTING
        type = 'OBJECT'.

  endmethod.


  method HANDLE_OBJREF.

    RAISE EXCEPTION TYPE zcx_dbgl_type_not_supported
      EXPORTING
        type = 'OBJECTREF'.

  endmethod.


  method HANDLE_SIMPLE.
    data_serialize_hex_format cl_tpda_script_elemdescr.
    serialize_hex_format.
  endmethod.


  method HANDLE_STRING.
    data_serialize_hex_format cl_tpda_script_stringdescr.
    serialize_hex_format.
  endmethod.


  method HANDLE_STRUCT.
    data_serialize_hex_format cl_tpda_script_structdescr.
    serialize_hex_format.
  endmethod.


  method HANDLE_TAB.
    DATA: tab_fragments TYPE STANDARD TABLE OF string,
          table TYPE REF TO cl_tpda_script_tabledescr,
          name_line TYPE string,
          len TYPE i.
    FIELD-SYMBOLS: <frag> TYPE string.

    table ?= descr.
    DO table->linecnt( ) TIMES.
      name_line = |{ name }[{ sy-index }]|.
      " if _handle( name_line ) is an json object or an json table
      " it is enclosed in brackets
      APPEND _handle( name = name_line is_object = abap_false )
        TO tab_fragments.
    ENDDO.

    fragment = '['.
    LOOP AT tab_fragments ASSIGNING <frag>.
      fragment = fragment && <frag>.
      IF sy-tabix < lines( tab_fragments ).
        fragment = fragment && comma.
      ENDIF.
    ENDLOOP.
    fragment = fragment && ']'.

    IF is_object = abap_true.
      fragment = quote && name && quote && colon && fragment.
    ENDIF.

  endmethod.


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
          exc_source_line TYPE i,
          len             TYPE i,
          idx             TYPE i.
    FIELD-SYMBOLS: <db_frag> TYPE zdbgl_variables.

    TRY.
        CREATE OBJECT parser
          EXPORTING
            program = program.

        parser->handle( ).

        parser->log_storage( program = program
          key_testcase = key_testcase ).
        parser->concat_json_fragments( IMPORTING db_fragments
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


  METHOD _handle.
    DATA: descr TYPE REF TO cl_tpda_script_data_descr,
          info  TYPE tpda_scr_quick_info.

    descr = cl_tpda_script_data_descr=>factory( name ).
    info = cl_tpda_script_data_descr=>get_quick_info( name ).

    CASE info-metatype.
      WHEN cl_tpda_script_data_descr=>mt_simple.
        fragment = handle_simple( name = name is_object = is_object
                       descr = descr ).
      WHEN cl_tpda_script_data_descr=>mt_struct.
        fragment = handle_struct( name  = name is_object = is_object
                       descr = descr ).
      WHEN cl_tpda_script_data_descr=>mt_string.
        fragment = handle_string( name  = name is_object = is_object
                       descr = descr ).
      WHEN cl_tpda_script_data_descr=>mt_tab.
        fragment = handle_tab( name  = name is_object = is_object
                    descr = descr ).
      WHEN cl_tpda_script_data_descr=>mt_datref.
        handle_dataref( name  = name
                        descr = descr ).
      WHEN cl_tpda_script_data_descr=>mt_object.
        handle_object( name  = name
                       descr = descr ).
      WHEN cl_tpda_script_data_descr=>mt_objref.
        handle_objref( name ).
      WHEN OTHERS.
        ASSERT FIELDS 'unknown type' CONDITION 1 = 0.
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
