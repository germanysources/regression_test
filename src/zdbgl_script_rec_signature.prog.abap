*<SCRIPT:PERSISTENT>
REPORT  rstpda_script_template.

*<SCRIPT:HEADER>
*<SCRIPTNAME>ZDBGL_SCRIPT_STORE_GLOBALS</SCRIPTNAME>
*<SCRIPT_CLASS>LCL_DEBUGGER_SCRIPT</SCRIPT_CLASS>
*<SCRIPT_COMMENT>Debugger Skript: Default Template</SCRIPT_COMMENT>
*<SINGLE_RUN>X</SINGLE_RUN>

*</SCRIPT:HEADER>

*<SCRIPT:PRESETTINGS>

*</SCRIPT:PRESETTINGS>

*<SCRIPT:SCRIPT_CLASS>
*---------------------------------------------------------------------*
*       CLASS lcl_debugger_script DEFINITION
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
CLASS lcl_debugger_script DEFINITION INHERITING FROM  cl_tpda_script_class_super  .

  PUBLIC SECTION.
    METHODS: prologue  REDEFINITION,
      init    REDEFINITION,
      script  REDEFINITION,
      end     REDEFINITION.

  PRIVATE SECTION.

    METHODS:
      get_locals
        IMPORTING
                  source_info   TYPE tpda_curr_source_pos
        RETURNING VALUE(result) TYPE REF TO zdbgl_getter
        RAISING   zcx_dbgl_testcase cx_tpda,
      create_file_name
        IMPORTING
                  source_info   TYPE tpda_curr_source_pos
        RETURNING VALUE(result) TYPE string,
      get_file_system_path
        IMPORTING
                  file_name   TYPE string
        RETURNING VALUE(path) TYPE string,
      create_file
        IMPORTING
          path             TYPE string
          signature_record TYPE REF TO zdbgl_signature_record.

ENDCLASS.                    "lcl_debugger_script DEFINITION
*---------------------------------------------------------------------*
*       CLASS lcl_debugger_script IMPLEMENTATION
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
CLASS lcl_debugger_script IMPLEMENTATION.
  METHOD prologue.
*** generate abap_source (source handler for ABAP)
    super->prologue( ).
  ENDMETHOD.                    "prolog

  METHOD init.
*** insert your initialization code here
  ENDMETHOD.                    "init

  METHOD script.
    DATA: source_info      TYPE REF TO data,
          signature_record TYPE REF TO zdbgl_signature_record.
    FIELD-SYMBOLS: <source_info> TYPE tpda_curr_source_pos.

    TRY.
        source_info = cl_tpda_ctrl_handler=>get_srcinfo( ).
        ASSIGN source_info->* TO <source_info>.

        CREATE OBJECT signature_record.

        signature_record->record_function_mod_signature( EXPORTING
          recorded_locals = get_locals( <source_info> )
          source_position = <source_info> ).

        create_file( path = get_file_system_path( create_file_name( <source_info> ) )
          signature_record = signature_record ).

      CATCH cx_static_check INTO DATA(fault).
        MESSAGE fault TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.
    me->break( ).

  ENDMETHOD.                    "script

  METHOD end.
*** insert your code which shall be executed at the end of the scripting (before trace is saved)
*** here

  ENDMETHOD.                    "end

  METHOD get_locals.
    DATA: parser          TYPE REF TO zdbgl_abstract_storage,
          function_module TYPE program.

    CREATE OBJECT parser TYPE zdbgl_store_locals.
    parser->handle( ).

    function_module = source_info-eventname.
    CREATE OBJECT result
      EXPORTING
        values  = parser->concat_json_fragments_string( )
        program = function_module.

  ENDMETHOD.

  METHOD create_file_name.
    DATA: suffix TYPE char10.

    IF source_info-flag_eoev = abap_true.
      suffix = 'end.json'.
    ELSE.
      suffix = 'begin.json'.
    ENDIF.
    result = |{ source_info-eventname }_{ suffix }|.

  ENDMETHOD.

  METHOD get_file_system_path.
    DATA: fields            TYPE STANDARD TABLE OF sval
          INITIAL SIZE 1,
          returncode(1),
          file_separator(1).
    FIELD-SYMBOLS: <field> TYPE sval.

    cl_gui_frontend_services=>get_file_separator(
      CHANGING file_separator = file_separator ).

    APPEND INITIAL LINE TO fields ASSIGNING <field>.
    <field>-tabname = 'ZDBGL_EXPORT_PATH'.
    <field>-fieldname = 'DIRECTORY_PATH'.
    <field>-fieldtext = text-dir.
    <field>-field_obl = abap_true.
    APPEND INITIAL LINE TO fields ASSIGNING <field>.
    <field>-tabname = 'ZDBGL_EXPORT_PATH'.
    <field>-fieldname = 'FILENAME'.
    <field>-fieldtext = text-fil.
    <field>-field_obl = abap_true.
    <field>-value = file_name.

    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
        popup_title = text-tca
      IMPORTING
        returncode  = returncode
      TABLES
        fields      = fields.
    IF returncode = 'A'.
      RETURN.
    ENDIF.

    READ TABLE fields ASSIGNING <field> INDEX 1.
    path = <field>-value.
    READ TABLE fields ASSIGNING <field> INDEX 2.
    CONCATENATE path file_separator <field>-value INTO path.

  ENDMETHOD.

  METHOD create_file.
    TYPES line_type(200).
    DATA: file_content TYPE STANDARD TABLE OF line_type,
          _file_content TYPE string,
          offset TYPE i.

    signature_record->get_parameter_values( IMPORTING
      signature = _file_content ).

    WHILE offset <= strlen( _file_content ).
      APPEND _file_content+offset TO file_content.
      offset = offset + 200.
    ENDWHILE.

    cl_gui_frontend_services=>gui_download( EXPORTING
      filename = path filetype = 'ASC' codepage = '4110'
      write_lf = abap_false
      CHANGING data_tab = file_content ).

  ENDMETHOD.

ENDCLASS.                    "lcl_debugger_script IMPLEMENTATION
*</SCRIPT:SCRIPT_CLASS>

*</SCRIPT:PERSISTENT>
