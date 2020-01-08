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

    METHODS: get_key_tdc_variant
      RETURNING VALUE(result) TYPE zdbgl_tdc_variant_key.

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
    DATA: key_tdc_variant TYPE zdbgl_tdc_variant_key,
          globals_parser  TYPE REF TO zdbgl_store_globals,
          locals_parser   TYPE REF TO zdbgl_store_locals,
          source_info     TYPE REF TO data,
          exception       TYPE REF TO cx_static_check.
    FIELD-SYMBOLS: <prog> TYPE tpda_curr_source_pos.

    TRY.

        source_info = cl_tpda_ctrl_handler=>get_srcinfo( ).
        ASSIGN source_info->* TO <prog>.

        key_tdc_variant = get_key_tdc_variant( ).
        IF key_tdc_variant IS INITIAL.
          RETURN.
        ENDIF.

        CREATE OBJECT globals_parser
          EXPORTING
            program = <prog>-program.
        CREATE OBJECT locals_parser.

        zdbgl_copy_to_tdc=>copy_from_debugger(
          tdc_variant_key = key_tdc_variant program = <prog>-program
          debugger_parser = globals_parser ).
        zdbgl_copy_to_tdc=>copy_from_debugger(
          tdc_variant_key = key_tdc_variant program = <prog>-program
          debugger_parser = locals_parser ).
        COMMIT WORK.
      CATCH cx_static_check INTO exception.
        MESSAGE exception TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

    me->break( ).

  ENDMETHOD.                    "script

  METHOD end.
*** insert your code which shall be executed at the end of the scripting (before trace is saved)
*** here

  ENDMETHOD.                    "end

  METHOD get_key_tdc_variant.
    DATA: fields        TYPE STANDARD TABLE OF sval
          INITIAL SIZE 3,
          returncode(1).
    FIELD-SYMBOLS: <field> TYPE sval.

    APPEND INITIAL LINE TO fields ASSIGNING <field>.
    <field>-tabname = 'ZDBGL_TDC_VARIANT_KEY'.
    <field>-fieldname = 'NAME'.
    <field>-fieldtext = text-nam.
    <field>-field_obl = abap_true.
    APPEND INITIAL LINE TO fields ASSIGNING <field>.
    <field>-tabname = 'ZDBGL_TDC_VARIANT_KEY'.
    <field>-fieldname = 'VERSION'.
    <field>-fieldtext = text-ver.
    <field>-field_obl = abap_true.
    APPEND INITIAL LINE TO fields ASSIGNING <field>.
    <field>-tabname = 'ZDBGL_TDC_VARIANT_KEY'.
    <field>-fieldname = 'VARIANT_NAME'.
    <field>-fieldtext = text-var.
    <field>-field_obl = abap_true.

    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
        popup_title = text-tdc
      IMPORTING
        returncode  = returncode
      TABLES
        fields      = fields.
    IF returncode = 'A'.
      RETURN.
    ENDIF.

    READ TABLE fields ASSIGNING <field> INDEX 1.
    result-name = <field>-value.
    READ TABLE fields ASSIGNING <field> INDEX 2.
    result-version = <field>-value.
    READ TABLE fields ASSIGNING <field> INDEX 3.
    result-variant_name = <field>-value.

  ENDMETHOD.

ENDCLASS.                    "lcl_debugger_script IMPLEMENTATION
*</SCRIPT:SCRIPT_CLASS>

*</SCRIPT:PERSISTENT>
