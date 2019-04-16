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

    METHODS: get_key_testcase
      RETURNING VALUE(testcase) TYPE zdbgl_key_testcases.

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
    DATA: key_testcase TYPE zdbgl_key_testcases,
          source_info  TYPE REF TO data.
    FIELD-SYMBOLS: <prog> TYPE tpda_curr_source_pos.

    source_info = cl_tpda_ctrl_handler=>get_srcinfo( ).
    ASSIGN source_info->* TO <prog>.
    key_testcase = get_key_testcase( ).
    IF key_testcase IS INITIAL.
      RETURN.
    ENDIF.
    zdbgl_store_globals=>store(
      program = <prog>-program
      key_testcase = key_testcase
      force = abap_true ).
    me->break( ).

  ENDMETHOD.                    "script

  METHOD end.
*** insert your code which shall be executed at the end of the scripting (before trace is saved)
*** here

  ENDMETHOD.                    "end

  METHOD get_key_testcase.
    DATA: fields        TYPE STANDARD TABLE OF sval
          INITIAL SIZE 1,
          returncode(1).
    FIELD-SYMBOLS: <field> TYPE sval.

    APPEND INITIAL LINE TO fields ASSIGNING <field>.
    <field>-tabname = 'ZDBGL_VARIABLES'.
    <field>-fieldname = 'KEY_TESTCASE'.
    <field>-fieldtext = text-tca.
    <field>-field_obl = abap_true.

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
    testcase = <field>-value.

  ENDMETHOD.

ENDCLASS.                    "lcl_debugger_script IMPLEMENTATION
*</SCRIPT:SCRIPT_CLASS>

*</SCRIPT:PERSISTENT>
