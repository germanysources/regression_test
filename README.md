# Regression tests in ABAP #

## Idea ##
Regression tests are usable for comparing the results of a program before and after a modification.
Results in ABAP legacy code (programs, functionpools) are often stored in global variables.
This repository contains a debugger script, which records the global variables while reaching a breakpoint
in the ABAP debugger.

## How to use it ##
### Regression test of a procedure ###
#### Before the modification ####
Set a breakpoint before the procedure is executed. Set a breakpoint after the procedure is finished.
Record the global variables at both breakpoints.
Recording is done with executing the debugger script ```zdbgl_script_store_globals```.
Go to "Script" Tab in the debugger and load the script ```zdbgl_script_store_globals``` from the database.
"@todo image

The script will prompt you to enter an Key for the testcase. For each record you should choose an unique id.
"@todo image

### Storage ###
The global variables are stored in the table ```zdbgl_variables```. The column "globals" 
contains the hexadecimal values of the global variables in an json format.
The hexadecimal values are encoded in base 64.

### Modification ###
Now you can modify the procedure.

### After the modification ###
Hopefully you changed the legacy code, so that it now can be covered by unit tests. While writing unit 
tests, you can use the recorded data with the clas ```zdbgl_get_globals```.
Example:
The example can be found in program ```zdbgl_demo_regression_test```.
We recorded the internal table "demo_itab" before the modification.
The first step to record is before the procedure "to_verify" is executed.
The second step tp record is after the procedure "to_verify" is executed. 
The two testcases have the key "BEFORE" and "AFTER".
Now we can modify the procedure "to_verify" and verify, that the
modification doesn't change any behaviour.

Therefore we write the unit test and make use of the records. The records can be used with clas ```zdbgl_get_globals```:
```ABAP
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
``` 

## Restrictions ##
In version 1.0.0 these types are supported:
* simple types (like characters, strings, integer)
* flat structures (complex structures containing components with tables or structures are not supported)
* tables with a flat structure or a simple type as the table line type

These types are not supported:
* all form of references
* complex structures
* tables with complex structures as the table line type or with tables as the table line type

The hexadecimal values are stored. If the system encoding is changed, the records in table ```zdbgl_variables``` must be deleted. The only ca be reused in the same system encoding.

## Installation ##
Installation is done with [abapGit](https://github.com/larshp/abapgit). ABAP 7.40 or higher is needed.

## Logs ##
Exceptions are logged in the checkpointgroup "zdbgl_store_globals" (see transaction saab).
