*&---------------------------------------------------------------------*
*& Report  ZDBGL_COPY_TO_TDC
*&
*&---------------------------------------------------------------------*
REPORT ZDBGL_COPY_LOCALS_TO_TDC.

PARAMETERS: program TYPE tpda_program,
  include TYPE tpda_include,
  line TYPE tpda_sc_line,
  key_tc TYPE zdbgl_key_testcases,
  tdc TYPE etobj_name,
  version TYPE etobj_ver,
  variant TYPE etvar_id.

START-OF-SELECTION.

  TRY.
      DATA(base64_storage) = zdbgl_get_locals=>factory(
        key_testcase = key_tc source_position = VALUE #(
          abap_program = program include = include line = line
        ) ).
      DATA(tdc_manager) = NEW zdbgl_copy_to_tdc( tdc = tdc
        tdc_version = version ).
      tdc_manager->copy_all_parameter( variant = variant
        recorded_variables = base64_storage ).
      MESSAGE text-sav TYPE 'S'.

    CATCH zcx_dbgl_testcase INTO DATA(failure).
      MESSAGE failure TYPE 'S' DISPLAY LIKE 'E'.
    CATCH zcx_dbgl_copy_error INTO DATA(copy_failure).
      MESSAGE copy_failure TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.
