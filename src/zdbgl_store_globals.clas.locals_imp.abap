DEFINE data_serialize_hex_format.
* Data definition for serialize in hex format.
* &1 class CL_TPDA_SCRIPT_STRINGDESCR, CL_TPDA_SCRIPT_ELEMDESCR, CL_TPDA_SCRIPT_STRUCTDESCR

  DATA: value TYPE string,
        element TYPE REF TO &1.

END-OF-DEFINITION.

DEFINE serialize_hex_format.

  element ?= descr.
  value = cl_http_utility=>encode_x_base64( element->hexvalue( ) ).
  " Add only the hexadecimal value to the json string
  IF is_object = abap_true.
    fragment = quote && name && quote && colon && quote && value
      && quote.
  ELSE.
    fragment = quote && value && quote.
  ENDIF.

END-OF-DEFINITION.
