DEFINE attribute_handler.

  IF node_open_element->if_sxml_named~qname-name = 'array'
    AND node_for_table_pending = abap_true.

    RAISE EXCEPTION TYPE zcx_dbgl_testcase
      EXPORTING
        textid = zcx_dbgl_testcase=>table_line_type_not_supported
        program = program
        linetype = cl_abap_typedescr=>typekind_struct2.

  ENDIF.

  LOOP AT node_open_element->get_attributes( ) INTO attribute.
    IF attribute->qname-name = 'name' AND attribute->get_value( ) = name
      AND node_open_element->if_sxml_named~qname-name = 'array'
      AND object_level = 1.
      node_for_table_pending = abap_true.
    ELSEIF attribute->qname-name = 'name'.
      object_element_name = attribute->get_value( ).
    ENDIF.
  ENDLOOP.

  IF node_open_element->if_sxml_named~qname-name = 'object'.
    ADD 1 TO object_level.
  ENDIF.

END-OF-DEFINITION.

DEFINE prepare_table_line.

  IF node_for_table_pending = abap_true.

    IF node_open_element->if_sxml_named~qname-name = 'object'
      AND line_has_simple_type( <line> ) = abap_false.
      " new line starts
      CLEAR: <line>.
    ENDIF.
    IF node_open_element->if_sxml_named~qname-name = 'str'
      AND line_has_simple_type( <line> ) = abap_false.
      " structured component
      ASSIGN COMPONENT object_element_name OF STRUCTURE <line> TO <line_element>.
    ELSEIF node_open_element->if_sxml_named~qname-name = 'str'.
      ASSIGN line->* TO <line_element>.
    ENDIF.

  ENDIF.

END-OF-DEFINITION.

DEFINE close_element_handler.

  CASE CAST if_sxml_close_element( node )->if_sxml_named~qname-name.
    WHEN 'array'.
      IF node_for_table_pending = abap_true.
        " parsing finished
        RETURN.
      ENDIF.
    WHEN 'object'.
      IF node_for_table_pending = abap_true.
        " line parsing finished for structured type
        INSERT <line> INTO TABLE value.
      ENDIF.
      object_level = object_level - 1.
    WHEN 'str'.
      IF line_has_simple_type( <line> ) = abap_true
        AND node_for_table_pending = abap_true.
        " line parsing finished for simple type
        INSERT <line> INTO TABLE value.
      ENDIF.
  ENDCASE.

END-OF-DEFINITION.
