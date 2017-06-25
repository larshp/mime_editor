*----------------------------------------------------------------------*
***INCLUDE ZMIME_EDITOR_F01.
*----------------------------------------------------------------------*

FORM run.

  SELECT SINGLE * FROM smimloio INTO gs_smimloio
    WHERE loio_id = p_loio.                             "#EC CI_GENBUFF
  IF sy-subrc <> 0.
    WRITE: / 'Not found'(001).
    RETURN.
  ENDIF.

  IF gs_smimloio-lo_class = wbmr_c_skwf_folder_class.
    WRITE: / 'Is folder'(002).
    RETURN.
  ENDIF.

  gv_init = abap_false.

  CALL SCREEN 2000.

ENDFORM.

FORM save.

  DATA: lv_string TYPE string.

  go_editor->get_textstream( IMPORTING text = lv_string ).
  cl_gui_cfw=>flush( ).
  lcl_smim=>save( lv_string ).
  MESSAGE 'Saved'(003) TYPE 'S'.
  go_editor->set_textmodified_status( 0 ).

ENDFORM.

FORM back.

  DATA: lv_status TYPE i.

  go_editor->get_textmodified_status( IMPORTING status = lv_status ).
  cl_gui_cfw=>flush( ).
  IF lv_status = 0.
    LEAVE TO SCREEN 0.
  ELSE.
    MESSAGE 'Not saved'(004) TYPE 'W'.
  ENDIF.

ENDFORM.

FORM init_2000.

  IF NOT go_container IS BOUND.
    CREATE OBJECT go_container
      EXPORTING
        container_name = 'CUSTOM_2000'.

    CREATE OBJECT go_splitter
      EXPORTING
        parent      = go_container
        orientation = 1.
    go_splitter->set_sash_position( 20 ).

    CREATE OBJECT go_tree
      EXPORTING
        parent              = go_splitter->top_left_container
        node_selection_mode = cl_gui_simple_tree=>node_sel_mode_single.
    PERFORM init_tree.

    CREATE OBJECT go_editor
      EXPORTING
        parent = go_splitter->bottom_right_container.
    go_editor->set_font_fixed( ).
  ENDIF.

  PERFORM init_editor.

ENDFORM.

FORM init_editor.

  DATA: lv_content TYPE string.

  IF gv_init = abap_false.
    lv_content = lcl_smim=>get_content( ).
    go_editor->set_textstream( lv_content ).
    go_editor->set_focus( go_editor ).
    gv_init = abap_true.
  ENDIF.

ENDFORM.

FORM init_tree.

  DATA: lt_events TYPE cntl_simple_events,
        lt_nodes  TYPE ty_nodes,
        ls_event  LIKE LINE OF lt_events.


  ls_event-eventid = cl_gui_simple_tree=>eventid_node_double_click.
  ls_event-appl_event = abap_true.
  APPEND ls_event TO lt_events.

  go_tree->set_registered_events(
    EXPORTING
      events                    = lt_events
    EXCEPTIONS
      cntl_error                = 1
      cntl_system_error         = 2
      illegal_event_combination = 3 ).
  ASSERT sy-subrc = 0.

* todo
*  SET HANDLER lcl_handler=>handle_node_double_click FOR go_tree.

  lt_nodes = lcl_tree_content=>build( ).

  go_tree->add_nodes(
    EXPORTING
      table_structure_name           = 'MTREESNODE'
      node_table                     = lt_nodes
    EXCEPTIONS
      failed                         = 1
      error_in_node_table            = 2
      dp_error                       = 3
      table_structure_name_not_found = 4
      OTHERS                         = 5 ).
  ASSERT sy-subrc = 0.

ENDFORM.
