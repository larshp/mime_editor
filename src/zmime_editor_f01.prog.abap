*----------------------------------------------------------------------*
***INCLUDE ZMIME_EDITOR_F01.
*----------------------------------------------------------------------*

FORM run.
  CALL SCREEN 2000.
ENDFORM.

FORM back.

  IF lcl_editor=>is_dirty( ) = abap_false.
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
    SET HANDLER lcl_handler=>double_click FOR go_tree.
    lcl_tree_content=>init( ).

    CREATE OBJECT go_editor
      EXPORTING
        parent = go_splitter->bottom_right_container.
    go_editor->set_font_fixed( ).
    go_editor->set_enable( abap_false ).
    go_editor->set_readonly_mode( 1 ).
  ENDIF.

ENDFORM.
