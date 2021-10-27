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

  IF go_container IS NOT BOUND.
    go_container = NEW #( container_name = 'CUSTOM_2000' ).

    go_splitter = NEW #(
      parent      = go_container
      orientation = 1 ).
    go_splitter->set_sash_position( 30 ).

    go_tree = NEW #(
      parent              = go_splitter->top_left_container
      node_selection_mode = cl_gui_simple_tree=>node_sel_mode_single ).
    SET HANDLER lcl_handler=>double_click FOR go_tree.
    lcl_tree_content=>init( ).

    go_editor = NEW #( parent = go_splitter->bottom_right_container ).
    go_editor->set_font_fixed( ).
    go_editor->set_enable( abap_false ).
    go_editor->set_readonly_mode( 1 ).
  ENDIF.

ENDFORM.
