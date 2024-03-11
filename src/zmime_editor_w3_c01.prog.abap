*&---------------------------------------------------------------------*
*&  Include           ZMIME_EDITOR_W3_C01
*&---------------------------------------------------------------------*

CLASS lcl_smim DEFINITION FINAL.

  PUBLIC SECTION.
    TYPES
      ty_wwwparams_tt TYPE STANDARD TABLE OF wwwparams WITH KEY relid objid name.

    CONSTANTS:
      BEGIN OF gc_param_names,
        fileext  TYPE w3_name VALUE 'fileextension',
        filesize TYPE w3_name VALUE 'filesize',
        filename TYPE w3_name VALUE 'filename',
      END OF gc_param_names.

    CLASS-METHODS:
      get_content
        IMPORTING is_smim           TYPE ty_smim
        RETURNING VALUE(rv_content) TYPE string,
      save
        IMPORTING is_smim   TYPE ty_smim
                  iv_string TYPE string,
      find_param
        IMPORTING it_params       TYPE ty_wwwparams_tt
                  iv_name         TYPE w3_name
        RETURNING VALUE(rv_value) TYPE string,
      update_param
        IMPORTING it_params        TYPE ty_wwwparams_tt
                  iv_name          TYPE w3_name
                  iv_value         TYPE string
        RETURNING VALUE(rt_params) TYPE ty_wwwparams_tt.
ENDCLASS.

CLASS lcl_tree_content DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS:
      init,
      get_by_key
        IMPORTING iv_key         TYPE tv_nodekey
        RETURNING VALUE(rs_smim) TYPE ty_smim.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_smim,
             key   TYPE tv_nodekey,
             text  TYPE string,
             relid TYPE wwwdata-relid,
             objid TYPE wwwdata-objid,
             ext   TYPE string,
           END OF ty_smim.

    CLASS-DATA mt_smim TYPE STANDARD TABLE OF ty_smim WITH DEFAULT KEY.

    CLASS-METHODS:
      build
        RETURNING VALUE(rt_nodes) TYPE ty_nodes,
      find_smim.

ENDCLASS.

CLASS lcl_tree_content IMPLEMENTATION.

  METHOD get_by_key.

    DATA ls_smim LIKE LINE OF mt_smim.

    READ TABLE mt_smim INTO ls_smim WITH KEY key = iv_key.
    ASSERT sy-subrc = 0.

    rs_smim = CORRESPONDING #( ls_smim ).

  ENDMETHOD.

  METHOD init.

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

    lt_nodes = build( ).

    go_tree->add_nodes(
      EXPORTING
        table_structure_name           = 'TREESNODE'
        node_table                     = lt_nodes
      EXCEPTIONS
        failed                         = 1
        error_in_node_table            = 2
        dp_error                       = 3
        table_structure_name_not_found = 4
        OTHERS                         = 5 ).
    ASSERT sy-subrc = 0.

  ENDMETHOD.

  METHOD build.

    DATA ls_node LIKE LINE OF rt_nodes.

    find_smim( ).

    LOOP AT mt_smim INTO DATA(ls_smim).
      ls_node-node_key = ls_smim-key.
      ls_node-text     = ls_smim-text.
      APPEND ls_node TO rt_nodes.
    ENDLOOP.

  ENDMETHOD.

  METHOD find_smim.

    TYPES: BEGIN OF ty_tadir,
             obj_name TYPE tadir-obj_name,
           END OF ty_tadir.

    DATA: lv_index    TYPE i,
          lt_tadir    TYPE STANDARD TABLE OF ty_tadir WITH KEY obj_name,
          ls_smim     LIKE gs_smim,
          lv_file     TYPE string,
          lv_ext      TYPE string,
          lt_w3params TYPE lcl_smim=>ty_wwwparams_tt.

    SELECT obj_name FROM tadir INTO TABLE @lt_tadir
      WHERE devclass = @p_devc AND obj_name IN @s_obj
      AND object = 'W3MI' ORDER BY obj_name.            "#EC CI_GENBUFF
    IF sy-subrc <> 0.
      MESSAGE |No MIME objects found| TYPE 'S'.
      RETURN.
    ENDIF.

    LOOP AT lt_tadir INTO DATA(ls_tadir).
      lv_index = sy-tabix.

      ls_smim-relid = 'MI'.
      ls_smim-objid = ls_tadir-obj_name.

      SELECT SINGLE * INTO CORRESPONDING FIELDS OF @ls_smim FROM wwwdata
        WHERE relid = @ls_smim-relid AND objid = @ls_smim-objid AND srtf2 = 0.
      IF sy-subrc <> 0.
        " does not exist
        CONTINUE.
      ENDIF.

      CALL FUNCTION 'WWWPARAMS_READ_ALL'
        EXPORTING
          type             = ls_smim-relid
          objid            = ls_smim-objid
        TABLES
          params           = lt_w3params
        EXCEPTIONS
          entry_not_exists = 1.
      IF sy-subrc = 0.
        lv_file = lcl_smim=>find_param(
          it_params = lt_w3params
          iv_name   = lcl_smim=>gc_param_names-filename ).
        lv_ext = to_upper( lcl_smim=>find_param(
          it_params = lt_w3params
          iv_name   = lcl_smim=>gc_param_names-fileext ) ).
        lv_ext = lv_ext+1.

        IF sy-subrc <> 0.
          CONTINUE.
        ELSEIF 'CSS,JS,JSON,TEXT,TXT,HTM,HTML,XML' NS lv_ext.
          CONTINUE.
        ENDIF.

      ENDIF.

      IF ls_smim-text IS INITIAL.
        ls_smim-text = |{ ls_smim-objid } ({ lv_file })|.
      ENDIF.

      APPEND VALUE #(
        key   = |KEY{ lv_index }|
        text  = ls_smim-text
        relid = ls_smim-relid
        objid = ls_smim-objid
        ext   = lv_ext ) TO mt_smim.
    ENDLOOP.

    IF mt_smim IS INITIAL.
      MESSAGE |No MIME objects found| TYPE 'S'.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_smim IMPLEMENTATION.

  METHOD get_content.

    DATA: ls_key      TYPE wwwdatatab,
          lt_w3mime   TYPE STANDARD TABLE OF w3mime,
          lt_w3html   TYPE STANDARD TABLE OF w3html,
          lt_w3params TYPE ty_wwwparams_tt,
          lv_size     TYPE i.

    ls_key = CORRESPONDING #( is_smim ).

    CALL FUNCTION 'WWWDATA_IMPORT'
      EXPORTING
        key               = ls_key
      TABLES
        mime              = lt_w3mime
        html              = lt_w3html
      EXCEPTIONS
        wrong_object_type = 1
        import_error      = 2
        OTHERS            = 3.
    IF sy-subrc <> 0.
      MESSAGE |Object { ls_key-objid } not found| TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    CALL FUNCTION 'WWWPARAMS_READ_ALL'
      EXPORTING
        type             = ls_key-relid
        objid            = ls_key-objid
      TABLES
        params           = lt_w3params
      EXCEPTIONS
        entry_not_exists = 1
        OTHERS           = 2.
    IF sy-subrc <> 0.
      MESSAGE |Object parameters { ls_key-objid } not found| TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    lv_size = find_param(
      it_params = lt_w3params
      iv_name   = gc_param_names-filesize ).

    CALL FUNCTION 'SCMS_BINARY_TO_STRING'
      EXPORTING
        input_length = lv_size
        encoding     = '4110'
      IMPORTING
        text_buffer  = rv_content
      TABLES
        binary_tab   = lt_w3mime
      EXCEPTIONS
        failed       = 1
        OTHERS       = 2.
    IF sy-subrc <> 0.
      MESSAGE |Error converting object content { ls_key-objid }| TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

  ENDMETHOD.

  METHOD save.

    DATA: lv_string   TYPE string,
          lv_xstring  TYPE xstring,
          lt_w3mime   TYPE STANDARD TABLE OF w3mime,
          lt_w3html   TYPE STANDARD TABLE OF w3html,
          lt_w3params TYPE ty_wwwparams_tt,
          lv_size     TYPE i,
          ls_key      TYPE wwwdatatab.

    ls_key = CORRESPONDING #( is_smim ).

    lv_string = iv_string.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf IN lv_string WITH cl_abap_char_utilities=>newline.

    CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
      EXPORTING
        text     = lv_string
        encoding = '4110'
      IMPORTING
        buffer   = lv_xstring
      EXCEPTIONS
        failed   = 1
        OTHERS   = 2.
    IF sy-subrc <> 0.
      MESSAGE |Error converting object content { ls_key-objid }| TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer        = lv_xstring
      IMPORTING
        output_length = lv_size
      TABLES
        binary_tab    = lt_w3mime
      EXCEPTIONS
        OTHERS        = 1.
    IF sy-subrc <> 0.
      MESSAGE |Error converting object content { ls_key-objid }| TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    CALL FUNCTION 'WWWPARAMS_READ_ALL'
      EXPORTING
        type             = ls_key-relid
        objid            = ls_key-objid
      TABLES
        params           = lt_w3params
      EXCEPTIONS
        entry_not_exists = 1
        OTHERS           = 2.
    IF sy-subrc <> 0.
      MESSAGE |Object parameters { ls_key-objid } not found| TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    lt_w3params = update_param(
      it_params = lt_w3params
      iv_name   = gc_param_names-filesize
      iv_value  = |{ lv_size }| ).

    CALL FUNCTION 'WWWPARAMS_UPDATE'
      TABLES
        params       = lt_w3params
      EXCEPTIONS
        update_error = 1
        OTHERS       = 2.
    IF sy-subrc <> 0.
      MESSAGE |Error saving object parameters { ls_key-objid }| TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    ls_key-tdate    = sy-datum.
    ls_key-ttime    = sy-uzeit.
    ls_key-chname   = sy-uname.
    ls_key-devclass = p_devc.

    CALL FUNCTION 'WWWDATA_EXPORT'
      EXPORTING
        key               = ls_key
      TABLES
        mime              = lt_w3mime
        html              = lt_w3html
      EXCEPTIONS
        wrong_object_type = 1
        export_error      = 2
        OTHERS            = 3.
    IF sy-subrc <> 0.
      MESSAGE |Error saving object content { ls_key-objid }| TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

  ENDMETHOD.

  METHOD find_param.

    READ TABLE it_params ASSIGNING FIELD-SYMBOL(<ls_param>) WITH KEY name = iv_name.
    IF sy-subrc = 0.
      rv_value = <ls_param>-value.
    ENDIF.

  ENDMETHOD.

  METHOD update_param.

    rt_params = it_params.
    READ TABLE rt_params ASSIGNING FIELD-SYMBOL(<ls_param>) WITH KEY name = iv_name.
    IF sy-subrc = 0.
      <ls_param>-value = iv_value.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_editor DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS:
      init IMPORTING iv_source_type TYPE string DEFAULT 'BSP',
      save,
      is_dirty RETURNING VALUE(rv_dirty) TYPE abap_bool,
      switch IMPORTING is_smim TYPE ty_smim,
      string_to_tab IMPORTING iv_string        TYPE string
                    RETURNING VALUE(rt_string) TYPE string_table,
      tab_to_string IMPORTING it_string        TYPE string_table
                    RETURNING VALUE(rv_string) TYPE string.

  PRIVATE SECTION.
    CLASS-DATA:
      ms_smim    TYPE ty_smim,
      mv_eof     TYPE abap_bool, "flag for newline at end-of-file
      mv_newline TYPE string. "crlf or lf at end-of-line

ENDCLASS.

CLASS lcl_editor IMPLEMENTATION.

  METHOD init.
    go_editor = NEW #(
      parent           = go_splitter->bottom_right_container
      max_number_chars = 1000
      source_type      = iv_source_type ).

    go_editor->upload_properties( EXCEPTIONS OTHERS = 4 ).
    IF sy-subrc <> 0.
      MESSAGE a215(ed).
    ENDIF.

    go_editor->set_enable( abap_false ).
    go_editor->set_readonly_mode( cl_gui_abapedit=>true ).
    go_editor->set_actual_name( p_name = 'BSP' ).
    go_editor->create_document( ).
    go_editor->set_toolbar_mode( cl_gui_abapedit=>true ).
    go_editor->set_tabbar_mode( tabbar_mode = cl_gui_abapedit=>false ).
    go_editor->set_statusbar_mode( statusbar_mode = cl_gui_abapedit=>true ).
  ENDMETHOD.

  METHOD is_dirty.

    DATA lv_status TYPE i.

    IF ms_smim IS INITIAL.
      rv_dirty = abap_false.
      RETURN.
    ENDIF.

    go_editor->get_textmodified_status( IMPORTING status = lv_status ).
    cl_gui_cfw=>flush( ).

    rv_dirty = xsdbool( lv_status <> 0 ).

  ENDMETHOD.

  METHOD save.

    DATA lt_string TYPE string_table.

    IF is_dirty( ) = abap_false.
      MESSAGE 'Nothing changed'(005) TYPE 'S'.
      RETURN.
    ENDIF.

    go_editor->get_text( IMPORTING table = lt_string ).
    cl_gui_cfw=>flush( ).
    lcl_smim=>save( is_smim   = ms_smim
                    iv_string = tab_to_string( lt_string ) ).
    MESSAGE 'Saved'(003) TYPE 'S'.
    go_editor->set_textmodified_status( ).

  ENDMETHOD.

  METHOD switch.

    DATA lv_content TYPE string.

    IF is_dirty( ) = abap_true.
      MESSAGE 'Not saved'(004) TYPE 'W'.
      RETURN.
    ENDIF.

    ms_smim = is_smim.

    CASE ms_smim-ext.
      WHEN 'XML'.
        init( 'XML' ).
      WHEN OTHERS.
        init( 'BSP' ).
    ENDCASE.

    go_editor->set_actual_name( p_name = ms_smim-objid ).
    go_editor->set_enable( abap_true ).
    go_editor->set_readonly_mode( cl_gui_abapedit=>false ).
    lv_content = lcl_smim=>get_content( ms_smim ).
    go_editor->set_text( string_to_tab( lv_content ) ).
    go_editor->set_focus( go_editor ).

  ENDMETHOD.

  METHOD string_to_tab.
    DATA lv_len TYPE i.

    FIND cl_abap_char_utilities=>cr_lf IN iv_string.
    IF sy-subrc = 0.
      SPLIT iv_string AT cl_abap_char_utilities=>cr_lf INTO TABLE rt_string.
      mv_newline = cl_abap_char_utilities=>cr_lf.
    ELSE.
      SPLIT iv_string AT cl_abap_char_utilities=>newline INTO TABLE rt_string.
      mv_newline = cl_abap_char_utilities=>newline.
    ENDIF.

    lv_len = strlen( iv_string ) - strlen( mv_newline ).
    mv_eof = boolc( iv_string+lv_len = mv_newline ).
  ENDMETHOD.

  METHOD tab_to_string.
    CONCATENATE LINES OF it_string INTO rv_string
      IN CHARACTER MODE
      SEPARATED BY mv_newline.
    IF mv_eof = abap_true.
      rv_string = rv_string && mv_newline.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_handler DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS
      double_click FOR EVENT node_double_click OF cl_gui_simple_tree
        IMPORTING node_key.

ENDCLASS.

CLASS lcl_handler IMPLEMENTATION.

  METHOD double_click.

    DATA ls_smim LIKE gs_smim.

    ls_smim = lcl_tree_content=>get_by_key( node_key ).
    lcl_editor=>switch( ls_smim ).

  ENDMETHOD.

ENDCLASS.
