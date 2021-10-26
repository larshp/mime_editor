*&---------------------------------------------------------------------*
*&  Include           ZMIME_EDITOR_SMIM
*&---------------------------------------------------------------------*

CLASS lcl_smim DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS:
      get_content
        IMPORTING is_smim           TYPE ty_smim
        RETURNING VALUE(rv_content) TYPE string,
      get_url
        IMPORTING is_smim       TYPE ty_smim
        RETURNING VALUE(rv_url) TYPE string,
      save
        IMPORTING is_smim   TYPE ty_smim
                  iv_string TYPE string.

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
             key      TYPE tv_nodekey,
             text     TYPE string,
             lo_class TYPE smimloio-lo_class,
             loio_id  TYPE smimloio-loio_id,
           END OF ty_smim.

    CLASS-DATA: mt_smim TYPE STANDARD TABLE OF ty_smim WITH DEFAULT KEY.

    CLASS-METHODS:
      build
        RETURNING VALUE(rt_nodes) TYPE ty_nodes,
      find_smim.

ENDCLASS.

CLASS lcl_tree_content IMPLEMENTATION.

  METHOD get_by_key.

    DATA: ls_smim LIKE LINE OF mt_smim.

    READ TABLE mt_smim INTO ls_smim WITH KEY key = iv_key.
    ASSERT sy-subrc = 0.

    MOVE-CORRESPONDING ls_smim TO rs_smim.

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
        table_structure_name           = 'MTREESNODE'
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

    DATA: ls_node LIKE LINE OF rt_nodes.

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

    DATA: lv_index TYPE i,
          lt_tadir TYPE STANDARD TABLE OF ty_tadir WITH DEFAULT KEY,
          ls_smim  LIKE gs_smim.


    SELECT obj_name FROM tadir INTO TABLE @lt_tadir
      WHERE devclass = @p_devc
      AND object = 'SMIM'.                              "#EC CI_GENBUFF
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    LOOP AT lt_tadir INTO DATA(ls_tadir).
      lv_index = sy-tabix.

      SELECT SINGLE * FROM smimloio
        INTO CORRESPONDING FIELDS OF @ls_smim
        WHERE loio_id = @ls_tadir-obj_name.             "#EC CI_GENBUFF
      IF sy-subrc <> 0 OR ls_smim-lo_class = wbmr_c_skwf_folder_class.
* ignore folders
        CONTINUE.
      ENDIF.

      DATA(lv_url) = lcl_smim=>get_url( ls_smim ).

      IF lv_url CP '*.png'.
        CONTINUE.
      ENDIF.

      APPEND VALUE #(
        key      = |KEY{ lv_index }|
        text     = lv_url
        lo_class = ls_smim-lo_class
        loio_id  = ls_smim-loio_id ) TO mt_smim.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_smim IMPLEMENTATION.

  METHOD get_content.

    DATA: li_api     TYPE REF TO if_mr_api,
          lv_content TYPE xstring,
          lv_url     TYPE string,
          lv_len     TYPE i,
          lo_obj     TYPE REF TO cl_abap_conv_in_ce.


    lv_url = get_url( is_smim ).

    li_api = cl_mime_repository_api=>if_mr_api~get_api( ).

    li_api->get(
      EXPORTING
        i_url              = lv_url
      IMPORTING
        e_content          = lv_content
      EXCEPTIONS
        parameter_missing  = 1
        error_occured      = 2
        not_found          = 3
        permission_failure = 4
        OTHERS             = 5 ).
    IF sy-subrc <> 0.
      BREAK-POINT.
    ENDIF.

    TRY.
        lo_obj = cl_abap_conv_in_ce=>create(
            input    = lv_content
            encoding = 'UTF-8' ).
        lv_len = xstrlen( lv_content ).

        lo_obj->read( EXPORTING n    = lv_len
                      IMPORTING data = rv_content ).
      CATCH cx_parameter_invalid_range
            cx_sy_codepage_converter_init
            cx_sy_conversion_codepage
            cx_parameter_invalid_type.
        BREAK-POINT.
    ENDTRY.

  ENDMETHOD.

  METHOD get_url.

    DATA: lv_url TYPE skwf_url,
          ls_io  TYPE skwf_io.


    ls_io-objtype = skwfc_obtype_loio.
    ls_io-class = is_smim-lo_class.
    ls_io-objid = is_smim-loio_id.

    CALL FUNCTION 'SKWF_NMSPC_IO_ADDRESS_GET'
      EXPORTING
        io  = ls_io
      IMPORTING
        url = lv_url.

    rv_url = lv_url.

  ENDMETHOD.

  METHOD save.

    DATA: lv_content  TYPE xstring,
          lv_lines    TYPE i,
          lv_filename TYPE skwf_filnm,
          li_api      TYPE REF TO if_mr_api,
          lv_url      TYPE string,
          ls_skwf_io  TYPE skwf_io,
          lt_strings  TYPE TABLE OF string,
          lo_obj      TYPE REF TO cl_abap_conv_out_ce.


    TRY.
        lo_obj = cl_abap_conv_out_ce=>create( encoding = 'UTF-8' ).
        lo_obj->convert( EXPORTING data = iv_string
                         IMPORTING buffer = lv_content ).
      CATCH cx_parameter_invalid_range
            cx_sy_codepage_converter_init
            cx_sy_conversion_codepage
            cx_parameter_invalid_type.
        BREAK-POINT.
    ENDTRY.

    lv_url = get_url( is_smim ).

    SPLIT lv_url AT '/' INTO TABLE lt_strings.
    lv_lines = lines( lt_strings ).
    ASSERT lv_lines > 0.
    READ TABLE lt_strings INDEX lv_lines INTO lv_filename.
    ASSERT sy-subrc = 0.

    ls_skwf_io-objid = gs_smim-loio_id.

    cl_wb_mime_repository=>determine_io_class(
      EXPORTING
        filename = lv_filename
      IMPORTING
        io_class = ls_skwf_io-class ).
    CONCATENATE ls_skwf_io-class '_L' INTO ls_skwf_io-class.

    li_api = cl_mime_repository_api=>if_mr_api~get_api( ).
    li_api->put(
      EXPORTING
        i_url                   = lv_url
        i_content               = lv_content
        i_new_loio              = ls_skwf_io
      EXCEPTIONS
        parameter_missing       = 1
        error_occured           = 2
        cancelled               = 3
        permission_failure      = 4
        data_inconsistency      = 5
        new_loio_already_exists = 6
        is_folder               = 7
        OTHERS                  = 8 ).
    IF sy-subrc <> 0.
      BREAK-POINT.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_editor DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS:
      save,
      is_dirty RETURNING VALUE(rv_dirty) TYPE abap_bool,
      switch IMPORTING is_smim TYPE ty_smim.

  PRIVATE SECTION.
    CLASS-DATA:
      ms_smim TYPE ty_smim.

ENDCLASS.

CLASS lcl_editor IMPLEMENTATION.

  METHOD is_dirty.

    DATA: lv_status TYPE i.


    IF ms_smim IS INITIAL.
      rv_dirty = abap_false.
      RETURN.
    ENDIF.

    go_editor->get_textmodified_status( IMPORTING status = lv_status ).
    cl_gui_cfw=>flush( ).

    rv_dirty = xsdbool( NOT lv_status = 0 ).

  ENDMETHOD.

  METHOD save.

    DATA: lv_string TYPE string.


    IF is_dirty( ) = abap_false.
      MESSAGE 'Nothing changed'(005) TYPE 'S'.
      RETURN.
    ENDIF.

    go_editor->get_textstream( IMPORTING text = lv_string ).
    cl_gui_cfw=>flush( ).
    lcl_smim=>save( is_smim   = ms_smim
                    iv_string = lv_string ).
    MESSAGE 'Saved'(003) TYPE 'S'.
    go_editor->set_textmodified_status( 0 ).

  ENDMETHOD.

  METHOD switch.

    DATA: lv_content TYPE string.


    IF is_dirty( ) = abap_true.
      MESSAGE 'Not saved'(004) TYPE 'W'.
      RETURN.
    ENDIF.

    ms_smim = is_smim.

    go_editor->set_enable( abap_true ).
    go_editor->set_readonly_mode( 0 ).
    lv_content = lcl_smim=>get_content( ms_smim ).
    go_editor->set_textstream( lv_content ).
    go_editor->set_focus( go_editor ).

  ENDMETHOD.

ENDCLASS.

CLASS lcl_handler DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS:
      double_click FOR EVENT node_double_click OF cl_gui_simple_tree
        IMPORTING node_key.

ENDCLASS.

CLASS lcl_handler IMPLEMENTATION.

  METHOD double_click.

    DATA: ls_smim LIKE gs_smim.

    ls_smim = lcl_tree_content=>get_by_key( node_key ).
    lcl_editor=>switch( ls_smim ).

  ENDMETHOD.

ENDCLASS.
