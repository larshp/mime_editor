*&---------------------------------------------------------------------*
*&  Include           ZMIME_EDITOR_SMIM
*&---------------------------------------------------------------------*

CLASS lcl_smim DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS:
      get_content
        RETURNING VALUE(rv_content) TYPE string,
      get_url
        IMPORTING io_lo_class   TYPE smimloio-lo_class
                  io_loio_id    TYPE smimloio-loio_id
        RETURNING VALUE(rv_url) TYPE string,
      save
        IMPORTING iv_string TYPE string.

ENDCLASS.

CLASS lcl_tree_content DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS: build
      RETURNING VALUE(rt_nodes) TYPE ty_nodes.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_smim,
             key      TYPE tv_nodekey,
             text     TYPE string,
             lo_class TYPE smimloio-lo_class,
             loio_id  TYPE smimloio-loio_id,
           END OF ty_smim.

    CLASS-DATA: mt_smim TYPE STANDARD TABLE OF ty_smim WITH DEFAULT KEY.

    CLASS-METHODS: find_smim.

ENDCLASS.

CLASS lcl_tree_content IMPLEMENTATION.

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

    DATA: lv_index    TYPE i,
          ls_smimloio TYPE smimloio.


    SELECT * FROM tadir INTO TABLE @DATA(lt_tadir)
      WHERE devclass = @p_devc
      AND object = 'SMIM'.

    LOOP AT lt_tadir INTO DATA(ls_tadir).
      lv_index = sy-tabix.

      SELECT SINGLE * FROM smimloio INTO ls_smimloio
        WHERE loio_id = ls_tadir-obj_name.              "#EC CI_GENBUFF
      IF sy-subrc <> 0 OR ls_smimloio-lo_class = wbmr_c_skwf_folder_class.
* ignore folders
        CONTINUE.
      ENDIF.

      DATA(lv_url) = lcl_smim=>get_url(
        io_lo_class = ls_smimloio-lo_class
        io_loio_id  = ls_smimloio-loio_id ).

      IF lv_url CP '*.svg' OR lv_url CP '*.png'.
        CONTINUE.
      ENDIF.

      APPEND VALUE #(
        key      = |KEY{ lv_index }|
        text     = lv_url
        lo_class = ls_smimloio-lo_class
        loio_id  = ls_smimloio-loio_id ) TO mt_smim.
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


    lv_url = lcl_smim=>get_url(
      io_lo_class = gs_smimloio-lo_class
      io_loio_id  = gs_smimloio-loio_id ).

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
    ls_io-class = io_lo_class.
    ls_io-objid = io_loio_id.

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

    lv_url = lcl_smim=>get_url(
      io_lo_class = gs_smimloio-lo_class
      io_loio_id = gs_smimloio-loio_id ).

    SPLIT lv_url AT '/' INTO TABLE lt_strings.
    lv_lines = lines( lt_strings ).
    ASSERT lv_lines > 0.
    READ TABLE lt_strings INDEX lv_lines INTO lv_filename.
    ASSERT sy-subrc = 0.

    ls_skwf_io-objid = p_loio.

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
