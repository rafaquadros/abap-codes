CLASS lcl_xml_popup DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES if_salv_csqt_content_manager.

    METHODS:
      constructor IMPORTING iv_xml TYPE string,
      show.
  PRIVATE SECTION.
    DATA:
      go_container TYPE REF TO cl_gui_container,
      go_view      TYPE REF TO cl_gui_html_viewer,
      gv_xml       TYPE string.
    METHODS
      display
        IMPORTING
          !io_container TYPE REF TO cl_gui_container.
ENDCLASS.

CLASS lcl_xml_popup IMPLEMENTATION.
  METHOD if_salv_csqt_content_manager~fill_container_content.

    me->go_container = r_container.

    me->display( io_container = me->go_container ).
  ENDMETHOD.

  METHOD constructor.

    me->gv_xml = iv_xml.
  ENDMETHOD.

  METHOD display.
    DATA:
      lv_url  TYPE char255,
      lv_size TYPE sytabix,
      lt_xml  TYPE swxmlcont.

    DATA(lo_xml) = NEW cl_xml_document( ).
    lo_xml->parse_string( stream = me->gv_xml ).

    me->go_view = NEW cl_gui_html_viewer(
      parent                   = me->go_container
    ).

    lo_xml->render_2_table(
*      EXPORTING
*        pretty_print = 'X'              " Format Output
      IMPORTING
*        retcode      =                  " Return code
        table        = lt_xml                 " Table (STREAM)
        size         = lv_size                " File Size (Number of Characters)
    ).

    go_view->load_data(
      EXPORTING
        subtype                = 'xml'             " Subtype of a MIME Object
        size                   = lv_size                " Length of Data
      IMPORTING
        assigned_url           = lv_url                 " URL
      CHANGING
        data_table             = lt_xml                 " data table
      EXCEPTIONS
        dp_invalid_parameter   = 1                " invalid parameter in a DP call
        dp_error_general       = 2                " gerneral error in a DP call
        cntl_error             = 3                " error
        html_syntax_notcorrect = 4                " HTML data is invalid and check all the tags' syntax
        OTHERS                 = 5
    ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO DATA(lv_message).
      RETURN.
    ENDIF.

    go_view->show_data(
      EXPORTING
        url                    = lv_url                 " URL
      EXCEPTIONS
        cntl_error             = 1                " Error in CFW Call
        cnht_error_not_allowed = 2                " Navigation outside R/3 is not allowed
        cnht_error_parameter   = 3                " Incorrect parameters
        dp_error_general       = 4                " Error in DP FM call
        OTHERS                 = 5
    ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO lv_message.
      RETURN.
    ENDIF.

    cl_gui_cfw=>flush(
      EXCEPTIONS
        cntl_system_error = 1                " cntl_system_error
        cntl_error        = 2                " cntl_error
        OTHERS            = 3
    ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO lv_message.
    ENDIF.
  ENDMETHOD.

  METHOD show.

    CALL FUNCTION 'ZR_SALV_POPUP_CONTAINER'
      EXPORTING
        r_content_manager = me                " Parameter display
        title             = 'XML Detail'(t04).
  ENDMETHOD.
ENDCLASS.
