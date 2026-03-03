*&---------------------------------------------------------------------*
*& Include zpm_i_orders_download_d01
*&---------------------------------------------------------------------*

*--------------------------------------------------------------------*
* CLASS  LCL_MEDIA
* ------------------------------------------------------------------ *
* OVERVIEW: Generic definition of a file media. It defines basic
*           methods for a media.
*--------------------------------------------------------------------*
CLASS lcl_media DEFINITION ABSTRACT.
  PUBLIC SECTION.
    DATA m_filename   TYPE string READ-ONLY.
    METHODS: open_file   RAISING cx_bapi_error,
*--------------------------------------------------------------------*
* OPEN_FILE
*--------------------------------------------------------------------*
*       EFFECTS.:  Retrieves the file content.
*--------------------------------------------------------------------*
      close_file  ABSTRACT RAISING cx_bapi_error,
*--------------------------------------------------------------------*
* CLOSE_FILE
*--------------------------------------------------------------------*
*       EFFECTS.:  Closes the file.
*--------------------------------------------------------------------*
      get_file_name RETURNING VALUE(r_fname) TYPE string,
*--------------------------------------------------------------------*
* GET_FILE_NAME
*--------------------------------------------------------------------*
*       EFFECTS.:  Returns the file name.
*--------------------------------------------------------------------*
      save_xlsx_file ABSTRACT RAISING cx_bapi_error,
      set_xlsx_file IMPORTING ir_xlsx_file TYPE REF TO zcl_excel,
      free.

  PROTECTED SECTION.
    DATA mr_xlsx_file TYPE REF TO zcl_excel.
    METHODS: confirm_filepathname IMPORTING i_path     TYPE string
                                            i_name     TYPE string
                                  EXPORTING r_pathname TYPE string
                                  RAISING   cx_bapi_error,
*--------------------------------------------------------------------*
* CONFIRM_FILEPATHNAME
*--------------------------------------------------------------------*
*       REQUIRES: Current path and file name must be given.
*       EFFECTS.: Returns the user option for path and file name.
*--------------------------------------------------------------------*
*  --> I_PATH     Path.
*  --> I_NAME     File name.
*  <-- R_PATHNAME Full name (Path + File name).
*--------------------------------------------------------------------*
      open  ABSTRACT RAISING cx_bapi_error.
*--------------------------------------------------------------------*
* OPEN_OTHERS
*--------------------------------------------------------------------*
*       REQUIRES: The file name, including path, must be given.
*       MODIFIES:  Creates an object of type lcl_csv or lcl_txt and
*                  stores it in o_file attribute.
*--------------------------------------------------------------------*
ENDCLASS.                    "lcl_media DEFINITION
*--------------------------------------------------------------------*
* CLASS  LCL_DESKTOP
* ------------------------------------------------------------------ *
* OVERVIEW: Desktop OS file system.
*--------------------------------------------------------------------*
CLASS lcl_windows DEFINITION INHERITING FROM lcl_media.
  PUBLIC SECTION.
    METHODS: constructor IMPORTING i_filename TYPE string OPTIONAL,
*--------------------------------------------------------------------*
* CONSTRUCTOR
*--------------------------------------------------------------------*
*       MODIFIES:  Creates an object of type lcl_media.
*--------------------------------------------------------------------*
* --> I_FILENAME  Name of the file.
*--------------------------------------------------------------------*
      close_file    REDEFINITION,
      save_xlsx_file REDEFINITION.
  PROTECTED SECTION.
    METHODS  open  REDEFINITION.
ENDCLASS.                    "lcl_desktop DEFINITION

CLASS: lcl_report DEFINITION DEFERRED,
       lcl_sap_api DEFINITION DEFERRED.

CLASS lcl_pm_order DEFINITION
 CREATE PRIVATE
 FRIENDS lcl_report.
  PUBLIC SECTION.
    DATA m_order_id TYPE aufnr.

    "========================
    " Tipos de retorno (genéricos)
    "========================
    TYPES ty_row  TYPE string_table.
    TYPES ty_rows TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.

    "========================
    " GETs por atributo (um por aba)
    "========================
    METHODS get_header_data
      EXPORTING
        et_header TYPE string_table
        et_values TYPE string_table.

    METHODS get_service_data
      EXPORTING
        et_header TYPE string_table
        et_values TYPE string_table.

    METHODS get_ref_order_item_data
      EXPORTING
        et_header TYPE string_table
        et_values TYPE string_table.

    METHODS get_jva_data
      EXPORTING
        et_header TYPE string_table
        et_values TYPE string_table.

    METHODS get_partners_data
      EXPORTING
        et_header TYPE string_table
        et_rows   TYPE ty_rows.

    METHODS get_operations_data
      EXPORTING
        et_header TYPE string_table
        et_rows   TYPE ty_rows.

    METHODS get_components_data
      EXPORTING
        et_header TYPE string_table
        et_rows   TYPE ty_rows.

    METHODS get_settlement_rules_data
      EXPORTING
        et_header TYPE string_table
        et_rows   TYPE ty_rows.

    METHODS get_costs_sum_data
      EXPORTING
        et_header TYPE string_table
        et_rows   TYPE ty_rows.

    METHODS get_costs_details_data
      EXPORTING
        et_header TYPE string_table
        et_rows   TYPE ty_rows.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF mc_log,
        sub_object TYPE balsubobj VALUE 'ZPM_ORD_MGRNT_ORD', " Log - PM Order
      END OF mc_log.

    DATA: ms_header                  TYPE bapi_alm_order_header_e,
          ms_service                 TYPE bapi_alm_order_srvdat_e,
          ms_ref_order_item          TYPE bapi_reforder_item,
          ms_join_venture_accounting TYPE bapi_alm_order_jva,
          mt_partners                TYPE bapi_alm_order_partner_t,
          mt_operations              TYPE bapi_alm_order_operation_et,
          mt_components              TYPE bapi_alm_order_component_et,
          mt_settlement_rules        TYPE bapi_alm_order_srule_et,
          mt_costs_sum               TYPE bapi_alm_order_costs_sum_et,
          mt_costs_details           TYPE bapi_alm_order_costs_detail_et,
          mr_log                     TYPE REF TO cl_cacs_bal,
          mr_sap_api                 TYPE REF TO lcl_sap_api.

    METHODS:
      constructor IMPORTING i_order_id TYPE aufnr,
      set_attributes RAISING cx_bapi_error.

    "========================
    " Helpers RTTI
    "========================
    METHODS rtti_structure_2header_values
      IMPORTING
        is_data   TYPE any
      EXPORTING
        et_header TYPE string_table
        et_values TYPE string_table.

    METHODS rtti_table_to_header_rows
      IMPORTING
        it_data   TYPE ANY TABLE
      EXPORTING
        et_header TYPE string_table
        et_rows   TYPE ty_rows.

ENDCLASS.

CLASS lcl_sap_api DEFINITION
 CREATE PRIVATE
 FRIENDS lcl_pm_order.

  PRIVATE SECTION.
    METHODS bapi_alm_order_get_detail IMPORTING i_order_id                 TYPE aufnr
                                      EXPORTING es_header                  TYPE bapi_alm_order_header_e
                                                es_service                 TYPE bapi_alm_order_srvdat_e
                                                es_ref_order_item          TYPE bapi_reforder_item
                                                es_join_venture_accounting TYPE bapi_alm_order_jva
                                                et_partners                TYPE bapi_alm_order_partner_t
                                                et_operations              TYPE bapi_alm_order_operation_et
                                                et_components              TYPE bapi_alm_order_component_et
                                                et_settlement_rules        TYPE bapi_alm_order_srule_et
                                                et_costs_sum               TYPE bapi_alm_order_costs_sum_et
                                                et_costs_details           TYPE bapi_alm_order_costs_detail_et
                                      RAISING   cx_bapi_error.
ENDCLASS.

*--------------------------------------------------------------------*
* CLASS  LCL_REPORT
* ------------------------------------------------------------------ *
* OVERVIEW: This class controls the program behavior, provided the
*           selection arguments have been set.
*--------------------------------------------------------------------*
CLASS lcl_report DEFINITION FRIENDS lcl_pm_order.
  PUBLIC SECTION.
    DATA: mr_media TYPE REF TO lcl_media,
          mr_log   TYPE REF TO cl_cacs_bal,
          m_path   TYPE string.
    METHODS:
      constructor,
      get_field_value IMPORTING i_fieldname     TYPE dynfnam
                      RETURNING VALUE(r_fvalue) TYPE dynfieldvalue,
*--------------------------------------------------------------------*
* GET_FIELD_VALUE
*--------------------------------------------------------------------*
*       REQUIRES:  The field name must be informed.
*       EFFECTS.:  Returns the values of a field, before PAI is
*                  executed.
*--------------------------------------------------------------------*
* --> I_FILENAME  Name of the file.
* <-- R_FVALUE    Field value.
*--------------------------------------------------------------------*
      run.
  PRIVATE SECTION.
    TYPES: BEGIN OF mys_order,
             instance TYPE REF TO lcl_pm_order,
           END OF myS_ORDER,
           myt_orders TYPE STANDARD TABLE OF mys_order WITH EMPTY KEY.

    CONSTANTS:
      BEGIN OF mc_log,
        main_object TYPE balobj_d  VALUE 'ZPM_ORD_MIGRATION',  "Log - Main object
        sub_object  TYPE balsubobj VALUE 'ZPM_ORD_MGRNT_DWLD', "Log - Orders Download
      END OF mc_log.

    DATA
        mt_orders TYPE myt_orders.
    METHODS:
      get_orders,
      convert_to_xlsx RAISING zcx_excel,
      download_xlsx,
      set_worksheets
        IMPORTING
                  ir_excel TYPE REF TO zcl_excel
        RAISING   zcx_excel,

      " Helpers p/ reduzir duplicação
      write_header_if_needed
        IMPORTING
          ir_ws          TYPE REF TO zcl_excel_worksheet
          it_header      TYPE string_table
        CHANGING
          cv_hdr_written TYPE abap_bool,

      write_rows
        IMPORTING
          ir_ws       TYPE REF TO zcl_excel_worksheet
          it_rows     TYPE lcl_pm_order=>ty_rows
        CHANGING
          cv_next_row TYPE i.
ENDCLASS.