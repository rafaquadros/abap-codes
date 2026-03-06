*&---------------------------------------------------------------------*
*& Include zpm_i_orders_download_d01
*&---------------------------------------------------------------------*

CLASS lcl_report DEFINITION DEFERRED.
*--------------------------------------------------------------------*
* CLASS  LCL_MEDIA
* ------------------------------------------------------------------ *
* OVERVIEW: Generic definition of a file media. It defines basic
*           methods for a media.
*--------------------------------------------------------------------*
CLASS lcl_media DEFINITION ABSTRACT FRIENDS lcl_report.
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
      close_file REDEFINITION,
      open_file  REDEFINITION.
  PROTECTED SECTION.
    METHODS  open  REDEFINITION.
ENDCLASS.                    "lcl_desktop DEFINITION

CLASS lcl_sap_api DEFINITION DEFERRED.

CLASS lcl_sap_api DEFINITION
 CREATE PRIVATE
 FRIENDS lcl_report.

  PRIVATE SECTION.
    METHODS bapi_alm_order_maintain IMPORTING it_methods              TYPE bapi_alm_order_method_t
                                              it_headers              TYPE bapi_alm_order_headers_i_t
                                              it_headers_upd          TYPE bapi_alm_order_headers_i_ut
                                              it_services             TYPE bapi_alm_order_srvdat_e_t
                                              it_services_upd         TYPE bapi_alm_order_srvdat_ut
                                              it_reforder_items       TYPE bapi_reforder_item_t
                                              it_reforder_items_upd   TYPE bapi_reforder_item_up_t
                                              it_jva                  TYPE bapi_alm_order_jva_tt
                                              it_jva_upd              TYPE bapi_alm_order_jva_up_tt
                                              it_partners             TYPE bapi_alm_order_partn_mul_t
                                              it_partners_upd         TYPE bapi_alm_order_partn_mul_ut
                                              it_operations           TYPE bapi_alm_order_operation_t
                                              it_operations_upd       TYPE bapi_alm_order_operation_ut
                                              it_components           TYPE bapi_alm_order_component_t
                                              it_components_upd       TYPE bapi_alm_order_component_ut
                                              it_settlement_rules     TYPE bapi_alm_order_srule_t
                                              it_settlement_rules_upd TYPE bapi_alm_order_srule_ut
                                    RETURNING VALUE(rt_orders)        TYPE bapi_alm_numbers_t
                                    RAISING   cx_bapi_error.
ENDCLASS.

*--------------------------------------------------------------------*
* CLASS  LCL_REPORT
* ------------------------------------------------------------------ *
* OVERVIEW: This class controls the program behavior, provided the
*           selection arguments have been set.
*--------------------------------------------------------------------*
CLASS lcl_report DEFINITION.
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

    CONSTANTS:
      BEGIN OF mc_log,
        main_object TYPE balobj_d  VALUE 'ZPM_ORD_MIGRATION',  "Log - Main object
        sub_object  TYPE balsubobj VALUE 'ZPM_ORD_MGRNT_UPLD', "Log - Orders Download
      END OF mc_log.

    DATA:
      mt_methods              TYPE bapi_alm_order_method_t,
      mt_headers              TYPE bapi_alm_order_headers_i_t,
      mt_headers_upd          TYPE bapi_alm_order_headers_i_ut,
      mt_services             TYPE bapi_alm_order_srvdat_e_t,
      mt_services_upd         TYPE bapi_alm_order_srvdat_ut,
      mt_reforder_items       TYPE bapi_reforder_item_t,
      mt_reforder_items_upd   TYPE bapi_reforder_item_up_t,
      mt_jva                  TYPE bapi_alm_order_jva_tt,
      mt_jva_upd              TYPE bapi_alm_order_jva_up_tt,
      mt_partners             TYPE bapi_alm_order_partn_mul_t,
      mt_partners_upd         TYPE bapi_alm_order_partn_mul_ut,
      mt_operations           TYPE bapi_alm_order_operation_t,
      mt_operations_upd       TYPE bapi_alm_order_operation_ut,
      mt_components           TYPE bapi_alm_order_component_t,
      mt_components_upd       TYPE bapi_alm_order_component_ut,
      mt_settlement_rules     TYPE bapi_alm_order_srule_t,
      mt_settlement_rules_upd TYPE bapi_alm_order_srule_ut.

    METHODS
      upload_xlsx.
    METHODS create_pm_orders.
    METHODS parse_xlsx.

ENDCLASS.