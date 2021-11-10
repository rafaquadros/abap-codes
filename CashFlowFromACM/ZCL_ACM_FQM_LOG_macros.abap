*"* use this source file for any macro definitions you need
*"* in the implementation part of the class
DEFINE m_raise_bapi_error.
  RAISE EXCEPTION TYPE cx_bapi_error
    EXPORTING
      status = VALUE #( ( id = sy-msgid
                          type = sy-msgty
                          number = sy-msgno
                          message = l_message
                          message_v1 = sy-msgv1
                          message_v2 = sy-msgv2
                          message_v3 = sy-msgv3
                          message_v4 = sy-msgv4 ) ).
end-OF-DEFINITION.