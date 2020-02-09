      *========================= NAMEADDR.cpy =========================*
      * Authors: Brian D Pead
      *
      * License: MIT
      *
      * Date        Version  Description
      * ----        -------  -----------
      * 2020-02-08  1.0      First release
      *================================================================*

      *01  NAMEADDR-REC.
           05  NA-TAXID            PIC X(12).
           05  NA-NAME.
               10  NA-PREFIX       PIC X(10).
               10  NA-FIRST-NAME   PIC X(25). 
               10  NA-LAST-NAME    PIC X(35). 
               10  NA-SUFFIX       PIC X(10).
           05  NA-ADDRESS.
               10  NA-STREET       PIC X(35).
               10  NA-CITY         PIC X(25).
               10  NA-STATE        PIC X(10).
               10  NA-ZIP-CODE     PIC X(10).