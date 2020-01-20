      *========================= ASC2EBC.cpy ==========================*
      * Authors: Brian D Pead
      *
      * License: MIT
      *
      * Date        Version  Description
      * ----        -------  -----------
      * 2020-01-12  0.1      First release
      *================================================================*

       01  W-ASCII-TO-EBCDIC-TABLE.
           05  FILLER-00-0F        PIC X(32)       VALUE
               '00010203372D2E2F1605250B0C0D0E0F'.
           05  FILLER-10-1F        PIC X(32)       VALUE
               '101112133C3D322618193F271C1D1E1F'.
           05  FILLER-20-2F        PIC X(32)       VALUE
               '405A7F7B5B6C507D4D5D5C4E6B604B61'.
           05  FILLER-30-3F        PIC X(32)       VALUE
               'F0F1F2F3F4F5F6F7F8F97A5E4C7E6E6F'.
           05  FILLER-40-4F        PIC X(32)       VALUE
               '7CC1C2C3C4C5C6C7C8C9D1D2D3D4D5D6'.
           05  FILLER-50-5F        PIC X(32)       VALUE
               'D7D8D9E2E3E4E5E6E7E8E9ADE0BD5F6D'.
           05  FILLER-60-6F        PIC X(32)       VALUE
               '79818283848586878889919293949596'.
           05  FILLER-70-7F        PIC X(32)       VALUE
               '979899A2A3A4A5A6A7A8A9C06AD0A107'.
           05  FILLER-80-8F        PIC X(32)       VALUE
               '68DC5142434447485253545756586367'.
           05  FILLER-90-9F        PIC X(32)       VALUE
               '719C9ECBCCCDDBDDDFECFCB0B1B23EB4'.
           05  FILLER-A0-AF        PIC X(32)       VALUE
               '4555CEDE49699A9BAB0FBAB8B7AA8A8B'.
           05  FILLER-B0-BF        PIC X(32)       VALUE
               '3C3D624F6465662021227023727374BE'.
           05  FILLER-C0-CF        PIC X(32)       VALUE
               '7677788024158C8D8E41061728299D2A'.
           05  FILLER-D0-DF        PIC X(32)       VALUE
               '2B2C090AAC4AAEAF1B3031FA1A333435'.
           05  FILLER-E0-EF        PIC X(32)       VALUE
               '36590838BC39A0BFCA3AFE3B04CFDA14'.
           05  FILLER-F0-FF        PIC X(32)       VALUE
               'E18F4675FDEBEEED90EFB3FBB9EABBFF'.
       01  FILLER REDEFINES W-ASCII-TO-EBCDIC-TABLE.
           05  FILLER                              OCCURS 256
                                                   INDEXED W-A2E-DX.
               10  W-A2E-HEX-1     PIC X(01).
               10  W-A2E-HEX-2     PIC X(01).
