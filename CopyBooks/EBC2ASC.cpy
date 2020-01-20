      *========================= EBC2ASC.cpy ==========================*
      * Authors: Brian D Pead
      *
      * License: MIT
      *
      * Date        Version  Description
      * ----        -------  -----------
      * 2020-01-12  0.1      First release
      *================================================================*

       01  W-EBCDIC-TO-ASCII-TABLE.
           05  FILLER-00-0F        PIC X(32)       VALUE
               '00010203EC09CA7FE2D2D30B0C0D0E0F'.
           05  FILLER-10-1F        PIC X(32)       VALUE
               '10111213EFC508CB1819DCD81C1D1E1F'.
           05  FILLER-20-2F        PIC X(32)       VALUE
               'B7B8B9BBC40A171BCCCDCFD0D1050607'.
           05  FILLER-30-3F        PIC X(32)       VALUE
               'D9DA16DDDEDFE004E3E5E9EB14159E1A'.
           05  FILLER-40-4F        PIC X(32)       VALUE
               '20C9838485A0F28687A4D52E3C282BB3'.
           05  FILLER-50-5F        PIC X(32)       VALUE
               '268288898AA18C8B8DE121242A293B5E'.
           05  FILLER-60-6F        PIC X(32)       VALUE
               '2D2FB28EB4B5B68F80A57C2C255F3E3F'.
           05  FILLER-70-7F        PIC X(32)       VALUE
               'BA90BCBDBEF3C0C1C2603A2340273D22'.
           05  FILLER-80-8F        PIC X(32)       VALUE
               'C3616263646566676869AEAFC6C7C8F1'.
           05  FILLER-90-9F        PIC X(32)       VALUE
               'F86A6B6C6D6E6F707172A6A791CE92A9'.
           05  FILLER-A0-AF        PIC X(32)       VALUE
               'E67E737475767778797AADA8D45BD6D7'.
           05  FILLER-B0-BF        PIC X(32)       VALUE
               '9B9C9DFA9FB1B0ACABFCAAFEE45DBFE7'.
           05  FILLER-C0-CF        PIC X(32)       VALUE
               '7B414243444546474849E8939495A2ED'.
           05  FILLER-D0-DF        PIC X(32)       VALUE
               '7D4A4B4C4D4E4F505152EE968197A398'.
           05  FILLER-E0-EF        PIC X(32)       VALUE
               '5CF0535455565758595AFDF599F7F6F9'.
           05  FILLER-F0-FF        PIC X(32)       VALUE
               '30313233343536373839DBFB9AF4EAFF'.
       01  FILLER REDEFINES W-EBCDIC-TO-ASCII-TABLE.
           05  FILLER                              OCCURS 256
                                                   INDEXED W-E2A-DX.
               10  W-E2A-HEX-1     PIC X(01).
               10  W-E2A-HEX-2     PIC X(01).
