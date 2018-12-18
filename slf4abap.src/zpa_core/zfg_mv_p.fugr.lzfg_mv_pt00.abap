*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 18.12.2018 at 11:21:03
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZTA_P...........................................*
DATA:  BEGIN OF STATUS_ZTA_P                         .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTA_P                         .
CONTROLS: TCTRL_ZTA_P
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZTA_P                         .
TABLES: ZTA_P                          .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
