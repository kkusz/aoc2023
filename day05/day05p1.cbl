         IDENTIFICATION DIVISION.
         PROGRAM-ID. DAY05P1.
            
         ENVIRONMENT DIVISION.  

         INPUT-OUTPUT SECTION. 
         FILE-CONTROL.
            SELECT INPUT-FILE ASSIGN TO INPFILE
            ORGANIZATION IS LINE SEQUENTIAL.
            
         DATA DIVISION.
         FILE SECTION. 
         FD INPUT-FILE.
         01 INPUT-RECORD.
            88 INPUT-END        VALUE LOW-VALUE.
            05 INPUT-TEXT       PIC X(300).

         WORKING-STORAGE SECTION.

         01 WS-RECORD           PIC X(200).

         01 WS-POINTER          USAGE POINTER.

         77 IX                  PIC 999.

         01 WS-PARSED-STATE     PIC 99 VALUE  0.
            88 PARSED-INIT             VALUE  0.
            88 PARSED-SEEDS            VALUE  1.
            88 PARSED-SPACE1           VALUE  2.
            88 PARSED-HEAD1            VALUE  3.
            88 PARSED-SEED2SOIL        VALUE  4.
            88 PARSED-HEAD2            VALUE  5.
            88 PARSED-SOIL2FERT        VALUE  6.
            88 PARSED-HEAD3            VALUE  7.
            88 PARSED-FERT2WATR        VALUE  8.
            88 PARSED-HEAD4            VALUE  9.
            88 PARSED-WATR2LIGH        VALUE 10.
            88 PARSED-HEAD5            VALUE 11.
            88 PARSED-LIGH2TEMP        VALUE 12.
            88 PARSED-HEAD6            VALUE 13.
            88 PARSED-TEMP2HUMD        VALUE 14.
            88 PARSED-HEAD7            VALUE 15.
            88 PARSED-HUMD2LOCA        VALUE 16.

         01 WS-SEEDS.
            05 WS-SEED          PIC 9(10) OCCURS 20
                                VALUE 0.

         01 WS-MAP-POINTER      USAGE POINTER.

         01 WS-SEED2SOIL-BUF    PIC X(3000).
         01 WS-SEED2SOIL-LEN    PIC 9(3) VALUE 0.

         01 WS-SOIL2FERT-BUF    PIC X(3000).
         01 WS-SOIL2FERT-LEN    PIC 9(3) VALUE 0.

         01 WS-FERT2WATR-BUF    PIC X(3000).
         01 WS-FERT2WATR-LEN    PIC 9(3) VALUE 0.

         01 WS-WATR2LIGH-BUF    PIC X(3000).
         01 WS-WATR2LIGH-LEN    PIC 9(3) VALUE 0.

         01 WS-LIGH2TEMP-BUF    PIC X(3000).
         01 WS-LIGH2TEMP-LEN    PIC 9(3) VALUE 0.

         01 WS-TEMP2HUMD-BUF    PIC X(3000).
         01 WS-TEMP2HUMD-LEN    PIC 9(3) VALUE 0.

         01 WS-HUMD2LOCA-BUF    PIC X(3000).
         01 WS-HUMD2LOCA-LEN    PIC 9(3) VALUE 0.

         LINKAGE SECTION. 

         01 WS-LINKED           PIC X(200).
         
         01 LN-MAP.
            05 LN-MAP-ITEM      OCCURS 100.
               10 LN-DEST       PIC 9(10).
               10 LN-SRC        PIC 9(10).
               10 LN-RANGE      PIC 9(10).

         01 LN-MAP-LEN          PIC 999.

         PROCEDURE DIVISION.
            SET WS-POINTER TO ADDRESS OF INPUT-RECORD
            SET ADDRESS OF WS-LINKED TO WS-POINTER
            
            OPEN INPUT INPUT-FILE
            READ INPUT-FILE
               AT END SET INPUT-END TO TRUE
            END-READ

            PERFORM UNTIL INPUT-END
               EVALUATE TRUE
                  WHEN PARSED-INIT
                     PERFORM PARSE-SEEDS
                  WHEN PARSED-SEEDS
                     SET PARSED-SPACE1 TO TRUE
                  WHEN PARSED-SPACE1
                     DISPLAY 'SEED TO SOIL'
                     SET ADDRESS OF LN-MAP 
                      TO ADDRESS OF WS-SEED2SOIL-BUF
                     SET ADDRESS OF LN-MAP-LEN 
                      TO ADDRESS OF WS-SEED2SOIL-LEN 
                     SET PARSED-HEAD1  TO TRUE
                  WHEN PARSED-HEAD1
                     IF INPUT-TEXT = SPACE
                        SET PARSED-SEED2SOIL TO TRUE
                     ELSE
                        PERFORM PARSE-MAP
                     END-IF
                  WHEN PARSED-SEED2SOIL 
                     DISPLAY 'SOIL TO FERTILIZER'
                     SET ADDRESS OF LN-MAP 
                      TO ADDRESS OF WS-SOIL2FERT-BUF
                     SET ADDRESS OF LN-MAP-LEN 
                      TO ADDRESS OF WS-SOIL2FERT-LEN 
                     SET PARSED-HEAD2  TO TRUE
                  WHEN PARSED-HEAD2
                     IF INPUT-TEXT = SPACE
                        SET PARSED-SOIL2FERT TO TRUE
                     ELSE
                        PERFORM PARSE-MAP
                     END-IF
                  WHEN PARSED-SOIL2FERT
                     DISPLAY 'FERTILIZER TO WATER'
                     SET ADDRESS OF LN-MAP 
                      TO ADDRESS OF WS-FERT2WATR-BUF
                     SET ADDRESS OF LN-MAP-LEN 
                      TO ADDRESS OF WS-FERT2WATR-LEN 
                     SET PARSED-HEAD3  TO TRUE
                  WHEN PARSED-HEAD3
                     IF INPUT-TEXT = SPACE
                        SET PARSED-FERT2WATR TO TRUE
                     ELSE
                        PERFORM PARSE-MAP
                     END-IF
                  WHEN PARSED-FERT2WATR
                     DISPLAY 'WATER TO LIGHT'
                     SET ADDRESS OF LN-MAP 
                      TO ADDRESS OF WS-WATR2LIGH-BUF
                     SET ADDRESS OF LN-MAP-LEN 
                      TO ADDRESS OF WS-WATR2LIGH-LEN 
                     SET PARSED-HEAD4  TO TRUE
                  WHEN PARSED-HEAD4
                     IF INPUT-TEXT = SPACE
                        SET PARSED-WATR2LIGH TO TRUE
                     ELSE
                        PERFORM PARSE-MAP
                     END-IF
                  WHEN PARSED-WATR2LIGH
                     DISPLAY 'LIGHT TO TEMPERATURE'
                     SET ADDRESS OF LN-MAP 
                      TO ADDRESS OF WS-LIGH2TEMP-BUF
                     SET ADDRESS OF LN-MAP-LEN 
                      TO ADDRESS OF WS-LIGH2TEMP-LEN 
                     SET PARSED-HEAD5  TO TRUE
                  WHEN PARSED-HEAD5
                     IF INPUT-TEXT = SPACE
                        SET PARSED-LIGH2TEMP TO TRUE
                     ELSE
                        PERFORM PARSE-MAP
                     END-IF
                  WHEN PARSED-LIGH2TEMP
                     DISPLAY 'TEMPERATURE TO HUMIDITY'
                     SET ADDRESS OF LN-MAP 
                      TO ADDRESS OF WS-TEMP2HUMD-BUF
                     SET ADDRESS OF LN-MAP-LEN 
                      TO ADDRESS OF WS-TEMP2HUMD-LEN 
                     SET PARSED-HEAD6  TO TRUE
                  WHEN PARSED-HEAD6
                     IF INPUT-TEXT = SPACE
                        SET PARSED-TEMP2HUMD TO TRUE
                     ELSE
                        PERFORM PARSE-MAP
                     END-IF
                  WHEN PARSED-TEMP2HUMD
                     DISPLAY 'HUMIDITY TO LOCATION'
                     SET ADDRESS OF LN-MAP 
                      TO ADDRESS OF WS-HUMD2LOCA-BUF
                     SET ADDRESS OF LN-MAP-LEN 
                      TO ADDRESS OF WS-HUMD2LOCA-LEN 
                     SET PARSED-HEAD7  TO TRUE
                  WHEN PARSED-HEAD7
                     IF INPUT-TEXT = SPACE
                        SET PARSED-HUMD2LOCA TO TRUE
                     ELSE
                        PERFORM PARSE-MAP
                     END-IF
                  WHEN OTHER
                     PERFORM PARSE-RECORD
               END-EVALUATE

               READ INPUT-FILE
                  AT END SET INPUT-END TO TRUE
               END-READ
            END-PERFORM

            CLOSE INPUT-FILE

            STOP RUN.

         PARSE-SEEDS SECTION.
            UNSTRING INPUT-TEXT(8:33)
               DELIMITED BY ' '
               INTO WS-SEED(1)
                   ,WS-SEED(2)
                   ,WS-SEED(3)
                   ,WS-SEED(4)
                   ,WS-SEED(5)
                   ,WS-SEED(6)
                   ,WS-SEED(7)
                   ,WS-SEED(8)
                   ,WS-SEED(9)
                   ,WS-SEED(10)
                   ,WS-SEED(11)
                   ,WS-SEED(12)
                   ,WS-SEED(13)
                   ,WS-SEED(14)
                   ,WS-SEED(15)
                   ,WS-SEED(16)
                   ,WS-SEED(17)
                   ,WS-SEED(18)
                   ,WS-SEED(19)
                   ,WS-SEED(20)
            END-UNSTRING
            DISPLAY WS-SEED(1) ' ' 
                    WS-SEED(2) ' '
                    WS-SEED(3) ' '
                    WS-SEED(4)
            SET PARSED-SEEDS TO TRUE
            .
         PARSE-RECORD SECTION.
            DISPLAY WS-LINKED(1:50)
            .

         PARSE-MAP SECTION.
            ADD 1 TO LN-MAP-LEN 
            UNSTRING INPUT-TEXT
               DELIMITED BY ' '
               INTO LN-DEST (LN-MAP-LEN)
                   ,LN-SRC  (LN-MAP-LEN)
                   ,LN-RANGE(LN-MAP-LEN)
            END-UNSTRING

            DISPLAY LN-MAP-LEN ' : '
                    LN-DEST (LN-MAP-LEN) ' , '
                   ,LN-SRC  (LN-MAP-LEN) ' , '
                   ,LN-RANGE(LN-MAP-LEN) ' , '
            .
