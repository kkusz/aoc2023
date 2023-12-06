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

         77 IX                  PIC 99.
         77 IX2                 PIC 9(10).
         77 IX3                 PIC 99.

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

         01 WS-BUFFERS.
           05 WS-SEED2SOIL-BUF    PIC X(3000).
           05 WS-SEED2SOIL-LEN    PIC 9(3) VALUE 0.

           05 WS-SOIL2FERT-BUF    PIC X(3000).
           05 WS-SOIL2FERT-LEN    PIC 9(3) VALUE 0.

           05 WS-FERT2WATR-BUF    PIC X(3000).
           05 WS-FERT2WATR-LEN    PIC 9(3) VALUE 0.

           05 WS-WATR2LIGH-BUF    PIC X(3000).
           05 WS-WATR2LIGH-LEN    PIC 9(3) VALUE 0.

           05 WS-LIGH2TEMP-BUF    PIC X(3000).
           05 WS-LIGH2TEMP-LEN    PIC 9(3) VALUE 0.

           05 WS-TEMP2HUMD-BUF    PIC X(3000).
           05 WS-TEMP2HUMD-LEN    PIC 9(3) VALUE 0.

           05 WS-HUMD2LOCA-BUF    PIC X(3000).
           05 WS-HUMD2LOCA-LEN    PIC 9(3) VALUE 0.

         01 WS-BUFFERS-TAB REDEFINES WS-BUFFERS.
           05 WS-MAP-ITEM          OCCURS 7.
              10 WS-MAP-ENTRY      OCCURS 100.
                 15 WS-ENTRY-DEST  PIC 9(10).
                 15 WS-ENTRY-SRC   PIC 9(10).
                 15 WS-ENTRY-RANGE PIC 9(10).
              10 WS-MAP-LEN        PIC 999.

         01 WS-SOURCE              PIC 9(10).
         01 WS-DESTINATION         PIC 9(10).

         01 WS-RANGE1              PIC 9(10).
         01 WS-RANGE2              PIC 9(10).

         01 WS-LOWEST-LOCA         PIC 9(10) VALUE 9999999999.

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
                     CONTINUE
               END-EVALUATE

               READ INPUT-FILE
                  AT END SET INPUT-END TO TRUE
               END-READ
            END-PERFORM

            CLOSE INPUT-FILE

            PERFORM VARYING IX3 FROM 1 BY 1 UNTIL WS-SEED(IX3) = 0
               MOVE WS-SEED(IX3) TO WS-SOURCE
               PERFORM TRAVERSE-MAP 
               IF WS-DESTINATION < WS-LOWEST-LOCA 
                  MOVE WS-DESTINATION TO WS-LOWEST-LOCA
               END-IF
            END-PERFORM

            DISPLAY WS-LOWEST-LOCA

            STOP RUN.

         PARSE-SEEDS SECTION.
            UNSTRING INPUT-TEXT(8:210)
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
            SET PARSED-SEEDS TO TRUE
            .

         PARSE-MAP SECTION.
            ADD 1 TO LN-MAP-LEN 
            UNSTRING INPUT-TEXT
               DELIMITED BY ' '
               INTO LN-DEST (LN-MAP-LEN)
                   ,LN-SRC  (LN-MAP-LEN)
                   ,LN-RANGE(LN-MAP-LEN)
            END-UNSTRING
            .

         TRAVERSE-MAP SECTION.
              MOVE 1 TO IX
              MOVE WS-SOURCE TO WS-DESTINATION

              PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 7
              PERFORM VARYING IX2 FROM 1 BY 1
                 UNTIL IX2 > WS-MAP-LEN(IX)
                 MOVE WS-ENTRY-SRC(IX IX2) TO WS-RANGE1
                                              WS-RANGE2
                 COMPUTE WS-RANGE2 = WS-RANGE2
                                   + WS-ENTRY-RANGE(IX IX2)
                                   - 1
                 IF  WS-SOURCE >= WS-RANGE1
                 AND WS-SOURCE <= WS-RANGE2
                   COMPUTE WS-DESTINATION = WS-SOURCE 
                                          + WS-ENTRY-DEST(IX IX2)
                                          - WS-ENTRY-SRC (IX IX2)
                 END-IF
              END-PERFORM
              MOVE WS-DESTINATION TO WS-SOURCE
              END-PERFORM
            .
