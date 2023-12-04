         IDENTIFICATION DIVISION.
         PROGRAM-ID. DAY03P2.
            
         ENVIRONMENT DIVISION.  
            
         DATA DIVISION.

         WORKING-STORAGE SECTION.

         01 WS-RECORD               PIC X(200).

         01 WS-SCHEMATIC-TABLE.
            05 WS-SCHEMATIC-ITEM    OCCURS 142.
               10 WS-SCHEMATIC-SPAN.
                  15 FILLER            PIC X.
                  15 WS-SCHEMATIC-STR  PIC X(140).
                  15 FILLER            PIC XX.
               10 WS-SCHEMATIC-CHARS REDEFINES WS-SCHEMATIC-SPAN.
                  15 WS-SCHEMATIC-CHAR PIC X OCCURS 143.

         01 WS-SUM                  PIC 9(10) VALUE 0.

         01 WS-NUMBER               PIC 999.
         01 WS-DIGIT                PIC 9.
         01 WS-CHAR REDEFINES WS-DIGIT PIC X.               

         77 IX                      PIC 999.
         77 IX2                     PIC 999.

         77 IX2-LEFT                PIC 999.
         77 IX2-RIGHT               PIC 999.
         77 IX-UP                   PIC 999.
         77 IX-DOWN                 PIC 999.

         01 CUR-IX.
            05 IX-CUR               PIC 999.
            05 IX2-CUR              PIC 999.

         01 CHECK-SYMBOL            PIC X.
            88 SYMBOL-PRESENT VALUE 
               '#' '$' '%' '&' '*' '+' '-' '/' '=' '@'.

         77 GIX                     PIC 9(5) VALUE 0.
         77 WS-GEAR-SIZE            PIC 9(5) VALUE 0.
         01 WS-GEAR-TABLE.
            05 WS-GEAR-ITEM OCCURS 2000 TIMES INDEXED BY IX-GEAR.
               10 WS-POSITION.
                  15 WS-GEAR-IX     PIC 999.
                  15 WS-GEAR-IX2    PIC 999.
               10 WS-GEAR-OCCUR     PIC 999 VALUE 0.
               10 WS-GEAR-N1        PIC 999 VALUE 0.
               10 WS-GEAR-N2        PIC 999 VALUE 0.

         PROCEDURE DIVISION.
            INITIALIZE WS-SCHEMATIC-TABLE
            MOVE SPACES TO WS-RECORD
            ACCEPT WS-RECORD
            MOVE 2 TO IX
            PERFORM UNTIL WS-RECORD = SPACES
               MOVE WS-RECORD TO WS-SCHEMATIC-STR(IX)

               ADD 1 TO IX

               MOVE SPACES TO WS-RECORD
               ACCEPT WS-RECORD
            END-PERFORM

            PERFORM VARYING IX FROM 2 BY 1 UNTIL IX > 141
               MOVE ZERO TO WS-NUMBER
               PERFORM VARYING IX2 FROM 2 BY 1 UNTIL IX2 > 142
                  MOVE WS-SCHEMATIC-CHAR(IX, IX2) TO WS-CHAR
                  IF WS-CHAR NUMERIC
                     COMPUTE WS-NUMBER = WS-NUMBER * 10 + WS-DIGIT
                     IF NOT SYMBOL-PRESENT
                        PERFORM CHECK-ADJACENT
                     END-IF
                  ELSE
                     IF WS-NUMBER > 0
                        IF SYMBOL-PRESENT
                           PERFORM PUT-IN-GEAR-TABLE
                        END-IF
                     END-IF
                     MOVE ZERO  TO WS-NUMBER
                     MOVE SPACE TO CHECK-SYMBOL
                  END-IF
               END-PERFORM

            END-PERFORM

            PERFORM VARYING GIX FROM 1 BY 1 UNTIL GIX > 2000
               IF WS-GEAR-OCCUR(GIX) = 2
                  COMPUTE WS-SUM = WS-SUM + WS-GEAR-N1(GIX)
                                          * WS-GEAR-N2(GIX)
               END-IF
            END-PERFORM

            DISPLAY WS-SUM
            
            STOP RUN.

          CHECK-ADJACENT SECTION.
             MOVE IX  TO IX-UP
                         IX-DOWN
             MOVE IX2 TO IX2-LEFT
                         IX2-RIGHT

             SUBTRACT 1 FROM IX-UP
             SUBTRACT 1 FROM IX2-LEFT
             ADD      1 TO   IX-DOWN
             ADD      1 TO   IX2-RIGHT
             
             MOVE IX-UP    TO IX-CUR
             MOVE IX2-LEFT TO IX2-CUR
             MOVE WS-SCHEMATIC-CHAR(IX-CUR, IX2-CUR) TO CHECK-SYMBOL 
             IF SYMBOL-PRESENT THEN GO TO CHECK-ADJACENT-EXIT.

             MOVE IX-UP    TO IX-CUR
             MOVE IX2      TO IX2-CUR             
             MOVE WS-SCHEMATIC-CHAR(IX-CUR, IX2-CUR) TO CHECK-SYMBOL 
             IF SYMBOL-PRESENT THEN GO TO CHECK-ADJACENT-EXIT.

             MOVE IX-UP     TO IX-CUR
             MOVE IX2-RIGHT TO IX2-CUR
             MOVE WS-SCHEMATIC-CHAR(IX-CUR, IX2-CUR) TO CHECK-SYMBOL 
             IF SYMBOL-PRESENT THEN GO TO CHECK-ADJACENT-EXIT.

             MOVE IX        TO IX-CUR
             MOVE IX2-RIGHT TO IX2-CUR
             MOVE WS-SCHEMATIC-CHAR(IX-CUR, IX2-CUR) TO CHECK-SYMBOL 
             IF SYMBOL-PRESENT THEN GO TO CHECK-ADJACENT-EXIT.

             MOVE IX-DOWN   TO IX-CUR
             MOVE IX2-RIGHT TO IX2-CUR
             MOVE WS-SCHEMATIC-CHAR(IX-CUR, IX2-CUR) TO CHECK-SYMBOL 
             IF SYMBOL-PRESENT THEN GO TO CHECK-ADJACENT-EXIT.

             MOVE IX-DOWN   TO IX-CUR
             MOVE IX2       TO IX2-CUR
             MOVE WS-SCHEMATIC-CHAR(IX-CUR, IX2-CUR) TO CHECK-SYMBOL 
             IF SYMBOL-PRESENT THEN GO TO CHECK-ADJACENT-EXIT.

             MOVE IX-DOWN   TO IX-CUR
             MOVE IX2-LEFT  TO IX2-CUR
             MOVE WS-SCHEMATIC-CHAR(IX-CUR, IX2-CUR) TO CHECK-SYMBOL 
             IF SYMBOL-PRESENT THEN GO TO CHECK-ADJACENT-EXIT.

             MOVE IX        TO IX-CUR
             MOVE IX2-LEFT  TO IX2-CUR
             MOVE WS-SCHEMATIC-CHAR(IX-CUR, IX2-CUR) TO CHECK-SYMBOL 
             IF SYMBOL-PRESENT THEN GO TO CHECK-ADJACENT-EXIT.

          CHECK-ADJACENT-EXIT.
             EXIT 
             .

          PUT-IN-GEAR-TABLE SECTION.
             SET IX-GEAR TO 1
             SEARCH WS-GEAR-ITEM
                AT END
                   ADD 1 TO WS-GEAR-SIZE
                   MOVE CUR-IX TO WS-POSITION(WS-GEAR-SIZE)
                   ADD 1 TO WS-GEAR-OCCUR(WS-GEAR-SIZE)
                   MOVE WS-NUMBER TO WS-GEAR-N1(WS-GEAR-SIZE)
                WHEN WS-POSITION(IX-GEAR) = CUR-IX 
                   ADD 1 TO WS-GEAR-OCCUR(IX-GEAR)
                   MOVE WS-NUMBER TO WS-GEAR-N2(IX-GEAR)
             END-SEARCH
             .
