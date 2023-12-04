         IDENTIFICATION DIVISION.
         PROGRAM-ID. DAY03P1.
            
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

         01 CHECK-SYMBOL            PIC X.
            88 SYMBOL-PRESENT VALUE 
               '#' '$' '%' '&' '*' '+' '-' '/' '=' '@'.

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
                           ADD WS-NUMBER TO WS-SUM
                        END-IF
                     END-IF
                     MOVE ZERO  TO WS-NUMBER
                     MOVE SPACE TO CHECK-SYMBOL
                  END-IF
               END-PERFORM

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
             
             MOVE WS-SCHEMATIC-CHAR(IX-UP, IX2-LEFT) TO CHECK-SYMBOL 
             IF SYMBOL-PRESENT THEN GO TO CHECK-ADJACENT-EXIT.
             
             MOVE WS-SCHEMATIC-CHAR(IX-UP, IX2) TO CHECK-SYMBOL 
             IF SYMBOL-PRESENT THEN GO TO CHECK-ADJACENT-EXIT.

             MOVE WS-SCHEMATIC-CHAR(IX-UP, IX2-RIGHT) TO CHECK-SYMBOL 
             IF SYMBOL-PRESENT THEN GO TO CHECK-ADJACENT-EXIT.

             MOVE WS-SCHEMATIC-CHAR(IX, IX2-RIGHT) TO CHECK-SYMBOL 
             IF SYMBOL-PRESENT THEN GO TO CHECK-ADJACENT-EXIT.

             MOVE WS-SCHEMATIC-CHAR(IX-DOWN, IX2-RIGHT) TO CHECK-SYMBOL 
             IF SYMBOL-PRESENT THEN GO TO CHECK-ADJACENT-EXIT.

             MOVE WS-SCHEMATIC-CHAR(IX-DOWN, IX2) TO CHECK-SYMBOL 
             IF SYMBOL-PRESENT THEN GO TO CHECK-ADJACENT-EXIT.

             MOVE WS-SCHEMATIC-CHAR(IX-DOWN, IX2-LEFT) TO CHECK-SYMBOL 
             IF SYMBOL-PRESENT THEN GO TO CHECK-ADJACENT-EXIT.

             MOVE WS-SCHEMATIC-CHAR(IX, IX2-LEFT) TO CHECK-SYMBOL 
             IF SYMBOL-PRESENT THEN GO TO CHECK-ADJACENT-EXIT.

          CHECK-ADJACENT-EXIT.
             EXIT 
             .

