         IDENTIFICATION DIVISION.
         PROGRAM-ID. DAY01P2.
            
         ENVIRONMENT DIVISION.  
            
         DATA DIVISION.

         WORKING-STORAGE SECTION.

         01 WS-RECORD           PIC X(100).

         01 WS-SUM              PIC 9(10) VALUE 0.

         01 WS-DIGIT1           PIC 9.
         01 WS-DIGIT2           PIC 9.

         01 WS-DIGIT            PIC 9.

         01 SW-FOUND            PIC 9.
            88 FOUND-DIGIT               VALUE 1.
            88 NOT-FOUND-DIGIT           VALUE 0.

         01 IX                  PIC 999.

         PROCEDURE DIVISION.
            MOVE SPACES TO WS-RECORD
            ACCEPT WS-RECORD
            PERFORM UNTIL WS-RECORD = SPACES
               MOVE 1 TO IX
               SET NOT-FOUND-DIGIT TO TRUE
               PERFORM UNTIL IX > 100 OR FOUND-DIGIT
                  PERFORM SCAN-DIGIT
                  ADD 1 TO IX
               END-PERFORM
               MOVE WS-DIGIT TO WS-DIGIT1 
               
               MOVE 100 TO IX
               SET NOT-FOUND-DIGIT TO TRUE
               PERFORM UNTIL IX < 1 OR FOUND-DIGIT
                  PERFORM SCAN-DIGIT
                  SUBTRACT 1 FROM IX
               END-PERFORM
               MOVE WS-DIGIT TO WS-DIGIT2 
               COMPUTE WS-SUM = WS-SUM + WS-DIGIT1 * 10 + WS-DIGIT2

               MOVE SPACES TO WS-RECORD
               ACCEPT WS-RECORD
            END-PERFORM

            DISPLAY WS-SUM 

            STOP RUN.

         SCAN-DIGIT SECTION.
            IF WS-RECORD(IX:1) IS NUMERIC
               MOVE WS-RECORD(IX:1) TO WS-DIGIT
               SET FOUND-DIGIT TO TRUE 
            END-IF

            IF WS-RECORD(IX:3) = 'one'
               MOVE 1 TO WS-DIGIT
               SET FOUND-DIGIT TO TRUE 
            END-IF

            IF WS-RECORD(IX:3) = 'two'
               MOVE 2 TO WS-DIGIT
               SET FOUND-DIGIT TO TRUE 
            END-IF
            
            IF WS-RECORD(IX:5) = 'three'
               MOVE 3 TO WS-DIGIT
               SET FOUND-DIGIT TO TRUE 
            END-IF

            IF WS-RECORD(IX:4) = 'four'
               MOVE 4 TO WS-DIGIT
               SET FOUND-DIGIT TO TRUE 
            END-IF

            IF WS-RECORD(IX:4) = 'five'
               MOVE 5 TO WS-DIGIT
               SET FOUND-DIGIT TO TRUE 
            END-IF

            IF WS-RECORD(IX:3) = 'six'
               MOVE 6 TO WS-DIGIT
               SET FOUND-DIGIT TO TRUE 
            END-IF

            IF WS-RECORD(IX:5) = 'seven'
               MOVE 7 TO WS-DIGIT
               SET FOUND-DIGIT TO TRUE 
            END-IF

            IF WS-RECORD(IX:5) = 'eight'
               MOVE 8 TO WS-DIGIT
               SET FOUND-DIGIT TO TRUE 
            END-IF

            IF WS-RECORD(IX:4) = 'nine'
               MOVE 9 TO WS-DIGIT
               SET FOUND-DIGIT TO TRUE 
            END-IF
            .
