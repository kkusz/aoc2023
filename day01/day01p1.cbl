         IDENTIFICATION DIVISION.
         PROGRAM-ID. DAY01P1.
            
         ENVIRONMENT DIVISION.  
            
         DATA DIVISION.

         WORKING-STORAGE SECTION.

         01 WS-RECORD           PIC X(100).

         01 WS-SUM              PIC 9(10) VALUE 0.

         01 WS-DIGIT1           PIC 9.
         01 WS-DIGIT2           PIC 9.

         01 IX                  PIC 999.



            
            
         PROCEDURE DIVISION.
            MOVE SPACES TO WS-RECORD
            ACCEPT WS-RECORD
            PERFORM UNTIL WS-RECORD = SPACES
               MOVE 1 TO IX
               PERFORM UNTIL IX > 100 OR WS-RECORD(IX:1) IS NUMERIC
                  ADD 1 TO IX
               END-PERFORM
               DISPLAY WS-RECORD
               MOVE WS-RECORD(IX:1) TO WS-DIGIT1 
               
               MOVE 100 TO IX
               PERFORM UNTIL IX < 1 OR WS-RECORD(IX:1) IS NUMERIC
                  SUBTRACT 1 FROM IX
               END-PERFORM
               MOVE WS-RECORD(IX:1) TO WS-DIGIT2 
               DISPLAY WS-DIGIT1 WS-DIGIT2
               COMPUTE WS-SUM = WS-SUM + WS-DIGIT1 * 10 + WS-DIGIT2

               MOVE SPACES TO WS-RECORD
               ACCEPT WS-RECORD
            END-PERFORM

            DISPLAY WS-SUM 

            STOP RUN.
