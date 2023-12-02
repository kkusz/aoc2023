         IDENTIFICATION DIVISION.
         PROGRAM-ID. DAY02P2.
            
         ENVIRONMENT DIVISION.  
            
         DATA DIVISION.

         WORKING-STORAGE SECTION.

         01 WS-RECORD           PIC X(200).

         01 WS-PARSED.
            05 WS-GAME-STR.
               10 FILLER            PIC X(5).
               10 WS-GAMENO-STR     PIC X(5).
            05 WS-CUBES-STR         PIC X(200).
            05 WS-SETS-STR          PIC X(100) OCCURS 10.
            05 WS-CUBE-STR          PIC X(5) OCCURS 10.

         01 WS-GAME.
            05 WS-GAME-NUMBER       PIC 999.
            05 WS-SETS              OCCURS 10.
               10 WS-RED-CUBES      PIC 999.
               10 WS-GREEN-CUBES    PIC 999.
               10 WS-BLUE-CUBES     PIC 999.

         01 WS-MINIMUM.
            05 WS-RED-MINIMUM       PIC 999.
            05 WS-GREEN-MINIMUM     PIC 999.
            05 WS-BLUE-MINIMUM      PIC 999.

         01 WS-SUM                  PIC 9(10) VALUE 0.

         77 IX                      PIC 999.

         PROCEDURE DIVISION.
            MOVE SPACES TO WS-RECORD
            ACCEPT WS-RECORD
            PERFORM UNTIL WS-RECORD = SPACES
               PERFORM PARSE-RECORD

               MOVE 1 TO IX
               INITIALIZE WS-MINIMUM
               PERFORM UNTIL IX > 10
                  IF WS-RED-CUBES(IX)   > WS-RED-MINIMUM 
                     MOVE WS-RED-CUBES(IX) TO WS-RED-MINIMUM
                  END-IF

                  IF WS-GREEN-CUBES(IX)   > WS-GREEN-MINIMUM 
                     MOVE WS-GREEN-CUBES(IX) TO WS-GREEN-MINIMUM
                  END-IF

                  IF WS-BLUE-CUBES(IX)   > WS-BLUE-MINIMUM 
                     MOVE WS-BLUE-CUBES(IX) TO WS-BLUE-MINIMUM
                  END-IF
                  ADD 1 TO IX
               END-PERFORM
               
               COMPUTE WS-SUM = WS-SUM +
                                WS-RED-MINIMUM *
                                WS-GREEN-MINIMUM *
                                WS-BLUE-MINIMUM

               MOVE SPACES TO WS-RECORD
               ACCEPT WS-RECORD
            END-PERFORM

            DISPLAY WS-SUM
            STOP RUN.

         PARSE-RECORD SECTION.
            INITIALIZE WS-PARSED
            UNSTRING WS-RECORD 
                DELIMITED BY ':'
                INTO WS-GAME-STR,
                     WS-CUBES-STR 
            END-UNSTRING 

            UNSTRING WS-CUBES-STR 
                DELIMITED BY ';'
                INTO WS-SETS-STR(1),
                     WS-SETS-STR(2),
                     WS-SETS-STR(3),
                     WS-SETS-STR(4),
                     WS-SETS-STR(5),
                     WS-SETS-STR(6),
                     WS-SETS-STR(7),
                     WS-SETS-STR(8),
                     WS-SETS-STR(9),
                     WS-SETS-STR(10)
            END-UNSTRING

            MOVE 1 TO IX
            INITIALIZE WS-GAME

            MOVE WS-GAMENO-STR TO WS-GAME-NUMBER 

            PERFORM UNTIL IX > 10
              UNSTRING WS-SETS-STR(IX)
                  DELIMITED BY ',' OR ' '
                  INTO WS-CUBE-STR(1),
                       WS-CUBE-STR(2),
                       WS-CUBE-STR(3),
                       WS-CUBE-STR(4),
                       WS-CUBE-STR(5),
                       WS-CUBE-STR(6),
                       WS-CUBE-STR(7),
                       WS-CUBE-STR(8),
                       WS-CUBE-STR(9),
                       WS-CUBE-STR(10)
              END-UNSTRING

              IF WS-CUBE-STR(3) = 'red'
                 MOVE WS-CUBE-STR(2) TO WS-RED-CUBES(IX)
              END-IF
              IF WS-CUBE-STR(3) = 'green'
                 MOVE WS-CUBE-STR(2) TO WS-GREEN-CUBES(IX)
              END-IF
              IF WS-CUBE-STR(3) = 'blue'
                 MOVE WS-CUBE-STR(2) TO WS-BLUE-CUBES(IX)
              END-IF
              IF WS-CUBE-STR(6) = 'red'
                 MOVE WS-CUBE-STR(5) TO WS-RED-CUBES(IX)
              END-IF
              IF WS-CUBE-STR(6) = 'green'
                 MOVE WS-CUBE-STR(5) TO WS-GREEN-CUBES(IX)
              END-IF
              IF WS-CUBE-STR(6) = 'blue'
                 MOVE WS-CUBE-STR(5) TO WS-BLUE-CUBES(IX)
              END-IF
              IF WS-CUBE-STR(9) = 'red'
                 MOVE WS-CUBE-STR(8) TO WS-RED-CUBES(IX)
              END-IF
              IF WS-CUBE-STR(9) = 'green'
                 MOVE WS-CUBE-STR(8) TO WS-GREEN-CUBES(IX)
              END-IF
              IF WS-CUBE-STR(9) = 'blue'
                 MOVE WS-CUBE-STR(8) TO WS-BLUE-CUBES(IX)
              END-IF

              ADD 1 TO IX
            END-PERFORM
           .
