         IDENTIFICATION DIVISION.
         PROGRAM-ID. DAY06P1.
            
         ENVIRONMENT DIVISION.  
            
         DATA DIVISION.

         WORKING-STORAGE SECTION.

         01 WS-TIME     PIC  9(15) VALUE 7.
         01 WS-DISTANCE PIC 9(15) VALUE 9.

         01 SW-FOUND    PIC 9.
            88 FOUND            VALUE 1.
            88 NOT-FOUND        VALUE 0.

         01 WS-HALF-TIME PIC 9(15).

         01 WS-LEFT     PIC 9(15).
         01 WS-MIDDLE   PIC 9(15).
         01 WS-RIGHT    PIC 9(15).

         01 WS-COMP-DIST PIC 9(15).
         01 WS-POSSIB    PIC 9(15).

      *  01 WS-INPUT-LEN PIC 99 VALUE 3.
      *  01 WS-INPUT.
      *     05 FILLER    PIC 9(15) VALUE   7.
      *     05 FILLER    PIC 9(15) VALUE   9.
      *     05 FILLER    PIC 9(15) VALUE  15.
      *     05 FILLER    PIC 9(15) VALUE  40.
      *     05 FILLER    PIC 9(15) VALUE  30.
      *     05 FILLER    PIC 9(15) VALUE 200.
      *     05 FILLER    PIC 9(15) VALUE   0.
      *     05 FILLER    PIC 9(15) VALUE   0.

      *  01 WS-INPUT-LEN PIC 99 VALUE 4.
      *  01 WS-INPUT.
      *     05 FILLER    PIC 9(15) VALUE   34.
      *     05 FILLER    PIC 9(15) VALUE  204.
      *     05 FILLER    PIC 9(15) VALUE   90.
      *     05 FILLER    PIC 9(15) VALUE 1713.
      *     05 FILLER    PIC 9(15) VALUE   89.
      *     05 FILLER    PIC 9(15) VALUE 1210.
      *     05 FILLER    PIC 9(15) VALUE   86.
      *     05 FILLER    PIC 9(15) VALUE 1780.

         01 WS-INPUT-LEN PIC 99 VALUE 1.
         01 WS-INPUT.
            05 FILLER    PIC 9(15) VALUE  34908986.
            05 FILLER    PIC 9(15) VALUE  204171312101780.
            
         01 WS-INPUT-TAB REDEFINES WS-INPUT.
            05 FILLER OCCURS 4.
               10 WS-INP-TIME PIC 9(15).
               10 WS-INP-DIST PIC 9(15).

         77 IX           PIC 99.

         01 WS-MULTIPLIER PIC 9(15) VALUE 1.

         PROCEDURE DIVISION.
            PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > WS-INPUT-LEN
               MOVE WS-INP-TIME(IX) TO WS-TIME
               MOVE WS-INP-DIST(IX) TO WS-DISTANCE
               PERFORM COMPUTE-POSSIB
               MULTIPLY WS-POSSIB BY WS-MULTIPLIER 
            END-PERFORM

            DISPLAY WS-MULTIPLIER

            STOP RUN
            .

         COMPUTE-POSSIB SECTION.
            COMPUTE WS-HALF-TIME ROUNDED = WS-TIME / 2

            MOVE 0 TO WS-LEFT
            MOVE WS-HALF-TIME TO WS-RIGHT 
            COMPUTE WS-MIDDLE = (WS-LEFT + WS-RIGHT) / 2

            SET NOT-FOUND TO TRUE
            PERFORM UNTIL WS-LEFT > WS-RIGHT OR FOUND
               COMPUTE WS-MIDDLE = (WS-LEFT + WS-RIGHT) / 2
               COMPUTE WS-COMP-DIST            =  WS-MIDDLE
                                               * ( WS-TIME
                                               - WS-MIDDLE )
               IF WS-COMP-DIST < WS-DISTANCE
                  MOVE WS-MIDDLE TO WS-LEFT
                  ADD 1 TO WS-LEFT
               ELSE
                  IF WS-COMP-DIST > WS-DISTANCE
                     MOVE WS-MIDDLE TO WS-RIGHT
                     SUBTRACT 1 FROM WS-RIGHT
                  ELSE
                     SET FOUND TO TRUE
                  END-IF
               END-IF
            END-PERFORM


            COMPUTE WS-POSSIB = WS-TIME - WS-MIDDLE * 2 - 1
            .
