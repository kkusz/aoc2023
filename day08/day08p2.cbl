         IDENTIFICATION DIVISION.
         PROGRAM-ID. DAY08P2.
            
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
            05 FILLER REDEFINES INPUT-TEXT.
               10 INPUT-NODE    PIC X(3).
               10 FILLER        PIC X(4).
               10 INPUT-LEFT    PIC X(3).
               10 FILLER        PIC X(2).
               10 INPUT-RIGHT   PIC X(3).
               10 FILLER        PIC X.

         WORKING-STORAGE SECTION.

         01 WS-RECORD           PIC X(200).

         01 WS-STEPS            PIC X(300).
         01 WS-STEPS-SIZE       PIC 999.
         01 WS-STEPS-COUNT      PIC 9(5) VALUE 0.

         01 WS-LETTERS          PIC X(3).
         01 WS-DIGIT            PIC 99.

         01 WS-POWER-TABLE.
            05 FILLER           PIC 9(5) VALUE 676.
            05 FILLER           PIC 9(5) VALUE  26.
            05 FILLER           PIC 9(5) VALUE   1.
         01 FILLER REDEFINES WS-POWER-TABLE.
            05 WS-POWER         PIC 9(5) OCCURS 3.

         01 WS-CONV-LETTER      PIC 9(5).

         77 IX                  PIC 9(5).
         77 IX2                 PIC 9 VALUE 0.
         77 IX3                 PIC 9 VALUE 0.
         77 TRAV-IX             PIC 9(5).


         01 WS-PARSED-TYPE         PIC 9 VALUE 0.
            88 WS-PAR-INIT               VALUE 0.
            88 WS-PAR-SPACE              VALUE 1.
            88 WS-PAR-STEP               VALUE 2.

         01 WS-CUR-NODES-TABLE.
            05 WS-CUR-NODE PIC 9(5) OCCURS 6.
         01 WS-CUR-NODES-SIZE      PIC 99.

         01 WS-END-NODES-TABLE.
            05 WS-END-NODE PIC 9(5) OCCURS 6.
         01 WS-END-NODES-SIZE      PIC 99.

         01 WS-REACHED-TERMINAL    PIC 99.

         01 WS-TRAVERSAL-TABLE.
            05 TRAV-NODE        OCCURS 17576.
               10 TRAV-LEFT        PIC 9(5).
               10 TRAV-RIGHT       PIC 9(5).

         01 DIV.
            05 DIV-RESULT       PIC 9(10).
            05 DIV-REMAINDER    PIC 9(10).

         01 WS-LOOP-TABLE.
            05 FILLER OCCURS 10.
               10 WS-LOOP          PIC 9(10).
               10 FILLER           PIC X VALUE SPACE.
         01 WS-LOOP-TERM-TABLE.
            05 FILLER OCCURS 10.
               10 WS-LOOP-TERM     PIC 9 VALUE 0.
                  88 REACHED-END     VALUE 1.
               10 FILLER           PIC X VALUE SPACE.

         LINKAGE SECTION. 


         PROCEDURE DIVISION.
            OPEN INPUT INPUT-FILE
            READ INPUT-FILE
               AT END SET INPUT-END   TO TRUE
            END-READ
            PERFORM UNTIL INPUT-END
               EVALUATE TRUE
                  WHEN WS-PAR-INIT
                     MOVE INPUT-TEXT  TO WS-STEPS
                     SET WS-PAR-SPACE TO TRUE
                  WHEN WS-PAR-SPACE
                     SET WS-PAR-STEP TO TRUE
                  WHEN WS-PAR-STEP
                     MOVE INPUT-NODE TO WS-LETTERS
                     PERFORM CONV-LETTER-TO-NUM
                     MOVE WS-CONV-LETTER TO TRAV-IX

                     IF WS-LETTERS(3:1) = 'A'
                        ADD 1 TO IX2
                        MOVE WS-CONV-LETTER TO WS-CUR-NODE(IX2)
                     END-IF

                     IF WS-LETTERS(3:1) = 'Z'
                        ADD 1 TO IX3
                        MOVE WS-CONV-LETTER TO WS-END-NODE(IX3)
                     END-IF

                     MOVE INPUT-LEFT TO WS-LETTERS
                     PERFORM CONV-LETTER-TO-NUM 
                     MOVE WS-CONV-LETTER TO TRAV-LEFT(TRAV-IX)

                     MOVE INPUT-RIGHT TO WS-LETTERS
                     PERFORM CONV-LETTER-TO-NUM
                     MOVE WS-CONV-LETTER TO TRAV-RIGHT(TRAV-IX)
               END-EVALUATE
               READ INPUT-FILE
                  AT END SET INPUT-END TO TRUE
               END-READ
            END-PERFORM
      *     DISPLAY 'IX ' IX2 ' : ' IX3
            MOVE IX2 TO WS-CUR-NODES-SIZE
            MOVE IX3 TO WS-END-NODES-SIZE

            CLOSE INPUT-FILE

      *     ---------------

            INSPECT WS-STEPS TALLYING WS-STEPS-SIZE 
                             FOR ALL 'L', ALL 'R'

            MOVE 1           TO TRAV-IX
            MOVE 0           TO IX
            MOVE 0           TO WS-REACHED-TERMINAL 
      *     DISPLAY 'T' WS-REACHED-TERMINAL ' ' WS-CUR-NODES-SIZE
            MOVE 0 TO WS-REACHED-TERMINAL
            PERFORM UNTIL WS-REACHED-TERMINAL = WS-CUR-NODES-SIZE
               ADD 1 TO IX
               ADD 1 TO WS-STEPS-COUNT

               IF IX > WS-STEPS-SIZE
                  MOVE 1 TO IX
               END-IF
               PERFORM VARYING IX2 FROM 1 BY 1
                       UNTIL IX2 > WS-CUR-NODES-SIZE
                  IF REACHED-END(IX2)
                     CONTINUE
                  ELSE
                    MOVE WS-CUR-NODE(IX2) TO TRAV-IX
                    EVALUATE WS-STEPS(IX:1)
                       WHEN 'L'
                          MOVE TRAV-LEFT(TRAV-IX)  TO TRAV-IX
                       WHEN 'R'
                          MOVE TRAV-RIGHT(TRAV-IX) TO TRAV-IX
                    END-EVALUATE
                    MOVE TRAV-IX TO WS-CUR-NODE(IX2)
  
                    SUBTRACT 1 FROM TRAV-IX
                    DIVIDE TRAV-IX BY 26 GIVING DIV-RESULT 
                                         REMAINDER DIV-REMAINDER
                    IF DIV-REMAINDER = 25
                       ADD 1 TO WS-REACHED-TERMINAL
                       SET REACHED-END(IX2) TO TRUE
                       MOVE WS-STEPS-COUNT TO WS-LOOP(IX2)
                    END-IF
                  END-IF
               END-PERFORM
      *        call "CBL_GC_NANOSLEEP" using "500000000" end-call
      *        DISPLAY 'T' WS-REACHED-TERMINAL ' ' WS-CUR-NODES-SIZE
      *        DISPLAY WS-LOOP-TERM-TABLE 
            END-PERFORM

            DISPLAY WS-LOOP-TABLE 
            DISPLAY 'Now calculate LCM using Wolfram or something'
              
            STOP RUN.

         CONV-LETTER-TO-NUM SECTION.
            MOVE ZERO TO WS-CONV-LETTER 

            PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 3
               EVALUATE WS-LETTERS(IX:1)
                  WHEN 'A' MOVE 0 TO WS-DIGIT
                  WHEN 'B' MOVE 1 TO WS-DIGIT
                  WHEN 'C' MOVE 2 TO WS-DIGIT
                  WHEN 'D' MOVE 3 TO WS-DIGIT
                  WHEN 'E' MOVE 4 TO WS-DIGIT
                  WHEN 'F' MOVE 5 TO WS-DIGIT
                  WHEN 'G' MOVE 6 TO WS-DIGIT
                  WHEN 'H' MOVE 7 TO WS-DIGIT
                  WHEN 'I' MOVE 8 TO WS-DIGIT
                  WHEN 'J' MOVE 9 TO WS-DIGIT
                  WHEN 'K' MOVE 10 TO WS-DIGIT
                  WHEN 'L' MOVE 11 TO WS-DIGIT
                  WHEN 'M' MOVE 12 TO WS-DIGIT
                  WHEN 'N' MOVE 13 TO WS-DIGIT
                  WHEN 'O' MOVE 14 TO WS-DIGIT
                  WHEN 'P' MOVE 15 TO WS-DIGIT
                  WHEN 'Q' MOVE 16 TO WS-DIGIT
                  WHEN 'R' MOVE 17 TO WS-DIGIT
                  WHEN 'S' MOVE 18 TO WS-DIGIT
                  WHEN 'T' MOVE 19 TO WS-DIGIT
                  WHEN 'U' MOVE 20 TO WS-DIGIT
                  WHEN 'V' MOVE 21 TO WS-DIGIT
                  WHEN 'W' MOVE 22 TO WS-DIGIT
                  WHEN 'X' MOVE 23 TO WS-DIGIT
                  WHEN 'Y' MOVE 24 TO WS-DIGIT
                  WHEN 'Z' MOVE 25 TO WS-DIGIT
               END-EVALUATE
               COMPUTE WS-CONV-LETTER = WS-CONV-LETTER
                                      + WS-POWER(IX)
                                      * WS-DIGIT
            END-PERFORM
            ADD 1 TO WS-CONV-LETTER
            .
