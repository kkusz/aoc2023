         IDENTIFICATION DIVISION.
         PROGRAM-ID. DAY04P2.
            
         ENVIRONMENT DIVISION.  
            
         DATA DIVISION.

         WORKING-STORAGE SECTION.

         01 WS-RECORD           PIC X(200).

         01 WS-PARSED.
            05 WS-CARD-STR.
               10 FILLER        PIC X(4).
               10 WS-CARD-NO    PIC ZZZZ.
            05 WS-WINNING-STR   PIC X(30).
            05 WS-WINNING-NUM   REDEFINES WS-WINNING-STR
                                PIC ZZZ OCCURS 10
                                INDEXED BY IX-WIN.            
            05 WS-OWNED-STR     PIC X(75).
            05 WS-OWNED-NUM     REDEFINES WS-OWNED-STR
                                PIC ZZZ OCCURS 25.

         01 WS-SUM              PIC 9(10) VALUE 0.

         01 WS-CARDS-PILE.
            05 WS-CARD OCCURS 192.
               10 WS-MATCHES    PIC 99.
               10 WS-QUANTITY   PIC 9(10).


         77 IX                  PIC 999.
         77 CIX                 PIC 9999.
         77 CIX2                PIC 9999.

         PROCEDURE DIVISION.
            MOVE SPACES TO WS-RECORD
            ACCEPT WS-RECORD
            PERFORM UNTIL WS-RECORD = SPACES
               PERFORM PARSE-RECORD
               PERFORM SEARCH-NUMBERS

               MOVE SPACES TO WS-RECORD
               ACCEPT WS-RECORD
            END-PERFORM

            PERFORM PROCESS-CARDS

            DISPLAY WS-SUM
            STOP RUN.

         PARSE-RECORD SECTION.
            INITIALIZE WS-PARSED
            UNSTRING WS-RECORD 
                DELIMITED BY ':' OR ' |'
                INTO WS-CARD-STR,
                     WS-WINNING-STR,
                     WS-OWNED-STR
            END-UNSTRING 
           .

         SEARCH-NUMBERS SECTION.
           MOVE WS-CARD-NO TO CIX
           MOVE 1          TO WS-QUANTITY(CIX)
           MOVE 0          TO WS-MATCHES(CIX)
           PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 25
              SET IX-WIN TO 1
              SEARCH WS-WINNING-NUM
                 AT END
                    CONTINUE
                 WHEN WS-WINNING-NUM(IX-WIN) = WS-OWNED-NUM(IX)
                  AND WS-WINNING-NUM(IX-WIN) NOT = SPACES
                    ADD 1 TO WS-MATCHES(CIX)
              END-SEARCH
           END-PERFORM
           .

         PROCESS-CARDS SECTION.
           PERFORM VARYING CIX FROM 1 BY 1 UNTIL CIX > 192
              PERFORM WS-QUANTITY(CIX) TIMES
                 MOVE CIX TO CIX2
                 PERFORM WS-MATCHES(CIX) TIMES
                    ADD 1 TO CIX2
                    IF CIX2 <= 192
                       ADD 1 TO WS-QUANTITY(CIX2)
                    END-IF
                 END-PERFORM
              END-PERFORM
              ADD WS-QUANTITY(CIX) TO WS-SUM
           END-PERFORM

           .
