         IDENTIFICATION DIVISION.
         PROGRAM-ID. DAY04P1.
            
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
         01 WS-MULTIPLIER       PIC 9(10) VALUE 0.

         77 IX                  PIC 999.

         PROCEDURE DIVISION.
            MOVE SPACES TO WS-RECORD
            ACCEPT WS-RECORD
            PERFORM UNTIL WS-RECORD = SPACES
               PERFORM PARSE-RECORD
               PERFORM SEARCH-NUMBERS

               MOVE SPACES TO WS-RECORD
               ACCEPT WS-RECORD
            END-PERFORM

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
           MOVE ZERO TO WS-MULTIPLIER
           PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 25
              SET IX-WIN TO 1
              SEARCH WS-WINNING-NUM
                 AT END
                    CONTINUE
                 WHEN WS-WINNING-NUM(IX-WIN) = WS-OWNED-NUM(IX)
                  AND WS-WINNING-NUM(IX-WIN) NOT = SPACES
                    EVALUATE WS-MULTIPLIER
                       WHEN 0
                          MOVE 1 TO WS-MULTIPLIER
                       WHEN OTHER
                          MULTIPLY 2 BY WS-MULTIPLIER
                    END-EVALUATE
              END-SEARCH
           END-PERFORM
           ADD WS-MULTIPLIER TO WS-SUM
           .
