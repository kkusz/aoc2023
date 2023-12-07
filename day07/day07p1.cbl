         IDENTIFICATION DIVISION.
         PROGRAM-ID. DAY07P1.
            
         ENVIRONMENT DIVISION.  

         INPUT-OUTPUT SECTION. 
         FILE-CONTROL.
            SELECT INPUT-FILE ASSIGN TO INPFILE
            ORGANIZATION IS LINE SEQUENTIAL.

            SELECT OUTPUT-FILE ASSIGN TO OUTFILE
            ORGANIZATION IS LINE SEQUENTIAL.

            SELECT SORT-INPUT-FILE ASSIGN TO OUTFILE
            ORGANIZATION IS LINE SEQUENTIAL.

            SELECT SORT-WORK-FILE ASSIGN TO WRKFILE
            ORGANIZATION IS LINE SEQUENTIAL.

            SELECT SORT-OUTPUT-FILE ASSIGN TO SRTFILE
            ORGANIZATION IS LINE SEQUENTIAL.

            SELECT SORTED-FILE ASSIGN TO SRTFILE
            ORGANIZATION IS LINE SEQUENTIAL.

         DATA DIVISION.
         FILE SECTION. 
         FD INPUT-FILE.
         01 INPUT-RECORD.
            88 INPUT-END        VALUE LOW-VALUE.
            05 INPUT-TEXT       PIC X(10).
            05 FILLER REDEFINES INPUT-TEXT.
               10 INPUT-HANDS    PIC X(5).
               10 INPUT-HAND     REDEFINES INPUT-HANDS
                                 PIC X OCCURS 5.
               10 FILLER        PIC X.
               10 INPUT-BID     PIC ZZZ9.

         FD OUTPUT-FILE.
         01 OUTPUT-RECORD.
            05 OUTPUT-TEXT      PIC X(25).

         FD SORT-INPUT-FILE.
         01 SORT-INPUT-RECORD.
            05 SORT-I-KEY       PIC X(11).
            05 FILLER           PIC X.
            05 SORT-I-HAND      PIC X(5).
            05 FILLER           PIC X.
            05 SORT-I-BID       PIC 9999.

         FD SORT-OUTPUT-FILE.
         01 SORT-OUTPUT-RECORD.
            05 SORT-O-KEY       PIC X(11).
            05 FILLER           PIC X.
            05 SORT-O-HAND      PIC X(5).
            05 FILLER           PIC X.
            05 SORT-O-BID       PIC 9999.

         SD SORT-WORK-FILE.
         01 SORT-WORK-RECORD.
            05 SORT-W-KEY       PIC X(11).
            05 FILLER           PIC X.
            05 SORT-W-HAND      PIC X(5).
            05 FILLER           PIC X.
            05 SORT-W-BID       PIC 9999.

         FD SORTED-FILE.
         01 SORTED-RECORD.
            88 SORTED-END       VALUE LOW-VALUE.
            05 SORTED-KEY       PIC X(11).
            05 FILLER           PIC X.
            05 SORTED-HAND      PIC X(5).
            05 FILLER           PIC X.
            05 SORTED-BID       PIC 9999.

         WORKING-STORAGE SECTION.

         77 IX                  PIC 9999.
         77 IX2                 PIC 9999.

         01 WS-CARD-LIST-TABLE.
            05 WS-CARD-LIST OCCURS 14.
               10 WS-CARD-NO PIC 99.


         01 WS-CONV-CARD.
            05 WS-CARD-TYPE         PIC 9.
               88 TY-FIVE-OF-A-KIND       VALUE 7.
               88 TY-FOUR-OF-A-KIND       VALUE 6.
               88 TY-FULL-HOUSE           VALUE 5.
               88 TY-THREE-OF-A-KIND      VALUE 4.
               88 TY-TWO-PAIR             VALUE 3.
               88 TY-ONE-PAIR             VALUE 2.
               88 TY-HIGH-CARD            VALUE 1.
            05 WS-CONV-HAND        PIC 99 OCCURS 5.
            05 FILLER              PIC X  VALUE SPACE.
            05 WS-ORIG-HAND        PIC X(5).
            05 FILLER              PIC X  VALUE SPACE.
            05 WS-CONV-BID         PIC 9999.

         01 WS-CARD-STATS.
            05 WS-STAT-FIVES       PIC 9.
            05 WS-STAT-FOURS       PIC 9.
            05 WS-STAT-TRIPLES     PIC 9.
            05 WS-STAT-PAIRS       PIC 9.

         01 WS-RANK                PIC 9(10).
         01 WS-RESULT              PIC 9(10).

         LINKAGE SECTION. 

         PROCEDURE DIVISION.
            
            OPEN INPUT  INPUT-FILE
            OPEN OUTPUT OUTPUT-FILE
            READ INPUT-FILE
               AT END SET INPUT-END TO TRUE
            END-READ

            PERFORM UNTIL INPUT-END
               PERFORM CONVERT-CARD
               MOVE WS-CONV-CARD TO OUTPUT-TEXT
               WRITE OUTPUT-RECORD
               READ INPUT-FILE
                  AT END SET INPUT-END TO TRUE
               END-READ
            END-PERFORM

           CLOSE INPUT-FILE
           CLOSE OUTPUT-FILE

           SORT SORT-WORK-FILE
             ON ASCENDING KEY SORT-O-KEY 
             USING SORT-INPUT-FILE
             GIVING SORT-OUTPUT-FILE
           
           MOVE 0     TO WS-RANK 
           MOVE 0     TO WS-RESULT
           OPEN INPUT SORTED-FILE
           READ SORTED-FILE
              AT END SET SORTED-END TO TRUE
           END-READ

           PERFORM UNTIL SORTED-END
              ADD 1 TO WS-RANK

              COMPUTE WS-RESULT = WS-RESULT + WS-RANK * SORTED-BID 

              READ SORTED-FILE
                 AT END SET SORTED-END TO TRUE
              END-READ
           END-PERFORM
           CLOSE SORTED-FILE

           DISPLAY WS-RESULT

           STOP RUN
           .

         CONVERT-CARD SECTION.
            INITIALIZE WS-CARD-LIST-TABLE
            PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 5
               EVALUATE INPUT-HAND(IX)
                  WHEN 'A'   MOVE 14               TO IX2
                  WHEN 'K'   MOVE 13               TO IX2
                  WHEN 'Q'   MOVE 12               TO IX2
                  WHEN 'J'   MOVE 11               TO IX2
                  WHEN 'T'   MOVE 10               TO IX2
                  WHEN OTHER MOVE INPUT-HAND(IX)   TO IX2
               END-EVALUATE
               ADD 1    TO WS-CARD-NO(IX2)
               MOVE IX2 TO WS-CONV-HAND(IX)
            END-PERFORM

            MOVE INPUT-BID   TO WS-CONV-BID
            MOVE INPUT-HANDS TO WS-ORIG-HAND

            PERFORM DETERMINE-TYPE
            .

         DETERMINE-TYPE SECTION.
              INITIALIZE WS-CARD-STATS
              PERFORM VARYING IX2 FROM 1 BY 1 UNTIL IX2 > 14
                 EVALUATE WS-CARD-NO(IX2)
                    WHEN 5   ADD 1 TO WS-STAT-FIVES
                    WHEN 4   ADD 1 TO WS-STAT-FOURS
                    WHEN 3   ADD 1 TO WS-STAT-TRIPLES
                    WHEN 2   ADD 1 TO WS-STAT-PAIRS
                 END-EVALUATE
              END-PERFORM

              EVALUATE TRUE
                 WHEN WS-STAT-FIVES   = 1
                    SET TY-FIVE-OF-A-KIND  TO TRUE 
                 WHEN WS-STAT-FOURS   = 1
                    SET TY-FOUR-OF-A-KIND  TO TRUE
                 WHEN WS-STAT-TRIPLES = 1
                  AND WS-STAT-PAIRS   = 1
                    SET TY-FULL-HOUSE      TO TRUE
                 WHEN WS-STAT-TRIPLES = 1
                    SET TY-THREE-OF-A-KIND TO TRUE
                 WHEN WS-STAT-PAIRS   = 2
                    SET TY-TWO-PAIR        TO TRUE
                 WHEN WS-STAT-PAIRS   = 1
                    SET TY-ONE-PAIR        TO TRUE
                 WHEN OTHER
                    SET TY-HIGH-CARD       TO TRUE
              END-EVALUATE
            .

