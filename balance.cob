       IDENTIFICATION DIVISION.
       PROGRAM-ID. BalanceReport.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LEDGER-FILE ASSIGN TO LEDGER-FILE-NAME
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT LEDGER-REPORT ASSIGN TO REPORT-NAME
               ORGANIZATION IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD LEDGER-FILE.
       01  RECORD-LINE.
           88 END-OF-FILE          VALUE HIGH-VALUES.
           05 DETAIL-LINE          PIC X(100).
       FD  LEDGER-REPORT.
       01  PRINT-LINE              PIC X(50).
                                        
       WORKING-STORAGE SECTION.
       01  REPORT-TYPE             PIC X(10).
       01  INFO-LINE 		       PIC X VALUE 'N'.
       01  BALANCE-INDEX	       PIC 9999 VALUE 1.
       01  LINE-COUNT              PIC 99 VALUE ZEROES.
       01  TMP-ACCOUNTS.
           05  TMP-GENERA	       PIC X(20).
           05  TMP-SPECIES	       PIC X(20).
	       05  TMP-INDIVIDUAL      PIC X(40).
           05  TMP-LINE-TOTAL      PIC 9(9)V99.
       01  BALANCE-LINE-TABLE.
           05  BALANCE-LINE        OCCURS 1 TO 1000 TIMES
                                        DEPENDING ON LINE-COUNT.
               10  GENERA-ACCT	   PIC X(20).
               10  SPECIES-ACCT    PIC X(20).
               10  INDIVIDUAL-ACCT PIC X(40).
               10  LINE-TOTAL	   PIC 9(9)V99.

       LINKAGE SECTION.
       01  NUM-OF-ARGS             PIC 99.
       01  LEDGER-FILE-NAME        PIC X(30).
       01  FILTER-TABLE.
           05  FILTERS             PIC X(30) OCCURS 1 TO 10 TIMES
                                        DEPENDING ON NUM-OF-ARGS
                                        INDEXED BY FILTER-IDX.
       01  FILTER-PTR              PIC 99.
       01  REPORT-NAME             PIC X(30).
       01  THIS-DATE               PIC X(8).
       

       PROCEDURE DIVISION USING NUM-OF-ARGS, LEDGER-FILE-NAME,
               FILTER-TABLE, FILTER-PTR, REPORT-NAME, THIS-DATE.    
       0200-BALANCE-MAIN.
           OPEN INPUT LEDGER-FILE, OUTPUT LEDGER-REPORT
           PERFORM 0210-GENERATE-TABLE
           SORT BALANCE-LINE ON ASCENDING KEY GENERA-ACCT
               SPECIES-ACCT
               INDIVIDUAL-ACCT
           DISPLAY BALANCE-LINE-TABLE

           CLOSE LEDGER-FILE, LEDGER-REPORT
           EXIT PROGRAM.

       0210-GENERATE-TABLE.
           READ LEDGER-FILE
               AT END SET END-OF-FILE TO TRUE
           END-READ

           PERFORM UNTIL END-OF-FILE
               IF DETAIL-LINE FUNCTION CHAR(1) = " " AND
                   DETAIL-LINE FUNCTION CHAR(2) = " " THEN
               UNSTRING DETAIL-LINE DELIMITED BY ":" OR "  "
                   INTO TMP-GENERA, TMP-SPECIES,
                   TMP-INDIVIDUAL, TMP-LINE-TOTAL
               END-UNSTRING
               IF FILTER-PTR > 1 THEN
                   SEARCH FILTERS
                       WHEN TMP-GENERA = FILTERS(FILTER-IDX) OR
                             TMP-SPECIES = FILTERS(FILTER-IDX) OR
                             TMP-INDIVIDUAL = FILTERS(FILTER-IDX)
                           MOVE "Y" TO INFO-LINE
                   END-SEARCH
               ELSE
                   MOVE "Y" TO INFO-LINE
               END-IF
               IF INFO-LINE = "Y" THEN
                   MOVE TMP-GENERA TO GENERA-ACCT(BALANCE-INDEX)
                   MOVE TMP-SPECIES TO SPECIES-ACCT(BALANCE-INDEX)
                   MOVE TMP-INDIVIDUAL TO INDIVIDUAL-ACCT(BALANCE-INDEX)
                   MOVE TMP-LINE-TOTAL TO LINE-TOTAL(BALANCE-INDEX)
                   ADD 1 TO BALANCE-INDEX
                   ADD 1 TO LINE-COUNT
               END-IF
               END-IF
               MOVE "N" TO INFO-LINE
               READ LEDGER-FILE
                   AT END SET END-OF-FILE TO TRUE
               END-READ
           END-PERFORM.    
                      
