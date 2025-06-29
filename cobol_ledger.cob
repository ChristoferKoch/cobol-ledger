       IDENTIFICATION DIVISION.
       PROGRAM-ID. CobolLedger.
       AUTHOR. Christofer Koch.
       DATE-COMPILED.
               
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  COMMAND-LINE-ARGS   PIC X(30).
       01  NUM-OF-ARGS         PIC 99.
       01  LEDGER-FILE-NAME    PIC X(30).
       01  REPORT-TYPE         PIC X(10).
       01  FILTER-TABLE.
           05  FILTERS         PIC X(30) OCCURS 1 TO 10 TIMES
                                        DEPENDING ON NUM-OF-ARGS
                                        INDEXED BY FILTER-IDX.
       01  FILTER-PTR          PIC 99 VALUE 1.
       01  REPORT-NAME         PIC X(30).
       01  THIS-DATE           PIC X(8).

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 0100-GET-COMMAND-LINE-ARGS

           DISPLAY FILTER-TABLE
               
           ACCEPT THIS-DATE FROM DATE YYYYMMDD

           STRING REPORT-TYPE DELIMITED BY SPACES
               "_report_"
               THIS-DATE
               ".rpt"
               INTO REPORT-NAME
           END-STRING

           EVALUATE REPORT-TYPE
               WHEN "balance"
                   CALL "BalanceReport" USING BY CONTENT NUM-OF-ARGS
                       LEDGER-FILE-NAME
                       FILTER-TABLE
                       FILTER-PTR
                       REPORT-NAME
                       FILTER-IDX
                       THIS-DATE
               WHEN "register"
                   CALL "RegisterReport"
               WHEN "cleared"
                   CALL "ClearedReport"
               WHEN OTHER DISPLAY "Invalid Report Type Given"
           END-EVALUATE.

           STOP RUN.

       0100-GET-COMMAND-LINE-ARGS.
           ACCEPT NUM-OF-ARGS
               FROM ARGUMENT-NUMBER
           END-ACCEPT

           SUBTRACT 1 FROM NUM-OF-ARGS

           PERFORM NUM-OF-ARGS TIMES
           ACCEPT COMMAND-LINE-ARGS
               FROM ARGUMENT-VALUE
           END-ACCEPT
               
           EVALUATE COMMAND-LINE-ARGS
               WHEN "-f" ACCEPT COMMAND-LINE-ARGS
                       FROM ARGUMENT-VALUE
                       MOVE COMMAND-LINE-ARGS TO LEDGER-FILE-NAME
               WHEN "balance"
                   MOVE COMMAND-LINE-ARGS TO REPORT-TYPE
               WHEN "register"
                   MOVE COMMAND-LINE-ARGS TO REPORT-TYPE
               WHEN "cleared"
                   MOVE COMMAND-LINE-ARGS TO REPORT-TYPE
               WHEN OTHER MOVE COMMAND-LINE-ARGS TO FILTERS(FILTER-PTR)
                       ADD 1 TO FILTER-PTR
           END-EVALUATE
           END-PERFORM.
                      
