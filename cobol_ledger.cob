       IDENTIFICATION DIVISION.
       PROGRAM-ID. CobolLedger.
       AUTHOR. Christofer Koch.
       DATE-COMPILED.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LEDGERFILE ASSIGN TO LEDGERFILENAME
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT LEDGERREPORT ASSIGN TO REPORTNAME
               ORGANIZATION IS SEQUENTIAL.
               
       DATA DIVISION.
       FILE SECTION.
       FD LEDGERFILE.
       01 RECORDLINE.
           88 ENDOFFILE            VALUE HIGH-VALUES.
           05 TYPECODE             PIC X.
               88 COMMENT          VALUE ";".
               88 DATELINE         PIC 9.
               88 TRANSACTIONLINE  VALUE " ".
           05 TRANSACTIONDATE      PIC X(5).
           05 TRANSACTIONSTATUS    PIC X.
           05 TRANSACTIONCODE      PIC X(10).
           05 TRANSACTIONDESC      PIC X(30).

       01 TRANSACTIONLINES.
           05 TYPECODE             PIC X.
           05 GENERAACCOUNT        PIC X(10).
           05 SPECIESACCOUNT       PIC X(10).
           05 SPECIFICACCOUNT      PIC X(20).
           05 LINEAMOUNT           PIC 9(9)V99.
                                        
       FD LEDGERREPORT.
       01 PRINTLINE                PIC X(50).

       WORKING-STORAGE SECTION.
       01 COMMANDLINEARGS          PIC X(30).
       01 NUMOFARGS                PIC 99.
       01 LEDGERFILENAME           PIC X(30).
       01 REPORTTYPE               PIC X(10).
       01 PRINTREPORT              PIC X VALUE "F".
       01 FILTERS                  PIC X(50).
       01 FILTERPTR                PIC 99 VALUE 1.
       01 REPORTNAME               PIC X(30).
       01 CURRENTDATE              PIC X(8).
       01 REPORTTOTAL              PIC $$$,$$$,$$9.99.
       REPORT SECTION.


       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 0100-GETCOMMANDLINEARGS
               
           ACCEPT CURRENTDATE FROM DATE YYYYMMDD

           STRING REPORTTYPE DELIMITED BY SPACES
               "_report_"
               CURRENTDATE
               ".rpt"
               INTO REPORTNAME
           END-STRING

           EVALUATE REPORTTYPE
               WHEN "balance"
                   CALL "BalanceReport" USING BY REFERENCE LEDGERFILE,
                       LEDGERREPORT, RECORDLINE, TRANSACTIONLINE
                       BY CONTENT CURRENTDATE, FILTERS                              
               WHEN "register"
                   CALL "RegisterReport" USING BY REFERENCE LEDGERFILE,
                       LEDGERREPORT, RECORDLINE, TRANSACTIONLINE
                       BY CONTENT CURRENTDATE, FILTERS   
               WHEN "cleared"
                   CALL "ClearedReport" USING BY REFERENCE LEDGERFILE,
                       LEDGERREPORT, RECORDLINE, TRANSACTIONLINE
                       BY CONTENT CURRENTDATE, FILTERS   
               WHEN OTHER DISPLAY "Invalid Report Type Given"
           END-EVALUATE.

           CLOSE LEDGERFILE, LEDGERREPORT.
                   
           STOP RUN.

       0100-GETCOMMANDLINEARGS.
           ACCEPT NUMOFARGS
               FROM ARGUMENT-NUMBER
           END-ACCEPT

           SUBTRACT 1 FROM NUMOFARGS

           PERFORM NUMOFARGS TIMES
           ACCEPT COMMANDLINEARGS
               FROM ARGUMENT-VALUE
           END-ACCEPT
               
           EVALUATE COMMANDLINEARGS
               WHEN "-f" ACCEPT COMMANDLINEARGS
                       FROM ARGUMENT-VALUE
                       MOVE COMMANDLINEARGS TO LEDGERFILENAME
               WHEN "balance"
                   MOVE COMMANDLINEARGS TO REPORTTYPE
               WHEN "register"
                   MOVE COMMANDLINEARGS TO REPORTTYPE
               WHEN "cleared"
                   MOVE COMMANDLINEARGS TO REPORTTYPE
               WHEN "-print"
                   MOVE "T" TO PRINTREPORT
               WHEN OTHER STRING COMMANDLINEARGS   DELIMITED BY SPACES
                       ","                         DELIMITED BY SIZE
                       INTO FILTERS WITH POINTER FILTERPTR
           END-EVALUATE
           END-PERFORM.

       0200-GENBALANCEREPORT.                       
           GENERATE DETAILLINE
           READ LEDGERFILE
               AT END SET ENDOFFILE TO TRUE
           END-READ.

       0300-GENREGISTERREPORT.
           GENERATE DETAILLINE
           READ LEDGERFILE
               AT END SET ENDOFFILE TO TRUE
           END-READ.
               
       0400-GENCLEAREDREPORT.
           GENERATE DETAILLINE
           READ LEDGERFILE
               AT END SET ENDOFFILE TO TRUE
           END-READ.
                      
