       IDENTIFICATION DIVISION.
       PROGRAM-ID. CobolLedger.
       AUTHOR. Christofer Koch.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LedgerFile ASSIGN TO LedgerFileName
               ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD LedgerFile.
       01 LedgerRecord.
           02 TransactionDate      PIC X(5).
           02 TransactionStatus    PIC X.
           02 TransactionCode      PIC X(10).
           02 TransactionDesc      PIC X(30).
           02 TransactionLines.
               03 GeneraAccount    PIC X(10).
               03 SpeciesAccount   PIC X(10).
               03 SpecificAccount  PIC X(20).
               03 LineAmount       PIC 9(9)V99.
       WORKING-STORAGE SECTION.
       01 CommandLineArgs  PIC X(30).
       01 NumOfArgs        PIC 99.
       01 LedgerFileName   PIC X(30).
       01 ReportType       PIC X(10).
       01 Filters          PIC X(50).
       01 FilterPtr        PIC 99 VALUE 1.
       01 ReportTotal      PIC $$$,$$$,$$9.99
       REPORT SECTION.


       PROCEDURE DIVISION.
       Main.
           PERFORM GetCommandLineArgs

           OPEN INPUT LedgerFile

           CLOSE LedgerFile
                   
           STOP RUN.

       GetCommandLineArgs.
           ACCEPT NumOfArgs
               FROM ARGUMENT-NUMBER
           END-ACCEPT

           SUBTRACT 1 FROM NumOfArgs

           PERFORM NumOfArgs TIMES
           ACCEPT CommandLineArgs
               FROM ARGUMENT-VALUE
           END-ACCEPT
               
           EVALUATE CommandLineArgs
               WHEN "-f" ACCEPT CommandLineArgs
                       FROM ARGUMENT-VALUE
                       MOVE CommandLineArgs TO LedgerFileName
               WHEN "balance"
                   MOVE CommandLineArgs TO ReportType
               WHEN "register"
                   MOVE CommandLineArgs TO ReportType
               WHEN "cleared"
                   MOVE CommandLineArgs TO ReportType
               WHEN OTHER STRING CommandLineArgs   DELIMITED BY SPACES
                       ","                         DELIMITED BY SIZE
                       INTO Filters WITH POINTER FilterPtr
           END-EVALUATE
           END-PERFORM.
                      
