       IDENTIFICATION DIVISION.
       PROGRAM-ID. CobolLedger.
       AUTHOR. Christofer Koch.

       ENVIRONMENT DIVISION.
       FILE-CONTROL.
           SELECT LedgerFile ASSIGN TO LedgerFileName
               ORGANIZATION IS SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD LedgerFile.
       01 LedgerRecord.
           02 TransactionDate      PIC X(5) VALUE SPACES.
           02 TransactionStatus    PIC X VALUE SPACE.
           02 TransactionCode      PIC X(10) VALUE SPACES.
           02 TransactionDesc      PIC X(30) VALUE SPACES.
           02 TransactionLines.
               03 GeneraAccount    PIC X(10) VALUE SPACES.
               03 SpeciesAccount   PIC X(10) VALUE SPACES.
               03 SpecificAccount  PIC X(10) VALUE SPACES.
               03 LineAmount       PIC $$$,$$$,$$$,$$$.99 VALUE ZEROES.
       WORKING-STORAGE SECTION.
       01 LedgerFileName   PIC X(30) VALUE SPACES.
       REPORT SECTION.
       MOVE   TO LedgerFileName

       PROCEDURE DIVISION.
                                        
