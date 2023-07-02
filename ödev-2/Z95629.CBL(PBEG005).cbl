      *-----------------------
      * Copyright Contributors to the COBOL Programming Course
      * SPDX-License-Identifier: CC-BY-4.0
      *-----------------------
       IDENTIFICATION DIVISION.
      *-----------------------
       PROGRAM-ID.    PBEG005.
       AUTHOR.        Halim Germiyan.
      *--------------------
       ENVIRONMENT DIVISION.
      *--------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PRINT-LINE ASSIGN    PRTLINE
                             STATUS    PRT-ST.
           SELECT ACCT-REC   ASSIGN    ACCTREC
                             STATUS    ACCT-ST.
      *-------------
       DATA DIVISION.
      *-------------
       FILE SECTION.
       FD  PRINT-LINE RECORDING MODE F.
       01  PRINT-REC.
           05  PRINT-ID       PIC 9(4).
           05  PRINT-NAME     PIC X(15).
           05  PRINT-SURNAME  PIC X(15).
           05  PRINT-B        PIC X(8).
           05  PRINT-BDATE    PIC 9(8).
           05  PRINT-C        PIC X(8).
           05  PRINT-TODAY    PIC 9(8).
           05  PRINT-D        PIC X(6).
           05  PRINT-FARK     PIC 9(10).

       FD  ACCT-REC RECORDING MODE F.
       01  ACCT-FIELDS.
           05  ACCT-NO            PIC 9(4).
           05  FIRST-NAME         PIC X(15).
           05  LAST-NAME          PIC X(15).
           05  ACCT-BDATE         PIC 9(8).
           05  ACCT-TODAY         PIC 9(8).
      *
       WORKING-STORAGE SECTION.
       01  WS-WORK-AREA.
           05 PRT-ST             PIC 9(2).
              88 PRT-SUCCESS    VALUE 00 97.
           05 ACCT-ST            PIC 9(2).
              88 ACCT-EOF       VALUE 10.
              88 ACCT-SUCCESS   VALUE 00 97.
           05 LASTREC           PIC X VALUE SPACE.
           05 WS-TODAY          PIC 9(8).
           05 WS-BDATE          PIC 9(8).
      *------------------
       PROCEDURE DIVISION.
      *------------------
       0000-MAIN.
           PERFORM H100-OPEN-FILES.
           PERFORM H200-PROCESS.
           PERFORM H999-PROGRAM-CLOSE.
           STOP RUN.
        0000-MAIN-END. EXIT.

       H100-OPEN-FILES.
           OPEN INPUT  ACCT-REC.
           OPEN OUTPUT PRINT-LINE.
           IF NOT ACCT-SUCCESS
               DISPLAY 'ACCT-REC OPEN ERROR : ' ACCT-ST
               PERFORM H999-PROGRAM-CLOSE
           END-IF.
           IF NOT PRT-SUCCESS
                DISPLAY 'PRINT-LINE OPEN ERROR : ' PRT-ST
                PERFORM H999-PROGRAM-CLOSE
           END-IF.
       H100-OPEN-FILES-END. EXIT.

       H200-PROCESS.
           PERFORM READ-RECORD
           PERFORM UNTIL LASTREC = 'Y'
               PERFORM WRITE-RECORD
               PERFORM READ-RECORD
           END-PERFORM
           .
       H200-PROCESS-END. EXIT.
       READ-RECORD.
           READ ACCT-REC
               AT END MOVE 'Y' TO LASTREC
           END-READ.
      *
       WRITE-RECORD.
           COMPUTE WS-TODAY = FUNCTION INTEGER-OF-DATE (ACCT-TODAY).
           COMPUTE WS-BDATE = FUNCTION INTEGER-OF-DATE (ACCT-BDATE).
           MOVE ACCT-NO      TO  PRINT-ID.
           MOVE FIRST-NAME   TO  PRINT-NAME.
           MOVE LAST-NAME    TO  PRINT-SURNAME.
           MOVE ' BDATE: '    TO  PRINT-B.
           MOVE ACCT-BDATE   TO  PRINT-BDATE.
           MOVE ' TODAY: '    TO  PRINT-C.
           MOVE ACCT-TODAY   TO  PRINT-TODAY.
           MOVE ' DAY: '      TO  PRINT-D.
           COMPUTE PRINT-FARK =  WS-TODAY - WS-BDATE.
           WRITE PRINT-REC.

       H999-PROGRAM-CLOSE.
           CLOSE ACCT-REC.
           CLOSE PRINT-LINE.
           STOP RUN.
       H999-PROGRAM-CLOSE-END. EXIT.
