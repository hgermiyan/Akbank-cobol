       IDENTIFICATION DIVISION.
       PROGRAM-ID. PBE005HW.
       AUTHOR.     Halim Germiyan.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IDX-FILE   ASSIGN IDXFILE
                             ORGANIZATION INDEXED
                             ACCESS MODE RANDOM
                             RECORD KEY IDX-KEY
                             STATUS IDX-ST.
           SELECT OUT-FILE   ASSIGN TO OUTFILE
                             STATUS OUT-ST.
           SELECT INP-FILE   ASSIGN TO INPFILE
                             STATUS INP-ST.
       DATA DIVISION.
       FILE SECTION.
       FD  IDX-FILE.
       01  IDX-REC.
           03 IDX-KEY.
              05 IDX-ID        PIC S9(05)  COMP-3.
              05 IDX-DVZ       PIC S9(03)  COMP.
           03 IDX-NAME         PIC X(30).
           03 IDX-DATE         PIC S9(7)  COMP-3.
           03 IDX-BALLANCE     PIC S9(15) COMP-3.

       FD  OUT-FILE RECORDING MODE F.
       01  OUT-REC.
           03 OUT-ID           PIC X(5).
           03 FILLER           PIC X(3) VALUE SPACES.
           03 OUT-DVZ          PIC X(3).
           03 FILLER           PIC X(3) VALUE SPACES.
           03 OUT-NAME         PIC X(30).
           03 OUT-DATE         PIC X(8).
           03 FILLER           PIC X(3) VALUE SPACES.
           03 OUT-OLD-BAL      PIC X(15).
           03 FILLER           PIC X(3) VALUE SPACES.
           03 OUT-NEW-BAL      PIC X(15).

       FD  INP-FILE RECORDING MODE F.
       01  INP-REC.
           03 INP-ID           PIC X(5).
           03 INP-DVZ          PIC X(3).

       WORKING-STORAGE SECTION.
       01  WS-WORK-AREA.
           03 IDX-ST           PIC 9(2).
              88 IDX-SUCCESS            VALUE 00 97.
              88 IDX-NOTFND             VALUE 23.
           03 OUT-ST           PIC 9(2).
              88 OUT-SUCCESS            VALUE 00 97.
              88 OUT-EOF                VALUE 10.
           03 INP-ST           PIC 9(2).
              88 INP-SUCCESS            VALUE 00 97.
              88 INP-EOF                VALUE 10.

           03 INT-DATE         PIC 9(7).
           03 GREG-DATE        PIC 9(8).
           03 INT-BALLANCE     PIC 9(15).
           03 ALPHA-BALLANCE   PIC X(15).

       01  HEADER-1.
           05 FILLER         PIC X(5) VALUE 'ID'.
           05 FILLER         PIC X(3) VALUE SPACE.
           05 FILLER         PIC X(3) VALUE 'DVZ'.
           05 FILLER         PIC X(3) VALUE SPACE.
           05 FILLER         PIC X(15) VALUE 'NAME'.
           05 FILLER         PIC X(15) VALUE 'SURNAME'.
           05 FILLER         PIC X(8) VALUE 'DATE'.
           05 FILLER         PIC X(3) VALUE SPACE.
           05 FILLER         PIC X(15) VALUE 'OLD BALANCE'.
           05 FILLER         PIC X(3) VALUE SPACE.
           05 FILLER         PIC X(15) VALUE 'NEW BALANCE'.
      *--------------------
       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM H100-OPEN-FILES.
           PERFORM H190-INITIALIZE.
           PERFORM H200-PROCESS UNTIL INP-EOF.
           PERFORM H999-PROGRAM-EXIT.
           STOP RUN.
       0000-END. EXIT.

       H100-OPEN-FILES.
           OPEN INPUT  INP-FILE.
           OPEN OUTPUT OUT-FILE.
           OPEN INPUT  IDX-FILE.
           IF (NOT IDX-SUCCESS)
           DISPLAY 'UNABLE TO OPEN IDXFILE: ' IDX-ST
           MOVE IDX-ST TO RETURN-CODE
           PERFORM H999-PROGRAM-EXIT
           END-IF.
           IF (NOT OUT-SUCCESS)
           DISPLAY 'UNABLE TO OPEN OUTFILE: ' OUT-ST
           MOVE OUT-ST TO RETURN-CODE
           PERFORM H999-PROGRAM-EXIT
           END-IF.
           IF (NOT INP-SUCCESS)
           DISPLAY 'UNABLE TO READ INPFILE: ' INP-ST
           MOVE INP-ST TO RETURN-CODE
           PERFORM H999-PROGRAM-EXIT
           END-IF.
       H100-END. EXIT.

       H190-INITIALIZE.
           MOVE SPACES TO OUT-REC.
           WRITE OUT-REC FROM HEADER-1 AFTER ADVANCING 1 LINE.
       H190-END. EXIT.

       H200-PROCESS.
           READ INP-FILE
              AT END SET INP-EOF TO TRUE
              NOT AT END
              PERFORM H300-PROCESS-RECORD
           END-READ
           .
       H200-END. EXIT.

       H300-PROCESS-RECORD.
           COMPUTE IDX-ID  = FUNCTION NUMVAL (INP-ID)
           COMPUTE IDX-DVZ = FUNCTION NUMVAL (INP-DVZ)
           READ IDX-FILE
                KEY IS IDX-KEY
                INVALID KEY
                PERFORM H410-WRONG-KEY
                NOT INVALID KEY
                PERFORM H400-PROCESS-RECORD
           END-READ.
       H300-END. EXIT.


       H400-PROCESS-RECORD.
           MOVE IDX-NAME TO OUT-NAME
           PERFORM DATE-CONVERT
           MOVE GREG-DATE TO OUT-DATE
           MOVE IDX-BALLANCE TO OUT-OLD-BAL
           PERFORM H500-NEWBAL
           MOVE ALPHA-BALLANCE TO OUT-NEW-BAL
           MOVE IDX-ID TO OUT-ID
           MOVE IDX-DVZ TO OUT-DVZ
           WRITE OUT-REC
           IF OUT-ST NOT = 0
              DISPLAY 'UNABLE TO WRITE OUTFILE: ' OUT-ST
              MOVE OUT-ST TO RETURN-CODE
              PERFORM H999-PROGRAM-EXIT
           END-IF.
       H400-END. EXIT.

       H410-WRONG-KEY.
           DISPLAY 'WRONG KEY: ' INP-ID INP-DVZ.
       H410-END. EXIT.

       DATE-CONVERT.
           COMPUTE INT-DATE = FUNCTION INTEGER-OF-DAY(IDX-DATE).
           COMPUTE GREG-DATE = FUNCTION DATE-OF-INTEGER(INT-DATE).
       DATE-END. EXIT.

       H500-NEWBAL.
           COMPUTE INT-BALLANCE = FUNCTION INTEGER (IDX-BALLANCE)
           COMPUTE INT-BALLANCE = INT-BALLANCE + 100
           MOVE INT-BALLANCE TO ALPHA-BALLANCE.
       H500-END. EXIT.

       H999-PROGRAM-EXIT.
           CLOSE IDX-FILE.
           CLOSE OUT-FILE.
           CLOSE INP-FILE.
       H999-END. EXIT.
      *
