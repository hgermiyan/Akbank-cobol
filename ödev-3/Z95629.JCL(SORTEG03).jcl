//SORTEG02 JOB ' ',CLASS=A,MSGLEVEL=(1,1),MSGCLASS=X,NOTIFY=&SYSUID
//DELET100 EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
   DELETE Z95629.QSAM.INP NONVSAM
   IF LASTCC LE 08 THEN SET MAXCC=0
//SORT0200 EXEC PGM=SORT
//SYSOUT   DD SYSOUT=*
//SORTIN   DD *
10003848
10002949
10001978
20002979
30002980
//SORTOUT  DD DSN=Z95629.QSAM.INP,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5,5),RLSE),
//            DCB=(RECFM=FB,LRECL=8)
//SYSIN    DD *
  SORT FIELDS=COPY
