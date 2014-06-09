C
C
C
      INTEGER FUNCTION   TABREC
     I                          (KEYST,KEYND,
     $                           TABNM1,TABSUB,
     $                           OPTNO,MESSU,MSGFL,
     $                           ECOUNT)
C
C     + + + PURPOSE + + +
C     Find the record in ucifl which contains information
C     pertaining to a specified optno, given the starting
C     and ending keys to the table.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     ECOUNT,MSGFL,KEYND,
     $            KEYST,MESSU,OPTNO,TABSUB
      CHARACTER*1 TABNM1(12)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     KEYST  - starting record number
C     KEYND  - ending record number
C     TABNM1 - table name
C     TABSUB - table subscript
C     OPTNO  - operation number
C     MESSU  - ftn unit no. to be used for printout of messages
C     MSGFL  - fortran unit number of HSPF message file
C     ECOUNT - count(s) of specific errors
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      FINDFG,I0,I12,KEY,OPFST,OPLST,SCLU,SGRP
      CHARACTER*80 UCIBF
C
C     + + + EXTERNALS + + +
      EXTERNAL   OMSG,OMSTC,OMSTI,GETUCI
C
C     + + + INPUT FORMATS + + +
 1010 FORMAT (2I5)
C
C     + + + END SPECIFICATIONS + + +
C
      I0    = 0
      I12   = 12
      TABREC= 0
      SCLU  = 213
C
      IF (KEYST.GT.0) THEN
C       table is in user's control input, read past table header
C
        KEY= KEYST
        CALL GETUCI (I0,
     M               KEY,
     O               UCIBF)
        FINDFG= 0
C
C       whiledo key<> keynd and findfg= 0
 10     CONTINUE
          IF (KEY .NE. KEYND) THEN
C           not at end of table yet
            READ (UCIBF,1010,ERR=20)  OPFST, OPLST
            GOTO 30
 20         CONTINUE
C             error - read error in line in first ten characters
              OPFST= 0
              OPLST= 0
              CALL OMSTC (I12,TABNM1)
              CALL OMSTI (TABSUB)
              SGRP= 3
              CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     O                   ECOUNT)
 30         CONTINUE
C
            IF (OPLST.EQ.0) THEN
C             handle blank oplst field
              OPLST= OPFST
            END IF
C
C           check validity of opfst,oplst range
            IF (OPFST.LE.0) THEN
C             error - opfst must be positive
              CALL OMSTC (I12,TABNM1)
              CALL OMSTI (TABSUB)
              CALL OMSTI (OPFST)
              SGRP = 1
              CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     O                   ECOUNT)
              OPFST= OPLST
            END IF
C
            IF (OPLST.LT.OPFST) THEN
C             error - oplst must be >= opfst
              CALL OMSTC (I12,TABNM1)
              CALL OMSTI (TABSUB)
              CALL OMSTI (OPLST)
              CALL OMSTI (OPFST)
              SGRP = 2
              CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     O                   ECOUNT)
            END IF
C
C           check optno against these values
            IF (OPTNO.GE.OPFST .AND. OPTNO.LE.OPLST) THEN
C             found the record
              FINDFG= 1
              TABREC= KEY
            END IF
            IF (FINDFG .EQ. 0) THEN
C             next record
              CALL GETUCI (I0,
     M                     KEY,
     O                     UCIBF)
            END IF
          END IF
        IF (KEY.NE.KEYND .AND. FINDFG.EQ.0) GO TO 10
      END IF
C
      RETURN
      END
