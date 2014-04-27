C
C
C
      SUBROUTINE   SETPST (MESSU, MSGFL, SUPFIL, OUTLEV,
     M                     ECOUNT) 
C
C     + + + PURPOSE + + +
C     Read (and echo) pest supplemental file
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      MESSU, MSGFL, SUPFIL, OUTLEV, ECOUNT
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSU  - unit to write messages on
C     MSGFL  - unit cotaining text for error messages
C     SUPFIL - unit number of pest supplemental file
C     OUTLEV - run interpreter output level
C     ECOUNT - count of errors 
C     
C     + + + COMMON BLOCKS + + +
      INCLUDE   'cpest.inc'
C
C     + + + LOCAL VARIABLES + + + 
      INTEGER   I, J, SGRP, SCLU, IECNT
C
C     + + + EXTERNALS + + +
      EXTERNAL  PMXTFT, OMSTC, OMSTI, OMSG
C
C     + + + OUTPUT FORMATS + + +
2000  FORMAT ('   Begin processing supplemental ID',I5,
     $        ' into position',I5,' with ',I5,' values')
2010  FORMAT ('     Supplemental values:',
     $        (10x,5(1x,1g14.6)))
2020  FORMAT ('   Processed ',I5,' supplemental IDs')
2030  FORMAT ('   Skip supplemental record')
2040  FORMAT ('   End of file on supplemental file after ',I5,' IDs')
C
C     + + + END SPECIFICATIONS + + +
C
C     cluster for messages
      SCLU = 241
C     start processing message
      SGRP = 1
      CALL PMXTFT (MSGFL,MESSU,SCLU,SGRP)
C
C     save original error count
      IECNT= ECOUNT
C
C     initialize common block
      DO 10 I= 1, MAXVEC
        DO 5 J=1, MAXNUM
          SUPVAR(J,I)= -1.0e37
    5   CONTINUE
   10 CONTINUE
C
      USED = 0
C     loop thru supplemental file
   20 CONTINUE
        USED = USED+ 1
        IF (USED .GT. MAXVEC) THEN
C         too many arrays, increase maxvec
          CALL OMSTI (MAXVEC)
          SGRP = 11
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
        ELSE
          READ (SUPFIL,*,ERR=25,END=100) ID(USED),NUM(USED)
          GOTO 30
   25     CONTINUE
C           error reading header
            CALL OMSTI(USED)
            CALL OMSTI(ID(USED))
            CALL OMSTI(NUM(USED))
            SGRP= 12
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 ECOUNT)
   30     CONTINUE
          IF (ID(USED) .EQ. 0) THEN
C           skip this supplemental record
            USED= USED- 1
            IF (OUTLEV .GT. 8) THEN
              WRITE(MESSU,2030)             
            END IF
          ELSE
            IF (OUTLEV .GT. 6) THEN
              WRITE(MESSU,2000) ID(USED),USED,NUM(USED)
            END IF
            IF (NUM(USED) .GT. MAXNUM) THEN
C             too many entries in array
              CALL OMSTI(USED)
              CALL OMSTI(ID(USED))
              CALL OMSTI(MAXNUM)
              CALL OMSTI(NUM(USED))
              SGRP = 13
              CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                   ECOUNT)
            ELSE
C             check to see if array identifier has already been used
              DO 40 J = 1, USED-1
                IF (ID(J) .EQ. ID(USED)) THEN
C                 already in use
                  CALL OMSTI(ID(USED))
                  CALL OMSTI(USED)
                  CALL OMSTI(J)
                  SGRP= 14
                  CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                       ECOUNT)
                END IF
   40         CONTINUE
C             read current values
              READ(SUPFIL,*,err=45,end=100) 
     $            (SUPVAR(I,USED),I=1,NUM(USED))
              IF (OUTLEV .GT. 7) THEN
                WRITE(MESSU,2010) (SUPVAR(I,USED),I=1,NUM(USED))
              END IF
              GOTO 50
   45         CONTINUE
C               error reading supplemental values
                CALL OMSTI(ID(USED))
                CALL OMSTI(USED)
                CALL OMSTI(NUM(USED))
                SGRP= 15
                CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                     ECOUNT)
   50         CONTINUE
            END IF
          END IF
        END IF
C       loop back up if no errors 
C       TODO: try to trap more errors in supp file
      IF (IECNT .EQ. ECOUNT) GOTO 20
C
C     get here on end of supp file or too many errors
  100 CONTINUE
      USED = USED- 1
      IF (OUTLEV .GT. 7) THEN
        WRITE(MESSU,2040) USED
      END IF
C
      IF (SUPVAR(NUM(USED),USED) .LT. -0.9e37) THEN
C       error reading data from array, can this ever happen?
        CALL OMSTI(ID(USED))
        CALL OMSTI(USED)
        CALL OMSTI(NUM(USED)) 
        SGRP= 16
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             ECOUNT)
      END IF
C
      IF (OUTLEV .GT. 2) THEN
C       echo processed supplemental file contents
        WRITE(MESSU,2020) USED
      END IF
C    
C     finished processing message
      SGRP = 2
      CALL PMXTFT (MSGFL,MESSU,SCLU,SGRP)
C
      RETURN
      END
C
C
C
      SUBROUTINE   GETVEC (MESSU, MSGFL, NVALS, IPOS, OUTLEV,
     M                     INBUFF, RVAL, ECOUNT, 
     O                     VALSET)
C
C     + + + PURPOSE + + +
C     Update information in uci with values from PEST supplemental file
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      MESSU, MSGFL, NVALS, IPOS, OUTLEV, ECOUNT, VALSET
      REAL         RVAL(NVALS)
      CHARACTER*80 INBUFF
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSU  - unit to write messages on
C     MSGFL  - unit cotaining text for error messages
C     NVALS  - number of values in table
C     IPOS   - position of last existing value in table
C     OUTLEV - run interpreter output level
C     INBUFF - record from uci file, pest ~X~ is removed in this routine
C     RVAL   - values from uci file 
C     ECOUNT - count of errors 
C     VALSET - flag indicating whether values set from this routine
C
C     + + + COMMON BLOCKS + + +
      INCLUDE   'cpest.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   SCLU, SGRP, T1, T2, I, IVEC, ERR, IIND
C
C     + + + EXTERNALS + + +
      EXTERNAL  OMSTI, OMSG
C
C     + + + OUTPUT FORMATS + + +
2000  FORMAT ('     Supplemental values from ID ',I5,
     $        ' have replaced values in UCI.')
2010  FORMAT (10x,5(1x,1g14.6))
2100  FORMAT ('   Begin processing supplemental ID',I5,' from UCI')
2110  FORMAT ('     Look thru',I5,' available update IDs')
2120  FORMAT ('     Found ID match at position',I5,' ID',I5)
2130  FORMAT ('     Update UCI record text to blank between',I5,
     $        ' and',I5)
2140  FORMAT ('     About to update',I5,' values beginning at',I5,
     $        ' internal array size is',I5)
C
C     + + + END SPECIFICATIONS + + +
C
C     no errors yet
      ERR = 0
C
C     cluster for messages
      SCLU = 241
C
      T1 = INDEX(INBUFF,'~') + 1
      IF (T1 .GT. 1) THEN
C       this should be a record to update with pest parms
        T2= INDEX(INBUFF,'~',.TRUE.) - 1
        IF (T1 .LE. T2) THEN
C         get the id for the pest supplemental info
          READ(INBUFF(T1:T2),*,ERR=20) IVEC
          IF (OUTLEV .GT. 4) THEN
C           echo
            WRITE(MESSU,2100) IVEC
            IF (OUTLEV .GT. 8) THEN
C             what are we looking thru
              WRITE(MESSU,2110) USED
            END IF
          END IF
          DO 10 I = 1, USED
            IF (ID(I) .EQ. IVEC) THEN
C             found the values we want
              IIND = I
              IF (OUTLEV .GT. 8) THEN
                WRITE(MESSU,2120) IIND,IVEC
              END IF
              EXIT
            ELSE IF (I .EQ. USED) THEN
C             no match for id
              CALL OMSTI(IVEC)
              SGRP= 21
              CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                   ECOUNT)             
              ERR= 1
            END IF
 10       CONTINUE
          GOTO 30
 20       CONTINUE
C           error reading id
            CALL OMSTI(T1)
            CALL OMSTI(T2)
            SGRP= 22
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 ECOUNT)             
            ERR = 1
 30       CONTINUE
        ELSE
C         problem with pest info flag delimeters
          CALL OMSTI(T1-1)
          CALL OMSTI(T2+1)
          SGRP= 23
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)             
          ERR = 1
        END IF
C
        IF (ERR .EQ. 0) THEN
          IF (OUTLEV .GT. 8) THEN
C           about to update uci record text
            WRITE(MESSU,2130) T1-1,T2+1
          END IF
          INBUFF(T1-1:T2+1) = ' '                                       
C
          IF (OUTLEV .GT. 6) THEN
C           about to replace message
            WRITE(MESSU,2140) NUM(IIND),IPOS+1,NVALS
          END IF
C
          IF (NVALS .NE. NUM(IIND)+IPOS-1) THEN
C           TODO: warning that counts dont match (might not be an error)
          END IF
C
          DO I= IPOS+1, IPOS+NUM(IIND)
            RVAL(I)= SUPVAR(I,IIND)
          END DO
C
          IF (OUTLEV .GT. 5) THEN
C           echo new values
            WRITE(MESSU,2000) ID(IIND)
            WRITE(MESSU,2010) (RVAL(I),I= IPOS+1, IPOS+NUM(IIND))
          END IF
C         updates made flag
          VALSET= 1          
        ELSE
C         no pest updates to this record due to errors
          VALSET= 0
        END IF
      ELSE
C       no pest updates for this record
        VALSET= 0  
      END IF
C
      RETURN  
      END
