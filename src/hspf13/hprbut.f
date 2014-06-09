C
C
C
      SUBROUTINE   OMSG
     I                 (MESSU,MESSFL,SCLU,SGRP,
     M                  COUNT)
C
C     + + + PURPOSE + + +
C     output an error or warning message
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSU,MESSFL,SCLU,SGRP,COUNT
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSU  - unit number to write message to
C     MESSFL - unit number containing text of message
C     SCLU   - cluster on message file containing message text
C     SGRP   - group on message file containing message text
C     COUNT  - count of messages written
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'phmsg.inc'
      INCLUDE 'chmsg.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      MAXCNT,MAXACT,MAXCLU,MAXGRP,
     #             ILEN,RLEN,SDIG,DECP,I0,JLEN,
     #             INITFG,OLEN,CONT,IPOS,RPOS,CPOS,TPOS
      INTEGER      I6
      CHARACTER*80 TBUFF
      CHARACTER*64 FNAME
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (TBUFF,TBUF1)
      CHARACTER*1  TBUF1(80)
C
C     + + + EXTERNALS + + +
      EXTERNAL     WMSGTT,OMSINI,DECCHX,INTCHR,HDMES3
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (7I5)
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (' **************************************************',
     $        '**********************************')
 2010 FORMAT (' ')
 2020 FORMAT (' * ',A80,' *')
 2040 FORMAT (' ERROR/WARNING ID:  ',2I4)
 2050 FORMAT (' DATE/TIME: ',I4,'/',I2,'/',I2,' ',I2,':',I2)
 2060 FORMAT (' *',82X,'*')
C
C     + + + END SPECIFICATIONS + + +
C
      I0= 0
      I6= 6
C
C     increment counter for this message
      COUNT= COUNT+ 1
C
      IF (COUNT .LE. 50) THEN
C       how many will we accept and what do we do when max is reached
        INITFG= 1
        OLEN  = 80
        CALL WMSGTT (MESSFL,SCLU,SGRP,INITFG,
     M               OLEN,
     O               TBUF1,CONT)
        INITFG= 0
        READ(TBUFF,1000) MAXCNT,MAXGRP,MAXACT,ILEN,RLEN,SDIG,DECP
        IF (ILEN .EQ. 0) THEN
C         default
          ILEN= 10
        END IF
        IF (RLEN .EQ. 0) THEN
C         default
          RLEN= 10
        END IF
        IF (SDIG .EQ. 0) THEN
C         default
          SDIG= 5
        END IF
        IF (DECP .EQ. 0) THEN
C         default
          DECP= 2
        END IF
      ELSE
C       assume we dont want this again
        MAXCNT= 1
      END IF
C
      IF (COUNT .LE. MAXCNT) THEN
C       write detailed error message
        WRITE (MESSU,2010)
        WRITE (MESSU,2010)
C       first write line of asterisks as separator
        WRITE (MESSU,2000)
        WRITE (MESSU,2060)
C       message id
C       WRITE (MESSU,2040) SCLU,SGRP
        WRITE (TBUFF,2040) SCLU, SGRP
        WRITE (MESSU,2020) TBUFF
        CALL HDMES3(I6,TBUFF)
        WRITE (MESSU,2060)
C       initialize position in output arrays
        IPOS= 0
        RPOS= 0
        CPOS= 0
 20     CONTINUE
          OLEN= 80
          CALL WMSGTT (MESSFL,SCLU,SGRP,INITFG,
     M                 OLEN,
     O                 TBUF1,CONT)
          IF (TBUFF(1:2) .EQ. '&D') THEN
C           include a date, use standard format
            WRITE(TBUFF,2050) DATIM
          ELSE
C           may include some user spec info
            TPOS= 0
 30         CONTINUE
              TPOS= TPOS+ 1
              IF (TBUFF(TPOS:TPOS+1) .EQ. '&I') THEN
C               an integer
                IPOS= IPOS+ 1
                IF (IPOS .LE. ICNT) THEN
C                 have an integer to write
                  CALL INTCHR (IMSVL(IPOS),ILEN,I0,
     O                         JLEN,TBUF1(TPOS))
                END IF
              ELSE IF (TBUFF(TPOS:TPOS+1) .EQ. '&R') THEN
C               a real
                RPOS= RPOS+ 1
                IF (RPOS .LE. RCNT) THEN
C                 have a real to write
                  DECP= -DECP
                  CALL DECCHX (RMSVL(RPOS),RLEN,SDIG,DECP,
     O                         TBUF1(TPOS))
                END IF
              ELSE IF (TBUFF(TPOS:TPOS+1) .EQ. '&C') THEN
C               include some characters
 40             CONTINUE
                  CPOS= CPOS+ 1
                  TBUF1(TPOS)= CMSVL(CPOS)
                  TPOS= TPOS+ 1
C                 loop back with more characters
                IF (TBUF1(TPOS).EQ.'C' .AND. TPOS.LT.80) GO TO 40
              END IF
            IF (TPOS .LT. OLEN) GO TO 30
          END IF
C         write the message
          WRITE(MESSU,2020) TBUFF
C         CALL HDMES3(I6,TBUFF)
C         loop back if more text available
        IF (CONT .EQ. 1) GO TO 20
C       bottom separator
        WRITE (MESSU,2060)
        WRITE (MESSU,2000)
        WRITE (MESSU,2010)
      END IF
C
      IF (COUNT .EQ. MAXCNT) THEN
C       last time to print, write two lines of asterisks as separator
        WRITE (MESSU,2010)
        WRITE (MESSU,2010)
        WRITE (MESSU,2000)
        WRITE (MESSU,2000)
        WRITE (MESSU,2060)
C       print last time message
        INITFG= 1
 10     CONTINUE
          MAXCLU= 205
          OLEN  = 80
          CALL WMSGTT (MESSFL,MAXCLU,MAXGRP,INITFG,
     M                 OLEN,
     O                 TBUF1,CONT)
          WRITE(MESSU,2020) TBUFF
C         CALL HDMES3(I6,TBUFF)
          INITFG= 0
C         loop back with more text
        IF (CONT .EQ. 1) GO TO 10
C       write two lines of asterisks as separator to messu
        WRITE (MESSU,2060)
        WRITE (MESSU,2000)
        WRITE (MESSU,2000)
        IF (MAXACT .EQ. 1) THEN
C         this is fatal!
          CALL HDMES3(I6,'Fatal HSPF Error')
          INQUIRE(UNIT=MESSU,NAME=FNAME)
          TBUFF= '  See file ' // FNAME
          CALL HDMES3(I6,TBUFF)
          STOP
        END IF
      END IF
C
C     reset storages
      CALL OMSINI
C
      RETURN
      END
C
C
C
      SUBROUTINE   OMSINI
C
C     + + + PURPOSE + + +
C     reset assoc parms to don't write
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'phmsg.inc'
      INCLUDE 'chmsg.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I
C
C     + + + END SPECIFICATIONS + + +
C
      ICNT= 0
      RCNT= 0
      CCNT= 0
      DO 10 I= 1,5
        DATIM(I)= 0
 10   CONTINUE
C
      RETURN
      END
C
C
C
C
      SUBROUTINE   OMSTI
     I                  (IVAL)
C
C     + + + PURPOSE + + +
C     save an integer value to output with a hspf message
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   IVAL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IVAL   - value to save
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'phmsg.inc'
      INCLUDE 'chmsg.inc'
C
C     + + + END SPECIFICATIONS + + +
C
C     increment counter of values saved
      ICNT= ICNT+ 1
      IF (ICNT .LE. MXMSI) THEN
C       save value
        IMSVL(ICNT)= IVAL
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   OMSTR
     I                  (RVAL)
C
C     + + + PURPOSE + + +
C     save an real value to output with a hspf message
C
C     + + + DUMMY ARGUMENTS + + +
      REAL   RVAL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     RVAL   - value to save
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'phmsg.inc'
      INCLUDE 'chmsg.inc'
C
C     + + + END SPECIFICATIONS + + +
C
C     increment counter of values saved
      RCNT= RCNT+ 1
      IF (RCNT .LE. MXMSR) THEN
C       save value
        RMSVL(RCNT)= RVAL
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   OMSTC
     I                  (CLEN,CVAL)
C
C     + + + PURPOSE + + +
C     save character value to output with a hspf message
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     CLEN
      CHARACTER*1 CVAL(CLEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CLEN   - length of character string
C     CVAL   - character string
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'phmsg.inc'
      INCLUDE 'chmsg.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I
C
C     + + + END SPECIFICATIONS + + +
C
      DO 10 I= 1,CLEN
C       increment counter of values saved
        CCNT= CCNT+ 1
        IF (CCNT .LE. MXMSC) THEN
C         save value
          CMSVL(CCNT)= CVAL(I)
        END IF
 10   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   OMSTD
     I                  (DATE)
C
C     + + + PURPOSE + + +
C     save a date to output with a hspf message
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   DATE(5)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DATE   - date to save
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'phmsg.inc'
      INCLUDE 'chmsg.inc'
C
C     + + + EXTERNALS + + +
      EXTERNAL   EXDATE
C
C     + + + END SPECIFICATIONS + + +
C
C     save date in external format
      CALL EXDATE (DATE,
     O             DATIM)
C
      RETURN
      END
