C
C     1.2.23
C
      INTEGER FUNCTION   BCWCAL
     I                          (BCWBTI,BCWNOV)
C
C     + + + PURPOSE + + +
C     Calculate a bcw from BCWBTI, BCWNOV
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    BCWBTI,BCWNOV
C
C     + + + ARGUMENT DEFINITIONS + + +
C     BCWBTI - block control word type indicator
C     BCWNOV - number of time frames represented by the tsb
C
C     + + + END SPECIFICATIONS + + +
      BCWCAL= BCWBTI*65536+BCWNOV
C
      RETURN
      END
C
C     1.2.24
C
      SUBROUTINE   BCWSPL
     I                  (BCW,
     O                   BCWBTI,BCWNOV)
C
C     + + + PURPOSE + + +
C     Split up a bcw into BCWBTI, BCWNOV
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    BCW,BCWBTI,BCWNOV
C
C     + + + ARGUMENT DEFINITIONS + + +
C     BCW    - ???
C     BCWBTI - block control word type indicator
C     BCWNOV - number of time frames represented by the tsb
C
C     + + + END SPECIFICATIONS + + +
C
      BCWNOV= 0
      BCWBTI= BCW
      IF(BCW .GT. 0) THEN
        BCWBTI= BCW/65536
        BCWNOV= BCW-(BCWBTI*65536)
      END IF
C
      RETURN
      END
C
C     1.2.20
C
      SUBROUTINE   MEXT
     I                (RECLT,BLEN,
     M                BADR,TBUFF)
C
C     + + + PURPOSE + + +
C     Move extension into buffer area as needed
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    BADR,BLEN,RECLT
      REAL       TBUFF(BLEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     RECLT  - length of buffer portion of tbuff
C     BLEN   - length of tbuff in real words
C     BADR   - largest address filled in vector tbuff
C     TBUFF  - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    NEXT
C
C     + + + END SPECIFICATIONS + + +
C
      NEXT = BADR -RECLT
      BADR = NEXT
      IF (BADR .LT. 0) BADR=0
   10 CONTINUE
      IF ((NEXT .GT. 0)) THEN
        TBUFF(NEXT) = TBUFF(RECLT+NEXT)
        NEXT        = NEXT -1
        GO TO 10
      END IF
C
      RETURN
      END
C
C     1.2.16
C
      SUBROUTINE   RBUFF
     I                 (REC,RECL,TSSFL,
     O                  BUFF)
C
C     + + + PURPOSE + + +
C     Read rec-th record into buff from file tssfl
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    REC,RECL,TSSFL
      REAL       BUFF(RECL)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     REC    - ???
C     RECL   - ???
C     TSSFL  - fortran unit number of time series store file
C     BUFF   - ???
C
C     + + + END SPECIFICATIONS + + +
C
      READ (TSSFL,REC=REC) BUFF
C
      RETURN
      END
C
C     4.1.01
C
      SUBROUTINE TFUNE
C
C     + + + PURPOSE + + +
C     Perform functional operations for the tss when the source
C     time interval = target time interval.  fills workspace with
C     values.
C
C     + + + COMMON BLOCKS- GETCOM + + +
      INCLUDE   'ctsin.inc'
      INCLUDE   'ctsex.inc'
      INCLUDE   'ctser.inc'
      INCLUDE   'ctsbu.inc'
      INCLUDE   'ctsbx.inc'
      INCLUDE   'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I
C
C     + + + INTRINSICS + + +
      INTRINSIC  ABS
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' ENTERING TFUNE')
 2010 FORMAT (3X, 'ZERO FLAG=',I6,3X,
     $        'UNDEFINED FLAG=',I6,3X,
     $        4(E16.7,2X), (/,3X, 7(E16.7,2X)))
C
C     + + + END SPECIFICATIONS + + +
C
      IF (TESTFG .GE. 2) WRITE (MESSU,2000)
C
C     Set flags to on here.  they are set off in the loop if a
C     Nonzero or undefined value is found.
      ZFLAG= 1
      UFLAG= 1
C
      DO 80 I=1,NCOMPS
        IF (STKIND(I) .GE. 3) GO TO 40
C         stkind is 1 or 2
C         point to point or mean to mean data
C
C         use same value
          XVAR(I)= XNEW(I)
          GO TO 60
C
 40     CONTINUE
C         point to mean data
C
C         use average
          XVAR(I)= (XOLD(I)+ XNEW(I))/2
 60     CONTINUE
C
C       set flag off if value is nonzero or undefined
C       if (xvar(i) .ne. 0.0) zflag= 0
        IF ((ABS(XVAR(I))).GT.0.0) ZFLAG= 0
        IF (XVAR(I) .GT. -1.E15) UFLAG= 0
C
C
C     Enddo
 80   CONTINUE
C
C
      IF (TESTFG .GE. 2) WRITE (MESSU,2010) ZFLAG,UFLAG,
     $                          (XVAR(I),I=1,NCOMPS)
C
      RETURN
      END
C
C     4.1.02
C
      SUBROUTINE TFUNG
     I                (TTIME,SDT)
C
C     + + + PURPOSE + + +
C     Functional operations for the tss when the source time
C     interval > target time interval
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   SDT,TTIME
C
C     + + + ARGUMENT DEFINITIONS + + +
C     TTIME - target time in minutes
C     SDT   - source time interval
C
C     + + + COMMON BLOCKS- GETCOM + + +
      INCLUDE   'ctsin.inc'
      INCLUDE   'ctsex.inc'
      INCLUDE   'ctser.inc'
      INCLUDE   'ctsbu.inc'
      INCLUDE   'ctsbx.inc'
      INCLUDE   'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,I4
      REAL      P
C
C     + + + INTRINSICS + + +
      INTRINSIC  ABS,FLOAT
C
C     + + + END SPECIFICATIONS + + +
C
C     rratio = sdt/target time interval
C
C     set flags to on here-then they are set to off in the
C     loop if non-zero/defined values are found
      ZFLAG= 1
      UFLAG= 1
      P = FLOAT (TTIME- TXOLD)/FLOAT (SDT)
C
      DO 70 I=1,NCOMPS
        I4=STKIND(I)
        GO TO (10,20,50), I4
C
   10     CONTINUE
C           point to point - interpolate
            XVAR(I)=XOLD(I) + P*(XNEW(I)-XOLD(I))
            GO TO 60
C
   20     CONTINUE
C           mean to mean- two cases-same=1,divide=2
            IF (STTRAN(I) .NE. 1) GO TO 30
              XVAR(I)=XNEW(I)
              GO TO 40
   30       CONTINUE
              XVAR(I)=XNEW(I)/RRATIO
   40       CONTINUE
            GO TO 60
C
   50     CONTINUE
C           point to mean - interpolate to the average value
            P=P -.5/RRATIO
            IF (P .LT. 0.0) P=0.0
            XVAR(I)=XOLD(I) +P*(XNEW(I)-XOLD(I))
            GO TO 60
C
   60   CONTINUE
C       if (xvar(i) .ne. 0) zflag= 0
        IF ((ABS(XVAR(I))).GT.0.0) ZFLAG= 0
        IF (XVAR(I) .GT. -1.E15) UFLAG= 0
   70 CONTINUE
      RETURN
      END
C
C     4.1.03
C
      SUBROUTINE TFUNL
     I                (TTIME)
C
C     + + + PURPOSE + + +
C     Functional operations for the tss when source time
C     interval < target time interval
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   TTIME
C
C     + + + ARGUMENT DEFINITIONS + + +
C     TTIME - target time in minutes from start of run
C
C     + + + COMMON BLOCKS- GETCOM  + + +
      INCLUDE   'ctsin.inc'
      INCLUDE   'ctsex.inc'
      INCLUDE   'ctser.inc'
      INCLUDE   'ctsbu.inc'
      INCLUDE   'ctsbx.inc'
      INCLUDE   'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,I4
      REAL      XN,XV
C
C     + + + INTRINSICS + + +
      INTRINSIC  AMAX1,AMIN1
C
C     + + + END SPECIFICATIONS + + +
C
      DO 120 I=1,NCOMPS
        XN=XNEW(I)
        XV=XVAR(I)
C
        I4=STKIND(I)
        GO TO (10,20,70), I4
   10     CONTINUE
C           point to point data-get last value
            XV=XN
            GO TO 115
C
   20     CONTINUE
C           mean to mean data
C
            I4=STTRAN(I) -2
            GO TO (30,40,50,60), I4
   30         CONTINUE
C               average - 3
                XV=XV +XN
                IF (TXNEW.EQ.TTIME) XV=XV/RRATIO
                GO TO 65
C
   40         CONTINUE
C               sum - 4
                XV=XV +XN
                GO TO 65
C
   50         CONTINUE
C               max - 5
                XV=AMAX1(XV,XN)
                GO TO 65
C
   60         CONTINUE
C               min - 6
                XV=AMIN1(XV,XN)
                GO TO 65
   65       CONTINUE
C
            GO TO 115
C
   70     CONTINUE
C           point to mean
C
            I4=STTRAN(I) -2
            GO TO (80,90,100,110), I4
C
   80         CONTINUE
C               average - 3
                XV=XV +XN +XOLD(I)
                IF (TXNEW .EQ. TTIME) XV=XV/(2*RRATIO)
                GO TO 112
C
   90         CONTINUE
C               sum - 4
                XV=XV +XN +XOLD(I)
                IF (TXNEW .EQ. TTIME) XV=XV/2
                GO TO 112
C
  100         CONTINUE
C               mx - 5
                XV=AMAX1(XV,XN)
                GO TO 112
C
  110         CONTINUE
C               min - 6
                XV=AMIN1(XV,XN)
                GO TO 112
  112       CONTINUE
            GO TO 115
  115   CONTINUE
        XVAR(I)=XV
  120 CONTINUE
C
      RETURN
      END
C
C     4.1.09
C
      SUBROUTINE XVINIT
     I                  (NCOMPS,FILE,FRMTIM,
     I                   MESSU,STTRAN,TYREND,YEAR,MSGFL,TESTFG,
     M                   KNT,
     O                   XVAR)
C
C     + + + PURPOSE + + +
C     Initialize the xvar values when source < target time interval
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   NCOMPS,MSGFL,FILE,FRMTIM,
     $          KNT,MESSU,STTRAN(20),TESTFG,TYREND,YEAR
      REAL      XVAR(20)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NCOMPS - ???
C     FILE   - ???
C     MESSU  - ftn unit no. to be used for printout of messages
C     FRMTIM - ???
C     STTRAN - ???
C     TYREND - ???
C     YEAR   - ???
C     MSGFL  - fortran unit number of HSPF message file
C     TESTFG - ???
C     KNT    - ???
C     XVAR   - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   DATIM(5),I,I4,SCLU,SGRP
      REAL      MAX,MIN
C
C     + + + EXTERNALS + + +
      EXTERNAL  OMSTI,OMSG,OMSTD,FDATIM
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' ENTERING XVINIT')
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU = 208
      IF (TESTFG .GE. 2) WRITE(MESSU,2000)
C
      MAX= 1.0E30
      MIN= -1.0E30
C
      DO 160 I= 1,NCOMPS
C
C       case entry sttran
        I4=STTRAN(I)
        GO TO (120,120,60,60,80,100,120,110), I4
C
C       case average,sum     sttran= 3,4
 60     CONTINUE
          XVAR(I)= 0.0
          GO TO 140
C
C       case max   sttran=5
 80     CONTINUE
          XVAR(I)= MIN
          GO TO 140
C
C       case min   sttran=6
 100    CONTINUE
          XVAR(I)= MAX
          GO TO 140
C
C       case last  sttran=8
 110    CONTINUE
          XVAR(I)= 0.0
          GO TO 140
C
C       case error
 120    CONTINUE
          CALL FDATIM (FRMTIM,YEAR,TYREND,DATIM)
          CALL OMSTD (DATIM)
          CALL OMSTI (FILE)
          SGRP = 1
          CALL OMSTI (STTRAN(I))
          CALL OMSTI (I)
          CALL OMSTI (FILE)
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               KNT)
C
C       endcase
 140    CONTINUE
C
 160  CONTINUE
C
      RETURN
      END
