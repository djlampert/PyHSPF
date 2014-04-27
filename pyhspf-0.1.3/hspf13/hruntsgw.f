C
C     4.1.3
C
      SUBROUTINE GETWDM
     I                  (DELT,WIDTH,WDMSFL,FSTCAL)
C
C     + + + PURPOSE + + +
C     get values from wdms file, transform them and fill a row on
C     the inpad
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   DELT,WIDTH,WDMSFL,FSTCAL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DELT   - simulation time interval in minutes
C     WIDTH  - inpad width
C     WDMSFL - watershed data management file unit number
C     FSTCAL - flag indicating first interval of run
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION GETCOM + + +
      INCLUDE   'ctsin.inc'
      INCLUDE   'ctsex.inc'
      INCLUDE   'ctser.inc'
      INCLUDE   'ctsbu.inc'
      INCLUDE   'ctsbx.inc'
      INCLUDE   'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,I1,I2,J,INPTIM(6),NGETS,NVALS,DTRAN,
     $          NPTS,RETCOD,INPNEW(6),NIVLS,PLACE,SCLU,SGRP,
     $          BRBFG,FSTFG,NVALSX
      REAL      CURVAL(1)
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDTGET,TIMADD,SETVEC,OMSTR,OMSTI,OMSG,LTRAN
C
C     + + + INTRINSICS + + +
      INTRINSIC   INT
C
C     + + + OUTPUT FORMATS + + +
 3000 FORMAT (/,' ENTERING GETWDM')
 3010 FORMAT (/,' LEAVING GETWDM',
     $          '   NGETS=',I6,';  NVALS=',I6,';  NPTS=',I6)
 3020 FORMAT (3X,'INPAD DELTA TIME=',I6,3X, 'WDM DELTAT TIME=',I6,
     $        3X,'WIDTH=',I12,/,3X,
     $        'INPAD START TIME (MIN)=',I10,/,3X,
     $        'INPAD START TIME (YR,MO,DY,ETC)=',6I6)
 3030 FORMAT (3X,'VOPADR=',I6,3X,'OFFSET=',I6,3X,'STKIND=',I6,/,
     $        3X,'STTRAN=',I6,3X,'INMODE=',I6,/,3X,'A=',E18.6,6X,
     $        'B=',E18.6)
C
C     + + + END SPECIFICATIONS + + +
C
      BRBFG= 0
C
      SCLU = 234
      IF (BRBFG .GE. 1) THEN
        WRITE (MESSU,3000)
        WRITE (MESSU,3020) DELT,DELTAT,WIDTH,INPSTR,INPDAT
        WRITE (MESSU,3030) VOPADR(1),OFFSET(1), STKIND(1),
     $                     STTRAN(1),INMODE(1), A(1),B(1)
      END IF
C
C     set constant values and values based on instruction
      I1 = 1
      I2 = 2
      IF (STTRAN(1).EQ.1.OR.STTRAN(1).EQ.3) DTRAN = 0
      IF (STTRAN(1).EQ.2.OR.STTRAN(1).EQ.4) DTRAN = 1
      IF (STTRAN(1).EQ.5) DTRAN = 3
      IF (STTRAN(1).EQ.6) DTRAN = 2
C
C     initialize inpad time to start time for inpad
      DO 30 I= 1,6
        INPTIM(I) = INPDAT(I)
 30   CONTINUE
C
C     calculate number of calls to wtdget
      NGETS = INT((WIDTH-1)/500) + 1
C
      NPTS = 1
C
C     call wdtget, transform data, and place data on pad;
C     repeat ngets times
      DO 60 I= 1,NGETS
C
C       number of values each call
        NVALS = 500
        IF (I.EQ.NGETS) NVALS= WIDTH - (NGETS - 1)*500
C
        IF (NVALS.GT.0) THEN
C         get values from wdmsfl
          CALL WDTGET (WDMSFL,FILE,DELT,INPTIM,NVALS,DTRAN,QLFG,I2,
     O                 TBUFF,RETCOD)
 2900     format(1x,a,20i5)
C          write(99,2900)
C     $      'GETWDM:wdmsfl,file,delt,inptim,nvals,dtran,qlfg,i2,retcod',
C     $              wdmsfl,file,delt,inptim,nvals,dtran,qlfg,i2,retcod
C
          FSTFG= 1
          IF (RETCOD.NE.0.AND.FSTCAL.EQ.1.AND.I.EQ.1) THEN
C           first time point of dataset not available - set a
C           flag; then get remaining data normally
C
C           write (messu,*) ' getwdm: first tm pt of dsn not avail'
            FSTFG= 0
            CALL TIMADD (INPTIM,I2,DELT,I1,
     O                   INPNEW)
            NVALSX = NVALS - 1
            CALL WDTGET (WDMSFL,FILE,DELT,INPNEW,NVALSX,DTRAN,QLFG,I2,
     O                   TBUFF,RETCOD)
          END IF
C
          IF (RETCOD.NE.0) THEN
            CALL SETVEC(NVALS,GAPVAL,
     O                  TBUFF)
            CALL OMSTI (FILE)
            CALL OMSTI (INPTIM(1))
            CALL OMSTI (INPTIM(2))
            CALL OMSTI (INPTIM(3))
            CALL OMSTI (INPTIM(4))
            CALL OMSTI (INPTIM(5))
            CALL OMSTI (INPTIM(6))
            CALL OMSTI (NVALS)
            CALL OMSTI (QLFG)
            CALL OMSTR (GAPVAL)
            CALL OMSTI (RETCOD)
            SGRP = 1
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 KNT)
          END IF
C
C         transform and place data values on inpad
          DO 40 J= 1,NVALS
            IF (FSTFG.EQ.0) THEN
C             first data pt not available
              IF (J.EQ.1) THEN
C               this is the first data pt
                IF (STKIND(1).EQ.1.OR.STKIND(1).EQ.3) THEN
C                 point time series, use second pt
                  CURVAL(1)= TBUFF(1)
                ELSE
C                 mean time series, use undefined value
                  CURVAL(1)= -1.0E+30
                END IF
              ELSE
C               all other data pts
                CURVAL(1)= TBUFF(J - 1)
              END IF
            ELSE
C             first data pt was available
              CURVAL(1)= TBUFF(J)
            END IF
C
            IF (LTRNFG.EQ.1.AND.CURVAL(1).GT.-1.E29) THEN
C             a linear transformation is needed
              CALL LTRAN (I1,A,B,
     M                    CURVAL)
            END IF
            PLACE = VOPADR(1) + NPTS
            IF (PLACE.GT.SCRSIZ) THEN
              WRITE (MESSU,*)  ' IN GETWDM: PLACE>SCRSIZ'
              PLACE = SCRSIZ
            END IF
            IF (INMODE(1).EQ.1) THEN
C             add values to current contents of row
              PAD(PLACE) = PAD(PLACE) + CURVAL(1)
              IF (BRBFG.GE.2) WRITE (MESSU,2300) PLACE,PAD(PLACE)
 2300         FORMAT (' WDM PLACE, PAD(PLACE):  ',I10,E12.4)
            ELSE
C             replace current contents of row with values
              PAD(PLACE) = CURVAL(1)
              IF (BRBFG.GE.2) WRITE (MESSU,2300) PLACE,PAD(PLACE)
            END IF
            NPTS = NPTS + 1
 40       CONTINUE
C
C         increment time to start of next set of values
          CALL TIMADD (INPTIM,I2,DELT,NVALS,
     O                 INPTIM)
C
        END IF
C
 60   CONTINUE
C
C     increment time to start of new inspan and update instruction
      NIVLS = WIDTH - 1
      CALL TIMADD (INPDAT,I2,DELT,NIVLS,
     O             INPDAT)
C
      IF (BRBFG .GE. 1) WRITE (MESSU,3010) NGETS,NVALS,NPTS
C
      RETURN
      END
