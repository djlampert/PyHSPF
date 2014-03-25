C
C     4.3.2.1
C
      SUBROUTINE   AGGWDS
     I                    (STTRAN,AGGVLS,BUFFIN,
     M                     BUFOUT)
C
C     + + + PURPOSE + + +
C     Handle various aggregation cases
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   STTRAN,AGGVLS
      REAL      BUFFIN,BUFOUT
C
C     + + + ARGUMENT DEFINITIONS + + +
C     STTRAN - ???
C     AGGVLS - ???
C     BUFFIN - ???
C     BUFOUT - ???
C
C     + + + INTRINSICS + + +
      INTRINSIC  FLOAT,AMAX1,AMIN1
C
C     + + + END SPECIFICATIONS + + +
C
      IF (STTRAN .EQ. 3) THEN
C       average
        BUFOUT = BUFOUT + BUFFIN/FLOAT(AGGVLS)
      ELSE IF (STTRAN .EQ. 4) THEN
C       sum
        BUFOUT = BUFOUT + BUFFIN
      ELSE IF (STTRAN .EQ. 5) THEN
C       maximum
        BUFOUT = AMAX1(BUFOUT,BUFFIN)
      ELSE IF (STTRAN .EQ. 6) THEN
C       minimum
        BUFOUT = AMIN1(BUFOUT,BUFFIN)
      ELSE IF (STTRAN .EQ. 8) THEN
C       last
        BUFOUT = BUFFIN
      END IF
C
      RETURN
      END
C
C     4.3.2
C
      SUBROUTINE   PUTWDM
     I                    (DELT,WIDTH,WDMSFL,FSTCAL,LSTCAL)
C
C     + + + PURPOSE + + +
C     Get a row of data from the pad, transform the data, and
C     put values in wdm file
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   DELT,WIDTH,WDMSFL,FSTCAL,LSTCAL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DELT   - simulation time interval in minutes
C     WIDTH  - inpad width
C     WDMSFL - watershed data management file unit number
C     FSTCAL - flag indicating first interval of run
C     LSTCAL - flag indicating last interval of run
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PUTCOM + + +
      INCLUDE   'ctsin.inc'
      INCLUDE   'ctsex.inc'
      INCLUDE   'ctser.inc'
      INCLUDE   'ctsbu.inc'
      INCLUDE   'ctsbx.inc'
      INCLUDE   'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,I1,I2,K,INPTIM(6),INPEND(6),NVALS,
     $          RETCOD,DTOVWR,SCLU,SGRP,
     $          AGGNUM,IVL,IVL1,IAGG,CURAGG,BRBFG
      REAL      INITVL(8),CURVAL
C
C     + + + EXTERNALS + + +
      EXTERNAL    AGGWDS,LTRAN,WDTPUT,OMSTI,OMSG,TIMADD
C
C     + + + OUTPUT FORMATS + + +
 3000 FORMAT (/,' ENTERING PUTWDM')
 3010 FORMAT (/,' LEAVING PUTWDM',
     $        '   NPUTS=',I6,'; NVALS=',I6,'; NPTS=',I6)
 3020 FORMAT (3X,'INPAD DELTA TIME=',I6,3X, 'WDM DELTAT TIME=',I6,
     $        3X,'WIDTH=',I12,/,3X,
     $        'INPAD START TIME (MIN)=',I10,/,3X,
     $        'INPAD START TIME (YR,MO,DY,ETC)=',6I6)
 3030 FORMAT (3X,'VOPADR=',I6,3X,'OFFSET=',I6,3X,'STKIND=',I6,/,
     $        3X,'STTRAN=',I6,3X,'INMODE=',I6,/,3X,'A=',E18.6,6X,
     $        'B=',E18.6)
 3040 FORMAT (' PUTWDM:  PLACE,PAD(PLACE):',I10,E12.4)
C
C     + + + DATA INITIALIZATIONS + + +
      DATA      INITVL/-999.,-999.,0.0,0.0,-1.0E30,1.0E30,-999.,0.0/
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU = 236
      BRBFG= 0
C
      IF (BRBFG .GE. 1) THEN
        WRITE (MESSU,3000)
        WRITE (MESSU,3020) DELT,DELTAT,WIDTH,INPSTR,INPDAT
        WRITE (MESSU,3030) VOPADR(1),OFFSET(1), STKIND(1),
     $                     STTRAN(1),INMODE(1), A(1),B(1)
      END IF
C
C     set constant values and values based on instruction
      I1     = 1
      I2     = 2
      DTOVWR = 0
      IF (AMODE .EQ. 3) DTOVWR = 1
C
C     initialize inpad time to start time for inpad
      DO 30 I= 1, 6
        INPTIM(I) = INPDAT(I)
 30   CONTINUE
C
      IAGG = 1
      IVL1 = 1
      IF (FSTCAL .EQ. 1) PVAR(1) = INITVL(STTRAN(1))
      AGGNUM = DELTAT/DELT
      CURAGG = AGGNUM - NREM
      IF ((AGGNUM .EQ. 1) .OR. (NREM .EQ. 0)) CURAGG = 0
C     loop thru width
      DO 100 IVL=1, (WIDTH - 1)
        IVL1   = IVL1 + 1
        CURAGG = CURAGG + 1
        IF (AGGRFG .EQ. 1) THEN
C         aggregate this value
          CURVAL = PAD(VOPADR(1) + IVL1)
          CALL AGGWDS
     I                (STTRAN(1),AGGNUM,CURVAL,
     M                 PVAR(1))
        ELSE
C         no aggregation, use value from pad
          PVAR(1) = PAD(VOPADR(1) + IVL1)
          IF (BRBFG .GE. 2) WRITE (MESSU,3040) VOPADR(1)+IVL1,
     $                             PAD(VOPADR(1)+IVL1)
        END IF
C
        IF ((CURAGG .EQ. AGGNUM) .OR.
     $      (IVL .EQ. (WIDTH - 1) .AND. LSTCAL .EQ. 1)) THEN
C         end of aggregation, transform if necessary and write to wdm
C         file if end of large group
          IF (LTRNFG .EQ. 1) CALL LTRAN (I1,A,B,  PVAR(1))
          TBUFF(IAGG) = PVAR(1)
 2900     format(1x,a,20i5)
C         write(99,2900) 'PUTWDM:iagg,width,ivl,aggnum',
C    $                         iagg,width,ivl,aggnum
          IF ((IAGG .EQ. 500) .OR. ((WIDTH-1-IVL) .LT. AGGNUM)) THEN
C           group of 500 values (or smaller group if almost at end of
C           inspan) is ready to write to wdm file
C            write(99,2900) 'PUTWDM:wdmsfl,file,inptim,nvals,i2,dtovwr',
C     $                             wdmsfl,file,inptim,nvals,i2,dtovwr
            IF (IAGG .GT. 0) THEN
C             put values in wdm file
              NVALS = IAGG
              CALL WDTPUT
     I                    (WDMSFL,FILE,DELTAT,INPTIM,NVALS,DTOVWR,QLFG,
     I                     I2,TBUFF,
     O                     RETCOD)
C
              IF (RETCOD .NE. 0) THEN
C                write(99,2900) '      :retcod',retcod
                CALL OMSTI (FILE)
                CALL OMSTI (INPTIM(1))
                CALL OMSTI (INPTIM(2))
                CALL OMSTI (INPTIM(3))
                CALL OMSTI (INPTIM(4))
                CALL OMSTI (INPTIM(5))
                CALL OMSTI (INPTIM(6))
                CALL OMSTI (DELTAT)
                CALL OMSTI (NVALS)
                CALL OMSTI (DTOVWR)
                CALL OMSTI (RETCOD)
                SGRP = 1
                CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                     KNT)
              END IF
C
C             increment time to start of next set of values
              CALL TIMADD
     I                    (INPTIM,I2,DELTAT,NVALS,
     O                     INPEND)
              DO 50 K= 1, 6
                INPTIM(K) = INPEND(K)
 50           CONTINUE
            END IF
C           initialize index for next group of 500 aggregated values
            IAGG = 0
          END IF
C         increment index for group of 500 values
          IAGG = IAGG + 1
C         initialize index and value for next aggregated value
          PVAR(1) = INITVL(STTRAN(1))
          CURAGG  = 0
        END IF
 100  CONTINUE
      NREM = AGGNUM - CURAGG
C
      DO 110 I=1, 6
        INPDAT(I) = INPTIM(I)
 110  CONTINUE
C
      IF (BRBFG .GE. 1) WRITE (MESSU,3010)
C
C
      RETURN
      END
