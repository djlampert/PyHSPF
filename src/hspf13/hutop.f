C
C
C
      SUBROUTINE   COPY
     I                  (STIVL,WIDTH)
C
C     + + + PURPOSE + + +
C     Copy a set of point- and/or mean-valued time series
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    STIVL,WIDTH
C
C     + + + ARGUMENT DEFINITIONS + + +
C     STIVL  - in inpad row
C     WIDTH  - of inpad row
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION COPY2 + + +
      INCLUDE    'ccopy.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     IVL,IVL1,N,IDELT,PIVLNO,DUM1,DUM2,DUM3,DUM4,
     #            DUM5,DUM6,I1,I12
      CHARACTER*6 OPTYP
C
C     + + + EXTERNALS + + +
      EXTERNAL    ADDTIM,SPECL,UPQUAN
C
C     + + + DATA INITIALIZATIONS + + +
      DATA        OPTYP/'COPY  '/
C
C     + + + END SPECIFICATIONS + + +
C
      IVL=  STIVL- 1
      IVL1= STIVL
      IDELT= DELT
      PIVLNO= 0
      I1= 1
      I12= 12
C
      IF ( ( STIVL .EQ. 1) .AND. (NPT.GT.0) ) THEN
C       put initial values of point-valued time series into inpad
C       transfer from input row to temporary array
        DO 20 N= 1,NPT
          IF (IPTFP(N) .GT. 0) THEN
            IF (OPTFP(N) .NE. IPTFP(N)) THEN
              PTVAL(N)= PAD(IPTFP(N)+IVL1)
            END IF
          END IF
 20     CONTINUE
C       transfer from temporary array to output row
        DO 30 N= 1,NPT
          IF ( (OPTFP(N) .GT. 0) .AND. (IPTFP(N) .GT. 0) ) THEN
            IF (OPTFP(N) .NE. IPTFP(N)) THEN
              PAD(OPTFP(N)+IVL1)= PTVAL(N)
            END IF
          END IF
 30     CONTINUE
      END IF
C
C     time loop
      DO 130 IVL= STIVL,WIDTH+ STIVL- 1
        IVL1= IVL1+ 1
        SPIVL= SPIVL+ 1
C
C       increment date/time
        CALL ADDTIM (IDELT,NDAY,I1,I12,
     M               DATIM,PIVLNO,
     O               NDAYS,DUM1,DUM2,DUM3,DUM4,DUM5,DUM6)
C
        IF (SPAFP .GT. 0 .AND. SPAFP .LE. SPAKND) THEN
C         special actions are being taken and there is at least one left
          CALL SPECL (OPTYP,COPYNO,SPAKND,SPOPNO,DATIM,MESSU,SPIVL,
     I                SPOUT,SPNUND,
     M                SPAFP)
        END IF
C
        IF (NPT.GT.0) THEN
C         copy point-valued time series to osv
          DO 40 N= 1,NPT
            IF (IPTFP(N) .GT. 0) THEN
              PTVAL(N)= PAD(IPTFP(N)+IVL1)
            END IF
 40       CONTINUE
        END IF
C
        IF (NMN.GT.0) THEN
C         copy mean-valued time series to osv
          DO 50 N= 1,NMN
            IF (IMNFP(N) .GT. 0) THEN
              MNVAL(N)= PAD(IMNFP(N)+IVL1)
            END IF
 50       CONTINUE
        END IF
C
        IF (NPT.GT.0) THEN
C         copy point-valued time series from osv to output
          DO 60 N= 1,NPT
            IF ( (OPTFP(N) .GT. 0) .AND. (IPTFP(N) .GT. 0) ) THEN
              IF (OPTFP(N) .NE. IPTFP(N)) THEN
                PAD(OPTFP(N)+IVL1)= PTVAL(N)
              END IF
            END IF
 60       CONTINUE
        END IF
C
        IF (NMN.GT.0) THEN
C         copy mean-valued time series from osv to output
          DO 70 N= 1,NMN
            IF ( (OMNFP(N) .GT. 0) .AND. (IMNFP(N) .GT. 0) ) THEN
              IF (OMNFP(N) .NE. IMNFP(N)) THEN
                PAD(OMNFP(N)+IVL1)= MNVAL(N)
              END IF
            END IF
 70       CONTINUE
        END IF
C
C       update pipes for user-defined variable quantities
        CALL UPQUAN (SPIVL,SPOPNO)
C
 130  CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   MUTSIN
     I                    (STIVL,WIDTH)
C
C     + + + PURPOSE + + +
C     Read a sequential input file containing multiple timeseries for
C     use by HSPF-format of file is same as a HSPF PLOTFL
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   STIVL,WIDTH
C
C     + + + ARGUMENT DEFINITIONS + + +
C     STIVL  - in inpad row
C     WIDTH  - of inpad row
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION MUTSIN2 + + +
      INCLUDE   'cmuts.inc'
      INCLUDE   'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   IDELT,N,EXDAT(5),NC,DUM1,DUM2,DUM3,DUM4,DUM5,DUM6,
     $          SEQDAT(5),EDUM,PYREND,CHKDAT,SCLU,SGRP
      CHARACTER*1 EBUFF(80)
C
C     + + + EXTERNALS + + +
      EXTERNAL  ADDTIM,EXDATE,OMSG,OMSTI,OMSTC
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (1X)
 1010 FORMAT (5X,I5,4I3,20(G14.0))
 1020 FORMAT (80A1)
C
C     + + + END SPECIFICATIONS + + +
C
      IVL  = STIVL- 1
      IVL1 = STIVL
      IDELT= DELT
      SCLU = 370
C
      IF (STFG.EQ.1) THEN
C       start of run
        STFG= 0
        EDUM= 0
C
        IF (NLINES.GT.0) THEN
C         skip n lines at beginning of mutfl
          DO 30 N= 1,NLINES
            READ(MUTFL,1000,END=10)
              GO TO 20
 10         CONTINUE
C             error - end of file reached before data found
              CALL OMSTI (MUTNO)
              CALL OMSTI (MUTFL)
              SGRP= 2
              CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                   EDUM)
 20         CONTINUE
 30       CONTINUE
        END IF
C
C       read first line from mutfl into val(*) to get in pt val stuff
        READ(MUTFL,1010,ERR=40,END=50) SEQDAT,(VAL(N),N=1,NCURV)
          GO TO 60
 40     CONTINUE
C         error - format incorrect for first data line in mutfl
          BACKSPACE MUTFL
          READ (MUTFL,1020) EBUFF
          CALL OMSTI (MUTNO)
          CALL OMSTI (MUTFL)
          N= 80
          CALL OMSTC (N,EBUFF)
          SGRP= 3
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               EDUM)
          GO TO 60
 50     CONTINUE
C         error - end of file reached at first data line in mutfl
          CALL OMSTI (MUTNO)
          CALL OMSTI (MUTFL)
          SGRP= 4
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               EDUM)
 60     CONTINUE
      END IF
C
      IF ( (STIVL .EQ. 1) .AND. (NPT.GT.0) ) THEN
C       put values from val(*) into inpad for all point valued timsers
        NC= 0
        DO 70 N= 1,NPT
          NC= NC+ 1
          IF (PTFP(N).GE.1) THEN
C           put value of pad
            PAD(PTFP(N)+ IVL1)= VAL(NC)
          END IF
 70     CONTINUE
      END IF
C
C     time loop
      DO 150 IVL= STIVL,WIDTH+ STIVL- 1
        IVL1= IVL1+ 1
C       increment date/time
        CALL ADDTIM (IDELT,NDAY,PIVL,PYREND,
     M               DATIM,PIVLNO,
     O               NDAYS,DUM1,DUM2,DUM3,DUM4,DUM5,DUM6)
C
C       get date/time to external format
        CALL EXDATE (DATIM,
     O               EXDAT)
C
 80     CONTINUE
          READ (MUTFL,1010,ERR=90,END=100) SEQDAT,(VAL(N),N=1,NCURV)
            GO TO 110
 90       CONTINUE
C           error - format incorrect in mutfl
            BACKSPACE MUTFL
            READ (MUTFL,1020) EBUFF
            CALL OMSTI (MUTNO)
            CALL OMSTI (MUTFL)
            CALL OMSTI (EXDAT(1))
            CALL OMSTI (EXDAT(2))
            CALL OMSTI (EXDAT(3))
            CALL OMSTI (EXDAT(4))
            CALL OMSTI (EXDAT(5))
            N= 80
            CALL OMSTC (N,EBUFF)
            SGRP= 5
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 EDUM)
            GO TO 110
 100      CONTINUE
C           error - end of file reached
            CALL OMSTI (MUTNO)
            CALL OMSTI (MUTFL)
            CALL OMSTI (EXDAT(1))
            CALL OMSTI (EXDAT(2))
            CALL OMSTI (EXDAT(3))
            CALL OMSTI (EXDAT(4))
            CALL OMSTI (EXDAT(5))
            SGRP= 6
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 EDUM)
 110      CONTINUE
C
C         check date
          IF (EXDAT(1).EQ.SEQDAT(1).AND.EXDAT(2).EQ.SEQDAT(2).AND.
     $        EXDAT(3).EQ.SEQDAT(3).AND.EXDAT(4).EQ.SEQDAT(4).AND.
     $        EXDAT(5).EQ.SEQDAT(5)) THEN
C           dates are equal
            CHKDAT= 0
          ELSE IF ( EXDAT(1).LT.SEQDAT(1)                        .OR.
     $        (EXDAT(1).EQ.SEQDAT(1).AND.EXDAT(2).LT.SEQDAT(2))  .OR.
     $        (EXDAT(1).EQ.SEQDAT(1).AND.EXDAT(2).EQ.SEQDAT(2).AND.
     $         EXDAT(3).LT.SEQDAT(3))                            .OR.
     $        (EXDAT(1).EQ.SEQDAT(1).AND.EXDAT(2).EQ.SEQDAT(2).AND.
     $         EXDAT(3).EQ.SEQDAT(3).AND.EXDAT(4).LT.SEQDAT(4))  .OR.
     $        (EXDAT(1).EQ.SEQDAT(1).AND.EXDAT(2).EQ.SEQDAT(2).AND.
     $         EXDAT(3).EQ.SEQDAT(3).AND.EXDAT(4).EQ.SEQDAT(4).AND.
     $         EXDAT(5).LT.SEQDAT(5))) THEN
C           date from file is greater than internal one
            CHKDAT= 1
          ELSE
C           date from file is less than internal date
            CHKDAT= -1
          END IF
C         loop back to read next date from file
        IF (CHKDAT.EQ.-1) GO TO 80
C
        IF (CHKDAT.EQ.1) THEN
C         values missing from file
          IF (MISSFG.NE.0) THEN
C           fill missing value
            BACKSPACE MUTFL
            IF (MISSFG.NE.3) THEN
              DO 120 N= 1,NCURV
                VAL(N)= FILVAL
 120          CONTINUE
            END IF
          ELSE
C           bad date-print error and quit
            CALL OMSTI (MUTNO)
            CALL OMSTI (MUTFL)
            CALL OMSTI (SEQDAT(1))
            CALL OMSTI (SEQDAT(2))
            CALL OMSTI (SEQDAT(3))
            CALL OMSTI (SEQDAT(4))
            CALL OMSTI (SEQDAT(5))
            CALL OMSTI (EXDAT(1))
            CALL OMSTI (EXDAT(2))
            CALL OMSTI (EXDAT(3))
            CALL OMSTI (EXDAT(4))
            CALL OMSTI (EXDAT(5))
            SGRP = 1
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 EDUM)
          END IF
        END IF
C
        NC= 0
C
        IF (NPT.GT.0) THEN
C         process point data
          DO 130 N= 1,NPT
            NC= NC+ 1
            IF (PTFP(N).GE.1) THEN
              PAD(PTFP(N)+ IVL1)= VAL(NC)
            END IF
 130      CONTINUE
        END IF
C
        IF (NMN.GT.0) THEN
C         process mean data
          DO 140 N= 1,NMN
            NC= NC+ 1
            IF (MNFP(N).GE.1) THEN
              PAD(MNFP(N)+ IVL1)= VAL(NC)
            END IF
 140      CONTINUE
        END IF
 150  CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   HDISPL
     I                   (STIVL,WIDTH,LSTCAL)
C
C     + + + PURPOSE + + +
C     display a time series in tabular form
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   STIVL,WIDTH,LSTCAL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     STIVL  - in inpad row
C     WIDTH  - of inpad row
C     LSTCAL - flag indicating last interval of run
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION DISPLY2 + + +
      INCLUDE   'cdisp.inc'
      INCLUDE   'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   IDELT,NDATE,NROWS,I,J,K,COL,DPIVL
      REAL      VAL
C
C     + + + EXTERNALS + + +
      EXTERNAL  ADDTIM,TRANS,SHDISP,YRDISP
C
C     + + + END SPECIFICATIONS + + +
C
      IVL  = STIVL- 1
      IVL1 = STIVL
      IDELT= DELT
C
C     dummy pivl, so pivlno can't overflow in subroutine addtim
      DPIVL= 999
C
C     time loop
      DO 130 IVL= STIVL,WIDTH+ STIVL- 1
        IVL1= IVL1+ 1
C       increment time and set time-related flags
        CALL ADDTIM (IDELT,NDAY,DPIVL,PYREND,
     M               DATIM,PIVLNO,
     O               NDAYS,NXTMON,HRFG,DAYFG,EDAYFG,EMONFG,EPYRFG)
C
        IF (IFP.GE.1) THEN
C         get and linearly transform the data
          VAL= A* PAD(IFP+IVL1)+ B
        ELSE
          VAL= 0.0
        END IF
C
        IF (PSHFG.GT.0) THEN
C         determine element of data(*,*) with which we are now working
          IF (PSHFG.EQ.1) THEN
            I= ((MIN-1)/PDELT)+ 1
            J= HR
          ELSE
            I= (((HR-1)*60+ MIN- 1)/PDELT)+ 1
            J= DAY
          END IF
          CALL TRANS (TRNCOD,PIVL,VAL,
     M                DATA(I,J))
        END IF
C
        IF (PYRFG.GT.0) THEN
C         transform data to annual summary (daily) interval
          K= MON- PYREND
          IF (K.LE.0) THEN
            COL= K+ 12
          ELSE
            COL= K
          END IF
          CALL TRANS (TRNCOD,IVLDAY,VAL,
     M                DYVAL(DAY,COL))
        END IF
C
        IF (EDAYFG.EQ.1) THEN
C         last interval in the day
          IF (PSHFG.EQ.1) THEN
            NDATE= 3
            NROWS= 24
            CALL SHDISP (HRSPAN,DASPAN,NDATE,NROWS,FILE1,TITLE,DATIM,
     $                   PDELT,TRAN,NIVL,TRNCOD,FMT1,STVAL,THRSH1,
     M                   DATA)
          END IF
C
          IF (EMONFG.EQ.1) THEN
C           last interval in the month
            IF (PSHFG.EQ.2) THEN
              NDATE= 2
              NROWS= NDAYS
              CALL SHDISP (DASPAN,MNSPAN,NDATE,NROWS,FILE1,TITLE,
     $                     DATIM,PDELT,TRAN,NIVL,TRNCOD,FMT1,STVAL,
     $                     THRSH1,
     M                     DATA)
            END IF
C
            IF (EPYRFG.EQ.1) THEN
C             last interval in printout year
              IF (PYRFG.EQ.1) THEN
                NDATE= 2
                CALL YRDISP (NDATE,FILE2,TITLE,DATIM,TRAN,TRNCOD,
     $                       PYREND,FMT2,FMT3,FMT4,FMT5,STVAL,
     $                       MONTHS,NDAY,BLANKR,
     M                       DYVAL)
              END IF
            END IF
          END IF
        END IF
 130  CONTINUE
C
      IF (LSTCAL.EQ.1) THEN
C       this is the end of the run, dump any partly completed tables
        IF (PSHFG.GT.0) THEN
          IF (PSHFG.EQ.1) THEN
            IF (EDAYFG.NE.1) THEN
              NDATE= 3
              NROWS= 24
              CALL SHDISP (HRSPAN,DASPAN,NDATE,NROWS,FILE1,TITLE,
     $                     DATIM,PDELT,TRAN,NIVL,TRNCOD,FMT1,STVAL,
     $                     THRSH1,
     M                     DATA)
            END IF
          ELSE
            IF (EMONFG.NE.1) THEN
              NDATE= 2
              NROWS= NDAYS
              CALL SHDISP (DASPAN,MNSPAN,NDATE,NROWS,FILE1,TITLE,
     $                     DATIM,PDELT,TRAN,NIVL,TRNCOD,FMT1,STVAL,
     $                     THRSH1,
     M                     DATA)
            END IF
          END IF
        END IF
C
        IF (PYRFG.EQ.1) THEN
          IF (EPYRFG.NE.1) THEN
            NDATE= 2
            CALL YRDISP (NDATE,FILE2,TITLE,DATIM,TRAN,TRNCOD,PYREND,
     $                   FMT2,FMT3,FMT4,FMT5,STVAL,MONTHS,NDAY,
     $                   BLANKR,
     M                   DYVAL)
          END IF
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   SHDISP
     I                   (SPAN1,SPAN2,NDATE,NROWS,FILE1,TITLE,DATIM,
     $                    PDELT,TRAN,NIVL,TRNCOD,FMT1,STVAL,THRSH1,
     M                    DATA)
C
C     + + + PURPOSE + + +
C     produce a short-span data summary
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   NDATE
      INTEGER   TITLE(7),DATIM(NDATE),SPAN1(2),SPAN2(2),TRAN(1),
     $          FMT1(16),FILE1,PDELT,NROWS,NIVL,TRNCOD
      REAL      DATA(60,31),STVAL,THRSH1
C
C     + + + ARGUMENT DEFINITIONS + + +
C     SPAN1  - ???
C     SPAN2  - ???
C     NDATE  - ???
C     NROWS  - ???
C     FILE1  - ???
C     TITLE  - ???
C     DATIM  - date and time of day
C     PDELT  - ???
C     TRAN   - ???
C     NIVL   - ???
C     TRNCOD - ???
C     FMT1   - ???
C     STVAL  - ???
C     THRSH1 - ???
C     DATA   - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,I5,J,K,PFG
      REAL        ROWV(31),TABV
      CHARACTER*4 FMTDUM(16)
C
C     + + + EXTERNALS + + +
      EXTERNAL    AGGREG
C
C     + + + INTRINSICS + + +
      INTRINSIC   MOD
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT('1',/,41X,8A4)
 2010 FORMAT(    /,41X,'Summary for ',A4,A2,' ',I4,2('/',I2))
 2020 FORMAT(    /,41X,'Data interval: ',I5,' mins')
 2030 FORMAT(    /,4X,A4,A2,5X,A4,22X,'Interval Number ')
 2040 FORMAT(5(' ',24X,12I8,/))
 2050 FORMAT(' ',A4,A2,' ',A4,':',1PE12.5)
C
C     + + + END SPECIFICATIONS + + +
C
      I5 = 5
      PFG= 0
C
      DO 10 I=1,NROWS
C       aggregate values to the hour or day level
        CALL AGGREG (DATA(1,I),NIVL,STVAL,TRNCOD,
     O               ROWV(I))
        IF (ROWV(I).GT.THRSH1) THEN
C         print the row
          PFG= 1
        END IF
 10   CONTINUE
C
C     get day or month value
      CALL AGGREG (ROWV(1),NROWS,STVAL,TRNCOD,
     O             TABV)
      IF (PFG.EQ.1) THEN
C       display if any row greater than threshold
        WRITE (FILE1,2000) TITLE
        WRITE (FILE1,2010) SPAN2, DATIM
        WRITE (FILE1,2020) PDELT
        WRITE (FILE1,2030) SPAN1, TRAN(1)
        WRITE (FILE1,2040) (I,I=1,NIVL)
        WRITE (FILE1,2040)
C
        DO 15 I=1, 16
           WRITE(FMTDUM(I),'(A4)')FMT1(I)
 15     CONTINUE
C
        DO 30 I= 1,NROWS
          IF (ROWV(I).GT.THRSH1) THEN
C           row above threshold, display it
            IF (PDELT.LT.60) THEN
C             for an hourly row value, label with 0 through 23
              K= I-1
            ELSE
              K= I
            END IF
            WRITE  (FILE1,FMTDUM) K,ROWV(I),(DATA(J,I),J=1,NIVL)
            IF ( MOD(I,I5).EQ.0) THEN
C             blank line every 5 rows
              WRITE (FILE1,2040)
            END IF
          END IF
 30     CONTINUE
C
        WRITE (FILE1,2050) SPAN2,TRAN(1),TABV
      END IF
C
C     reset values in the table to starting value
      DO 60 I=1,31
        DO 50 J=1,60
          DATA(J,I)= STVAL
 50     CONTINUE
 60   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   AGGREG
     I                   (DAT,N,STVAL,TRNCOD,
     O                    DATNEW)
C
C     + + + PURPOSE + + +
C     aggregate an array of data to a larger interval
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   N,TRNCOD
      REAL      DAT(N),STVAL,DATNEW
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DAT    - ???
C     N      - ???
C     STVAL  - ???
C     TRNCOD - ???
C     DATNEW - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I
C
C     + + + INTRINSICS + + +
      INTRINSIC   AMAX1,AMIN1
C
C     + + + END SPECIFICATIONS + + +
C
C     initialize accumulator
      DATNEW= STVAL
C
C     casentry trncod
      GO TO (10,30,50,70,90),TRNCOD
C
 10   CONTINUE
C       case 1      sum
        DO 20 I=1,N
          DATNEW= DATNEW+ DAT(I)
 20     CONTINUE
        GO TO 100
C
 30   CONTINUE
C       case 2      aver
        DO 40 I=1,N
          DATNEW= DATNEW+ DAT(I)
 40     CONTINUE
        DATNEW= DATNEW/N
        GO TO 100
C
 50   CONTINUE
C       case 3      max
        DO 60 I=1,N
          DATNEW= AMAX1(DATNEW,DAT(I))
 60     CONTINUE
        GO TO 100
C
 70   CONTINUE
C       case 4      min
        DO 80 I=1,N
          DATNEW= AMIN1(DATNEW,DAT(I))
 80     CONTINUE
        GO TO 100
C
 90   CONTINUE
C       case 5      last
        DATNEW= DAT(N)
        GO TO 100
C
C       endcase
 100  CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   TRANS
     I                  (TRNCOD,NIVLS,VAL,
     M                   DVAL)
C
C     + + + PURPOSE + + +
C     transform data from basic interval to display interval
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   TRNCOD,NIVLS
      REAL      VAL,DVAL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     TRNCOD - ???
C     NIVLS  - ???
C     VAL    - ???
C     DVAL   - ???
C
C     + + + INTRINSICS + + +
      INTRINSIC   AMAX1,AMIN1
C
C     + + + END SPECIFICATIONS + + +
C
      IF (TRNCOD .EQ. 1) THEN
C       case 1    sum
        DVAL= DVAL+ VAL
      ELSE IF (TRNCOD .EQ. 2) THEN
C       case 2    aver
        DVAL= DVAL+ (VAL/NIVLS)
      ELSE IF (TRNCOD .EQ. 3) THEN
C       case 3    max
        DVAL= AMAX1(DVAL,VAL)
      ELSE IF (TRNCOD .EQ. 4) THEN
C       case 4    min
        DVAL= AMIN1(DVAL,VAL)
      ELSE IF (TRNCOD .EQ. 5) THEN
C       case 5    last
        DVAL= VAL
      ELSE IF (TRNCOD .EQ. 6) THEN
C       case 1    pct - treat as sum
        DVAL= DVAL+ VAL
      END IF
C
      RETURN
      END
C
C     4.2(13).3
C
      SUBROUTINE   YRDISP
     I                    (NDATE,FILE2,TITLE,DATIM,TRAN,TRNCOD,PYREND,
     $                     FMT2,FMT3,FMT4,FMT5,STVAL,MONTHS,NDAY,BLANKR,
     M                     DYVAL)
C
C     + + + PURPOSE + + +
C     produce a long-span (yearly) data summary
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     NDATE
      INTEGER     FILE2,DATIM(NDATE),TITLE(7),TRNCOD,MONTHS(2,12),
     $            FMT2(30),FMT3(30),FMT4(30),FMT5(30),NDAY(12),TRAN(1),
     $            PYREND
      REAL        DYVAL(31,12),BLANKR,STVAL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NDATE  - ???
C     FILE2  - ???
C     TITLE  - ???
C     DATIM  - date and time of day
C     TRAN   - ???
C     TRNCOD - ???
C     PYREND - ???
C     FMT2   - ???
C     FMT3   - ???
C     FMT4   - ???
C     FMT5   - ???
C     STVAL  - ???
C     MONTHS - ???
C     NDAY   - ???
C     BLANKR - ???
C     DYVAL  - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     COL,DA,FEB,FEBCOL,FEBDAY,FEBYR,I5,K,MO,NDA4,NMO4,Y,YR,
     $            I,J
      CHARACTER*4 FMTDUM(30)
      REAL        MOVAL(12),YRVAL
C
C     + + + FUNCTION + + +
      INTEGER     DAYMNH
C
C     + + + EXTERNALS + + +
      EXTERNAL    AGGREG,SETVEC,DAYMNH
C
C     + + + INTRINSICS + + +
      INTRINSIC   MOD
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT('1',/////,37X,8A4)
 2010 FORMAT(/,26X,'Annual data display: Summary for period ending ',
     $       I4,'/',I2)
 2020 FORMAT(/,4X,'Day ',2X,24A4)
 2030 FORMAT(' ')
 2040 FORMAT(/,'  ',A4,' of monthly values ',1PE12.5)
C
C     + + + END SPECIFICATIONS + + +
C
      I5= 5
C
      WRITE (FILE2,2000) TITLE
      WRITE (FILE2,2010) DATIM
      WRITE (FILE2,2020) MONTHS
C     write a blank line
      WRITE (FILE2,2030)
      YR= DATIM(1)
C
      DO 5 I=1, 30
         WRITE(FMTDUM(I),'(A4)')FMT2(I)
 5    CONTINUE
C
C     write out data for first 25 days, in 5 groups of 5
      DO 10 DA= 1,25
        WRITE (FILE2,FMTDUM) DA,(DYVAL(DA,J),J=1,12)
        IF ( MOD(DA,I5).EQ.0) WRITE (FILE2,2030)
 10   CONTINUE
C
C     find out how many days there were in february
      K= 2- PYREND
      IF (K.GT.0) GO TO 20
        FEBYR = YR
        FEBCOL= K+ 12
        GO TO 30
 20   CONTINUE
        FEBYR = YR- 1
        FEBCOL= K
 30   CONTINUE
C
      FEB   = 2
      FEBDAY= DAYMNH (FEBYR,FEB,NDAY)
      DO 40 DA= 26,FEBDAY
        WRITE (FILE2,FMTDUM) DA,(DYVAL(DA,J),J=1,12)
 40   CONTINUE
C
C     take care of feb 29
      K= FEBDAY+ 1
      IF (FEBDAY.EQ.28) DYVAL(29,FEBCOL)= BLANKR
C
      DO 45 I=1, 30
         WRITE(FMTDUM(I),'(A4)')FMT3(I)
 45   CONTINUE
C
      DO 50 DA= K,30
        WRITE (FILE2,FMTDUM) DA,(DYVAL(DA,J),J=1,12)
 50   CONTINUE
C
      DO 55 I=1, 30
         WRITE(FMTDUM(I),'(A4)')FMT4(I)
 55   CONTINUE
C
      DA= 31
      WRITE (FILE2,FMTDUM) DA,(DYVAL(DA,J),J=1,12)
C
C     aggregate data for each month of the printout year, print
C     monthly values and reset to initial values
      DO 80 COL= 1,12
        K= COL+ PYREND
        IF (K.GT.12) GO TO 60
          MO= K
          Y = YR- 1
          GO TO 70
 60     CONTINUE
          MO= K- 12
          Y = YR
 70     CONTINUE
C
        NDA4= DAYMNH (Y,MO,NDAY)
        CALL AGGREG (DYVAL(1,COL),NDA4,STVAL,TRNCOD,
     O               MOVAL(COL))
C       reset
        CALL SETVEC (NDA4,STVAL,
     O               DYVAL(1,COL))
C       reset feb 29 as well, regardless of year
        IF (NDA4.EQ.28.OR.NDA4.EQ.29) DYVAL(29,COL)= STVAL
 80   CONTINUE
C
      DO 85 I=1, 30
         WRITE(FMTDUM(I),'(A4)')FMT5(I)
 85   CONTINUE
C
      WRITE (FILE2,FMTDUM) TRAN(1),MOVAL
C
C     aggregate to the yearly level and printout
      NMO4= 12
      CALL AGGREG (MOVAL,NMO4,STVAL,TRNCOD,
     O             YRVAL)
      WRITE (FILE2,2040) TRAN(1),YRVAL
C
      RETURN
      END
C
C     4.2(15)
C
      SUBROUTINE   GENER
     I                   (STIVL,WIDTH)
C
C     + + + PURPOSE + + +
C     perform transgeneration
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   STIVL,WIDTH
C
C     + + + ARGUMENT DEFINITIONS + + +
C     STIVL  - in inpad row
C     WIDTH  - of inpad row
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION GENER2 + + +
      INCLUDE   'cgene.inc'
      INCLUDE   'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,UNDF,IDELT,L1,L12,DUM1,DUM2,DUM3,
     $            DUM4,DUM5,DUM6,PIVLNO,REQFG,TSSUB(2)
      REAL        VALONE,VALTWO,RESULT
      CHARACTER*6 OPTYP,TSNAM,OPFGNM,DUMNAM
C
C     + + + INTRINSICS + + +
      INTRINSIC   ABS,SQRT,AINT,ALOG,ALOG10,SIN,COS,TAN
C
C     + + + EXTERNALS + + +
      EXTERNAL    ADDTIM,SPECL,UPQUAN,HREQTS
C
C     + + + DATA INITIALIZATIONS + + +
      DATA        TSSUB/1,1/
      DATA        OPTYP,OPFGNM/'GENER ','OPCODE'/
C
C     + + + END SPECIFICATIONS + + +
C
      IVL1=   STIVL
      IDELT=  DELT
      PIVLNO= 0
      L1=     1
      L12=    12
C
C     time loop
      DO 330 IVL= STIVL,WIDTH+ STIVL- 1
        IVL1= IVL1+ 1
        SPIVL= SPIVL+ 1
C
C       increment date/time
        CALL ADDTIM (IDELT,NDAY,L1,L12,
     M               DATIM,PIVLNO,
     O               NDAYS,DUM1,DUM2,DUM3,DUM4,DUM5,DUM6)
C
        IF (SPAFP .GT. 0 .AND. SPAFP .LE. SPAKND) THEN
C         special actions are being taken and there is at least one left
          CALL SPECL (OPTYP,GENRNO,SPAKND,SPOPNO,DATIM,MESSU,SPIVL,
     I                SPOUT,SPNUND,
     M                SPAFP)
        END IF
C
        IF (OPCODE .EQ. 24) THEN
C         no input timeseries required
          UNDF= 0
        ELSE
C         check to see if timeseries are available and have defined values
          REQFG= 6
          TSNAM= 'ONE   '
          CALL HREQTS (DSFP1,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,GENRNO,
     I                 TSNAM,TSSUB,DUMNAM,DUMNAM,OPFGNM,OPCODE,
     O                 VALONE)
          IF (VALONE.LE.-1.0E29) THEN
C           undefined value for first operand
            UNDF= 1
          ELSE
C           first operand is defined
            IF ( (OPCODE .GE. 16) .AND. (OPCODE .LE. 23) ) THEN
C             operator is binary
              REQFG= 6
              TSNAM= 'TWO   '
              CALL HREQTS (DSFP2,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,
     I                     GENRNO,TSNAM,TSSUB,DUMNAM,DUMNAM,OPFGNM,
     I                     OPCODE,
     O                     VALTWO)
              IF (VALTWO.LE.-1.0E29) THEN
C               undefined value for second operand
                UNDF= 1
              ELSE
C               second operand is defined
                UNDF= 0
              END IF
            ELSE
C             no second operator, so all are defined
              UNDF= 0
            END IF
          END IF
        END IF
C
        IF (UNDF .EQ. 0) THEN
          RESULT= -1.0E30
C         casentry opcode
          GO TO (10,20,30,40,50,60,70,80,90,100,110,120,130,140,
     $    150,160,170,180,190,200,210,220,230,240,250,260), OPCODE
C           case 1: c= abs(a)
 10           RESULT= ABS(VALONE)
              GO TO 300
C           case 2: c= sqrt(a)
 20           IF (VALONE.GE.0) RESULT= SQRT(VALONE)
              GO TO 300
C           case 3: c= trunc(a) 
 30           RESULT= AINT(VALONE)
              GO TO 300
C           case 4: c= ceil(a)
 40           RESULT= AINT(VALONE)
C             if (result.ne.VALONE.and.VALONE.gt.0)
C    $          result= result+ 1
              IF ((ABS(RESULT-VALONE)).GT.1.0E-5.AND.VALONE.GT.0.0)
     $           RESULT= RESULT + 1.0
              GO TO 300
C           case 5: c= floor(a)
 50           RESULT= AINT(VALONE)
C             if (result.ne.VALONE.and.VALONE.lt.0)
C    $          result= result- 1
              IF ((ABS(RESULT-VALONE)).GT.1.0E-5.AND.VALONE.LT.0.0)
     $           RESULT= RESULT - 1.0
              GO TO 300
C           case 6: c= ln(a)
 60           IF (VALONE.GT.0) RESULT= ALOG(VALONE)
              GO TO 300
C           case 7: c= log10(a)
 70           IF (VALONE.GT.0) RESULT= ALOG10(VALONE)
              GO TO 300
C           case 8: c= polynomial in a
 80           RESULT= K(1)
              IF (NTERMS .GE. 2) THEN
C               do rest of series
                DO 85 I= 2,NTERMS
                  RESULT= RESULT+ K(I)*VALONE**(I-1)
 85             CONTINUE
              END IF
              GO TO 300
C           case 9: c= k**a
C             if (k(1).ne.0.0.or.VALONE.gt.0.0) result=k(1)**VALONE
 90           IF ((ABS(K(1))).GT.0.0.OR.VALONE.GT.0.0)
     $           RESULT= K(1)**VALONE
              GO TO 300
C           case 10: c= a**k
C             if (VALONE.ne.0.0.or.k(1).gt.0.0) result=VALONE**k(1)
 100          IF ((ABS(VALONE)).GT.0.0.OR.K(1).GT.0.0)
     $           RESULT= VALONE**K(1)
              GO TO 300
C           case 11: c= a+k
 110          RESULT=VALONE + K(1)
              GO TO 300
C           case 12: c= sin(k)
 120          RESULT=SIN(VALONE)
              GO TO 300
C           case 13: c= cos(k)
 130          RESULT=COS(VALONE)
              GO TO 300
C           case 14: c= tan(k)
 140          RESULT=TAN(VALONE)
              GO TO 300
C           case 15: c= cum(a)
 150          RESULT=SUM + VALONE
              SUM   =RESULT
              GO TO 300
C           case 16: c= a+b
 160          RESULT= VALONE+ VALTWO
              GO TO 300
C           case 17: c= a-b
 170          RESULT= VALONE- VALTWO
              GO TO 300
C           case 18: c= a*b
 180          RESULT= VALONE* VALTWO
              GO TO 300
C           case 19: c= a/b
C             if (VALTWO.ne.0) result= VALONE/VALTWO
 190          IF ((ABS(VALTWO)).GT.0.0) RESULT= VALONE/VALTWO
              GO TO 300
C           case 20: c= max(a,b)
 200          RESULT= VALONE
              IF (VALTWO.GT.RESULT) RESULT= VALTWO
              GO TO 300
C           case 21: c= min(a,b)
 210          RESULT= VALONE
              IF (VALTWO.LT.RESULT) RESULT= VALTWO
              GO TO 300
C           case 22: c= a**b
C             if (VALONE.ne.0.0.or.VALTWO.gt.0.0)
C    $          result=VALONE**VALTWO
 220          IF ((ABS(VALONE)).GT.0.0.OR.
     $            VALTWO.GT.0.0) RESULT= VALONE**VALTWO
              GO TO 300
C           case 23: c= cumulative departure of a below b
 230          RESULT= VALONE- VALTWO- SUM
              IF (RESULT.LT.0.0) THEN
                SUM   = SUM- VALONE+ VALTWO
                RESULT= 0.0
              ELSE
                SUM= 0.0
              END IF
              GO TO 300
C           case 24: c= k
 240          RESULT= K(1)
              GO TO 300
C           case 25: c= max(a,k)
 250          RESULT= K(1)
              IF (VALONE .GT. RESULT) THEN
                RESULT= VALONE
              END IF
              GO TO 300
C           case 26: c= min(a,k)
 260          RESULT= K(1)
              IF (VALONE .LT. RESULT) THEN
                RESULT= VALONE
              END IF
              GO TO 300
C
C         end case
 300      CONTINUE
C
        ELSE
C         result is undefined
          RESULT= -1.0E30
        END IF
C        
        IF (DSFP3.GT.0) PAD(DSFP3+ IVL1)= RESULT
C
C       update pipes for user-defined variable quantities
        CALL UPQUAN (SPIVL,SPOPNO)
C
 330  CONTINUE
C
      RETURN
      END
C
C     4.2(12).1
C
      SUBROUTINE   PLTAGG
     I                    (DATIM,NDAYS,PYREND,PIVL,IDELT,
     O                     PIVLTM)
C
C     + + + PURPOSE + + +
C     Calculate number of intervals in this month or year
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   DATIM(5),NDAYS(12),PYREND,PIVL,IDELT,PIVLTM
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DATIM  - date and time of day
C     NDAYS  - no. of days in this month
C     PYREND - ???
C     PIVL   - ???
C     IDELT  - ???
C     PIVLTM - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   TMPTIM(5)
C
C     + + + FUNCTIONS + + +
      INTEGER   DAYMNH
C
C     + + + EXTERNALS + + +
      EXTERNAL  DAYMNH,DIFTIM
C
C     + + + END SPECIFICATIONS + + +
C
      TMPTIM(4)= 24
      TMPTIM(5)= 60
C
      TMPTIM(1)= DATIM(1)
      IF (PIVL.EQ.-1) GOTO 103
        IF (PYREND.LT.DATIM(2)) TMPTIM(1)= TMPTIM(1)+ 1
        TMPTIM(2)= PYREND
        GOTO 105
 103  CONTINUE
        TMPTIM(2)= DATIM(2)
 105  CONTINUE
      TMPTIM(3)= DAYMNH(TMPTIM(1),TMPTIM(2),NDAYS)
      CALL DIFTIM (DATIM,TMPTIM,NDAYS,
     O             PIVLTM)
C     check to see if we are at the end now
      IF (PIVLTM.GT.0) GO TO 140
C       yes, find next end
        IF (PIVL.NE.-1) GO TO  120
          TMPTIM(2)= TMPTIM(2)+ 1
          IF (TMPTIM(2).NE.13) GO TO 115
            TMPTIM(1)= TMPTIM(1)+ 1
            TMPTIM(2)= 1
 115      CONTINUE
          GO TO 130
 120    CONTINUE
          TMPTIM(1)= TMPTIM(1)+ 1
 130    CONTINUE
        TMPTIM(3)= DAYMNH(TMPTIM(1),TMPTIM(2),NDAYS)
        CALL DIFTIM (DATIM,TMPTIM,NDAYS,
     O               PIVLTM)
 140  CONTINUE
      PIVLTM= PIVLTM/IDELT
C
      RETURN
      END
C
C     4.2(12)
C
      SUBROUTINE   PLTGEN
     I                    (STIVL,WIDTH,LSTCAL)
C
C     + + + PURPOSE + + +
C     Prepare a set of point- and/or mean-valued time series for
C     display on an incremental plotter
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   STIVL,WIDTH,LSTCAL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     STIVL  - in inpad row
C     WIDTH  - of inpad row
C     LSTCAL - flag indicating last interval of run
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PLTGEN2 + + +
      INCLUDE   'cpltg.inc'
      INCLUDE   'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     IDELT,N,EXDAT(6),NC,DUM1,DUM2,DUM3,DUM4,EMONFG,EPYRFG,
     $            TRNCOD,PDELT,PLTNOW,I,NBLNK,ERRFLG,SCLU,SGRP,
     $            NULSDT(5),NULEDT(5)
      REAL        VALNOW
      DOUBLE PRECISION JTIME,DYFRAC
      CHARACTER*6  OPTYP
C
C     + + + EXTERNALS + + +
      EXTERNAL    EXDATE,PLTAGG,SPECL,ADDTIM,TRANS,UPQUAN,OMSTI,OMSG,
     $            MJDATE,ZIPR,TIMCVT
C
C     + + + DATA INITIALIZATIONS + + +
      DATA        OPTYP/'PLTGEN'/
      DATA        NULSDT,NULEDT/1859,1,1,0,0,1859,1,3,0,0/
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (A4,1X,'HSPF FILE FOR DRIVING SEPARATE PLOT PROGRAM',/,
     $        A4,1X,'Time interval:',I5,' mins',10X,
     $        'Last month in printout year:',I3,/,
     $        A4,1X,'No. of curves plotted:  Point-valued:',I3,
     $        '   Mean-valued:',I3,'   Total',I3)
 2010 FORMAT (A4,1X,'Label flag:',I3,10X,'Pivl:',I5,10X,'Idelt:',I5)
 2020 FORMAT (A4,1X,'Label flag:',I3,10X,'Pivl:',5X,'Month',5X,'Idelt:',
     $        I5)
 2030 FORMAT (A4,1X,'Label flag:',I3,10X,'Pivl:',5X,'Year ',5X,'Idelt:',
     $        I5)
 2040 FORMAT (A4,1X,'Plot title:   ',10A4,/,
     $        A4,1X,'Y-axis label: ',5A4,/,
     $        A4,1X,'Scale info:  Ymin: ',G12.5,10X,' Threshold:',G12.5,
     $        /,A4,1X,'             Ymax: ',G12.5,/,
     $        A4,1X,'             Time: ',G12.5,' intervals/inch')
 2050 FORMAT (A4,1X,'Data for each curve (Point-valued first,',
     $        ' then mean-valued):',/,
     $        A4,1X,'Label                   LINTYP     INTEQ',
     $        '    COLCOD      TRAN   TRANCOD')
 2060 FORMAT (A4,1X,4A4,4X,3I10,6X,A4,I10)
 2070 FORMAT (A4)
 2080 FORMAT (A4,1X,'Time series (pt-valued, then mean-valued):',/,
     $        A4,/,
     $        A4,1X,'Date/time                      Values',/,
     $        A4)
 2090 FORMAT (A4,1X,I5,4I3,20(1PG14.7))
C
C     + + + END SPECIFICATIONS + + +
C
C     Record length of plotfiles
C
C       #curves     lrecl
C          1          80 (36)
C          2          80 (50)
C          3          80 (64)
C          4          80 (78)
C          5          92
C          6         106
C          7         120
C          8         134
C          9         148
C         10         162
C         11         176
C         12         190
C         13         204
C         14         218
C         15         232
C         16         246
C         17         260
C         18         274
C         19         288
C         20         302
C
C
C
      IVL=   STIVL- 1
      IVL1=  STIVL
      IDELT= DELT
C
      IF (STFG .EQ. 1) THEN
C       start of run
        STFG= 0
        ERRFLG= 0
        SCLU= 370
C
C       check for missing input time series
        DO 10 N= 1, NPT
          IF (PTFP(N) .LT. 1) THEN
C           error - missing an input point time series
            CALL OMSTI (PLTNO)
            CALL OMSTI (NPT)
            CALL OMSTI (N)
            SGRP= 11
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 ERRFLG)
          END IF
 10     CONTINUE
        DO 20 N= 1, NMN
          IF (MNFP(N) .LT. 1) THEN
C           error - missing an input mean time series
            CALL OMSTI (PLTNO)
            CALL OMSTI (NMN)
            CALL OMSTI (N)
            SGRP= 12
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 ERRFLG)
          END IF
 20     CONTINUE
        IF (ERRFLG .GT. 0) THEN
C         error - at least one missing time series: stop run
          I= 0
          CALL OMSTI (PLTNO)
          SGRP= 13
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               I)
        END IF
C
C       write file header
C
        IF (TYPEFG .EQ. 1) THEN
C         formal pltgen format
C         write labeling and scaling info to the plot file plotfl
C         exact format is given on the printout design sheet
C
          PDELT= IDELT* PIVL
          IF (PIVL .LT. 0) THEN
C           monthly or annual plotfile
            PDELT= PIVL
          END IF
          WRITE (PLOTFL,2000) CODE,CODE,PDELT,PYREND,CODE,NPT,NMN,NCURV
C
          IF (PIVL .GT. 0) THEN
C           normal plot file
            WRITE (PLOTFL,2010) CODE,LABLFG,PIVL,IDELT
          ELSE IF (PIVL .EQ. -1) THEN
C           monthly plot file
            WRITE (PLOTFL,2020) CODE,LABLFG,IDELT
          ELSE IF (PIVL .EQ. -2) THEN
C           annual plot file
            WRITE (PLOTFL,2030) CODE,LABLFG,IDELT
          END IF
C
          WRITE (PLOTFL,2040) CODE,TITLE,CODE,YLABL,CODE,YMIN,THRESH,
     $                        CODE,YMAX,CODE,IVLIN
C
          WRITE (PLOTFL,2050) CODE,CODE
          DO 30 N= 1, NCURV
            WRITE (PLOTFL,2060) CODE,(CUVDAT(I,N),I= 1, 9)
 30       CONTINUE
C
          IF (NCURV .LT. 10) THEN
            NBLNK= 10- NCURV
            DO 40 N= 1,NBLNK
              WRITE (PLOTFL,2070) CODE
 40         CONTINUE
          END IF
C
C         write heading that goes immed above time series data
          WRITE (PLOTFL,2080) CODE,CODE,CODE,CODE
C
C         handle the initial points
C
C         get date/time to external format
          CALL EXDATE (DATIM,
     O                 EXDAT)
C
C         assemble the values to be written in the first record
          NC= 0
          IF (NPT .GT. 0) THEN
            DO 50 N= 1, NPT
              NC= NC+ 1
              VAL(NC)= PAD(PTFP(N)+ 1)
 50         CONTINUE
          END IF
C
C         initial value for a mean-valued time series is meaningless
          IF (NMN .GT. 0) THEN
            DO 60 N= 1, NMN
              NC= NC+ 1
              VAL(NC)= -1.0E30
 60         CONTINUE
          END IF
C
          IF (SPAFP .GT. 0) THEN
C           no initial values when special actions are being used
C           to turn on plotfile
            PLTFLG= 0
          END IF
          IF (PLTFLG .NE. 0) THEN
C           write initial values
            WRITE (PLOTFL,2090) CODE,(EXDAT(N),N=1,5),(VAL(N),N=1,NC)
          END IF
        ELSE IF (TYPEFG .EQ. 2 .OR. TYPEFG .EQ. 4) THEN
C         header for feq dtsf format written by ppltgn
C
C         set number of curves
          NC= NMN
        ELSE IF (TYPEFG .EQ. 3) THEN
C         write header for feq ptsf format
C
C         write code indicating flow time series
          WRITE (PLOTFL) 4
C
C         write null event
          VALNOW= 0.0
          CALL MJDATE (NULSDT,
     O                 JTIME,DYFRAC)
          WRITE (PLOTFL) JTIME,VALNOW
          CALL MJDATE (NULEDT,
     O                 JTIME,DYFRAC)
          WRITE (PLOTFL) JTIME,VALNOW
C
C         write initial value
          VAL(1)= PAD(PTFP(1)+ 1)
          CALL EXDATE (DATIM,
     O                 EXDAT)          
          CALL MJDATE (EXDAT,
     O                 JTIME,DYFRAC)
          WRITE (PLOTFL) JTIME,VAL(1)
C
C         set number of curves
          NC= 1
        END IF
C
C       reset val
        DO 110 N= 1, NC
          TRNCOD= CUVDAT(9,N)
C         caseentry trncod
          GO TO (70,70,80,90,70),TRNCOD
C         case 1,2 and 5 sum,aver,last
 70       CONTINUE
            VAL(N)= 0.0
            GO TO 100
C         case 3 max
 80       CONTINUE
            VAL(N)= -1.0E30
            GO TO 100
C         case 4 min
 90       CONTINUE
            VAL(N)= 1.0E30
C         endcase
 100      CONTINUE
C       end loop
 110    CONTINUE
C
        IF (PIVL .LE. 0) THEN
C         determine how many intervals this month or year
          CALL PLTAGG (DATIM,NDAY,PYREND,PIVL,IDELT,
     O                 PIVLTM)
        ELSE
C         number of intervals input by user
          PIVLTM= PIVL
        END IF
      END IF
C
C     time loop
      DO 200 IVL= STIVL, WIDTH+ STIVL- 1
        IVL1= IVL1+ 1
        SPIVL= SPIVL+ 1
C
        IF ( (SPAFP .GT. 0) .AND. (SPAFP .LE. SPAKND) ) THEN
C         special actions are being taken and there is at least one left
          CALL SPECL (OPTYP,PLTNO,SPAKND,SPOPNO,DATIM,MESSU,SPIVL,
     I                SPOUT,SPNUND,
     M                SPAFP)
        END IF
C
C       increment date/time
        CALL ADDTIM (IDELT,NDAY,PIVLTM,PYREND,
     M               DATIM,PIVLNO,
     O               NDAYS,DUM1,DUM2,DUM3,DUM4,EMONFG,EPYRFG)
C
        IF (TYPEFG .GE. 2) THEN
C         dtsf or ptsf - need modified julian date
          CALL EXDATE (DATIM,
     O                 EXDAT)
          CALL MJDATE (EXDAT,
     O                 JTIME,DYFRAC)
        END IF
C
C       assemble the values to be written to a single record
        NC= 0
        IF (NPT .GT. 0) THEN
          DO 120 N= 1, NPT
            NC= NC+ 1
            VALNOW= PAD(PTFP(N)+ IVL1)
            CALL TRANS (CUVDAT(9,NC),PIVLTM,VALNOW,
     M                  VAL(NC))
 120      CONTINUE
        END IF
C
        IF (NMN .GT. 0) THEN
          DO 130 N= 1, NMN
            NC= NC+ 1
            VALNOW= PAD(MNFP(N)+ IVL1)
            CALL TRANS (CUVDAT(9,NC),PIVLTM,VALNOW,
     M                  VAL(NC))
 130      CONTINUE
        END IF
C
        IF (PIVLNO .EQ. PIVLTM) THEN
C         time to write a line
C
C         get date/time to external format
          CALL EXDATE (DATIM,
     O                 EXDAT)
C
          PLTNOW= 0
          IF (PLTFLG .EQ. 1) THEN
C           check values against threshold
            DO 140 N= 1, NC
              IF (VAL(N) .GT. THRESH) THEN
C               this value above threshold
                PLTNOW= 1
              END IF
 140        CONTINUE
          END IF
C
          IF (PLTNOW .EQ. 1) THEN
C           write a line this interval
            IF (TYPEFG .EQ. 1) THEN
C             formatted pltgen file
              WRITE (PLOTFL,2090) CODE,(EXDAT(N),N=1,5),(VAL(N),N=1,NC)
            ELSE IF (TYPEFG .EQ. 2 .OR. TYPEFG .EQ. 4) THEN
C             dtsf format
              EXDAT(6)= 0
              CALL TIMCVT
     M                    (EXDAT)
              IF (TYPEFG .EQ. 2) THEN
                WRITE (PLOTFL)  JTIME,DYFRAC,(EXDAT(N),N=1,3),
     $                         (VAL(N), N= 1, NC)
              ELSE
                WRITE (PLOTFL,REC=CURREC) JTIME,DYFRAC,(EXDAT(N),N=1,3),
     $                                   (VAL(N), N= 1, NC)
C            write(99,*) "PLTGEN: REC=",CURREC," JTIME,DYFRAC,EXDAT,VALS"
C            write(99,*) JTIME,DYFRAC,(EXDAT(N),N=1,3),(VAL(N),N=1,NC)
                CURREC= CURREC + 1
              END IF
            ELSE IF (TYPEFG .EQ. 3) THEN
C             ptsf format
              WRITE (PLOTFL)  JTIME,VAL(1)
            END IF
          END IF
C
C         reset val
          DO 190 N= 1, NC
            TRNCOD= CUVDAT(9,N)
C           caseentry trncod
            GO TO (150,150,160,170,150),TRNCOD
C           case 1,2 and 5 sum,aver,last
 150        CONTINUE
              VAL(N)= 0.0
              GO TO 180
C           case 3 max
 160        CONTINUE
              VAL(N)= -1.0E30
              GO TO 180
C           case 4 min
 170        CONTINUE
              VAL(N)= 1.0E30
C           endcase
 180        CONTINUE
C         end loop
 190      CONTINUE
C
          IF (PIVL .LE. 0) THEN
C           determine how many intervals this month or year
            CALL PLTAGG (DATIM,NDAY,PYREND,PIVL,IDELT,
     O                   PIVLTM)
            PIVLNO= 0
          END IF
        END IF
C
C       update pipes for user-defined variable quantities
        CALL UPQUAN (SPIVL,SPOPNO)
C
 200  CONTINUE
C
      IF (LSTCAL .EQ. 1) THEN
C       last interval of run
        IF (TYPEFG .EQ. 2 .OR. TYPEFG .EQ. 4) THEN
C         feq dtsf file - write last record of all zeroes
          VALNOW= 0.0
          N= NC+ 7
          CALL ZIPR (N,VALNOW,
     O               VAL)
          IF (TYPEFG .EQ. 2) THEN
            WRITE (PLOTFL) (VAL(N),N=1,NC+7)
          ELSE
            WRITE (PLOTFL,REC=CURREC) (VAL(N),N=1,NC+7)
          END IF
        ELSE IF (TYPEFG .EQ. 3) THEN
C         feq ptsf file - write last record of all zeroes
          VALNOW= 0.0
          WRITE (PLOTFL) VALNOW,VALNOW,VALNOW
        END IF
      END IF
C
      RETURN
      END
