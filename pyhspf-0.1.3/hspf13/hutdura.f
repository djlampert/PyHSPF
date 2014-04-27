C
C
C
      SUBROUTINE   PDURAN
     I                   (NDELT,SDATIM,EDATIM,NDAMON,EMFG,MAXOSV,
     M                    OSVKEY)
C
C     + + + PURPOSE + + +
C     Process the input for the duranl module
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   NDELT,SDATIM(5),EDATIM(5),NDAMON(12),EMFG,MAXOSV,OSVKEY
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NDELT  - simulation time interval in minutes
C     SDATIM - starting date/time
C     EDATIM - ending date/time
C     NDAMON - no. of days in each month of calendar year
C     EMFG   - english/metric units flag (english-1,metric-2)
C     MAXOSV - maximum size of osv
C     OSVKEY - last osv file record written
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION DURANL1 + + +
      INCLUDE   'cdura.inc'
      INCLUDE   'crin2.inc'
      INCLUDE   'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   FGERR,I,J,I1,I2,I3,I4,I5,I10,I16,OSVKST,OSVKND,OSVREC,
     #          SCLU,SGRP
C
C     + + + EXTERNALS + + +
      EXTERNAL  ITABLE,HSCKFL,STDATE,ENDATE,RTABLE,PUTOSV,OMSG,OMSTR
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT(/,' ',132('+'),
     $       /,' PROCESSING INPUT FOR DURATION ANALYSIS OPERATION NO. ',
     $         I5,'   TIME INTERVAL= ',I5,' MINS')
 2020 FORMAT(/,' FINISHED PROCESSING DURATION ANALYSIS OPERATION NO. ',
     $         I5,/,' ',132('+'))
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU= 372
C
C     initialize the entire osv
      DO 10 I=1,6000
        IPAD(I)= -999
 10   CONTINUE
C
      MESSU = FILE(1)
      MSGFL = FILE(15)
      I1    = 1
      I2    = 2
      I3    = 3
      I4    = 4
      I5    = 5
      I10   = 10
      I16   = 16
C
      IF (OUTLEV.GT.0) THEN
        WRITE (MESSU,2000) OPTNO,NDELT
      END IF
C
C     incorporate information from global and opn sequence blocks
      DO 20 I= 1,5
        DATIM(I) = SDATIM(I)
        DATIMS(I)= SDATIM(I)
        DATIME(I)= EDATIM(I)
 20   CONTINUE
      DO 30 I= 1,12
        NDAY(I)= NDAMON(I)
 30   CONTINUE
C
      UUNITS= EMFG
      DURANO= OPTNO
      DELT  = NDELT
      PIVLNO= 0
      PIVL  = 1
      PYREND= 1
C
C     get general data - table-type gen-durdata
      CALL ITABLE
     I             (I1,I1,I16,I1,
     M              GENDAT)
      M= NLEV+ 2
C
C     check output file - if not open,
C     then open it with a standard name
      IF (PUNIT .GT. 0) THEN
        CALL HSCKFL
     I              (PUNIT)
      END IF
C
C     get analysis season - table-type season
      CALL ITABLE
     I             (I2,I1,I10,I1,
     M              SEASON)
C
C     check that starting mo/day/hr/min are valid and convert to
C     internal format
      CALL STDATE
     I             (NDAMON,MESSU,MSGFL,
     M              ECOUNT,SESONS)
C
C     check that ending mo/day/hr/min are valid and convert to
C     internal format
      CALL ENDATE
     I             (NDAMON,MESSU,MSGFL,
     M              ECOUNT,SESONE)
C
C
C     get and check durations to be analyzed - durations
      CALL ITABLE
     I             (I3,I1,NDUR,I1,
     M              DURAT)
C
C     get the "levels" - table-type levels
      CALL RTABLE
     I             (I4,I1,NLEV,I1,
     M              LEVEL(2))
      LEVEL(1)= -1.0E30
      LEVEL(M)= +1.0E30
      FGERR   = 0
      DO 100 I= 1,NLEV
        IF (LEVEL(I+1).GE.LEVEL(I+2)) THEN
          FGERR= 1
        END IF
 100  CONTINUE
C
      IF (FGERR.EQ.1) THEN
C       error - levels are not in ascending order
        CALL OMSTR (LEVEL(2))
        CALL OMSTR (LEVEL(3))
        CALL OMSTR (LEVEL(4))
        CALL OMSTR (LEVEL(5))
        CALL OMSTR (LEVEL(6))
        CALL OMSTR (LEVEL(7))
        CALL OMSTR (LEVEL(8))
        CALL OMSTR (LEVEL(9))
        SGRP= 11
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             ECOUNT)
      END IF
C
      IF (LCNUM.GT.0) THEN
C       input lc curves
        DO 112 I= 1,LCNUM
          J= I
          CALL RTABLE
     I                 (I5,J,NDUR,I1,
     M                  LCCONC(1,J))
 112    CONTINUE
      END IF
C
C     number of records required for osv
      OSVREC= 7
C
C     write the osv to disc and record the keys in opntab
      OSVKST= OSVKEY+ 1
      OSVKND= OSVKEY+ OSVREC
      CALL PUTOSV
     I             (OSVKST,OSVKND,MAXOSV,IPAD)
      OPNTAB(7,OPNO)= OSVKST
      OPNTAB(8,OPNO)= OSVKND
      OSVKEY        = OSVKND
C
      IF (OUTLEV.GT.0) THEN
        WRITE (MESSU,2020) OPTNO
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   DURANL
     I                   (STIVL,WIDTH,FSTCAL,LSTCAL)
C
C     + + + PURPOSE + + +
C     perform duration analysis on a time series
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   STIVL,WIDTH,FSTCAL,LSTCAL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     STIVL  - in inpad row
C     WIDTH  - of inpad row
C     FSTCAL - flag indicating first interval of run
C     LSTCAL - flag indicating last interval of run
C
C     + + + COMMON BLOCK- SCRTCH, VERSION DURANL2 + + +
      INCLUDE   'cdura.inc'
      INCLUDE   'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     IDELT,IDUM1,IDUM2,IDUM3,IDUM4,IDUM5,IDUM6,
     $            IDUM7,STAFG,FINFG,MXLEV,MXDUR,MXLC,LCGTLT,
     $            REQFG,TSSUB(2),FLGVAL
      REAL        FREVPS(10,22),FREVNG(10,22),FREVNW(10),VALUE
      CHARACTER*6 OPTYP,TSNAM,DUMNAM
C
C     + + + EXTERNALS + + +
      EXTERNAL    ADDTIM,DURCLC,HREQTS
C
C     + + + DATA INITIALIZATIONS + + +
      DATA TSSUB/1,1/
      DATA OPTYP/'DURANL'/
C
C     + + + END SPECIFICATIONS + + +
C
      IVL  = STIVL- 1
      IVL1 = STIVL
      IDELT = DELT
      MXLEV = 22
      MXDUR = 10
      MXLC  = 5
C     hspf only looks for lethality greater than
      LCGTLT= 1
C
      DO 150 IVL= 1,WIDTH
        IVL1= IVL1+1
C       increment time and set time-related flags
        CALL ADDTIM
     I              (IDELT,NDAY,PIVL,PYREND,
     M               DATIM,PIVLNO,
     O               IDUM1,IDUM2,IDUM3,IDUM4,IDUM5,IDUM6,IDUM7)
CTHJ        VALUE= PAD(DSFP+IVL1)
        REQFG= 1
        TSNAM= 'TIMSER'
        CALL HREQTS (DSFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,DURANO,
     I               TSNAM,TSSUB,DUMNAM,DUMNAM,DUMNAM,FLGVAL,
     O               VALUE)
C
        IF ( (FSTCAL .EQ. 1) .AND. (IVL .EQ. 1) ) THEN
C         first time of first call
          STAFG= 1
        ELSE
C         not first
          STAFG= 0
        END IF
C
        IF ( (LSTCAL .EQ. 1) .AND. (IVL .EQ. WIDTH) ) THEN
C         last time of last call
          FINFG= 1
        ELSE
C         not last
          FINFG= 0
        END IF
C       make duration calculations
        CALL DURCLC(MXLEV,M,NLEV,MXDUR,NDUR,MXLC,LCNUM,
     I              STAFG,FINFG,LCOUT,PUNIT,PRFG,TITLE,DELT,DURANO,
     I              DATIM,VALUE,DURAT,LEVEL,LCLEV,LCCONC,LCGTLT,
     M              DATIMS,DATIME,SESFG,SESONS,SESONE,
     M              CURPOS,CURNEG,LASPOS,LASNEG,
     M              LGTNW1,LGTPOS,LGTNEG,
     M              NUM,SUM,SUMSQ,MAX,MINIM,
     M              FRQNW,SNW,SQNW,
     M              FRQPOS,SPOS,SQPOS,
     M              FRQNEG,SNEG,SQNEG,
     M              LCTIML,LCTSCT,LCTSTO,
     O              MNW,MPOS,MNEG,PTNW,PTPOS,PTNEG,
     O              PT1NW,PT1POS,PT1NEG,FREVNW,FREVPS,FREVNG,
     O              MEAN)
C
 150  CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   DURCLC
     I                   (MXLEV,M,NLEV,MXDUR,NDUR,MXLC,LCNUM,
     I                    STAFG,FINFG,LCOUT,PUNIT,PRFG,
     I                    TITLE,DELT,DURANO,
     I                    DATIM,VALUE,DURAT,LEVEL,LCLEV,LCCONC,LCGTLT,
     M                    DATIMS,DATIME,SESFG,SESONS,SESONE,
     M                    CURPOS,CURNEG,LASPOS,LASNEG,
     M                    LGTNW1,LGTPOS,LGTNEG,
     M                    NUM,SUM,SUMSQ,MAX,MINIM,
     M                    FRQNW,SNW,SQNW,
     M                    FRQPOS,SPOS,SQPOS,
     M                    FRQNEG,SNEG,SQNEG,
     M                    LCTIML,LCTSCT,LCTSTO,
     O                    MNW,MPOS,MNEG,PTNW,PTPOS,PTNEG,
     O                    PT1NW,PT1POS,PT1NEG,FREVNW,FREVPS,FREVNG,
     O                    MEAN)
C
C     + + + PURPOSE + + +
C     make duration analysis calculations
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER  MXLEV,M,NLEV,MXDUR,NDUR,MXLC,LCNUM,
     $         STAFG,FINFG,PUNIT,PRFG,TITLE(10),DURANO,LCOUT,DATIM(5),
     $         DATIMS(5),DATIME(5),
     $         SESFG,SESONS(5),SESONE(5),DURAT(MXDUR),LCGTLT,
     $         CURPOS,CURNEG,LASPOS,LASNEG,LGTNW1(1),NUM,
     $         LGTPOS(MXLEV),LGTNEG(MXLEV),
     $         LCLEV(MXDUR,MXLC),LCTIML(MXDUR,MXLC),
     $         LCTSCT(MXDUR,MXLC),LCTSTO(MXLC)
      REAL     VALUE,LEVEL(MXLEV),SUM,SUMSQ,MAX,MINIM,DELT,
     $         FRQNW(MXDUR),SNW(MXDUR),SQNW(MXDUR),
     $         FRQPOS(MXDUR,MXLEV),SPOS(MXDUR,MXLEV),SQPOS(MXDUR,MXLEV),
     $         FRQNEG(MXDUR,MXLEV),SNEG(MXDUR,MXLEV),SQNEG(MXDUR,MXLEV),
     $         LCCONC(MXDUR,MXLC),
     $         MNW(MXDUR),MPOS(MXDUR,MXLEV),MNEG(MXDUR,MXLEV),
     $         PTNW(MXDUR),PTPOS(MXDUR,MXLEV),PTNEG(MXDUR,MXLEV),
     $         PT1NW(MXDUR),PT1POS(MXDUR,MXLEV),PT1NEG(MXDUR,MXLEV),
     $         FREVNW(MXDUR),FREVPS(MXDUR,MXLEV),FREVNG(MXDUR,MXLEV),
     $         MEAN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MXLEV  - maximum number of levels allowed
C     M      - number of levels + 2 (low and high buckets)
C     NLEV   - number of levels in use
C     MXDUR  - maximum number of durations allowed
C     NDUR   - number of durations in use
C     MXLC   - maximum number of lethal conc curves allowed
C     LCNUM  - number of lethal conc curves in use
C     STAFG  - flag indicating first interval of first call
C     FINFG  - flag indicating last interval in last call
C     LCOUT  - lethal conc output flag
C     PUNIT  - print file unit number
C     PRFG   - print output level
C     TITLE  - heading title for output
C     DELT   - time interval of data in minutes
C     DURANO - duration id number
C     DATIM  - current date (yr,mo,dy,hr,mn)
C     VALUE  - current data value
C     DURAT  - values for each duration
C     LEVEL  - values for each level
C     LCLEV  - lethal concentration levels
C     LCCONC - lethal concentrations
C     LCGTLT - lethal greater than(1) or less than(2) flag
C     DATIMS - starting date/time
C     DATIME - ending date/time
C     SESFG  - season flag
C     SESONS - season start date/time
C     SESONS - season end date/time
C     CURPOS - current positive level
C     CURNEG - current negative level
C     LASPOS - last positive level
C     LASNEG - last negative level
C     LGTNW1 - number of intervals with no water
C     LGTPOS - number of intervals less
C     LGTNEG - number of intervals less
C     NUM    - number of valid values
C     SUM    - sum of valid values
C     SUMSQ  - sum of valid values squared
C     MAX    - maximum valid value
C     MINIM  - minimum valid value
C     FRQNW  - number of no water events for each duration
C     SNW    - length of no water events for each duration
C     SQNW   - stand dev of len of no water events for each duration
C     FRQPOS - number of pos events for each duration at each level
C     SPOS   - length of positive events for each duration at each level
C     SQPOS  - stand dev of len of pos events for each dur at each level
C     FRQNEG - number of neg events for each duration at each level
C     SNEG   - length of negative events for each duration at each level
C     SQNEG  - stand dev of len of neg events for each dur at each level
C     LCTIML - amount of time in lethal events at each level
C     LCTSCT - amount of time in potential lethal event
C     LCTSTO - amout of time spent in lethal events
C     MNW    - ave len of no water events for each duration
C     MPOS   - ave len of pos events for each duration for each level
C     MNEG   - ave len of neg events for each duration for each level
C     PTNW   - frac of time in no water events for each dur
C     PTPOS  - frac of time in pos events for each dur for each level
C     PTNEG  - frac of time in neg events for each dur for each level
C     PT1NW  - frac time in nw events/frac time in dur1 for each dur
C     PT1POS - frac time in pos events/frac time in dur1 for each dur & lev
C     PT1NEG - frac time in neg events/frac time in dur1 for each dur & lev
C     FREVNW - frac nw events/frac nw events in dur1 for each dur
C     FREVPS - frac pos events/frac pos events in dur1 for each dur & lev
C     FREVNG - frac neg events/frac neg events in dur1 for each dur & lev
C     MEAN   - mean of valid values
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I1,DIF,DIFS,JJ,EXDAT(5),
     $          LCA,LCX,LCY,LCZ,LCLEVO,LCLEVN,LCTEMP,LCCHK
C
C     + + + EXTERNALS + + +
      EXTERNAL  TDIFX,DURDMP,EXDATE,DURFIN,DURINI
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT(5X,I4,'/',I2,'/',I2,' ',I2,':',I2,5X,'LC CURVE #',I2,
     $       3X,' # INTERVALS ',I5,3X,' CONC LEVEL ',G10.4)
C
C     + + + END SPECIFICATIONS + + +
C
      I1= 1
C
      IF (STAFG .EQ. 1) THEN
C       first time, need to initialize
        CALL DURINI (MXLEV,MXDUR,MXLC,M,LEVEL,DATIM,LCNUM,
     M               SESONS,SESONE,
     O               LASPOS,LASNEG,LGTNW1,LGTPOS,LGTNEG,
     O               FRQNW,SNW,SQNW,
     O               FRQPOS,FRQNEG,SPOS,SNEG,SQPOS,SQNEG,
     O               NUM,SUM,SUMSQ,MEAN,MAX,MINIM,
     O               SESFG,
     O               LCLEV,LCTIML,LCTSCT,LCTSTO)
      END IF
C
      IF (SESFG.LT.2) THEN
C       we are doing a seasonal analysis
        CALL TDIFX (DATIM(1),DATIM,
     O              DIFS)
        IF (SESFG.EQ.0) THEN
C         we have been outside the analysis season - find out if
C         we're now in it
          CALL TDIFX (DATIM(1),SESONS,
     O                DIF)
          IF (DIFS .GT. DIF) THEN
C           we're in - update date/time for start of next season
C           WRITE(*,*) 'into   season:',DIFS,DIF,DATIM
            SESFG    = 1
            SESONS(1)= SESONS(1)+ 1
          END IF
        ELSE
C         we have been inside the analysis season - find out if
C         we've moved outside
          CALL TDIFX (DATIM(1),SESONE,
     O                DIF)
          IF (DIFS .GT. DIF) THEN
C           we're out - update date/time for end of next season
C           WRITE(*,*) 'out of season:',DIFS,DIF,DATIM
            SESFG    = 0
            SESONE(1)= SESONE(1)+ 1
          END IF
        END IF
      END IF
C
      IF (SESFG.GT.0) THEN
C       we are in the analysis season
        IF (VALUE.LT.-1.0E10) THEN
C         invalid value
          CURPOS= 1
          CURNEG= M
          LGTNW1(1)= LGTNW1(1)+ 1
        ELSE
C         valid value
          NUM  = NUM+ 1
          SUM  = SUM+ VALUE
          SUMSQ= SUMSQ+ VALUE*VALUE
          IF (VALUE.GT.MAX) THEN
C           new max value
            MAX= VALUE
          END IF
          IF (VALUE.LT.MINIM) THEN
C           new min value
            MINIM= VALUE
          END IF
          CALL DURDMP
     I               (I1,I1,I1,LGTNW1,NDUR,DURAT,
     M                FRQNW,SNW,SQNW)
C
          JJ= 1
 70       CONTINUE
            IF (LEVEL(JJ).LE.VALUE) THEN
C             try next level
              CURPOS= JJ
              JJ= JJ+ 1
            ELSE
C             this is it
              JJ= 0
            END IF
          IF (JJ .GT. 0) GO TO 70
C
          CURNEG= CURPOS+ 1
C
          JJ= 1
 90       CONTINUE
            IF (JJ.LE.CURPOS) THEN
              LGTPOS(JJ)= LGTPOS(JJ)+ 1
              JJ= JJ+ 1
            ELSE
              JJ= 0
            END IF
          IF (JJ .GT. 0) GO TO 90
C
          JJ= CURNEG
 110      CONTINUE
            IF (JJ.LE.M) THEN
              LGTNEG(JJ)= LGTNEG(JJ)+ 1
              JJ= JJ+ 1
            ELSE
              JJ= 0
            END IF
          IF (JJ .GT. 0) GO TO 110
C
          JJ= CURPOS+ 1
          CALL DURDMP
     I               (JJ,LASPOS,MXLEV,LGTPOS,NDUR,DURAT,
     M                FRQPOS,SPOS,SQPOS)
          JJ= CURNEG- 1
          CALL DURDMP
     I               (LASNEG,JJ,MXLEV,LGTNEG,NDUR,DURAT,
     M                FRQNEG,SNEG,SQNEG)
          LASPOS= CURPOS
          LASNEG= CURNEG
C
          IF (LCNUM.GT.0) THEN
C           lethal concentration analysis
            DO 128 LCA= 1,LCNUM
              LCTEMP= 0
              LCZ   = NDUR+ 1
              DO 125 LCX= 1,NDUR
                LCLEVO= LCLEV(LCX,LCA)
                LCLEVN= 1
                IF (LCGTLT .EQ. 1) THEN
C                 looking for greater than
                  IF (VALUE.GE.LCCONC(LCX,LCA)) THEN
                    LCCHK= 1
                  ELSE
                    LCCHK= 0
                  END IF
                ELSE IF (LCGTLT .EQ. 2) THEN
C                 looking for less than
                  IF (VALUE.LE.LCCONC(LCX,LCA)) THEN
                    LCCHK= 1
                  ELSE
                    LCCHK= 0
                  END IF
                END IF
C
                IF (LCCHK .EQ. 1) THEN
C                 possibly a lethal event at this duration
                  LCLEVN= 2
                  LCTSCT(LCX,LCA)= LCTSCT(LCX,LCA)+ 1
                  IF (LCTSCT(LCX,LCA).GE.DURAT(LCX)) THEN
C                   lethal event at this duration
                    LCLEVN= 3
                    LCTEMP= LCTSCT(LCX,LCA)- LCTIML(LCX,LCA)
C                   lethal at this interval for the first time
C                   pick up earlier times now known to be lethal
                    IF (LCLEVO.EQ.3) THEN
                      LCTEMP= 1
                    END IF
C                   lethal last time too-no time to pick up
                    DO 121 LCY= 1,LCX
                      LCTIML(LCX,LCA)= 0
 121                CONTINUE
                    LCZ= LCX+ 1
                  END IF
                ELSE
C                 this is not a lethal event at this duration
                  IF (LCLEVO.NE.1) THEN
C                   might have been lethal last time
                    IF (LCLEVO.NE.2.AND.LCOUT.GT.0.AND.PUNIT.GT.0) THEN
C                     it was, summarize it
                      CALL EXDATE (DATIM,
     O                             EXDAT)
                      WRITE(PUNIT,2000) EXDAT,LCA,LCTSCT(LCX,LCA),
     $                                  LCCONC(LCX,LCA)
                    END IF
                    LCTSCT(LCX,LCA)= 0
                    LCTIML(LCX,LCA)= 0
                  END IF
                END IF
                LCLEV(LCX,LCA)= LCLEVN
 125          CONTINUE
              LCTSTO(LCA)= LCTSTO(LCA)+ LCTEMP
              IF(LCZ.LE.NDUR) THEN
                DO 126 LCX= LCZ,NDUR
                  LCTIML(LCX,LCA)= LCTIML(LCX,LCA)+ LCTEMP
 126            CONTINUE
              END IF
 128        CONTINUE
          END IF
        END IF
      END IF
C
      IF (FINFG.EQ.1) THEN
C       final processing
        CALL DURDMP
     I              (I1,I1,I1,LGTNW1,NDUR,DURAT,
     M               FRQNW,SNW,SQNW)
        CALL DURDMP
     I              (I1,M,MXLEV,LGTPOS,NDUR,DURAT,
     M               FRQPOS,SPOS,SQPOS)
        CALL DURDMP
     I              (I1,M,MXLEV,LGTNEG,NDUR,DURAT,
     M               FRQNEG,SNEG,SQNEG)
C       compute final results
        CALL DURFIN (MXDUR,MXLC,NDUR,MXLEV,NLEV,M,PUNIT,PRFG,NUM,
     I               SNW,SPOS,SNEG,
     I               LEVEL,DURAT,TITLE,DELT,DURANO,MAX,MINIM,
     I               LCNUM,LCTSTO,
     M               DATIMS,DATIME,SESONS,SESONE,
     M               FRQNW,FRQPOS,FRQNEG,SQNW,SQPOS,SQNEG,
     O               MEAN,SUM,SUMSQ,
     O               MNW,MPOS,MNEG,PTNW,PTPOS,PTNEG,
     O               PT1NW,PT1POS,PT1NEG,FREVNW,FREVPS,FREVNG)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   DURINI
     I                   (MXLEV,MXDUR,MXLC,M,LEVEL,DATIM,LCNUM,
     M                    SESONS,SESONE,
     O                    LASPOS,LASNEG,LGTNW1,LGTPOS,LGTNEG,
     O                    FRQNW,SNW,SQNW,
     O                    FRQPOS,FRQNEG,SPOS,SNEG,SQPOS,SQNEG,
     O                    NUM,SUM,SUMSQ,MEAN,MAX,MINIM,
     O                    SESFG,
     O                    LCLEV,LCTIML,LCTSCT,LCTSTO)
C
C     + + + PURPOSE + + +
C     initialize duration internal variables
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MXLEV,MXDUR,MXLC,M,DATIM(5),LCNUM,SESONS(5),SESONE(5),
     $          LASPOS,LASNEG,LGTNW1(1),LGTPOS(MXLEV),LGTNEG(MXLEV),
     $          NUM,SESFG,LCLEV(MXDUR,MXLC),LCTIML(MXDUR,MXLC),
     $          LCTSCT(MXDUR,MXLC),LCTSTO(MXLC)
      REAL      LEVEL(MXLEV),FRQNW(MXDUR),SNW(MXDUR),SQNW(MXDUR),
     $          FRQPOS(MXDUR,MXLEV),FRQNEG(MXDUR,MXLEV),
     $          SPOS(MXDUR,MXLEV),SNEG(MXDUR,MXLEV),
     $          SQPOS(MXDUR,MXLEV),SQNEG(MXDUR,MXLEV),
     $          SUM,SUMSQ,MEAN,MAX,MINIM
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MXLEV  - maximum number of levels allowed
C     MXDUR  - maximum number of durations allowed
C     MXLC   - maximum number of lethal conc curves allowed
C     M      - number of levels + 2 (low and high buckets)
C     LEVEL  - values for each level
C     DATIM  - current date (yr,mo,dy,hr,mn)
C     LCNUM  - number of lethal conc curves in use
C     SESONS - season start date/time
C     SESONS - season end date/time
C     LASPOS - last positive level
C     LASNEG - last negative level
C     LGTNW1 - number of intervals with no water
C     LGTPOS - number of intervals less
C     LGTNEG - number of intervals less
C     FRQNW  - number of no water events for each duration
C     SNW    - length of no water events for each duration
C     SQNW   - stand dev of len of no water events for each duration
C     FRQPOS - number of pos events for each duration at each level
C     FRQNEG - number of neg events for each duration at each level
C     SPOS   - length of positive events for each duration at each level
C     SNEG   - length of negative events for each duration at each level
C     SQPOS  - stand dev of len of pos events for each dur at each level
C     SQNEG  - stand dev of len of neg events for each dur at each level
C     NUM    - number of valid values
C     SUM    - sum of valid values
C     SUMSQ  - sum of valid values squared
C     MEAN   - mean of valid values
C     MAX    - maximum valid value
C     MINIM  - minimum valid value
C     SESFG  - season flag
C     LCLEV  - lethal concentration levels
C     LCTIML - amount of time in lethal events at each level
C     LCTSCT - amount of time in potential lethal event
C     LCTSTO - amount of time spent in lethal events
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,J,YR,DIFE,DIFS,DIFX
C
C     + + + EXTERNALS + + +
      EXTERNAL  TDIFX
C
C     + + + END SPECIFICATIONS + + +
C
      YR = DATIM(1)
C
C     initialize pointers and counters
      LASPOS= 1
      LASNEG= M
      LGTNW1(1)= 0
      DO 120 J= 1,MXLEV
        LGTPOS(J)= 0
        LGTNEG(J)= 0
 120  CONTINUE
C
      DO 130 I= 1,MXDUR
        FRQNW(I)= 0.0
        SNW(I)  = 0.0
        SQNW(I) = 0.0
 130  CONTINUE
C
      DO 150 J= 1,MXLEV
        DO 140 I= 1,MXDUR
          FRQPOS(I,J)= 0.0
          FRQNEG(I,J)= 0.0
          SPOS(I,J)  = 0.0
          SNEG(I,J)  = 0.0
          SQPOS(I,J) = 0.0
          SQNEG(I,J) = 0.0
 140    CONTINUE
 150  CONTINUE
C
C     initialize statistical data
      NUM   = 0
      SUM   = 0.0
      SUMSQ = 0.0
      MEAN  = 0.0
      MAX   = LEVEL(1)
      MINIM = LEVEL(M)
C     find out whether or not we are starting in the analysis season
C     set sesfg to 1 if we are
      CALL TDIFX (YR,DATIM,
     O            DIFX)
C
      SESONS(1)= YR
      CALL TDIFX (YR,SESONS,
     O            DIFS)
      DIFS= DIFS- DIFX
C
      SESONE(1)= YR
      CALL TDIFX (YR,SESONE,
     O            DIFE)
      DIFE= DIFE- DIFX
C
C     WRITE(*,*) DATIM
C     WRITE(*,*) SESONS
C     WRITE(*,*) SESONE
C     WRITE(*,*) DIFX,DIFS,DIFE
C
      IF (DIFS.EQ.DIFE) THEN
C       no seasonal analysis
        SESFG= 2
      ELSE
        SESFG= 0
        IF (DIFS.LT.0) THEN
C         start of season is < than present
          IF (DIFE.LT.0.AND.DIFE.LE.DIFS) THEN
            SESFG= 1
          END IF
          IF (DIFE.GE.0) THEN
            SESFG= 1
          END IF
        ELSE
C         start of season is >= present
          IF (DIFE.GE.0.AND.DIFE.LE.DIFS) THEN
            SESFG= 1
          END IF
        END IF
C
C       find out whether next encounter with start & end will be
C       this year or next
        IF (DIFS.LT.0) THEN
          SESONS(1)= YR+ 1
        END IF
        IF (DIFE.LT.0) THEN
          SESONE(1)= YR+ 1
        END IF
      END IF
C
      IF (LCNUM .GT. 0) THEN
C       initialize arrays related to lc calc
        DO 170 J= 1,MXLC
          DO 160 I= 1,MXDUR
            LCLEV(I,J) = 1
            LCTIML(I,J)= 0
            LCTSCT(I,J)= 0
 160      CONTINUE
          LCTSTO(J)= 0
 170    CONTINUE
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   DURDMP
     I                   (J1,J2,K,LGTH,NDUR,DURAT,
     M                    FREQ,S,SQ)
C
C     + + + PURPOSE + + +
C     Dump results of duration analysis.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   J1,J2,K,LGTH(K),NDUR,DURAT(10)
      REAL      FREQ(10,K),S(10,K),SQ(10,K)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     J1     - first level
C     J2     - last level
C     K      - max level
C     LGTH   - length to process
C     NDUR   - number of durations
C     DURAT  - array of duration values
C     FREQ   - count
C     S      - sum
C     SQ     - sum*sum
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,J,IFLG,JFLG
C
C     + + + END SPECIFICATIONS + + +
C
      J= J1
 10   CONTINUE
        JFLG= 1
        IF (J.LE.J2) THEN
          I= 1
 20       CONTINUE
            IFLG= 1
            IF ((DURAT(I).LE.LGTH(J)).AND.(I.LE.NDUR)) THEN
              FREQ(I,J)= FREQ(I,J)+ 1
              S(I,J)   = S(I,J)+ LGTH(J)
              SQ(I,J)  = SQ(I,J)+ LGTH(J)*LGTH(J)
              I= I+ 1
            ELSE
C             kick out of 20 loop
              IFLG= 0
            END IF
          IF (IFLG.GT.0) GO TO 20
          LGTH(J)= 0
          J= J+ 1
        ELSE
C         kick out of 10 loop
          JFLG= 0
        END IF
      IF (JFLG.GT.0) GO TO 10
C
      RETURN
      END
C
C     4.2(14).2
C
      SUBROUTINE   DURFIN
     I                   (MXDUR,MXLC,NDUR,MXLEV,NLEV,M,PUNIT,PRFG,NUM,
     I                    SNW,SPOS,SNEG,
     I                    LEVEL,DURAT,TITLE,DELT,DURANO,MAX,MINIM,
     I                    LCNUM,LCTSTO,
     M                    DATIMS,DATIME,SESONS,SESONE,
     M                    FRQNW,FRQPOS,FRQNEG,SQNW,SQPOS,SQNEG,
     O                    MEAN,SUM,SUMSQ,
     O                    MNW,MPOS,MNEG,PTNW,PTPOS,PTNEG,
     O                    PT1NW,PT1POS,PT1NEG,FREVNW,FREVPS,FREVNG)
C
C     + + + PURPOSE + + +
C     compute duration analysis results
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MXDUR,NDUR,MXLEV,NLEV,M,PUNIT,PRFG,NUM,DURAT(MXDUR),
     $          TITLE(10),DURANO,LCNUM,MXLC,LCTSTO(MXLC),
     $          DATIMS(5),DATIME(5),SESONS(5),SESONE(5)
      REAL      SNW(MXDUR),SPOS(MXDUR,MXLEV),SNEG(MXDUR,MXLEV),
     $          LEVEL(MXLEV),MAX,MINIM,DELT,
     $          FRQNW(MXDUR),FRQPOS(MXDUR,MXLEV),FRQNEG(MXDUR,MXLEV),
     $          SQNW(MXDUR),SQPOS(MXDUR,MXLEV),SQNEG(MXDUR,MXLEV),
     $          MEAN,SUM,SUMSQ,
     $          MNW(MXDUR),MPOS(MXDUR,MXLEV),MNEG(MXDUR,MXLEV),
     $          PTNW(MXDUR),PTPOS(MXDUR,MXLEV),PTNEG(MXDUR,MXLEV),
     $          PT1NW(MXDUR),PT1POS(MXDUR,MXLEV),PT1NEG(MXDUR,MXLEV),
     $          FREVNW(MXDUR),FREVPS(MXDUR,MXLEV),FREVNG(MXDUR,MXLEV)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MXDUR  - maximum number of durations allowed
C     NDUR   - number of durations in use
C     MXLEV  - maximum number of levels allowed
C     NLEV   - number of levels in use
C     M      - number of levels + 2 (low and high buckets)
C     PUNIT  - print file unit number
C     PRFG   - print output level
C     NUM    - number of valid values
C     SNW    - length of no water events for each duration
C     SPOS   - length of positive events for each duration at each level
C     SNEG   - length of negative events for each duration at each level
C     LEVEL  - values for each level
C     DURAT  - values for each duration
C     TITLE  - heading title for output
C     DELT   - time interval of data in minutes
C     DURANO - duration id number
C     MAX    - maximum valid value
C     MINIM  - minimum valid value
C     LCNUM  - number of lethal conc curves in use
C     LCTSTO - amount of time spent in lethal events
C     DATIMS - starting date/time
C     DATIME - ending date/time
C     SESONS - season start date/time
C     SESONS - season end date/time
C     FRQNW  - number of no water events for each duration
C     FRQPOS - number of pos events for each duration at each level
C     FRQNEG - number of neg events for each duration at each level
C     SQNW   - stand dev of len of no water events for each duration
C     SQPOS  - stand dev of len of pos events for each dur at each level
C     SQNEG  - stand dev of len of neg events for each dur at each level
C     MEAN   - mean of valid values
C     SUM    - sum of valid values
C     SUMSQ  - sum of valid values squared
C     MNW    - ave len of no water events for each duration
C     MPOS   - ave len of pos events for each duration for each level
C     MNEG   - ave len of neg events for each duration for each level
C     PTNW   - frac of time in no water events for each dur
C     PTPOS  - frac of time in pos events for each dur for each level
C     PTNEG  - frac of time in neg events for each dur for each level
C     PT1NW  - frac time in nw events/frac time in dur1 for each dur
C     PT1POS - frac time in pos events/frac time in dur1 for each dur & lev
C     PT1NEG - frac time in neg events/frac time in dur1 for each dur & lev
C     FREVNW - frac nw events/frac nw events in dur1 for each dur
C     FREVPS - frac pos events/frac pos events in dur1 for each dur & lev
C     FREVNG - frac neg events/frac neg events in dur1 for each dur & lev
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,J
      REAL      S
C
C     + + + EXTERNALS + + +
      EXTERNAL  DURPUT
C
C     + + + INTRINSICS + + +
      INTRINSIC SQRT
C
C     + + + END SPECIFICATIONS + + +
C
C     initialize output tables
      DO 10 I= 1,MXDUR
        MNW(I)   = 0.0
        PTNW(I)  = 0.0
        PT1NW(I) = 0.0
        FREVNW(I)= 0.0
   10 CONTINUE
C
      DO 30 J= 1,MXLEV
        DO 20 I= 1,MXDUR
          MPOS(I,J)  = 0.0
          MNEG(I,J)  = 0.0
          PTPOS(I,J) = 0.0
          PTNEG(I,J) = 0.0
          PT1POS(I,J)= 0.0
          PT1NEG(I,J)= 0.0
          FREVPS(I,J)= 0.0
          FREVNG(I,J)= 0.0
   20   CONTINUE
   30 CONTINUE
C
C     compute table and summary information
      DO 90 I= 1,NDUR
        J= 2
   40   CONTINUE
          IF (SPOS(I,1).GT.0.0) PTPOS(I,J) = SPOS(I,J)/SPOS(I,1)
          IF (SNEG(I,M).GT.0.0) PTNEG(I,J) = SNEG(I,J)/SNEG(I,M)
          IF (SPOS(1,J).GT.0.0) PT1POS(I,J)= SPOS(I,J)/SPOS(1,J)
          IF (SNEG(1,J).GT.0.0) PT1NEG(I,J)= SNEG(I,J)/SNEG(1,J)
          IF (FRQPOS(I,J) .GT. 0.0) THEN
            S= SQPOS(I,J)- (SPOS(I,J)**2/FRQPOS(I,J))
            IF (S .GE. 0.0) THEN
              SQPOS(I,J)= SQRT(S/FRQPOS(I,J))
            ELSE
              IF (PUNIT .NE. 0) THEN
                WRITE (PUNIT,9999)SQPOS(I,J),SPOS(I,J),FRQPOS(I,J),I,J
 9999           FORMAT (' ',80('*'),//,' REPORT TO',
     $                  ' EPA',
     $                  /,' DEBUG,DUR',3G12.5,2I5,
     $                   /)
              END IF
              SQPOS(I,J)= 1.0E30
            END IF
            MPOS(I,J)= SPOS(I,J)/FRQPOS(I,J)
          END IF
          IF (FRQPOS(1,J) .GT. 0.0) THEN
            FREVPS(I,J)= FRQPOS(I,J)/FRQPOS(1,J)
          END IF
C
          IF (FRQNEG(I,J) .GT. 0.0) THEN
            S= SQNEG(I,J)- (SNEG(I,J)**2/FRQNEG(I,J))
            MNEG(I,J)= SNEG(I,J)/FRQNEG(I,J)
            IF (S .GE. 0.0) THEN
              SQNEG(I,J)= SQRT(S/FRQNEG(I,J))
            ELSE
              SQNEG(I,J)= 1.0E30
            END IF
          END IF
C
          IF (FRQNEG(1,J) .GT. 0.0) THEN
            FREVNG(I,J)= FRQNEG(I,J)/FRQNEG(1,J)
          END IF
C
          J= J+ 1
        IF (J.LE.NLEV+1) GO TO 40
C
        IF (SPOS(1,1).GT.0.0) PTNW(I) = SNW(I)/SPOS(1,1)
        IF (SNW(1).GT.0.0)    PT1NW(I)= SNW(I)/SNW(1)
        IF (FRQNW(I).GT.0.0) THEN
          S= SQNW(I)- (SNW(I)**2/FRQNW(I))
          MNW(I) = SNW(I)/FRQNW(I)
          IF (S .GE. 0.0) THEN
            SQNW(I)= SQRT(S/FRQNW(I))
          ELSE
            SQNW(I)= 1.0E30
          END IF
        END IF
        IF (FRQNW(1) .GT. 0.0) THEN
          FREVNW(I)= FRQNW(I)/FRQNW(1)
        END IF
   90 CONTINUE
C
      IF (NUM.GT.0) THEN
        S= SUMSQ- (SUM*SUM/NUM)
        MEAN = SUM/NUM
        IF (S .GE. 0.0) THEN
          SUMSQ= SQRT(S/NUM)
        ELSE
          SUMSQ= 1.0E30
        END IF
      END IF
C
      IF (PUNIT .GT. 0) THEN
C       output final results to file
        CALL DURPUT (MXDUR,MXLC,NDUR,MXLEV,NLEV,DURANO,TITLE,PUNIT,PRFG,
     I               LEVEL,DURAT,DELT,NUM,MAX,MINIM,MEAN,SUMSQ,
     I               LCNUM,LCTSTO,
     I               PTPOS,PTNEG,PTNW,
     I               PT1POS,PT1NEG,PT1NW,
     I               SPOS,SNEG,SNW,
     I               FRQPOS,FRQNEG,FRQNW,
     I               MPOS,MNEG,MNW,
     I               SQPOS,SQNEG,SQNW,
     I               FREVPS,FREVNG,FREVNW,
     M               DATIMS,DATIME,SESONS,SESONE)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   DURPUT
     I                   (MXDUR,MXLC,NDUR,MXLEV,NLEV,DURANO,TITLE,PUNIT,
     I                    PRFG,LEVEL,DURAT,DELT,NUM,MAX,MINIM,MEAN,
     I                    SUMSQ,LCNUM,LCTSTO,
     I                    PTPOS,PTNEG,PTNW,
     I                    PT1POS,PT1NEG,PT1NW,
     I                    SPOS,SNEG,SNW,
     I                    FRQPOS,FRQNEG,FRQNW,
     I                    MPOS,MNEG,MNW,
     I                    SQPOS,SQNEG,SQNW,
     I                    FREVPS,FREVNG,FREVNW,
     M                    DATIMS,DATIME,SESONS,SESONE)
C
C     + + + PURPOSE + + +
C     output duration analysis results
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MXDUR,NDUR,MXLEV,NLEV,MXLC,
     $          DURANO,TITLE(10),PUNIT,PRFG,NUM,LCNUM,LCTSTO(MXLC),
     $          DURAT(MXDUR),DATIMS(5),DATIME(5),SESONS(5),SESONE(5)
      REAL      LEVEL(MXLEV),MAX,MINIM,MEAN,SUMSQ,DELT,
     $          PTPOS(MXDUR,MXLEV),PTNEG(MXDUR,MXLEV),PTNW(MXDUR),
     $          PT1POS(MXDUR,MXLEV),PT1NEG(MXDUR,MXLEV),PT1NW(MXDUR),
     $          SPOS(MXDUR,MXLEV),SNEG(MXDUR,MXLEV),SNW(MXDUR),
     $          FRQPOS(MXDUR,MXLEV),FRQNEG(MXDUR,MXLEV),FRQNW(MXDUR),
     $          MPOS(MXDUR,MXLEV),MNEG(MXDUR,MXLEV),MNW(MXDUR),
     $          SQPOS(MXDUR,MXLEV),SQNEG(MXDUR,MXLEV),SQNW(MXDUR),
     $          FREVPS(MXDUR,MXLEV),FREVNG(MXDUR,MXLEV),FREVNW(MXDUR)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MXDUR  - maximum number of durations allowed
C     NDUR   - number of durations in use
C     MXLEV  - maximum number of levels allowed
C     NLEV   - number of levels in use
C     DURANO - duration id number
C     TITLE  - heading title for output
C     PUNIT  - print file unit number
C     PRFG   - print output level
C     LEVEL  - values for each level
C     DURAT  - values for each duration
C     DELT   - time interval of data in minutes
C     NUM    - number of valid values
C     MAX    - maximum valid value
C     MINIM  - minimum valid value
C     MEAN   - mean of valid values
C     SUMSQ  - sum of valid values squared
C     LCNUM  - number of lethal conc curves in use
C     LCTSTO - amount of time spent in lethal events
C     PTPOS  - frac of time in pos events for each dur for each level
C     PTNEG  - frac of time in neg events for each dur for each level
C     PTNW   - frac of time in no water events for each dur
C     PT1POS - frac time in pos events/frac time in dur1 for each dur & lev
C     PT1NEG - frac time in neg events/frac time in dur1 for each dur & lev
C     PT1NW  - frac time in nw events/frac time in dur1 for each dur
C     SPOS   - length of positive events for each duration at each level
C     SNEG   - length of negative events for each duration at each level
C     SNW    - length of no water events for each duration
C     FRQPOS - number of pos events for each duration at each level
C     FRQNEG - number of neg events for each duration at each level
C     FRQNW  - number of no water events for each duration
C     MPOS   - ave len of pos events for each duration for each level
C     MNEG   - ave len of neg events for each duration for each level
C     MNW    - ave len of no water events for each duration
C     SQPOS  - stand dev of len of pos events for each dur at each level
C     SQNEG  - stand dev of len of neg events for each dur at each level
C     SQNW   - stand dev of len of no water events for each duration
C     FREVPS - frac pos events/frac pos events in dur1 for each dur & lev
C     FREVNG - frac neg events/frac neg events in dur1 for each dur & lev
C     FREVNW - frac nw events/frac nw events in dur1 for each dur
C     DATIMS - starting date/time
C     DATIME - ending date/time
C     SESONS - season start date/time
C     SESONS - season end date/time
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,PID
      REAL      DUM
C
C     + + + EXTERNALS + + +
      EXTERNAL  EXDATE,DUROUT
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT(    '1',
     $       ///,' Duration analysis operation no. ',I5)
 2010 FORMAT(  /,' ',10A4)
 2020 FORMAT(  /,' Start date: ',I4,2('/',I2),I3,':',I2,
     $             '  End date: ',I4,2('/',I2),I3,':',I2)
 2030 FORMAT(  /,' Analysis season starts: ',I2,'/',I2,1X,I2,':',I2,
     $             '  Ends: ',I2,'/',I2,1X,I2,':',I2)
 2040 FORMAT(    '1',
     $       ///,'     SUMMARY')
 2050 FORMAT(  /,'     TOTAL LENGTH OF DEFINED EVENTS:   ',F10.0,
     $             ' INTERVALS')
 2060 FORMAT(  /,'     TOTAL LENGTH OF UNDEFINED EVENTS: ',F10.0,
     $             ' INTERVALS')
 2065 FORMAT(  /,'     TOTAL LENGTH OF ANALYSIS: ',F10.2,' DAYS')
 2070 FORMAT(  /,'     SAMPLE SIZE: ',I8)
 2080 FORMAT(  /,'     SAMPLE MAXIMUM: ',G12.6)
 2090 FORMAT(  /,'     SAMPLE MINIMUM: ',G12.6)
 2100 FORMAT(  /,'     SAMPLE MEAN: ',G12.6)
 2110 FORMAT(  /,'     SAMPLE STANDARD DEVIATION: ',G12.6)
 2120 FORMAT(  /,' ',
     $       ///,'     LETHAL LEVEL EXCEEDANCE SUMMARY ',
     $        //,'     CURVE NUMBER','   FRACTION EXCEEDED')
 2130 FORMAT(  /,'             ',I4,10X,G12.6)
C
C     + + + END SPECIFICATIONS + + +
C
C     convert dates and times to external format
      CALL EXDATE(DATIMS,
     O            DATIMS)
      CALL EXDATE(DATIME,
     O            DATIME)
      WRITE (PUNIT,2000) DURANO
      WRITE (PUNIT,2010) TITLE
      WRITE (PUNIT,2020) DATIMS,DATIME
      CALL EXDATE (SESONS,
     O             SESONS)
      CALL EXDATE (SESONE,
     O             SESONE)
      WRITE (PUNIT,2030) (SESONS(I),I=2,5),(SESONE(I),I=2,5)
C
C     output tables (always basic)
      PID= 1
      CALL DUROUT (PUNIT,MXLEV,NLEV,LEVEL,MXDUR,NDUR,DURAT,
     I             PTPOS,PTNEG,PTNW,PID)
      IF (PRFG.GE.2) THEN
        PID= PID+ 1
        CALL DUROUT (PUNIT,MXLEV,NLEV,LEVEL,MXDUR,NDUR,DURAT,
     I               PT1POS,PT1NEG,PT1NW,PID)
        IF (PRFG.GE.3) THEN
          PID= PID+ 1
          CALL DUROUT (PUNIT,MXLEV,NLEV,LEVEL,MXDUR,NDUR,DURAT,
     I                 SPOS,SNEG,SNW,PID)
          IF (PRFG.GE.4) THEN
            PID= PID+ 1
            CALL DUROUT (PUNIT,MXLEV,NLEV,LEVEL,MXDUR,NDUR,DURAT,
     I                   FRQPOS,FRQNEG,FRQNW,PID)
            IF (PRFG.GE.5) THEN
              PID= PID+ 1
              CALL DUROUT (PUNIT,MXLEV,NLEV,LEVEL,MXDUR,NDUR,DURAT,
     I                     MPOS,MNEG,MNW,PID)
              IF (PRFG.GE.6) THEN
                PID= PID+ 1
                CALL DUROUT (PUNIT,MXLEV,NLEV,LEVEL,MXDUR,NDUR,DURAT,
     I                       SQPOS,SQNEG,SQNW,PID)
                IF (PRFG.GE.7) THEN
                  PID= PID+ 1
                  CALL DUROUT (PUNIT,MXLEV,NLEV,LEVEL,MXDUR,NDUR,DURAT,
     I                         FREVPS,FREVNG,FREVNW,PID)
                END IF
              END IF
            END IF
          END IF
        END IF
      END IF
C
C     output summary information
      WRITE (PUNIT,2040)
      WRITE (PUNIT,2050) SPOS(1,1)
      WRITE (PUNIT,2060) SNW(1)
C     dum is length of analysis in days here
      DUM= SPOS(1,1)* DELT/ 1440
      WRITE (PUNIT,2065) DUM
      WRITE (PUNIT,2070) NUM
      WRITE (PUNIT,2080) MAX
      WRITE (PUNIT,2090) MINIM
      WRITE (PUNIT,2100) MEAN
      WRITE (PUNIT,2110) SUMSQ
C
      IF (LCNUM.GT.0) THEN
C       lethality output
        WRITE(PUNIT,2120)
        DO 110 I= 1,LCNUM
          DUM= LCTSTO(I)
          DUM= DUM/NUM
          WRITE(PUNIT,2130) I,DUM
 110    CONTINUE
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   DUROUT
     I                   (PUNIT,MXLEV,NLEV,LEVEL,MXDUR,NDUR,DURAT,
     I                    POS,NEG,NW,PID)
C
C     + + + PURPOSE + + +
C     general output of summary tables
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   PUNIT,MXLEV,NLEV,MXDUR,NDUR,DURAT(MXDUR),PID
      REAL      LEVEL(MXLEV),POS(MXDUR,MXLEV),NEG(MXDUR,MXLEV),NW(MXDUR)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     PUNIT  - print file unit number
C     MXLEV  - maximum number of levels allowed
C     NLEV   - number of levels in use
C     LEVEL  - values for each level
C     MXDUR  - maximum number of durations allowed
C     NDUR   - number of durations in use
C     DURAT  - values for each duration
C     POS    - positive excursion array
C     NEG    - negative excursion array
C     NW     - no water array
C     PID    - print table id
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,J
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT('1',///,
     $       ' FRACTION OF TIME EACH LEVEL EQUALED OR EXCEEDED ',
     $         'WITH DURATION >= THE SPECIFIED DURATIONS',
     $     /,' FRACTION IS RELATIVE TO TOTAL TIME SPAN')
 2010 FORMAT('1',///,
     $       ' FRACTION OF TIME SPENT IN EVENTS AT EACH ',
     $         'LEVEL WITH DURATION >= THE SPECIFIED DURATIONS',
     $     /,' FRACTION IS RELATIVE TO THE TIME SPENT IN ',
     $         'EVENTS AT EACH LEVEL')
 2020 FORMAT('1',///,
     $       ' TIME SPENT IN EVENTS AT EACH LEVEL WITH ',
     $         'DURATION >= THE SPECIFIED DURATIONS')
 2030 FORMAT('1',///,
     $       ' NUMBER OF EVENTS AT EACH LEVEL WITH DURATION ',
     $         '>= THE SPECIFIED DURATIONS')
 2040 FORMAT('1',///,
     $       ' AVERAGE DURATION OF EVENTS AT EACH LEVEL GIVEN ',
     $         'THAT THE DURATION >= THE SPECIFIED DURATIONS')
 2050 FORMAT('1',///,
     $       ' STANDARD DEVIATION OF DURATION OF EVENTS AT EACH ',
     $         'LEVEL GIVEN THAT THE DURATION >= THE SPECIFIED ',
     $         'DURATIONS')
 2055 FORMAT('1',///,
     $       ' FRACTION OF EVENTS WITH DURATION N WITH RESPECT ',
     $         'TO THE TOTAL NUMBER OF EXCURSIONS (DURATION 1)',
     $         'FOR EACH LEVEL')
C
 2060 FORMAT(    ' ',
     $       ///,' EVENTS GREATER THAN')
 2070 FORMAT(  /,'                       DURATIONS')
 2080 FORMAT(    '            ',10(1X,I10))
 2090 FORMAT(    ' LEVELS')
 2100 FORMAT(    ' ',11(1X,G10.4))
 2110 FORMAT(    ' ',
     $       ///,' EVENTS LESS THAN   ')
 2120 FORMAT(    ' ',
     $       ///,' UNDEFINED EVENTS (NO WATER)')
 2130 FORMAT(    '            ',10(1X,G10.4))
C
C     + + + END SPECIFICATIONS + + +
C
      IF (PID.EQ.1) THEN
        WRITE (PUNIT,2000)
      ELSE IF (PID.EQ.2) THEN
        WRITE (PUNIT,2010)
      ELSE IF (PID.EQ.3) THEN
        WRITE (PUNIT,2020)
      ELSE IF (PID.EQ.4) THEN
        WRITE (PUNIT,2030)
      ELSE IF (PID.EQ.5) THEN
        WRITE (PUNIT,2040)
      ELSE IF (PID.EQ.6) THEN
        WRITE (PUNIT,2050)
      ELSE IF (PID.EQ.7) THEN
        WRITE (PUNIT,2055)
      END IF
C
      WRITE (PUNIT,2060)
      WRITE (PUNIT,2070)
      WRITE (PUNIT,2080) (DURAT(I),I=1,NDUR)
      WRITE (PUNIT,2090)
C
      DO 10 J=1,NLEV
        WRITE (PUNIT,2100) LEVEL(J+1),(POS(I,J+1),I=1,NDUR)
   10 CONTINUE
C
      WRITE (PUNIT,2110)
      WRITE (PUNIT,2070)
      WRITE (PUNIT,2080) (DURAT(I),I=1,NDUR)
      WRITE (PUNIT,2090)
C
      DO 20 J=1,NLEV
        WRITE (PUNIT,2100) LEVEL(J+1),(NEG(I,J+1),I=1,NDUR)
   20 CONTINUE
C
      WRITE (PUNIT,2120)
      WRITE (PUNIT,2070)
      WRITE (PUNIT,2080) (DURAT(I),I=1,NDUR)
      WRITE (PUNIT,2130) (NW(I),I=1,NDUR)
C
      RETURN
      END
