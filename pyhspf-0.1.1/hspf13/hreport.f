C
C
C
      SUBROUTINE   PREPRT
     I                    (NDELT,SDATIM,NDAMON,EMFG,MAXOSV,
     M                     OSVKEY)
C
C     + + + PURPOSE + + +
C     Process the input for the report module
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   NDELT,SDATIM(5),NDAMON(12),EMFG,MAXOSV,OSVKEY
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NDELT  - simulation time interval in minutes ???
C     SDATIM - starting date/time
C     NDAMON - no. of days in each month of calendar year
C     EMFG   - english/metric units flag (english-1,metric-2)
C     MAXOSV - maximum size of osv
C     OSVKEY - key to where we are in OSV file
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION REPT1 + + +
      INCLUDE    'crin2.inc'
      INCLUDE    'crept.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,OSVREC,OSVKST,OSVKND,SCLU,SGRP,IVAL(8*MAXCON),PTR,
     $             CON,SRC,TIM,TBNO,TBSB,NVAL,INITFG,CLEN,CONT,I4,I7
      REAL         STVAL
      CHARACTER*4  KWDLIB(6)
      CHARACTER*80 CHSTR
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (CHSTR,CHSTR1),(KWDLIB,KWDLB1)
      CHARACTER*1  CHSTR1(80),KWDLB1(24)
C
C     + + + FUNCTIONS + + +
      INTEGER      CHKSTR,REPPTR
C
C     + + + EXTERNALS + + +
      EXTERNAL     WMSGTT,ITABLE,HSCKFL,CHKSTR,OMSTC,OMSG
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (7(A4,1X))
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' ',132('+'),
     $        /,' PROCESSING REPORT NO:',I4,
     $            '     TIME STEP(DELT):',I5,'  MINS')
 2010 FORMAT (/,' FINISHED PROCESSING REPORT NO. ',I4,
     $        /,' ',132('+'))
 2020 FORMAT (  ' ',132('-'))
 2030 FORMAT (A4)
C
C     + + + END SPECIFICATIONS + + +
C
      IF (RESMFG.EQ.1) THEN
C       read in the rest of the osv from osvfl
C       - not implemented in this release of hspf
      ELSE
C       initialize the entire osv area
        DO 10 I= 1,MAXOSV
          IPAD(I)= -999
 10     CONTINUE
      END IF
C
      SCLU= 374
      I4= 4
      I7= 7
C
      MESSU= FILE(1)
      MSGFL= FILE(15)
C
      IF (OUTLEV.GT.0) THEN
        WRITE (MESSU,2000) OPTNO,NDELT
      END IF
C
C     place run interp info into common
      REPTNO= OPTNO
      DELT  = NDELT
      CALL EXDATE (SDATIM,
     O             SDATE)
C      
      DO 30 I= 1,12
        NDAY(I)= NDAMON(I)
 30   CONTINUE
      DO 40 I= 1, 5
        DATIM(I)= SDATIM(I)
 40   CONTINUE
C
C     put english/metric units flag into common
      UUNITS = EMFG
C
C     size of osv for this operation
      OSVREC= 35
C
C     initialize counters
      PIVLNO= 0
C
C     get keyword and other strings
      SGRP  = 11
      INITFG= 1
      CLEN  = 80
      CALL WMSGTT (MSGFL,SCLU,SGRP,INITFG,
     M             CLEN,
     O             CHSTR1,CONT)
C
      READ (CHSTR,1000) KWDLIB
      READ (CHSTR,1000) TRNKWD
C
C     process table-type report-flags
      TBNO= 1
      TBSB= 1
      NVAL= 9
      CALL ITABLE (TBNO,TBSB,NVAL,UUNITS,
     M             RPTINF)
C
C     check files - if not open, then open with standard name
      IF (REPTFL .GT. 0) THEN
C       make sure output file is open
        CALL HSCKFL (REPTFL)
      END IF
      IF (FORMFG .GE. 4) THEN
C       make sure input file is open
        CALL HSCKFL (FORMFG)
      END IF
C
      IF (FORMFG .GE. 1) THEN
C       process table-type report-title
        TBNO= 2
        TBSB= 1
        NVAL= 15
        CALL ITABLE (TBNO,TBSB,NVAL,UUNITS,
     M               TITLE)
      END IF
C
C     process table-type report-src
      TBNO= 3
      TBSB= 1
      NVAL= NSRC*5
      CALL ITABLE (TBNO,TBSB,NVAL,UUNITS,
     M             IVAL)
      DO 50 SRC= 1, NSRC
        SRCID(1,SRC)= IVAL(SRC*5- 4)
        SRCID(2,SRC)= IVAL(SRC*5- 3)
        SRCID(3,SRC)= IVAL(SRC*5- 2)
        SRCID(4,SRC)= IVAL(SRC*5- 1)
        SRCID(5,SRC)= IVAL(SRC*5)
 50   CONTINUE
C
C     process table-type report-con
      TBNO= 4
      TBSB= 1
      NVAL= NCON*8
      CALL ITABLE (TBNO,TBSB,NVAL,UUNITS,
     M             IVAL)
      DO 120 CON= 1, NCON
        CONID(1,CON)= IVAL(CON*8- 7)
        CONID(2,CON)= IVAL(CON*8- 6)
        CONID(3,CON)= IVAL(CON*8- 5)
        CONID(4,CON)= IVAL(CON*8- 4)
        CONID(5,CON)= IVAL(CON*8- 3)
        SDIG(CON)= IVAL(CON*8- 1)
        DECPLA(CON)= IVAL(CON*8)
C
C       check constituent transformation keyword
        WRITE (CHSTR(1:4),2030) IVAL(CON*8- 2)
        TRNCOD(CON)= CHKSTR (I4,I7,CHSTR1,KWDLB1)
C
        IF (TRNCOD(CON) .EQ. 0) THEN
C         invalid keyword
          WRITE (CHSTR(1:4),2030) IVAL(CON*8- 2)
          CALL OMSTC (I4,CHSTR1)
          SGRP= 12
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
        ELSE IF (TRNCOD(CON) .EQ. 7) THEN
C         field was left blank
          TRNCOD(CON)= 1
        END IF
C
C       determine the initial value to go into arrays/accumulators
C       casentry trncod
        GO TO (60,60,70,80,60),TRNCOD(CON)
C
 60     CONTINUE
C         case 1, 2, and 5       sum, aver, and last
          STVAL= 0.0
          GO TO 90
 70     CONTINUE
C         case 3                 max
          STVAL= -1.0E30
          GO TO 90
 80     CONTINUE
C         case 4                 min
          STVAL= +1.0E30
          GO TO 90
C         endcase
 90     CONTINUE
C
        MTIM= INT (MAXACC/(NSRC*NCON))
        DO 110 SRC= 1, NSRC
          DO 100 TIM= 1, MTIM
            PTR= REPPTR (NCON,NSRC,CON,SRC,TIM)
            ACCUM(PTR)= STVAL
 100      CONTINUE
 110    CONTINUE
C
 120  CONTINUE
C
      IF (FORMFG .GE. 1) THEN
C       process table-type report-summ
        TBNO= 5
        TBSB= 1
        NVAL= 13
        CALL ITABLE (TBNO,TBSB,NVAL,UUNITS,
     M               IVAL)
        DO 130 I= 1, 5
          SRCHED(I)= IVAL(I)
          TIMHED(I)= IVAL(I+6)
 130    CONTINUE
        STTRFG= IVAL(13)
C
C       check source summary transformation keyword
        WRITE (CHSTR(1:4),2030) IVAL(6)
        SRCTRC= CHKSTR (I4,I7,CHSTR1,KWDLB1)
C
        IF (SRCTRC .EQ. 0) THEN
C         invalid keyword
          WRITE (CHSTR(1:4),2030) IVAL(6)
          CALL OMSTC (I4,CHSTR1)
          SGRP= 12
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
        ELSE IF (SRCTRC .EQ. 7) THEN
C         field was left blank
          SRCTRC= 1
        END IF
C
C       check time summary transformation keyword
        WRITE (CHSTR(1:4),2030) IVAL(12)
        TIMTRC= CHKSTR (I4,I7,CHSTR1,KWDLB1)
C
        IF (TIMTRC .EQ. 0) THEN
C         invalid keyword
          WRITE (CHSTR(1:4),2030) IVAL(12)
          CALL OMSTC (I4,CHSTR1)
          SGRP= 12
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
        ELSE IF (TIMTRC .EQ. 7) THEN
C         field was left blank
          TIMTRC= 1
        END IF
      ELSE
C       dummy values for database format
        SRCTRC= 1
        TIMTRC= 1
        STTRFG= 2
      END IF
C
C     initialize timestep pointer
      CURTIM= 1
C
C     write the osv to disc and record the keys in opntab
      OSVKST= OSVKEY+ 1
      OSVKND= OSVKEY+ OSVREC
      CALL PUTOSV (OSVKST,OSVKND,MAXOSV,IPAD)
      OPNTAB(7,OPNO)= OSVKST
      OPNTAB(8,OPNO)= OSVKND
      OSVKEY        = OSVKND
C
      IF (OUTLEV.GT.0) THEN
C       done processing message
        WRITE (MESSU,2010) REPTNO
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   REPORT
     I                   (STIVL,WIDTH,LSTCAL)
C
C     + + + PURPOSE + + +
C     Write a report to disk.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    STIVL,WIDTH,LSTCAL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     STIVL  - of inpad
C     WIDTH  - inpad width
C     LSTCAL - flag indicating last interval of run
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION REPT2 + + +
      INCLUDE    'crept.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     IDELT,I1,MPIVL,CON,SRC,TIM,SCLU,PTR,PTR1,PTR2,PTR3
      INTEGER     DUM1,DUM2,DUM3
      REAL        VAL,SUMTIM(MAXCON*MAXSRC),SUMSRC(MAXCON*MAXTIM),
     $            SUMTS(MAXCON),STVAL(6)
C
C     + + + FUNCTIONS + + +
      INTEGER     REPPTR
C
C     + + + EXTERNALS + + +
      EXTERNAL    ADDTIM,TRANS,RPTLAB,REPWRT,REPPTR
C
C     + + + DATA INITIALIZATIONS + + +
      DATA I1/1/,MPIVL/999999/
      DATA STVAL/0.0,0.0,-1.0E30,1.0E30,0.0,0.0/
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU= 374
C
      IVL=  STIVL- 1
      IVL1= STIVL
      IDELT= DELT
C
C     time loop
      DO 50 IVL= STIVL,WIDTH+ STIVL- 1
        IVL1= IVL1+ 1
C
C       increment date/time
        CALL ADDTIM (IDELT,NDAY,MPIVL,PYREND,
     M               DATIM,PIVLNO,
     O               NDAYS,DUM1,DUM2,DUM3,EDAYFG,EMONFG,EPYRFG)
C
        IF (CURTIM .LE. MTIM) THEN
C         ready to handle new data
C
C         accumulation loop
          DO 20 CON= 1, NCON
            DO 10 SRC= 1, NSRC
              IF (IFP(CON,SRC) .GE. 1) THEN
C               fetch and accumulate value
                VAL= PAD(IFP(CON,SRC)+ IVL1)
C               accumulate according to trasformation code
C               for now, treat AVER as SUM by using I1 instead of NIVL
                PTR= REPPTR (NCON,NSRC,CON,SRC,CURTIM)
                CALL TRANS (TRNCOD(CON),I1,VAL,
     M                      ACCUM(PTR))
              END IF
 10         CONTINUE
 20       CONTINUE
C
          IF ( ( (PCODE .EQ. 3) .AND. (EDAYFG .EQ. 1) ) .OR.
     $         ( (PCODE .EQ. 4) .AND. (EMONFG .EQ. 1) ) .OR.
     $         ( (PCODE .EQ. 5) .AND. (EPYRFG .EQ. 1) ) ) THEN
C           we have reached the end of the current accumulation period
            IF (PIVLNO .GT. 1) THEN
C             do averaging as needed
              DO 40 CON= 1, NCON
                IF (TRNCOD(CON) .EQ. 2) THEN
C                 this sum must be divided by number of intervals
                  DO 30 SRC= 1, NSRC
                    PTR= REPPTR (NCON,NSRC,CON,SRC,CURTIM)
                    ACCUM(PTR)= ACCUM(PTR)/PIVLNO
 30               CONTINUE
                END IF
 40           CONTINUE
            END IF
C
C           create time label for current timestep
            CALL RPTLAB (DATIM,PCODE,
     O                   TIMLAB(1,CURTIM))
C
C           increment interval pointer and reset interval counter
            CURTIM= CURTIM+ 1
            PIVLNO= 0
          END IF
        END IF
C
C       end of interval loop
 50   CONTINUE
C
      IF (LSTCAL .EQ. 1) THEN
C       time to write out report
C
        IF ( (PIVLNO .EQ. 0) .OR. (CURTIM .GT. MTIM) )THEN
C         undo last interval pointer increment - accumulator empty
          CURTIM= CURTIM- 1
        END IF
C
        IF (CURTIM .GE. 1) THEN
C         at least one value was accumulated
C
C         compute summaries
          DO 100 CON= 1, NCON
C
C           initialize overall summary
            IF (STTRFG .EQ. 1) THEN
C             overall summary is based on time summaries
              SUMTS(CON)= STVAL(SRCTRC)
            ELSE IF (STTRFG .EQ. 2) THEN
C             overall summary is based on source summaries
              SUMTS(CON)= STVAL(TIMTRC)
            END IF
C
C           summaries over sources
            DO 70 TIM= 1, CURTIM
              PTR2= REPPTR (NCON,I1,CON,TIM,I1)
              SUMSRC(PTR2)= STVAL(SRCTRC)
              DO 60 SRC= 1, NSRC
                PTR1= REPPTR (NCON,NSRC,CON,SRC,TIM)
                CALL TRANS (SRCTRC,NSRC,ACCUM(PTR1),
     M                      SUMSRC(PTR2))
 60           CONTINUE
              IF (STTRFG .EQ. 2) THEN
C               overall summary is based on source summaries
                CALL TRANS (TIMTRC,CURTIM,SUMSRC(PTR2),
     M                      SUMTS(CON))
              END IF
 70         CONTINUE
C
C           summaries over time
            DO 90 SRC= 1, NSRC
              PTR2= REPPTR (NCON,I1,CON,SRC,I1)
              SUMTIM(PTR2)= STVAL(TIMTRC)
              DO 80 TIM= 1, CURTIM
                PTR1= REPPTR (NCON,NSRC,CON,SRC,TIM)
                CALL TRANS (TIMTRC,CURTIM,ACCUM(PTR1),
     M                      SUMTIM(PTR2))
 80           CONTINUE
              IF (STTRFG .EQ. 1) THEN
C               overall summary is based on source summaries
                CALL TRANS (SRCTRC,CURTIM,SUMTIM(PTR2),
     M                      SUMTS(CON))
              END IF
 90         CONTINUE
C
 100      CONTINUE
C
          IF (SRCTRC .EQ. 6) THEN
C           percent source summary - divide through by total for all sources
            DO 130 SRC= 1, NSRC
              DO 120 CON= 1, NCON
                DO 110 TIM= 1, CURTIM
                  PTR1= REPPTR (NCON,NSRC,CON,SRC,TIM)
                  PTR2= REPPTR (NCON,I1,CON,TIM,I1)
                  IF (SUMSRC(PTR2) .EQ. 0.0) THEN
C                   avoid division by zero
                    ACCUM(PTR1)= 0.0
                  ELSE
C                   calculate percentage
                    ACCUM(PTR1)= ACCUM(PTR1)/SUMSRC(PTR2)*100.0
                  END IF
 110            CONTINUE
                PTR3= REPPTR (NCON,I1,CON,SRC,I1)
                IF (SUMTS(CON) .EQ. 0.0) THEN
C                 avoid division by zero
                  SUMTIM(PTR3)= 0.0
                ELSE
C                 calculate percentage
                  SUMTIM(PTR3)= SUMTIM(PTR3)/SUMTS(CON)*100.0
                END IF
 120          CONTINUE
 130        CONTINUE
            DO 150 CON= 1, NCON
              DO 140 TIM= 1, CURTIM
                PTR2= REPPTR (NCON,I1,CON,TIM,I1)
                SUMSRC(PTR2)= 100.0
 140          CONTINUE
              SUMTS(CON)= 100.0
 150        CONTINUE
          END IF
C
          CALL REPWRT (REPTNO,REPTFL,FORMFG,TITLE,NSRC,SRCID,NCON,
     I                 CONID,CURTIM,TIMLAB,MAXCON,MAXACC,ACCUM,SUMTIM,
     I                 SUMSRC,SUMTS,SDIG,DECPLA,TRNCOD,TRNKWD,SRCHED,
     I                 TIMHED,CWID,PWID,PLIN,SCLU,MESSU,MSGFL,IFP)
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   RPTLAB
     I                    (DATIM,PCODE,
     O                     TIMLAB)
C
C     + + + PURPOSE + + +
C     Create the current time label based on date and print
C     interval code.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     DATIM,PCODE,TIMLAB(3)
C
C     + + + LOCAL VARIABLES
      INTEGER      CURDAT(5)
      CHARACTER*3  MONNAM(12)
      CHARACTER*12 BUFF
C
C     + + + EXTERNALS + + +
      EXTERNAL     EXDATE
C
C     + + + DATA INITIALIZATIONS + + +
      DATA MONNAM/'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug',
     $            'Sep','Oct','Nov','Dec'/
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (3A4)
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (I4,8X)
 2010 FORMAT (A3,1X,I4,4X)
 2020 FORMAT (I2,1X,A3,1X,I4,1X)
C
C     + + + END SPECIFICATIONS + + +
C
      CALL EXDATE (DATIM,
     O             CURDAT)
C
      IF (PCODE .EQ. 5) THEN
C       annual label
        WRITE (BUFF,2000) CURDAT(1)
      ELSE IF (PCODE .EQ. 4) THEN
C       monthly label
        WRITE (BUFF,2010) MONNAM(CURDAT(2)),CURDAT(1)
      ELSE IF (PCODE .EQ. 3) THEN
C       daily label
        WRITE (BUFF,2020) CURDAT(3),MONNAM(CURDAT(2)),CURDAT(1)
      END IF
      READ (BUFF,1000) TIMLAB
C
      RETURN
      END
C
C
C
      SUBROUTINE   REPWRT
     I                    (REPTNO,REPTFL,FORMFG,TITLE,NSRC,SRCID,NCON,
     I                     CONID,CURTIM,TIMLAB,MAXCON,MAXACC,ACCUM,
     I                     SUMTIM,SUMSRC,SUMTS,SDIG,DECPLA,TRNCOD,
     I                     TRNKWD,SRCHED,TIMHED,CWID,PWID,PLIN,SCLU,
     I                     MESSU,MSGFL,IFP)
C
C     + + + PURPOSE + + +
C     Write out a report in user-defined format.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER        REPTNO,REPTFL,FORMFG,TITLE(15),NSRC,SRCID(5,NSRC),
     $               NCON,CONID(5,NCON),CURTIM,TIMLAB(3,CURTIM),MAXCON,
     $               MAXACC,SDIG(NCON),DECPLA(NCON),TRNCOD(NCON),
     $               TRNKWD(7),SRCHED(5),TIMHED(5),CWID,PWID,PLIN,SCLU,
     $               MESSU,MSGFL,IFP(MAXCON,NSRC)
      REAL           ACCUM(MAXACC),SUMTIM(NCON*NSRC),
     $               SUMSRC(NCON*CURTIM),SUMTS(NCON)
C
C     + + + PARAMETERS + + +
      INTEGER        MAXFLD,MAXLIN
      PARAMETER     (MAXFLD=50)
      PARAMETER     (MAXLIN=40)
C
C     + + + LOCAL VARIABLES + + +
      INTEGER        I,J,I0,I1,I3,I4,I9,I12,I20,I60,I256,I1000,FMTLEN,
     $               REM,ATPOS,KEY,CON,SRC,TIM,ERRFLG,RPECNT(3),CONT,
     $               INITFG,LINEPT,LINECT,KPOS,LPLEV(3),MAXLP(3),
     $               LEVCT(3),LPIDX(3),LOOPFG,SGRP,BUFLEN,NFLD,FLD,
     $               A,B,VALFG,FLDKEY(MAXFLD),FLDPOS(MAXFLD),
     $               FLDLEN(MAXFLD),FLDSUB(3,MAXFLD),FLDCOL(MAXFLD),
     $               WRTFLG,FPOS,BPOS,SUB(3),PTR,NXTAT,NXTFMT,LDEC
      REAL           VAL
      CHARACTER*1    AT,BLANK,CTAB,COMMA,DELIM
      CHARACTER*3    LVKWD(3),INKWD(3)
      CHARACTER*4    ATKWD(9)
      CHARACTER*60   TBUFF
      CHARACTER*256  BUFF(MAXLIN),FMT
      CHARACTER*1000 OBUFF
C
C     + + + EQUIVALENCE + + +
      CHARACTER*1    FMT1(256),OBUFF1(1000),TBUFF1(60),ATKWD1(36),
     $               LVKWD1(9),INKWD1(9)
      EQUIVALENCE   (FMT,FMT1),(OBUFF,OBUFF1),(TBUFF,TBUFF1),
     $              (ATKWD,ATKWD1),(LVKWD,LVKWD1),(INKWD,INKWD1)
C
C     + + + FUNCTIONS + + +
      INTEGER        LENSTR,STRFND,CHKSTR,REPPTR
C
C     + + + INTRINSICS + + +
      INTRINSIC      MIN,MAX,ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL       ZIPI,LENSTR,WMSGTT,ZIPC,STRFND,CHKSTR,RPINDX,
     $               COPYC,LFTSTR,RHTSTR,DECCHX,REPPTR
C
C     + + + DATA INITIALIZATIONS + + +
      DATA I0,I1,I3,I4,I9,I12,I20,I60,I256,I1000
     $     /0, 1, 3, 4, 9, 12, 20, 60, 256, 1000/
      DATA AT,BLANK,COMMA/'@',' ',','/
      DATA ATKWD/'LOOP','TITL','TRAN','CON:',
     $           'SRC:','TIM:','SSUM','TSUM','BLAN'/
      DATA LVKWD,INKWD/'TAB','ROW','COL','CON','SRC','TIM'/
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (A256)
 1010 FORMAT (I2)
 1020 FORMAT (I2,1X,I2,1X,I2)
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (15A4)
 2010 FORMAT (1PE20.5)
 2020 FORMAT (1000A1)
 2030 FORMAT (2(5A4),3A4,100A1)
C
C     + + + END SPECIFICATIONS + + +
C
C     set error counters
      RPECNT(1)= 0
      RPECNT(2)= 0
      RPECNT(3)= 0
C
      CTAB= CHAR(9)
      IF (CWID .EQ. -1) THEN
C       tab-delimited buffer
        DELIM= CTAB
      ELSE IF (CWID .EQ. -2) THEN
C       comma-delimited buffer
        DELIM= COMMA
      END IF
C
      IF (FORMFG .EQ. 0) THEN
C       database format
        DO 7 CON= 1, NCON
          DO 5 SRC= 1, NSRC
            IF (IFP(CON,SRC) .GE. 1) THEN
C             a timeseries was input for this variable
              DO 3 TIM= 1, CURTIM
                PTR= REPPTR (NCON,NSRC,CON,SRC,TIM)
                VAL= ACCUM(PTR)
                CALL ZIPC (I1000,BLANK,
     O                     OBUFF1)
                BPOS= 1
                IF (CWID .GE. 1) THEN
C                 formatted buffer
                  CALL DECCHX (VAL,CWID,SDIG(CON),DECPLA(CON),
     O                         OBUFF1(1))
                  CALL RHTSTR (CWID,
     M                         OBUFF1(1))
                  WRITE (REPTFL,2030) (CONID(J,CON), J= 1, 5),
     $                                (SRCID(J,SRC), J= 1, 5),
     $                                (TIMLAB(J,TIM), J= 1, 3),
     $                                (OBUFF1(J),J=1,CWID)
                ELSE
C                 delimited buffer
                  WRITE (OBUFF(1:20),2000) (CONID(J,CON), J= 1, 5)
                  CALL LFTSTR (I20,
     O                         OBUFF1(1))
                  BPOS= LENSTR (I1000,OBUFF1)+ 1
                  OBUFF1(BPOS)= DELIM
                  WRITE (OBUFF(BPOS+1:BPOS+20),2000)
     $                                     (SRCID(J,SRC), J= 1, 5)
                  CALL LFTSTR (I20,
     O                         OBUFF1(BPOS+1))
                  BPOS= LENSTR (I1000,OBUFF1)+ 1
                  OBUFF1(BPOS)= DELIM
                  WRITE (OBUFF(BPOS+1:BPOS+12),2000)
     $                                     (TIMLAB(J,TIM), J= 1, 3)
                  CALL LFTSTR (I12,
     O                         OBUFF1(BPOS+1))
                  BPOS= LENSTR (I1000,OBUFF1)+ 1
                  OBUFF1(BPOS)= DELIM
                  CALL DECCHX (VAL,I20,SDIG(CON),DECPLA(CON),
     O                         OBUFF1(BPOS+1))
                  CALL LFTSTR (I20,
     O                         OBUFF1(BPOS+1))
                  BUFLEN= LENSTR (I1000,OBUFF1)
                  WRITE (REPTFL,2020) (OBUFF1(J),J=1, BUFLEN)
                END IF
 3            CONTINUE
            END IF
 5        CONTINUE
 7      CONTINUE
      ELSE
C       tabular format
        LOOPFG= 0
        LINECT= 0
        I= 3
        CALL ZIPI (I,I0,
     O             LPLEV)
        CALL ZIPI (I,I0,
     O             LPIDX)
C
        IF (FORMFG .GE. 4) THEN
C         first rewind format file in case it is used by more than 1 report
          REWIND (FORMFG)
        ELSE
C         initialize for reading template
          INITFG= 1
        END IF
C
C       begin loop on tables
        LEVCT(1)= 0
        MAXLP(1)= 1
 10     CONTINUE
          LEVCT(1)= LEVCT(1)+ 1
          LINEPT= 0
C
C         begin loop on format lines
 20       CONTINUE
C
C           first, fetch a new line from input file, wdm file, or stored buffer
            IF (LEVCT(1) .GE. 2) THEN
C             fetch line from buffer
              LINEPT= LINEPT+ 1
              FMT= BUFF(LINEPT)
              FMTLEN= LENSTR (I256,FMT1)
              IF (LINEPT .LT. LINECT) THEN
C               more lines to read after this one
                CONT= 1
              ELSE
C               last line
                CONT= 0
              END IF
            ELSE
C             reading lines for first time
              IF (FORMFG .GE. 4) THEN
C               using format file
                READ (FORMFG,1000,END=120) FMT
                FMTLEN= LENSTR (I256,FMT1)
                CONT= 1
              ELSE
C               template in message file
                SGRP= FORMFG
                FMTLEN= 80
                CALL WMSGTT (MSGFL,SCLU,SGRP,INITFG,
     M                       FMTLEN,
     O                       FMT1,CONT)
                INITFG= 0
              END IF
              I= 256- FMTLEN
              CALL ZIPC (I,BLANK,
     O                   FMT1(FMTLEN+1))
              IF ( (LOOPFG .EQ. 1) .AND. (LEVCT(1) .EQ. 1) ) THEN
C               first time through loop - store unexpanded line
                LINECT= LINECT+ 1
                BUFF(LINECT)= FMT
              END IF
            END IF
C
C           begin loop on rows
            LEVCT(2)= 0
            MAXLP(2)= 1
            WRTFLG= 1
            NFLD= 0
 30         CONTINUE
              LEVCT(2)= LEVCT(2)+ 1
              FPOS= 1
C
              IF (LEVCT(2) .EQ. 1) THEN
C               first time through for this row
C
                IF (CWID .GE. 1) THEN
C                 copy format into output buffer
                  OBUFF= FMT
                  BUFLEN= FMTLEN
                ELSE
C                 blank output buffer for delimited file
                  CALL ZIPC (I1000,BLANK,
     O                       OBUFF1)
                  BUFLEN= 1
                END IF
                BPOS= 1
C
C               begin loop on fields
 40             CONTINUE
                  MAXLP(3)= 1
                  ERRFLG= 0
                  REM= FMTLEN- FPOS+ 1
                  IF (REM .GE. 1) THEN
C                   search rest of line for next field
                    ATPOS= STRFND (REM,FMT1(FPOS),I1,AT)
                  ELSE
C                   at end of line
                    ATPOS= 0
                  END IF
                  IF (ATPOS .GE. 1) THEN
C                   process a field marker
C
C                   move pointers in format and output buffer
                    FPOS= FPOS+ ATPOS- 1
                    BPOS= BPOS+ ATPOS- 1
C
C                   look for next field
                    REM= FMTLEN- FPOS
                    IF (REM .GE. 1) THEN
C                     look for next field, if any
                      NXTAT= STRFND (REM,FMT1(FPOS+1),I1,AT)
                    ELSE
                      NXTAT= 0
                    END IF
C
C                   process keyword
                    KEY= CHKSTR (I4,I9,FMT1(FPOS+1),ATKWD1)
                    IF (KEY .EQ. 1) THEN
C                     loop keyword
                      IF (LOOPFG .EQ. 0) THEN
C                       havent processed loop line yet
                        LOOPFG= 1
                        WRTFLG= 0
C
C                       parse loop keywords
                        REM= REM- 6
                        FPOS= FPOS+ 6
                        DO 60 I= 1, 3
                          KPOS= STRFND (REM,FMT1(FPOS),I3,
     I                                  LVKWD1(I*3-2))
                          IF (KPOS .GE. 1) THEN
C                           loop keyword found
                            KPOS= KPOS+ FPOS- 1
                            LPLEV(I)= CHKSTR (I3,I3,FMT1(KPOS+4),INKWD1)
                            IF (LPLEV(I) .EQ. 0) THEN
                              STOP 'INVALID LOOP INDEX NAME'
                            END IF
                            DO 50 J= 1, I-1
                              IF (LPLEV(I) .EQ. LPLEV(J)) THEN
                                STOP 'DUPLICATE LOOP NOT ALLOWED'
                              END IF
 50                         CONTINUE
                            LPIDX(LPLEV(I))= I
                          END IF
 60                     CONTINUE
                      END IF
                    ELSE
C                     this is a normal line
                      NFLD= NFLD+ 1
                      FLDKEY(NFLD)= KEY
                      FLDPOS(NFLD)= BPOS
                      FLDSUB(1,NFLD)= 0
                      FLDSUB(2,NFLD)= 0
                      FLDSUB(3,NFLD)= 0
                      IF (KEY .EQ. 2) THEN
C                       insert title
                        FLDLEN(NFLD)= 60
                      ELSE IF (KEY .EQ. 3) THEN
C                       insert transform code for a constituent
                        FLDLEN(NFLD)= 4
                        CALL RPINDX (REPTNO,MESSU,MSGFL,SCLU,
     I                               FMT(FPOS+6:FPOS+7),LEVCT,
     I                               LPIDX(1),I1,NCON,
     M                               RPECNT,MAXLP,
     O                               ERRFLG,FLDSUB(1,NFLD))
                      ELSE IF (KEY .EQ. 4) THEN
C                       insert name of a constituent
                        FLDLEN(NFLD)= 20
                        CALL RPINDX (REPTNO,MESSU,MSGFL,SCLU,
     I                               FMT(FPOS+5:FPOS+6),LEVCT,
     I                               LPIDX(1),I1,NCON,
     M                               RPECNT,MAXLP,
     O                               ERRFLG,FLDSUB(1,NFLD))
                      ELSE IF (KEY .EQ. 5) THEN
C                       insert name of a source
                        FLDLEN(NFLD)= 20
                        CALL RPINDX (REPTNO,MESSU,MSGFL,SCLU,
     I                               FMT(FPOS+5:FPOS+6),LEVCT,
     I                               LPIDX(2),I1,NSRC,
     M                               RPECNT,MAXLP,
     O                               ERRFLG,FLDSUB(2,NFLD))
                      ELSE IF (KEY .EQ. 6) THEN
C                       insert timestep label
                        FLDLEN(NFLD)= 12
                        CALL RPINDX (REPTNO,MESSU,MSGFL,SCLU,
     I                               FMT(FPOS+5:FPOS+6),LEVCT,
     I                               LPIDX(3),I1,CURTIM,
     M                               RPECNT,MAXLP,
     O                               ERRFLG,FLDSUB(3,NFLD))
                      ELSE IF ( (KEY .EQ. 7) .OR. (KEY .EQ. 8) ) THEN
C                       insert source or time summary header
                        READ (FMT(FPOS+6:FPOS+7),1010,ERR=70)
     $                        FLDLEN(NFLD)
                          IF ( (FLDLEN(NFLD).LE. 1) .OR.
     $                         (FLDLEN(NFLD) .GE. 20) ) THEN
C                           error in summary header width
                          GO TO 70
                        END IF
                        GO TO 80
 70                     CONTINUE
C                         error - bad summary header width
                          WRITE (*,*) 'ERROR - BAD SUMMARY HEADER WIDTH'
                          STOP
 80                     CONTINUE
                      ELSE IF (KEY .EQ. 9) THEN
C                       blank field
                        IF (CWID .GE. 1) THEN
C                         formatted buffer - skip at least keyword width
                          FLDLEN(NFLD)= MAX (CWID,6)
                        ELSE
C                         delimited buffer
                          FLDLEN(NFLD)= 1
                        END IF
                      ELSE
C                       value field
                        IF (CWID .GE. 1) THEN
C                         formatted buffer - use natural column width
                          FLDLEN(NFLD)= CWID
                        ELSE
C                         delimited buffer - skip to next character
                          FLDLEN(NFLD)= 1
                        END IF
C
C                       get first index - constituent
                        CALL RPINDX (REPTNO,MESSU,MSGFL,SCLU,
     I                               FMT(FPOS+1:FPOS+2),LEVCT,
     I                               LPIDX(1),I1,NCON,
     M                               RPECNT,MAXLP,
     O                               ERRFLG,FLDSUB(1,NFLD))
                        IF (ERRFLG .EQ. 0) THEN
C                         get second index - source
                          CALL RPINDX (REPTNO,MESSU,MSGFL,SCLU,
     I                                 FMT(FPOS+4:FPOS+5),LEVCT,
     I                                 LPIDX(2),I0,NSRC,
     M                                 RPECNT,MAXLP,
     O                                 ERRFLG,FLDSUB(2,NFLD))
                        END IF
                        IF (ERRFLG .EQ. 0) THEN
C                         get third index - timestep
                          CALL RPINDX (REPTNO,MESSU,MSGFL,SCLU,
     I                                 FMT(FPOS+7:FPOS+8),LEVCT,
     I                                 LPIDX(3),I0,CURTIM,
     M                                 RPECNT,MAXLP,
     O                                 ERRFLG,FLDSUB(3,NFLD))
                        END IF
                      END IF
                      IF (ERRFLG .EQ. 0) THEN
C                       process column information for this field
                        FLDCOL(NFLD)= MAXLP(3)
C
                        IF (CWID .GE. 1) THEN
C                         formatted buffer
                          IF (MAXLP(3) .GT. 1) THEN
C                           this field loops on columns
C
C                           correct field length for column width
                            FLDLEN(NFLD)= CWID
C
C                           update buffer and pointer
C
C                           compute expanded width
                            I= CWID*FLDCOL(NFLD)
C
C                           make sure there is room in the buffer
                            J= I1000- BPOS+ 1
                            I= MIN (I,J)
C
C                           make sure field doesn't step on next keyword
C                           in case cwid is bigger than keyword spacing
C                           - subsequent fields will be pushed back
                            NXTFMT= FLDLEN(NFLD)
                            IF (NXTAT .GE. 1) THEN
C                             don't overwrite next field marker
                              NXTFMT= MIN (NXTAT,NXTFMT)
                            END IF
C
C                           push rest of buffer back to make room for columns
                            B= BPOS+ NXTFMT
                            A= BUFLEN- B+ 1
                            CALL COPYC (A,OBUFF1(B),
     O                                  OBUFF1(BPOS+I))
C
C                           blank out added space for columns 2-n
                            A= I- CWID
                            CALL ZIPC (A,BLANK,
     O                                 OBUFF1(BPOS+CWID))
C
C                           update buffer pointer and length
                            BPOS= BPOS+ I
                            BUFLEN= LENSTR (I1000,OBUFF)
C
C                           update position in format
                            FPOS= FPOS+ NXTFMT
                          ELSE
C                           not a looped column
                            IF (NXTAT .GE. 1) THEN
C                             truncate this field to avoid stepping on next
                              FLDLEN(NFLD)= MIN (NXTAT,FLDLEN(NFLD))
                            END IF
C
C                           update buffer pointer
                            BPOS= BPOS+ FLDLEN(NFLD)
C
C                           update position in format
                            FPOS= FPOS+ FLDLEN(NFLD)
                          END IF
C
                          IF (BPOS .GT. BUFLEN) THEN
C                           extend output buffer to end of last field
                            BUFLEN= BPOS
                          END IF
                        ELSE
C                         delimited buffer
                          IF (MAXLP(3) .GT. 1) THEN
C                           this field loops on columns
C
C                           move forward one char in format to get next field
                            FPOS= FPOS+ 1
                          ELSE
C                           no column loop - update position in format
                            FPOS= FPOS+ FLDLEN(NFLD)
                          END IF
                        END IF
                      ELSE
C                        there was an error reading a subscript
                        FLDKEY(NFLD)= -1
                      END IF
                    END IF
                  END IF
C
C               end of loop on fields
                IF ( (ATPOS .GE. 1) .AND. (FPOS .LT. FMTLEN) ) GO TO 40
              END IF
C
C             fill in values of fields for current row
              BPOS= 1
              DO 110 FLD= 1, NFLD
                BPOS= FLDPOS(FLD)
C
C               begin loop on columns
                LEVCT(3)= 0
 90             CONTINUE
                  LEVCT(3)= LEVCT(3)+ 1
C
C                 set temporary subscripts
                  DO 100 I= 1, 3
                    SUB(I)= FLDSUB(I,FLD)
                    IF ( (LPIDX(I) .GE. 1) .AND. (SUB(I) .GE. 1) ) THEN
C                     reset temporary subscript for current loop
                      SUB(I)= LEVCT(LPIDX(I))
                    END IF
 100              CONTINUE
C
                  CON= SUB(1)
                  SRC= SUB(2)
                  TIM= SUB(3)
                  CALL ZIPC (I60,BLANK,
     O                       TBUFF1)
                  IF (FLDKEY(FLD) .NE. 0) THEN
C                   character variable
                    IF (FLDKEY(FLD) .EQ. 2) THEN
C                     insert title
                      WRITE (TBUFF,2000) (TITLE(I),I=1,15)
                    ELSE IF (FLDKEY(FLD) .EQ. 3) THEN
C                     insert transform code for a constituent
                      WRITE (TBUFF,2000) TRNKWD(TRNCOD(CON))
                    ELSE IF (FLDKEY(FLD) .EQ. 4) THEN
C                     insert name of a constituent
                      WRITE (TBUFF,2000) (CONID(I,CON),I=1,5)
                    ELSE IF (FLDKEY(FLD) .EQ. 5) THEN
C                     insert name of a source
                      WRITE (TBUFF,2000) (SRCID(I,SRC),I=1,5)
                    ELSE IF (FLDKEY(FLD) .EQ. 6) THEN
C                     insert timestep label
                      WRITE (TBUFF,2000) (TIMLAB(I,TIM),I=1,3)
                    ELSE IF (FLDKEY(FLD) .EQ. 7) THEN
C                     insert source summary header
                      WRITE (TBUFF,2000) (SRCHED(I),I=1,5)
                    ELSE IF (FLDKEY(FLD) .EQ. 8) THEN
C                     insert time summary header
                      WRITE (TBUFF,2000) (TIMHED(I),I=1,5)
                    END IF
                    IF (CWID .LE. -1) THEN
C                     delimited buffer
                      BPOS= LENSTR (I1000,OBUFF1)+ 1
                      CALL LFTSTR (I60,
     O                             TBUFF1)
                      A= LENSTR (I60,TBUFF1)
                      IF (A .GE. 1) THEN
C                       something to write
                        CALL COPYC (A,TBUFF1,
     O                              OBUFF1(BPOS))
                      END IF
                      BUFLEN= MAX (BPOS+ A,I1)
                      OBUFF1(BUFLEN)= DELIM
                    ELSE
C                     formatted buffer
                      CALL COPYC (FLDLEN(FLD),TBUFF1,
     O                            OBUFF1(BPOS))
                      IF (FLDCOL(FLD) .GE. 2) THEN
C                       right-justify column header
                        J= MIN (FLDLEN(NFLD),CWID)
                        CALL RHTSTR (FLDLEN(FLD),
     M                               OBUFF1(BPOS))
                      END IF
                    END IF
                  ELSE
C                   value field
C
                    VALFG= 1
                    VAL= -999.0
                    IF (TIM .EQ. 0) THEN
C                     time step summary requested
                      IF (SRC .EQ. 0) THEN
C                       summary for sum of sources
                        VAL= SUMTS(CON)
                      ELSE
C                       summary for single source
                        IF (IFP(CON,SRC) .GE. 1) THEN
C                         a timeseries was input for this variable
                          PTR= REPPTR (NCON,I1,CON,SRC,I1)
                          VAL= SUMTIM(PTR)
                        ELSE
C                         skip - no timeseries
                          VALFG= 0
                        END IF
                      END IF
                    ELSE
C                     single time step
                      IF (SRC .EQ. 0) THEN
C                       sum of sources
                        PTR= REPPTR (NCON,I1,CON,TIM,I1)
                        VAL= SUMSRC(PTR)
                      ELSE
C                       single source
                        IF (IFP(CON,SRC) .GE. 1) THEN
C                         a timeseries was input for this variable
                          PTR= REPPTR (NCON,NSRC,CON,SRC,TIM)
                          VAL= ACCUM(PTR)
                        ELSE
C                         skip - no timeseries
                          VALFG= 0
                        END IF
                      END IF
                    END IF
                    IF (CWID .GE. 1) THEN
C                     formatted buffer
                      IF (VALFG .EQ. 1) THEN
C                       write value
                        IF (ABS(VAL) .GE. 1.0E+08) THEN
C                         force exponential format
                          LDEC= -DECPLA(CON)
                        ELSE
C                         allow general format
                          LDEC= DECPLA(CON)
                        END IF
                        CALL DECCHX (VAL,CWID,SDIG(CON),LDEC,
     O                               OBUFF1(BPOS))
                        CALL RHTSTR (CWID,
     M                               OBUFF1(BPOS))
                      ELSE
C                       null value - blank out
                        CALL ZIPC (CWID,BLANK,
     O                             OBUFF1(BPOS))
                      END IF
                    ELSE
C                     delimited buffer
                      BPOS= LENSTR (I1000,OBUFF1)
                      IF (VALFG .EQ. 1) THEN
C                       write value
                        WRITE (OBUFF(BPOS+1:BPOS+20),2010) VAL
                        CALL LFTSTR (I20,
     O                               OBUFF1(BPOS+1))
                        A= LENSTR (I20,OBUFF1(BPOS+1))+ 1
                      ELSE
C                       null value - only write delimiter
                        A= 1
                      END IF
                      BUFLEN= BPOS+ A
                      OBUFF1(BUFLEN)= DELIM
                    END IF
                  END IF
                  BPOS= BPOS+ FLDLEN(FLD)
                IF ( (LOOPFG .GE. 1) .AND.
     $               (LEVCT(3) .LT. FLDCOL(FLD)) ) GO TO 90
 110          CONTINUE
              IF (WRTFLG .EQ. 1) THEN
C               write current row to report
                IF ( (BUFLEN .GE. 2) .AND. (CWID .LE. -1) ) THEN
C                 skip final delimiter
                  BUFLEN= BUFLEN- 1
                END IF
                WRITE (REPTFL,2020) (OBUFF1(I),I= 1, BUFLEN)
              END IF
C
C           end of loop on rows
            IF ( (LOOPFG .GE. 1) .AND. (LEVCT(2) .LT. MAXLP(2)) )
     $            GO TO 30
C
C         end of loop on format lines
          IF (CONT .EQ. 1) GO TO 20
C
C         here on end of file
 120      CONTINUE
C
C         end of loop on tables
        IF ( (LOOPFG .GE. 1) .AND. (LEVCT(1) .LT. MAXLP(1)) ) GO TO 10
C
      END IF
C 
      CLOSE (REPTFL)
C
      RETURN
      END
C
C
C
      SUBROUTINE   RPINDX
     I                    (REPTNO,MESSU,MSGFL,SCLU,BUFF,LEVCT,LOOPFG,
     I                     MIN,MAX,
     M                     RPECNT,MAXLP,
     O                     ERRFLG,INDEX)
C
C     + + + PURPOSE + + +
C     Returns index based on text, loop status, and allowable range
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     REPTNO,MESSU,MSGFL,SCLU,LEVCT(3),LOOPFG,MIN,MAX,
     $            RPECNT(3),MAXLP(3),ERRFLG,INDEX
      CHARACTER*2 BUFF
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     SGRP
      CHARACTER*2 STARS
C
C     + + + EXTERNALS + + +
      EXTERNAL    OMSTI,OMSTC,OMSG
C
C     + + + DATA INITIALIZATIONS + + +
      DATA STARS/'**'/
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (I2)
C
C     + + + END SPECIFICATIONS + + +
C
      ERRFLG= 0
C
      READ (BUFF,1000,ERR=10) INDEX
        GO TO 20
 10   CONTINUE
C       not a valid number
        IF (BUFF .EQ. STARS) THEN
C         check if implied loop is legal
          IF (LOOPFG .GE. 1) THEN
C           there is a legal loop for this index
            INDEX= LEVCT(LOOPFG)
            MAXLP(LOOPFG)= MAX
          ELSE
C           error - no legal loop declared
            SGRP= 21
            CALL OMSTI (REPTNO)
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 RPECNT(1))
C      write (*,*) 'error - no legal loop declared'
            ERRFLG= 1
            INDEX= 1
          END IF
        ELSE
C         error - invalid numeric input
          SGRP= 21
          CALL OMSTI (REPTNO)
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               RPECNT(1))
C      write (*,*) 'invalid numeric input'
          ERRFLG= 1
          INDEX= 1
        END IF
 20   CONTINUE
C
      IF (ERRFLG .EQ. 0) THEN
C       check index against range
        IF ( (INDEX .LT. MIN) .OR. (INDEX .GT. MAX) ) THEN
C         error - index out of bounds
          SGRP= 22
          CALL OMSTI (REPTNO)
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               RPECNT(2))
C      write (*,*) 'index out of bounds'
          ERRFLG= 1
          INDEX= 1
        END IF
      END IF
C
      RETURN
      END
C
C
C
      INTEGER FUNCTION   REPPTR
     I                          (NCON,NSRC,CON,SRC,TIM)
C
C     + + + PURPOSE + + +
C     Compute 1-dimensional pointer from 3 virtual subscripts
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     NCON,NSRC,CON,SRC,TIM
C
C     + + + END SPECIFICATIONS + + +
C
      REPPTR= (TIM- 1)*NCON*NSRC+ (SRC- 1)*NCON+ CON
C
      RETURN
      END
