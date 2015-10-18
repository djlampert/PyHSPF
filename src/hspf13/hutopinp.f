C
C
C
      SUBROUTINE   PCOPY
     I                   (NDELT,SDATIM,NDAMON,EMFG,MAXOSV,
     M                    OSVKEY)
C
C     + + + PURPOSE + + +
C     Process the input for the copy module
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    NDELT,SDATIM(5),NDAMON(12),EMFG,MAXOSV,OSVKEY
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NDELT  - simulation time interval in minutes
C     SDATIM - starting date/time
C     NDAMON - no. of days in each month of calendar year
C     EMFG   - english/metric units flag (english-1,metric-2)
C     MAXOSV - maximum size of osv
C     OSVKEY - last osv file record written
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION COPY1 + + +
      INCLUDE   'ccopy.inc'
      INCLUDE   'crin2.inc'
      INCLUDE   'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,I1,I2,OSVKND,OSVKST,OSVREC
C
C     + + + FUNCTIONS + + +
      INTEGER    DAYMNH
C
C     + + + EXTERNALS + + +
      EXTERNAL   ITABLE,PUTOSV,DAYMNH
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' ',132('+'),/,' ','PROCESSING COPY OPERATION NO. ',I4)
 2010 FORMAT (/,' FINISHED PROCESSING COPY OPERATION NO. ',I4,
     $        /,' ',132('+'))
C
C     + + + END SPECIFICATIONS + + +
C
      I1   = 1
      I2   = 2
      MESSU= FILE(1)
C
      IF (OUTLEV.GT.0) THEN
        WRITE (MESSU,2000) OPTNO
      END IF
C
      IF (RESMFG.EQ.1) THEN
C       read the general part of the osv from osvfl
C       - not implemented in this release of hspf
      ELSE
C       initialize the entire osv
        DO 10 I= 1,MAXOSV
          IPAD(I)= -999
 10     CONTINUE
C
C       place information into osv
        COPYNO= OPTNO
        MESSU=  FILE(1)
        DELT=   NDELT
        DO 20 I= 1, 12
          NDAY(I)= NDAMON(I)
 20     CONTINUE
        DO 30 I= 1, 5
          DATIM(I)= SDATIM(I)
 30     CONTINUE
C       following value is assigned so that subroutine addtim will
C       work ok
        NDAYS= DAYMNH (YR,MON,NDAY)
        SPIVL=  0
        SPOPNO= OPNO
        UUNITS= EMFG
C
C       initialize current values
        DO 40 I= 1, 20
          PTVAL(I)= -1.0E30
          MNVAL(I)= -1.0E30
 40     CONTINUE
      END IF
C
C     get no. of time series to be copied -- table-type timeseries
      CALL ITABLE (I1,I1,I2,I1,
     M             NUMBR)
C
      OSVREC= 1
C
C     write the osv to disc and record the keys in opntab
      OSVKST= OSVKEY+ 1
      OSVKND= OSVKEY+ OSVREC
      CALL PUTOSV
     I            (OSVKST,OSVKND,MAXOSV,IPAD)
      OPNTAB(7,OPNO)= OSVKST
      OPNTAB(8,OPNO)= OSVKND
      OSVKEY        = OSVKND
C
      IF (OUTLEV.GT.0) THEN
        WRITE (MESSU,2010)  OPTNO
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PDISPL
     I                    (NDELT,SDATIM,NDAMON,EMFG,MAXOSV,
     M                     OSVKEY)
C
C     + + + PURPOSE + + +
C     Process the input for the disply module
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   NDELT,SDATIM(5),NDAMON(12),EMFG,MAXOSV,OSVKEY
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NDELT  - simulation time interval in minutes
C     SDATIM - starting date/time
C     NDAMON - no. of days in each month of calendar year
C     EMFG   - english/metric units flag (english-1,metric-2)
C     MAXOSV - maximum size of osv
C     OSVKEY - last osv file record written
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION DISPLY1 + + +
      INCLUDE   'cdisp.inc'
      INCLUDE   'crin2.inc'
      INCLUDE   'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,J,K,I1,I2,I4,I6,I15,I1440,MONTHT(2,12),
     $             COL,Y,MO,ND,ND28,
     $             FORS(3),FORF(2),FORA(2),FORE,FORT(3),
     $             NB4,ND4,OSVKND,
     $             OSVKST,OSVREC,SUBB,FMT11(16),SCLU,SGRP,
     $             INITFG,CONT,CLEN
      INTEGER      BLNK1,ZEROC
      CHARACTER*4  KWDLIB(6)
      CHARACTER*80 CHSTR
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (CHSTR,CHSTR1),(KWDLIB,KWDLB1)
      CHARACTER*1  CHSTR1(80),KWDLB1(24)
C
C     + + + FUNCTION + + +
      INTEGER      DAYMNH,CHKSTR
C
C     + + + INTRINSICS + + +
      INTRINSIC   MOD
C
C     + + + EXTERNALS + + +
      EXTERNAL     ITABLE,HSCKFL,OMSG,OMSTC,OMSTI,RTABLE,CHKSTR,PUTOSV,
     $             SETVEC,WMSGTT,DAYMNH
C
C     + + + INPUT FORMATS + + +
 1020 FORMAT (6(A4,1X),2A1)
 1030 FORMAT (16A4)
 1040 FORMAT (12A4)
 1050 FORMAT (9A4)
 1060 FORMAT (3A4)
 1070 FORMAT (3(A4,A2))
 1080 FORMAT (A4)
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' ',132('+'),
     $        /,' PROCESSING DISPLY OPERATION NO.',I5,
     $            '  TIME INTERVAL',I5,' MINS')
 2030 FORMAT (/,' FINISHED PROCESSING DISPLAY OPERATION NO.',I5,
     $        /,' ',132('+'))
 2070 FORMAT (A4)
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU = 371
C
      IF (RESMFG.EQ.1) THEN
C       read the osv from osvfl - not implemented in this release
C        of hspf
      ELSE
C       initialize the entire osv area
        DO 10 I=1,MAXOSV
          IPAD(I)= -999
 10     CONTINUE
      END IF
C
      MESSU= FILE(1)
      MSGFL= FILE(15)
      I1   = 1
      I2   = 2
      I4   = 4
      I6   = 6
      I15  = 15
      I1440= 1440
C
      IF (OUTLEV.GT.0) THEN
        WRITE (MESSU,2000) OPTNO,NDELT
      END IF
C
C     get keyword and other strings
      SGRP  = 1
      INITFG= 1
      CLEN  = 80
      CALL WMSGTT (MSGFL,SCLU,SGRP,INITFG,
     M             CLEN,
     O             CHSTR1,CONT)
C
      READ (CHSTR,1020) KWDLIB,BLNK1,ZEROC
C
C     incorporate information obtained from global and opn seq blocks
      DISPNO= OPTNO
      DELT  = NDELT
      DO 20 I= 1,5
        DATIM(I)= SDATIM(I)
 20   CONTINUE
      DO 30 I= 1,12
        NDAY(I)= NDAMON(I)
 30   CONTINUE
C
      PIVLNO= 0
      UUNITS= EMFG
C
C     process table-type disply-info1
      CALL ITABLE
     I             (I1,I1,I15,I1,
     M              INFO1)
C
      IF (DIGIT1.EQ.BLNK1) THEN
        DIGIT1= ZEROC
      END IF
C
      IF (DIGIT2.EQ.BLNK1) THEN
        DIGIT2= ZEROC
      END IF
C
C     check output files - if not open, then open with standard name
      IF (FILE1 .GT. 0) THEN
        CALL HSCKFL
     I              (FILE1)
      END IF
C
      IF (FILE2 .GT. 0) THEN
        CALL HSCKFL
     I              (FILE2)
      END IF
C
C     check for valid keywords
      WRITE (CHSTR(1:4),2070) TRAN(1)
      TRNCOD= CHKSTR (I4,I6,CHSTR1,KWDLB1)
C
      IF (TRNCOD .EQ. 0) THEN
C       invalid keyword
        WRITE (CHSTR(1:4),2070) TRAN(1)
        CALL OMSTC (I4,CHSTR1)
        SGRP= 2
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             ECOUNT)
      ELSE
C       check for default
        IF (TRNCOD .EQ. 6) THEN
          TRNCOD= 1
          READ (KWDLIB(1),1080) TRAN(1)
        END IF
      END IF
C
C     determine the initial value to go into arrays/accumulators
C     casentry trncod
      GO TO (80,80,90,100,80),TRNCOD
C
 80   CONTINUE
C       case 1, 2, and 5       sum, aver, and last
        STVAL= 0.0
        GO TO 110
 90   CONTINUE
C       case 3                 max
        STVAL= -1.0E30
        GO TO 110
100   CONTINUE
C       case 4                 min
        STVAL= +1.0E30
        GO TO 110
C       endcase
110   CONTINUE
C
C     read formatting data from msgfl
      SGRP  = 3
      INITFG= 1
      CLEN  = 80
      CALL WMSGTT (MSGFL,SCLU,SGRP,INITFG,
     M             CLEN,
     O             CHSTR1,CONT)
      READ (CHSTR,1030) FMT1
C
      INITFG= 0
      CLEN  = 80
      CALL WMSGTT (MSGFL,SCLU,SGRP,INITFG,
     M             CLEN,
     O             CHSTR1,CONT)
      READ (CHSTR,1030) FMT11
C
      CLEN= 80
      CALL WMSGTT (MSGFL,SCLU,SGRP,INITFG,
     M             CLEN,
     O             CHSTR1,CONT)
      READ (CHSTR,1040) ((MONTHT(I,J),I=1,2),J=1,6)
C
      CLEN= 80
      CALL WMSGTT (MSGFL,SCLU,SGRP,INITFG,
     M             CLEN,
     O             CHSTR1,CONT)
      READ (CHSTR,1040) ((MONTHT(I,J),I=1,2),J=7,12)
C
      CLEN= 80
      CALL WMSGTT (MSGFL,SCLU,SGRP,INITFG,
     M             CLEN,
     O             CHSTR1,CONT)
      READ (CHSTR,1050) FORS,FORF,FORA,FORE,BLANKR
C
      CLEN= 80
      CALL WMSGTT (MSGFL,SCLU,SGRP,INITFG,
     M             CLEN,
     O             CHSTR1,CONT)
      READ (CHSTR,1060) FORT
C
      CLEN= 80
      CALL WMSGTT (MSGFL,SCLU,SGRP,INITFG,
     M             CLEN,
     O             CHSTR1,CONT)
      READ (CHSTR,1070) HRSPAN,DASPAN,MNSPAN
C
      IF (PIVL.GT.0) THEN
C       short-span summary is to be printed
        PDELT= PIVL* NDELT
        IF (MOD(I1440,PDELT).NE.0) THEN
C         error - data interval, as implied by pivl, must be an
C         integer fraction of a day
          CALL OMSTI (OPTNO)
          CALL OMSTI (PDELT)
          SGRP= 4
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
        END IF
C
C       determine the particulars of this display
        IF (PDELT.LT.60) THEN
C         use alternate format, so hours go from 0: through 23:
          DO 125 I=1,16
            FMT1(I)=FMT11(I)
 125      CONTINUE
          PSHFG= 1
          NIVL = 60/PDELT
        ELSE
          PSHFG= 2
          NIVL = 1440/PDELT
        END IF
C
C       put the user's specification of the number of decimal digits
C       into these format building blocks
C
        FMT1(5) = DIGIT1
        FMT1(9) = DIGIT1
        FMT1(15)= DIGIT1
C
C       initialize the data array
        DO 160 I=1,31
          DO 150 J=1,60
            DATA(J,I)= STVAL
 150      CONTINUE
 160    CONTINUE
      ELSE
C       no short span display
        PSHFG= 0
      END IF
C
      IF (PYRFG.EQ.1) THEN
C       annual summary is required - initialize data array and
C       build printout formats
        FORF(2)= DIGIT2
        DO 190 I=1,3
          FMT2(I)= FORS(I)
          FMT3(I)= FORS(I)
          FMT4(I)= FORS(I)
          FMT5(I)= FORT(I)
 190    CONTINUE
C
        DO 310 COL= 1,12
          K= COL+ PYREND
          IF (K.LE.12) THEN
            MO= K
          ELSE
            MO= K- 12
          END IF
C         subscript base for format array
          SUBB= (2*COL)+ 2
C         build heading
          DO 220 I=1,2
            MONTHS(I,COL)= MONTHT(I,MO)
 220      CONTINUE
C         build formats
          DO 230 I=1,2
            FMT2(SUBB+I-1)= FORF(I)
            FMT5(SUBB+I-1)= FORF(I)
 230      CONTINUE
C         find no. of days in a leap year
          Y = 1976
          ND= DAYMNH(Y,MO,NDAMON)
C
C         casentry (nd-28)
          ND28= ND-28
          IF (ND28 .EQ. 1) THEN
C           feb
            DO 250 I=1,2
              FMT3(SUBB+I-1)= FORA(I)
              FMT4(SUBB+I-1)= FORA(I)
 250        CONTINUE
          ELSE IF (ND28 .EQ. 2) THEN
C           apr,jun,sep,nov
            DO 270 I=1,2
              FMT3(SUBB+I-1)= FORF(I)
              FMT4(SUBB+I-1)= FORA(I)
 270        CONTINUE
          ELSE IF (ND28 .EQ. 3) THEN
C           jan,mar,may,jul,aug,oct,dec
            DO 290 I=1,2
              FMT3(SUBB+I-1)= FORF(I)
              FMT4(SUBB+I-1)= FORF(I)
 290        CONTINUE
          END IF
C
C         initialize data array
          ND4= ND
          CALL SETVEC
     I                 (ND4,STVAL,
     O                  DYVAL(1,COL))
C         blanks
          NB4= 31- ND4
          IF (NB4.GT.0) THEN
            CALL SETVEC
     I                   (NB4,BLANKR,
     O                    DYVAL((ND+1),COL))
          END IF
 310    CONTINUE
C
C       finish the formats
        FMT2(28)= FORE
        FMT3(28)= FORE
        FMT4(28)= FORE
        FMT5(28)= FORE
C       find no. of intervals in a day
        IVLDAY  = 1440/NDELT
      END IF
C
C     process table-type disply-info2
      CALL RTABLE
     I             (I2,I1,I4,I1,
     M              INFO2)
C
C     no. of records (1000 2-byte words long) for osv
      OSVREC= 5
C     write the osv to disc and record the keys in opntab
      OSVKST= OSVKEY+ 1
      OSVKND= OSVKEY+ OSVREC
      CALL PUTOSV
     I            (OSVKST,OSVKND,MAXOSV,IPAD)
      OPNTAB(7,OPNO)= OSVKST
      OPNTAB(8,OPNO)= OSVKND
      OSVKEY        = OSVKND
C
      IF (OUTLEV.GT.0) THEN
        WRITE (MESSU,2030) OPTNO
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PGENER
     I                    (NDELT,SDATIM,NDAMON,EMFG,MAXOSV,
     M                     OSVKEY)
C
C     + + + PURPOSE + + +
C     Process the input for the gener module
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   NDELT,SDATIM(5),NDAMON(12),EMFG,MAXOSV,OSVKEY
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NDELT  - simulation time interval in minutes
C     SDATIM - starting date/time
C     NDAMON - no. of days in each month of calendar year
C     EMFG   - english/metric units flag (english-1,metric-2)
C     MAXOSV - maximum size of osv
C     OSVKEY - last osv file record written
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION GENER1 + + +
      INCLUDE   'cgene.inc'
      INCLUDE   'crin2.inc'
      INCLUDE   'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,I1,I2,I3,I4,IVAL(1),OSVKND,OSVKST,OSVREC
C
C     + + + FUNCTIONS + + +
      INTEGER   DAYMNH
C
C     + + + EXTERNALS + + +
      EXTERNAL  ITABLE,RTABLE,PUTOSV,DAYMNH
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' ',132('+'),
     $        /,' PROCESSING INPUT FOR TRANSGENERATION OPERATION NO.',
     $          I5,'  TIME INTERVAL=',I5,' MINS')
 2010 FORMAT (/,' FINISHED PROCESSING TRANSGENERATION OPERATION NO.',I5,
     $        /,' ',132('+'))
C
C     + + + END SPECIFICATIONS + + +
C
C     initialize the entire osv
      DO 10 I=1,MAXOSV
        IPAD(I)= -999
 10   CONTINUE
C
      I1   = 1
      I2   = 2
      I3   = 3
      I4   = 4
      MESSU= FILE(1)
C
      IF (OUTLEV.GT.0) THEN
        WRITE (MESSU,2000) OPTNO, NDELT
      END IF
C
C     place information into osv
      GENRNO= OPTNO
      MESSU= FILE(1)
      MSGFL= FILE(15)
      DELT= NDELT
      DO 13 I= 1, 12
        NDAY(I)= NDAMON(I)
 13   CONTINUE
      DO 16 I= 1, 5
        DATIM(I)= SDATIM(I)
 16   CONTINUE
C     following value is assigned so that subroutine addtim will
C     work ok
      NDAYS= DAYMNH (YR,MON,NDAY)
      SPIVL=  0
      SPOPNO= OPNO
      UUNITS= EMFG
C
      SUM= 0.0
C     get the operation code - table-type opcode
      CALL ITABLE
     I             (I1,I1,I1,I1,
     M              IVAL)
      OPCODE= IVAL(1)
      IF (OPCODE.EQ.8) THEN
C       operation is power series - get number of terms
        CALL ITABLE
     I               (I2,I1,I1,I1,
     M                IVAL)
        NTERMS= IVAL(1)
C       get coefficients
        CALL RTABLE
     I               (I3,I1,NTERMS,I1,
     M                K)
      END IF
      IF ( (OPCODE .GE. 9) .AND. (OPCODE .LE. 11) ) THEN
C       constant needed
        I= 1
      ELSE IF ( (OPCODE .GE. 24) .AND. (OPCODE .LE. 26) ) THEN
C       constant needed
        I= 1
      ELSE
C       no constant needed
        I= 0
      END IF
C
      IF (I .EQ. 1) THEN
C       get constant
        CALL RTABLE
     I               (I4,I1,I1,I1,
     M                K)
      END IF
C
      OSVREC= 1
C     write the osv to disc and record the keys in opntab
      OSVKST= OSVKEY+ 1
      OSVKND= OSVKEY+ OSVREC
      CALL PUTOSV
     I            (OSVKST,OSVKND,MAXOSV,IPAD)
      OPNTAB(7,OPNO)= OSVKST
      OPNTAB(8,OPNO)= OSVKND
      OSVKEY        = OSVKND
C
      IF (OUTLEV.GT.0) THEN
        WRITE (MESSU,2010) OPTNO
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PMUTSN
     I                    (NDELT,SDATIM,NDAMON,EMFG,MAXOSV,
     M                     OSVKEY)
C
C     + + + PURPOSE + + +
C     process the input for the mutsin module
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   NDELT,SDATIM(5),NDAMON(12),EMFG,MAXOSV,OSVKEY
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NDELT  - simulation time interval in minutes
C     SDATIM - starting date/time
C     NDAMON - no. of days in each month of calendar year
C     EMFG   - english/metric units flag (english-1,metric-2)
C     MAXOSV - maximum size of osv
C     OSVKEY - last osv file record written
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION MUTSIN1 + + +
      INCLUDE   'cmuts.inc'
      INCLUDE   'crin2.inc'
      INCLUDE   'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   OSVKST,OSVKND,OSVREC,I,I1,I4,SCLU,SGRP
C
C     + + + FUNCTIONS + + +
      INTEGER   DAYMNH
C
C     + + + EXTERNALS + + +
      EXTERNAL  DAYMNH,ITABLE,PUTOSV,HSCKFL,OMSG,OMSTI
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' ',132('+'),
     $        /,' ','PROCESSING MUTSIN OPERATION NO. ',I4)
 2030 FORMAT (/,' FINISHED PROCESSING MUTSIN OPERATION NO. ',I4,
     $        /,' ',132('+'))
C
C     + + + END SPECIFICATIONS + + +
C
      I1   = 1
      SCLU = 371
      MESSU= FILE(1)
C
      IF (OUTLEV.GT.0) THEN
        WRITE (MESSU,2000) OPTNO
      END IF
C
      IF (RESMFG.EQ.1) THEN
C       read the osv from osvfl
C       - not implemented in this release of hspf
      ELSE
C       initialize the entire osv area
        DO 30 I= 1,MAXOSV
          IPAD(I)= -999
 30     CONTINUE
      END IF
C
      MUTNO = OPTNO
      MESSU = FILE(1)
      MSGFL = FILE(15)
C
      DELT= NDELT
      DO 10 I= 1,5
        DATIM(I)= SDATIM(I)
 10   CONTINUE
      DO 15 I= 1,12
        NDAY(I)= NDAMON(I)
 15   CONTINUE
C
      STFG  = 1
      UUNITS= EMFG
      OSVREC= 1
C     following values are assigned so that subroutine addtim will
C     work ok
      PIVL  = 1
      PIVLNO= 0
      NDAYS = DAYMNH(YR,MON,NDAY)
C
C     get info for operation -- table-type mutsinfo
      I4= 5
      CALL ITABLE
     I             (I1,I1,I4,I1,
     M              MUTINF)
C
C     check output file - if not open,
C     then open it with a standard name
      IF (MUTFL .GT. 0) THEN
        CALL HSCKFL
     I              (MUTFL)
      END IF
C
      NCURV= NPT+ NMN
      IF (NCURV.LT.1.OR.NCURV.GT.20) THEN
C       total no. of curves to be plotted is not within valid range
        CALL OMSTI (NCURV)
        SGRP= 21
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             ECOUNT)
      END IF
C
      IF (MISSFG.EQ.2) THEN
        FILVAL= -1.0E30
      ELSE
        FILVAL= 0.0
      END IF
C
C     write the osv to disc and record the keys in opntab
      OSVKST= OSVKEY+ 1
      OSVKND= OSVKEY+ OSVREC
      CALL PUTOSV
     I            (OSVKST,OSVKND,MAXOSV,IPAD)
      OPNTAB(7,OPNO)= OSVKST
      OPNTAB(8,OPNO)= OSVKND
      OSVKEY        = OSVKND
C
      IF (OUTLEV.GT.0) THEN
        WRITE (MESSU,2030) OPTNO
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PPLTGN
     I                    (NDELT,SDATIM,EDATIM,NDAMON,EMFG,MAXOSV,
     M                     OSVKEY)
C
C     + + + PURPOSE + + +
C     process the input for the pltgen module
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
C     + + + COMMON BLOCKS- SCRTCH, VERSION PLTGEN1 + + +
      INCLUDE   'cpltg.inc'
      INCLUDE   'crin2.inc'
      INCLUDE   'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      OSVKST,OSVKND,OSVREC,I,I1,I2,I4,I6,N,
     #             TRNCOD,PLVX,SCLU,SGRP,INITFG,CONT,CLEN
      CHARACTER*4  KWDLIB(6),BLANK
      CHARACTER*80 CHSTR
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (CHSTR,CHSTR1),(KWDLIB,KWDLB1)
      CHARACTER*1  CHSTR1(80),KWDLB1(24)
C
C     + + + FUNCTIONS + + +
      INTEGER   DAYMNH,CHKSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL  DAYMNH,ITABLE,HSCKFL,RTABLE,CHKSTR,PUTOSV
      EXTERNAL  OMSG,OMSTI,OMSTC,WMSGTT
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (6(A4,1X))
 1010 FORMAT (A4)
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' ',132('+'),
     $        /,' PROCESSING PLTGEN OPERATION NO. ',I4)
 2030 FORMAT (/,' FINISHED PROCESSING PLTGEN OPERATION NO. ',I4,
     $        /,' ',132('+'))
 2070 FORMAT (A4)
C
C     + + + END SPECIFICATIONS + + +
C
      I1   = 1
      BLANK= '    '
      SCLU = 371
      MESSU= FILE(1)
C
      IF (OUTLEV.GT.0) THEN
        WRITE (MESSU,2000) OPTNO
      END IF
C
      IF (RESMFG.EQ.1) THEN
C       read the osv from osvfl
C       - not implemented in this release of hspf
      ELSE
C       initialize the entire osv area used
        DO 10 I= 1,MAXOSV
          IPAD(I)= -999
 10     CONTINUE
      END IF
C
      PLTNO = OPTNO
      MESSU = FILE(1)
      MSGFL = FILE(15)
C
      DELT= NDELT
      DO 20 I= 1,5
        DATIM(I)= SDATIM(I)
 20   CONTINUE
      DO 30 I= 1,12
        NDAY(I)= NDAMON(I)
 30   CONTINUE
C
      STFG  = 1
      SPIVL=  0
      SPOPNO= OPNO
      UUNITS= EMFG
      OSVREC= 3
      PLTFLG= 1
C     following values are assigned so that subroutine addtim will
C     work ok
      PIVL  = 1
      PIVLNO= 0
      NDAYS = DAYMNH(YR,MON,NDAY)
C
C     get info on time series to be plotted -- table-type plotinfo
      I4= 7
      CALL ITABLE
     I             (I1,I1,I4,I1,
     M              PLTINF)
C
C     check output file - if not open, then open it with a standard name
      IF (PLOTFL .GT. 0) THEN
C       check that file is open
C
        IF (TYPEFG .EQ. 1) THEN
C         indicate that this is a pltgen file for vax special case
          PLVX= PLOTFL+ 9999
        ELSE 
C         indicate that this is a dtsf or ptsf file for feq
          PLVX= PLOTFL- 9999
        END IF
        CALL HSCKFL
     I              (PLVX)
      END IF
C
C     check that total no. of curves to be plotted is within valid range
C     we need pivl for write interval,
C     but it is read in table plotinfo for user case
      PIVL= NCURV
      IF (PIVL .EQ. 0) THEN
C       zero input - default to one
        PIVL= 1
      END IF
      NCURV= NPT+ NMN
C
      IF (TYPEFG .EQ. 1) THEN
C       original format
        IF ( (NCURV .LT. 1) .OR. (NCURV .GT. 20) ) THEN
C         error - bad total number of curves
          CALL OMSTI (NCURV)
          SGRP= 31
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
        END IF
      ELSE IF (TYPEFG .EQ. 2 .OR. TYPEFG .EQ. 4) THEN
C       dtsf format
        IF (NPT .GT. 0) THEN
C         error - dtsf is mean-valued only
          CALL OMSTI (NPT)
          SGRP= 34
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
        END IF
      ELSE IF (TYPEFG .EQ. 3) THEN
C       ptsf format
        IF (NMN .GT. 0) THEN
C         error - ptsf is point valued only
          CALL OMSTI (NMN)
          SGRP= 35
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
        END IF
        IF (NPT .GT. 1) THEN
C         error - ptsf is single valued only
          CALL OMSTI (NPT)
          SGRP= 36
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
        END IF
      END IF
C
C     read transformation keyword library
      SGRP  = 32
      INITFG= 1
      CLEN  = 80
      CALL WMSGTT (MSGFL,SCLU,SGRP,INITFG,
     M             CLEN,
     O             CHSTR1,CONT)
C
      READ (CHSTR,1000) KWDLIB
C
      IF (LABLFG .NE.(-1)) THEN
C       plot label is required
C       get general labels for plot -- table-type gen-labels
        I2= 2
        I4= 15
        CALL ITABLE
     I              (I2,I1,I4,I1,
     M               GLABL)
      ELSE
C       general label fields are blank
        DO 40 I= 1,15
          READ (BLANK,2070) GLABL(I)
 40     CONTINUE
      END IF
C
C     get scale info -- table-type scaling
      I2= 3
      I4= 4
      CALL RTABLE
     I             (I2,I1,I4,I1,
     M              SCALE)
C
C     get info for each individual curve on plot -- table-type curv-data
      DO 50 N= 1, NCURV
        I2=4
        I4=8
        CALL ITABLE
     I               (I2,N,I4,I1,
     M                CUVDAT(1,N))
C       check transformation keyword
        WRITE (CHSTR(1:4),2070) CUVDAT(8,N)
        I4= 4
        I6= 6
        TRNCOD= CHKSTR (I4,I6,CHSTR1,KWDLB1)
        IF (TRNCOD.EQ.0) THEN
C         invalid keyword
          WRITE (CHSTR,2070) CUVDAT(8,N)
          I4= 4
          CALL OMSTC (I4,CHSTR1)
          SGRP= 33
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
        ELSE
C         check for default
          IF (TRNCOD.EQ.6) THEN
            TRNCOD     = 1
            READ (KWDLIB(1),1010) CUVDAT(8,N)
          END IF
          CUVDAT(9,N)= TRNCOD
        END IF
 50   CONTINUE
C
      IF (TYPEFG .EQ. 2 .OR. TYPEFG .EQ. 4) THEN
C       write header for feq dtsf format
        CALL HEDTSF (PLOTFL,NDELT,NDAMON,GLABL,NMN,CUVDAT,
     I               SDATIM,EDATIM,TYPEFG,
     O               CURREC)
      END IF
C
C     write the osv to disc and record the keys in opntab
      OSVKST= OSVKEY+ 1
      OSVKND= OSVKEY+ OSVREC
      CALL PUTOSV
     I            (OSVKST,OSVKND,MAXOSV,IPAD)
      OPNTAB(7,OPNO)= OSVKST
      OPNTAB(8,OPNO)= OSVKND
      OSVKEY        = OSVKND
C
      IF (OUTLEV.GT.0) THEN
        WRITE (MESSU,2030) OPTNO
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   HEDTSF
     I                    (PLOTFL,NDELT,NDAMON,GLABL,NMN,CUVDAT,
     I                     SDATIM,EDATIM,TYPEFG,
     O                     CURREC)
C
C     + + + PURPOSE + + +
C     Write header for FEQ DTSF file (TYPEFG=2 or 4).
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER PLOTFL,NDELT,NDAMON(12),GLABL(15),NMN,CUVDAT(9,NMN),
     $        SDATIM(5),EDATIM(5),TYPEFG,CURREC
C
C     + + + ARGUMENT DEFINITIONS + + +
C     PLOTFL - fortran unit number of pltgen output file
C     NDELT  - number of minutes of run interval
C     NDAMON - no. of days in each month of calendar year
C     GLABL  - character label for entire file
C     NMN    - number of (mean-valued) time series
C     CUVDAT - character label for each time series
C     SDATIM - starting date/time
C     EDATIM - ending date/time
C     TYPEFG - orginal (TYPEFG=2) or revised (TYPEFG=4)
C     CURREC - current record number on DTSF file (TYPEFG=4)
C
C     + + + LOCAL VARIABLES + + +
      INTEGER I,N,I0,I1,I5,I12,LEN,LLEN,IREC(106),YR,MO,DY,HR,MI,SC,
     $        NHEAD,CTIME,NULSDT(5),NULEDT(5),NULLDT(5),DUM1,DUM2,DUM3,
     $        DUM4,DUM5,DUM6,DUM7,DUM8,NSTEP,XDAT(6)
      DOUBLE PRECISION SJTIME,EJTIME,DYFRAC,NJTIME
      CHARACTER*64 LFILE
      LOGICAL      LOPEN
C
C     + + + EXTERNALS + + +
      EXTERNAL ZIPI,SYDATM,EXDATE,TIMCVT
C
C     + + + DATA INITIALIZATIONS + + +
      DATA I0,I1,I5,I12/0,1,5,12/
      DATA NULSDT,NULEDT/1859,1,1,0,0,1859,1,3,0,0/
C
C     + + + END SPECIFICATIONS + + +
C
C     initialize record
      LEN= NMN+ 7
      CALL ZIPI (LEN,I0,
     O           IREC)
C
      IF (TYPEFG.EQ.4) THEN
C       revised format, use direct acess file
        INQUIRE (PLOTFL,OPENED=LOPEN,NAME=LFILE)
C       write(99,*) "PLTGEN: unit # ",PLOTFL," is named ",LFILE
        IF (LOPEN .EQV. .TRUE.) THEN
C         close file before opening
          CLOSE (PLOTFL)
        END IF
C       determine record length and write it on 1st record
        LLEN= 4*LEN
C       write(99,*) "PLTGEN: About to open ",LFILE,
C     $             " with length ",LLEN
        OPEN (PLOTFL,FILE=LFILE,STATUS="REPLACE",
     $        ACCESS="DIRECT",RECL=LLEN)
        WRITE (PLOTFL,REC=1) LLEN,(IREC(I),I=2,LEN)
C       write(99,*) "PLTGEN: wrote LLEN and Dummy 0s, like this:"
C       write(99,*) "PLTGEN: ",LLEN,(IREC(I),I=1,4)
      END IF
C
C     write first record
      CALL SYDATM
     O            (YR,MO,DY,HR,MI,SC)
      IF (YR .LT. 90) THEN
C       assume file created after 1999
        YR= YR+ 2000
      ELSE
C       assume file created before 2000
        YR= YR+ 1900
      END IF
      CTIME= HR*100+ MI
      NHEAD= NMN+ 3
      IF (TYPEFG.EQ.2) THEN
        WRITE (PLOTFL) YR,MO,DY,CTIME,NHEAD,NMN,I1,(IREC(I),I=8,LEN)
      ELSE
        WRITE (PLOTFL,REC=2) YR,MO,DY,CTIME,NHEAD,NMN,I1,
     $                      (IREC(I),I=8,LEN)
      END IF
C
C     write second record
      CALL EXDATE (SDATIM,
     O             XDAT)
      XDAT(6)= 0
      CALL TIMCVT
     M            (XDAT)
      IF (TYPEFG.EQ.2) THEN
        WRITE (PLOTFL) (XDAT(I),I=1,3),(EDATIM(I),I= 1,3),
     $                 (IREC(I),I=7,LEN)
      ELSE
        WRITE (PLOTFL,REC=3) (XDAT(I),I=1,3),(EDATIM(I),I= 1,3),
     $                       (IREC(I),I=7,LEN)
      END IF
C
C     write tsf label record
      IF (LEN .LT. 10) THEN
C       only write part of label
        LLEN= LEN
      ELSE
C       write whole label
        LLEN= 10
      END IF
      IF (TYPEFG.EQ.2) THEN
        WRITE (PLOTFL) (GLABL(I),I=1,LLEN),(IREC(I),I=11,LEN)
      ELSE
        WRITE (PLOTFL,REC=4) (GLABL(I),I=1,LLEN),(IREC(I),I=11,LEN)
      END IF
C
C     write dataset header records
      LLEN= 4
      DO 10 N= 1, NMN
        IF (TYPEFG.EQ.2) THEN
          WRITE (PLOTFL) (CUVDAT(I,N),I=1,LLEN),(IREC(I),I=5,LEN)
        ELSE
          WRITE (PLOTFL,REC=4+N) (CUVDAT(I,N),I=1,LLEN),
     $                           (IREC(I),I=5,LEN)
        END IF
 10   CONTINUE
C
C     write null event
C
C     write header line for null event
      CALL MJDATE (NULEDT,
     O             EJTIME,DYFRAC)
      CALL MJDATE (NULSDT,
     O             SJTIME,DYFRAC)
      IF (TYPEFG .EQ. 2) THEN
        WRITE (PLOTFL) -SJTIME,DYFRAC,(NULSDT(I),I=1,3),EJTIME,
     $                 (IREC(I),I=10,LEN)
      ELSE
        CURREC = NMN + 5
        WRITE (PLOTFL,REC=CURREC) -SJTIME,DYFRAC,(NULSDT(I),I=1,3),
     $                            EJTIME,(IREC(I),I=10,LEN)
      END IF
C
C     write two days of zero flows for null event
      NSTEP= 2880/NDELT
      DUM1= 0
      CALL COPYI (I5,NULSDT,
     O            NULLDT)
C     convert to internal format
      CALL HDATIN
     M            (NULLDT(1),NULLDT(2),NULLDT(3),NULLDT(4),NULLDT(5))
      DO 20 N= 1, NSTEP
        CALL ADDTIM (NDELT,NDAMON,I1,I12,
     M               NULLDT,DUM1,
     O               DUM2,DUM3,DUM4,DUM5,DUM6,DUM7,DUM8)
        CALL EXDATE (NULLDT,
     O               XDAT)
        CALL MJDATE (XDAT,
     O               NJTIME,DYFRAC)
        XDAT(6)= 0
        CALL TIMCVT
     M              (XDAT)
        IF (TYPEFG .EQ. 2) THEN
          WRITE (PLOTFL) NJTIME,DYFRAC,(XDAT(I),I=1,3),(IREC(I),I=8,LEN)
        ELSE
          WRITE (PLOTFL,REC=CURREC+N) NJTIME,DYFRAC,(XDAT(I),I=1,3),
     $                                (IREC(I),I=8,LEN)
        END IF
 20   CONTINUE
C
C     write header line for main event
      CALL EXDATE (EDATIM,
     O             XDAT)
      CALL MJDATE (XDAT,
     O             EJTIME,DYFRAC)
      CALL EXDATE (SDATIM,
     O             XDAT)
      CALL MJDATE (XDAT,
     O             SJTIME,DYFRAC)
      XDAT(6)= 0
      CALL TIMCVT
     M            (XDAT)
      IF (TYPEFG .EQ. 2) THEN
        WRITE (PLOTFL) -SJTIME,DYFRAC,(XDAT(I),I=1,3),EJTIME,
     $                 (IREC(I), I= 10,LEN)
      ELSE
        CURREC = CURREC + NSTEP + 1
        WRITE (PLOTFL,REC=CURREC) -SJTIME,DYFRAC,(XDAT(I),I=1,3),EJTIME,
     $                            (IREC(I), I= 10,LEN)
        CURREC = CURREC + 1
      END IF
C
      RETURN
      END
