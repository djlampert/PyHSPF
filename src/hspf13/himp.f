C
C
C
      SUBROUTINE   PIMPLN
     I                   (OUTLEV,MESSUG,MESSFL,RESMFG,NDELT,
     I                    SDATIM,NDAMON,OPNO,EMFG,MAXOPN,MAXOSV,
     M                    OSVKEY,OPNTAB,ECOUNT)
C
C     + + + PURPOSE + + +
C     Process the input for the implnd module
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   OUTLEV,MESSUG,MESSFL,MAXOPN,MAXOSV,
     $          RESMFG,NDELT,SDATIM(5),NDAMON(12),OPNO,
     $          OSVKEY,OPNTAB(20,MAXOPN),ECOUNT,EMFG
C
C     + + + ARGUMENT DEFINITIONS + + +
C     OUTLEV - run interp output level
C     MESSUG - file unit to write messages onto
C     MESSFL - message file unit number
C     RESMFG - resume mode flag (1-ON)
C     NDELT  - simulation time interval in minutes
C     SDATIM - starting date/time
C     NDAMON - no. of days in each month of calendar year
C     OPNO   - where we are in operation table array
C     EMFG   - english/metric units flag (english-1,metric-2)
C     MAXOPN - maximum number of operations
C     MAXOSV - maximum size of osv
C     OSVKEY - key to where we are in OSV file
C     OPNTAB - operation table array
C     ECOUNT - count of run interp errors
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION IGEN1 + + +
      INCLUDE    'cilge.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,LEV,OSVKND,OSVKST,OSVREC
C
C     + + + INTRINSICS + + +
      INTRINSIC MAX
C
C     + + + EXTERNALS + + +
      EXTERNAL  PIGEN,PATEMP,PSNOW,PIWATR,PSOLID,PIWTGS,PIQUAL,IMPRST,
     $          PUTOSV
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' ',132('+'),
     $        /,' PROCESSING IMPERVIOUS LAND-SEGMENT NO:',I4,
     $            '     TIME STEP(DELT):',I5,'  MINS')
 2010 FORMAT (/,' FINISHED PROCESSING IMPERVIOUS LAND-SEGMENT NO. ',I4,
     $        /,' ',132('+'))
 2020 FORMAT (  ' ',132('-'))
C
C     + + + HISTORY + + +
C     06/22/2004  BRB fixed error in IWTGAS when OUTLEV <= 1
C
C     + + + END SPECIFICATIONS + + +
C
      IF (OUTLEV.GT.0) THEN
        WRITE (MESSUG,2000) OPNTAB(3,OPNO), NDELT
      END IF
C
      IF (RESMFG.EQ.0) THEN
C       initialize the entire osv area
        DO 20 I= 1,MAXOSV
          IPAD(I)= -999
 20     CONTINUE
      END IF
C
C     save a local copy of where to store messages
      MESSU = MESSUG
C     id for this implnd
      LSNO  = OPNTAB(3,OPNO)
C     minimum size of osv for this operation
      OSVREC= 1
C
C     process the general input
      CALL PIGEN (MESSFL,MESSU,NDELT,SDATIM,OUTLEV,OPNO,
     $            NDAMON,
     M            ECOUNT)
      IF (OUTLEV.GT.1) THEN
C       section delimeter
        WRITE (MESSU,2020)
      END IF
C     put english/metric units flag into implnd common
      UUNITS = EMFG
C
      IF (RESMFG.EQ.1) THEN
C       read in the rest of the osv from osvfl
C       - not implemented in this release of hspf
      END IF
C
      IF (AIRTFG.EQ.1) THEN
C       input for section atemp
        CALL PATEMP
        IF (OUTLEV.GT.1) THEN
C         section delimeter
          WRITE (MESSU,2020)
        END IF
        OSVREC= MAX(OSVREC,1)
      END IF
C
      IF (SNOWFG.EQ.1) THEN
C       input for snow section
        CALL PSNOW
        IF (OUTLEV.GT.1) THEN
C         section delimeter
          WRITE (MESSU,2020)
        END IF
        OSVREC= MAX(OSVREC,1)
      END IF
C
      IF (IWATFG.EQ.1) THEN
C       input for iwater section
        CALL PIWATR (OUTLEV)
        IF (OUTLEV.GT.1) THEN
C         section delimeter
          WRITE (MESSU,2020)
        END IF
        OSVREC= MAX(OSVREC,1)
      END IF
C
      IF (SLDFG.EQ.1) THEN
C       input for solids section
        CALL PSOLID (OUTLEV)
        IF (OUTLEV.GT.1) THEN
C         section delimeter
          WRITE (MESSU,2020)
        END IF
        OSVREC= MAX(OSVREC,1)
      END IF
C
      IF (IWGFG.EQ.1) THEN
C       input for section iwtgas
        CALL PIWTGS (OUTLEV)
        IF (OUTLEV.GT.1) THEN
C         section delimeter
          WRITE (MESSU,2020)
        END IF
C       bigger osv needed
        OSVREC= MAX(OSVREC,2)
      END IF
C
      IF (IQALFG.EQ.1) THEN
C       input for section iqual
        CALL PIQUAL (MESSFL,OUTLEV,
     M               ECOUNT)
        IF (OUTLEV.GT.1) THEN
C         section delimeter
          WRITE (MESSU,2020)
        END IF
C       bigger osv needed
        OSVREC= MAX(OSVREC,4)
      END IF
C
C     set flux accumulators to zero
      DO 200 LEV= 2,5
        CALL IMPRST(LEV)
 200  CONTINUE
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
        WRITE (MESSU,2010) LSNO
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PIGEN
     I                  (MESSFL,MESSUG,NDELT,
     I                   SDATIM,OUTLEV,OPNO,
     $                   NDAMON,
     M                   ECOUNT)
C
C     + + + PURPOSE + + +
C     Process the general input for the implnd module
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,MESSUG,NDAMON(12),NDELT,OPNO,
     1          SDATIM(5),OUTLEV,ECOUNT
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - unit number of message file
C     MESSUG - file unit to write messages onto
C     OPNO   - index of operation on opntab
C     NDELT  - simulation time interval in minutes
C     SDATIM - starting date/time
C     OUTLEV - run interp output level
C     NDAMON - no. of days in each month of calendar year
C     ECOUNT - count of run interp errors
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION IGEN1 + + +
      INCLUDE    'cilge.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,I0,I1,I2,I3,I4,I6,I8,I1440,SCLU,I11,
     $          IVAL(11),J,JLKXXX,L,PDELT,SGRP,LAST
C
C     + + + FUNCTIONS + + +
      INTEGER   DAYMNH
C
C     + + + EXTERNALS + + +
      EXTERNAL  DAYMNH,ITABLE,HSCKFL,OMSTI,OMSG,HSCKFLX
C
C     + + + INTRINSICS + + +
      INTRINSIC   MIN0,MOD
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' PROCESSING GENERAL INPUT')
 2050 FORMAT (/,' FINISHED PROCESSING GENERAL INPUT')
C
C     + + + END SPECIFICATIONS + + +
C
      I0   = 0
      I1   = 1
      I2   = 2
      I3   = 3
      I4   = 4
      I6   = 6
      I8   = 8
      I11  = 11
      I1440= 1440
      MESSU= MESSUG
      MSGFL= MESSFL
      SCLU = 320
C
      IF (OUTLEV.GT.1) THEN
C       processing message
        WRITE (MESSU,2000)
      END IF
C
C     incorporate information obtained from global and opn sequence blks
      DELT  = NDELT
      DELT60= DELT/60.0
C
      DO 10 J= 1,5
        DATIM(J)= SDATIM(J)
 10   CONTINUE
C
      NDAYS= DAYMNH(YR,MON,NDAMON)
      DO 20 J= 1,12
        NDAY(J)= NDAMON(J)
 20   CONTINUE
C
      HRFG  = 1
      DAYFG = 1
      PIVLNO= 0
      STFG  = 1
      SPIVL=  0
      SPOPNO= OPNO
C
      IF (MON.LT.12) THEN
        NXTMON= MON+ 1
      ELSE
        NXTMON= 1
      END IF
C
C     process active sections vector
      CALL ITABLE (I1,I1,I6,I1,
     M             ASVEC)
C
C     find the highest numbered active section
      LAST= 0
      DO 50 L= 1,6
        IF (ASVEC(L).EQ.1) THEN
          LAST= L
        END IF
 50   CONTINUE
C
      IF (LAST.EQ.0) THEN
C       error - there are no active sections
        CALL OMSTI(LSNO)
        SGRP = 1
        CALL OMSG (MESSU,MESSFL,SCLU,SGRP,
     M             ECOUNT)
      END IF
C
C     process print - info
      CALL ITABLE (I2,I1,I8,I1,
     M             IVAL)
C
      DO 70 I=1,6
        PFLAG(I)= IVAL(I)
 70   CONTINUE
      PIVL  = IVAL(7)
      PYREND= IVAL(8)
C
C     set printout levels for inactive sections to 6 and find minimum lev
      IMPPFG= 6
      DO 80 I= 1,6
        IF (ASVEC(I).EQ.0) THEN
          PFLAG(I)= 6
        END IF
        JLKXXX = PFLAG(I)
        IMPPFG = MIN0(IMPPFG,JLKXXX)
 80   CONTINUE
C
      IF (IMPPFG.EQ.2) THEN
C       check pivl for validity
        PDELT= PIVL*NDELT
        IF (MOD(I1440,PDELT).NE.0) THEN
C         error - printout frequency, as implied by pivl,
C         must be an integer fraction of a day
          CALL OMSTI(LSNO)
          CALL OMSTI(PDELT)
          SGRP= 2
          CALL OMSG (MESSU,MESSFL,SCLU,SGRP,
     M               ECOUNT)
        END IF
      END IF
C
C     process binary - info
      CALL ITABLE (I3,I1,I8,I1,
     M             IVAL)
C
      DO 90 I=1,6
        BFLAG(I)= IVAL(I)
 90   CONTINUE
      BIVL  = IVAL(7)
      BYREND= IVAL(8)
C
C     set bin out levels for inactive sections to 6 and find minimum lev
      IMPBFG= 6
      DO 100 I= 1,6
        IF (ASVEC(I).EQ.0) THEN
          BFLAG(I)= 6
        END IF
        JLKXXX = BFLAG(I)
        IMPBFG = MIN0(IMPBFG,JLKXXX)
 100  CONTINUE
C
      IF (IMPBFG.EQ.2) THEN
C       check bivl for validity
        PDELT= BIVL*NDELT
        IF (MOD(I1440,PDELT).NE.0) THEN
C         error - printout frequency, as implied by bivl,
C         must be an integer fraction of a day
          CALL OMSTI(LSNO)
          CALL OMSTI(PDELT)
          SGRP= 3
          CALL OMSG (MESSU,MESSFL,SCLU,SGRP,
     M               ECOUNT)
        END IF
      END IF
C
C     process table - type gen-info
      CALL ITABLE (I4,I1,I11,I1,
     M             IVAL)
C
      DO 110 J= 1,5
        LSID(J)= IVAL(J)
 110  CONTINUE
C
      DO 120 J= 1,6
        UNIT(J+1)= IVAL(5+J)
 120  CONTINUE
C
C     check output files - if not open, then open with standard name
      DO 130 J= 4,5
        IF (UNIT(J) .GT. 0) THEN
          CALL HSCKFL
     I                (UNIT(J))
        END IF
 130  CONTINUE
      DO 140 J= 6,7
        IF (UNIT(J) .GT. 0) THEN
          CALL HSCKFLX
     I                 (I0,UNIT(J))
        END IF
 140  CONTINUE
C
C     check for error where bivl <> pivl
      IF ((UNIT(6).NE.0 .OR. UNIT(7).NE.0) .AND.
     1    (UNIT(4).NE.0 .OR. UNIT(5).NE.0)) THEN
        IF (BIVL.NE.PIVL) THEN
C         error - bivl must equal pivl
          CALL OMSTI (BIVL)
          CALL OMSTI (PIVL)
          SGRP= 4
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
        END IF
      END IF
C
      IF (OUTLEV.GT.1) THEN
C       end processing message
        WRITE (MESSU,2050)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   IMPLND
     I                   (STIVL,WIDTH)
C
C     + + + PURPOSE + + +
C     Simulate hydrological and/or water quality processes for an
C     impervious land-segment for one INSPAN.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    STIVL,WIDTH
C
C     + + + ARGUMENT DEFINITIONS + + +
C     STIVL  - of inpad
C     WIDTH  - inpad width
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION IGEN2 + + +
      INCLUDE    'cilge.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     IDELT
      CHARACTER*6 OPTYP
C
C     + + + EXTERNALS + + +
      EXTERNAL    IPTOT,ADDTIM,SPECL,ATEMP,SNOW,IWATER,SOLIDS,IWTGAS,
     $            IQUAL,IBAROT,IPRINT,UPQUAN
C
C     + + + DATA INITIALIZATIONS + + +
      DATA        OPTYP/'IMPLND'/
C
C     + + + END SPECIFICATIONS + + +
C
      IVL  = STIVL- 1
      IVL1 = STIVL
      IDELT= DELT
C
      IF (STIVL .EQ. 1) THEN
C       put initial values of point-valued time series into INPAD
        CALL IPTOT
      END IF
C
C     time loop
      DO 10 IVL= STIVL,WIDTH+ STIVL- 1
        IVL1= IVL + 1
        SPIVL= SPIVL+ 1
C       increment time and set time-related flags
        CALL ADDTIM
     I             (IDELT,NDAY,PIVL,PYREND,
     M              DATIM,PIVLNO,
     O              NDAYS,NXTMON,HRFG,DAYFG,EDAYFG,
     O              EMONFG,EPYRFG)
C       hour and day flags are always set on in first interval, to
C       force calculation of intermittently computed values
        IF (STFG .EQ. 1) THEN
C         first interval of run
          STFG = 0
          HRFG = 1
          DAYFG= 1
        END IF
C
        IF (SPAFP .GT. 0 .AND. SPAFP .LE. SPAKND) THEN
C         special actions are being taken and there is at least one left
          CALL SPECL (OPTYP,LSNO,SPAKND,SPOPNO,DATIM,MESSU,SPIVL,
     I                SPOUT,SPNUND,
     M                SPAFP)
        END IF
C
C       perform the simulation
        IF (AIRTFG .EQ. 1) THEN
          CALL ATEMP (OPTYP)
        END IF
C
        IF (SNOWFG .EQ. 1) THEN
          CALL SNOW (OPTYP)
        END IF
C
        IF (IWATFG .EQ. 1) THEN
          CALL IWATER
        END IF
C
        IF (SLDFG .EQ. 1) THEN
          CALL SOLIDS
        END IF
C
        IF (IWGFG .EQ. 1) THEN
          CALL IWTGAS
        END IF
C
        IF (IQALFG .EQ. 1) THEN
          CALL IQUAL
        END IF
C
C       output time series
        CALL IPTOT
        CALL IBAROT
C
C       handle flux accumulation, printout
        IF ( (IMPPFG .LT. 6) .OR. (IMPBFG .LT. 6) ) THEN
          CALL IPRINT
        END IF
C
C       update pipes for user-defined variable quantities
        CALL UPQUAN (SPIVL,SPOPNO)
C
 10   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   IBAROT
C
C     + + + PURPOSE + + +
C     Place the current values of all bar-valued output
C     time series in the INPAD.
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION IGEN2 + + +
      INCLUDE  'cilge.inc'
      INCLUDE  'cmpad.inc'
C
C     + + + EXTERNALS + + +
      EXTERNAL   SNOWPB,IWATIB,SLDIB,IWGSIB,IQALIB
C
C     + + + END SPECIFICATIONS + + +
C
      IF (SNOWFG .EQ. 1) THEN
        CALL SNOWPB
      END IF
C
      IF (IWATFG .EQ. 1) THEN
        CALL IWATIB
      END IF
C
      IF (SLDFG .EQ. 1)  THEN
        CALL SLDIB
      END IF
C
      IF (IWGFG .EQ. 1)  THEN
        CALL IWGSIB
      END IF
C
      IF (IQALFG .EQ. 1) THEN
        CALL IQALIB
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   IMPACC
     I                   (FRMROW,TOROW)
C
C     + + + PURPOSE + + +
C     Accumulate fluxes for output
C       (Don't make assumptions about use of output, just do it
C        jlk 4/28/2005)
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    FRMROW,TOROW
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FRMROW - row containing incremental flux accumulation
C     TOROW  - flux row to be incremented
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION IGEN2 + + +
      INCLUDE    'cilge.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + EXTERNALS + + +
      EXTERNAL   SNOACC,IWAACC,SLDACC,IWGACC,IQACC
C
C     + + + END SPECIFICATIONS + + +
C
C     Section atemp has no printout fluxes
C
      IF (SNOWFG .EQ. 1) THEN
C       section snow is active 
        CALL SNOACC
     I             (FRMROW,TOROW)
      END IF
C
      IF (IWATFG .EQ. 1) THEN
C       section iwater is active 
        CALL IWAACC
     I             (FRMROW,TOROW)
      END IF
C
      IF (SLDFG .EQ. 1) THEN
C       section solids is active 
        CALL SLDACC
     I             (FRMROW,TOROW)
      END IF
C
      IF (IWGFG .EQ. 1) THEN
C       section iwtgas is active 
        CALL IWGACC
     I             (FRMROW,TOROW)
      END IF
C
      IF (IQALFG .EQ. 1) THEN
C       section iqual is active 
        CALL IQACC
     I             (FRMROW,TOROW)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   IMPPRT
     I                   (UNITFG,LEV,PRINTU,BINU)
C
C     + + + PURPOSE + + +
C     Perform printout and continuity checks for module IMPLND.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   LEV,PRINTU,UNITFG,BINU
C
C     + + + ARGUMENT DEFINITIONS + + +
C     UNITFG - output units   1-english, 2-metric
C     LEV    - current output level (2-pivl,3-day,4-mon,5-ann)
C     PRINTU - fortran unit number on which to print output
C     BINU   - fortran unit number on which to write binary output
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION IGEN2 + + +
      INCLUDE    'cilge.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     OPERFG,LPRTU,LBINU
C
C     + + + EXTERNALS + + +
      EXTERNAL   AIRPRT,SNOPRT,IWAPRT,SLDPRT,IWGPRT,IQPRT
C
C     + + + END SPECIFICATIONS + + +
C
      OPERFG = 2
C
      IF (AIRTFG .EQ. 1) THEN
C       section atemp is active
        IF (PRINTU .GT. 0 .AND. PFLAG(1) .LE. LEV) THEN
          LPRTU = PRINTU
        ELSE
          LPRTU = 0
        END IF
        IF (BINU .GT. 0 .AND. ABS(BFLAG(1)) .LE. LEV) THEN
          LBINU = BINU
        ELSE
          LBINU = 0
        END IF
        IF (LPRTU.GT.0 .OR. LBINU.GT.0) THEN
C         section atemp produces printout at this level
          CALL AIRPRT
     I               (UNITFG,LEV,LPRTU,OPERFG,LBINU)
        END IF
      END IF
C
      IF (SNOWFG .EQ. 1) THEN
C       section snow is active
        IF (PRINTU .GT. 0 .AND. PFLAG(2) .LE. LEV) THEN
          LPRTU = PRINTU
        ELSE
          LPRTU = 0
        END IF
        IF (BINU .GT. 0 .AND. ABS(BFLAG(2)) .LE. LEV) THEN
          LBINU = BINU
        ELSE
          LBINU = 0
        END IF
        IF (LPRTU.GT.0 .OR. LBINU.GT.0) THEN
C         section snow produces printout at this level
          CALL SNOPRT
     I               (UNITFG,LEV,LPRTU,OPERFG,LBINU)
        END IF
      END IF
C
      IF (IWATFG .EQ. 1) THEN
C       section iwater is active
        IF (PRINTU .GT. 0 .AND. PFLAG(3) .LE. LEV) THEN
          LPRTU = PRINTU
        ELSE
          LPRTU = 0
        END IF
        IF (BINU .GT. 0 .AND. ABS(BFLAG(3)) .LE. LEV) THEN
          LBINU = BINU
        ELSE
          LBINU = 0
        END IF
        IF (LPRTU.GT.0 .OR. LBINU.GT.0) THEN
C         section iwater produces printout at this level
          CALL IWAPRT
     I               (UNITFG,LEV,LPRTU,LBINU)
        END IF
      END IF
C
      IF (SLDFG .EQ. 1) THEN
C       section solids is active
        IF (PRINTU .GT. 0 .AND. PFLAG(4) .LE. LEV) THEN
          LPRTU = PRINTU
        ELSE
          LPRTU = 0
        END IF
        IF (BINU .GT. 0 .AND. ABS(BFLAG(4)) .LE. LEV) THEN
          LBINU = BINU
        ELSE
          LBINU = 0
        END IF
        IF (LPRTU.GT.0 .OR. LBINU.GT.0) THEN
C         section solids produces printout at this level
          CALL SLDPRT
     I               (UNITFG,LEV,PRINTU,LBINU)
        END IF
      END IF
C
      IF (IWGFG .EQ. 1) THEN
C       section iwtgas is active
        IF (PRINTU .GT. 0 .AND. PFLAG(5) .LE. LEV) THEN
          LPRTU = PRINTU
        ELSE
          LPRTU = 0
        END IF
        IF (BINU .GT. 0 .AND. ABS(BFLAG(5)) .LE. LEV) THEN
          LBINU = BINU
        ELSE
          LBINU = 0
        END IF
        IF (LPRTU.GT.0 .OR. LBINU.GT.0) THEN
C         section iwtgas produces printout at this level
          CALL IWGPRT
     I               (UNITFG,LEV,PRINTU,LBINU)
        END IF
      END IF
C
      IF (IQALFG .EQ. 1) THEN
C       section iqual is active
        IF (PRINTU .GT. 0 .AND. PFLAG(6) .LE. LEV) THEN
          LPRTU = PRINTU
        ELSE
          LPRTU = 0
        END IF
        IF (BINU .GT. 0 .AND. ABS(BFLAG(6)) .LE. LEV) THEN
          LBINU = BINU
        ELSE
          LBINU = 0
        END IF
        IF (LPRTU.GT.0 .OR. LBINU.GT.0) THEN
C         section iqual produces printout at this level
          CALL IQPRT
     I              (UNITFG,LEV,PRINTU,LBINU)
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   IMPRST
     I                   (LEV)
C
C     + + + PURPOSE + + +
C     Reset all flux accumulators and those state variables used
C     in material balance checks.
C       (Don't make assumptions about use of accumulators, 
C        just reset them for all active sections, jlk 4/28/2005)
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   LEV
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEV    - current output level (2-pivl,3-day,4-mon,5-ann)
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION IGEN2 + + +
      INCLUDE  'cilge.inc'
      INCLUDE  'cmpad.inc'
C
C     + + + EXTERNALS + + +
      EXTERNAL  SNORST,IWARST,SLDRST,IWGRST,IQRST
C
C     + + + END SPECIFICATIONS + + +
C
C     section ATEMP has no printout fluxes
C
      IF (SNOWFG .EQ. 1) THEN
C       section snow is active and producing printout
        CALL SNORST
     I             (LEV)
      END IF
C
      IF (IWATFG .EQ. 1) THEN
C       section iwater is active and producing printout
        CALL IWARST
     I             (LEV)
      END IF
C
      IF (SLDFG .EQ. 1) THEN
C       section solids is active and producing printout
        CALL SLDRST
     I             (LEV)
      END IF
C
      IF (IWGFG .EQ. 1) THEN
C       section iwtgas is active and producing printout
        CALL IWGRST
     I             (LEV)
      END IF
C
      IF (IQALFG .EQ. 1) THEN
C       section iqual is active & producing printout
        CALL IQRST
     I            (LEV)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   IPRINT
C
C     + + + PURPOSE + + +
C     Accumulate fluxes, produce printed output and perform
C     materials balance checks for IMPLND module.
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION IGEN2 + + +
      INCLUDE   'cilge.inc'
      INCLUDE   'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   EXDAT(5),I1,I2,I3,I4,I5,J,PRINTU,UNITFG,BINU
C
C     + + + EXTERNALS + + +
      EXTERNAL  IMPACC, EXDATE, IMPPRT, IMPRST
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT ('1',//,' IMPERVIOUS LAND SEGMENT NO. ',I3,6X,18X,5A4,6X,
     $        'REPORT FOR ',I4,' INTERVALS ENDING ',I4,'/',I2,'/',I2,
     $        I3,':',I2)
 2010 FORMAT ('1',//,' IMPERVIOUS LAND SEGMENT NO. ',I3,6X,18X,5A4,30X,
     $        'REPORT FOR DAY ',I4,'/',I2,'/',I2)
 2020 FORMAT ('1',//,' IMPERVIOUS LAND SEGMENT NO. ',I3,6X,18X,5A4,31X,
     $        'REPORT FOR MONTH ',I4,'/',I2)
 2030 FORMAT ('1',//,' IMPERVIOUS LAND SEGMENT NO. ',I3,6X,18X,5A4,16X,
     $        'REPORT FOR PRINTOUT YEAR ENDING ',I4,'/',I2)
C
C     + + + END SPECIFICATIONS + + +
C
      I1= 1
      I2= 2
      I3= 3
      I4= 4
      I5= 5
C     flux accumulation
C
      IF (IMPPFG .EQ. 2 .OR. IMPBFG .EQ. 2) THEN
C       some printout at the pivl level is being produced, so
C       accumulate fluxes to this level
        CALL IMPACC
     I             (I1,I2)
      END IF
C
C     always accumulate to the daily level
      CALL IMPACC
     I           (I1,I3)
C
      IF (EDAYFG .EQ. 1) THEN
C       it's the last interval of the day - accumulate daily
C       fluxes to the month level
        CALL IMPACC
     I             (I3,I4)
C
        IF (EMONFG .EQ. 1) THEN
C         it's the last interval of the month - accumulate
C         monthly fluxes to the year level
          CALL IMPACC
     I               (I4,I5)
        END IF
      END IF
C
C     printout and continuity check
      DO 10 UNITFG= 1,2
        PRINTU= PUNIT(UNITFG)
        BINU  = BUNIT(UNITFG)
        IF (PRINTU .NE. 0 .OR. BINU .NE. 0) THEN
C         printout is required in this set of external units
C         unitfg= 1 for english, 2 for metric.  printu is the fortran
C         logical unit no. to be used for printout
C
          IF ((PIVLNO .EQ. PIVL .AND. IMPPFG .EQ. 2) .OR.
     1        (PIVLNO .EQ. BIVL .AND. IMPBFG .EQ. 2)) THEN
C           it's time to handle any pivl level printout, and some
C           is required
C           convert hour and minute fields in date/time
C           to external format
            CALL EXDATE
     I                 (DATIM,
     O                  EXDAT)
            IF (PRINTU .GT. 0 .AND. IMPPFG .EQ. 2) THEN
              WRITE (PRINTU,2000)  LSNO, LSID, PIVL, EXDAT
            END IF
            CALL IMPPRT
     I                 (UNITFG,I2,PRINTU,BINU)
          END IF
C
          IF (EDAYFG .EQ. 1) THEN
            IF (IMPPFG .LE. 3 .OR. IMPBFG .LE. 3) THEN
C             it's time to handle daily printout
              IF (PRINTU .GT. 0 .AND. IMPPFG .LE. 3) THEN
                WRITE (PRINTU,2010)  LSNO, LSID, (DATIM(J),J=1,3)
              END IF
              CALL IMPPRT
     I                   (UNITFG,I3,PRINTU,BINU)
            END IF
C
            IF (EMONFG .EQ. 1) THEN
              IF (IMPPFG .LE. 4 .OR. IMPBFG .LE. 4) THEN
C               it's time to handle monthly printout
                IF (PRINTU .GT. 0 .AND. IMPPFG .LE. 4) THEN
                  WRITE (PRINTU,2020)  LSNO, LSID, (DATIM(J),J=1,2)
                END IF
                CALL IMPPRT
     I                     (UNITFG,I4,PRINTU,BINU)
              END IF
C
              IF (EPYRFG .EQ. 1) THEN
C               it's time to handle yearly printout
                IF (PRINTU .GT. 0 .AND. IMPPFG .LE. 5) THEN
                  WRITE (PRINTU,2030)  LSNO, LSID, (DATIM(J),J=1,2)
                END IF
                CALL IMPPRT
     I                     (UNITFG,I5,PRINTU,BINU)
              END IF
            END IF
          END IF
        ELSE
C         printout is not required for this set of external units
        END IF
 10   CONTINUE
C
C     reset all flux accumulators and those state variables
C     used in material balance checks
C
      IF ((PIVLNO .EQ. PIVL .AND. IMPPFG .EQ. 2) .OR.
     1    (PIVLNO .EQ. BIVL .AND. IMPBFG .EQ. 2)) THEN
C       reset any pivl level variables in use
        CALL IMPRST
     I             (I2)
      END IF
C
      IF (EDAYFG .EQ. 1) THEN
C       reset any daily variables in use
        CALL IMPRST
     I             (I3)
C
        IF (EMONFG .EQ. 1) THEN
C         reset any monthly variables in use
          CALL IMPRST
     I               (I4)
C
          IF (EPYRFG .EQ. 1) THEN
C           reset any yearly variables in use
            CALL IMPRST
     I                 (I5)
          END IF
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   IPTOT
C
C     + + + PURPOSE + + +
C     Place the current values of all point-valued output
C     time series in the INPAD.
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION IGEN2 + + +
      INCLUDE  'cilge.inc'
      INCLUDE  'cmpad.inc'
C
C     + + + EXTERNALS + + +
      EXTERNAL  AIRTPT,SNOWPT,IWATIP,SLDIP,IWGSIP,IQALIP
C
C     + + + END SPECIFICATIONS + + +
C
      IF (AIRTFG .EQ. 1) THEN
        CALL AIRTPT
      END IF
C
      IF (SNOWFG .EQ. 1) THEN
        CALL SNOWPT
      END IF
C
      IF (IWATFG .EQ. 1) THEN
        CALL IWATIP
      END IF
C
      IF (SLDFG .EQ. 1)  THEN
        CALL SLDIP
      END IF
C
      IF (IWGFG .EQ. 1)  THEN
        CALL IWGSIP
      END IF
C
      IF (IQALFG .EQ. 1) THEN
        CALL IQALIP
      END IF
C
      RETURN
      END
