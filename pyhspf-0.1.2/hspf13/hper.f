C
C
C
      SUBROUTINE   PPERLN
     I                   (NDELT,SDATIM,NDAMON,EMFG,MAXOSV,
     M                    OSVKEY)
C
C     + + + PURPOSE + + +
C     Process the input for the perlnd module
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   NDELT,SDATIM(5),NDAMON(12),OSVKEY,EMFG,MAXOSV
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NDELT  - simulation time interval in minutes
C     SDATIM - starting date/time
C     NDAMON - no. of days in each month of calendar year
C     EMFG   - english/metric units flag (english-1,metric-2)
C     MAXOSV - maximum size of osv
C     OSVKEY - record on OSV where info for this op will go
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PGEN1 + + +
      INCLUDE 'cplpg.inc'
      INCLUDE 'crin2.inc'
      INCLUDE 'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,LEV,OSVKND,OSVKST,OSVREC
C
C     + + + INTRINSICS + + +
      INTRINSIC MAX
C
C     + + + EXTERNALS + + +
      EXTERNAL  PPGEN,PATEMP,PSNOW,PPWATR,PSEDMT,PPSTMP,PPWTGS,PPQUAL,
     $          PMSTLA,PPEST,PNITR,PPHOS,PTRACR,PERRST,PUTOSV
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' ',132('+'),
     $        /,' PROCESSING PERVIOUS LAND-SEGMENT NO:',I4,
     $          '     TIME STEP(DELT):',I5,'  MINS')
 2010 FORMAT (/,' FINISHED PROCESSING PERVIOUS LAND-SEGMENT NO. ',I4,
     $        /,' ',132('+'))
 2020 FORMAT (  ' ',132('-'))
C
C     + + + END SPECIFICATIONS + + +
C
      IF (RESMFG.EQ.0) THEN
C       not resume, initialize the entire osv area
        DO 20 I= 1,MAXOSV
          IPAD(I)= -999
 20     CONTINUE
      END IF
C
      MESSU= FILE(1)
      MSGFL= FILE(15)
C
      IF (OUTLEV.GT.0) THEN
C       processing message
        WRITE (MESSU,2000) OPTNO, NDELT
      END IF
C
C     minimum size of osv for this operation
      OSVREC= 1
C     process the general input
      CALL PPGEN (NDELT,SDATIM,NDAMON)
      IF (OUTLEV.GT.1) THEN
C       section delimeter
        WRITE (MESSU,2020)
      END IF
C     put english/metric units flag into perlnd common
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
C       input for section snow
        CALL PSNOW
        IF (OUTLEV.GT.1) THEN
C         section delimeter
          WRITE (MESSU,2020)
        END IF
        OSVREC= MAX(OSVREC,1)
      END IF
C
      IF (PWATFG.EQ.1) THEN
C       input for section pwater
        CALL PPWATR (SDATIM,NDAMON,
     M               OSVREC)
        IF (OUTLEV.GT.1) THEN
C         section delimeter
          WRITE (MESSU,2020)
        END IF
      END IF
C
      IF (SEDFG.EQ.1) THEN
C       input for section sedmnt
        CALL PSEDMT
        IF (OUTLEV.GT.1) THEN
C         section delimeter
          WRITE (MESSU,2020)
        END IF
C       bigger osv needed
        OSVREC= MAX(OSVREC,3)
      END IF
C
      IF (PSTFG.EQ.1) THEN
C       input for section pstemp
        CALL PPSTMP
        IF (OUTLEV.GT.1) THEN
C         section delimeter
          WRITE (MESSU,2020)
        END IF
C       bigger osv needed
        OSVREC= MAX(OSVREC,3)
      END IF
C
      IF (PWGFG.EQ.1) THEN
C       input for section pwtgas
        CALL PPWTGS
        IF (OUTLEV.GT.1) THEN
C         section delimeter
          WRITE (MESSU,2020)
        END IF
C       bigger osv needed
        OSVREC= MAX(OSVREC,3)
      END IF
C
      IF (PQALFG.EQ.1) THEN
C       input for section pqual
        CALL PPQUAL
        IF (OUTLEV.GT.1) THEN
C         section delimeter
          WRITE (MESSU,2020)
        END IF
C       bigger osv needed
        OSVREC= MAX(OSVREC,7)
      END IF
C
      IF (AGFG.EQ.1) THEN
C       agricultural chemical sections
C
        IF (MSTLFG.EQ.1) THEN
C         input for section mstlay
          CALL PMSTLA
          IF (OUTLEV.GT.1) THEN
C           section delimeter
            WRITE (MESSU,2020)
          END IF
C         bigger osv needed
          OSVREC= MAX(OSVREC,7)
        END IF
C
        IF (PESTFG.EQ.1) THEN
C         input for section pest
          CALL PPEST
          IF (OUTLEV.GT.1) THEN
C           section delimeter
            WRITE (MESSU,2020)
          END IF
C         bigger osv needed
          OSVREC= MAX(OSVREC,10)
        END IF
C                   
        IF (NITRFG.EQ.1) THEN
C         input for section nitr
          CALL PNITR
          IF (OUTLEV.GT.1) THEN
C           section delimeter
            WRITE (MESSU,2020)
          END IF
C         bigger osv needed
          OSVREC= MAX(OSVREC,14)
        END IF
C
        IF (PHOSFG.EQ.1) THEN
C         input for section phos
          CALL PPHOS
          IF (OUTLEV.GT.1) THEN
C           section delimeter
            WRITE (MESSU,2020)
          END IF
C         bigger osv needed
          OSVREC= MAX(OSVREC,15)
        END IF
C
        IF (TRACFG.EQ.1) THEN
C         input for section tracer
          CALL PTRACR
          IF (OUTLEV.GT.1) THEN
C           section delimeter
            WRITE (MESSU,2020)
          END IF
C         bigger osv needed
          OSVREC= MAX(OSVREC,15)
        END IF
      END IF
C
C     set flux accumulators to zero
      DO 200 LEV= 2,5
        CALL PERRST(LEV)
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
        WRITE (MESSU,2010)  OPTNO
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PPGEN
     I                  (NDELT,SDATIM,NDAMON)
C
C     + + + PURPOSE + + +
C     Process the general input for the PERLND module
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   NDELT,SDATIM(5),NDAMON(12)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NDELT  - simulation time interval in minutes
C     SDATIM - starting date/time
C     NDAMON - no. of days in each month of calendar year
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PGEN1 + + +
      INCLUDE   'cplpg.inc'
      INCLUDE   'crin2.inc'
      INCLUDE   'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,I1,I1440,LAST,TBNO,TBSB,NVAL,I0,
     $          IVAL(14),J,JLKXXX,L,PDELT,SCLU,SGRP
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
      I1440= 1440
C     error/warn message cluster
      SCLU = 300
C
      IF (OUTLEV.GT.1) THEN
C       processing message
        WRITE (MESSU,2000)
      END IF
C
C     incorporate information obtained from global and opn sequence blks
      LSNO  = OPTNO
C
      DELT  = NDELT
      DELT60= DELT/60.0
      DELT5 = DELT/5.0
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
      TBNO= 1
      TBSB= 1
      NVAL= 12
      CALL ITABLE(TBNO,TBSB,NVAL,I1,
     M            ASVEC)
C
C     find the highest numbered active section
      LAST= 0
      DO 50 L= 1,12
        IF (ASVEC(L).EQ.1) THEN
C         new highest
          LAST= L
        END IF
 50   CONTINUE
C
      IF (LAST.EQ.0) THEN
C       error - there are no active sections
        CALL OMSTI (LSNO)
        SGRP= 1
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             ECOUNT)
      END IF
C
C     determine whether any agricultural chemical section is active
      AGFG= 0
      DO 70 I= 8,12
        IF (ASVEC(I).EQ.1) THEN
C         ag sections on
          AGFG= 1
        END IF
 70   CONTINUE
C
C     process print - info
      TBNO= 2
      TBSB= 1
      NVAL= 14
      CALL ITABLE(TBNO,TBSB,NVAL,I1,
     M            IVAL)
C
      DO 75 I=1,12
        PFLAG(I)= IVAL(I)
 75   CONTINUE
      PIVL  = IVAL(13)
      PYREND= IVAL(14)

C     set printout levels for active sections to 6 and find min level
      PERPFG= 6
      DO 80 I= 1,12
        IF (ASVEC(I).EQ.0) THEN
C         inactive
          PFLAG(I)= 6
        END IF
        JLKXXX = PFLAG(I)
        PERPFG = MIN0(PERPFG,JLKXXX)
 80   CONTINUE
C
      IF (PERPFG.EQ.2) THEN
C       check pivl for validity
        PDELT= PIVL*NDELT
        IF (MOD(I1440,PDELT).NE.0) THEN
C         error - printout frequency, as implied by
C         pivl, must be an integer fraction of a day
          CALL OMSTI (OPTNO)
          CALL OMSTI (PDELT)
          SGRP= 2
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
        END IF
      END IF
C
C     process binary - info
      TBNO= 3
      TBSB= 1
      NVAL= 14
      CALL ITABLE(TBNO,TBSB,NVAL,I1,
     M            IVAL)
      DO 90 I=1,12
        BFLAG(I)= IVAL(I)
 90   CONTINUE
      BIVL  = IVAL(13)
      BYREND= IVAL(14)
C
C     set bin out levels for active sections to 6 and find min level
      PERBFG= 6
      DO 100 I= 1,12
        IF (ASVEC(I).EQ.0) THEN
C         inactive
          BFLAG(I)= 6
        END IF
        JLKXXX = BFLAG(I)
        PERBFG = MIN0(PERBFG,JLKXXX)
 100  CONTINUE
C
      IF (PERBFG.EQ.2) THEN
C       check bivl for validity
        PDELT= BIVL*NDELT
        IF (MOD(I1440,PDELT).NE.0) THEN
C         error - printout frequency, as implied by
C         bivl, must be an integer fraction of a day
          CALL OMSTI (OPTNO)
          CALL OMSTI (PDELT)
          SGRP= 3
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
        END IF
      END IF
C
C     process table - type gen-info
      TBNO= 4
      TBSB= 1
      NVAL= 11
      CALL ITABLE(TBNO,TBSB,NVAL,I1,
     M            IVAL)
C
      DO 110 J= 1,5
        LSID(J)= IVAL(J)
 110  CONTINUE
C
      DO 120 J= 1,6
        UNIT(J+1)= IVAL(5+J)
 120  CONTINUE
C
C     check output files - if not open, open them with standard name
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
C       finished processing message
        WRITE (MESSU,2050)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PERLND
     I                   (STIVL,WIDTH)
C
C     + + + PURPOSE + + +
C     Simulate hydrological and/or water quality processes for a
C     pervious land-segment for one ISPAN.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   STIVL,WIDTH
C
C     + + + ARGUMENT DEFINITIONS + + +
C     STIVL  - of inpad
C     WIDTH  - inpad width
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PGEN2 + + +
      INCLUDE    'cplpg.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     IDELT
      CHARACTER*6 OPTYP
C
C     + + + EXTERNALS + + +
      EXTERNAL    PPTOT, ADDTIM, SPECL, ATEMP, SNOW, PWATER, SEDMNT,
     $            PSTEMP, PWTGAS, PQUAL, MSTLAY, PEST, NITR, PHOS,
     $            TRACER, PBAROT, PPRINT, UPQUAN
C
C     + + + DATA INITIALIZATIONS + + +
      DATA       OPTYP /'PERLND'/
C
C     + + + END SPECIFICATIONS + + +
C
      IVL  = STIVL- 1
      IVL1 = STIVL
      IDELT= DELT
C
      IF (STIVL .EQ. 1) THEN
C       put initial values of point-valued time series into INPAD
        CALL PPTOT
      END IF
C
C     time loop
      DO 10 IVL= STIVL,WIDTH+ STIVL- 1
        IVL1= IVL+ 1
        SPIVL= SPIVL+ 1
C       increment time and set time-related flags
        CALL ADDTIM
     I             (IDELT,NDAY,PIVL,PYREND,
     M              DATIM,PIVLNO,
     O              NDAYS,NXTMON,HRFG,DAYFG,EDAYFG,
     O              EMONFG,EPYRFG)
C
C       hour and day flags are always set on in first interval,to
C       force calculation of intermittently computed values
        IF (STFG .EQ. 1) THEN
C         first interval of run
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
        IF (PWATFG .EQ. 1) THEN
          CALL PWATER
        END IF
C
        IF (SEDFG .EQ. 1) THEN
          CALL SEDMNT
        END IF
C
        IF (PSTFG .EQ. 1) THEN
          CALL PSTEMP
        END IF
C
        IF (PWGFG .EQ. 1) THEN
          CALL PWTGAS
        END IF
C
        IF (PQALFG .EQ. 1) THEN
          CALL PQUAL
        END IF
C
        IF (AGFG .EQ. 1) THEN
C         code to call agricultural sections
          IF (MSTLFG .EQ. 1) THEN
            CALL MSTLAY
          END IF
C
          IF (PESTFG .EQ. 1) THEN
            CALL PEST
          END IF
C
          IF (NITRFG .EQ. 1) THEN
            CALL NITR
          END IF
C
          IF (PHOSFG .EQ. 1) THEN
            CALL PHOS
          END IF
C
          IF (TRACFG .EQ. 1) THEN
            CALL TRACER
          END IF
        END IF
C
C       output time series
        CALL PPTOT
        CALL PBAROT
C
C       handle flux accumulation, printout
        IF ( (PERPFG .LT. 6) .OR. (PERBFG .LT. 6) ) THEN
          CALL PPRINT
        END IF
C       not at start of run anymore
        STFG= 0
C
C       update pipes for user-defined variable quantities
        CALL UPQUAN (SPIVL,SPOPNO)
C
C       end time loop
 10   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   PBAROT
C
C     + + + PURPOSE + + +
C     Place the current values of all bar-valued output
C     time series in the INPAD.
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PGEN2 + + +
      INCLUDE  'cplpg.inc'
      INCLUDE  'cmpad.inc'
C
C     + + + EXTERNALS + + +
      EXTERNAL SNOWPB,PWATPB,SEDTPB,PWGSPB,PQALPB,PESTPB,NITRPB,
     $         PHOSPB,TRACPB
C
C     + + + END SPECIFICATIONS + + +
C
      IF (SNOWFG .EQ. 1) THEN
        CALL SNOWPB
      END IF
C
      IF (PWATFG .EQ. 1) THEN
        CALL PWATPB
      END IF
C
      IF (SEDFG .EQ. 1) THEN
        CALL SEDTPB
      END IF
C
      IF (PWGFG .EQ. 1) THEN
        CALL PWGSPB
      END IF
C
      IF (PQALFG .EQ. 1) THEN
        CALL PQALPB
      END IF
C
      IF (AGFG .EQ. 1) THEN
C       one or more of the agri-chemical sections are on
        IF (PESTFG .EQ. 1) THEN
          CALL PESTPB
        END IF
C
        IF (NITRFG .EQ. 1) THEN
          CALL NITRPB
        END IF
C
        IF (PHOSFG .EQ. 1) THEN
          CALL PHOSPB
        END IF
C
        IF (TRACFG .EQ. 1) THEN
          CALL TRACPB
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PERACC
     I                   (FRMROW,TOROW)
C
C
C     + + + PURPOSE + + +
C     Accumulate fluxes for output
C       (Don't make assumptions about use of output, just do it
C        jlk 4/28/2005)
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   FRMROW,TOROW
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FRMROW - row containing incremental flux accumulation
C     TOROW  - flux row to be incremented
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PGEN2 + + +
      INCLUDE   'cplpg.inc'
      INCLUDE   'cmpad.inc'
C
C     + + + EXTERNALS + + +
      EXTERNAL  SNOACC,PWAACC,SDACC,PWGACC,PQACC,PSTACC,NITACC,
     $          PHOACC, TRACC
C
C     + + + END SPECIFICATIONS + + +
C
C     section ATEMP has no output fluxes
C
      IF (SNOWFG .EQ. 1) THEN
C       section snow is active 
        CALL SNOACC
     I             (FRMROW,TOROW)
      END IF
C
      IF (PWATFG .EQ. 1) THEN
C       section pwater is active 
        CALL PWAACC
     I             (FRMROW,TOROW)
      END IF
C
      IF (SEDFG .EQ. 1) THEN
C       section sedmnt is active 
        CALL SDACC
     I            (FRMROW,TOROW)
      END IF
C
C     section PSTEMP has no output fluxes
C
      IF (PWGFG .EQ. 1) THEN
C       section pwtgas is active 
        CALL PWGACC
     I             (FRMROW,TOROW)
      END IF
C
      IF (PQALFG .EQ. 1) THEN
C       section pqual is active 
        CALL PQACC
     I            (FRMROW,TOROW)
      END IF
C
      IF (AGFG .EQ. 1) THEN
C       section mstlay has no output fluxes
C
        IF (PESTFG .EQ. 1) THEN
C         section pest is active 
          CALL PSTACC
     I               (FRMROW,TOROW)
        END IF
C
        IF (NITRFG .EQ. 1) THEN
C         section nitr is active 
          CALL NITACC
     I               (FRMROW,TOROW)
        END IF
C
        IF (PHOSFG .EQ. 1) THEN
C         section phos is active 
          CALL PHOACC
     I               (FRMROW,TOROW)
        END IF
C
        IF (TRACFG .EQ. 1) THEN
C         section tracer is active 
          CALL TRACC
     I              (FRMROW,TOROW)
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PERPRT
     I                   (UNITFG,LEV,PRINTU,BINU)
C
C     + + + PURPOSE + + +
C     Perform printout and continuity checks for module PERLND.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    UNITFG,LEV,PRINTU,BINU
C
C     + + + ARGUMENT DEFINITIONS + + +
C     UNITFG - output units   1-english, 2-metric
C     LEV    - current output level (2-pivl,3-day,4-mon,5-ann)
C     PRINTU - fortran unit number on which to print output
C     BINU   - fortran unit number on which to write binary output
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PGEN2 + + +
      INCLUDE     'cplpg.inc'
      INCLUDE     'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     OPERFG,LPRTU,LBINU
      REAL        MFACTA,MFACTB
      CHARACTER*8 AGUNIT(2),AGMAID
C
C     + + + INTRINSICS + + +
      INTRINSIC   ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL    AIRPRT,SNOPRT,PWAPRT,SDPRT,PTPRT,PWGPRT,PQPRT,
     $            MSTPRT,PESPRT,NITPRT,PHOPRT,TRAPRT
C
C     + + + DATA INITIALIZATIONS + + +
      DATA        AGUNIT/'(LB/AC) ','(KG/HA) ' /
C
C     + + + END SPECIFICATIONS + + +
C
      OPERFG = 1
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
      IF (PWATFG .EQ. 1) THEN
C       section pwater is active
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
C         section pwater produces printout at this level
          CALL PWAPRT
     I               (UNITFG,LEV,LPRTU,LBINU)
        END IF
      END IF
C
      IF (SEDFG .EQ. 1) THEN
C       section sedmnt is active
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
C         section sedmnt produces printout at this level
          CALL SDPRT
     I               (UNITFG,LEV,LPRTU,LBINU)
        END IF
      END IF
C
      IF (PSTFG .EQ. 1) THEN
C       section pstemp is active
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
C         section pstemp produces printout at this level
          CALL PTPRT
     I               (UNITFG,LEV,LPRTU,LBINU)
        END IF
      END IF
C
      IF (PWGFG .EQ. 1) THEN
C       section pwtgas is active
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
C         section pwtgas produces printout at this level
          CALL PWGPRT
     I               (UNITFG,LEV,LPRTU,LBINU)
        END IF
      END IF
C
      IF (PQALFG .EQ. 1) THEN
C       section pqual is active
        IF (PRINTU .GT. 0 .AND. PFLAG(7) .LE. LEV) THEN
          LPRTU = PRINTU
        ELSE
          LPRTU = 0
        END IF
        IF (BINU .GT. 0 .AND. ABS(BFLAG(7)) .LE. LEV) THEN
          LBINU = BINU
        ELSE
          LBINU = 0
        END IF
        IF (LPRTU.GT.0 .OR. LBINU.GT.0) THEN
C         section pqual produces printout at this level
          CALL PQPRT
     I               (UNITFG,LEV,LPRTU,LBINU)
        END IF
      END IF
C
      IF (AGFG .EQ. 1) THEN
C       mass per area conversion factors
        IF (UUNITS .EQ. 1) THEN
          IF (UNITFG .EQ. 1) THEN
C           english to english
            MFACTA= 1.0
          ELSE
C           english to metric
            MFACTA= 1.122
          END IF
        ELSE
          IF (UNITFG .EQ. 1) THEN
C           metric to english
            MFACTA= 0.8913
          ELSE
C           metric to metric
            MFACTA= 1.0
          END IF
        END IF
C
        MFACTB= 0.0
C
C       get correct values for identifying printout units
        IF (UNITFG .EQ. 1) THEN
          AGMAID= AGUNIT(1)
        ELSE
          AGMAID= AGUNIT(2)
        END IF
C
        IF (MSTLFG .EQ. 1) THEN
C         section mstlay is active
          IF (PRINTU .GT. 0 .AND. PFLAG(8) .LE. LEV) THEN
            LPRTU = PRINTU
          ELSE
            LPRTU = 0
          END IF
          IF (BINU .GT. 0 .AND. ABS(BFLAG(8)) .LE. LEV) THEN
            LBINU = BINU
          ELSE
            LBINU = 0
          END IF
          IF (LPRTU.GT.0 .OR. LBINU.GT.0) THEN
C           section mstlay produces printout at this level
            CALL MSTPRT
     I                 (LEV,LPRTU,AGMAID,MFACTA,MFACTB,UNITFG,LBINU)
          END IF
        END IF
C
        IF (PESTFG .EQ. 1) THEN
C         section pest is active
          IF (PRINTU .GT. 0 .AND. PFLAG(9) .LE. LEV) THEN
            LPRTU = PRINTU
          ELSE
            LPRTU = 0
          END IF
          IF (BINU .GT. 0 .AND. ABS(BFLAG(9)) .LE. LEV) THEN
            LBINU = BINU
          ELSE
            LBINU = 0
          END IF
          IF (LPRTU.GT.0 .OR. LBINU.GT.0) THEN
C           section pest produces printout at this level
            CALL PESPRT
     I                 (LEV,LPRTU,AGMAID,MFACTA,MFACTB,UNITFG,LBINU)
          END IF
        END IF
C
        IF (NITRFG .EQ. 1) THEN
C         section nitr is active
          IF (PRINTU .GT. 0 .AND. PFLAG(10) .LE. LEV) THEN
            LPRTU = PRINTU
          ELSE
            LPRTU = 0
          END IF
          IF (BINU .GT. 0 .AND. ABS(BFLAG(10)) .LE. LEV) THEN
            LBINU = BINU
          ELSE
            LBINU = 0
          END IF
          IF (LPRTU.GT.0 .OR. LBINU.GT.0) THEN
C           section nitr produces printout at this level
            CALL NITPRT
     I                 (LEV,LPRTU,AGMAID,MFACTA,MFACTB,UNITFG,LBINU)
          END IF
        END IF
C
        IF (PHOSFG .EQ. 1) THEN
C         section phos is active
          IF (PRINTU .GT. 0 .AND. PFLAG(11) .LE. LEV) THEN
            LPRTU = PRINTU
          ELSE
            LPRTU = 0
          END IF
          IF (BINU .GT. 0 .AND. ABS(BFLAG(11)) .LE. LEV) THEN
            LBINU = BINU
          ELSE
            LBINU = 0
          END IF
          IF (LPRTU.GT.0 .OR. LBINU.GT.0) THEN
C           section phos produces printout at this level
            CALL PHOPRT
     I                 (LEV,LPRTU,AGMAID,MFACTA,MFACTB,UNITFG,LBINU)
          END IF
        END IF
C
        IF (TRACFG .EQ. 1) THEN
C         section tracer is active
          IF (PRINTU .GT. 0 .AND. PFLAG(12) .LE. LEV) THEN
            LPRTU = PRINTU
          ELSE
            LPRTU = 0
          END IF
          IF (BINU .GT. 0 .AND. ABS(BFLAG(12)) .LE. LEV) THEN
            LBINU = BINU
          ELSE
            LBINU = 0
          END IF
          IF (LPRTU.GT.0 .OR. LBINU.GT.0) THEN
C           section tracer produces printout at this level
            CALL TRAPRT
     I                 (LEV,LPRTU,AGMAID,MFACTA,MFACTB,UNITFG,LBINU)
          END IF
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PERRST
     I                  (LEV)
C
C     + + + PURPOSE + + +
C     Reset all flux accumulators and those state variables used
C     in material balance checks.
C       (Don't make assumptions about use of accumulators, 
C        just reset them for all active sections, jlk 4/28/2005)
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    LEV
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEV    - current output level (2-pivl,3-day,4-mon,5-ann)
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PGEN2 + + +
      INCLUDE    'cplpg.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + EXTERNALS + + +
      EXTERNAL   SNORST,PWARST,SDRST,PQRST,PSTRST,NITRST,
     $           PHORST,TRRST,PWGRST
C
C     + + + END SPECIFICATIONS + + +
C
C     section ATEMP has no output fluxes
C
      IF (SNOWFG .EQ. 1) THEN
C       section snow is active 
        CALL SNORST
     I             (LEV)
      END IF
C
      IF (PWATFG .EQ. 1) THEN
C       section pwater is active 
        CALL PWARST
     I             (LEV)
      END IF
C
      IF (SEDFG .EQ. 1) THEN
C       section sedmnt is active 
        CALL SDRST
     I             (LEV)
      END IF
C
C     section PSTEMP has no output fluxes
C
      IF (PWGFG .EQ. 1) THEN
C       section pwtgas is active 
        CALL PWGRST
     I             (LEV)
      END IF
C
      IF (PQALFG .EQ. 1) THEN
C       section pqual is active 
        CALL PQRST
     I            (LEV)
      END IF
C
      IF (AGFG .EQ. 1) THEN
C       section mstlay has no output fluxes
C
        IF (PESTFG .EQ. 1) THEN
C         section pest is active 
          CALL PSTRST
     I               (LEV)
        END IF
C
        IF (NITRFG .EQ. 1) THEN
C         section nitr is active 
          CALL NITRST
     I               (LEV)
        END IF
C
        IF (PHOSFG .EQ. 1) THEN
C         section phos is active 
          CALL PHORST
     I               (LEV)
        END IF
C
        IF (TRACFG .EQ. 1) THEN
C         section tracer is active
          CALL TRRST
     I               (LEV)
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PPRINT
C
C     + + + PURPOSE + + +
C     Accumulate fluxes, produce printed output and perform
C     materials balance checks for PERLND module.
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PGEN2 + + +
      INCLUDE    'cplpg.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    EXDAT(5),I1,I2,I3,I4,I5,J,PRINTU,UNITFG,BINU
C
C     + + + EXTERNALS + + +
      EXTERNAL   PERACC, EXDATE, PERPRT, PERRST
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT ('1',//,' PERVIOUS LAND SEGMENT NO. ',I3,6X,20X,5A4,6X,
     $        'REPORT FOR ',I4,' INTERVALS ENDING ',I4,'/',I2,'/',
     $        I2,I3,':',I2)
 2010 FORMAT ('1',//,' PERVIOUS LAND SEGMENT NO. ',I3,6X,20X,5A4,30X,
     $        'REPORT FOR DAY ',I4,'/',I2,'/',I2)
 2020 FORMAT ('1',//,' PERVIOUS LAND SEGMENT NO. ',I3,6X,20X,5A4,31X,
     $        'REPORT FOR MONTH ',I4,'/',I2)
 2030 FORMAT ('1',//,' PERVIOUS LAND SEGMENT NO. ',I3,6X,20X,5A4,16X,
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
      IF (PERPFG .EQ. 2 .OR. PERBFG .EQ. 2) THEN
C       some printout at the pivl level is being produced, so
C       accumulate fluxes to this level
        CALL PERACC
     I             (I1,I2)
      END IF
C
C     always accumulate to the daily level
      CALL PERACC
     I           (I1,I3)
C
      IF (EDAYFG .EQ. 1) THEN
C       it's the last interval of the day - accumulate daily
C       fluxes to the month level
        CALL PERACC
     I             (I3,I4)
C
        IF (EMONFG .EQ. 1) THEN
C         it's the last interval of the month - accumulate
C         monthly fluxes to the year level
          CALL PERACC
     I               (I4,I5)
        END IF
      END IF
C
C     printout and continuity check
      DO 10 UNITFG= 1,2
        PRINTU= PUNIT(UNITFG)
        BINU  = BUNIT(UNITFG)
        IF (PRINTU .NE. 0 .OR. BINU .NE. 0) THEN
C         printout is required in this set of external units.
C         unitfg= 1 for english, 2 for metric.  printu is the
C         fortran logical unit no. to be used for printout.
C
          IF ((PIVLNO .EQ. PIVL .AND. PERPFG .EQ. 2) .OR.
     1        (PIVLNO .EQ. BIVL .AND. PERBFG .EQ. 2)) THEN
C           it's time to handle any pivl level printout, and some is required
C           transform hour and minute fields in date/time to external format
            CALL EXDATE
     I                 (DATIM,
     O                  EXDAT)
            IF (PRINTU .GT. 0 .AND. PERPFG .EQ. 2) THEN
              WRITE (PRINTU,2000)  LSNO, LSID, PIVL, EXDAT
            END IF
            CALL PERPRT
     I                 (UNITFG,I2,PRINTU,BINU)
          END IF
C
          IF (EDAYFG .EQ. 1) THEN
            IF (PERPFG .LE. 3 .OR. PERBFG .LE. 3) THEN
C             it's time to handle daily printout
              IF (PRINTU .GT. 0 .AND. PERPFG .LE. 3) THEN
                WRITE (PRINTU,2010)  LSNO, LSID, (DATIM(J),J=1,3)
              END IF
              CALL PERPRT
     I                   (UNITFG,I3,PRINTU,BINU)
            END IF
C
            IF (EMONFG .EQ. 1) THEN
              IF (PERPFG .LE. 4 .OR. PERBFG .LE. 4) THEN
C               it's time to handle monthly printout
                IF (PRINTU .GT. 0 .AND. PERPFG .LE. 4) THEN
                  WRITE (PRINTU,2020)  LSNO, LSID, (DATIM(J),J=1,2)
                END IF
                CALL PERPRT
     I                     (UNITFG,I4,PRINTU,BINU)
              END IF
C
              IF (EPYRFG .EQ. 1) THEN
C               it's time to handle yearly printout
                IF (PRINTU .GT. 0 .AND. PERPFG .LE. 5) THEN
                  WRITE (PRINTU,2030)  LSNO, LSID, (DATIM(J),J=1,2)
                END IF
                CALL PERPRT
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
      IF ((PIVLNO .EQ. PIVL .AND. PERPFG .EQ. 2) .OR.
     1    (PIVLNO .EQ. BIVL .AND. PERBFG .EQ. 2)) THEN
C       reset any pivl level variables in use
        CALL PERRST
     I             (I2)
      END IF
C
      IF (EDAYFG .EQ. 1) THEN
C       reset any daily variables in use
        CALL PERRST
     I             (I3)
C
        IF (EMONFG .EQ. 1) THEN
C         reset any monthly variables in use
          CALL PERRST
     I               (I4)
C
          IF (EPYRFG .EQ. 1) THEN
C           reset any yearly variables in use
            CALL PERRST
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
      SUBROUTINE   PPTOT
C
C     + + + PURPOSE + + +
C     Place the current values of all point-valued output
C     time series in the INPAD.
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PGEN2 + + +
      INCLUDE  'cplpg.inc'
      INCLUDE  'cmpad.inc'
C
C     + + + EXTERNALS + + +
      EXTERNAL AIRTPT,SNOWPT,PWATPT,SEDTPT,PSTPT,PWGSPT,PQALPT,
     $         MSTLPT,PESTPT,NITRPT,PHOSPT,TRACPT
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
      IF (PWATFG .EQ. 1) THEN
        CALL PWATPT
      END IF
C
      IF (SEDFG .EQ. 1) THEN
         CALL SEDTPT
      END IF
C
      IF (PSTFG .EQ. 1) THEN
         CALL PSTPT
      END IF
C
      IF (PWGFG .EQ. 1) THEN
         CALL PWGSPT
      END IF
C
      IF (PQALFG .EQ. 1) THEN
        CALL PQALPT
      END IF
C
      IF (AGFG .EQ. 1) THEN
C       one or more of the agri-chemical sections are on
        IF (MSTLFG .EQ. 1) THEN
          CALL MSTLPT
        END IF
C
        IF (PESTFG .EQ. 1) THEN
          CALL PESTPT
        END IF
C
        IF (NITRFG .EQ. 1) THEN
          CALL NITRPT
        END IF
C
        IF (PHOSFG .EQ. 1) THEN
          CALL PHOSPT
        END IF
C
        IF (TRACFG .EQ. 1) THEN
          CALL TRACPT
        END IF
      END IF
C
      RETURN
      END
