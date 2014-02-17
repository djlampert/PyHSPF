C
C
C
      SUBROUTINE   PIQUAL
     I                    (MESSFL,OUTLEV,
     M                     ECOUNT)
C
C     + + + PURPOSE + + +
C     Process input for section iqual
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,ECOUNT,OUTLEV
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - message file unit number
C     ECOUNT - count of run interp errors
C     OUTLEV - run interp output level
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION IQUAL1 + + +
      INCLUDE    'ciliq.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I1,I2,I4,IVAL(11),J,N,I,SCLU,SGRP,RETCOD
      REAL         RVAL(12),SQOLIM,R0
      CHARACTER*12 CSTR
C
C     + + + EQUIVALENCES + + +
      CHARACTER*1  CSTR1(12)
      EQUIVALENCE (CSTR,CSTR1)
C
C     + + + EXTERNALS + + +
      EXTERNAL     ITABLE,RTABLE,OMSG,OMSTC,ZIPR,MDATBL
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' PROCESSING INPUT FOR SECTION IQUAL')
 2010 FORMAT (3A4)
 2040 FORMAT (/,' FINISHED PROCESSING INPUT FOR SECTION IQUAL')
C
C     + + + END SPECIFICATIONS + + +
C
      I1= 1
      R0 = 0.0
C
      SCLU= 326
C
      IF (OUTLEV.GT.1) THEN
C       processing message
        WRITE (MESSU,2000)
      END IF
C
C     initialize month-data input
      I= 12*MXQUAL
      CALL ZIPR (I,R0,
     O           IQAFXM)
      CALL ZIPR (I,R0,
     O           IQACNM)
C
C     initialize atmospheric deposition fluxes
      I= 5*MXQUAL
      CALL ZIPR (I,R0,
     O           IQCF3)
      CALL ZIPR (I,R0,
     O           IQCF4)
C
      IF (IWGFG .EQ. 0) THEN
C       process values in table-type lat-factor
        I2= 27
        I4= 2
        CALL RTABLE (I2,I1,I4,UUNITS,
     M               LIFAC)
      END IF
C
C     find number of quality constituents - table-type nquals
      I2= 31
      I4= 1
      CALL ITABLE (I2,I1,I4,UUNITS,
     M             IVAL)
      NQUAL= IVAL(1)
C
C     get atmospheric deposition flags - table-type iql-ad-flags
      I2= 32
      I4= MXQAL2
      CALL ITABLE (I2,I1,I4,UUNITS,
     M             IQADFG)
C
C     read in month-data tables where necessary
      DO 50 J= 1, NQUAL
        N= 2*(J- 1)+ 1
        IF (IQADFG(N) .GT. 0) THEN
C         monthly flux must be read
          CALL MDATBL
     I                (IQADFG(N),
     O                 IQAFXM(1,J),RETCOD)
C         convert units to internal - not done by MDATBL
          IF (UUNITS .EQ. 1) THEN
C           convert from qty/ac.day to qty/ac.ivl
            DO 10 I= 1, 12
              IQAFXM(I,J)= IQAFXM(I,J)*DELT60/24.0
 10         CONTINUE
          ELSE IF (UUNITS .EQ. 2) THEN
C           convert from qty/ha.day to qty/ac.ivl (not qty/ha.ivl)
            DO 20 I= 1, 12
              IQAFXM(I,J)= IQAFXM(I,J)*0.4047*DELT60/24.0
 20         CONTINUE
          END IF
        END IF
        IF (IQADFG(N+1) .GT. 0) THEN
C         monthly ppn conc must be read
          CALL MDATBL
     I                (IQADFG(N+1),
     O                 IQACNM(1,J),RETCOD)
C         convert units to internal - not done by MDATBL
          IF (UUNITS .EQ. 1) THEN
C           convert from qty/ft3 to qty/ac.in
            DO 30 I= 1, 12
              IQACNM(I,J)= IQACNM(I,J)*3630.0
 30         CONTINUE
          ELSE IF (UUNITS .EQ. 2) THEN
C           convert from qty/L to qty/ac.in (not qty/ha.in)
            DO 40 I= 1, 12
              IQACNM(I,J)= IQACNM(I,J)*102833.0
 40         CONTINUE
          END IF
        END IF
 50   CONTINUE
C
C     initialize counters
      NQSD= 0
      NQOF= 0
C     initialize outflow and washoff arrays
      DO 60 J= 1,MXQUAL
        SOQS(J) = 0.0
        SOQO(J) = 0.0
 60   CONTINUE
C
      DO 90 N= 1,NQUAL
C       get id's and flags - table-type qual-props
        I2= 33
        I4= 8
        CALL ITABLE (I2,N,I4,UUNITS,
     M               IVAL)
        DO 70 J= 1,3
          QUALID(J,N)= IVAL(J)
 70     CONTINUE
        QTYID(1,N)= IVAL(4)
C
        QSDFG(N)= IVAL(5)
        IF (QSDFG(N).NE.0) THEN
C         this is a qualsd
          NQSD= NQSD+ 1
          IF (NQSD .GT. MXQUAL) THEN
C           error - too many sediment associated quality constituents
            CALL OMSTI (MXQUAL)
            WRITE (CSTR,2010) (QUALID(J,N),J=1,3)
            I = 12
            CALL OMSTC (I,CSTR1)
            SGRP= 1
            CALL OMSG (MESSU,MESSFL,SCLU,SGRP,
     M                 ECOUNT)
            NQSD= MXQUAL
          END IF
          QSDFP(N)    = NQSD
          VPFWFG(NQSD)= IVAL(6)
        ELSE
          QSDFP(N)= 0
        END IF
C
        QSOFG(N)= IVAL(7)
        IF (QSOFG(N).NE.0) THEN
C         this is a qualof
          NQOF= NQOF+ 1
          IF (NQOF.GT.MXQUAL) THEN
C           error - too many overland-flow-associated
C           quality constituents
            CALL OMSTI (MXQUAL)
            WRITE (CSTR,2010) (QUALID(J,N),J=1,3)
            I= 12
            CALL OMSTC (I,CSTR1)
            SGRP= 2
            CALL OMSG (MESSU,MESSFL,SCLU,SGRP,
     M                 ECOUNT)
            NQOF= MXQUAL
          END IF
          QSOFP(N)= NQOF
          VQOFG(NQOF)= IVAL(8)
        ELSE
          QSOFP(N)= 0
C
          J= 2*(N- 1)+ 1
          IF ( (IQADFG(J) .NE. 0) .OR. (IQADFG(J+1) .NE. 0) ) THEN
C           error - non-qualof cannot have atmospheric deposition
            WRITE (CSTR,2010) (QUALID(I,N),I=1,3)
            I= 12
            CALL OMSTC (I,CSTR1)
            SGRP= 3
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 ECOUNT)
            IQADFG(J)= 0
            IQADFG(J+1)= 0
          END IF
        END IF
C
C       read in storage on surface and values for any
C       parameters which do not vary seasonally - qual-input
        I2= 34
        I4= 5
        CALL RTABLE (I2,N,I4,UUNITS,
     M               RVAL)
C
        IF (QSDFG(N).NE.0) THEN
          POTFW(NQSD)= RVAL(2)
        END IF
C
        IF (QSOFG(N).GE.1) THEN
          SQO(NQOF)   = RVAL(1)
          ACQOP(NQOF) = RVAL(3)
          SQOLIM      = RVAL(4)
C         compute removal rate
          REMQOP(NQOF)= ACQOP(NQOF)/SQOLIM
          WSFAC(NQOF) = 2.30/RVAL(5)
        ELSE IF (QSOFG(N) .EQ. -1) THEN
C         special case for ches bay - allow constant conc qualof
C         this converts units from mg/l to lb/ac/in
          ACQOP(NQOF) = RVAL(3)*0.2266
          SQO(NQOF)   = 0.0
        END IF
C
        IF (QSDFG(N).NE.0) THEN
          IF (VPFWFG(NQSD).EQ.1) THEN
C           get monthly values of washoff potency
C           factor - table-type mon-potfw
            I2= 35
            I4= 12
            CALL RTABLE (I2,NQSD,I4,UUNITS,
     M                   POTFWM(1,NQSD))
          END IF
        END IF
C
        IF (QSOFG(N).GE.1) THEN
C         using buildup/washoff method
          IF (VQOFG(NQOF).EQ.1) THEN
C           get monthly values of accumulation rates -
C           table-type mon-accum
            I2= 36
            I4= 12
            CALL RTABLE (I2,NQOF,I4,UUNITS,
     M                   ACQOPM(1,NQOF))
C           get monthly values of limiting storage
C           table-type mon-sqolim
            I2=37
            I4=12
            CALL RTABLE (I2,NQOF,I4,UUNITS,
     M                   RVAL)
C           calculate monthly values of removal rate
            DO 80 J = 1,12
              REMQOM(J,NQOF)= ACQOPM(J,NQOF)/RVAL(J)
 80         CONTINUE
          END IF
        END IF
 90   CONTINUE
C
      IF (OUTLEV.GT.1) THEN
C       end processing message
        WRITE (MESSU,2040)
      END IF
C
      RETURN
      END
C
C     4.2(2).6
C
      SUBROUTINE   IQUAL
C
C     + + + PURPOSE + + +
C     Simulate quality constituents (other than sediment, heat, dox,
C     and co2) using simple relationships with solids and runoff
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION IQUAL2 + + +
      INCLUDE   'ciliq.inc'
      INCLUDE   'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + + +
      INTEGER   N,QOFP,QSFP,J,REQFG,TSSUB(2),FLGVAL
      REAL      SUROQO,SUROQS,ATDPFX,ATDPCN
      CHARACTER*6 OPTYP,TSNAM,SECNAM,MSECNM,OPFGNM
C
C     + + + EXTERNALS + + +
      EXTERNAL WASHSD,WASHOF,HREQTS
C
C     + + + DATA INITIALIZATIONS + + +
      DATA TSSUB/1,1/
      DATA OPTYP,SECNAM/'IMPLND','IQUAL '/
C
C     + + + END SPECIFICATIONS + + +
C
C     Simulate washoff of quality constituents (other than solids,
C     Heat, dox, and co2) using simple relationships with solids
C     And/or water yield
C
C     nqual is the number of constituents being simulated
      DO 140 N= 1,NQUAL
C       simulate constituent n
C
C       simulate by association with solids
        IF (QSDFG(N).NE.0) THEN
C         constituent n is simulated by association with solids
C         the value of qsfp refers to the set of solids
C         associated parameters to use
          QSFP= QSDFP(N)
C
C         get input time series
          IF (SLDFG.EQ.0) THEN
C           read time series from inpad
CTHJ            SOSLD= PAD(SOSDFP+IVL1)
            REQFG= 5
            MSECNM= 'SOLIDS'
            OPFGNM= 'QSDFG '
            TSNAM= 'SOSLD '
            CALL HREQTS (SOSDFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,
     I                   LSNO,TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,
     I                   QSDFG(N),
     O                   SOSLD)
            IF (SLSDFP .GE. 1) THEN
C             sediment lateral inflow present
              SLSLD= PAD(SLSDFP+IVL1)
            ELSE
C             no sediment lateral inflow
              SLSLD= 0.0
            END IF
          ELSE
C           sosld, and optionally slsld, are available from solids
          END IF
C
          IF ( (SLSLD .LE. 0.0) .OR. (SLIQSX(QSFP) .LT. 1) ) THEN
C           no defined potency factor of lateral inflow
            SLIQSP(QSFP)= -1.0E30
          ELSE
C           there is qualsd potency factor of lateral inflow
            SLIQSP(QSFP)= PAD(SLIQSX(QSFP)+IVL1)
          END IF
C
          CALL WASHSD (DAYFG,VPFWFG(QSFP),SOSLD,LIFAC(1),SLIQSP(QSFP),
     I                 POTFWM(1,QSFP),MON,NXTMON,DAY,NDAYS,
     M                 POTFW(QSFP),
     O                 SOQS(QSFP),SOQSP(QSFP))
C
          SUROQS= SOQS(QSFP)
C
        ELSE
          SUROQS= 0.0
C
        END IF
C
C       simulate by association with overland flow
        IF (QSOFG(N).NE.0) THEN
C         constituent n is simulated by association with overland
C         flow
C         the value of qofp refers to the set of overland flow
C         associated parameters to use
          QOFP= QSOFP(N)
C
C         get input time series
          IF (IWATFG.EQ.0) THEN
C           read time series from inpad
CTHJ            SURO= PAD(SOFP+IVL1)
            REQFG= 5
            MSECNM= 'IWATER'
            OPFGNM= 'QSOFG '
            TSNAM= 'SURO  '
            CALL HREQTS (SOFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,
     I                   LSNO,TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,
     I                   QSOFG(N),
     O                   SURO)
          ELSE
C           suro is available from iwater
          END IF
CTHJ          PREC= PAD(PRECFP+IVL1)
          REQFG= 2
          TSNAM= 'PREC  '
          CALL HREQTS (PRECFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,
     I                 LSNO,TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,FLGVAL,
     O                 PREC)
C
          IF (SLIQOX(QOFP) .GE. 1) THEN
C           lateral inflow of qualof
            SLIQO(QOFP)= PAD(SLIQOX(QOFP)+IVL1)
          ELSE
C           no lateral inflow
            SLIQO(QOFP)= 0.0
          END IF
C
C         initialize atmospheric deposition
          ATDPFX= 0.0
          ATDPCN= 0.0
          J= (N-1)*2+ 1
          IF (IQADFG(J) .LE. -1) THEN
C           flux time series
CTHJ            ATDPFX= PAD(IQAFFP(N)+IVL1)
            REQFG= 4
            OPFGNM= 'IQADFG'
            TSNAM= 'IQADFX'
            TSSUB(1)= N
            CALL HREQTS (IQAFFP(N),IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,
     I                   LSNO,TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,
     I                   IQADFG(J),
     O                   ATDPFX)
          END IF
          IF (IQADFG(J+1) .LE. -1) THEN
C           conc time series
CTHJ            ATDPCN= PAD(IQACFP(N)+IVL1)
            REQFG= 4
            OPFGNM= 'IQADFG'
            TSNAM= 'IQADCN'
            TSSUB(1)= N
            CALL HREQTS (IQACFP(N),IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,
     I                   LSNO,TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,
     I                   IQADFG(J+1),
     O                   ATDPCN)
          END IF
          TSSUB(1)= 1
C
          IF (QSOFG(N) .GE. 1) THEN
C           standard qualof simulation
            CALL WASHOF (DAYFG,VQOFG(QOFP),ACQOPM(1,QOFP),
     I                   REMQOM(1,QOFP),SURO,WSFAC(QOFP),MON,NXTMON,DAY,
     I                   NDAYS,PREC,IQADFG(J),IQADFG(J+1),IQAFXM(1,N),
     I                   IQACNM(1,N),ATDPFX,ATDPCN,SLIQO(QOFP),DELT60,
     I                   QSOFG(N),
     M                   ACQOP(QOFP),REMQOP(QOFP),SQO(QOFP),
     O                   SOQO(QOFP),SOQOC(QOFP),IQADDR(N),IQADWT(N),
     O                   IQADEP(N))
C
          ELSE IF (QSOFG(N) .EQ. -1) THEN
C           special case for ches bay - constant conc of qualof
C           input value of acqop = mg/l and soqo = lb/ac
C           note - this assumes that qty = lb
C           note - acqop is converted to (lb/ac/in) in the run interpeter
C           the computed concs (soqoc and soqc) are reported in qty/ft3
C           the internal units are lb/ac/in and external units are lb/ft3
C           the storage (sqo) is reported as zero
            SOQO(QOFP)  = SURO * ACQOP(QOFP)
            SOQOC(QOFP) = ACQOP(QOFP)
            SQO(QOFP)   = 0.0
          END IF
C
          SUROQO= SOQO(QOFP)
C
        ELSE
          SUROQO= 0.0
C
        END IF
C
C       sum outflows of constituent n from the land surface
        SOQUAL(N) = SUROQS+SUROQO
C
C       compute the concentration - units are qty/acre-in.
        IF (IWATFG.EQ.0) THEN
C         read time series from inpad
CTHJ          SURO= PAD(SOFP+IVL1)
          REQFG= 3
          MSECNM= 'IWATER'
          TSNAM= 'SURO  '
          CALL HREQTS (SOFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,
     I                 LSNO,TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,FLGVAL,
     O                 SURO)
        ELSE
C         suro is available from iwater
        END IF
C
        IF (SURO.GT.0.0) THEN
          SOQC(N)= SOQUAL(N)/SURO
        ELSE
          SOQC(N)= -1.0E30
        END IF
C
 140  CONTINUE
C
      RETURN
      END
C
C     4.2(2).9.1.5
C
      SUBROUTINE   IQACC
     I                   (FRMROW,TOROW)
C
C     + + + PURPOSE + + +
C     Accumulate fluxes for section iqual
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    FRMROW,TOROW
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FRMROW - row containing incremental flux accumulation
C     TOROW  - flux row to be incremented
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION IQUAL2 + + +
      INCLUDE    'ciliq.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES +
      INTEGER    N
C
C     + + + END SPECIFICATIONS + + +
C
      DO 10 N= 1,MXQUAL
        IQIF(N,TOROW)=  IQIF(N,TOROW)+  IQIF(N,FRMROW)
        IQCF1(N,TOROW)= IQCF1(N,TOROW)+ IQCF1(N,FRMROW)
        IQCF2(N,TOROW)= IQCF2(N,TOROW)+ IQCF2(N,FRMROW)
 10   CONTINUE
C
      DO 20 N= 1,MXQUAL
        IQCF3(N,TOROW)= IQCF3(N,TOROW)+ IQCF3(N,FRMROW)
        IQCF4(N,TOROW)= IQCF4(N,TOROW)+ IQCF4(N,FRMROW)
 20   CONTINUE
C
      RETURN
      END
C
C     4.2(2).8.4
C
      SUBROUTINE   IQALIB
C
C     + + + PURPOSE + + +
C     Handle section iqual
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION IQUAL2 + + +
      INCLUDE 'ciliq.inc'
      INCLUDE 'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   N,QOFP,QSFP
C
C     + + + END SPECIFICATIONS + + +
C
      DO 10 N= 1,NQUAL
C       sediment associated constituents
        IF (QSDFG(N).NE.0) THEN
C         this qual is a qualsd
          QSFP= QSDFP(N)
          IF (SOQSFP(QSFP).GE.1) THEN
            PAD(SOQSFP(QSFP)+IVL1)= SOQS(QSFP)
          END IF
        END IF
C
C       surface runoff-associated quality constituents
        IF (QSOFG(N).NE.0) THEN
C         this qual is a qualof
          QOFP= QSOFP(N)
          IF (SOQOFP(QOFP).GE.1) THEN
            PAD(SOQOFP(QOFP)+IVL1)= SOQO(QOFP)
          END IF
C
          IF (SOQOCX(QOFP).GE.1) THEN
            PAD(SOQOCX(QOFP)+IVL1)= SOQOC(QOFP)
          END IF
C
          IF (IQADDX(N) .GE. 1) THEN
            PAD(IQADDX(N)+IVL1)= IQADDR(N)
          END IF
C
          IF (IQADWX(N) .GE. 1) THEN
            PAD(IQADWX(N)+IVL1)= IQADWT(N)
          END IF
C
          IF (IQADPX(N) .GE. 1) THEN
            PAD(IQADPX(N)+IVL1)= IQADEP(N)
          END IF
        END IF
C
C       output of global variables
        IF (SOQFP(N).GE.1) THEN
          PAD(SOQFP(N) +IVL1)= SOQUAL(N)
        END IF
C
        IF (SOQCFP(N).GE.1) THEN
          PAD(SOQCFP(N)+IVL1)= SOQC(N)
        END IF
 10   CONTINUE
C
      RETURN
      END
C
C     4.2(2).7.4
C
      SUBROUTINE   IQALIP
C
C     + + + PURPOSE + + +
C     Handle section iqual
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION IQUAL2 + + +
      INCLUDE 'ciliq.inc'
      INCLUDE 'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   N,QOFP,QSFP
C
C     + + + END SPECIFICATIONS + + +
C
      DO 10 N= 1,NQUAL
C
        IF (QSOFG(N).NE.0) THEN
C         this qual is a qualof
          QOFP= QSOFP(N)
          IF (SQOFP(QOFP).GE.1) THEN
            PAD(SQOFP(QOFP)+IVL1)=SQO(QOFP)
          END IF
        END IF
C
        IF (QSDFG(N).NE.0) THEN
C         this qual is a qualsd
          QSFP= QSDFP(N)
          IF (SOQSPX(QSFP).GE.1) THEN
            PAD(SOQSPX(QSFP)+IVL1)= SOQSP(QSFP)
          END IF
        END IF
C
 10   CONTINUE
C
      RETURN
      END
C
C     4.2(2).9.2.6
C
      SUBROUTINE   IQPRT
     I                   (UNITFG,LEV,PRINTU,BINU)
C
C     + + + PURPOSE + + +
C     Convert quantities from internal to external units,
C     and produce printout.
C     Note: local arrays have
C     identical sizes and structures to the corresponding arrays in
C     the osv apart from dropping the dimension lev for fluxes
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    LEV,PRINTU,UNITFG,BINU
C
C     + + + ARGUMENT DEFINITIONS + + +
C     UNITFG - output units   1-english, 2-metric
C     LEV    - current output level (2-pivl,3-day,4-mon,5-ann)
C     PRINTU - fortran unit number on which to print output
C     BINU   - fortran unit number on which to write binary output
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION IQUAL2 + + +
      INCLUDE    'ciliq.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I0,I1,I,J,N,QOFP,QSFP,CONCFG,FLUXFG,ACNT,
     $           CLEN(13*NQUAL),EXDAT(5)
      REAL       FACTA,CFACTA,PFACTA,PLSTAT,PSTAT1(4),PCFLX1,PCFLX2,
     $           PSOQAL,PCFLX3,PCFLX4,PADTOT,PLIFLX,PIFLX,UNDEF,
     $           APRINT(13*NQUAL)
      CHARACTER*8   CSTAT1(4)
      CHARACTER*12  QUALNAME(NQUAL)
      CHARACTER*256 CHEAD(13*NQUAL)
C
C     + + + EXTERNALS + + +
      EXTERNAL ZIPR,EXDATE
C
C     + + + DATA INITIALIZATIONS + + +
      DATA UNDEF/-1.0E30/
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' *** IQUAL ***')
 2010 FORMAT (/,'   STATE VARIABLES',13X,'   STORAGE  LAT CONC',
     $          '<---OUTFLOW CONCENTRATIONS--->')
 2020 FORMAT (  31X,'FLOW ASSOC ON SOLIDS ON SOLIDS   IN FLOW',
     $          '     TOTAL')
 2030 FORMAT (  31X,'       SQO    SLIQSP     SOQSP     SOQOC',
     $          '      SOQC')
 2040 FORMAT (  31X,'  (QTY/AC) (QTY/TON) (QTY/TON) (QTY/FT3)',
     $          ' (QTY/FT3)')
 2050 FORMAT (  31X,'  (QTY/HA) (QTY/TNE) (QTY/TNE)   (QTY/L)',
     $          '   (QTY/L)')
 2060 FORMAT (/,'   STATE VARIABLES',13X,'   STORAGE<---OUTFLO',
     $          'W CONCENTRATIONS--->')
 2070 FORMAT (  31X,'FLOW ASSOC ON SOLIDS   IN FLOW     TOTAL')
 2080 FORMAT (  31X,'       SQO     SOQSP     SOQOC      SOQC')
 2090 FORMAT (  31X,'  (QTY/AC) (QTY/TON) (QTY/FT3) (QTY/FT3)')
 2100 FORMAT (  31X,'  (QTY/HA) (QTY/TNE)   (QTY/L)   (QTY/L)')
 2110 FORMAT (  3X,3A4,' (',A4,')',9X,1PG10.3,4G10.3)
 2115 FORMAT (3A4)
 2120 FORMAT (/,'   FLUXES',22X,'<---ATMOSPHERIC DEPOSITION--->',
     $          '   LATERAL          <----------OUTFLOWS---------->')
 2130 FORMAT (  31X,'       DRY       WET     TOTAL    INFLOW',
     $          '     TOTAL  SEDIMENT      FLOW     TOTAL')
 2140 FORMAT (  31X,'    IQADDR    IQADWT     ATDEP     SLIQO',
     $          '    INFLOW      SOQS      SOQO    SOQUAL')
 2150 FORMAT (/,'   OUTFLOW FLUXES')
 2160 FORMAT (  31X,'  SEDIMENT      FLOW     TOTAL')
 2170 FORMAT (  31X,'      SOQS      SOQO    SOQUAL')
 2180 FORMAT (  3X,3A4,' (',A4,'/AC)',6X,1PG10.3,7G10.3)
 2190 FORMAT (  3X,3A4,' (',A4,'/HA)',6X,1PG10.3,7G10.3)
C
C     + + + END SPECIFICATIONS + + +
C
      I0= 0
      I1= 1
      ACNT = 0
C
      CONCFG= 0
      FLUXFG= 0
      DO 10 N=1, NQUAL
        WRITE (QUALNAME(N),2115) (QUALID(J,N),J=1,3)
        QUALNAME = ADJUSTL(QUALNAME)
        IF (QSDFG(N).NE.0) THEN
C         check qualsd
          QSFP= QSDFP(N)
          IF (SLIQSX(QSFP) .GE. 1) THEN
C           lateral inflow of qualsd
            CONCFG= 1
          END IF
        END IF
        IF (QSOFG(N).NE.0) THEN
C         check qualof
          QOFP= QSOFP(N)
          IF (SLIQOX(QOFP) .GE. 1) THEN
C           lateral inflow of qualof
            FLUXFG= 1
          END IF
          J= (N-1)*2+ 1
          IF ( (IQADFG(J) .NE. 0) .OR. (IQADFG(J+1) .NE. 0) ) THEN
C           atmospheric deposition
            FLUXFG= 1
          END IF
        END IF
 10   CONTINUE
C
C     assign values to parameter used for conversion from
C     internal to external units
      IF (UNITFG.EQ.1) THEN
C       english system
        FACTA= 1.0
        CFACTA= 2.7548E-04
        PFACTA= 1.0
      ELSE
C       metric system
        FACTA= 2.471
        CFACTA= 9.7275E-06
        PFACTA= 1.1025
      END IF
C
C     compute and write results
      IF (PRINTU .GT. 0 .AND. PFLAG(6) .LE. LEV) THEN
        WRITE (PRINTU,2000)
      END IF
C
C     state variables
C
      IF (CONCFG .EQ. 1) THEN
        IF (PRINTU .GT. 0 .AND. PFLAG(6) .LE. LEV) THEN
C         write header
C         need to write inflow sed conc plus storage and outflow concs
          WRITE (PRINTU,2010)
          WRITE (PRINTU,2020)
          WRITE (PRINTU,2030)
          IF (UNITFG.EQ.1) THEN
C           english system
            WRITE (PRINTU,2040)
          ELSE
C           metric system
            WRITE (PRINTU,2050)
          END IF
        END IF
C       initialize array counter for binary printout, store variable
C       names in local strings for use in building binary headers
        CSTAT1(1) = 'SLIQSP'
        CSTAT1(2) = 'SOQSP'
        CSTAT1(3) = 'SOQOC'
        CSTAT1(4) = 'SOQC'
      ELSE
        IF (PRINTU .GT. 0 .AND. PFLAG(6) .LE. LEV) THEN
C         just need to write storage and outflow concs
          WRITE (PRINTU,2060)
          WRITE (PRINTU,2070)
          WRITE (PRINTU,2080)
          IF (UNITFG.EQ.1) THEN
C           english system
            WRITE (PRINTU,2090)
          ELSE
C           metric system
            WRITE (PRINTU,2100)
          END IF
        END IF
C       initialize array counter for binary printout, store variable
C       names in local strings for use in building binary headers
        CSTAT1(1) = 'SQO'
        CSTAT1(2) = 'SOQSP'
        CSTAT1(3) = 'SOQOC'
        CSTAT1(4) = 'SOQC'
      END IF
      DO 30 N= 1, NQUAL
C
C       all start undefined
        J= 4
        CALL ZIPR (J,UNDEF,
     O             PSTAT1)
C
        IF (QSDFG(N).NE.0) THEN
C         qual is qualsd
          QSFP= QSDFP(N)
          IF (CONCFG .EQ. 1) THEN
C           compute lateral inflow concentration
            IF (SLIQSP(QSFP) .GE. 0.0) THEN
C             lateral inflow potency factor needs conversion
              PLSTAT= SLIQSP(QSFP)*PFACTA
            ELSE
C             no potency factor
              PLSTAT= UNDEF
            END IF
          ELSE
C           no potency factor
            PLSTAT= UNDEF
          END IF
          IF (SOQSP(QSFP) .GE. 0.0) THEN
C           outflow potency factor needs conversion
            PSTAT1(2)= SOQSP(QSFP)*PFACTA
          END IF
        ELSE
C         not a qualsd
          PLSTAT= UNDEF
          PSTAT1(2)= UNDEF
        END IF
C
        IF (QSOFG(N).NE.0) THEN
C         constituent is simulated by association with
C         overland flow
C         storage
          QOFP= QSOFP(N)
          PSTAT1(1)= SQO(QOFP)*FACTA
          IF (SOQOC(QOFP) .GE. 0.0) THEN
C           outflow concentration
            PSTAT1(3)= SOQOC(QOFP)*CFACTA
          END IF
        ELSE
C         not a qualof
          PSTAT1(1)= 0.0
          PSTAT1(3)= UNDEF
        END IF
C
        IF (SOQC(N) .GE. 0.0) THEN
C         total outflow concentration
          PSTAT1(4)= SOQC(N)*CFACTA
        ELSE
C         concentration is undefined
          PSTAT1(4)= UNDEF
        END IF
C
C       write results
        IF (CONCFG .EQ. 1) THEN
C         lateral inflow concentration
          IF (PRINTU .GT. 0 .AND. PFLAG(6) .LE. LEV) THEN
            WRITE (PRINTU,2110) (QUALID(J,N),J=1,3),QTYID(1,N),PLSTAT,
     $                          PSTAT1
          END IF
          IF (BINU .GT. 0 .AND. ABS(BFLAG(6)) .LE. LEV) THEN
C           compile values for direct access printout
            ACNT = ACNT + 1
            APRINT(ACNT) = PLSTAT
            CHEAD(ACNT) = 'SQO-'
            CHEAD(ACNT) = TRIM(CHEAD(ACNT)) // QUALNAME(N)
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          END IF
        ELSE
C         no lateral inflow concentration
          IF (PRINTU .GT. 0 .AND. PFLAG(6) .LE. LEV) THEN
            WRITE (PRINTU,2110) (QUALID(J,N),J=1,3),QTYID(1,N),PSTAT1
          END IF
        END IF
        IF (BINU .GT. 0 .AND. ABS(BFLAG(6)) .LE. LEV) THEN
          DO 20 I = 1, 4
            ACNT = ACNT + 1
            APRINT(ACNT) = PSTAT1(I)
            CHEAD(ACNT) = CSTAT1(I)
            CHEAD(ACNT) = TRIM(CHEAD(ACNT)) // '-'
            CHEAD(ACNT) = TRIM(CHEAD(ACNT)) // QUALNAME(N)
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 20       CONTINUE
        END IF
 30   CONTINUE
C
C     write fluxes
C
C     write header
      IF (PRINTU .GT. 0 .AND. PFLAG(6) .LE. LEV) THEN
        IF (FLUXFG .EQ. 1) THEN
C         need to write inflows plus outflows
          WRITE (PRINTU,2120)
          WRITE (PRINTU,2130)
          WRITE (PRINTU,2140)
        ELSE
C         just need to write outflows
          WRITE (PRINTU,2150)
          WRITE (PRINTU,2160)
          WRITE (PRINTU,2170)
        END IF
      END IF
C
      DO 40 N= 1, NQUAL
        IF (QSDFG(N).NE.0) THEN
C         qual is qualsd
          PCFLX1= IQCF1(QSDFP(N),LEV)*FACTA
        ELSE
C         not qualsd
          PCFLX1= 0.0
        END IF
C
        IF (QSOFG(N).NE.0) THEN
C         constituent is simulated by association with
C         overland flow
          QOFP= QSOFP(N)
C
          IF (FLUXFG .EQ. 1) THEN
C           there is atmospheric deposition and/or lateral inflow to output
C
C           atdep for this qual
            PCFLX3= IQCF3(N,LEV)*FACTA
            PCFLX4= IQCF4(N,LEV)*FACTA
            PADTOT= PCFLX3+ PCFLX4
C
            PLIFLX= IQIF(QOFP,LEV)*FACTA
            PIFLX= PADTOT+ PLIFLX
          END IF
C
C         flux
          PCFLX2= IQCF2(QOFP,LEV)*FACTA
        ELSE
C         not qualof
          PCFLX2= 0.0
          PCFLX3= 0.0
          PCFLX4= 0.0
          PADTOT= 0.0
          PLIFLX= 0.0
          PIFLX= 0.0
        END IF
C
C       sum outflows of constituent from the land surface
        PSOQAL= PCFLX1+ PCFLX2
C
        IF (PRINTU .GT. 0 .AND. PFLAG(6) .LE. LEV) THEN
          IF (UNITFG.EQ.1) THEN
C           english system
            IF (FLUXFG .EQ. 1) THEN
C             inflows and outflows
              WRITE (PRINTU,2180) (QUALID(J,N),J=1,3),QTYID(1,N),
     $          PCFLX3,PCFLX4,PADTOT,PLIFLX,PIFLX,PCFLX1,PCFLX2,PSOQAL
            ELSE
              WRITE (PRINTU,2180)  (QUALID(J,N),J=1,3),QTYID(1,N),
     $          PCFLX1,PCFLX2,PSOQAL
            END IF
          ELSE
C           metric system
            IF (FLUXFG .EQ. 1) THEN
C             inflows and outflows
              WRITE (PRINTU,2190) (QUALID(J,N),J=1,3),QTYID(1,N),
     $          PCFLX3,PCFLX4,PADTOT,PLIFLX,PIFLX,PCFLX1,PCFLX2,PSOQAL
            ELSE
              WRITE (PRINTU,2190)  (QUALID(J,N),J=1,3),QTYID(1,N),
     $          PCFLX1,PCFLX2,PSOQAL
            END IF
          END IF
        END IF
        IF (BINU .GT. 0 .AND. ABS(BFLAG(6)) .LE. LEV) THEN
          IF (FLUXFG .EQ. 1) THEN
            ACNT = ACNT + 1
            APRINT(ACNT) = PCFLX3
            CHEAD(ACNT) = 'IQADDR-'
            CHEAD(ACNT) = TRIM(CHEAD(ACNT)) // QUALNAME(N)
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
            ACNT = ACNT + 1
            APRINT(ACNT) = PCFLX4
            CHEAD(ACNT) = 'IQADWT-'
            CHEAD(ACNT) = TRIM(CHEAD(ACNT)) // QUALNAME(N)
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
            ACNT = ACNT + 1
            APRINT(ACNT) = PADTOT
            CHEAD(ACNT) = 'ATDEP-'
            CHEAD(ACNT) = TRIM(CHEAD(ACNT)) // QUALNAME(N)
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
            ACNT = ACNT + 1
            APRINT(ACNT) = PLIFLX
            CHEAD(ACNT) = 'SLIQO-'
            CHEAD(ACNT) = TRIM(CHEAD(ACNT)) // QUALNAME(N)
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
            ACNT = ACNT + 1
            APRINT(ACNT) = PIFLX
            CHEAD(ACNT) = 'INFLOW-'
            CHEAD(ACNT) = TRIM(CHEAD(ACNT)) // QUALNAME(N)
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
            ACNT = ACNT + 1
            APRINT(ACNT) = PCFLX1
            CHEAD(ACNT) = 'SOQS-'
            CHEAD(ACNT) = TRIM(CHEAD(ACNT)) // QUALNAME(N)
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
            ACNT = ACNT + 1
            APRINT(ACNT) = PCFLX2
            CHEAD(ACNT) = 'SOQO-'
            CHEAD(ACNT) = TRIM(CHEAD(ACNT)) // QUALNAME(N)
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
            ACNT = ACNT + 1
            APRINT(ACNT) = PSOQAL
            CHEAD(ACNT) = 'SOQUAL-'
            CHEAD(ACNT) = TRIM(CHEAD(ACNT)) // QUALNAME(N)
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          ELSE
            ACNT = ACNT + 1
            APRINT(ACNT) = PCFLX1
            CHEAD(ACNT) = 'SOQS-'
            CHEAD(ACNT) = TRIM(CHEAD(ACNT)) // QUALNAME(N)
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
            ACNT = ACNT + 1
            APRINT(ACNT) = PCFLX2
            CHEAD(ACNT) = 'SOQO-'
            CHEAD(ACNT) = TRIM(CHEAD(ACNT)) // QUALNAME(N)
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
            ACNT = ACNT + 1
            APRINT(ACNT) = PSOQAL
            CHEAD(ACNT) = 'SOQUAL-'
            CHEAD(ACNT) = TRIM(CHEAD(ACNT)) // QUALNAME(N)
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          END IF
        END IF
 40   CONTINUE
C
      IF (BINU .GT. 0 .AND. ABS(BFLAG(6)) .LE. LEV) THEN
C       write binary output
        CALL EXDATE(
     I              DATIM,
     O              EXDAT)
        IF (BFLAG(6) .GT. 0) THEN
C         at start of run, write the header
          WRITE (BINU) I0,'IMPLND  ',LSNO,'IQUAL   ',
     1          (CLEN(I),(CHEAD(I)(J:J),J=1,CLEN(I)),I=1,ACNT)
C         set bflag to negative to not write headers anymore
          BFLAG(6) = -BFLAG(6)
        END IF
        WRITE (BINU) I1,'IMPLND  ',LSNO,'IQUAL   ',UNITFG,
     1               LEV,(EXDAT(I),I=1,5),(APRINT(I),I=1,ACNT)
      END IF
C
      RETURN
      END
C
C     4.2(2).9.3.5
C
      SUBROUTINE   IQRST
     I                   (LEV)
C
C     + + + PURPOSE + + +
C     Reset all flux accumulators in section iqual
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    LEV
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEV    - current output level (2-pivl,3-day,4-mon,5-ann)
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION IQUAL2 + + +
      INCLUDE    'ciliq.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    N
C
C     + + + END SPECIFICATIONS + + +
C
      DO 10 N= 1,MXQUAL
        IQIF(N,LEV)=  0.0
        IQCF1(N,LEV)= 0.0
        IQCF2(N,LEV)= 0.0
 10   CONTINUE
C
      DO 20 N= 1,MXQUAL
        IQCF3(N,LEV)= 0.0
        IQCF4(N,LEV)= 0.0
 20   CONTINUE
C
      RETURN
      END
C
C     4.2(2).6.2
C
      SUBROUTINE   WASHOF
     I                    (DAYFG,VQOFG,ACQOPM,REMQOM,SURO,WSFAC,MON,
     I                     NXTMON,DAY,NDAYS,PREC,ADFXFG,ADCNFG,ADFXMN,
     I                     ADCNMN,ADFLX,ADCNC,SLIQO,DELT60,QSOFG,
     M                     ACQOP,REMQOP,SQO,
     O                     SOQO,SOQOC,ADFXFX,ADCNFX,ADTOT)
C
C     + + + PURPOSE + + +
C     Simulate accumulation of a quality constituent on the land
C     surface and its removal using a constant unit rate and by direct
C     washoff by overland flow
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    DAYFG,VQOFG,MON,NXTMON,DAY,NDAYS,ADFXFG,ADCNFG,QSOFG
      REAL       ACQOPM(12),REMQOM(12),SURO,WSFAC,PREC,ADFXMN(12),
     $           ADCNMN(12),ADFLX,ADCNC,SLIQO,ACQOP,REMQOP,SQO,SOQO,
     $           SOQOC,ADFXFX,ADCNFX,ADTOT,DELT60
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DAYFG  - flag for first day or day change
C     VQOFG  - ???
C     ACQOPM - ???
C     REMQOM - ???
C     SURO   - surface output
C     WSFAC  - ???
C     MON    - calendar month
C     NXTMON - next calendar month
C     DAY    - day of month
C     NDAYS  - no. of days in this month
C     PREC   - precipitation during current interval in inches
C     ADFXFG - flag indicating source of dry deposition flux
C              positive means monthly, -1 means time series,
C              0 means none
C     ADCNFG - flag indicating source of wet deposition flux
C              postive means monthly, -1 means time series,
C              0 means none
C     ADFXMN - monthly dry atmospheric deposition in qty/ac
C     ADCNMN - monthly wet atmospheric deposition in qty/ac.inch
C     ADFLX  - current dry atmospheric deposition in qty/ac
C     ADCNC  - current wet atmospheric deposition in qty/ac.inch
C     SLIQO  - lateral inflow of qualof
C     ACQOP  - ???
C     REMQOP - ???
C     SQO    - ???
C     SOQO   - ???
C     SOQOC  - ???
C     ADFXFX - actual dry atmospheric deposition flux for current interval
C     ADCNFX - actual wet atmospheric deposition flux for current interval
C     ADTOT  - actual total atmospheric deposition flux for current interval
C
C     + + + LOCAL VARIABLES + + +
      REAL       DUMMY
C
C     + + + FUNCTIONS + + +
      REAL       DAYVAL
C
C     + + + EXTERNALS + + +
      EXTERNAL   DAYVAL
C
C     + + + INTRINSICS + + +
      INTRINSIC  EXP
C
C     + + + END SPECIFICATIONS + + +
C
      IF (DAYFG.EQ.1) THEN
C       it is the first interval of the day
C
        IF (VQOFG.EQ.1) THEN
C         accumulation rate of this quality constituent is allowed
C         to vary throughout the year
C         interpolate for the daily value
C         linearly interpolate acqop between two values from the
C         monthly array acqopm(12) for this overland flow associated
C         quality constituent (no. qofp)
          ACQOP= DAYVAL(ACQOPM(MON),ACQOPM(NXTMON),DAY,NDAYS)
C         removal unit rate of this quality constituent is allowed
C         to vary throughout the year
C         interpolate for the daily value
C         linearly interpolate remqop between two values from the
C         monthly array remqom(12) for this overland flow associated
C         quality constituent (no. qofp)
          REMQOP= DAYVAL(REMQOM(MON),REMQOM(NXTMON),DAY,NDAYS)
C
        END IF
C
        IF (QSOFG .EQ. 1) THEN
C         update storage due to accumulation and removal which occurs
C         independent of runoff - units are qty/acre
          SQO= ACQOP+ SQO*(1.0-REMQOP)
        END IF
      END IF
C
C     handle atmospheric deposition
C     dry deposition
      IF (ADFXFG .LE. -1) THEN
C       dry flux has been input as a time series
        ADFXFX= ADFLX
      ELSE IF (ADFXFG .GE. 1) THEN
C       dry flux is taken from monthly values
        ADFXFX= DAYVAL(ADFXMN(MON),ADFXMN(NXTMON),DAY,NDAYS)
      ELSE
        ADFXFX= 0.0
      END IF
C     wet deposition
      IF (ADCNFG .LE. -1) THEN
C       wet deposition concentration has been input as a time series
        ADCNFX= PREC*ADCNC
      ELSE IF (ADCNFG .GE. 1) THEN
C       wet deposition concentration is taken from monthly values
        ADCNFX= PREC*DAYVAL(ADCNMN(MON),ADCNMN(NXTMON),DAY,NDAYS)
      ELSE
        ADCNFX= 0.0
      END IF
      ADTOT= ADFXFX+ ADCNFX
C
      IF (QSOFG .EQ. 2) THEN
C       update storage due to accumulation and removal which occurs
C       independent of runoff - units are qty/acre
        DUMMY= REMQOP+ (ADTOT+SLIQO)/(ACQOP/REMQOP)
        IF (DUMMY .GT. 1.0) DUMMY=1.0
        SQO= ACQOP*(DELT60/24.0)+ SQO*(1.0-DUMMY)**(DELT60/24.0)
      END IF
C
C     update storage
      SQO= SQO+ SLIQO+ ADTOT
C
C     simulate washoff by overland flow - units are qty/acre-ivl
      IF (SURO.GT.0.0) THEN
C       there is overland flow
        IF (SQO.GT.0.0) THEN
C         there is some quality constituent (no. qofp) in storage;
C         washoff can occur
          DUMMY= 1.0-EXP(-SURO*WSFAC)
          SOQO = SQO*DUMMY
C
C         update storage of constituent - units are in qty/acre
          SQO=SQO- SOQO
        ELSE
C         no washoff load
          SOQO= 0.0
        END IF
C
      ELSE
        SOQO= 0.0
      END IF
C
C     compute and output concentration - units are qty/acre-in.
      IF (SURO.GT.0.0) THEN
C       define concentration
        SOQOC= SOQO/SURO
      ELSE
C       soqoc is undefined
        SOQOC= -1.0E30
      END IF
C
      RETURN
      END
C
C     4.2(2).6.1
C
      SUBROUTINE   WASHSD
     I                    (DAYFG,VPFWFG,SOSLD,LIFAC,SLIQSP,POTFWM,MON,
     I                     NXTMON,DAY,NDAYS,
     M                     POTFW,
     O                     SOQS,SOQSP)
C
C     + + + PURPOSE + + +
C     Simulate washoff of a quality constituent from the land surface
C     by association with solids
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    DAY,DAYFG,MON,NDAYS,NXTMON,VPFWFG
      REAL       POTFW,POTFWM(12),SOQS,SOSLD,LIFAC,SLIQSP,SOQSP
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DAYFG  - flag for first day or day change
C     VPFWFG - ???
C     SOSLD  - ???
C     LIFAC  - weighting factor for lateral inflow potency factor
C     SLIQSP - potency factor on lateral inflow of sediment
C     POTFWM - ???
C     MON    - calendar month
C     NXTMON - next calendar month
C     DAY    - day of month
C     NDAYS  - no. of days in this month
C     POTFW  - ???
C     SOQS   - ???
C     SOQSP  - potency factor on outflow of sediment
C
C     + + + FUNCTIONS + + +
      REAL       DAYVAL
C
C     + + + EXTERNALS + + +
      EXTERNAL   DAYVAL
C
C     + + + INTRINSICS + + +
      INTRINSIC  ABS
C
C     + + + END SPECIFICATIONS + + +
C
      IF (DAYFG.EQ.1) THEN
C       it is the first interval of the day
C
        IF (VPFWFG.EQ.1) THEN
C         washoff potency factors are allowed to vary throughout the
C         year
C         interpolate for the daily value
C         linearly interpolate potfw between two values from the
C         monthly array potfwm(12) for this solids associated quality
C         constituent (no. qsfp)
          POTFW= DAYVAL(POTFWM(MON),POTFWM(NXTMON),DAY,NDAYS)
        ELSE
C         washoff potency factors do not vary throughout the year.
C         potfw value has been supplied by the run interpreter
        END IF
C
      END IF
C
C     associate with washoff of solids - units are qty/acre-ivl
C
C     if (sosld.eq.0.0) then
      IF ((ABS(SOSLD)).LE.0.0) THEN
        SOQS= 0.0
      ELSE
        IF (SLIQSP .GE. 0.0) THEN
C         lateral inflow has an effect on washoff potency factor
          SOQSP= SLIQSP*LIFAC+ POTFW*(1.0- LIFAC)
          SOQS= SOSLD*SOQSP
        ELSE
C         no effect of lateral inflow
          SOQSP= POTFW
          SOQS= SOSLD*POTFW
        END IF
      END IF
C
      RETURN
      END
