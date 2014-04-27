C
C
C
      SUBROUTINE   PPQUAL
C
C     + + + PURPOSE + + +
C     Process input for section pqual
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PQUAL1 + + +
      INCLUDE    'cplpq.inc'
      INCLUDE    'crin2.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I1,I2,I4,IVAL(16),J,N,I12,SGRP,SCLU,RETCOD,I
      REAL         RVAL(12),SQOLIM,R0,CVT
      CHARACTER*12 CHSTR
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (CHSTR,CHSTR1)
      CHARACTER*1  CHSTR1(12)
C
C     + + + EXTERNALS + + +
      EXTERNAL   ITABLE,RTABLE,OMSG,OMSTC
      EXTERNAL   ZIPR,MDATBL
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' PROCESSING INPUT FOR SECTION PQUAL')
 2040 FORMAT (/,' FINISHED PROCESSING INPUT FOR SECTION PQUAL')
 2070 FORMAT (3A4)
C
C     + + + END SPECIFICATIONS + + +
C
      IF (OUTLEV.GT.1) THEN
C       processing message
        WRITE (MESSU,2000)
      END IF
C
      R0  = 0.0
      I1  = 1
      I12 = 12
C
      IF (UUNITS .EQ. 1) THEN
C       english units - conversion from mg/l to lb/ft3
        CVT= 6.238E-5
      ELSE
C       metric units - conversion from mg/l to kg/l
        CVT= 1.0E-6
      END IF
C
C     error/warn message cluster
      SCLU= 307
C
C     initialize month-data input
      I= 120
      CALL ZIPR (I,R0,
     O           PQAFXM)
      CALL ZIPR (I,R0,
     O           PQACNM)
C
C     initialize atmospheric deposition fluxes
      I= 50
      CALL ZIPR (I,R0,
     O           PQCF6)
      CALL ZIPR (I,R0,
     O           PQCF7)
C
      IF (PWGFG .EQ. 0) THEN
C       process values in table-type lat-factor
        I2= 60
        I4= 4
        CALL RTABLE (I2,I1,I4,UUNITS,
     M               LIFAC)
      END IF
C
C     find number of quality constituents - table-type nquals
      I2= 67
      I4= 1
      CALL ITABLE (I2,I1,I4,UUNITS,
     M             IVAL)
      NQUAL= IVAL(1)
C
C     get atmospheric deposition flags - table-type pql-ad-flags
      I2= 68
      I4= MXQAL2
      CALL ITABLE (I2,I1,I4,UUNITS,
     M             PQADFG)
C
C     read in month-data tables where necessary
      DO 50 J= 1, NQUAL
        N= 2*(J- 1)+ 1
        IF (PQADFG(N) .GT. 0) THEN
C         monthly flux must be read
          CALL MDATBL
     I                (PQADFG(N),
     O                 PQAFXM(1,J),RETCOD)
C         convert units to internal - not done by MDATBL
          IF (UUNITS .EQ. 1) THEN
C           convert from qty/ac.day to qty/ac.ivl
            DO 10 I= 1, 12
              PQAFXM(I,J)= PQAFXM(I,J)*DELT60/24.0
 10         CONTINUE
          ELSE IF (UUNITS .EQ. 2) THEN
C           convert from qty/ha.day to qty/ac.ivl (not qty/ha.ivl)
            DO 20 I= 1, 12
              PQAFXM(I,J)= PQAFXM(I,J)*0.4047*DELT60/24.0
 20         CONTINUE
          END IF
        END IF
        IF (PQADFG(N+1) .GT. 0) THEN
C         monthly ppn conc must be read
          CALL MDATBL
     I                (PQADFG(N+1),
     O                 PQACNM(1,J),RETCOD)
C         convert units to internal - not done by MDATBL
          IF (UUNITS .EQ. 1) THEN
C           convert from qty/ft3 to qty/ac.in
            DO 30 I= 1, 12
              PQACNM(I,J)= PQACNM(I,J)*3630.0
 30         CONTINUE
          ELSE IF (UUNITS .EQ. 2) THEN
C           convert from qty/L to qty/ac.in (not qty/ha.in)
            DO 40 I= 1, 12
              PQACNM(I,J)= PQACNM(I,J)*102833.0
 40         CONTINUE
          END IF
        END IF
 50   CONTINUE
C
C     initialize counters
      NQSD= 0
      NQOF= 0
      NQIF= 0
      NQGW= 0
C     initialize flux arrays
      DO 60 J= 1,MXQUAL
        WASHQS(J)= 0.0
        SCRQS(J) = 0.0
        SOQO(J)  = 0.0
        IOQUAL(J)= 0.0
        AOQUAL(J)= 0.0
 60   CONTINUE
C
      DO 120 N= 1,NQUAL
C       get id's and flags - table-type qual-props
        I2= 69
        I4= 13
        CALL ITABLE (I2,N,I4,UUNITS,
     M               IVAL)
C
        DO 80 J= 1,3
          QUALID(J,N)= IVAL(J)
 80     CONTINUE
C
        QTYID(1,N)= IVAL(4)
C
        QSDFG(N)= IVAL(5)
        IF (QSDFG(N) .NE. 0) THEN
C         this is a qualsd
          NQSD= NQSD+ 1
C
          IF (NQSD.GT.MXQUAL) THEN
C           error - too many sediment associated quality constituents
            CALL OMSTI (MXQUAL)
            WRITE (CHSTR,2070) (QUALID(J,N),J=1,3)
            CALL OMSTC (I12,CHSTR1)
            SGRP= 1
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 ECOUNT)
            NQSD= MXQUAL
          END IF
C
          QSDFP(N)    = NQSD
          VPFWFG(NQSD)= IVAL(6)
          VPFSFG(NQSD)= IVAL(7)
        ELSE
C         not sed related
          QSDFP(N)= 0
        END IF
C
        QSOFG(N)= IVAL(8)
        IF (QSOFG(N).NE.0) THEN
C         this is a qualof
          NQOF= NQOF+ 1
C
          IF (NQOF.GT.MXQUAL) THEN
C           error - too many overland-flow-associated quality constituents
            CALL OMSTI (MXQUAL)
            WRITE (CHSTR,2070) (QUALID(J,N),J=1,3)
            CALL OMSTC (I12,CHSTR1)
            SGRP= 2
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 ECOUNT)
            NQOF= MXQUAL
          END IF
C
          QSOFP(N)   = NQOF
          VQOFG(NQOF)= IVAL(9)
        ELSE
C         not a qualof
          QSOFP(N)= 0
C
          J= 2*(N- 1)+ 1
          IF ( (PQADFG(J) .NE. 0) .OR. (PQADFG(J+1) .NE. 0) ) THEN
C           error - non-qualof cannot have atmospheric deposition
            WRITE (CHSTR,2070) (QUALID(I,N),I=1,3)
            CALL OMSTC (I12,CHSTR1)
            SGRP= 5
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 ECOUNT)
            PQADFG(J)= 0
            PQADFG(J+1)= 0
          END IF
        END IF
C
        QIFWFG(N)= IVAL(10)
        IF (QIFWFG(N).NE.0) THEN
C         this is a qualif
          NQIF= NQIF+ 1
C
          IF (NQIF.GT.MXQUAL) THEN
C           error - too many interflow-associated quality constituents
            WRITE (CHSTR,2070) (QUALID(J,N),J=1,3)
            CALL OMSTI (MXQUAL)
            CALL OMSTC (I12,CHSTR1)
            SGRP= 3
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 ECOUNT)
            NQIF= MXQUAL
          END IF
C
          QIFWFP(N)   = NQIF
          VIQCFG(NQIF)= IVAL(11)
        ELSE
C         not a qualif
          QIFWFP(N)= 0
        END IF
C
        QAGWFG(N)= IVAL(12)
        IF (QAGWFG(N).NE.0) THEN
C         this is a qualgw
          NQGW= NQGW+ 1
C
          IF (NQGW.GT.MXQUAL) THEN
C           error - too many groundwater-associated
C           quality constituents
            CALL OMSTI (MXQUAL)
            WRITE (CHSTR,2070) (QUALID(J,N),J=1,3)
            CALL OMSTC (I12,CHSTR1)
            SGRP= 4
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 ECOUNT)
            NQGW= MXQUAL
          END IF
C
          QAGWFP(N)   = NQGW
          VAQCFG(NQGW)= IVAL(13)
        ELSE
C         not a qualgw
          QAGWFP(N)= 0
        END IF
C
C       read in storage on surface and values for any
C       parameters which do not vary seasonally - qual-input
        I2= 70
        I4= 8
        CALL RTABLE (I2,N,I4,UUNITS,
     M               RVAL)
C
        IF (QSDFG(N).NE.0) THEN
          POTFW(NQSD)= RVAL(2)
          POTFS(NQSD)= RVAL(3)
        END IF
C
        IF (QSOFG(N).NE.0) THEN
          SQO(NQOF)  = RVAL(1)
          ACQOP(NQOF)= RVAL(4)
          SQOLIM     = RVAL(5)
C
C         compute removal rate
          REMQOP(NQOF)= ACQOP(NQOF)/SQOLIM
          WSFAC(NQOF) = 2.30/RVAL(6)
        END IF
C
        IF (QIFWFG(N).NE.0) THEN
          IOQC(NQIF)= RVAL(7)
        END IF
C
        IF (QAGWFG(N).NE.0) THEN
          AOQC(NQGW)= RVAL(8)
        END IF
C
        IF (QSDFG(N).NE.0) THEN
C         allow optional no interpolation of potency factor
C         for chesapeake bay model
          IF (VPFWFG(NQSD).GT.0) THEN
C           get monthly vals of washoff pot factor - table-type mon-potfw
            I2= 71
            I4= 12
            CALL RTABLE (I2,NQSD,I4,UUNITS,
     M                   POTFWM(1,NQSD))
          END IF
C
          IF (VPFSFG(NQSD).EQ.1) THEN
C           get monthly vals of scour pot factor - table-type mon-potfs
            I2= 72
            I4= 12
            CALL RTABLE (I2,NQSD,I4,UUNITS,
     M                   POTFSM(1,NQSD))
          END IF
        END IF
C
        IF (QSOFG(N).NE.0) THEN
C
          IF (VQOFG(NQOF).EQ.1) THEN
C           get monthly values of accum rates - table-type mon-accum
            I2= 73
            I4= 12
            CALL RTABLE (I2,NQOF,I4,UUNITS,
     M                   ACQOPM(1,NQOF))
C
C           get monthly values of limiting storage - table-type mon-sqolim
            I2= 74
            I4= 12
            CALL RTABLE (I2,NQOF,I4,UUNITS,
     M                   RVAL)
C
C           calculate monthly values of removal rate
            DO 90 J= 1,12
              REMQOM(J,NQOF)= ACQOPM(J,NQOF)/RVAL(J)
 90         CONTINUE
          END IF
        END IF
C
        IF (QIFWFG(N).NE.0) THEN
          IF (NQIF.GT.0) THEN
            IF (VIQCFG(NQIF).GT.0) THEN
C             get monthly values of concentration in
C             interflow - table-type mon-ifwconc
              I2= 75
              I4= 12
              CALL RTABLE (I2,NQIF,I4,UUNITS,
     M                     IOQCM(1,NQIF))
C
C             modification to allow units of mg/l (for chesapeake bay)
              IF (VIQCFG(NQIF).EQ.3 .OR. VIQCFG(NQIF).EQ.4) THEN
                DO 100 I4 = 1,12
                  IOQCM(I4,NQIF) = IOQCM(I4,NQIF)*CVT
 100            CONTINUE
                VIQCFG(NQIF) = VIQCFG(NQIF) - 2
              END IF
C             end chesapeake bay modification
            END IF
          END IF
        END IF
C
        IF (QAGWFG(N).NE.0 .AND. VAQCFG(NQGW).GT.0) THEN
C         get monthly values of concentration in
C         groundwater - table-type mon-grndconc
          I2= 76
          I4= 12
          CALL RTABLE (I2,NQGW,I4,UUNITS,
     M                 AOQCM(1,NQGW))
C
C         modification to allow units of mg/l (for chesapeake bay)
          IF (VAQCFG(NQGW).EQ.3.OR.VAQCFG(NQGW).EQ.4) THEN
            DO 110 I4 = 1,12
              AOQCM(I4,NQGW) = AOQCM(I4,NQGW)*CVT
 110        CONTINUE
            VAQCFG(NQGW) = VAQCFG(NQGW) - 2
          END IF
C         end chesapeake bay modification
        END IF
C       end nqual loop
 120  CONTINUE
C
      IF (OUTLEV.GT.1) THEN
C       finished processing message
        WRITE (MESSU,2040)
      END IF
C
      RETURN
      END
C
C     4.2(1).7
C
      SUBROUTINE   PQUAL
C
C     + + + PURPOSE + + +
C     Simulate quality constituents (other than sediment, heat, dox,
C     and co2) using simple relationships with sediment and water
C     yield
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PQUAL2 + + +
      INCLUDE    'cplpq.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     N,QAFP,QIFP,QOFP,QSFP,J,REQFG,TSSUB(2),FLGVAL
      REAL        SUROQO,SUROQS,ATDPFX,ATDPCN
      CHARACTER*6 OPTYP,TSNAM,SECNAM,MSECNM,OPFGNM
C
C     + + + EXTERNALS + + +
      EXTERNAL   QUALSD,QUALOF,QUALIF,QUALGW,HREQTS
C
C     + + + DATA INITIALIZATIONS + + +
      DATA TSSUB/1,1/
      DATA OPTYP,SECNAM/'PERLND','PQUAL '/
C
C     + + + END SPECIFICATIONS + + +
C
C     nqual is the number of constituents being simulated
      DO 10 N= 1,NQUAL
C       simulate constituent n
C
C       simulate by association with sediment
        IF (QSDFG(N).NE.0) THEN
C         constituent n is simulated by association with sediment
C         the value of qsfp refers to the set of sediment
C         associated parameters to use
          QSFP= QSDFP(N)
C
C         get input time series
          IF (SEDFG.EQ.0) THEN
C           get time series from inpad
CTHJ            WSSD = PAD(WSDFP+IVL1)
            REQFG= 5
            MSECNM= 'SEDMNT'
            OPFGNM= 'QSDFG '
            TSNAM= 'WSSD  '
            CALL HREQTS (WSDFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,
     I                   LSNO,TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,
     I                   QSDFG(N),
     O                   WSSD)
CTHJ            SCRSD= PAD(CSDFP+IVL1)
            TSNAM= 'SCRSD '
            CALL HREQTS (CSDFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,
     I                   LSNO,TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,
     I                   QSDFG(N),
     O                   SCRSD)
            IF (SLSDFP .GE. 1) THEN
C             sediment lateral inflow present
              SLSED= PAD(SLSDFP+IVL1)
            ELSE
C             no sediment lateral inflow
              SLSED= 0.0
            END IF
          ELSE
C           wssd,scrsd, and optionally slsed, are available from sedmnt
          END IF
C
          IF ( (SLSED .LE. 0.0) .OR. (SLIQSX(QSFP) .LT. 1) ) THEN
C           no defined potency factor of lateral inflow
            SLIQSP(QSFP)= -1.0E30
          ELSE
C           there is qualsd potency factor of lateral inflow
            SLIQSP(QSFP)= PAD(SLIQSX(QSFP)+IVL1)
          END IF
C
          CALL QUALSD (DAYFG,VPFWFG(QSFP),VPFSFG(QSFP),WSSD,SCRSD,
     I                 LIFAC(1),SLIQSP(QSFP),POTFWM(1,QSFP),
     I                 POTFSM(1,QSFP),MON,NXTMON,DAY,NDAYS,
     M                 POTFW(QSFP),POTFS(QSFP),
     O                 WASHQS(QSFP),SCRQS(QSFP),SOQS(QSFP),SOQSP(QSFP))
          SUROQS= SOQS(QSFP)
        ELSE
C         not a qualsd
          SUROQS= 0.0
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
          IF (PWATFG.EQ.0) THEN
C           get time series from inpad
CTHJ            SURO= PAD(SOFP+IVL1)
            REQFG= 5
            MSECNM= 'PWATER'
            OPFGNM= 'QSOFG '
            TSNAM= 'SURO  '
            CALL HREQTS (SOFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,
     I                   LSNO,TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,
     I                   QSOFG(N),
     O                   SURO)
          ELSE
C           suro is available from pwater
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
          IF (PQADFG(J) .LE. -1) THEN
C           flux time series
CTHJ            ATDPFX= PAD(PQAFFP(N)+IVL1)
            REQFG= 4
            OPFGNM= 'PQADFG'
            TSNAM= 'PQADFX'
            TSSUB(1)= N
            CALL HREQTS (PQAFFP(N),IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,
     I                   LSNO,TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,
     I                   PQADFG(J),
     O                   ATDPFX)
          END IF
          IF (PQADFG(J+1) .LE. -1) THEN
C           conc time series
CTHJ            ATDPCN= PAD(PQACFP(N)+IVL1)
            REQFG= 4
            OPFGNM= 'PQADFG'
            TSNAM= 'PQADCN'
            TSSUB(1)= N
            CALL HREQTS (PQACFP(N),IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,
     I                   LSNO,TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,
     I                   PQADFG(J+1),
     O                   ATDPCN)
          END IF
          TSSUB(1)= 1
C
          CALL QUALOF (DAYFG,VQOFG(QOFP),ACQOPM(1,QOFP),REMQOM(1,QOFP),
     I                 MON,NXTMON,DAY,NDAYS,SURO,WSFAC(QOFP),PREC,
     I                 PQADFG(J),PQADFG(J+1),PQAFXM(1,N),PQACNM(1,N),
     I                 ATDPFX,ATDPCN,SLIQO(QOFP),DELT60,QSOFG(N),
     M                 ACQOP(QOFP),REMQOP(QOFP),SQO(QOFP),
     O                 SOQO(QOFP),SOQOC(QOFP),PQADDR(N),
     O                 PQADWT(N),PQADEP(N),ISQO(QOFP))
C
          SUROQO= SOQO(QOFP)
        ELSE
C         not a qualof
          SUROQO= 0.0
        END IF
C
C       sum outflows of constituent n from the land surface
        SOQUAL(N) = SUROQS+ SUROQO
        POQUAL(N) = SOQUAL(N)
C
C       compute the concentration - units are qty/acre-inch
        IF (PWATFG.EQ.0) THEN
C         read time series from inpad
CTHJ          SURO= PAD(SOFP+IVL1)
          REQFG= 5
          MSECNM= 'PWATER'
          OPFGNM= 'QSDFG '
          TSNAM= 'SURO  '
          CALL HREQTS (SOFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,
     I                 LSNO,TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,QSDFG(N),
     O                 SURO)
        ELSE
C         suro is available from pwater
        END IF
C
        IF (SURO.GT.0.0) THEN
C         define concentration
          SOQC(N)= SOQUAL(N)/SURO
        ELSE
C         undefined
          SOQC(N)= -1.0E30
        END IF
C
C       simulate quality constituent in interflow
        IF (QIFWFG(N).NE.0) THEN
C         constiuent n is present in interflow
C         get input time series
          QIFP= QIFWFP(N)
          IF (PWATFG.EQ.0) THEN
CTHJ            IFWO= PAD(IOFP+IVL1)
            REQFG= 5
            MSECNM= 'PWATER'
            OPFGNM= 'QIFWFG'
            TSNAM= 'IFWO  '
            CALL HREQTS (IOFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,
     I                   LSNO,TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,
     I                   QIFWFG(N),
     O                   IFWO)
            IF (ILIFP .GE. 1) THEN
C             interflow lateral inflow present
              IFWLI= PAD(ILIFP+IVL1)
            ELSE
C             no interflow lateral inflow
              IFWLI= 0.0
            END IF
          ELSE
C           ifwo, and optionally ifwli, are available from pwater
          END IF
C
          IF ( (IFWLI .LE. 0.0) .OR. (ILIQCX(QIFP) .LT. 1) ) THEN
C           no defined concentration in lateral inflow
            ILIQC(QIFP)= -1.0E30
          ELSE
C           there is qualif concentration of lateral inflow
            ILIQC(QIFP)= PAD(ILIQCX(QIFP)+IVL1)
          END IF
C
          CALL QUALIF (DAYFG,VIQCFG(QIFP),IFWO,LIFAC(3),ILIQC(QIFP),
     I                 IOQCM(1,QIFP),MON,NXTMON,DAY,NDAYS,
     M                 IOQC(QIFP),
     O                 IOQUAL(QIFP),IOQCE(QIFP))
C
C         cumulate outflow
          POQUAL(N)= POQUAL(N)+ IOQUAL(QIFP)
        END IF
C
C       simulate quality constituent in active groundwater outflow
        IF (QAGWFG(N).NE.0) THEN
C         constituent n is present in groundwater
C         get groundwater time series input
          QAFP= QAGWFP(N)
          IF (PWATFG.EQ.0) THEN
CTHJ            AGWO= PAD(AOFP+IVL1)
            REQFG= 5
            MSECNM= 'PWATER'
            OPFGNM= 'QAGWFG'
            TSNAM= 'AGWO  '
            CALL HREQTS (AOFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,
     I                   LSNO,TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,
     I                   QAGWFG(N),
     O                   AGWO)
            IF (ALIFP .GE. 1) THEN
C             baseflow lateral inflow present
              AGWLI= PAD(ALIFP+IVL1)
            ELSE
C             no baseflow lateral inflow
              AGWLI= 0.0
            END IF
          ELSE
C           agwo, and optionally agwli, are available from pwater
          END IF
C
          IF ( (AGWLI .LE. 0.0) .OR. (ALIQCX(QAFP) .LT. 1) ) THEN
C           no defined concentration in lateral inflow
            ALIQC(QAFP)= -1.0E30
          ELSE
C           there is qualgw concentration of lateral inflow
            ALIQC(QAFP)= PAD(ALIQCX(QAFP)+IVL1)
          END IF
C
          CALL QUALGW (DAYFG,VAQCFG(QAFP),AGWO,LIFAC(4),ALIQC(QAFP),
     I                 AOQCM(1,QAFP),MON,NXTMON,DAY,NDAYS,
     M                 AOQC(QAFP),
     O                 AOQUAL(QAFP),AOQCE(QAFP))
C
C         cumulate outflow
          POQUAL(N)= POQUAL(N)+ AOQUAL(QAFP)
        END IF
C
C       compute the concentration of constituent n in the total
C       outflow
C       get water outflow time series
        IF (PWATFG.EQ.0) THEN
C         need pero as time series
CTHJ          PERO= PAD(POFP+IVL1)
          REQFG= 3
          MSECNM= 'PWATER'
          TSNAM= 'PERO  '
          CALL HREQTS (POFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,
     I                 LSNO,TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,FLGVAL,
     O                 PERO)
        ELSE
C         pero is available from pwater
        END IF
C
        IF (PERO.GT.0.0) THEN
C         define concentration
          POQC(N)= POQUAL(N)/PERO
        ELSE
C         undefined
          POQC(N)= -1.0E30
        END IF
C
 10   CONTINUE
C
      RETURN
      END
C
C     4.2(1).15.1.5
C
      SUBROUTINE   PQACC
     I                   (FRMROW,TOROW)
C
C     + + + PURPOSE + + +
C     Accumulate fluxes for section pqual
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   FRMROW,TOROW
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FRMROW - row containing incremental flux accumulation
C     TOROW  - flux row to be incremented
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PQUAL2 + + +
      INCLUDE  'cplpq.inc'
      INCLUDE  'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   N
C
C     + + + END SPECIFICATIONS + + +
C
      DO 10 N= 1,MXQUAL
        PQIF(N,TOROW)=  PQIF(N,TOROW)+  PQIF(N,FRMROW)
        PQCF1(N,TOROW)= PQCF1(N,TOROW)+ PQCF1(N,FRMROW)
        PQCF2(N,TOROW)= PQCF2(N,TOROW)+ PQCF2(N,FRMROW)
        PQCF3(N,TOROW)= PQCF3(N,TOROW)+ PQCF3(N,FRMROW)
        PQCF4(N,TOROW)= PQCF4(N,TOROW)+ PQCF4(N,FRMROW)
        PQCF5(N,TOROW)= PQCF5(N,TOROW)+ PQCF5(N,FRMROW)
 10   CONTINUE
C
      DO 20 N= 1, MXQUAL
        PQCF6(N,TOROW)= PQCF6(N,TOROW)+ PQCF6(N,FRMROW)
        PQCF7(N,TOROW)= PQCF7(N,TOROW)+ PQCF7(N,FRMROW)
 20   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   PQALPB
C
C     + + + PURPOSE + + +
C     handle section pqual
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PQUAL2 + + +
      INCLUDE    'cplpq.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    N,QAFP,QIFP,QOFP,QSFP
C
C     + + + END SPECIFICATIONS + + +
C
      DO 10 N= 1,NQUAL
C       sediment associated constituents
        IF (QSDFG(N).NE.0) THEN
C         this qual is a qualsd
          QSFP= QSDFP(N)
          IF (WSQSFP(QSFP).GE.1) THEN
            PAD(WSQSFP(QSFP)+IVL1)= WASHQS(QSFP)
          END IF
          IF (SCQSFP(QSFP).GE.1) THEN
            PAD(SCQSFP(QSFP)+IVL1)= SCRQS(QSFP)
          END IF
          IF (SOQSFP(QSFP).GE.1) THEN
            PAD(SOQSFP(QSFP)+IVL1)= SOQS(QSFP)
          END IF
C
        END IF
C
C       surface runoff-associated quality constituents
        IF (QSOFG(N).NE.0) THEN
C         this qual is a qualof
          QOFP= QSOFP(N)
          IF (SOQOFP(QOFP).GE.1) THEN
            PAD(SOQOFP(QOFP)+IVL1)= SOQO(QOFP)
          END IF
          IF (SOQOCX(QOFP).GE.1) THEN
            PAD(SOQOCX(QOFP)+IVL1)= SOQOC(QOFP)
          END IF
          IF (PQADDX(N) .GE. 1) THEN
            PAD(PQADDX(N)+IVL1)= PQADDR(N)
          END IF
          IF (PQADWX(N) .GE. 1) THEN
            PAD(PQADWX(N)+IVL1)= PQADWT(N)
          END IF
          IF (PQADPX(N) .GE. 1) THEN
            PAD(PQADPX(N)+IVL1)= PQADEP(N)
          END IF
          IF (ISQOFP(QOFP) .GE. 1) THEN
            PAD(ISQOFP(QOFP)+IVL1)= ISQO(QOFP)
          END IF
C
        END IF
C
C       interflow associated constituents
        IF (QIFWFG(N).NE.0) THEN
C         this qual is a qualif
          QIFP= QIFWFP(N)
          IF (IOQFP(QIFP).GE.1) THEN
            PAD(IOQFP(QIFP)+IVL1)= IOQUAL(QIFP)
          END IF
        END IF
C
C       groundwater associated constituents
        IF (QAGWFG(N).NE.0) THEN
C         this qual is a qualgw
          QAFP= QAGWFP(N)
          IF (AOQFP(QAFP).GE.1) THEN
            PAD(AOQFP(QAFP)+IVL1)= AOQUAL(QAFP)
          END IF
        END IF
C
C       output of global variables
        IF (SOQFP(N).GE.1) THEN
          PAD(SOQFP(N) +IVL1)= SOQUAL(N)
        END IF
        IF (SOQCFP(N).GE.1) THEN
          PAD(SOQCFP(N)+IVL1)= SOQC(N)
        END IF
        IF (POQFP(N).GE.1) THEN
          PAD(POQFP(N) +IVL1)= POQUAL(N)
        END IF
        IF (POQCFP(N).GE.1) THEN
          PAD(POQCFP(N)+IVL1)= POQC(N)
        END IF
C
 10   CONTINUE
C
      RETURN
      END
C
C     4.2(1).13.7
C
      SUBROUTINE   PQALPT
C
C     + + + PURPOSE + + +
C     Handle section PQUAL.
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PQUAL2 + + +
      INCLUDE   'cplpq.inc'
      INCLUDE   'cmpad.inc'
C     + + + LOCAL VARIABLES + + +
      INTEGER   N,QOFP,QSFP,QIFP,QAFP
C
C     + + + END SPECIFICATION + + +
C
C     handle section pqual
      DO 10 N= 1,NQUAL
C
        IF (QSDFG(N).NE.0) THEN
C         this qual is a qualsd
          QSFP= QSDFP(N)
          IF (SOQSPX(QSFP).GE.1) THEN
            PAD(SOQSPX(QSFP)+IVL1)= SOQSP(QSFP)
          END IF
        END IF
C
        IF (QSOFG(N).NE.0) THEN
C         this qual is a qualof
          QOFP= QSOFP(N)
          IF (SQOFP(QOFP).GE.1) THEN
            PAD(SQOFP(QOFP)+IVL1)= SQO(QOFP)
          END IF
        END IF
C
        IF (QIFWFG(N).NE.0) THEN
C         this qual is a qualif
          QIFP= QIFWFP(N)
          IF (IOQCFP(QIFP).GE.1) THEN
            PAD(IOQCFP(QIFP)+IVL1)= IOQCE(QIFP)
          END IF
        END IF
C
        IF (QAGWFG(N).NE.0) THEN
C         this qual is a qualgw
          QAFP= QAGWFP(N)
          IF (AOQCFP(QAFP).GE.1) THEN
            PAD(AOQCFP(QAFP)+IVL1)= AOQCE(QAFP)
          END IF
        END IF
C
 10   CONTINUE
C
      RETURN
      END
C
C     4.2(1).15.2.7
C
      SUBROUTINE   PQPRT
     I                   (UNITFG,LEV,PRINTU,BINU)
C
C     + + + PURPOSE + + +
C     Convert quantities from internal to external units,
C     and produce printout
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
C     + + + COMMON BLOCKS- SCRTCH, VERSION PQUAL2 + + +
      INCLUDE   'cplpq.inc'
      INCLUDE   'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,I0,I1,J,N,QAFP,QIFP,QOFP,QSFP,CONCFG,FLUXFG,
     $          ACNT,CLEN(23*NQUAL),EXDAT(5)
      REAL      FACTA,CFACTA,PFACTA,PSTAT1(7),PSTAT2(3),PCFLX1,
     $          PCFLX2,PCFLX3,PCFLX4,PCFLX5,PCFLX6,PCFLX7,PADTOT,PTIFLX,
     $          PSOQS,PSOQAL,PPOQAL,PLIFLX,UNDEF,APRINT(23*NQUAL)
      CHARACTER*8   CSTAT1(7),CSTAT2(3)
      CHARACTER*12  QUALNAME(NQUAL)
      CHARACTER*256 CHEAD(23*NQUAL)
C
C     + + + EXTERNALS + + +
      EXTERNAL ZIPR,EXDATE
C
C     + + + DATA INITIALIZATIONS + + +
      DATA UNDEF/-1.0E30/
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' *** PQUAL ***')
 2010 FORMAT (/,'   STATE VARIABLES')
 2020 FORMAT (/,'     LAT INFLOW CONCENTRATIONS   SEDIMENT INTERFLOW',
     $          ' GRNDWATER')
 2030 FORMAT (  31X,'    SLIQSP     ILIQC     ALIQC')
 2040 FORMAT (  31X,' (QTY/TON) (QTY/FT3) (QTY/FT3)')
 2050 FORMAT (  31X,' (QTY/TNE)   (QTY/L)   (QTY/L)')
 2060 FORMAT (  3X,3A4,' (',A4,')',9X,1PG10.3,6G10.3)
 2065 FORMAT (3A4)
 2070 FORMAT (/,'     STOR AND OUTFLOW CONC        STORAGE<---------',
     $          '---------OUTFLOW CONCENTRATIONS------------------>')
 2080 FORMAT (  '                            OF FLOW ASSOC    ON SED',
     $          ' FLOW ASSO  TOT SURF INTERFLOW GRNDWATER     TOTAL')
 2090 FORMAT (  '                                      SQO     SOQSP',
     $          '     SOQOC      SOQC      IOQC      AOQC      POQC')
 2100 FORMAT (  31X,'  (QTY/AC) (QTY/TON)',5(' (QTY/FT3)'))
 2110 FORMAT (  31X,'  (QTY/HA) (QTY/TNE)',5('   (QTY/L)'))
 2120 FORMAT (/,'   FLUXES')
 2130 FORMAT (/,'     INFLOWS',19X,'<---ATMOSPHERIC DEPOSITION--->',
     $          '   LATERAL')
 2140 FORMAT (  35X,'   DRY       WET     TOTAL    INFLOW     TOTAL')
 2150 FORMAT (  35X,'PQADDR    PQADWT     ATDEP     SLIQO    INFLOW')
 2160 FORMAT (  3X,3A4,' (',A4,'/AC)',6X,1PG10.3,7G10.3)
 2170 FORMAT (  3X,3A4,' (',A4,'/HA)',6X,1PG10.3,7G10.3)
 2180 FORMAT (/,'     OUTFLOWS',18X,'<--------------------SURFACE-',
     $          '--------------------> INTERFLOW GRNDWATER     TOTAL')
 2190 FORMAT (  31X,                '<----SEDIMENT ASSOCIATED----->',
     $          'FLOW ASSOC     TOTAL                       OUTFLOW')
 2200 FORMAT (  31X,                '    WASHQS     SCRQS      SOQS',
     $          '      SOQO    SOQUAL    IOQUAL    AOQUAL    POQUAL')
C
C     + + + END SPECIFICATIONS + + +
C
      I0= 0
      I1= 1
C
C     initialize array counter for binary printout, store variable
C     names in local strings for use in building binary headers
      ACNT = 0
      CSTAT2(1) = 'SLIQSP'
      CSTAT2(2) = 'ILIQC'
      CSTAT2(3) = 'ALIQC'
      CSTAT1(1) = 'SQO'
      CSTAT1(2) = 'SOQSP'
      CSTAT1(3) = 'SOQOC'
      CSTAT1(4) = 'SOQC'
      CSTAT1(5) = 'IOQC'
      CSTAT1(6) = 'AOQC'
      CSTAT1(7) = 'POQC'
C
      CONCFG= 0
      FLUXFG= 0
      DO 10 N=1, NQUAL
        WRITE (QUALNAME(N),2065) (QUALID(J,N),J=1,3)
        QUALNAME = ADJUSTL(QUALNAME)
        IF (QSDFG(N) .NE. 0) THEN
C         check qualsd
          IF (SLIQSX(QSDFP(N)) .GE. 1) THEN
C           lateral inflow of qualsd
            CONCFG= 1
          END IF
        END IF
        IF (QSOFG(N) .NE. 0) THEN
C         check qualof
          IF (SLIQOX(QSOFP(N)) .GE. 1) THEN
C           lateral inflow of qualof
            FLUXFG= 1
          END IF
          J= (N-1)*2+ 1
          IF ( (PQADFG(J) .NE. 0) .OR. (PQADFG(J+1) .NE. 0) ) THEN
C           atmospheric deposition
            FLUXFG= 1
          END IF
        END IF
        IF (QIFWFG(N) .NE. 0) THEN
C         check qualif
          IF (ILIQCX(QIFWFP(N)) .GE. 1) THEN
C           lateral inflow of qualif
            CONCFG= 1
          END IF
        END IF
        IF (QAGWFG(N) .NE. 0) THEN
C         check qualgw
          IF (ALIQCX(QAGWFP(N)) .GE. 1) THEN
C           lateral inflow of qualgw
            CONCFG= 1
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
      IF (PRINTU .GT. 0 .AND. PFLAG(7) .LE. LEV) THEN
        WRITE (PRINTU,2000)
C
C       state variables
C
C       write header
        WRITE (PRINTU,2010)
      END IF
C
      IF (CONCFG .EQ. 1) THEN
C       write lateral inflow concentrations
C
        IF (PRINTU .GT. 0 .AND. PFLAG(7) .LE. LEV) THEN
C         write header
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
C
C       convert to printout units
        DO 20 N= 1, NQUAL
C
C         all start undefined
          J= 3
          CALL ZIPR (J,UNDEF,
     O               PSTAT2)
C
          IF (QSDFG(N).NE.0) THEN
C           qual is qualsd
            QSFP= QSDFP(N)
            IF (SLIQSP(QSFP) .GE. 0.0) THEN
C             lateral inflow potency factor needs conversion
              PSTAT2(1)= SLIQSP(QSFP)*PFACTA
            END IF
          END IF
C
          IF (QIFWFG(N).NE.0) THEN
C           qual is qualif
            QIFP= QIFWFP(N)
            IF (ILIQC(QIFP) .GE. 0.0) THEN
C             lateral inflow concentration needs conversion
              PSTAT2(2)= ILIQC(QIFP)*CFACTA
            END IF
          END IF
C
          IF (QAGWFG(N).NE.0) THEN
C           qual is qualgw
            QAFP= QAGWFP(N)
            IF (ALIQC(QAFP) .GE. 0.0) THEN
C             lateral inflow concentration needs conversion
              PSTAT2(3)= ALIQC(QAFP)*CFACTA
            END IF
          END IF
C
C         write results
          IF (PRINTU .GT. 0 .AND. PFLAG(7) .LE. LEV) THEN
            WRITE (PRINTU,2060) (QUALID(J,N),J=1,3),QTYID(1,N),PSTAT2
          END IF
          IF (BINU .GT. 0 .AND. ABS(BFLAG(7)) .LE. LEV) THEN
            DO 22 I= 1, 3
              ACNT = ACNT + 1
              APRINT(ACNT) = PSTAT2(I)
              CHEAD(ACNT) = TRIM(CSTAT2(I)) // '-'
              CHEAD(ACNT) = TRIM(CHEAD(ACNT)) // QUALNAME(N)
              CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 22         CONTINUE
          END IF
 20     CONTINUE
      END IF
C
C     write storages and outflow concentrations
      IF (PRINTU .GT. 0 .AND. PFLAG(7) .LE. LEV) THEN
        WRITE (PRINTU,2070)
        WRITE (PRINTU,2080)
        WRITE (PRINTU,2090)
C
        IF (UNITFG.EQ.1) THEN
C         english system
          WRITE (PRINTU,2100)
        ELSE
C         metric system
          WRITE (PRINTU,2110)
        END IF
      END IF
C
      DO 30 N= 1, NQUAL
C
C       all start undefined
        J= 7
        CALL ZIPR (J,UNDEF,
     O             PSTAT1)
C
        IF (QSDFG(N).NE.0) THEN
C         qual is qualsd
          QSFP= QSDFP(N)
          IF (SOQSP(QSFP) .GE. 0.0) THEN
C           outflow potency factor needs conversion
            PSTAT1(2)= SOQSP(QSFP)*PFACTA
          END IF
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
        END IF
C
        IF ( (QSDFG(N) .NE. 0) .OR. (QSOFG(N) .NE. 0) ) THEN
C         there is some surface association
          IF (SOQC(N) .GE. 0.0) THEN
C           total surface outflow concentration
            PSTAT1(4)= SOQC(N)*CFACTA
          END IF
        END IF
C
        IF (QIFWFG(N).NE.0) THEN
C         qual is qualif
          QIFP= QIFWFP(N)
          IF (IOQCE(QIFP) .GE. 0.0) THEN
C           outflow concentration needs conversion
            PSTAT1(5)= IOQCE(QIFP)*CFACTA
          END IF
        END IF
C
        IF (QAGWFG(N).NE.0) THEN
C         qual is qualgw
          QAFP= QAGWFP(N)
          IF (AOQCE(QAFP) .GE. 0.0) THEN
C           outflow concentration needs conversion
            PSTAT1(6)= AOQCE(QAFP)*CFACTA
          END IF
        END IF
C
        IF (POQC(N) .GE. 0.0) THEN
C         total outflow concentration
          PSTAT1(7)= POQC(N)*CFACTA
        END IF
C
C       write results
        IF (PRINTU .GT. 0 .AND. PFLAG(7) .LE. LEV) THEN
          WRITE (PRINTU,2060) (QUALID(J,N),J=1,3),QTYID(1,N),PSTAT1
        END IF
        IF (BINU .GT. 0 .AND. ABS(BFLAG(7)) .LE. LEV) THEN
          DO 29 I= 1, 7
            ACNT = ACNT + 1
            APRINT(ACNT) = PSTAT1(I)
            CHEAD(ACNT) = TRIM(CSTAT1(I)) // '-'
            CHEAD(ACNT) = TRIM(CHEAD(ACNT)) // QUALNAME(N)
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 29      CONTINUE
        END IF
 30   CONTINUE
C
C     write fluxes
C
      IF (PRINTU .GT. 0 .AND. PFLAG(7) .LE. LEV) THEN
C       write header
        WRITE (PRINTU,2120)
      END IF
C
      IF (FLUXFG .EQ. 1) THEN
C       there is atmospheric deposition and/or lateral inflow to output
C
        IF (PRINTU .GT. 0 .AND. PFLAG(7) .LE. LEV) THEN
C         write header
          WRITE (PRINTU,2130)
          WRITE (PRINTU,2140)
          WRITE (PRINTU,2150)
        END IF
C
        DO 40 N= 1, NQUAL
          IF (QSOFG(N).NE.0) THEN
C           constituent is simulated by association with
C           overland flow
C
C           atdep for this qual
            PCFLX6= PQCF6(N,LEV)*FACTA
            PCFLX7= PQCF7(N,LEV)*FACTA
            PADTOT= PCFLX6+ PCFLX7
C
            PLIFLX= PQIF(QSOFP(N),LEV)*FACTA
            PTIFLX= PLIFLX+ PADTOT
C
C           write results
            IF (PRINTU .GT. 0 .AND. PFLAG(7) .LE. LEV) THEN
              IF (UNITFG.EQ.1) THEN
C               english system
                WRITE (PRINTU,2160) (QUALID(J,N),J=1,3),QTYID(1,N),
     $            PCFLX6,PCFLX7,PADTOT,PLIFLX,PTIFLX
              ELSE
C               metric system
                WRITE (PRINTU,2170) (QUALID(J,N),J=1,3),QTYID(1,N),
     $            PCFLX6,PCFLX7,PADTOT,PLIFLX,PTIFLX
              END IF
            END IF
            IF (BINU .GT. 0 .AND. ABS(BFLAG(7)) .LE. LEV) THEN
              ACNT = ACNT + 1
              APRINT(ACNT) = PCFLX6
              CHEAD(ACNT) = 'PQADDR-'
              CHEAD(ACNT) = TRIM(CHEAD(ACNT)) // QUALNAME(N)
              CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
              ACNT = ACNT + 1
              APRINT(ACNT) = PCFLX7
              CHEAD(ACNT) = 'PQADWT-'
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
              APRINT(ACNT) = PTIFLX
              CHEAD(ACNT) = 'INFLOW-'
              CHEAD(ACNT) = TRIM(CHEAD(ACNT)) // QUALNAME(N)
              CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
            END IF
          END IF
 40     CONTINUE
      END IF
C
C     write outflow fluxes
      IF (PRINTU .GT. 0 .AND. PFLAG(7) .LE. LEV) THEN
        WRITE (PRINTU,2180)
        WRITE (PRINTU,2190)
        WRITE (PRINTU,2200)
      END IF
C
      DO 50 N= 1, NQUAL
        IF (QSDFG(N).NE.0) THEN
C         qual is qualsd
          QSFP= QSDFP(N)
          PCFLX1= PQCF1(QSFP,LEV)*FACTA
          PCFLX2= PQCF2(QSFP,LEV)*FACTA
          PSOQS=  PCFLX1+ PCFLX2
        ELSE
C         not qualsd
          PCFLX1= 0.0
          PCFLX2= 0.0
          PSOQS= 0.0
        END IF
C
        IF (QSOFG(N).NE.0) THEN
C         qual is qualof
          PCFLX3= PQCF3(QSOFP(N),LEV)*FACTA
        ELSE
C         not qualof
          PCFLX3= 0.0
        END IF
C
        IF (QIFWFG(N).NE.0) THEN
C         constituent is simulated in interflow
          PCFLX4= PQCF4(QIFWFP(N),LEV)*FACTA
        ELSE
C         no interflow association
          PCFLX4= 0.0
        END IF
C
        IF (QAGWFG(N).NE.0) THEN
C         constituent is simulated in groundwater flow
          PCFLX5= PQCF5(QAGWFP(N),LEV)*FACTA
        ELSE
C         no baseflow association
          PCFLX5= 0.0
        END IF
C
C       cumulate total outflow
        PSOQAL= PSOQS+ PCFLX3
        PPOQAL= PSOQAL+ PCFLX4+ PCFLX5
C
C       write results
C
        IF (PRINTU .GT. 0 .AND. PFLAG(7) .LE. LEV) THEN
          IF (UNITFG.EQ.1) THEN
C           english system
            WRITE (PRINTU,2160) (QUALID(J,N),J=1,3),QTYID(1,N),
     $        PCFLX1,PCFLX2,PSOQS,PCFLX3,PSOQAL,PCFLX4,PCFLX5,PPOQAL
          ELSE
C           metric system
            WRITE (PRINTU,2170) (QUALID(J,N),J=1,3),QTYID(1,N),
     $        PCFLX1,PCFLX2,PSOQS,PCFLX3,PSOQAL,PCFLX4,PCFLX5,PPOQAL
          END IF
        END IF
        IF (BINU .GT. 0 .AND. ABS(BFLAG(7)) .LE. LEV) THEN
          ACNT = ACNT + 1
          APRINT(ACNT) = PCFLX1
          CHEAD(ACNT) = 'WASHQS-'
          CHEAD(ACNT) = TRIM(CHEAD(ACNT)) // QUALNAME(N)
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          ACNT = ACNT + 1
          APRINT(ACNT) = PCFLX2
          CHEAD(ACNT) = 'SCRQS-'
          CHEAD(ACNT) = TRIM(CHEAD(ACNT)) // QUALNAME(N)
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          ACNT = ACNT + 1
          APRINT(ACNT) = PSOQS
          CHEAD(ACNT) = 'SOQS-'
          CHEAD(ACNT) = TRIM(CHEAD(ACNT)) // QUALNAME(N)
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          ACNT = ACNT + 1
          APRINT(ACNT) = PCFLX3
          CHEAD(ACNT) = 'SOQO-'
          CHEAD(ACNT) = TRIM(CHEAD(ACNT)) // QUALNAME(N)
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          ACNT = ACNT + 1
          APRINT(ACNT) = PSOQAL
          CHEAD(ACNT) = 'SOQUAL-'
          CHEAD(ACNT) = TRIM(CHEAD(ACNT)) // QUALNAME(N)
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          ACNT = ACNT + 1
          APRINT(ACNT) = PCFLX4
          CHEAD(ACNT) = 'IOQUAL-'
          CHEAD(ACNT) = TRIM(CHEAD(ACNT)) // QUALNAME(N)
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          ACNT = ACNT + 1
          APRINT(ACNT) = PCFLX5
          CHEAD(ACNT) = 'AOQUAL-'
          CHEAD(ACNT) = TRIM(CHEAD(ACNT)) // QUALNAME(N)
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          ACNT = ACNT + 1
          APRINT(ACNT) = PPOQAL
          CHEAD(ACNT) = 'POQUAL-'
          CHEAD(ACNT) = TRIM(CHEAD(ACNT)) // QUALNAME(N)
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
       END IF
 50   CONTINUE
C
      IF (BINU .GT. 0 .AND. ABS(BFLAG(7)) .LE. LEV) THEN
C       write binary output
        CALL EXDATE(
     I              DATIM,
     O              EXDAT)
        IF (BFLAG(7) .GT. 0) THEN
C         at start of run, write the header
          WRITE (BINU) I0,'PERLND  ',LSNO,'PQUAL   ',
     1          (CLEN(I),(CHEAD(I)(J:J),J=1,CLEN(I)),I=1,ACNT)
C         set bflag to negative to not write headers anymore
          BFLAG(7) = -BFLAG(7)
        END IF
        WRITE (BINU) I1,'PERLND  ', LSNO,'PQUAL   ',UNITFG,
     1               LEV,(EXDAT(I),I=1,5),(APRINT(I),I=1,ACNT)
      END IF
C
      RETURN
      END
C
C     4.2(1).15.3.5
C
      SUBROUTINE   PQRST
     I                   (LEV)
C
C     + + + PURPOSE + + +
C     Reset all flux accumulators in section pqual
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    LEV
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEV    - current output level (2-pivl,3-day,4-mon,5-ann)
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PQUAL2 + + +
      INCLUDE    'cplpq.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    N
C
C     + + + END SPECIFICATIONS + + +
C
      DO 10 N= 1,MXQUAL
        PQIF(N,LEV)=  0.0
        PQCF1(N,LEV)= 0.0
        PQCF2(N,LEV)= 0.0
        PQCF3(N,LEV)= 0.0
        PQCF4(N,LEV)= 0.0
        PQCF5(N,LEV)= 0.0
 10   CONTINUE
C
      DO 20 N= 1, MXQUAL
        PQCF6(N,LEV)= 0.0
        PQCF7(N,LEV)= 0.0
 20   CONTINUE
C
      RETURN
      END
C
C     4.2(1).7.4
C
      SUBROUTINE   QUALGW
     I                    (DAYFG,VAQCFG,AGWO,LIFAC,ALIQC,AOQCM,MON,
     I                     NXTMON,DAY,NDAYS,
     M                     AOQC,
     O                     AOQUAL,AOQCE)
C
C     + + + PURPOSE + + +
C     Simulate quality constituents by fixed concentration in
C     groundwater flow
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    DAY,DAYFG,MON,NDAYS,NXTMON,VAQCFG
      REAL       AGWO,AOQC,AOQCM(12),AOQUAL,LIFAC,ALIQC,AOQCE
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DAYFG  - flag for first day or day change
C     VAQCFG - ???
C     AGWO   - ???
C     LIFAC  - weighting factor for lateral inflow concentration
C     ALIQC  - concentration in lateral inflow
C     AOQCM  - ???
C     MON    - calendar month
C     NXTMON - next calendar month
C     DAY    - day of month
C     NDAYS  - no. of days in this month
C     AOQC   - ???
C     AOQUAL - ???
C     AOQCE  - concentration in outflow
C
C     + + + FUNCTIONS + + +
      REAL       DAYVAL
C
C     + + + EXTERNALS + + +
      EXTERNAL   DAYVAL
C
C     + + + END SPECIFICATIONS + + +
C
      IF (DAYFG.EQ.1) THEN
C       it is the first interval of the day
        IF (VAQCFG.GT.0) THEN
C
C         modification to allow optional no interpolation of
C         monthly values  (modified for chesapeake bay)
          IF (VAQCFG.EQ.2) THEN
C           no interpolation
            AOQC = AOQCM(MON)
          ELSE
C           concentrations are allowed to vary throughout the year
C           interpolate for the daily value
C           linearly interpolate aoqc between two values from the
C           monthly array aoqcm(12) for this interflow quality
C           constituent (no. qafp)
            AOQC = DAYVAL(AOQCM(MON),AOQCM(NXTMON),DAY,NDAYS)
          END IF
        ELSE
C         concentrations do not vary throughout the year.
C         aoqc value has been supplied by the run interpreter
        END IF
C
      END IF
C
C     simulate constituents carried by groundwater flow - units
C     are qty/acre-ivl
      IF (AGWO.GT.0.0) THEN
C       there is baseflow
        IF (ALIQC .GE. 0.0) THEN
C         lateral inflow has an effect on concentration
          AOQCE= ALIQC*LIFAC+ AOQC*(1.0- LIFAC)
        ELSE
C         no effect of lateral inflow
          AOQCE= AOQC
        END IF
        AOQUAL= AOQCE*AGWO
      ELSE
C       no baseflow
        AOQCE= -1.0E30
        AOQUAL= 0.0
      END IF
C
      RETURN
      END
C
C     4.2(1).7.3
C
      SUBROUTINE   QUALIF
     I                    (DAYFG,VIQCFG,IFWO,LIFAC,ILIQC,IOQCM,
     I                     MON,NXTMON,DAY,NDAYS,
     M                     IOQC,
     O                     IOQUAL,IOQCE)
C
C     + + + PURPOSE + + +
C     Simulate quality constituents by fixed concentration in interflow
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    DAY,DAYFG,MON,NDAYS,NXTMON,VIQCFG
      REAL       IFWO,IOQC,IOQCM(12),IOQUAL,LIFAC,ILIQC,IOQCE
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DAYFG  - flag for first day or day change
C     VIQCFG - ???
C     IFWO   - ???
C     LIFAC  - weighting factor for lateral inflow concentration
C     ILIQC  - concentration in lateral inflow
C     IOQCM  - ???
C     MON    - calendar month
C     NXTMON - next calendar month
C     DAY    - day of month
C     NDAYS  - no. of days in this month
C     IOQC   - ???
C     IOQUAL - ???
C     IOQCE  - concentration in outflow
C
C     + + + FUNCTIONS + + +
      REAL       DAYVAL
C
C     + + + EXTERNALS + + +
      EXTERNAL   DAYVAL
C
C     + + + END SPECIFICATIONS + + +
C
      IF (DAYFG.EQ.1) THEN
C       it is the first interval of the day
        IF (VIQCFG.GT.0) THEN
C
C         modification to allow optional no interpolation of
C         monthly values  (modified for chesapeake bay)
          IF (VIQCFG.EQ.2) THEN
C           no interpolation
            IOQC = IOQCM(MON)
          ELSE
C           concentrations are allowed to vary throughout the year
C           interpolate for the daily value
C           linearly interpolate ioqc between two values from the
C           monthly array ioqcm(12) for this interflow quality
C           constituent (no. qifp)
            IOQC = DAYVAL(IOQCM(MON),IOQCM(NXTMON),DAY,NDAYS)
          END IF
        ELSE
C         concentrations do not vary throughout the year.
C         ioqc value has been supplied by the run interpreter
        END IF
C
      END IF
C
C     simulate constituents carried by interflow - units are
C     qty/acre-ivl
      IF (IFWO.GT.0.0) THEN
C       there is interflow
        IF (ILIQC .GE. 0.0) THEN
C         lateral inflow has an effect on concentration
          IOQCE= ILIQC*LIFAC+ IOQC*(1.0- LIFAC)
        ELSE
C         no effect of lateral inflow
          IOQCE= IOQC
        END IF
        IOQUAL= IOQCE*IFWO
      ELSE
C       no interflow
        IOQCE= -1.0E30
        IOQUAL= 0.0
      END IF
C
      RETURN
      END
C
C     4.2(1).7.2
C
      SUBROUTINE   QUALOF
     I                    (DAYFG,VQOFG,ACQOPM,REMQOM,MON,NXTMON,DAY,
     I                     NDAYS,SURO,WSFAC,PREC,ADFXFG,ADCNFG,ADFXMN,
     I                     ADCNMN,ADFLX,ADCNC,SLIQO,DELT60,QSOFG,
     M                     ACQOP,REMQOP,SQO,
     O                     SOQO,SOQOC,ADFXFX,ADCNFX,ADTOT,INTOT)
C
C     + + + PURPOSE + + +
C     Simulate accumulation of a quality constituent on the land
C     surface and its removal by a constant unit rate and by overland
C     flow
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    DAYFG,VQOFG,MON,NXTMON,DAY,NDAYS,ADFXFG,ADCNFG,QSOFG
      REAL       ACQOPM(12),REMQOM(12),SURO,WSFAC,PREC,ADFXMN(12),
     #           ADCNMN(12),ADFLX,ADCNC,SLIQO,ACQOP,REMQOP,SQO,SOQO,
     #           SOQOC,ADFXFX,ADCNFX,ADTOT,INTOT,DELT60
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DAYFG  - flag for first day or day change
C     VQOFG  - ???
C     ACQOPM - ???
C     REMQOM - ???
C     MON    - calendar month
C     NXTMON - next calendar month
C     DAY    - day of month
C     NDAYS  - no. of days in this month
C     SURO   - surface output
C     WSFAC  - ???
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
      INTRINSIC   EXP
C
C     + + + END SPECIFICATIONS + + +
C
      IF (DAYFG.EQ.1) THEN
C       it is the first interval of the day
        IF (VQOFG.EQ.1) THEN
C         accumulation rate of this quality constituent is allowed
C         to vary throughout the year
C         interpolate for the daily value
C         linearly interpolate acqop between two values from the
C         monthly array acqopm(12) for this overland flow associated
C         quality constituent (no. qofp)
          ACQOP= DAYVAL(ACQOPM(MON),ACQOPM(NXTMON),DAY,NDAYS)
C
C         removal unit rate of this quality constituent is allowed
C         to vary throughout the year
C         interpolate for the daily value
C         linearly interpolate remqop between two values from the
C         monthly array remqom(12) for this overland flow associated
C         quality constituent (no. qofp)
          REMQOP= DAYVAL(REMQOM(MON),REMQOM(NXTMON),DAY,NDAYS)
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
C     add lateral inflow
      INTOT= ADTOT+ SLIQO
C
      IF (QSOFG .EQ. 2) THEN
C       update storage due to accumulation and removal which occurs
C       independent of runoff - units are qty/acre
        DUMMY= REMQOP+ INTOT/(ACQOP/REMQOP)
        IF (DUMMY .GT. 1.0) DUMMY=1.0
        SQO= ACQOP*(DELT60/24.0)+ SQO*(1.0-DUMMY)**(DELT60/24.0)
      END IF
C
C     update storage
      SQO= SQO+ INTOT
C
C     simulate washoff by overland flow - units are qty/acre-ivl
      IF (SURO.GT.0.0) THEN
C       there is overland flow
        IF (SQO.GT.0.0) THEN
C         there is some quality constituent (no. qofp) in storage,
C         washoff can occur
          DUMMY= SURO*WSFAC
          IF (DUMMY .LT. 1.0E-5) THEN
C           washoff too small for stable calculation - set to zero
            SOQO= 0.0
          ELSE
C           calculate washoff
            DUMMY= 1.0- EXP(-DUMMY)
            SOQO = SQO*DUMMY
C
C           update storage of constituent - units are in qty/acre
            SQO=SQO- SOQO
          END IF
        ELSE
C         no washoff load
          SOQO= 0.0
        END IF
      ELSE
        SOQO= 0.0
      END IF
C
C     compute and output concentration - units are qty/acre-inch
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
C     4.2(1).7.1
C
      SUBROUTINE   QUALSD
     I                    (DAYFG,VPFWFG,VPFSFG,WSSD,SCRSD,LIFAC,SLIQSP,
     I                     POTFWM,POTFSM,MON,NXTMON,DAY,NDAYS,
     M                     POTFW,POTFS,
     O                     WASHQS,SCRQS,SOQS,SOQSP)
C
C     + + + PURPOSE + + +
C     Simulate removal of a quality constituent from the land
C     surface by association with sediment
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    DAY,DAYFG,MON,NDAYS,NXTMON,VPFSFG,VPFWFG
      REAL       POTFS,POTFSM(12),POTFW,POTFWM(12),SCRQS,SCRSD,SOQS,
     $           WASHQS,WSSD,LIFAC,SLIQSP,SOQSP
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DAYFG  - flag for first day or day change
C     VPFWFG - ???
C     VPFSFG - ???
C     WSSD   - ???
C     SCRSD  - ???
C     LIFAC  - weighting factor for lateral inflow potency factor
C     SLIQSP - potency factor on lateral inflow of sediment
C     POTFWM - ???
C     POTFSM - ???
C     MON    - calendar month
C     NXTMON - next calendar month
C     DAY    - day of month
C     NDAYS  - no. of days in this month
C     POTFW  - ???
C     POTFS  - ???
C     WASHQS - ???
C     SCRQS  - ???
C     SOQS   - ???
C     SOQSP  - potency factor on outflow of sediment
C
C     + + + LOCAL VARIABLES + + +
      REAL       LSOSED
C
C     + + + FUNCTIONS + + +
      REAL       DAYVAL
C
C     + + + EXTERNALS + + +
      EXTERNAL   DAYVAL
C
C     + + + INTRINSICS + + +
      INTRINSIC   ABS
C
C     + + + END SPECIFICATIONS + + +
C
      IF (DAYFG.EQ.1) THEN
C       it is the first interval of the day
        IF (VPFWFG.GT.0) THEN
C
C         modification to allow optional no interpolation of
C         monthly values  (modified for chesapeake bay)
          IF (VPFWFG.EQ.2) THEN
C           no interpolation
            POTFW = POTFWM(MON)
          ELSE
C           potency factors are allowed to vary throughout the year
C           interpolate for the daily value
C           linearly interpolate potfw between two values from the
C           monthly array potfwm(12) for this sediment associated quality
C           constituent (no. qsfp)
            POTFW = DAYVAL(POTFWM(MON),POTFWM(NXTMON),DAY,NDAYS)
          END IF
        ELSE
C         washoff potency factors do not vary throughout the year.
C         potfw value has been supplied by the run interpreter
        END IF
C
        IF (VPFSFG.EQ.1) THEN
C         scour potency factors are allowed to vary throughout the
C         year
C         interpolate for the daily value
C         linearly interpolate potfs between two values from the
C         monthly array potfsm(12) for this sediment associated
C         quality constituent (no. qsfp)
          POTFS= DAYVAL(POTFSM(MON),POTFSM(NXTMON),DAY,NDAYS)
        ELSE
C         scour potency factors do not vary throughout the year.
C         potsw value has been supplied by the run interpreter
        END IF
C
      END IF
C
C     associate with washoff of detached sediment - units are
C     qty/acre-ivl
      IF (ABS(WSSD).LE.0.0) THEN
C       no washoff of sediment
        WASHQS= 0.0
      ELSE
        IF (SLIQSP .GE. 0.0) THEN
C         lateral inflow has an effect on washoff potency factor
          WASHQS= WSSD*(SLIQSP*LIFAC+ POTFW*(1.0- LIFAC))
        ELSE
C         no effect of lateral inflow
          WASHQS= WSSD*POTFW
        END IF
      END IF
C
C     associate with scouring of soil matrix - units are
C     qty/acre-ivl
      IF (ABS(SCRSD).LE.0.0) THEN
C       no scour
        SCRQS= 0.0
      ELSE
C       scour
        SCRQS= SCRSD*POTFS
      END IF
C
C     sum removals
      SOQS= WASHQS+SCRQS
C
C     calculate effective outflow potency factor
      LSOSED= WSSD+ SCRSD
      IF (LSOSED .GT. 0.0) THEN
C       there is some sediment outflow
        SOQSP= SOQS/LSOSED
      ELSE
C       no sediment outflow
        SOQSP= -1.0E30
      END IF
C
      RETURN
      END
