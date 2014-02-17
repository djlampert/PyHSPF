C
C
C
      SUBROUTINE   PNUTRX
C
C     + + + PURPOSE + + +
C     Process input for the nutrx section of rchres application module
C
C     + + + COMMON BLOCKS- SCRTCH, VERISON NUTRX1 + + +
      INCLUDE    'crhnu.inc'
      INCLUDE    'crin2.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + EXTERNALS + + +
      EXTERNAL   RTABLE,ITABLE,OMSG,OMSTI,ZIPI
      EXTERNAL   ZIPR,MDATBL
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,J,I1,I2,I4,IVAL(8),SGRP,SCLU,N,RETCOD
      REAL       RVAL(6),R0
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' PROCESSING INPUT FOR SECTION NUTRX')
 2030 FORMAT (/,' FINISHED PROCESSING INPUT FOR SECTION NUTRX')
C
C     + + + END SPECIFICATIONS + + +
C
      IF (OUTLEV.GT.1) THEN
        WRITE (MESSU,2000)
      END IF
C
      I1  = 1
      R0 = 0.0
      SCLU= 348
C
C     initialize month-data input
      I= 36
      CALL ZIPR (I,R0,
     O           NUAFXM)
      CALL ZIPR (I,R0,
     O           NUACNM)
C
C     initialize atmospheric deposition fluxes
      I= 15
      CALL ZIPR (I,R0,
     O           NUCF11)
      CALL ZIPR (I,R0,
     O           NUCF12)
C
      I= 4
      J= 0
      CALL ZIPI(I,J,NUECNT)
      CALL ZIPI(I1,J,NUWCNT)
C
C     flags - table-type nut-flags
      I2= 84
      I4=  8
      CALL ITABLE
     I             (I2,I1,I4,UUNITS,
     M              IVAL)
      DO 10 I= 1, 7
        NUPM1(I)= IVAL(I)
 10   CONTINUE
      PHFLAG= IVAL(8)
C
      IF ((TAMFG .EQ. 0 .AND. (AMVFG .EQ. 1 .OR. ADNHFG .EQ. 1))
     $         .OR. (PO4FG .EQ. 0 .AND. ADPOFG .EQ. 1) ) THEN
C       error - either: 1) tam is not being simulated, and nh3 volat. or
C       nh4 adsorption is being simulated; or 2) po4 is not being
C       simulated, and po4 adsorption is being simulated
        SGRP= 11
        CALL OMSTI(RCHNO)
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             ECOUNT)
      END IF
C
      IF ((ADNHFG .EQ. 1 .OR. ADPOFG .EQ. 1) .AND. SEDFG .EQ. 0) THEN
C       error - sediment associated nh4 and/or po4 is being simulated,
C       but sediment is not being simulated in section sedtrn
        SGRP= 12
        CALL OMSTI(RCHNO)
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             ECOUNT)
      END IF
C
C     atmospheric deposition flags - table-type nut-ad-flags
      I2= 85
      I4=  6
      CALL ITABLE
     I             (I2,I1,I4,UUNITS,
     M              NUADFG)
C
C     read in month-data tables where necessary
      DO 40 J= 1, 3
        N= 2*(J- 1)+ 1
        IF (NUADFG(N) .GT. 0) THEN
C         monthly flux must be read
          CALL MDATBL
     I                (NUADFG(N),
     O                 NUAFXM(1,J),RETCOD)
C         convert units to internal - not done by MDATBL
          IF (UUNITS .EQ. 1) THEN
C           convert from lb/ac.day to mg.ft3/l.ft2.ivl
            DO 20 I= 1, 12
              NUAFXM(I,J)= NUAFXM(I,J)*0.3677*DELT60/24.0
 20         CONTINUE
          ELSE IF (UUNITS .EQ. 2) THEN
C           convert from kg/ha.day to mg.m3/l.m2.ivl
            DO 30 I= 1, 12
              NUAFXM(I,J)= NUAFXM(I,J)*0.1*DELT60/24.0
 30         CONTINUE
          END IF
        END IF
        IF (NUADFG(N+1) .GT. 0) THEN
C         monthly ppn conc must be read
          CALL MDATBL
     I                (NUADFG(N+1),
     O                 NUACNM(1,J),RETCOD)
        END IF
 40   CONTINUE
C
C     conversion factors - table-type conv-val1
      I2= 86
      I4=  4
      CALL RTABLE
     I             (I2,I1,I4,UUNITS,
     M              RVAL)
C
      CVBO  = RVAL(1)
      CVBPC = RVAL(2)
      CVBPN = RVAL(3)
      BPCNTC= RVAL(4)
C
C     calculate derived values
      CVBP= (31.*BPCNTC)/(1200.*CVBPC)
      CVBN= 14.0*CVBPN*CVBP/31.0
      CVOC= BPCNTC/(100.0*CVBO)
      CVON= CVBN/CVBO
      CVOP= CVBP/CVBO
C
      IF ( (BENRFG .EQ. 1) .OR. (PLKFG .EQ. 1) ) THEN
C       benthal release parms - table-type nut-benparm
        I2= 87
        I4=  5
        CALL RTABLE
     I               (I2,I1,I4,UUNITS,
     M                NUPM2)
C
C       convert units from 1/hr to 1/ivl
        DO 50 I= 1,4
          NUPM2(I)= NUPM2(I)*DELT60
 50     CONTINUE
C
      END IF
C
C     nitrification parameters - table-type nut-nitdenit
      I2= 88
      I4=  6
      CALL RTABLE
     I             (I2,I1,I4,UUNITS,
     M              NUPM4)
C
C     convert units from 1/hr to 1/ivl
      KTAM20= KTAM20*DELT60
      KNO220= KNO220*DELT60
      KNO320= KNO320*DELT60
C
      IF (TAMFG .EQ. 1 .AND. AMVFG .EQ. 1) THEN
C       ammonia volatilization parameters table nut-nh3volat
        I2= 89
        I4=  2
        CALL RTABLE
     I               (I2,I1,I4,UUNITS,
     M                NUPM5)
C
      END IF
C
      IF (TAMFG .EQ. 1 .AND. PHFLAG .EQ. 3) THEN
C       monthly ph values table mon-phval
        I2= 94
        I4= 12
        CALL RTABLE
     I               (I2,I1,I4,UUNITS,
     M                PHVALM)
C
      END IF
C
      IF ((TAMFG .EQ. 1 .AND. ADNHFG .EQ. 1) .OR.
     $    (PO4FG .EQ. 1 .AND. ADPOFG .EQ. 1)) THEN
C       bed sediment concentrations of nh4 and po4 - table nut-bedconc
        I2= 90
        I4=  6
        CALL RTABLE
     I               (I2,I1,I4,UUNITS,
     M                RVAL)
C
C       convert concentrations from mg/kg to internal units of mg/mg
        DO 60 I= 1, 6
          NUPM3(I) = RVAL(I)/1.0E6
 60     CONTINUE
C
C       initialize adsorbed nutrient mass storages in bed
        RSNH4(8) = 0.0
        RSPO4(8) = 0.0
        DO 70 I= 5,7
          RSNH4(I)= BNH4(I-4) * RSED(I)
          RSPO4(I)= BPO4(I-4) * RSED(I)
          RSNH4(8)= RSNH4(8) + RSNH4(I)
          RSPO4(8)= RSPO4(8) + RSPO4(I)
 70     CONTINUE
C
C       adsorption parameters - table-type nut-adsparm
        I2= 91
        I4=  6
        CALL RTABLE
     I               (I2,I1,I4,UUNITS,
     M                RVAL)
C
C       convert adsorption parameters from ml/g to internal units of l/mg
        DO 80 I= 1, 6
          NUADPM(I) = RVAL(I)/1.0E6
 80     CONTINUE
C
      END IF
C
C     initial conditions - table-type nut-dinit
      I2= 92
      I4=  5
      CALL RTABLE
     I             (I2,I1,I4,UUNITS,
     M              RVAL)
C
C     assign concentrations and initialize nutrient mass storages
      DO 90 I= 1,4
        DNUST(I) = RVAL(I)
        DNUST2(I)= DNUST(I) * VOL
 90   CONTINUE
      IF (TAMFG .EQ. 1) THEN
C       do the tam-associated initial values (nh4 nh3 phval)
        PHVAL    = RVAL(5)
C       assume nh4 and nh3 are 0.99 x tam and 0.01 x tam respectively
        DNUST(5) =0.99*DNUST(2)
        DNUST2(5)=DNUST(5) * VOL
        DNUST(6) =0.01*DNUST(2)
        DNUST2(6)=DNUST(6) * VOL
      END IF
C
      IF ((TAMFG .EQ. 1 .AND. ADNHFG .EQ. 1) .OR.
     $    (PO4FG .EQ. 1 .AND. ADPOFG .EQ. 1)) THEN
C       suspended sediment concentrations of nh4 and po4 - table nut-adsinit
C       (input concentrations are mg/kg - these are converted to mg/mg for
C       internal computations)
        I2= 93
        I4=  6
        CALL RTABLE
     I               (I2,I1,I4,UUNITS,
     M                RVAL)
C
        DO 100 I = 1, 3
          SNH4(I) = RVAL(I)/1.0E6
          SPO4(I) = RVAL(I+3)/1.0E6
 100    CONTINUE
C       initialize adsorbed nutrient mass storages in suspension
        RSNH4(4) = 0.0
        RSPO4(4) = 0.0
        DO 110 I= 1,3
          RSNH4(I)= SNH4(I) * RSED(I)
          RSPO4(I)= SPO4(I) * RSED(I)
          RSNH4(4)= RSNH4(4) + RSNH4(I)
          RSPO4(4)= RSPO4(4) + RSPO4(I)
 110    CONTINUE
C       initialize totals on sand, silt, clay, and grand total
        RSNH4(9)  = RSNH4(1) + RSNH4(5)
        RSNH4(10) = RSNH4(2) + RSNH4(6)
        RSNH4(11) = RSNH4(3) + RSNH4(7)
        RSNH4(12) = RSNH4(4) + RSNH4(8)
        RSPO4(9)  = RSPO4(1) + RSPO4(5)
        RSPO4(10) = RSPO4(2) + RSPO4(6)
        RSPO4(11) = RSPO4(3) + RSPO4(7)
        RSPO4(12) = RSPO4(4) + RSPO4(8)
C
      END IF
C
C     initialize total storages of nutrients in reach
      NUST(1,1) = DNUST2(1)
      NUST(2,1) = DNUST2(2)
      IF (ADNHFG .EQ. 1) THEN
        NUST(2,1) = NUST(2,1) + RSNH4(4)
      END IF
      NUST(3,1) = DNUST2(3)
      NUST(4,1) = DNUST2(4)
      IF (ADPOFG .EQ. 1) THEN
        NUST(4,1) = NUST(4,1) + RSPO4(4)
      END IF
C
C     initialize nutrient flux if nutrient is not simulated
      IF (TAMFG .EQ. 0) THEN
        NUCF1(2,1)= 0.0
        DO 120 N = 1,NEXITS
          OTAM(N)= 0.0
 120    CONTINUE
      END IF
      IF (ADNHFG .EQ. 0) THEN
        DO 130 I = 1,4
          ROSNH4(I) = 0.0
          DSNH4(I) = 0.0
          ADNH4(I) = 0.0
 130    CONTINUE
        DO 140 N = 1, NEXITS
          OSNH4(N,1) = 0.0
          OSNH4(N,2) = 0.0
          OSNH4(N,3) = 0.0
          OSNH4(N,4) = 0.0
 140    CONTINUE
      END IF
C
      IF (NO2FG .EQ. 0) THEN
        NUCF1(3,1)= 0.0
        DO 150 N= 1,NEXITS
          ONO2(N) = 0.0
 150    CONTINUE
      END IF
C
      IF (PO4FG .EQ. 0) THEN
        NUCF1(4,1)= 0.0
        DO 160 N = 1,NEXITS
          OPO4(N) = 0.0
 160    CONTINUE
      END IF
      IF (ADPOFG .EQ. 0) THEN
        DO 170 I = 1,4
          ROSPO4(I) = 0.0
          DSPO4(I) = 0.0
          ADPO4(I) = 0.0
 170    CONTINUE
        DO 180 N = 1,NEXITS
          OSPO4(N,1) = 0.0
          OSPO4(N,2) = 0.0
          OSPO4(N,3) = 0.0
          OSPO4(N,4) = 0.0
 180    CONTINUE
      END IF
C
C     initialize nutrient process fluxes (including ads/des and dep/scour)
      DO 190 I= 1, 6
        NUCF4(I,1)= 0.0
        NUCF5(I,1)= 0.0
        NUCF7(I,1)= 0.0
 190  CONTINUE
      NUCF4(7,1)= 0.0
      NUCF5(7,1)= 0.0
      NUCF5(8,1)= 0.0
      NUCF6(1,1)= 0.0
      DO 210 I= 1, 4
        DO 200 J = 1,2
          NUCF3(I,J,1)= 0.0
          NUCF8(I,J,1)= 0.0
 200    CONTINUE
 210  CONTINUE
C
      IF (OUTLEV.GT.1) THEN
        WRITE (MESSU,2030)
      END IF
C
      RETURN
      END
C
C     4.2(3).7.2
C
      SUBROUTINE   NUTRX
C
C     + + + PURPOSE + + +
C     Determine primary inorganic nitrogen and phosphorus balances
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION NUTRX2 + + +
      INCLUDE      'crhnu.inc'
      INCLUDE      'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      J,K,FPT,I,N,REQFG,TSSUB(2),FLGVAL
      REAL         NH3VLT,BENTAM,BENPO4,DECNIT,DECPO4,DODEMD,TAMNIT,
     $             NO2NTC,NO3NIT,NO3DE,VOLSP,TOTPM1,TOTPM2,TOTPM3,
     $             TOTNM1,TOTNM2,TOTNM3,TWKELV,AVDEPM,DUMXXX,INNO3,
     $             INTAM,INPO4,NUADFX,NUADCN
      DOUBLE PRECISION DTAM,DNO2,DNO3,DPO4
      CHARACTER*6 OPTYP,TSNAM,SECNAM,MSECNM,OPFGNM
      CHARACTER*20 NUTID(2)
C
C     + + + FUNCTIONS + + +
      REAL         DAYVAL
C
C     + + + EXTERNALS + + +
      EXTERNAL     DAYVAL,ADVECT,ADVNUT,AMMION,BENTH,HREQTS
      EXTERNAL     NH3VOL,NITRIF,DENIT,DECBAL,ADDSNU
C
C     + + + DATA INITIALIZATIONS + + +
      DATA NUTID/'NH4                 ','PO4                 '/
      DATA TSSUB/1,1/
      DATA OPTYP,SECNAM/'RCHRES','NUTRX '/
C
C     + + + END SPECIFICATIONS + + +
C
C     single precision version of vol
C
      VOLSP= VOL
C
C     get time series
      IF (PRECFP .GE. 1) THEN
C       precipitation is input
        PREC= PAD(PRECFP+IVL1)
      ELSE
C       no precipitation
        PREC= 0.0
      END IF
      IF (HYDRFG .NE. 1) THEN
C       section hydr is inactive, so sarea must be on the pad if needed
CTHJ        SAREA= PAD(SAFP+IVL1)
        REQFG= 3
        TSNAM= 'SAREA '
        MSECNM= 'HYDR  '
        CALL HREQTS (SAFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,RCHNO,
     I               TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,FLGVAL,
     O               SAREA)
      END IF
C
C     compute atmospheric deposition influx
      DO 10 I= 1, 3
        N= 2*(I-1)+ 1
C       dry deposition
        IF (NUADFG(N) .LE. -1) THEN
CTHJ          NUADDR(I)= SAREA*PAD(NUAFFP(I)+IVL1)
          REQFG= 4
          TSNAM= 'NUADFX'
          OPFGNM= 'NUADFG'
          CALL HREQTS (NUAFFP(I),IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,
     I                 RCHNO,TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,
     I                 NUADFG(N),
     O                 NUADFX)
          NUADDR(I)= SAREA*NUADFX
        ELSE IF (NUADFG(N) .GE. 1) THEN
          NUADDR(I)= SAREA*DAYVAL(NUAFXM(MON,I),NUAFXM(NXTMON,I),DAY,
     I                            NDAYS)
        ELSE
          NUADDR(I)= 0.0
        END IF
C       wet deposition
        IF (NUADFG(N+1) .LE. -1) THEN
CTHJ      NUADWT(I)= PREC*SAREA*PAD(NUACFP(I)+IVL1)
          REQFG= 4
          TSNAM= 'NUADCN'
          OPFGNM= 'NUADFG'
          CALL HREQTS (NUACFP(I),IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,
     I                 RCHNO,TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,
     I                 NUADFG(N+1),
     O                 NUADCN)
          NUADWT(I)= PREC*SAREA*NUADCN
        ELSE IF (NUADFG(N+1) .GE. 1) THEN
          NUADWT(I)= PREC*SAREA*DAYVAL(NUACNM(MON,I),NUACNM(NXTMON,I),
     I                                 DAY,NDAYS)
        ELSE
          NUADWT(I)= 0.0
        END IF
        NUADEP(I)= NUADDR(I)+ NUADWT(I)
 10   CONTINUE
C
C     get inflowing material from pad
      IF (INO3FP .GT. 0) THEN
        INO3= PAD(INO3FP + IVL1)
      ELSE
        INO3= 0.0
      END IF
      TNUIF(1)= INO3
C
      INNO3= INO3+ NUADEP(1)
C
C     advect nitrate
      DNO3= NO3
      CALL ADVECT
     I            (INNO3,VOLS,SROVOL,VOL,EROVOL,SOVOL,
     I             EOVOL,NEXITS,
     M             DNO3,
     O             RONO3,ONO3)
      NO3= DNO3
      TNUCF1(1)= RONO3
      IF (NEXITS .GT. 1) THEN
        DO 15 N= 1, NEXITS
          TNUCF2(N,1)= ONO3(N)
 15     CONTINUE
      END IF
C
      IF (TAMFG .NE. 0) THEN
        IF (ITAMFP .GT. 0) THEN
          ITAM= PAD(ITAMFP + IVL1)
        ELSE
          ITAM= 0.0
        END IF
C
        INTAM= ITAM+ NUADEP(2)
C
C       advect total ammonia
        DTAM= TAM
        CALL ADVECT
     I              (INTAM,VOLS,SROVOL,VOL,EROVOL,SOVOL,
     I               EOVOL,NEXITS,
     M               DTAM,
     O               ROTAM,OTAM)
        TAM= DTAM
      END IF
C
      IF (NO2FG .NE. 0) THEN
        IF (INO2FP .GT. 0) THEN
          INO2= PAD(INO2FP + IVL1)
        ELSE
          INO2= 0.0
        END IF
        TNUIF(3)= INO2
C
C       advect nitrite
        DNO2= NO2
        CALL ADVECT
     I              (INO2,VOLS,SROVOL,VOL,EROVOL,SOVOL,
     I               EOVOL,NEXITS,
     M               DNO2,
     O               RONO2,ONO2)
        NO2= DNO2
        TNUCF1(3)= RONO2
        IF (NEXITS .GT. 1) THEN
          DO 17 N= 1, NEXITS
            TNUCF2(N,3)= ONO2(N)
 17       CONTINUE
        END IF
      END IF
C
      IF (PO4FG .NE. 0) THEN
        IF (IPO4FP .GT. 0) THEN
          IPO4= PAD(IPO4FP + IVL1)
        ELSE
          IPO4= 0.0
        END IF
C
        INPO4= IPO4+ NUADEP(3)
C
C       advect ortho-phosphorus
        DPO4= PO4
        CALL ADVECT
     I              (INPO4,VOLS,SROVOL,VOL,EROVOL,SOVOL,
     I               EOVOL,NEXITS,
     M               DPO4,
     O               ROPO4,OPO4)
        PO4= DPO4
      END IF
C
      IF (ADPOFG .NE. 0) THEN
C       advect adsorbed phosphate
C
C       zero the accumulators
        ISPO4(4)= 0.0
        DSPO4(4)= 0.0
        ROSPO4(4)= 0.0
        IF (NEXITS .GT. 1) THEN
          DO 18 N= 1, NEXITS
            OSPO4(N,4)= 0.0
 18       CONTINUE
        END IF
C
C       repeat for each sediment fraction
C
        DO 20 J= 1,3
C         get data on sediment-associated phosphate
          FPT= ISPOFP(J)
C
          IF (FPT .GT. 0) THEN
            ISPO4(J)= PAD(FPT + IVL1)
          ELSE
            ISPO4(J)= 0.0
          END IF
C
          CALL ADVNUT
     I                  (ISPO4(J),RSED(J),RSED(J +3),DEPSCR(J),
     I                   ROSED(J),OSED(1,J),NEXITS,RCHNO,
     I                   MESSU,MSGFL,DATIM,
     I                   NUTID(2),J,RSPO4(J),RSPO4(J + 4),BPO4(J),
     M                   NUECNT(3),
     O                   SPO4(J),DSPO4(J),
     O                   ROSPO4(J),OSPO4(1,J))
C
          ISPO4(4)= ISPO4(4)+ ISPO4(J)
          DSPO4(4)= DSPO4(4)+ DSPO4(J)
          ROSPO4(4)= ROSPO4(4)+ ROSPO4(J)
          IF (NEXITS .GT. 1) THEN
            DO 19 N= 1, NEXITS
              OSPO4(N,4)= OSPO4(N,4)+ OSPO4(N,J)
 19         CONTINUE
          END IF
 20     CONTINUE
C
        TNUIF(4)= IPO4+ ISPO4(4)
        TNUCF1(4)= ROPO4+ ROSPO4(4)
        IF (NEXITS .GT. 1) THEN
          DO 25 N= 1, NEXITS
            TNUCF2(N,4)= OPO4(N)+ OSPO4(N,4)
 25       CONTINUE
        END IF
      ELSE
C       no adsorbed fraction
        TNUIF(4)= IPO4
        TNUCF1(4)= ROPO4
        IF (NEXITS .GT. 1) THEN
          DO 27 N= 1, NEXITS
            TNUCF2(N,4)= OPO4(N)
 27       CONTINUE
        END IF
      END IF
C
      IF (TAMFG .NE. 0 .AND. ADNHFG .NE. 0) THEN
C       advect adsorbed ammonium
C
C       zero the accumulators
        ISNH4(4)= 0.0
        DSNH4(4)= 0.0
        ROSNH4(4)= 0.0
        IF (NEXITS .GT. 1) THEN
          DO 28 N= 1, NEXITS
            OSNH4(N,4)= 0.0
 28       CONTINUE
        END IF
C
C       repeat for each sediment fraction
C
        DO 30 J= 1,3
C         get data on sediment-associated ammonium
          FPT= ISNHFP(J)
C
          IF (FPT .GT. 0) THEN
            ISNH4(J)= PAD(FPT + IVL1)
          ELSE
            ISNH4(J)= 0.0
          END IF
C
          CALL ADVNUT
     I                  (ISNH4(J),RSED(J),RSED(J +3),DEPSCR(J),
     I                   ROSED(J),OSED(1,J),NEXITS,RCHNO,
     I                   MESSU,MSGFL,DATIM,
     I                   NUTID(1),J,RSNH4(J),RSNH4(J + 4),BNH4(J),
     M                   NUECNT(3),
     O                   SNH4(J),DSNH4(J),
     O                   ROSNH4(J),OSNH4(1,J))
C
          ISNH4(4)= ISNH4(4) + ISNH4(J)
          DSNH4(4)= DSNH4(4) + DSNH4(J)
          ROSNH4(4)= ROSNH4(4) + ROSNH4(J)
          IF (NEXITS .GT. 1) THEN
            DO 29 N= 1, NEXITS
              OSNH4(N,4)= OSNH4(N,4)+ OSNH4(N,J)
 29         CONTINUE
          END IF
C
 30     CONTINUE
C
        TNUIF(2)= ITAM+ ISNH4(4)
        TNUCF1(2)= ROTAM+ ROSNH4(4)
        IF (NEXITS .GT. 1) THEN
          DO 35 N= 1, NEXITS
            TNUCF2(N,2)= OTAM(N)+ OSNH4(N,4)
 35       CONTINUE
        END IF
      ELSE
C       no adsorbed fraction
        TNUIF(2)= ITAM
        TNUCF1(2)= ROTAM
        IF (NEXITS .GT. 1) THEN
          DO 37 N= 1, NEXITS
            TNUCF2(N,2)= OTAM(N)
 37       CONTINUE
        END IF
      END IF
C
      IF (TAMFG .NE. 0) THEN
C       calculate ammonia ionization in water column
C
C       get ph values
C
C       casentry phflag
        GO TO (40,50,60) , PHFLAG
C
C       case 1
 40     CONTINUE
C         time series value
          IF (PHFG .EQ. 1) THEN
C           use value computed in last time step
            PHVAL= PH
          ELSE
C           need an input time series
            PHVAL= PAD(PHVFP + IVL1)
            REQFG= 5
            TSNAM= 'PHVAL '
            MSECNM= 'PHCARB'
            OPFGNM= 'PHFLAG'
            CALL HREQTS (PHVFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,
     I                   RCHNO,TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,
     I                   PHFLAG,
     O                   PHVAL)
          END IF
          GO TO 70
C
C       case 2
 50     CONTINUE
C         user-supplied value, read in by run interpreter
          GO TO 70
C
C       case 3
 60     CONTINUE
C         12 user-supplied monthly values
C
          IF (DAYFG .EQ. 1) THEN
C           interpolate a new value
            PHVAL= DAYVAL(PHVALM(MON),PHVALM(NXTMON),DAY,NDAYS)
          END IF
C
 70     CONTINUE
C       end case
C
C       compute ammonia ionization
C
        CALL AMMION
     I              (TW,PHVAL,TAM,
     O               NH3,NH4)
C
      END IF
C
      IF (AVDEPE .GT. 0.17) THEN
        IF (BENRFG .NE. 0) THEN
C         simulate benthal release of inorganic nitrogen and
C         ortho-phosphorus; and compute associated fluxes
          IF (TAMFG .NE. 0) THEN
            CALL BENTH
     I                 (DOX,ANAER,BRTAM,SCRFAC,DEPCOR,
     M                  TAM,
     O                  BENTAM)
            BNRTAM= BENTAM*VOLSP
          ELSE
C           benthal release of inorganic n not simulated
          END IF
C
          IF (PO4FG .NE. 0) THEN
            CALL BENTH
     I                 (DOX,ANAER,BRPO4,SCRFAC,DEPCOR,
     M                  PO4,
     O                  BENPO4)
            BNRPO4= BENPO4*VOLSP
          END IF
        ELSE
C         benthal releases are not considered
        END IF
C
        IF (TAMFG .NE. 0) THEN
          IF (AMVFG .NE. 0) THEN
C           compute ammonia volatilization
C
C           get windspeed value
CBRB        WIND= PAD(WDFP + IVL1)
            REQFG= 2
            TSNAM= 'WIND  '
            CALL HREQTS (WDFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,
     I                   RCHNO,TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,
     I                   FLGVAL,
     O                   WIND)
C
C           convert water temperature to degrees kelvin and depth to meters
            TWKELV= TW+ 273.16
            AVDEPM= AVDEPE * .3048
C
            CALL NH3VOL
     I                  (EXPNVG,EXPNVL,KOREA,WIND,DELT60,DELTS,
     I                   AVDEPM,TWKELV,TW,PHVAL,
     M                   TAM,
     O                   NH3VLT)
C
            VOLNH3= -NH3VLT*VOLSP
          ELSE
            VOLNH3= 0.0
          END IF
C
C         calculate amount of nitrification; nitrification does not
C         take place if the do concentration is less than 2.0 mg/l
          CALL NITRIF
     I                (KTAM20,TCNIT,TW,NO2FG,KNO220,
     M                 TAM,NO2,NO3,DOX,
     O                 DODEMD,TAMNIT,NO2NTC,NO3NIT)
C
C         compute nitrification fluxes
          NITDOX= -DODEMD*VOLSP
          NITTAM= -TAMNIT*VOLSP
          NITNO2= NO2NTC*VOLSP
          NITNO3= NO3NIT*VOLSP
        ELSE
C         ammonia is not simulated; volatilization and
C         nitrification of tam are not considered
C
        END IF
C
        IF (DENFG .NE. 0) THEN
C         consider denitrification processes, and compute associated fluxes
          CALL DENIT
     I                (KNO320,TCDEN,TW,DOX,DENOXT,
     M                 NO3,
     O                 NO3DE)
C
          DENNO3= -NO3DE*VOLSP
C
        END IF
C
C       calculate amount of inorganic constituents released by
C       bod decay in reach water
        DECNIT= BODOX*CVON
        DECPO4= BODOX*CVOP
        DECCO2= BODOX*CVOC
C
C       update state variables of inorganic constituents which
C       are end products of bod decay; and compute associated fluxes
        CALL DECBAL
     I              (TAMFG,PO4FG,DECNIT,DECPO4,
     M               TAM,NO3,PO4)
C
        IF (TAMFG .NE. 0) THEN
          BODTAM= DECNIT*VOLSP
        ELSE
          BODNO3= DECNIT*VOLSP
        END IF
C
        IF (PO4FG .EQ. 1) THEN
          BODPO4= DECPO4*VOLSP
        END IF
C
        IF (PO4FG .NE. 0 .AND. SEDFG .NE. 0 .AND. ADPOFG .NE. 0) THEN
C         compute adsorption/desorption of phosphate
C
          CALL ADDSNU
     I                 (VOLSP,RSED(1),ADPOPM(1),
     M                  PO4,SPO4(1),DUMXXX,
     O                  ADPO4(1))
        ELSE
C         computation of phosphate adsorption/desorption is not necessary
        END IF
C
        IF (TAMFG .NE. 0 .AND. SEDFG .NE. 0 .AND. ADNHFG .NE. 0) THEN
C         compute adsorption/desorption of ammonium
C
C         first compute ammonia ionization
C
          CALL AMMION
     I                (TW,PHVAL,TAM,
     O                 NH3,NH4)
          CALL ADDSNU
     I                (VOLSP,RSED(1),ADNHPM(1),
     M                 NH4,SNH4(1),TAM,
     O                 ADNH4(1))
C
C         then re-compute ammonia ionization
C
          CALL AMMION
     I                (TW,PHVAL,TAM,
     O                 NH3,NH4)
        ELSE
C         computation of ammonium adsorption/desorption is not wanted
        END IF
C
      ELSE
C       too little water is in reach to warrant simulation
C       of quality processes
        DECNIT= 0.0
        DECPO4= 0.0
        DECCO2= 0.0
        NITDOX= 0.0
        DENBOD= 0.0
        NITTAM= 0.0
        BNRTAM= 0.0
        VOLNH3= 0.0
        BODTAM= 0.0
        NITNO2= 0.0
        NITNO3= 0.0
        DENNO3= 0.0
        BODNO3= 0.0
        BNRPO4= 0.0
        BODPO4= 0.0
C
        DO 80 K= 1,4
          ADNH4(K)= 0.0
          ADPO4(K)= 0.0
 80     CONTINUE
C
      END IF
      TOTDOX= READOX+ BODDOX+ BENDOX+ NITDOX
      TOTBOD= DECBOD+ BNRBOD+ SNKBOD+ DENBOD
      TOTNO3= NITNO3+ DENNO3+ BODNO3
      TOTTAM= NITTAM+ VOLNH3+ BNRTAM+ BODTAM
      TOTPO4= BNRPO4+ BODPO4
C
      IF (PO4FG .NE. 0 .AND. SEDFG .NE. 0 .AND. ADPOFG .NE. 0) THEN
C       find total quantity of phosphate on various forms of sediment
        TOTPM1= 0.0
        TOTPM2= 0.0
        TOTPM3= 0.0
C
        DO 90 J= 1,3
C         compute mass of phosphate adsorbed to each suspended fraction
          RSPO4(J)    = SPO4(J)*RSED(J)
C         compute mass of phosphate adsorbed to each bed fraction
          RSPO4(J + 4)= BPO4(J)*RSED(J + 3)
C         compute total mass of phosphate on each sediment fraction
          RSPO4(J + 8)= RSPO4(J) + RSPO4(J + 4)
C
          TOTPM1= TOTPM1 + RSPO4(J)
          TOTPM2= TOTPM2 + RSPO4(J + 4)
          TOTPM3= TOTPM3 + RSPO4(J + 8)
 90     CONTINUE
C
C       compute total suspended phosphate
        RSPO4(4) = TOTPM1
C       compute total bed phosphate
        RSPO4(8) = TOTPM2
C       compute total sediment-associated phosphate
        RSPO4(12)= TOTPM3
      END IF
C
      IF (TAMFG .NE. 0 .AND. SEDFG .NE. 0 .AND. ADNHFG .NE. 0) THEN
C       find total amount of ammonium on various forms of sediment
        TOTNM1= 0.0
        TOTNM2= 0.0
        TOTNM3= 0.0
C
        DO 100 J= 1,3
C         compute mass of ammonium adsorbed to each suspended fraction
          RSNH4(J)    = SNH4(J)*RSED(J)
C         compute mass of ammonium adsorbed to each bed fraction
          RSNH4(J + 4)= BNH4(J)*RSED(J + 3)
C         compute total mass of ammonium on each sediment fraction
          RSNH4(J + 8)= RSNH4(J) + RSNH4(J + 4)
C
          TOTNM1= TOTNM1 + RSNH4(J)
          TOTNM2= TOTNM2 + RSNH4(J + 4)
          TOTNM3= TOTNM3 + RSNH4(J + 8)
 100    CONTINUE
C
C       compute total suspended ammonium
        RSNH4(4) = TOTNM1
C       compute total bed ammonium
        RSNH4(8) = TOTNM2
C       compute total sediment-associated ammonium
        RSNH4(12)= TOTNM3
      END IF
C
      RETURN
      END
C
C     4.2(3).7.2.8
C
      SUBROUTINE   ADDSNU
     I                    (VOL,RSED,ADPM,
     M                     DNUT,SNUT,DNUTXX,
     O                     ADNUT)
C
C     + + + PURPOSE + + +
C     Simulate exchange of nutrient (phosphate or ammonium) between the
C     dissolved state and adsorption on suspended sediment- 3 adsorption
C     sites are considered: 1- suspended sand  2- susp. silt
C     3- susp. clay
C      assumes instantaneous linear equilibrium
C
C     + + + DUMMY ARGUMENTS + + +
      REAL       VOL,RSED(3),ADPM(3),
     $           DNUT,SNUT(3),ADNUT(4),DNUTXX
C
C     + + + ARGUMENT DEFINITIONS + + +
C     VOL    - volume of water in reach above bed
C     RSED   - storage of sediment fraction suspended in water column
C     ADPM   - nutrient partition coefficient for sediment fraction j
C     DNUT   - dissolved nutrient concentration in reach water
C     SNUT   - concentration of nutrient adsorbed to sediment fractions
C     DNUTXX - ???
C     ADNUT  - flux of nutrient adsorbed (+) or desorbed (-) from each
C              suspended sediment fraction (1-3) and from total suspended
C              sediment (4)
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    J
      REAL       DENOM,NUM,TEMP,DNUTIN
C
C     + + + END SPECIFICATIONS + + +
C
      IF (VOL .GT. 0.0) THEN
C       adsorption/desorption can take place
C
C       establish nutrient equilibrium between reach water and suspended
C       sediment; first find the new dissolved nutrient conc. in reach water
        DNUTIN= DNUT
        NUM   = VOL*DNUT
        DENOM = VOL
C
        DO 20 J=1,3
C
          IF (RSED(J) .GT. 0.0) THEN
C           accumulate terms for numerator and denominator
C           in dnut equation
            NUM  = NUM + SNUT(J)*RSED(J)
            DENOM= DENOM + ADPM(J)*RSED(J)
          END IF
C
 20     CONTINUE
C
C       calculate new dissolved concentration-units are mg/l
        DNUT  = NUM/DENOM
C       also calculate new tam conc if doing nh4 adsorption
        DNUTXX= DNUTXX - (DNUTIN - DNUT)
C
C       calculate new conc on each sed class and the corresponding
C       adsorption/desorption flux
        ADNUT(4)= 0.0
C
        DO 30 J=1,3
C
          IF (RSED(J) .GT. 0.0) THEN
C           this sediment class is present-calculate data
C           pertaining to it
C
C           new concentration
            TEMP    = DNUT*ADPM(J)
C
C           quantity of material transferred
            ADNUT(J)= (TEMP - SNUT(J))*RSED(J)
            SNUT(J) = TEMP
C
C           accumulate total adsorption/desorption flux above bed
            ADNUT(4)= ADNUT(4) + ADNUT(J)
C
          ELSE
C           this sediment class is absent
            ADNUT(J)= 0.0
C           snut(j) is unchanged-"undefined"
          END IF
 30     CONTINUE
C
      ELSE
C       no water, no adsorption/desorption
C
        DO 40 J= 1, 7
          ADNUT(J)= 0.0
C         snut(1 thru 3) and dnut should already have been set to
C         undefined values
 40     CONTINUE
C
      END IF
C
      RETURN
      END
C
C     4.2(3).7.2.5
C
      SUBROUTINE   ADVNUT
     I                    (ISNUT,RSED,BSED,DEPSCR,ROSED,OSED,NEXITS,
     I                     RCHNO,MESSU,MSGFL,DATIM,
     I                     NUTID,J,RSNUTS,RBNUTS,BNUT,
     M                     ECNT,
     O                     SNUT,DSNUT,ROSNUT,OSNUT)
C
C     + + + PURPOSE + + +
C     Simulate the advective processes, including deposition and
C     scour for the inorganic nutrient adsorbed to one sediment size
C     fraction
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      ECNT(2),MSGFL,J,MESSU,
     $             NEXITS,RCHNO,DATIM
      REAL         BSED,BNUT,DEPSCR,DSNUT,ISNUT,OSED(5),OSNUT(5),
     $             RBNUTS,ROSED,ROSNUT,RSED,RSNUTS,SNUT
      CHARACTER*20 NUTID
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ISNUT  - inflow of adsorbed nutrient to the rchres as a result
C              of inflowing sediment fraction, expressed as mg.ft3/l.ivl
C              or mg.m3/l.ivl
C     RSED   - amount of sediment fraction in suspension at end of
C              interval expressed in mg.ft3/l or mg.m3/l
C     BSED   - amount of sediment fraction in bed at end of interval
C              expressed in mg.ft3/l or mg.m3/l
C     DEPSCR - amount of depositon (-) or scour (+) of sediment fraction
C              occurring during interval, expressed as mg.ft3/l or mg.m3/l
C     ROSED  - amount of sediment fraction contained in outflow from the
C              rchres during interval, expressed as mg.ft3/l.ivl or
C              mg.m3/l.ivl
C     OSED   - amount of sediment fraction contained in outflow to an
C              individual gate during interval, expressed as mg.ft3/l.ivl
C              or mg.m3/l.ivl
C     NEXITS - number of outflow gates from rchres
C     RCHNO  - ???
C     MESSU  - ftn unit no. to be used for printout of messages
C     MSGFL  - fortran unit number of HSPF message file
C     NUTID  - ???
C     J      - ???
C     RSNUTS - storage of inorganic nutrient on suspended sediment fraction
C              at start of interval expressed in mg.ft3/l or mg.m3/l
C     RBNUTS - storage of inorganic nutrient on bed sediment fraction
C              at start of interval expressed in mg.ft3/l or mg.m3/l
C     BNUT   - user-specified concentration of adsorbed inorganic nutrient
C              on bed sediment fraction expressed in mg/mg
C     ECNT   - ???
C     SNUT   - concentration of adsorbed inorganic nutrient on suspended
C              sediment fraction expressed in mg/mg
C     DSNUT  - amount of inorganic nutrient entering or leaving the water
C              column due to scour or depositon of a sediment fraction
C              expressed in mg.ft3/l.ivl or mg.m3/l.ivl
C     ROSNUT - amount of inorganic nutrient leaving the rchres due to
C              outflow of sediment fraction, expressed in mg.ft3/l.ivl or
C              mg.m3/l.ivl
C     OSNUT  - amount of inorganic nutrient leaving the rchres through
C              an individual gate due to outflow of sediment fraction,
C              expressed as mg.ft3/l.ivl or mg.m3/l.ivl
C     DATIM  - date and time
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      SCLU,SGRP,L,N,I20
      REAL         DENOM
      CHARACTER*20 CHSTR
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (CHSTR,CHSTR1)
      CHARACTER*1  CHSTR1(20)
C
C     + + + INTRINSICS + + +
      INTRINSIC  ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL     OMSG,OMSTI,OMSTC,OMSTR,OMSTD
C
C     + + + END SPECIFICATIONS + + +
C
      I20  = 20
      SCLU = 348
      IF (DEPSCR.LT.0.0) THEN
C       there was sediment scour during the interval
C
C       compute flux of nutrient mass into water column with scoured
C       sediment fraction
        DSNUT = BNUT*DEPSCR
C
C       calculate concentration in suspension-under these conditions,
C       denominator should never be zero
        SNUT  = (ISNUT + RSNUTS - DSNUT)/(RSED + ROSED)
        ROSNUT= ROSED*SNUT
C
      ELSE
C       there was deposition or no scour/deposition
C       during the interval
        DENOM= RSED + DEPSCR + ROSED
C
        IF ((ABS(DENOM)).LE.0.0) THEN
C         there was no sediment in suspension during the interval
          SNUT  = -1.0E30
          ROSNUT= 0.0
          DSNUT = 0.0
C
C         fix sed-nut problem caused by very small sediment loads
C         that are stored in wdm file as zero (due to wdm attribute tolr > 0.0
C         when adsorbed nut load is not zero;
C         changed comparison from 0.0 to 1.0e-3; this should not cause
C         any mass balance errors since the condition is not likely
C         to exist over a long period and will be insignificant compared to
C         the total mass over a printout period; note that
C         1.0e-3 mg*ft3/l is 0.028 mg (a very, very small mass)
C
          IF ((ABS(ISNUT)) .GT. 1.0E-3 .OR.
     $        (ABS(RSNUTS)) .GT. 1.0E-3) THEN
C           error-under these conditions these values should be zero
C
            CALL OMSTD (DATIM)
            CALL OMSTI (RCHNO)
            CALL OMSTR (ISNUT)
            CALL OMSTR (RSNUTS)
            CHSTR = NUTID
            CALL OMSTC (I20,CHSTR1)
            CALL OMSTI (J)
            SGRP = 1
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 ECNT(1))
          END IF
C
        ELSE
C         there was some suspended sediment during the interval
C
C         calculate conc on suspended sed
          SNUT  = (ISNUT + RSNUTS)/DENOM
          ROSNUT= ROSED*SNUT
          DSNUT = DEPSCR*SNUT
C
          IF ((ABS(RSED)).LE.0.0) THEN
C           rchres ended up without any suspended sediment-revise
C           value for snut, but values obtained for rosnut, and dsnut
C           are still ok
            SNUT= -1.0E30
          END IF
C
        END IF
C
C       calculate conditions on the bed
C
        IF ((ABS(BSED)).LE.0.0) THEN
C         no bed sediments at end of interval
C
          IF ((ABS(DSNUT)).GT.0.0.OR.
     $        (ABS(RBNUTS)).GT.0.0) THEN
C           error-under this condition these values should be zero
C
            CALL OMSTD (DATIM)
            CALL OMSTI (RCHNO)
            CALL OMSTR (DSNUT)
            CALL OMSTR (RBNUTS)
            CHSTR = NUTID
            CALL OMSTC (I20,CHSTR1)
            CALL OMSTI (J)
            SGRP = 2
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 ECNT(2))
          END IF
C
        ELSE
C         there is bed sediment at the end of the interval; bed
C         concentration of nutrient is as defined by user
        END IF
C
      END IF
C
      IF (NEXITS.GT.1) THEN
C       compute outflow through each individual exit
C
        IF ((ABS(ROSED)).LE.0.0) THEN
C         all zero
          DO 120 L=1,5
            OSNUT(L)=0.0
 120      CONTINUE
        ELSE
          DO 140 N=1,NEXITS
            OSNUT(N)= ROSNUT*OSED(N)/ROSED
 140      CONTINUE
        END IF
C
      END IF
C
      RETURN
      END
C
C     4.2(3).7.2.3
C
      SUBROUTINE   AMMION
     I                    (TW,PH,TAM,
     O                     NH3,NH4)
C
C     + + + PURPOSE + + +
C     Simulate ionization of ammonia to ammonium using empirical relationships
C     developed by Loehr, 1973
C
C     + + + DUMMY ARGUMENTS + + +
      REAL       TW,PH,TAM,NH3,NH4
C
C     + + + ARGUMENT DEFINITIONS + + +
C     TW     - water temperature in degrees c
C     PH     - ph of water
C     TAM    - total ammonia (nh3 + nh4) expressed as mg n/l
C     NH3    - free or un-ionized ammonia expressed as mg nh3-n/l
C     NH4    - ammonium (nh4) expressed as mg nh4-n/l
C
C     + + + LOCAL VARIABLES + + +
      REAL       RATIO,FRAC,TWX,PHX
C
C     + + + INTRINSICS + + +
      INTRINSIC  LOG
C
C     + + + END SPECIFICATIONS + + +
C
      IF (TAM .GE. 0.0) THEN
C       tam is defined, compute fractions
C
C       adjust very low or high values of water temperature to fit limits of dat
C       used to develop empirical relationship
        IF (TW .LT. 5.0) THEN
          TWX= 5.0
        ELSE
          TWX= TW
          IF (TW .GT. 35.0) THEN
            TWX= 35.0
          END IF
        END IF
C
        IF (PH .LT. 4.0) THEN
          PHX= 4.0
        ELSE
          PHX= PH
          IF (PH .GT. 10.0) THEN
            PHX= 10.0
          END IF
        END IF
C
C       compute ratio of ionization constant values for aqueous ammonia
C       and water at current water temperatue
        RATIO= (-3.39753*LOG(0.02409*TWX))*10.**(9.)
C
C       compute fraction of total ammonia that is un-ionized
        FRAC= 10.**(PHX)/(10.**(PHX) + RATIO)
C
C       update nh3 and nh4 state variables to account for ionization
        NH3=  FRAC*TAM
        NH4=  TAM - NH3
C
      ELSE
C       tam conc undefined
        NH3= -1.0E30
        NH4= -1.0E30
      END IF
C
      RETURN
      END
C
C     4.2(3).7.2.6
C
      SUBROUTINE   DENIT
     I                   (KNO320,TCDEN,TW,DOX,DENOXT,
     M                    NO3,
     O                    DENNO3)
C
C     + + + PURPOSE + + +
C     Calculate amount of denitrification; denitrification does not
C     take place if the DO concentration is above user-specified
C     threshold DO value (DENOXT)
C
C     + + + DUMMY ARGUMENTS  + + +
      REAL       KNO320,TCDEN,TW,DOX,DENOXT,NO3,DENNO3
C
C     + + + ARGUMENT DEFINITIONS + + +
C     KNO320 - unit denitrification rate of nitrate at 20 degrees c,
C              expressed as /ivl
C     TCDEN  - temperature correction coefficient for denitrification
C     TW     - water temperature, degrees c
C     DOX    - dissolved oxygen concentration expressed as mg/l
C     DENOXT - threshold value for dissolved oxygen concentration
C              above which denitrification does not occur
C     NO3    - dissolved nitrate concentration expressed as mg/l
C     DENNO3 - flux of nitrate denitrified to nitrogen gas expressed
C              as mg n/l.ivl
C
C     + + + END SPECIFICATIONS + + +
C
      IF (DOX .LE. DENOXT) THEN
C       calculate amount of no3 denitirified to nitrogen gas
        DENNO3= 0.0
        IF (NO3 .GT. 0.001) THEN
          DENNO3= KNO320*(TCDEN**(TW-20.))*NO3
          NO3   = NO3 - DENNO3
C
          IF (NO3 .LT. 0.001) THEN
C           adjust amount of no3 denitrified so that no3 state variable
C           is not a negative number; set no3 to a value of .001 mg/l
            DENNO3= DENNO3 + NO3 - .001
            NO3   = .001
          END IF
        END IF
C
      ELSE
C       denitrification does not occur; set amount of no3 denitrified
C       to zero
        DENNO3= 0.0
C
      END IF
C
      RETURN
      END
C
C     4.2(3).7.2.4.1
C
      SUBROUTINE   HCINTP
     I                    (PHVAL,TW,
     O                     HCNH3)
C
C     + + + PURPOSE + + +
C     Calculate Henry's constant for ammonia based on pH and water
C     temperature
C
C     + + + DUMMY ARGUMENTS + + +
      REAL       PHVAL,TW,HCNH3
C
C     + + + ARGUMENT DEFINITIONS + + +
C     PHVAL  - ph of water column
C     TW     - water temperature expressed in degrees c
C     HCNH3  - henry's constant for ammonia, expressed as atm.m3/g.mol
C
C     + + + LOCAL VARIABLES + + +
      REAL       HPLUS,HCMF,XTW(4),XHPLUS(5),YHENC(4,5),YHTMP(5),
     $           YTWTMP(4),TWX
      INTEGER    I,J,I4,I5
C
C     + + + LOCAL VARIABLE DEFINITIONS + + +
C     XTW    - array of temperature values for which henry's constant values
C              are available
C     XHPLUS - array of modified hydrogen ion concentrations for which henry's
C              constant values are available
C     YHEMC  - two dimensional array of temperature- and ph-dependent
C              henry's comnstant values
C
C     + + + EXTERNALS + + +
      EXTERNAL   INTRP1
C
C     + + + DATA INITIALIZATIONS + + +
      DATA   XTW /4.44,15.56,26.67,37.78/
      DATA   XHPLUS /1.,10.,100.,1000.,10000./
      DATA   YHENC /0.000266,0.000754,0.00198,0.00486,0.00266,0.00753,
     $              0.0197,0.0480,0.0263,0.0734,0.186,0.428,0.238,0.586,
     $              1.20,2.05,1.2,1.94,2.65,3.31/
C
C     + + + END SPECIFICATIONS + + +
C
C     adjust very low or very high values of water temperature to fit limits
C     of henry's contant data range
      IF (TW .LT. 4.44) THEN
C       use low temperature range values for henry's constant (4.4 degrees c
C       or 40 degrees f)
        TWX= 4.44
      ELSE
        IF (TW .GT. 37.78) THEN
C         use high temperature range values for henry's constant (37.78
C         degrees c or 100 degrees f)
          TWX= 37.78
        ELSE
C         use unmodified water temperature value in interpolation
          TWX= TW
        END IF
      END IF
C
C     convert ph value to a modified version of hydrogen ion concentration
C     because our interpolation routine cant seem to work with small numbers
      HPLUS= 10.**(PHVAL)/1.0E6
C
C     adjust very low or very high values of hydrogen ion concentration to fit
C     limits of henry's constant data range
      IF (HPLUS .GT. 10000.) THEN
C       use low hydrogen ion concentration range values for henry's constant
        HPLUS= 10000.
      ELSE
        IF (HPLUS .LT. 1.0) THEN
C         use high hydrogen ion concentration range values for henry's
C         constant
          HPLUS= 1.0
        ELSE
C         use unmodified hydrogen ion concentration value in interpolation
        END IF
      END IF
C
C     perform two-dimensional interpolation of henry's constant values to
C     estimate henry's constant for water temperature and ph conditions
C     in water column (based on p. 97 of numerical recipes)
      I4= 4
      I5= 5
      DO 10 I= 1, 4
        DO 20 J= 1, 5
C         copy row into temporary storage
          YHTMP(J)= YHENC(I,J)
 20     CONTINUE
C       perform linear interpolation within row of values
        CALL INTRP1
     I              (XHPLUS,YHTMP,I5,HPLUS,
     O               YTWTMP(I))
 10   CONTINUE
C
C     do final interpolation in remaining dimension
      CALL INTRP1
     I            (XTW,YTWTMP,I4,TWX,
     O             HCMF)
C
C     convert henry's constant from molar fraction form to units of
C     atm.m3/mole:  assume 1) dilute air and water solutions
C                          2) ideal gas law
C                          3  stp i.e., 1 atm total pressure
C                          3) 1 gram water = 1 cm3
C
C      xa(air)                        1
C     --------- * -----------------------------------------
C     xa(water)    (1.e+6 m3/g water)/(18.01 g/mole water)
C
      HCNH3= HCMF*(18.01/1.E+6)
C
      RETURN
      END
C
C     4.2(3).7.2.4.1.1
C
      SUBROUTINE   INTRP1
     I                    (XARR,YARR,LEN,XVAL,
     O                     YVAL)
C
C     + + + PURPOSE + + +
C     Perform one-dimensional interpolation of Henry's constant values
C     for ammonia (based on p. 82 of Numerical Recipes)
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    LEN
      REAL       XARR(LEN),YARR(LEN),XVAL,YVAL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     XARR  - one-dimensional array of independent variable values for
C             which dependent variable values are available in yarr
C     YARR  - one-dimensional array of dependent variable values
C     LEN   - number of elements in xarr and yarr (max = 10,
C             or modify c,d below)
C     XVAL  - value of independent variable for which a value of
C             dependent variable is to be determined by interpolation
C     YVAL  - interpolated value of dependent variable
C
C     + + + INTRINSICS + + +
      INTRINSIC  ABS
C
C     + + + LOCAL VARIABLES + + +
      REAL       C(10),D(10),DIF,DIFT,HO,HP,W,DEN,DYVAL
      INTEGER    I,J,NS
C
C     + + + END SPECIFICATIONS + + +
C
      NS = 1
      DIF= ABS(XVAL-XARR(1))
C     find the index ns of the closest array entry
      DO 10 I= 1, LEN
        DIFT= ABS(XVAL - XARR(I))
        IF (DIFT .LT. DIF) THEN
          NS = I
          DIF= DIFT
        END IF
C       initialize correction array values
        C(I)= YARR(I)
        D(I)= YARR(I)
 10   CONTINUE
C
C     select intial approximation of yval
      YVAL= YARR(NS)
      NS  = NS - 1
C     loop over the current values in correction value arrays (c & d)
C     to update them
      DO 30 J= 1, LEN -1
        DO 20 I= 1, LEN - J
          HO  = XARR(I) - XVAL
          HP  = XARR(I + J) - XVAL
          W   = C(I + 1) - D(I)
          DEN = HO - HP
          DEN = W/DEN
C         update correction array values
          D(I)= HP*DEN
          C(I)= HO*DEN
 20     CONTINUE
C       select correction to yval
        IF (2*NS .LT. LEN-J) THEN
          DYVAL= C(NS + 1)
        ELSE
          DYVAL= D(NS)
          NS   = NS - 1
        END IF
C
C       compute yval
        YVAL= YVAL + DYVAL
 30   CONTINUE
C
      RETURN
      END
C
C     4.2(3).7.2.4
C
      SUBROUTINE   NH3VOL
     I                    (EXPNVG,EXPNVL,KOREA,WIND,DELT60,DELTS,
     I                     AVDEPM,TWKELV,TW,PHVAL,
     M                     TAM,
     O                     NH3VLT)
C
C     + + + PURPOSE + + +
C     calculate ammonia volatilization using two-layer theory
C
C     + + + DUMMY ARGUMENTS + + +
      REAL       EXPNVG,EXPNVL,KOREA,WIND,DELT60,DELTS,
     $           AVDEPM,TWKELV,TW,PHVAL,TAM,NH3VLT
C
C     + + + ARGUMENT DEFINITIONS + + +
C     EXPNVG - exponent to gas film mass transfer equation for ammonia;
C              default value is 1/2 as per mills
C     EXPNVL - exponent to liquid film mass transfer equation for ammonia;
C              default value is 2/3 as per thibodeaux
C     KOREA  - oxygen transfer coefficient expressed in units of per
C              interval
C     WIND   - windspeed expressed as meters per interval
C     DELT60 - conversion from units of per hour to units of per interval
C     DELTS  - ???
C     AVDEPM - average depth of water in reach expressed in meters
C     TWKELV - water temperature expressed in degrees kelvin
C     TW     - water temperature expressed in degrees c
C     PHVAL  - ph of water column
C     TAM    - total ammonia (nh3 + nh4) in mg n/l
C     NH3VLT - flux of ammonia out of reach due to volatilization, expressed
C              in mg/l.ivl
C
C     + + + LOCAL VARIABLES +
      REAL       NH3KL,NH3KG,DOKL,HCNH3,KRINV,KR,WINDSP,KNVOL,CHK
C
C     + + + EXTERNALS + + +
      EXTERNAL   HCINTP
C
C     + + + HISTORY + + +
C     8/2004   BRB corrected divide by zero errors
C
C     + + + END SPECIFICATIONS + + +
C
      IF (TAM .GT. 0.0) THEN
C       convert reaeration coefficient into units needed for computatuion
C       of bulk liquid film gas transfer coefficient (cm/hr) based on
C       average depth of water
        DOKL= KOREA*(AVDEPM*100.)/DELT60
C
C       compute bulk liquid film gas transfer coefficient for ammonia using
C       equation 183 of mccutcheon; 1.8789 equals the ratio of oxygen
C       molecule molecular weight to ammonia molecular weight
        NH3KL= DOKL*(1.8789**(EXPNVL/2.))
C
C       convert wind speed from meters/ivl (wind) to meters/sec (windsp)
        WINDSP= WIND/DELTS
C
C       compute bulk gas film gas transfer coefficient (cm/hr) for ammonia
C       using equation 184 of mccutcheon; the product of the expression
C       (700.*windsp) is expressed in cm/hr; 1.0578 equals the ratio of water
C       molecule molecular weight to ammonia molecular weight
        IF (WINDSP .LE. 0.0) WINDSP = 0.001
        NH3KG= 700.*WINDSP*(1.0578**(EXPNVG/2.))
C
C       compute henry's constant for ammonia as a function of temperature
        CALL HCINTP
     I              (PHVAL,TW,
     O               HCNH3)
C
C       avoid divide by zero errors
        CHK= NH3KL*HCNH3
        IF (CHK .GT. 0.0) THEN
C         compute overall mass transfer coefficient for ammonia (kr) in cm/hr
C         using equation 177 of mccutcheon; first calculate the inverse of kr
C         (krinv); 8.21e-05 equals ideal gas constant value expressed as
C         atm/degrees k mole
          KRINV= (1./NH3KL) + ((8.21E-05)*TWKELV)/(HCNH3*NH3KG)
          KR   = (1./KRINV)
C
C         compute reach-specific gas transfer coefficient (units are /interval)
          KNVOL= (KR/(AVDEPM*100.))*DELT60
        ELSE
C         korea or hcnh3 was zero (or less)
          KNVOL= 0.0
        END IF        
C
C       compute ammonia flux out of reach due to volatilization;  assumes
C       that equilibrium concentration of ammonia is sufficiently small
C       to be considered zero
        NH3VLT = KNVOL*TAM
        IF (NH3VLT .GE. TAM) THEN
          NH3VLT= 0.99*TAM
          TAM= 0.01*TAM
        ELSE
          TAM= TAM - NH3VLT
        END IF
      ELSE
C       no ammonia present; hence, no volatilization occurs
        NH3VLT= 0.0
      END IF
C
      RETURN
      END
C
C     4.2(3).7.2.5
C
      SUBROUTINE   NITRIF
     I                    (KTAM20,TCNIT,TW,NO2FG,KNO220,
     M                     TAM,NO2,NO3,DOX,
     O                     DODEMD,TAMNIT,NO2NTC,NO3NIT)
C
C     + + + PURPOSE + + +
C     Calculate amount of nitrification; nitrification does not
C     take place if the DO concentration is less than 2.0 mg/l
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    NO2FG
      REAL       KTAM20,TCNIT,TW,KNO220,TAM,NO2,NO3,DOX,
     $           DODEMD,TAMNIT,NO2NTC,NO3NIT
C
C     + + + ARGUMENT DEFINITIONS + + +
C     KTAM20 - ???
C     TCNIT  - ???
C     TW     - water temperature in degrees C
C     NO2FG  - ???
C     KNO220 - ???
C     TAM    - total ammonia (nh3 + nh4) in mg n/l
C     NO2    - ???
C     NO3    - dissolved nitrate concentration in mg/l
C     DOX    - dissolved oxygen concentration in mg/l
C     DODEMD - ???
C     TAMNIT - ???
C     NO2NTC - ???
C     NO3NIT - ???
C
C     + + + LOCAL VARIABLES + + +
      REAL       NO2NIT,RHO,RHOC3,RHOC2
C
C     + + + END SPECIFICATIONS + + +
C
      IF (DOX .GE. 2.0) THEN
C       calculate amount of tam oxidized to no2; tamnit is
C       expressed as mg tam-n/l
        TAMNIT= 0.0
        IF (TAM .GT. 0.001) THEN
          TAMNIT= KTAM20*(TCNIT**(TW-20.))*TAM
          TAM   = TAM - TAMNIT
C
          IF (TAM .LT. 0.001) THEN
C           adjust amount of tam oxidized so that tam state variable
C           is not a negative number; set tam to a value of .001 mg/l
            TAMNIT= TAMNIT + TAM - .001
            TAM   = .001
          END IF
        END IF
C
        IF (NO2FG .NE. 0) THEN
C         calculate amount of no2 oxidized to no3; no2nit is
C         expressed as mg no2-n/l
          NO2NIT= 0.0
          IF (NO2 .GT. 0.001) THEN
            NO2NIT= KNO220*(TCNIT**(TW-20.))*NO2
          END IF
C
C         update no2 state variable to account for nitrification
          IF (NO2NIT .GT. 0.0) THEN
            IF ((NO2+TAMNIT-NO2NIT) .LE. 0.0) THEN
              NO2NIT= 0.9*(NO2 + TAMNIT)
              NO2   = 0.1*(NO2 + TAMNIT)
            ELSE
              NO2   = NO2 + TAMNIT - NO2NIT
            END IF
          ELSE
            NO2= NO2 + TAMNIT
          END IF
          NO2NTC= TAMNIT - NO2NIT
C
        ELSE
C         no2 is not simulated; tam oxidized is fully oxidized to
C         no3
          NO2NIT= TAMNIT
          NO2NTC= 0.0
C
        END IF
C
C       update no3 state variable to account for nitrification
C       and compute concentration flux of no3
        NO3   = NO3 + NO2NIT
        NO3NIT= NO2NIT
C
C       find oxygen demand due to nitrification
        DODEMD= 3.22*TAMNIT + 1.11*NO2NIT
C
        IF (DOX .LT. DODEMD) THEN
C         adjust nitrification demands on oxygen so that dox will
C         not be zero;  routine proportionally reduces tam oxidation
C         to no2 and no2 oxidation to no3
C
          RHO= DOX/DODEMD
          IF (RHO .LT. 0.001) THEN
            RHO=0.0
          END IF
          RHOC3= (1. - RHO)*TAMNIT
          RHOC2= (1. - RHO)*NO2NIT
          TAM  = TAM + RHOC3
          IF (NO2FG .NE. 0) THEN
            NO2= NO2 - RHOC3 + RHOC2
          END IF
          NO3   = NO3 - RHOC2
          DODEMD= DOX
          DOX   = 0.0
          TAMNIT= TAMNIT - RHOC3
          NO2NIT= NO2NIT - RHOC2
          NO3NIT= NO3NIT - RHOC2
          IF (NO2FG .NE. 0) THEN
            NO2NTC= NO2NTC - RHOC3 + RHOC2
          END IF
C
        ELSE
C         projected do value is acceptable
          DOX= DOX - DODEMD
        END IF
C
      ELSE
C       nitrification does not occur; set amounts of tam and no2
C       oxidized to zero
        TAMNIT= 0.0
        NO2NIT= 0.0
        DODEMD= 0.0
        NO2NTC= 0.0
        NO3NIT= 0.0
C
      END IF
C
      RETURN
      END
C
C     4.2(3).10.1.7
C
      SUBROUTINE   NUTACC
     I                    (FRMROW,TOROW)
C
C     + + + PURPOSE + + +
C     Accumulate fluxes in subroutine group nutrx for printout
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    FRMROW,TOROW
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FRMROW - row containing incremental flux accumulation
C     TOROW  - flux row to be incremented
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION NUTRX2 + + +
      INCLUDE    'crhnu.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,I4,I6,I7,I8
C
C     + + + EXTERNALS + + +
      EXTERNAL  ACCVEC
C
C     + + + END SPECIFICATIONS + + +
C
      I4= 4
      I6= 6
      I7= 7
      I8= 8
C
C
C     no3
      NUIF1(1,TOROW)= NUIF1(1,TOROW) + NUIF1(1,FRMROW)
      NUCF1(1,TOROW)= NUCF1(1,TOROW) + NUCF1(1,FRMROW)
      NUCF11(1,TOROW)= NUCF11(1,TOROW) + NUCF11(1,FRMROW)
      NUCF12(1,TOROW)= NUCF12(1,TOROW) + NUCF12(1,FRMROW)
C
      CALL ACCVEC
     I            (I7,NUCF4(1,FRMROW),
     M             NUCF4(1,TOROW))
C
      IF (NEXITS .GT. 1) THEN
        CALL ACCVEC
     I              (NEXITS,NUCF9(1,1,FRMROW),
     M               NUCF9(1,1,TOROW))
      END IF
C
C
C     tam
      IF (TAMFG .EQ. 1) THEN
        NUIF1(2,TOROW)= NUIF1(2,TOROW) + NUIF1(2,FRMROW)
        NUCF1(2,TOROW)= NUCF1(2,TOROW) + NUCF1(2,FRMROW)
        NUCF11(2,TOROW)= NUCF11(2,TOROW) + NUCF11(2,FRMROW)
        NUCF12(2,TOROW)= NUCF12(2,TOROW) + NUCF12(2,FRMROW)
C
        CALL ACCVEC
     I              (I8,NUCF5(1,FRMROW),
     M               NUCF5(1,TOROW))
C
        IF (ADNHFG .EQ. 1) THEN
          CALL ACCVEC
     I                (I4,NUIF2(1,1,FRMROW),
     M                 NUIF2(1,1,TOROW))
          CALL ACCVEC
     I                (I4,NUCF2(1,1,FRMROW),
     M                 NUCF2(1,1,TOROW))
          CALL ACCVEC
     I                (I4,NUCF3(1,1,FRMROW),
     M                 NUCF3(1,1,TOROW))
          CALL ACCVEC
     I                (I4,NUCF8(1,1,FRMROW),
     M                 NUCF8(1,1,TOROW))
        END IF
C
        IF (NEXITS .GT. 1) THEN
          CALL ACCVEC
     I                (NEXITS,NUCF9(1,2,FRMROW),
     M                 NUCF9(1,2,TOROW))
C
          IF (ADNHFG .EQ. 1) THEN
            DO 10 I= 1, 4
              CALL ACCVEC
     I                    (NEXITS,NUCF10(1,I,1,FRMROW),
     M                     NUCF10(1,I,1,TOROW))
 10         CONTINUE
          END IF
        END IF
      END IF
C
C
C     no2
      IF (NO2FG .EQ. 1) THEN
        NUIF1(3,TOROW)= NUIF1(3,TOROW) + NUIF1(3,FRMROW)
        NUCF1(3,TOROW)= NUCF1(3,TOROW) + NUCF1(3,FRMROW)
        NUCF6(1,TOROW)= NUCF6(1,TOROW) + NUCF6(1,FRMROW)
C
        IF (NEXITS .GT. 1) THEN
          CALL ACCVEC
     I                (NEXITS,NUCF9(1,3,FRMROW),
     M                 NUCF9(1,3,TOROW))
        END IF
C
      END IF
C
C
C     po4
      IF (PO4FG .EQ. 1) THEN
        NUIF1(4,TOROW)= NUIF1(4,TOROW) + NUIF1(4,FRMROW)
        NUCF1(4,TOROW)= NUCF1(4,TOROW) + NUCF1(4,FRMROW)
        NUCF11(3,TOROW)= NUCF11(3,TOROW) + NUCF11(3,FRMROW)
        NUCF12(3,TOROW)= NUCF12(3,TOROW) + NUCF12(3,FRMROW)
C
        CALL ACCVEC
     I              (I6,NUCF7(1,FRMROW),
     M               NUCF7(1,TOROW))
C
        IF (ADPOFG .EQ. 1) THEN
          CALL ACCVEC
     I                (I4,NUIF2(1,2,FRMROW),
     M                 NUIF2(1,2,TOROW))
          CALL ACCVEC
     I                (I4,NUCF2(1,2,FRMROW),
     M                 NUCF2(1,2,TOROW))
          CALL ACCVEC
     I                (I4,NUCF3(1,2,FRMROW),
     M                 NUCF3(1,2,TOROW))
          CALL ACCVEC
     I                (I4,NUCF8(1,2,FRMROW),
     M                 NUCF8(1,2,TOROW))
        END IF
C
        IF (NEXITS .GT. 1) THEN
          CALL ACCVEC
     I                (NEXITS,NUCF9(1,4,FRMROW),
     M                 NUCF9(1,4,TOROW))
C
          IF (ADPOFG .EQ. 1) THEN
            DO 20 I= 1, 4
              CALL ACCVEC
     I                    (NEXITS,NUCF10(1,I,2,FRMROW),
     M                     NUCF10(1,I,2,TOROW))
 20         CONTINUE
          END IF
        END IF
C
      END IF
C
      RETURN
      END
C
C     4.2(3).10.2.7
C
      SUBROUTINE   NUTPRT
     I                    (LEV,PRINTU,FACTA,FACTB,FLUXID,UNITFG,BINU)
C
C     + + + PURPOSE + + +
C     Convert quantities from internal to external units,
C     and print out results
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     LEV,PRINTU,UNITFG,BINU
      REAL        FACTA,FACTB
      CHARACTER*4 FLUXID
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEV    - current output level (2-pivl,3-day,4-mon,5-ann)
C     PRINTU - fortran unit number on which to print output
C     FACTA  - ???
C     FACTB  - ???
C     FLUXID - ???
C     UNITFG - output units   1-english, 2-metric
C     BINU   - fortran unit number on which to write binary output
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION NUTRX2 + + +
      INCLUDE    'crhnu.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,IX,J,K,N,I0,I1,I2,I3,I4,I5,I6,I7,PADFG,JLEN,ACNT,
     $            CLEN(106+NEXITS*12),EXDAT(5)
      REAL        PIFLX(4),PISFLX(4,2),PCFLX1(4),PCFLX2(4,2),
     #            PCFLX3(4,2),PCFLX4(6),PCFLX5(7),PCFLX6,PCFLX7(5),
     #            PCFLX8(4,2),PCFLX9(5,4),PCFX10(5,4,2),PRNO3,PRNO2,
     #            PRTAM(3),PRPO4,TCFLX4,TCFLX5,TCFLX7,PRNO3S,PRNO2S,
     #            PRTAMS,PRPO4S,NO3IN,NO3DIF,TAMIN,TAMDIF,NO2IN,NO2DIF,
     #            PO4IN,PO4DIF,PRSNH4(4),PRSPO4(4),PRTAMT,PRPO4T,
     #            TOTIN,TOTOUT,PSNH4(3),PSPO4(3),PADTOT(3),PCFX11(3),
     #            PCFX12(3),PTIFLX,APRINT(106+NEXITS*12)
      CHARACTER*32 UNITID,CSNH4(3),CRTAM(3),CRSNH4(4),CCFLX4(6),
     $             CSPO4(3),CISFLX(4),CCFLX2(4),CCFLX8(4),CCFLX3(4),
     $             CCFLX5(7),CCFLX7(5),CCFX10(4),CRSPO4(4)
      CHARACTER*1   CSTR(2)
      CHARACTER*256 CHEAD(106+NEXITS*12)
C
C     note: local arrays have same dimensions as corresponding arrays in
C     Osv, except for dropping of dimension lev, where applicable
C
C     + + + EXTERNALS + + +
      EXTERNAL   TRNVEC,BALCHK,INTCHR,EXDATE
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' *** NUTRX ***')
 2010 FORMAT (/,'   TOTAL AMMONIA(TAM-N)')
 2020 FORMAT (/,'   STATE VARIABLES',28X,'TAM',7X,'NH4',7X,'NH3')
 2030 FORMAT (  '     DISSOLVED CONCENTRATION',2X,'(MG/L)',5X,
     $        1PE10.3,2X,1PE10.3,2X,1PE10.3)
 2040 FORMAT (/,'     CONCENTRATION OF NH4 ASSOCIATED',5X,
     $        'SUSP. SAND  SUSP. SILT  SUSP. CLAY')
 2050 FORMAT (  '     WITH SEDIMENT',2X,'(MG/KG)',14X,1PE10.3,2X,
     $        1PE10.3,2X,1PE10.3)
 2060 FORMAT (/,'     DISSOLVED STORAGE',2X,'(',A3,')',12X,1PE10.3,
     $        2X,1PE10.3,2X,1PE10.3)
 2070 FORMAT (/,'     STORAGE ON SUSPENDED',18X,
     $        'ON SAND','     ON SILT     ON CLAY       TOTAL')
 2080 FORMAT (  7X,'SEDIMENT',2X,'(',A3,')',19X,3(1PE10.3,2X),1PE10.3)
 2090 FORMAT (/,'     TOTAL STORAGE',2X,'(',A3,')',16X,1PE10.3)
 2100 FORMAT (/,'   FLUXES (',A3,')')
 2110 FORMAT (  '     INFLOW OF DISSOLVED TAM',13X,1PE10.3)
 2120 FORMAT (  '     INFLOW OF DISSOLVED TAM',3X,'<---ATMOSPHERIC',
     #        ' DEPOSITION--->     OTHER')
 2130 FORMAT ( 38X,'DRY       WET     TOTAL    INFLOW')
 2140 FORMAT ( 32X,'NUADDR(2) NUADWT(2)   ATM DEP      ITAM')
 2150 FORMAT ( 31X,5(1PE10.3))
 2160 FORMAT (/,'     INFLOW OF NH4 ON SEDIMENT',11X,'   ON SAND',
     $        '     ON SILT     ON CLAY       TOTAL')
 2170 FORMAT ( 39X,4(2X,1PE10.3))
 2180 FORMAT (/,'     TOTAL INFLOW OF TAM',17X,1PE10.3)
 2190 FORMAT (/,'     ADSORPTION/DESORPTION OF NH4',8X,'SUSP. SAND  '
     $        ,'SUSP. SILT  SUSP. CLAY       TOTAL')
 2200 FORMAT (  7X,'(POSITIVE INDICATES ADSORPTION)',3X,1PE10.3,
     $        2X,1PE10.3,2X,1PE10.3,2X,1PE10.3)
 2210 FORMAT (/,'     DEPOSITION/SCOUR OF NH4',13X,'   ON SAND     ',
     $        'ON SILT     ON CLAY       TOTAL')
 2220 FORMAT (  7X,'(POSITIVE INDICATES DEPOSITION)',3X,1PE10.3,
     $        2X,1PE10.3,2X,1PE10.3,2X,1PE10.3)
 2230 FORMAT (/,42X,'  NITRIF-    VOLATIL-     BENTHAL',
     $        '   BOD DECAY  PHYTOPLANK   ZOOPLANK.  BENTH.ALG.')
 2240 FORMAT (  '     OTHER GAINS/LOSSES',11X,'TOTAL     ICATION',
     $        '     IZATION     RELEASE            ',
     $        '      GROWTH       DEATH      GROWTH')
 2250 FORMAT (  7X,'(POS. INDICATES GAIN)',1X,8(1PE10.3,2X))
 2260 FORMAT (/,'     TOTAL OUTFLOW OF DISSOLVED TAM',6X,1PE10.3)
 2270 FORMAT (/,'     TOTAL OUTFLOW OF NH4 ON SEDIMENT',4X,
     $        '   ON SAND     ON SILT     ON CLAY       TOTAL')
 2280 FORMAT ( 41X,1PE10.3,2X,1PE10.3,2X,1PE10.3,2X,1PE10.3)
 2290 FORMAT (/,'     TOTAL OUTFLOW',23X,1PE10.3)
 2300 FORMAT (/,'     DISSOLVED TAM OUTFLOW FOR EACH EXIT')
 2310 FORMAT (  7X,'EXIT',I3,27X,1PE10.3)
 2320 FORMAT (/,'     NH4 OUTFLOW ON SEDIMENT FOR EACH EXIT',
     $        '  ON SAND     ON SILT     ON CLAY       TOTAL')
 2330 FORMAT (  7X,'EXIT',I3,25X,4(2X,1PE10.3))
 2340 FORMAT (  5X,A3)
 2350 FORMAT (/,3X,'NITRITE(NO2-N)')
 2360 FORMAT (/,3X,'STATE VARIABLES')
 2370 FORMAT (  5X,'CONCENTRATION  (MG/L)',15X,1PE10.3)
 2380 FORMAT (/,5X,'STORAGE (',A3,')',11X,1PE10.3)
 2390 FORMAT (  5X,'INFLOW',18X,1PE10.3)
 2400 FORMAT (  5X,'INFLOW OF NITRATE',9X,'<---ATMOSPHERIC',
     $        ' DEPOSITION--->     OTHER     TOTAL')
 2410 FORMAT ( 32X,'NUADDR(1) NUADWT(1)   ATM DEP      INO3',
     $        '    INFLOW')
 2420 FORMAT (/,44X,'NITRIF-')
 2430 FORMAT (  5X,'OTHER GAINS/LOSSES',11X,'TOTAL     ICATION')
 2440 FORMAT (/,5X,'OUTFLOWS',21X,'TOTAL',6X,
     $        'INDIVIDUAL GATE OUTFLOWS',/,32X,'OUTFLOW',2X,5(I10,2X))
 2450 FORMAT ( 29X,6(1PE10.3,2X))
 2460 FORMAT (/,5X,'OUTFLOW',17X,1PE10.3)
 2470 FORMAT (/,3X,'NITRATE(NO3-N)')
 2480 FORMAT (/,44X,'NITRIF-   DENITRIF-   BOD DECAY',
     $        '  PHYTOPLANK   ZOOPLANK.  BENTH.ALG.')
 2490 FORMAT (  5X,'OTHER GAINS/LOSSES',11X,'TOTAL     ICATION',
     $        '     ICATION                  GROWTH',
     $        '       DEATH      GROWTH')
 2500 FORMAT (/,3X,'ORTHO-PHOSPHATE(PO4-P)')
 2510 FORMAT (  5X,'DISSOLVED CONCENTRATION',2X,'(MG/L)',5X,
     $        1PE10.3)
 2520 FORMAT (/,5X,'CONCENTRATION ASSOCIATED',12X,'SUSP. SAND  ',
     $        'SUSP. SILT  SUSP. CLAY')
 2530 FORMAT (/,5X,'DISSOLVED STORAGE',2X,'(',A3,')',12X,1PE10.3)
 2540 FORMAT (/,'     STORAGE ON SUSPENDED',18X,
     $        'ON SAND','     ON SILT     ON CLAY       TOTAL')
 2550 FORMAT (  5X,'INFLOW OF DISSOLVED PO4',13X,1PE10.3)
 2560 FORMAT (  5X,'INFLOW OF DISSOLVED PO4',3X,'<---ATMOSPHERIC',
     #        ' DEPOSITION--->     OTHER')
 2570 FORMAT ( 32X,'NUADDR(3) NUADWT(3)   ATM DEP      IPO4')
 2580 FORMAT (/,5X,'INFLOW OF PO4 ON SEDIMENT',11X,'   ON SAND',
     $        '     ON SILT     ON CLAY    ON ALL')
 2590 FORMAT (/,5X,'TOTAL INFLOW',24X,1PE10.3)
 2600 FORMAT (/,5X,'ADSORPTION/DESORPTION',15X,'SUSP. SAND  ',
     $        'SUSP. SILT  SUSP. CLAY       TOTAL')
 2610 FORMAT (/,5X,'DEPOSITION/SCOUR',20X,'   ON SAND     ',
     $        'ON SILT     ON CLAY       TOTAL')
 2620 FORMAT (/,44X,'BENTHAL   BOD DECAY  PHYTOPLANK   ZOOPLANK.',
     $        '  BENTH.ALG.')
 2630 FORMAT (  5X,'OTHER GAINS/LOSSES',11X,'TOTAL     RELEASE',
     $        '                  GROWTH       DEATH      GROWTH')
 2640 FORMAT (/,5X,'TOTAL OUTFLOW OF DISSOLVED PO4',6X,1PE10.3)
 2650 FORMAT (/,5X,'TOTAL OUTFLOW OF PO4 ON SEDIMENT',4X,
     $        '   ON SAND     ON SILT     ON CLAY       TOTAL')
 2660 FORMAT (/,5X,'DISSOLVED OUTFLOW FOR EACH EXIT')
 2670 FORMAT (/,5X,'OUTFLOW ON SEDIMENT FOR EACH EXIT',3X,
     $        '   ON SAND     ON SILT     ON CLAY       TOTAL')
C
C     + + + END SPECIFICATIONS + + +
C
      I0    = 0
      I1    = 1
      I2    = 2
      I3    = 3
      I4    = 4
      I5    = 5
      I6    = 6
      I7    = 7
C
C     initialize array counter for binary printout, store variable
C     names in local strings for use in building binary headers
      ACNT = 0
      CSNH4(1)  = 'NH4CONC-SUSPSAND'
      CSNH4(2)  = 'NH4CONC-SUSPSILT'
      CSNH4(3)  = 'NH4CONC-SUSPCLAY'
      CRTAM(1)  = 'TAM-STORDIS'
      CRTAM(2)  = 'NH4-STORDIS'
      CRTAM(3)  = 'NH3-STORDIS'
      CRSNH4(1) = 'NH4-STORPART-SUSPSAND'
      CRSNH4(2) = 'NH4-STORPART-SUSPSILT'
      CRSNH4(3) = 'NH4-STORPART-SUSPCLAY'
      CRSNH4(4) = 'NH4-STORPART-SUSPTOT'
      CISFLX(1) = '-INPART-SAND'
      CISFLX(2) = '-INPART-SILT'
      CISFLX(3) = '-INPART-CLAY'
      CISFLX(4) = '-INPART-TOT'
      CCFLX8(1) = '-ADSDES-SAND'
      CCFLX8(2) = '-ADSDES-SILT'
      CCFLX8(3) = '-ADSDES-CLAY'
      CCFLX8(4) = '-ADSDES-TOT'
      CCFLX3(1) = '-SCOURDEP-SAND'
      CCFLX3(2) = '-SCOURDEP-SILT'
      CCFLX3(3) = '-SCOURDEP-CLAY'
      CCFLX3(4) = '-SCOURDEP-TOT'
      CCFLX5(1) = 'TAM-PROCFLUX-NITR'
      CCFLX5(2) = 'TAM-PROCFLUX-VOLAT'
      CCFLX5(3) = 'TAM-PROCFLUX-BENTHAL'
      CCFLX5(4) = 'TAM-PROCFLUX-BODDEC'
      CCFLX5(5) = 'TAM-PROCFLUX-PHYTO'
      CCFLX5(6) = 'TAM-PROCFLUX-ZOO'
      CCFLX5(7) = 'TAM-PROCFLUX-BENTHIC'
      CCFLX2(1) = '-OUTPART-SAND'
      CCFLX2(2) = '-OUTPART-SILT'
      CCFLX2(3) = '-OUTPART-CLAY'
      CCFLX2(4) = '-OUTPART-TOT'
      CCFX10(1) = '-OUTPART-SAND-EXIT'
      CCFX10(2) = '-OUTPART-SILT-EXIT'
      CCFX10(3) = '-OUTPART-CLAY-EXIT'
      CCFX10(4) = '-OUTPART-TOT-EXIT'
      CCFLX4(1) = 'NO3-PROCFLUX-NITR'
      CCFLX4(2) = 'NO3-PROCFLUX-DENITR'
      CCFLX4(3) = 'NO3-PROCFLUX-BODDEC'
      CCFLX4(4) = 'NO3-PROCFLUX-PHYTO'
      CCFLX4(5) = 'NO3-PROCFLUX-ZOO'
      CCFLX4(6) = 'NO3-PROCFLUX-BENTHIC'
      CSPO4(1)  = 'PO4-CONC-SUSPSAND'
      CSPO4(2)  = 'PO4-CONC-SUSPSILT'
      CSPO4(3)  = 'PO4-CONC-SUSPCLAY'
      CRSPO4(1) = 'PO4-STORPART-SUSPSAND'
      CRSPO4(2) = 'PO4-STORPART-SUSPSILT'
      CRSPO4(3) = 'PO4-STORPART-SUSPCLAY'
      CRSPO4(4) = 'PO4-STORPART-SUSPTOT'
      CCFLX7(1) = 'PO4-PROCFLUX-BENTHAL'
      CCFLX7(2) = 'PO4-PROCFLUX-BODDEC'
      CCFLX7(3) = 'PO4-PROCFLUX-PHYTO'
      CCFLX7(4) = 'PO4-PROCFLUX-ZOO'
      CCFLX7(5) = 'PO4-PROCFLUX-BENTHIC'
C
      TCFLX4= 0.0
      TCFLX5= 0.0
      TCFLX7= 0.0
      DO 20 I= 1, 4
        DO 10 J= 1, 2
          PISFLX(I,J)= 0.0
          PCFLX2(I,J)= 0.0
          PCFLX3(I,J)= 0.0
          PCFLX8(I,J)= 0.0
 10     CONTINUE
 20   CONTINUE
      DO 30 I= 1, 3
        PADTOT(I)= 0.0
 30   CONTINUE
      IF (PRINTU .GT. 0 .AND. PFLAG(8) .LE. LEV) THEN
        WRITE (UNITID,2340) FLUXID(1:3)
      END IF
C
C
      PADFG= 0
      DO 40 I= 1, 3
        J= (I-1)*2+ 1
        IF ( (NUADFG(J) .NE. 0) .OR. (NUADFG(J+1) .NE. 0) ) THEN
          PADFG= 1
        END IF
 40   CONTINUE
C
      IF (PADFG .EQ. 1) THEN
C       compute atmospheric deposition fluxes as applicable
        DO 50 I= 1, 3
          J= (I-1)*2+ 1
          IF (NUADFG(J) .NE. 0) THEN
            PCFX11(I)= NUCF11(I,LEV)*FACTA
          ELSE
            PCFX11(I)= 0.0
          END IF
          IF (NUADFG(J+1) .NE. 0) THEN
            PCFX12(I)= NUCF12(I,LEV)*FACTA
          ELSE
            PCFX12(I)= 0.0
          END IF
          PADTOT(I)= PCFX11(I)+ PCFX12(I)
 50     CONTINUE
      END IF
C
C     convert variables to external units
C
C
C     no3
C     nitrate inflow flux
      PIFLX(1) = NUIF1(1,LEV)*FACTA
C
C     nitrate computed fluxes
      PCFLX1(1)= NUCF1(1,LEV)*FACTA
      CALL TRNVEC
     I            (I6,NUCF4(1,LEV),FACTA,FACTB,
     O             PCFLX4)
      DO 60 J= 1,6
        TCFLX4= TCFLX4+ PCFLX4(J)
 60   CONTINUE
C
C     storage
      PRNO3 = NUST(1,1)*FACTA
      PRNO3S= NUST(1,LEV)*FACTA
C
      IF (NEXITS .GT. 1) THEN
        CALL TRNVEC
     I              (NEXITS,NUCF9(1,1,LEV),FACTA,FACTB,
     O               PCFLX9(1,1))
      END IF
C
C
C     tam
      IF (TAMFG .EQ. 1) THEN
C       total dissolved ammonia inflow flux
        PIFLX(2)= NUIF1(2,LEV)*FACTA
C
        IF (ADNHFG .EQ. 1) THEN
C         adsorbed concentrations
          DO 70 I= 1, 3
            PSNH4(I)= SNH4(I)*1.0E6
 70       CONTINUE
C
C         inflow of nh4 on sediment
          CALL TRNVEC
     I                (I4,NUIF2(1,1,LEV),FACTA,FACTB,
     O                 PISFLX(1,1))
C
C         adsorption/desorption fluxes between nh4 and sediment
          CALL TRNVEC
     I                (I4,NUCF8(1,1,LEV),FACTA,FACTB,
     O                 PCFLX8(1,1))
C
C         deposition/scour fluxes of nh4
          CALL TRNVEC
     I                (I4,NUCF3(1,1,LEV),FACTA,FACTB,
     O                 PCFLX3(1,1))
C
C         computed outflows of adsorbed nh4
          CALL TRNVEC
     I                (I4,NUCF2(1,1,LEV),FACTA,FACTB,
     O                 PCFLX2(1,1))
C
        END IF
C
C       computed total outflow flux for dissolved tam
        PCFLX1(2)= NUCF1(2,LEV)*FACTA
C
C       computed tam fluxes for other gains and losses
        CALL TRNVEC
     I              (I7,NUCF5(1,LEV),FACTA,FACTB,
     O               PCFLX5)
        DO 80 J= 1,7
          TCFLX5= TCFLX5+ PCFLX5(J)
 80     CONTINUE
C
C       dissolved storage
        PRTAM(1)= DNUST2(2)*FACTA
        PRTAM(2)= DNUST2(5)*FACTA
        PRTAM(3)= DNUST2(6)*FACTA
C
C       total storage for mass balance calculation
        PRTAMT= NUST(2,1)*FACTA
        PRTAMS= NUST(2,LEV)*FACTA
C
C       adsorbed storage
        DO 90 J=1,4
          IF (ADNHFG .EQ. 1) THEN
            PRSNH4(J)= RSNH4(J)*FACTA
          ELSE
            PRSNH4(J)= 0.0
          END IF
 90     CONTINUE
C
        IF (NEXITS .GT. 1) THEN
          CALL TRNVEC
     I                (NEXITS,NUCF9(1,2,LEV),FACTA,FACTB,
     O                 PCFLX9(1,2))
          IF (ADNHFG .NE. 0) THEN
            DO 100 J= 1, 4
              CALL TRNVEC
     I                    (NEXITS,NUCF10(1,J,1,LEV),FACTA,FACTB,
     O                     PCFX10(1,J,1))
 100        CONTINUE
          END IF
        END IF
      END IF
C
C
C     no2
      IF (NO2FG .EQ. 1) THEN
C       nitrite inflow flux
        PIFLX(3) = NUIF1(3,LEV)*FACTA
C
C       nitrite computed fluxes
        PCFLX1(3)= NUCF1(3,LEV)*FACTA
        PCFLX6   = NUCF6(1,LEV)*FACTA
C
C       storage
        PRNO2 = NUST(3,1)*FACTA
        PRNO2S= NUST(3,LEV)*FACTA
C
        IF (NEXITS .GT. 1) THEN
          CALL TRNVEC
     I                (NEXITS,NUCF9(1,3,LEV),FACTA,FACTB,
     O                 PCFLX9(1,3))
        END IF
      END IF
C
C
C     po4
      IF (PO4FG .EQ. 1) THEN
C       dissolved ortho-phosphorus inflow flux
        PIFLX(4)= NUIF1(4,LEV)*FACTA
C
        IF (ADPOFG .EQ. 1) THEN
C         adsorbed concentrations
          DO 110 I= 1, 3
            PSPO4(I)= SPO4(I)*1.0E6
 110      CONTINUE
C
C         inflow of phosphate on sediment
          CALL TRNVEC
     I                (I4,NUIF2(1,2,LEV),FACTA,FACTB,
     O                 PISFLX(1,2))
C
C         adsorption/desorption fluxes between po4 and sediment
          CALL TRNVEC
     I                (I4,NUCF8(1,2,LEV),FACTA,FACTB,
     O                 PCFLX8(1,2))
C
C         deposition/scour fluxes of po4
          CALL TRNVEC
     I                (I4,NUCF3(1,2,LEV),FACTA,FACTB,
     O                 PCFLX3(1,2))
C
C         computed outflows of adsorbed po4
          CALL TRNVEC
     I                (I4,NUCF2(1,2,LEV),FACTA,FACTB,
     O                 PCFLX2(1,2))
C
        END IF
C
C       ortho-phosphorus total outflow flux
        PCFLX1(4)= NUCF1(4,LEV)*FACTA
C
C       ortho-phosphorus computed fluxes from other gains and losses
        CALL TRNVEC
     I              (I5,NUCF7(1,LEV),FACTA,FACTB,
     O               PCFLX7)
        DO 120 J= 1,5
          TCFLX7= TCFLX7+ PCFLX7(J)
 120    CONTINUE
C
C       dissolved storage
        PRPO4 = DNUST2(4)*FACTA
C
C       total storage for mass balance calculation
        PRPO4T= NUST(4,1)*FACTA
        PRPO4S= NUST(4,LEV)*FACTA
C
C       adsorbed storage
        DO 130 J=1,4
          IF (ADPOFG .EQ. 1) THEN
            PRSPO4(J)= RSPO4(J)*FACTA
          ELSE
            PRSPO4(J)= 0.0
          END IF
 130    CONTINUE
C
        IF (NEXITS .GT. 1) THEN
          CALL TRNVEC
     I                (NEXITS,NUCF9(1,4,LEV),FACTA,FACTB,
     O                 PCFLX9(1,4))
          IF (ADPOFG .NE. 0) THEN
            DO 140 J= 1, 4
              CALL TRNVEC
     I                    (NEXITS,NUCF10(1,J,2,LEV),FACTA,FACTB,
     O                     PCFX10(1,J,2))
 140        CONTINUE
          END IF
        END IF
      END IF
C
C     do printout on unit printu
      IF (PRINTU .GT. 0 .AND. PFLAG(4) .LE. LEV) THEN
        WRITE (PRINTU,2000)
      END IF
      IF (TAMFG .EQ. 1) THEN
C
C       ammonia printout
C
        IF (PRINTU .GT. 0 .AND. PFLAG(8) .LE. LEV) THEN
          WRITE (PRINTU,2010)
          WRITE (PRINTU,2020)
          WRITE (PRINTU,2030)  DNUST(2),DNUST(5),DNUST(6)
C
          IF (ADNHFG .EQ. 1) THEN
            WRITE (PRINTU,2040)
            WRITE (PRINTU,2050) (PSNH4(J), J= 1,3)
          END IF
C
          WRITE (PRINTU,2060) FLUXID,PRTAM
C
          IF (ADNHFG .EQ. 1) THEN
            WRITE (PRINTU,2070)
            WRITE (PRINTU,2080) FLUXID,PRSNH4
            WRITE (PRINTU,2090) FLUXID,PRTAMT
          END IF
C
          WRITE (PRINTU,2100) FLUXID
          IF (PADFG .EQ. 0) THEN
            WRITE (PRINTU,2110) PIFLX(2)
          ELSE
            WRITE (PRINTU,2120)
            WRITE (PRINTU,2130)
            WRITE (PRINTU,2140)
            WRITE (PRINTU,2150) PCFX11(2),PCFX12(2),PADTOT(2),PIFLX(2)
          END IF
C
          IF (ADNHFG .EQ. 1) THEN
            WRITE (PRINTU,2160)
            WRITE (PRINTU,2170) (PISFLX(J,1), J= 1,4)
          END IF
        END IF
        IF (BINU .GT. 0 .AND. ABS(BFLAG(8)) .LE. LEV) THEN
          ACNT = ACNT + 1
          APRINT(ACNT) = DNUST(2)
          CHEAD(ACNT) = 'TAM-CONCDIS'
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          ACNT = ACNT + 1
          APRINT(ACNT) = DNUST(5)
          CHEAD(ACNT) = 'NH4-CONCDIS'
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          ACNT = ACNT + 1
          APRINT(ACNT) = DNUST(6)
          CHEAD(ACNT) = 'NH3-CONCDIS'
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          DO 141 I= 1, 3
            ACNT = ACNT + 1
            APRINT(ACNT) = PSNH4(I)
            CHEAD(ACNT) = CSNH4(I)
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 141      CONTINUE
          DO 142 I= 1, 3
            ACNT = ACNT + 1
            APRINT(ACNT) = PRTAM(I)
            CHEAD(ACNT) = CRTAM(I)
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 142      CONTINUE
          IF (ADNHFG .EQ. 1) THEN
            DO 143 I= 1, 4
              ACNT = ACNT + 1
              APRINT(ACNT) = PRSNH4(I)
              CHEAD(ACNT) = CRSNH4(I)
              CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 143        CONTINUE
            ACNT = ACNT + 1
            APRINT(ACNT) = PRTAMT
            CHEAD(ACNT) = 'TAM-STOR'
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          END IF
          IF (PADFG .EQ. 0) THEN
            ACNT = ACNT + 1
            APRINT(ACNT) = PIFLX(2)
            CHEAD(ACNT) = 'TAM-INDIS'
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          ELSE
            ACNT = ACNT + 1
            APRINT(ACNT) = PCFX11(2)
            CHEAD(ACNT) = 'TAM-ATMDEPDRY'
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
            ACNT = ACNT + 1
            APRINT(ACNT) = PCFX12(2)
            CHEAD(ACNT) = 'TAM-ATMDEPWET'
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
            ACNT = ACNT + 1
            APRINT(ACNT) = PADTOT(2)
            CHEAD(ACNT) = 'TAM-ATMDEPTOT'
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
            ACNT = ACNT + 1
            APRINT(ACNT) = PIFLX(2)
            CHEAD(ACNT) = 'TAM-INDIS'
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          END IF
          IF (ADNHFG .EQ. 1) THEN
            DO 144 I= 1, 4
              ACNT = ACNT + 1
              APRINT(ACNT) = PISFLX(I,1)
              CHEAD(ACNT) = 'NH4' // CISFLX(I)
              CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 144        CONTINUE
          END IF
        END IF
C
C       calculate and print total inflow of tam
C
        TOTIN= PADTOT(2)+ PIFLX(2)+ PISFLX(4,1)
        IF (PRINTU .GT. 0 .AND. PFLAG(8) .LE. LEV) THEN
          WRITE (PRINTU,2180) TOTIN
C
          IF (ADNHFG .EQ. 1) THEN
            WRITE (PRINTU,2190)
            WRITE (PRINTU,2200) (PCFLX8(J,1),J=1,4)
            WRITE (PRINTU,2210)
            WRITE (PRINTU,2220) (PCFLX3(J,1),J=1,4)
          END IF
C
C         print other gains/losses of tam
C
          WRITE (PRINTU,2230)
          WRITE (PRINTU,2240)
          WRITE (PRINTU,2250)  TCFLX5, (PCFLX5(J),J=1,7)
C
          WRITE (PRINTU,2260) PCFLX1(2)
C
          IF (ADNHFG .EQ. 1) THEN
            WRITE (PRINTU,2270)
            WRITE (PRINTU,2280) (PCFLX2(J,1),J=1,4)
          END IF
        END IF
        IF (BINU .GT. 0 .AND. ABS(BFLAG(8)) .LE. LEV) THEN
          ACNT = ACNT + 1
          APRINT(ACNT) = TOTIN
          CHEAD(ACNT) = 'TAM-INTOT'
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          IF (ADNHFG .EQ. 1) THEN
            DO 145 I= 1, 4
              ACNT = ACNT + 1
              APRINT(ACNT) = PCFLX8(I,1)
              CHEAD(ACNT) = 'TAM' // CCFLX8(I)
              CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 145        CONTINUE
            DO 146 I= 1, 4
              ACNT = ACNT + 1
              APRINT(ACNT) = PCFLX3(I,1)
              CHEAD(ACNT) = 'TAM' // CCFLX3(I)
              CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 146        CONTINUE
          END IF
          ACNT = ACNT + 1
          APRINT(ACNT) = TCFLX5
          CHEAD(ACNT) = 'TAM-PROCFLUX-TOT'
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          DO 147 I= 1, 7
            ACNT = ACNT + 1
            APRINT(ACNT) = PCFLX5(I)
            CHEAD(ACNT) = CCFLX5(I)
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 147      CONTINUE
          ACNT = ACNT + 1
          APRINT(ACNT) = PCFLX1(2)
          CHEAD(ACNT) = 'TAM-OUTDIS'
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          IF (ADNHFG .EQ. 1) THEN
            DO 148 I= 1, 4
              ACNT = ACNT + 1
              APRINT(ACNT) = PCFLX2(I,1)
              CHEAD(ACNT) = 'TAM' // CCFLX2(I)
              CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 148        CONTINUE
          END IF
        END IF
C
C       calculate and print total outflow of tam
C
        TOTOUT= PCFLX1(2) + PCFLX2(4,1)
        IF (PRINTU .GT. 0 .AND. PFLAG(8) .LE. LEV) THEN
          WRITE (PRINTU,2290) TOTOUT
C
          IF (NEXITS .GT. 1) THEN
            WRITE (PRINTU,2300)
            DO 150 N= 1,NEXITS
              WRITE (PRINTU,2310) N,PCFLX9(N,2)
 150        CONTINUE
C
            IF (ADNHFG .EQ. 1) THEN
              WRITE (PRINTU,2320)
              DO 160 N= 1,NEXITS
                WRITE (PRINTU,2330) N,(PCFX10(N,K,1), K=1,4)
 160          CONTINUE
            END IF
          END IF
        END IF
        IF (BINU .GT. 0 .AND. ABS(BFLAG(8)) .LE. LEV) THEN
          ACNT = ACNT + 1
          APRINT(ACNT) = TOTOUT
          CHEAD(ACNT) = 'TAM-OUTTOT'
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          DO 165 I= 1, NEXITS
            CALL INTCHR (I, I2, I1,
     O                   JLEN, CSTR)
            ACNT = ACNT + 1
            APRINT(ACNT) = PCFLX9(I,2)
            CHEAD(ACNT) = 'TAM-OUTDIS-EXIT'
            DO 161 IX= 1, JLEN
              CHEAD(ACNT) = TRIM(CHEAD(ACNT)) // CSTR(IX)
 161        CONTINUE
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
            IF (ADNHFG .EQ. 1) THEN
              DO 164 J= 1, 4
                ACNT = ACNT + 1
                APRINT(ACNT) = PCFX10(I,J,1)
                CHEAD(ACNT) = 'TAM' // CCFX10(J)
                DO 163 IX= 1, JLEN
                  CHEAD(ACNT) = TRIM(CHEAD(ACNT)) // CSTR(IX)
 163            CONTINUE
                CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 164          CONTINUE
            END IF
 165      CONTINUE
        END IF
C
C       material balance
        TAMIN = PADTOT(2)+ PIFLX(2)+ PISFLX(4,1)- PCFLX3(4,1)+ 
     #          PCFLX5(3)+ PCFLX5(4)+PCFLX5(6)
        TAMDIF= TAMIN+ PCFLX5(1)+ PCFLX5(2)+ PCFLX5(5)+ PCFLX5(7)-
     #          PCFLX1(2)- PCFLX2(4,1)
        CALL BALCHK
     I              (I3,RCHNO,DATIM,MESSU,PRINTU,MSGFL,
     I               PRTAMS,PRTAMT,TAMIN,TAMDIF,UNITID,I1,
     M               NUWCNT)
      END IF
C
      IF (NO2FG .EQ. 1) THEN
C
C       nitrite printout
C
        IF (PRINTU .GT. 0 .AND. PFLAG(8) .LE. LEV) THEN
          WRITE (PRINTU,2350)
          WRITE (PRINTU,2360)
          WRITE (PRINTU,2370)  DNUST(3)
          WRITE (PRINTU,2380)  FLUXID, PRNO2
          WRITE (PRINTU,2100)  FLUXID
          WRITE (PRINTU,2390)  PIFLX(3)
          WRITE (PRINTU,2420)
          WRITE (PRINTU,2430)
          WRITE (PRINTU,2250)  PCFLX6, PCFLX6
C
          IF (NEXITS .GT. 1) THEN
            WRITE (PRINTU,2440)  (N,N=1,NEXITS)
            WRITE (PRINTU,2450)  PCFLX1(3), (PCFLX9(N,3),N=1,NEXITS)
          ELSE
            WRITE (PRINTU,2460)  PCFLX1(3)
          END IF
        END IF
        IF (BINU .GT. 0 .AND. ABS(BFLAG(8)) .LE. LEV) THEN
          ACNT = ACNT + 1
          APRINT(ACNT) = DNUST(3)
          CHEAD(ACNT) = 'NO2-CONCDIS'
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          ACNT = ACNT + 1
          APRINT(ACNT) = PRNO2
          CHEAD(ACNT) = 'NO2-STOR'
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          ACNT = ACNT + 1
          APRINT(ACNT) = PIFLX(3)
          CHEAD(ACNT) = 'NO2-INTOT'
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          ACNT = ACNT + 1
          APRINT(ACNT) = PCFLX6
          CHEAD(ACNT) = 'NO2-PROCFLUX-TOT'
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          ACNT = ACNT + 1
          APRINT(ACNT) = PCFLX6
          CHEAD(ACNT) = 'NO2-PROCFLUX-NIT'
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          ACNT = ACNT + 1
          APRINT(ACNT) = PCFLX1(3)
          CHEAD(ACNT) = 'NO2-OUTTOT'
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          IF (NEXITS .GT. 1) THEN
            DO 167 I= 1, NEXITS
              CALL INTCHR (I, I2, I1,
     O                     JLEN, CSTR)
              ACNT = ACNT + 1
              APRINT(ACNT) = PCFLX9(I,3)
              CHEAD(ACNT) = 'NO2-OUTDIS-EXIT'
              DO 166 IX= 1, JLEN
                CHEAD(ACNT) = TRIM(CHEAD(ACNT)) // CSTR(IX)
 166          CONTINUE
              CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 167        CONTINUE
          END IF
        END IF
C
C       material balance
C
Cthj    NO2IN = PIFLX(3)
Cthj    NO2DIF= NO2IN+ PCFLX6- PCFLX1(3)
Cthj    net nitrification can dominate the nitrite balance.  this
Cthj    can cause problems with false balance check errors when
Cthj    initial storage plus inflow is very small relative to
Cthj    a positive net nitrification.  this type of change may
Cthj    be appropriate for other net fluxes (i.e. may be + or -)
Cthj    in other parts of the program, although unless that flux
Cthj    can dominate, it is not likely to cause problems.
C
        IF (PCFLX6 .GE. 0.0) THEN
C         net nitrification is positive - count as inflow
          NO2IN = PIFLX(3)+ PCFLX6
          NO2DIF= NO2IN- PCFLX1(3)
        ELSE
C         net nitrification is negative - count as outflow
          NO2IN = PIFLX(3)
          NO2DIF= NO2IN+ PCFLX6- PCFLX1(3)
        END IF
        CALL BALCHK
     I              (I3,RCHNO,DATIM,MESSU,PRINTU,MSGFL,
     I               PRNO2S,PRNO2,NO2IN,NO2DIF,UNITID,I1,
     M               NUWCNT)
C
      END IF
C
C     nitrate printout
C
      IF (PRINTU .GT. 0 .AND. PFLAG(8) .LE. LEV) THEN
        WRITE (PRINTU,2470)
        WRITE (PRINTU,2360)
        WRITE (PRINTU,2370)  DNUST(1)
        WRITE (PRINTU,2380)  FLUXID, PRNO3
        WRITE (PRINTU,2100)  FLUXID
        IF (PADFG .EQ. 0) THEN
          WRITE (PRINTU,2390)  PIFLX(1)
        ELSE
          WRITE (PRINTU,2400)
          WRITE (PRINTU,2130)
          WRITE (PRINTU,2410)
          PTIFLX= PADTOT(1)+ PIFLX(1)
          WRITE (PRINTU,2150)  PCFX11(1),PCFX12(1),PADTOT(1),PIFLX(1),
     $                         PTIFLX
        END IF
        WRITE (PRINTU,2480)
        WRITE (PRINTU,2490)
        WRITE (PRINTU,2250)  TCFLX4, (PCFLX4(J),J=1,6)
C
        IF (NEXITS .GT. 1) THEN
          WRITE (PRINTU,2440)  (N,N=1,NEXITS)
          WRITE (PRINTU,2450)  PCFLX1(1), (PCFLX9(N,1),N=1,NEXITS)
        ELSE
          WRITE (PRINTU,2460)  PCFLX1(1)
        END IF
      END IF
      IF (BINU .GT. 0 .AND. ABS(BFLAG(8)) .LE. LEV) THEN
        ACNT = ACNT + 1
        APRINT(ACNT) = DNUST(1)
        CHEAD(ACNT) = 'NO3-CONCDIS'
        CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
        ACNT = ACNT + 1
        APRINT(ACNT) = PRNO3
        CHEAD(ACNT) = 'NO3-STOR'
        CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
        IF (PADFG .EQ. 0) THEN
          ACNT = ACNT + 1
          APRINT(ACNT) = PIFLX(1)
          CHEAD(ACNT) = 'NO3-INTOT'
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
        ELSE
          ACNT = ACNT + 1
          APRINT(ACNT) = PCFX11(1)
          CHEAD(ACNT) = 'NO3-ATMDEPDRY'
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          ACNT = ACNT + 1
          APRINT(ACNT) = PCFX12(1)
          CHEAD(ACNT) = 'NO3-ATMDEPWET'
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          ACNT = ACNT + 1
          APRINT(ACNT) = PADTOT(1)
          CHEAD(ACNT) = 'NO3-ATMDEPTOT'
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          ACNT = ACNT + 1
          APRINT(ACNT) = PIFLX(1)
          CHEAD(ACNT) = 'NO3-INDIS'
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          ACNT = ACNT + 1
          APRINT(ACNT) = PTIFLX
          CHEAD(ACNT) = 'NO3-INTOT'
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
        END IF
        ACNT = ACNT + 1
        APRINT(ACNT) = TCFLX4
        CHEAD(ACNT) = 'NO3-PROCFLUX-TOT'
        CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
        DO 168 I= 1, 6
          ACNT = ACNT + 1
          APRINT(ACNT) = PCFLX4(I)
          CHEAD(ACNT) = CCFLX4(I)
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 168    CONTINUE
        ACNT = ACNT + 1
        APRINT(ACNT) = PCFLX1(1)
        CHEAD(ACNT) = 'NO3-OUTTOT'
        CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
        IF (NEXITS .GT. 1) THEN
          DO 171 I= 1, NEXITS
            CALL INTCHR (I, I2, I1,
     O                   JLEN, CSTR)
            ACNT = ACNT + 1
            APRINT(ACNT) = PCFLX9(I,3)
            CHEAD(ACNT) = 'NO3-OUTDIS-EXIT'
            DO 170 IX= 1, JLEN
              CHEAD(ACNT) = TRIM(CHEAD(ACNT)) // CSTR(IX)
 170        CONTINUE
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 171      CONTINUE
        END IF
      END IF
C
C     material balance
      NO3IN = PADTOT(1)+ PIFLX(1)+ PCFLX4(1)+ PCFLX4(3)+ PCFLX4(5)
      NO3DIF= NO3IN+ PCFLX4(2)+ PCFLX4(4)+ PCFLX4(6)- PCFLX1(1)
      CALL BALCHK
     I            (I3,RCHNO,DATIM,MESSU,PRINTU,MSGFL,
     I             PRNO3S,PRNO3,NO3IN,NO3DIF,UNITID,I1,
     M             NUWCNT)
C
C
      IF (PO4FG .EQ. 1) THEN
C
C       ortho-phosphate printout
C
        IF (PRINTU .GT. 0 .AND. PFLAG(8) .LE. LEV) THEN
          WRITE (PRINTU,2500)
          WRITE (PRINTU,2360)
          WRITE (PRINTU,2510)  DNUST(4)
C
          IF (ADPOFG .EQ. 1) THEN
            WRITE (PRINTU,2520)
            WRITE (PRINTU,2050) (PSPO4(J), J= 1,3)
          END IF
C
          WRITE (PRINTU,2530) FLUXID,PRPO4
C
          IF (ADPOFG .EQ. 1) THEN
            WRITE (PRINTU,2540)
            WRITE (PRINTU,2080) FLUXID,PRSPO4
            WRITE (PRINTU,2090) FLUXID,PRPO4T
          END IF
C
          WRITE (PRINTU,2100) FLUXID
          IF (PADFG .EQ. 0) THEN
            WRITE (PRINTU,2550)  PIFLX(4)
          ELSE
            WRITE (PRINTU,2560)
            WRITE (PRINTU,2130)
            WRITE (PRINTU,2570)
            WRITE (PRINTU,2150)  PCFX11(3),PCFX12(3),PADTOT(3),PIFLX(4)
          END IF
C
          IF (ADPOFG .EQ. 1) THEN
            WRITE (PRINTU,2580)
            WRITE (PRINTU,2170) (PISFLX(J,2), J= 1, 4)
          END IF
        END IF
        IF (BINU .GT. 0 .AND. ABS(BFLAG(8)) .LE. LEV) THEN
          ACNT = ACNT + 1
          APRINT(ACNT) = DNUST(4)
          CHEAD(ACNT) = 'PO4-CONCDIS'
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          IF (ADPOFG .EQ. 1) THEN
            DO 172 I= 1, 3
              ACNT = ACNT + 1
              APRINT(ACNT) = PSPO4(I)
              CHEAD(ACNT) = CSPO4(I)
              CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 172        CONTINUE
          END IF
          ACNT = ACNT + 1
          APRINT(ACNT) = PRPO4
          CHEAD(ACNT) = 'PO4-STORDIS'
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          IF (ADPOFG .EQ. 1) THEN
            DO 173 I= 1, 4
              ACNT = ACNT + 1
              APRINT(ACNT) = PRSPO4(I)
              CHEAD(ACNT) = CRSPO4(I)
              CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 173        CONTINUE
            ACNT = ACNT + 1
            APRINT(ACNT) = PRPO4T
            CHEAD(ACNT) = 'PO4-STOR'
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          END IF
          IF (PADFG .EQ. 0) THEN
            ACNT = ACNT + 1
            APRINT(ACNT) = PIFLX(4)
            CHEAD(ACNT) = 'PO4-INDIS'
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          ELSE
            ACNT = ACNT + 1
            APRINT(ACNT) = PCFX11(3)
            CHEAD(ACNT) = 'PO4-ATMDEPDRY'
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
            ACNT = ACNT + 1
            APRINT(ACNT) = PCFX12(3)
            CHEAD(ACNT) = 'PO4-ATMDEPWET'
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
            ACNT = ACNT + 1
            APRINT(ACNT) = PADTOT(3)
            CHEAD(ACNT) = 'PO4-ATMDEPTOT'
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
            ACNT = ACNT + 1
            APRINT(ACNT) = PIFLX(4)
            CHEAD(ACNT) = 'PO4-INDIS'
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          END IF
          IF (ADPOFG .EQ. 1) THEN
            DO 174 I= 1, 4
              ACNT = ACNT + 1
              APRINT(ACNT) = PISFLX(I,2)
              CHEAD(ACNT) = 'PO4' // CISFLX(I)
              CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 174        CONTINUE
          END IF
        END IF
C
C       calculate and print total inflow of po4
C
        TOTIN= PADTOT(3)+ PIFLX(4)+ PISFLX(4,2)
        IF (PRINTU .GT. 0 .AND. PFLAG(8) .LE. LEV) THEN
          WRITE (PRINTU,2590) TOTIN
C
          IF (ADPOFG .EQ. 1) THEN
            WRITE (PRINTU,2600)
            WRITE (PRINTU,2200) (PCFLX8(J,2),J=1,4)
            WRITE (PRINTU,2610)
            WRITE (PRINTU,2220) (PCFLX3(J,2),J=1,4)
          END IF
C
C         print other gains/losses of po4
C
          WRITE (PRINTU,2620)
          WRITE (PRINTU,2630)
          WRITE (PRINTU,2250)  TCFLX7, (PCFLX7(J),J=1,5)
C
          WRITE (PRINTU,2640) PCFLX1(4)
C
          IF (ADPOFG .EQ. 1) THEN
            WRITE (PRINTU,2650)
            WRITE (PRINTU,2280) (PCFLX2(J,2),J=1,4)
          END IF
        END IF
        IF (BINU .GT. 0 .AND. ABS(BFLAG(8)) .LE. LEV) THEN
          ACNT = ACNT + 1
          APRINT(ACNT) = TOTIN
          CHEAD(ACNT) = 'PO4-INTOT'
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          IF (ADPOFG .EQ. 1) THEN
            DO 175 I= 1, 4
              ACNT = ACNT + 1
              APRINT(ACNT) = PCFLX8(I,2)
              CHEAD(ACNT) = 'PO4' // CCFLX8(I)
              CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 175        CONTINUE
            DO 176 I= 1, 4
              ACNT = ACNT + 1
              APRINT(ACNT) = PCFLX3(I,2)
              CHEAD(ACNT) = 'PO4' // CCFLX3(I)
              CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 176        CONTINUE
          END IF
          ACNT = ACNT + 1
          APRINT(ACNT) = TCFLX7
          CHEAD(ACNT) = 'PO4-PROCFLUX-TOT'
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          DO 177 I= 1, 5
            ACNT = ACNT + 1
            APRINT(ACNT) = PCFLX7(I)
            CHEAD(ACNT) = CCFLX7(I)
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 177      CONTINUE
          ACNT = ACNT + 1
          APRINT(ACNT) = PCFLX1(4)
          CHEAD(ACNT) = 'PO4-OUTDIS'
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          IF (ADPOFG .EQ. 1) THEN
            DO 178 I= 1, 4
              ACNT = ACNT + 1
              APRINT(ACNT) = PCFLX2(I,2)
              CHEAD(ACNT) = 'PO4' // CCFLX2(I)
              CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 178        CONTINUE
          END IF
        END IF
C
C       calculate and print total outflow of po4
C
        TOTOUT= PCFLX1(4) + PCFLX2(4,2)
        IF (PRINTU .GT. 0 .AND. PFLAG(8) .LE. LEV) THEN
          WRITE (PRINTU,2290) TOTOUT
C
          IF (NEXITS .GT. 1) THEN
            WRITE (PRINTU,2660)
            DO 179 N= 1,NEXITS
              WRITE (PRINTU,2310) N,PCFLX9(N,4)
 179        CONTINUE
C
            IF (ADPOFG .EQ. 1) THEN
              WRITE (PRINTU,2670)
              DO 180 N= 1,NEXITS
                WRITE (PRINTU,2330) N,(PCFX10(N,K,2), K=1,4)
 180          CONTINUE
            END IF
          END IF
        END IF
        IF (BINU .GT. 0 .AND. ABS(BFLAG(8)) .LE. LEV) THEN
          ACNT = ACNT + 1
          APRINT(ACNT) = TOTOUT
          CHEAD(ACNT) = 'PO4-OUTTOT'
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          DO 185 I= 1, NEXITS
            CALL INTCHR (I, I2, I1,
     O                   JLEN, CSTR)
            ACNT = ACNT + 1
            APRINT(ACNT) = PCFLX9(I,4)
            CHEAD(ACNT) = 'PO4-OUTDIS-EXIT'
            DO 181 IX= 1, JLEN
              CHEAD(ACNT) = TRIM(CHEAD(ACNT)) // CSTR(IX)
 181        CONTINUE
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
            IF (ADPOFG .EQ. 1) THEN
              DO 184 J= 1, 4
                ACNT = ACNT + 1
                APRINT(ACNT) = PCFX10(I,J,2)
                CHEAD(ACNT) = 'PO4' // CCFX10(J)
                DO 183 IX= 1, JLEN
                  CHEAD(ACNT) = TRIM(CHEAD(ACNT)) // CSTR(IX)
 183            CONTINUE
                CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 184          CONTINUE
            END IF
 185      CONTINUE
        END IF
C
C       material balance
C
C       calculate quantity of po4 entering rchres
        PO4IN= PADTOT(3)+ PIFLX(4)+ PISFLX(4,2)- PCFLX3(4,2)+ 
     #         PCFLX7(1)+ PCFLX7(2)+PCFLX7(4)
C
C       calculate net gain or loss of po4 to the rchres
        PO4DIF= PO4IN+ PCFLX7(3)+ PCFLX7(5)- PCFLX1(4)- PCFLX2(4,2)
C
        CALL BALCHK
     I              (I3,RCHNO,DATIM,MESSU,PRINTU,MSGFL,
     I               PRPO4S,PRPO4T,PO4IN,PO4DIF,UNITID,I1,
     M               NUWCNT)
C
      END IF
C
      IF (BINU .GT. 0 .AND. ABS(BFLAG(8)) .LE. LEV) THEN
C       write binary output
        CALL EXDATE(
     I              DATIM,
     O              EXDAT)
        IF (BFLAG(8) .GT. 0) THEN
C         at start of run, write the header
          WRITE (BINU) I0,'RCHRES  ',RCHNO,'NUTRX   ',
     1          (CLEN(I),(CHEAD(I)(J:J),J=1,CLEN(I)),I=1,ACNT)
C         set bflag to negative to not write headers anymore
          BFLAG(8) = -BFLAG(8)
        END IF
        WRITE (BINU) I1,'RCHRES  ',RCHNO,'NUTRX   ',UNITFG,
     1               LEV,(EXDAT(J),J=1,5),(APRINT(J),J=1,ACNT)
      END IF
C
      RETURN
      END
C
C     4.2(3).9.7
C
      SUBROUTINE   NUTRB
C
C     + + + PURPOSE + + +
C     Handle subroutine group nutrx
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION NUTRX2 + + +
      INCLUDE    'crhnu.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,J,K,N
C
C     + + + END SPECIFICATIONS + + +
C
      DO 20 I= 1, 4
        IF (RCDNUX(I) .GE. 1) THEN
          PAD(RCDNUX(I)+ IVL1)= NUIF1(I,1)
        END IF
        IF (TNUIFX(I) .GE. 1) THEN
          PAD(TNUIFX(I)+ IVL1)= TNUIF(I)
        END IF
        IF (NUCF1X(I) .GE. 1) THEN
          PAD(NUCF1X(I)+ IVL1)= NUCF1(I,1)
        END IF
        IF (TNUC1X(I) .GE. 1) THEN
          PAD(TNUC1X(I)+ IVL1)= TNUCF1(I)
        END IF
        DO 10 J= 1, 2
          IF (NUCF2X(I,J) .GE. 1) THEN
            PAD(NUCF2X(I,J)+ IVL1)= NUCF2(I,J,1)
          END IF
          IF (NUCF3X(I,J) .GE. 1) THEN
            PAD(NUCF3X(I,J)+ IVL1)= NUCF3(I,J,1)
          END IF
          IF (NUCF8X(I,J) .GE. 1) THEN
            PAD(NUCF8X(I,J)+ IVL1)= NUCF8(I,J,1)
          END IF
 10     CONTINUE
 20   CONTINUE
C
      DO 30 I= 1, 7
        IF (NUCF4X(I) .GE. 1) THEN
          PAD(NUCF4X(I)+ IVL1)= NUCF4(I,1)
        END IF
 30   CONTINUE
C
      DO 40 I= 1, 8
        IF (NUCF5X(I) .GE. 1) THEN
          PAD(NUCF5X(I)+ IVL1)= NUCF5(I,1)
        END IF
 40   CONTINUE
C
      IF (NUCF6X(1) .GE. 1) THEN
        PAD(NUCF6X(1)+ IVL1)= NUCF6(1,1)
      END IF
C
      DO 50 I= 1, 6
        IF (NUCF7X(I) .GE. 1) THEN
          PAD(NUCF7X(I)+ IVL1)= NUCF7(I,1)
        END IF
 50   CONTINUE
C
      IF (NEXITS .GT. 1) THEN
        DO 70 J= 1, 4
          DO 60 N= 1, NEXITS
            IF (NUCF9X(N,J) .GE. 1) THEN
              PAD(NUCF9X(N,J)+ IVL1)= NUCF9(N,J,1)
            END IF
            IF (TNUC2X(N,J) .GE. 1) THEN
              PAD(TNUC2X(N,J)+ IVL1)= TNUCF2(N,J)
            END IF
 60       CONTINUE
 70     CONTINUE
C
        DO 100 K= 1, 2
          DO 90 J= 1, 4
            DO 80 N= 1, NEXITS
              IF (NUC10X(N,J,K) .GE. 1) THEN
                PAD(NUC10X(N,J,K)+ IVL1)= NUCF10(N,J,K,1)
              END IF
 80         CONTINUE
 90       CONTINUE
 100    CONTINUE
      END IF
C
      DO 110 I= 1, 3
        IF (NUADDX(I) .GE. 1) THEN
          PAD(NUADDX(I)+ IVL1)= NUCF11(I,1)
        END IF
        IF (NUADWX(I) .GE. 1) THEN
          PAD(NUADWX(I)+ IVL1)= NUCF12(I,1)
        END IF
        IF (NUADPX(I) .GE. 1) THEN
          PAD(NUADPX(I)+ IVL1)= NUADEP(I)
        END IF
 110  CONTINUE

      DO 130 K= 1, 2
        DO 120 J= 1, 4
          IF (RCSNUX(J,K) .GE. 1) THEN
            PAD(RCSNUX(J,K)+ IVL1)= NUIF2(J,K,1)
          END IF
 120    CONTINUE
 130  CONTINUE
C
      RETURN
      END
C
C     4.2(3).8.7
C
      SUBROUTINE   NUTRP
C
C     + + + PURPOSE + + +
C     Handle subroutine group nutrx
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION NUTRX2 + + +
      INCLUDE    'crhnu.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I
C
C     + + + END SPECIFICATIONS + + +
C
      DO 10 I= 1,6
        IF (DNUSTX(I) .GE. 1) THEN
          PAD(DNUSTX(I) + IVL1)= DNUST(I)
        END IF
        IF (DNUS2X(I) .GE. 1) THEN
          PAD(DNUS2X(I) + IVL1)= DNUST2(I)
        END IF
 10   CONTINUE
C
      DO 20 I= 1,3
        IF (SNH4X(I) .GE. 1) THEN
          PAD(SNH4X(I) + IVL1)= SNH4(I)
        END IF
        IF (SPO4X(I) .GE. 1) THEN
          PAD(SPO4X(I) + IVL1)= SPO4(I)
        END IF
 20   CONTINUE
C
      DO 30 I= 1,12
        IF (RSNH4X(I) .GE. 1) THEN
          PAD(RSNH4X(I) + IVL1)= RSNH4(I)
        END IF
        IF (RSPO4X(I) .GE. 1) THEN
          PAD(RSPO4X(I) + IVL1)= RSPO4(I)
        END IF
 30   CONTINUE
C
      DO 40 I= 1,4
        IF (NUSTX(I) .GE. 1) THEN
          PAD(NUSTX(I) + IVL1)= NUST(I,1)
        END IF
 40   CONTINUE
C
      RETURN
      END
C
C     4.2(3).10.3.7
C
      SUBROUTINE   NUTRST
     I                    (LEV)
C
C     + + + PURPOSE + + +
C     Reset flux and state variables for subroutine group nutrx
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    LEV
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEV    - current output level (2-pivl,3-day,4-mon,5-ann)
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION NUTRX2 + + +
      INCLUDE    'crhnu.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,I4,I5,I6,I7
C
C     + + + EXTERNALS + + +
      EXTERNAL   SETVEC
C
C     + + + END SPECIFICATIONS + + +
C
      I4= 4
      I5= 5
      I6= 6
      I7= 7
C
C
C     no3
      NUIF1(1,LEV)= 0.0
      NUCF1(1,LEV)= 0.0
      NUCF11(1,LEV)= 0.0
      NUCF12(1,LEV)= 0.0
      CALL SETVEC
     I            (I6,0.0,
     O             NUCF4(1,LEV))
C
      IF (NEXITS .GT. 1) THEN
        CALL SETVEC
     I              (NEXITS,0.0,
     O               NUCF9(1,1,LEV))
      END IF
C
C     save storage for bal check
      NUST(1,LEV)= NUST(1,1)
C
C
C     tam
      IF (TAMFG .EQ. 1) THEN
        NUIF1(2,LEV)= 0.0
        NUCF1(2,LEV)= 0.0
        NUCF11(2,LEV)= 0.0
        NUCF12(2,LEV)= 0.0
C
        CALL SETVEC
     I              (I7,0.0,
     O               NUCF5(1,LEV))
C
        IF (ADNHFG .EQ. 1) THEN
          CALL SETVEC
     I                (I4,0.0,
     O                 NUIF2(1,1,LEV))
          CALL SETVEC
     I                (I4,0.0,
     O                 NUCF2(1,1,LEV))
          CALL SETVEC
     I                (I4,0.0,
     O                 NUCF3(1,1,LEV))
          CALL SETVEC
     I                (I4,0.0,
     O                 NUCF8(1,1,LEV))
        END IF
C
        IF (NEXITS .GT. 1) THEN
          CALL SETVEC
     I                (NEXITS,0.0,
     O                 NUCF9(1,2,LEV))
C
          IF (ADNHFG .EQ. 1) THEN
            DO 10 I= 1, 4
              CALL SETVEC
     I                    (NEXITS,0.0,
     M                     NUCF10(1,I,1,LEV))
 10         CONTINUE
          END IF
        END IF
C
C       save storage for bal check
        NUST(2,LEV)= NUST(2,1)
C
      END IF
C
C
C     no2
      IF (NO2FG .EQ. 1) THEN
        NUIF1(3,LEV)= 0.0
        NUCF1(3,LEV)= 0.0
        NUCF6(1,LEV)= 0.0
C
        IF (NEXITS .GT. 1) THEN
          CALL SETVEC
     I                (NEXITS,0.0,
     O                 NUCF9(1,3,LEV))
        END IF
C
C       save storage for bal check
        NUST(3,LEV)= NUST(3,1)
C
      END IF
C
C
C     po4
      IF (PO4FG .EQ. 1) THEN
        NUIF1(4,LEV)= 0.0
        NUCF1(4,LEV)= 0.0
        NUCF11(3,LEV)= 0.0
        NUCF12(3,LEV)= 0.0
C
        CALL SETVEC
     I              (I5,0.0,
     O               NUCF7(1,LEV))
C
        IF (ADPOFG .EQ. 1) THEN
          CALL SETVEC
     I                (I4,0.0,
     O                 NUIF2(1,2,LEV))
          CALL SETVEC
     I                (I4,0.0,
     O                 NUCF2(1,2,LEV))
          CALL SETVEC
     I                (I4,0.0,
     O                 NUCF3(1,2,LEV))
          CALL SETVEC
     I                (I4,0.0,
     O                 NUCF8(1,2,LEV))
        END IF
C
        IF (NEXITS .GT. 1) THEN
          CALL SETVEC
     I                (NEXITS,0.0,
     O                 NUCF9(1,4,LEV))
C
          IF (ADPOFG .EQ. 1) THEN
            DO 20 I= 1, 4
              CALL SETVEC
     I                    (NEXITS,0.0,
     M                     NUCF10(1,I,2,LEV))
 20         CONTINUE
          END IF
        END IF
C
C       save storage for bal check
        NUST(4,LEV)= NUST(4,1)
C
      END IF
C
      RETURN
      END
