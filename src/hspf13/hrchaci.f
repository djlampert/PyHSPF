C
C
C
      SUBROUTINE   PACID
C
C     + + + PURPOSE + + +
C     Process the input for section acidph
C
C     + + + COMMON BLOCKS + + +
      INCLUDE    'crhac.inc'
      INCLUDE    'crin2.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,J,I1,IX,IY,NCNTMP,NGQTMP,SCLU,SGRP
C
C     + + + EXTERNALS + + +
      EXTERNAL   ITABLE, RTABLE, ACINIT, OMSG, OMSTI, ZIPI
C
C     + + + INTRINSICS + + +
      INTRINSIC  ALOG10
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' PROCESSING INPUT FOR SECTION ACIDPH')
 2010 FORMAT (/,' FINISHED PROCESSING INPUT FOR SECTION ACIDPH')
C
C     + + + END SPECIFICATIONS + + +
C
      I1 = 1
C
      IF (OUTLEV.GT.1) THEN
        WRITE (MESSU,2000)
      END IF
C
C     set error and warning message counters
      I= 10
      J= 0
      CALL ZIPI(I,J,ACECNT)
      I= 5
      CALL ZIPI(I,J,ACWCNT)
C
C     table-type acid-flags
      IX= 121
      IY= 14
      CALL ITABLE
     I            (IX,I1,IY,UUNITS,
     M             ACFLAG)
C
C     table-type acid-parms
      IX= 122
      IY= 7
      CALL RTABLE
     I            (IX,I1,IY,UUNITS,
     M             ACPARM)
C
C     table-type acid-init
      IX= 123
      IY= 7
      CALL RTABLE
     I            (IX,I1,IY,UUNITS,
     M             ACCONC)
C
C     initialize storages
      DO 10 I= 1, 7
        ACSTOR(I,1)= ACCONC(I)*VOL
 10   CONTINUE
C
      ACPH= -ALOG10(ACCONC(5))
C
C     initialize all computed fluxes
      DO 30 I= 1, 7
        ACFLX1(I,1)= 0.0
        ACFLX3(I,1)= 0.0
        DO 20 J= 1,5
          ACFLX2(J,I,1)= 0.0
 20     CONTINUE
 30   CONTINUE
C
      DO 40 I= 1, 10
        ACFLXC(I,1)= 0.0
 40   CONTINUE
C
      DO 50 I= 1, 3
        ACFLXG(I,1)= 0.0
 50   CONTINUE
C
C     initialize number of chemicals, cons, gquals, and names, and
C     molecular weights
      CALL ACINIT
     I            (ACFLAG(2), 
     O             NUMCHM,NUMCON,NUMGQL,ACNAME,ACCONV)
C
C     check number of chemicals, cons, and gquals
      NCNTMP= NUMCON
      IF (NUMCON .EQ. 0) THEN
        NCNTMP= -999
      END IF
      NGQTMP= NUMGQL
      IF (NUMGQL .EQ. 0) THEN
        NGQTMP= -999
      END IF
C
      IF (NCNTMP .GT. NCONS .OR. NGQTMP .GT. NGQUAL) THEN
C       error - must simulate at least as many species in cons and gqual
C       as are used in acidph section
        CALL OMSTI (RCHNO)
        CALL OMSTI (NUMCON)
        CALL OMSTI (NCONS)
        CALL OMSTI (NUMGQL)
        CALL OMSTI (NGQUAL)
        SCLU= 351
        SGRP= 3
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             ECOUNT)
      END IF
C
      IF (OUTLEV.GT.1) THEN
        WRITE (MESSU,2010)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   ACINIT
     I                    (IMETHOD,
     O                     NCHEMS,NCONS,NGQLS,INAMES,MOLWT)
C
C     + + + PURPOSE + + +
C     Initialize common block versions of number of chemicals, number
C     of cons, number of gquals, chemical names, and molec wts
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      IMETHOD,NCHEMS,NCONS,NGQLS,INAMES(7,3)
      REAL         MOLWT(7)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IMETHOD - method 
C     NCHEMS - number of chemical species
C     NCONS  - number of CONServatives
C     NGQLS  - number of GQUALs
C     INAMES - chemical names
C     MOLWT  - molecular weights of chemicals
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,J
      CHARACTER*12 CNAMES(7)
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (3A4)
C
C     + + + END SPECIFICATIONS + + +
C
      IF (IMETHOD .EQ. 1 .OR. IMETHOD .EQ. 2 .OR. IMETHOD .EQ. 3) THEN
        NCHEMS    = 7
        NCONS     = 2
        NGQLS     = 0
        CNAMES(1) = '  TOTAL AL+3'
        CNAMES(2) = '   FREE AL+3'
        CNAMES(3) = '  TOTAL FE+3'
        CNAMES(4) = '   FREE FE+3'
        CNAMES(5) = '          H+'
        CNAMES(6) = ' TOT INORG C'
        CNAMES(7) = '  ALKALINITY'
        MOLWT(1) = 26.98
        MOLWT(2) = 26.98
        MOLWT(3) = 55.85
        MOLWT(4) = 55.85
        MOLWT(5) = 1.008
        MOLWT(6) = 12.01
        MOLWT(7) = 50.04
C
      ELSE IF (IMETHOD .EQ. 4) THEN
        NCHEMS    = 5
        NCONS     = 1
        NGQLS     = 0
        CNAMES(1) = '        Al+3'
        CNAMES(2) = '        Fe+3'
        CNAMES(3) = '        Mn+2'
        CNAMES(4) = '     ACIDITY'
        CNAMES(5) = '          H+'
        MOLWT(1) = 26.98
        MOLWT(2) = 55.85
        MOLWT(3) = 54.94
        MOLWT(4) = 50.04
        MOLWT(5) = 1.008
      END IF
C
      DO 10 I= 1, NCHEMS
        READ (CNAMES(I),1000) (INAMES(I,J),J=1,3)
 10   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   ACIDPH
     I                    (FSTCAL)
C
C     + + + PURPOSE + + +
C     Simulate acid-base chemistry of mine drainage-affected reaches
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       FSTCAL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FSTCAL - flag indicating whether this is the first call to acidph
C
C     + + + COMMON BLOCKS + + +
      INCLUDE   'crhac.inc'
      INCLUDE   'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,INUM,SCLU,SGRP,REQFG,TSSUB(2),FLGVAL
      REAL        AVDEPE
      DOUBLE PRECISION DACCNC(7),DACCNS(7),DACPM1,DACPM2,TWKELV,CO2,
     $                 RKAL1,RKAL2,RKAL3,RKAL4,RKALS1,RKALS2,RKALF1,
     $                 RKALF2,RKALF3,RKALF4,RKALF5,RKALF6,RKFE1,RKFE2,
     $                 RKFE3,RKFE4,RKFES1,RKFES2,RKFEF1,RKFEF2,RKFEF3,
     $                 RKCO21,RKCO22,RKH2O,RKSP,RKHCO2,ALOH,ALSO4,ALFL,
     $                 FEOH,FESO4,FEFL,FLFREE,SO4FRE,ALKH2O,ALKCO2,
     $                 ALKAL,ALKFE,DPH,DMOLWT(7),DACPM4,DACPM5
      CHARACTER*6 OPTYP,TSNAM,SECNAM,MSECNM,OPFGNM
C
C     + + + INTRINSICS + + +
      INTRINSIC  DLOG10
C
C     + + + EXTERNALS + + +
      EXTERNAL   OMSG,OMSTI,OMSTD,HREQTS
      EXTERNAL   ADVECT,ACDATA,ACCAL1,ACCAL2,ACCAL3
C
C     + + + DATA INITIALIZATIONS + + +
      DATA TSSUB/1,1/
      DATA OPTYP,SECNAM/'RCHRES','ACIDPH'/
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU = 351
      IF (FSTCAL .EQ. 1) THEN
C       check ratio of co2 to atmospheric co2
        IF (ACPARM(1) .LE. 0.0 .OR. ACPARM(1) .GT. 2.0) THEN
          ACPARM(1)= 1.0
C         warn that ratio has been changed
          SGRP = 2
          CALL OMSTD (DATIM)
          CALL OMSTI (RCHNO)
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ACWCNT(2))
        END IF
      END IF
C
C     get average depth
      IF (HYDRFG .EQ. 0) THEN
C       get depth from pad (input time series)
CTHJ        AVDEP= PAD(AVDFP + IVL1)
        REQFG= 3
        TSNAM= 'SAREA '
        MSECNM= 'HYDR  '
        CALL HREQTS (AVDFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,RCHNO,
     I               TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,FLGVAL,
     O               AVDEP)
      ELSE
C       get depth from section hydr
      END IF
C
C     convert depth to english units
      IF (UUNITS .EQ. 1) THEN
C       english
        AVDEPE= AVDEP
      ELSE
C       metric to english
        AVDEPE= AVDEP*3.28
      END IF
C
C     get inflows from pad - must convert to molar units
      DO 10 I= 1, NUMCHM
        IF (ACINFP(I) .GT. 0) THEN
          ACINFL(I,1)= PAD(ACINFP(I) + IVL1)/ACCONV(I)
        ELSE
          ACINFL(I,1)= 0.0
        END IF
 10   CONTINUE
C
C     advect all chemicals
      DO 20 I= 1, NUMCHM
        DACCNC(I)= ACCONC(I)
        CALL  ADVECT
     I               (ACINFL(I,1),VOLS,SROVOL,VOL,EROVOL,SOVOL,
     I                EOVOL,NEXITS,
     M                DACCNC(I),
     O                ACFLX1(I,1),ACFLX2(1,I,1))
        ACCONC(I)= DACCNC(I)
 20   CONTINUE
C
      IF (VOL .GT. 0.0) THEN
C       water in reach, attempt to simulate processes
C
C       get water temperature
        IF (HTFG .EQ. 0) THEN
C         water temperature is not simulated; get temperature from inpad
C         or constant value from user
          IF (ACPARM(3) .LE. 0.0) THEN
C           check inpad for available data
CTHJ        TW= PAD(TWFP + IVL1)
            REQFG= 3
            TSNAM= 'TW    '
            MSECNM= 'HTRCH '
            CALL HREQTS (TWFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,RCHNO,
     I                   TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,FLGVAL,
     O                   TW)
            TWKELV= TW + 273.16
          ELSE
C           get user-specified temperature from input parameters
            TWKELV= ACPARM(3) + 273.16
          END IF
        ELSE
C         water temperature from section htrch
          TWKELV= TW + 273.16
        END IF
C
C       check for sufficient water (at least 2 inches) in reach
        IF (AVDEPE .GT. 0.17) THEN
C         enough water to warrant simulation of chemical processes
C
C         get constants used by all routines
          DACPM1 = ACPARM(1)
          DACPM2 = ACPARM(2)
          CALL  ACDATA
     I                (TWKELV,ACFLAG(2),ACFLAG(3),DACPM1,DACPM2,
     O                 RKSP,CO2,RKHCO2,RKH2O,RKCO21,RKCO22,
     O                 RKAL1,RKAL2,RKAL3,RKAL4,RKALS1,RKALS2,
     O                 RKALF1,RKALF2,RKALF3,RKALF4,RKALF5,RKALF6,
     O                 RKFE1,RKFE2,RKFE3,RKFE4,RKFES1,RKFES2,
     O                 RKFEF1,RKFEF2,RKFEF3)
C
C         assign double-precision versions of concs
C         and save previous time step values
          DO 30 I= 1, NUMCHM
            DACCNS(I)= ACCONC(I)
            DACCNC(I)= ACCONC(I)
            DMOLWT(I)= ACCONV(I)
 30       CONTINUE
C
C         call the simulation routines, depending on option flag
          IF (ACFLAG(2) .EQ. 1) THEN
C           option 1: al(oh)3 (gibbsite) saturation and fe
            CALL ACCAL1
     I                 (DACCNS(1),DACCNS(3),CON(1),CON(2),CO2,
     I                  RKAL1,RKAL2,RKAL3,RKAL4,RKALS1,RKALS2,
     I                  RKALF1,RKALF2,RKALF3,RKALF4,RKALF5,RKALF6,
     I                  RKFE1,RKFE2,RKFE3,RKFE4,RKFES1,RKFES2,
     I                  RKFEF1,RKFEF2,RKFEF3,RKCO21,RKCO22,RKH2O,RKSP,
     I                  TWKELV,RKHCO2,
     M                  DACCNC(5),
     O                  DPH,DACCNC(2),ALOH,ALSO4,ALFL,DACCNC(4),FEOH,
     O                  FESO4,FEFL,DACCNC(7),ALKH2O,ALKCO2,ALKAL,ALKFE,
     O                  DACCNC(6),FLFREE,SO4FRE,INUM)
C
          ELSE IF (ACFLAG(2) .EQ. 2) THEN
C           option 2: fe(oh)3 saturation and al
            CALL ACCAL2
     I                 (DACCNS(1),DACCNS(3),CON(1),CON(2),CO2,
     I                  RKAL1,RKAL2,RKAL3,RKAL4,RKALS1,RKALS2,
     I                  RKALF1,RKALF2,RKALF3,RKALF4,RKALF5,RKALF6,
     I                  RKFE1,RKFE2,RKFE3,RKFE4,RKFES1,RKFES2,
     I                  RKFEF1,RKFEF2,RKFEF3,RKCO21,RKCO22,RKH2O,RKSP,
     I                  TWKELV,RKHCO2,
     M                  DACCNC(5),
     O                  DPH,DACCNC(2),ALOH,ALSO4,ALFL,DACCNC(4),FEOH,
     O                  FESO4,FEFL,DACCNC(7),ALKH2O,ALKCO2,ALKAL,ALKFE,
     O                  DACCNC(6),FLFREE,SO4FRE,INUM)
C
          ELSE IF (ACFLAG(2) .EQ. 3) THEN
C           option 3: no solid saturated and al and fe
            CALL ACCAL3
     I                 (DACCNS(1),DACCNS(3),CON(1),CON(2),DACCNS(7),
     I                  CO2,RKAL1,RKAL2,RKAL3,RKAL4,RKALS1,RKALS2,
     I                  RKALF1,RKALF2,RKALF3,RKALF4,RKALF5,RKALF6,
     I                  RKFE1,RKFE2,RKFE3,RKFE4,RKFES1,RKFES2,
     I                  RKFEF1,RKFEF2,RKFEF3,RKCO21,RKCO22,RKH2O,
     I                  TWKELV,RKHCO2,
     M                  DACCNC(5),
     O                  DPH,DACCNC(2),ALOH,ALSO4,ALFL,DACCNC(4),FEOH,
     O                  FESO4,FEFL,ALKH2O,ALKCO2,ALKAL,ALKFE,
     O                  DACCNC(6),FLFREE,SO4FRE,INUM)
C
          ELSE IF (ACFLAG(2) .EQ. 4) THEN
C           option 4: WV method (Al, Fe, Mn, acidity, conductivity)
            DACPM4 = ACPARM(4)
            DACPM5 = ACPARM(5)
            CALL ACCAL4
     I                 (DACCNS(1),DACCNS(2),DACCNS(3),DACCNS(4),CON(1),
     I                  DMOLWT(1),DMOLWT(2),DMOLWT(3),DACPM4,DACPM5,
     M                  DACCNC(5),
     O                  DPH,DACCNC(1),DACCNC(2),DACCNC(3),DACCNC(4))
C
          ELSE
C           error no. 2, invalid value of acflag(2) (this is a fatal error)
            SGRP = 1
            CALL OMSTD (DATIM)
            CALL OMSTI (RCHNO)
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 ACECNT(2))
C
          END IF
C
C         assign fluxes, storages, and real*4 concs
          DO 40 I= 1, NUMCHM
            ACFLX3(I,1)= (DACCNC(I) - DACCNS(I))*VOL
            ACSTOR(I,1)= DACCNC(I)*VOL
            ACCONC(I)  = DACCNC(I)
 40       CONTINUE
          ACPH= DPH
C
        ELSE
C         not enough water to warrant simulation of chemical processes
C         fluxes set to zero
          DO 50 I= 1, NUMCHM
            ACSTOR(I,1)= DACCNC(I)*VOL
            ACCONC(I)= DACCNC(I)
            ACFLX3(I,1)= 0.0
 50       CONTINUE
          ACPH= - DLOG10(DACCNC(5))
        END IF
      ELSE
C       reach is dry, set concentrations, storages and fluxes appropriately
        DO 60 I= 1, NUMCHM
          ACCONC(I)  = -1.0E+30
          ACSTOR(I,1)= 0.0
          ACFLX3(I,1)= 0.0
 60     CONTINUE
        ACPH = -1.0E+30
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   ACCAL1
     I                    (TOTAL,TOTFE,ST,FT,CO2,
     I                     RKAL1,RKAL2,RKAL3,RKAL4,RKALS1,RKALS2,
     I                     RKALF1,RKALF2,RKALF3,RKALF4,RKALF5,RKALF6,
     I                     RKFE1,RKFE2,RKFE3,RKFE4,RKFES1,RKFES2,
     I                     RKFEF1,RKFEF2,RKFEF3,RKC1,RKC2,RKW,RKSP,
     I                     TWKELV,RKHCO2,
     M                     H,
     O                     PH,ALFREE,ALOH,ALS,ALF,FEFREE,FEOH,FES,FEF,
     O                     ALK,ALKW,ALKC,ALKAL,ALKFE,TIC,F1,S1,INUM)
C
C     + + + PURPOSE + + +
C     Estimate pH value based on total Al, Fe conc. and Gibbsite control
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    INUM
      DOUBLE PRECISION TOTAL,TOTFE,FT,ST,ALK,ALKW,ALKC,ALKAL,ALKFE,TIC,
     $                 CO2,H,PH,ALFREE,ALOH,ALS,ALF,FEFREE,FEOH,FES,FEF,
     $                 S1,F1,RKAL1,RKAL2,RKAL3,RKAL4,RKALS1,RKALS2,
     $                 RKALF1,RKALF2,RKALF3,RKALF4,RKALF5,RKALF6,RKFE1,
     $                 RKFE2,RKFE3,RKFE4,RKFES1,RKFES2,RKFEF1,RKFEF2,
     $                 RKFEF3,RKC1,RKC2,RKW,RKSP,TWKELV,RKHCO2
C
C     + + + ARGUMENT DEFINITIONS + + +
C     TOTAL  - ???
C     TOTFE  - ???
C     ST     - ???
C     FT     - ???
C     CO2    - ???
C     RKAL1  - ???
C     RKAL2  - ???
C     RKAL3  - ???
C     RKAL4  - ???
C     RKALS1 - ???
C     RKALS2 - ???
C     RKALF1 - ???
C     RKALF2 - ???
C     RKALF3 - ???
C     RKALF4 - ???
C     RKALF5 - ???
C     RKALF6 - ???
C     RKFE1  - ???
C     RKFE2  - ???
C     RKFE3  - ???
C     RKFE4  - ???
C     RKFES1 - ???
C     RKFES2 - ???
C     RKFEF1 - ???
C     RKFEF2 - ???
C     RKFEF3 - ???
C     RKC1   - ???
C     RKC2   - ???
C     RKW    - ???
C     RKSP   - ???
C     TWKELV - ???
C     RKHCO2 - ???
C     H      - ???
C     PH     - ???
C     ALFREE - ???
C     ALOH   - ???
C     ALS    - ???
C     ALF    - ???
C     FEFREE - ???
C     FEOH   - ???
C     FES    - ???
C     FEF    - ???
C     ALK    - ???
C     ALKW   - ???
C     ALKC   - ???
C     ALKAL  - ???
C     ALKFE  - ???
C     TIC    - ???
C     F1     - ???
C     S1     - ???
C     INUM   - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    NXX,I,MIN,MAX,ITER
      DOUBLE PRECISION AAL0,AAL1,AAL2,AAL3,AAL4,A,B,C,D,E,F,G,AC0,AC1,
     $                 AC2,ERROR,NEWTON,FRTIO1,SRTIO1,HMIN,HMAX,SLOPE1,
     $                 ERMAX,ERMIN,ALTMAX,AFE0,AFE1,AFE2,AFE3,AFE4,
     $                 FRTIO2,SRTIO2,FETX(3),FET,SLOPE
C
C     + + + INTRINSICS + + +
      INTRINSIC  DABS, DLOG10, DSQRT
C
C     + + + EXTERNALS + + +
      EXTERNAL   NEWTON,CONV
C
C     + + + END SPECIFICATIONS + + +
C
C     initialize parameters used in pH determining algorithm
C
      ITER = 0
      MIN  = 0
      MAX  = 0
      HMIN = 0.0
      HMAX = 0.0
      ERMIN= 0.0
      ERMAX= 0.0
C
      INUM = 0
C
C     set initial estimate of [H+]
      IF (H .GT. 1.0D-14 .AND. H .LT. 1.0D-1) THEN
C       use previous value
      ELSE
C       use ph=5
        H = 10.0**(-5)
      END IF
C
C     compute free aluminum conc. based on Gibbsite mineral solubility
 100  CONTINUE
      INUM   = INUM + 1
      ALFREE = RKSP*(H/RKW)**3
C
C     Al(+3) __ aloh(+2) __ al(oh)2(+1) __ al(oh)3(0) __ al(oh)4(-)
C
      AAL0 = 1.D0/(1.D0 + RKAL1/H*(1.D0 + RKAL2/H*(1.D0 + RKAL3/H*
     $            (1.D0 + RKAL4/H))))
      AAL1 = RKAL1/H*AAL0
      AAL2 = RKAL2/H*AAL1
      AAL3 = RKAL3/H*AAL2
      AAL4 = RKAL4/H*AAL3
C
C     Fe(+3) __ feoh(+2) __ fe(oh)2(+1) __ fe(oh)3(0) __ fe(oh)4(-)
C
      AFE0 = 1.D0/(1.D0 + RKFE1/H*(1.D0 + RKFE2/H*(1.D0 + RKFE3/H*
     $            (1.D0 + RKFE4/H))))
      AFE1 = RKFE1/H*AFE0
      AFE2 = RKFE2/H*AFE1
      AFE3 = RKFE3/H*AFE2
      AFE4 = RKFE4/H*AFE3
C
      FETX(1) = TOTFE
C
 200  CONTINUE
      DO 300 I=1,2
        FRTIO1 = 0.0
        FRTIO2 = 0.0
        SRTIO1 = 0.0
        SRTIO2 = 0.0
        FET    = FETX(I)
C
C       compute sulfate complexation
        IF (ST .GT. 0.D0) THEN
          A      = RKALS2*ALFREE + RKFES2*FET*AFE0
          B      = 1.D0 + RKALS1*ALFREE + RKFES1*FET*AFE0
          C      = -ST
          S1     = (-B + DSQRT(B*B - 4.D0*A*C))/(2.D0*A)
          SRTIO1 = AAL0*(RKALS1*S1 + RKALS2*S1*S1)
          SRTIO2 = AFE0*(RKFES1*S1 + RKFES2*S1*S1)
        END IF
C
C       compute fluoride complexation
        IF (FT .GT. 0.D0) THEN
          A      = -FT
          B      = 1.D0 + RKALF1*ALFREE + RKFEF1*FET*AFE0
          C      = RKALF2*ALFREE + RKFEF2*FET*AFE0
          D      = RKALF3*ALFREE + RKFEF3*FET*AFE0
          E      = RKALF4*ALFREE
          F      = RKALF5*ALFREE
          G      = RKALF6*ALFREE
          NXX    = 6
          F1     = NEWTON(NXX,FT,A,B,C,D,E,F,G)
          FRTIO1 = AAL0*(RKALF1*F1 + RKALF2*F1**2.D0 + RKALF3*F1**3.D0 +
     $             RKALF4*F1**4.D0 + RKALF5*F1**5.D0 + RKALF6*F1**6.D0)
          FRTIO2 = AFE0*(RKFEF1*F1 + RKFEF2*F1**2.D0 + RKFEF3*F1**3.D0)
        END IF
C
        FETX(I+1) = TOTFE / (1.0 + FRTIO2 + SRTIO2)
C
        IF (DABS((FETX(I+1) - FETX(I))/FETX(I)) .LT. 0.001) GO TO 400
C
 300  CONTINUE
C
      SLOPE1  = (FETX(3) - FETX(2))/(FETX(2) - FETX(1))
      FETX(1) = FETX(3) + SLOPE1/(1.0 - SLOPE1)*(FETX(3) - FETX(2))
C
      GO TO 200
C
 400  CONTINUE
C
      FEFREE = FET * AFE0
      FEOH   = FET - FEFREE
      FEF    = FET * FRTIO2
      FES    = FET * SRTIO2
      ALOH   = (ALFREE / AAL0) - ALFREE
      ALF    = (ALFREE + ALOH) * FRTIO1
      ALS    = (ALFREE + ALOH) * SRTIO1
C
C     compute total aluminum concentration
      ALTMAX = ALOH + ALF + ALS + ALFREE
C
      ERROR  = ALTMAX - TOTAL
      IF (DABS(ERROR) .LE. 1.0D-8) GO TO 600
      IF (MIN*MAX .NE. 1) THEN
        IF (ERROR .LT. 0.0) THEN
          HMAX  = H
          ERMAX = ERROR
          MAX   = 1
          IF (MIN .EQ. 1) GO TO 500
          H = H * 2.0
        ELSE
          HMIN  = H
          ERMIN = ERROR
          MIN   = 1
          IF (MAX .EQ. 1) GO TO 500
          H = H * 0.5
       END IF
       GO TO 100
      END IF
C
 500  CONTINUE
      ITER = ITER + 1
C
      IF (ITER .LE. 1) THEN
        SLOPE  = (ERMAX - ERMIN)/(HMAX - HMIN)
        H      = HMAX - ERMAX/SLOPE
      ELSE
        CALL CONV
     I            (HMIN,HMAX,ERMIN,ERMAX,ERROR,
     M             H)
      END IF
C
      GO TO 100
C
 600  CONTINUE
C
      PH = - DLOG10(H)
C
C     H2c03(*) __ hco3(-) __ co3(-2)
C
      AC0  = 1.D0/(1.D0 + RKC1/H*(1.D0 + RKC2/H))
      AC1  = RKC1/H*AC0
      AC2  = RKC2/H*AC1
      TIC  = CO2*(273.15/TWKELV)*RKHCO2/AC0
C
C     compute components of alkalinity CO3--AL--H2O
C
      ALKC  = (AC1 + 2.D0*AC2)*TIC
      ALKAL = (4.D0*AAL4 + 3.D0*AAL3 + 2.D0*AAL2 + AAL1)*(ALOH + ALFREE)
      ALKFE = (4.D0*AFE4 + 3.D0*AFE3 + 2.D0*AFE2 + AFE1)*FET
      ALKW  = RKW/H - H
      ALK   = ALKC + ALKW + ALKAL + ALKFE
C
      RETURN
      END
C
C
C
      SUBROUTINE   ACCAL2
     I                    (TOTAL,TOTFE,ST,FT,CO2,
     I                     RKAL1,RKAL2,RKAL3,RKAL4,RKALS1,RKALS2,
     I                     RKALF1,RKALF2,RKALF3,RKALF4,RKALF5,RKALF6,
     I                     RKFE1,RKFE2,RKFE3,RKFE4,RKFES1,RKFES2,
     I                     RKFEF1,RKFEF2,RKFEF3,RKC1,RKC2,RKW,RKSP,
     I                     TWKELV,RKHCO2,
     M                     H,
     O                     PH,ALFREE,ALOH,ALS,ALF,FEFREE,FEOH,FES,FEF,
     O                     ALK,ALKW,ALKC,ALKAL,ALKFE,TIC,F1,S1,INUM)
C
C     + + + PURPOSE + + +
C     Estimate pH value based on total Al, Fe conc. and Fe(OH)3 control
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    INUM
      DOUBLE PRECISION TOTAL,TOTFE,FT,ST,ALK,ALKW,ALKC,ALKAL,ALKFE,TIC,
     $                 CO2,H,PH,ALFREE,ALOH,ALS,ALF,FEFREE,FEOH,FES,FEF,
     $                 S1,F1,RKAL1,RKAL2,RKAL3,RKAL4,RKALS1,RKALS2,
     $                 RKALF1,RKALF2,RKALF3,RKALF4,RKALF5,RKALF6,RKFE1,
     $                 RKFE2,RKFE3,RKFE4,RKFES1,RKFES2,RKFEF1,RKFEF2,
     $                 RKFEF3,RKC1,RKC2,RKW,RKSP,TWKELV,RKHCO2
C
C     + + + ARGUMENT DEFINITIONS + + +
C     TOTAL  - ???
C     TOTFE  - ???
C     ST     - ???
C     FT     - ???
C     CO2    - ???
C     RKAL1  - ???
C     RKAL2  - ???
C     RKAL3  - ???
C     RKAL4  - ???
C     RKALS1 - ???
C     RKALS2 - ???
C     RKALF1 - ???
C     RKALF2 - ???
C     RKALF3 - ???
C     RKALF4 - ???
C     RKALF5 - ???
C     RKALF6 - ???
C     RKFE1  - ???
C     RKFE2  - ???
C     RKFE3  - ???
C     RKFE4  - ???
C     RKFES1 - ???
C     RKFES2 - ???
C     RKFEF1 - ???
C     RKFEF2 - ???
C     RKFEF3 - ???
C     RKC1   - ???
C     RKC2   - ???
C     RKW    - ???
C     RKSP   - ???
C     TWKELV - ???
C     RKHCO2 - ???
C     H      - ???
C     PH     - ???
C     ALFREE - ???
C     ALOH   - ???
C     ALS    - ???
C     ALF    - ???
C     FEFREE - ???
C     FEOH   - ???
C     FES    - ???
C     FEF    - ???
C     ALK    - ???
C     ALKW   - ???
C     ALKC   - ???
C     ALKAL  - ???
C     ALKFE  - ???
C     TIC    - ???
C     F1     - ???
C     S1     - ???
C     INUM   - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    NXX,I,MIN,MAX,ITER
      DOUBLE PRECISION AAL0,AAL1,AAL2,AAL3,AAL4,A,B,C,D,E,F,G,AC0,AC1,
     $                 AC2,ERROR,NEWTON,FRTIO1,SRTIO1,HMIN,HMAX,SLOPE1,
     $                 ERMAX,ERMIN,FETMAX,AFE0,AFE1,AFE2,AFE3,AFE4,
     $                 FRTIO2,SRTIO2,ALTX(3),ALT,SLOPE
C
C     + + + INTRINSICS + + +
      INTRINSIC  DABS, DLOG10, DSQRT
C
C     + + + EXTERNALS + + +
      EXTERNAL   CONV,NEWTON
C
C     + + + END SPECIFICATIONS + + +
C
C     initialize parameters used in pH determining algorithm
C
      ITER = 0
      MIN  = 0
      MAX  = 0
      HMIN = 0.0
      HMAX = 0.0
      ERMIN= 0.0
      ERMAX= 0.0
C
      INUM = 0
C
C     set initial estimate of [H+]
      IF (H .GT. 1.0D-14 .AND. H .LT. 1.0D-1) THEN
C       use previous h
      ELSE
C       use ph = 5
        H = 10.0**(-5)
      END IF
C
C     compute free Aluminum conc. based on Gibbsite mineral solubility
 100  CONTINUE
      INUM   = INUM + 1
      FEFREE = 10.0**(RKSP)*H**3
C
C     Al(+3) __ aloh(+2) __ al(oh)2(+1) __ al(oh)3(0) __ al(oh)4(-)
C
      AAL0 = 1.D0/(1.D0 + RKAL1/H*(1.D0 + RKAL2/H*(1.D0 + RKAL3/H*
     $            (1.D0 + RKAL4/H))))
      AAL1 = RKAL1/H*AAL0
      AAL2 = RKAL2/H*AAL1
      AAL3 = RKAL3/H*AAL2
      AAL4 = RKAL4/H*AAL3
C
C
C     Fe(+3) __ feoh(+2) __ fe(oh)2(+1) __ fe(oh)3(0) __ fe(oh)4(-)
C
      AFE0 = 1.D0/(1.D0 + RKFE1/H*(1.D0 + RKFE2/H*(1.D0 + RKFE3/H*
     $            (1.D0 + RKFE4/H))))
      AFE1 = RKFE1/H*AFE0
      AFE2 = RKFE2/H*AFE1
      AFE3 = RKFE3/H*AFE2
      AFE4 = RKFE4/H*AFE3
C
      ALTX(1) = TOTAL
C
 200  CONTINUE
      DO 300 I= 1, 2
        FRTIO1 = 0.0
        FRTIO2 = 0.0
        SRTIO1 = 0.0
        SRTIO2 = 0.0
        ALT    = ALTX(I)
C
C       compute sulfate complexation
        IF (ST .GT. 0.D0) THEN
          A      = RKALS2*ALT*AAL0 + RKFES2*FEFREE
          B      = 1.D0 + RKALS1*ALT*AAL0 + RKFES1*FEFREE
          C      = -ST
          S1     = (-B + DSQRT(B*B - 4.D0*A*C))/(2.D0*A)
          SRTIO1 = AAL0*(RKALS1*S1 + RKALS2*S1*S1)
          SRTIO2 = AFE0*(RKFES1*S1 + RKFES2*S1*S1)
        END IF
C
C       compute fluoride complexation
        IF (FT .GT. 0.D0) THEN
          A      = -FT
          B      = 1.D0 + RKALF1*ALT*AAL0 + RKFEF1*FEFREE
          C      = RKALF2*ALT*AAL0 + RKFEF2*FEFREE
          D      = RKALF3*ALT*AAL0 + RKFEF3*FEFREE
          E      = RKALF4*ALT*AAL0
          F      = RKALF5*ALT*AAL0
          G      = RKALF6*ALT*AAL0
          NXX    = 6
          F1     = NEWTON(NXX,FT,A,B,C,D,E,F,G)
          FRTIO1 = AAL0*(RKALF1*F1 + RKALF2*F1**2.D0 + RKALF3*F1**3.D0 +
     $             RKALF4*F1**4.D0 + RKALF5*F1**5.D0 + RKALF6*F1**6.D0)
          FRTIO2 = AFE0*(RKFEF1*F1 + RKFEF2*F1**2.D0 + RKFEF3*F1**3.D0)
        END IF
C
        ALTX(I+1) = TOTAL / (1.0 + FRTIO1 + SRTIO1)
C
        IF (DABS((ALTX(I+1) - ALTX(I))/ALTX(I)) .LT. 0.001) GO TO 400
C
 300  CONTINUE
C
      SLOPE1  = (ALTX(3) - ALTX(2))/(ALTX(2) - ALTX(1))
      ALTX(1) = ALTX(3) + SLOPE1/(1.0 - SLOPE1)*(ALTX(3) - ALTX(2))
C
      GO TO 200
C
 400  CONTINUE
C
      FEOH   = (FEFREE/AFE0) - FEFREE
      FEF    = (FEFREE+FEOH) * FRTIO2
      FES    = (FEFREE+FEOH) * SRTIO2
      ALFREE = ALT * AAL0
      ALOH   = ALT - ALFREE
      ALF    = ALT * FRTIO1
      ALS    = ALT * SRTIO1
C
C     compute total aluminum concentration
      FETMAX = FEOH + FEF + FES + FEFREE
C
      ERROR  = FETMAX - TOTFE
      IF (DABS(ERROR) .LE. 1.0D-8) GO TO 600
      IF (MIN*MAX .NE. 1) THEN
        IF (ERROR .LT. 0.0) THEN
          HMAX  = H
          ERMAX = ERROR
          MAX   = 1
          IF (MIN .EQ. 1) GO TO 500
          H = H * 2.0
        ELSE
          HMIN  = H
          ERMIN = ERROR
          MIN   = 1
          IF (MAX .EQ. 1) GO TO 500
          H = H * 0.5
       END IF
       GO TO 100
      END IF
C
 500  CONTINUE
C
      ITER = ITER + 1
C
      IF (ITER .LE. 1) THEN
        SLOPE  = (ERMAX - ERMIN)/(HMAX-HMIN)
        H      = HMAX - ERMAX/SLOPE
      ELSE
        CALL CONV
     I            (HMIN,HMAX,ERMIN,ERMAX,ERROR,
     M             H)
      ENDIF
C
      GO TO 100
C
 600  CONTINUE
C
      PH = - DLOG10(H)
C
C     H2c03(*) __ hco3(-) __ co3(-2)
      AC0  = 1.D0/(1.D0 + RKC1/H*(1.D0 + RKC2/H))
      AC1  = RKC1/H*AC0
      AC2  = RKC2/H*AC1
      TIC  = CO2*(273.15/TWKELV)*RKHCO2/AC0
C
C     compute components of alkalinity CO3--AL--H2O
      ALKC  = (AC1 + 2.D0*AC2)*TIC
      ALKAL = (4.D0*AAL4 + 3.D0*AAL3 + 2.D0*AAL2 + AAL1)*(ALOH + ALFREE)
      ALKFE = (4.D0*AFE4 + 3.D0*AFE3 + 2.D0*AFE2 + AFE1)*(FEOH + FEFREE)
      ALKW  = RKW/H - H
      ALK   = ALKC + ALKW + ALKAL + ALKFE
C
      RETURN
      END
C
C
C
      SUBROUTINE   ACCAL3
     I                    (TOTAL,TOTFE,ST,FT,ALK,CO2,
     I                     RKAL1,RKAL2,RKAL3,RKAL4,RKALS1,RKALS2,
     I                     RKALF1,RKALF2,RKALF3,RKALF4,RKALF5,RKALF6,
     I                     RKFE1,RKFE2,RKFE3,RKFE4,RKFES1,RKFES2,
     I                     RKFEF1,RKFEF2,RKFEF3,RKC1,RKC2,RKW,
     I                     TWKELV,RKHCO2,
     M                     H,
     O                     PH,ALFREE,ALOH,ALS,ALF,FEFREE,FEOH,FES,FEF,
     O                     ALKW,ALKC,ALKAL,ALKFE,TIC,F1,S1,INUM)
C
C     + + + PURPOSE + + +
C     Estimate pH value based on total Al and Fe concs. and alkalinity
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    INUM
      DOUBLE PRECISION TOTAL,TOTFE,FT,ST,PH,ALK,ALF,ALS,ALOH,ALFREE,CO2,
     $                 H,FEF,FES,FEOH,FEFREE,S1,F1,ALKAL,ALKW,ALKC,
     $                 ALKFE,TIC,RKAL1,RKAL2,RKAL3,RKAL4,RKALS1,RKALS2,
     $                 RKALF1,RKALF2,RKALF3,RKALF4,RKALF5,RKALF6,RKFE1,
     $                 RKFE2,RKFE3,RKFE4,RKFES1,RKFES2,RKFEF1,RKFEF2,
     $                 RKFEF3,RKC1,RKC2,RKW,TWKELV,RKHCO2
C
C     + + + ARGUMENT DEFINITIONS + + +
C     TOTAL  - ???
C     TOTFE  - ???
C     ST     - ???
C     FT     - ???
C     ALK    - ???
C     CO2    - ???
C     RKAL1  - ???
C     RKAL2  - ???
C     RKAL3  - ???
C     RKAL4  - ???
C     RKALS1 - ???
C     RKALS2 - ???
C     RKALF1 - ???
C     RKALF2 - ???
C     RKALF3 - ???
C     RKALF4 - ???
C     RKALF5 - ???
C     RKALF6 - ???
C     RKFE1  - ???
C     RKFE2  - ???
C     RKFE3  - ???
C     RKFE4  - ???
C     RKFES1 - ???
C     RKFES2 - ???
C     RKFEF1 - ???
C     RKFEF2 - ???
C     RKFEF3 - ???
C     RKC1   - ???
C     RKC2   - ???
C     RKW    - ???
C     TWKELV - ???
C     RKHCO2 - ???
C     H      - ???
C     PH     - ???
C     ALFREE - ???
C     ALOH   - ???
C     ALS    - ???
C     ALF    - ???
C     FEFREE - ???
C     FEOH   - ???
C     FES    - ???
C     FEF    - ???
C     ALKW   - ???
C     ALKC   - ???
C     ALKAL  - ???
C     ALKFE  - ???
C     TIC    - ???
C     F1     - ???
C     S1     - ???
C     INUM   - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    NXX,I,MIN,MAX,ITER
      DOUBLE PRECISION AAL0,AAL1,AAL2,AAL3,AAL4,A,B,C,D,E,F,G,AC0,AC1,
     $                 AC2,ALT1,ALKT,ERROR,NEWTON,FRTIO1,SRTIO1,HMIN,
     $                 HMAX,ALTX(3),SLOPE1,ERMAX,ERMIN,AFE0,AFE1,AFE2,
     $                 AFE3,AFE4,FRTIO2,SRTIO2,FETX(3),FET1,SLOPE2,SLOPE
C
C     + + + INTRINSICS + + +
      INTRINSIC  DABS, DLOG10, DSQRT
C
C     + + + EXTERNALS + + +
      EXTERNAL   NEWTON, CONV
C
C     + + + END SPECIFICATIONS + + +
C
C     initialize parameters used in pH determining algorithm
C
      ITER   = 0
      MIN    = 0
      MAX    = 0
      HMIN   = 0.0
      HMAX   = 0.0
      ERMIN  = 0.0
      ERMAX  = 0.0
C
      INUM   = 0
C
C     set initial estimate of [H+]
      IF (H .GT. 1.0D-14 .AND. H .LT. 1.0D-1) THEN
C       use previous ph
      ELSE
C       use ph = 5
        H  = 10.0**(-5)
      END IF
C
 100  CONTINUE
C
      INUM = INUM + 1
C
C     Al(+3) __ aloh(+2) __ al(oh)2(+1) __ al(oh)3(0) __ al(oh)4(-)
C
      AAL0 = 1.D0/(1.D0 + RKAL1/H*(1.D0 + RKAL2/H*(1.D0 + RKAL3/H*
     $            (1.D0 + RKAL4/H))))
      AAL1 = RKAL1/H*AAL0
      AAL2 = RKAL2/H*AAL1
      AAL3 = RKAL3/H*AAL2
      AAL4 = RKAL4/H*AAL3
C
C
C     Fe(+3) __ feoh(+2) __ fe(oh)2(+1) __ fe(oh)3(0) __ fe(oh)4(-)
C
      AFE0 = 1.D0/(1.D0 + RKFE1/H*(1.D0 + RKFE2/H*(1.D0 + RKFE3/H*
     $            (1.D0 + RKFE4/H))))
      AFE1 = RKFE1/H*AFE0
      AFE2 = RKFE2/H*AFE1
      AFE3 = RKFE3/H*AFE2
      AFE4 = RKFE4/H*AFE3
C
      ALTX(1) = TOTAL
      FETX(1) = TOTFE
C
 200  CONTINUE
      DO 300 I= 1, 2
        FRTIO1 = 0.0
        FRTIO2 = 0.0
        SRTIO1 = 0.0
        SRTIO2 = 0.0
        ALT1   = ALTX(I)
        FET1   = FETX(I)
C
C       compute sulfate complexation
        IF (ST .GT. 0.D0) THEN
          A      = RKALS2*ALT1*AAL0 + RKFES2*FET1*AFE0
          B      = 1.D0 + RKALS1*ALT1*AAL0 + RKFES1*FET1*AFE0
          C      = -ST
          S1     = (-B + DSQRT(B*B - 4.D0*A*C))/(2.D0*A)
          SRTIO1 = AAL0*(RKALS1*S1 + RKALS2*S1*S1)
          SRTIO2 = AFE0*(RKFES1*S1 + RKFES2*S1*S1)
        END IF
C
C       compute fluoride complexation
        IF (FT .GT. 0.D0) THEN
          A      = -FT
          B      = 1.D0 + RKALF1*ALT1*AAL0 + RKFEF1*FET1*AFE0
          C      = RKALF2*ALT1*AAL0 + RKFEF2*FET1*AFE0
          D      = RKALF3*ALT1*AAL0 + RKFEF3*FET1*AFE0
          E      = RKALF4*ALT1*AAL0
          F      = RKALF5*ALT1*AAL0
          G      = RKALF6*ALT1*AAL0
          NXX    = 6
          F1     = NEWTON(NXX,FT,A,B,C,D,E,F,G)
          FRTIO1 = AAL0*(RKALF1*F1 + RKALF2*F1**2.D0 + RKALF3*F1**3.D0 +
     $             RKALF4*F1**4.D0 + RKALF5*F1**5.D0 + RKALF6*F1**6.D0)
          FRTIO2 = AFE0*(RKFEF1*F1 + RKFEF2*F1**2.D0 + RKFEF3*F1**3.D0)
        ENDIF
C
        ALTX(I+1) = TOTAL / (1.0 + FRTIO1 + SRTIO1)
        FETX(I+1) = TOTFE / (1.0 + FRTIO2 + SRTIO2)
C
        IF (DABS((ALTX(I+1) - ALTX(I))/ALTX(I)) .LT. 0.001) THEN
          IF (TOTFE .LE. 0.0) GO TO 400
          IF (DABS((FETX(I+1) - FETX(I))/FETX(I)) .LT. 0.001) GO TO 400
        END IF
C
 300  CONTINUE
C
      SLOPE1  = (ALTX(3) - ALTX(2)) / (ALTX(2) - ALTX(1))
C
      IF (TOTFE .GT. 0.0) THEN
        SLOPE2= (FETX(3) - FETX(2))/(FETX(2) - FETX(1))
      END IF
C
      ALTX(1) = ALTX(3) + SLOPE1/(1.0 - SLOPE1)*(ALTX(3) - ALTX(2))
C
      IF (TOTFE .GT. 0.0) THEN
        FETX(1) = FETX(3) + SLOPE2/(1.0 - SLOPE2)*(FETX(3) - FETX(2))
      END IF
C
      GO TO 200
C
 400  CONTINUE
C
C     H2c03(*) __ hco3(-) __ co3(-2)
C
      AC0  = 1.D0/(1.D0 + RKC1/H*(1.D0 + RKC2/H))
      AC1  = RKC1/H*AC0
      AC2  = RKC2/H*AC1
      TIC  = CO2*(273.15/TWKELV)*RKHCO2/AC0
C
C     compute components of alkalinity CO3--AL--H2O
C
      ALKC  = (AC1 + 2.D0*AC2)*TIC
      ALKAL = (4.D0*AAL4 + 3.D0*AAL3 + 2.D0*AAL2 + AAL1)*ALT1
      ALKFE = (4.D0*AFE4 + 3.D0*AFE3 + 2.D0*AFE2 + AFE1)*FET1
      ALKW  = RKW/H - H
      ALKT  = ALKC + ALKAL + ALKW + ALKFE
C
      ERROR = ALKT - ALK
      IF (DABS(ERROR) .LT. 1.0D-8) GO TO 600
      IF (MIN*MAX .NE. 1) THEN
        IF (ERROR .LT. 0.0) THEN
C         under-estimate, set upper limit, set max = 1
          HMAX  = H
          ERMAX = ERROR
          MAX   = 1
          IF (MIN .EQ. 1) GO TO 500
          H     = H * 0.5
        ELSE
C         over-estimate, set lower limit, set min = 1
          HMIN  = H
          ERMIN = ERROR
          MIN   = 1
          IF (MAX .EQ. 1) GO TO 500
          H     = H * 2.0
        END IF
        GO TO 100
      ENDIF
C
 500  CONTINUE
C
      ITER  = ITER + 1
C
      IF (ITER .LE. 1) THEN
        SLOPE = (ERMAX - ERMIN)/(HMAX - HMIN)
        H     = HMAX - ERMAX/SLOPE
      ELSE
        CALL CONV
     I            (HMIN,HMAX,ERMIN,ERMAX,ERROR,
     M             H)
      ENDIF
C
      GO TO 100
C
 600  CONTINUE
C
      PH     = -DLOG10(H)
      FEFREE = FET1 * AFE0
      FEOH   = FET1 - FEFREE
      FEF    = FET1 * FRTIO2
      FES    = FET1 * SRTIO2
      ALFREE = ALT1 * AAL0
      ALOH   = ALT1 - ALFREE
      ALF    = ALT1 * FRTIO1
      ALS    = ALT1 * SRTIO1
C
      RETURN
      END
C
C
C
      SUBROUTINE   ACCAL4
     I                    (TOTAL,TOTFE,TOTMN,TOTAC,COND,
     I                     RMLWAL,RMLWFE,RMLWMN,RCOEFM,RCOEFH,
     M                     H,
     O                     PH,NEWAL,NEWFE,NEWMN,NEWAC)
C
C     + + + PURPOSE + + +
C     estimates pH value based on total Al, final ferric iron conc.
C	 (Fe 3+), Manganese (Mn 2+)
C
C     + + + DUMMY ARGUMENTS + + +
      DOUBLE PRECISION TOTAL,TOTFE,TOTMN,TOTAC,COND,RMLWAL,RMLWFE,
     $	 RMLWMN,RCOEFM,RCOEFH,H,PH,NEWAL,NEWFE,NEWMN,NEWAC
C
C     + + + ARGUMENT DEFINITIONS + + +
C     TOTAL  - initial total aluminum concentration     mol/L
C     TOTFE  - initial total iron concentration         mol/L
C     TOTMN  - initial total manganese concentration    mol/L
C     TOTAC  - initial acidity                          mol/L
C     COND   - conductivity    (microSiemens/cm)        uS/cm
C     RMLWAL - mole weight aluminum
C     RMLWFE - mole weight of iron
C     RMLWMN - mole weight of manganese
C     RCOEFM - coefficient for metal precipitation (user-defined)
C     RCOEFH - coefficient for pH (user-defined)
C     H      - hydrogen concentration                   mol/L
C     PH     - new pH
C     NEWAL  - new concentration of aluminum            mol/L
C     NEWFE  - new concentration of iron                mol/L
C     NEWMN  - new concentration of manganese           mol/L
C     NEWAC  - new calculated acidity                   mol/L
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    IVALFE,IVALAL,IVALMN
      DOUBLE PRECISION RSTDPOT,RPARTPS,RFACTA,RFACTB,RFACTC,RKSP2,               
     $    RFACTD,RION,RHPOW,RKPOW,RGAMMA,RACTH,RACTOH,RFE,RAL,
     $	  RMN,RAC,RIH,RIPH,RCAL,RCFE,RCMN,RK,RCAC,TMPMC,
     $    TMPAL,TMPFE,TMPMN,TMPALR,TMPFER,TMPMNR
C
C     + + + INTRINSICS + + +
      INTRINSIC  DLOG10, DSQRT
C
C     + + + END SPECIFICATIONS + + +
C
C     initialize constants used in pH determining algorithm
      IVALFE = 3
      IVALAL = 3
      IVALMN = 2
      RFACTA = 0.5029
      RFACTD = 10.0**(-3.0)
      RFACTB = 0.014*RFACTD
      RFACTC = 0.24
      RSTDPOT = -0.615
      RPARTPS = 0.21
      RKSP1 = 10**(-33)
      RKSP2 = 10**(-38.46)
C
C     set initial estimate of [H+]
      IF (H .LT. 10.0D-2) THEN
C       use previous value
        RIH = H
      ELSE
C       use ph=6.5
        RIH = 10.0**(-6.5)
      END IF
C
C     calculate initial pH 
      RIPH = - DLOG10(H)
C
C     compute free aluminum conc. 
C
      RION = COND*RFACTB
C
      RKPOW = -RFACTA * (IVALAL**2.D0) *
     $         ((DSQRT(RION) / (1.D0+DSQRT(RION))) - RFACTC * RION)
      RGAMMA = 10.D0**(RKPOW)
      RACTH = H
      RACTOH = 10.D0**(-14) / RACTH
C
      RAL = (RKSP1 / (RGAMMA * RACTOH**3.D0)) * TOTAL
C
      IF (RIH .LT. 10.0D-7) THEN
        TMPAL = TOTAL * (1.D0 - RCOEFM)
        RCAL = TMPAL
      ELSE
        TMPALR = TOTAL * RGAMMA
        IF (TMPALR .GE. RAL) THEN
          TMPAL = RAL
          RCAL = TOTAL - TMPALR
        ELSE
          RCAL = 0.
        END IF
      END IF
      NEWAL = TOTAL - RCAL
C
C     compute ferric iron concentration
C
      RKPOW = -RFACTA * (IVALFE**2.D0) *
     $        ((DSQRT(RION) / (1.D0+DSQRT(RION))) - RFACTC * RION)
      RGAMMA = 10.D0**(RKPOW)
      RACTH = H
      RACTOH = 10.D0**(-14) / RACTH
C
      RFE = (RKSP2 / (RGAMMA * RACTOH**3.D0)) * TOTFE
C
      IF (RIH .LT. 10.0D-7) THEN
        TMPFE = TOTFE * (1.D0 - RCOEFM)
        RCFE = TMPFE
      ELSE
        TMPFER = TOTFE * RGAMMA
        IF (TOTFE .GE. RFE) THEN
          TMPFE = RFE
          RCFE = TOTFE - TMPFER
        ELSE
          RCFE = 0.
        END IF
      END IF
      NEWFE = TOTFE - RCFE
C
C     compute manganese concentration
C
      RKPOW = -RFACTA * (IVALMN**2.D0) * 
     $        ((DSQRT(RION) / (1.D0+DSQRT(RION))) - RFACTC * RION)
      RGAMMA = 10.D0**(RKPOW)
      RACTH = H 
      RK = 10.D0**(2.D0*RSTDPOT / RFACTA)
      RMN = (RK * (RACTH**2.D0)) / (RGAMMA * (RPARTPS**(0.5))) 
C
      IF (RIH .LT. 10.0D-7) THEN
        TMPMN = TOTMN * (1.D0 - RCOEFM)
        RCMN = TMPMN
      ELSE
        TMPMNR = TOTMN * RGAMMA
        IF (TOTMN .GE. RMN) THEN
          TMPMN = RMN
          RCMN = TOTMN - TMPMNR
        ELSE
          RCMN = 0.
        END IF
      END IF
      NEWMN = TOTMN - RCMN
C
C     recalculate pH
C
      RHPOW = DLOG10(RIH)
      RACTH = 10.D0**RHPOW
      RKPOW = -RFACTA * 
     $        ((DSQRT(RION) / (1.D0+DSQRT(RION))) - RFACTC * RION)
C     use the change in conc. values
      RGAMMA = 10.D0**(RKPOW)
      TMPMC = ((3.D0 * RCFE) + (3.D0 * RCAL) + (2.D0 * RCMN)) * 
     $        RGAMMA * (1.D0 - ((RIPH + RCOEFH)/10.D0))
C     new pH
      PH = - DLOG10(RACTH + TMPMC)
C
C     new H value
      H = 10.0**(-PH)
C
C     compute acidity
C
C     use the initial conc. of the metals
      RAC = (3.D0 * TOTFE) + (3.D0 * TOTAL) + 
     $      (2.D0 * TOTMN) +  10.D0**(-RIPH)
C
C     use the change in conc. of the metals
      RCAC = (3.D0 * RCFE) + (3.D0 * RCAL) + 
     $       (2.D0 * RCMN) + 10.D0**(-PH)
C
      IF (PH .GT. 7.D0) THEN
        NEWAC = 0.D0
      ELSE 
        IF (PH .LT. 2.D0) THEN
C         use the new acidity calculated from the recalculated conc. of metals
          NEWAC = RCAC
        ELSE
          NEWAC = (RCAC*TOTAC)/RAC
        END IF
      END IF
C
      RETURN
      END	
C
C
C
      SUBROUTINE   ACDATA
     I                    (TWKELV,OPTFLG,KSPFLG,FRACO2,RKSPIN,
     O                     RKSP,CO2,RKHCO2,RKH2O,RKCO21,RKCO22,
     O                     RKAL1,RKAL2,RKAL3,RKAL4,RKALS1,RKALS2,
     O                     RKALF1,RKALF2,RKALF3,RKALF4,RKALF5,RKALF6,
     O                     RKFE1,RKFE2,RKFE3,RKFE4,RKFES1,RKFES2,
     O                     RKFEF1,RKFEF2,RKFEF3)
C
C
C     + + + PURPOSE + + +
C     Initialize constants and assign temperature-dependent values to
C     constants used by acidph routines
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     OPTFLG,KSPFLG
      DOUBLE PRECISION TWKELV,FRACO2,RKSPIN,RKSP,CO2,RKHCO2,RKH2O,
     $                 RKCO21,RKCO22,RKAL1,RKAL2,RKAL3,RKAL4,RKALS1,
     $                 RKALS2,RKALF1,RKALF2,RKALF3,RKALF4,RKALF5,RKALF6,
     $                 RKFE1,RKFE2,RKFE3,RKFE4,RKFES1,RKFES2,RKFEF1,
     $                 RKFEF2,RKFEF3
C
C     + + + ARGUMENT DEFINITIONS + + +
C     TWKELV - ???
C     OPTFLG - ???
C     KSPFLG - ???
C     FRACO2 - ???
C     RKSPIN - ???
C     RKSP   - ???
C     CO2    - ???
C     RKHCO2 - ???
C     RKH2O  - ???
C     RKCO21 - ???
C     RKCO22 - ???
C     RKAL1  - ???
C     RKAL2  - ???
C     RKAL3  - ???
C     RKAL4  - ???
C     RKALS1 - ???
C     RKALS2 - ???
C     RKALF1 - ???
C     RKALF2 - ???
C     RKALF3 - ???
C     RKALF4 - ???
C     RKALF5 - ???
C     RKALF6 - ???
C     RKFE1  - ???
C     RKFE2  - ???
C     RKFE3  - ???
C     RKFE4  - ???
C     RKFES1 - ???
C     RKFES2 - ???
C     RKFEF1 - ???
C     RKFEF2 - ???
C     RKFEF3 - ???
C
C     + + + LOCAL VARIABLES + + +
      DOUBLE PRECISION TA,ATMCO2
C
C     + + + INTRINSICS + + +
      INTRINSIC   DLOG10,DEXP
C
C     + + + END SPECIFICATIONS + + +
C
C     Al - OH and Al - SO4 complexation constants
      RKAL1  = 1.03D-5
      RKAL2  = 7.14D-6
      RKAL3  = 1.0D-14
      RKAL4  = 94.2
      RKALS1 = 1.63D3
      RKALS2 = 1.29D5
C
C     Al - F complexation constants
      RKALF1 = 1.05D7
      RKALF2 = 5.77D12
      RKALF3 = 1.07D17
      RKALF4 = 5.37D19
      RKALF5 = 8.33D20
      RKALF6 = 7.49D20
C
C     Fe - F complexation constants
      RKFEF1 = 1.0D6
      RKFEF2 = 1.585D9
      RKFEF3 = 5.012D11
C
C     Fe - OH and Fe - SO4 complexation constants
      RKFE1  = 6.457D-3
      RKFE2  = 3.162D-4
      RKFE3  = 3.981D-8
      RKFE4  = 3.162D-9
      RKFES1 = 1.413D4
      RKFES2 = 2.399D5
C
C     atmospheric CO2 (atm)
      ATMCO2 = 3.16D-4
C
C     first dissociation constant of H2CO3
      RKCO21 = 10.**(545.56 + 0.12675*TWKELV -
     $         215.21*DLOG10(TWKELV) - 17052./TWKELV)
C
C     second dissociation constant of H2CO3
      RKCO22 = 10.**(-2902.39/TWKELV - 0.02379*TWKELV + 6.498)
C
C     ion product of water (Stumm and Morgan, 1981, p. 127)
      RKH2O  = 10.0**(-4470.99/TWKELV + 6.0875 - 0.01706*TWKELV)
C
C     -log(henry's law constant of co2) in units of mole/liter/atm
      TA     = -2385.73/TWKELV + 14.0184 - 0.0152642*TWKELV
C
C     use ideal gas law to convert to dimensionless form
      RKHCO2 = 10.**(-TA)*.082057*TWKELV
C
C     compute CO2 concentration as fraction of atmospheric value
      CO2    = FRACO2*ATMCO2*10.**(-TA)
C
C     ksp of solid mineral
      IF (OPTFLG .EQ. 1) THEN
C       gibbsite
        IF (KSPFLG .EQ. 2) THEN
C         microcrystalline (flag=2)
          RKSP= 2.308D-33
        ELSE IF (KSPFLG .EQ. 3) THEN
C         natural (flag=3)
          RKSP= 6.068D-34
        ELSE IF (KSPFLG .EQ. 4) THEN
C         synthetic (flag=4)
          RKSP= 1.329D-34
        ELSE IF (KSPFLG .EQ. 5) THEN
C         user value (flag=5)
          RKSP= RKSPIN
        ELSE
C         amorphous (flag=1 or flag=invalid)
          RKSP= 6.501D-32
        END IF
        RKSP= RKSP*DEXP(32.0632 - 9560./TWKELV)
C
      ELSE IF (OPTFLG .EQ. 2) THEN
C       fe(oh)3
        RKSP= 2.7
C
      ELSE
C       ksp not used
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   ACIACC
     I                    (FRMROW,TOROW)
C
C     + + + PURPOSE + + +
C     Accumulate fluxes in module section ACIDPH for printout
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    FRMROW,TOROW
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FRMROW - source row of flux array being accumulated
C     TOROW  - target row of flux array being accumulated
C
C     + + + COMMON BLOCKS + + +
      INCLUDE    'crhac.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,I7,I10
C
C     + + + EXTERNALS + + +
      EXTERNAL   ACCVEC
C
C     + + + END SPECIFICATIONS + + +
C
      I7 = 7
      I10= 10
C
C     inflows
      CALL ACCVEC
     I             (I7,ACINFL(1,FRMROW),
     M              ACINFL(1,TOROW))
C
C     outflows
      CALL ACCVEC
     I             (I7,ACFLX1(1,FRMROW),
     M              ACFLX1(1,TOROW))
C
C     individual exit outflows
      IF (NEXITS .GT. 1) THEN
        DO 10 I= 1, NUMCHM
          CALL ACCVEC
     I                 (NEXITS,ACFLX2(1,I,FRMROW),
     M                  ACFLX2(1,I,TOROW))
 10     CONTINUE
      END IF
C
C     computed fluxes
      CALL ACCVEC
     I             (I7,ACFLX3(1,FRMROW),
     M              ACFLX3(1,TOROW))
C
C     section CONS fluxes
      CALL ACCVEC
     I             (I10,ACFLXC(1,FRMROW),
     M              ACFLXC(1,TOROW))
C
C     section GQUAL fluxes
      CALL ACCVEC
     I             (I7,ACFLXG(1,FRMROW),
     M              ACFLXG(1,TOROW))
C
      RETURN
      END
C
C
C
      SUBROUTINE   ACIDRB
C
C     + + + PURPOSE + + +
C     Section ACIDPH
C     put current values of all mean-valued output time series in pad
C
C     + + + COMMON BLOCKS + + +
      INCLUDE   'crhac.inc'
      INCLUDE   'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,J
C
C     + + + END SPECIFICATIONS + + +
C
      DO 10 I= 1, NUMCHM
C       outflows
        IF (ACF1FP(I) .GE. 1) THEN
          PAD(ACF1FP(I)+ IVL1)= ACFLX1(I,1)*ACCONV(I)
        END IF
C       inflows        
        IF (RCACIX(I) .GE. 1) THEN
          PAD(RCACIX(I)+ IVL1)= ACINFL(I,1)*ACCONV(I)
        END IF
 10   CONTINUE
C
C     individual exit outflows
      IF (NEXITS .GT. 1) THEN
        DO 30 J= 1, NUMCHM
          DO 20 I= 1, NEXITS
            IF (ACF2FP(I,J) .GE. 1) THEN
              PAD(ACF2FP(I,J) + IVL1) = ACFLX2(I,J,1)*ACCONV(I)
            END IF
 20       CONTINUE
 30     CONTINUE
      END IF
C
C     section cons fluxes
      DO 40 I= 1, NUMCON
        IF (ACFCFP(I) .GE. 1) THEN
          PAD(ACFCFP(I) + IVL1) = ACFLXC(I,1)
        END IF
 40   CONTINUE
C
C     section gqual fluxes
      DO 50 I= 1, NUMGQL
        IF (ACFGFP(I) .GE. 1) THEN
          PAD(ACFGFP(I) + IVL1) = ACFLXG(I,1)
        END IF
 50   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   ACIDRP
C
C     + + + PURPOSE + + +
C     Section ACIDPH
C     put current values of all point-valued output time series in pad
C
C     + + + COMMON BLOCKS + + +
      INCLUDE   'crhac.inc'
      INCLUDE   'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I
C
C     + + + END SPECIFICATIONS + + +
C
C     state variables (concentrations and storages)
      DO 10 I= 1, NUMCHM
        IF (ACCNFP(I) .GE. 1) THEN
          PAD(ACCNFP(I) + IVL1)= ACCONC(I)
        END IF
        IF (ACSTFP(I) .GE. 1) THEN
          PAD(ACSTFP(I) + IVL1)= ACSTOR(I,1)*ACCONV(I)
        END IF
 10   CONTINUE
C
C     ph value
      IF (ACPHFP .GE. 1) THEN
        PAD(ACPHFP + IVL1) = ACPH
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   ACIPRT
     I                    (UNITFG,LEV,PRINTU,BINU)
C
C     + + + PURPOSE + + +
C     Convert quantities from internal to external units, calculate
C     materials balance, and print results
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     UNITFG,LEV,PRINTU,BINU
C
C     + + + ARGUMENT DEFINITIONS + + +
C     UNITFG - printout units flag, 1=english, 2=metric
C     LEV    - level of printout, 2=pivl, 3=day, 4=month, 5=year
C     PRINTU - unit number for printout
C     BINU   - fortran unit number on which to write binary output
C
C     + + + COMMON BLOCKS + + +
      INCLUDE     'crhac.inc'
      INCLUDE     'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I0,I1,I2,I,IX,J,ACNT,CLEN(1+NUMCHM*(9+NEXITS*2)),JLEN,
     $            EXDAT(5)
      REAL        FACTA,FACTAC,FACTB,FACTC,PRST2(7),PRST2S(7),PRIF(7),
     $            PRCF1(7),PRCF2(5,7),PRCF3(7),CHMIN,CHMDIF,
     $            APRINT(1+NUMCHM*(9+NEXITS*2))
      CHARACTER*8 CONCU(2),UNITID
      CHARACTER*4 MASSU,MASSUM
      CHARACTER*1   CSTR(2)
      CHARACTER*12  STAVAR(NUMCHM)
      CHARACTER*256 CHEAD(1+NUMCHM*(9+NEXITS*2))
C
C     + + + EXTERNALS + + +
      EXTERNAL    TRNVEC, BALCHK, INTCHR, EXDATE
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' ','*** ACIDPH ***')
 2010 FORMAT (/,' ','  STATE VARIABLES',13X,7(2X,3A4),/)
 2015 FORMAT (3A4)
 2020 FORMAT (' ','    CONCENTRATION  ',A8,3X,7(4X,1PE10.3))
 2030 FORMAT (' ','    STORAGE  (',A4,')',11X,7(4X,1PE10.3))
 2040 FORMAT (/,' ','    pH VALUE',22X,F10.2)
 2050 FORMAT (/,' ','  FLUXES',/)
 2060 FORMAT (' ','    INFLOW   (',A4,')',11X,7(4X,1PE10.3))
 2070 FORMAT (' ','    OUTFLOW  (',A4,')',11X,7(4X,1PE10.3))
 2080 FORMAT (' ','    INDIVIDUAL EXIT OUTFLOWS  (',A4,')')
 2090 FORMAT (' ',6X,'EXIT',I2,18X,7(4X,1PE10.3))
 2100 FORMAT ('    ',A4)
C
C     + + + END SPECIFICATIONS + + +
C
      I0= 0
      I1= 1
      I2= 2
C
C     initialize array counter for binary printout, store variable
C     names in local strings for use in building binary headers
      ACNT = 0
      DO 5 I= 1, NUMCHM
        WRITE (STAVAR(I), 2015) (ACNAME(I,J),J=1,3)
 5    CONTINUE
C
      CONCU(1) = '(MOLE/L)'
      CONCU(2) = '  (MG/L)'
      MASSUM   = 'MOLE'
C
      DO 20 I= 1, 5
        DO 10 J= 1, 7
          PRCF2(I,J)= 0.0
 10     CONTINUE
 20   CONTINUE
C
C     assign values to conversion factors
      IF (UNITFG .EQ. 1) THEN
C       printout is in english system
        MASSU= '  LB'
        IF (UUNITS .EQ. 1) THEN
C         english to english
          FACTA=  6.23E-2
          FACTB=  0.0
          FACTC= 28.3
        ELSE
C         metric to english
          FACTA=  2.2
          FACTB=  0.0
          FACTC=  1.0E3
        END IF
      ELSE
C       printout is in metric system
        MASSU= '  KG'
        IF (UUNITS .EQ. 1) THEN
C         english to metric
          FACTA=  2.83E-2
          FACTB=  0.0
          FACTC= 28.3
        ELSE
C         metric to metric
          FACTA=  1.0
          FACTB=  0.0
          FACTC=  1.0E3
        END IF
      END IF
C
C     convert to external units
      DO 30 I= 1, NUMCHM
C
C       storages
        PRST2(I) = ACSTOR(I,1)*FACTA*ACCONV(I)
        PRST2S(I)= ACSTOR(I,LEV)*FACTA*ACCONV(I)
C
C       inflows
        PRIF(I)  = ACINFL(I,LEV)*FACTA*ACCONV(I)
C
C       outflows
        PRCF1(I) = ACFLX1(I,LEV)*FACTA*ACCONV(I)
C
C       exit outflows
        IF (NEXITS .GT. 1) THEN
          FACTAC= FACTA*ACCONV(I)
          CALL TRNVEC
     I                (NEXITS,ACFLX2(1,I,LEV),FACTAC,FACTB,
     O                 PRCF2(1,I))
        END IF
C
C       computed fluxes
        PRCF3(I)= ACFLX3(I,LEV)*FACTA*ACCONV(I)
C
 30   CONTINUE
C
C     print results
C
      IF (PRINTU .GT. 0 .AND. PFLAG(11) .LE. LEV) THEN
        WRITE (PRINTU,2000)
C
C       state variable names
        WRITE (PRINTU,2010) ((ACNAME(I,J),J=1,3),I=1,NUMCHM)
C
C       concentrations
C
C       molar conc units
        IF (ACFLAG(1) .EQ. 1 .OR. ACFLAG(1) .EQ. 3) THEN
          WRITE (PRINTU,2020) CONCU(1),(ACCONC(I),I=1,NUMCHM)
        END IF
C
C       mass conc units (mg/l)
        IF (ACFLAG(1) .EQ. 2 .OR. ACFLAG(1) .EQ. 3) THEN
          WRITE (PRINTU,2020) CONCU(2),(ACCONV(I)*ACCONC(I)*1000.,
     $              I=1,NUMCHM)
        END IF
C
C       storages
C
C       molar units
        IF (ACFLAG(1) .EQ. 1 .OR. ACFLAG(1) .EQ. 3) THEN
          WRITE (PRINTU,2030) MASSUM,((ACSTOR(I,1)*FACTC),I=1,NUMCHM)
        END IF
C
C       mass units
        IF (ACFLAG(1) .EQ. 2 .OR. ACFLAG(1) .EQ. 3) THEN
          WRITE (PRINTU,2030) MASSU,(PRST2(I),I=1,NUMCHM)
        END IF
C
C       ph value
        WRITE (PRINTU,2040) ACPH
C
C       fluxes
        WRITE (PRINTU,2050)
C
C       inflows
C
C       molar units
        IF (ACFLAG(1) .EQ. 1 .OR. ACFLAG(1) .EQ. 3) THEN
          WRITE (PRINTU,2060) MASSUM,((ACINFL(I,LEV)*FACTC),I=1,NUMCHM)
        END IF
C
C       mass units
        IF (ACFLAG(1) .EQ. 2 .OR. ACFLAG(1) .EQ. 3) THEN
          WRITE (PRINTU,2060) MASSU,(PRIF(I),I=1,NUMCHM)
        END IF
C
C       outflows
C
C       molar units
        IF (ACFLAG(1) .EQ. 1 .OR. ACFLAG(1) .EQ. 3) THEN
          WRITE (PRINTU,2070) MASSUM,((ACFLX1(I,LEV)*FACTC),I=1,NUMCHM)
        END IF
C
C       mass units
        IF (ACFLAG(1) .EQ. 2 .OR. ACFLAG(1) .EQ. 3) THEN
          WRITE (PRINTU,2070) MASSU,(PRCF1(I),I=1,NUMCHM)
        END IF
C
C       exit outflows
        IF (NEXITS .GT. 1) THEN
C
C         molar units
          IF (ACFLAG(1) .EQ. 1 .OR. ACFLAG(1) .EQ. 3) THEN
            WRITE (PRINTU,2080) MASSUM
            DO 50 I= 1, NEXITS
              WRITE (PRINTU,2090) I,((ACFLX2(I,J,LEV)*FACTC),J=1,NUMCHM)
 50         CONTINUE
          END IF
C
C         mass units
          IF (ACFLAG(1) .EQ. 2 .OR. ACFLAG(1) .EQ. 3) THEN
            WRITE (PRINTU,2080) MASSU
            DO 60 I= 1, NEXITS
              WRITE (PRINTU,2090) I,(PRCF2(I,J),J=1,NUMCHM)
 60         CONTINUE
          END IF
        END IF
C
C       material balance check
        WRITE (UNITID,2100) MASSU
      END IF
C
      IF (BINU .GT. 0 .AND. ABS(BFLAG(11)) .LE. LEV) THEN
        IF (ACFLAG(1) .EQ. 1 .OR. ACFLAG(1) .EQ. 3) THEN
          DO 70 I= 1, NUMCHM
            ACNT = ACNT + 1
            APRINT(ACNT) = ACCONC(I)
            CHEAD(ACNT) = TRIM(STAVAR(I)) // '-CONC-'
            CHEAD(ACNT) = TRIM(CHEAD(ACNT)) // CONCU(1)
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 70       CONTINUE
        END IF
        IF (ACFLAG(1) .EQ. 2 .OR. ACFLAG(1) .EQ. 3) THEN
          DO 80 I= 1, NUMCHM
            ACNT = ACNT + 1
            APRINT(ACNT) = ACCONV(I)*ACCONC(I)*1000.
            CHEAD(ACNT) = TRIM(STAVAR(I)) // '-CONC-'
            CHEAD(ACNT) = TRIM(CHEAD(ACNT)) // CONCU(2)
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 80       CONTINUE
        END IF
        IF (ACFLAG(1) .EQ. 1 .OR. ACFLAG(1) .EQ. 3) THEN
          DO 90 I= 1, NUMCHM
            ACNT = ACNT + 1
            APRINT(ACNT) = ACSTOR(I,1)*FACTC
            CHEAD(ACNT) = TRIM(STAVAR(I)) // '-CONC-'
            CHEAD(ACNT) = TRIM(CHEAD(ACNT)) // MASSUM
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 90       CONTINUE
        END IF
        IF (ACFLAG(1) .EQ. 2 .OR. ACFLAG(1) .EQ. 3) THEN
          DO 100 I= 1, NUMCHM
            ACNT = ACNT + 1
            APRINT(ACNT) = PRST2(I)
            CHEAD(ACNT) = TRIM(STAVAR(I)) // '-CONC-'
            CHEAD(ACNT) = TRIM(CHEAD(ACNT)) // MASSU
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 100      CONTINUE
        END IF
        ACNT = ACNT + 1
        APRINT(ACNT) = ACPH
        CHEAD(ACNT) = 'pH'
        CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
        IF (ACFLAG(1) .EQ. 1 .OR. ACFLAG(1) .EQ. 3) THEN
          DO 110 I= 1, NUMCHM
            ACNT = ACNT + 1
            APRINT(ACNT) = ACINFL(I,LEV)*FACTC
            CHEAD(ACNT) = TRIM(STAVAR(I)) // '-CONC-'
            CHEAD(ACNT) = TRIM(CHEAD(ACNT)) // MASSUM
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 110      CONTINUE
        END IF
        IF (ACFLAG(1) .EQ. 2 .OR. ACFLAG(1) .EQ. 3) THEN
          DO 120 I= 1, NUMCHM
            ACNT = ACNT + 1
            APRINT(ACNT) = PRIF(I)
            CHEAD(ACNT) = TRIM(STAVAR(I)) // '-CONC-'
            CHEAD(ACNT) = TRIM(CHEAD(ACNT)) // MASSU
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 120      CONTINUE
        END IF
        IF (ACFLAG(1) .EQ. 1 .OR. ACFLAG(1) .EQ. 3) THEN
          DO 130 I= 1, NUMCHM
            ACNT = ACNT + 1
            APRINT(ACNT) = ACFLX1(I,LEV)*FACTC
            CHEAD(ACNT) = TRIM(STAVAR(I)) // '-CONC-'
            CHEAD(ACNT) = TRIM(CHEAD(ACNT)) // MASSUM
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 130      CONTINUE
        END IF
        IF (ACFLAG(1) .EQ. 2 .OR. ACFLAG(1) .EQ. 3) THEN
          DO 140 I= 1, NUMCHM
            ACNT = ACNT + 1
            APRINT(ACNT) = PRCF1(I)
            CHEAD(ACNT) = TRIM(STAVAR(I)) // '-CONC-'
            CHEAD(ACNT) = TRIM(CHEAD(ACNT)) // MASSU
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 140      CONTINUE
        END IF
        IF (NEXITS .GT. 1) THEN
          IF (ACFLAG(1) .EQ. 1 .OR. ACFLAG(1) .EQ. 3) THEN
            DO 170 I= 1, NEXITS
              CALL INTCHR (I, I2, I1,
     O                     JLEN, CSTR)
              DO 160 J= 1, NUMCHM
                ACNT = ACNT + 1
                APRINT(ACNT) = ACFLX2(I,J,LEV)*FACTC
                CHEAD(ACNT) = TRIM(STAVAR(J)) // '-OUTFLOW-EXIT-'
                DO 150 IX= 1, JLEN
                  CHEAD(ACNT) = TRIM(CHEAD(ACNT)) // CSTR(IX)
 150            CONTINUE
                CHEAD(ACNT) = TRIM(CHEAD(ACNT)) // '-'
                CHEAD(ACNT) = TRIM(CHEAD(ACNT)) // MASSUM
                CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 160          CONTINUE
 170        CONTINUE
          END IF
          IF (ACFLAG(1) .EQ. 2 .OR. ACFLAG(1) .EQ. 3) THEN
            DO 200 I= 1, NEXITS
              CALL INTCHR (I, I2, I1,
     O                     JLEN, CSTR)
              DO 190 J= 1, NUMCHM
                ACNT = ACNT + 1
                APRINT(ACNT) = PRCF2(I,J)
                CHEAD(ACNT) = TRIM(STAVAR(J)) // '-OUTFLOW-EXIT-'
                DO 180 IX= 1, JLEN
                  CHEAD(ACNT) = TRIM(CHEAD(ACNT)) // CSTR(IX)
 180            CONTINUE
                CHEAD(ACNT) = TRIM(CHEAD(ACNT)) // '-'
                CHEAD(ACNT) = TRIM(CHEAD(ACNT)) // MASSU
                CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 190          CONTINUE
 200        CONTINUE
          END IF
        END IF
      END IF
C
      DO 210 I= 1, NUMCHM
        CHMIN =  PRIF(I)
        CHMDIF= CHMIN - PRCF1(I)
        CHMDIF= CHMDIF + PRCF3(I)
        IF (PRCF3(I) .GT. 0.0) THEN
          CHMIN=  CHMIN + PRCF3(I)
        END IF
C
        J= 3
        CALL BALCHK
     I             (J,RCHNO,DATIM,MESSU,PRINTU,MSGFL,
     I              PRST2S(I),PRST2(I),CHMIN,CHMDIF,UNITID,I,
     M              ACWCNT(1))
 210  CONTINUE
C
      IF (BINU .GT. 0 .AND. ABS(BFLAG(11)) .LE. LEV) THEN
C       write binary output
        CALL EXDATE(
     I              DATIM,
     O              EXDAT)
        IF (BFLAG(11) .GT. 0) THEN
C         at start of run, write the header
          WRITE (BINU) I0,'RCHRES  ',RCHNO,'ADCALC  ',
     1          (CLEN(I),(CHEAD(I)(J:J),J=1,CLEN(I)),I=1,ACNT)
C         set bflag to negative to not write headers anymore
          BFLAG(11) = -BFLAG(11)
        END IF
        WRITE (BINU) I1,'RCHRES  ',RCHNO,'ADCALC  ',UNITFG,
     1               LEV,(EXDAT(J),J=1,5),(APRINT(J),J=1,ACNT)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   ACIRST
     I                    (LEV)
C
C     + + + PURPOSE + + +
C     reset state variables and fluxes for module section ACIDPH
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    LEV
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEV    - level of array being reset
C
C     + + + COMMON BLOCKS + + +
      INCLUDE    'crhac.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,I7,I10
      REAL       RZERO
C
C     + + + EXTERNALS + + +
      EXTERNAL   SETVEC
C
C     + + + END SPECIFICATIONS + + +
C
      I7 = 7
      I10= 10
C
      RZERO= 0.0
C
C     inflows
      CALL SETVEC
     I             (I7,RZERO,
     O              ACINFL(1,LEV))
C
C     outflows
      CALL SETVEC
     I             (I7,RZERO,
     O              ACFLX1(1,LEV))
C
C     individual exit outflows
      IF (NEXITS .GT. 1) THEN
        DO 10 I= 1, NUMCHM
          CALL SETVEC
     I                 (NEXITS,RZERO,
     O                  ACFLX2(1,I,LEV))
 10     CONTINUE
      END IF
C
C     storages
      DO 20 I= 1, NUMCHM
        ACSTOR(I,LEV)= ACSTOR(I,1)
 20   CONTINUE
C
C     computed fluxes
      CALL SETVEC
     I            (I7,RZERO,
     O             ACFLX3(1,LEV))
C
C     section cons fluxes
      CALL SETVEC
     I            (I10,RZERO,
     O             ACFLXC(1,LEV))
C
C     section gqual fluxes
      CALL SETVEC
     I            (I7,RZERO,
     M             ACFLXG(1,LEV))
C
      RETURN
      END
C
C
C
      SUBROUTINE   CONV
     I                  (HMIN,HMAX,ERMIN,ERMAX,ERROR,
     M                   H)
C
C
C     + + + PURPOSE + + +
C     Convergence algorithm for acid ph module
C
C     + + + DUMMY ARGUMENTS + + +
      DOUBLE PRECISION HMIN,HMAX,ERMIN,ERMAX,ERROR,H
C
C     + + + ARGUMENT DEFINITIONS + + +
C     HMIN   - ???
C     HMAX   - ???
C     ERMIN  - ???
C     ERMAX  - ???
C     ERROR  - ???
C     H      - ???
C
C     + + + LOCAL VARIABLES + + +
      DOUBLE PRECISION SMIN,SMAX,XMIN,XMAX,TMIN,TMAX
      INTEGER    MIN,MAX
C
C     + + + INTRINSICS + + +
      INTRINSIC  DABS
C
C     + + + END SPECIFICATIONS + + +
C
      MIN = 0
      MAX = 0
      IF (ERROR .LT. 0.0) MIN = 1
      IF (ERROR .GT. 0.0) MAX = 1
      SMIN = (ERMIN - ERROR)/(HMIN - H)
      SMAX = (ERMAX - ERROR)/(HMAX - H)
      XMIN = HMIN - ERMIN/SMIN
      XMAX = HMAX - ERMAX/SMAX
      TMAX = DABS(H - HMIN)
      TMIN = DABS(H - HMAX)
C
      IF (MIN .EQ. 1) THEN
        ERMIN = ERROR
        HMIN  = H
      END IF
C
      IF (MAX .EQ. 1) THEN
        ERMAX = ERROR
        HMAX  = H
      END IF
C
      IF ((XMAX - HMAX)*(XMAX - HMIN) .GE. 0.0) THEN
        XMAX = HMIN
        TMAX = 1.0
        TMIN = 2.0
      ELSE IF ((XMIN - HMAX)*(XMIN - HMIN) .GE. 0.0) THEN
        XMIN = HMAX
        TMIN = 1.0
        TMAX = 2.0
      END IF
C
      H = (TMAX*XMAX + TMIN*XMIN) / (TMAX + TMIN)
C
      RETURN
      END
C
C
C
      DOUBLE PRECISION FUNCTION   NEWTON
     I                                   (M,XMAX,A,B,C,D,E,F,G)
C
C     + + + PURPOSE + + +
C     Performs Newton-Raphson solution for acid ph module
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     M
      DOUBLE PRECISION XMAX,A,B,C,D,E,F,G
C
C     + + + ARGUMENT DEFINITIONS + + +
C     M      - ???
C     XMAX   - ???
C     A      - multiplying factor for the linear transform A*X + B
C     B      - shift value for the linear transform
C     C      - ???
C     D      - ???
C     E      - ???
C     F      - ???
C     G      - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I
      DOUBLE PRECISION SLOPE,X,X0,XTON(6),Y0
C
C     + + + INTRINSICS + + +
      INTRINSIC   DABS
C
C     + + + END SPECIFICATIONS + + +
C
      DO 10 I= 1, 6
        XTON(I) = 0.0
 10   CONTINUE
C
      X= XMAX/2.
C
 20   CONTINUE
        X0      = X
        XTON(1) = X0
C
        DO 30 I= 2, M
          XTON(I) = XTON(I-1)*X0
 30     CONTINUE
C
        Y0    = A + B*XTON(1) + C*XTON(2) + D*XTON(3) + E*XTON(4) +
     $           F*XTON(5) + G*XTON(6)
        SLOPE = B + 2.*C*XTON(1) + 3.*D*XTON(2) + 4.*E*XTON(3) +
     $              5.*F*XTON(4) + 6.*G*XTON(5)
        X     = X0 - Y0/SLOPE
C
      IF (DABS((X - X0)/X0) .GT. .001) GO TO 20
C
      NEWTON  = X
C
      RETURN
      END
