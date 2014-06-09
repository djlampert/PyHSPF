C
C
C
      SUBROUTINE   PPHOS
C
C     + + + PURPOSE + + +
C     Process input for section phos
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PHOS1 + + +
      INCLUDE    'cplph.inc'
      INCLUDE    'crin2.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I0,I00,I1,I2,I3,I4,I2A,I2B,I4A,I4B,N,I,L,RETCOD,K,
     $             SCLU,SGRP
      REAL         IPX(1),R0,SUMFRC,TOLER
      CHARACTER*60 HEADG
C
C     + + + INTRINSICS + + +
      INTRINSIC    ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL     SOLDAT,ITABLE,PLNTPM,FIRSTP,SVALP,STORGE,CYLDPM
      EXTERNAL     ZIPR,MDATBL,OMSTI,OMSTR,OMSG,RTABLE
C
C     + + + DATA INITIALIZATIONS + + +
      DATA TOLER/1.0E-5/
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' PROCESSING INPUT FOR SECTION PHOS')
 2010 FORMAT (/,' FINISHED PROCESSING INPUT FOR SECTION PHOS')
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU= 311
C
      I00= 0
      I0 = 0
      R0 = 0.0
      I1 = 1
      I2 = 2
      I3 = 3
      I4 = 4
C
      IF (OUTLEV.GT.1) THEN
C       processing section message
        WRITE (MESSU,2000)
      END IF
C
C     initialize month-data input
      I= 48
      CALL ZIPR (I,R0,
     O           PHAFXM)
      CALL ZIPR (I,R0,
     O           PHACNM)
C
C     initialize atmospheric deposition fluxes
      I= 20
      CALL ZIPR (I,R0,
     O           PCFX7)
      CALL ZIPR (I,R0,
     O           PCFX6)
C
C     initialize yield-based plant uptake fluxes and targets
      SPDFC= 0.0
      UPDFC= 0.0
      LPDFC= 0.0
      APDFC= 0.0
      TPDFC= 0.0
      I= 4
      CALL ZIPR (I,R0,
     O           PUPTG)
C
C     initialize special action accumulators
      I= 15
      CALL ZIPR (I,R0,
     O           PHOIF)
C
      MSGFL = FILE(15)
C
C     warning and error message offsets and counter initialization
      PWCNT(1)= 0
      PWCNT(2)= 0
      PWCNT(3)= 0
      PWCNT(4)= 0
      PWCNT(5)= 0
      PWCNT(6)= 0
      PECNT(1)= 0
C
C     heading for printing out total storages in surface, upper,
C     lower, and active groundwater layers
      HEADG = '      ORGP      P4AD      P4SU       PLTP'
C
      IF (PESTFG.EQ.0 .AND. NITRFG.EQ.0) THEN
C       read in pesticide warning counts, because some of those
C       warnings are also used by phos
        PSWCNT(1)= 0
        PSWCNT(2)= 0
        PSWCNT(3)= 0
        PSWCNT(4)= 0
        PSWCNT(5)= 0
        PSWCNT(6)= 0
C
C       read in soil data - table type soil-data
        CALL SOLDAT (UUNITS,
     O               SOILM,SOILD)
      END IF
C
C     table-type phos-flags
      I2A= 131
      I4A= 6
      CALL ITABLE (I2A,I1,I4A,UUNITS,
     M             PHOFG)
C
C     get atmospheric deposition flags - table-type phos-ad-flags
      I2A= 132
      I4A= 8
      CALL ITABLE (I2A,I1,I4A,UUNITS,
     M             PHADFG)
C
C     read in month-data tables where necessary
      DO 50 I= 1, 2
        DO 40 L= 1,2
          N= 4*(I-1)+ 2*(L-1)+ 1
          IF (PHADFG(N) .GT. 0) THEN
C           monthly flux must be read
            CALL MDATBL
     I                  (PHADFG(N),
     O                   PHAFXM(1,I,L),RETCOD)
C           convert units to internal - not done by MDATBL
C           from lb/ac.day to lb/ac.ivl or from kg/ha.day to kg/ha.ivl
            DO 10 K= 1, 12
              PHAFXM(K,I,L)= PHAFXM(K,I,L)*DELT60/24.0
 10         CONTINUE
          END IF
          IF (PHADFG(N+1) .GT. 0) THEN
C           monthly ppn conc must be read
            CALL MDATBL
     I                  (PHADFG(N+1),
     O                   PHACNM(1,I,L),RETCOD)
C           convert units to internal - not done by MDATBL
            IF (UUNITS .EQ. 1) THEN
C             convert from mg/l to lb/ac.in
              DO 20 K= 1, 12
                PHACNM(K,I,L)= PHACNM(K,I,L)*0.226635
 20           CONTINUE
            ELSE IF (UUNITS .EQ. 2) THEN
C             convert from mg/l to kg/ha.in
              DO 30 K= 1, 12
                PHACNM(K,I,L)= PHACNM(K,I,L)*0.01
 30           CONTINUE
            END IF
          END IF
 40     CONTINUE
 50   CONTINUE
C
      IF (PUPTFG .EQ. 0) THEN
C       get plant uptake parameters
        I2A= 133
        I2B= 134
        CALL PLNTPM (MESSU,VPUTFG,I2A,UUNITS,DELT60,OUTLEV,I2B,
     O               KPLP,SKPLPM,UKPLPM,LKPLPM,AKPLPM)
      ELSE IF (PUPTFG .EQ. 1) THEN
C       get yield-based plant uptake parameters
C
C       get phosphorus parameters - table-type phos-yield
        I2A= 139
        I4A= 2
        CALL RTABLE (I2A,I1,I4A,UUNITS,
     M               PYLDPM)
C
        IF ( (NITRFG .EQ. 0) .OR. (NUPTFG .NE. 1) ) THEN
C         get crop and soil parameters
          CALL CYLDPM (MESSU,MSGFL,UUNITS,OUTLEV,NDAY,SOILD,
     M                 ECOUNT,
     O                 WILTPT,NCRP,CRPDAT,CRPDAY,CRPFRC)
        END IF
C
C       get monthly total uptake fractions - table-type mon-pupt-fr1
        I2A= 140
        I4A= 12
        CALL RTABLE (I2A,I1,I4A,UUNITS,
     M               PUPTFM)
C
C       check fractions for consistency
        SUMFRC= 0.0
        DO 60 I= 1, 12
          SUMFRC= SUMFRC+ PUPTFM(I)
 60     CONTINUE
        IF (ABS(SUMFRC-1.0) .GT. TOLER) THEN
C         error - fractions must sum to unity
          CALL OMSTI (LSNO)
          CALL OMSTR (SUMFRC)
          SGRP= 4
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
        END IF
C
C       get monthly layer uptake fractions - table-type mon-pupt-fr2
        I2A= 141
        I4A= 12
        CALL RTABLE (I2A,I1,I4A,UUNITS,
     M               SPUPTM)
        CALL RTABLE (I2A,I2,I4A,UUNITS,
     M               UPUPTM)
        CALL RTABLE (I2A,I3,I4A,UUNITS,
     M               LPUPTM)
        CALL RTABLE (I2A,I4,I4A,UUNITS,
     M               APUPTM)
C
C       check each month that fractions sum to unity
        DO 70 I= 1, 12
          SUMFRC= SPUPTM(I)+ UPUPTM(I)+ LPUPTM(I)+ APUPTM(I)
          IF ( (ABS(SUMFRC-1.0) .GT. TOLER) .AND.
     $         (PUPTFM(I) .GT. 0.0) ) THEN
C           error - fractions don't sum to unity when there is uptake
            CALL OMSTI (LSNO)
            CALL OMSTI (I)
            CALL OMSTR (SUMFRC)
            SGRP= 5
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 ECOUNT)
          END IF
 70     CONTINUE
C
      END IF
C
C     get other parameters required to handle first-order reactions
      I2A= 135
      I2B= 136
      I4A= 5
      I4B= 4
      CALL FIRSTP (OUTLEV,UUNITS,DELT60,I2A,I1,I4A,I2B,I0,I4B,
     $             CNUMP,BNUMP,LSNO,MESSU,MSGFL,
     M             PWCNT(6),
     O             GPPM(1),SPPM(1),UPPM(1),LPPM(1),APPM(1))
C
      IF (FORPFG.NE.0) THEN
C       get parameters to simulate phosphate ads/des using
C       single-valued freundlich equation
        I2A= 137
        I2B= 138
        CALL SVALP (OUTLEV,MESSU,UUNITS,I2A,I1,I2B,I0,
     O              GPPM(6),SPPM(5),UPPM(5),LPPM(5),
     $              APPM(5))
      END IF
C
C     state variables
C     counters for indicating when reaction rates are to be recalculated
      BIVLP= BNUMP
      CIVLP= CNUMP
C
C     initial storages
      I2A= 142
      I2B= 143
      I4A= 4
      I4B= 1
      CALL STORGE (MESSU,OUTLEV,UUNITS,I2A,I4A,HEADG,I2B,I4B,
     M             I0,I00,
     O             SP,UP,IPX,LP,AP,TP,TOTPHO)
      IP= IPX(1)
      TPHO(1)= SP(1)+ SP(2)+ SP(3)+ SP(4)
      TPHO(2)= UP(1)+ UP(2)+ UP(3)+ UP(4)
      TPHO(3)= IP
      TPHO(4)= LP(1)+ LP(2)+ LP(3)+ LP(4)
      TPHO(5)= AP(1)+ AP(2)+ AP(3)+ AP(4)
C
      IF (OUTLEV.GT.1) THEN
C       end processing section message
        WRITE (MESSU,2010)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PHOS
C
C     + + + PURPOSE + + +
C     Simulate phosphorus behavior in detail
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PHOS2 + + +
      INCLUDE     'cplph.inc'
      INCLUDE     'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     BRXPFG,CRXPFG,I,J,TMPFG,N,REQFG,TSSUB(2),FLGVAL
      REAL        FSD,MOISTM,PHADFX,PHADCN
      CHARACTER*4 LAYID(4)
      CHARACTER*6 OPTYP,TSNAM,SECNAM,MSECNM,OPFGNM
C
C     + + + FUNCTIONS + + +
      REAL        DAYVAL
C
C     + + + EXTERNALS + + +
      EXTERNAL    DAYVAL,AGRGET,SDFRAC,SEDMOV,TOPMOV,SUBMOV,PHORXN,
     $            YUPTGT,YUPINI,HREQTS
C
C     + + + DATA INITIALIZATIONS + + +
      DATA        LAYID/'SURF','UPPR','LOWR','GRND'/
      DATA        TSSUB/1,1/
      DATA        OPTYP,SECNAM/'PERLND','PHOS  '/
C
C     + + + END SPECIFICATIONS + + +
C
      TMPFG= 0
C     check whether soil temperature is required
      IF (FORPFG.EQ.0) TMPFG= 1
      IF ((PESTFG.EQ.0.AND.NITRFG.EQ.0).OR.TMPFG.NE.0) THEN
C       get time series needed for agrichemical sections
        CALL AGRGET (MESSU,MSGFL,DATIM,LSNO,SEDFG,MSTLFG,PSTFG,TMPFG,
     I               IVL1,SOSDFP,MSTFP,FRACFP,SLTFP,ULTFP,LGTFP,SECNAM,
     O               SOSED,MST,FRAC,SLTMP,ULTMP,LGTMP)
      ELSE
C       the time series for the agri-chemical sections
C       are already available
      END IF
CTHJ      PREC= PAD(PRECFP+IVL1)
      REQFG= 2
      TSNAM= 'PREC  '
      CALL HREQTS (PRECFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,LSNO,
     I             TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,FLGVAL,
     O             PREC)
C
C     get lateral inflows
      DO 5 I= 1, 5
        IF (LIP4SX(I) .GE. 1) THEN
C         lateral inflow of solution phosphate
          LIP4S(I)= PAD(LIP4SX(I)+IVL1)
        ELSE
          LIP4S(I)= 0.0
        END IF
 5    CONTINUE
C
      DO 7 I= 1, 2
        IF (LISDPX(I) .GE. 1) THEN
C         lateral inflow of sediment-associated p
          LISEDP(I)= PAD(LISDPX(I)+IVL1)
        ELSE
C         no lateral inflow
          LISEDP(I)= 0.0
        END IF
 7    CONTINUE
C
C     compute atmospheric deposition influx
      DO 20 J= 1, 2
        DO 10 I= 1, 2
          N= 4*(J-1)+ 2*(I-1)+ 1
C         dry deposition
          IF (PHADFG(N) .LE. -1) THEN
CTHJ            PHADDR(J,I)= PAD(PHAFFP(J,I)+IVL1)
            REQFG= 4
            OPFGNM= 'PHADFG'
            TSNAM= 'PHADFX'
            TSSUB(1)= J
            TSSUB(2)= I
            CALL HREQTS (PHAFFP(J,I),IVL1,REQFG,MESSU,MSGFL,DATIM,
     I                   OPTYP,LSNO,TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,
     I                   PHADFG(N),
     O                   PHADFX)
            PHADDR(J,I)= PHADFX
          ELSE IF (PHADFG(N) .GE. 1) THEN
            PHADDR(J,I)= DAYVAL(PHAFXM(MON,J,I),PHAFXM(NXTMON,J,I),DAY,
     I                          NDAYS)
          ELSE
            PHADDR(J,I)= 0.0
          END IF
C         wet deposition
          IF (PHADFG(N+1) .LE. -1) THEN
CTHJ            PHADWT(J,I)= PREC*PAD(PHACFP(J,I)+IVL1)
            REQFG= 4
            OPFGNM= 'PHADFG'
            TSNAM= 'PHADCN'
            TSSUB(1)= J
            TSSUB(2)= I
            CALL HREQTS (PHACFP(J,I),IVL1,REQFG,MESSU,MSGFL,DATIM,
     I                   OPTYP,LSNO,TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,
     I                   PHADFG(N+1),
     O                   PHADCN)
            PHADWT(J,I)= PREC*PHADCN
          ELSE IF (PHADFG(N+1) .GE. 1) THEN
            PHADWT(J,I)= PREC*DAYVAL(PHACNM(MON,J,I),
     I                               PHACNM(NXTMON,J,I),DAY,NDAYS)
          ELSE
            PHADWT(J,I)= 0.0
          END IF
          PHADEP(J,I)= PHADDR(J,I)+ PHADWT(J,I)
 10     CONTINUE
 20   CONTINUE
      TSSUB(1)= 1
      TSSUB(2)= 1
C
C     re-sum application accumulators in case a special action
C     occurred this interval
      DO 30 J= 1, 5
       PHOIF(3,J)= PHOIF(1,J)+ PHOIF(2,J)
 30   CONTINUE
C
C     determine when reactions should be done
C
      IF (BIVLP.EQ.BNUMP) THEN
C       biochemical reaction fluxes are to be recalculated this
C       interval
C       set biochemical reaction flag on
        BRXPFG= 1
        BIVLP = 1
      ELSE
C       biochemical reaction fluxes are not to be recalculated
C       this interval
        BIVLP = BIVLP+ 1
        BRXPFG= 0
      END IF
C
      IF (CIVLP.EQ.CNUMP) THEN
C       chemical reaction (adsorption/desorption) fluxes are
C       to be calculated this interval
C       set chemical (adsorption/desorption) reaction flag on
        CRXPFG= 1
        CIVLP = 1
      ELSE
C       chemical reaction (adsorption/desorption) fluxes are not
C       to be recalculated this interval
        CIVLP = CIVLP+ 1
        CRXPFG= 0
      END IF
C
      IF (DAYFG.EQ.1) THEN
C       it is the first interval of the day
        IF ( (PUPTFG .EQ. 0) .AND. (VPUTFG .EQ. 1) ) THEN
C         first order plant uptake reaction rate parmameters for
C         phosphorus are allowed to vary throughout the year
C         interpolate for the daily value
C
C         linearly interpolate skplp between two values from the
C         monthly array skplpm(12)
          SKPLP= DAYVAL(SKPLPM(MON),SKPLPM(NXTMON),DAY,NDAYS)
C
C         linearly interpolate ukplp between two values from the
C         monthly array ukplpm(12)
          UKPLP= DAYVAL(UKPLPM(MON),UKPLPM(NXTMON),DAY,NDAYS)
C
C         linearly interpolate lkplp between two values from the
C         monthly array lkplpm(12)
          LKPLP= DAYVAL(LKPLPM(MON),LKPLPM(NXTMON),DAY,NDAYS)
C
C         linearly interpolate akplp between two values from the
C         monthly array akplpm(12)
          AKPLP= DAYVAL(AKPLPM(MON),AKPLPM(NXTMON),DAY,NDAYS)
        ELSE
C         plant uptake reaction rate parameters for phosphorus do not
C         vary throughout the year.  values for skplp, ukplp, lkplp,
C         and akplp have been supplied by the run interpreter
        END IF
C
        IF (PUPTFG .EQ. 1) THEN
C         yield-based plant uptake parameters vary monthly, and
C         daily targets must be calculated from a trapezoidal
C         function
C
          IF (STFG .EQ. 1) THEN
C           get initial values of previous month's final daily uptake target
            CALL YUPINI (DELT60,YR,MON,DAY,NDAY,NCRP,CRPDAT,CRPDAY,
     I                   CRPFRC,PUPTGT,PUPTFM,SPUPTM,UPUPTM,LPUPTM,
     I                   APUPTM,
     O                   SPPUTG,UPPUTG,LPPUTG,APPUTG)
          END IF
          CALL YUPTGT (DELT60,YR,MON,DAY,NDAYS,NCRP,CRPDAT,CRPDAY,
     I                 CRPFRC,PUPTGT,PUPTFM,SPUPTM,UPUPTM,LPUPTM,APUPTM,
     M                 SPPUTG,UPPUTG,LPPUTG,APPUTG,SPDFC,UPDFC,LPDFC,
     M                 APDFC,TPDFC,
     O                 SPUPTG,UPUPTG,LPUPTG,APUPTG)
C
        END IF
      END IF
C
C     update storages for atmospheric deposition
      SP(1)= SP(1)+ LISEDP(1)+ PHADEP(2,1)
      SP(2)= SP(2)+ LISEDP(2)
      SP(3)= SP(3)+ LIP4S(1)+  PHADEP(1,1)
      UP(1)= UP(1)+            PHADEP(2,2)
      UP(3)= UP(3)+ LIP4S(2)+  PHADEP(1,2)
      IP=    IP+    LIP4S(3)
C
      IF (SOSED.GT.0.0) THEN
C       there is sediment/soil being eroded from the land surface
C
C       determine the fraction of phosphorus in the
C       surface layer storage being removed on/with sediment
        CALL SDFRAC (SOSED,SLME,LSNO,DATIM,MESSU,MSGFL,
     M               PSWCNT(1),PSWCNT(2),
     O               FSD)
C
C       transport organic phosphorus with/on sediment
        CALL SEDMOV (FSD,
     M               SP(1),
     O               SEDP(1))
C
C       transport adsorbed phosphate with/on sediment
        CALL SEDMOV (FSD,
     M               SP(2),
     O               SEDP(2))
      ELSE
C       there is no sediment/soil being eroded from the land
C       surface so zero fluxes
        SEDP(1)= 0.0
        SEDP(2)= 0.0
      END IF
C
C     move solution phosphate with water in the topsoil layers
      CALL TOPMOV (FRAC,
     M             SP(3),UP(3),IP,
     O             TSP4S)
C
C     perform reactions on phosphorus in the surface layer storage
      CALL PHORXN (LSNO,MSGFL,DATIM,MESSU,
     I             ITMAXP,GPPM,BRXPFG,CRXPFG,FORPFG,
     I             SLTMP,MST(1),SLSM,SPPM,LAYID(1),SKPLP,
     I             PUPTFG,SPUPTG,PMXRAT,WILTPT(1),SURS,
     M             SPDFC,PWCNT,PECNT,SP,SPRXF)
C
C     perform reactions on phosphorus in the upper layer
C     principal storage
      CALL PHORXN (LSNO,MSGFL,DATIM,MESSU,
     I             ITMAXP,GPPM,BRXPFG,CRXPFG,FORPFG,
     I             ULTMP,MST(2),ULSM,UPPM,LAYID(2),UKPLP,
     I             PUPTFG,UPUPTG,PMXRAT,WILTPT(2),UZS,
     M             UPDFC,PWCNT,PECNT,UP,UPRXF)
C
C     add lateral inflows
      LP(3)= LP(3)+ LIP4S(4)
      AP(3)= AP(3)+ LIP4S(5)
C
C     transport solution phosphate in the subsurface layers
      CALL SUBMOV (TSP4S(3),FRAC(6),FRAC(7),FRAC(8),
     M             LP(3),AP(3),
     O             SSP4S)
C
C     perform reactions on phosphorus in the lower layer storage
      CALL PHORXN (LSNO,MSGFL,DATIM,MESSU,
     I             ITMAXP,GPPM,BRXPFG,CRXPFG,FORPFG,LGTMP,
     I             MST(4),LLSM,LPPM,LAYID(3),LKPLP,
     I             PUPTFG,LPUPTG,PMXRAT,WILTPT(3),LZS,
     M             LPDFC,PWCNT,PECNT,LP,LPRXF)
C
C     perform reactions on phosphorus in the active
C     groundwater storage
C     the calculation of mst(5) in section mstlay results in a
C     non-zero value when frac(8)=1.0.  this causes adsorption
C     calculations by sv to compute a negative po4 storage;
C     moistm is a dummy soil moisture which is set to 0.0 if
C     frac(8)= 1.0.
      MOISTM = MST(5)
      IF (FRAC(8).GE.1.0) MOISTM= 0.0
C
      CALL PHORXN (LSNO,MSGFL,DATIM,MESSU,
     I             ITMAXP,GPPM,BRXPFG,CRXPFG,FORPFG,LGTMP,
     I             MOISTM,ALSM,APPM,LAYID(4),AKPLP,
     I             PUPTFG,APUPTG,PMXRAT,WILTPT(4),AGWS,
     M             APDFC,PWCNT,PECNT,AP,APRXF)
C
C     find total phosphorus outflows due to overland flow erosion
      SOSEDP= SEDP(1)+ SEDP(2)
C
C     find total outflow of phosphorus from the pervious land segment
      POPHOS= TSP4S(1)+ TSP4S(5)+ SSP4S(3)+ SOSEDP
C
C     store fluxes for po4 immobilization, orgp mineralization
      P4IMB(1)= SPRXF(4)
      ORPMN(1)= SPRXF(3)
      P4IMB(2)= UPRXF(4)
      ORPMN(2)= UPRXF(3)
      P4IMB(3)= LPRXF(4)
      ORPMN(3)= LPRXF(3)
      P4IMB(4)= APRXF(4)
      ORPMN(4)= APRXF(3)
      P4IMB(5)= P4IMB(1)+ P4IMB(2)+ P4IMB(3)+ P4IMB(4)
      ORPMN(5)= ORPMN(1)+ ORPMN(2)+ ORPMN(3)+ ORPMN(4)
C
C     find the totals of phosphorus in soil storage
      TP(1)= SP(1)+ UP(1)+ LP(1)+ AP(1)
      TP(2)= SP(2)+ UP(2)+ LP(2)+ AP(2)
      TP(3)= SP(3)+ IP+ UP(3)+ LP(3)+ AP(3)
C
C     find the total phosphorus in plant storage
      TP(4) = SP(4)+ UP(4)+ LP(4)+ AP(4)
C
      IF (PUPTFG .EQ. 1) THEN
C       find the total P uptake deficit
        TPDFC= SPDFC+ UPDFC+ LPDFC+ APDFC
      END IF
C
C     total phosphorus in storage
      TPHO(1)= SP(1)+ SP(2)+ SP(3)+ SP(4)
      TPHO(2)= UP(1)+ UP(2)+ UP(3)+ UP(4)
      TPHO(3)= IP
      TPHO(4)= LP(1)+ LP(2)+ LP(3)+ LP(4)
      TPHO(5)= AP(1)+ AP(2)+ AP(3)+ AP(4)
      TOTPHO=  TP(1)+ TP(2)+ TP(3)+ TP(4)
C
      RETURN
      END
C
C
C
      SUBROUTINE   PHOACC
     I                    (FRMROW,TOROW)
C
C     + + + PURPOSE + + +
C     Accumulate fluxes for section phos
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    FRMROW,TOROW
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FRMROW - row containing incremental flux accumulation
C     TOROW  - flux row to be incremented
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PHOS2 + + +
      INCLUDE    'cplph.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I2,I3,I5,I,J
      REAL       R0
C
C     + + + EXTERNALS + + +
      EXTERNAL   ACCVEC,SETVEC
C
C     + + + END SPECIFICATIONS + + +
C
      I2= 2
      I3= 3
      I5= 5
      R0= 0.0
C
cthj  add next line to fix bug
      PHOIF(3,FRMROW)= PHOIF(1,FRMROW)+ PHOIF(2,FRMROW)
      CALL ACCVEC (I3,PHOIF(1,FRMROW),
     M             PHOIF(1,TOROW))
      IF (TOROW .EQ. 3) THEN
C       reset current value of special-action accumulators
        CALL SETVEC (I3,R0,
     O               PHOIF(1,1))
      END IF
C
      CALL ACCVEC (I5,PHOLIF(1,FRMROW),
     M             PHOLIF(1,TOROW))
C
      CALL ACCVEC (I2,PSDIF(1,FRMROW),
     M             PSDIF(1,TOROW))
C
      CALL ACCVEC (I2,PCFX1(1,FRMROW),
     M             PCFX1(1,TOROW))
C
      CALL ACCVEC (I5,PCFX2(1,FRMROW),
     M             PCFX2(1,TOROW))
C
      CALL ACCVEC (I3,PCFX3(1,FRMROW),
     M             PCFX3(1,TOROW))
C
      CALL ACCVEC (I5,PCFX4(1,FRMROW),
     M             PCFX4(1,TOROW))
C
      CALL ACCVEC (I5,PCFX5(1,FRMROW),
     M             PCFX5(1,TOROW))
C
      DO 20 J= 1, 2
        DO 10 I= 1, 2
          PCFX6(J,I,TOROW)= PCFX6(J,I,TOROW)+ PCFX6(J,I,FRMROW)
          PCFX7(J,I,TOROW)= PCFX7(J,I,TOROW)+ PCFX7(J,I,FRMROW)
 10     CONTINUE
 20   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   PHOPRT
     I                    (LEV,PRINTU,AGMAID,MFACTA,MFACTB,UNITFG,BINU)
C
C     + + + PURPOSE + + +
C     Convert quanities from internal to external units, and
C     produce printout
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     LEV,PRINTU,UNITFG,BINU
      REAL        MFACTA,MFACTB
      CHARACTER*8 AGMAID
C     BINU   - fortran unit number on which to write binary output
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEV    - current output level (2-pivl,3-day,4-mon,5-ann)
C     PRINTU - fortran unit number on which to print output
C     AGMAID - ???
C     MFACTA - ???
C     MFACTB - ???
C     UNITFG - ???
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PHOS2 + + +
      INCLUDE    'cplph.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I0,I1,I2,I3,I4,I5,I,J,N,ADFG,LATFG,ACNT,CLEN(75),
     $            EXDAT(5)
      REAL        MATIN,MATDIF,PCFLX1(2),PCFLX2(5),PCFLX3(3),PIFLX(3),
     $            PCFLX4(5),PCFLX5(5),PDEFCT(5),PPOPHO,PPOPO4,PSOSD,
     $            PSTAT(4),PSTATI,PSTOR,PSTORS,TOTAL,PCFLX6(2,2),
     $            PCFLX7(2,2),PADTOT(2,2),PADALL,PLIFX1(5),PLIFX2(2),
     $            PLIP4S,PLIPSD,PLIPHO,APRINT(75)
      CHARACTER*48  UNITID,CDEFCT(5),CSTAT(4),CCFLX6(2,2),CCFLX7(2,2),
     $              CADTOT(2,2),CIFLX(3),CLIFX1(5),CLIFX2(2),CCFLX2(5),
     $              CCFLX3(3),CCFLX1(2),CCFLX4(5),CCFLX5(5)
      CHARACTER*256 CHEAD(75)
C
C     + + + EXTERNALS + + +
      EXTERNAL   TRNVEC,BALCHK,EXDATE
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' *** PHOS ***')
 2020 FORMAT (/,'   STATE VARIABLES   ',A8,69X,'LAYER DERIVED')
 2030 FORMAT (/,'     STORAGES BY LAYERS',9X,'ORGANIC P',11X,
     $        'PO4-P ADS PO4-P SOL',33X,'PLANT P',14X,'TOTALS')
 2040 FORMAT (/,' ',6X,'SURFACE LAYER',11X,F10.3,10X,2F10.3,30X,F10.3,
     $        10X,F10.3)
 2050 FORMAT (' ',6X,'UPPER PRINCIPAL',9X,F10.3,10X,2F10.3,30X,F10.3,
     $        10X,F10.3)
 2060 FORMAT (' ',6X,'UPPER TRANSITORY(INTER) ',30X,F10.3,50X,F10.3)
 2070 FORMAT (' ',6X,'LOWER LAYER',13X,F10.3,10X,2F10.3,30X,F10.3,
     $        10X,F10.3)
 2080 FORMAT (' ',6X,'ACTIVE GROUNDWATER',6X,F10.3,10X,2F10.3,30X,F10.3,
     $        10X,F10.3)
 2090 FORMAT (/,' ',6X,'TOTALS',18X,F10.3,10X,2F10.3,30X,F10.3,
     $        10X,F10.3)
 2093 FORMAT (/,5X,'  UPTAKE DEFICITS BY LAYER',13X,'SURFACE',10X,
     $          'UPPER PRIN',15X,'LOWER',11X,'ACTIVE GW',15X,'TOTAL')
 2095 FORMAT (  45X,' SPDFC',14X,' UPDFC',14X,' LPDFC',14X,' APDFC',
     $          14X,' TPDFC')
 2098 FORMAT (  31X,5(10X,(1PE10.3)))
 2100 FORMAT (/,'   FLUXES',12X,A8)
 2110 FORMAT (/,'     ATMOSPHERIC DEPOSITION    <-------SURFACE',
     $          ' LAYER--------><------UPPER LAYER PRIN------>')
 2120 FORMAT (  31X,'       DRY       WET     TOTAL       DRY',
     $          '       WET     TOTAL')
 2130 FORMAT (  7X,'PO4-P',19X,6(1PE10.3))
 2140 FORMAT (  7X,'ORG-P',19X,6(1PE10.3))
 2145 FORMAT (/,5X,'APPLICATIONS',14X,' PHOSPHATE ORGANIC P',
     $          ' PHOSPHORUS')
 2147 FORMAT (  31X,'      IPO4      IORP ALL FORMS')
 2148 FORMAT (  31X,3F10.3)
 2241 FORMAT (  '     LATERAL INFLOWS           <------------',
     $          '-------------SOLUTION------------------------->',
     $          '<---------ON SEDIMENT-------->     TOTAL')
 2242 FORMAT (  31X,'   SURFACE     UPPER INTERFLOW     LOWER',
     $          ' ACTIVE GW     TOTAL      ORGP     PO4-P     TOTAL')
 2243 FORMAT (  31X,'    SLIP4S    ULIP4S    ILIP4S    LLIP4S',
     $          '    ALIP4S    TLIP4S    SDIORP    SDIP4A    SDIPHO',
     $          '    TLIPHO')
 2244 FORMAT (  31X,1PE10.3,9E10.3)
 2150 FORMAT (/,'     FLOWS OF P IN SOLUTION    <--SURFACE LAYER--->',
     $        '<-UPPER LAYER PRIN-> INTERFLOW',10X,
     $        '<---LOWER LAYER---->',9X,'GROUNDWATER')
 2160 FORMAT (' ',30X,'   OUTFLOW      PERC      PERC  TO TRANS   ',
     $        'OUTFLOW',16X,'PERC DEEP PERC',13X,'OUTFLOW')
 2170 FORMAT (' ',6X,'PO4-P IN SOLUTION',7X,5(1PE10.3),10X,
     $        2(1PE10.3),10X,(1PE10.3))
 2180 FORMAT (/,'     OTHER OUTFLOWS',14X,
     $        'SEDIMENT ASSOCIATED OUTFLOWS',10X,
     $        '<--TOTAL OUTFLOW--->')
 2190 FORMAT (' ',30X,' ORGANIC P PO4-P ADS     TOTAL',15X,
     $        'PO4-P  PHOSPHOR')
 2200 FORMAT (' ',35X,'SDORP     SDP4A    SOSEDP',15X,
     $        'POPO4    POPHOS')
 2210 FORMAT (' ',30X,3(1PE10.3),10X,2(1PE10.3))
 2220 FORMAT (/,'  PO4 IMMOBILIZATION BY LAYER',15X,'SURFACE',10X,
     $        'UPPER PRIN',15X,'LOWER',11X,'ACTIVE GW',15X,'TOTAL')
 2230 FORMAT (' ',44X,'SP4IMB',14X,'UP4IMB',14X,'LP4IMB',14X,'AP4IMB',
     $        14X,'TP4IMB')
 2240 FORMAT (' ',30X,5(10X,(1PE10.3)))
 2250 FORMAT (/,' ','ORGP MINERALIZATION BY LAYER',15X,'SURFACE',10X,
     $        'UPPER PRIN',15X,'LOWER',11X,'ACTIVE GW',15X,'TOTAL')
 2260 FORMAT (' ',44X,'SORPMN',14X,'UORPMN',14X,'LORPMN',14X,'AORPMN',
     $        14X,'TORPMN')
C
C     + + + HISTORY + + +
C     03/27/2007  jlk fixed applications so that data is always 
C                 written if header has been written
C
C     + + + END SPECIFICATIONS + + +
C
      I0= 0
      I1= 1
      I2= 2
      I3= 3
      I4= 4
      I5= 5
C
C     initialize array counter for binary printout, store variable
C     names in local strings for use in building binary headers
      ACNT = 0
      CSTAT(1)    = 'ORGANIC P'
      CSTAT(2)    = 'PO4-P ADS'
      CSTAT(3)    = 'PO4-P SOL'
      CSTAT(4)    = 'PLANT P'
      CDEFCT(1)   = 'SPDFC'
      CDEFCT(2)   = 'UPDFC'
      CDEFCT(3)   = 'LPDFC'
      CDEFCT(4)   = 'APDFC'
      CDEFCT(5)   = 'TPDFC'
      CCFLX6(1,1) = 'PO4-P - SURFACELAYER - DRY'
      CCFLX6(1,2) = 'PO4-P - UPPER LAYER - DRY'
      CCFLX6(2,1) = 'ORG-P - SURFACELAYER - DRY'
      CCFLX6(2,2) = 'ORG-P - UPPER LAYER - DRY'
      CCFLX7(1,1) = 'PO4-P - SURFACELAYER - WET'
      CCFLX7(1,2) = 'PO4-P - UPPER LAYER - WET'
      CCFLX7(2,1) = 'ORG-P - SURFACELAYER - WET'
      CCFLX7(2,2) = 'ORG-P - UPPER LAYER - WET'
      CADTOT(1,1) = 'PO4-P - SURFACELAYER - TOTAL'
      CADTOT(1,2) = 'PO4-P - UPPER LAYER - TOTAL'
      CADTOT(2,1) = 'ORG-P - SURFACELAYER -TOTAL'
      CADTOT(2,2) = 'ORG-P - UPPER LAYER - TOTAL'
      CIFLX(1) = 'IPO4'
      CIFLX(2) = 'IORP'
      CIFLX(3) = 'PHOSPHORUS - ALL FORMS'
      CLIFX1(1) = 'SLIP4S'
      CLIFX1(2) = 'ULIP4S'
      CLIFX1(3) = 'ILIP4S'
      CLIFX1(4) = 'LLIP4S'
      CLIFX1(5) = 'ALIP4S'
      CLIFX2(1) = 'SDIORP'
      CLIFX2(2) = 'SDIP4A'
      CCFLX2(1) = 'PO4-P IN SOLUTION - SURFACE LAYER - OUTFLOW'
      CCFLX2(2) = 'PO4-P IN SOLUTION - SURFACE LAYER - PERC'
      CCFLX2(3) = 'PO4-P IN SOLUTION - UPPER LAYER - PERC'
      CCFLX2(4) = 'PO4-P IN SOLUTION - UPPER LAYER - TO TRANS'
      CCFLX2(5) = 'PO4-P IN SOLUTION - INTERFLOW - OUTFLOW'
      CCFLX3(1) = 'PO4-P IN SOLUTION - LOWER LAYER - PERC'
      CCFLX3(2) = 'PO4-P IN SOLUTION - LOWER LAYER - DEEP PERC'
      CCFLX3(3) = 'PO4-P IN SOLUTION - GROUNDWATER - OUTFLOW'
      CCFLX1(1) = 'SDORP'
      CCFLX1(2) = 'SDP4A'
      CCFLX4(1) = 'SP4IMB'
      CCFLX4(2) = 'UP4IMB'
      CCFLX4(3) = 'LP4IMB'
      CCFLX4(4) = 'AP4IMB'
      CCFLX4(5) = 'TP4IMB'
      CCFLX5(1) = 'SORPMN'
      CCFLX5(2) = 'UORPMN'
      CCFLX5(3) = 'LORPMN'
      CCFLX5(4) = 'AORPMN'
      CCFLX5(5) = 'TORPMN'
C
      IF (PRINTU .GT. 0 .AND. PFLAG(11) .LE. LEV) THEN
        WRITE (PRINTU,2000)
C
C       print headings on unit printu
        WRITE (PRINTU,2020)  AGMAID
        WRITE (PRINTU,2030)
      END IF
C
      CALL TRNVEC (I4,SP,MFACTA,MFACTB,
     O             PSTAT)
      TOTAL= PSTAT(1)+ PSTAT(2)+ PSTAT(3)+ PSTAT(4)
      IF (PRINTU .GT. 0 .AND. PFLAG(11) .LE. LEV) THEN
        WRITE (PRINTU,2040)  PSTAT, TOTAL
      END IF
      IF (BINU .GT. 0 .AND. ABS(BFLAG(11)) .LE. LEV) THEN
C       compile values for binary printout
        DO 10 I = 1, 4
          ACNT = ACNT + 1
          APRINT(ACNT) = PSTAT(I)
          CHEAD(ACNT) = TRIM(CSTAT(I)) // ' - SURFACE LAYER'
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 10     CONTINUE
        ACNT = ACNT + 1
        APRINT(ACNT) = TOTAL
        CHEAD(ACNT) = 'TOTAL - SURFACE LAYER'
        CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
      END IF
C
      CALL TRNVEC (I4,UP,MFACTA,MFACTB,
     O             PSTAT)
      TOTAL= PSTAT(1)+ PSTAT(2)+ PSTAT(3)+ PSTAT(4)
      IF (PRINTU .GT. 0 .AND. PFLAG(11) .LE. LEV) THEN
        WRITE (PRINTU,2050)  PSTAT, TOTAL
      END IF
      IF (BINU .GT. 0 .AND. ABS(BFLAG(11)) .LE. LEV) THEN
C       compile values for binary printout
        DO 20 I = 1, 4
          ACNT = ACNT + 1
          APRINT(ACNT) = PSTAT(I)
          CHEAD(ACNT) = TRIM(CSTAT(I)) // ' - UPPER PRINCIPAL'
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 20     CONTINUE
        ACNT = ACNT + 1
        APRINT(ACNT) = TOTAL
        CHEAD(ACNT) = 'TOTAL - UPPER PRINCIPAL'
        CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
      END IF
C
      PSTATI= IP*MFACTA+ MFACTB
      IF (PRINTU .GT. 0 .AND. PFLAG(11) .LE. LEV) THEN
        WRITE (PRINTU,2060)  PSTATI, PSTATI
      END IF
      IF (BINU .GT. 0 .AND. ABS(BFLAG(11)) .LE. LEV) THEN
C       compile values for binary printout
        ACNT = ACNT + 1
        APRINT(ACNT) = PSTATI
        CHEAD(ACNT) = TRIM(CSTAT(3)) // ' - UPPER TRANSITORY'
        CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
        ACNT = ACNT + 1
        APRINT(ACNT) = PSTATI
        CHEAD(ACNT) = 'TOTAL - UPPER TRANSITORY'
        CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
      END IF
C
      CALL TRNVEC (I4,LP,MFACTA,MFACTB,
     O             PSTAT)
      TOTAL= PSTAT(1)+ PSTAT(2)+ PSTAT(3)+ PSTAT(4)
      IF (PRINTU .GT. 0 .AND. PFLAG(11) .LE. LEV) THEN
        WRITE (PRINTU,2070)  PSTAT, TOTAL
      END IF
      IF (BINU .GT. 0 .AND. ABS(BFLAG(11)) .LE. LEV) THEN
C       compile values for binary printout
        DO 30 I = 1, 4
          ACNT = ACNT + 1
          APRINT(ACNT) = PSTAT(I)
          CHEAD(ACNT) = TRIM(CSTAT(I)) // ' - LOWER LAYER'
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 30     CONTINUE
        ACNT = ACNT + 1
        APRINT(ACNT) = TOTAL
        CHEAD(ACNT) = 'TOTAL - LOWER LAYER'
        CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
      END IF
C
      CALL TRNVEC (I4,AP,MFACTA,MFACTB,
     O             PSTAT)
      TOTAL= PSTAT(1)+ PSTAT(2)+ PSTAT(3)+ PSTAT(4)
      IF (PRINTU .GT. 0 .AND. PFLAG(11) .LE. LEV) THEN
        WRITE (PRINTU,2080)  PSTAT, TOTAL
      END IF
      IF (BINU .GT. 0 .AND. ABS(BFLAG(11)) .LE. LEV) THEN
C       compile values for binary printout
        DO 40 I = 1, 4
          ACNT = ACNT + 1
          APRINT(ACNT) = PSTAT(I)
          CHEAD(ACNT) = TRIM(CSTAT(I)) // ' - ACTIVE GROUNDWATER'
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 40     CONTINUE
        ACNT = ACNT + 1
        APRINT(ACNT) = TOTAL
        CHEAD(ACNT) = 'TOTAL - ACTIVE GROUNDWATER'
        CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
      END IF
C
      CALL TRNVEC (I4,TP,MFACTA,MFACTB,
     O             PSTAT)
      TOTAL= PSTAT(1)+ PSTAT(2)+ PSTAT(3)+ PSTAT(4)
      IF (PRINTU .GT. 0 .AND. PFLAG(11) .LE. LEV) THEN
        WRITE (PRINTU,2090)  PSTAT, TOTAL
      END IF
      IF (BINU .GT. 0 .AND. ABS(BFLAG(11)) .LE. LEV) THEN
C       compile values for binary printout
        DO 50 I = 1, 4
          ACNT = ACNT + 1
          APRINT(ACNT) = PSTAT(I)
          CHEAD(ACNT) = TRIM(CSTAT(I)) // ' - TOTALS'
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 50     CONTINUE
        ACNT = ACNT + 1
        APRINT(ACNT) = TOTAL
        CHEAD(ACNT) = 'TOTAL - TOTALS'
        CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
      END IF
C
      IF (PUPTFG .EQ. 1) THEN
C       print out deficits from yield-based plant uptake algorithm
        IF (PRINTU .GT. 0 .AND. PFLAG(11) .LE. LEV) THEN
          WRITE (PRINTU,2093)
          WRITE (PRINTU,2095)
        END IF
        CALL TRNVEC (I5,PDFCT,MFACTA,MFACTB,
     O               PDEFCT)
        IF (PRINTU .GT. 0 .AND. PFLAG(11) .LE. LEV) THEN
          WRITE (PRINTU,2098)  PDEFCT
        END IF
        IF (BINU .GT. 0 .AND. ABS(BFLAG(11)) .LE. LEV) THEN
C         compile values for binary printout
          DO 60 I = 1, 5
            ACNT = ACNT + 1
            APRINT(ACNT) = PDEFCT(I)
            CHEAD(ACNT) = CDEFCT(I)
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 60       CONTINUE
        END IF
      END IF
C
      IF (PRINTU .GT. 0 .AND. PFLAG(11) .LE. LEV) THEN
        WRITE (PRINTU,2100)  AGMAID
      END IF
C
      LATFG= 0
      ADFG= 0
      DO 70 N= 1, 5
        IF (LIP4SX(N) .GE. 1) THEN
          LATFG= 1
        END IF
 70   CONTINUE
      DO 80 N= 1, 2
        IF (LISDPX(N) .GE. 1) THEN
          LATFG= 1
        END IF
 80   CONTINUE
C
      DO 100 J= 1, 2
        DO 90 I= 1, 2
          N= 4*(J-1)+ 2*(I-1)+ 1
          IF ( (PHADFG(N) .NE. 0) .OR. (PHADFG(N+1) .NE. 0) ) THEN
            ADFG= 1
          END IF
 90     CONTINUE
 100  CONTINUE
C
      PADALL= 0.0
      IF (ADFG .EQ. 1) THEN
        DO 120 J= 1, 2
          DO 110 I= 1, 2
            N= 4*(J-1)+ 2*(I-1)+ 1
            IF ( (PHADFG(N) .NE. 0) .OR. (PHADFG(N+1) .NE. 0) ) THEN
              IF (PHADFG(N) .NE. 0) THEN
                PCFLX6(I,J)= PCFX6(I,J,LEV)*MFACTA
              ELSE
                PCFLX6(I,J)= 0.0
              END IF
              IF (PHADFG(N+1) .NE. 0) THEN
                PCFLX7(I,J)= PCFX7(I,J,LEV)*MFACTA
              ELSE
                PCFLX7(I,J)= 0.0
              END IF
              PADTOT(I,J)= PCFLX6(I,J)+ PCFLX7(I,J)
              PADALL= PADALL+ PADTOT(I,J)
            END IF
 110      CONTINUE
 120    CONTINUE
C
        IF (PRINTU .GT. 0 .AND. PFLAG(11) .LE. LEV) THEN
          WRITE (PRINTU,2110)
          WRITE (PRINTU,2120)
          WRITE (PRINTU,2130) PCFLX6(1,1),PCFLX7(1,1),PADTOT(1,1),
     #                        PCFLX6(1,2),PCFLX7(1,2),PADTOT(1,2)
          WRITE (PRINTU,2140) PCFLX6(2,1),PCFLX7(2,1),PADTOT(2,1),
     #                        PCFLX6(2,2),PCFLX7(2,2),PADTOT(2,2)
        END IF
        IF (BINU .GT. 0 .AND. ABS(BFLAG(11)) .LE. LEV) THEN
          DO 140 I = 1, 2
            DO 130 J = 1, 2
              ACNT = ACNT + 1
              APRINT(ACNT) = PCFLX6(I,J)
              CHEAD(ACNT) = CCFLX6(I,J)
              CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
              ACNT = ACNT + 1
              APRINT(ACNT) = PCFLX7(I,J)
              CHEAD(ACNT) = CCFLX7(I,J)
              CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
              ACNT = ACNT + 1
              APRINT(ACNT) = PADTOT(I,J)
              CHEAD(ACNT) = CADTOT(I,J)
              CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 130        CONTINUE
 140      CONTINUE
        END IF
C
      END IF
C
      CALL TRNVEC (I3,PHOIF(1,LEV),MFACTA,MFACTB,
     O             PIFLX)
      IF (PIFLX(3) .GT. 0.0) THEN
C       print application amount
        IF (PRINTU .GT. 0 .AND. PFLAG(11) .LE. LEV) THEN
          WRITE (PRINTU,2145)
          WRITE (PRINTU,2147)
          WRITE (PRINTU,2148) PIFLX
        END IF
      END IF
C     need to always write out binary form data (if requested)
C     to match headers
      IF (BINU .GT. 0 .AND. ABS(BFLAG(11)) .LE. LEV) THEN
        DO 150 I = 1, 3
          ACNT = ACNT + 1
          APRINT(ACNT) = PIFLX(I)
          CHEAD(ACNT) = CIFLX(I)
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 150    CONTINUE
      END IF
C
      PLIPHO= 0.0
      IF (LATFG .EQ. 1) THEN
C       lateral inflows to print
        PLIP4S= 0.0
        PLIPSD= 0.0
        DO 160 N= 1, 5
          PLIFX1(N)= PHOLIF(N,LEV)*MFACTA
          PLIP4S= PLIP4S+ PLIFX1(N)
          PLIPHO= PLIPHO+ PLIFX1(N)
 160    CONTINUE
        DO 170 N= 1, 2
          PLIFX2(N)= PSDIF(N,LEV)*MFACTA
          PLIPSD= PLIPSD+ PLIFX2(N)
          PLIPHO= PLIPHO+ PLIFX2(N)
 170    CONTINUE
C
        IF (PRINTU .GT. 0 .AND. PFLAG(11) .LE. LEV) THEN
          WRITE (PRINTU,2241)
          WRITE (PRINTU,2242)
          WRITE (PRINTU,2243)
          WRITE (PRINTU,2244) PLIFX1,PLIP4S,PLIFX2,PLIPSD,PLIPHO
        END IF
        IF (BINU .GT. 0 .AND. ABS(BFLAG(11)) .LE. LEV) THEN
          DO 180 I = 1, 5
            ACNT = ACNT + 1
            APRINT(ACNT) = PLIFX1(I)
            CHEAD(ACNT) = CLIFX1(I)
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 180      CONTINUE
          ACNT = ACNT + 1
          APRINT(ACNT) = PLIP4S
          CHEAD(ACNT) = 'TLIP4S'
          CLEN(ACNT) = 6
          DO 190 I = 1, 2
            ACNT = ACNT + 1
            APRINT(ACNT) = PLIFX2(I)
            CHEAD(ACNT) = CLIFX2(I)
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 190      CONTINUE
          ACNT = ACNT + 1
          APRINT(ACNT) = PLIPSD
          CHEAD(ACNT) = 'SDIPHO'
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          ACNT = ACNT + 1
          APRINT(ACNT) = PLIPHO
          CHEAD(ACNT) = 'TLIPHO'
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
        END IF
      END IF
C
      IF (PRINTU .GT. 0 .AND. PFLAG(11) .LE. LEV) THEN
        WRITE (PRINTU,2150)
        WRITE (PRINTU,2160)
      END IF
      CALL TRNVEC (I5,PCFX2(1,LEV),MFACTA,MFACTB,
     O             PCFLX2)
C
      CALL TRNVEC (I3,PCFX3(1,LEV),MFACTA,MFACTB,
     O             PCFLX3)
C
      IF (PRINTU .GT. 0 .AND. PFLAG(11) .LE. LEV) THEN
        WRITE (PRINTU,2170)  PCFLX2, PCFLX3
      END IF
      IF (BINU .GT. 0 .AND. ABS(BFLAG(11)) .LE. LEV) THEN
        DO 200 I = 1, 5
          ACNT = ACNT + 1
          APRINT(ACNT) = PCFLX2(I)
          CHEAD(ACNT) = CCFLX2(I)
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 200    CONTINUE
        DO 210 I = 1, 3
          ACNT = ACNT + 1
          APRINT(ACNT) = PCFLX3(I)
          CHEAD(ACNT) = CCFLX3(I)
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 210    CONTINUE
      END IF
C
      IF (PRINTU .GT. 0 .AND. PFLAG(11) .LE. LEV) THEN
        WRITE (PRINTU,2180)
        WRITE (PRINTU,2190)
        WRITE (PRINTU,2200)
      END IF
C
      CALL TRNVEC (I2,PCFX1(1,LEV),MFACTA,MFACTB,
     O             PCFLX1)
C
      PSOSD = PCFLX1(1)+ PCFLX1(2)
      PPOPO4= PCFLX1(2)+ PCFLX2(1)+ PCFLX2(5)+ PCFLX3(3)
      PPOPHO= PPOPO4+ PCFLX1(1)
C
      IF (PRINTU .GT. 0 .AND. PFLAG(11) .LE. LEV) THEN
        WRITE (PRINTU,2210)  PCFLX1, PSOSD, PPOPO4, PPOPHO
        WRITE (PRINTU,2220)
        WRITE (PRINTU,2230)
      END IF
      IF (BINU .GT. 0 .AND. ABS(BFLAG(11)) .LE. LEV) THEN
        DO 220 I = 1, 2
          ACNT = ACNT + 1
          APRINT(ACNT) = PCFLX1(I)
          CHEAD(ACNT) = CCFLX1(I)
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 220    CONTINUE
        ACNT = ACNT + 1
        APRINT(ACNT) = PSOSD
        CHEAD(ACNT) = 'SOSEDP'
        CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
        ACNT = ACNT + 1
        APRINT(ACNT) = PPOPO4
        CHEAD(ACNT) = 'POPO4'
        CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
        ACNT = ACNT + 1
        APRINT(ACNT) = PPOPHO
        CHEAD(ACNT) = 'POPHOS'
        CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
      END IF
C
      CALL TRNVEC (I5,PCFX4(1,LEV),MFACTA,MFACTB,
     O             PCFLX4)
C
      IF (PRINTU .GT. 0 .AND. PFLAG(11) .LE. LEV) THEN
        WRITE (PRINTU,2240) PCFLX4
        WRITE (PRINTU,2250)
        WRITE (PRINTU,2260)
      END IF
      IF (BINU .GT. 0 .AND. ABS(BFLAG(11)) .LE. LEV) THEN
        DO 230 I = 1, 5
          ACNT = ACNT + 1
          APRINT(ACNT) = PCFLX4(I)
          CHEAD(ACNT) = CCFLX4(I)
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 230    CONTINUE
      END IF
C
      CALL TRNVEC (I5,PCFX5(1,LEV),MFACTA,MFACTB,
     O             PCFLX5)
C
      IF (PRINTU .GT. 0 .AND. PFLAG(11) .LE. LEV) THEN
        WRITE (PRINTU,2240) PCFLX5
      END IF
      IF (BINU .GT. 0 .AND. ABS(BFLAG(11)) .LE. LEV) THEN
        DO 240 I = 1, 5
          ACNT = ACNT + 1
          APRINT(ACNT) = PCFLX5(I)
          CHEAD(ACNT) = CCFLX5(I)
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 240    CONTINUE
      END IF
C
C
C     phosphorus balance check and report
      IF (UNITFG .EQ. 1) THEN
C       english
        UNITID= '   LB/AC'
      ELSE
C       metric
        UNITID= '   KG/HA'
      END IF
C
C     convert storages to external units for balance
      PSTORS= TOTP(LEV)*MFACTA+ MFACTB
      PSTOR = TOTP(1)*MFACTA+ MFACTB
C
C     find the net output of phosphorus from the pls
C     application plus deposition minus outflow minus deep gw loss
      MATIN= PADALL+ PIFLX(3)+ PLIPHO
      MATDIF= MATIN- (PPOPHO+ PCFLX3(2))
C
      CALL BALCHK (I1,LSNO,DATIM,MESSU,PRINTU,MSGFL,
     I             PSTORS,PSTOR,MATIN,MATDIF,UNITID,I1,
     M             PWCNT(1))
C
      IF (BINU .GT. 0 .AND. ABS(BFLAG(11)) .LE. LEV) THEN
C       write binary output
        CALL EXDATE(
     I              DATIM,
     O              EXDAT)
        IF (BFLAG(11) .GT. 0) THEN
C         at start of run, write the header
          WRITE (BINU) I0,'PERLND  ',LSNO,'PHOS    ',
     1          (CLEN(I),(CHEAD(I)(J:J),J=1,CLEN(I)),I=1,ACNT)
C         set bflag to negative to not write headers anymore
          BFLAG(11) = -BFLAG(11)
        END IF
        WRITE (BINU) I1,'PERLND  ', LSNO,'PHOS    ',UNITFG,
     1               LEV,(EXDAT(I),I=1,5),(APRINT(I),I=1,ACNT)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PHORST
     I                    (LEV)
C
C     + + + PURPOSE + + +
C     Reset all flux accumulators and those state variables
C     used in material balance check for section phos
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    LEV
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEV    - current output level (2-pivl,3-day,4-mon,5-ann)
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PHOS2 + + +
      INCLUDE    'cplph.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I2,I3,I5,I,J
C
C     + + + EXTERNALS + + +
      EXTERNAL  SETVEC
C
C     + + + END SPECIFICATIONS + + +
C
      I2=2
      I3=3
      I5=5
C     set flux accumulators to zero
C
      CALL SETVEC (I3,0.0,
     O             PHOIF(1,LEV))
C
      CALL SETVEC (I5,0.0,
     O             PHOLIF(1,LEV))
C
      CALL SETVEC (I2,0.0,
     O             PSDIF(1,LEV))
C
      CALL SETVEC (I2,0.0,
     O             PCFX1(1,LEV))
C
      CALL SETVEC (I5,0.0,
     O             PCFX2(1,LEV))
C
      CALL SETVEC (I3,0.0,
     O             PCFX3(1,LEV))
C
      CALL SETVEC (I5,0.0,
     O             PCFX4(1,LEV))
C
      CALL SETVEC (I5,0.0,
     O             PCFX5(1,LEV))
C
      DO 20 J= 1, 2
        DO 10 I= 1, 2
          PCFX6(J,I,LEV)= 0.0
          PCFX7(J,I,LEV)= 0.0
 10     CONTINUE
 20   CONTINUE
C
C     keep storage in state variable used for
C     material balance check
C
      TOTP(LEV)= TOTP(1)
C
      RETURN
      END
C
C
C
      SUBROUTINE   PHORXN
     I                    (LSNO,MSGFL,DATIM,MESSU,
     I                     ITMAXP,GPPM,BRXPFG,CRXPFG,FORPFG,
     I                     TMP,MOISTM,SOILM,PPM,LAYID,KPLP,
     I                     PUPTFG,PUPTG,PMXRAT,WILTPT,SMST,
     M                     PDEFC,PWCNT,PECNT,PHO,PHORXF)
C
C     + + + PURPOSE + + +
C     Perform reactions on phosphorus forms
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     BRXPFG,CRXPFG,MSGFL,DATIM(5),FORPFG,ITMAXP,
     $            LSNO,MESSU,PECNT(1),PWCNT(6),PUPTFG
      REAL        GPPM(6),KPLP,MOISTM,PHO(4),PHORXF(5),
     $            PPM(8),SOILM,TMP,PUPTG,PMXRAT,WILTPT,SMST,PDEFC
      CHARACTER*4 LAYID
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LSNO   - line number in the opn sequence block of uci
C     MSGFL  - fortran unit number of HSPF message file
C     DATIM  - date and time of day
C     MESSU  - ftn unit no. to be used for printout of messages
C     ITMAXP - maximum number of iterations allowed for convergence of
C              Freundlich method for phosphate adsorption/desorption
C     GPPM   - general phosphorus parameters
C     BRXPFG - flag indicating whether biological reaction fluxes are
C              recalculated this interval
C     CRXPFG - flag indicating whether chemical reaction fluxes are
C              recalculated this interval (adsorption/desorption)
C     FORPFG - flag indicating which method is used to calculate adsorption/
C              desorption - 1: first-order rates; 2: single-valued Freundlich
C     TMP    - soil temperature in this layer
C     MOISTM - soil moisture in this layer (lb or kg)
C     SOILM  - soil mass of this layer (lb or kg)
C     PPM    - phosphorus parameters for this layer
C     LAYID  - character identifier for this layer
C     KPLP   - first-order plant-uptake parameter for this layer
C     PUPTFG - flag indicating which method is used to calculate plant uptake
C              0: first-order rate; 1: yield-based algorithm
C     PUPTG  - plant uptake target for this layer (PUPTFG= 1)
C     PMXRAT - ratio of maximum plant uptake to target uptake (PUPTFG= 1)
C     WILTPT - wilting point: soil moisture cutoff for plant uptake for this
C              layer (PUPTFG= 1)
C     SMST   - soil moisture storage in inches
C     PDEFC  - cumulative plant uptake deficit for this layer (PUPTFG= 1)
C     PWCNT  - warning counts
C     PECNT  - error count
C     PHO    - storages of each species of phosphorus in this layer
C     PHORXF - current reaction fluxes for this layer
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     SCLU,SGRP,I4
      REAL        ADSP4,CMAXP4,DESP4,DIF35,FRAC,IMMP4,KADP,KDSP,KF1P,
     $            KIMP,KIMPK,KMP,KMPK,KPLPK,MINZOP,N1IP,ORGP,PLTP,
     $            P4AD,P4CY,P4SU,TFRAC,THKADP,THKDSP,THKIMP,
     $            THKMP,THPLP,TORGP,TPO4,TP4AD,TP4SU,UTP4,XFIXP4,
     $            XMAXP4,UTP4TG,MAXUPT
      CHARACTER*4 PO4ID(5),CHSTR
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (CHSTR,CHSTR1)
      CHARACTER*1  CHSTR1(4)
C
C     + + + EXTERNALS + + +
      EXTERNAL    FIRORD,SV,OMSG,OMSTI,OMSTD,OMSTC,OMSTR
C
C     + + + DATA INITIALIZATIONS + + +
      DATA       PO4ID/'PHOS','PHAT','E   ','    ','    '/
C
C     + + + END SPECIFICATIONS + + +
C
      I4   = 4
      SCLU = 311
C     assign values to local variables where necessary
C     general parameters
      THPLP = GPPM(1)
      THKDSP= GPPM(2)
      THKADP= GPPM(3)
      THKIMP= GPPM(4)
      THKMP = GPPM(5)
      CMAXP4= GPPM(6)
C
C     layer specific parameters
      KDSP  = PPM(1)
      KADP  = PPM(2)
      KIMP  = PPM(3)
      KMP   = PPM(4)
      XFIXP4= PPM(5)
      XMAXP4= PPM(6)
      KF1P  = PPM(7)
      N1IP  = PPM(8)
C
C     layer specific storages of phosphorus
      ORGP= PHO(1)
      P4AD= PHO(2)
      P4SU= PHO(3)
      PLTP= PHO(4)
C
C     layer specific reaction fluxes
      ADSP4 = PHORXF(1)
      DESP4 = PHORXF(2)
      MINZOP= PHORXF(3)
      IMMP4 = PHORXF(4)
      UTP4  = PHORXF(5)
C
      IF (CRXPFG .EQ. 1) THEN
C       chemical (adsorption/desorption) fluxes are
C       recalculated this interval
        IF (FORPFG .NE. 1) THEN
C         phosphate is adsorbed/desorbed by first order kinetics
C         with this method the adsorption/desorption fluxes are
C         calculated every cnump intervals in units of the basic
C         simulation interval (mass/area-ivl); the updating of the
C         storages is done every interval
C
          CALL FIRORD (TMP,MOISTM,KDSP,KADP,THKDSP,THKADP,
     $                 P4SU,P4AD,
     O                 ADSP4,DESP4)
        ELSE
C         phosphate is adsorbed/desorbed using the single value
C         freundlich method
C         with this method the adsorption/desorption is instantaneous
C         and is done every cnump intervals.  because this method is
C         instantaneous, no updating of the storages is done during
C         intermediate intervals
C
C         total phosphate
          TPO4= P4AD+ P4SU
C
          CALL SV (MOISTM,SOILM,TPO4,XFIXP4,CMAXP4,XMAXP4,KF1P,
     I             N1IP,LSNO,MESSU,MSGFL,DATIM,
     I             ITMAXP,PO4ID,LAYID,
     M             P4SU,PECNT(1),
     O             P4CY,P4AD)
C
C         zero fluxes since this method is based on
C         instantaneous equilibrium
          ADSP4= 0.0
          DESP4= 0.0
C
C         any crystalline phosphate formed is considered adsorbed
          P4AD= P4AD+ P4CY
C
        END IF
C
      END IF
C
      IF (BRXPFG .EQ. 1) THEN
C       biochemical transformation fluxes are recalculated
C       this interval
C
        IF ( (TMP .GT.  4.0) .AND. (MOISTM .GT. 100.0) ) THEN
C         there is sufficient soil layer temperature (in deg c)
C         and moisture for biochemical transformations to occur
C
          IF (TMP .LT. 35.0) THEN
C           soil layer temperature in deg c is less than
C           optimum, modify inputted first order reaction rates
C           decrease the inputted first order reaction rates
C           by the modified arrenhius equation
            DIF35= TMP- 35.0
            KIMPK= KIMP*THKIMP**DIF35
            KMPK = KMP*THKMP**DIF35
            IF (PUPTFG .EQ. 0) THEN
C             first order plant uptake
              KPLPK= KPLP*THPLP**DIF35
            END IF
          ELSE
C           soil layer temperature in deg c is at optimum,
C           use inputted first order reaction rates
            KIMPK= KIMP
            KMPK = KMP
            IF (PUPTFG .EQ. 0) THEN
C             first order plant uptake
              KPLPK= KPLP
            END IF
          END IF
C
C         recompute transformation fluxes.  this is done every
C         bnump intervals in units of the basic simulation
C         interval (mass/area-ivl); however, the updating
C         of the storages is done every interval
C
C         organic phosphorus mineralization
          MINZOP= ORGP*KMPK
C
C         phosphorus immobilization
          IMMP4 = P4SU*KIMPK
C
          IF (PUPTFG .EQ. 0) THEN
C           plant uptake is first-order
C
C           plant uptake of phosphate
            UTP4  = P4SU*KPLPK
C
          ELSE IF (PUPTFG .EQ. 1) THEN
C           plant uptake is yield-based
C
            IF (SMST .GE. WILTPT) THEN
C             soil moisture is at or above wilting point
C
C             try to take up optimum target plus seasonal deficit
              UTP4TG= PUPTG+ PDEFC
C
C             make sure maximum rate is not exceeded
              MAXUPT= PUPTG*PMXRAT
              IF (UTP4TG .GT. MAXUPT) THEN
C               reduce to maximum rate
                UTP4TG= MAXUPT
              END IF
            ELSE
C             soil moisture is below wilting point
              UTP4TG= 0.0
            END IF
C           attempt to take up entire target
            UTP4= UTP4TG
          END IF
        ELSE
C         there are no biochemical transformations occurring due
C         to either low temperatures or low moisture
C         zero fluxes
          MINZOP= 0.0
          IMMP4 = 0.0
          UTP4  = 0.0
          UTP4TG= 0.0
        END IF
      ELSE
C       biochemical fluxes are not recalculated
        IF (PUPTFG .EQ. 1) THEN
C         restore local target for yield-based 
          UTP4TG= UTP4
        END IF
      END IF
C
C     update all storages to account for fluxes - done every
C     interval; check and fix any storages that may be negative
C
C     initalize the fraction used to change any negative storages
C     that may have been computed; frac also acts as a flag
C     indicating negative storages were projected (when < 1.0)
      FRAC = 1.0
C
C     calculate temporary organic phosphorus in storage
      TORGP= ORGP- MINZOP+ IMMP4
C
      IF (TORGP .LT. 0.0) THEN
C       negative storage value is unrealistic
C       calculate that fraction of the flux that is
C       needed to make the storage zero
        FRAC= ORGP/(ORGP-TORGP)
C
C       write a warning that the organic phosphorus value will
C       be fixed up so that it does not go negative
C
        CALL OMSTD (DATIM)
        CALL OMSTI (LSNO)
        CALL OMSTR (FRAC)
        CALL OMSTR (ORGP)
        CALL OMSTR (TORGP)
        CALL OMSTR (MINZOP)
        CALL OMSTR (IMMP4)
        CHSTR = LAYID
        CALL OMSTC (I4,CHSTR1)
        SGRP = 1
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             PWCNT(3))
C
      END IF
C
C     calculate temporary adsorbed phosphorus in storage
      TP4AD= P4AD- DESP4+ ADSP4
C
      IF (TP4AD .LT. 0.0) THEN
C       negative storage value is unrealistic
C       calculate that fraction of the flux that is
C       needed to make the storage zero
        TFRAC= P4AD/(P4AD-TP4AD)
C
C       keep the smaller fraction; the smaller fraction
C       of the fluxes will make all the storages either zero
C       or positive
        IF (TFRAC.LT.FRAC)  FRAC= TFRAC
C
C       write a warning that the adsorbed value of phosphate will
C       be fixed up so that it does not go negative
C
        CALL OMSTD (DATIM)
        CALL OMSTI (LSNO)
        CALL OMSTR (FRAC)
        CALL OMSTR (P4AD)
        CALL OMSTR (TP4AD)
        CALL OMSTR (ADSP4)
        CALL OMSTR (DESP4)
        CHSTR = LAYID
        CALL OMSTC (I4,CHSTR1)
        SGRP = 2
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             PWCNT(4))
C
      END IF
C
C     calculate temporary solution phosphorus in storage
      TP4SU= P4SU+ DESP4+ MINZOP- (ADSP4+IMMP4+UTP4)
C
      IF (TP4SU .LT. 0.0) THEN
C       negative storage value is unrealistic
C       calculate that fraction of the flux that is
C       needed to make the storage zero
        TFRAC= P4SU/(P4SU-TP4SU)
C
C       keep the smaller fraction; the smaller fraction
C       of the fluxes will make all the storages either zero
C       or positive
        IF (TFRAC.LT.FRAC)  FRAC= TFRAC
C
C       write a warning that the solution value of phosphate will
C       be fixed up so that it does not go negative
C
        CALL OMSTD (DATIM)
        CALL OMSTI (LSNO)
        CALL OMSTR (FRAC)
        CALL OMSTR (P4SU)
        CALL OMSTR (TP4SU)
        CALL OMSTR (ADSP4)
        CALL OMSTR (DESP4)
        CALL OMSTR (MINZOP)
        CALL OMSTR (IMMP4)
        CALL OMSTR (UTP4)
        CHSTR = LAYID
        CALL OMSTC (I4,CHSTR1)
        SGRP = 3
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             PWCNT(5))
C
      END IF
C
      IF (FRAC .GE. 1.0) THEN
C       no storages have gone negative; use the temporary values
        ORGP= TORGP
        P4AD= TP4AD
        P4SU= TP4SU
      ELSE
C       at least one of the storages has gone negative
C       use frac to adjust the fluxes to make all the storages
C       zero or positive
        FRAC  = FRAC*0.99
        IMMP4 = IMMP4*FRAC
        MINZOP= MINZOP*FRAC
        ADSP4 = ADSP4*FRAC
        DESP4 = DESP4*FRAC
        UTP4  = UTP4*FRAC
C
C       recalculate the storages
        ORGP  = ORGP- MINZOP+ IMMP4
        P4AD  = P4AD+ ADSP4- DESP4
        P4SU  = P4SU+ DESP4+ MINZOP- (ADSP4+IMMP4+UTP4)
C
      END IF
C
C     accumulate plant phosphorus storage
      PLTP= PLTP+ UTP4
C
      IF (PUPTFG .EQ. 1) THEN
C       accumulate any deficit
        PDEFC= PDEFC+ PUPTG- UTP4
        IF (PDEFC .LT. 1.0E-06) THEN
C         deficit has been erased
          PDEFC= 0.0
        END IF
      END IF
C
C     reassign storages to "permanent" array
      PHO(1)= ORGP
      PHO(2)= P4AD
      PHO(3)= P4SU
      PHO(4)= PLTP
C
C     reassign fluxes to "permanent" array
      PHORXF(1)= ADSP4
      PHORXF(2)= DESP4
      PHORXF(3)= MINZOP
      PHORXF(4)= IMMP4
      PHORXF(5)= UTP4
C
      RETURN
      END
C
C
C
      SUBROUTINE   PHOSPB
C
C     + + + PURPOSE + + +
C     Handle section PHOS.
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PHOS2 + + +
      INCLUDE    'cplph.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,J
C
C     + + + END SPECIFICATIONS + + +
C
C     handle section phos
      DO 20 J= 1,2
        IF (SDPFP(J).GE.1) THEN
          PAD(SDPFP(J) +IVL1) = SEDP(J)
        END IF
C
        DO 10 I= 1, 2
          IF (PHADDX(J,I) .GE. 1) THEN
            PAD(PHADDX(J,I)+IVL1)= PHADDR(J,I)
          END IF
          IF (PHADWX(J,I) .GE. 1) THEN
            PAD(PHADWX(J,I)+IVL1)= PHADWT(J,I)
          END IF
          IF (PHADPX(J,I) .GE. 1) THEN
            PAD(PHADPX(J,I)+IVL1)= PHADEP(J,I)
          END IF
 10     CONTINUE
 20   CONTINUE
C
      DO 30 J= 1,5
        IF (TSP4SX(J).GE.1) THEN
          PAD(TSP4SX(J)+IVL1)= TSP4S(J)
        END IF
C
        IF (P4IMBX(J).GE.1) THEN
          PAD(P4IMBX(J)+IVL1)= P4IMB(J)
        END IF
C
        IF (ORPMNX(J).GE.1) THEN
          PAD(ORPMNX(J)+IVL1)= ORPMN(J)
        END IF
 30   CONTINUE
C
      DO 40 J= 1,3
        IF (SSP4SX(J).GE.1) THEN
          PAD(SSP4SX(J)+IVL1)= SSP4S(J)
        END IF
C
        IF (PHOIFX(J).GE.1) THEN
          PAD(PHOIFX(J)+IVL1)= PHOIF(J,1)
        END IF
 40   CONTINUE
C
      DO 50 J= 1, 4
        IF (PUPTGX(J).GE.1) THEN
          PAD(PUPTGX(J)+IVL1)= PUPTG(J)
        END IF
 50   CONTINUE
C
      IF (OSDPFP.GE.1) THEN
        PAD(OSDPFP+IVL1)= SOSEDP
      END IF
C
      IF (POPFP.GE.1) THEN
        PAD(POPFP +IVL1)= POPHOS
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PHOSPT
C
C     + + + PURPOSE + + +
C     Handle section PHOS.
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PHOS2 + + +
      INCLUDE    'cplph.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    J
C
C     + + + END SPECIFICATIONS + + +
C
C     handle section phos
      DO 10 J= 1,4
        IF (SPFP(J).GE.1) THEN
          PAD(SPFP(J)+IVL1)= SP(J)
        END IF
C
        IF (UPFP(J).GE.1) THEN
          PAD(UPFP(J)+IVL1)= UP(J)
        END IF
C
        IF (LPFP(J).GE.1) THEN
          PAD(LPFP(J)+IVL1)= LP(J)
        END IF
C
        IF (APFP(J).GE.1) THEN
          PAD(APFP(J)+IVL1)= AP(J)
        END IF
C
        IF (TPFP(J).GE.1) THEN
          PAD(TPFP(J)+IVL1)= TP(J)
        END IF
 10   CONTINUE
C
      DO 20 J=1, 5
        IF (PDFCTX(J).GE.1) THEN
          PAD(PDFCTX(J)+IVL1)= PDFCT(J)
        END IF
C
        IF (TPHFP(J).GE.1) THEN
          PAD(TPHFP(J)+IVL1)= TPHO(J)
        END IF
 20   CONTINUE
C
      IF (IPFP.GE.1) THEN
        PAD(IPFP  +IVL1)= IP
      END IF
C
      IF (TPHOFP.GE.1) THEN
        PAD(TPHOFP+IVL1)= TOTPHO
      END IF
C
      RETURN
      END
