C
C
C
      SUBROUTINE   PRQUAL
     M                    (OSVREC)
C
C     + + + PURPOSE + + +
C     Process input for the rqual sections of rchres application module
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    OSVREC
C
C     + + + ARGUMENT DEFINITIONS + + +
C     OSVREC - ???
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION RQUAL1 + + +
      INCLUDE     'crhrq.inc'
      INCLUDE     'crin2.inc'
      INCLUDE     'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I1,I2,I4,BENFGX(1)
      CHARACTER*4 FLUXID(2)
C
C     + + + INTRINSICS + + +
      INTRINSIC  MAX
C
C     + + + EXTERNALS + + +
      EXTERNAL   RTABLE,ITABLE,POXRX,PNUTRX,PPLANK,PPHCAR
C
C     + + + DATA INITIALIZATIONS + + +
      DATA        FLUXID  /'LBS ','KG  '/
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (A4)
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' PROCESSING INPUT FOR SECTIONS OXRX, NUTRX, ',
     $        'PLANK, AND PHCARB')
 2010 FORMAT (/,' FINISHED PROCESSING INPUT FOR SECTIONS OXRX, ',
     $          'NUTRX, PLANK, AND PHCARB')
 2020 FORMAT (  ' ',132('-'))
C
C     + + + END SPECIFICATIONS + + +
C
      I1=1
      IF (OUTLEV.GT.1) THEN
        WRITE (MESSU,2000)
      END IF
C
C     table-type benth-flag
      BENFGX(1)= BENRFG
      I2= 69
      I4=  1
      CALL ITABLE
     I             (I2,I1,I4,UUNITS,
     M              BENFGX)
      BENRFG= BENFGX(1)
C
C     table-type scour-parms
      I2= 70
      I4=  2
      CALL RTABLE
     I             (I2,I1,I4,UUNITS,
     M              SCRPM)
      IF (OUTLEV.GT.1) THEN
        WRITE (MESSU,2020)
      END IF
C
C     flux id's
      READ(FLUXID(1),1000) RQFLID(1)
      READ(FLUXID(2),1000) RQFLID(2)
C
      CALL POXRX
C
      IF (OUTLEV.GT.1) THEN
        WRITE (MESSU,2020)
      END IF
      OSVREC= MAX(OSVREC,33)
C
      IF (NUTFG.EQ.1) THEN
        CALL PNUTRX
        IF (OUTLEV.GT.1) THEN
          WRITE (MESSU,2020)
        END IF
        OSVREC= MAX(OSVREC,35)
        IF (PLKFG.EQ.1) THEN
          CALL PPLANK
          IF (OUTLEV.GT.1) THEN
            WRITE (MESSU,2020)
          END IF
          OSVREC= MAX(OSVREC,37)
          IF (PHFG.EQ.1) THEN
            CALL PPHCAR
            IF (OUTLEV.GT.1) THEN
              WRITE (MESSU,2020)
            END IF
            OSVREC= MAX(OSVREC,37)
          END IF
        END IF
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
      SUBROUTINE   RQUAL
C
C     + + + PURPOSE + + +
C     Simulate constituents involved in biochemical transformations
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION RQUAL2 + + +
      INCLUDE 'crhrq.inc'
      INCLUDE 'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     REQFG,TSSUB(2),FLGVAL
      REAL        VOLSP,DOXS
      CHARACTER*6 OPTYP,TSNAM,SECNAM,MSECNM,OPFGNM
C
C     + + + EXTERNALS + + +
      EXTERNAL   OXRX,NUTRX,PLANK,PHCARB,HREQTS
C
C     + + + DATA INITIALIZATIONS + + +
      DATA TSSUB/1,1/
      DATA OPTYP,SECNAM/'RCHRES','RQUAL '/
C
C     + + + END SPECIFICATIONS + + +
C
C     volume, average depth, and average velocity of water in
C     reach/res are obtained from inpad if module section hydr
C     is inactive
C
C     single precision version of vol
      VOLSP= VOL
C
      IF (HYDRFG .EQ. 0) THEN
CTHJ        AVDEP= PAD(AVDFP+IVL1)
        REQFG= 3
        MSECNM= 'HYDR  '
        TSNAM= 'AVDEP '
        CALL HREQTS (AVDFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,RCHNO,
     I               TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,FLGVAL,
     O               AVDEP)
CTHJ        AVVEL= PAD(AVVFP+IVL1)
        TSNAM= 'AVVEL '
        CALL HREQTS (AVVFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,RCHNO,
     I               TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,FLGVAL,
     O               AVVEL)
      END IF
C
C     convert average depth and average velocity to english units,
C     if necessary
C
      IF (UUNITS .EQ. 2) THEN
        AVDEPE= AVDEP*3.28
        AVVELE= AVVEL*3.28
      ELSE
        AVDEPE= AVDEP
        AVVELE= AVVEL
      END IF
      IF (AVDEPE .GT. 0.0) THEN
C       define conversion factor from mg/m2 to mg/l; avdepe is in feet
        DEPCOR= 3.28084E-3/AVDEPE
      ELSE
        DEPCOR= -1.E30
      END IF
      IF (BENRFG .EQ. 1) THEN
C       calculate scouring factor
C
        IF (AVVELE .GT. SCRVEL) THEN
          SCRFAC= SCRMUL
        ELSE
C         adjustment for scouring is not necessary
          SCRFAC= 1.0
        END IF
C
      END IF
C
C     temperature of water in reach/res is obtained from inpad
C     if module section htrch is inactive
      IF (HTFG .EQ. 0) THEN
CTHJ        TW= PAD(TWFP + IVL1)
        REQFG= 3
        MSECNM= 'HTRCH '
        TSNAM= 'TW    '
        CALL HREQTS (TWFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,RCHNO,
     I               TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,FLGVAL,
     O               TW)
C       set undefined temp value to 20 degc
        IF (TW .LT. -100.) THEN
          TW= 20.
        END IF
      END IF
C
C     simulate primary do and bod balances
      CALL OXRX
C
      IF (NUTFG .EQ. 1) THEN
C       simulate primary inorganic nitrogen and phosphorus balances
        CALL NUTRX
C
        IF (PLKFG .EQ. 1) THEN
C         simulate plankton populations and associated reactions
          CALL PLANK
C
          IF (PHFG .EQ. 1) THEN
C           simulate ph and carbon species
            CALL PHCARB
          END IF
        END IF
C       update totals of nutrients
        RNO3 = NO3* VOL
        RTAM = TAM* VOL
        RNO2 = NO2* VOL
        RPO4 = PO4* VOL
        RNH4 = NH4* VOL
        RNH3 = NH3* VOL
        RRNO3= NO3* VOL
        RRTAM= TAM* VOL
        IF (ADNHFG .EQ. 1) THEN
C         add adsorbed suspended nh4 to dissolved
          RRTAM = RRTAM + RSNH4(4)
        END IF
        RRNO2= NO2* VOL
        RRPO4= PO4* VOL
        IF (ADPOFG .EQ. 1) THEN
C         add adsorbed suspended po4 to dissolved
          RRPO4 = RRPO4 + RSPO4(4)
        END IF
      END IF
C
C     check do level; if dox exceeds user specified level of
C     supersaturation, then release excess do to the atmosphere
      DOXS= DOX
      IF (DOX .GT. SUPSAT*SATDO) THEN
        DOX= SUPSAT*SATDO
      END IF
      READOX= READOX + (DOX - DOXS)*VOLSP
      TOTDOX= READOX+ BODDOX+ BENDOX+ NITDOX+ PHYDOX+ ZOODOX+ BALDOX
C     update dissolved totals and totals of nutrients
      RDOX = DOX* VOL
      RBOD = BOD* VOL
C
      RETURN
      END
C
C
C
      SUBROUTINE   BENTH
     I                   (DOX,ANAER,BRCON,SCRFAC,DEPCOR,
     M                    CONC,
     O                    RELEAS)
C
C     + + + PURPOSE + + +
C     Simulate benthal release of constituent
C
C     + + + DUMMY ARGUMENTS + + +
      REAL       DOX,ANAER,BRCON(2),SCRFAC,DEPCOR,CONC,RELEAS
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DOX    - dissolved oxygen concentration in mg/l
C     ANAER  - ???
C     BRCON  - ???
C     SCRFAC - ???
C     DEPCOR - ???
C     CONC   - ???
C     RELEAS - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I
C
C     + + + END SPECIFICATIONS + + +
C
      IF (DOX .GT. ANAER) THEN
C       assign aerobic release rate
        I= 1
      ELSE
C       assign anaerobic release rate
        I= 2
      END IF
C
C     calculate benthal release of constituent; release is a step
C     function of aerobic/anaerobic conditions, and stream velocity;
C     scrfac, the scouring factor dependent on stream velocity and
C     depcor, the conversion factor from mg/m2 to mg/l, are both
C     calculated in rqual; releas is expressed in mg/m2.ivl
      RELEAS= BRCON(I)*SCRFAC*DEPCOR
C
C     add release to constituent state variable
      CONC  = CONC + RELEAS
C
      RETURN
      END
C
C
C
      SUBROUTINE   DECBAL
     I                    (TAMFG,PO4FG,DECNIT,DECPO4,
     M                     TAM,NO3,PO4)
C
C     + + + PURPOSE + + +
C     Perform materials balance for transformation from organic to
C     inorganic material by decay in reach water
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    TAMFG,PO4FG
      REAL       DECNIT,DECPO4,TAM,NO3,PO4
C
C     + + + ARGUMENT DEFINITIONS + + +
C     TAMFG  - ???
C     PO4FG  - ???
C     DECNIT - ???
C     DECPO4 - ???
C     TAM    - total ammonia (nh3 + nh4) in mg n/l
C     NO3    - dissolved nitrate concentration in mg/l
C     PO4    - ???
C
C     + + + END SPECIFICATIONS + + +
C
      IF (TAMFG .NE. 0) THEN
C       add nitrogen transformed to inorganic nitrogen by biomass
C       decomposition to tam state variable
        TAM= TAM + DECNIT
      ELSE
C       add nitrogen transformed to inorganic nitrogen by biomass
C       decomposition to no3 state variable
        NO3= NO3 + DECNIT
      END IF
C
      IF (PO4FG .NE. 0) THEN
C       add phosphorus transformed to inorganic phosphorus by
C       biomass decomposition to po4 state variable
        PO4= PO4 + DECPO4
      ELSE
C       po4 is not simulated
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   SINK
     I                  (VOL,AVDEPE,KSET,
     M                   CONC,
     O                   SNKMAT)
C
C     + + + PURPOSE + + +
C     Calculate quantity of material settling out of the control
C     volume; determine the change in concentration as a result of
C     sinking
C
C     + + + DUMMY ARGUMENTS + + +
      REAL       AVDEPE,KSET,SNKMAT
      DOUBLE PRECISION CONC,VOL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     VOL    - volume of water in reach above bed
C     AVDEPE - ???
C     KSET   - ???
C     CONC   - ???
C     SNKMAT - ???
C
C     + + + LOCAL VARIABLES + + +
      REAL       SNKOUT
C
C     + + + END SPECIFICATIONS + + +
C
      IF (KSET.GT.0.0 .AND. AVDEPE.GT.0.17) THEN
C       calculate concentration change due to outgoing material;
C       snkout is expressed in mass/liter/ivl; kset is expressed
C       as ft/ivl and avdepe as feet
C
        IF (KSET .LT. AVDEPE) THEN
C         calculate portion of material which settles out of the
C         control volume during time step; snkout is expressed as
C         mass/liter.ivl; conc is the concentration of material in
C         the control volume
          SNKOUT= CONC*(KSET/AVDEPE)
        ELSE
C         all material sinks out of control volume
          SNKOUT= CONC
        END IF
C
C       calculate remaining concentration of material in the control
C       volume
        CONC= CONC- SNKOUT
C
C       find quantity of material that sinks out; units are
C       mass.ft3/l.ivl in english system, and mass.m3/l.ivl in
C       metric system
        SNKMAT= SNKOUT*VOL
      ELSE
C       assume no settling occurs
        SNKOUT= 0.0
        SNKMAT= 0.0
      END IF
C
      RETURN
      END
