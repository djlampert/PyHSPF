C
C
C
      SUBROUTINE   FIRSTP
     I                   (OUTLEV,UUNITS,DELT60,GNUM,GSUB,GNVALS,
     I                    NUM,SUBBAS,NVALS,CNUM,BNUM,LSNO,MESSU,
     I                    MSGFL,
     M                    WCOUNT,
     O                    GPARM,SPARM,UPARM,LPARM,APARM)
C
C     + + + PURPOSE + + +
C     Process reaction parameters required for first-order kinetics
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    BNUM,CNUM,GNUM,GNVALS,GSUB,LSNO,MESSU,
     $           NUM,NVALS,OUTLEV,SUBBAS,UUNITS,MSGFL,
     $           WCOUNT
      REAL       DELT60,APARM(NVALS),GPARM(GNVALS),LPARM(NVALS),
     $           SPARM(NVALS),UPARM(NVALS)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     OUTLEV - run interpreter output level
C     UUNITS - system of units   1-english, 2-metric
C     DELT60 - simulation time interval in hours
C     GNUM   - ???
C     GSUB   - ???
C     GNVALS - ???
C     NUM    - ???
C     SUBBAS - ???
C     NVALS  - ???
C     CNUM   - ???
C     BNUM   - ???
C     LSNO   - land surface id number
C     MESSU  - ftn unit no. to be used for printout of messages
C     MSGFL  - fortran unit number of warning message file
C     WCOUNT - ???
C     GPARM  - ???
C     SPARM  - ???
C     UPARM  - ???
C     LPARM  - ???
C     APARM  - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     SUB
      CHARACTER*4 LAYID(4)
C
C     + + + EXTERNALS + + +
      EXTERNAL    RTABLE, FSTTAB
C
C     + + + DATA INITIALIZATIONS + + +
      DATA        LAYID/'SURF','UPPR','LOWR','GRND'/
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' PARAMETERS TO BE USED FOR FIRST-ORDER KINETIC ',
     $            'REACTIONS')
 2010 FORMAT (/,' PARAMETERS FOR SURFACE LAYER')
 2020 FORMAT (/,' PARAMETERS FOR UPPER LAYER')
 2030 FORMAT (/,' PARAMETERS FOR LOWER LAYER')
 2040 FORMAT (/,' PARAMETERS FOR ACTIVE GROUNDWATER LAYER')
C
C     + + + END SPECIFICATIONS + + +
C
      IF (OUTLEV.GT.2) THEN
C       processing kinetic parameters message
        WRITE (MESSU,2000)
      END IF
C
C     get general parameters, such as first-order kinetics temp
C     correction factors
      CALL RTABLE (GNUM,GSUB,GNVALS,UUNITS,
     M             GPARM)
C
      IF (OUTLEV.GT.2) THEN
C       surface layer message
        WRITE (MESSU,2010)
      END IF
      SUB= SUBBAS + 1
      CALL FSTTAB (NUM,SUB,NVALS,UUNITS,DELT60,CNUM,BNUM,LSNO,
     $             MESSU,MSGFL,LAYID(1),
     M             WCOUNT,
     O             SPARM)
C
      IF (OUTLEV.GT.2) THEN
C       upper layer message
        WRITE (MESSU,2020)
      END IF
      SUB= SUBBAS + 2
      CALL FSTTAB (NUM,SUB,NVALS,UUNITS,DELT60,CNUM,BNUM,LSNO,
     $             MESSU,MSGFL,LAYID(2),
     M             WCOUNT,
     O             UPARM)
C
      IF (OUTLEV.GT.2) THEN
C       lower layer message
        WRITE (MESSU,2030)
      END IF
      SUB= SUBBAS + 3
      CALL FSTTAB (NUM,SUB,NVALS,UUNITS,DELT60,CNUM,BNUM,LSNO,
     $             MESSU,MSGFL,LAYID(3),
     M             WCOUNT,
     O             LPARM)
C
      IF (OUTLEV.GT.2) THEN
C       active groundwater layer message
        WRITE (MESSU,2040)
      END IF
      SUB= SUBBAS + 4
      CALL FSTTAB (NUM,SUB,NVALS,UUNITS,DELT60,CNUM,BNUM,LSNO,
     $             MESSU,MSGFL,LAYID(4),
     M             WCOUNT,
     O             APARM)
C
      RETURN
      END
C
C
C
      SUBROUTINE   FSTTAB
     I                   (TNUM,TSUB,NVALS,UUNITS,DELT60,CNUM,BNUM,LSNO,
     I                    MESSU,MSGFL,LAYID,
     M                    WCOUNT,
     O                    RVAL)
C
C     + + + PURPOSE + + +
C     Process a table containing first-order reaction parameters
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     BNUM,CNUM,LSNO,MESSU,NVALS,
     $            TNUM,TSUB,UUNITS,MSGFL,WCOUNT
      REAL        DELT60,RVAL(NVALS)
      CHARACTER*4 LAYID
C
C     + + + ARGUMENT DEFINITIONS + + +
C     TNUM   - ???
C     TSUB   - ???
C     NVALS  - ???
C     UUNITS - system of units   1-english, 2-metric
C     DELT60 - simulation time interval in hours
C     CNUM   - ???
C     BNUM   - ???
C     LSNO   - land surface id number
C     MESSU  - ftn unit no. to be used for printout of messages
C     MSGFL  - fortran unit number of hspf message file
C     LAYID  - ???
C     WCOUNT - ???
C     RVAL   - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,I4,WARNFG,WARNG(10),SGRP,SCLU
      CHARACTER*4 CHSTR
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (CHSTR,CHSTR1)
      CHARACTER*1  CHSTR1(4)
C
C     + + + EXTERNALS + + +
      EXTERNAL   RTABLE,OMSG,OMSTI,OMSTC
C
C     + + + INTRINSICS + + +
      INTRINSIC FLOAT
C
C     + + + END SPECIFICATIONS + + +
C
C     error/warn message cluster
      SCLU= 308
C
      CALL RTABLE (TNUM,TSUB,NVALS,UUNITS,
     M             RVAL)
C
C     change units from 1/day to 1/ivl
      DO 10 I= 1,NVALS
        RVAL(I)= RVAL(I)*DELT60/24.0
 10   CONTINUE
C
C     check whether they are likely to give negative concentrations
C     adsorption/desorption rate parms
      WARNFG= 0
      DO 40 I= 1,2
        I4= CNUM
        IF ((RVAL(I)*FLOAT(I4)) .GT. 0.5) THEN
C         likely problem
          WARNFG  = 1
          WARNG(I)= 1
        ELSE
          WARNG(I)= 0
        END IF
 40   CONTINUE
C
      IF (NVALS.GT.2) THEN
C       biochemical rate parms
        DO 70 I= 3,NVALS
          I4= BNUM
          IF ((RVAL(I)*FLOAT(I4)) .GT. 0.5) THEN
            WARNFG  = 1
            WARNG(I)= 1
          ELSE
            WARNG(I)= 0
          END IF
 70     CONTINUE
      END IF
C
      IF (WARNFG.EQ.1) THEN
C       one or more rate parms is likely to give negative concs
        CALL OMSTI (LSNO)
        DO 90 I = 1,NVALS
          CALL OMSTI (WARNG(I))
 90     CONTINUE
        CHSTR= LAYID
        I4   = 4
        CALL OMSTC (I4,CHSTR1)
        SGRP = 4
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             WCOUNT)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PLNTPM
     I                   (MESSU,VUTFG,NUM1,UUNITS,DELT60,OUTLEV,NUM2,
     O                    KPL,SKPLM,UKPLM,LKPLM,AKPLM)
C
C     + + + PURPOSE + + +
C     Process plant uptake parameters
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSU,NUM1,NUM2,OUTLEV,UUNITS,VUTFG
      REAL      AKPLM(12),DELT60,KPL(4),LKPLM(12),SKPLM(12),UKPLM(12)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSU  - ftn unit no. to be used for printout of messages
C     VUTFG  - ???
C     NUM1   - ???
C     UUNITS - system of units   1-english, 2-metric
C     DELT60 - simulation time interval in hours
C     OUTLEV - run interpreter output level
C     NUM2   - ???
C     KPL    - ???
C     SKPLM  - ???
C     UKPLM  - ???
C     LKPLM  - ???
C     AKPLM  - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,I1,I2,I3,I4,I12,J
C
C     + + + EXTERNALS + + +
      EXTERNAL  RTABLE
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' PLANT UPTAKE RATE PARAMETERS FOR SURFACE, ',
     $            'UPPER, LOWER, AND GROUNDWATER LAYERS (1/DAY)')
C
C     + + + END SPECIFICATIONS + + +
C
      I1 = 1
      I2 = 2
      I3 = 3
      I4 = 4
      I12= 12
      IF (VUTFG .EQ. 0) THEN
C       plant uptake reaction rate parms do not vary throughout year
        CALL RTABLE (NUM1,I1,I4,UUNITS,
     M               KPL)
C
C       convert units to 1/ivl
        DO 10 J= 1,4
          KPL(J)= KPL(J)*DELT60/24.0
 10     CONTINUE
      ELSE
C       parms do vary throughout the year
        IF (OUTLEV.GT.2) THEN
C         header message
          WRITE (MESSU,2000)
        END IF
C
        CALL RTABLE (NUM2,I1,I12,UUNITS,
     M               SKPLM)
C
        CALL RTABLE (NUM2,I2,I12,UUNITS,
     M               UKPLM)
C
        CALL RTABLE (NUM2,I3,I12,UUNITS,
     M               LKPLM)
C
        CALL RTABLE (NUM2,I4,I12,UUNITS,
     M               AKPLM)
C
C       convert units from 1/day to 1/ivl
        DO 30 I= 1,12
          SKPLM(I)= SKPLM(I)*DELT60/24.0
          UKPLM(I)= UKPLM(I)*DELT60/24.0
          LKPLM(I)= LKPLM(I)*DELT60/24.0
          AKPLM(I)= AKPLM(I)*DELT60/24.0
 30     CONTINUE
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   SVALP
     I                  (OUTLEV,MESSU,UUNITS,GNUM,GSUB,NUM,SUBBAS,
     O                   GPARM,SPARM,UPARM,LPARM,APARM)
C
C     + + + PURPOSE + + +
C     Process parameters required for single-valued freundlich
C     adsorption/desorption calculations
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   GNUM,GSUB,MESSU,NUM,OUTLEV,SUBBAS,UUNITS
      REAL      APARM(4),GPARM(1),LPARM(4),SPARM(4),UPARM(4)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     OUTLEV - run interpreter output level
C     MESSU  - ftn unit no. to be used for printout of messages
C     UUNITS - system of units   1-english, 2-metric
C     GNUM   - ???
C     GSUB   - ???
C     NUM    - ???
C     SUBBAS - ???
C     GPARM  - ???
C     SPARM  - ???
C     UPARM  - ???
C     LPARM  - ???
C     APARM  - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I1,SUB
      REAL      CMAX
C
C     + + + EXTERNALS + + +
      EXTERNAL  RTABLE,SVTAB
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' PARAMETERS TO BE USED FOR SINGLE-VALUED ',
     $            'FREUNDLICH ADSORPTION/DESORPTION CALCULATIONS')
 2010 FORMAT (/,' PARAMETERS FOR SURFACE LAYER')
 2020 FORMAT (/,' PARAMETERS FOR THE UPPER LAYER')
 2030 FORMAT (/,' PARAMETERS FOR THE LOWER LAYER')
 2040 FORMAT (/,' PARAMETERS FOR THE ACTIVE GROUNDWATER LAYER')
C
C     + + + END SPECIFICATIONS + + +
C
      I1= 1
C
      IF (OUTLEV.GT.2) THEN
C       parameters message
        WRITE (MESSU,2000)
      END IF
C
C     get cmax - maximum solubility
      CALL RTABLE (GNUM,GSUB,I1,UUNITS,
     M             GPARM)
      CMAX= GPARM(1)
C
      IF (OUTLEV.GT.2) THEN
C       surface layer message
        WRITE (MESSU,2010)
      END IF
      SUB= SUBBAS + 1
      CALL SVTAB (NUM,SUB,UUNITS,CMAX,
     O            SPARM)
C
      IF (OUTLEV.GT.2) THEN
C       upper layer message
        WRITE (MESSU,2020)
      END IF
      SUB= SUBBAS + 2
      CALL SVTAB (NUM,SUB,UUNITS,CMAX,
     O            UPARM)
C
      IF (OUTLEV.GT.2) THEN
C       lower layer message
        WRITE (MESSU,2030)
      END IF
      SUB= SUBBAS + 3
      CALL SVTAB (NUM,SUB,UUNITS,CMAX,
     O            LPARM)
C
      IF (OUTLEV.GT.2) THEN
C       groundwater layer message
        WRITE (MESSU,2040)
      END IF
      SUB= SUBBAS + 4
      CALL SVTAB (NUM,SUB,UUNITS,CMAX,
     O            APARM)
C
      RETURN
      END
C
C
C
      SUBROUTINE   SVTAB
     I                  (TNUM,TSUB,UUNITS,CMAX,
     O                   PARM)
C
C     + + + PURPOSE + + +
C     Process a table containing single-valued freundlich reaction
C     parameters
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    TNUM,TSUB,UUNITS
      REAL       CMAX,PARM(4)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     TNUM   - table id number
C     TSUB   - table subscript
C     UUNITS - system of units   1-english, 2-metric
C     CMAX   - ???
C     PARM   - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I3
      REAL      RVAL(3)
C
C     + + + EXTERNALS + + +
      EXTERNAL  RTABLE
C
C     + + + END SPECIFICATIONS + + +
C
      I3= 3
      CALL RTABLE (TNUM,TSUB,I3,UUNITS,
     M             RVAL)
C
C     fixed capacity - xfix
      PARM(1)= RVAL(1)
C
C     freundlich k
      PARM(3)= RVAL(2)
C
C     freundlich n - store its inverse
      PARM(4)= 1.0/RVAL(3)
C
C     calculate xmax - max adsorption capacity
      PARM(2)= PARM(3)*(CMAX**PARM(4))+ PARM(1)
C
      RETURN
      END
C
C
C
      SUBROUTINE   SOLDAT
     I                   (UUNITS,
     O                    SOILM,SOILD)
C
C     + + + PURPOSE + + +
C     Get data on properties of soil - table-type soil-data
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   UUNITS
      REAL      SOILM(5),SOILD(4)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     UUNITS - system of units   1-english, 2-metric
C     SOILM  - mass of soil in layer
C     SOILD  - depth of soil layer
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,TBNO,TBSB,NVAL
      REAL      RVAL(8),FACT
C
C     + + + EXTERNALS + + +
      EXTERNAL  RTABLE
C
C     + + + END SPECIFICATIONS + + +
C
      TBNO= 27
      TBSB= 1
      NVAL= 8
      CALL RTABLE (TBNO,TBSB,NVAL,UUNITS,
     M             RVAL)
C
C     convert soil depths to mass
      IF (UUNITS.EQ.1) THEN
C       ft3/acre-inch
        FACT= 3.63E03
      ELSE
C       kg.cm2/g.ha
        FACT= 1.00E05
      END IF
C
      DO 30 I= 1,4
        SOILD(I)= RVAL(I)
        SOILM(I+1)= SOILD(I)*RVAL(I+4)*FACT
 30   CONTINUE
C
C     get mass of surface layer in tons/acre
      IF (UUNITS.EQ.1) THEN
        SOILM(1)= SOILM(2)/2000.0
      ELSE
        SOILM(1)= SOILM(2)/2241.0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   STORGE
     I                   (MESSU,OUTLEV,UUNITS,NUM,NVALS,HEADG,NUMI,
     I                    NVALSI,
     M                    SUB,SUBI,
     O                    SMAT,UMAT,IMAT,LMAT,AMAT,TMAT,TOTMAT)
C
C     + + + PURPOSE + + +
C     Process input of the initial storages of a material in the soil
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      MESSU,NUM,NUMI,NVALS,NVALSI,OUTLEV,SUB,SUBI,UUNITS
      REAL         AMAT(NVALS),IMAT(NVALSI),LMAT(NVALS),SMAT(NVALS),
     $             TMAT(NVALS),TOTMAT,UMAT(NVALS)
      CHARACTER*60 HEADG
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSU  - ftn unit no. to be used for printout of messages
C     OUTLEV - run interpreter output level
C     UUNITS - system of units   1-english, 2-metric
C     NUM    - ???
C     NVALS  - ???
C     HEADG  - ???
C     NUMI   - ???
C     NVALSI - ???
C     SUB    - ???
C     SUBI   - ???
C     SMAT   - ???
C     UMAT   - ???
C     IMAT   - ???
C     LMAT   - ???
C     AMAT   - ???
C     TMAT   - ???
C     TOTMAT - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I
      CHARACTER*8 AGUNIT(2)
C
C     + + + EXTERNALS + + +
      EXTERNAL    RTABLE
C
C     + + + DATA INITIALIZATIONS + + +
      DATA        AGUNIT /'(LB/AC) ','(KG/HA) '/
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' SEGMENT-WIDE STORAGES IN SURFACE, UPPER, AND ',
     $            'INTERFLOW LAYERS')
 2020 FORMAT (/,' SEGMENT-WIDE STORAGES IN SURFACE, UPPER, AND ',
     $            'INTERFLOW LAYERS ',3X,A8)
 2030 FORMAT (/,' ',A60)
 2040 FORMAT (  ' ',10(1PE10.3))
 2050 FORMAT (/,' SEGMENT-WIDE STORAGES IN LOWER AND ACTIVE ',
     $            'GROUNDWATER LAYERS')
 2060 FORMAT (/,' TOTAL STORAGE IN THE SEGMENT ',3X,A8)
 2070 FORMAT (/,' GRAND TOTAL IS: ',1PE10.3)
C
C     + + + END SPECIFICATIONS + + +
C
C     topsoil layers have not been subdivided into blocks
      IF (OUTLEV.GT.2) THEN
C       processing total storage message
        WRITE (MESSU,2000)
      END IF
      SUB= SUB+ 1
      CALL RTABLE (NUM,SUB,NVALS,UUNITS,
     M             SMAT)
C
      SUB= SUB+ 1
      CALL RTABLE (NUM,SUB,NVALS,UUNITS,
     M             UMAT)
C
      SUBI= SUBI+ 1
      CALL RTABLE (NUMI,SUBI,NVALSI,UUNITS,
     M             IMAT)
C
      IF (OUTLEV.GT.2) THEN
C       storage summary message
        WRITE (MESSU,2050)
      END IF
      SUB= SUB+ 1
      CALL RTABLE (NUM,SUB,NVALS,UUNITS,
     M             LMAT)
C
      SUB= SUB+ 1
      CALL RTABLE (NUM,SUB,NVALS,UUNITS,
     M             AMAT)
C
C     determine total storage in the system
      DO 70 I= 1, NVALS
        TMAT(I)= SMAT(I)+ UMAT(I)+ LMAT(I)+ AMAT(I)
 70   CONTINUE
C
C     add in materials in interflow storage
      DO 80 I= 1, NVALSI
        IF ( (I .EQ. 1) .OR. (I .EQ. 2) ) THEN
C         normal interflow storages
          TMAT(2+I)= IMAT(I)+ TMAT(2+I)
        ELSE IF (I .EQ. 3) THEN
C         labile organic nitrogen
          TMAT(1)= IMAT(I)+ TMAT(1)
        ELSE IF (I .EQ. 4) THEN
C         labile organic nitrogen
          TMAT(6)= IMAT(I)+ TMAT(6)
        ELSE IF ( (I .EQ. 5) .OR. (I .EQ. 6) ) THEN
C         above-ground plant n or litter n
          TMAT(5)= IMAT(I)+ TMAT(5)
        END IF
 80   CONTINUE
C
C     grand total
      TOTMAT= 0.0
      DO 90 I= 1, NVALS
        TOTMAT= TOTMAT+ TMAT(I)
 90   CONTINUE
C
      IF (OUTLEV.GT.2) THEN
C       total message
        WRITE (MESSU,2060)  AGUNIT(UUNITS)
        WRITE (MESSU,2030)  HEADG
        WRITE (MESSU,2040)  TMAT
        WRITE (MESSU,2070)  TOTMAT
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   TOPMOV
     I                   (FRAC,
     M                    SSCM,USCM,ISCM,
     O                    TSCM)
C
C     + + + PURPOSE + + +
C     Move solutes with the water in the topsoil (surface and
C     upper layers) and update storages
C
C     + + + DUMMY ARGUMENTS + + +
      REAL       FRAC(5),ISCM,SSCM,TSCM(5),USCM
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FRAC   - ???
C     SSCM   - ???
C     USCM   - ???
C     ISCM   - ???
C     TSCM   - ???
C
C     + + + LOCAL VARIABLES + + +
      REAL       DUMMY,FII,FIO,FSO,FSP,FUP,IICM,IOCM,SOCM,SPCM,UPCM
C
C     + + + END SPECIFICATIONS + + +
C
C     units of fluxes are mass/area-ivl and storages are mass/area,
C     i.e. lbs/acre for english units and kg/ha for metric units
C
C     assign fractional fluxes to local variables
      FSO= FRAC(1)
      FSP= FRAC(2)
      FII= FRAC(3)
      FUP= FRAC(4)
      FIO= FRAC(5)
C
C     surface layer
      DUMMY= FSO+ FSP
      IF (SSCM.GT.1.0E-8 .AND. DUMMY.GT.0.0) THEN
C       there is sufficient solute in surface layer storage that is to
C       be moved by water determine the amount in surface (runoff) outflow
        SOCM= SSCM*FSO
C
C       determine the amount (leached) percolated from the
C       surface layer storage to upper layer storage
        SPCM= SSCM*FSP
C
C       remove from the surface layer storage
        SSCM= SSCM- (SOCM+SPCM)
        IF (SSCM.LT.1.0E-10) THEN
C         adjust, in case round-off has made storage negative
          SSCM= 1.0E-10
        END IF
      ELSE
C       there is either very little solute in the surface layer
C       storage, or there is no water moving from that layer that
C       will transport solute
        SOCM= 0.0
        SPCM= 0.0
      END IF
C
C     upper layer
C     add solute leached from the surface layer to the upper layer storage
      USCM= USCM+ SPCM
C
      DUMMY= USCM+ ISCM
      IF (DUMMY.GT.1.0E-7) THEN
C       there is some solute in the upper layer storages (principal
C       and interflow)
C       determine the amount (leached) percolated from the
C       upper layer storage to lower layer storage
        UPCM= USCM*FUP
C
C       determine transfer from the upper layer principal storage
C       to the upper layer transitory (interflow) storage
        IICM= USCM*FII
C
C       add to interflow storage
        ISCM= ISCM+ IICM
C
C       determine amount carried in interflow outflow
        IOCM= ISCM*FIO
C
C       remove from interflow storage
        ISCM= ISCM- IOCM
        IF (ISCM.LT.0.0) THEN
C         adjust, in case round-off has made storage negative
          ISCM= 0.0
        END IF
C
C       remove solutes from the upper layer principal storage
        USCM= USCM- (IICM+UPCM)
        IF (USCM.LT.0.0) THEN
C         adjust, in case round-off has made storage negative
          USCM= 0.0
        END IF
      ELSE
C       there is very little solute in the upper layer (principal
C       and interflow) storages so zero fluxes
        UPCM= 0.0
        IICM= 0.0
        IOCM= 0.0
      END IF
C
C     assign chemical fluxes to "permanent" storage
      TSCM(1)= SOCM
      TSCM(2)= SPCM
      TSCM(3)= UPCM
      TSCM(4)= IICM
      TSCM(5)= IOCM
C
      RETURN
      END
C
C
C
      SUBROUTINE   AGRGET
     I                   (MESSU,MSGFL,DATIM,LSNO,SEDFG,MSTLFG,PSTFG,
     I                    TMPFG,IVL1,SOSDFP,MSTFP,FRACFP,SLTFP,ULTFP,
     I                    LGTFP,SECNAM,
     O                    SOSED,MST,FRAC,SLTMP,ULTMP,LGTMP)
C
C     + + + PURPOSE + + +
C     Get time series required by agri-chemical sections
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MESSU,MSGFL,DATIM(5),LSNO,SEDFG,MSTLFG,PSTFG,TMPFG,
     $            IVL1,SOSDFP,MSTFP(5),FRACFP(8),SLTFP,ULTFP,LGTFP
      REAL        SOSED,MST(5),FRAC(8),SLTMP,ULTMP,LGTMP
      CHARACTER*6 SECNAM
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSU  - ???
C     MSGFL  - ???
C     DATIM  - ???
C     LSNO   - ???
C     SEDFG  - ???
C     MSTLFG - ???
C     PSTFG  - ???
C     TMPFG  - ???
C     IVL1   - ???
C     SOSDFP - ???
C     MSTFP  - ???
C     FRACFP - ???
C     SLTFP  - ???
C     ULTFP  - ???
C     LGTFP  - ???
C     SECNAM - ???
C     SOSED  - ???
C     MST    - ???
C     FRAC   - ???
C     SLTMP  - ???
C     ULTMP  - ???
C     LGTMP  - ???
C
C     + + + COMMON BLOCKS + + +
      INCLUDE    'cmdum.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     J,REQFG,TSSUB(2),FLGVAL
      CHARACTER*6 OPTYP,TSNAM,MSECNM,OPFGNM
C
C     + + + EXTERNALS + + +
      EXTERNAL    HREQTS
C
C     + + + DATA INITIALIZATIONS + + +
      DATA        OPTYP/'PERLND'/
C
C     + + + END SPECIFICATIONS + + +
C
      IF (SEDFG .EQ. 0) THEN
C       read time series supplied by sedmnt
C       surface and upper layers of the land segment have not
C       been subdivided into blocks
CTHJ        SOSED= PAD(SOSDFP+IVL1)
        REQFG= 3
        MSECNM= 'SEDMNT'
        TSNAM= 'SOSED '
        CALL HREQTS (SOSDFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,
     I               LSNO,TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,FLGVAL,
     O               SOSED)
      ELSE
C       the above time series are available from sedmnt
      END IF
C
      IF (MSTLFG .EQ. 0) THEN
C       read time series supplied by mstlay
        REQFG= 3
        TSNAM= 'MST   '
        DO 60 J= 1,5
CTHJ          MST(J)= PAD(MSTFP(J)+IVL1)
          CALL HREQTS (MSTFP(J),IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,
     I                 LSNO,TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,FLGVAL,
     O                 MST(J))
 60     CONTINUE
C
        TSNAM= 'FRAC  '
        DO 70 J= 1,8
CTHJ          FRAC(J)= PAD(FRACFP(J)+IVL1)
          CALL HREQTS (FRACFP(J),IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,
     I                 LSNO,TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,FLGVAL,
     O                 FRAC(J))
 70     CONTINUE
      ELSE
C       the above time series are available from mstlay
      END IF
C
      IF (TMPFG .EQ. 1) THEN
C       soil temperature is required, find it somewhere.
        IF (PSTFG .EQ. 0) THEN
          SLTMP= PAD(SLTFP+IVL1)
          ULTMP= PAD(ULTFP+IVL1)
          LGTMP= PAD(LGTFP+IVL1)
        ELSE
C         the above time series are available from pstemp
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   ITER
     I                 (TCM,MOISTM,SOILM,KF,NI,XFIX,ITMAX,CMID,LAYID,
     I                  LSNO,MESSU,MSGFL,DATIM,FIXCAP,
     M                  C,ECNT,
     O                  X)
C
C     + + + PURPOSE + + +
C     Iterate until a sufficiently close approximation for the adsorbed
C     and solution values on the freundlich isotherm is reached
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     ECNT,MSGFL,ITMAX,LSNO,MESSU,DATIM(5)
      REAL        C,FIXCAP,KF,MOISTM,NI,SOILM,TCM,X,XFIX
      CHARACTER*4 LAYID,CMID(5)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     TCM    - ???
C     MOISTM - ???
C     SOILM  - ???
C     KF     - ???
C     NI     - ???
C     XFIX   - ???
C     ITMAX  - ???
C     CMID   - ???
C     LAYID  - ???
C     BLK    - current block number
C     LSNO   - land surface id number
C     MESSU  - ftn unit no. to be used for printout of messages
C     MSGFL  - fortran unit number of error message file
C     FIXCAP - ???
C     C      - ???
C     ECNT   - ???
C     X      - ???
C     DATIM  - date and time of day
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      COUNT,I4,I20,SGRP,SCLU,J
      REAL         RCLOSE,FRAC,DENOM
      CHARACTER*4  CHSTR,CHSTR4(5)
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (CHSTR,CHSTR1)
      CHARACTER*1  CHSTR1(4)
      EQUIVALENCE (CHSTR4,CHSTR2)
      CHARACTER*1  CHSTR2(20)
C
C     + + + EXTERNALS + + +
      EXTERNAL   OMSTD,OMSTI,OMSTC,OMSTR,OMSG
C
C     + + + INTRINSICS + + +
      INTRINSIC  ABS
C
C     + + + END SPECIFICATIONS + + +
C
C     error/warn message cluster
      SCLU=  308
      I4=      4
      I20=    20
      COUNT=   0
C
C     dountil
 10   CONTINUE
C
C       estimate adsorbed phase value
        X= KF*(C**NI)+ XFIX
C
C       recalculate the fraction which compares the freundlich
C       estimates with the total chemical
        DENOM= (X*SOILM+ C*MOISTM)*1.0E-06- FIXCAP
        IF (DENOM .LE. 0.0) THEN
C         denominator too small - perturb to make positive
          FRAC= 2.0
          WRITE (MESSU,*) 'WARNING - ITER: DENOM',DENOM,' COUNT',
     #                     COUNT,' FRAC 2.0'
        ELSE
C         denominator is ok - compute next iteration
          FRAC= (TCM-FIXCAP)/DENOM
        END IF
C
C       determine if these estimates are acceptable
        RCLOSE= FRAC- 1.0
C
        IF ( (ABS(RCLOSE) .GT. 0.01) .AND. (COUNT .LE. ITMAX) ) THEN
C         get ready for new iteration
C
          COUNT= COUNT+ 1
C
C         estimate solution phase value
          C= C*FRAC
        END IF
      IF ( (ABS(RCLOSE) .GT. 0.01) .AND. (COUNT .LE. ITMAX) ) GO TO 10
C
      IF (COUNT .GE. ITMAX) THEN
C       iterative freundlich solution did not converge
C       before reaching the iteration limit - error
        CALL OMSTD (DATIM)
        CALL OMSTI (LSNO)
        CHSTR= LAYID
        CALL OMSTC (I4,CHSTR1)
        DO 5 J= 1,5
C         put char*4 arg into local for equivalencing
          CHSTR4(J)= CMID(J)
  5     CONTINUE
        CALL OMSTC (I20,CHSTR2)
        CALL OMSTR (FRAC)
        CALL OMSTR (TCM)
        CALL OMSTR (X)
        CALL OMSTR (SOILM)
        CALL OMSTR (C)
        CALL OMSTR (MOISTM)
        SGRP= 1
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             ECNT)
      END IF
C     adjust c and x to account for tolerance in the iterative
C     process
      C= C*FRAC
      X= XFIX+ (X- XFIX)*FRAC
C
      RETURN
      END
C
C
C
      SUBROUTINE   SDFRAC
     I                   (SOSED,SLME,LSNO,DATIM,MESSU,MSGFL,
     M                    WCNT1,WCNT2,
     O                    FSD)
C
C     + + + PURPOSE + + +
C     Calculate the fraction of the surface layer that is eroding -
C     units are tons/acre-ivl for the amount eroded, sosed, and
C     tons/acre for the amount of surface layer soil, slme
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    DATIM(5),LSNO,MESSU,MSGFL,WCNT1,WCNT2
      REAL       FSD,SLME,SOSED
C
C     + + + ARGUMENT DEFINITIONS + + +
C     SOSED  - ???
C     SLME   - ???
C     LSNO   - land surface id number
C     DATIM  - date and time of day
C     MESSU  - ftn unit no. to be used for printout of messages
C     MSGFL  - fortran unit number of warning message file
C     WCNT1  - ???
C     WCNT2  - ???
C     FSD    - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    SCLU,SGRP
C
C     + + + EXTERNALS + + +
      EXTERNAL   OMSTI,OMSTD,OMSTR,OMSG
C
C     + + + END SPECIFICATIONS + + +
C
C     error/warn message cluster
      SCLU = 308
C
      FSD  = SOSED/SLME
C
      IF (FSD.GT.1.0) THEN
C       issue a warning that the amount of eroded soil/sediment
C       is more than the surface layer can supply, and that fsd
C       has been arbitrarily reduced to 1.0
        CALL OMSTD (DATIM)
        CALL OMSTI (LSNO)
        CALL OMSTR (FSD)
        CALL OMSTR (SOSED)
        CALL OMSTR (SLME)
        SGRP= 2
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             WCNT1)
        FSD= 1.0
      ELSE IF (FSD.LT.1.0E-05) THEN
C       issue a warning that the calculated value of fsd
C       was < 1/100000, so it has been set to zero to avoid
C       problems of continuity errors due to the use of single
C       precision variables
        CALL OMSTD (DATIM)
        CALL OMSTI (LSNO)
        CALL OMSTR (FSD)
        CALL OMSTR (SOSED)
        CALL OMSTR (SLME)
        SGRP= 3
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             WCNT2)
        FSD= 0.0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   SUBMOV
     I                   (UPCM,FLP,FLDP,FAO,
     M                    LSCM,ASCM,
     O                    SSCM)
C
C     + + + PURPOSE + + +
C     Move solutes with the water in the subsurface layers (lower
C     and groundwater layers) and update storages
C
C     + + + DUMMY ARGUMENTS + + +
      REAL       ASCM,FAO,FLDP,FLP,LSCM,SSCM(3),UPCM
C
C     + + + ARGUMENT DEFINITIONS + + +
C     UPCM   - ???
C     FLP    - ???
C     FLDP   - ???
C     FAO    - ???
C     LSCM   - ???
C     ASCM   - ???
C     SSCM   - ???
C
C     + + + LOCAL VARIABLES + + +
      REAL       AOCM,DUMMY,LDPCM,LPCM
C
C     + + + END SPECIFICATIONS + + +
C
C     units of fluxes are mass/area-ivl and storages are mass/area,
C     i.e. lbs/acre for english units and kg/ha for metric units
C
C     lower layer
C
C     add solute percolated (leached) from the upper layer storage
C     to the lower layer
      LSCM= LSCM+ UPCM
C
      DUMMY= FLP+ FLDP
C
      IF (LSCM.GT.1.0E-6 .AND. DUMMY.GT.0.0) THEN
C       there is sufficient solute in the lower layer storage that is to
C       be leached determine the amount percolated (leached) from the
C       lower layer storage to the active groundwater storage
        LPCM= LSCM*FLP
C
C       determine the amount percolated (leached) from the lower
C       layer storage to the deep (inactive) groundwater sink
        LDPCM= LSCM*FLDP
C
C       remove solutes leached from the lower layer
        LSCM= LSCM- (LPCM+LDPCM)
        IF (LSCM.LT.0.0) THEN
C         adjust, in case round-off has made storage negative
          LSCM= 0.0
        END IF
      ELSE
C       there is insufficient solute in the lower layer storage
C       that is leached
        LPCM = 0.0
        LDPCM= 0.0
      END IF
C
C     groundwater layer
C     inactive groundwater is lost from the system
C     add solute leached from the lower layer to the active
C     groundwater storage
      ASCM= ASCM+ LPCM
C
      IF (ASCM.GT.1.0E-6 .AND. FAO.GT.0.0) THEN
C       there is sufficient solute in the active groundwater storage
C       that is to be moved with water
C       determine solute in active groundwater (baseflow) outflow
        AOCM= ASCM*FAO
C
C       remove outflow solutes from storage
        ASCM= ASCM- AOCM
        IF (ASCM.LT.0.0) THEN
C         adjust, in case round-off has made storage negative
          ASCM= 0.0
        END IF
      ELSE
C       there is insufficient solute in the active groundwater
C       storage that is moved with water
        AOCM= 0.0
      END IF
C
C     assign fluxes to "permanent" array
      SSCM(1)= LPCM
      SSCM(2)= LDPCM
      SSCM(3)= AOCM
C
      RETURN
      END
C
C
C
      SUBROUTINE   SV
     I               (MOISTM,SOILM,TCM,XFIX,CMAX,XMAX,KF1,N1I,
     I                LSNO,MESSU,MSGFL,DATIM,
     I                ITMAX,CMID,LAYID,
     M                CMSU,ECNT,
     O                CMCY,CMAD)
C
C     + + + PURPOSE + + +
C     Calculate the adsorption/desorption of chemicals by the
C     single value freundlich method
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     ECNT,MSGFL,ITMAX,LSNO,MESSU,DATIM(5)
      REAL        CMAD,CMAX,CMCY,CMSU,KF1,MOISTM,
     $            N1I,SOILM,TCM,XFIX,XMAX
      CHARACTER*4 LAYID,CMID(5)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MOISTM - ???
C     SOILM  - ???
C     TCM    - ???
C     XFIX   - ???
C     CMAX   - ???
C     XMAX   - ???
C     KF1    - ???
C     N1I    - ???
C     LSNO   - land surface id number
C     MESSU  - ftn unit no. to be used for printout of messages
C     MSGFL  - fortran unit number of HSPF message file
C     ITMAX  - ???
C     CMID   - ???
C     LAYID  - ???
C     CMSU   - ???
C     ECNT   - ???
C     CMCY   - ???
C     CMAD   - ???
C     DATIM  - date and time of day
C
C     + + + LOCAL VARIABLES + + +
      REAL       C,FIXCAP,MAXAD,MAXSU,REMCM,X
C
C     + + + EXTERNALS + + +
      EXTERNAL   ITER
C
C     + + + END SPECIFICATIONS + + +
C
      IF (MOISTM.GE.0.001) THEN
C       there is sufficient moisture for adsorption/desorption to occur
C
C       determine the capacity of soil to fix the chemical in mass/area
        FIXCAP= XFIX*SOILM*1.0E-06
C
        IF (TCM.GT.FIXCAP) THEN
C         there is more chemical than the fixed capacity, so
C         determine where the surplus resides
C
C         calculate the maximum soluble and adsorbed chemical -
C         units are mass/area
          MAXSU= CMAX*MOISTM*1.0E-06
          MAXAD= XMAX*SOILM*1.0E-06
C
C         determine if maximum adsorption capacity and solubility
C         have been reached
          REMCM= TCM- MAXAD- MAXSU
C
          IF (REMCM .GE. 0.0) THEN
C           maximum adsorption capacity and solubilty have been
C           reached, so solution and adsorbed forms are at capacity;
C           the remaining chemical is considered to be in the
C           crystalline form
            CMAD= MAXAD
            CMSU= MAXSU
            CMCY= REMCM
          ELSE
C           total amount is less than amount needed to reach capacity.
C           therefore, no crystalline form exists and adsorption/
C           desorption amounts must be determined from
C           the freundlich isotherm
            CMCY= 0.0
C
C           make initial estimate of the freundlich value for chemical
C           concentration in solution(c) - units are ppm in solution
            IF (CMSU .GT. 0.0) THEN
C             use current concentration
              C= (CMSU*1.0E06)/MOISTM
            ELSE
C             use maximum
              C= CMAX
            END IF
C
C           find values on freundlich isotherm
            CALL ITER (TCM,MOISTM,SOILM,KF1,N1I,XFIX,ITMAX,CMID,
     I                 LAYID,LSNO,MESSU,MSGFL,DATIM,FIXCAP,
     M                 C,ECNT,
     O                 X)
C
C           convert the freundlich isotherm concentration
C           to mass/area units
            CMAD= X*SOILM*1.0E-06
            IF (CMAD.GT.TCM) CMAD= TCM
            CMSU= TCM-CMAD
          END IF
        ELSE
C         there is insufficient chemical to fullfill the fixed capacity
C         the fixed portion is part of the adsorbed phase
          CMAD= TCM
          CMCY= 0.0
          CMSU= 0.0
        END IF
      ELSE
C       insufficient moisture for adsorption/desorption to occur
        MAXAD= XMAX*SOILM*1.0E-06
        IF (TCM .GT. MAXAD) THEN
          CMAD= MAXAD
          CMCY= TCM-MAXAD
        ELSE
          CMAD= TCM
          CMCY= 0.0
        END IF
        CMSU= 0.0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   FIRORD
     I                   (TMP,MOISTM,KDS,KAD,THKDS,THKAD,
     I                    CMSU,CMAD,
     O                    ADS,DES)
C
C     + + + PURPOSE + + +
C     Calculate the adsorption and desorption fluxes using
C     temperature dependent first order kinetics
C     internal units for first order reaction rate parameters
C     (kds,kad) are per interval
C
C     + + + DUMMY ARGUMENTS + + +
      REAL       ADS,CMAD,CMSU,DES,KAD,KDS,MOISTM,THKAD,THKDS,TMP
C
C     + + + ARGUMENT DEFINITIONS + + +
C     TMP    - ???
C     MOISTM - ???
C     KDS    - ???
C     KAD    - ???
C     THKDS  - ???
C     THKAD  - ???
C     CMSU   - ???
C     CMAD   - ???
C     ADS    - ???
C     DES    - ???
C
C     + + + LOCAL VARIABLES + + +
      REAL       DIF35,KADK,KDSK
C
C     + + + END SPECIFICATIONS + + +
C
      IF (TMP.GE.0.0 .AND. MOISTM.GE.0.001) THEN
C       soil layer temperature in deg c is warm enough, and soil
C       moisture in mass/area is sufficient to adsorb/desorb
C
        IF (TMP .LT. 35.0) THEN
C         soil layer temperature is less than optimum,
C         modify inputted reaction rate parameter
C         decrease inputted reaction rate parameter by use of the
C         modified arrenhius equation
          DIF35= TMP- 35.0
          KDSK = KDS*THKDS**DIF35
          KADK = KAD*THKAD**DIF35
        ELSE
C         temperature is optimum,use inputted reaction rate parameter
          KDSK= KDS
          KADK= KAD
        END IF
C
C       calculate the actual adsorption and desorption fluxes - units
C       are mass/area-ivl
        ADS= CMSU*KADK
        DES= CMAD*KDSK
      ELSE
C       either soil temperature is too cold or the soil layer is
C       too dry, zero fluxes
        ADS= 0.0
        DES= 0.0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   SEDMOV
     I                   (FSD,
     M                    SSCM,
     O                    SDCM)
C
C     + + + PURPOSE + + +
C     Move the chemical on/with sediment from the land surface
C
C     + + + DUMMY ARGUMENTS + + +
      REAL       FSD,SDCM,SSCM
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FSD    - ???
C     SSCM   - ???
C     SDCM   - ???
C
C     + + + END SPECIFICATIONS + + +
C
      IF (SSCM .GT. 0.0) THEN
C       there is sufficient chemical in the surface storage to
C       be removed on or with the sediment
C
C       determine the amount of the surface layer chemical
C       removed on sediment - units are mass/area-ivl
        SDCM= SSCM*FSD
C
C       remove from the surface layer
        SSCM= SSCM- SDCM
      ELSE
C       there is insufficient chemical to be removed from the
C       surface storage on or with the sediment
        SDCM= 0.0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   CYLDPM
     I                    (MESSU,MSGFL,UUNITS,OUTLEV,NDAY,
     I                     SOILD,
     M                     ECOUNT,
     O                     WILTPT,NCRP,CRPDAT,CRPDAY,CRPFRC)
C
C     + + + PURPOSE + + +
C     Read tables needed for yield-based plant uptake and compute
C     fractions of each month's uptake for each crop.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER MESSU,MSGFL,UUNITS,OUTLEV,NDAY(12),ECOUNT,NCRP,
     $        CRPDAT(4,3),CRPDAY(13,3)
      REAL    SOILD(4),WILTPT(4),CRPFRC(13,3)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSU  - ftn unit no. to be used for printout of messages
C     MSGFL  - ftn unit no. of message WDM file
C     UUNITS - system of units   1-english, 2-metric
C     OUTLEV - run interpreter output level
C     NDAY   - number of days in each month
C     SOILD  - depth of soil layer
C     ECOUNT - count of errors in run interpreter
C     WILTPT - wilting point for each soil layer
C     NCRP   - number of crops per year, max 3
C     CRPDAT - planting and harvesting dates (month/day) for each crop
C     CRPDAY - number of days in a month that a crop is growing
C     CRPFRC - fraction of monthly plant uptake for each crop
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,I0,LNDAY(12),NC,MTH,TCDAY(13)
      REAL      R0
C
C     + + + INTRINSICS + + +
      INTRINSIC FLOAT
C
C     + + + EXTERNALS + + +
      EXTERNAL ZIPI,ZIPR,COPYI,CKCRDT,SOLDA2,CROPDT
C
C     + + + DATA INITIALIZATIONS + + +
      DATA I0/0/
      DATA R0/0.0/
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' PARAMETERS TO BE USED FOR YIELD-BASED PLANT UPTAKE')
C
C     + + + END SPECIFICATIONS + + +
C
      IF (OUTLEV .GT. 2) THEN
C       processing yield-based parameters message
        WRITE (MESSU,2000)
      END IF
C
C     get wilting points - table-type soil-data2
      CALL SOLDA2 (UUNITS,
     O             WILTPT)
C     convert from in/in or in/cm to inches
      DO 10 I= 1, 4
        WILTPT(I)= WILTPT(I)* SOILD(I)
 10   CONTINUE
C
C     get cropping data - table-type crop-dates
      CALL CROPDT (MESSU,MSGFL,UUNITS,
     M             ECOUNT,
     O             NCRP,CRPDAT)
C
C     calculate monthly crop fractions of plant uptake
C
C     initialize variables
      I= 12
      CALL COPYI (I,NDAY,
     O            LNDAY)
      I= 13
      CALL ZIPI (I,I0,
     O           TCDAY)
      LNDAY(2)= 28
      I= 39
      CALL ZIPI (I,I0,
     O           CRPDAY)
      CALL ZIPR (I,R0,
     O           CRPFRC)
C
C     compute crop days in month
      DO 80 NC= 1, NCRP
        MTH= CRPDAT(1,NC)- 1
C
C       do-until loop - start with planting month
 70     CONTINUE
          MTH= MTH+ 1
          IF (MTH .GT. 12) THEN
C           wrap around end of year
            MTH= 1
          END IF
C
          IF (MTH .EQ. CRPDAT(3,NC)) THEN
C           month is harvest month - only go to end of season
            CRPDAY(MTH,NC)= CRPDAT(4,NC)
            IF (MTH .EQ. 2) THEN
C             compute leap year february as month 13
              CRPDAY(13,NC)= CRPDAY(2,NC)
            END IF
          ELSE
C           season goes to end of month
            CRPDAY(MTH,NC)= LNDAY(MTH)
            IF (MTH .EQ. 2) THEN
C             compute leap year february as month 13
              CRPDAY(13,NC)= 29
            END IF
          END IF
          IF (MTH .EQ. CRPDAT(1,NC)) THEN
C           month is planting month - subtract days before planting
            CRPDAY(MTH,NC)= CRPDAY(MTH,NC)- CRPDAT(2,NC)+ 1
            IF (MTH .EQ. 2) THEN
C             compute leap year february as month 13
              CRPDAY(13,NC)= CRPDAY(2,NC)- CRPDAT(2,NC)+ 1
            END IF
          END IF
C
C         accumulate total crop days per month
          TCDAY(MTH)= TCDAY(MTH)+ CRPDAY(MTH,NC)
          IF (MTH .EQ. 2) THEN
C           compute leap year february as month 13
            TCDAY(13)= TCDAY(13)+ CRPDAY(13,NC)
          END IF
C
C       end of do-until loop - stop when reach harvest month
        IF (MTH .NE. CRPDAT(3,NC)) GO TO 70
 80   CONTINUE
C
C     compute fractions
      DO 100 NC= 1, NCRP
        DO 90 MTH= 1, 13
          IF (TCDAY(MTH) .EQ. 0) THEN
C           no uptake for month
            CRPFRC(MTH,NC)= 0.0
          ELSE
C           compute fraction
            CRPFRC(MTH,NC)= FLOAT (CRPDAY(MTH,NC)) / FLOAT (TCDAY(MTH))
          END IF
 90     CONTINUE
 100  CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   CKCRDT
     I                    (DATE1,DATE2,
     O                     FLAG)
C
C     + + + PURPOSE + + +
C     Check two dates in month/day format to determine order.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER DATE1(2),DATE2(2),FLAG
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DATE1  - month and day of first date
C     DATE2  - month and day of second date
C     FLAG   - return value:
C              1 - first date is before second
C              0 - first date is same as second
C             -1 - first date is after second
C
C     + + + LOCAL VARIABLES + + +
      INTEGER MON1,MON2,DAY1,DAY2
C
C     + + + END SPECIFICATIONS + + +
C
      MON1= DATE1(1)
      MON2= DATE2(1)
      DAY1= DATE1(2)
      DAY2= DATE2(2)
C
      IF (MON1 .LT. MON2) THEN
C       first date is before
        FLAG= 1
      ELSE IF (MON1 .GT. MON2) THEN
C       first date is before
        FLAG= -1
      ELSE
C       same month
        IF (DAY1 .LT. DAY2) THEN
C         first date is before
          FLAG= 1
        ELSE IF (DAY1 .GT. DAY2) THEN
C         first date is before
          FLAG= -1
        ELSE
C         same day
          FLAG= 0
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   YUPTGT
     I                    (DELT60,YR,MON,DAY,NDAYS,NCRP,CRPDAT,CRPDAY,
     I                     CRPFRC,TUPTGT,UPTFM,SUPTM,UUPTM,
     I                     LUPTM,AUPTM,
     M                     SPUTG,UPUTG,LPUTG,APUTG,SDFC,UDFC,LDFC,ADFC,
     M                     TDFC,
     O                     SUPTG,UUPTG,LUPTG,AUPTG)
C
C     + + + PURPOSE + + +
C     Calculates daily yield-based plant uptake targets for each soil
C     layer based on user-specified monthly fractions of the annual
C     target and a trapezoidal function to interpolate between months.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER YR,MON,DAY,NDAYS,NCRP,CRPDAT(4,3),CRPDAY(13,3)
      REAL    DELT60,CRPFRC(13,3),TUPTGT,UPTFM(12),SUPTM(12),UUPTM(12),
     $        LUPTM(12),AUPTM(12),SPUTG,UPUTG,LPUTG,APUTG,SUPTG,UUPTG,
     $        LUPTG,AUPTG,SDFC,UDFC,LDFC,ADFC,TDFC
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DELT60 - simulation time interval in hours
C     YR     - current year
C     MON    - current month
C     DAY    - current day of month
C     NDAYS  - number of days in current month
C     NCRP   - number of crops per year
C     CRPDAT - month/day of planting and harvesting for each crop
C     CRPDAY - number of days in month that each crop is growing
C     CRPFRC - fraction of monthly target plant uptake for each crop
C     TUPTGT - total annual target plant uptake
C     UPTFM  - fraction of annual target plant uptake applied to each month
C     SUPTM  - fraction of monthly target plant uptake from surface soil layer
C     UUPTM  - fraction of monthly target plant uptake from upper soil layer
C     LUPTM  - fraction of monthly target plant uptake from lower soil layer
C     AUPTM  - fraction of monthly target plant uptake from active groundwater
C              layer
C     SPUTG  - daily target plant uptake on last day of previous month for
C              surface soil layer
C     UPUTG  - daily target plant uptake on last day of previous month for
C              upper soil layer
C     LPUTG  - daily target plant uptake on last day of previous month for
C              lower soil layer
C     APUTG  - daily target plant uptake on last day of previous month for
C              active groundwater layer
C     SUPTG  - current interval target plant uptake from surface soil layer
C     UUPTG  - current interval target plant uptake from upper soil layer
C     LUPTG  - current interval target plant uptake from lower soil layer
C     AUPTG  - current interval target plant uptake from active groundwater
C              layer
C     SDFC   - cumulative plant uptake deficit from surface soil layer
C     UDFC   - cumulative plant uptake deficit from upper soil layer
C     LDFC   - cumulative plant uptake deficit from lower soil layer
C     ADFC   - cumulative plant uptake deficit from active groundwater layer
C     TDFC   - cumulative plant uptake deficit from all layers
C
C     + + + LOCAL VARIABLES + + +
      INTEGER ICROP,LPYRFG,CURDAY
C
C     + + + EXTERNALS + + +
      EXTERNAL CRPSEL,LPYEAR,YUPLAY
C
C     + + + END SPECIFICATIONS + + +
C
C
C     determine current crop
      CALL CRPSEL (MON,DAY,CRPDAT,NCRP,
     O             ICROP)
C
      IF (ICROP .EQ. 0) THEN
C       no active crop - reset previous and current targets and deficits
        SPUTG= 0.0
        UPUTG= 0.0
        LPUTG= 0.0
        APUTG= 0.0
        SUPTG= 0.0
        UUPTG= 0.0
        LUPTG= 0.0
        AUPTG= 0.0
        SDFC= 0.0
        UDFC= 0.0
        LDFC= 0.0
        ADFC= 0.0
        TDFC= 0.0
      ELSE
C       compute interval targets
C
C       find current day of season this month
        IF (MON .EQ. CRPDAT(1,ICROP)) THEN
C         planting took place earlier this month
          CURDAY= DAY- CRPDAT(2,ICROP)+ 1
        ELSE
C         season began before current month
          CURDAY= DAY
        END IF
C
C       determine if current year is a leap year
        CALL LPYEAR (YR,
     O               LPYRFG)
C
C       find target for surface layer
        CALL YUPLAY (DELT60,LPYRFG,MON,DAY,CURDAY,NDAYS,ICROP,CRPDAT,
     I               CRPDAY,CRPFRC,TUPTGT,UPTFM,SUPTM,
     M               SPUTG,
     O               SUPTG)
C       find target for upper layer
        CALL YUPLAY (DELT60,LPYRFG,MON,DAY,CURDAY,NDAYS,ICROP,CRPDAT,
     I               CRPDAY,CRPFRC,TUPTGT,UPTFM,UUPTM,
     M               UPUTG,
     O               UUPTG)
C       find target for lower layer
        CALL YUPLAY (DELT60,LPYRFG,MON,DAY,CURDAY,NDAYS,ICROP,CRPDAT,
     I               CRPDAY,CRPFRC,TUPTGT,UPTFM,LUPTM,
     M               LPUTG,
     O               LUPTG)
C       find target for active groundwater layer
        CALL YUPLAY (DELT60,LPYRFG,MON,DAY,CURDAY,NDAYS,ICROP,CRPDAT,
     I               CRPDAY,CRPFRC,TUPTGT,UPTFM,AUPTM,
     M               APUTG,
     O               AUPTG)
C
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   YUPLAY
     I                    (DELT60,LPYRFG,MON,DAY,CURDAY,NDAYS,ICROP,
     I                     CRPDAT,CRPDAY,CRPFRC,TUPTGT,UPTFM,
     I                     UPTM,
     M                     PUTG,
     O                     UPTG)
C
C     + + + PURPOSE + + +
C     Calculates daily yield-based plant uptake targets for a soil
C     layer based on user-specified monthly fractions of the annual
C     target and a trapezoidal function to interpolate between months.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER LPYRFG,MON,DAY,CURDAY,NDAYS,ICROP,CRPDAT(4,3),
     $        CRPDAY(13,3)
      REAL    DELT60,CRPFRC(13,3),TUPTGT,UPTFM(12),UPTM(12),PUTG,UPTG
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DELT60 - simulation time interval in hours
C     MON    - current month
C     DAY    - current day of month
C     CURDAY - current day of season this month
C     NDAYS  - number of days in current month
C     ICROP  - index of current crop
C     CRPDAT - month/day of planting and harvesting for each crop
C     CRPDAY - number of days in month that each crop is growing
C     CRPFRC - fraction of monthly target plant uptake for each crop
C     TUPTGT - total annual target plant uptake
C     UPTFM  - fraction of annual target plant uptake applied to each month
C     UPTM   - fraction of monthly target plant uptake from soil layer
C     PUTG   - daily target plant uptake on last day of previous month for
C              soil layer
C     UPTG   - current interval target plant uptake from soil layer
C
C     + + + LOCAL VARIABLES + + +
      INTEGER MTH,LNDAYS,SUMDAY(31)
      REAL    MONTGT,DELTGT
C
C     + + + DATA INITIALIZATIONS + + +
      DATA SUMDAY/1,3,6,10,15,21,28,36,45,55,66,78,91,105,120,136,153,
     #        171,190,210,231,253,276,300,325,351,378,406,435,465,496/
C
C     + + + END SPECIFICATIONS + + +
C
C     calculate monthly target
      MONTGT= TUPTGT* UPTFM(MON)* UPTM(MON)* CRPFRC(MON,ICROP)
C
C     find daily change in monthly target
      IF ( (LPYRFG .EQ. 1) .AND. (MON .EQ. 2) ) THEN
C       use month 13 for 29-day february
        MTH= 13
        LNDAYS= 29
      ELSE
C       use current month
        MTH= MON
        LNDAYS= NDAYS
      END IF
      DELTGT= (MONTGT- PUTG*CRPDAY(MTH,ICROP)) /
     #         SUMDAY(CRPDAY(MTH,ICROP))
C
C     find daily target
      UPTG= PUTG+ CURDAY*DELTGT
      IF (UPTG .LT. 0.0) THEN
C       cut off uptake
        UPTG= 0.0
      END IF
C
C     update daily target at end of previous month
      IF ( (MON .EQ. CRPDAT(3,ICROP)) .AND.
     $     (DAY .EQ. CRPDAT(4,ICROP)) ) THEN
C       today is harvest day - reset previous target to zero
        PUTG= 0.0
      ELSE IF (DAY .EQ. LNDAYS) THEN
C       today is last day of month - this becomes previous target
        PUTG= UPTG
      END IF
C
C     convert from daily target to interval target
      UPTG= UPTG* DELT60/24.0
C
      RETURN
      END
C
C
C
      SUBROUTINE   YUPINI
     I                    (DELT60,YR,MON,DAY,NDAY,NCRP,CRPDAT,CRPDAY,
     I                     CRPFRC,TUPTGT,UPTFM,SUPTM,UUPTM,LUPTM,AUPTM,
     O                     SPUTG,UPUTG,LPUTG,APUTG)
C
C     + + + PURPOSE + + +
C     Calculate initial values of the daily plant uptake target on
C     last day of previous month.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER YR,MON,DAY,NDAY(12),NCRP,CRPDAT(4,3),CRPDAY(13,3)
      REAL    DELT60,CRPFRC(13,3),TUPTGT,UPTFM(12),SUPTM(12),UUPTM(12),
     $        LUPTM(12),AUPTM(12),SPUTG,UPUTG,LPUTG,APUTG
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DELT60 - simulation time interval in hours
C     YR     - current year
C     MON    - current month
C     DAY    - current day of month
C     NDAY   - number of days in each month
C     NCRP   - number of crops per year
C     CRPDAT - month/day of planting and harvesting for each crop
C     CRPDAY - number of days in month that each crop is growing
C     CRPFRC - fraction of monthly target plant uptake for each crop
C     TUPTGT - total annual target plant uptake
C     UPTFM  - fraction of annual target plant uptake applied to each month
C     SUPTM  - fraction of monthly target plant uptake from surface soil layer
C     UUPTM  - fraction of monthly target plant uptake from upper soil layer
C     LUPTM  - fraction of monthly target plant uptake from lower soil layer
C     AUPTM  - fraction of monthly target plant uptake from active groundwater
C              layer
C     SPUTG  - daily target plant uptake on last day of previous month for
C              surface soil layer
C     UPUTG  - daily target plant uptake on last day of previous month for
C              upper soil layer
C     LPUTG  - daily target plant uptake on last day of previous month for
C              lower soil layer
C     APUTG  - daily target plant uptake on last day of previous month for
C              active groundwater layer
C
C     + + + LOCAL VARIABLES + + +
      INTEGER ICROP,YEAR,MTH,LDAY,LPYRFG,PMON,PDAY
      REAL    SUPTG,UUPTG,LUPTG,AUPTG,SDFC,UDFC,LDFC,ADFC,TDFC
C
C     + + + EXTERNALS + + +
      EXTERNAL CRPSEL,YUPTGT,LPYEAR
C
C     + + + END SPECIFICATIONS + + +
C
C     deficits and previous targets are all zero during off-season
      SPUTG= 0.0
      UPUTG= 0.0
      LPUTG= 0.0
      APUTG= 0.0
      SDFC= 0.0
      UDFC= 0.0
      LDFC= 0.0
      ADFC= 0.0
      TDFC= 0.0
C
C     determine which crop is in effect at beginning of run
      CALL CRPSEL (MON,DAY,CRPDAT,NCRP,
     O             ICROP)
C
      IF (ICROP .GT. 0) THEN
C       run starts during a crop season - trace uptake targets
C       from beginning of season
C
        PMON= CRPDAT(1,ICROP)
        PDAY= CRPDAT(2,ICROP)
        IF ( (MON .GT. PMON) .OR.
     $       ( (MON .EQ. PMON) .AND. (DAY .GE. PDAY) ) ) THEN
C         season began same calendar year
          YEAR= YR
        ELSE
C         season began previous calendar year
          YEAR= YR- 1
        END IF
     
        MTH= PMON- 1
C       determine if current year is a leap year
        CALL LPYEAR (YEAR,
     O               LPYRFG)
C       do-until loop
 80     CONTINUE
          MTH= MTH+ 1
          IF (MTH .GT. 12) THEN
C           wrap around end of year
            MTH= 1
            YEAR= YEAR+ 1
C           determine if current year is a leap year
            CALL LPYEAR (YEAR,
     O                   LPYRFG)
          END IF
          IF (MTH .NE. MON) THEN
C           calculate interval targets for last day of month
            IF (MTH .EQ. 2) THEN
C             february is special case
              IF (LPYRFG .EQ. 1) THEN
C               long february
                LDAY= 29
              ELSE
C               regular february
                LDAY= 28
              END IF
            ELSE
C             use natural end of month
              LDAY= NDAY(MTH)
            END IF
            CALL YUPTGT (DELT60,YEAR,MTH,LDAY,LDAY,NCRP,CRPDAT,CRPDAY,
     I                   CRPFRC,TUPTGT,UPTFM,SUPTM,UUPTM,LUPTM,AUPTM,
     M                   SPUTG,UPUTG,LPUTG,APUTG,SDFC,UDFC,LDFC,ADFC,
     M                   TDFC,
     O                   SUPTG,UUPTG,LUPTG,AUPTG)
          END IF
C       end do-until
        IF (MTH .NE. MON) GO TO 80
C
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   CRPSEL
     I                    (MON,DAY,CRPDAT,NCRP,
     O                     ICROP)
C
C     + + + PURPOSE + + +
C     Determines which, if any, of the current crop seasons
C     includes the current day and month.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER MON,DAY,CRPDAT(4,3),NCRP,ICROP
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MON    - current month
C     DAY    - current day of month
C     CRPDAT - month/day of planting and harvesting for each crop
C     NCRP   - number of crops per year
C     ICROP  - index of current crop; or zero if none is current
C
C     + + + LOCAL VARIABLES + + +
      INTEGER I,PMON,PDAY,HMON,HDAY
C
C     + + + END SPECIFICATIONS + + +
C
      ICROP= 0
C
      IF (NCRP .GT. 0) THEN
C       check which crop is active
        I= 0
 10     CONTINUE
          I= I+ 1
          PMON= CRPDAT(1,I)
          PDAY= CRPDAT(2,I)
          HMON= CRPDAT(3,I)
          HDAY= CRPDAT(4,I)
          IF ( (PMON .LT. HMON) .OR.
     $         ( (PMON .EQ. HMON) .AND.
     $           (PDAY .LT. HDAY) ) ) THEN
C           season does not cross year boundary
            IF ( (MON .GT. PMON) .AND. (MON .LT. HMON) ) THEN
C             whole current month is in season
              ICROP= I
            ELSE IF ( (MON .EQ. PMON) .AND. (MON .EQ. HMON) ) THEN
C             whole season is in current month
              IF ( (DAY .GE. PDAY) .AND. (DAY .LE. HDAY) ) THEN
C               current day is in season
                ICROP= I
              END IF
            ELSE IF ( (MON .EQ. PMON) .AND. (DAY .GE. PDAY) ) THEN
C             current day is after planting this month
              ICROP= I
            ELSE IF ( (MON .EQ. HMON) .AND. (DAY .LE. HDAY) ) THEN
C             current day is before harvesting this month
              ICROP= I
            END IF
          ELSE
C           season crosses year boundary
            IF ( (MON .GT. PMON) .OR. (MON .LT. HMON) ) THEN
C             whole current month is in season
              ICROP= I
            ELSE IF ( (MON .EQ. PMON) .AND. (MON .EQ. HMON) ) THEN
C             whole off-season is in current month
              IF ( (DAY .GE. PDAY) .OR. (DAY .LE. HDAY) ) THEN
C               current day is in season
                ICROP= I
              END IF
            ELSE IF ( (MON .EQ. PMON) .AND. (DAY .GE. PDAY) ) THEN
C             current day is after planting this month
              ICROP= I
            ELSE IF ( (MON .EQ. HMON) .AND. (DAY .LE. HDAY) ) THEN
C             current day is before harvesting this month
              ICROP= I
            END IF
          END IF
C       end do-until loop
        IF ( (ICROP .EQ. 0) .AND. (I .LT. NCRP) ) GO TO 10
C
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   SOLDA2
     I                   (UUNITS,
     O                    WILTPT)
C
C     + + + PURPOSE + + +
C     Get soil wilting points - table-type soil-data2
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   UUNITS
      REAL      WILTPT(4)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     UUNITS - system of units   1-english, 2-metric
C     WILTPT - wilting point for each soil layer
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   TBNO,TBSB,NVAL
C
C     + + + EXTERNALS + + +
      EXTERNAL  RTABLE
C
C     + + + END SPECIFICATIONS + + +
C
      TBNO= 28
      TBSB= 1
      NVAL= 4
      CALL RTABLE (TBNO,TBSB,NVAL,UUNITS,
     M             WILTPT)
C
      RETURN
      END
C
C
C
      SUBROUTINE   SOLDA3
     I                   (UUNITS,
     O                    FDCAP)
C
C     + + + PURPOSE + + +
C     Get soil field capacities - table-type soil-data3
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   UUNITS
      REAL      FDCAP(4)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     UUNITS - system of units   1-english, 2-metric
C     FDCAP  - field capacity for each soil layer
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   TBNO,TBSB,NVAL
C
C     + + + EXTERNALS + + +
      EXTERNAL  RTABLE
C
C     + + + END SPECIFICATIONS + + +
C
      TBNO= 29
      TBSB= 1
      NVAL= 4
      CALL RTABLE (TBNO,TBSB,NVAL,UUNITS,
     M             FDCAP)
C
      RETURN
      END
C
C
C
      SUBROUTINE   CROPDT
     I                    (MESSU,MSGFL,UUNITS,
     M                     ECOUNT,
     O                     NCRP,CRPDAT)
C
C     + + + PURPOSE + + +
C     Read dates for cropping seasons
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER MESSU,MSGFL,UUNITS,ECOUNT,NCRP,CRPDAT(4,3)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSU  - ftn unit no. to be used for printout of messages
C     MSGFL  - ftn unit no. of message WDM file
C     UUNITS - system of units   1-english, 2-metric
C     ECOUNT - count of errors in run interpreter
C     NCRP   - number of crops per year, max 3
C     CRPDAT - planting and harvesting dates (month/day) for each crop
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   SCLU,SGRP,TBNO,TBSB,NVAL,IVAL(13),I,J,JJ,WRAPFG,LAPFG,
     $          PFG,HFG
C
C     + + + EXTERNALS + + +
      EXTERNAL ITABLE,CKCRDT,OMSTI,OMSG
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU= 308
C
C     get cropping data - table-type crop-dates
      TBNO= 30
      TBSB= 1
      NVAL= 13
      CALL ITABLE (TBNO,TBSB,NVAL,UUNITS,
     M             IVAL)
      NCRP= IVAL(1)
      DO 20 J= 1, 3
        DO 10 I= 1, 4
          CRPDAT(I,J)= IVAL(4*(J-1)+I+1)
 10     CONTINUE
 20   CONTINUE
C
C     check crop dates for consistency - no overlaps
      DO 50 J= 1, NCRP
        CALL CKCRDT (CRPDAT(1,J),CRPDAT(3,J),
     O               WRAPFG)
C       check for overlap with later crops
        DO 40 JJ= J+1, NCRP
          LAPFG= 0
C         check both planting and harvesting dates
          DO 30 I= 1, 3, 2
            CALL CKCRDT (CRPDAT(I,JJ),CRPDAT(1,J),
     O                   PFG)
            CALL CKCRDT (CRPDAT(3,J),CRPDAT(I,JJ),
     O                   HFG)
            IF ( (PFG .EQ. 0) .OR. (HFG .EQ. 0) ) THEN
C             one or more terminal dates coincides
              LAPFG= 1
            ELSE IF (WRAPFG .NE. -1) THEN
C             season doesn't wrap around end of year
              IF ( (PFG .EQ. -1) .AND. (HFG .EQ. -1) ) THEN
C               terminal date is inside season
                LAPFG= 1
              END IF
            ELSE IF (WRAPFG .EQ. -1) THEN
C             season does wrap around end of year
              IF ( (PFG .EQ.-1) .OR. (HFG .EQ.-1) ) THEN
C               terminal date is inside season
                LAPFG= 1
              END IF
            END IF
 30       CONTINUE
C
          IF (LAPFG .EQ. 1) THEN
C           error - two seasons overlap
            CALL OMSTI (J)
            CALL OMSTI (CRPDAT(1,J))
            CALL OMSTI (CRPDAT(2,J))
            CALL OMSTI (CRPDAT(3,J))
            CALL OMSTI (CRPDAT(4,J))
            CALL OMSTI (JJ)
            CALL OMSTI (CRPDAT(1,JJ))
            CALL OMSTI (CRPDAT(2,JJ))
            CALL OMSTI (CRPDAT(3,JJ))
            CALL OMSTI (CRPDAT(4,JJ))
            SGRP= 5
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 ECOUNT)
          END IF
 40     CONTINUE
 50   CONTINUE
C
      RETURN
      END
