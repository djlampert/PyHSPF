C
C
C
      SUBROUTINE   PPLANK
C
C     + + + PURPOSE + + +
C     Process input for the plank section of the rchres application
C     module
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PLANK1 + + +
      INCLUDE    'crhpl.inc'
      INCLUDE    'crin2.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,I1,I2,I4,J,SCLU,SGRP,N,RETCOD,IVAL(9)
      REAL       RVAL(7),R0,HRPYR,LSNH4(3),LSPO4(3)
      PARAMETER  (HRPYR = 8760.0)
C
C     + + + EXTERNALS + + +
      EXTERNAL   RTABLE,ITABLE,OMSG,ZIPI,OMSTI
      EXTERNAL   ZIPR,MDATBL,PKSUMS
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' PROCESSING INPUT FOR SECTION PLANK')
 2030 FORMAT (/,' FINISHED PROCESSING INPUT FOR SECTION PLANK')
C
C     + + + END SPECIFICATIONS + + +
C
      IF (OUTLEV.GT.1) THEN
        WRITE (MESSU,2000)
      END IF
C
      SCLU= 349
      I1  = 1
      R0 = 0.0
C
C     initialize month-data input
      I= 36
      CALL ZIPR (I,R0,
     O           PLAFXM)
      CALL ZIPR (I,R0,
     O           PLACNM)
C
C     initialize atmospheric deposition fluxes
      I= 15
      CALL ZIPR (I,R0,
     O           PKCF3)
      CALL ZIPR (I,R0,
     O           PKCF4)
C
      I= 4
      J= 0
      CALL ZIPI(I,J,PKECNT)
C
C     flags - table-type plnk-flgs
      I2= 95
      I4=  9
      CALL ITABLE
     I             (I2,I1,I4,UUNITS,
     M              PKFG)
C
      IF (ZOOFG.EQ.1.AND.PHYFG.EQ.0) THEN
C       error - zooplankton cannot be simulated without phytoplankton
        SGRP = 1
        CALL OMSTI (RCHNO)
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             ECOUNT)
      END IF
C
      IF (NSFG.EQ.1.AND.TAMFG.EQ.0) THEN
C       error - ammonia cannot be included in n supply if it is not
C       being simulated
        SGRP = 2
        CALL OMSTI (RCHNO)
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             ECOUNT)
      END IF
C
      IF (PO4FG.EQ.0) THEN
C       error - phosphate must be simulated if plankton are being
C       simulated
        SGRP = 3
        CALL OMSTI (RCHNO)
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             ECOUNT)
      END IF
C
      IF (BALFG .EQ. 2) THEN
C       user has selected multiple species with more complex kinetics
C
C       additional benthic algae flags - table-type BENAL-FLAG
        I2= 96
        I4=  5
        CALL ITABLE
     I              (I2,I1,I4,UUNITS,
     M               IVAL)
        NUMBAL= IVAL(1)
        BINVFG= IVAL(2)
        DO 5 I= 1, 4
          BFIXFG(I)= IVAL(I+ 2)
 5      CONTINUE
      ELSE
C       single species or none
        NUMBAL= BALFG
      END IF
C
C     atmospheric deposition flags - table-type plnk-ad-flgs
      I2= 97
      I4=  6
      CALL ITABLE
     I             (I2,I1,I4,UUNITS,
     M              PLADFG)
C
C     read in month-data tables where necessary
      DO 30 J= 1, 3
        N= 2*(J- 1)+ 1
        IF (PLADFG(N) .GT. 0) THEN
C         monthly flux must be read
          CALL MDATBL
     I                (PLADFG(N),
     O                 PLAFXM(1,J),RETCOD)
C         convert units to internal - not done by MDATBL
          IF (UUNITS .EQ. 1) THEN
C           convert from lb/ac.day to mg.ft3/l.ft2.ivl
            DO 10 I= 1, 12
              PLAFXM(I,J)= PLAFXM(I,J)*0.3677*DELT60/24.0
 10         CONTINUE
          ELSE IF (UUNITS .EQ. 2) THEN
C           convert from kg/ha.day to mg.m3/l.m2.ivl
            DO 20 I= 1, 12
              PLAFXM(I,J)= PLAFXM(I,J)*0.1*DELT60/24.0
 20         CONTINUE
          END IF
        END IF
        IF (PLADFG(N+1) .GT. 0) THEN
C         monthly ppn conc must be read
          CALL MDATBL
     I                (PLADFG(N+1),
     O                 PLACNM(1,J),RETCOD)
        END IF
 30   CONTINUE
C
      IF (HTFG.EQ.0) THEN
C       fraction of surface exposed - table-type surf-exposed
        I2= 98
        I4=  1
        CALL RTABLE
     I               (I2,I1,I4,UUNITS,
     M                RVAL)
        CFSAEX= RVAL(1)
      END IF
C
C     general parameters - group 1.  table-type plnk-parm1
      I2= 99
      I4=  7
      CALL RTABLE
     I             (I2,I1,I4,UUNITS,
     M              PKPM1)
C
C     convert max growth rate to units 1/ivl
      MALGR= MALGR*DELT60
C
C     compute derived conversion factors
      CVBC  = BPCNTC/100.0
      CVNRBO= NONREF*CVBO
      CVPB  = 31.0/(1000.*CVBP)
      CVBCL = 31.0*RATCLP/CVPB
C
C     general parameters - group 2.  table-type plnk-parm2
      I2= 100
      I4=  7
      CALL RTABLE
     I             (I2,I1,I4,UUNITS,
     M              PKPM2)
C
C     general parameters - group 3.  table-type plnk-parm3
      I2= 101
      I4=  6
      CALL RTABLE
     I             (I2,I1,I4,UUNITS,
     M              PKPM3)
C
C     convert rates from 1/hr to 1/ivl
      DO 40 I= 1,4
        PKPM3(I)= PKPM3(I)*DELT60
 40   CONTINUE
C
C     general parameters - group 4.  table-type plnk-parm4
      I2= 102
      I4=  5
      CALL RTABLE
     I             (I2,I1,I4,UUNITS,
     M              PKPM12)
C
C     phytoplankton-specific parms - table-type phyto-parm
C
C     this table must always be input so that REFSET is read
C
      I2= 103
      I4=  6
      CALL RTABLE
     I            (I2,I1,I4,UUNITS,
     M             PKPM4)
C
C     change settling rates to units of 1/ivl
      PHYSET= PHYSET*DELT60
      REFSET= REFSET*DELT60
C
      IF (PHYFG.EQ.1) THEN
C       phytoplankton on - zooplankton may be simulated  
        IF (ZOOFG.EQ.1) THEN
C         zooplankton-specific parameters.  table-type zoo-parm1
          I2= 104
          I4=  5
          CALL RTABLE
     I                 (I2,I1,I4,UUNITS,
     M                 PKPM5)
C
C         convert rates from 1/hr to 1/ivl
          DO 50 J= 1,5
            PKPM5(J)= PKPM5(J)*DELT60
 50       CONTINUE
C
C         second group of zoo parameters.  table-type zoo-parm2
          I2= 105
          I4=  4
          CALL RTABLE
     I                 (I2,I1,I4,UUNITS,
     M                  PKPM6)
C
        END IF
      END IF
C
      IF (BALFG.GE.1) THEN
C       benthic algae-specific parms
C
C       table-type benal-parm
        I2= 106
        I4=  7
        CALL RTABLE
     I               (I2,I1,I4,UUNITS,
     M                PKPM7)
C       convert maximum benthic algae to micromoles of phosphorus
        MBAL= MBAL/CVPB
        MINBAL= MINBAL/CVPB
C
        IF (BALFG .EQ. 2) THEN
C         user has selected multiple species with more complex kinetics
C
          DO 55 I= 1, NUMBAL
C
C           species-specific growth parms - table type benal-grow
            I2= 107
            I4=   7
            CALL RTABLE
     I                   (I2,I,I4,UUNITS,
     M                    RVAL)
            MBALGR(I)= RVAL(1)*DELT60
            TCBALG(I)= RVAL(2)
            CMMNB(I)=  RVAL(3)
            CMMPB(I)=  RVAL(4)
            CMMD1(I)=  RVAL(5)
            CMMD2(I)=  RVAL(6)
            CSLIT(I)=  RVAL(7)
C
C           species-specific resp and scour parms - table type benal-resscr
            I2= 108
            I4=   5
            CALL RTABLE
     I                  (I2,I,I4,UUNITS,
     M                   RVAL)
            BALR20(I)= RVAL(1)*DELT60
            TCBALR(I)= RVAL(2)
            CSLOF1(I)= RVAL(3)*DELT60
            CSLOF2(I)= RVAL(4)
            GRORES(I)= RVAL(5)
 55       CONTINUE
C
C         grazing and disturbance parms - table-type benal-graze
          I2= 109
          I4=   4
          CALL RTABLE
     I                (I2,I1,I4,UUNITS,
     M                 PKPM8)
          CREMVL= (CREMVL/CVPB) / HRPYR * DELT60
C
          IF (SDLTFG.EQ.2) THEN
C           turbidity regression parms - table-type benal-light
            I2= 110
            I4=   4
            CALL RTABLE
     I                  (I2,I1,I4,UUNITS,
     M                   PKPM9)
          END IF
C
          IF (BINVFG .EQ. 3) THEN
C           monthly benthic invertebrate density - table-type mon-binv
            I2= 115
            I4= 12
            CALL RTABLE
     I                  (I2,I1,I4,UUNITS,
     M                   BINVM(1))
          END IF
        END IF
C
C       first set of riffle parms - table-type benal-riff1
        I2= 111
        I4=   5
        CALL RTABLE
     I              (I2,I1,I4,UUNITS,
     M               PKPM10)
C
C       second set of riffle parms - table-type benal-riff2
        I2= 112
        I4=   8
        CALL RTABLE
     I              (I2,I1,I4,UUNITS,
     M               PKPM11)
      END IF
C
C     initial conditions.  table-type plnk-init
      I2= 113
      I4=  6
      CALL RTABLE
     I             (I2,I1,I4,UUNITS,
     M              RVAL)
C
      PHYTO= RVAL(1)
C
      IF (PHYFG .EQ. 0) THEN
C       initialize fluxes of inactive constituent
        ROPHYT= 0.0
        DO 60 I= 1, NEXITS
          OPHYT(I)= 0.0
 60     CONTINUE
        PHYDOX= 0.0
        PHYBOD= 0.0
        PHYTAM= 0.0
        PHYNO3= 0.0
        PHYPO4= 0.0
        PHYORN= 0.0
        PHYORP= 0.0
        PHYORC= 0.0
        PYCO2= 0.0
        DTHPHY= 0.0
        GROPHY= 0.0
        TOTPHY= 0.0
      END IF
C
      IF (ZOOFG .EQ. 1) THEN
C       convert zoo to mg/l
        ZOO= RVAL(2)*ZOMASS
      ELSE
C       zooplankton not simulated, but use default values of
CTHJC       zomass and zoo to convert/compute zoo in case of bugs in later code
CTHJ        ZOO= .03*.0003
C       initialize fluxes of inactive constituent
        ROZOO= 0.0
        DO 70 I= 1, NEXITS
          OZOO(I)= 0.0
 70     CONTINUE
        ZOODOX= 0.0
        ZOOBOD= 0.0
        ZOOTAM= 0.0
        ZOONO3= 0.0
        ZOOPO4= 0.0
        ZOOORN= 0.0
        ZOOORP= 0.0
        ZOOORC= 0.0
        ZOOPHY= 0.0
        ZOCO2= 0.0
        GROZOO= 0.0
        DTHZOO= 0.0
        TOTZOO= 0.0
      END IF
      ORN= RVAL(4)
      ORP= RVAL(5)
      ORC= RVAL(6)
C
      IF (NUMBAL .EQ. 1) THEN
C       single species
        BENAL(1)= RVAL(3)
      ELSE IF (NUMBAL .GE. 2) THEN
C       multiple species - table-type benal-init
        I2= 114
        I4= NUMBAL
        CALL RTABLE
     I             (I2,I1,I4,UUNITS,
     M              BENAL)
      ELSE
C       no benthic algae simulated
        BALDOX= 0.0
        BALBOD= 0.0
        BALTAM= 0.0
        BALNO3= 0.0
        BALPO4= 0.0
        BALORN= 0.0
        BALORP= 0.0
        BALORC= 0.0
        BACO2= 0.0
        DO 75 I= 1, 3
          DO 73 J= 1, 4
            FLXBAL(I,J)= 0.0
 73       CONTINUE
 75     CONTINUE
      END IF
C
C     compute derived quantities
      PHYCLA= PHYTO*CVBCL
      DO 80 I= 1, NUMBAL
        BALCLA(I)= BENAL(I)*CVBCL
 80   CONTINUE
      IF (VOL .GT. 0.0) THEN
C       compute initial summary concentrations
        DO 90 I= 1, 3
          LSNH4(I)= RSNH4(I)/VOL
          LSPO4(I)= RSPO4(I)/VOL
 90     CONTINUE
        CALL PKSUMS (PHYFG,ZOOFG,TAMFG,NO2FG,PO4FG,ADNHFG,ADPOFG,
     I               CVBN,CVBP,CVBC,CVBO,CVNRBO,PHYTO,ZOO,ORN,ORP,ORC,
     I               NO3,TAM,NO2,LSNH4(1),LSNH4(2),LSNH4(3),PO4,
     I               LSPO4(1),LSPO4(2),LSPO4(3),BOD,
     O               TORN,TORP,TORC,POTBOD,TN,TP)
      ELSE
C       undefined summary concentrations
        TORN= -1.0E30
        TORP= -1.0E30
        TORC= -1.0E30
        POTBOD= -1.0E30
        TN= -1.0E30
        TP= -1.0E30
      END IF
C
      IF (OUTLEV.GT.1) THEN
        WRITE (MESSU,2030)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PLANK
C
C     + + + PURPOSE + + +
C     Simulate behavior of plankton populations and associated
C     reactions
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PLANK2 + + +
      INCLUDE  'crhpl.inc'
      INCLUDE  'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER  I,N,REQFG,TSSUB(2),FLGVAL
      REAL     BALLIT,CFLIT,EXTCLA,EXTSED,INLIT,PHYLIT,REFR,
     $         DOPHY,DOZOO,DOBALG,PHDTH,PHGRO,ZEAT,ORNPHY,ORPPHY,
     $         ORCPHY,BODPHY,BODZOO,BODBAL,TAMPHY,NO3PHY,PO4PHY,
     $         TAMBAL,NO3BAL,PO4BAL,NITZOO,PO4ZOO,VOLSP,INORN,INORP,
     $         INORC,ZGRO,ZDTH,ZORN,ZORP,ZORC,BGRO(4),BDTH(4),ORNBAL,
     $         ORPBAL,ORCBAL,DUMVAL,INNO3,INTAM,INPO4,PLADFX,PLADCN,
     $         TURB,GROBAL(4),DTHBAL(4),LSNH4(3),LSPO4(3)
      DOUBLE PRECISION DORC,DORN,DORP,DPHYTO
      CHARACTER*4 LIMIT(7),LIMC
      CHARACTER*6 OPTYP,TSNAM,SECNAM,MSECNM,OPFGNM
C
C     + + + FUNCTIONS + + +
      REAL         DAYVAL
C
C     + + + EXTERNALS + + +
      EXTERNAL ADVPLK,SINK,ADVECT,DAYVAL,HREQTS
      EXTERNAL LITRCH,PHYRX,ZORX,BALRX,BALRX2,PKSUMS,AMMION
C
C     + + + DATA INITIALIZATIONS + + +
      DATA LIMIT/'LIT ','NON ','TEM ','NIT ','PO4 ','NONE','WAT '/
      DATA TSSUB/1,1/
      DATA OPTYP,SECNAM/'RCHRES','PLANK '/
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT(A4)
C
C     + + + HISTORY + + +
C     12/11/2003   BRB  added call to ammion to redistribute TAM after algal influence
C
C     + + + END SPECIFICATIONS + + +
C
C     single precision version of vol
      VOLSP= VOL
C
C     define fraction of biomass which is refractory material
      REFR= 1.0- NONREF
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
        IF (PLADFG(N) .LE. -1) THEN
CTHJ          PLADDR(I)= SAREA*PAD(PLAFFP(I)+IVL1)
          REQFG= 4
          TSNAM= 'PLADFX'
          OPFGNM= 'PLADFG'
          CALL HREQTS (PLAFFP(I),IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,
     I                 RCHNO,TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,
     I                 PLADFG(N),
     O                 PLADFX)
          PLADDR(I)= SAREA*PLADFX
        ELSE IF (PLADFG(N) .GE. 1) THEN
          PLADDR(I)= SAREA*DAYVAL(PLAFXM(MON,I),PLAFXM(NXTMON,I),DAY,
     I                            NDAYS)
        ELSE
          PLADDR(I)= 0.0
        END IF
C       wet deposition
        IF (PLADFG(N+1) .LE. -1) THEN
CTHJ          PLADWT(I)= PREC*SAREA*PAD(PLACFP(I)+IVL1)
          REQFG= 4
          TSNAM= 'PLADCN'
          OPFGNM= 'PLADFG'
          CALL HREQTS (PLACFP(I),IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,
     I                 RCHNO,TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,
     I                 PLADFG(N+1),
     O                 PLADCN)
          PLADWT(I)= PREC*SAREA*PLADCN
        ELSE IF (PLADFG(N+1) .GE. 1) THEN
          PLADWT(I)= PREC*SAREA*DAYVAL(PLACNM(MON,I),PLACNM(NXTMON,I),
     I                                 DAY,NDAYS)
        ELSE
          PLADWT(I)= 0.0
        END IF
        PLADEP(I)= PLADDR(I)+ PLADWT(I)
 10   CONTINUE
C
C     get inflowing material from pad; advect
      IF (PHYFG .EQ. 1) THEN
C       phytoplankton simulated
        IF (IPYFP .GT. 0) THEN
          IPHYTO= PAD(IPYFP + IVL1)
        ELSE
          IPHYTO= 0.0
        END IF
C       advect phytoplankton
        CALL ADVPLK
     I              (IPHYTO,VOLS,SROVOL,VOL,EROVOL,SOVOL,
     I               EOVOL,NEXITS,OREF,MXSTAY,SEED,DELTS,
     M               PHYTO,
     O               ROPHYT,OPHYT)
C
        DPHYTO= PHYTO
        CALL SINK
     I            (VOL,AVDEPE,PHYSET,
     M             DPHYTO,
     O             SNKPHY)
        PHYTO = DPHYTO
        SNKPHY= -SNKPHY
C
        IF (ZOOFG .EQ. 1) THEN
C         zooplankton on
          IF (IZFP .GT. 0) THEN
            IZOO= PAD(IZFP + IVL1)
          ELSE
            IZOO= 0.0
          END IF
C         advect zooplankton
          CALL ADVPLK
     I                (IZOO,VOLS,SROVOL,VOL,EROVOL,SOVOL,
     I                 EOVOL,NEXITS,OREF,MXSTAY,SEED,DELTS,
     M                 ZOO,
     O                 ROZOO,OZOO)
        END IF
      END IF
C
      IF (IONFP .GT. 0) THEN
C       input organic nitrogen
        IORN= PAD(IONFP + IVL1)
      ELSE
C       no input organic nitrogen
        IORN= 0.0
      END IF
C
      INORN= IORN+ PLADEP(1)
C
C     advect organic nitrogen
      DORN= ORN
      CALL ADVECT
     I            (INORN,VOLS,SROVOL,VOL,EROVOL,SOVOL,
     I             EOVOL,NEXITS,
     M             DORN,
     O             ROORN,OORN)
      CALL SINK
     I          (VOL,AVDEPE,REFSET,
     M           DORN,
     O           SNKORN)
      ORN= DORN
      SNKORN= -SNKORN
C
      IF (IOPFP .GT. 0) THEN
C       input organic phosphorus
        IORP= PAD(IOPFP + IVL1)
      ELSE
C       no input organic phosphorus
        IORP= 0.0
      END IF
C
      INORP= IORP+ PLADEP(2)
C
C     advect organic phosphorus
      DORP= ORP
      CALL ADVECT
     I            (INORP,VOLS,SROVOL,VOL,EROVOL,SOVOL,
     I             EOVOL,NEXITS,
     M             DORP,
     O             ROORP,OORP)
C
      CALL SINK
     I          (VOL,AVDEPE,REFSET,
     M           DORP,
     O           SNKORP)
      ORP= DORP
      SNKORP= -SNKORP
C
      IF (IOCFP .GT. 0) THEN
C       input total organic carbon
        IORC= PAD(IOCFP + IVL1)
      ELSE
C       no input total organic carbon
        IORC= 0.0
      END IF
C
      INORC= IORC+ PLADEP(3)
C
C     advect total organic carbon
      DORC= ORC
      CALL ADVECT
     I            (INORC,VOLS,SROVOL,VOL,EROVOL,SOVOL,
     I             EOVOL,NEXITS,
     M             DORC,
     O             ROORC,OORC)
      CALL SINK
     I          (VOL,AVDEPE,REFSET,
     M           DORC,
     O           SNKORC)
      ORC= DORC
      SNKORC= -SNKORC
C
      IF (AVDEPE .GT. 0.17) THEN
C       enough water to warrant computation of water quality reactions
C
        IF (FRRIF .LT. 1.0) THEN                                          
C         make adjustments to average water velocity and depth for the  
C         portion of the reach that consists of riffle areas.
          IF (RO .LT. RIFCQ1) THEN
C           below first cutoff flow
            I= 1
          ELSE IF (RO .LT. RIFCQ2) THEN
C           below second cutoff flow
            I= 2
          ELSE IF (RO .LT. RIFCQ3) THEN                                  
C           below third cutoff flow
            I= 3
          ELSE                                                        
C           above third cutoff flow
            I= 4
          ENDIF                                                         
C         calculate the adjusted velocity and depth for riffle sections
          BALVEL = RIFVEL(I)*AVVELE
          BALDEP = RIFDEP(I)*AVDEPE
        ELSE
C         use full depth and velocity
          BALVEL= AVVELE
          BALDEP= AVDEPE
        END IF                                                           
C                                                                       
C       calculate solar radiation absorbed; solrad is the solar
C       radiation at gage, corrected for location of reach; 0.97
C       accounts for surface reflection (assumed 3 per cent); cfsaex
C       is the ratio of radiation incident to water surface to gage
C       radiation values (accounts for regional differences, shading
C       of water surface, etc); inlit is a measure of light intensity
C       immediately below surface of reach/res and is expressed as
C       ly/min, adjusted for fraction that is photosynthetically
C       active.
CTHJ        SOLRAD= PAD(SOLFP + IVL1)
        REQFG= 2
        TSNAM= 'SOLRAD'
        CALL HREQTS (SOLFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,RCHNO,
     I               TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,FLGVAL,
     O               SOLRAD)
        INLIT = 0.97*CFSAEX*SOLRAD/DELT*PARADF
C
        IF (SDLTFG .EQ. 1) THEN
C         influence of sediment on light extinction is considered
          IF (SEDFG .EQ. 0) THEN
C           read total sediment conc. from pad; units are mg/l
CTHJ            SSED(4)= PAD(SSEDFP(4) + IVL1)
            REQFG= 5
            TSNAM= 'SSED  '
            TSSUB(1)= 4
            MSECNM= 'SEDTRN'
            OPFGNM= 'SDLTFG'
            CALL HREQTS (SSEDFP(4),IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,
     I                   RCHNO,TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,SDLTFG,
     O                   SSED(4))
            TSSUB(1)= 1
          ELSE
C           data are available from module section sedtrn
          END IF
C
C         estimate contribution of sediment to light extinction
          EXTSED= LITSED*SSED(4)
C
        ELSE IF (SDLTFG.EQ.2) THEN
C         equations from DSSAMt for estimating the extinction coefficient
C         based on discharge and turbidity
C
C         estimate turbidity based on linear regression on flow
          TURB = CTRBQ1*RO**CTRBQ2
C
C         estimate the portion of the extinction coefficient due to
C         sediment based upon a system-wide regression of total
C         extinction to turbidity, and then subtracting the
C         base extinction coefficient
          EXTSED = (CKTRB1*TURB**CKTRB2)- EXTB                         
          IF (EXTSED .LT. 0.0) THEN
C           no effective sediment shading
            EXTSED = 0.0
          END IF
        ELSE
C         sediment light extinction not considered
          EXTSED= 0.0
        END IF
C
C       calculate contribution of phytoplankton to light extinction
C       (self-shading)
        EXTCLA= .00452*PHYTO*CVBCL
C
C       calculate light available for algal growth
        CALL LITRCH (INLIT,EXTB,EXTCLA,EXTSED,AVDEPE,BALDEP,PHYFG,BALFG,
     O               PHYLIT,BALLIT,CFLIT)
C
        IF (PHYFG .EQ. 1) THEN
C         simulate phytoplankton
          CALL PHYRX
     I               (PHYLIT,TW,TALGRL,TALGRH,TALGRM,MALGR,CMMP,
     I                CMMNP,TAMFG,AMRFG,NSFG,CMMN,CMMLT,DELT60,
     I                CFLIT,ALR20,CVBPN,PHFG,DECFG,CVBPC,PALDH,
     I                NALDH,CLALDH,ALDL,ALDH,ANAER,OXALD,ALNPR,
     I                CVBO,REFR,CVNRBO,CVPB,CVBCL,LIMIT,CO2,
     I                NMINGR,PMINGR,CMINGR,LMINGR,NMINC,
     M                PO4,NO3,TAM,DOX,ORN,ORP,ORC,BOD,PHYTO,
     O                LIMPHY,PYCO2,PHYCLA,DOPHY,BODPHY,TAMPHY,
     O                NO3PHY,PO4PHY,PHDTH,PHGRO,ORNPHY,ORPPHY,
     O                ORCPHY)
C
C         compute associated fluxes
          PHYDOX= DOPHY*VOLSP
          PHYBOD= BODPHY*VOLSP
          PHYTAM= TAMPHY*VOLSP
          PHYNO3= NO3PHY*VOLSP
          PHYPO4= PO4PHY*VOLSP
          DTHPHY= -PHDTH*VOLSP
          GROPHY= PHGRO*VOLSP
          PHYORN= ORNPHY*VOLSP
          PHYORP= ORPPHY*VOLSP
          PHYORC= ORCPHY*VOLSP
C
          IF (ZOOFG .EQ. 1) THEN
C           simulate zooplankton
            CALL ZORX
     I                (ZFIL20,TCZFIL,TW,PHYTO,MZOEAT,ZEXDEL,CVPB,
     I                 ZRES20,TCZRES,ANAER,ZOMASS,TAMFG,REFR,
     I                 ZFOOD,ZD,OXZD,CVBN,CVBP,CVBC,CVNRBO,CVBO,
     M                 DOX,BOD,ZOO,ORN,ORP,ORC,TAM,NO3,PO4,
     O                 ZEAT,ZOCO2,DOZOO,BODZOO,NITZOO,PO4ZOO,
     O                 ZGRO,ZDTH,ZORN,ZORP,ZORC)
C
C           compute associated fluxes
            ZOODOX= -DOZOO*VOLSP
            ZOOBOD= BODZOO*VOLSP
            IF (TAMFG .NE. 0) THEN
C             ammonia on, so nitrogen excretion goes to ammonia
              ZOOTAM= NITZOO*VOLSP
            ELSE
C             ammonia off, so nitrogen excretion goes to nitrate
              ZOONO3= NITZOO*VOLSP
            END IF
            ZOOPO4= PO4ZOO*VOLSP
            ZOOPHY= -ZEAT*VOLSP
            ZOOORN= ZORN*VOLSP
            ZOOORP= ZORP*VOLSP
            ZOOORC= ZORC*VOLSP
            GROZOO= ZGRO*VOLSP
            DTHZOO= -ZDTH*VOLSP
            TOTZOO= GROZOO+ DTHZOO
C
C           update phytoplankton state variable to account for
C           zooplankton predation
            PHYTO = PHYTO - ZEAT
C
C           convert phytoplankton expressed as mg biomass/l to
C           chlorophyll a expressed as ug/l
            PHYCLA= PHYTO*CVBCL
          END IF
          TOTPHY= SNKPHY+ ZOOPHY+ DTHPHY+ GROPHY
        END IF
C
        IF (BALFG .EQ. 1) THEN
C         simulate benthic algae
          CALL BALRX
     I               (BALLIT,TW,TALGRL,TALGRH,TALGRM,MALGR,CMMP,
     I                CMMNP,TAMFG,AMRFG,NSFG,CMMN,CMMLT,DELT60,
     I                CFLIT,ALR20,CVBPN,PHFG,DECFG,CVBPC,PALDH,
     I                NALDH,ALDL,ALDH,ANAER,OXALD,CFBALG,CFBALR,
     I                ALNPR,CVBO,REFR,CVNRBO,CVPB,MBAL,DEPCOR,
     I                LIMIT,CVBCL,CO2,NMINGR,PMINGR,CMINGR,LMINGR,
     I                NMINC,
     M                PO4,NO3,TAM,DOX,ORN,ORP,ORC,BOD,BENAL(1),
     O                LIMBAL(1),BACO2,BALCLA(1),DOBALG,BODBAL,
     O                TAMBAL,NO3BAL,PO4BAL,BGRO,BDTH,ORNBAL,
     O                ORPBAL,ORCBAL)
C
C         compute associated fluxes
          BALDOX= DOBALG*VOLSP
          BALBOD= BODBAL*VOLSP
          BALTAM= TAMBAL*VOLSP
          BALNO3= NO3BAL*VOLSP
          BALPO4= PO4BAL*VOLSP
          BALORN= ORNBAL*VOLSP
          BALORP= ORPBAL*VOLSP
          BALORC= ORCBAL*VOLSP
          GROBAL(1)= BGRO(1)
          DTHBAL(1)= -BDTH(1)
        ELSE IF (BALFG .EQ. 2) THEN
C         simulate enhanced benthic algae equations from DSSAMt
C
C         first check for required input timeseries
          IF (BINVFG .EQ. 1) THEN
C           require input of BINV as a timeseries
            REQFG= 4
            TSNAM= 'BINV'
            OPFGNM= 'BINVFG'
            CALL HREQTS (BINVFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,
     I                   RCHNO,TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,BINVFG,
     O                   BINV)
C         ELSE IF (BINVFG .EQ. 3) THEN
C           varies monthly
            IF (DAYFG .EQ. 1) THEN
C             interpolate a new value
              BINV= DAYVAL(BINVM(MON),BINVM(NXTMON),DAY,NDAYS)
            END IF
          END IF
C
C         then perform reactions
          CALL BALRX2 (BALLIT,TW,TAMFG,NSFG,DELT60,CVBPN,PHFG,DECFG,
     I                 CVBPC,ALNPR,CVBO,REFR,CVNRBO,CVPB,DEPCOR,
     I                 LIMIT,CVBCL,CO2,NUMBAL,MBALGR,CMMPB,CMMNB,
     I                 BALR20,TCBALG,BALVEL,CMMV,BFIXFG,CSLIT,CMMD1,
     I                 CMMD2,TCBALR,FRRIF,CREMVL,CMMBI,BINV,TCGRAZ,
     I                 CSLOF1,CSLOF2,MINBAL,FRAVL,BNPFG,CAMPR,NMINGR,
     I                 PMINGR,CMINGR,LMINGR,NMINC,NMAXFX,GRORES,
     M                 PO4,NO3,TAM,DOX,ORN,ORP,ORC,BOD,BENAL,
     O                 LIMBAL,BACO2,BALCLA,DOBALG,BODBAL,TAMBAL,
     O                 NO3BAL,PO4BAL,BGRO,BDTH,ORNBAL,ORPBAL,
     O                 ORCBAL)
C
C         compute associated fluxes
          BALDOX= DOBALG*VOLSP
          BALBOD= BODBAL*VOLSP
          BALTAM= TAMBAL*VOLSP
          BALNO3= NO3BAL*VOLSP
          BALPO4= PO4BAL*VOLSP
          BALORN= ORNBAL*VOLSP
          BALORP= ORPBAL*VOLSP
          BALORC= ORCBAL*VOLSP
          DO 20 I= 1, NUMBAL
            GROBAL(I)= BGRO(I)
            DTHBAL(I)= -BDTH(I)
 20       CONTINUE
        END IF
      ELSE
C       not enough water in reach/res to warrant simulation of
C       quality processes
        PHYORN= 0.0
        BALORN= 0.0
        ZOOORN= 0.0
        PHYORP= 0.0
        BALORP= 0.0
        ZOOORP= 0.0
        PHYORC= 0.0
        BALORC= 0.0
        ZOOORC= 0.0
        PYCO2= 0.0
        BACO2= 0.0
        ZOCO2= 0.0
        PHYDOX= 0.0
        ZOODOX= 0.0
        BALDOX= 0.0
        PHYBOD= 0.0
        ZOOBOD= 0.0
        BALBOD= 0.0
        PHYTAM= 0.0
        ZOOTAM= 0.0
        BALTAM= 0.0
        PHYNO3= 0.0
        ZOONO3= 0.0
        BALNO3= 0.0
        PHYPO4= 0.0
        ZOOPO4= 0.0
        BALPO4= 0.0
C
        IF (PHYFG .EQ. 1) THEN
C         water scarcity limits phytoplankton growth
          LIMC  = LIMIT(7)
          READ (LIMC,1000) LIMPHY
          PHYCLA= PHYTO*CVBCL
          GROPHY= 0.0
          DTHPHY= 0.0
          ZOOPHY= 0.0
          TOTPHY= SNKPHY
        END IF
C
        IF (BALFG .EQ. 1) THEN
C         water scarcity limits benthic algae growth
          LIMC= LIMIT(7)
          READ (LIMC,1000) LIMBAL(1)
          BALCLA(1)= BENAL(1)*CVBCL
          GROBAL(1)= 0.0
          DTHBAL(1)= 0.0
        ELSE IF (BALFG .EQ. 2) THEN
C         water scarcity limits benthic algae growth
          LIMC= LIMIT(7)
          DO 30 I= 1, NUMBAL
            READ (LIMC,1000) LIMBAL(I)
            BALCLA(I)= BENAL(I)*CVBCL
            GROBAL(I)= 0.0
            DTHBAL(I)= 0.0
 30       CONTINUE
        END IF                                                           
C
        IF (ZOOFG .EQ. 1) THEN
C         water scarcity limits zooplankton growth
          GROZOO= 0.0
          DTHZOO= 0.0
          TOTZOO= 0.0
        END IF
      END IF
C
      IF (BALFG.GE.1) THEN
C       store final benthic sums and fluxes in common block
        TBENAL(1)= 0.0
        GROTBA= 0.0
        DTHTBA= 0.0
        DO 40 I= 1, NUMBAL
          FLXBAL(1,I)= GROBAL(I)
          FLXBAL(2,I)= DTHBAL(I)
          FLXBAL(3,I)= GROBAL(I)+ DTHBAL(I)
          TBENAL(1)= TBENAL(1)+ BENAL(I)
          GROTBA= GROTBA+ GROBAL(I)
          DTHTBA= DTHTBA+ DTHBAL(I)
 40     CONTINUE
        TBENAL(2)= TBENAL(1)*CVBCL
        TOTTBA= GROTBA+ DTHTBA
      END IF
C
C     compute final process fluxes for oxygen, nutrients and organics
      TOTDOX= READOX+ BODDOX+ BENDOX+ NITDOX+ PHYDOX+ ZOODOX+ BALDOX
      TOTBOD= DECBOD+ BNRBOD+ SNKBOD+ DENBOD+ PHYBOD+ ZOOBOD+ BALBOD
      TOTNO3= NITNO3+ DENNO3+ BODNO3+ PHYNO3+ ZOONO3+ BALNO3
      TOTTAM= NITTAM+ VOLNH3+ BNRTAM+ BODTAM+ PHYTAM+ ZOOTAM+ BALTAM
      TOTPO4= BNRPO4+ BODPO4+ PHYPO4+ ZOOPO4+ BALPO4
C
      TOTORN= SNKORN+ PHYORN+ ZOOORN+ BALORN
      TOTORP= SNKORP+ PHYORP+ ZOOORP+ BALORP
      TOTORC= SNKORC+ PHYORC+ ZOOORC+ BALORC
C
C     compute summaries of total organics, total n and p, and potbod
C
C     concentrations
      IF (VOL .GT. 0.0) THEN
C       compute summary concentrations
        DO 45 I= 1, 3
          LSNH4(I)= RSNH4(I)/VOL
          LSPO4(I)= RSPO4(I)/VOL
 45     CONTINUE
        CALL PKSUMS (PHYFG,ZOOFG,TAMFG,NO2FG,PO4FG,ADNHFG,ADPOFG,
     I               CVBN,CVBP,CVBC,CVBO,CVNRBO,PHYTO,ZOO,ORN,ORP,ORC,
     I               NO3,TAM,NO2,LSNH4(1),LSNH4(2),LSNH4(3),PO4,
     I               LSPO4(1),LSPO4(2),LSPO4(3),BOD,
     O               TORN,TORP,TORC,POTBOD,TN,TP)
      ELSE
C       undefined summary concentrations
        TORN= -1.0E30
        TORP= -1.0E30
        TORC= -1.0E30
        POTBOD= -1.0E30
        TN= -1.0E30
        TP= -1.0E30
      END IF
C
C     total inflows
      INNO3= INO3+ NUADEP(1)
      INTAM= ITAM+ NUADEP(2)
      INPO4= IPO4+ NUADEP(3)
      CALL PKSUMS (PHYFG,ZOOFG,TAMFG,NO2FG,PO4FG,ADNHFG,ADPOFG,
     I             CVBN,CVBP,CVBC,CVBO,CVNRBO,IPHYTO,IZOO,INORN,INORP,
     I             INORC,INNO3,INTAM,INO2,ISNH4(1),ISNH4(2),ISNH4(3),
     I             INPO4,ISPO4(1),ISPO4(2),ISPO4(3),IBOD,
     O             ITORN,ITORP,ITORC,DUMVAL,ITOTN,ITOTP)
C
C     total outflows
      CALL PKSUMS (PHYFG,ZOOFG,TAMFG,NO2FG,PO4FG,ADNHFG,ADPOFG,
     I             CVBN,CVBP,CVBC,CVBO,CVNRBO,ROPHYT,ROZOO,ROORN,ROORP,
     I             ROORC,RONO3,ROTAM,RONO2,ROSNH4(1),ROSNH4(2),
     I             ROSNH4(3),ROPO4,ROSPO4(1),ROSPO4(2),ROSPO4(3),ROBOD,
     O             ROTORN,ROTORP,ROTORC,DUMVAL,ROTOTN,ROTOTP)
C
      IF (NEXITS .GT. 1) THEN
C       outflows by exit
        DO 50 I= 1, NEXITS
          CALL PKSUMS (PHYFG,ZOOFG,TAMFG,NO2FG,PO4FG,ADNHFG,ADPOFG,
     I                 CVBN,CVBP,CVBC,CVBO,CVNRBO,OPHYT(I),OZOO(I),
     I                 OORN(I),OORP(I),OORC(I),ONO3(I),OTAM(I),ONO2(I),
     I                 OSNH4(I,1),OSNH4(I,2),OSNH4(I,3),OPO4(I),
     I                 OSPO4(I,1),OSPO4(I,2),OSPO4(I,3),OBOD(I),
     O                 OTORN(I),OTORP(I),OTORC(I),DUMVAL,OTOTN(I),
     O                 OTOTP(I))
 50     CONTINUE
      END IF
C
C     recompute ammonia ionization
CBRB  added call to ammion to redistribute TAM after algal influence
      CALL AMMION
     I            (TW,PHVAL,TAM,
     O             NH3,NH4)
C
      RETURN
      END
C
C
C
      SUBROUTINE   ADVPLK
     I                    (IPLANK,VOLS,SROVOL,VOL,EROVOL,SOVOL,
     I                     EOVOL,NEXITS,OREF,MXSTAY,SEED,DELTS,
     M                     PLANK,
     O                     ROPLK,OPLK)
C
C     + + + PURPOSE + + +
C     Advect plankton
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    NEXITS
      REAL       IPLANK,SROVOL,EROVOL,SOVOL(NEXITS),
     $           EOVOL(NEXITS),OREF,MXSTAY,SEED,DELTS,PLANK,
     $           ROPLK,OPLK(NEXITS)
      DOUBLE PRECISION VOL,VOLS
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IPLANK - ???
C     VOLS   - ???
C     SROVOL - ???
C     VOL    - volume of water in reach above bed
C     EROVOL - ???
C     SOVOL  - ???
C     EOVOL  - ???
C     NEXITS - number of exits from the operation
C     OREF   - ???
C     MXSTAY - ???
C     SEED   - ???
C     DELTS  - ???
C     PLANK  - ???
C     ROPLK  - ???
C     OPLK   - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    N
      REAL       OFLO,STAY,MSTAY,PLNKAD
      DOUBLE PRECISION DPLKAD
C
C     + + + EXTERNALS + + +
      EXTERNAL   ADVECT
C
C     + + + END SPECIFICATIONS + + +
C
C     calculate concentration of plankton not subject to advection
C     during interval
      OFLO= (SROVOL + EROVOL)/DELTS
C
      IF (OREF .GT. 0.0 .AND. OFLO/OREF .LE. 100.0) THEN
        STAY= (MXSTAY - SEED)*(2.0**(-OFLO/OREF)) + SEED
      ELSE
        STAY= SEED
      END IF
C
      IF (PLANK .GT. STAY) THEN
C       convert stay to units of mass; this mass will be converted
C       back to units of concentration based on the volume of the
C       reach/res at the end of the interval
        MSTAY= STAY*VOLS
C
C       determine concentration of plankton subject to advection;
C       this value is passed into subroutine advect
        PLNKAD = PLANK - STAY
C
C       advect plankton
        DPLKAD= PLNKAD
        CALL ADVECT
     I              (IPLANK,VOLS,SROVOL,VOL,EROVOL,SOVOL,
     I               EOVOL,NEXITS,
     M               DPLKAD,
     O               ROPLK,OPLK)
        PLNKAD= DPLKAD
C
C       determine final concentration of plankton in reach/res after
C       advection
        IF (VOL .GT. 0.0) THEN
          PLANK= PLNKAD + MSTAY/VOL
        ELSE
          PLANK=PLNKAD
        END IF
      ELSE
C       no plankton leaves the reach/res
        ROPLK= 0.0
        DO 20 N= 1,NEXITS
          OPLK(N)= 0.0
 20     CONTINUE
        MSTAY= PLANK*VOLS
C
        IF (VOL .GT. 0.0) THEN
          PLANK= (MSTAY + IPLANK)/VOL
        ELSE
          PLANK= -1.0E30
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   ALGRO
     I                   (LIGHT,PO4,NO3,TW,TALGRL,TALGRH,TALGRM,MALGR,
     I                    CMMP,CMMNP,TAMFG,AMRFG,TAM,NSFG,CMMN,CMMLT,
     I                    ALR20,CFLIT,DELT60,LIMIT,NMINGR,PMINGR,
     I                    LMINGR,
     O                    LIMR,GRO,RES)
C
C     + + + PURPOSE + + +
C     Calculate unit growth and respiration rates for algae
C     population; both are expressed in units of per interval
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     TAMFG,AMRFG,NSFG
      REAL        LIGHT,PO4,NO3,TW,TALGRL,TALGRH,TALGRM,MALGR,
     $            CMMP,CMMNP,TAM,CMMN,CMMLT,ALR20,CFLIT,DELT60,
     $            NMINGR,PMINGR,LMINGR,LIMR,GRO,RES
      CHARACTER*4 LIMIT(7)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LIGHT  - ???
C     PO4    - ???
C     NO3    - dissolved nitrate concentration in mg/l
C     TW     - water temperature in degrees C
C     TALGRL - ???
C     TALGRH - ???
C     TALGRM - ???
C     MALGR  - ???
C     CMMP   - ???
C     CMMNP  - ???
C     TAMFG  - ???
C     AMRFG  - ???
C     TAM    - total ammonia (nh3 + nh4) in mg n/l
C     NSFG   - ???
C     CMMN   - ???
C     CMMLT  - ???
C     ALR20  - ???
C     CFLIT  - ???
C     DELT60 - simulation time interval in hours
C     LIMIT  - ???
C     NMINGR - minimum nitrate concentration for algal growth
C     PMINGR - minimum phosphate concentration for algal growth
C     LMINGR - minimum light intensity for algal growth
C     LIMR   - ???
C     GRO    - ???
C     RES    - ???
C
C     + + + LOCAL VARIABLES + + +
      REAL        TCMALG,MALGRT,GROP,MALGN,LOLIM,MMN,GRON,GROL
      CHARACTER*4 LIM
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT(A4)
C
C     + + + END SPECIFICATIONS + + +
C
      IF (LIGHT .GT. LMINGR) THEN
C       sufficient light to support growth
        IF (PO4 .GT. PMINGR .AND. NO3 .GT. NMINGR) THEN
C         sufficient nutrients to support growth
          IF (TW .GT. TALGRL .AND. TW .LT. TALGRH) THEN
C           water temperature allows growth
C
C           calculate temperature correction fraction
            IF (TW .LT. TALGRM) THEN
              TCMALG= (TW - TALGRL)/(TALGRM - TALGRL)
            ELSE
C             no temperature correction to maximum unit growth rate
C             is necessary; water temperature is in the optimum
C             range for phytoplankton growth
              TCMALG= 1.0
            END IF
C
C           perform temperature correction to maximum unit growth
C           rate; units of malgrt are per interval
            MALGRT= MALGR*TCMALG
C
C           calculate maximum phosphorus limited unit growth rate
            GROP= MALGRT*PO4*NO3/((PO4 + CMMP)*(NO3 + CMMNP))
C
C           calculate the maximum nitrogen limited unit growth rate
            IF (TAMFG .NE. 0) THEN
C             consider influence of tam on growth rate
              IF (AMRFG .NE. 0) THEN
C               calculate tam retardation to nitrogen limited
C               growth rate
                MALGN= MALGRT - 0.757*TAM + 0.051*NO3
C
C               check that calculated unit growth rate does not
C               exceed maximum allowable growth rate
                IF (MALGN .GT. MALGRT) THEN
                  MALGN= MALGRT
                ELSE
C                 check that calculated unit growth rate is not
C                 less than .001 of the maximum unit growth rate;
C                 if it is, set the unit growth rate equal to .001
C                 of the maximum unit growth rate
                  LOLIM= .001*MALGRT
                  IF (MALGN .LT. LOLIM) THEN
                    MALGN= LOLIM
                  END IF
                END IF
              ELSE
C               ammonia retardation is not considered
                MALGN= MALGRT
              END IF
C
              IF (NSFG .NE. 0) THEN
C               include tam in nitrogen pool for calculation of
C               nitrogen limited growth rate
                MMN= NO3 + TAM
              ELSE
C               tam is not included in nitrogen pool for calculation
C               of nitrogen limited growth
                MMN= NO3
              END IF
            ELSE
C             tam is not simulated
              MALGN= MALGRT
              MMN  = NO3
            END IF
C
C           calculate the maximum nitrogen limited unit growth rate
            GRON= (MALGN*MMN)/(CMMN + MMN)
C
C           calculate the maximum light limited unit growth rate
            GROL= (MALGRT*LIGHT)/(CMMLT + LIGHT)
C
C           find the actual algal unit growth rate (gro); gro is
C           the smallest of the three computed unit growth rates
C           (grop,gron,grol) and is expressed in units of per
C           interval; assign a three letter label to lim which will
C           be printed in the output to indicate the growth limiting
C           factor for the interval: limit(1)= 'lit'
C                                    limit(2)= 'non'
C                                    limit(3)= 'tem'
C                                    limit(4)= 'nit'
C                                    limit(5)= 'po4'
C                                    limit(6)= 'none'
C                                    limit(7)= 'wat'
C
            IF (GROP .LT. GRON .AND. GROP .LT. GROL) THEN
              GRO= GROP
              LIM= LIMIT(5)
            ELSE
              IF (GRON .LT. GROL) THEN
                GRO= GRON
                LIM= LIMIT(4)
              ELSE
                GRO= GROL
                LIM= LIMIT(1)
              END IF
            END IF
C
            IF (GRO .LT. (.000001*DELT60)) THEN
              GRO= 0.0
            END IF
C
            IF (GRO .GT. (.95*MALGRT)) THEN
C             there is no limiting factor to cause less than maximum
C             growth rate
              LIM= LIMIT(6)
            END IF
C
C           adjust growth rate if control volume is not entirely
C           contained within the euphotic zone; e.g. if only one
C           half of the control volume is in the euphotic zone, gro
C           would be reduced to one half of its specified value
            GRO= GRO*CFLIT
          ELSE
C           water temperature does not allow algal growth
            GRO= 0.0
            LIM= LIMIT(3)
          END IF
        ELSE
C         no algal growth occurs; necessary nutrients are not
C         available
          GRO= 0.0
          LIM= LIMIT(2)
        END IF
      ELSE
C       no algal growth occurs; necessary light is not available
        GRO= 0.0
        LIM= LIMIT(1)
      END IF
C
C     calculate unit algal respiration rate; res is expressed in
C     units of per interval; alr20 is the respiration rate at 20
C     degrees c
      RES= ALR20*TW/20.
C
C     save limiting factor character string as real
      READ (LIM,1000) LIMR
C
      RETURN
      END
C
C
C
      SUBROUTINE   BALDTH
     I                    (NSFG,NO3,TAM,PO4,PALDH,NALDH,ALDL,
     I                     ALDH,MBAL,DOX,ANAER,OXALD,BAL,DEPCOR,
     O                     DTHBAL)
C
C     + + + PURPOSE + + +
C     Calculate benthic algae death
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    NSFG
      REAL       NO3,TAM,PO4,PALDH,NALDH,ALDL,ALDH,MBAL,
     $           DOX,ANAER,OXALD,BAL,DEPCOR,DTHBAL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NSFG   - ???
C     NO3    - dissolved nitrate concentration in mg/l
C     TAM    - total ammonia (nh3 + nh4) in mg n/l
C     PO4    - ???
C     PALDH  - ???
C     NALDH  - ???
C     ALDL   - ???
C     ALDH   - ???
C     MBAL   - ???
C     DOX    - dissolved oxygen concentration in mg/l
C     ANAER  - ???
C     OXALD  - ???
C     BAL    - ???
C     DEPCOR - ???
C     DTHBAL - ???
C
C     + + + LOCAL VARIABLES + + +
      REAL       NIT,ALD,SLOF,BALMAX
C
C     + + + END SPECIFICATIONS + + +
C
C     determine whether to use high or low unit death rate; all
C     unit death rates are expressed in units of per interval
C
C     determine available inorganic nitrogen pool for test of
C     nutrient scarcity
      IF (NSFG .NE. 0) THEN
        NIT= NO3 + TAM
      ELSE
        NIT= NO3
      END IF
C
      IF (PO4 .GT. PALDH .AND. NIT .GT. NALDH) THEN
C       unit death rate is not incremented by nutrient scarcity
C       check for benthic algae overcrowding
        BALMAX= MBAL*DEPCOR
C
        IF (BAL .LT. BALMAX) THEN
C         unit death rate is not incremented by benthic algae
C         overcrowding
          ALD = ALDL
          SLOF= 0.0
        ELSE
C         augment unit death rate to account for benthic algae
C         overcrowding; set bal value equal to maximum; calculate
C         amount of benthic algae in excess of mbal; these benthic
C         algae are added to aldth
          ALD = ALDH
          SLOF= BAL - BALMAX
          BAL = BALMAX
        END IF
      ELSE
C       augment unit death rate to account for nutrient scarcity
        ALD= ALDH
      END IF
C
      IF (DOX .LT. ANAER) THEN
C       conditions are anaerobic, augment unit death rate
        ALD= ALD + OXALD
      END IF
C
C     use unit death rate to compute death rate; dthbal is expressed
C     as umoles of phosphorus per liter per interval
      DTHBAL= (ALD*BAL) + SLOF
C
      RETURN
      END
C
C
C
      SUBROUTINE   BALRX
     I                   (BALLIT,TW,TALGRL,TALGRH,TALGRM,MALGR,CMMP,
     I                    CMMNP,TAMFG,AMRFG,NSFG,CMMN,CMMLT,DELT60,
     I                    CFLIT,ALR20,CVBPN,PHFG,DECFG,CVBPC,PALDH,
     I                    NALDH,ALDL,ALDH,ANAER,OXALD,CFBALG,CFBALR,
     I                    ALNPR,CVBO,REFR,CVNRBO,CVPB,MBAL,DEPCOR,
     I                    LIMIT,CVBCL,CO2,NMINGR,PMINGR,CMINGR,LMINGR,
     I                    NMINC,
     M                    PO4,NO3,TAM,DOX,ORN,ORP,ORC,BOD,BENAL,
     O                    LIMBAL,BACO2,BALCLA,DOBALG,BODBAL,TAMBAL,
     O                    NO3BAL,PO4BAL,BALGRO,BDTH,BALORN,BALORP,
     O                    BALORC)
C
C     + + + PURPOSE + + +
C     Simulate behavior of benthic algae in units of umoles P per
C     liter; these units are used internally within BALRX so that
C     algal subroutines may be shared by PHYTO and BALRX; externally,
C     the benthic algae population is expressed in terms of areal
C     mass, since the population is resident entirely on the
C     bottom surface
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     TAMFG,AMRFG,NSFG,PHFG,DECFG
      REAL        BALLIT,TW,TALGRL,TALGRH,TALGRM,MALGR,CMMP,CMMNP,
     $            CMMN,CMMLT,DELT60,CFLIT,ALR20,CVBPN,CVBPC,PALDH,
     $            NALDH,ALDL,ALDH,ANAER,OXALD,CFBALG,CFBALR,ALNPR,
     $            CVBO,REFR,CVNRBO,CVPB,MBAL,DEPCOR,CVBCL,CO2,NMINGR,
     $            PMINGR,CMINGR,LMINGR,NMINC,PO4,NO3,TAM,DOX,ORN,ORP,
     $            ORC,BOD,BENAL,BACO2,BALCLA,DOBALG,BODBAL,TAMBAL,
     $            NO3BAL,PO4BAL,BALGRO,BDTH,BALORN,BALORP,BALORC
      REAL        LIMBAL
      CHARACTER*4 LIMIT(7)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     BALLIT - ???
C     TW     - water temperature in degrees C
C     TALGRL - ???
C     TALGRH - ???
C     TALGRM - ???
C     MALGR  - ???
C     CMMP   - ???
C     CMMNP  - ???
C     TAMFG  - ???
C     AMRFG  - ???
C     NSFG   - ???
C     CMMN   - ???
C     CMMLT  - ???
C     DELT60 - simulation time interval in hours
C     CFLIT  - ???
C     ALR20  - ???
C     CVBPN  - ???
C     PHFG   - ???
C     DECFG  - ???
C     CVBPC  - ???
C     PALDH  - ???
C     NALDH  - ???
C     ALDL   - ???
C     ALDH   - ???
C     ANAER  - ???
C     OXALD  - ???
C     CFBALG - ???
C     CFBALR - ???
C     ALNPR  - ???
C     CVBO   - ???
C     REFR   - ???
C     CVNRBO - ???
C     CVPB   - ???
C     MBAL   - ???
C     DEPCOR - ???
C     LIMIT  - ???
C     CVBCL  - ???
C     CO2    - ???
C     NMINGR - minimum nitrate concentration for algal growth
C     PMINGR - minimum phosphate concentration for algal growth
C     CMINGR - minimum CO2-carbon concentration for algal growth
C     LMINGR - minimum light intensity for algal growth
C     PO4    - ???
C     NO3    - dissolved nitrate concentration in mg/l
C     TAM    - total ammonia (nh3 + nh4) in mg n/l
C     DOX    - dissolved oxygen concentration in mg/l
C     ORN    - ???
C     ORP    - ???
C     ORC    - ???
C     BOD    - ???
C     BENAL  - ???
C     LIMBAL - ???
C     BACO2  - ???
C     BALCLA - ???
C     DOBALG - ???
C     BODBAL - ???
C     TAMBAL - ???
C     NO3BAL - ???
C     PO4BAL - ???
C     BALGRO - ???
C     BDTH   - ???
C     BALORN - ???
C     BALORP - ???
C     BALORC - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I0
      REAL       BAL,GRO,DTHBAL,MINBAL,RES,GROBAL,GRTOTN
C
C     + + + EXTERNALS + + +
      EXTERNAL   ALGRO,GROCHK,BALDTH,ORGBAL,NUTRUP
C
C     + + + END SPECIFICATIONS + + +
C
      I0= 0
C
C     convert benal to units of umoles phosphorus/l (bal) for
C     internal calculations
      BAL= (BENAL/CVPB)*DEPCOR
C
C     compute unit growth and respiration rates for benthic algae;
C     determine growth limiting factor
      CALL ALGRO
     I           (BALLIT,PO4,NO3,TW,TALGRL,TALGRH,TALGRM,MALGR,CMMP,
     I            CMMNP,TAMFG,AMRFG,TAM,NSFG,CMMN,CMMLT,ALR20,
     I            CFLIT,DELT60,LIMIT,NMINGR,PMINGR,LMINGR,
     O            LIMBAL,GRO,RES)
C
C     calculate net growth rate of algae; grobal is expressed as
C     umoles phosphorus per liter per interval; benthic algae growth
C     will be expressed in terms of volume rather than area for the
C     duration of the subroutines subordinate to balrx; the output
C     values for benthic algae are converted to either mg biomass per
C     sq meter or mg chla per sq meter, whichever the user
C     specifies; cfbalg and cfbalr are the specified ratio of benthic
C     algae growth rate to phytoplankton growth rate and ratio of
C     benthic algae respiration rate to phytoplankton respiration
C     rate, respectively
C
      GROBAL= (GRO*CFBALG - RES*CFBALR)*BAL
C
      IF (GROBAL .GT. 0.0) THEN
C       adjust growth rate to account for limitations imposed by
C       availability of required nutrients
        GRTOTN= GROBAL
        CALL GROCHK (PO4,NO3,TAM,PHFG,DECFG,CO2,CVBPC,CVBPN,NSFG,
     I               NMINGR,PMINGR,CMINGR,I0,GRTOTN,
     M               GROBAL)
      END IF
C
C     calculate benthic algae death
      CALL BALDTH
     I            (NSFG,NO3,TAM,PO4,PALDH,NALDH,ALDL,
     I             ALDH,MBAL,DOX,ANAER,OXALD,BAL,DEPCOR,
     O             DTHBAL)
C
C     determine the new benthic algae population
      BAL= BAL + GROBAL
C
C     adjust net growth rate, if necessary, so that population
C     does not fall below minimum level
      MINBAL= .0001*DEPCOR
      IF (BAL .LT. MINBAL) THEN
        GROBAL= GROBAL+ (MINBAL - BAL)
        BAL   = MINBAL
      END IF
      BAL= BAL - DTHBAL
C
C     adjust death rate, if necessary, so that population does not
C     drop below minimum level
      IF (BAL .LT. MINBAL) THEN
        DTHBAL= DTHBAL - (MINBAL - BAL)
        BAL   = MINBAL
      END IF
C
C     update do state variable to account for net effect of benthic
C     algae photosynthesis and respiration
      DOBALG= CVPB*CVBO*GROBAL
C     dox   = dox + (cvpb*cvbo*grobal)
      IF (DOX .GT. -DOBALG) THEN
        DOX= DOX + DOBALG
      ELSE
        DOBALG= -DOX
        DOX   = 0.0
      END IF
C
C     calculate amount of refractory organic constituents which result
C     from benthic algae death
      BALORN= REFR*DTHBAL*CVBPN*.014
      BALORP= REFR*DTHBAL*.031
      BALORC= REFR*DTHBAL*CVBPC*.012
C
C     calculate amount of nonrefractory organics (bod) which result
C     from benthic algae death
      BODBAL= CVNRBO*CVPB*DTHBAL
C
C     perform materials balance resulting from benthic algae death
      CALL ORGBAL
     I            (BALORN,BALORP,BALORC,BODBAL,
     M             ORN,ORP,ORC,BOD)
C
C     perform materials balance resulting from uptake of nutrients
C     by benthic algae
      CALL NUTRUP
     I            (GROBAL,NSFG,CVBPN,ALNPR,CVBPC,PHFG,DECFG,NMINC,
     M             PO4,TAM,NO3,
     O             BACO2,TAMBAL,NO3BAL,PO4BAL)
      BACO2= -BACO2
C
C     convert bal back to external units; benal is expressed as
C     mg biomass/m2 and balcla is expressed as ug chlorophyll a/m2
      BENAL = (BAL*CVPB)/DEPCOR
      BALGRO= (GROBAL*CVPB)/DEPCOR
      BDTH=   (DTHBAL*CVPB)/DEPCOR
      BALCLA=  BENAL*CVBCL
C
      RETURN
      END
C
C
C
      SUBROUTINE   GROCHK
     I                    (PO4,NO3,TAM,PHFG,DECFG,CO2,CVBPC,CVBPN,NSFG,
     I                     NMINGR,PMINGR,CMINGR,NFIXFG,GRTOTN,
     M                     GROW)
C
C     + + + PURPOSE + + +
C     Check whether computed growth rate demands too much of any
C     nutrient; adjust growth rate, if necessary, so that at least
C     minimum allowed level of each nutrient remains after growth
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    PHFG,DECFG,NSFG,NFIXFG
      REAL       PO4,NO3,TAM,CO2,CVBPC,CVBPN,NMINGR,PMINGR,CMINGR,
     $           GRTOTN,GROW
C
C     + + + ARGUMENT DEFINITIONS + + +
C     PO4    - ???
C     NO3    - dissolved nitrate concentration in mg/l
C     TAM    - total ammonia (nh3 + nh4) in mg n/l
C     PHFG   - ???
C     DECFG  - ???
C     CO2    - ???
C     CVBPC  - ???
C     CVBPN  - ???
C     NSFG   - ???
C     NMINGR - minimum nitrate concentration for algal growth
C     PMINGR - minimum phosphate concentration for algal growth
C     CMINGR - minimum CO2-carbon concentration for algal growth
C     NFIXFG - flag indicating if N-fixation occurs
C     GRTOTN - cumulative algal growth that affects available N 
C     GROW   - ???
C
C     + + + LOCAL VARIABLES + + +
      REAL       UPLIMP,UPLIMN,UPLIMC,UPLIM
C
C     + + + INTRINSICS + + +
      INTRINSIC  AMIN1
C
C     + + + END SPECIFICATIONS + + +
C
C     calculate growth rate which results in minimum free po4
C     remaining after growth; uplimp is expressed as umoles
C     phosphorus per liter per interval
      UPLIMP= (PO4 - PMINGR)*32.29
C
C     calculate growth rate which results in minimum free
C     inorganic nitrogen remaining after growth; uplimn is expressed
C     as umoles phosphorus per interval
      IF (NSFG .EQ. 0) THEN
C       tam is not considered as a possible nutrient
        UPLIMN= (NO3 - NMINGR)*71.43/CVBPN
      ELSE
        UPLIMN= (NO3 + TAM - NMINGR)*71.43/CVBPN
      END IF
C
      UPLIMC= 1.0E30
      IF (PHFG .NE. 0 .AND. PHFG .NE. 2 .AND. DECFG .EQ. 0) THEN
C       phcarb is on, and co2 is being considered as a possible
C       limiting nutrient to algal growth
        IF (CO2 .GE. 0.0) THEN
C         calculate growth rate which results in minimum free
C         carbon dioxide remaining after growth; uplimc is expressed
C         as umoles phosphorus per liter per interval
          UPLIMC= (CO2 - CMINGR)*83.33/CVBPC
        END IF
      END IF
C
C     calculate difference between available nutrient concentrations and
C     nutrients needed for modeled growth; amount needed for growth may differ 
C     for N if nitrogen-fixation occurs in any of the algal types
      UPLIMP = UPLIMP-GROW
      UPLIMN = UPLIMN-GRTOTN
      UPLIMC = UPLIMC-GROW
C
C     check that calculated growth does not result in less than
C     minimum allowed concentrations of orthophosphate, inorganic
C     nitrogen, or carbon dioxide; if it does, adjust growth
C
      IF (NFIXFG .NE. 1) THEN
C       n-fixation is not occurring for this algal type
        UPLIM= AMIN1(UPLIMP,UPLIMN,UPLIMC)
      ELSE
C       n-fixation is occurring, nitrogen does not limit growth
        UPLIM= AMIN1(UPLIMP,UPLIMC)
      END IF
      IF (UPLIM .LT. 0.0) THEN
C       reduce growth rate to limit
        GROW= GROW + UPLIM
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   LITRCH
     I                    (INLIT,EXTB,EXTCLA,EXTSED,AVDEPE,BALDEP,
     I                     PHYFG,BALFG,
     O                     PHYLIT,BALLIT,CFLIT)
C
C     + + + PURPOSE + + +
C     Calculate light correction factor to algal growth (cflit);
C     determine amount of light available to phytoplankton and benthic
C     algae
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    PHYFG,BALFG
      REAL       INLIT,EXTB,EXTCLA,EXTSED,AVDEPE,BALDEP
      REAL       PHYLIT,BALLIT,CFLIT
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INLIT  - light in langleys per minute
C     EXTB   - base extinction coefficient for light in per foot
C     EXTCLA - phytoplankton self-shading ext. coeff. in per foot
C     EXTSED - suspended sediment extinction coefficient in per foot
C     AVDEPE - depth in feet
C     BALDEP - adjusted depth for benthic algae in feet
C     PHYFG  - phytoplankton simulated?
C     BALFG  - benthic algae simulated?
C     PHYLIT - light avail. to phytoplankton in langleys per minute
C     BALLIT - light avail. to benthic algae in langleys per minute
C     CFLIT  - light correction factor to algal growth
C
C     + + + LOCAL VARIABLES + + +
      REAL       EXTCO,EUDEP,LN01
C     4.60517 = minus natural log 0.01
      PARAMETER  (LN01 = 4.60517)
C
C
C     + + + INTRINSICS + + +
      INTRINSIC  EXP,AMIN1
C
C     + + + END SPECIFICATIONS + + +
C
      IF (INLIT .GT. 0.0) THEN
C       calculate extinction of light based on the base extinction
C       coefficient of the water incremented by self-shading effects
C       of phytoplankton and light extinction due to total sediment
C       suspension
        EXTCO = EXTB + EXTCLA + EXTSED
C
C       calculate euphotic depth; euphotic depth is the distance,
C       in feet, below the surface of the water body at which one
C       percent of incident light is present
        EUDEP = LN01/EXTCO
C
        IF (EUDEP .LT. AVDEPE) THEN
C         calculate fraction of layer which is contained in the
C         euphotic zone; this fraction, cflit, will be multiplied
C         by calculated growth in algro to assure that growth only
C         occurs in the euphotic zone
          CFLIT = EUDEP/AVDEPE
          IF (CFLIT .LT. 0.0001) THEN
            CFLIT = 0.0
          END IF
        ELSE
          CFLIT = 1.0
        END IF
C
        IF (PHYFG .NE. 0) THEN
C         calculate amount of light available to phytoplankton; all
C         phytoplankton are assumed to be at mid-depth of the reach;
C         light is expressed as langleys per minute
          PHYLIT = INLIT*EXP(-EXTCO*(0.5*AMIN1(EUDEP,AVDEPE)))
          IF (PHYLIT .LT. 0.0001) THEN
            PHYLIT = 0.0
          END IF
        END IF
C
        IF (BALFG .NE. 0) THEN
C         calculate amount of light available to benthic algae; all
C         benthic algae are assumed to be at the bottom depth of the
C         reach;light is expressed as langleys per minute
          BALLIT = INLIT*EXP(-EXTCO*BALDEP)
          IF (BALLIT .LT. 0.0001) THEN
            BALLIT = 0.0
          END IF
        END IF
      ELSE
C       there is no incident solar radiation; algal growth cannot
C       occur
        CFLIT  = 0.0
        PHYLIT = 0.0
        BALLIT = 0.0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   NUTRUP
     I                    (GROW,NSFG,CVBPN,ALNPR,CVBPC,PHFG,DECFG,
     I                     NMINC,
     M                     PO4,TAM,NO3,
     O                     ALCO2,TAMALG,NO3ALG,PO4ALG)
C
C     + + + PURPOSE + + +
C     Perform materials balance for transformation from inorganic to
C     organic material; uptake of PO4, NO3, TAM, and CO2 are considered
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    NSFG,PHFG,DECFG
      REAL       GROW,CVBPN,ALNPR,CVBPC,NMINC,PO4,TAM,NO3,ALCO2,
     $           TAMALG,NO3ALG,PO4ALG
C
C     + + + ARGUMENT DEFINITIONS + + +
C     GROW   - ???
C     NSFG   - ???
C     CVBPN  - ???
C     ALNPR  - ???
C     CVBPC  - ???
C     PHFG   - ???
C     DECFG  - ???
C     NMINC  - minimum concentration of inorganic N species in mg N/L
C     PO4    - ???
C     TAM    - total ammonia (nh3 + nh4) in mg n/l
C     NO3    - dissolved nitrate concentration in mg/l
C     ALCO2  - ???
C     TAMALG - ???
C     NO3ALG - ???
C     PO4ALG - ???
C
C     + + + LOCAL VARIABLES + + +
      REAL       GROWN,ALTAM,ALNO3,NO3LIM,TAMLIM,TAMS,NO3S
C
C     + + + END SPECIFICATIONS + + +
C
C     calculate po4 balance subsequent to algal uptake or release;
C     .031 is the conversion from umoles p per liter to mg of p per
C     liter
      PO4   = PO4 - .031*GROW
      PO4ALG= -.031*GROW
C
      TAMALG= 0.0
      IF (NSFG .NE. 0) THEN
C       calculate tam balance subsequent to algal uptake or release
C       express calculated growth rate in terms of equivalent
C       nitrogen; grown is expressed as umoles nitrogen per interval
        GROWN= GROW*CVBPN
C
        IF (GROW .LT. 0.0) THEN
C         algal respiration exceeds growth; nitrogen released by
C         respiration is released in the form of tam; no uptake or
C         release of no3 occurs
          ALTAM= GROWN
          ALNO3= 0.0
        ELSE
C         calculate amount of n uptake which is no3 and amount which
C         is tam
          ALNO3= ALNPR*GROWN
          ALTAM= GROWN - ALNO3
C         check that computed uptake of no3 does not consume more
C         than 99 percent of available free no3; if it does, satisfy
C         excess demand with free tam; no3lim is expressed as umoles
C         n per liter per interval
          NO3LIM= 70.72*NO3
C
          IF (ALNO3 .GT. NO3LIM) THEN
            ALTAM= ALTAM + ALNO3 - NO3LIM
            ALNO3= NO3LIM
          ELSE
C           check that calculated uptake of tam does not consume
C           more than 99 percent of available free tam; if it does,
C           satisfy excess demand with free no3; tamlim is expressed
C           as umoles n per liter per interval
            TAMLIM= 70.72*TAM
C
            IF (ALTAM .GT. TAMLIM) THEN
              ALNO3= ALNO3 + ALTAM - TAMLIM
              ALTAM= TAMLIM
            ELSE
C             calculated uptake of inorganic nitrogen is acceptable
            END IF
          END IF
        END IF
C
C       calculate net uptake or release of tam by algae; .014 is
C       the conversion from umoles of n per liter per interval to
C       mg n per liter per interval
        TAMS  = TAM
        TAMALG= -0.014*ALTAM
        TAM   = TAM - .014*ALTAM
        IF (TAM .LT. NMINC) THEN
          TAMALG= -TAMS
        END IF
        IF (TAM .LT. NMINC) THEN
          TAM= 0.0
        END IF
      ELSE
C       all inorganic n is in the form of no3
        ALNO3= GROW*CVBPN
      END IF
C
C     calculate no3 balance subsequent to algal uptake or release;
C     eliminate insignificant values of no3
      NO3S  = NO3
      NO3ALG= -.014*ALNO3
      NO3   = NO3 - .014*ALNO3
      IF (NO3 .LT. NMINC) THEN
        NO3ALG= -NO3S
      END IF
C
      IF (NO3 .LT. NMINC) THEN
        NO3= 0.0
      END IF
C
      IF (PHFG .NE. 0 .AND. DECFG .EQ. 0) THEN
C       calculate amount of algal uptake of co2; alco2 is expressed
C       as mg co2-c/liter
        ALCO2= GROW*CVBPC*.012
      ELSE
        ALCO2= 0.0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   ORGBAL
     I                    (DTHORN,DTHORP,DTHORC,DTHBOD,
     M                     ORN,ORP,ORC,BOD)
C
C     + + + PURPOSE + + +
C     Perform materials balance for transformation from living to
C     dead organic material
C
C     + + + DUMMY ARGUMENTS + + +
      REAL       DTHORN,DTHORP,DTHORC,DTHBOD,ORN,ORP,ORC,BOD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DTHORN - ???
C     DTHORP - ???
C     DTHORC - ???
C     DTHBOD - ???
C     ORN    - ???
C     ORP    - ???
C     ORC    - ???
C     BOD    - ???
C
C     + + + END SPECIFICATIONS + + +
C
C     calculate dead refractory organic nitrogen balance
C     subsequent to plankton death; plankton death may be
C     either algal death, zooplankton death, or phytoplankton
C     filtered by zooplankton but not assimilated
      ORN= ORN + DTHORN
C
C     calculate dead refractory organic phosphorus balance
C     subsequent to plankton death
      ORP= ORP + DTHORP
C
C     calculate dead refractory organic carbon balance
C     subsequent to plankton death
      ORC= ORC + DTHORC
C
C     calculate bod balance subsequent to plankton death
      BOD= BOD + DTHBOD
C
      RETURN
      END
C
C
C
      SUBROUTINE   PHYDTH
     I                    (NSFG,NO3,TAM,PO4,PALDH,NALDH,PHYCLA,CLALDH,
     I                     ALDL,ALDH,DOX,ANAER,OXALD,STC,
     O                     DTHPHY)
C
C     + + + PURPOSE + + +
C     Calculate phytoplankton death
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    NSFG
      REAL       NO3,TAM,PO4,PALDH,NALDH,CLALDH,ALDL,ALDH,
     $           DOX,ANAER,OXALD,STC,DTHPHY,PHYCLA
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NSFG   - ???
C     NO3    - dissolved nitrate concentration in mg/l
C     TAM    - total ammonia (nh3 + nh4) in mg n/l
C     PO4    - ???
C     PALDH  - ???
C     NALDH  - ???
C     PHYCLA - ???
C     CLALDH - ???
C     ALDL   - ???
C     ALDH   - ???
C     DOX    - dissolved oxygen concentration in mg/l
C     ANAER  - ???
C     OXALD  - ???
C     STC    - ???
C     DTHPHY - ???
C
C     + + + LOCAL VARIABLES + + +
      REAL       NIT,ALD
C
C     + + + END SPECIFICATIONS + + +
C
C     determine whether to use high or low unit death rate; all unit
C     death rates are expressed in units of per interval
C     determine available inorganic nitrogen pool for test of nutrient
C     scarcity
      IF (NSFG .NE. 0) THEN
        NIT= NO3 + TAM
      ELSE
        NIT= NO3
      END IF
C
      IF (PO4 .GT. PALDH .AND. NIT .GT. NALDH) THEN
C       unit death rate is not incremented by nutrient scarcity
C       check for phytoplankton overcrowding
C
        IF (PHYCLA .LT. CLALDH) THEN
C         unit death rate is not incremented by phytoplankton
C         overcrowding
          ALD= ALDL
        ELSE
C         augment unit death rate to account for overcrowding
          ALD= ALDH
        END IF
C
      ELSE
C       augment unit death rate to account for nutrient scarcity
        ALD= ALDH
C
      END IF
C
C     augment unit death rate if conditions are anaerobic
      IF (DOX .LT. ANAER) THEN
        ALD= ALD + OXALD
      END IF
C
C     use unit death rate to compute death rate; aldth is expressed
C     as umoles of phosphorus per liter per interval
      DTHPHY= ALD*STC
C
      RETURN
      END
C
C
C
      SUBROUTINE   PHYRX
     I                   (PHYLIT,TW,TALGRL,TALGRH,TALGRM,MALGR,CMMP,
     I                    CMMNP,TAMFG,AMRFG,NSFG,CMMN,CMMLT,DELT60,
     I                    CFLIT,ALR20,CVBPN,PHFG,DECFG,CVBPC,PALDH,
     I                    NALDH,CLALDH,ALDL,ALDH,ANAER,OXALD,ALNPR,
     I                    CVBO,REFR,CVNRBO,CVPB,CVBCL,LIMIT,CO2,
     I                    NMINGR,PMINGR,CMINGR,LMINGR,NMINC,
     M                    PO4,NO3,TAM,DOX,ORN,ORP,ORC,BOD,PHYTO,
     O                    LIMPHY,PYCO2,PHYCLA,DOPHY,BODPHY,TAMPHY,
     O                    NO3PHY,PO4PHY,PHDTH,PHGRO,PHYORN,PHYORP,
     O                    PHYORC)
C
C     + + + PURPOSE + + +
C     Simulate behavior of phytoplankton, as standing crop, in units
C     of umoles P per liter
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     TAMFG,AMRFG,NSFG,PHFG,DECFG
      REAL        PHYLIT,TW,TALGRL,TALGRH,TALGRM,MALGR,CMMP,CMMNP,
     $            CMMN,CMMLT,DELT60,CFLIT,ALR20,CVBPN,CVBPC,PALDH,
     $            NALDH,CLALDH,ALDL,ALDH,ANAER,OXALD,ALNPR,CVBO,REFR,
     $            CVNRBO,CVPB,CVBCL,CO2,NMINGR,PMINGR,CMINGR,LMINGR,
     $            NMINC,PO4,NO3,TAM,DOX,ORN,ORP,ORC,BOD,PHYTO,LIMPHY,
     $            PYCO2,PHYCLA,DOPHY,BODPHY,TAMPHY,NO3PHY,PO4PHY,
     $            PHDTH,PHGRO,PHYORN,PHYORP,PHYORC
      CHARACTER*4 LIMIT(7)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     PHYLIT - ???
C     TW     - water temperature in degrees C
C     TALGRL - ???
C     TALGRH - ???
C     TALGRM - ???
C     MALGR  - ???
C     CMMP   - ???
C     CMMNP  - ???
C     TAMFG  - ???
C     AMRFG  - ???
C     NSFG   - ???
C     CMMN   - ???
C     CMMLT  - ???
C     DELT60 - simulation time interval in hours
C     CFLIT  - ???
C     ALR20  - ???
C     CVBPN  - ???
C     PHFG   - ???
C     DECFG  - ???
C     CVBPC  - ???
C     PALDH  - ???
C     NALDH  - ???
C     CLALDH - ???
C     ALDL   - ???
C     ALDH   - ???
C     ANAER  - ???
C     OXALD  - ???
C     ALNPR  - ???
C     CVBO   - ???
C     REFR   - ???
C     CVNRBO - ???
C     CVPB   - ???
C     CVBCL  - ???
C     LIMIT  - ???
C     CO2    - ???
C     NMINGR - minimum nitrate concentration for algal growth
C     PMINGR - minimum phosphate concentration for algal growth
C     CMINGR - minimum CO2-carbon concentration for algal growth
C     LMINGR - minimum light intensity for algal growth
C     NMINC  - minimum concentration of inorganic N species in mg N/L
C     PO4    - ???
C     NO3    - dissolved nitrate concentration in mg/l
C     TAM    - total ammonia (nh3 + nh4) in mg n/l
C     DOX    - dissolved oxygen concentration in mg/l
C     ORN    - ???
C     ORP    - ???
C     ORC    - ???
C     BOD    - ???
C     PHYTO  - ???
C     LIMPHY - ???
C     PYCO2  - ???
C     PHYCLA - ???
C     DOPHY  - ???
C     BODPHY - ???
C     TAMPHY - ???
C     NO3PHY - ???
C     PO4PHY - ???
C     PHDTH  - ???
C     PHGRO  - ???
C     PHYORN - ???
C     PHYORP - ???
C     PHYORC - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I0
      REAL       GRO,PHYBD,RES,STC,GROPHY,DTHPHY,GRTOTN
C
C     + + + EXTERNALS + + +
      EXTERNAL   ALGRO,GROCHK,PHYDTH,ORGBAL,NUTRUP
C
C     + + + END SPECIFICATIONS + + +
C
      I0= 0
C
C     convert phyto to units of umoles phosphorus (stc) and
C     ug chlorophyll a/l (phycla) for internal calculations
      STC   = PHYTO/CVPB
      PHYCLA= PHYTO*CVBCL
C
C     compute unit growth and respiration rates for phytoplankton;
C     determine growth limiting factor
      CALL ALGRO
     I           (PHYLIT,PO4,NO3,TW,TALGRL,TALGRH,TALGRM,MALGR,CMMP,
     I            CMMNP,TAMFG,AMRFG,TAM,NSFG,CMMN,CMMLT,ALR20,
     I            CFLIT,DELT60,LIMIT,NMINGR,PMINGR,LMINGR,
     O            LIMPHY,GRO,RES)
C
C     calculate net growth rate of phytoplankton; grophy is
C     expressed as umol phosphorus per liter per interval
      GROPHY= (GRO - RES)*STC
C
      IF (GROPHY .GT. 0.0) THEN
C       adjust growth rate to account for limitations imposed by
C       availability of required nutrients
        GRTOTN= GROPHY
        CALL GROCHK (PO4,NO3,TAM,PHFG,DECFG,CO2,CVBPC,CVBPN,NSFG,
     I               NMINGR,PMINGR,CMINGR,I0,GRTOTN,
     M               GROPHY)
C
      END IF
C
C     calculate phytoplankton death
      CALL PHYDTH
     I            (NSFG,NO3,TAM,PO4,PALDH,NALDH,PHYCLA,CLALDH,
     I             ALDL,ALDH,DOX,ANAER,OXALD,STC,
     O             DTHPHY)
C
C     determine the new phytoplankton population
      STC= STC + GROPHY
C
C     adjust net growth rate, if necessary, so population does not
C     fall below minimum level
      IF (STC .LT. .0025) THEN
        GROPHY= GROPHY - (.0025 - STC)
        STC   = .0025
      END IF
      STC= STC - DTHPHY
C
C     adjust death rate, if necessary, so that population does
C     not drop below minimum level
      IF (STC .LT. .0025) THEN
        DTHPHY= DTHPHY - (.0025 - STC)
        STC   = .0025
      END IF
C
C     update do state variable to account for net effect of
C     phytoplankton photosynthesis and respiration
      DOPHY= (CVPB*CVBO*GROPHY)
C     dox  = dox + (cvpb*cvbo*grophy)
      IF (DOX .GT. -DOPHY) THEN
        DOX= DOX + DOPHY
      ELSE
        DOPHY= -DOX
        DOX  = 0.0
      END IF
C
C     calculate amount of refractory organic constituents which
C     result from phytoplankton death
      PHYORN= REFR*DTHPHY*CVBPN*.014
      PHYORP= REFR*DTHPHY*.031
      PHYORC= REFR*DTHPHY*CVBPC*.012
C
C     calculate amount of nonrefractory organics (bod) which result
C     from phytoplankton death
      PHYBD = CVNRBO*CVPB*DTHPHY
      BODPHY= PHYBD
C
C     perform materials balance resulting from phytoplankton death
      CALL ORGBAL
     I            (PHYORN,PHYORP,PHYORC,PHYBD,
     M             ORN,ORP,ORC,BOD)
C
C     perform materials balance resulting from uptake of nutrients
C     by phytoplankton
      CALL NUTRUP
     I            (GROPHY,NSFG,CVBPN,ALNPR,CVBPC,PHFG,DECFG,NMINC,
     M             PO4,TAM,NO3,
     O             PYCO2,TAMPHY,NO3PHY,PO4PHY)
      PYCO2= -PYCO2
C
C     convert stc to units of mg biomass/l (phyto) and
C     ug chlorophyll a/l (phycla)
      PHYTO = STC*CVPB
      PHGRO = GROPHY*CVPB
      PHDTH = DTHPHY*CVPB
      PHYCLA= PHYTO*CVBCL
C
      RETURN
      END
C
C
C
      SUBROUTINE   PLKACC
     I                    (FRMROW,TOROW)
C
C     + + + PURPOSE + + +
C     Accumulate fluxes in subroutine group plank for printout
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    FRMROW,TOROW
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FRMROW - row containing incremental flux accumulation
C     TOROW  - flux row to be incremented
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PLANK2 + + +
      INCLUDE    'crhpl.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER I3,I5,I
C
C     + + + EXTERNALS + + +
      EXTERNAL  ACCVEC
C
C     + + + DATA INITIALIZATIONS + + +
      DATA I3,I5/3,5/
C
C     + + + END SPECIFICATIONS + + +
C
      IF (PHYFG .EQ. 1) THEN
C       handle flux groups dealing with reach-wide variables
        PKIF(1,TOROW) = PKIF(1,TOROW) + PKIF(1,FRMROW)
        PKCF1(1,TOROW)= PKCF1(1,TOROW) + PKCF1(1,FRMROW)
C
        CALL ACCVEC (I5,PKCF5(1,FRMROW),
     M               PKCF5(1,TOROW))
C
        IF (NEXITS .GT. 1) THEN
C         handle flux groups dealing with individual exit gates
          CALL ACCVEC (NEXITS,PKCF2(1,1,FRMROW),
     M                 PKCF2(1,1,TOROW))
        END IF
C
        IF (ZOOFG .EQ. 1) THEN
C         handle flux groups dealing with reach-wide variables
          PKIF(2,TOROW) = PKIF(2,TOROW) + PKIF(2,FRMROW)
          PKCF1(2,TOROW)= PKCF1(2,TOROW) + PKCF1(2,FRMROW)
C
          CALL ACCVEC (I3,PKCF6(1,FRMROW),
     M                 PKCF6(1,TOROW))
C
          IF (NEXITS .GT. 1) THEN
C           handle flux groups dealing with individual exit gates
            CALL ACCVEC
     I                  (NEXITS,PKCF2(1,2,FRMROW),
     M                   PKCF2(1,2,TOROW))
          END IF
        END IF
      END IF
C
      IF (BALFG .GE. 1) THEN
C       handle flux groups dealing with reach-wide variables
C
        DO 5 I= 1, NUMBAL
          CALL ACCVEC (I3,PKCF7(1,I,FRMROW),
     M                 PKCF7(1,I,TOROW))
 5      CONTINUE
C
        CALL ACCVEC (I3,TPKCF7(1,FRMROW),
     M               TPKCF7(1,TOROW))
      END IF
C
C     accumulate fluxes of orn, orp, orc, and summaries
      CALL ACCVEC (I3,PKIF(3,FRMROW),
     M             PKIF(3,TOROW))
      CALL ACCVEC (I3,PKCF1(3,FRMROW),
     M             PKCF1(3,TOROW))
      CALL ACCVEC (I5,TPKIF(1,FRMROW),
     M             TPKIF(1,TOROW))
      CALL ACCVEC (I5,TPKCF1(1,FRMROW),
     M             TPKCF1(1,TOROW))
      CALL ACCVEC (I3,PKCF3(1,FRMROW),
     M             PKCF3(1,TOROW))
      CALL ACCVEC (I3,PKCF4(1,FRMROW),
     M             PKCF4(1,TOROW))
C
      CALL ACCVEC (I5,PKCF8(1,FRMROW),
     M             PKCF8(1,TOROW))
      CALL ACCVEC (I5,PKCF9(1,FRMROW),
     M             PKCF9(1,TOROW))
      CALL ACCVEC (I5,PKCF10(1,FRMROW),
     M             PKCF10(1,TOROW))
C
      IF (NEXITS .GT. 1) THEN
C       handle flux groups dealing with individual exit gates
        DO 10 I= 3, 5
          CALL ACCVEC
     I                (NEXITS,PKCF2(1,I,FRMROW),
     M                 PKCF2(1,I,TOROW))
 10     CONTINUE
        DO 20 I= 1, 5
          CALL ACCVEC
     I                (NEXITS,TPKCF2(1,I,FRMROW),
     M                 TPKCF2(1,I,TOROW))
 20     CONTINUE
C
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PLKPRT
     I                    (LEV,PRINTU,FACTA,FACTB,FLUXID,UNITFG,BINU)
C
C     + + + PURPOSE + + +
C     Convert quantities from internal to external units, and print
C     Out results
C     Note: local arrays have same dimensions as corresponding arrays
C       in osv, except for dropping of dimension lev, where applicable
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
C     + + + COMMON BLOCKS- SCRTCH, VERSION PLANK2 + + +
      INCLUDE    'crhpl.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,IX,J,PADFG,I0,I1,I2,I3,I4,I5,ACNT,JLEN,
     $            CLEN(70+NEXITS*8+NUMBAL*6),EXDAT(5)
      REAL        R1,R0,PIFLX(5),PCFLX1(5),PCFLX2(5,5),PZOO,PCFLX3(3),
     $            PCFLX4(3),PADTOT(3),PCFLX5(4),TCFLX5,PCFLX6(2),
     $            TCFLX6,PCFLX7(3,4),TCFLX7(3),PCFLX8(4),TCFLX8,
     $            PCFLX9(4),TCFLX9,PCFX10(4),TCFX10,PTIFLX(5),
     $            PTFLX1(5),PTFLX2(5,5),TSTAT2(2),
     $            APRINT(70+NEXITS*8+NUMBAL*6)
      CHARACTER*32 CKST1(3),CKST2(3),CKST3(7),CKST4(2),CCFLX5(4),
     $             CCFLX6(2),CCFLX(4)
      CHARACTER*1   CSTR(2)
      CHARACTER*256 CHEAD(70+NEXITS*8+NUMBAL*6)
C
C     + + + EXTERNALS + + +
      EXTERNAL   TRNVEC,INTCHR,EXDATE
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' *** PLANK ***')
 2010 FORMAT (/,'   STATE VARIABLES')
 2020 FORMAT (  '     PHYTOPLANKTON',18X,'PHYTO    PHYCLA    LIMPHY',
     $           7X,'ZOO')
 2030 FORMAT (  7X,'& ZOOPLANKTON',17X,'MG/L      UG/L',15X,'ORG/L')
 2040 FORMAT (  31X,2(1PE10.3),6X,A4,1PE10.3)
 2050 FORMAT (  '     PHYTOPLANKTON',18X,'PHYTO    PHYCLA    LIMPHY')
 2060 FORMAT (  37X,'MG/L      UG/L')
 2070 FORMAT (/,'     BENTHIC ALGAE',18X,'BENAL    BALCLA    LIMBAL')
 2080 FORMAT (  36X,'MG/M2     UG/M2')
 2085 FORMAT (  9X,'SPECIES',I3,12X,2(1PE10.3),6X,A4)
 2087 FORMAT (  7X,'TOTAL',19X,2(1PE10.3))
 2090 FORMAT (/,'     ORGANIC CONSTITUENTS',13X,'ORN       ORP',
     $        7X,'ORC      TORN      TORP      TORC    POTBOD')
 2100 FORMAT (  31X,7(6X,'MG/L'))
 2110 FORMAT (  31X,7(1PE10.3))
 2120 FORMAT (/,'     OTHER TOTALS',17X,'TOTAL N   TOTAL P')
 2130 FORMAT (  31X,2(6X,'MG/L'))
 2140 FORMAT (/,'   FLUXES',27X,'TOTAL     TOTAL',
     $          '    INDIVIDUAL GATE OUTFLOWS')
 2150 FORMAT (  35X,'INFLOW   OUTFLOW',5I10)
 2160 FORMAT (  '     PHYTOPLANKTON (',A3,')',11X,'IPHYTO    ROPHYT',
     $          15X,'OPHYT')
 2170 FORMAT (/,'     ZOOPLANKTON (',A3,')',15X,'IZOO     ROZOO',
     $          16X,'OZOO')
 2180 FORMAT (/,'    DEAD REFRACTORY ORGANICS')
 2190 FORMAT (/,'     NITROGEN (',A3,')',18X,'IORN     ROORN',
     $          16X,'OORN')
 2200 FORMAT (  31X,7(1PE10.3))
 2210 FORMAT (/,'     PHOSPHORUS (',A3,')',16X,'IORP     ROORP',
     $          16X,'OORP')
 2220 FORMAT (/,'     CARBON (',A3,')',20X,'IORC     ROORC',
     $          16X,'OORC')
 2230 FORMAT (/,' ','  FLUXES',22X,'<---ATMOSPHERIC DEPOSITION--->',
     $          '     OTHER     TOTAL    INDIVIDUAL GATE OUTFLOWS')
 2240 FORMAT (  ' ',37X,'DRY       WET     TOTAL    INFLOW   OUTFLOW',
     $          5I10)
 2250 FORMAT (  '     PHYTOPLANKTON (',A3,')',41X,'IPHYTO    ROPHYT',
     $          15X,'OPHYT')
 2260 FORMAT (  61X,10(1PE10.3))
 2270 FORMAT (/,'     ZOOPLANKTON (',A3,')',45X,'IZOO     ROZOO',
     $          16X,'OZOO')
 2280 FORMAT (/,' ','    NITROGEN (',A3,')',13X,'PLADDR(1)',
     $          ' PLADWT(1)   ATM DEP      IORN     ROORN',16X,'OORN')
 2290 FORMAT (/,'  ','   PHOSPHORUS (',A3,')',11X,'PLADDR(2)',
     $          ' PLADWT(2)   ATM DEP      IORP     ROORP',16X,'OORP')
 2300 FORMAT (/,' ','    CARBON (',A3,')',15X,'PLADDR(3)',
     $          ' PLADWT(3)   ATM DEP      IORC     ROORC',16X,'OORC')
 2310 FORMAT (/,'   FLUXES',27X,'TOTAL     TOTAL')
 2320 FORMAT (  '     PHYTOPLANKTON (',A3,')',11X,'IPHYTO    ROPHYT')
 2330 FORMAT (/,'     ZOOPLANKTON (',A3,')',15X,'IZOO     ROZOO')
 2340 FORMAT (/,'     NITROGEN (',A3,')',18X,'IORN     ROORN')
 2350 FORMAT (/,'     PHOSPHORUS (',A3,')',16X,'IORP     ROORP')
 2360 FORMAT (/,'     CARBON (',A3,')',20X,'IORC     ROORC')
 2370 FORMAT (/,'   FLUXES',22X,'<---ATMOSPHERIC DEPOSITION--->',
     $          '     OTHER     TOTAL')
 2380 FORMAT (  '     PHYTOPLANKTON (',A3,')',41X,'IPHYTO    ROPHYT')
 2390 FORMAT (/,'     ZOOPLANKTON (',A3,')',45X,'IZOO     ROZOO')
 2400 FORMAT (/,'     NITROGEN (',A3,')',13X,'PLADDR(1)',
     $          ' PLADWT(1)   ATM DEP      IORN     ROORN')
 2410 FORMAT (/,'     PHOSPHORUS (',A3,')',11X,'PLADDR(2)',
     $          ' PLADWT(2)   ATM DEP      IORP     ROORP')
 2420 FORMAT (/,'     CARBON (',A3,')',15X,'PLADDR(3)',
     $          ' PLADWT(3)   ATM DEP      IORC     ROORC')
 2430 FORMAT (/,'     OTHER GAINS/LOSSES (POS. INDICATES GAIN)')
 2440 FORMAT (/,'     PHYTOPLANKTON (',A3,')',12X,'TOTAL        SINK',
     $          '   ZOO GRAZE       DEATH      GROWTH')
 2450 FORMAT (  31X,5(1PE10.3,2X))
 2460 FORMAT (/,'     ZOOPLANKTON (',A3,')',14X,'TOTAL      GROWTH',
     $          '       DEATH')
 2470 FORMAT (/,'     BENTHIC ALGAE (MG/M2)',10X,'TOTAL      GROWTH',
     $          '       DEATH')
 2475 FORMAT (  9X,'SPECIES',I3,12X,3(1PE10.3,2X))
 2477 FORMAT (  7X,'TOTAL',19X,3(1PE10.3,2X))
 2480 FORMAT (/,'     NITROGEN (',A3,')',17X,'TOTAL',
     $          '        SINK PHYTO DEATH   ZOO DEATH BENAL DEATH')
 2490 FORMAT (/,'     PHOSPHORUS (',A3,')',15X,'TOTAL',
     $          '        SINK PHYTO DEATH   ZOO DEATH BENAL DEATH')
 2500 FORMAT (/,'     CARBON (',A3,')',19X,'TOTAL        SINK',
     $          ' PHYTO DEATH   ZOO DEATH BENAL DEATH')
 2510 FORMAT (/,'    SUMMARY FLUXES',28X,'TOTAL     TOTAL',
     $          '    INDIVIDUAL GATE OUTFLOWS')
 2515 FORMAT (  45X,'INFLOW   OUTFLOW',5I10)
 2520 FORMAT (/,'     TOTAL ORGANIC NITROGEN (',A3,')',13X,'ITORN',
     $          '    ROTORN     OTORN')
 2525 FORMAT (  41X,7(1PE10.3))
 2530 FORMAT (/,'     TOTAL ORGANIC PHOSPHORUS (',A3,')',11X,'ITORP',
     $          '    ROTORP     OTORP')
 2540 FORMAT (/,'     TOTAL ORGANIC CARBON (',A3,')',15X,'ITORC',
     $          '    ROTORC     OTORC')
 2550 FORMAT (/,'     TOTAL NITROGEN (',A3,')',21X,'ITOTN',
     $          '    ROTOTN     OTOTN')
 2560 FORMAT (/,'     TOTAL PHOSPHORUS (',A3,')',19X,'ITOTP',
     $          '    ROTOTP     OTOTP')
 2570 FORMAT (/,'    SUMMARY FLUXES',28X,'TOTAL     TOTAL')
 2580 FORMAT (/,'     TOTAL ORGANIC NITROGEN (',A3,')',13X,'ITORN',
     $          '    ROTORN')
 2590 FORMAT (/,'     TOTAL ORGANIC PHOSPHORUS (',A3,')',11X,'ITORP',
     $          '    ROTORP')
 2600 FORMAT (/,'     TOTAL ORGANIC CARBON (',A3,')',15X,'ITORC',
     $          '    ROTORC')
 2610 FORMAT (/,'     TOTAL NITROGEN (',A3,')',21X,'ITOTN',
     $          '    ROTOTN')
 2620 FORMAT (/,'     TOTAL PHOSPHORUS (',A3,')',19X,'ITOTP',
     $          '    ROTOTP')
C
C     + + + END SPECIFICATIONS + + +
C
      I0= 0
      I1= 1
      I2= 2
      I3= 3
      I4= 4
      I5= 5
      R1= 1.0
      R0= 0.0
C
C     initialize array counter for binary printout, store variable
C     names in local strings for use in building binary headers
      ACNT = 0
      CKST1(1)  = 'PHYTO'
      CKST1(2)  = 'PHYCLA'
      CKST1(3)  = 'LIMPHY'
      CKST2(1)  = 'BENAL'
      CKST2(2)  = 'BALCLA'
      CKST2(3)  = 'LIMBAL'
      CKST3(1)  = 'N-REFORG-'
      CKST3(2)  = 'P-REFORG-'
      CKST3(3)  = 'C-REFORG-'
      CKST3(4)  = 'N-TOTORG-'
      CKST3(5)  = 'P-TOTORG-'
      CKST3(6)  = 'C-TOTORG-'
      CKST3(7)  = 'POTBOD'
      CKST4(1)  = 'N-TOT-'
      CKST4(2)  = 'P-TOT-'
      CCFLX5(1) = 'PHYTO-SINK'
      CCFLX5(2) = 'PHYTO-ZOOPRED'
      CCFLX5(3) = 'PHYTO-DEATH'
      CCFLX5(4) = 'PHYTO-GROWTH'
      CCFLX6(1) = 'ZOO-GROWTH'
      CCFLX6(2) = 'ZOO-DEATH'
      CCFLX(1)  = '-SINK'
      CCFLX(2)  = '-PHYTODEATH'
      CCFLX(3)  = '-ZOODEATH'
      CCFLX(4)  = '-BENTHICDEATH'
C
      DO 10 I= 1, 3
        PADTOT(I)= 0.0
 10   CONTINUE
C
      PADFG= 0
      DO 20 I= 1, 3
        J= (I-1)*2+ 1
        IF ( (PLADFG(J) .NE. 0) .OR. (PLADFG(J+1) .NE. 0) ) THEN
          PADFG= 1
        END IF
 20   CONTINUE
C
C     convert variables to external units
      IF (PHYFG .EQ. 1) THEN
C       phytoplankton inflow flux
        PIFLX(1) = PKIF(1,LEV)*FACTA
C
C       phytoplankton computed fluxes
        PCFLX1(1)= PKCF1(1,LEV)*FACTA
C
        CALL TRNVEC
     I              (I4,PKCF5(1,LEV),FACTA,FACTB,
     O               PCFLX5(1))
        TCFLX5= PCFLX5(1)+ PCFLX5(2)+ PCFLX5(3)+ PCFLX5(4)
C
        IF (NEXITS .GT. 1) THEN
          CALL TRNVEC
     I                (NEXITS,PKCF2(1,1,LEV),FACTA,FACTB,
     O                 PCFLX2(1,1))
        END IF
C
        IF (ZOOFG .EQ. 1) THEN
C         zooplankton concentration
          PZOO     = ZOO/ZOMASS
C
C         zooplankton inflow flux
          PIFLX(2) = PKIF(2,LEV)*FACTA
C
C         zooplankton computed fluxes
          PCFLX1(2)= PKCF1(2,LEV)*FACTA
          CALL TRNVEC
     I                (I2,PKCF6(1,LEV),FACTA,FACTB,
     O                 PCFLX6(1))
          TCFLX6= PCFLX6(1)+ PCFLX6(2)
C
          IF (NEXITS .GT. 1) THEN
            CALL TRNVEC
     I                  (NEXITS,PKCF2(1,2,LEV),FACTA,FACTB,
     O                   PCFLX2(1,2))
          END IF
        END IF
      END IF
      IF (BALFG .GE. 1) THEN
C       benthic algae fluxes
        TSTAT2(1)= 0.0
        TSTAT2(2)= 0.0
        DO 30 I= 1, NUMBAL
          CALL TRNVEC
     I                (I3,PKCF7(1,I,LEV),R1,R0,
     O                 PCFLX7(1,I))
          TSTAT2(1)= TSTAT2(1)+ PKST2(I,1)
          TSTAT2(2)= TSTAT2(2)+ PKST2(I,2)
 30     CONTINUE
        CALL TRNVEC
     I              (I3,TPKCF7(1,LEV),R1,R0,
     O               TCFLX7(1))
      END IF
C
C     fluxes for organics
      DO 40 I= 3,5
C       inflow fluxes
        PIFLX(I) = PKIF(I,LEV)*FACTA
C
C       computed fluxes for organics
        PCFLX1(I)= PKCF1(I,LEV)*FACTA
        IF (PADFG .EQ. 1) THEN
          PCFLX3(I-2)= PKCF3(I-2,LEV)*FACTA
          PCFLX4(I-2)= PKCF4(I-2,LEV)*FACTA
          PADTOT(I-2)= PCFLX3(I-2)+ PCFLX4(I-2)
        END IF
C
        IF (NEXITS .GT. 1) THEN
          CALL TRNVEC
     I                (NEXITS,PKCF2(1,I,LEV),FACTA,FACTB,
     O                 PCFLX2(1,I))
        END IF
 40   CONTINUE
C
C     summary fluxes
      CALL TRNVEC
     I            (I5,TPKIF(1,LEV),FACTA,FACTB,
     O             PTIFLX(1))
      CALL TRNVEC
     I            (I5,TPKCF1(1,LEV),FACTA,FACTB,
     O             PTFLX1(1))
      IF (NEXITS .GT. 1) THEN
        DO 50 I= 1, 5
          CALL TRNVEC
     I                (NEXITS,TPKCF2(1,I,LEV),FACTA,FACTB,
     O                 PTFLX2(1,I))
 50     CONTINUE
      END IF
C
C     reaction fluxes
      CALL TRNVEC
     I            (I4,PKCF8(1,LEV),FACTA,FACTB,
     O             PCFLX8(1))
      TCFLX8= PCFLX8(1)+ PCFLX8(2)+ PCFLX8(3)+ PCFLX8(4)
      CALL TRNVEC
     I            (I4,PKCF9(1,LEV),FACTA,FACTB,
     O             PCFLX9(1))
      TCFLX9= PCFLX9(1)+ PCFLX9(2)+ PCFLX9(3)+ PCFLX9(4)
      CALL TRNVEC
     I            (I4,PKCF10(1,LEV),FACTA,FACTB,
     O             PCFX10(1))
      TCFX10= PCFX10(1)+ PCFX10(2)+ PCFX10(3)+ PCFX10(4)
C
C     do printout on unit printu
      IF (PRINTU .GT. 0 .AND. PFLAG(9) .LE. LEV) THEN
        WRITE (PRINTU,2000)
C
        WRITE (PRINTU,2010)
C
        IF (PHYFG .EQ. 1) THEN
          IF (ZOOFG .EQ. 1) THEN
            WRITE (PRINTU,2020)
            WRITE (PRINTU,2030)
            WRITE (PRINTU,2040)  PKST1, PZOO
          ELSE
            WRITE (PRINTU,2050)
            WRITE (PRINTU,2060)
            WRITE (PRINTU,2040)  PKST1
          END IF
        END IF
C
        IF (BALFG .GE. 1) THEN
          WRITE (PRINTU,2070)
          WRITE (PRINTU,2080)
          DO 60 I= 1, NUMBAL
            IF (NUMBAL .GE. 2) THEN
              WRITE (PRINTU,2085) I,(PKST2(I,J),J=1,3)
            ELSE
              WRITE (PRINTU,2040) (PKST2(I,J),J=1,3)
            END IF
 60       CONTINUE
          IF (BALFG .GT. 1) THEN
            WRITE (PRINTU,2087) (TSTAT2(J),J=1,2)
          END IF
        END IF
C
        WRITE (PRINTU,2090)
        WRITE (PRINTU,2100)
        WRITE (PRINTU,2110)  PKST3
C
        WRITE (PRINTU,2120)
        WRITE (PRINTU,2130)
        WRITE (PRINTU,2110)  PKST4
C
C       fluxes
C
C       inflows and outflows
C
        IF (NEXITS .GT. 1) THEN
          IF (PADFG .EQ. 0) THEN
            WRITE (PRINTU,2140)
            WRITE (PRINTU,2150)  (J,J=1,NEXITS)
C
            IF (PHYFG .EQ. 1) THEN
              WRITE (PRINTU,2160)  FLUXID
              WRITE (PRINTU,2200)  PIFLX(1), PCFLX1(1),
     $                            (PCFLX2(J,1),J=1,NEXITS)
C
              IF (ZOOFG .EQ. 1) THEN
                WRITE (PRINTU,2170)  FLUXID
                WRITE (PRINTU,2200)  PIFLX(2), PCFLX1(2),
     $                             (PCFLX2(J,2),J=1,NEXITS)
              END IF
            END IF
C
            WRITE (PRINTU,2180)
            WRITE (PRINTU,2190)  FLUXID
            WRITE (PRINTU,2200)  PIFLX(3), PCFLX1(3),
     $                          (PCFLX2(J,3),J=1,NEXITS)
C
            WRITE (PRINTU,2210)  FLUXID
            WRITE (PRINTU,2200)  PIFLX(4), PCFLX1(4),
     $                        (PCFLX2(J,4),J=1,NEXITS)
C
            WRITE (PRINTU,2220)  FLUXID
            WRITE (PRINTU,2200)  PIFLX(5), PCFLX1(5),
     $                          (PCFLX2(J,5),J=1,NEXITS)
C
          ELSE
            WRITE (PRINTU,2230)
            WRITE (PRINTU,2240)  (J,J=1,NEXITS)
C
            IF (PHYFG .EQ. 1) THEN
              WRITE (PRINTU,2250)  FLUXID
              WRITE (PRINTU,2260)  PIFLX(1), PCFLX1(1),
     $                          (PCFLX2(J,1),J=1,NEXITS)
C
              IF (ZOOFG .EQ. 1) THEN
                WRITE (PRINTU,2270)  FLUXID
                WRITE (PRINTU,2260)  PIFLX(2), PCFLX1(2),
     $                              (PCFLX2(J,2),J=1,NEXITS)
              END IF
            END IF
C
            WRITE (PRINTU,2180)
            WRITE (PRINTU,2280)  FLUXID
            WRITE (PRINTU,2200)  PCFLX3(1), PCFLX4(1), PADTOT(1),
     $                           PIFLX(3), PCFLX1(3),
     $                          (PCFLX2(J,3),J=1,NEXITS)
C
            WRITE (PRINTU,2290)  FLUXID
            WRITE (PRINTU,2200)  PCFLX3(2), PCFLX4(2), PADTOT(2),
     $        PIFLX(4), PCFLX1(4), (PCFLX2(J,4),J=1,NEXITS)
C
            WRITE (PRINTU,2300)  FLUXID
            WRITE (PRINTU,2200)  PCFLX3(3), PCFLX4(3), PADTOT(3),
     $        PIFLX(5), PCFLX1(5), (PCFLX2(J,5),J=1,NEXITS)
C
          END IF
        ELSE
C         single exit
          IF (PADFG .EQ. 0) THEN
            WRITE (PRINTU,2310)
            WRITE (PRINTU,2150)
C
            IF (PHYFG .EQ. 1) THEN
              WRITE (PRINTU,2320)  FLUXID
              WRITE (PRINTU,2200)  PIFLX(1), PCFLX1(1)
C
              IF (ZOOFG .EQ. 1) THEN
                WRITE (PRINTU,2330)  FLUXID
                WRITE (PRINTU,2200)  PIFLX(2), PCFLX1(2)
              END IF
            END IF
C
            WRITE (PRINTU,2180)
            WRITE (PRINTU,2340)  FLUXID
            WRITE (PRINTU,2200)  PIFLX(3), PCFLX1(3)
C
            WRITE (PRINTU,2350)  FLUXID
            WRITE (PRINTU,2200)  PIFLX(4), PCFLX1(4)
C
            WRITE (PRINTU,2360)  FLUXID
            WRITE (PRINTU,2200)  PIFLX(5), PCFLX1(5)
          ELSE
            WRITE (PRINTU,2370)
            WRITE (PRINTU,2150)
C
            IF (PHYFG .EQ. 1) THEN
              WRITE (PRINTU,2380)  FLUXID
              WRITE (PRINTU,2260)  PIFLX(1), PCFLX1(1)
C
              IF (ZOOFG .EQ. 1) THEN
                WRITE (PRINTU,2390)  FLUXID
                WRITE (PRINTU,2260)  PIFLX(2), PCFLX1(2)
              END IF
            END IF
C
            WRITE (PRINTU,2180)
            WRITE (PRINTU,2400)  FLUXID
            WRITE (PRINTU,2200)  PCFLX3(1), PCFLX4(1), PADTOT(1),
     $                         PIFLX(3), PCFLX1(3)
C
            WRITE (PRINTU,2410)  FLUXID
            WRITE (PRINTU,2200)  PCFLX3(2), PCFLX4(2), PADTOT(2),
     $                         PIFLX(4), PCFLX1(4)
C
            WRITE (PRINTU,2420)  FLUXID
            WRITE (PRINTU,2200)  PCFLX3(3), PCFLX4(3), PADTOT(3),
     $                         PIFLX(5), PCFLX1(5)
          END IF
        END IF
C
C       reaction fluxes
C
        WRITE (PRINTU,2430)
        IF (PHYFG .EQ. 1) THEN
          WRITE (PRINTU,2440) FLUXID
          WRITE (PRINTU,2450) TCFLX5,PCFLX5
          IF (ZOOFG .EQ. 1) THEN
            WRITE (PRINTU,2460) FLUXID
            WRITE (PRINTU,2450) TCFLX6,PCFLX6
          END IF
        END IF
C
        IF (BALFG .GE. 1) THEN
          WRITE (PRINTU,2470)
          DO 70 I= 1, NUMBAL
            IF (NUMBAL .GE. 2) THEN
              WRITE (PRINTU,2475) I,PCFLX7(3,I),PCFLX7(1,I),PCFLX7(2,I)
            ELSE
              WRITE (PRINTU,2450) PCFLX7(3,I),PCFLX7(1,I),PCFLX7(2,I)
            END IF
 70       CONTINUE
          IF (NUMBAL .GT. 1) THEN
            WRITE (PRINTU,2477) TCFLX7(3),TCFLX7(1),TCFLX7(2)
          END IF
        END IF
C
        WRITE (PRINTU,2180)
        WRITE (PRINTU,2480) FLUXID
        WRITE (PRINTU,2450) TCFLX8,PCFLX8
        WRITE (PRINTU,2490) FLUXID
        WRITE (PRINTU,2450) TCFLX9,PCFLX9
        WRITE (PRINTU,2500) FLUXID
        WRITE (PRINTU,2450) TCFX10,PCFX10
C
C       summary fluxes
C
        IF (NEXITS .GT. 1) THEN
          WRITE (PRINTU,2510)
          WRITE (PRINTU,2515) (J,J=1,NEXITS)
          WRITE (PRINTU,2520)  FLUXID
          WRITE (PRINTU,2525)  PTIFLX(1),PTFLX1(1),
     $                        (PTFLX2(J,1),J=1,NEXITS)
C
          WRITE (PRINTU,2530)  FLUXID
          WRITE (PRINTU,2525)  PTIFLX(2),PTFLX1(2),
     $                        (PTFLX2(J,2),J=1,NEXITS)
C
          WRITE (PRINTU,2540)  FLUXID
          WRITE (PRINTU,2525)  PTIFLX(3),PTFLX1(3),
     $                      (PTFLX2(J,1),J=1,NEXITS)
C
          WRITE (PRINTU,2550)  FLUXID
          WRITE (PRINTU,2525)  PTIFLX(4),PTFLX1(4),
     $                        (PTFLX2(J,4),J=1,NEXITS)
C
          WRITE (PRINTU,2560)  FLUXID
          WRITE (PRINTU,2525)  PTIFLX(5),PTFLX1(5),
     $                        (PTFLX2(J,5),J=1,NEXITS)
        ELSE
          WRITE (PRINTU,2570)
          WRITE (PRINTU,2515)
          WRITE (PRINTU,2580)  FLUXID
          WRITE (PRINTU,2525)  PTIFLX(1),PTFLX1(1)
C
          WRITE (PRINTU,2590)  FLUXID
          WRITE (PRINTU,2525)  PTIFLX(2),PTFLX1(2)
C
          WRITE (PRINTU,2600)  FLUXID
          WRITE (PRINTU,2525)  PTIFLX(3),PTFLX1(3)
C
          WRITE (PRINTU,2610)  FLUXID
          WRITE (PRINTU,2525)  PTIFLX(4),PTFLX1(4)
C
          WRITE (PRINTU,2620)  FLUXID
          WRITE (PRINTU,2525)  PTIFLX(5),PTFLX1(5)
        END IF
      END IF
C
      IF (BINU .GT. 0 .AND. ABS(BFLAG(9)) .LE. LEV) THEN
        IF (PHYFG .EQ. 1) THEN
          DO 80 I= 1, 3
            ACNT = ACNT + 1
            APRINT(ACNT) = PKST1(I)
            CHEAD(ACNT) = CKST1(I)
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 80       CONTINUE
          IF (ZOOFG .EQ. 1) THEN
            ACNT = ACNT + 1
            APRINT(ACNT) = PZOO
            CHEAD(ACNT) = 'ZOO'
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          END IF
        END IF
C
        IF (BALFG .GE. 1) THEN
          DO 110 I= 1, NUMBAL
            CALL INTCHR (I, I2, I1,
     O                   JLEN, CSTR)
            DO 100 J= 1, 3
              ACNT = ACNT + 1
              APRINT(ACNT) = PKST2(I,J)
              CHEAD(ACNT) = CKST2(J)
              DO 90 IX= 1, JLEN
                CHEAD(ACNT) = TRIM(CHEAD(ACNT)) // CSTR(IX)
 90           CONTINUE
              CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 100        CONTINUE
 110      CONTINUE
          IF (BALFG .GT. 1) THEN
            DO 120 I= 1, 2
              ACNT = ACNT + 1
              APRINT(ACNT) = TSTAT2(I)
              CHEAD(ACNT) = CKST2(I)
              CHEAD(ACNT) = TRIM(CKST2(I)) // '-TOT'
              CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 120        CONTINUE
          END IF
        END IF
C
        DO 130 I= 1, 7
          ACNT = ACNT + 1
          APRINT(ACNT) = PKST3(I)
          CHEAD(ACNT) = CKST3(I)
          IF (I .LT. 7) THEN
            CHEAD(ACNT) = TRIM(CHEAD(ACNT)) // 'CONC'
          END IF
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 130    CONTINUE
        DO 140 I= 1, 2
          ACNT = ACNT + 1
          APRINT(ACNT) = PKST4(I)
          CHEAD(ACNT) = CKST4(I)
          CHEAD(ACNT) = TRIM(CHEAD(ACNT)) // 'CONC'
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 140    CONTINUE
C
C       fluxes
C
C       inflows and outflows
C
        IF (PHYFG .EQ. 1) THEN
          ACNT = ACNT + 1
          APRINT(ACNT) = PIFLX(1)
          CHEAD(ACNT) = 'PHYTO-IN'
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          ACNT = ACNT + 1
          APRINT(ACNT) = PCFLX1(1)
          CHEAD(ACNT) = 'PHYTO-OUT'
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          IF (NEXITS .GT. 1) THEN
            DO 160 I= 1, NEXITS
              CALL INTCHR (I, I2, I1,
     O                     JLEN, CSTR)
              ACNT = ACNT + 1
              APRINT(ACNT) = PCFLX2(I,1)
              CHEAD(ACNT) = 'PHYTO-OUT-EXIT'
              DO 150 IX= 1, JLEN
                CHEAD(ACNT) = TRIM(CHEAD(ACNT)) // CSTR(IX)
 150          CONTINUE
              CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 160        CONTINUE
          END IF
          IF (ZOOFG .EQ. 1) THEN
            ACNT = ACNT + 1
            APRINT(ACNT) = PIFLX(2)
            CHEAD(ACNT) = 'ZOO-IN'
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
            ACNT = ACNT + 1
            APRINT(ACNT) = PCFLX1(2)
            CHEAD(ACNT) = 'ZOO-OUT'
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
            IF (NEXITS .GT. 1) THEN
              DO 180 I= 1, NEXITS
                CALL INTCHR (I, I2, I1,
     O                       JLEN, CSTR)
                ACNT = ACNT + 1
                APRINT(ACNT) = PCFLX2(I,2)
                CHEAD(ACNT) = 'ZOO-OUT-EXIT'
                DO 170 IX= 1, JLEN
                  CHEAD(ACNT) = TRIM(CHEAD(ACNT)) // CSTR(IX)
 170            CONTINUE
                CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 180          CONTINUE
            END IF
          END IF
        END IF
C
        IF (PADFG .EQ. 0) THEN
          ACNT = ACNT + 1
          APRINT(ACNT) = PIFLX(3)
          CHEAD(ACNT) = 'N-REFORG-IN'
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          ACNT = ACNT + 1
          APRINT(ACNT) = PCFLX1(3)
          CHEAD(ACNT) = 'N-REFORG-OUT'
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          IF (NEXITS .GT. 1) THEN
            DO 200 I= 1, NEXITS
              CALL INTCHR (I, I2, I1,
     O                     JLEN, CSTR)
              ACNT = ACNT + 1
              APRINT(ACNT) = PCFLX2(I,3)
              CHEAD(ACNT) = 'N-REFORG-OUT-EXIT'
              DO 190 IX= 1, JLEN
                CHEAD(ACNT) = TRIM(CHEAD(ACNT)) // CSTR(IX)
 190          CONTINUE
              CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 200        CONTINUE
          END IF
C
          ACNT = ACNT + 1
          APRINT(ACNT) = PIFLX(4)
          CHEAD(ACNT) = 'P-REFORG-IN'
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          ACNT = ACNT + 1
          APRINT(ACNT) = PCFLX1(4)
          CHEAD(ACNT) = 'P-REFORG-OUT'
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          IF (NEXITS .GT. 1) THEN
            DO 220 I= 1, NEXITS
              CALL INTCHR (I, I2, I1,
     O                     JLEN, CSTR)
              ACNT = ACNT + 1
              APRINT(ACNT) = PCFLX2(I,4)
              CHEAD(ACNT) = 'P-REFORG-OUT-EXIT'
              DO 210 IX= 1, JLEN
                CHEAD(ACNT) = TRIM(CHEAD(ACNT)) // CSTR(IX)
 210          CONTINUE
              CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 220        CONTINUE
          END IF
C
          ACNT = ACNT + 1
          APRINT(ACNT) = PIFLX(5)
          CHEAD(ACNT) = 'C-REFORG-IN'
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          ACNT = ACNT + 1
          APRINT(ACNT) = PCFLX1(5)
          CHEAD(ACNT) = 'C-REFORG-OUT'
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          IF (NEXITS .GT. 1) THEN
            DO 240 I= 1, NEXITS
              CALL INTCHR (I, I2, I1,
     O                     JLEN, CSTR)
              ACNT = ACNT + 1
              APRINT(ACNT) = PCFLX2(I,5)
              CHEAD(ACNT) = 'C-REFORG-OUT-EXIT'
              DO 230 IX= 1, JLEN
                CHEAD(ACNT) = TRIM(CHEAD(ACNT)) // CSTR(IX)
 230          CONTINUE
              CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 240        CONTINUE
          END IF
C
        ELSE
          ACNT = ACNT + 1
          APRINT(ACNT) = PCFLX3(1)
          CHEAD(ACNT) = 'P-ATMDEPDRY'
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          ACNT = ACNT + 1
          APRINT(ACNT) = PCFLX4(1)
          CHEAD(ACNT) = 'P-ATMDEPWET'
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          ACNT = ACNT + 1
          APRINT(ACNT) = PADTOT(1)
          CHEAD(ACNT) = 'P-ATMDEPTOT'
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          ACNT = ACNT + 1
          APRINT(ACNT) = PIFLX(3)
          CHEAD(ACNT) = 'N-REFORG-IN'
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          ACNT = ACNT + 1
          APRINT(ACNT) = PCFLX1(3)
          CHEAD(ACNT) = 'N-REFORG-OUT'
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          IF (NEXITS .GT. 1) THEN
            DO 260 I= 1, NEXITS
              CALL INTCHR (I, I2, I1,
     O                     JLEN, CSTR)
              ACNT = ACNT + 1
              APRINT(ACNT) = PCFLX2(I,3)
              CHEAD(ACNT) = 'N-REFORG-OUT-EXIT'
              DO 250 IX= 1, JLEN
                CHEAD(ACNT) = TRIM(CHEAD(ACNT)) // CSTR(IX)
 250          CONTINUE
              CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 260        CONTINUE
          END IF
C
          ACNT = ACNT + 1
          APRINT(ACNT) = PCFLX3(2)
          CHEAD(ACNT) = 'P-ATMDEPDRY'
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          ACNT = ACNT + 1
          APRINT(ACNT) = PCFLX4(2)
          CHEAD(ACNT) = 'P-ATMDEPWET'
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          ACNT = ACNT + 1
          APRINT(ACNT) = PADTOT(2)
          CHEAD(ACNT) = 'P-ATMDEPTOT'
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          ACNT = ACNT + 1
          APRINT(ACNT) = PIFLX(4)
          CHEAD(ACNT) = 'P-REFORG-IN'
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          ACNT = ACNT + 1
          APRINT(ACNT) = PCFLX1(4)
          CHEAD(ACNT) = 'P-REFORG-OUT'
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          IF (NEXITS .GT. 1) THEN
            DO 280 I= 1, NEXITS
              CALL INTCHR (I, I2, I1,
     O                     JLEN, CSTR)
              ACNT = ACNT + 1
              APRINT(ACNT) = PCFLX2(I,4)
              CHEAD(ACNT) = 'P-REFORG-OUT-EXIT'
              DO 270 IX= 1, JLEN
                CHEAD(ACNT) = TRIM(CHEAD(ACNT)) // CSTR(IX)
 270          CONTINUE
              CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 280        CONTINUE
          END IF
C
          ACNT = ACNT + 1
          APRINT(ACNT) = PCFLX3(3)
          CHEAD(ACNT) = 'C-ATMDEPDRY'
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          ACNT = ACNT + 1
          APRINT(ACNT) = PCFLX4(3)
          CHEAD(ACNT) = 'C-ATMDEPWET'
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          ACNT = ACNT + 1
          APRINT(ACNT) = PADTOT(3)
          CHEAD(ACNT) = 'C-ATMDEPTOT'
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          ACNT = ACNT + 1
          APRINT(ACNT) = PIFLX(5)
          CHEAD(ACNT) = 'C-REFORG-IN'
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          ACNT = ACNT + 1
          APRINT(ACNT) = PCFLX1(5)
          CHEAD(ACNT) = 'C-REFORG-OUT'
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          IF (NEXITS .GT. 1) THEN
            DO 300 I= 1, NEXITS
              CALL INTCHR (I, I2, I1,
     O                     JLEN, CSTR)
              ACNT = ACNT + 1
              APRINT(ACNT) = PCFLX2(I,5)
              CHEAD(ACNT) = 'C-REFORG-OUT-EXIT'
              DO 290 IX= 1, JLEN
                CHEAD(ACNT) = TRIM(CHEAD(ACNT)) // CSTR(IX)
 290          CONTINUE
              CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 300        CONTINUE
          END IF
C
        END IF
C
C       reaction fluxes
C
        IF (PHYFG .EQ. 1) THEN
          ACNT = ACNT + 1
          APRINT(ACNT) = TCFLX5
          CHEAD(ACNT) = 'PHYTO-TOTPROCFLUX'
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          DO 310 I= 1, 4
            ACNT = ACNT + 1
            APRINT(ACNT) = PCFLX5(I)
            CHEAD(ACNT) = CCFLX5(I)
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 310      CONTINUE
          IF (ZOOFG .EQ. 1) THEN
            ACNT = ACNT + 1
            APRINT(ACNT) = TCFLX6
            CHEAD(ACNT) = 'ZOO-TOTPROCFLUX'
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
            DO 320 I= 1, 2
              ACNT = ACNT + 1
              APRINT(ACNT) = PCFLX6(I)
              CHEAD(ACNT) = CCFLX6(I)
              CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 320        CONTINUE
          END IF
        END IF
C
        IF (BALFG .GE. 1) THEN
          DO 360 I= 1, NUMBAL
            CALL INTCHR (I, I2, I1,
     O                   JLEN, CSTR)
            ACNT = ACNT + 1
            APRINT(ACNT) = PCFLX7(3,I)
            CHEAD(ACNT) = 'BENAL-'
            DO 330 IX= 1, JLEN
              CHEAD(ACNT) = TRIM(CHEAD(ACNT)) // CSTR(IX)
 330        CONTINUE
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
            ACNT = ACNT + 1
            APRINT(ACNT) = PCFLX7(1,I)
            CHEAD(ACNT) = 'BENTHICGROWTH-'
            DO 340 IX= 1, JLEN
              CHEAD(ACNT) = TRIM(CHEAD(ACNT)) // CSTR(IX)
 340        CONTINUE
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
            ACNT = ACNT + 1
            APRINT(ACNT) = PCFLX7(2,I)
            CHEAD(ACNT) = 'BENTHICDEATH-'
            DO 350 IX= 1, JLEN
              CHEAD(ACNT) = TRIM(CHEAD(ACNT)) // CSTR(IX)
 350        CONTINUE
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 360      CONTINUE
          IF (NUMBAL .GT. 1) THEN
            ACNT = ACNT + 1
            APRINT(ACNT) = TCFLX7(3)
            CHEAD(ACNT) = 'BENAL-TOT'
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
            ACNT = ACNT + 1
            APRINT(ACNT) = TCFLX7(1)
            CHEAD(ACNT) = 'BENTHICGROWTH-TOT'
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
            ACNT = ACNT + 1
            APRINT(ACNT) = TCFLX7(2)
            CHEAD(ACNT) = 'BENTHICDEATH-TOT'
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          END IF
        END IF
C
        ACNT = ACNT + 1
        APRINT(ACNT) = TCFLX8
        CHEAD(ACNT) = 'N-REFORG-TOTPROCFLUX'
        CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
        DO 370 I= 1, 4
          ACNT = ACNT + 1
          APRINT(ACNT) = PCFLX8(I)
          CHEAD(ACNT) = 'N-REFORG' // CCFLX(I)
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 370    CONTINUE
        ACNT = ACNT + 1
        APRINT(ACNT) = TCFLX9
        CHEAD(ACNT) = 'P-REFORG-TOTPROCFLUX'
        CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
        DO 380 I= 1, 4
          ACNT = ACNT + 1
          APRINT(ACNT) = PCFLX9(I)
          CHEAD(ACNT) = 'P-REFORG' // CCFLX(I)
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 380    CONTINUE
        ACNT = ACNT + 1
        APRINT(ACNT) = TCFX10
        CHEAD(ACNT) = 'C-REFORG-TOTPROCFLUX'
        CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
        DO 390 I= 1, 4
          ACNT = ACNT + 1
          APRINT(ACNT) = PCFX10(I)
          CHEAD(ACNT) = 'C-REFORG' // CCFLX(I)
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 390    CONTINUE
C
C       summary fluxes
C
        ACNT = ACNT + 1
        APRINT(ACNT) = PTIFLX(1)
        CHEAD(ACNT) = 'N-TOTORG-IN'
        CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
        ACNT = ACNT + 1
        APRINT(ACNT) = PTFLX1(1)
        CHEAD(ACNT) = 'N-TOTORG-OUT'
        CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
        IF (NEXITS .GT. 1) THEN
          DO 410 I= 1, NEXITS
            CALL INTCHR (I, I2, I1,
     O                   JLEN, CSTR)
            ACNT = ACNT + 1
            APRINT(ACNT) = PTFLX2(I,1)
            CHEAD(ACNT) = 'N-TOTORG-OUT-EXIT'
            DO 400 IX= 1, JLEN
              CHEAD(ACNT) = TRIM(CHEAD(ACNT)) // CSTR(IX)
 400        CONTINUE
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 410      CONTINUE
        END IF
C
        ACNT = ACNT + 1
        APRINT(ACNT) = PTIFLX(2)
        CHEAD(ACNT) = 'P-TOTORG-IN'
        CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
        ACNT = ACNT + 1
        APRINT(ACNT) = PTFLX1(2)
        CHEAD(ACNT) = 'P-TOTORG-OUT'
        CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
        IF (NEXITS .GT. 1) THEN
          DO 430 I= 1, NEXITS
            CALL INTCHR (I, I2, I1,
     O                   JLEN, CSTR)
            ACNT = ACNT + 1
            APRINT(ACNT) = PTFLX2(I,2)
            CHEAD(ACNT) = 'P-TOTORG-OUT-EXIT'
            DO 420 IX= 1, JLEN
              CHEAD(ACNT) = TRIM(CHEAD(ACNT)) // CSTR(IX)
 420        CONTINUE
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 430      CONTINUE
        END IF
        ACNT = ACNT + 1
        APRINT(ACNT) = PTIFLX(3)
        CHEAD(ACNT) = 'C-TOTORG-IN'
        CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
        ACNT = ACNT + 1
        APRINT(ACNT) = PTFLX1(3)
        CHEAD(ACNT) = 'C-TOTORG-OUT'
        CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
        IF (NEXITS .GT. 1) THEN
          DO 450 I= 1, NEXITS
            CALL INTCHR (I, I2, I1,
     O                   JLEN, CSTR)
            ACNT = ACNT + 1
            APRINT(ACNT) = PTFLX2(I,3)
            CHEAD(ACNT) = 'C-TOTORG-OUT-EXIT'
            DO 440 IX= 1, JLEN
              CHEAD(ACNT) = TRIM(CHEAD(ACNT)) // CSTR(IX)
 440        CONTINUE
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 450      CONTINUE
        END IF
        ACNT = ACNT + 1
        APRINT(ACNT) = PTIFLX(4)
        CHEAD(ACNT) = 'N-TOT-IN'
        CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
        ACNT = ACNT + 1
        APRINT(ACNT) = PTFLX1(4)
        CHEAD(ACNT) = 'N-TOT-OUT'
        CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
        IF (NEXITS .GT. 1) THEN
          DO 470 I= 1, NEXITS
            CALL INTCHR (I, I2, I1,
     O                   JLEN, CSTR)
            ACNT = ACNT + 1
            APRINT(ACNT) = PTFLX2(I,4)
            CHEAD(ACNT) = 'N-TOT-OUT-EXIT'
            DO 460 IX= 1, JLEN
              CHEAD(ACNT) = TRIM(CHEAD(ACNT)) // CSTR(IX)
 460        CONTINUE
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 470      CONTINUE
        END IF
        ACNT = ACNT + 1
        APRINT(ACNT) = PTIFLX(5)
        CHEAD(ACNT) = 'P-TOT-IN'
        CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
        ACNT = ACNT + 1
        APRINT(ACNT) = PTFLX1(5)
        CHEAD(ACNT) = 'P-TOT-OUT'
        CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
        IF (NEXITS .GT. 1) THEN
          DO 490 I= 1, NEXITS
            CALL INTCHR (I, I2, I1,
     O                   JLEN, CSTR)
            ACNT = ACNT + 1
            APRINT(ACNT) = PTFLX2(I,5)
            CHEAD(ACNT) = 'P-TOT-OUT-EXIT'
            DO 480 IX= 1, JLEN
              CHEAD(ACNT) = TRIM(CHEAD(ACNT)) // CSTR(IX)
 480        CONTINUE
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 490      CONTINUE
        END IF
C
      END IF
C
      IF (BINU .GT. 0 .AND. ABS(BFLAG(9)) .LE. LEV) THEN
C       write binary output
        CALL EXDATE(
     I              DATIM,
     O              EXDAT)
        IF (BFLAG(9) .GT. 0) THEN
C         at start of run, write the header
          WRITE (BINU) I0,'RCHRES  ',RCHNO,'PLANK   ',
     1          (CLEN(I),(CHEAD(I)(J:J),J=1,CLEN(I)),I=1,ACNT)
C         set bflag to negative to not write headers anymore
          BFLAG(9) = -BFLAG(9)
        END IF
        WRITE (BINU) I1,'RCHRES  ',RCHNO,'PLANK   ',UNITFG,
     1               LEV,(EXDAT(J),J=1,5),(APRINT(J),J=1,ACNT)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PLKRB
C
C     + + + PURPOSE + + +
C     Handle subroutine group plank
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PLANK2 + + +
      INCLUDE    'crhpl.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,J
C
C     + + + END SPECIFICATIONS + + +
C
      DO 10 I= 1,5
        IF (RCPKFP(I) .GE. 1) THEN
          PAD(RCPKFP(I)+ IVL1)= PKIF(I,1)
        END IF
        IF (RCTPKX(I) .GE. 1) THEN
          PAD(RCTPKX(I)+ IVL1)= TPKIF(I,1)
        END IF
        IF (PKCF1X(I) .GE. 1) THEN
          PAD(PKCF1X(I)+ IVL1)= PKCF1(I,1)
        END IF
        IF (TPKC1X(I) .GE. 1) THEN
          PAD(TPKC1X(I)+ IVL1)= TPKCF1(I,1)
        END IF
        IF (PKCF5X(I) .GE. 1) THEN
          PAD(PKCF5X(I)+ IVL1)= PKCF5(I,1)
        END IF
        IF (PKCF8X(I) .GE. 1) THEN
          PAD(PKCF8X(I)+ IVL1)= PKCF8(I,1)
        END IF
        IF (PKCF9X(I) .GE. 1) THEN
          PAD(PKCF9X(I)+ IVL1)= PKCF9(I,1)
        END IF
        IF (PKC10X(I) .GE. 1) THEN
          PAD(PKC10X(I)+ IVL1)= PKCF10(I,1)
        END IF
 10   CONTINUE
C
      DO 20 I= 1, 10
 20   CONTINUE
C
      IF (NEXITS .GT. 1) THEN
        DO 40 J= 1, 5
          DO 30 I= 1, NEXITS
            IF (PKCF2X(I,J) .GE. 1) THEN
              PAD(PKCF2X(I,J)+ IVL1)= PKCF2(I,J,1)
            END IF
            IF (TPKC2X(I,J) .GE. 1) THEN
              PAD(TPKC2X(I,J)+ IVL1)= TPKCF2(I,J,1)
            END IF
 30       CONTINUE
 40     CONTINUE
      END IF
C
      DO 50 I= 1, 3
        IF (PLADDX(I) .GE. 1) THEN
          PAD(PLADDX(I)+ IVL1)= PKCF3(I,1)
        END IF
C
        IF (PLADWX(I) .GE. 1) THEN
          PAD(PLADWX(I)+ IVL1)= PKCF4(I,1)
        END IF
C
        IF (PLADPX(I) .GE. 1) THEN
          PAD(PLADPX(I)+ IVL1)= PLADEP(I)
        END IF
C
        IF (PKCF6X(I) .GE. 1) THEN
          PAD(PKCF6X(I)+ IVL1)= PKCF6(I,1)
        END IF
C
        DO 45 J= 1, NUMBAL
          IF (PKCF7X(I,J) .GE. 1) THEN
            PAD(PKCF7X(I,J)+ IVL1)= PKCF7(I,J,1)
          END IF
 45     CONTINUE
C
        IF (TPKC7X(I) .GE. 1) THEN
          PAD(TPKC7X(I)+ IVL1)= TPKCF7(I,1)
        END IF
 50   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   PLKRP
C
C     + + + PURPOSE + + +
C     Handle subroutine group plank
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PLANK2 + + +
      INCLUDE    'crhpl.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I
C
C     + + + END SPECIFICATIONS + + +
C
      DO 10 I= 1, 7
        IF (PKST3X(I) .GE. 1) THEN
          PAD(PKST3X(I) + IVL1)= PKST3(I)
        END IF
 10   CONTINUE
C
      DO 20 I= 1, 2
        IF (PKST4X(I) .GE. 1) THEN
          PAD(PKST4X(I) + IVL1)= PKST4(I)
        END IF
 20   CONTINUE
C
      IF (PYFP .GE. 1) THEN
        PAD(PYFP + IVL1)= PHYTO
      END IF
C
      IF (PYCLFP .GE. 1) THEN
        PAD(PYCLFP + IVL1)= PHYCLA
      END IF
C
      DO 30 I= 1, NUMBAL
        IF (BAFP(I) .GE. 1) THEN
          PAD(BAFP(I) + IVL1)= BENAL(I)
        END IF
C
        IF (BACLFP(I) .GE. 1) THEN
          PAD(BACLFP(I) + IVL1)= BALCLA(I)
        END IF
 30   CONTINUE
C
      DO 40 I= 1, 2
        IF (TBALFP(I) .GE. 1) THEN
          PAD(TBALFP(I) + IVL1)= TBENAL(I)
        END IF
 40   CONTINUE
C
      IF (ZFP .GE. 1) THEN
        PAD(ZFP + IVL1)   = ZOO
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PLKRST
     I                    (LEV)
C
C     + + + PURPOSE + + +
C     Reset flux and state variables for subroutine group plank
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    LEV
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEV    - current output level (2-pivl,3-day,4-mon,5-ann)
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PLANK2 + + +
      INCLUDE    'crhpl.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I3,I5,I
C
C     + + + EXTERNALS + + +
      EXTERNAL   SETVEC
C
C     + + + DATA INITIALIZATIONS + + +
      DATA I3,I5/3,5/
C
C     + + + END SPECIFICATIONS + + +
C
      IF (PHYFG .EQ. 1) THEN
C       handle flux groups dealing with reach-wide variables
        PKIF(1,LEV) = 0.0
        PKCF1(1,LEV)= 0.0
        CALL SETVEC (I5,0.0,
     O               PKCF5(1,LEV))
C
        IF (NEXITS .GT. 1) THEN
C         handle flux groups dealing with individual exit gates
          CALL SETVEC (NEXITS,0.0,
     O                 PKCF2(1,1,LEV))
        END IF
C
        IF (ZOOFG .EQ. 1) THEN
C         handle flux groups dealing with reach-wide variables
          PKIF(2,LEV) = 0.0
          PKCF1(2,LEV)= 0.0
          CALL SETVEC (I3,0.0,
     O                 PKCF6(1,LEV))
C
          IF (NEXITS .GT. 1) THEN
C           handle flux groups dealing with individual exit gates
            CALL SETVEC
     I                  (NEXITS,0.0,
     O                   PKCF2(1,2,LEV))
          END IF
        END IF
      END IF
C
      IF (BALFG .GE. 1) THEN
C       handle flux groups dealing with reach-wide variables
        DO 5 I= 1, NUMBAL
          CALL SETVEC (I3,0.0,
     O                 PKCF7(1,I,LEV))
 5      CONTINUE
        CALL SETVEC (I3,0.0,
     O               TPKCF7(1,LEV))
      END IF
C
C     handle fluxes of orp, orn, and orc
      CALL SETVEC (I3,0.0,
     O             PKIF(3,LEV))
      CALL SETVEC (I3,0.0,
     O             PKCF1(3,LEV))
      CALL SETVEC (I5,0.0,
     O             TPKIF(1,LEV))
      CALL SETVEC (I5,0.0,
     O             TPKCF1(1,LEV))
      CALL SETVEC (I3,0.0,
     O             PKCF3(1,LEV))
      CALL SETVEC (I3,0.0,
     O             PKCF4(1,LEV))
      CALL SETVEC (I5,0.0,
     O             PKCF8(1,LEV))
      CALL SETVEC (I5,0.0,
     O             PKCF9(1,LEV))
      CALL SETVEC (I5,0.0,
     O             PKCF10(1,LEV))
C
      IF (NEXITS .GT. 1) THEN
C       handle flux groups dealing with individual exit gates
        DO 10 I= 3, 5
          CALL SETVEC
     I                (NEXITS,0.0,
     O                 PKCF2(1,I,LEV))
 10     CONTINUE
        DO 20 I= 1, 5
          CALL SETVEC
     I                (NEXITS,0.0,
     O                 TPKCF2(1,I,LEV))
 20     CONTINUE
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   ZORX
     I                  (ZFIL20,TCZFIL,TW,PHYTO,MZOEAT,ZEXDEL,CVPB,
     I                   ZRES20,TCZRES,ANAER,ZOMASS,TAMFG,REFR,
     I                   ZFOOD,ZD,OXZD,CVBN,CVBP,CVBC,CVNRBO,CVBO,
     M                   DOX,BOD,ZOO,ORN,ORP,ORC,TAM,NO3,PO4,
     O                   ZEAT,ZCO2,DOZOO,ZBOD,ZNIT,ZPO4,
     O                   ZOGR,ZDTH,ZORN,ZORP,ZORC)
C
C     + + + PURPOSE + + +
C     Calculate zooplankton population balance
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    TAMFG,ZFOOD
      REAL       ZFIL20,TCZFIL,TW,PHYTO,MZOEAT,ZEXDEL,CVPB,ZRES20,
     $           TCZRES,ANAER,ZOMASS,REFR,ZD,OXZD,CVBN,CVBP,CVBC,
     $           CVNRBO,CVBO,DOX,BOD,ZOO,ORN,ORP,ORC,TAM,NO3,PO4,ZEAT,
     $           ZCO2,DOZOO,ZBOD,ZNIT,ZPO4,ZOGR,ZDTH,ZORN,ZORP,ZORC
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ZFIL20 - ???
C     TCZFIL - ???
C     TW     - water temperature in degrees C
C     PHYTO  - ???
C     MZOEAT - ???
C     ZEXDEL - ???
C     CVPB   - ???
C     ZRES20 - ???
C     TCZRES - ???
C     ANAER  - ???
C     ZOMASS - ???
C     TAMFG  - ???
C     REFR   - ???
C     NONREF - ???
C     ZFOOD  - ???
C     ZD     - ???
C     OXZD   - ???
C     CVBN   - ???
C     CVBP   - ???
C     CVBC   - ???
C     CVNRBO - ???
C     CVBO   - ???
C     DOX    - dissolved oxygen concentration in mg/l
C     BOD    - ???
C     ZOO    - ???
C     ORN    - ???
C     ORP    - ???
C     ORC    - ???
C     TAM    - total ammonia (nh3 + nh4) in mg n/l
C     NO3    - dissolved nitrate concentration in mg/l
C     PO4    - ???
C     ZEAT   - ???
C     ZCO2   - ???
C     DOZOO  - ???
C     ZBOD   - ???
C     ZNIT   - ???
C     ZPO4   - ???
C     ZOGR   - ???
C     ZDTH   - ???
C     ZORN   - ???
C     ZORP   - ???
C     ZORC   - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I1
      REAL       ZFIL,ZOEAT,ZEXDEC,ZEFF,ZEXMAS,ZREFEX,ZINGEX,ZNRFEX,
     $           ZRES,LOLIM
C
C     + + + EXTERNALS + + +
      EXTERNAL   DECBAL,ORGBAL
C
C     + + + END SPECIFICATIONS + + +
C
      I1= 1
C     calculate zooplankton unit grazing rate expressed as liters
C     of water filtered per mg zooplankton per interval
      ZFIL= ZFIL20*(TCZFIL**(TW - 20.))
C
C     calculate mass of phytoplankton biomass ingested per mg
C     zooplankton per interval
      ZOEAT= ZFIL*PHYTO
C
C     check that calculated unit ingestion rate does not exceed
C     maximum allowable unit ingestion rate (mzoeat); if it does,
C     set unit ingestion rate equal to mzoeat
C
      IF (ZOEAT .GE. MZOEAT) THEN
        ZOEAT= MZOEAT
C       nonrefractory portion of excretion is only partially
C       decomposed; zexdec is the fraction of nonrefractory
C       material which is decomposed
        ZEXDEC= ZEXDEL
      ELSE
C       calculated unit ingestion rate is acceptable
C       all nonrefractory excretion is decomposed
        ZEXDEC= 1.0
      END IF
C
C     calculate phytoplankton consumed by zooplankton; zeat is
C     expressed as mg biomass per interval
      ZEAT= ZOEAT*ZOO
C
C     check that calculated ingestion does not reduce phytoplankton
C     concentration to less than .0025 umoles p; if it does, adjust
C     ingestion so that .0025 umoles of phytoplankton (expressed as
C     p) remain
C
      IF ((PHYTO - ZEAT) .LT. (.0025*CVPB)) THEN
        ZEAT= PHYTO - (.0025*CVPB)
      END IF
C
C     calculate zooplankton assimilation efficiency
      IF (ZFOOD .EQ. 1) THEN
C       calculate assimilation efficiency resulting from ingestion
C       of high quality food; zeff is dimensionless
        ZEFF= -.06*PHYTO + 1.03
C
C       set upper limit on efficiency at 99 percent
        IF (ZEFF .GT. .99) THEN
          ZEFF= .99
        END IF
      ELSE IF (ZFOOD .EQ. 2) THEN
C       calculate assimilation efficiency resulting from ingestion
C       of medium quality food
        ZEFF= -.03*PHYTO + .47
C
C       set lower limit on efficiency at 20 percent
        IF (ZEFF .LT. .20) THEN
          ZEFF= .20
        END IF
      ELSE
C       calculate assimilation efficiency resulting from ingestion
C       of low quality food
        ZEFF= -.013*PHYTO + .17
C
C       set lower limit on efficiency at 3 percent
        IF (ZEFF .LT. .03) THEN
          ZEFF= .03
        END IF
      END IF
C
C     calculate zooplankton growth; zogr is expressed as mg biomass
C     per liter per interval
      ZOGR= ZEFF*ZEAT
C
C     calculate total zooplankton excretion (zexmas),excretion
C     decomposed to inorganic constituents (zingex), excretion
C     released as dead refractory constituents (zrefex), and
C     excretion released as dead nonrefractory material (znrfex)
      ZEXMAS= ZEAT - ZOGR
      ZREFEX= REFR*ZEXMAS
      ZINGEX= ZEXDEC*(ZEXMAS - ZREFEX)
      ZNRFEX= ZEXMAS - ZREFEX - ZINGEX
C
C     calculate zooplankton respiration; zres is expressed as mg
C     biomass per liter per interval
      ZRES  = ZRES20*(TCZRES**(TW - 20.))*ZOO
C
C     calculate zooplankton death; zdth is expressed as mg biomass
C     per liter per interval
      IF (DOX .GT. ANAER) THEN
C       calculate death using aerobic death rate
        ZDTH= ZD*ZOO
      ELSE
C       calculate death using sum of aerobic death rate and
C       anaerobic increment
        ZDTH= (ZD + OXZD)*ZOO
      END IF
C
C     calculate zooplankton population after growth, respiration,
C     and death; adjust respiration and death, if necessary, to
C     assure minimum population of zooplankton
C
C     first, account for net growth (growth - respiration)
      ZOO= ZOO + ZOGR - ZRES
C
C     maintain minimum population of .03 organisms per liter; zomass
C     is a user specified conversion factor from organisms/l to
C     mg biomass/l
      LOLIM= 0.03*ZOMASS
C
      IF (ZOO .LT. LOLIM) THEN
        ZRES= ZRES + ZOO - LOLIM
        ZOO = LOLIM
      ELSE
C       calculated respiration is acceptable
      END IF
C
C     subtract oxygen required to satisfy zooplankton respiration
C     from do state variable
      DOZOO= 1.1*ZRES
C     dox  = dox - 1.1*zres
      DOX  = DOX- DOZOO
C
      ZBOD= 0.0
      IF (DOX .LT. 0.0) THEN
C       include oxygen deficit in bod value
        ZBOD= -DOX
        DOX = 0.0
      END IF
C
C     subtract computed zooplankton death from zooplankton state
C     variable
      ZOO= ZOO - ZDTH
C
      IF (ZOO .LT. LOLIM) THEN
        ZDTH= ZDTH + ZOO - LOLIM
        ZOO = LOLIM
      ELSE
C       calculated death is acceptable
      END IF
C
C     calculate amount of inorganic constituents which are released
C     by zooplankton respiration and inorganic excretion
      ZNIT= (ZINGEX + ZRES)*CVBN
      ZPO4= (ZINGEX + ZRES)*CVBP
      ZCO2= (ZINGEX + ZRES)*CVBC
C
C     update state variables for inorganic constituents to account
C     for additions from zooplankton respiration and inorganic
C     excretion
      CALL DECBAL
     I            (TAMFG,I1,ZNIT,ZPO4,
     M             TAM,NO3,PO4)
C
C     calculate amount of refractory organic constituents which
C     result from zooplankton death and excretion
      ZORN= ((REFR*ZDTH) + ZREFEX)*CVBN
      ZORP= ((REFR*ZDTH) + ZREFEX)*CVBP
      ZORC= ((REFR*ZDTH) + ZREFEX)*CVBC
C
C     calculate amount of nonrefractory organics (bod) which result
C     from zooplankton death and excretion
      ZBOD= ZBOD + (ZDTH*CVNRBO) + (ZNRFEX*CVBO)
C
      CALL ORGBAL
     I            (ZORN,ZORP,ZORC,ZBOD,
     M             ORN,ORP,ORC,BOD)
C
      RETURN
      END
C
C
C
      SUBROUTINE   PKSUMS
     I                    (PHYFG,ZOOFG,TAMFG,NO2FG,PO4FG,ADNHFG,ADPOFG,
     I                     CVBN,CVBP,CVBC,CVBO,CVNRBO,PHYTO,ZOO,ORN,
     I                     ORP,ORC,NO3,TAM,NO2,SNH41,SNH42,SNH43,PO4,
     I                     SPO41,SPO42,SPO43,BOD,
     O                     TORN,TORP,TORC,POTBOD,TN,TP)
C
C     + + + PURPOSE + + +
C     Computes summaries of: total organic N, P, C; total N, P; POTBOD
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    PHYFG,ZOOFG,TAMFG,NO2FG,PO4FG,ADNHFG,ADPOFG
      REAL       CVBN,CVBP,CVBC,CVBO,CVNRBO,PHYTO,ZOO,ORN,ORP,ORC,NO3,
     $           TAM,NO2,SNH41,SNH42,SNH43,PO4,SPO41,SPO42,SPO43,BOD,
     $           TORN,TORP,TORC,POTBOD,TN,TP
C
C     + + + ARGUMENT DEFINITIONS + + +
C     PHYFG  - ???
C     ZOOFG  - ???
C     TAMFG  - ???
C     NO2FG  - ???
C     PO4FG  - ???
C     ADNHFG - ???
C     ADPOFG - ???
C     CVBN   - ???
C     CVBP   - ???
C     CVBC   - ???
C     CVBO   - ???
C     CVNRBO - ???
C     PHYTO  - ???
C     ZOO    - ???
C     ORN    - ???
C     ORP    - ???
C     ORC    - ???
C     NO3    - ???
C     TAM    - ???
C     NO2    - ???
C     PO4    - ???
C     SPO41  - ???
C     SPO42  - ???
C     SPO43  - ???
C     BOD    - ???
C     TORN   - ???
C     TORP   - ???
C     TORC   - ???
C     POTBOD - ???
C     TN     - ???
C     TP     - ???
C
C     + + + LOCAL VARIABLES
      REAL TVAL
C
C     + + + END SPECIFICATIONS + + +
C
      TVAL  = BOD/CVBO
      IF (PHYFG.EQ.1) THEN
        TVAL= TVAL+ PHYTO
        IF (ZOOFG.EQ.1) THEN
          TVAL= TVAL+ ZOO
        END IF
      END IF
C
      TORN  = ORN+ CVBN*TVAL
      TORP  = ORP+ CVBP*TVAL
      TORC  = ORC+ CVBC*TVAL
      POTBOD= BOD
C
      TN= TORN+ NO3
      IF (TAMFG .EQ. 1) THEN
        TN= TN+ TAM
      END IF
      IF (NO2FG .EQ. 1) THEN
        TN= TN+ NO2
      END IF
      IF (ADNHFG .EQ. 1) THEN
        TN= TN+ SNH41+ SNH42+ SNH43
      END IF
C
      TP= TORP
      IF (PO4FG .EQ. 1) THEN
        TP= TP+ PO4
      END IF
      IF (ADPOFG .EQ. 1) THEN
        TP= TP+ SPO41+ SPO42+ SPO43
      END IF
C
      IF (PHYFG.EQ.1) THEN
        POTBOD= POTBOD+ (CVNRBO*PHYTO)
        IF (ZOOFG.EQ.1) THEN
          POTBOD= POTBOD+ (CVNRBO*ZOO)
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   ALGRO2
     I                    (BALLIT,PO4,NO3,TW,MBALGR,CMMP,TAMFG,TAM,
     I                    NSFG,CMMN,BALR20,DELT60,LIMIT,TCBALG,BALVEL,
     I                    CMMV,BFIXFG,CSLIT,CMMD1,CMMD2,SUMBA,TCBALR,
     I                    NMINGR,PMINGR,LMINGR,NMAXFX,GRORES,
     O                    NFIXFG,LIMR,GROBA,RESBA)
C
C     + + + PURPOSE + + +
C     Calculate unit growth and respiration rates for benthic algae
C     using more complex kinetics; both are expressed in units of per
C     interval
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     TAMFG,NSFG,BFIXFG,NFIXFG
      REAL        BALLIT,PO4,NO3,TW,MBALGR,CMMP,TAM,CMMN,BALR20,DELT60,
     $            TCBALG,BALVEL,CMMV,CSLIT,CMMD1,CMMD2,SUMBA,TCBALR,
     $            NMINGR,PMINGR,LMINGR,NMAXFX,GRORES,GROBA,RESBA,LIMR
      CHARACTER*4 LIMIT(7)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     BALLIT - amount of light available to benthic algae in lgly/min
C     PO4    - dissolved inorganic phosphorus in mg P/L
C     NO3    - dissolved nitrate concentration in mg/l
C     TW     - water temperature in degrees C
C     MBALGR - optimal benthic algal growth rate at 20C
C     CMMP   - phosphorus half-saturation coefficient for algal growth
C     TAMFG  - switch for simulation of ammonia nitrogen
C     TAM    - total ammonia (nh3 + nh4) in mg N/L
C     NSFG   - switch for algal uptake of both NO3 and TAM
C     CMMN   - nitrogen half-saturation coefficient for algal growth
C     BALR20 - benthic algal respiration rate at 20C
C     DELT60 - simulation time interval in hours
C     TCBALG - temperature coefficient for benthic algal growth
C     BALVEL - stream velocity for benthic algae
C     CMMV   - velocity half-saturation coefficient for nutrient avail.
C     BFIXFG - switch for whether algal type is N-fixing
C     CSLIT  - saturation light level for current algal type
C     CMMD1  - intercept for density function for current algal type
C     CMMD2  - half-saturation coefficient for density function
C     SUMBA  - total benthic algal biomass in mg OM/m2
C     TCBALR - temperature coefficient for benthic algal respiration
C     LIMIT  - labels indicating which factor is limiting algal growth
C     NMINGR - minimum nitrate concentration for algal growth
C     PMINGR - minimum phosphate concentration for algal growth
C     LMINGR - minimum light intensity for algal growth
C     NMAXFX - maximum available nitrogen concentration for n-fixation to occur
C     GRORES - fraction of photorespiration required to support growth
C     NFIXFG - flag indicating if n-fixation occurs
C     LIMR   - active label of growth-limiting factor
C     GROBA  - growth rate of benthic algae in per interval
C     RESBA  - respiration rate of benthic algae in per interval
C
C     + + + LOCAL VARIABLES + + +
      REAL        TCMBAG,GROFP,MMN,GROFN,GROFL,GROFV,GROFD,GROMIN
      CHARACTER*4 LIM
C
C     + + + INTRINSICS + + +
      INTRINSIC  EXP
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT(A4)
C
C     + + + END SPECIFICATIONS + + +
C
      IF (BALLIT .GT. LMINGR) THEN
C       sufficient light to support growth
        IF (PO4 .GT. PMINGR .AND. NO3 .GT. NMINGR) THEN
C         sufficient nutrients to support growth
C
C         calculate temperature correction fraction
          TCMBAG= TCBALG**(TW- 20.0)
C
C         calculate velocity limitation on nutrient availability
          GROFV= BALVEL/(CMMV+ BALVEL)
C
C         calculate phosphorus limited unit growth factor
          GROFP= PO4*GROFV/(CMMP+ PO4*GROFV)
C
C         calculate the nitrogen limited unit growth factor
          IF (TAMFG .NE. 0) THEN
C           consider influence of tam on growth rate
            IF (NSFG .NE. 0) THEN
C             include tam in nitrogen pool for calculation of
C             nitrogen limited growth rate
              MMN= NO3+ TAM
            ELSE
C             tam is not included in nitrogen pool for calculation
C             of nitrogen limited growth
               MMN= NO3
            END IF
          ELSE
C           tam is not simulated
            MMN= NO3
          END IF
C
          IF (BFIXFG .NE. 1) THEN
C           calculate the maximum nitrogen limited unit growth rate
            NFIXFG= 0
            GROFN= (MMN*GROFV) / (CMMN+ MMN*GROFV)
          ELSE
C           N-fixing blue-green algae.
C           determine if available nitrogen concentrations are high
C           enough to suppress nitrogen fixation
            IF (MMN .GE. NMAXFX) THEN
              NFIXFG= 0
              GROFN= (MMN*GROFV) / (CMMN+ MMN*GROFV)
            ELSE
C             available nitrogen concentrations do not suppress n-fixation
              NFIXFG= 1
C             nitrogen does not limit growth rate
              GROFN= 1.0
            END IF
          END IF
C
C         calculate the maximum light limited unit growth rate
          GROFL= (BALLIT/CSLIT) * EXP(1.0- (BALLIT/CSLIT))
C
C         calculate density limitation on growth rate
          GROFD= (CMMD1*SUMBA+ CMMD2) / (SUMBA+ CMMD2)
C
C         find the limiting algal growth factor; gromin is
C         the smallest of the three computed growth factors
C         (grofp,grofn,grofl);
C         assign a three letter label to lim which will
C         be printed in the output to indicate the growth limiting
C         factor for the interval: limit(1)= 'lit'
C                                  limit(2)= 'non'
C                                  limit(3)= 'tem'
C                                  limit(4)= 'nit'
C                                  limit(5)= 'po4'
C                                  limit(6)= 'none'
C                                  limit(7)= 'wat'
C
          IF ((GROFP .LT. GROFN) .AND. (GROFP .LT. GROFL)) THEN
C           phosphorus limited
            GROMIN= GROFP
            LIM= LIMIT(5)
          ELSE IF (GROFN .LT. GROFL) THEN
C           nitrogen limited
            GROMIN= GROFN
            LIM= LIMIT(4)
          ELSE
C           light limited
            GROMIN= GROFL
            LIM= LIMIT(1)
          END IF
C
          IF (GROMIN .GT. 0.95) THEN
C           there is no limiting factor to cause less than maximum
C           growth rate
            LIM= LIMIT(6)
          END IF
C
C         calculate overall growth rate; expressed in units of per interval
          GROBA = MBALGR*TCMBAG*GROMIN*GROFD
C
          IF (GROBA .LT. (1.0E-06 * DELT60)) THEN
            GROBA = 0.0
          END IF
C
        ELSE
C         no algal growth occurs; necessary nutrients are not
C         available
          GROBA= 0.0
          LIM= LIMIT(2)
        END IF
      ELSE
C       no algal growth occurs; necessary light is not available
        GROBA= 0.0
        LIM= LIMIT(1)
      END IF
C
C     calculate unit algal respiration rate; res is expressed in
C     units of per interval; balr20 is the benthic algal respiration
C     rate at 20 degrees c
      RESBA = BALR20*TCBALR**(TW- 20.0)+ GRORES*GROBA
C
C     save limiting factor character string as real
      READ (LIM,1000) LIMR
C
      RETURN
      END
C
C
C
      SUBROUTINE   BALREM
     I                    (CREM,SUMBA,SUMBAL,CMMBI,TCGRAZ,TW,BINV,
     I                     BAL,CSLOF1,CSLOF2,BALVEL,
     O                     REMBA)
C
C     + + + PURPOSE + + +
C     Calculate benthic algae removal due to macroinvertebrates and
C     scouring, based upon equations from DSSAMt.  This subroutine
C     provides similar functionality for the enhanced periphyton
C     kinetics (BALRX2) that BALDTH provides for the original
C     HSPF benthic algae (BALRX).
C
C     + + + DUMMY ARGUMENTS + + +
      REAL       CREM,SUMBA,SUMBAL,CMMBI,TW,TCGRAZ,BINV,BAL,CSLOF1,
     $           CSLOF2,BALVEL,REMBA
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CREM   - coeff. for invertebrate removal in umole P algae/mg invert./ivl
C     SUMBA  - total benthic algal biomass all algal types in mg OM/m2
C     SUMBAL - total benthic algal biomass all algal types in umole P/L
C     CMMBI  - half-sat. constant for invertebrate removal in mg OM /m2
C     TW     - water temperature in deg C
C     TCGRAZ - temperature correction for grazing rate
C     BINV   - benthic macroinvert. biomass in mg/m2
C     BAL    - benthic algae biomass, selected algal type, in umole P/L
C     CSLOF1 - scour regression coeff. for algal type in per interval
C     CSLOF2 - scour regression exponent for selected algal type
C     BALVEL - stream velocity for benthic algae
C     REMBA  - biomass removed for algal type in umole P/L.ivl
C
C     + + + LOCAL VARIABLES + + +
      REAL       SLOF,REMINV,MAXVEL
C
C     + + + INTRINSICS + + +
      INTRINSIC  EXP
C
C     + + + END SPECIFICATIONS + + +
C
C     calculate the total benthic algae removed per interval
C     due to grazing and disturbance of macroinvertebrates
      REMINV= CREM * (SUMBA/(SUMBA+CMMBI)) * TCGRAZ**(TW- 20) * BINV
C
C     calculate portion of total algae removed represented by the
C     present benthic algal group
      REMBA= REMINV * BAL / SUMBAL
C
C     calculate scour loss rate of benthic algae in per interval
      MAXVEL= (LOG (1./CSLOF1))/CSLOF2
      IF (BALVEL .LT. MAXVEL) THEN
        SLOF= CSLOF1 * EXP(CSLOF2*BALVEL)
      ELSE
        SLOF= 1.
      END IF
C
C     calculate amount of present benthic algae group removed through
C     grazing, disturbance, and scouring
      REMBA= REMBA + SLOF*BAL
C
      RETURN
      END
C
C
C
      SUBROUTINE   NUTUP2
     I                    (GROW,NSFG,CVBPN,ALNPR,CVBPC,PHFG,DECFG,
     I                     BNPFG,CAMPR,SUMGRN,NMINC,
     M                     PO4,TAM,NO3,
     O                     ALCO2,TAMALG,NO3ALG,PO4ALG)
C
C     + + + PURPOSE + + +
C     Perform materials balance for transformation from inorganic to
C     organic material; uptake of PO4, NO3, TAM, and CO2 are considered.
C     Used instead of NUTRUP by the BALRX2 subroutine; adds a
C     calculated nitrogen preference function and adjustments for 
C     N-fixation to the NUTRUP code.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    NSFG,PHFG,DECFG,BNPFG
      REAL       GROW,CVBPN,ALNPR,CVBPC,CAMPR,SUMGRN,NMINC,PO4,TAM,NO3,
     $           ALCO2,TAMALG,NO3ALG,PO4ALG
C
C     + + + ARGUMENT DEFINITIONS + + +
C     GROW   - net growth
C     NSFG   - ammonia part of available N in nitrogen limited growth?
C     CVBPN  - phosphorus to nitrogen equivalency
C     ALNPR  - fraction of N requirement for growth satisfied by NO3
C     CVBPC  - phosphorus to carbon equivalency
C     PHFG   - switch
C     DECFG  - carbon dioxide decoupled from growth
C     BNPFG  - switch to use nitrogen preference function from DSSAmt
C     CAMPR  - coefficient for calculating nitrogen preference factor
C     SUMGRN - net growth that affects available N 
C     NMINC  - minimum concentration of inorganic N species in mg N/L
C     PO4    - concentration of orthophosphorus as P in mg P/L
C     TAM    - total ammonia (nh3 + nh4) in mg N/L
C     NO3    - dissolved nitrate concentration in mg/L
C     ALCO2  - algal uptake of CO2 due to growth
C     TAMALG - algal uptake of TAM due to growth
C     NO3ALG - algal uptake of NO3 due to growth
C     PO4ALG - algal uptake of PO4 due to growth
C
C     + + + LOCAL VARIABLES + + +
      REAL       GROWN,ALTAM,ALNO3,NO3LIM,TAMLIM,TAMS,NO3S,ALNPR2
C     conversions for elements in mg per umol
      REAL       NMGPUM,CMGPUM,PMGPUM,PRCT99
      PARAMETER  (NMGPUM=0.0140067,CMGPUM=0.012011,PMGPUM=0.0309738)
      PARAMETER  (PRCT99 = 0.99)
C
C     + + + END SPECIFICATIONS + + +
C
C     calculate po4 balance subsequent to algal uptake or release;
C     0.031 is the conversion from umoles p per liter to mg of p per
C     liter
      PO4   = PO4- PMGPUM*GROW
      PO4ALG= -PMGPUM*GROW
C
      TAMALG= 0.0
      IF (NSFG .NE. 0) THEN
C       calculate tam balance subsequent to algal uptake or release
C       express calculated growth rate in terms of equivalent
C       nitrogen; grown is expressed as umoles nitrogen per interval;
C       use accumulated growth that affects available N (SUMGRN)
        GROWN= SUMGRN*CVBPN
C
        IF (SUMGRN .LT. 0.0) THEN
C         algal respiration exceeds growth; nitrogen released by
C         respiration is released in the form of tam; no uptake or
C         release of no3 occurs
          ALTAM= GROWN
          ALNO3= 0.0
        ELSE
C         calculate amount of n uptake which is no3 and amount which
C         is tam
C
C         check nitrogen preference flag; if BNPFG.NE.1 use original
C         approach of %NO3, if BNPFG.EQ.1 use preference function
C         from DSSAMt/SSAMIV
          IF (BNPFG .EQ. 1) THEN
C           use DSSAMt ammonia preference function, equal to 1-ALNPR
            ALNPR2= 1.0- (CAMPR*TAM) / (CAMPR*TAM+ NO3)
          ELSE
            ALNPR2= ALNPR
          END IF
C
          ALNO3= ALNPR2*GROWN
          ALTAM= GROWN- ALNO3
C         check that computed uptake of no3 does not consume more
C         than 99 percent of available free no3; if it does, satisfy
C         excess demand with free tam; no3lim is expressed as umoles
C         n per liter per interval
          NO3LIM= (PRCT99/NMGPUM)*NO3
C
          IF (ALNO3 .GT. NO3LIM) THEN
            ALTAM = ALTAM + ALNO3 - NO3LIM
            ALNO3 = NO3LIM
          ELSE
C           check that calculated uptake of tam does not consume
C           more than 99 percent of available free tam; if it does,
C           satisfy excess demand with free no3; tamlim is expressed
C           as umoles n per liter per interval
            TAMLIM = (PRCT99/NMGPUM)*TAM
C
            IF (ALTAM .GT. TAMLIM) THEN
              ALNO3 = ALNO3 + ALTAM - TAMLIM
              ALTAM = TAMLIM
            ELSE
C             calculated uptake of inorganic nitrogen is acceptable
            END IF
          END IF
        END IF
C
C       calculate net uptake or release of tam by algae; .014 is
C       the conversion from umoles of n per liter per interval to
C       mg n per liter per interval
        TAMS   = TAM
        TAMALG = -NMGPUM*ALTAM
        TAM    = TAM - NMGPUM*ALTAM
        IF (TAM .LT. NMINC) THEN
          TAMALG = -TAMS
        END IF
        IF (TAM .LT. NMINC) THEN
          TAM = 0.0
        END IF
      ELSE
C       all inorganic n is in the form of no3
        ALNO3 = SUMGRN*CVBPN
      END IF
C
C     calculate no3 balance subsequent to algal uptake or release;
C     eliminate insignificant values of no3
      NO3S   = NO3
      NO3ALG = -NMGPUM*ALNO3
      NO3    = NO3 - NMGPUM*ALNO3
      IF (NO3 .LT. NMINC) THEN
        NO3ALG = -NO3S
      END IF
C
      IF (NO3 .LT. NMINC) THEN
        NO3 = 0.0
      END IF
C
      IF ((PHFG .NE. 0) .AND. (DECFG .EQ. 0)) THEN
C       calculate amount of algal uptake of co2; alco2 is expressed
C       as mg co2-c/liter
        ALCO2 = GROW*CVBPC*CMGPUM
      ELSE
        ALCO2 = 0.0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   BALRX2
     I                    (BALLIT,TW,TAMFG,NSFG,DELT60,CVBPN,PHFG,
     I                     DECFG,CVBPC,ALNPR,CVBO,REFR,CVNRBO,CVPB,
     I                     DEPCOR,LIMIT,CVBCL,CO2,NUMBAL,MBALGR,CMMPB,
     I                     CMMNB,BALR20,TCBALG,BALVEL,CMMV,BFIXFG,
     I                     CSLIT,CMMD1,CMMD2,TCBALR,FRRIF,CREMVL,
     I                     CMMBI,BINV,TCGRAZ,CSLOF1,CSLOF2,MINBAL,
     I                     FRAVL,BNPFG,CAMPR,NMINGR,PMINGR,CMINGR,
     I                     LMINGR,NMINC,NMAXFX,GRORES,
     M                     PO4,NO3,TAM,DOX,ORN,ORP,ORC,BOD,BENAL,
     O                     LIMBAL,BACO2,BALCLA,DOBALG,BODBAL,TAMBAL,
     O                     NO3BAL,PO4BAL,BALGRO,BDTH,BALORN,BALORP,
     O                     BALORC)
C
C     + + + PURPOSE + + +
C     Simulate behavior of up to four types of benthic algae using
C     algorithms adapted from the DSSAMt model.  This subroutine was
C     adapted from BALRX.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     TAMFG,NSFG,PHFG,DECFG,NUMBAL,BFIXFG(NUMBAL),BNPFG
      REAL        BALLIT,TW,DELT60,CVBPN,CVBPC,ALNPR,CVBO,REFR,CVNRBO,
     $            CVPB,DEPCOR,CVBCL,CO2,MBALGR(NUMBAL),CMMPB(NUMBAL),
     $            CMMNB(NUMBAL),BALR20(NUMBAL),TCBALG(NUMBAL),
     $            BALVEL,CMMV,CSLIT(NUMBAL),CMMD1(NUMBAL),CMMD2(NUMBAL),
     $            TCBALR(NUMBAL),FRRIF,CREMVL,CMMBI,BINV,TCGRAZ,
     $            CSLOF1(NUMBAL),CSLOF2(NUMBAL),MINBAL,FRAVL,CAMPR,
     $            NMINGR,PMINGR,CMINGR,LMINGR,NMINC,NMAXFX,
     $            GRORES(NUMBAL),PO4,NO3,TAM,DOX,ORN,ORP,ORC,BOD,
     $            BENAL(NUMBAL),LIMBAL(NUMBAL),BACO2,BALCLA(NUMBAL),
     $            DOBALG,BODBAL,TAMBAL,NO3BAL,PO4BAL,BALGRO(NUMBAL),
     $            BDTH(NUMBAL),BALORN,BALORP,BALORC
      CHARACTER*4 LIMIT(7)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     BALLIT - amount of light available to benthic algae in lgly/min
C     TW     - water temperature in degrees C
C     TAMFG  - switch for simulation of ammonia nitrogen
C     NSFG   - switch for algal uptake of both NO3 and TAM
C     DELT60 - simulation time interval in hours
C     CVBPN  - phosphorus to nitrogen equivalency
C     PHFG   - switch for simulation of pH in section PHCARB
C     DECFG  - switch for carbon dioxide decoupled from growth
C     CVBPC  - phosphorus to carbon equivalency
C     ALNPR  - fraction of N requirement for growth satisfied by NO3
C     CVBO   - conversion from biomass to oxygen
C     REFR   - refractory fraction
C     CVNRBO - conversion from biomass to BOD
C     CVPB   - conversion between biomass and umol P/L
C     DEPCOR - depth correction factor to convert from areal to volume terms
C     LIMIT  - label indicating which factor is limiting algal growth
C     CVBCL  - conversion between biomass and ug chla/L
C     CO2    - concentration of CO2 as carbon
C     NUMBAL - number of benthic algae types in the model
C     MBALGR - vector of optimal benthic algae growth rates at 20C
C     CMMPB  - vector of phosphorus half-saturation coefficients
C     CMMNB  - vector of nitrogen half-saturation coefficients
C     BALR20 - vector of benthic algal respiration rates at 20C
C     TCBALG - vector of temperature correction coefficients for growth
C     BALVEL - stream velocity for benthic algae
C     CMMV   - velocity half-saturation coefficients for nutrients
C     BFIXFG - vector of switches for whether N-fixing blue-green
C     CSLIT  - vector of saturation light levels for algae in lgly/min
C     CMMD1  - vector of intercepts for benthic algae density functions
C     CMMD2  - vector of half-saturation coeff. for density functions
C     TCBALR - vector of temperature coefficients for respiration
C     FRRIF  - fraction of reach that is riffle
C     CREMVL - coeff. of invertebrate removal in mg algae/mg invert./ivl
C     CMMBI  - half-sat. coeff. for invertebrate removal in mg OM/m2
C     BINV   - benthic macroinvertebrate biomass in mg OM/m2
C     TCGRAZ - temperature correction for grazing rate
C     CSLOF1 - vector of scour regression coeff. in per interval
C     CSLOF2 - vector of scour regression exponents for each algae type
C     MINBAL - minimum benthic algae biomass allowed in mg OM/m2
C     FRAVL  - fraction of nonrefractory biomass for avail. nutrients
C     BNPFG  - switch for nitrogen preference function (1=DSSAMt's)
C     CAMPR  - coeff. for calculating nitrogen preference factor
C     NMINGR - minimum nitrate concentration for algal growth
C     PMINGR - minimum phosphate concentration for algal growth
C     CMINGR - minimum CO2-carbon concentration for algal growth
C     LMINGR - minimum light intensity for algal growth
C     NMINC  - minimum concentration of inorganic N species in mg N/L
C     NMAXFX - maximum available nitrogen concentration for n-fixation to occur
C     GRORES - fraction of photorespiration required to support growth for each species
C     PO4    - dissolved inorganic phosphorus in mg P /L
C     NO3    - dissolved nitrate concentration in mg/L
C     TAM    - total ammonia (nh3 + nh4) in mg n/L
C     DOX    - dissolved oxygen concentration in mg/L
C     ORN    - dead refractory organic nitrogen
C     ORP    - dead refractory organic phosphorus
C     ORC    - dead refractory organic carbon
C     BOD    - biochemical oxygen demand
C     BENAL  - benthic algae biomass for each species in mg OM/m2
C     LIMBAL - limiting factor for benthic algae growth for each species
C     BACO2  - effect on dissolved CO2 due to benthic algae
C     BALCLA - benthic algae biomass in ug chl-a/m2 for each species
C     DOBALG - effect on dissolved O2 due to benthic algae
C     BODBAL - effect on BOD due to benthic algae
C     TAMBAL - effect on ammonia due to benthic algae
C     NO3BAL - effect on nitrate due to benthic algae
C     PO4BAL - effect on orthophosphate due to benthic algae
C     BALGRO - net growth/respiration of each species of benthic algae
C     BDTH   - death/removal of each species of benthic algae
C     BALORN - effect on organic refractory nitrogen due to benthic algae
C     BALORP - effect on organic refractory phosphorus due to benthic algae
C     BALORC - effect on organic refractory carbon due to benthic algae
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,NFIXFG
      REAL       BAL(4),GROBA,DTHBAL(4),RESBA,GROBAL(4),SUMBA,SUMBAL,
     $           GROTOT,GROTMP,SUMGRO,SUMDTH,BALMIN,GROBAN,GRTOTN,
     $           GROTM2,SUMGRN,CREM,AVLPHO,AVLNIT
C     conversions for elements in mg per umol
      REAL       NMGPUM,CMGPUM,PMGPUM
      PARAMETER (NMGPUM=0.0140067,CMGPUM=0.012011,PMGPUM=0.0309738)
C
C     + + + EXTERNALS + + +
      EXTERNAL   ALGRO2,GROCHK,BALREM,ORGBAL,NUTUP2
C
C     + + + END SPECIFICATIONS + + +
C
C     compute total biomass of all benthic algal types
      SUMBA= 0.0
      DO 10 I= 1, NUMBAL
        SUMBA= SUMBA+ BENAL(I)
 10   CONTINUE
C     convert to umoles P/l
      SUMBAL= (SUMBA/CVPB)*DEPCOR*FRRIF
C
C     initialize variables for cumulative net growth/removal
C     of benthic algae types, and for growth affecting available N conc.
      GROTOT = 0.0
      GRTOTN = 0.0
      SUMGRO = 0.0
      SUMGRN = 0.0
      SUMDTH = 0.0
C
      DO 20 I = 1, NUMBAL
C
C       convert benal to units of umoles phosphorus/l (bal) for
C       internal calculations, and adjust for % riffle to which
C       periphyton are limited
        BAL(I)= (BENAL(I)/CVPB)*DEPCOR*FRRIF
C
C       compute unit growth and respiration rates
        CALL ALGRO2 (BALLIT,PO4,NO3,TW,MBALGR(I),CMMPB(I),
     I               TAMFG,TAM,NSFG,CMMNB(I),BALR20(I),
     I               DELT60,LIMIT,TCBALG(I),BALVEL,CMMV,BFIXFG(I),
     I               CSLIT(I),CMMD1(I),CMMD2(I),SUMBA,TCBALR(I),
     I               NMINGR,PMINGR,LMINGR,NMAXFX,GRORES(I),
     O               NFIXFG,LIMBAL(I),GROBA,RESBA)
C
C       calculate net growth rate of algae; grobal is expressed as
C       umoles phosphorus per liter per interval; benthic algae growth
C       will be expressed in terms of volume rather than area for the
C       duration of the subroutines subordinate to balrx; the output
C       values for benthic algae are converted to either mg biomass per
C       sq meter or mg chla per sq meter, whichever the user
C       specifies
        GROBAL(I)= (GROBA- RESBA)*BAL(I)
C
C       track growth that affects water column available N concentrations
        IF (NFIXFG .NE. 1) THEN
C         algae are not fixing N, net growth affects concentrations
          GROBAN= GROBAL(I)
        ELSE
C         algae that are fixing N affect water column N through respiration only
          GROBAN= -RESBA * BAL(I)
        END IF
C
C       calculate cumulative net growth of algal types simulated so far
        GROTOT= GROTOT+ GROBAL(I)
C
C       calculate cumulative algal growth that affects available N
        GRTOTN= GRTOTN+ GROBAN
C
        IF ( (GROBAL(I) .GT. 0.0) .AND. (GROTOT .GT. 0.0) ) THEN
C         check that cumulative growth rate of algal types does not exceed
C         limitations imposed by the availability of required nutrients;
C         if so, reduce growth rate of last algal type
C
C         set temporary variable for comparison purposes
          GROTMP= GROTOT
C
          CALL GROCHK (PO4,NO3,TAM,PHFG,DECFG,CO2,CVBPC,CVBPN,NSFG,
     I                 NMINGR,PMINGR,CMINGR,NFIXFG,GRTOTN,
     M                 GROTOT)
          IF (GROTOT .LT. 0.0) THEN
C           this should never happen
            GROTOT= 0.0
          END IF
C
C         compare nutrient-checked growth to original cumulative total,
          IF (GROTOT .LT. GROTMP) THEN
C           adjust growth rate of last algal type
            GROBAL(I)= GROBAL(I)- (GROTMP- GROTOT)
C           track changes in growth that affect available N concentrations
            IF (NFIXFG .NE. 1) THEN
C             N-fixation not occurring, all growth affects N
              GROBAN= GROBAL(I)
            ELSE
C             N-fixation is occurring, proportionately adjust
C             respiration (GROBAN)
              GROBAN= GROBAN * GROTOT/GROTMP
            END IF
          END IF
        END IF
C
C       calculate benthic algae removal
        CREM= CREMVL*DEPCOR*FRRIF
        CALL BALREM (CREM,SUMBA,SUMBAL,CMMBI,TCGRAZ,TW,BINV,BAL(I),
     I               CSLOF1(I),CSLOF2(I),BALVEL,
     O               DTHBAL(I))
C
C       add the net growth
        BAL(I)= BAL(I)+ GROBAL(I)
C
        BALMIN= MINBAL*DEPCOR*FRRIF
        IF ( (BAL(I) .LT. BALMIN) .AND. (GROBAL(I) .LT. 0.0) ) THEN
C         adjust net growth rate so that population does not fall
C         below minimum level
C
C         set temporary variable for growth
          GROTM2= GROBAL(I)
          GROBAL(I)= GROBAL(I)+ (BALMIN- BAL(I))
          BAL(I)= BALMIN
C         adjust growth that affects available N concentrations
          IF (NFIXFG .NE. 1) THEN
C           N-fixation not occurring, all growth affects N
            GROBAN= GROBAL(I)
          ELSE
C           N-fixation is occurring, proportionately adjust
C           respiration (GROBAN)
            GROBAN= GROBAN * GROBAL(I) / GROTM2
          END IF
        END IF
C
C       subtract death/removal
        BAL(I)= BAL(I)- DTHBAL(I)
C
        IF (BAL(I) .LT. BALMIN) THEN
C         adjust death rate so that population does not
C         drop below minimum level
          DTHBAL(I)= DTHBAL(I)- (BALMIN- BAL(I))
          IF (DTHBAL(I).LT.0.0) THEN
            DTHBAL(I)= 0.0
          END IF
          BAL(I)= BALMIN
        END IF
C
C       calculate total net growth and removal of all benthic algae types
        SUMGRO= SUMGRO+ GROBAL(I)
        SUMDTH= SUMDTH+ DTHBAL(I)
        SUMGRN= SUMGRN + GROBAN
C       update internal loop tracking variables for cumulative growth 
C       to account for GROCHK and minimum biomass adjustments
        GROTOT= SUMGRO
        GRTOTN= SUMGRN
C
 20   CONTINUE
C
C     update do state variable to account for net effect of benthic
C     algae photosynthesis and respiration
      DOBALG= CVPB*CVBO*SUMGRO
C
      IF (DOX .GT. -DOBALG) THEN
C       enough oxygen available to satisfy demand
        DOX= DOX+ DOBALG
      ELSE
C       take only available oxygen
        DOBALG= -DOX
        DOX   = 0.0
      END IF
C
C     calculate amount of refractory organic constituents which result
C     from benthic algae death
      BALORN= REFR*SUMDTH*CVBPN*NMGPUM
      BALORP= REFR*SUMDTH*PMGPUM
      BALORC= REFR*SUMDTH*CVBPC*CMGPUM
C
C     add to ORC the carbon associated with nutrients immediately
C     released to the available pool from removed benthic algal
C     biomass
      BALORC= BALORC+ FRAVL*(1.0- REFR)*SUMDTH*CVBPC*CMGPUM
C
C     calculate amount of nonrefractory organics (bod) which result
C     from benthic algae death
      BODBAL= CVNRBO*(1.0- FRAVL)*CVPB*SUMDTH
C
C     perform materials balance resulting from benthic algae death
      CALL ORGBAL (BALORN,BALORP,BALORC,BODBAL,
     M             ORN,ORP,ORC,BOD)
C
C     perform materials balance resulting from uptake of nutrients
C     by benthic algae
      CALL NUTUP2 (SUMGRO,NSFG,CVBPN,ALNPR,CVBPC,PHFG,DECFG,BNPFG,
     I             CAMPR,SUMGRN,NMINC,
     M             PO4,TAM,NO3,
     O             BACO2,TAMBAL,NO3BAL,PO4BAL)
      BACO2= -BACO2
C
C     update available nutrient pools with nutrients immediately
C     released from benthic algal biomass removal processes; nutrients
C     immediately cycled are calculated as a fraction (FRAVL) of the
C     nonrefractory biomass
      AVLPHO= FRAVL*(1.0- REFR)*SUMDTH*PMGPUM
      PO4= PO4+ AVLPHO
      PO4BAL= PO4BAL+ AVLPHO
C
C     nitrogen is released as tam if tam is simulated otherwise as no3
      AVLNIT= FRAVL*(1.0- REFR)*SUMDTH*CVBPN*NMGPUM
      IF (TAMFG .NE. 0) THEN
        TAM= TAM+ AVLNIT
        TAMBAL= TAMBAL+ AVLNIT
      ELSE
        NO3= NO3+ AVLNIT
        NO3BAL= NO3BAL+ AVLNIT
      END IF
C
C     convert biomass to units of mg biomass/m2 and ug chlorophyll a/m2
      DO 30 I = 1, NUMBAL
        BENAL(I) = (BAL(I)*CVPB)/(DEPCOR*FRRIF)
        BALGRO(I)= (GROBAL(I)*CVPB)/(DEPCOR*FRRIF)
        BDTH(I)=   (DTHBAL(I)*CVPB)/(DEPCOR*FRRIF)
        BALCLA(I)=  BENAL(I)*CVBCL
 30   CONTINUE
C
      RETURN
      END
