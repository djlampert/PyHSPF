C
C
C
      SUBROUTINE   PPWTGS
C
C     + + + PURPOSE + + +
C     Process input for section pwtgas
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PWTGAS1 + + +
      INCLUDE    'cplps.inc'
      INCLUDE    'crin2.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I1,I2,I4
      REAL      ELEV,RVAL(5)
C
C     + + + EXTERNALS + + +
      EXTERNAL  ITABLE,RTABLE
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' PROCESSING INPUT FOR SECTION PWTGAS')
 2010 FORMAT (/,' FINISHED PROCESSING INPUT FOR SECTION PWTGAS')
C
C     + + + END SPECIFICATIONS + + +
C
      I1= 1
C
      IF (OUTLEV.GT.1) THEN
C       processing section message
        WRITE (MESSU,2000)
      END IF
C
C     process values in table-type pwt-parm1
      I2= 58
      I4= 4
      CALL ITABLE (I2,I1,I4,UUNITS,
     M             PGPM1)
C
C     process values in table-type pwt-parm2
      I2= 59
      I4= 5
      CALL RTABLE (I2,I1,I4,UUNITS,
     M             RVAL)
      ELEV= RVAL(1)
C
C     compute factor for correcting dissolved gas
C     saturated concentrations for altitude
      ELEVGC= ((288.0- 0.00198*ELEV)/288.0)**5.256
C
C     dissolved gas concentrations in interflow
C     and groundwater
      IDOXP= RVAL(2)
      ICO2P= RVAL(3)
      ADOXP= RVAL(4)
      ACO2P= RVAL(5)
C
C     process values in table-type lat-factor
      I2= 60
      I4= 4
      CALL RTABLE (I2,I1,I4,UUNITS,
     M             LIFAC)
C
      IF (PWATFG.EQ.0 .AND. SEDFG.EQ.0) THEN
C       need to set csnofg
C        I2= 26
C        I4= 1
C        CALL ITABLE(I2,I1,I4,UUNITS,
C     M              CSNOFG)
        CSNOFG= 0
      END IF
C
      IF (IDVFG.EQ.1) THEN
C       check for monthly values of interflow do concentration -
C       table-type mon-ifwdox
        I2= 61
        I4= 12
        CALL RTABLE (I2,I1,I4,UUNITS,
     M               IDOXPM)
      END IF
C
      IF (ICVFG.EQ.1) THEN
C       check for monthly values of interflow co2 concentration -
C       table-type mon-ifwco2
        I2= 62
        I4= 12
        CALL RTABLE (I2,I1,I4,UUNITS,
     M               ICO2PM)
      END IF
C
      IF (GDVFG.EQ.1) THEN
C       check for monthly values of groundwater do concentration -
C       table-type mon-grnddox
        I2= 63
        I4= 12
        CALL RTABLE (I2,I1,I4,UUNITS,
     M               ADOXPM)
      END IF
C
      IF (GCVFG.EQ.1) THEN
C       check for monthly values of groundwater co2 concentration -
C       table-type mon-grndco2
        I2= 64
        I4= 12
        CALL RTABLE (I2,I1,I4,UUNITS,
     M               ACO2PM)
      END IF
C
C     initial temperatures - table-type pwt-temps
      I2= 65
      I4= 3
      CALL RTABLE (I2,I1,I4,UUNITS,
     M             PGST1)
C
C     initial do and co2 concentrations - table-type pwt-gases
      I2= 66
      I4= 6
      CALL RTABLE (I2,I1,I4,UUNITS,
     M             PGST2)
C
      IF (OUTLEV.GT.1) THEN
C       end processing message
        WRITE (MESSU,2010)
      END IF
C
      RETURN
      END
C
C     4.2(1).6
C
      SUBROUTINE   PWTGAS
C
C     + + + PURPOSE + + +
C     Estimate water temperature, dissolved oxygen, and carbon
C     dioxide in the outflows from a pervious land
C     segment. calculate associated fluxes through exit gates
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PWTGAS2 + + +
      INCLUDE 'cplps.inc'
      INCLUDE 'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     REQFG,TSSUB(2),FLGVAL
      REAL        ABSTMP,DUMMY
      CHARACTER*6 OPTYP,TSNAM,SECNAM,MSECNM,OPFGNM
C
C     + + + FUNCTIONS + + +
      REAL    DAYVAL
C
C     + + + EXTERNALS + + +
      EXTERNAL   DAYVAL,HREQTS
C
C     + + + DATA INITIALIZATIONS + + +
      DATA TSSUB/1,1/
      DATA OPTYP,SECNAM/'PERLND','PWTGAS'/
C
C     + + + END SPECIFICATIONS + + +
C
C     get hydrological time series
      IF (PWATFG.EQ.0) THEN
C       get time series from inpad
CTHJ        SURO= PAD(SOFP+IVL1)
        REQFG= 3
        MSECNM= 'PWATER'
        TSNAM= 'SURO  '
        CALL HREQTS (SOFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,LSNO,
     I               TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,FLGVAL,
     O               SURO)
CTHJ        IFWO= PAD(IOFP+IVL1)
        TSNAM= 'SURO  '
        CALL HREQTS (IOFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,LSNO,
     I               TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,FLGVAL,
     O               IFWO)
CTHJ        AGWO= PAD(AOFP+IVL1)
        CALL HREQTS (AOFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,LSNO,
     I               TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,FLGVAL,
     O               AGWO)
        IF (SLIFP .GE. 1) THEN
C         surface lateral inflow present
          SURLI= PAD(SLIFP+IVL1)
        ELSE
C         no surface lateral inflow
          SURLI= 0.0
        END IF
        IF (ILIFP .GE. 1) THEN
C         interflow lateral inflow present
          IFWLI= PAD(ILIFP+IVL1)
        ELSE
C         no interflow lateral inflow
          IFWLI= 0.0
        END IF
        IF (ALIFP .GE. 1) THEN
C         baseflow lateral inflow present
          AGWLI= PAD(ALIFP+IVL1)
        ELSE
C         no baseflow lateral inflow
          AGWLI= 0.0
        END IF
      ELSE
C       outflows and lat inflows are available from pwater
      END IF
C
C     outflow temperatures are based on soil temperatures - units
C     are deg. c
      IF (PSTFG.EQ.0) THEN
C       get soil temperatures from the inpad
CTHJ        SLTMP= PAD(SLTFP+IVL1)
        REQFG= 3
        MSECNM= 'PSTEMP'
        TSNAM= 'SLTMP '
        CALL HREQTS (SLTFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,LSNO,
     I               TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,FLGVAL,
     O               SLTMP)
CTHJ        ULTMP= PAD(ULTFP+IVL1)
        TSNAM= 'ULTMP '
        CALL HREQTS (ULTFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,LSNO,
     I               TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,FLGVAL,
     O               ULTMP)
CTHJ        LGTMP= PAD(LGTFP+IVL1)
        TSNAM= 'LGTMP '
        CALL HREQTS (LGTFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,LSNO,
     I               TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,FLGVAL,
     O               LGTMP)
      ELSE
C       soil temperatures have been calculated in pstemp
      END IF
C
C     get surface lateral inflow temp and concentrations
      IF (SURLI .GT. 0.0) THEN
C       there is lateral inflow
        IF (SLITFP .GE. 1) THEN
C         there is temperature of surface lateral inflow
          SLITMP= PAD(SLITFP+IVL1)
        ELSE
C         no defined temp for surface lateral inflow
          SLITMP= -1.0E30
        END IF
        IF (SLDOFP .GE. 1) THEN
C         there is do conc of surface lateral inflow
          SLIDOX= PAD(SLDOFP+IVL1)
        ELSE
C         no defined do conc for surface lateral inflow
          SLIDOX= -1.0E30
        END IF
        IF (SLCOFP .GE. 1) THEN
C         there is co2 conc of surface lateral inflow
          SLICO2= PAD(SLCOFP+IVL1)
        ELSE
C         no defined temp for surface lateral inflow
          SLICO2= -1.0E30
        END IF
      ELSE
C       no surface lateral inflow
        SLITMP= -1.0E30
        SLIDOX= -1.0E30
        SLICO2= -1.0E30
      END IF
C
      IF (SURO .GT. 0.0) THEN
C       there is surface outflow
C
C       local surface outflow temp equals surface soil temp
        SOTMP= SLTMP
        IF (SOTMP.LT.0.5) THEN
C         min water temp
          SOTMP= 0.5
        END IF
        IF (CSNOFG .EQ. 1) THEN
C         effects of snow are considered
C         adjust surface outflow temperature if snowmelt is occurring
          IF (SNOWFG .EQ. 0) THEN
C           section SNOW inactive - need time series
CTHJ            WYIELD= PAD(WYFP+IVL1)
            REQFG= 5
            TSNAM= 'WYIELD'
            MSECNM= 'SNOW  '
            CALL HREQTS (WYFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,LSNO,
     I                   TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,CSNOFG,
     O                   WYIELD)
          ELSE
C           wyield is available from snow
          END IF
          IF (WYIELD .GT. 0.0) THEN
C           snowmelt is occuring - use min temp
            SOTMP= 0.5
          END IF
        END IF
C
C       oxygen calculation
        DUMMY= SOTMP*(0.007991-0.77774E-4*SOTMP)
        SODOX= (14.652+SOTMP*(-0.41022+DUMMY))*ELEVGC
C
C       carbon dioxide calculation
        ABSTMP= SOTMP+ 273.16
        DUMMY = 2385.73/ABSTMP- 14.0184+ 0.0152642*ABSTMP
        SOCO2 = 10.0**DUMMY*3.16E-04*ELEVGC*12000.0
C
        IF ( (SURLI .GT. 0.0) .AND. (LIFAC(2) .GT. 0.0) ) THEN
C         check for effects of lateral inflow
C
          IF (SLITMP .GE. -1.0E10) THEN
C           there is temperature of surface lateral inflow
            SOTMP= SLITMP*LIFAC(2)+ SOTMP*(1.0- LIFAC(2))
          END IF
          IF (SLIDOX .GE. 0.0) THEN
C           there is do conc of surface lateral inflow
            SODOX= SLIDOX*LIFAC(2)+ SODOX*(1.0- LIFAC(2))
          END IF
          IF (SLICO2 .GE. 0.0) THEN
C           there is co2 conc of surface lateral inflow
            SOCO2= SLICO2*LIFAC(2)+ SOCO2*(1.0- LIFAC(2))
          END IF
        END IF
      ELSE
C       for zero outflow, report outflow temps and concentrations as "undefined"
        SOTMP= -1.0E30
        SODOX= -1.0E30
        SOCO2= -1.0E30
      END IF
C
C     get interflow lateral inflow temp and concentrations
      IF (IFWLI .GT. 0.0) THEN
C       there is lateral inflow
        IF (ILITFP .GE. 1) THEN
C         there is temperature of interflow lateral inflow
          ILITMP= PAD(ILITFP+IVL1)
        ELSE
C         no defined temp for interflow lateral inflow
          ILITMP= -1.0E30
        END IF
        IF (ILDOFP .GE. 1) THEN
C         there is do conc of interflow lateral inflow
          ILIDOX= PAD(ILDOFP+IVL1)
        ELSE
C         no defined do conc for interflow lateral inflow
          ILIDOX= -1.0E30
        END IF
        IF (ILCOFP .GE. 1) THEN
C         there is co2 conc of interflow lateral inflow
          ILICO2= PAD(ILCOFP+IVL1)
        ELSE
C         no defined temp for interflow lateral inflow
          ILICO2= -1.0E30
        END IF
      ELSE
C       no interflow lateral inflow
        ILITMP= -1.0E30
        ILIDOX= -1.0E30
        ILICO2= -1.0E30
      END IF
C
      IF (DAYFG .EQ. 1) THEN
C       it is the first interval of the day
        IF (IDVFG .EQ. 1) THEN
C         interflow dox parameter is allowed to vary throughout the year
C         interpolate for the daily value
C         linearly interpolate idoxp between two values from the
C         monthly array idoxpm(12)
          IDOXP= DAYVAL(IDOXPM(MON),IDOXPM(NXTMON),DAY,NDAYS)
        ELSE
C         interflow dox parameter does not vary throughout the
C         year. idoxp value has been supplied by the run interpreter
        END IF
C
        IF (ICVFG .EQ. 1) THEN
C         interflow co2 parameter is allowed to vary throughout the year
C         interpolate for the daily value
C         linearly interpolate ico2p between two values from the
C         monthly array ico2pm(12)
          ICO2P= DAYVAL(ICO2PM(MON),ICO2PM(NXTMON),DAY,NDAYS)
        ELSE
C         interflow co2 parameter does not vary throughout the year.
C         ico2p value has been supplied by the run interpreter
        END IF
      END IF
C
      IF (IFWO .GT. 0.0) THEN
C       there is interflow outflow
C
C       local interflow outflow temp equals upper soil temp
        IOTMP= ULTMP
        IF (IOTMP.LT.0.5) THEN
C         min water temp
          IOTMP= 0.5
        END IF
        IODOX= IDOXP
        IOCO2= ICO2P
C
        IF ( (IFWLI .GT. 0.0) .AND. (LIFAC(3) .GT. 0.0) ) THEN
C         check for effects of lateral inflow
C
          IF (ILITMP .GE. -1.0E10) THEN
C           there is temperature of interflow lateral inflow
            IOTMP= ILITMP*LIFAC(3)+ IOTMP*(1.0- LIFAC(3))
          END IF
          IF (ILIDOX .GE. 0.0) THEN
C           there is do conc of interflow lateral inflow
            IODOX= ILIDOX*LIFAC(3)+ IODOX*(1.0- LIFAC(3))
          END IF
          IF (ILICO2 .GE. 0.0) THEN
C           there is co2 conc of interflow lateral inflow
            IOCO2= ILICO2*LIFAC(3)+ IOCO2*(1.0- LIFAC(3))
          END IF
        END IF
      ELSE
C       for zero outflow, report outflow temps and concentrations as "undefined"
        IOTMP= -1.0E30
        IODOX= -1.0E30
        IOCO2= -1.0E30
      END IF
C
C     get baseflow lateral inflow temp and concentrations
      IF (AGWLI .GT. 0.0) THEN
C       there is lateral inflow
        IF (ALITFP .GE. 1) THEN
C         there is temperature of baseflow lateral inflow
          ALITMP= PAD(ALITFP+IVL1)
        ELSE
C         no defined temp for baseflow lateral inflow
          ALITMP= -1.0E30
        END IF
        IF (ALDOFP .GE. 1) THEN
C         there is do conc of baseflow lateral inflow
          ALIDOX= PAD(ALDOFP+IVL1)
        ELSE
C         no defined do conc for baseflow lateral inflow
          ALIDOX= -1.0E30
        END IF
        IF (ALCOFP .GE. 1) THEN
C         there is co2 conc of baseflow lateral inflow
          ALICO2= PAD(ALCOFP+IVL1)
        ELSE
C         no defined temp for baseflow lateral inflow
          ALICO2= -1.0E30
        END IF
      ELSE
C       no baseflow lateral inflow
        ALITMP= -1.0E30
        ALIDOX= -1.0E30
        ALICO2= -1.0E30
      END IF
C
      IF (DAYFG .EQ. 1) THEN
C       it is the first interval of the day
        IF (GDVFG .EQ. 1) THEN
C         groundwater flow dox parameter is allowed to vary
C         throughout the year
C         interpolate for the daily value
C         linearly interpolate adoxp between two values from the
C         monthly array adoxpm(12)
          ADOXP= DAYVAL(ADOXPM(MON),ADOXPM(NXTMON),DAY,NDAYS)
        ELSE
C         groundwater flow dox parameter does not vary throughout the
C         year. adoxp value has been supplied by the run interpreter
        END IF
C
        IF (GCVFG .EQ. 1) THEN
C         groundwater flow co2 parameter is allowed to vary
C         throughout the year
C         interpolate for the daily value
C         linearly interpolate aco2p between two values from the
C         monthly array aco2pm(12)
          ACO2P= DAYVAL(ACO2PM(MON),ACO2PM(NXTMON),DAY,NDAYS)
        ELSE
C         groundwater flow co2 parameter does not vary throughout the
C         year.  aco2p value has been supplied by the run interpreter
        END IF
      END IF
C
      IF (AGWO .GT. 0.0) THEN
C       there is baseflow
C
C       local baseflow temp equals lower/gw soil temp
        AOTMP= LGTMP
        IF (AOTMP.LT.0.5) THEN
C         min water temp
          AOTMP= 0.5
        END IF
        AODOX= ADOXP
        AOCO2= ACO2P
C
        IF ( (AGWLI .GT. 0.0) .AND. (LIFAC(4) .GT. 0.0) ) THEN
C         check for effects of lateral inflow
C
          IF (ALITMP .GE. -1.0E10) THEN
C           there is temperature of baseflow lateral inflow
            AOTMP= ALITMP*LIFAC(4)+ AOTMP*(1.0- LIFAC(4))
          END IF
          IF (ALIDOX .GE. 0.0) THEN
C           there is do conc of baseflow lateral inflow
            AODOX= ALIDOX*LIFAC(4)+ AODOX*(1.0- LIFAC(4))
          END IF
          IF (ALICO2 .GE. 0.0) THEN
C           there is co2 conc of baseflow lateral inflow
            AOCO2= ALICO2*LIFAC(4)+ AOCO2*(1.0- LIFAC(4))
          END IF
        END IF
      ELSE
C       for zero outflow, report outflow temps and concentrations as "undefined"
        AOTMP= -1.0E30
        AODOX= -1.0E30
        AOCO2= -1.0E30
      END IF
C
C     compute the outflow of heat energy in water - units are
C     deg. c-in./ivl
      SOHT= SOTMP*SURO
      IOHT= IOTMP*IFWO
      AOHT= AOTMP*AGWO
      POHT= SOHT+IOHT+AOHT
C
C     calculate outflow mass of dox - units are mg-in./l-ivl
      SODOXM= SODOX*SURO
      IODOXM= IODOX*IFWO
      AODOXM= AODOX*AGWO
      PODOXM= SODOXM+ IODOXM+ AODOXM
C
C     calculate outflow mass of co2 - units are mg-in./l-ivl
      SOCO2M= SOCO2*SURO
      IOCO2M= IOCO2*IFWO
      AOCO2M= AOCO2*AGWO
      POCO2M= SOCO2M+ IOCO2M+ AOCO2M
C
      RETURN
      END
C
C     4.2(1).15.1.4
C
      SUBROUTINE   PWGACC
     I                    (FRMROW,TOROW)
C
C     + + + PURPOSE + + +
C     Accumulate fluxes for section pwtgas
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    FRMROW,TOROW
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FRMROW - row containing incremental flux accumulation
C     TOROW  - flux row to be incremented
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PWTGAS2 + + +
      INCLUDE    'cplps.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I3,I6
C
C     + + + EXTERNALS + + +
      EXTERNAL   ACCVEC
C
C     + + + END SPECIFICATIONS + + +
C
      I3=3
      I6=6
C
      CALL ACCVEC (I3,PGCF1(1,FRMROW),
     M             PGCF1(1,TOROW))
C
      CALL ACCVEC (I6,PGCF2(1,FRMROW),
     M             PGCF2(1,TOROW))
C
      RETURN
      END
C
C     4.2(1).15.2.6
C
      SUBROUTINE   PWGPRT
     I                    (UNITFG,LEV,PRINTU,BINU)
C
C     + + + PURPOSE + + +
C     Convert quantities from internal to external units.
C     Note: local arrays have
C     identical sizes and structures to the corresponding arrays in
C     the osv apart from dropping the dimension lev for fluxes
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
C     + + + COMMON BLOCKS- SCRTCH, VERSION PWTGAS2 + + +
      INCLUDE    'cplps.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I,J,I0,I1,I3,I6,LATTMP,LATGAS,ACNT,CLEN(30),
     $              EXDAT(5)

      REAL          EFACTA,EFACTB,MFACTA,MFACTB,PCFLX1(3),PCFLX2(6),
     $              PSTAT1(3),PLITMP(3),PPOCO2,PPODOX,PPOHT,TFACTA,
     $              TFACTB,APRINT(30)
      CHARACTER*8   CCFLX1(3),CCFLX2(6),CSTAT1(3),CLITMP(3),CGST2(6)
      CHARACTER*256 CHEAD(30)
C
C     + + + EXTERNALS + + +
      EXTERNAL   TRNVEC,EXDATE
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' *** PWTGAS ***')
 2010 FORMAT (/,'   STATE VARIABLES')
 2020 FORMAT (/,31X,'<---SURFACE FLOW---><----INTERFLOW----->',
     $        '<-GROUNDWATER FLOW->')
 2030 FORMAT (  '     WATER TEMPERATURES',23X,'SOTMP',
     $        15X,'IOTMP',15X,'AOTMP')
 2040 FORMAT (  7X,'(DEG F)',17X,3(10X,G10.3))
 2050 FORMAT (  7X,'(DEG C)',17X,3(10X,G10.3))
 2055 FORMAT (  '     WATER TEMPERATURES',12X,'SLITMP     SOTMP',
     $        '    ILITMP     IOTMP    ALITMP     AOTMP')
 2056 FORMAT (  7X,'(DEG F)',17X,6G10.3)
 2057 FORMAT (  7X,'(DEG C)',17X,6G10.3)
 2060 FORMAT (/,'     DISSOLVED GASES',16X,
     $        'SODOX     SOCO2     IODOX     IOCO2     AODOX     AOCO2')
 2070 FORMAT (  7X,'(MG/L)',18X,6G10.3)
 2075 FORMAT (/,'     DISSOLVED GAS CONC (MG/L)')
 2076 FORMAT (  7X,'LATERAL INFLOW',10X,'    SLIDOX    SLICO2',
     $        '    ILIDOX    ILICO2    ALIDOX    ALICO2')
 2077 FORMAT (  7X,'OUTFLOW',17X,'     SODOX     SOCO2     IODOX',
     $        '     IOCO2     AODOX     AOCO2')
 2078 FORMAT (  31X,6G10.3)
 2080 FORMAT (/,'   FLUXES')
 2090 FORMAT (/,'     OUTFLOWS IN WATER',82X,'(TOTAL)')
 2100 FORMAT (  7X,'HEAT ENERGY (BTU/AC)',20X,'SOHT',16X,'IOHT',
     $        16X,'AOHT',16X,'POHT')
 2110 FORMAT (  7X,'HEAT ENERGY (KCAL/HA)',19X,'SOHT',16X,'IOHT',
     $        16X,'AOHT',16X,'POHT')
 2120 FORMAT (  9X,'(RELATIVE TO FREEZING)',4(10X,G10.4))
 2130 FORMAT (  7X,'DISSOLVED GASES (LB/AC)',5X,
     $  'SODOXM    SOCO2M    IODOXM    IOCO2M    AODOXM    AOCO2M',
     $        '    PODOXM    POCO2M')
 2140 FORMAT (  7X,'DISSOLVED GASES (KG/HA)',5X,
     $  'SODOXM    SOCO2M    IODOXM    IOCO2M    AODOXM    AOCO2M',
     $        '    PODOXM    POCO2M')
 2150 FORMAT (  31X,1P,8G10.3)
C
C     + + + END SPECIFICATIONS + + +
C
      I0=0
      I1=1
      I3=3
      I6=6
C
C     initialize array counter for binary printout, store variable
C     names in local strings for use in building binary headers
      ACNT = 0
      CSTAT1(1) = 'SOTMP'
      CSTAT1(2) = 'IOTMP'
      CSTAT1(3) = 'AOTMP'
      CLITMP(1) = 'SLITMP'
      CLITMP(2) = 'ILITMP'
      CLITMP(3) = 'ALITMP'
      CGST2(1)  = 'SODOX'
      CGST2(2)  = 'SOCO2'
      CGST2(3)  = 'IODOX'
      CGST2(4)  = 'IOCO2'
      CGST2(5)  = 'AODOX'
      CGST2(6)  = 'AOCO2'
      CCFLX2(1) = 'SODOXM'
      CCFLX2(2) = 'SOCO2M'
      CCFLX2(3) = 'IODOXM'
      CCFLX2(4) = 'IOCO2M'
      CCFLX2(5) = 'AODOXM'
      CCFLX2(6) = 'AOCO2M'
      CCFLX1(1) = 'SOHT'
      CCFLX1(2) = 'IOHT'
      CCFLX1(3) = 'AOHT'
C
C     assign values to parameters used for conversion from internal
C     to external units
C
      IF (UNITFG .EQ. 1) THEN
C       english system
C       parameters for variables with energy units
        EFACTA= 407960.
        EFACTB= 0.0
C
C       parameters for variables with temperature units
        TFACTA= 1.8
        TFACTB= 32.0
C
C       parameters for variables for dissolved gases with mass units
        MFACTA= 0.2266
        MFACTB= 0.0
      ELSE
C       metric system
C       parameters for variables with energy units
        EFACTA= 253900.
        EFACTB= 0.0
C
C       parameters for variables with temperature units
        TFACTA= 1.0
        TFACTB= 0.0
C
C       parameters for variables for dissolved gases with mass units
        MFACTA= 0.2540
        MFACTB= 0.0
      END IF
C
      LATTMP= 0
      LATGAS= 0
      IF ( (SLITFP .GE. 1) .OR. (ILITFP .GE. 1) .OR.
     $     (ALITFP .GE. 1) ) THEN
C      	there is temperature of lateral inflow
        LATTMP= 1
      END IF
      IF ( (SLDOFP .GE. 1) .OR. (ILDOFP .GE. 1) .OR.
     $     (ALDOFP .GE. 1) .OR. (SLCOFP .GE. 1) .OR.
     $     (ILCOFP .GE. 1) .OR. (ALCOFP .GE. 1) ) THEN
C      	there is gas conc of lateral inflow
        LATGAS= 1
      END IF
C
C     state variables with temperature units
      CALL TRNVEC (I3,PGST1,TFACTA,TFACTB,
     O             PSTAT1)
      IF (LATTMP .EQ. 1) THEN
C       convert lat inflow temps
        PLITMP(1)= SLITMP*TFACTA+ TFACTB
        PLITMP(2)= ILITMP*TFACTA+ TFACTB
        PLITMP(3)= ALITMP*TFACTA+ TFACTB
      END IF
C
C     state variables with concentration units do not have to be
C     converted
C     fluxes - energy units
      CALL TRNVEC (I3,PGCF1(1,LEV),EFACTA,EFACTB,
     O             PCFLX1)
C
C     fluxes of dissolved gases in mass units
      CALL TRNVEC (I6,PGCF2(1,LEV),MFACTA,MFACTB,
     O             PCFLX2)
C
C     do printout on unit printu
C
      IF (PRINTU .GT. 0 .AND. PFLAG(6) .LE. LEV) THEN
        WRITE (PRINTU,2000)
C
        WRITE (PRINTU,2010)
        WRITE (PRINTU,2020)
      END IF
C
      IF (LATTMP .EQ. 0) THEN
C       no lateral inflows
        IF (PRINTU .GT. 0 .AND. PFLAG(6) .LE. LEV) THEN
          WRITE (PRINTU,2030)
          IF (UNITFG .EQ. 1) THEN
C           english
            WRITE (PRINTU,2040)  PSTAT1
          ELSE
C           metric
            WRITE (PRINTU,2050)  PSTAT1
          END IF
        END IF
        IF (BINU .GT. 0 .AND. ABS(BFLAG(6)) .LE. LEV) THEN
C         compile values for binary printout
          DO 10 I = 1, 3
            ACNT = ACNT + 1
            APRINT(ACNT) = PSTAT1(I)
            CHEAD(ACNT) = CSTAT1(I)
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 10       CONTINUE
        END IF
      ELSE
C       lateral inflows
        IF (PRINTU .GT. 0 .AND. PFLAG(6) .LE. LEV) THEN
          WRITE (PRINTU,2055)
          IF (UNITFG .EQ. 1) THEN
C           english
            WRITE (PRINTU,2056)  PLITMP(1),PSTAT1(1),PLITMP(2),
     $                           PSTAT1(2),PLITMP(3),PSTAT1(3)
          ELSE
C           metric
            WRITE (PRINTU,2057)  PLITMP(1),PSTAT1(1),PLITMP(2),
     $                           PSTAT1(2),PLITMP(3),PSTAT1(3)	
          END IF
        END IF
        IF (BINU .GT. 0 .AND. ABS(BFLAG(6)) .LE. LEV) THEN
C         compile values for binary printout
          DO 20 I = 1, 3
            ACNT = ACNT + 1
            APRINT(ACNT) = PLITMP(I)
            CHEAD(ACNT) = CLITMP(I)
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
            ACNT = ACNT + 1
            APRINT(ACNT) = PSTAT1(I)
            CHEAD(ACNT) = CSTAT1(I)
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 20       CONTINUE
        END IF
      END IF
C
      IF (LATGAS .EQ. 0) THEN
C       no lat inflow conc for gases
        IF (PRINTU .GT. 0 .AND. PFLAG(6) .LE. LEV) THEN
          WRITE (PRINTU,2060)
          WRITE (PRINTU,2070)  PGST2
        END IF
        IF (BINU .GT. 0 .AND. ABS(BFLAG(6)) .LE. LEV) THEN
          DO 30 I = 1, 6
            ACNT = ACNT + 1
            APRINT(ACNT) = PGST2(I)
            CHEAD(ACNT) = CGST2(I)
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 30       CONTINUE
        END IF
      ELSE
C       lateral inflow conc for gases
        IF (PRINTU .GT. 0 .AND. PFLAG(6) .LE. LEV) THEN
          WRITE (PRINTU,2075)
          WRITE (PRINTU,2076)
          WRITE (PRINTU,2078)  SLIDOX,SLICO2,ILIDOX,ILICO2,ALIDOX,ALICO2
          WRITE (PRINTU,2077)
          WRITE (PRINTU,2078)  PGST2
        END IF
        IF (BINU .GT. 0 .AND. ABS(BFLAG(6)) .LE. LEV) THEN
          ACNT = ACNT + 1
          APRINT(ACNT) = SLIDOX
          CHEAD(ACNT) = 'SLIDOX'
          CLEN(ACNT) = 6
          ACNT = ACNT + 1
          APRINT(ACNT) = SLICO2
          CHEAD(ACNT) = 'SLICO2'
          CLEN(ACNT) = 6
          ACNT = ACNT + 1
          APRINT(ACNT) = ILIDOX
          CHEAD(ACNT) = 'ILIDOX'
          CLEN(ACNT) = 6
          ACNT = ACNT + 1
          APRINT(ACNT) = ILICO2
          CHEAD(ACNT) = 'ILICO2'
          CLEN(ACNT) = 6
          ACNT = ACNT + 1
          APRINT(ACNT) = ALIDOX
          CHEAD(ACNT) = 'ALIDOX'
          CLEN(ACNT) = 6
          ACNT = ACNT + 1
          APRINT(ACNT) = ALICO2
          CHEAD(ACNT) = 'ALICO2'
          CLEN(ACNT) = 6
          DO 40 I = 1, 6
            ACNT = ACNT + 1
            APRINT(ACNT) = PGST2(I)
            CHEAD(ACNT) = CGST2(I)
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 40       CONTINUE
        END IF
      END IF
C
      IF (PRINTU .GT. 0 .AND. PFLAG(6) .LE. LEV) THEN
        WRITE (PRINTU,2080)
        WRITE (PRINTU,2090)
      END IF
      PPOHT = PCFLX1(1)+ PCFLX1(2)+ PCFLX1(3)
      PPODOX= PCFLX2(1)+ PCFLX2(3)+ PCFLX2(5)
      PPOCO2= PCFLX2(2)+ PCFLX2(4)+ PCFLX2(6)
C
      IF (PRINTU .GT. 0 .AND. PFLAG(6) .LE. LEV) THEN
        IF (UNITFG.NE.1) GO TO 50
          WRITE (PRINTU,2100)
          GO TO 60
 50     CONTINUE
          WRITE (PRINTU,2110)
 60     CONTINUE
C
        WRITE (PRINTU,2120)  PCFLX1, PPOHT
      END IF
      IF (BINU .GT. 0 .AND. ABS(BFLAG(6)) .LE. LEV) THEN
        DO 90 I = 1, 3
          ACNT = ACNT + 1
          APRINT(ACNT) = PCFLX1(I)
          CHEAD(ACNT) = CCFLX1(I)
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 90     CONTINUE
        ACNT = ACNT + 1
        APRINT(ACNT) = PPOHT
        CHEAD(ACNT) = 'POHT'
        CLEN(ACNT) = 4
      END IF
C
      IF (PRINTU .GT. 0 .AND. PFLAG(6) .LE. LEV) THEN
        IF (UNITFG.NE.1) GO TO 70
          WRITE (PRINTU,2130)
          GO TO 80
 70     CONTINUE
          WRITE (PRINTU,2140)
 80     CONTINUE
C
        WRITE (PRINTU,2150)  PCFLX2, PPODOX, PPOCO2
      END IF
      IF (BINU .GT. 0 .AND. ABS(BFLAG(6)) .LE. LEV) THEN
        DO 100 I = 1, 6
          ACNT = ACNT + 1
          APRINT(ACNT) = PCFLX2(I)
          CHEAD(ACNT) = CCFLX2(I)
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 100    CONTINUE
        ACNT = ACNT + 1
        APRINT(ACNT) = PPODOX
        CHEAD(ACNT) = 'PODOXM'
        CLEN(ACNT) = 6
        ACNT = ACNT + 1
        APRINT(ACNT) = PPOCO2
        CHEAD(ACNT) = 'POCO2M'
        CLEN(ACNT) = 6
      END IF
C
      IF (BINU .GT. 0 .AND. ABS(BFLAG(6)) .LE. LEV) THEN
C       write binary output
        CALL EXDATE(
     I              DATIM,
     O              EXDAT)
        IF (BFLAG(6) .GT. 0) THEN
C         at start of run, write the header
          WRITE (BINU) I0,'PERLND  ',LSNO,'PWTGAS  ',
     1          (CLEN(I),(CHEAD(I)(J:J),J=1,CLEN(I)),I=1,ACNT)
C         set bflag to negative to not write headers anymore
          BFLAG(6) = -BFLAG(6)
        END IF
        WRITE (BINU) I1,'PERLND  ', LSNO,'PWTGAS  ',UNITFG,
     1               LEV,(EXDAT(I),I=1,5),(APRINT(I),I=1,ACNT)
      END IF
C
      RETURN
      END
C
C     4.2(1).15.3.4
C
      SUBROUTINE   PWGRST
     I                    (LEV)
C
C     + + + PURPOSE + + +
C     Reset all flux accumulators and those state variables
C     used in material balance check for section pwtgas
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   LEV
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEV    - current output level (2-pivl,3-day,4-mon,5-ann)
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PWTGAS2 + + +
      INCLUDE    'cplps.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I3,I6
C
C     + + + EXTERNALS + + +
      EXTERNAL  SETVEC
C
C     + + + END SPECIFICATIONS + + +
C
      I3=3
      I6=6
C
      CALL SETVEC (I3,0.0,
     O             PGCF1(1,LEV))
C
      CALL SETVEC (I6,0.0,
     O             PGCF2(1,LEV))
C
      RETURN
      END
C
C     4.2(1).14.4
C
      SUBROUTINE   PWGSPB
C
C     + + + PURPOSE + + +
C     Handle section PWTGAS.
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PWTGAS2 + + +
      INCLUDE 'cplps.inc'
      INCLUDE 'cmpad.inc'
C
C     + + + END SPECIFICATIONS + + +
C
C     handle section pwtgas
      IF (SOHTFP.GE.1) THEN
        PAD(SOHTFP+IVL1)= SOHT
      END IF
C
      IF (IOHTFP.GE.1) THEN
        PAD(IOHTFP+IVL1)= IOHT
      END IF
C
      IF (AOHTFP.GE.1) THEN
        PAD(AOHTFP+IVL1)= AOHT
      END IF
C
      IF (POHTFP.GE.1) THEN
        PAD(POHTFP+IVL1)= POHT
      END IF
C
      IF (SODOMX.GE.1) THEN
        PAD(SODOMX+IVL1)= SODOXM
      END IF
C
      IF (SOCDMX.GE.1) THEN
        PAD(SOCDMX+IVL1)= SOCO2M
      END IF
C
      IF (IODOMX.GE.1) THEN
        PAD(IODOMX+IVL1)= IODOXM
      END IF
C
      IF (IOCDMX.GE.1) THEN
        PAD(IOCDMX+IVL1)= IOCO2M
      END IF
C
      IF (AODOMX.GE.1) THEN
        PAD(AODOMX+IVL1)= AODOXM
      END IF
C
      IF (AOCDMX.GE.1) THEN
        PAD(AOCDMX+IVL1)= AOCO2M
      END IF
C
      IF (PODOMX.GE.1) THEN
        PAD(PODOMX+IVL1)= PODOXM
      END IF
C
      IF (POCDMX.GE.1) THEN
        PAD(POCDMX+IVL1)= POCO2M
      END IF
C
      RETURN
      END
C
C     4.2(1).13.6
C
      SUBROUTINE   PWGSPT
C
C     + + + PURPOSE + + +
C     Handle section PWTGAS.
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PWTGAS2 + + +
      INCLUDE 'cplps.inc'
      INCLUDE 'cmpad.inc'
C
C     + + + END SPECIFICATIONS + + +
C
C     handle section pwtgas
      IF (SOTFP.GE.1) THEN
        PAD(SOTFP +IVL1) = SOTMP
      END IF
C
      IF (IOTFP.GE.1) THEN
        PAD(IOTFP +IVL1) = IOTMP
      END IF
C
      IF (AOTFP.GE.1) THEN
        PAD(AOTFP +IVL1) = AOTMP
      END IF
C
      IF (SODOFP.GE.1) THEN
        PAD(SODOFP+IVL1)= SODOX
      END IF
C
      IF (SOCDFP.GE.1) THEN
        PAD(SOCDFP+IVL1)= SOCO2
      END IF
C
      IF (IODOFP.GE.1) THEN
        PAD(IODOFP+IVL1)= IODOX
      END IF
C
      IF (IOCDFP.GE.1) THEN
        PAD(IOCDFP+IVL1)= IOCO2
      END IF
C
      IF (AODOFP.GE.1) THEN
        PAD(AODOFP+IVL1)= AODOX
      END IF
C
      IF (AOCDFP.GE.1) THEN
        PAD(AOCDFP+IVL1)= AOCO2
      END IF
C
      RETURN
      END
