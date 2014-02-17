C
C
C
      SUBROUTINE   PIWTGS
     I                    (OUTLEV)
C
C     + + + PURPOSE + + +
C     Process input for section iwtgas
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   OUTLEV
C
C     + + + ARGUMENT DEFINITIONS + + +
C     OUTLEV - run interp output level
C
C      + + + COMMON BLOCKS- SCRTCH, VERSION IWTGAS1 + + +
      INCLUDE    'cilig.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     TBNO,TBSB,NVAL,IVAL(2)
      REAL        ELEV,RVAL(3)
C
C     + + + EXTERNALS + + +
      EXTERNAL  ITABLE,RTABLE
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' PROCESSING INPUT FOR SECTION IWTGAS')
 2010 FORMAT (/,' FINISHED PROCESSING INPUT FOR SECTION IWTGAS')
C
C     + + + END SPECIFICATIONS + + +
C
      IF (OUTLEV.GT.1) THEN
C       processing message
        WRITE (MESSU,2000)
      END IF
C
C     process values in table-type iwt-parm1
      TBNO= 25
      TBSB= 1
      NVAL= 2
      CALL ITABLE (TBNO,TBSB,NVAL,UUNITS,
     M             IVAL)
      WTFVFG= IVAL(1)
C
      IF (IWATFG.EQ.0) THEN
        CSNOFG= IVAL(2)
      END IF
C
C     process values in table-type iwt-parm2
      TBNO= 26
      TBSB= 1
      NVAL= 3
      CALL RTABLE (TBNO,TBSB,NVAL,UUNITS,
     M             RVAL)
      ELEV= RVAL(1)
C
C     compute factor for correcting dissolved gas
C     saturated concentrations for altitude
      ELEVGC= ((288.0- 0.00198*ELEV)/288.0)**5.256
C
C     dissolved gas concentrations in interflow and groundwater
      AWTF= RVAL(2)
      BWTF= RVAL(3)
C
C     process values in table-type lat-factor
      TBNO= 27
      TBSB= 1
      NVAL= 2
      CALL RTABLE (TBNO,TBSB,NVAL,UUNITS,
     M             LIFAC)
C
      IF (WTFVFG.EQ.1) THEN
C       check for monthly values of awtf - table-type mon-awtf
        TBNO= 28
        TBSB= 1
        NVAL= 12
        CALL RTABLE (TBNO,TBSB,NVAL,UUNITS,
     M               AWTFM)
      END IF
C
      IF (WTFVFG.EQ.1) THEN
C       check for monthly values of bwtf -
C       table-type mon-bwtf
        TBNO= 29
        TBSB= 1
        NVAL= 12
        CALL RTABLE (TBNO,TBSB,NVAL,UUNITS,
     M               BWTFM)
      END IF
C
C     initial values - table-type iwt-init
      TBNO= 30
      TBSB= 1
      NVAL= 3
      CALL RTABLE (TBNO,TBSB,NVAL,UUNITS,
     M             IGST1)
C
      IF (OUTLEV.GT.1) THEN
C       end processing message
        WRITE (MESSU,2010)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   IWTGAS
C
C     + + + PURPOSE + + +
C     Estimate water temperature, dissolved oxygen, and carbon
C     dioxide in the outflows from a impervious land
C     segment. calculate associated fluxes through exit gate
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION IWTGAS2 + + +
      INCLUDE 'cilig.inc'
      INCLUDE 'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + + +
      INTEGER     REQFG,TSSUB(2),FLGVAL
      REAL        ABSTMP,DUMMY
      CHARACTER*6 OPTYP,TSNAM,SECNAM,MSECNM,OPFGNM
C
C     + + + FUNCTIONS + + + +
      REAL      DAYVAL
C
C     + + + EXTERNALS + + +
      EXTERNAL  DAYVAL,HREQTS
C
C     + + + DATA INITIALIZATIONS + + +
      DATA TSSUB/1,1/
      DATA OPTYP,SECNAM/'IMPLND','IWTGAS'/
C
C     + + + END SPECIFICATIONS + + +
C
C     get hydrological time series
      IF (IWATFG.EQ.0) THEN
C       get time series from inpad
CTHJ        SURO= PAD(SOFP+IVL1)
        REQFG= 3
        MSECNM= 'IWATER'
        TSNAM= 'SURO  '
        CALL HREQTS (SOFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,LSNO,
     I               TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,FLGVAL,
     O               SURO)
        IF (SLIFP .GE. 1) THEN
C         surface lateral inflow present
          SURLI= PAD(SLIFP+IVL1)
        ELSE
C         no surface lateral inflow
          SURLI= 0.0
        END IF
C     ELSE
C       suro, and optionally surli, are available from iwater
      END IF
C
      IF (AIRTFG.EQ.0) THEN
C       get air temperature in deg f from the inpad
CTHJ        AIRTMP= PAD(AIRTFP+IVL1)
        REQFG= 2
        TSNAM= 'AIRTMP'
        CALL HREQTS (AIRTFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,LSNO,
     I               TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,FLGVAL,
     O               AIRTMP)
      ELSE
C       air temperatures, in degrees f, are available from section atemp
      END IF
C
C     convert to centigrade
      AIRTC= (AIRTMP-32.0)*0.555
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
C     obtain latest values for temperature calculation parameters
      IF (DAYFG.EQ.1) THEN
C       it is the first interval of the day
        IF (WTFVFG.EQ.1) THEN
C         water temperature regression parameters are allowed to
C         vary throughout the year
C         interpolate for the daily values
C         linearly interpolate awtf between two values from the
C         monthly array awtfm(12)
          AWTF= DAYVAL(AWTFM(MON),AWTFM(NXTMON),DAY,NDAYS)
C         linearly interpolate bwtf between two values from the
C         monthly array bwtfm(12)
          BWTF= DAYVAL(BWTFM(MON),BWTFM(NXTMON),DAY,NDAYS)
        ELSE
C         water temperature regression parameters do not vary
C         throughout the year. values for awtf and bwtf have been
C         supplied by the run interpreter
        END IF
      END IF
C
      IF (SURO .GT. 0.0) THEN
C       there is surface outflow
C
C       calculate impervious surface outflow temperature - in deg. c
        SOTMP= AWTF+ BWTF*AIRTC
C
        IF (SOTMP .LT. 0.5) THEN
C         don't let water temp drop below 0.5 deg c
          SOTMP= 0.5
        END IF
C
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
C       calculate dissolved oxygen and carbon dioxide concentrations
C       in impervious surface runoff - units are mg/l of do and
C       and mg c/l of co2
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
C     compute outflow of heat energy in water - units are deg. c-in./ivl
      SOHT= SOTMP*SURO
C
C     calculate outflow mass of dox - units are mg-in./l-ivl
      SODOXM= SODOX*SURO
C
C     calculate outflow mass of co2 - units are mg-in./l-ivl
      SOCO2M= SOCO2*SURO
C
      RETURN
      END
C
C
C
      SUBROUTINE   IWGACC
     I                    (FRMROW,TOROW)
C
C     + + + PURPOSE + + +
C     Accumulate fluxes for section iwtgas
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   FRMROW,TOROW
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FRMROW - row containing incremental flux accumulation
C     TOROW  - flux row to be incremented
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION IWTGAS2 + + +
      INCLUDE  'cilig.inc'
      INCLUDE  'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I3
C
C     + + + EXTERNALS + + +
      EXTERNAL  ACCVEC
C
C     + + + END SPECIFICATIONS + + +
C
      I3=3
      CALL ACCVEC (I3,IGCF1(1,FRMROW),
     M             IGCF1(1,TOROW))
C
      RETURN
      END
C
C
C
      SUBROUTINE   IWGPRT
     I                    (UNITFG,LEV,PRINTU,BINU)
C
C     + + + PURPOSE + + +
C     Convert quantities from internal to external units.
C     Note: local arrays have identical sizes and structures to the
C     corresponding arrays in the osv apart from dropping
C     the dimension lev for fluxes
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   LEV,PRINTU,UNITFG,BINU
C
C     + + + ARGUMENT DEFINITIONS + + +
C     UNITFG - output units   1-english, 2-metric
C     LEV    - current output level (2-pivl,3-day,4-mon,5-ann)
C     PRINTU - fortran unit number on which to print output
C     BINU   - fortran unit number on which to write binary output
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION IWTGAS2 + + +
      INCLUDE  'cilig.inc'
      INCLUDE  'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,J,I0,I1,LATFG,ACNT,CLEN(9),EXDAT(5)
      REAL      EFACTA,MFACTA,PCFLX1,PCFLX2,PCFLX3,PSTAT,TFACTA,TFACTB,
     $          PLITMP,APRINT(9)
      CHARACTER*256 CHEAD(9)
C
C     + + + EXTERNALS + + +
      EXTERNAL   EXDATE
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' *** IWTGAS ***')
 2010 FORMAT (/,'   STATE VARIABLES',13X,'WATER TEMP',11X,
     $        'DISSOLVED:   OXYGEN      CARBON DIOXIDE')
 2020 FORMAT (36X,'SOTMP',25X,'SODOX',15X,'SOCO2')
 2030 FORMAT (36X,'DEG F',26X,'MG/L',11X,'MG OF C/L')
 2040 FORMAT (36X,'DEG C',26X,'MG/L',11X,'MG OF C/L')
 2050 FORMAT (31X,G10.3,10X,2(10X,G10.3) )
 2052 FORMAT (/,'   STATE VARIABLES',13X,'WATER TEMP',11X,
     $        'DISSOLVED:   OXYGEN                CARBON DIOXIDE')
 2053 FORMAT (31X,'    SLITMP     SOTMP',10X,'    SLIDOX     SODOX',
     $        '    SLICO2     SOCO2')
 2054 FORMAT (31X,'     DEG F     DEG F',10X,'      MG/L      MG/L',
     $        ' MG OF C/L MG OF C/L')
 2055 FORMAT (31X,'     DEG C     DEG C',10X,'      MG/L      MG/L',
     $        ' MG OF C/L MG OF C/L')
 2056 FORMAT (31X,2G10.3,10X,4G10.3)
 2060 FORMAT ('   FLUXES',28X,'HEAT           DISSOLVED:   OXYGEN',
     $        6X,'CARBON DIOXIDE')
 2070 FORMAT ('     OUTFLOWS IN WATER',15X,'SOHT',24X,'SODOXM',14X,
     $        'SOCO2M')
 2080 FORMAT (34X,' BTU/AC',10X,2(15X,'LB/AC') )
 2090 FORMAT (34X,'KCAL/HA',10X,2(15X,'KG/HA') )
 2100 FORMAT (31X,G10.3,10X,2(10X,G10.3) )
C
C     + + + END SPECIFICATIONS + + +
C
C     + + + END SPECIFICATIONS + + +
C
      I0= 0
      I1= 1
C
C     initialize array counter for binary printout
      ACNT = 0
C
C     assign values to parameters used for conversion from internal
C     to external units
C
      IF (UNITFG.EQ.1) THEN
C       english system
C       parameters for variables with energy units
        EFACTA= 407960.
C
C       parameters for variables with temperature units
        TFACTA= 1.8
        TFACTB= 32.0
C
C       parameters for variables for dissolved gases with mass units
        MFACTA= 0.2266
      ELSE
C       metric system
C       parameters for variables with energy units
        EFACTA= 253900.
C
C       parameters for variables with temperature units
        TFACTA= 1.0
        TFACTB= 0.0
C
C       parameters for variables for dissolved gases with mass units
        MFACTA= 0.2540
      END IF
C
      LATFG= 0
      IF ( (SLITFP .GE. 1) .OR. (SLDOFP .GE. 1) .OR.
     $     (SLCOFP .GE. 1) ) THEN
C       some lateral inflow defined
        LATFG= 1
      END IF
C
      IF (SLITMP .GT. -1.0E+29) THEN
C       convert to printout units
        PLITMP = SLITMP*TFACTA+ TFACTB
      END IF
      IF (SOTMP .GT. -1.0E+29) THEN
C       convert to printout units
        PSTAT = SOTMP*TFACTA+ TFACTB
      END IF
C
C     state variables with concentration units do not
C     have to be converted
C
C     fluxes - energy units
      PCFLX1= IGCF1(1,LEV)*EFACTA
C     fluxes - mass units
      PCFLX2= IGCF1(2,LEV)*MFACTA
      PCFLX3= IGCF1(3,LEV)*MFACTA
C
      IF (PRINTU .GT. 0 .AND. PFLAG(5) .LE. LEV) THEN
        WRITE (PRINTU,2000)
      END IF
C
      IF (LATFG .EQ. 0) THEN
C       no lateral inflow state variables
        IF (PRINTU .GT. 0 .AND. PFLAG(5) .LE. LEV) THEN
          WRITE (PRINTU,2010)
          WRITE (PRINTU,2020)
C
          IF (UNITFG.EQ.1) THEN
C           english
            WRITE (PRINTU,2030)
          ELSE
C           metric
            WRITE (PRINTU,2040)
          END IF
C
          WRITE (PRINTU,2050)  PSTAT, SODOX, SOCO2
        END IF
        IF (BINU .GT. 0 .AND. ABS(BFLAG(5)) .LE. LEV) THEN
C         compile values for direct access printout
          ACNT = ACNT + 1
          APRINT(ACNT) = PSTAT
          CHEAD(ACNT) = 'SOTMP'
          CLEN(ACNT) = 5
          ACNT = ACNT + 1
          APRINT(ACNT) = SODOX
          CHEAD(ACNT) = 'SODOX'
          CLEN(ACNT) = 5
          ACNT = ACNT + 1
          APRINT(ACNT) = SOCO2
          CHEAD(ACNT) = 'SOCO2'
          CLEN(ACNT) = 5
        END IF
      ELSE
C       no lateral inflow state variables
        IF (PRINTU .GT. 0 .AND. PFLAG(5) .LE. LEV) THEN
          WRITE (PRINTU,2052)
          WRITE (PRINTU,2053)
C
          IF (UNITFG.EQ.1) THEN
C           english
            WRITE (PRINTU,2054)
          ELSE
C           metric
            WRITE (PRINTU,2055)
          END IF
C
          WRITE (PRINTU,2056)  PLITMP,PSTAT,SLIDOX,SODOX,SLICO2,SOCO2
        END IF
        IF (BINU .GT. 0 .AND. ABS(BFLAG(5)) .LE. LEV) THEN
          ACNT = ACNT + 1
          APRINT(ACNT) = PLITMP
          CHEAD(ACNT) = 'SLITMP'
          CLEN(ACNT) = 6
          ACNT = ACNT + 1
          APRINT(ACNT) = PSTAT
          CHEAD(ACNT) = 'SOTMP'
          CLEN(ACNT) = 5
          ACNT = ACNT + 1
          APRINT(ACNT) = SLIDOX
          CHEAD(ACNT) = 'SLIDOX'
          CLEN(ACNT) = 6
          ACNT = ACNT + 1
          APRINT(ACNT) = SODOX
          CHEAD(ACNT) = 'SODOX'
          CLEN(ACNT) = 5
          ACNT = ACNT + 1
          APRINT(ACNT) = SLICO2
          CHEAD(ACNT) = 'SLICO2'
          CLEN(ACNT) = 6
          ACNT = ACNT + 1
          APRINT(ACNT) = SOCO2
          CHEAD(ACNT) = 'SOCO2'
          CLEN(ACNT) = 5
        END IF
      END IF
C
      IF (PRINTU .GT. 0 .AND. PFLAG(5) .LE. LEV) THEN
        WRITE (PRINTU,2060)
C
C       outflows
        WRITE (PRINTU,2070)
C
        IF (UNITFG.EQ.1) THEN
C         english
          WRITE (PRINTU,2080)
        ELSE
C         metric
          WRITE (PRINTU,2090)
        END IF
C
        WRITE (PRINTU,2100)  PCFLX1, PCFLX2, PCFLX3
      END IF
      IF (BINU .GT. 0 .AND. ABS(BFLAG(5)) .LE. LEV) THEN
        ACNT = ACNT + 1
        APRINT(ACNT) = PCFLX1
        CHEAD(ACNT) = 'SOHT'
        CLEN(ACNT) = 4
        ACNT = ACNT + 1
        APRINT(ACNT) = PCFLX2
        CHEAD(ACNT) = 'SODOXM'
        CLEN(ACNT) = 6
        ACNT = ACNT + 1
        APRINT(ACNT) = PCFLX3
        CHEAD(ACNT) = 'SOCO2M'
        CLEN(ACNT) = 6
      END IF
C
      IF (BINU .GT. 0 .AND. ABS(BFLAG(5)) .LE. LEV) THEN
C       write binary output
        CALL EXDATE(
     I              DATIM,
     O              EXDAT)
        IF (BFLAG(5) .GT. 0) THEN
C         at start of run, write the header
          WRITE (BINU) I0,'IMPLND  ',LSNO,'IWTGAS  ',
     1          (CLEN(I),(CHEAD(I)(J:J),J=1,CLEN(I)),I=1,ACNT)
C         set bflag to negative to not write headers anymore
          BFLAG(5) = -BFLAG(5)
        END IF
        WRITE (BINU) I1,'IMPLND  ',LSNO,'IWTGAS  ',UNITFG,
     1               LEV,(EXDAT(I),I=1,5),(APRINT(I),I=1,ACNT)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   IWGRST
     I                    (LEV)
C
C     + + + PURPOSE + + +
C     Reset all flux accumulators and those state variables
C     used in material balance check for section iwtgas
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   LEV
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEV    - current output level (2-pivl,3-day,4-mon,5-ann)
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION IWTGAS2 + + +
      INCLUDE  'cilig.inc'
      INCLUDE  'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I3
C
C     + + + EXTERNALS + + +
      EXTERNAL  SETVEC
C
C     + + + END SPECIFICATIONS + + +
C
      I3= 3
      CALL SETVEC (I3,0.0,
     O             IGCF1(1,LEV))
C
      RETURN
      END
C
C
C
      SUBROUTINE   IWGSIB
C
C     + + + PURPOSE + + +
C     Handle section iwtgas
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION IWTGAS2 + + +
      INCLUDE 'cilig.inc'
      INCLUDE 'cmpad.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      IF (SOHTFP.GE.1) THEN
        PAD(SOHTFP+IVL1)= SOHT
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
      RETURN
      END
C
C
C
      SUBROUTINE   IWGSIP
C
C     + + + PURPOSE + + +
C     Handle section iwtgas
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION IWTGAS2 + + +
      INCLUDE 'cilig.inc'
      INCLUDE 'cmpad.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      IF (SOTFP.GE.1) THEN
        PAD(SOTFP +IVL1)= SOTMP
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
      RETURN
      END
