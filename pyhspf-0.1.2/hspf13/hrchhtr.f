C
C
C
      SUBROUTINE   PHTRCH
     M                    (OSVREC)
C
C     + + + PURPOSE + + +
C     Process input for section htrch of the rchres application
C     module
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     OSVREC
C
C     + + + ARGUMENT DEFINITIONS + + +
C     OSVREC - ???
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION HTRCH1 + + +
      INCLUDE    'crhht.inc'
      INCLUDE    'crin2.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I1,IX,IY,J,SCLU,SGRP,INITFG,CONT,
     $             CLEN,IVAL(3)
      REAL         RVAL(7)
      CHARACTER*80 CHSTR
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (CHSTR,CHSTR1)
      CHARACTER*1  CHSTR1(80)
C
C     + + + EXTERNALS + + +
      EXTERNAL   ITABLE,RTABLE,WMSGTT
C
C     + + + INTRINSICS + + +
      INTRINSIC  MAX
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (8F10.0)
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' PROCESSING INPUT FOR SECTION HTRCH')
 2010 FORMAT (/,' FINISHED PROCESSING INPUT FOR SECTION HTRCH')
C
C     + + + END SPECIFICATIONS + + +
C
      IF (OUTLEV.GT.1) THEN
        WRITE (MESSU,2000)
      END IF
C
      I1 = 1
C
      HTWCNT= 0
C
C     read in dry air lapse rate - deg c/ft
      SCLU  = 344
      SGRP  = 1
      INITFG= 1
      CLEN  = 80
      CALL WMSGTT (MSGFL,SCLU,SGRP,INITFG,
     M             CLEN,
     O             CHSTR1,CONT)
      READ (CHSTR,1000) (LAPSE(J),J=1,8)
C
      INITFG= 0
      CLEN  = 80
      CALL WMSGTT (MSGFL,SCLU,SGRP,INITFG,
     M             CLEN,
     O             CHSTR1,CONT)
      READ (CHSTR,1000) (LAPSE(J),J=9,16)
C
      CLEN  = 80
      CALL WMSGTT (MSGFL,SCLU,SGRP,INITFG,
     M             CLEN,
     O             CHSTR1,CONT)
      READ (CHSTR,1000) (LAPSE(J),J=17,24)
C
C     bed conduction flags - table-type heat-bed-flags
      IX= 21
      IY= 3
      CALL ITABLE
     I             (IX,I1,IY,UUNITS,
     M              IVAL)
      BEDFLG= IVAL(1)
      TGFLG = IVAL(2)
      TSTOP = IVAL(3)
C
C     other heat parameters - table-type heat-parm
      IX= 22
      IY= 6
      CALL RTABLE
     I             (IX,I1,IY,UUNITS,
     M              HTPM)
C
C     bed heat conduction tables
      IF (BEDFLG .GT. 0) THEN
        IF (BEDFLG .NE. 3) THEN
C         heat bed conduction parameters - table-type heat-bed-parm
          IX= 23
          IY= 4
          CALL RTABLE
     I               (IX,I1,IY,UUNITS,
     M                RVAL)
          MUDDEP = RVAL(1)
          TGRND  = RVAL(2)
          KMUD   = RVAL(3)
          KGRND  = RVAL(4)
C
C         convert rate coefficients from kcal/m2/C/hr to kcal/m2/C/ivl
          KMUD= KMUD*DELT60
          KGRND= KGRND*DELT60
        END IF
C
        IF ((BEDFLG .NE. 3) .AND. (TGFLG .EQ. 3)) THEN
C         monthly values of ground temp - table-type mon-heat-tgrnd
          IX= 24
          IY= 12
          CALL RTABLE
     I                 (IX,I1,IY,UUNITS,
     M                  TGRNDM)
        END IF
C
        IF (BEDFLG .EQ. 3) THEN
C         sediment-water heat fluxes - table-type heat-bed-delh
          IX= 25
          IY= TSTOP
          CALL RTABLE
     I                 (IX,I1,IY,UUNITS,
     M                  DELH)
C
C         temp changes in last tstop ivls - table-type heat-bed-deltt
          IX= 26
          IY= TSTOP
          CALL RTABLE
     I                 (IX,I1,IY,UUNITS,
     M                  DELTT)
        END IF
      END IF
C
C     shade parameters - table-type shade-parm
      IX= 35
      IY= 7
      CALL RTABLE
     I             (IX,I1,IY,UUNITS,
     M              RVAL)
C
      SHADFG= RVAL(1)
      IF (SHADFG .EQ. 1) THEN      
C       process rest of shade inputs
        CALL PSHADE
     I             (RVAL)
      END IF
C      
C     calculate the pressure correction factor for conductive-
C     convective heat transport
      CFPRES= ((288.0- 0.001981*ELEV)/288.0)**5.256
C
C     initial temperatures - table-type heat-init
      IX= 27
      IY= 2
      CALL RTABLE
     I             (IX,I1,IY,UUNITS,
     M              HTST2)
C
C     compute initial value of heat storage
      RHEAT= TW*VOL
C
C     compute initial tmud and tmuddt for brock/caupp model
C     assume tmud= tw and tmuddt is small + negative (at midnight)
      IF (BEDFLG .EQ. 2) THEN      
        TMUD= TW
        TMUDDT= -0.1
      END IF
C
      IF (SHADFG .EQ. 1) THEN
C       need SHADE module OSV
        OSVREC= MAX (OSVREC,98)
      ELSE
C       only need HTRCH OSV
        OSVREC = MAX (OSVREC,23)
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
      SUBROUTINE   HTRCH
C
C     + + + PURPOSE + + +
C     Simulate heat exchange and water temperature
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION HTRCH2 + + +
      INCLUDE 'crhht.inc'
      INCLUDE 'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,REQFG,TSSUB(2),FLGVAL
      REAL        AVDEPE,CLDFAC,CVQT,DELTTW,EVAP,MPREC,SPD,TAKELV,TMP,
     $            TWKELV,VPRESA,VPRESW,TWS,OTW,SRAD
      DOUBLE PRECISION   DTW
      CHARACTER*6 OPTYP,TSNAM,SECNAM,MSECNM,OPFGNM
C
C     + + + FUNCTIONS + + +
      REAL     VAPOR,DAYVAL
C
C     + + + EXTERNALS + + +
      EXTERNAL ADVECT,RATEMP,DAYVAL,BEDHT2,HREQTS
C
C     + + + INTRINSICS + + +
      INTRINSIC  ABS
C
C     + + + DATA INITIALIZATIONS + + +
      DATA TSSUB/1,1/
      DATA OPTYP,SECNAM,MSECNM/'RCHRES','HTRCH ','HYDR  '/
C
C     + + + END SPECIFICATIONS + + +
C
C     define vapor function based on temperature (deg c); vapor
C     pressure is expressed in millibars
      VAPOR(TMP)= 33.8639*((.00738*TMP+.8072)**8 -.000019
     $            *ABS(1.8*TMP+48.)+.001316)
C
C     get input time series; inflow of heat to rch/res is
C     expressed in kcal.vol/l.ivl; heat is relative to 0 degreees c
      IF (IHTFP .GT. 0) THEN
        IHEAT= PAD(IHTFP + IVL1)
      ELSE
        IHEAT= 0.0
      END IF
C
C     volume of water in rch/res is obtained from inpad if module
C     section hydr is inactive for this operation; avdepe is the
C     average depth in english units
      IF (HYDRFG .EQ. 0) THEN
CTHJ        AVDEP= PAD(AVDFP + IVL1)
        REQFG= 3
        MSECNM= 'HYDR  '
        TSNAM= 'AVDEP '
        CALL HREQTS (AVDFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,RCHNO,
     I               TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,FLGVAL,
     O               AVDEP)
      END IF
C
C     convert average depth to english units and surface area to
C     metric units, if necesary, for htrch calculations
      IF (UUNITS .EQ. 2) THEN
        AVDEPE= AVDEP*3.28
      ELSE
        AVDEPE= AVDEP
      END IF
C
C     adcalc is always active if constituents are being simulated,
C     so time series obtained from it do not have to be read from
C     inpad
C
C     perform advection; watertemp tw is treated as a concentration
C     also save TW (TWS) for later use in the bed conduction models
      DTW= TW
      TWS= TW
      CALL ADVECT
     I            (IHEAT,VOLS,SROVOL,VOL,EROVOL,SOVOL,
     I             EOVOL,NEXITS,
     M             DTW,
     O             ROHEAT,OHEAT)
      TW= DTW
C
C     simulate heat exchange with the atmosphere
C      
C     calculate solar radiation absorbed(qsolar); solrad,
C     which is expressed in langleys/ivl, is the solar
C     radiation at gage corrected for location of reach;
C
CTHJ      SOLRAD= PAD(SOLFP + IVL1)
      REQFG= 2
      TSNAM= 'SOLRAD'
      CALL HREQTS (SOLFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,RCHNO,
     I             TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,FLGVAL,
     O             SOLRAD)
C
      IF (SHADFG .EQ. 1) THEN
C       perform shading calculations
C
        IF (DAYFG .EQ. 1) THEN
C         get total daily input radiation
CTHJ          DSOL= PAD(DSOLFP+ IVL1)
          REQFG= 4
          TSNAM= 'DSOLAR'
          OPFGNM= 'SHADFG'
          FLGVAL= 1
          CALL HREQTS (DSOLFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,RCHNO,
     I                 TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,FLGVAL,
     O                 DSOLAR)
        END IF
C
C       use shade module to compute solar radiation absorbed by stream
        CALL SHADEH
     O              (SRAD)
C       10.0 is the conversion from ly/ivl to kcal/m2.ivl.
        QSOLAR= SRAD*10.
      ELSE
C       use constant shading factor
C       0.97 accounts for surface reflection (assumed 3 percent);
C       cfsaex is the ratio of radiation incident to water surface
C       to gage radiation values (accounts for regional
C       differences, shading of water surface,etc);
C       10.0 is the conversion from ly/ivl to kcal/m2.ivl.
        QSOLAR= 0.97*CFSAEX*SOLRAD*10.0
      END IF
C
C     calculate heat transfer rates for water surface; units are
C     kcal/m2.ivl
C
C     get quantity of precipitation and convert ft/ivl to m/ivl,
C     if necessary
      IF (PRECFP .GT. 0) THEN
        PREC= PAD(PRECFP + IVL1)
        IF (PREC .GT. 0.0) THEN
          IF (IUNITS .EQ. 2) THEN
            MPREC= PREC
          ELSE
            MPREC= PREC/3.2808
          END IF
C
C         calculate heat added by precip, assuming temperature is
C         equal to reach/res water temperature
          QPREC= MPREC*TW*1000.0
        ELSE
          QPREC= 0.0
          MPREC= 0.0
        END IF
C
      ELSE
C       precipitation not considered
        QPREC= 0.0
        MPREC= 0.0
      END IF
C
C     calculate cloud cover factor for determination of
C     atmospheric longwave radiation
CTHJ      CLOUD = PAD(CCFP+IVL1)
      REQFG= 2
      TSNAM= 'CLOUD '
      CALL HREQTS (CCFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,RCHNO,
     I             TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,FLGVAL,
     O             CLOUD)
      CLDFAC= 1. + (0.0017*(CLOUD**2))
C
C     get gage air temperature
CTHJ      GATMP = PAD(GATFP+IVL1)
      REQFG= 2
      TSNAM= 'GATMP '
      CALL HREQTS (GATFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,RCHNO,
     I             TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,FLGVAL,
     O             GATMP)
C
C     get gage dewpoint temperature
CTHJ      DEWTMP= PAD(DEWFP+IVL1)
      REQFG= 2
      TSNAM= 'DEWTMP'
      CALL HREQTS (DEWFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,RCHNO,
     I             TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,FLGVAL,
     O             DEWTMP)
C
C     get wind movement expressed in m/ivl
CBRB  WIND  = PAD(WDFP+IVL1)
      REQFG= 2
      TSNAM= 'WIND  '
      CALL HREQTS (WDFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,RCHNO,
     I             TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,FLGVAL,
     O             WIND)
C
C     correct air temperature for elevation differences
      CALL RATEMP
     I            (MPREC,DELT,LAPSE,GATMP,ELDAT,HR,
     O             AIRTMP)
C
      IF (AVDEPE .GT. 0.17) THEN
C
C       convert water temperature and air temperature to degrees
C       kelvin for determination of atmospheric longwave radiation
        TWKELV= TW + 273.16
        TAKELV= AIRTMP + 273.16
C
C       calculate net flux of longwave radiation; qlongw is expressed
C       in kcal/m2.ivl; 4.73e-8 is the stephan-boltzmann constant
C       multiplied by .97 to account for emissivity of water; katrad
C       is the atmospheric longwave radiation coefficient
C       (changed sign of qlongw to make it consistent with other         
C       fluxes; ie, positive = gain of heat by reach; brb 6/95)
        QLONGW= 4.73E-8*((TWKELV**4)-KATRAD*1.0E-6*CLDFAC*(TAKELV**6))
     $      *DELT60*(-1.0)
C
C       calculate conductive-convective heat transport; qcon is
C       expressed in kcal/m2.ivl; kcond is the heat transport
C       coefficient for conduction-convection
C       (changed sign of qcon to make it consistent with other         
C       fluxes; ie, positive = gain of heat by reach; brb 6/95)
        QCON= CFPRES*KCOND*(1.0E-4)*WIND*(AIRTMP - TW)
C
C       determine vapor pressure of air above water surface; vpresa
C       is expressed in millibars
        VPRESA= VAPOR(DEWTMP)
C
C       determine saturation vapor pressure at the water surface;
C       vpresw is expressed in millibars
        VPRESW= VAPOR(TW)
C
C       calculate quantity of water evaporated during interval; evap
C       is expressed in meters/ivl; kevap is the evaporation
C       coefficient
        EVAP= KEVAP*(1.0E-9)*WIND*(VPRESW - VPRESA)
C
C       calculate heat loss due to evaporation; qevap is expressed in
C       kcal/m2.ivl; (597300. - 570.*tw)= latent heat of vaporization
C       (597.3 - .57*tw) multiplied by the density of water
C       (1000 kg/m3)
C       (changed sign of qevap to make it consistent with other         
C       fluxes; ie, positive = gain of heat by reach; brb 6/95)
        QEVAP= (597300. - 570.*TW)*EVAP*(-1.0)
C
C       bed conduction         
C       first get ground temperature if method 1 or 2
        IF (BEDFLG .EQ. 1 .OR. BEDFLG .EQ. 2) THEN
          IF (TGFLG .EQ. 1) THEN
C           user-defined timeseries of ground temperature
CTHJ            TGRND= PAD(TGRNDX + IVL1)
            REQFG= 4
            TSNAM= 'TGRND '
            OPFGNM= 'BEDFLG'
            CALL HREQTS (TGRNDX,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,
     I                   RCHNO,TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,BEDFLG,
     O                   TGRND)
          ELSE IF (TGFLG .EQ. 2) THEN
C           single used-defined value
          ELSE IF (TGFLG .EQ. 3) THEN
C           monthly values supplied by user            
            IF (DAYFG .EQ. 1) THEN
              TGRND= DAYVAL(TGRNDM(MON),TGRNDM(NXTMON),DAY,NDAYS)
            END IF
          END IF              
        END IF        
C
C       compute conduction heat flux
        IF (BEDFLG .EQ. 1) THEN
C         one-layer bed conduction model
          QBED= KMUD*(TGRND - TW)
        ELSE IF (BEDFLG .EQ. 2) THEN
C         two-layer bed conduction model - brock/caupp
          CALL BEDHT2
     I                (TW,KMUD,KGRND,MUDDEP,TGRND,
     M                 TMUD,TMUDDT,
     O                 QBED)
        ELSE IF (BEDFLG .EQ. 3) THEN
C         Jobson's bed conduction model; set qbed to 0 initially
C         in order to compute preliminary deltt which will be
C         used later for computing qbed
          QBED= 0.0
        ELSE
C         no bed conductance
          QBED= 0.0
        END IF
C
C       calculate total heat exchange at water surface; qtotal is
C       expressed in kcal/m2.ivl
        QTOTAL= QSOLAR + QLONGW + QCON + QEVAP + QPREC + QBED
C
        IF (ABS(QTOTAL) .GT. 1.0) THEN
C         if net heat flux > 1 kcal/m2.ivl, calculate new water
C         temperature
C
C         solution technique requires sum of partial derivatives of
C         qlongw, qcon, and qevap with respect to water temperature;
C         this value, spd, is derived by a series of three sets
C         of operations; the actual value of spd is not derived
C         until the last operation
C
          SPD= 18.92E-8*(TWKELV**3)*DELT60+ CFPRES*KCOND*1.0E-4*WIND
          SPD= SPD+ KEVAP*1.0E-9*WIND*588750.*(.4436+TW*(28.63195E-3+TW*
     $         (.8E-3+TW*(.01124E-3+TW*.00013E-3))))
          SPD= .5*SPD
C
C         calculate conversion factor to convert total heat exchange
C         expressed in kcal/m2.ivl to degrees c/ivl for the volume
C         of water in the reach;
C         3.281e-3= (1000 cal/kcal)*(1 m3/10e6 cm3)*(3.281 ft/m)
          CVQT    = (3.281E-3)/AVDEPE
C
C         calculate change in water temperature
C         (if Jobsons bed conduction method is being used, 
C         this is a preliminary calculation of TW and DELTTW)
          DELTTW= CVQT*QTOTAL/(1.0 + SPD*CVQT)
          OTW   = TW
          TW    = TW + DELTTW
          IF (TW .LT. 0.04) THEN
            DELTTW= DELTTW + 0.04 - TW
            TW    = 0.04
          END IF
C        
          IF (BEDFLG .EQ. 3) THEN
C           Jobson's bed conductance model
            IF (TWS .LT. -1.0E10) THEN
              DELTT(1)= 0.0
            ELSE
              DELTT(1)= TW - TWS
            END IF
            DO 10 I= 1, TSTOP
              QBED= QBED + DELH(I)*DELTT(I)
 10         CONTINUE       
            QTOTAL= QTOTAL + QBED
C           recalculate change in water temperature
            DELTTW= CVQT*QTOTAL/(1.0 + SPD*CVQT)
            TW    = OTW + DELTTW
            IF (TW .LT. 0.04) THEN
              DELTTW= DELTTW + 0.04 - TW
              TW    = 0.04
            END IF
          END IF
C          
          HTEXCH= DELTTW*VOL
        ELSE
C         water temperature remains unchanged
          HTEXCH= 0.0
        END IF
C
      ELSE
C       there is too little water in reach to simulate heat exchange
C       with atmosphere; set water temp to air temp
        DELTTW= AIRTMP - TW
        TW    = AIRTMP
        IF (TW .LT. 0.04) THEN
          DELTTW= DELTTW + 0.04 - TW
          TW    = 0.04
        END IF
        HTEXCH= DELTTW * VOL
C       set all atmospheric/bed fluxes to 0
        DO 20 I= 1, 7
          HTCF4(I,1)= 0.0
 20     CONTINUE
C
      END IF
C
C     update deltt array for next time step of
C     jobsons bed conductance model
      IF (BEDFLG .EQ. 3) THEN
        IF (TWS .LT. -1.0E10 .OR. TW .LT. -1.0E10) THEN
          DELTT(1)= 0.0
        ELSE
          DELTT(1)= TW - TWS
        END IF
        DO 30 I= TSTOP, 2, -1
          DELTT(I)= DELTT(I-1)
 30     CONTINUE
      END IF
C
C     calculate storage of thermal energy in rchres
      RHEAT= TW*VOL
C
      RETURN
      END
C
C
C
      SUBROUTINE   BEDHT2
     I                    (TW,KMUD,KGRND,MUDDEP,TGRND,
     M                     TMUD,TMUDDT,
     O                     QBED)
C
C     + + + PURPOSE + + +
C     Compute bed conduction heat flux using 2-interface model
C     based on Caupp's and Brock's (1994) model of the Truckee.
C
C     + + + DUMMY VARIABLES + + +
      REAL       TW,KMUD,KGRND,MUDDEP,TGRND,TMUD,TMUDDT,QBED
C
C     + + + ARGUMENT DEFINITIONS + + +
C     TW     - water temperature at current time (C)
C     KMUD   - water-mud heat conductance coefficient (kcal/m2/C/ivl)
C     KGRND  - ground-mud heat conductance coefficient (kcal/m2/C/ivl)
C     MUDDEP - depth of mud (m)
C     TGRND  - ground temperature (C)
C     TMUD   - temperature of mud (C)
C     TMUDDT - slope of mud temperature-time curve (C/ivl)
C     QBED   - bed heat flux (kcal/m2/ivl)
C
C     + + + LOCAL VARIABLES + + +
      REAL       CPR,BFLUX,BTHALF,BEDINS
C
C     + + + END SPECIFICATIONS + + +
C
C     CPR = density * specific heat of water (and mud) 
C     CPR = 1 gm/cm3 * 1 kcal/kg/C * 1000 cm3.kg/m3/g = 1000 kcal/m3/C
C     this model uses CPR for both water and mud, per Caupp
      CPR = 1000.
C
C     compute mud temperature at center of current time step; 
C     tmuddt is slope of mud temperature curve
      TMUD = TMUD + TMUDDT/2.
C
C     compute heat flux between mud and water based on water 
C     temperature in last time step and mud temperature at center
C     of current time step; BFLUX = kcal/m2/ivl;
C     CMUD is the heat conductance coefficient (kcal/m2/C/ivl);
C     it is an input parameter; Caupp uses 0.02 kcal/m2/C/s; 
C     WQRRS = 0.001; "Oldman River CMUD" = 0.014
      BFLUX = (TMUD - TW) * KMUD
C
C     compute a new mud temperature slope using heat flux and
C     heat capacity of mud/water and depth ("thermal capacity") of mud
      TMUDDT = -BFLUX/CPR/MUDDEP
C
C     mud temperature at center of time step 
      BTHALF = TMUD
C
C     compute heat flux between ground and mud based on mud temperature
C     at center of current time step and input ground temperature,
C     which can be estimated by the mean annual air temperature;
C     this flux will be used to compute mud temperature at end of  
C     current time step; the eqn. uses mud depth (m), CPR (kcal/m3/C), 
C     24 hr/day, and streambed thermal gradient (KGRND kcal/C/ivl/m3) 
C     to express heat flux (BEDINS) in units of C/ivl; 
C     depth of water is used in error; should be depth of mud, per Caupp
C     (KGRND=0.1419 cal/C/hr/cm2 apparently assumes ground depth= 1 m ?)
      BEDINS = KGRND*(TGRND - TMUD)/MUDDEP/CPR
C
C     compute the new mud temperature at end of current time step
C     first, account for heat flux between water and mud
C     second, account for heat flux between ground and mud
      TMUD = TMUD + TMUDDT/2. + BEDINS
C
C     compute heat flux between mud and water using mud temperature
C     at center of time step and current water temperature;
C     KMUD is the mud-water heat conductance coefficient (kcal/m2/C/ivl)
      QBED = (BTHALF - TW) * KMUD
C
      RETURN
      END
C
C
C
      SUBROUTINE   HTPRT
     I                   (UNITFG,LEV,PRINTU,BINU)
C
C     + + + PURPOSE + + +
C     Convert quantities from internal to external units, calculate
C     materials balance and print out results
C     Note: local arrays have same dimensions as corresponding arrays
C      in osv, except for dropping of dimension lev, where applicable
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
C     + + + COMMON BLOCKS- SCRTCH, VERSION HTRCH2 + + +
      INCLUDE    'crhht.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I0,I1,I2,I7,I,IX,J,CLEN(13+NEXITS),JLEN,ACNT,EXDAT(5)
      REAL        FFACTA,FFACTB,XFACTA,HTDIF,PATMP,PCFLX2(2),PCFLX3(5),
     $            PCFLX4(7),PDTMP,PIFLX,PRHT,PRHTS,PWTMP,SFACTA,SFACTB,
     $            APRINT(13+NEXITS)
      CHARACTER*8 UNITID,CCFLX2(2),CCFLX4(7)
      CHARACTER*1   CSTR(2)
      CHARACTER*256 CHEAD(13+NEXITS)
C
C     + + + EXTERNALS + + +
      EXTERNAL   TRNVEC,BALCHK,INTCHR,EXDATE
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' *** HTRCH ***')
 2010 FORMAT (/,3X,'STATE VARIABLES',18X,'AIR    DEWPOINT',7X,
     $        'WATER')
 2020 FORMAT (  5X,'TEMPERATURES (DEG F)',8X,'AIRTMP      DEWTMP',
     $        10X,'TW')
 2030 FORMAT (  5X,'TEMPERATURES (DEG C)',8X,'AIRTMP      DEWTMP',
     $        10X,'TW')
 2040 FORMAT ( 29X,3(F10.1,2X))
 2050 FORMAT (/,3X,'FLUXES',25X,'TOTAL       ATMOS       TOTAL',
     $        '    INDIVIDUAL GATE OUTFLOWS (OHEAT)')
 2060 FORMAT (  5X,'HEAT GAINS/LOSSES (BTU)     INFLOW        EXCH',
     $        '     OUTFLOW',5I12)
 2070 FORMAT (  5X,'HEAT GAINS/LOSSES (KCAL)    INFLOW        EXCH',
     $        '     OUTFLOW',5I12)
 2080 FORMAT ( 34X,'IHEAT      HTEXCH      ROHEAT',19X,'OHEAT')
 2090 FORMAT ( 29X,8(1PE10.3,2X))
 2100 FORMAT (/,3X,'FLUXES',25X,'TOTAL       ATMOS       TOTAL')
 2110 FORMAT ( 34X,'IHEAT      HTEXCH      ROHEAT')
 2140 FORMAT (/,3X,'ATMOS/BED HEAT BALANCE COMPONENTS (BTU/FT2)',/,
     $          '     (POSITIVE INDICATES GAIN)')
 2150 FORMAT (/,3X,'ATMOS/BED HEAT BALANCE COMPONENTS (KCAL/M2)',/,
     $          '     (POSITIVE INDICATES GAIN)')
 2160 FORMAT ( 34X,'TOTAL       SOLAR    LONGWAVE            ',
     $                   '    CONVECT/                     BED',/,         
     $         34X,' HEAT   RADIATION   RADIATION      EVAPOR',
     $                  '     CONDUCT      PRECIP     CONDUCT',/,
     $         33X,'QTOTAL      QSOLAR      QLONGW       QEVAP', 
     $                     '        QCON       QPREC        QBED')
 2170 FORMAT (/,5X,'EFFECTIVE SHADE (-)',5X,F10.3)
C
C     + + + END SPECIFICATIONS + + +
C
      I0= 0
      I1= 1
      I2= 2
      I7= 7
C
C     initialize array counter for binary printout, store variable
C     names in local strings for use in building binary headers
      ACNT = 0
      CCFLX2(1) = 'HTEXCH'
      CCFLX2(2) = 'ROHEAT'
      CCFLX4(1) = 'QTOTAL'
      CCFLX4(2) = 'QSOLAR'
      CCFLX4(3) = 'QLONGW'
      CCFLX4(4) = 'QEVAP'
      CCFLX4(5) = 'QCON'
      CCFLX4(6) = 'QPREC'
      CCFLX4(7) = 'QBED'
C
C     assign values to parameters used for conversion from internal
C     to external units
      IF (UNITFG .EQ. 1) THEN
C       printout is in english system
C
C       parameters for state variables with temperature units
        SFACTA= 1.8
        SFACTB= 32.0
C       parameter for heat balance components (kcal/m2 or btu/ft2)        
        XFACTA= 0.369
C
C       parameters for flux variables with entrained thermal
C       energy units
        IF (UUNITS .EQ. 1) THEN
C         english to english
          FFACTA= 112.37
          FFACTB= 0.0
        ELSE
C         metric to english
          FFACTA= 3.97E03
          FFACTB= 0.0
        END IF
C
      ELSE
C       printout is in metric system
C
C       parameters for state variables with temperature units
        SFACTA= 1.0
        SFACTB= 0.0
C       parameter for heat balance components (kcal/m2 or btu/ft2)        
        XFACTA= 1.0
C
C       parameters for flux variables with entrained thermal
C       energy units
        IF (UUNITS .EQ. 1) THEN
C         english to metric
          FFACTA= 28.32
          FFACTB= 0.0
        ELSE
C         metric to metric
          FFACTA= 1.0E03
          FFACTB= 0.0
        END IF
C
      END IF
C
C     convert variables to external units
C
C     Rchres-wide variables
C
C     state variables
      PATMP= AIRTMP*SFACTA + SFACTB
      PDTMP= DEWTMP*SFACTA + SFACTB
      PWTMP= TW*SFACTA + SFACTB
C
C     inflow fluxes
      PIFLX= HTIF(LEV)*FFACTA
C
C     storages
      PRHT = HTST(1)*FFACTA + FFACTB
      PRHTS= HTST(LEV)*FFACTA + FFACTB
C
C     computed fluxes
      CALL TRNVEC
     I            (I2,HTCF2(1,LEV),FFACTA,FFACTB,
     O             PCFLX2)
C
      IF (NEXITS .GT. 1) THEN
C       exit-specific variables
        CALL TRNVEC
     I              (NEXITS,HTCF3(1,LEV),FFACTA,FFACTB,
     O               PCFLX3)
      END IF
C
C     atmospheric/bed heat balance components
      CALL TRNVEC
     I            (I7,HTCF4(1,LEV),XFACTA,FFACTB,
     O             PCFLX4)
C
C     do printout on unit printu
      IF (PRINTU .GT. 0 .AND. PFLAG(4) .LE. LEV) THEN
        WRITE (PRINTU,2000)
        WRITE (PRINTU,2010)
C
        IF (UNITFG .EQ. 1) THEN
          WRITE (PRINTU,2020)
        ELSE
          WRITE (PRINTU,2030)
        END IF
        WRITE (PRINTU,2040)  PATMP, PDTMP, PWTMP
C       
        IF (SHADFG .EQ. 1) THEN        
          WRITE (PRINTU,2170) SHDFAC
        END IF        
      END IF
      IF (BINU .GT. 0 .AND. ABS(BFLAG(4)) .LE. LEV) THEN
        ACNT = ACNT + 1
        APRINT(ACNT) = PATMP
        CHEAD(ACNT) = 'AIRTMP'
        CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
        ACNT = ACNT + 1
        APRINT(ACNT) = PDTMP
        CHEAD(ACNT) = 'DEWTMP'
        CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
        ACNT = ACNT + 1
        APRINT(ACNT) = PWTMP
        CHEAD(ACNT) = 'TW'
        CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
      END IF
C
      IF (NEXITS .GT. 1) THEN
        IF (PRINTU .GT. 0 .AND. PFLAG(4) .LE. LEV) THEN
          WRITE (PRINTU,2050)
          IF (UNITFG .EQ. 1) THEN
            WRITE (PRINTU,2060)  (J,J=1,NEXITS)
          ELSE
            WRITE (PRINTU,2070)  (J,J=1,NEXITS)
          END IF
          WRITE (PRINTU,2080)
          WRITE (PRINTU,2090)  PIFLX, PCFLX2, (PCFLX3(J),J=1,NEXITS)
        END IF
        IF (BINU .GT. 0 .AND. ABS(BFLAG(4)) .LE. LEV) THEN
          ACNT = ACNT + 1
          APRINT(ACNT) = PIFLX
          CHEAD(ACNT) = 'IHEAT'
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          DO 10 I= 1, 2
            ACNT = ACNT + 1
            APRINT(ACNT) = PCFLX2(I)
            CHEAD(ACNT) = CCFLX2(I)
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 10       CONTINUE
          DO 30 I= 1, NEXITS
            CALL INTCHR (I, I2, I1,
     O                   JLEN, CSTR)
            ACNT = ACNT + 1
            APRINT(ACNT) = PCFLX3(I)
            CHEAD(ACNT) = 'OHEAT - EXIT-'
            DO 20 IX= 1, JLEN
              CHEAD(ACNT) = TRIM(CHEAD(ACNT)) // CSTR(IX)
 20         CONTINUE
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 30       CONTINUE
        END IF
C
      ELSE
C       single exit
        IF (PRINTU .GT. 0 .AND. PFLAG(4) .LE. LEV) THEN
          WRITE (PRINTU,2100)
          IF (UNITFG .EQ. 1) THEN
            WRITE (PRINTU,2060)
          ELSE
            WRITE (PRINTU,2070)
          END IF
          WRITE (PRINTU,2110)
          WRITE (PRINTU,2090)  PIFLX, PCFLX2
        END IF
        IF (BINU .GT. 0 .AND. ABS(BFLAG(4)) .LE. LEV) THEN
          ACNT = ACNT + 1
          APRINT(ACNT) = PIFLX
          CHEAD(ACNT) = 'IHEAT'
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          DO 40 I= 1, 2
            ACNT = ACNT + 1
            APRINT(ACNT) = PCFLX2(I)
            CHEAD(ACNT) = CCFLX2(I)
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 40       CONTINUE
        END IF
C
      END IF
C
C     atmospheric/bed heat balance components printout
      IF (PRINTU .GT. 0 .AND. PFLAG(4) .LE. LEV) THEN
        IF (UNITFG .EQ. 1) THEN
          WRITE (PRINTU,2140)
        ELSE
          WRITE (PRINTU,2150)
        END IF
        WRITE (PRINTU,2160)
        WRITE (PRINTU,2090)  (PCFLX4(J),J=1,7)
      END IF
      IF (BINU .GT. 0 .AND. ABS(BFLAG(4)) .LE. LEV) THEN
        DO 50 I= 1, 7
          ACNT = ACNT + 1
          APRINT(ACNT) = PCFLX4(I)
          CHEAD(ACNT) = CCFLX4(I)
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 50     CONTINUE
      END IF
C
C     material balance check
      IF (UNITFG .EQ. 1) THEN
C       english
        UNITID= '     BTU'
      ELSE
C       metric
        UNITID= '    KCAL'
      END IF
C
C     calculate net quantity of thermal energy entering rchres
      HTDIF= PIFLX + PCFLX2(1) - PCFLX2(2)
      J    = 3
      CALL BALCHK
     I            (J,RCHNO,DATIM,MESSU,PRINTU,MSGFL,
     I             PRHTS,PRHT,PIFLX,HTDIF,UNITID,I1,
     M             HTWCNT)
C
      IF (BINU .GT. 0 .AND. ABS(BFLAG(4)) .LE. LEV) THEN
C       write binary output
        CALL EXDATE(
     I              DATIM,
     O              EXDAT)
        IF (BFLAG(4) .GT. 0) THEN
C         at start of run, write the header
          WRITE (BINU) I0,'RCHRES  ',RCHNO,'HTRCH   ',
     1          (CLEN(I),(CHEAD(I)(J:J),J=1,CLEN(I)),I=1,ACNT)
C         set bflag to negative to not write headers anymore
          BFLAG(4) = -BFLAG(4)
        END IF
        WRITE (BINU) I1,'RCHRES  ',RCHNO,'HTRCH   ',UNITFG,
     1               LEV,(EXDAT(J),J=1,5),(APRINT(J),J=1,ACNT)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   HTRB
C
C     + + + PURPOSE + + +
C     Handle section htrch
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION HTRCH2 + + +
      INCLUDE   'crhht.inc'
      INCLUDE   'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I
C
C     + + + END SPECIFICATIONS + + +
C
      IF (ATHTFP .GE. 1) THEN
        PAD(ATHTFP+ IVL1)= HTEXCH
      END IF
C      
      IF (ROHTFP .GE. 1) THEN
        PAD(ROHTFP+ IVL1)= ROHEAT
      END IF
C      
      IF (RCHTFP .GE. 1) THEN
        PAD(RCHTFP+ IVL1)= IHEAT
      END IF
C      
      IF (NEXITS .GT. 1) THEN
        DO 10 I= 1, NEXITS
          IF (OHTFP(I) .GE. 1) THEN
            PAD(OHTFP(I)+ IVL1)= OHEAT(I)
          END IF
 10     CONTINUE
      END IF
C      
C     handle atmospheric/bed heat balance components
      DO 20 I= 1,7
        IF (HTCF4X(I) .GE. 1) THEN
          PAD(HTCF4X(I)+ IVL1)= HTCF4(I,1)
        END IF
 20   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   HTRP
C
C     + + + PURPOSE + + +
C     Handle section htrch
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION HTRCH2 + + +
      INCLUDE 'crhht.inc'
      INCLUDE 'cmpad.inc'
C
C     + + + END SPECIFICATIONS + + +
C
C     handle section htrch
      IF (TWFP .GE. 1) THEN
        PAD(TWFP +IVL1)   = TW
      END IF
      IF (AIRTFP .GE. 1) THEN
        PAD(AIRTFP + IVL1)= AIRTMP
      END IF
      IF (SHADFG .EQ.1 .AND. SHDFP .GE. 1) THEN
        PAD(SHDFP + IVL1)= SHDFAC
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   HTRST
     I                   (LEV)
C
C     + + + PURPOSE + + +
C     Reset flux and state variables for module section htrch
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    LEV
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEV    - current output level (2-pivl,3-day,4-mon,5-ann)
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION HTRCH2 + + +
      INCLUDE    'crhht.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I2,I7
C
C     + + + EXTERNALS + + +
      EXTERNAL   SETVEC
C
C     + + + END SPECIFICATIONS + + +
C
      I2= 2
      I7= 7
C     handle flux groups dealing with reach-wide variables
      HTIF(LEV)= 0.0
      CALL SETVEC
     I            (I2,0.0,
     O             HTCF2(1,LEV))
C
      IF (NEXITS .GT. 1) THEN
C       handle flux groups dealing with individual exit gates
        CALL SETVEC
     I              (NEXITS,0.0,
     O               HTCF3(1,LEV))
      END IF
C
C     keep present thermal storage in state variable used for
C     material balance check
      HTST(LEV)= HTST(1)
C
C     handle atmospheric/bed heat balance components
      CALL SETVEC
     I            (I7,0.0,
     O             HTCF4(1,LEV))
C
      RETURN
      END
C
C
C
      SUBROUTINE   RATEMP
     I                    (MPREC,DELT,LAPSE,GATMP,ELDAT,HR,
     O                     AIRTMP)
C
C     + + + PURPOSE + + +
C     Correct air temperature for elevation difference between mean
C     segment elevation and gage elevation
C
C     + + + DUMMY VARIABLES + + +
      INTEGER    HR
      REAL       AIRTMP,DELT,ELDAT,GATMP,LAPSE(24),MPREC
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MPREC  - ???
C     DELT   - simulation time interval in minutes
C     LAPSE  - ???
C     GATMP  - ???
C     ELDAT  - ???
C     HR     - ???
C     AIRTMP - ???
C
C     + + + LOCAL VARIABLES + + +
      REAL       LAPS,PRRAT
C
C     + + + END SPECIFICATIONS + + +
C
C     find precipitation rate during the interval; prrat is
C     expressed in m/min
C
      PRRAT= MPREC/DELT
      IF (PRRAT .GT. 2.0E-5) THEN
C       use rain period lapse rate expressed as deg c/ft
        LAPS= 1.94E-03
      ELSE
C       use dry period lapse rate expressed as deg c/ft
        LAPS= LAPSE(HR)
      END IF
C
C     compute corrected air temperature for the end of the current
C     interval; airtmp is expressed in degrees c
      AIRTMP= GATMP - LAPS*ELDAT
C
      RETURN
      END
C
C
C
      SUBROUTINE   HTACC
     I                   (FRMROW,TOROW)
C
C     + + + PURPOSE + + +
C     Accumulate fluxes in module section htrch for printout
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   FRMROW,TOROW
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FRMROW - row containing incremental flux accumulation
C     TOROW  - flux row to be incremented
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION HTRCH2 + + +
      INCLUDE   'crhht.inc'
      INCLUDE   'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I2,I7
C
C     + + + EXTERNALS + + +
      EXTERNAL  ACCVEC
C
C     + + + END SPECIFICATIONS + + +
C
      I2= 2
      I7= 7
C
C     handle flux groups dealing with reach-wide variables
      HTIF(TOROW)= HTIF(TOROW) + HTIF(FRMROW)
      CALL ACCVEC
     I            (I2,HTCF2(1,FRMROW),
     M             HTCF2(1,TOROW))
C
      IF (NEXITS .GT. 1) THEN
C       handle flux groups dealing with individual exit gates
        CALL ACCVEC
     I              (NEXITS,HTCF3(1,FRMROW),
     M               HTCF3(1,TOROW))
      END IF
C
C     handle atmospheric/bed heat balance components
      CALL ACCVEC
     I            (I7,HTCF4(1,FRMROW),
     M             HTCF4(1,TOROW))
C
      RETURN
      END
