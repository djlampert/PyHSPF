C
C
C
      SUBROUTINE   PADCAL
C
C     + + + PURPOSE + + +
C     Process input to section adcalc of the rchres module
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION ADCALC1 + + +
      INCLUDE    'crhad.inc'
      INCLUDE    'crin2.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I1,I2,I16
      REAL       RVAL(2)
C
C     + + + EXTERNALS + + +
      EXTERNAL   RTABLE
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' PROCESSING INPUT FOR SECTION ADCALC')
 2010 FORMAT (/,' FINISHED PROCESSING INPUT FOR SECTION ADCALC')
C
C     + + + END SPECIFICATIONS + + +
C
      I1 = 1
      I2 = 2
      I16= 16
      IF (OUTLEV.GT.1) THEN
        WRITE (MESSU,2000)
      END IF
C
C     process value in table-type adcalc-data
      CALL RTABLE
     I             (I16,I1,I2,UUNITS,
     M              RVAL)
      CRRAT= RVAL(1)
C
C     if section hydr is inactive, initial volume is handled
C        in adcalc
      IF (HYDRFG.EQ.0) THEN
        VOL= RVAL(2)
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
      SUBROUTINE    ADCALC
C
C     + + + PURPOSE + + +
C     Prepare to simulate advection of fully entrained constituents
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION ADCALC2 + + +
      INCLUDE    'crhad.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    N,REQFG,TSSUB(2),FLGVAL
      REAL       COJS,JS,RAT,VOLSP
      CHARACTER*6 OPTYP,TSNAM,SECNAM,MSECNM,OPFGNM
C
C     + + + EXTERNALS + + +
      EXTERNAL  HREQTS
C
C     + + + DATA INITIALIZATIONS + + +
      DATA TSSUB/1,1/
      DATA OPTYP,SECNAM,MSECNM/'RCHRES','ADCALC','HYDR  '/
C
C     + + + END SPECIFICATIONS + + +
C
      IF (HYDRFG .EQ. 0) THEN
C       read time series supplied by previous hydr simulation; hydr
C       is inactive
        ROS= 0.0
        RO = 0.0
C
        REQFG= 3
        TSNAM= 'O     '
        DO 10 N= 1,NEXITS
          OS(N)= O(N)
CTHJ          O(N) = PAD(OFP(N)+IVL1)
          TSSUB(1)= N
          CALL HREQTS (OFP(N),IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,RCHNO,
     I                 TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,FLGVAL,
     O                 O(N))
          ROS  = ROS+ OS(N)
          RO   = RO+ O(N)
 10     CONTINUE
        TSSUB(1)= 1
C
        VOLS= VOL
CTHJ        VOL = PAD(VOLFP+IVL1)
        REQFG= 3
        TSNAM= 'VOL   '
        CALL HREQTS (VOLFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,RCHNO,
     I               TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,FLGVAL,
     O               VOLSP)
        VOL= VOLSP
      ELSE
C       above time series are available from hydr
      END IF
C
C     calculate weighting factors given to rate of outflow of
C     constituents at start (js) and end (cojs) of interval,
C     in calculating mean rate of outflow over the ivl
C
      IF (ROS .GT. 0.0) THEN
C       calculate ratio of volume to outflow volume; delts is the
C       simulation interval in seconds
C
        RAT= VOLS/(ROS*DELTS)
        IF (RAT .LT. CRRAT) THEN
C         part of water in outflow volume entered control volume as
C         inflow during same interval; hence, concentration of
C         inflowing material will affect outflow concentration and
C         js will be < 1.0
          JS= RAT/CRRAT
        ELSE
C         all water in outflow volume was contained in control volume
C         at beginning of ivl; mean rate of outflow over ivl will be
C         wholly dependent upon rate of outflow of constituents at
C         start of ivl
          JS= 1.0
        END IF
      ELSE
C       reach/res has no outflow at start of ivl
        JS= 0.0
      END IF
C
C     cojs is the complement of js
      COJS= 1.0 - JS
C
C     calculate weighted volumes of outflow at start of ivl (srovol),
C     and end of ivl (erovol)
      SROVOL= JS*ROS*DELTS
      EROVOL= COJS*RO*DELTS
C
      IF (NEXITS .GT. 1) THEN
C       determine weighted volume of outflow at start and end of ivl
C       for each exit gate
        DO 80 N= 1,NEXITS
          SOVOL(N)= JS*OS(N)*DELTS
          EOVOL(N)= COJS*O(N)*DELTS
 80     CONTINUE
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   ADVECT
     I                    (IMAT,VOLS,SROVOL,VOL,EROVOL,SOVOL,
     I                     EOVOL,NEXITS,
     M                     CONC,
     O                     ROMAT,OMAT)
C
C     + + + PURPOSE + + +
C     Simulate advection of constituent totally entrained in water
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    NEXITS
      REAL       EOVOL(5),EROVOL,IMAT,OMAT(5),ROMAT,SOVOL(5),
     $           SROVOL
      DOUBLE PRECISION CONC,VOL,VOLS
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IMAT   - ???
C     VOLS   - ???
C     SROVOL - ???
C     VOL    - volume of water in reach above bed
C     EROVOL - ???
C     SOVOL  - ???
C     EOVOL  - ???
C     NEXITS - number of exits from the operation
C     CONC   - ???
C     ROMAT  - ???
C     OMAT   - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    N
      DOUBLE PRECISION CONCS
C
C     + + + END SPECIFICATIONS + + +
C
C     save starting concentration value
C
      CONCS= CONC
      IF (VOL .GT. 0.0) THEN
C       reach/res contains water; perform advection normally
C
C       calculate new concentration of material in reach/res based
C       on quantity of material entering during interval (imat),
C       weighted volume of outflow based on conditions at start of
C       ivl (srovol), and weighted volume of outflow based on
C       conditions at end of ivl (erovol)
C
        CONC = (IMAT + CONCS*(VOLS - SROVOL))/(VOL + EROVOL)
C
C       calculate total amount of material leaving reach/res during
C       interval
        ROMAT= SROVOL*CONCS + EROVOL*CONC
C
        IF (NEXITS .GT. 1) THEN
C         calculate amount of material leaving through each exit gate;
C         omat is expressed as qty.vol/l.ivl
C
          DO 10 N= 1,NEXITS
            OMAT(N)= SOVOL(N)*CONCS + EOVOL(N)*CONC
 10       CONTINUE
C
        END IF
C
      ELSE
C       reach/res has gone dry during the interval; set conc equal to
C       an undefined value
        CONC = -1.0E30
C
C       calculate total amount of material leaving during interval;
C       this is equal to material inflow + material initially present
        ROMAT= IMAT + (CONCS*VOLS)
C
        IF (NEXITS .GT. 1) THEN
C         calculate amount of material leaving through each exit gate;
C         omat is expressed as qty.vol/l.ivl
C
          DO 60 N= 1,NEXITS
            IF (SROVOL .GT. 0.0) THEN
              OMAT(N)= (SOVOL(N)/SROVOL)*ROMAT
            ELSE
              OMAT(N)= 0.0
            END IF
 60       CONTINUE
C
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   OXREA
     I                   (LKFG,WIND,CFOREA,AVVELE,AVDEPE,TCGINV,
     I                    REAMFG,REAK,REAKT,EXPRED,EXPREV,LEN,
     I                    DELTH,TW,DELTS,DELT60,UUNITS,
     O                    KOREA)
C
C     + + + PURPOSE + + +
C     Calculate oxygen reaeration coefficient
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    LKFG,REAMFG,UUNITS
      REAL       AVDEPE,AVVELE,CFOREA,DELTH,DELTS,DELT60,EXPRED,EXPREV,
     #           KOREA,LEN,REAK,REAKT,TCGINV,TW,WIND
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LKFG   - ???
C     WIND   - ???
C     CFOREA - ???
C     AVVELE - ???
C     AVDEPE - ???
C     TCGINV - ???
C     REAMFG - ???
C     REAK   - ???
C     REAKT  - ???
C     EXPRED - ???
C     EXPREV - ???
C     LEN    - ???
C     DELTH  - ???
C     TW     - water temperature in degrees C
C     DELTS  - ???
C     DELT60 - simulation time interval in hours
C     UUNITS - system of units   1-english, 2-metric
C     KOREA  - ???
C
C     + + + LOCAL VARIABLES + + +
      REAL       DELTHE,FLOTIM,LENE,TRANDP,WINDF,WINDSP
C
C     + + + INTRINSICS + + +
      INTRINSIC   ABS
C
C     + + + END SPECIFICATIONS + + +
C
      IF (LKFG .EQ. 1) THEN
C       this reach/res is a lake or reservoir
C
C       calculate reaeration coefficient based on windspeed, surface
C       area, and volume; windsp is windspeed in m/sec; wind is
C       wind movement in m/ivl
        WINDSP= WIND/DELTS
C
        IF (WINDSP .GT. 6.0) THEN
C         determine windspeed factor empirically
          WINDF= WINDSP*( -.46 + .136*WINDSP)
        ELSE
C         assign value of 2.0 as windspeed factor
          WINDF= 2.0
        END IF
C       calculate reaeration coefficient
        KOREA= (.032808*WINDF*CFOREA/AVDEPE)*DELT60
      ELSE
C       calculate reaeration coefficient for free-flowing reach
C
        IF (REAMFG .EQ. 1) THEN
C         convert length and drop in energy line along length of
C         rchres to english units, if necessary
          IF (UUNITS .EQ. 2) THEN
            LENE  = LEN*3.28
            DELTHE= DELTH*3.28
          ELSE
            LENE  = LEN
            DELTHE= DELTH
          END IF
C
C         calculate reaeration coefficient based on energy
C         dissipation principles (tsivoglou method)
          IF ((ABS(AVVELE)) .GT. 0.0) THEN
            FLOTIM= LENE/AVVELE
            KOREA = REAKT*(DELTHE/FLOTIM)*(TCGINV**(TW - 20.))*DELTS
          ELSE
            KOREA = 0.0
          END IF
        ELSE
          IF (REAMFG .EQ. 2) THEN
C           calculate reaeration coefficient as a power function of
C           average hydraulic depth and velocity; determine exponents
C           to depth and velocity terms and assign value to reak
            IF (AVDEPE .LE. 2.0) THEN
C             use owen's formulation for reaeration
              REAK  = .906
              EXPREV= 0.67
              EXPRED= -1.85
            ELSE
C             calculate transition depth; transition depth determines
C             which method of calculation is used given the current
C             velocity
              IF (AVVELE .LT. 1.7) THEN
                TRANDP= 0.0
              ELSE
                TRANDP= .4263*(AVVELE**2.9135)
              END IF
C
              IF ((AVDEPE - TRANDP) .LE. 0.0) THEN
C               use churchill's formulation for reaeration
                REAK  = .484
                EXPREV= .969
                EXPRED= -1.673
              ELSE
C               use o'connor-dobbins formulation for reaeration
                REAK  = .538
                EXPREV= 0.5
                EXPRED= -1.5
              END IF
            END IF
          ELSE
C           values for reak, exprev, and expred are input by user
C
          END IF
C
C         calculate reaeration coefficient
          KOREA= REAK*(AVVELE**EXPREV)*(AVDEPE**EXPRED)
     $           *(TCGINV**(TW - 20.))*DELT60
        END IF
      END IF
C
      IF (KOREA .GT. 1.0) THEN
        KOREA= .999
      END IF
C
      RETURN
      END
