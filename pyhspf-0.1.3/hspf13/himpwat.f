C
C
C
      SUBROUTINE   PIWATR
     I                    (OUTLEV)
C
C     + + + PURPOSE + + +
C     Process input for iwater section of module implnd
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   OUTLEV
C
C     + + + ARGUMENT DEFINITIONS + + +
C     OUTLEV - run interp output level
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION IWATER1 + + +
      INCLUDE  'ciliw.inc'
      INCLUDE  'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I1,I2,I4
      REAL      RVAL(4)
C
C     + + + EXTERNALS + + +
      EXTERNAL  ITABLE,RTABLE
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' PROCESSING INPUT FOR SECTION IWATER')
 2010 FORMAT (/,' FINISHED PROCESSING INPUT FOR SECTION IWATER')
C
C     + + + END SPECIFICATIONS + + +
C
      I1=1
C
      IF (OUTLEV.GT.1) THEN
C       processing message
        WRITE (MESSU,2000)
      END IF
C
C     warning message counter initialization
      IWWCNT(1)= 0
C
C     initialize flag state variables
      SMSFG = 0
      FSMSFG= 0
C
C     process values in table - type iwat-parm1
      I2= 13
      I4= 5
      CALL ITABLE (I2,I1,I4,UUNITS,
     M             IWPM1)
C
C     process values in table - type iwat-parm2
      I2= 14
      I4= 4
      CALL RTABLE (I2,I1,I4,UUNITS,
     M             RVAL)
C
      LSUR = RVAL(1)
      SLSUR= RVAL(2)
      NSUR = RVAL(3)
      RETSC= RVAL(4)
C
C     process values in table - type iwat-parm3
      I2= 15
      I4= 2
      CALL RTABLE (I2,I1,I4,UUNITS,
     M             IWPM3)
C
      IF (VRSFG.EQ.1) THEN
C       get monthly retention storage capacity - table-type mon-retn
        I2= 16
        I4= 12
        CALL RTABLE (I2,I1,I4,UUNITS,
     M               RETSCM)
      END IF
C
      IF (VNNFG.EQ.1) THEN
C       get monthly values of manning's n - table-type mon-manning
        I2= 17
        I4= 12
        CALL RTABLE (I2,I1,I4,UUNITS,
     M               NSURM)
      END IF
C
C     initial conditions, process values in table-type iwat-state1
      I2= 18
      I4= 2
      CALL RTABLE (I2,I1,I4,UUNITS,
     M             IWST1)
C
C     total storage in the ils
      IMPS= RETS+ SURS
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
      SUBROUTINE   IWATER
C
C     + + + PURPOSE + + +
C     Simulate the water budget for an impervious land segment.
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION IWATER2 + + +
      INCLUDE  'ciliw.inc'
      INCLUDE  'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     REQFG,TSSUB(2),FLGVAL
      REAL        MSUPY,RETI
      CHARACTER*6 TSNAM,OPTYP,SECNAM,MSECNM,OPFGNM
C
C     + + + EXTERNALS + + +
      EXTERNAL    RETN,IROUTE,EVRETN,HREQTS
C
C     + + + DATA INITIALIZATIONS + + +
      DATA TSSUB/1,1/
      DATA OPTYP,SECNAM/'IMPLND','IWATER'/
C
C     + + + END SPECIFICATIONS + + +
C
C     get input PET
CTHJ      PETINP= PAD(PETIFP + IVL1)
      REQFG= 2   
      TSNAM= 'PETINP'
      CALL HREQTS (PETIFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,LSNO,
     I             TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,FLGVAL,
     O             PETINP)
C
      IF (CSNOFG .EQ. 1) THEN
C       snow is being considered - allow for it
C       find the moisture supplied to retention storage
C       rainf is rainfall in in./ivl. adjust for fraction of land
C       segment covered by snow. wyield is the water yielded by the
C       snowpack in in./ivl. it has already been adjusted to an
C       effective yield over the entire land segment.
C
C       get input time series
        IF (AIRTFG .EQ. 0) THEN
C         get air temperature data
CTHJ          AIRTMP= PAD(AIRTFP + IVL1)
          REQFG= 5
          MSECNM= 'ATEMP '
          OPFGNM= 'CSNOFG'
          CALL HREQTS (AIRTFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,
     I                 LSNO,TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,CSNOFG,
     O                 AIRTMP)
        ELSE
C         air temperatures, in degrees f, are available from
C         section atemp
        END IF
C
        IF (SNOWFG .EQ. 0) THEN
C         get snow time series
Cthj          RAINF= PAD(RNFFP+ IVL1)
          REQFG= 5
          MSECNM= 'SNOW  '
          OPFGNM= 'CSNOFG'
          TSNAM= 'RAINF '
          CALL HREQTS (RNFFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,LSNO,
     I                 TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,CSNOFG,
     O                 RAINF)
Cthj          SNOCOV= PAD(SNOCFP+ IVL1)
          TSNAM= 'SNOCOV'
          CALL HREQTS (SNOCFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,LSNO,
     I                 TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,CSNOFG,
     O                 SNOCOV)
Cthj        WYIELD= PAD(WYFP+ IVL1)
          TSNAM= 'WYIELD'
          CALL HREQTS (WYFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,LSNO,
     I                 TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,CSNOFG,
     O                 WYIELD)
        ELSE
C         the above time series are available from snow
        END IF
C
        SUPY= RAINF*(1.0 - SNOCOV) + WYIELD
C
        IF (HRFG .EQ. 1) THEN
C         it is time to recalculate intermittently computed numbers
C
C         adjustment factor for input pet to account for snowcover
          PETADJ= (1.0 - SNOCOV)
C
          IF (AIRTMP .LT. PETMAX) THEN
C           adjustment factor may be reduced
            IF (AIRTMP .LT. PETMIN) THEN
C             pet is completely shut off
              PETADJ= 0.0
            ELSE
              IF (PETADJ .GT. 0.5) THEN
                PETADJ= 0.5
              END IF
            END IF
C
          END IF
C
        ELSE
C         petadj remains unchanged
C
        END IF
C
C       adjust input pet
        PET= PETINP*PETADJ
C
      ELSE
C       snow is not being considered
C       all precipitation is assumed to be rain;  therefore, the
C       moisture supply is considered to be precipitation
CTHJ        PREC= PAD(PRECFP + IVL1)
        REQFG= 4
        TSNAM= 'PREC  '
        OPFGNM= 'CSNOFG'
        CALL HREQTS (PRECFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,LSNO,
     I               TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,CSNOFG,
     O               PREC)
        SUPY= PREC
        PET = PETINP
C
      END IF
C
C     get surface lateral inflow if any
      IF (SLIFP .GT. 0) THEN
        SURLI= PAD(SLIFP + IVL1)
      ELSE
        SURLI= 0.0
      END IF
C
      IF (RTLIFG .EQ. 1) THEN
C       surface lateral inflow (if any) is subject to retention
        RETI= SUPY + SURLI
C
        CALL RETN
     I           (VRSFG,DAYFG,RETSCM,RETI,MON,NXTMON,DAY,NDAYS,
     M            RETSC,RETS,
     O            RETO)
C
        SURI= RETO
C
      ELSE
C       surface lateral inflow (if any) is not subject to retention
        RETI= SUPY
C
        CALL RETN
     I           (VRSFG,DAYFG,RETSCM,RETI,MON,NXTMON,DAY,NDAYS,
     M            RETSC,RETS,
     O            RETO)
C
        SURI= RETO + SURLI
C
      END IF
C
      MSUPY= SURI + SURS
C
C     set flags which indicate whether or not there is any surface
C     moisture supply and whether or not this is the first in a
C     series of wet intervals
C
      IF (MSUPY .GT. 0.0) THEN
C       there is moisture on the surface
C
        IF (SMSFG .EQ. 0) THEN
C         this is the first interval with surface moisture supply
C         after one or more intervals with none
          FSMSFG= 1
        ELSE
C         this is not the first wet interval
          FSMSFG= 0
        END IF
C
C       there is surface moisture supply
        SMSFG= 1
C
      ELSE
C       there is no surface moisture supply
        SMSFG = 0
        FSMSFG= 0
C
      END IF
C
      IF (SMSFG .EQ. 1) THEN
C       there is surface moisture supply
C       determine how much of the moisture supply runs off
C       in one simulation interval
        CALL IROUTE
     I             (FSMSFG,DAYFG,MSUPY,SURI,VNNFG,NSURM,LSUR,
     I              SLSUR,RTOPFG,DELT60,MON,NXTMON,DAY,NDAYS,MESSU,
     I              MSGFL,LSNO,
     M              NSUR,SURS,DEC,SRC,
     O              SURO)
      ELSE
        SURS= 0.0
        SURO= 0.0
      END IF
C
C     determine evaporation from retention storage
      CALL EVRETN
     I           (PET,
     M            RETS,
     O            IMPEV)
C
C     total input of water to the impervious land segment
      WATIN = SUPY + SURLI
C
C     net input of water to the impervious segment
      WATDIF= WATIN - (SURO + IMPEV)
C
C     total moisture storage
      IMPS  = RETS + SURS
C
      RETURN
      END
C
C
C
      SUBROUTINE   EVRETN
     I                   (PET,
     M                    RETS,
     O                    IMPEV)
C
C     + + + PURPOSE + + +
C     Simulate evaporation from retention storage.
C
C     + + + DUMMY ARGUMENTS + + +
      REAL       IMPEV,PET,RETS
C
C     + + + ARGUMENT DEFINITIONS + + +
C     PET    - ???
C     RETS   - ???
C     IMPEV  - ???
C
C     + + + END SPECIFICATIONS + + +
C
      IF (RETS .GT. 0.0) THEN
C       there is something in retention storage to evaporate
        IF (PET .GT. RETS) THEN
C         evaporation from retention storage is limited by
C         quantity available
          IMPEV= RETS
          RETS = 0.0
        ELSE
C         evaporation will not exhaust storage, so empty at potential
          IMPEV= PET
          RETS = RETS - IMPEV
        END IF
      ELSE
C       there is no evaporation from retention storage
        IMPEV= 0.0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   IROUTE
     I                  (FSMSFG,DAYFG,MSUPY,SURI,VNNFG,NSURM,LSUR,
     I                   SLSUR,RTOPFG,DELT60,MON,NXTMON,DAY,NDAYS,MESSU,
     I                   MSGFL,LSNO,
     M                   NSUR,SURS,DEC,SRC,
     O                   SURO)
C
C     + + + PURPOSE + + +
C     Determine how much of the moisture supply (MSUPY) runs off
C     in one simulation period.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    DAY,DAYFG,FSMSFG,MESSU,MON,NDAYS,NXTMON,RTOPFG,VNNFG,
     $           MSGFL,LSNO
      REAL       DEC,DELT60,LSUR,MSUPY,NSUR,NSURM(12),SLSUR,SRC,SURI,
     $           SURO,SURS
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FSMSFG - ???
C     DAYFG  - flag for first day or day change
C     MSUPY  - ???
C     SURI   - ???
C     VNNFG  - ???
C     NSURM  - ???
C     LSUR   - ???
C     SLSUR  - ???
C     RTOPFG - ???
C     DELT60 - simulation time interval in hours
C     MON    - calendar month
C     NXTMON - next calendar month
C     DAY    - day of month
C     NDAYS  - no. of days in this month
C     MESSU  - ftn unit no. to be used for printout of messages
C     MSGFL  - fortran unit number of error message file
C     LSNO   - line number in the opn sequence block of uci
C     NSUR   - ???
C     SURS   - ???
C     DEC    - ???
C     SRC    - ???
C     SURO   - surface output
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    COUNT,SCLU,SGRP,DUMCNT
      REAL       DUMMY,SSUPR,SURSE,SURSM,TSURO,FACT,SURSNW,A1,FSURO,
     $           DFSURO,DSURO,CHANGE,RATIO,DTERM,STERM,FFACT,DFACT
C
C     + + + FUNCTIONS + + +
      REAL       DAYVAL
C
C     + + + INTRINSICS + + +
      INTRINSIC  ABS,SQRT
C
C     + + + EXTERNALS + + +
      EXTERNAL   DAYVAL,OMSTI,OMSTR,OMSG
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU = 323
C
      IF (FSMSFG .EQ. 1 .OR. DAYFG .EQ. 1) THEN
C       it is time to recompute any varying parameters
C
        IF (VNNFG .EQ. 1) THEN
C         mannings n is allowed to vary throughout the year
C         interpolate for the daily value
C         linearly interpolate nsur between two values from the
C         monthly array nsurm(12)
          NSUR= DAYVAL(NSURM(MON),NSURM(NXTMON),DAY,NDAYS)
        ELSE
C         mannings n does not vary throughout the year
C         nsur value has been supplied by the run interpreter
        END IF
C
C       calculate parameters for routing surface runoff
        DEC= 0.00982*(NSUR*LSUR/SQRT(SLSUR))**0.6
        SRC= 1020.0*(SQRT(SLSUR)/(NSUR*LSUR))
      END IF
C
      IF (MSUPY .GT. 0.0002) THEN
C       something is worth routing on the surface
        IF (RTOPFG .NE. 1) THEN
C         do routing the new way
C         estimate the rate of supply to the overland flow surface -
C         in./hour
          SSUPR= SURI/DELT60
C         determine equilibrium depth for this supply rate
          SURSE= 0.0
          IF (SSUPR .GT. 0.0) THEN
            SURSE= DEC*SSUPR**0.6
          END IF
C         determine runoff by iteration - newton's method
C         estimate the new surface storage
          SURSNW= MSUPY
          SURO  = 0.0
          COUNT = 0
C         dountil relative error is small
 10       CONTINUE
            IF (SSUPR .GT. 0.0) THEN
              RATIO= SURSNW/SURSE
              IF (RATIO .LE. 1.0) THEN
C               flow is increasing
                FACT= 1.0 + 0.6*RATIO**3
              ELSE
                FACT= 1.6
              END IF
            ELSE
C             ratio is arbitrarily large for supply rate <= 0
              RATIO= 1.0E30
              FACT = 1.6
            END IF
C
C           coefficient in outflow equation
            A1    = DELT60*SRC*FACT**1.667
            STERM = SURSNW**1.667
            COUNT = COUNT + 1
            FFACT = A1*STERM
            FSURO = FFACT - SURO
            DFACT = -1.667*FFACT
            DFSURO= DFACT/SURSNW - 1.0
            IF (RATIO .LE. 1.0) THEN
C             additional term required in derivative wrt suro
              DTERM =DFACT/(FACT*SURSE)*1.8*RATIO**2
              DFSURO= DFSURO + DTERM
            END IF
            DSURO= FSURO/DFSURO
C
            IF (COUNT .GT. 100) THEN
C             error message -- didn't converge
              CALL OMSTI (LSNO)
              CALL OMSTR (SURSE)
              CALL OMSTR (SURO)
              CALL OMSTR (SURSNW)
              CALL OMSTR (FSURO)
              CALL OMSTR (DFSURO)
              CALL OMSTR (DSURO)
              SGRP  = 1
              DUMCNT= 0
              CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                   DUMCNT)
            END IF
C
            SURO  = SURO - DSURO
            SURSNW= MSUPY - SURO
            CHANGE= ABS(DSURO/SURO)
          IF (CHANGE .GE. 0.01) GO TO 10
C         enddo
          SURS= SURSNW
        ELSE
C         do routing the way it is done in arm, nps, and hspx
C         estimate the rate of supply to the overland flow surface -
C         in./ivl
          SSUPR= SURI
C         estimate the mean surface detention storage over the
C         interval
          SURSM= (SURS + MSUPY)*0.5
C
C         estimate the equilibrium detention depth for this supply
C         rate - surse
          IF (SSUPR .GT. 0.0) THEN
C           preliminary estimate of surse
            DUMMY= DEC*SSUPR**0.6
C
            IF (DUMMY .GT. SURSM) THEN
C             flow is increasing
              SURSE= DUMMY
              DUMMY= SURSM*(1.0 + 0.6*(SURSM/SURSE)**3)
            ELSE
C             flow on surface is at equilibrium or receding
              DUMMY= SURSM*1.6
            END IF
          ELSE
C           flow on the surface is receding - equilibrium detention is
C           assumed equal to actual detention
            DUMMY= SURSM*1.6
          END IF
C
          TSURO= DELT60*SRC*DUMMY**1.67
C
C         check the temporary calculation of surface outflow
          IF (TSURO .GT. MSUPY) THEN
C           too much impervious surface runoff is estimated
            SURO= MSUPY
            SURS= 0.0
          ELSE
            SURO= TSURO
            SURS= MSUPY - SURO
          END IF
        END IF
      ELSE
C       send what is on the overland flow plane straight to the
C       channel
        SURO= MSUPY
        SURS= 0.0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   IWAACC
     I                   (FRMROW,TOROW)
C
C     + + + PURPOSE + + +
C     Accumulate fluxes for section IWATER.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    FRMROW,TOROW
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FRMROW - row containing incremental flux accumulation
C     TOROW  - flux row to be incremented
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION IWATER2 + + +
      INCLUDE    'ciliw.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I2,I4
C
C     + + + EXTERNALS + + +
      EXTERNAL   ACCVEC
C
C     + + + END SPECIFICATIONS + + +
C
      I2= 2
      I4= 4
C
      IF (SLIFP .GT. 0) THEN
C       lateral input flux is being considered and printed
        IWIF(TOROW) = IWIF(TOROW) + IWIF(FRMROW)
      END IF
C
      CALL ACCVEC
     I           (I4,IWCF1(1,FRMROW),
     M            IWCF1(1,TOROW))
C
      CALL ACCVEC
     I           (I2,IWCF2(1,FRMROW),
     M            IWCF2(1,TOROW))
C
      RETURN
      END
C
C
C
      SUBROUTINE   IWAPRT
     I                   (UNITFG,LEV,PRINTU,BINU)
C
C     + + + PURPOSE + + +
C     Convert quantities from internal to external units, calculate
C     water balance, and print out results.
C     Note: local arrays have
C     identical sizes and structures to the corresponding arrays in
C     the OSV apart from dropping the dimension LEV for fluxes.
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
C     + + + COMMON BLOCKS- SCRTCH, VERSION IWATER2 + + +
      INCLUDE    'ciliw.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,J,I0,I1,I2,I4,ACNT,CLEN(12),EXDAT(5)
      REAL       DFACTA,DFACTB,PCFLX1(4),PCFLX2(2),PIFLX,PIMPS,PIMPSS,
     $           PSTAT1(2),APRINT(12)
      CHARACTER*8   UNITID,CCFLX1(4),CSTAT1(2)
      CHARACTER*256 CHEAD(12)
C
C     + + + EXTERNALS + + +
      EXTERNAL   TRNVEC,BALCHK,EXDATE
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' *** IWATER ***')
 2010 FORMAT (/,'   STATE VARIABLES',14X,'RETN STOR SURF STOR')
 2020 FORMAT (  37X,'RETS      SURS',24X,'PETADJ')
 2030 FORMAT (  37X,'RETS      SURS')
 2040 FORMAT (  37X,'  IN        IN')
 2050 FORMAT (  37X,'  MM        MM')
 2060 FORMAT (  31X,F10.2,F10.2,20X,F10.2)
 2070 FORMAT (/,'   FLUXES',24X,'MOISTURE   OUTFLOW EVAP. POT',
     $          '    ACTUAL      LATERAL INFLOW')
 2080 FORMAT (  37X,'SUPY      SURO       PET     IMPEV',15X,'SURLI')
 2090 FORMAT (  31X,4(8X,'IN'),18X,'IN')
 2100 FORMAT (  31X,4(8X,'MM'),18X,'MM')
 2110 FORMAT (  31X,2F10.3,2F10.2,10X,F10.3)
 2120 FORMAT (/,'   FLUXES',24X,'MOISTURE   OUTFLOW EVAP. POT',
     $          '    ACTUAL')
 2130 FORMAT (  37X,'SUPY      SURO       PET     IMPEV')
 2140 FORMAT (  31X,4(8X,'IN') )
 2150 FORMAT (  31X,4(8X,'MM') )
C
C     + + + END SPECIFICATIONS + + +
C
      I0= 0
      I1= 1
      I2= 2
      I4= 4
C
C     initialize array counter for binary printout, store variable
C     names in local strings for use in building binary headers
      ACNT = 0
      CSTAT1(1) = 'RETS'
      CSTAT1(2) = 'SURS'
      CCFLX1(1) = 'SUPY'
      CCFLX1(2) = 'SURO'
      CCFLX1(3) = 'PET'
      CCFLX1(4) = 'IMPEV'
C
C     dimensionless variable does not need to be converted
C     assign conversion constant for dimensional variables with
C     depth units
      IF (UNITFG .EQ. 1) THEN
C       english system
        DFACTA= 1.0
      ELSE
C       metric system
        DFACTA= 25.4
      END IF
C
      DFACTB= 0.0
C
C     convert dimensional variables to external units
C
C     segment-wide state variables
      CALL TRNVEC
     I           (I2,IWST1,DFACTA,DFACTB,
     O            PSTAT1)
C
      PIMPS = IWST3(1)*DFACTA
      PIMPSS= IWST3(LEV)*DFACTA
C
      IF (SLIFP .GT. 0) THEN
C       lateral inflows are being handled
        PIFLX= IWIF(LEV)*DFACTA
      END IF
C
C     computed fluxes
      CALL TRNVEC
     I           (I4,IWCF1(1,LEV),DFACTA,DFACTB,
     O            PCFLX1)
C
      CALL TRNVEC
     I           (I2,IWCF2(1,LEV),DFACTA,DFACTB,
     O            PCFLX2)
C
      IF (PRINTU .GT. 0 .AND. PFLAG(3) .LE. LEV) THEN
        WRITE (PRINTU,2000)
        WRITE (PRINTU,2010)
C
        IF (CSNOFG .EQ. 1) THEN
          WRITE (PRINTU,2020)
        ELSE
          WRITE (PRINTU,2030)
        END IF
C
        IF (UNITFG .EQ. 1) THEN
          WRITE (PRINTU,2040)
        ELSE
          WRITE (PRINTU,2050)
        END IF
      END IF
C
      IF (CSNOFG .EQ. 1) THEN
        IF (PRINTU .GT. 0 .AND. PFLAG(3) .LE. LEV) THEN
          WRITE (PRINTU,2060)  PSTAT1,IWST2
        END IF
        IF (BINU .GT. 0 .AND. ABS(BFLAG(3)) .LE. LEV) THEN
C         compile values for direct access printout
          DO 10 I = 1, 2
            ACNT = ACNT + 1
            APRINT(ACNT) = PSTAT1(I)
            CHEAD(ACNT) = CSTAT1(I)
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 10       CONTINUE
          ACNT = ACNT + 1
          APRINT(ACNT) = IWST2
          CHEAD(ACNT) = 'PETADJ'
          CLEN(ACNT) = 6
        END IF
      ELSE
        IF (PRINTU .GT. 0 .AND. PFLAG(3) .LE. LEV) THEN
          WRITE (PRINTU,2060)  PSTAT1
        END IF
        IF (BINU .GT. 0 .AND. ABS(BFLAG(3)) .LE. LEV) THEN
C         compile values for direct access printout
          DO 20 I = 1, 2
            ACNT = ACNT + 1
            APRINT(ACNT) = PSTAT1(I)
            CHEAD(ACNT) = CSTAT1(I)
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 20       CONTINUE
        END IF
      END IF
C
C     fluxes
      IF (SLIFP .GT. 0) THEN
C       lateral inflow is considered
        IF (PRINTU .GT. 0 .AND. PFLAG(3) .LE. LEV) THEN
          WRITE (PRINTU,2070)
          WRITE (PRINTU,2080)
C
          IF (UNITFG .EQ. 1) THEN
            WRITE (PRINTU,2090)
          ELSE
            WRITE (PRINTU,2100)
          END IF
C
          WRITE (PRINTU,2110)  PCFLX1, PIFLX
        END IF
        IF (BINU .GT. 0 .AND. ABS(BFLAG(3)) .LE. LEV) THEN
C         compile values for direct access printout
          DO 30 I = 1, 4
            ACNT = ACNT + 1
            APRINT(ACNT) = PCFLX1(I)
            CHEAD(ACNT) = CCFLX1(I)
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 30       CONTINUE
          ACNT = ACNT + 1
          APRINT(ACNT) = PIFLX
          CHEAD(ACNT) = 'SURLI'
          CLEN(ACNT) = 5
        END IF
      ELSE
C       no lateral inflow considered
        IF (PRINTU .GT. 0 .AND. PFLAG(3) .LE. LEV) THEN
          WRITE (PRINTU,2120)
          WRITE (PRINTU,2130)
C
          IF (UNITFG .EQ. 1) THEN
            WRITE (PRINTU,2140)
          ELSE
            WRITE (PRINTU,2150)
          END IF
C
          WRITE (PRINTU,2110)  PCFLX1
        END IF
        IF (BINU .GT. 0 .AND. ABS(BFLAG(3)) .LE. LEV) THEN
C         compile values for direct access printout
          DO 40 I = 1, 4
            ACNT = ACNT + 1
            APRINT(ACNT) = PCFLX1(I)
            CHEAD(ACNT) = CCFLX1(I)
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 40       CONTINUE
        END IF
      END IF
C
C     water balance check and report
C
      IF (UNITFG .EQ. 1) THEN
C       english
        UNITID= '  INCHES'
      ELSE
C       metric
        UNITID= '      MM'
      END IF
C
      CALL BALCHK
     I           (I2,LSNO,DATIM,MESSU,PRINTU,MSGFL,
     I            PIMPSS,PIMPS,PCFLX2(1),PCFLX2(2),UNITID,I1,
     M            IWWCNT(1))
C
      IF (BINU .GT. 0 .AND. ABS(BFLAG(3)) .LE. LEV) THEN
C       write binary output
        CALL EXDATE(
     I              DATIM,
     O              EXDAT)
        IF (BFLAG(3) .GT. 0) THEN
C         at start of run, write the header
          WRITE (BINU) I0,'IMPLND  ',LSNO,'IWATER  ',
     1          (CLEN(I),(CHEAD(I)(J:J),J=1,CLEN(I)),I=1,ACNT)
C         set bflag to negative to not write headers anymore
          BFLAG(3) = -BFLAG(3)
        END IF
        WRITE (BINU) I1,'IMPLND  ',LSNO,'IWATER  ',UNITFG,
     1               LEV,(EXDAT(I),I=1,5),(APRINT(I),I=1,ACNT)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   IWARST
     I                   (LEV)
C
C     + + + PURPOSE + + +
C     Reset all flux accumulators and those state variables
C     used in material balance check for section IWATER.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    LEV
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEV    - current output level (2-pivl,3-day,4-mon,5-ann)
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION IWATER2 + + +
      INCLUDE    'ciliw.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I2,I4
C
C     + + + EXTERNALS + + +
      EXTERNAL   SETVEC
C
C     + + + END SPECIFICATIONS + + +
C
      I2= 2
      I4= 4
C     handle flux groups containing segment-wide variables
C
C     lateral input fluxes may be printed
      IWIF(LEV)= 0.0
C
      CALL SETVEC
     I           (I4,0.0,
     O            IWCF1(1,LEV))
C
      CALL SETVEC
     I           (I2,0.0,
     O            IWCF2(1,LEV))
C
C     keep present water storage in state variable used for
C     material balance check
      IWST3(LEV)= IWST3(1)
C
      RETURN
      END
C
C
C
      SUBROUTINE   IWATIB
C
C     + + + PURPOSE + + +
C     Handle section IWATER.
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION IWATER2 + + +
      INCLUDE 'ciliw.inc'
      INCLUDE 'cmpad.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      IF (SUPYFP .GE. 1) THEN
        PAD(SUPYFP + IVL1)= SUPY
      END IF
C
      IF (SURIFP .GE. 1) THEN
        PAD(SURIFP + IVL1)= SURI
      END IF
C
      IF (SOFP .GE. 1) THEN
        PAD(SOFP + IVL1)  = SURO
      END IF
C
      IF (PETFP .GE. 1) THEN
        PAD(PETFP + IVL1) = PET
      END IF
C
      IF (IEVFP .GE. 1) THEN
        PAD(IEVFP + IVL1) = IMPEV
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   IWATIP
C
C     + + + PURPOSE + + +
C     Handle section IWATER.
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION IWATER2 + + +
      INCLUDE 'ciliw.inc'
      INCLUDE 'cmpad.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      IF (RETSFP .GE. 1) THEN
        PAD(RETSFP + IVL1)= RETS
      END IF
C
      IF (SSFP .GE. 1) THEN
        PAD(SSFP + IVL1)  = SURS
      END IF
C
      IF (IMPSFP .GE. 1) THEN
        PAD(IMPSFP + IVL1)= IMPS
      END IF
C
      IF (PETAFP .GE. 1) THEN
        PAD(PETAFP + IVL1)= PETADJ
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   RETN
     I                 (VRSFG,DAYFG,RETSCM,RETI,MON,NXTMON,DAY,NDAYS,
     M                  RETSC,RETS,
     O                  RETO)
C
C     + + + PURPOSE + + +
C     Simulate the retention of moisture by the impervious area.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    DAY,DAYFG,MON,NDAYS,NXTMON,VRSFG
      REAL       RETI,RETO,RETS,RETSC,RETSCM(12)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     VRSFG  - ???
C     DAYFG  - flag for first day or day change
C     RETSCM - ???
C     RETI   - ???
C     MON    - calendar month
C     NXTMON - next calendar month
C     DAY    - day of month
C     NDAYS  - no. of days in this month
C     RETSC  - ???
C     RETS   - ???
C     RETO   - ???
C
C     + + + FUNCTIONS + + +
      REAL       DAYVAL
C
C     + + + EXTERNALS + + +
      EXTERNAL   DAYVAL
C
C     + + + END SPECIFICATIONS + + +
C
      IF (DAYFG .EQ. 1) THEN
C       it is the first interval of the day
        IF (VRSFG .EQ. 1) THEN
C         retention capacity is allowed to vary throughout the year
C         interpolate for the daily value
C         linearly interpolate retsc between two values from the
C         monthly array retscm(12)
          RETSC= DAYVAL(RETSCM(MON),RETSCM(NXTMON),DAY,NDAYS)
        ELSE
C         retention capacity does not vary throughout the year
C         retsc value has been supplied by the run interpreter
        END IF
      END IF
C
C     add to retention storage
      RETS= RETS + RETI
      IF (RETS .GT. RETSC) THEN
C       there is outflow from retention storage
        RETO= RETS - RETSC
        RETS= RETSC
      ELSE
        RETO= 0.0
      END IF
C
      RETURN
      END
