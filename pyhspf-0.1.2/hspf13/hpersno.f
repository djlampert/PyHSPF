C
C
C
      SUBROUTINE   PSNOW
C
C     + + + PURPOSE + + +
C     Process input for snow section of module perlnd or implnd
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION SNOW1 + + +
      INCLUDE    'cplsn.inc'
      INCLUDE    'crin2.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   IVAL(2),SCLU,SGRP,TBNO,TBSB,NVAL,N
      REAL      RVAL(6)
C
C     + + + EXTERNALS + + +
      EXTERNAL  ITABLE,OMSG,RTABLE,NOPACK,ALBED
C
C     + + + INTRINSICS + + +
      INTRINSIC  ABS
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' PROCESSING INPUT FOR SECTION SNOW')
 2030 FORMAT (/,' FINISHED PROCESSING INPUT FOR SECTION SNOW')
C
C     + + + END SPECIFICATIONS + + +
C
      IF (OUTLEV.GT.1) THEN
C       processing message
        WRITE (MESSU,2000)
      END IF
C     error/warn message cluster
      SCLU= 302
C
C     warning message counter initialization
      SNWCNT(1)= 0
C     error message counter initialization
      SNECNT(1)= 0
      SNECNT(2)= 0
C
C     svp of water
      SVP(1) = 1.005
      SVP(2) = 1.005
      SVP(3) = 1.005
      SVP(4) = 1.005
      SVP(5) = 1.005
      SVP(6) = 1.005
      SVP(7) = 1.005
      SVP(8) = 1.005
      SVP(9) = 1.005
      SVP(10)= 1.005
      SVP(11)= 1.01
      SVP(12)= 1.01
      SVP(13)= 1.015
      SVP(14)= 1.02
      SVP(15)= 1.03
      SVP(16)= 1.04
      SVP(17)= 1.06
      SVP(18)= 1.08
      SVP(19)= 1.1
      SVP(20)= 1.29
      SVP(21)= 1.66
      SVP(22)= 2.13
      SVP(23)= 2.74
      SVP(24)= 3.49
      SVP(25)= 4.40
      SVP(26)= 5.55
      SVP(27)= 6.87
      SVP(28)= 8.36
      SVP(29)= 10.1
      SVP(30)= 12.2
      SVP(31)= 14.6
      SVP(32)= 17.5
      SVP(33)= 20.9
      SVP(34)= 24.8
      SVP(35)= 29.3
      SVP(36)= 34.6
      SVP(37)= 40.7
      SVP(38)= 47.7
      SVP(39)= 55.7
      SVP(40)= 64.9
C
C     initialize other variables
      DRYFG= 1
C
      IF (HR.GE.7) THEN
        HR6FG= 0
      ELSE
        HR6FG= 1
      END IF
C
C     process values in table - type iceflag
      TBNO= 6
      TBSB= 1
      NVAL= 1
      CALL ITABLE(TBNO,TBSB,NVAL,UUNITS,
     M            IVAL)
C
      ICEFG= IVAL(1)
C
      IF (ICEFG.EQ.1 .AND .DELT.GT.360) THEN
C       error - snow simulation cannot function properly with delt> 360
C               if ice flag is on
        SGRP= 3
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             ECOUNT)
      END IF
C
C     process values in table - type snow-flags
      TBNO= 7
      TBSB= 1
      NVAL= 2
      CALL ITABLE(TBNO,TBSB,NVAL,UUNITS,
     M            IVAL)
      SNOPFG= IVAL(1)
      VKMFG= IVAL(2)
C
C     process values in table - type snow-parm1
      TBNO= 8
      TBSB= 1
      NVAL= 7
      CALL RTABLE(TBNO,TBSB,NVAL,UUNITS,
     M            SNPM1)
      KMELT= KMELT*DELT/1440.
C
C     process values in table-type snow-parm2
      TBNO= 9
      TBSB= 1
      NVAL= 6
      CALL RTABLE (TBNO,TBSB,NVAL,UUNITS,
     M             SNPM2)
C
C     convert to internal units
      MGMELT= MGMELT*DELT/1440.
C
      IF (VKMFG .EQ. 1) THEN
C       get monthly values of kmelt - table-type mon-melt-fac
        TBNO= 10
        TBSB= 1
        NVAL= 12
        CALL RTABLE (TBNO,TBSB,NVAL,UUNITS,
     M               KMELTM)
        DO 10 N= 1, 12
          KMELTM(N)= KMELTM(N)*DELT/1440.
 10     CONTINUE
      END IF
C
C     process values in table-type snow-init1
      TBNO= 11
      TBSB= 1
      NVAL= 6
      CALL RTABLE (TBNO,TBSB,NVAL,UUNITS,
     M             RVAL)
C
      PACKF = (RVAL(1)+ RVAL(2))
      PACKI = RVAL(2)
      PACKW = RVAL(3)
      RDENPF= RVAL(4)
      DULL  = RVAL(5)
      PAKTMP= RVAL(6)
      PACK  = PACKF+ PACKW
C
C     process values in snow-init2
      TBNO= 12
      TBSB= 1
      NVAL= 3
      CALL RTABLE(TBNO,TBSB,NVAL,UUNITS,
     M            RVAL)
C
      COVINX= RVAL(1)
      XLNMLT= RVAL(2)
      SKYCLR= RVAL(3)
C
      IF ((ABS(PACK)).LE.1.0E-5) THEN
C       set state variables to values for no pack
        CALL NOPACK (SNOPFG,COVIND,
     O               HR6FG,PACKF,PACKI,PACKW,PACK,PDEPTH,RDENPF,COVINX,
     O               SNOCOV,DULL,ALBEDO,XLNMLT,MNEGHS,PAKTMP,
     O               NEGHTS,PACKWC,NEGHT,GMELTR,COMPCT,SNOWEP,
     O               MOSTHT,VAP)
      ELSE
C       if (covinx.eq.0.0) covinx= 0.1*covind
        IF ((ABS(COVINX)).LE.1.0E-5) THEN
          COVINX= 0.1*COVIND
        END IF
C       calculate derived variables
        PDEPTH= PACKF/RDENPF
        IF (PACKF.LT.COVINX) THEN
          SNOCOV= PACKF/COVINX
        ELSE
          SNOCOV= 1.0
        END IF
C
        IF (SNOPFG .EQ. 0) THEN
C         initialize albedo
          CALL ALBED (MON,LAT,DULL,
     O                ALBEDO)
        END IF
C
C       initialize neghts based on initial pack temperature and storage
        NEGHTS= (32.0 - PAKTMP)*0.00695*PACKF
      END IF
C
C     initialize snotmp - this will remain a constant if snopfg=1
      SNOTMP= TSNOW
C
      IF (SNOPFG .EQ. 1) THEN
C       set dummy values for unused variables
        DULL  = -1.0E30
        ALBEDO= -1.0E30
        SNOWEP= -1.0E30
        VAP   = -1.0E30
        SKYCLR= -1.0E30
C       also dummy dewtemp in case not used
        DEWTMP= -1.0E30
      END IF
C
      IF (OUTLEV.GT.1) THEN
C       finished processing message
        WRITE (MESSU,2030)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   SNOW
     I                   (OPTYP)
C
C     + + + PURPOSE + + +
C     Simulate the accumulation and melting of snow and ice,
C     and retention and release of liquid water.
C
C     + + + DUMMY ARGUMENTS + + +
      CHARACTER*6 OPTYP
C
C     + + + ARGUMENT DEFINITIONS + + +
C     OPTYP  - operation type ("PERLND" or "IMPLND")
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION SNOW2 + + +
      INCLUDE    'cplsn.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     ECNT1,SGRP,SCLU,REQFG,TSSUB(2),FLGVAL
      REAL        RNFRZ,RNSHT,SUMHT
      CHARACTER*6 TSNAM,SECNAM,MSECNM,OPFGNM
C
C     + + + INTRINSICS + + +
      INTRINSIC ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL  HREQTS,METEOR,EFFPRC,COMPAC,SNOWEV,HEXCHR,COOLER,
     $          WARMUP,MELTER,LIQUID,ICING,GMELT,DEGDAY,
     $          NOPACK,OMSG,OMSTR,OMSTD,OMSTI
C
C     + + + DATA INITIALIZATIONS + + +
      DATA TSSUB/1,1/
      DATA SECNAM,MSECNM/'SNOW  ','ATEMP '/
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU= 302
C     get input time series
Cthj      PREC= PAD(PRECFP + IVL1)
      REQFG= 2
      TSNAM= 'PREC  '
      CALL HREQTS (PRECFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,LSNO,
     I             TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,FLGVAL,
     O             PREC)
C
      IF (AIRTFG .EQ. 0) THEN
C       if air temperature is an input series, not from atemp
Cthj        AIRTMP= PAD(AIRTFP + IVL1)
        REQFG= 3
        TSNAM= 'AIRTMP'
        CALL HREQTS (AIRTFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,LSNO,
     I               TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,FLGVAL,
     O               AIRTMP)
      END IF
C
      IF (SNOPFG .EQ. 0) THEN
C       dewpoint is input - required for energy balance
Cthj        DTMPG= PAD(DTGFP + IVL1)
        REQFG= 4
        TSNAM= 'DTMPG '
        OPFGNM= 'SNOPFG'
        FLGVAL= 0
        CALL HREQTS (DTGFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,LSNO,
     I               TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,FLGVAL,
     O               DTMPG)
      ELSE IF (DTGFP .GE. 1) THEN
C       dewpoint is input - optional for degree-day
        DTMPG= PAD(DTGFP + IVL1)
      ELSE
C       dewpoint is not input - use dummy value
        DTMPG= -999.0
      END IF
C
      IF (SNOPFG .EQ. 0) THEN
C       wind and solar radiation are required for energy balance
Cthj        WINMOV= PAD(WINMFP + IVL1)
Cthj        SOLRAD= PAD(SOLRFP + IVL1)
        REQFG= 4
        OPFGNM= 'SNOPFG'
        FLGVAL= 0
        TSNAM= 'WINMOV'
        CALL HREQTS (WINMFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,LSNO,
     I               TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,FLGVAL,
     O               WINMOV)
        TSNAM= 'SOLRAD'
        CALL HREQTS (SOLRFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,LSNO,
     I               TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,FLGVAL,
     O               SOLRAD)
      END IF
C
C     estimate meteorological conditions
C
      CALL METEOR
     I           (SNOPFG,PREC,HRFG,AIRTMP,TSNOW,DTMPG,SNOWCF,RDCSN,
     I            DELT,CCFP,IVL1,
     M            SNOTMP,DRYFG,SKYCLR,
     O            DEWTMP,SNOFFG,SNOWF,RAINFG,RAINF,RDNSN)
C
C     do the pack simulation if a pack already exists
C     or if it is snowing
C
      IF (PACKF .GT. 0.0 .OR. SNOFFG .EQ. 1) THEN
C       there is a snowpack or it is snowing,
C       simulate snow accumulation and melt
        IF ((ABS(PACKF)) .LE. 0.0) THEN
C         a pack is just starting to accumulate
C         initialize variables
C         iregfg is set on to trigger intermittent calculations
          IREGFG= 1
          IF (SNOPFG .EQ. 0) THEN
C           dull was previously undefined
            DULL  = 0.0
          END IF
        ELSE
C         iregfg governs intermittently performed calculations
          IF (HRFG .NE. 0) THEN
            IREGFG= 1
          ELSE
            IREGFG= 0
          END IF
        END IF
C
C       start the calculations by simulating the effect of precipitation
C       on the pack
C
        CALL EFFPRC
     I             (SNOPFG,SNOFFG,SNOWF,RAINFG,RAINF,RDNSN,COVIND,
     I              DELT60,SNOCOV,
     M              COVINX,PACKF,PDEPTH,DULL,
     O              PRAIN)
C
C       simulate compaction of the pack
C
        CALL COMPAC
     I             (IREGFG,DELT60,PACKF,
     M              PDEPTH,
     O              COMPCT)
C
        ECNT1= SNECNT(1)
C
        IF (SNOPFG .EQ. 0) THEN
C         estimate vapor pressures and rate of evaporation from snow
C
          CALL SNOWEV
     I               (IREGFG,SVP,DEWTMP,AIRTMP,SNOCOV,SNOEVP,
     I                WINMOV,LSNO,MESSU,MSGFL,DATIM,
     M                SNOWEP,PACKF,PDEPTH,PACKI,ECNT1,
     O                SNOWE,VAP)
        ELSE
C         evaporation subsumed under other parameters
          SNOWE= 0.0
        END IF
C
C       estimate the rate of heat exchange, except for contributions
C       from groundmelt and rain  - this is done intermittently
C
        IF (IREGFG .NE. 0) THEN
          IF (SNOPFG .EQ. 0) THEN
C           energy balance approach
            CALL HEXCHR
     I                 (CCFACT,WINMOV,VAP,AIRTMP,MELEV,MON,LAT,DULL,
     I                  SOLRAD,SHADE,DELT60,SKYCLR,
     O                  MOSTHT,ALBEDO)
          ELSE
C           degree-day approach
            CALL DEGDAY (AIRTMP,TBASE,DAYFG,VKMFG,KMELTM,MON,DAY,NXTMON,
     I                   NDAYS,
     M                   KMELT,
     O                   MOSTHT)
          END IF
        END IF
C
C       find sensible heat available from rain -- done
C       every interval
C
        IF (RAINF .GT. 0.0) THEN
          RNSHT= (AIRTMP - 32.0) * RAINF / 144.0
        ELSE
          RNSHT= 0.0
        END IF
C
C       sum the heat components calculated so far and correct for
C       incomplete areal snowcover, if necessary - units are
C       inches of water with equivalent latent heat/ivl -- done
C       every interval
C
        SUMHT= MOSTHT + RNSHT
        IF (SNOCOV .LT. 1.0)  THEN
          SUMHT= SUMHT*SNOCOV
        END IF
C
C       simulate the heating or cooling of the pack
C
C       find the current temperature of the pack (paktmp)
C
        IF ((ABS(NEGHTS)) .LE. 0.0) THEN
          PAKTMP= 32.0
        ELSE
          PAKTMP= 32.0 - NEGHTS / (0.00695 * PACKF)
        END IF
C
C       when heat transfer (sumht) is negative,
C       simulate loss of heat from the pack
C
        CALL COOLER
     I             (IREGFG,AIRTMP,PACKF,PAKTMP,DELT60,
     M              SUMHT,MNEGHS,NEGHT,NEGHTS)
C
C       when sumht is positive, use some or all of it and
C       latent heat of rain to warm the pack towards 32 deg f
        IF (NEGHTS .GT. 0.0) THEN
C         there is negative heat storage in the pack,
C         so it can be warmed up
          CALL WARMUP
     I               (PRAIN,
     M                SUMHT,NEGHTS,PDEPTH,PACKF,
     O                RNFRZ)
        ELSE
C         pack cannot be warmed up, so no rain can freeze
          RNFRZ= 0.0
        END IF
C
C       when there is still unused heat, melt some of the pack
        CALL MELTER
     I             (SUMHT,
     M              PACKF,PACKI,PDEPTH,
     O              MELT)
C
C       handle liquid water
C
        CALL LIQUID
     I             (IREGFG,PDEPTH,MWATER,PACKF,MELT,
     I              PRAIN,RNFRZ,DELT60,
     M              PACKW,PACKWC,
     O              WYIELD)
C
        IF (ICEFG .NE. 0) THEN
C         simulate the effects of frozen ground
          CALL ICING
     I              (HR,SNOCOV,AIRTMP,
     M               HR6FG,XLNMLT,WYIELD,PACKF,PACKI,PDEPTH)
        END IF
C
C       the component of ice in the pack cannot exceed the
C       total frozen contents
C
        IF (PACKI .GT. PACKF) THEN
C         this condition should never occur.  after the code
C         has shaken down, remove the test from the module
C         ice content of pack exceeds total content of pack.
          CALL OMSTD (DATIM)
          CALL OMSTI (LSNO)
          CALL OMSTR (PACKF)
          CALL OMSTR (PACKI)
          SGRP= 1
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               SNECNT(2))
        END IF
C
C       simulate ground melt
C
        CALL GMELT
     I            (IREGFG,PAKTMP,MGMELT,
     M             PACKF,WYIELD,PACKW,PDEPTH,PACKI,
     M             GMELTR,NEGHTS)
C
C       final exercises
C
        IF (PACKF .GT. 0.005) THEN
C         total storage in the pack
          PACK= PACKF+ PACKW
C         find relative density of the pack
          RDENPF= PACKF/PDEPTH
C
          IF ((ABS(NEGHTS)) .LE. 0.0) THEN
            PAKTMP= 32.0
          ELSE
            PAKTMP= 32.0 - NEGHTS/(.00695*PACKF)
          END IF
C
          IF (PACKF .LT. COVINX) THEN
            SNOCOV= PACKF/COVINX
          ELSE
            SNOCOV= 1.0
          END IF
        ELSE
C         assume pack is gone.
          MELT  = MELT+ PACKF
          WYIELD= WYIELD+ PACKF+ PACKW
          CALL NOPACK (SNOPFG,COVIND,
     O                 HR6FG,PACKF,PACKI,PACKW,PACK,PDEPTH,RDENPF,
     O                 COVINX,SNOCOV,DULL,ALBEDO,XLNMLT,MNEGHS,PAKTMP,
     O                 NEGHTS,PACKWC,NEGHT,GMELTR,COMPCT,SNOWEP,
     O                 MOSTHT,VAP)
        END IF
      ELSE
C       there is no snow or pack
        PRAIN = 0.0
        SNOWE = 0.0
        WYIELD= 0.0
        MELT  = 0.0
      END IF
C
C     total moisture supplied to the pack
      PAKIN = SNOWF + PRAIN
C
C     net moisture change in the pack
      PAKDIF= PAKIN - (SNOWE + WYIELD)
C
      RETURN
      END
C
C
C
      SUBROUTINE   COMPAC
     I                 (IREGFG,DELT60,PACKF,
     M                  PDEPTH,
     O                  COMPCT)
C
C     + + + PURPOSE + + +
C     Simulate compaction of the pack.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   IREGFG
      REAL      COMPCT,DELT60,PACKF,PDEPTH
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IREGFG - irreqular calc needed flag
C     DELT60 - simulation time interval in hours
C     PACKF  - content of snowpack
C     PDEPTH - ???
C     COMPCT - ???
C
C     + + + LOCAL VARIABLES + + +
      REAL      RDENPF
C
C     + + + END SPECIFICATIONS + + +
C
C     recalculation of the rate of compaction is done each hour,
C     as long as a pack exists
      IF (IREGFG .EQ. 1) THEN
C       find rate of change of depth
        RDENPF= PACKF / PDEPTH
        IF (RDENPF .LT. 0.55) THEN
          COMPCT= 1.0 - (0.00002 * DELT60 * PDEPTH * (0.55 - RDENPF))
        ELSE
          COMPCT= 1.0
        END IF
      END IF
C
C     compact the pack - done each interval
      IF (COMPCT .LT. 1.0) THEN
        PDEPTH= PDEPTH * COMPCT
      ELSE
C       no further compaction
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   COOLER
     I                   (IREGFG,AIRTMP,PACKF,PAKTMP,DELT60,
     M                    SUMHT,MNEGHS,NEGHT,NEGHTS)
C
C     + + + PURPOSE + + +
C     Simulate loss of heat from the pack.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   IREGFG
      REAL      AIRTMP,DELT60,MNEGHS,NEGHT,NEGHTS,PACKF,PAKTMP,SUMHT
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IREGFG - irreqular calc needed flag
C     AIRTMP - ???
C     PACKF  - content of snowpack
C     PAKTMP - ???
C     DELT60 - simulation time interval in hours
C     SUMHT  - ???
C     MNEGHS - ???
C     NEGHT  - ???
C     NEGHTS - ???
C
C     + + + LOCAL VARIABLES + + +
      REAL      RELTMP
C
C     + + + END SPECIFICATIONS + + +
C
C     set maximum negative heat storage - done irregularly
C     units in inches of water with equiv latent heat
      IF (IREGFG .EQ. 1) THEN
        RELTMP= AIRTMP - 32.0
        IF (RELTMP .GT. 0.0) THEN
          MNEGHS= 0.0
        ELSE
          MNEGHS= 0.00695 * (PACKF / 2.0) * (-RELTMP)
        END IF
C
C       find rate of increase of negative heat storage - in/ivl
        IF (PAKTMP .GT. AIRTMP) THEN
          NEGHT= 0.0007 * (PAKTMP - AIRTMP) * DELT60
        ELSE
          NEGHT= 0.0
       END IF
      END IF
C
C     cool the pack if it is warmer than the air -
C     done every interval
      IF (SUMHT .LT. 0.0) THEN
        IF (PAKTMP .GT. AIRTMP) THEN
          NEGHTS= NEGHTS + NEGHT
          IF (NEGHTS .GT. MNEGHS) THEN
            NEGHTS= MNEGHS
          END IF
        END IF
C
C       assume heat loss has been accounted for. sumht is
C       set to zero to ensure that later subroutines function
C       correctly
        SUMHT= 0.0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   EFFPRC
     I                   (SNOPFG,SNOFFG,SNOWF,RAINFG,RAINF,RDNSN,
     I                    COVIND,DELT60,SNOCOV,
     M                    COVINX,PACKF,PDEPTH,DULL,
     O                    PRAIN)
C
C     + + + PURPOSE + + +
C     Determine the effect of precipitation on the pack.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   SNOPFG,RAINFG,SNOFFG
      REAL      COVIND,COVINX,DELT60,DULL,PACKF,
     $          PDEPTH,PRAIN,RAINF,RDNSN,SNOCOV,SNOWF
C
C     + + + ARGUMENT DEFINITIONS + + +
C     SNOPFG - flag indicating whether to use energy balance (0) or
C              degree-days (1)
C     SNOFFG - snowing flag - 1:yes
C     SNOWF  - ???
C     RAINFG - raining flag - 1:yes
C     RAINF  - ???
C     RDNSN  - ???
C     COVIND - ???
C     DELT60 - simulation time interval in hours
C     SNOCOV - ???
C     COVINX - ???
C     PACKF  - content of snowpack
C     PDEPTH - ???
C     DULL   - ???
C     PRAIN  - ???
C
C     + + + LOCAL VARIABLES + + +
      REAL      DUMMY
C
C     + + + END SPECIFICATIONS + + +
C
      IF (SNOFFG .EQ. 1) THEN
C       it is snowing - add to the pack and find new depth
        PACKF = PACKF + SNOWF
        PDEPTH= PDEPTH + SNOWF / RDNSN
C       adjust the current value of packf required to ensure
C       complete areal cover (covinx)
        IF (PACKF .GT. COVINX) THEN
          IF (PACKF .GT. COVIND) THEN
C           packf is more than the maximum needed to ensure
C           complete snow cover of the land segment
            COVINX= COVIND
          ELSE
            COVINX= PACKF
          END IF
        END IF
C
        IF (SNOPFG .EQ. 0) THEN
C         adjust the dullness index for snowpack -
C         used later to estimate albedo
          DUMMY= 1000.0 * SNOWF
          IF (DUMMY .GE. DULL) THEN
            DULL= 0.0
          ELSE
            DULL= DULL - DUMMY
          END IF
        END IF
C
C       rain entering pack
        PRAIN= 0.0
      ELSE
C       it is not snowing
        IF (RAINFG .EQ. 1) THEN
C         it is raining - find out how much rain actually
C         enters the pack
          PRAIN= RAINF * SNOCOV
        ELSE
          PRAIN= 0.0
        END IF
C
      END IF
C
      IF (SNOPFG .EQ. 0) THEN
C       augment the dullness index
        IF (DULL .LT. 800.0) THEN
C         surface can get duller
          DULL= DULL + DELT60
        ELSE
C         no further dulling of the pack
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   GMELT
     I                  (IREGFG,PAKTMP,MGMELT,
     M                   PACKF,WYIELD,PACKW,PDEPTH,PACKI,GMELTR,NEGHTS)
C
C     + + + PURPOSE + + +
C     Melt the pack using heat from the ground.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   IREGFG
      REAL      GMELTR,MGMELT,NEGHTS,PACKF,PACKI,PACKW,PAKTMP,
     $          PDEPTH,WYIELD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IREGFG - irreqular calc needed flag
C     PAKTMP - ???
C     MGMELT - ???
C     PACKF  - content of snowpack
C     WYIELD - ???
C     PACKW  - ???
C     PDEPTH - ???
C     PACKI  - ???
C     GMELTR - ???
C     NEGHTS - ???
C
C     + + + LOCAL VARIABLES + + +
      REAL       DUMMY
C
C     + + + END SPECIFICATIONS + + +
C
      IF (IREGFG .EQ. 1) THEN
C       estimate the capacity for ground melt - depends on the
C       temperature of the pack
        IF (PAKTMP .GE. 32.0) THEN
          GMELTR= MGMELT
        ELSE
          IF (PAKTMP .GT. 5.0) THEN
            GMELTR= MGMELT * (1.0 - 0.03 * (32.0 - PAKTMP))
          ELSE
C           ground melt is at its minimum value if paktmp is <= 5 deg f
            GMELTR= 0.19 * MGMELT
          END IF
        END IF
      ELSE
C       ground melt rate is unchanged
      END IF
C
C     melt the pack
      IF (PACKF .LE. GMELTR) THEN
C       the whole pack will melt
        WYIELD= WYIELD + PACKF + PACKW
        PACKF = 0.0
        PACKI = 0.0
        PACKW = 0.0
        PDEPTH= 0.0
        NEGHTS= 0.0
      ELSE
C       part of the pack will melt
        PACKW = PACKW + GMELTR
C       update depth, negative heat storage, and water equivalent
C       of the frozen pack to account for ground melt
        DUMMY = 1.0 - GMELTR / PACKF
        PDEPTH= PDEPTH * DUMMY
        NEGHTS= NEGHTS * DUMMY
        PACKF = PACKF - GMELTR
C       ground melt takes ice storage first
        IF (PACKI .GT. GMELTR) THEN
          PACKI= PACKI - GMELTR
        ELSE
          PACKI= 0.0
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   HEXCHR
     I                   (CCFACT,WINMOV,VAP,AIRTMP,MELEV,MON,LAT,DULL,
     I                    SOLRAD,SHADE,DELT60,SKYCLR,
     O                    MOSTHT,ALBEDO)
C
C     + + + PURPOSE + + +
C     Estimate rate of heat exchange (except components supplied
C     by ground melt and rain heat).  Units are inches of water
C     with equivalent latent heat/ivl.  At this stage we act as though
C     snow covers the entire segment.  Correction for areal
C     extent of snow cover is done later.  These calculations are
C     done intermittently.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MON
      REAL      AIRTMP,ALBEDO,CCFACT,DELT60,DULL,LAT,MELEV,MOSTHT,
     $          SHADE,SKYCLR,SOLRAD,VAP,WINMOV
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CCFACT - ???
C     WINMOV - ???
C     VAP    - ???
C     AIRTMP - ???
C     MELEV  - ???
C     MON    - calendar month
C     LAT    - ???
C     DULL   - ???
C     SOLRAD - ???
C     SHADE  - ???
C     DELT60 - simulation time interval in hours
C     SKYCLR - ???
C     MOSTHT - ???
C     ALBEDO - ???
C
C     + + + LOCAL VARIABLES + + +
      REAL      CONDHT,CONVHT,DUMMY,FACTR,LONG,RADHT,RELTMP,SHORT
C
C     + + + EXTERNALS + + +
      EXTERNAL ALBED
C
C     + + + END SPECIFICATIONS + + +
C
C     Calculate factor used below, from wind data and from
C     adjustment parameter for field conditions
      FACTR= CCFACT * 0.00026 * WINMOV
C
C     heat supplied by condensation
      IF (VAP .GT. 6.108) THEN
C       air is moist enough to condense
        DUMMY = 8.59 * (VAP - 6.108)
        CONDHT= DUMMY * FACTR
      ELSE
        CONDHT= 0.0
      END IF
C
C     heat supplied by convection
      IF (AIRTMP .GT. 32.0) THEN
C       convection heating can occur
        DUMMY = (AIRTMP - 32.0) * (1.0 - 0.3 * MELEV / 10000.0)
        CONVHT= DUMMY * FACTR
      ELSE
        CONVHT= 0.0
      END IF
C
C     find albedo based on dullness index
      CALL ALBED (MON,LAT,DULL,
     O            ALBEDO)
C
C     heat supplied by shortwave radiation
C     estimate shortwave energy - langleys/ivl
      SHORT = SOLRAD * (1.0 - ALBEDO) * (1.0 - SHADE)
C
C     estimate longwave energy - langleys/ivl
      RELTMP= AIRTMP - 32.0
      IF (RELTMP .GT. 0.0) THEN
        LONG= (SHADE * 0.26 * RELTMP + (1.0 - SHADE)
     $          * (0.2 * RELTMP - 6.6)) * DELT60
      ELSE
        LONG= (SHADE * 0.20 * RELTMP + (1.0 - SHADE)
     $          * (0.17 * RELTMP - 6.6)) * DELT60
      END IF
C
C     reduce longwave radiation to account for back radiation
C     from clouds, if net heat transfer was outwards from pack
      IF (LONG .LT. 0.0) THEN
        LONG= LONG * SKYCLR
      END IF
C
C     find total radiative heat transfer and convert to units:
C     inches water equiv latent heat
      RADHT = (SHORT + LONG) / 203.2
C     mostht is the total heat available, except for
C     rain and ground heat
      MOSTHT= RADHT + CONVHT + CONDHT
C
      RETURN
      END
C
C
C
      SUBROUTINE   ICING
     I                  (HR,SNOCOV,AIRTMP,
     M                   HR6FG,XLNMLT,WYIELD,PACKF,PACKI,PDEPTH)
C
C     + + + PURPOSE + + +
C     Simulate the occurrence of frozen ground - ice (PACKI) is regarded
C     as part of the pack.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   HR,HR6FG
      REAL      AIRTMP,PACKF,PACKI,PDEPTH,SNOCOV,WYIELD,XLNMLT
C
C     + + + ARGUMENT DEFINITIONS + + +
C     HR     - ???
C     SNOCOV - ???
C     AIRTMP - ???
C     HR6FG  - ???
C     XLNMLT - ???
C     WYIELD - ???
C     PACKF  - content of snowpack
C     PACKI  - ???
C     PDEPTH - ???
C
C     + + + LOCAL VARIABLES + + +
      REAL      FREEZE,RELTMP,XLNEM
C
C     + + + END SPECIFICATIONS + + +
C
C     once per day, at approx 6 AM, update the possible increment
C     to frozen ground
      IF (HR .GE. 7) THEN
        IF (HR6FG .NE. 0) THEN
C         update the possible increment
          IF (SNOCOV .LT. 1.0) THEN
C           land segment is not completely covered, so
C           the water freezing capacity can increase
            RELTMP= AIRTMP - 32.0
C           compute new potential increment to ice storage
            XLNEM= -RELTMP * 0.01
C
C           use the larger value --  only a positive value will
C           mean that there is capacity
            IF (XLNEM .GT. XLNMLT) THEN
              XLNMLT= XLNEM
            END IF
          ELSE
C           completely covered ground will keep the current value
C           for freezing capacity - no further increment
          END IF
C
          HR6FG= 0
        END IF
      ELSE
C       set the flag so that the freezing capacity will be updated
C       next time 6 am is passed
        HR6FG= 1
      END IF
C
C     check whether any water yielded by the pack is to be frozen
C     in this interval
      IF (WYIELD .GT. 0.0 .AND. XLNMLT .GT. 0.0) THEN
C       water is available and there is some remaining freezing
C       capacity
        IF (WYIELD .LT. XLNMLT) THEN
C         all water available will be frozen
          FREEZE= WYIELD
C         reduce remaining freezing capacity
          XLNMLT= XLNMLT - WYIELD
          WYIELD= 0.0
        ELSE
C         some of the yield will be frozen
          FREEZE= XLNMLT
          WYIELD= WYIELD - XLNMLT
C         freezing capacity is exhausted
          XLNMLT= 0.0
        END IF
C
C       update the pack
        PACKF= PACKF + FREEZE
        PACKI= PACKI + FREEZE
C       assume ice has relative density= 1
        PDEPTH= PDEPTH + FREEZE
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   LIQUID
     I                   (IREGFG,PDEPTH,MWATER,PACKF,MELT,PRAIN,RNFRZ,
     M                    DELT60,PACKW,PACKWC,
     O                    WYIELD)
C
C     + + + PURPOSE + + +
C     Handle liquid water in the pack.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   IREGFG
      REAL      MELT,MWATER,PACKF,PACKW,PACKWC,PDEPTH,PRAIN,RNFRZ,
     $          WYIELD,DELT60
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IREGFG - irreqular calc needed flag
C     PDEPTH - ???
C     MWATER - ???
C     PACKF  - content of snowpack
C     MELT   - ???
C     PRAIN  - ???
C     RNFRZ  - ???
C     DELT60 - simulation time interval in hours
C     PACKW  - ???
C     PACKWC - ???
C     WYIELD - ???
C
C     + + + LOCAL VARIABLES + + +
      REAL      DUMMY,MPWS,PWSUPY,RDENPF
C
C     + + + END SPECIFICATIONS + + +
C
      IF (IREGFG .EQ. 1) THEN
C       compute the liquid water holding capacity (packwc) of the
C       pack - units are in inches of water equivalent
        IF (PACKF .GT. 0.0) THEN
C         first find the relative density of the
C         frozen contents of the pack
          RDENPF= PACKF / PDEPTH
          IF (RDENPF .LE. 0.6) THEN
C           liquid water holding capacity is at its maximum
            PACKWC= MWATER
          ELSE
C           determine the less than maximum water holding capacity
            DUMMY= 3.0 - 3.33 * RDENPF
            IF (DUMMY .GE. 0.0) THEN
              PACKWC= MWATER * DUMMY
            ELSE
              PACKWC= 0.0
            END IF
          END IF
        END IF
      END IF
C
C     compare available liquid water with storage capacity of pack
C
C     find the available water
      PWSUPY= PACKW + MELT + PRAIN - RNFRZ
C     compute the storage capacity - units are inches
      MPWS  = PACKWC * PACKF
      IF ((PWSUPY - MPWS) .GT. (0.01 * DELT60)) THEN
C       supply exceeds storage capacity by more than tolerance, so
C       water is yielded and the liquid water is held at capacity
        WYIELD= PWSUPY - MPWS
        PACKW = MPWS
      ELSE
C       liquid water supply does not exceed capacity by more than
C       tolerance, so there is no moisture leaving the pack
        PACKW = PWSUPY
        WYIELD= 0.0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   MELTER
     I                   (SUMHT,
     M                    PACKF,PACKI,PDEPTH,
     O                    MELT)
C
C     + + + PURPOSE + + +
C     Use any remainder of SUMHT to melt the pack.
C
C     + + + DUMMY ARGUMENTS + + +
      REAL       MELT,PACKF,PACKI,PDEPTH,SUMHT
C
C     + + + ARGUMENT DEFINITIONS + + +
C     SUMHT  - ???
C     PACKF  - content of snowpack
C     PACKI  - ???
C     PDEPTH - ???
C     MELT   - ???
C
C     + + + END SPECIFICATIONS + + +
C
      IF (SUMHT .GE. PACKF) THEN
C       there is enough to melt the whole pack
        MELT  = PACKF
        PACKF = 0.0
        PDEPTH= 0.0
        PACKI = 0.0
      ELSE
        IF (SUMHT .GT. 0.0) THEN
C         can melt part of the pack
          MELT  = SUMHT
          PDEPTH= PDEPTH * (1.0 - MELT / PACKF)
          PACKF = PACKF - MELT
C         ice is melted after snow has gone
          IF (PACKI .GT. PACKF) THEN
            PACKI= PACKF
          END IF
        ELSE
C         no melting
          MELT= 0.0
        END IF
C
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   METEOR
     I                 (SNOPFG,PREC,HRFG,AIRTMP,TSNOW,DTMPG,SNOWCF,
     I                  RDCSN,DELT,CCFP,IVL1,
     M                  SNOTMP,DRYFG,SKYCLR,
     O                  DEWTMP,SNOFFG,SNOWF,RAINFG,RAINF,RDNSN)
C
C     + + + PURPOSE + + +
C     Estimate meteorological conditions.
C     air temp has been done in module section atemp.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   SNOPFG,DRYFG,HRFG,RAINFG,SNOFFG,CCFP,IVL1
      REAL      AIRTMP,DELT,DEWTMP,DTMPG,PREC,RAINF,RDCSN,RDNSN,
     $          SKYCLR,SNOTMP,SNOWCF,SNOWF,TSNOW
C
C     + + + ARGUMENT DEFINITIONS + + +
C     SNOPFG - flag indicating whether to use energy balance (0) or
C              degree-days (1)
C     PREC   - ???
C     HRFG   - ???
C     AIRTMP - ???
C     TSNOW  - ???
C     DTMPG  - ???
C     SNOWCF - ???
C     RDCSN  - ???
C     DELT   - simulation time interval in minutes
C     CCFP   - ???
C     IVL1   - ???
C     SNOTMP - ???
C     DRYFG  - ???
C     SKYCLR - ???
C     DEWTMP - ???
C     SNOFFG - snowing flag - 1:yes
C     SNOWF  - ???
C     RAINFG - raining flag - 1:yes
C     RAINF  - ???
C     RDNSN  - ???
C
C     + + + COMMON BLOCKS + + +
      INCLUDE    'cmdum.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   FPRFG
      REAL      DTSNOW
C
C     + + + END SPECIFICATIONS + + +
C
C     determine whether this is the first interval with precipitation
C     after a dry period
      IF (PREC .GT. 0.0) THEN
C       there is precipitation
        IF (DRYFG .NE. 0) THEN
C         this is the first interval with precipitation after
C         one or more with none
          FPRFG= 1
          DRYFG= 0
        ELSE
C         this is not the first precipitation interval
          FPRFG= 0
        END IF
      ELSE
C       there is no precipitation
        DRYFG= 1
        FPRFG= 0
      END IF
C
      IF ( (DTMPG .GT. -990.0) .AND. (HRFG .NE. 0) ) THEN
C       it is time to estimate the dewpoint
        IF (PREC .GT. 0.0 .AND. AIRTMP .GT. TSNOW) THEN
C         adjust the dewpoint since it will not be meaningfull
C         when used in the equation if higher than air temperature
          DEWTMP= AIRTMP
        ELSE
          IF (DTMPG .GT. AIRTMP) THEN
C           adjust dewpoint since it cannot be higher than air temp
            DEWTMP= AIRTMP
          ELSE
C           gage dewpoint is ok
            DEWTMP= DTMPG
          END IF
        END IF
      END IF
C
C     find the temperature which divides snow from rain, and compute
C     the quantity of snow or rain fall
      IF (PREC .GT. 0.0) THEN
C       there is precipitation
        IF (DTMPG .GE. -990.0) THEN
C         dewpoint is input - correct for humidity        
          IF (HRFG .NE. 0 .OR. FPRFG .NE. 0) THEN
C           compute the air temperature below which precipitation
C           will be snow (snotmp)
C           account for the effect of humidity on snowfall temperature
            DTSNOW= (AIRTMP - DEWTMP)*(0.12 + 0.008*AIRTMP)
C           restrict increase to 1 deg f
            IF (DTSNOW .LT. 1.0) THEN
              SNOTMP= TSNOW + DTSNOW
            ELSE
              SNOTMP= TSNOW + 1.0
            ENDIF
          ENDIF
        ENDIF
C
        IF (SNOPFG .EQ. 0) THEN
C         there is maximum cloud cover
          SKYCLR= 0.15
        END IF
C
        IF (AIRTMP .LT. SNOTMP) THEN
C         it is snowing
          SNOFFG= 1
          SNOWF = SNOWCF*PREC
          RAINF = 0.0
          RAINFG= 0
          IF (HRFG .NE.0 .OR. FPRFG .NE. 0) THEN
C           estimate the relative density of the new snow
            IF (AIRTMP .GT. 0.0) THEN
C             adjust
              RDNSN= RDCSN + (AIRTMP/100.0)**2
            ELSE
C             use value for really cold snow
              RDNSN= RDCSN
            END IF
          END IF
        ELSE
C         it is raining
          RAINFG= 1
          RAINF = PREC
          SNOFFG= 0
          SNOWF = 0.0
        END IF
      ELSE
C       it is clear, or clearing
        RAINF = 0.0
        RAINFG= 0
        SNOFFG= 0
        SNOWF = 0.0
        IF (SNOPFG .EQ. 0) THEN
C         account for sky clearing
          IF (SKYCLR .LT. 1.0) THEN
            SKYCLR= SKYCLR + (0.0004 * DELT)
            IF (SKYCLR .GT. 1.0) THEN
              SKYCLR= 1.0
            END IF
          END IF
        END IF
      END IF
C
      IF (SNOPFG .EQ. 0) THEN
C       use cloud cover time series if available
        IF (CCFP .GE. 0) THEN
          SKYCLR= 1.0 - (PAD (CCFP + IVL1) / 10.0)
          IF (SKYCLR .LT. 0.15) THEN
            SKYCLR= 0.15
          END IF
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   NOPACK
     I                    (SNOPFG,COVIND,
     O                     HR6FG,PACKF,PACKI,PACKW,PACK,PDEPTH,
     O                     RDENPF,COVINX,SNOCOV,DULL,ALBEDO,XLNMLT,
     O                     MNEGHS,PAKTMP,NEGHTS,PACKWC,NEGHT,GMELTR,
     O                     COMPCT,SNOWEP,MOSTHT,VAP)
C
C     + + + PURPOSE + + +
C     Reset state variables when snowpack disappears.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER  SNOPFG,HR6FG
      REAL     ALBEDO,COMPCT,COVIND,COVINX,DULL,GMELTR,MNEGHS,
     $         MOSTHT,NEGHT,NEGHTS,PACK,PACKF,PACKI,PACKW,PACKWC,PAKTMP,
     $         PDEPTH,RDENPF,SNOCOV,SNOWEP,VAP,XLNMLT
C
C     + + + ARGUMENT DEFINITIONS + + +
C     SNOPFG - flag indicating whether to use energy balance (0) or
C              degree-days (1)
C     COVIND - ???
C     HR6FG  - ???
C     PACKF  - content of snowpack
C     PACKW  - ???
C     PACKI  - ???
C     PACK   - ???
C     PDEPTH - ???
C     RDENPF - ???
C     COVINX - ???
C     SNOCOV - ???
C     DULL   - ???
C     ALBEDO - ???
C     XLNMLT - ???
C     MNEGHS - ???
C     PAKTMP - ???
C     NEGHTS - ???
C     PACKWC - ???
C     NEGHT  - ???
C     GMELTR - ???
C     COMPCT - ???
C     SNOWEP - ???
C     MOSTHT - ???
C     VAP    - ???
C
C     + + + END SPECIFICATIONS + + +
C
      HR6FG = 1
      PACKF = 0.0
      PACKI = 0.0
      PACKW = 0.0
      PACK  = 0.0
      PDEPTH= 0.0
      RDENPF= -1.0E30
      COVINX= 0.1*COVIND
      SNOCOV= 0.0
      XLNMLT= 0.0
      MNEGHS= -1.0E30
      PAKTMP= 32.0
      NEGHTS= 0.0
      PACKWC= -1.0E30
      NEGHT = -1.0E30
      GMELTR= -1.0E30
      COMPCT= -1.0E30
      MOSTHT= -1.0E30
C
      IF (SNOPFG .EQ. 0) THEN
C       reset energy balance variables
        DULL  = -1.0E30
        ALBEDO= -1.0E30
        SNOWEP= -1.0E30
        VAP   = -1.0E30
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   SNOACC
     I                   (FRMROW,TOROW)
C
C     + + + PURPOSE + + +
C     Accumulate fluxes for section SNOW.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   FRMROW,TOROW
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FRMROW - row containing incremental flux accumulation
C     TOROW  - flux row to be incremented
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION SNOW2 + + +
      INCLUDE   'cplsn.inc'
      INCLUDE   'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I2,I5
C
C     + + + EXTERNALS + + +
      EXTERNAL  ACCVEC
C
C     + + + END SPECIFICATIONS + + +
C
      I2= 2
      I5= 5
      SNIF(TOROW)= SNIF(TOROW) + SNIF(FRMROW)
C
      CALL ACCVEC
     I           (I5,SNCF1(1,FRMROW),
     M            SNCF1(1,TOROW))
C
      CALL ACCVEC
     I           (I2,SNCF2(1,FRMROW),
     M            SNCF2(1,TOROW))
C
      RETURN
      END
C
C
C
      SUBROUTINE   SNOWEV
     I                   (IREGFG,SVP,DEWTMP,AIRTMP,SNOCOV,SNOEVP,
     I                    WINMOV,LSNO,MESSU,MSGFL,DATIM,
     M                    SNOWEP,PACKF,PDEPTH,PACKI,ECNT1,
     O                    SNOWE,VAP)
C
C     + + + PURPOSE + + +
C     Estimate vapor pressures and rate of evaporation from snow.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   IREGFG,ECNT1,MSGFL,LSNO,MESSU,DATIM(5)
      REAL      AIRTMP,DEWTMP,PACKF,PACKI,PDEPTH,
     $          SNOCOV,SNOEVP,SNOWE,SNOWEP,SVP(40),VAP,WINMOV
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IREGFG - irreqular calc needed flag
C     SVP    - ???
C     DEWTMP - ???
C     AIRTMP - ???
C     SNOCOV - ???
C     SNOEVP - ???
C     WINMOV - ???
C     LSNO   - line number in the opn sequence block of uci
C     MESSU  - ftn unit no. to be used for printout of messages
C     MSGFL  - fortran unit number of error message file
C     SNOWEP - ???
C     PACKF  - content of snowpack
C     PDEPTH - ???
C     PACKI  - ???
C     ECNT1  - ???
C     SNOWE  - ???
C     VAP    - ???
C     DATIM  - date and time of day
C
C     + + + LOCAL VARIABLES + + +
      REAL       DUMMY,SATVAP
C
C     + + + EXTERNALS + + +
      EXTERNAL   VAPOR
C
C     + + + END SPECIFICATIONS + + +
C
      IF (IREGFG .EQ. 1) THEN
C       it is time to estimate - vapor press is in millibars
        CALL VAPOR
     I            (SVP,DEWTMP,LSNO,MESSU,MSGFL,DATIM,
     M             ECNT1,
     O             VAP)
C
        CALL VAPOR
     I            (SVP,AIRTMP,LSNO,MESSU,MSGFL,DATIM,
     M             ECNT1,
     O             SATVAP)
C
C       estimate rate of evap from pack - inches water equiv/ivl
        IF (VAP .GE. 6.108) THEN
C         snow will not evaporate
C         snowep is the potential rate of evap
          SNOWEP= 0.0
        ELSE
C         evaportion is possible so calculate potential rate
          DUMMY = (SATVAP - VAP) * SNOCOV
          SNOWEP= SNOEVP * 0.0002 * WINMOV * DUMMY
        END IF
C
      END IF
C
C     subtract snow evap from the pack
      IF (SNOWEP .GE. PACKF) THEN
C       the pack is gone - all liquid water will be released in
C       module liquid, but may be refrozen by module icing
        SNOWE = PACKF
        PDEPTH= 0.0
        PACKI = 0.0
        PACKF = 0.0
      ELSE
C       subtract evaporation from the pack and continue calculations
        PDEPTH= PDEPTH * (1.0 - SNOWEP / PACKF)
        PACKF = PACKF - SNOWEP
        SNOWE = SNOWEP
        IF (PACKI .GT. PACKF) THEN
          PACKI= PACKF
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   VAPOR
     I                  (SVP,TMP,LSNO,MESSU,MSGFL,DATIM,
     M                   ECNT1,
     O                   VAP)
C
C     + + + PURPOSE + + +
C     Calculate the saturation vapor pressure (millibars)
C     for the given temperature (deg F).
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   LSNO,MESSU,ECNT1,MSGFL,DATIM(5)
      REAL      SVP(40),TMP,VAP
C
C     + + + ARGUMENT DEFINITIONS + + +
C     SVP    - ???
C     TMP    - ???
C     LSNO   - line number in the opn sequence block of uci
C     MESSU  - ftn unit no. to be used for printout of messages
C     MSGFL  - fortran unit number of HSPF message file
C     ECNT1  - ???
C     VAP    - ???
C     DATIM  - date and time of day
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   LOWER,UPPER,SCLU,SGRP
      REAL      INDEX,REMAIN
C
C     + + + INTRINSICS + + +
      INTRINSIC REAL,IFIX
C
C     + + + EXTERNALS + + +
      EXTERNAL  OMSTR,OMSG,OMSTD,OMSTI
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU = 302
C     find the elements of SVP(*) which bracket this temperature
      INDEX= (TMP + 100.0) * 0.2
      LOWER= IFIX(INDEX)
      IF (LOWER .LT. 1) THEN
C       saturation vapor pressure array is out of bounds
        CALL OMSTD (DATIM)
        CALL OMSTI (LSNO)
        CALL OMSTR (INDEX)
        CALL OMSTR (TMP)
        SGRP= 2
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             ECNT1)
      END IF
C
      UPPER= LOWER + 1
      IF (UPPER .GT. 40) THEN
C       saturation vapor pressure array is out of bounds
        CALL OMSTD (DATIM)
        CALL OMSTI (LSNO)
        CALL OMSTR (INDEX)
        CALL OMSTR (TMP)
        SGRP= 2
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             ECNT1)
      END IF
C
      REMAIN= INDEX - REAL(LOWER)
C
C     do linear interpolation
      VAP= SVP(LOWER) + REMAIN * (SVP(UPPER) - SVP(LOWER))
C
      RETURN
      END
C
C
C
      SUBROUTINE   WARMUP
     I                   (PRAIN,
     M                    SUMHT,NEGHTS,PDEPTH,PACKF,
     O                    RNFRZ)
C
C     + + + PURPOSE + + +
C     Warm the pack to as much as 32 deg F, if possible
C     neghts is the negative heat storage in the pack.
C
C     + + + DUMMY ARGUMENTS + + +
      REAL       NEGHTS,PACKF,PDEPTH,PRAIN,RNFRZ,SUMHT
C
C     + + + ARGUMENT DEFINITIONS + + +
C     PRAIN  - ???
C     SUMHT  - ???
C     NEGHTS - ???
C     PDEPTH - ???
C     PACKF  - content of snowpack
C     RNFRZ  - ???
C
C     + + + END SPECIFICATIONS + + +
C
      IF (SUMHT .GT. 0.0) THEN
C       there is heat available to warm the pack
        IF (SUMHT .GT. NEGHTS) THEN
C         there is more than enough heat to get to 32 deg f
          SUMHT = SUMHT - NEGHTS
          NEGHTS= 0.0
        ELSE
C         there is not enough
          NEGHTS= NEGHTS - SUMHT
          SUMHT = 0.0
        END IF
      END IF
C
C     use up some or all of the latent heat of rain.
C     since negative heat is considered in inches of water to melt the
C     frozen contents of the snowpack, the heat energy released
C     in freezing (the latent heat of fusion) is subtractable
C     without any conversion.
      IF (PRAIN .GT. 0.0) THEN
C       there is rain entering the pack
        IF (PRAIN .GT. NEGHTS) THEN
C         there is more than enough heat to get to 32 deg f
C         rnfrz is the amount of rain which freezes
          RNFRZ = NEGHTS
          PACKF = PACKF + RNFRZ
          NEGHTS= 0.0
        ELSE
C         there is not enough latent heat to get to 32 deg f
C         all rain entering pack will freeze
          RNFRZ = PRAIN
          NEGHTS= NEGHTS - PRAIN
          PACKF = PACKF + PRAIN
        END IF
C
C       assume freezing rain does not increase depth
C       of pack, unless relative density is >= 1.0
        IF (PACKF .GT. PDEPTH) THEN
          PDEPTH= PACKF
        END IF
      ELSE
C       no addition of rain to the pack means no freezing of rain
        RNFRZ= 0.0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   SNOPRT
     I                   (UNITFG,LEV,PRINTU,OPERFG,BINU)
C
C     + + + PURPOSE + + +
C     Convert quantities from internal to external units, calculate
C     water balance and print out results.  NOTE: Local arrays have
C     identical sizes and structures to the corresponding arrays in
C     the OSV, apart from dropping the dimension LEV, for fluxes
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   UNITFG,LEV,PRINTU,OPERFG,BINU
C
C     + + + ARGUMENT DEFINITIONS + + +
C     UNITFG - output units   1-english, 2-metric
C     LEV    - current output level (2-pivl,3-day,4-mon,5-ann)
C     PRINTU - fortran unit number on which to print output
C     OPERFG - operation flag   1-perlnd, 2-implnd
C     BINU   - fortran unit number on which to write binary output
C
C     + + + COMMON BLOCKS-SCRTCH, VERSION SNOW2 + + +
      INCLUDE   'cplsn.inc'
      INCLUDE   'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,J,I0,I1,I2,I3,I5,I7,ACNT,CLEN(22),EXDAT(5)
      REAL      DFACTA,DFACTB,PCFLX1(5),PCFLX2(2),PIFLX,PPACK,PPACKS,
     $          PSTAT1(7),PSTAT3(3),TFACTA,TFACTB,APRINT(22)
      CHARACTER*8   UNITID,CSTAT1(7),CSNST2(5),CSTAT3(3),CCFLX1(5)
      CHARACTER*256 CHEAD(22)
C
C     + + + INTRINSICS + + +
      INTRINSIC  ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL   TRNVEC, BALCHK, EXDATE
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' *** SNOW ***')
 2010 FORMAT (/,'   STATE VARIABLES                   PACK     PACKF',
     $  '     PACKW     PACKI    PDEPTH    COVINX    NEGHTS    XLNMLT')
 2020 FORMAT (' ',30X,8(8X,'IN'))
 2030 FORMAT (' ',30X,8(8X,'MM'))
 2040 FORMAT (' ',30X,8F10.2)
 2050 FORMAT (/,' ',30X,'    RDENPF    SKYCLR    SNOCOV      DULL',
     $         '    ALBEDO    PAKTMP    DEWTMP   SNOWTMP')
 2060 FORMAT (  ' ',80X,3(5X,'DEG F'))
 2070 FORMAT (  ' ',80X,3(5X,'DEG C'))
 2080 FORMAT (  ' ',30X,3F10.2,F10.1,F10.2,3F10.1)
 2082 FORMAT (/,' ',30X,'    RDENPF    SNOCOV    PAKTMP    DEWTMP',
     $  '   SNOWTMP')
 2084 FORMAT (  ' ',50X,3(5X,'DEG F'))
 2086 FORMAT (  ' ',50X,3(5X,'DEG C'))
 2090 FORMAT (/,'   NO PACK')
 2100 FORMAT (/,'   FLUXES',22X,
     $  '    PRECIP     SNOWF     PRAIN     SNOWE    WYIELD      MELT')
 2110 FORMAT (  ' ',30X,6(8X,'IN'))
 2120 FORMAT (  ' ',30X,6(8X,'MM'))
 2130 FORMAT (  ' ',30X,6F10.2)
 2140 FORMAT (/,'   FLUXES ALL ZERO')
C
C     + + + END SPECIFICATIONS + + +
C
      I0= 0
      I1= 1
      I2= 2
      I3= 3
      I5= 5
      I7= 7
C
C     initialize array counter for binary printout, store variable
C     names in local strings for use in building binary headers
      ACNT = 0
      CSTAT1(1) = 'PACKF'
      CSTAT1(2) = 'PACKW'
      CSTAT1(3) = 'PACKI'
      CSTAT1(4) = 'PDEPTH'
      CSTAT1(5) = 'COVINX'
      CSTAT1(6) = 'NEGHTS'
      CSTAT1(7) = 'XLNMLT'
      CSNST2(1) = 'RDENPF'
      CSNST2(2) = 'SKYCLR'
      CSNST2(3) = 'SNOCOV'
      CSNST2(4) = 'DULL'
      CSNST2(5) = 'ALBEDO'
      CSTAT3(1) = 'PAKTMP'
      CSTAT3(2) = 'DEWTMP'
      CSTAT3(3) = 'SNOTMP'
      CCFLX1(1) = 'SNOWF'
      CCFLX1(2) = 'PRAIN'
      CCFLX1(3) = 'SNOWE'
      CCFLX1(4) = 'WYIELD'
      CCFLX1(5) = 'MELT'
C
C     assign values to parameters used for conversion from internal
C     to external units
C
      IF (UNITFG .EQ. 1) THEN
C       english system, parameters for variables with depth units
        DFACTA= 1.0
        DFACTB= 0.0
C       parameters for variables with temperature units
        TFACTA= 1.0
        TFACTB= 0.0
      ELSE
C       metric system, parameters for variables with depth units
        DFACTA= 25.4
        DFACTB= 0.0
C       parameters for variables with temperature units
        TFACTA= .555
        TFACTB= -17.777
      END IF
C
C     dimensionless state variables do not have to be converted
C
C     convert to external units
C
C     state variables with depth units
      CALL TRNVEC
     I           (I7,SNST1,DFACTA,DFACTB,
     O            PSTAT1)
C
C     initial and final pack storages
      PPACKS= SNST4(LEV)*DFACTA
      PPACK = SNST4(1)*DFACTA
C
C     state variables with temperature units
      CALL TRNVEC
     I           (I3,SNST3,TFACTA,TFACTB,
     O            PSTAT3)
C     fluxes - depth units
C
C     input flux
      PIFLX= SNIF(LEV)*DFACTA
C
C     computed fluxes
      CALL TRNVEC
     I           (I5,SNCF1(1,LEV),DFACTA,DFACTB,
     O            PCFLX1)
C
      CALL TRNVEC
     I           (I2,SNCF2(1,LEV),DFACTA,DFACTB,
     O            PCFLX2)
C
      IF (PRINTU .GT. 0 .AND. PFLAG(2) .LE. LEV) THEN
C       do printout, on unit PRINTU
        WRITE (PRINTU,2000)
      END IF
C
      IF (PPACK .GT. 0.0) THEN
        IF (PRINTU .GT. 0 .AND. PFLAG(2) .LE. LEV) THEN
C         write out state variables
          WRITE (PRINTU,2010)
C
          IF (UNITFG .EQ. 1) THEN
            WRITE (PRINTU,2020)
          ELSE
            WRITE (PRINTU,2030)
          END IF
C
          WRITE (PRINTU,2040)  PPACK, PSTAT1
        END IF
      ELSE
C       no pack
        IF (PRINTU .GT. 0 .AND. PFLAG(2) .LE. LEV) THEN
          WRITE (PRINTU,2090)
        END IF
C
      END IF

      IF (BINU .GT. 0 .AND. ABS(BFLAG(2)) .LE. LEV) THEN
C       compile values for binary printout
        ACNT = ACNT + 1
        APRINT(ACNT) = PPACK
        CHEAD(ACNT) = 'PACK'
        CLEN(ACNT) = 4
        DO 10 I = 1, 7
          ACNT = ACNT + 1
          APRINT(ACNT) = PSTAT1(I)
          CHEAD(ACNT) = CSTAT1(I)
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 10     CONTINUE
      END IF
C
      IF (SNOPFG .EQ. 0) THEN
        IF (PPACK .GT. 0.0) THEN
C         energy balance method
          IF (PRINTU .GT. 0 .AND. PFLAG(2) .LE. LEV) THEN
            WRITE (PRINTU,2050)
C
            IF (UNITFG .EQ. 1) THEN
              WRITE (PRINTU,2060)
            ELSE
              WRITE (PRINTU,2070)
            END IF
C
            WRITE (PRINTU,2080)  SNST2, PSTAT3
          END IF
        END IF
        IF (BINU .GT. 0 .AND. ABS(BFLAG(2)) .LE. LEV) THEN
C         compile values for binary printout
          DO 20 I = 1, 5
            ACNT = ACNT + 1
            APRINT(ACNT) = SNST2(I)
            CHEAD(ACNT) = CSNST2(I)
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 20       CONTINUE
          DO 30 I = 1, 3
            ACNT = ACNT + 1
            APRINT(ACNT) = PSTAT3(I)
            CHEAD(ACNT) = CSTAT3(I)
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 30       CONTINUE
        END IF
      ELSE
        IF (PPACK .GT. 0.0) THEN
C         degree-day method
          IF (PRINTU .GT. 0 .AND. PFLAG(2) .LE. LEV) THEN
            WRITE (PRINTU,2082)
C
            IF (UNITFG .EQ. 1) THEN
              WRITE (PRINTU,2084)
            ELSE
              WRITE (PRINTU,2086)
            END IF
C
            WRITE (PRINTU,2080)  SNST2(1), SNST2(3), PSTAT3
          END IF
        END IF
        IF (BINU .GT. 0 .AND. ABS(BFLAG(2)) .LE. LEV) THEN
C         compile values for binary printout
          ACNT = ACNT + 1
          APRINT(ACNT) = SNST2(1)
          CHEAD(ACNT) = CSNST2(1)
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          ACNT = ACNT + 1
          APRINT(ACNT) = SNST2(3)
          CHEAD(ACNT) = CSNST2(3)
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          DO 40 I = 1, 3
            ACNT = ACNT + 1
            APRINT(ACNT) = PSTAT3(I)
            CHEAD(ACNT) = CSTAT3(I)
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 40       CONTINUE
        END IF
      END IF
C
C     see whether there are any fluxes to write out
      IF (PCFLX2(1) .GT. 0.0 .OR. ABS(PCFLX2(2)) .GT. 0.005) THEN
C       there was some change in the contents of the pack so we
C       have printout
        IF (PRINTU .GT. 0 .AND. PFLAG(2) .LE. LEV) THEN
          WRITE (PRINTU,2100)
C
          IF (UNITFG .EQ. 1) THEN
            WRITE (PRINTU,2110)
          ELSE
            WRITE (PRINTU,2120)
          END IF
C
          WRITE (PRINTU,2130)  PIFLX, PCFLX1
        END IF
      END IF
      IF (BINU .GT. 0 .AND. ABS(BFLAG(2)) .LE. LEV) THEN
C       compile values for binary printout
        ACNT = ACNT + 1
        APRINT(ACNT) = PIFLX
        CHEAD(ACNT) = 'PRECIP'
        CLEN(ACNT) = 6
        DO 50 I = 1, 5
          ACNT = ACNT + 1
          APRINT(ACNT) = PCFLX1(I)
          CHEAD(ACNT) = CCFLX1(I)
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 50     CONTINUE
      END IF
C
      IF (PCFLX2(1) .GT. 0.0 .OR. ABS(PCFLX2(2)) .GT. 0.005) THEN
C       material balance check and report
        IF (UNITFG .EQ. 1) THEN
C         english
          UNITID= '  INCHES'
        ELSE
C         metric
          UNITID= '      MM'
        END IF
C
        CALL BALCHK
     I             (I1,LSNO,DATIM,MESSU,PRINTU,MSGFL,
     I              PPACKS,PPACK,PCFLX2(1),PCFLX2(2),UNITID,I1,
     M              SNWCNT(1))
C
      ELSE
C       no fluxes to report
        IF (PRINTU .GT. 0 .AND. PFLAG(2) .LE. LEV) WRITE (PRINTU,2140)
      END IF
C
      IF (BINU .GT. 0 .AND. ABS(BFLAG(2)) .LE. LEV) THEN
C       write binary output
        CALL EXDATE(
     I              DATIM,
     O              EXDAT)
        IF (BFLAG(2) .GT. 0) THEN
C         at start of run, write the header
          IF (OPERFG .EQ. 1) THEN
            WRITE (BINU) I0,'PERLND  ',LSNO,'SNOW    ',
     1            (CLEN(I),(CHEAD(I)(J:J),J=1,CLEN(I)),I=1,ACNT)
          ELSE IF (UNITFG .EQ. 2) THEN
            WRITE (BINU) I0,'IMPLND  ',LSNO,'SNOW    ',
     1            (CLEN(I),(CHEAD(I)(J:J),J=1,CLEN(I)),I=1,ACNT)
          END IF
C         set bflag to negative to not write headers anymore
          BFLAG(2) = -BFLAG(2)
        END IF
        IF (OPERFG .EQ. 1) THEN
          WRITE (BINU) I1,'PERLND  ', LSNO,'SNOW    ',UNITFG,
     1                 LEV,(EXDAT(I),I=1,5),(APRINT(I),I=1,ACNT)
        ELSE IF (OPERFG.EQ. 2) THEN
          WRITE (BINU) I1,'IMPLND  ', LSNO,'SNOW    ',UNITFG,
     1                 LEV,(EXDAT(I),I=1,5),(APRINT(I),I=1,ACNT)
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   SNORST
     I                   (LEV)
C
C     + + + PURPOSE + + +
C     Reset all flux accumulators and those state variables
C     used in material balance check for section SNOW.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   LEV
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEV    - current output level (2-pivl,3-day,4-mon,5-ann)
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION SNOW2 + + +
      INCLUDE    'cplsn.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I2,I5
C
C     + + + EXTERNALS + + +
      EXTERNAL  SETVEC
C
C     + + + END SPECIFICATIONS + + +
C
      I2= 2
      I5= 5
C
C     set flux accumulators to zero
      SNIF(LEV)= 0.0
C
      CALL SETVEC
     I           (I5,0.0,
     O            SNCF1(1,LEV))
C
      CALL SETVEC
     I           (I2,0.0,
     O            SNCF2(1,LEV))
C
C     keep present pack storage in state variable used for
C     material balance check
C
      SNST4(LEV)= SNST4(1)
C
      RETURN
      END
C
C
C
      SUBROUTINE   SNOWPB
C
C     + + + PURPOSE + + +
C     Handle section SNOW.
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION SNOW2 + + +
      INCLUDE 'cplsn.inc'
      INCLUDE 'cmpad.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      IF (SNFFP .GE. 1) THEN
        PAD(SNFFP + IVL1)= SNOWF
      END IF
C
      IF (PRNFP .GE. 1) THEN
        PAD(PRNFP + IVL1)= PRAIN
      END IF
C
      IF (RNFFP .GE. 1) THEN
        PAD(RNFFP + IVL1)= RAINF
      END IF
C
      IF (SNEFP .GE. 1) THEN
        PAD(SNEFP + IVL1)= SNOWE
      END IF
C
      IF (WYFP .GE. 1) THEN
        PAD(WYFP + IVL1)= WYIELD
      END IF
C
      IF (MELTFP .GE. 1) THEN
        PAD(MELTFP + IVL1)= MELT
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   SNOWPT
C
C     + + + PURPOSE + + +
C     Handle section SNOW.
C
C     + + + COMMON BLOCKS- SCRTCH VERSION SNOW2 + + +
      INCLUDE 'cplsn.inc'
      INCLUDE 'cmpad.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      IF (PFFP .GE. 1) THEN
        PAD(PFFP + IVL1)= PACKF
      END IF
C
      IF (PDEPFP .GE. 1) THEN
        PAD(PDEPFP + IVL1)= PDEPTH
      END IF
C
      IF (RDPFFP .GE. 1) THEN
        PAD(RDPFFP + IVL1)= RDENPF
      END IF
C
      IF (SNOCFP .GE. 1) THEN
        PAD(SNOCFP + IVL1)= SNOCOV
      END IF
C
      IF (PWFP .GE. 1) THEN
        PAD(PWFP + IVL1)= PACKW
      END IF
C
      IF (PIFP .GE. 1) THEN
        PAD(PIFP + IVL1)= PACKI
      END IF
C
      IF (PAKTFP .GE. 1) THEN
        PAD(PAKTFP + IVL1)= PAKTMP
      END IF
C
      IF (ALBFP .GE. 1) THEN
        PAD(ALBFP + IVL1)= ALBEDO
      END IF
C
      IF (PACKFP .GE. 1) THEN
        PAD(PACKFP + IVL1)= PACK
      END IF
C
      IF (COVXFP .GE. 1) THEN
        PAD(COVXFP + IVL1)= COVINX
      END IF
C
      IF (NHTSFP .GE. 1) THEN
        PAD(NHTSFP + IVL1)= NEGHTS
      END IF
C
      IF (XLNMFP .GE. 1) THEN
        PAD(XLNMFP + IVL1)= XLNMLT
      END IF
C
      IF (SKYCFP .GE. 1) THEN
        PAD(SKYCFP + IVL1)= SKYCLR
      END IF
C
      IF (DULLFP .GE. 1) THEN
        PAD(DULLFP + IVL1)= DULL
      END IF
C
      IF (DEWTFP .GE. 1) THEN
        PAD(DEWTFP + IVL1)= DEWTMP
      END IF
C
      IF (SNOTFP .GE. 1) THEN
        PAD(SNOTFP + IVL1)= SNOTMP
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   ALBED
     I                   (MON,LAT,DULL,
     O                    ALBEDO)
C
C     + + + PURPOSE + + +
C     Estimate albedo from dullness index.  The equation used depends
C     on the season, which is a function of time of year and hemisphere
C     - positive latitude is n hemisphere.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER MON
      REAL    LAT,DULL,ALBEDO
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MON    - month of year
C     LAT    - latitude
C     DULL   - dullness index of snow pack
C     ALBEDO - albedo of snow pack
C
C     + + + LOCAL VARIABLES + + +
      INTEGER EQN
C
C     + + + FUNCTIONS + + +
      REAL      SQRT
C
C     + + + INTRINSICS + + +
      INTRINSIC SQRT
C
C     + + + END SPECIFICATIONS + + +
C
      IF (MON .LE. 3 .OR. MON .GE. 10) THEN
C       winter time
        IF (LAT .GE. 0.0) THEN
          EQN= 1
        ELSE
          EQN= 2
        END IF
      ELSE
C       summer time
        IF (LAT .GE. 0.0) THEN
          EQN= 2
        ELSE
          EQN= 1
        END IF
      END IF
C
C     estimate albedo of snow pack from dullness index
      IF (EQN .EQ. 1) THEN
        ALBEDO= 0.85 - 0.07 * SQRT(DULL / 24.0)
        IF (ALBEDO .LT. 0.6) THEN
          ALBEDO= 0.6
        END IF
      ELSE
        ALBEDO= 0.80 - 0.10 * SQRT(DULL / 24.0)
        IF (ALBEDO .LT. 0.45) THEN
          ALBEDO= 0.45
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   DEGDAY
     I                    (AIRTMP,TBASE,DAYFG,VKMFG,KMELTM,MON,DAY,
     I                     NXTMON,NDAYS,
     M                     KMELT,
     O                     MOSTHT)
C
C     + + + PURPOSE + + +
C     Approximate rate of heat exchange (except components supplied
C     by ground melt and rain heat) using degree-day parameter.  Units
C     are inches of water with equivalent latent heat/ivl.  
C     These calculations are done intermittently.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   DAYFG,VKMFG,MON,DAY,NXTMON,NDAYS
      REAL      AIRTMP,TBASE,KMELTM(12),KMELT,MOSTHT
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MON    - calendar month
C     MOSTHT - ???
C
C     + + + LOCAL VARIABLES + + +
C
C     + + + FUNCTIONS + + +
      REAL     DAYVAL
C
C     + + + EXTERNALS + + +
      EXTERNAL DAYVAL
C
C     + + + END SPECIFICATIONS + + +
C
      IF (VKMFG .EQ. 1) THEN
C       kmelt is allowed to vary throughout the year
C       interpolate for the daily value
C       linearly interpolate kmelt between two values from the
C       monthly array kmeltm(12)
        IF (DAYFG .EQ. 1) THEN
C         it is the first interval of the day
          KMELT= DAYVAL (KMELTM(MON),KMELTM(NXTMON),DAY,NDAYS)
        ELSE
C         kmelt does not vary throughout the year and
C         has been supplied by the run interpreter
        END IF
      END IF
C
      MOSTHT= KMELT*(AIRTMP- TBASE)
C
      RETURN
      END
