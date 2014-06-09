C
C
C
      SUBROUTINE   PSED
C
C     + + + PURPOSE + + +
C     Process input for section sedtrn of rchres application module
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION SED1 + + +
      INCLUDE    'crhse.inc'
      INCLUDE    'crin2.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,I1,I2,I4,IVAL(1),J,SCLU,SGRP
      REAL       FACT,RHOMN,RVAL(4),RWTSED,TOTL,VOLSED
C
C     + + + EXTERNALS + + +
      EXTERNAL   RTABLE,ITABLE,OMSG,OMSTR,ZIPI
C
C     + + + INTRINSICS + + +
      INTRINSIC  ABS
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' PROCESSING INPUT FOR SECTION SEDTRN')
 2010 FORMAT (/,' SAND PARAMETERS')
 2020 FORMAT (/,' SILT PARAMETERS')
 2030 FORMAT (/,' CLAY PARAMETERS')
 2070 FORMAT (/,' FINISHED PROCESSING INPUT FOR SECTION SEDTRN')
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU= 345
      I1  = 1
C
      IF (OUTLEV.GT.1) THEN
        WRITE (MESSU,2000)
      END IF
C
C     warning counters
      J= 0
      I= 6
      CALL ZIPI(I,J,SDWCT1)
      I= 2
      CALL ZIPI(I,J,SDWCT2)
C
C     error counters
      I= 3
      CALL ZIPI(I,J,SDECNT)
C
C     sandload method flag- table-type sandfg
      I2= 28
      I4= 1
      CALL ITABLE
     I             (I2,I1,I4,UUNITS,
     M              IVAL(1))
C
      SANDFG= IVAL(1)
C
C     check that aux3fg in section hydr is on
      IF (HYDRFG.EQ.1.AND.AUX3FG.EQ.0) THEN
C       error- sediment transport requires aux3fg to be on
        SGRP = 4
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             ECOUNT)
      END IF
C
C     get general sediment-related parms- table-type sed-genparm
      I2= 29
      I4= 3
      CALL RTABLE
     I             (I2,I1,I4,UUNITS,
     M              SDGPM(1))
C
      IF (HYDRFG.EQ.0) THEN
C       get values that would ordinarily be read in section hydr-
C       table-type sed-hydparm
        I2= 30
        I4= 3
        CALL RTABLE
     I               (I2,I1,I4,UUNITS,
     M                RVAL(1))
C
        LEN  = RVAL(1)
        DELTH= RVAL(2)
        DB50 = RVAL(3)
        SLOPE= DELTH/LEN
      END IF
C
      IF (OUTLEV.GT.2) THEN
        WRITE (MESSU,2010)
      END IF
C
C     get sand parameters- table-type sand-pm
      I2= 31
      I4= 5
      CALL RTABLE
     I             (I2,I1,I4,UUNITS,
     M              SDPM(1,1))
C
C     convert settling velocity from m/sec to m/ivl
      SDPM(2,1)= SDPM(2,1)*DELTS
C
      DO 40 J= 2,3
C       get parameters for cohesive material- table-type
C       silt-clay-pm
        I= J - 1
C
        IF (OUTLEV.GT.2) THEN
          IF (J.EQ.2) WRITE (MESSU,2020)
          IF (J.EQ.3) WRITE (MESSU,2030)
        END IF
C
        I2= 32
        I4= 6
        CALL RTABLE
     I               (I2,I,I4,UUNITS,
     M                SDPM(1,J))
C
C       convert settling velocity from m/sec to m/ivl
        SDPM(2,J)= SDPM(2,J)*DELTS
C
C       convert erodibility coeff from /day to /ivl
        SDPM(6,J)= SDPM(6,J)*DELT60/24.0
C
 40   CONTINUE
C
C     initial conditions
C
C     suspended sediment concentrations- table-type ssed-init
      I2= 33
      I4= 3
      CALL RTABLE
     I           (I2,I1,I4,UUNITS,
     M            SSED(1))
C
C     find the total concentration
      TOTL= 0.0
C
      DO 50 I=1,3
        TOTL= TOTL + SSED(I)
 50   CONTINUE
C
      SSED(4)= TOTL
C
C     bed sediment conditions- table-type bed-init
      I2= 34
      I4= 4
      CALL RTABLE
     I           (I2,I1,I4,UUNITS,
     M            RVAL(1))
C
C     check that sum of fractions is nearly 1.0
      TOTL= 0.0
C
      DO 60 J=1, 3
        TOTL= TOTL + RVAL(J + 1)
 60   CONTINUE
C
      IF (ABS(TOTL - 1.0).GT.0.01) THEN
C       sum of fractions is not close enough to 1.0
        CALL OMSTR (RVAL(2))
        CALL OMSTR (RVAL(3))
        CALL OMSTR (RVAL(4))
        SGRP= 5
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             ECOUNT)
      END IF
C
C     justify the fractions to add to 1.0, and find the mean
C     relative density of the sediment particles
      FACT = (1.0/TOTL)
      RHOMN= 0.0
C
      DO 80 J=1,3
        RVAL(J + 1)= RVAL(J + 1)*FACT
        RHOMN      = RHOMN + RVAL(J + 1)*SDPM(3,J)
 80   CONTINUE
C
      BEDDEP= RVAL(1)
C
C     find the total volume of sediment particles- ft3 or m3
      VOLSED= LEN*BEDWID*BEDDEP*(1.0 - POR)
C
C     total weight relative to water- rhomn is in parts/part (same as
C     kg/l)
      RWTSED= VOLSED*RHOMN
C
C     find the weight of each fraction- units are (mg/l)*ft3 or
C     (mg/l)*m3
C
C     factor 1.0e06 converts from kg/l to mg/l
      RWTSED= RWTSED*1.0E06
C
      DO 90 J=1,3
        RSED(J + 3)= RVAL(J + 1)*RWTSED
 90   CONTINUE
C
C     find the total quantity (bed and suspended) of each sediment
C     size fraction
      RSED(10)= 0.0
      TSED(1)= 0.0
      TSED(2)= 0.0
C
      DO 100 J=1,3
        RSED(J)    = SSED(J)*VOL
        RSED(J + 6)= RSED(J)+ RSED(J+ 3)
        RSSED(J)   = RSED(J+ 6)
        RSED(10)   = RSED(10)+ RSED(J+ 6)
        TSED(1)    = TSED(1)+ RSED(J)
        TSED(2)    = TSED(2)+ RSED(J+ 3)
 100  CONTINUE
      TSED(3)= RSED(10)
C
C     evaluate some quantities used in colby and/or toffaleti sand
C     transport simulation methods
      IF (UUNITS.EQ.1) THEN
        DB50E= DB50
        DB50M= DB50*304.8
      ELSE
        DB50E= DB50*3.28
        DB50M= DB50*1000.0
      END IF
C
C     convert fall velocity from m/ivl to ft/sec
      WSANDE= SDPM(2,1)*3.28/DELTS
C
      IF (OUTLEV.GT.1) THEN
        WRITE (MESSU,2070)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   SEDTRN
C
C     + + + PURPOSE + + +
C     Simulate behavior of inorganic sediment
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION SED2 + + +
      INCLUDE    'crhse.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     SCLU,SGRP,J,N,DNFG,REQFG,TSSUB(2),FLGVAL
      REAL        AVDEPM,AVDEPE,AVVELE,EXPSND,FSL,HRADE,KSAND,ROM,
     $            TWIDE,VOLSED
      CHARACTER*6 OPTYP,TSNAM,SECNAM,MSECNM,OPFGNM
C
C     + + + EXTERNALS + + +
      EXTERNAL   COHESV,SANDLD,OMSTR,OMSTI,OMSTD,OMSG,HREQTS
C
C     + + + INTRINSICS + + +
      INTRINSIC  ABS
C
C     + + + DATA INITIALIZATIONS + + +
      DATA TSSUB/1,1/
      DATA OPTYP,SECNAM/'RCHRES','SEDTRN'/
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU = 345
C     get input time series- inflow of sediment is in units
C     of mg.ft3/l.ivl (english) or mg.m3/l.ivl (metric)
C
      ISED(4)= 0.0
      DO 30 J=1,3
        IF (SDIFFP(J) .GT. 0) THEN
C         there is external input of sediment fraction j
          ISED(J)= PAD(SDIFFP(J) + IVL1)
        ELSE
          ISED(J)= 0.0
        END IF
        ISED(4)= ISED(4)+ ISED(J)
 30   CONTINUE
C
      IF (HYDRFG .EQ. 0) THEN
C       get time series computed by section hydr
CTHJ        TAU  = PAD(TAUFP + IVL1)
        REQFG= 3
        MSECNM= 'HYDR  '
        TSNAM= 'TAU   '
        CALL HREQTS (TAUFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,RCHNO,
     I               TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,FLGVAL,
     O               TAU)
CTHJ        AVDEP= PAD(AVDFP + IVL1)
        TSNAM= 'AVDEP '
        CALL HREQTS (AVDFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,RCHNO,
     I               TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,FLGVAL,
     O               AVDEP)
CTHJ        AVVEL= PAD(AVVFP + IVL1)
        TSNAM= 'AVVEL '
        CALL HREQTS (AVVFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,RCHNO,
     I               TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,FLGVAL,
     O               AVVEL)
C
        IF (SANDFG .NE. 3) THEN
C         get additional time series
CTHJ          RO  = PAD(ROFP + IVL1)
          REQFG= 5
          OPFGNM= 'SANDFG'
          TSNAM= 'RO    '
          CALL HREQTS (AVDFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,RCHNO,
     I                 TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,SANDFG,
     O                 AVDEP)
CTHJ          HRAD= PAD(HRADFP + IVL1)
          TSNAM= 'HRAD  '
          CALL HREQTS (HRADFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,RCHNO,
     I                 TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,SANDFG,
     O                 HRAD)
CTHJ          TWID= PAD(TWIDFP + IVL1)
          TSNAM= 'TWID  '
          CALL HREQTS (TWIDFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,RCHNO,
     I                 TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,SANDFG,
     O                 TWID)
        END IF
      END IF
C
      IF (HTFG .EQ. 0 .AND. SANDFG .NE. 3) THEN
C       water temperature must be read in
CTHJ        TW= PAD(TWFP + IVL1)
        REQFG= 5
        MSECNM= 'HTRCH '
        OPFGNM= 'SANDFG'
        TSNAM= 'TW    '
        CALL HREQTS (TWFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,RCHNO,
     I               TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,SANDFG,
     O               TW)
C       set undefined temp value to 20 degc
        IF (TW .LT. -100.) TW= 20.
      END IF
C
C     perform any necessary unit conversions
      IF (UUNITS .EQ. 2) THEN
C       uci is in metric units
        AVVELE= AVVEL*3.28
        AVDEPM= AVDEP
        AVDEPE= AVDEP*3.28
        ROM   = RO
        HRADE = HRAD*3.28
        TWIDE = TWID*3.28
      ELSE
C       uci is in english units
        AVVELE= AVVEL
        AVDEPM= AVDEP*0.3048
        AVDEPE= AVDEP
        ROM   = RO*0.0283
        HRADE = HRAD
        TWIDE = TWID
      END IF
C
C     simulate silt and clay
      CALL COHESV
     I            (ISED(1),VOLS,SROVOL,VOL,EROVOL,SOVOL(1),EOVOL(1),
     I             NEXITS,AVDEPM,AVDEPE,SDPM(1,1),TAU,
     M             SSED(1),RSED(1),
     O             ROSED(1),OSED(1,1),DEPSCR(1))
C
C     simulate sandload.  done after washload because washload
C     affects sand transport if the colby method is used
C
C     compute fine sediment load
      FSL   = SSED(2) + SSED(3)
      KSAND = SDPM(4,1)
      EXPSND= SDPM(5,1)
C
      CALL SANDLD
     I            (ISED(1),VOLS,SROVOL,VOL,EROVOL,SOVOL(1),EOVOL(1),
     I             NEXITS,KSAND,AVVELE,EXPSND,ROM,SANDFG,DB50E,HRADE,
     I             SLOPE,TW,WSANDE,TWIDE,DB50M,FSL,RCHNO,
     I             MESSU,MSGFL,DATIM,AVDEPE,
     M             SSED(1),RSED(1),RSED(4),SDECNT(1),
     O             DEPSCR(1),ROSED(1),OSED(1,1))
C
C     set small concentrations to zero
      DO 86 J=1,3
        IF (ABS(SSED(J)) .LT. 1.0E-15) THEN
C         small conc., set to zero
          IF (DEPSCR(J) .GT. 0.0) THEN
C           deposition has occurred, add small storage to deposition
            DEPSCR(J)= DEPSCR(J) + RSED(J)
            RSED(J+3)= RSED(J+3) + RSED(J)
          ELSE
C           add small storage to outflow
            ROSED(J) = ROSED(J) + RSED(J)
            DEPSCR(J)= 0.0
            IF (NEXITS .GT. 1) THEN
              DNFG= 0
              DO 84 N=1,NEXITS
                IF (OSED(N,J) .GT. 0.0 .AND. DNFG .EQ. 0) THEN
                  OSED(N,J)= OSED(N,J) + RSED(J)
                  DNFG= 1
                END IF
 84           CONTINUE
            END IF
          END IF
C
          RSED(J)= 0.0
          SSED(J)= 0.0
        END IF
 86   CONTINUE
C
C     calculate total quantity of material in suspension and in
C     the bed; check bed conditions
C
      VOLSED   = 0.0
      SSED(4)  = 0.0
      RSED(10) = 0.0
      TSED(1)  = 0.0
      TSED(2)  = 0.0
      DEPSCR(4)= 0.0
      ROSED(4) = 0.0
C
      IF (NEXITS .GT. 1) THEN
        DO 90 N= 1,NEXITS
          OSED(N,4)= 0.0
 90     CONTINUE
      END IF
C
      DO 150 J=1,3
        SSED(4)  = SSED(4) + SSED(J)
        DEPSCR(4)= DEPSCR(4) + DEPSCR(J)
        ROSED(4) = ROSED(4) + ROSED(J)
C
        IF (NEXITS .GT. 1) THEN
          DO 110 N=1,NEXITS
            OSED(N,4)= OSED(N,4) + OSED(N,J)
 110      CONTINUE
        END IF
C
C       total storage in mg.vol/l
        RSED(J+ 6)= RSED(J)+ RSED(J+ 3)
        RSED(10)= RSED(10)+ RSED(J+ 6)
        TSED(1)= TSED(1)+ RSED(J)
        TSED(2)= TSED(2)+ RSED(J+ 3)
C       rssed is used in material balance checks
        RSSED(J)= RSED(J+ 6)
C
        IF ((ABS(RSED(J+3))) .LE. 0.0) THEN
C         warn that bed is empty
          CALL OMSTD (DATIM)
          CALL OMSTI (RCHNO)
          CALL OMSTI (J)
          SGRP = 1
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               SDWCT1(2,J))
        END IF
C
C       find the volume occupied by this fraction of bed sediment-
C       ft3 or m3
C
        VOLSED= VOLSED + RSED(J +3)/(SDPM(3,J)*1.0E06)
C
 150  CONTINUE
      TSED(3)= RSED(10)
C
C     find total depth of sediment
C
C     allow for porosity
      VOLSED= VOLSED/(1.0 - POR)
C     calculate thickness of bed- ft or m
      BEDDEP= VOLSED/(LEN*BEDWID)
C
      IF (BEDDEP .GT. BEDWRN) THEN
C       warn that bed depth appears excessive
        CALL OMSTD (DATIM)
        CALL OMSTI (RCHNO)
        CALL OMSTR (BEDDEP)
        CALL OMSTR (BEDWRN)
        SGRP = 2
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             SDWCT2(1))
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   BDEXCH
     I                    (AVDEPM,W,TAU,TAUCD,TAUCS,
     I                     M,VOL,FRCSED,
     M                     SUSP,BED,
     O                     DEPSCR)
C
C     + + + PURPOSE + + +
C     Simulate deposition and scour of a cohesive sediment fraction-
C     silt or clay
C
C     + + + DUMMY ARGUMENTS + + +
      REAL        AVDEPM,BED,DEPSCR,M,SUSP,TAU,TAUCD,TAUCS,W,FRCSED
      DOUBLE PRECISION VOL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     AVDEPM - ???
C     W      - ???
C     TAU    - ???
C     TAUCD  - ???
C     TAUCS  - ???
C     M      - ???
C     VOL    - volume of water in reach above bed
C     FRCSED - ???
C     SUSP   - ???
C     BED    - ???
C     DEPSCR - ???
C
C     + + + LOCAL VARIABLES + + +
      REAL        DEPMAS,EXPNT,SCR,SCRMAS
C
C     + + + INTRINSICS + + +
      INTRINSIC  EXP
C
C     + + + END SPECIFICATIONS + + +
C
      IF (W.GT.0.0 .AND. TAU.LT.TAUCD .AND. SUSP.GT.1.0E-30) THEN
C       deposition will occur
        EXPNT = -W/AVDEPM*(1.0 - TAU/TAUCD)
        DEPMAS= SUSP*(1.0 - EXP(EXPNT))
        SUSP  = SUSP - DEPMAS
        BED   = BED + DEPMAS
      ELSE
C       no deposition- concentrations unchanged
        DEPMAS= 0.0
      END IF
C
      IF (TAU.GT.TAUCS .AND. M.GT.0.0) THEN
C       scour can occur- units are:
C        m- kg/m2.ivl  avdepm- m  scr- mg/l
C
        SCR= FRCSED*M/AVDEPM*1000.*(TAU/TAUCS - 1.0)
C
C       check availability of material
        SCRMAS= SCR*VOL
C
        IF (SCRMAS .GT. BED) THEN
          SCRMAS= BED
        END IF
C
C       update storages
        SUSP= SUSP + SCRMAS
        BED = BED - SCRMAS
      ELSE
C       no scour
        SCRMAS= 0.0
      END IF
C
C     calculate net deposition or scour
      DEPSCR= DEPMAS - SCRMAS
C
      RETURN
      END
C
C
C
      SUBROUTINE   COHESV
     I                    (ISED,VOLS,SROVOL,VOL,EROVOL,SOVOL,
     I                     EOVOL,NEXITS,AVDEPM,AVDEPE,SDPM,TAU,
     M                     SSED,RSED,
     O                     ROSED,OSED,DEPSCR)
C
C     + + + PURPOSE + + +
C     Simulate behavior of cohesive sediments (silt and clay)
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    NEXITS
      REAL       AVDEPM,AVDEPE,DEPSCR(3),EOVOL(5),EROVOL,ISED(3),
     $           OSED(5,3),ROSED(3),RSED(6),SDPM(6,3),
     $           SOVOL(5),SROVOL,SSED(3),TAU
      DOUBLE PRECISION VOL,VOLS
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ISED   - ???
C     VOLS   - ???
C     SROVOL - ???
C     VOL    - volume of water in reach above bed
C     EROVOL - ???
C     SOVOL  - ???
C     EOVOL  - ???
C     NEXITS - number of exits from the operation
C     AVDEPM - ???
C     AVDEPE - ???
C     SDPM   - ???
C     TAU    - ???
C     SSED   - ???
C     RSED   - ???
C     ROSED  - ???
C     OSED   - ???
C     DEPSCR - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    J
      REAL       TOTBED,FRCSED(2)
      DOUBLE PRECISION DPSSED(3)
C
C     + + + EXTERNALS + + +
      EXTERNAL   ADVECT,BDEXCH
C
C     + + + END SPECIFICATIONS + + +
C
C     compute bed fractions for scour apportionment
      TOTBED= RSED(5) + RSED(6)
C
      IF (TOTBED .GT. 0.0) THEN
C       compute bed fractions based on relative storages
        FRCSED(1)= RSED(5)/TOTBED
        FRCSED(2)= RSED(6)/TOTBED
      ELSE
C       no bed at start of interval, assume equal fractions
        FRCSED(1)= .5
        FRCSED(2)= .5
      END IF
C
      DO 10 J= 2,3
C
        DPSSED(J)=SSED(J)
        CALL ADVECT
     I              (ISED(J),VOLS,SROVOL,VOL,EROVOL,SOVOL(1),EOVOL(1),
     I               NEXITS,
     M               DPSSED(J),
     O               ROSED(J),OSED(1,J))
        SSED(J)=DPSSED(J)
C
C       calculate exchange between bed and suspended sediment
        RSED(J)= SSED(J)*VOL
C
        IF (AVDEPE .GT. 0.17) THEN
C         consider deposition and scour
C
          CALL BDEXCH
     I                (AVDEPM,SDPM(2,J),TAU,SDPM(4,J),SDPM(5,J),
     I                 SDPM(6,J),VOL,FRCSED(J - 1),
     M                 RSED(J),RSED(J + 3),
     O                 DEPSCR(J))
C
        ELSE
C         rchres depth is less than two inches -
C         no deposition or scour is allowed
          DEPSCR(J)= 0.0
        END IF
C
        IF (VOL .GT. 0.0) THEN
C         water is present in the rchres
          SSED(J)  = RSED(J)/VOL
        ELSE
C         rchres is dry
          SSED(J)  = -1.0E30
        END IF
C
 10   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   COLBY
     I                   (V,DB50M,FHRAD,FSL,MESSU,TEMPR,
     O                    GSI,FERROR,D50ERR,HRERR,VELERR)
C
C     + + + PURPOSE + + +
C     This subroutine uses colby's method to calculate the capacity of
C     the flow to transport sand.
C      called by: sandld
C     the colby method has the following units and applicable ranges of
C     variables.
C        average velocity.............v.......fps.........1-10 fps
C        hydraulic radius.............fhrad...ft..........1-100 ft
C        median bed material size.....db50....mm..........0.1-0.8 mm
C        temperature..................tmpr....deg f.......32-100 deg.
C        fine sediment concentration..fsl.....mg/liter....0-200000 ppm
C        total sediment load..........gsi.....ton/day.ft..
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   D50ERR,FERROR,HRERR,MESSU,VELERR
      REAL      DB50M,FHRAD,FSL,GSI,TEMPR,V
C
C     + + + ARGUMENT DEFINITIONS + + +
C     V      - average velocity (ft/s)
C     DB50M  - median bed sediment diameter (mm)
C     FHRAD  - hydraulic radius   (ft)
C     FSL    - total concentration of suspended silt and clay
C              (fine sediment) (mg/l)
C     MESSU  - ftn unit no. to be used for printout of messages
C     TEMPR  - water temperature (degrees c)
C     GSI    - total sand transport (tons/day.ft width)
C     FERROR - fatal error flag (if on, switch to toffaleti method)
C     D50ERR - ???
C     HRERR  - ???
C     VELERR - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,ID1,ID2,ID501,ID502,IF1,IF2,II(2),
     $          IP1,IP2,IT1,IT2,IV1,IV2,I1,J,JJ(2),J1,J3,
     $          K,KK(2),K1
      REAL      CF(5),CFD,CFF,CFT,DB50,DF(10),DG(4),DP(11),D50G(6),
     $          F(5,10),FFF,G(4,8,6),GTUC,P(11),P1,P2,T(7,4),
     $          TCF,TEMP(7),TMPR,VG(8),X(2,2),XA(2),
     $          XCT(2),XD,XDX,XDY,XDZ,XF(2,2),XG(2),XNT,
     $          XN1,XN2,XN3,XN4,XR,XT(2,2),XV,XX(2),YY(2),ZZ(2)
C
C     + + + INTRINSICS + + +
      INTRINSIC  ABS,ALOG10
C
C     + + + DATA INITIALIZATIONS + + +
      DATA  G(1,1,1),G(2,1,1),G(3,1,1),G(4,1,1)/1.0, 0.30, 0.06, 0.00/
      DATA  G(1,2,1),G(2,2,1),G(3,2,1),G(4,2,1)/3.00, 3.30, 2.50, 2.00/
      DATA  G(1,3,1),G(2,3,1),G(3,3,1),G(4,3,1)/5.40, 9.0, 10.0, 20.0/
      DATA  G(1,4,1),G(2,4,1),G(3,4,1),G(4,4,1)/11.0, 26.0, 50.0,150.0/
      DATA  G(1,5,1),G(2,5,1),G(3,5,1),G(4,5,1)/17., 49., 130., 500./
      DATA  G(1,6,1),G(2,6,1),G(3,6,1),G(4,6,1)/29., 101., 400., 1350./
      DATA  G(1,7,1),G(2,7,1),G(3,7,1),G(4,7,1)/44.,160.,700.,2500./
      DATA  G(1,8,1),G(2,8,1),G(3,8,1),G(4,8,1)/60.,220.,1000.,4400./
      DATA  G(1,1,2),G(2,1,2),G(3,1,2),G(4,1,2)/0.38, 0.06, 0.0, 0.0/
      DATA  G(1,2,2),G(2,2,2),G(3,2,2),G(4,2,2)/1.60, 1.20, 0.65, 0.10/
      DATA  G(1,3,2),G(2,3,2),G(3,3,2),G(4,3,2)/3.70, 5., 4., 3./
      DATA  G(1,4,2),G(2,4,2),G(3,4,2),G(4,4,2)/10., 18., 30., 52./
      DATA  G(1,5,2),G(2,5,2),G(3,5,2),G(4,5,2)/17., 40., 80., 160./
      DATA  G(1,6,2),G(2,6,2),G(3,6,2),G(4,6,2)/36., 95., 230., 650./
      DATA  G(1,7,2),G(2,7,2),G(3,7,2),G(4,7,2)/60., 150., 415., 1200./
      DATA  G(1,8,2),G(2,8,2),G(3,8,2),G(4,8,2)/81., 215., 620., 1500./
      DATA  G(1,1,3),G(2,1,3),G(3,1,3),G(4,1,3)/0.14, 0.0, 0.0, 0.0/
      DATA  G(1,2,3),G(2,2,3),G(3,2,3),G(4,2,3)/1., 0.60, 0.15, 0.0/
      DATA  G(1,3,3),G(2,3,3),G(3,3,3),G(4,3,3)/3.30, 3.00, 1.70, 0.50/
      DATA  G(1,4,3),G(2,4,3),G(3,4,3),G(4,4,3)/11., 15., 17., 14./
      DATA  G(1,5,3),G(2,5,3),G(3,5,3),G(4,5,3)/20., 35., 49., 70./
      DATA  G(1,6,3),G(2,6,3),G(3,6,3),G(4,6,3)/44., 85., 150., 250./
      DATA  G(1,7,3),G(2,7,3),G(3,7,3),G(4,7,3)/71., 145., 290., 500./
      DATA  G(1,8,3),G(2,8,3),G(3,8,3),G(4,8,3)/100., 202., 400., 700./
      DATA  G(1,1,4),G(2,1,4),G(3,1,4),G(4,1,4)/0.0, 0.0, 0.0, 0.0/
      DATA  G(1,2,4),G(2,2,4),G(3,2,4),G(4,2,4)/0.70, 0.30, 0.06, 0.0/
      DATA  G(1,3,4),G(2,3,4),G(3,3,4),G(4,3,4)/2.9, 2.3, 1.0, 0.06/
      DATA  G(1,4,4),G(2,4,4),G(3,4,4),G(4,4,4)/11.5, 13., 12., 7./
      DATA  G(1,5,4),G(2,5,4),G(3,5,4),G(4,5,4)/22., 31., 40., 50./
      DATA  G(1,6,4),G(2,6,4),G(3,6,4),G(4,6,4)/47., 84., 135., 210./
      DATA  G(1,7,4),G(2,7,4),G(3,7,4),G(4,7,4)/75., 140., 240., 410./
      DATA  G(1,8,4),G(2,8,4),G(3,8,4),G(4,8,4)/106., 190., 350., 630./
      DATA  G(1,1,5),G(2,1,5),G(3,1,5),G(4,1,5)/0.0, 0.0, 0.0, 0.0/
      DATA  G(1,2,5),G(2,2,5),G(3,2,5),G(4,2,5)/0.44, 0.06, 0.0, 0.0/
      DATA  G(1,3,5),G(2,3,5),G(3,3,5),G(4,3,5)/2.8, 1.8, 0.6, 0.0/
      DATA  G(1,4,5),G(2,4,5),G(3,4,5),G(4,4,5)/12., 12.5, 10., 4.5/
      DATA  G(1,5,5),G(2,5,5),G(3,5,5),G(4,5,5)/24., 30., 35., 37./
      DATA  G(1,6,5),G(2,6,5),G(3,6,5),G(4,6,5)/52., 78., 120., 190./
      DATA  G(1,7,5),G(2,7,5),G(3,7,5),G(4,7,5)/83., 180., 215., 380./
      DATA  G(1,8,5),G(2,8,5),G(3,8,5),G(4,8,5)/120., 190., 305., 550./
      DATA  G(1,1,6),G(2,1,6),G(3,1,6),G(4,1,6)/0.0, 0.0, 0.0, 0.0/
      DATA  G(1,2,6),G(2,2,6),G(3,2,6),G(4,2,6)/0.3, 0.0, 0.0, 0.0/
      DATA  G(1,3,6),G(2,3,6),G(3,3,6),G(4,3,6)/2.9, 1.4, 0.3, 0.0/
      DATA  G(1,4,6),G(2,4,6),G(3,4,6),G(4,4,6)/14., 11., 7.7, 3.0/
      DATA  G(1,5,6),G(2,5,6),G(3,5,6),G(4,5,6)/27., 29., 30., 30./
      DATA  G(1,6,6),G(2,6,6),G(3,6,6),G(4,6,6)/57., 75., 110., 170./
      DATA  G(1,7,6),G(2,7,6),G(3,7,6),G(4,7,6)/90., 140., 200., 330./
      DATA  G(1,8,6),G(2,8,6),G(3,8,6),G(4,8,6)/135., 190., 290., 520./
      DATA  F(1,1),F(2,1),F(3,1),F(4,1),F(5,1)/1., 1.1, 1.6, 2.6, 4.2/
      DATA  F(1,2),F(2,2),F(3,2),F(4,2),F(5,2)/1.,1.1,1.65, 2.75, 4.9/
      DATA  F(1,3),F(2,3),F(3,3),F(4,3),F(5,3)/1., 1.1, 1.7, 3., 5.5/
      DATA  F(1,4),F(2,4),F(3,4),F(4,4),F(5,4)/1., 1.12, 1.9, 3.6, 7./
      DATA  F(1,5),F(2,5),F(3,5),F(4,5),F(5,5)/1.,1.17,2.05, 4.3, 8.7/
      DATA  F(1,6),F(2,6),F(3,6),F(4,6),F(5,6)/1., 1.2, 2.3, 5.5, 11.2/
      DATA  F(1,7),F(2,7),F(3,7),F(4,7),F(5,7)/1., 1.22, 2.75, 8., 22./
      DATA  F(1,8),F(2,8),F(3,8),F(4,8),F(5,8)/1., 1.25, 3., 9.6, 29./
      DATA  F(1,9),F(2,9),F(3,9),F(4,9),F(5,9)/1., 1.3, 3.5, 12., 43./
      DATA  F(1,10),F(2,10),F(3,10),F(4,10),F(5,10)/1.,1.4,4.9,22.,120./
C
      DATA  T /1.2, 1.15, 1.10, 0.96, 0.90, 0.85, 0.82, 1.35, 1.25,
     $          1.12, 0.92, 0.86, 0.80, 0.75, 1.60, 1.40, 1.20, 0.89,
     $          0.80, 0.72, 0.66, 2.00, 1.65, 1.30, 0.85, 0.72, 0.63,
     $          0.55/
C
      DATA  DF /0.10, 0.20, 0.30, 0.60, 1.00, 2.00, 6.00, 10.00,
     $           20.00, 1.E2/
C
      DATA  CF /0.00, 1.E4, 5.E4, 1.E5, 1.5E5/
C
      DATA  P /0.60, 0.90, 1.0, 1.0, 0.83, 0.60, 0.40, 0.25, 0.15,
     $          0.09, 0.05/
C
      DATA  DP /0.10, 0.15, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70,
     $           0.80, 0.90, 1.00/
C
      DATA  DG /0.10, 1.00, 10.0, 100./
C
      DATA  VG/1.0, 1.5, 2.0, 3.0, 4.0, 6.0, 8.0, 10./
C
      DATA  D50G/0.10, 0.20, 0.30, 0.40, 0.60, 0.80/
C
      DATA  TEMP/32., 40., 50., 70., 80., 90., 100./
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT(//10X,'***** SUBROUTINE COLBY -- FSL WENT > 1.E+5')
C
C     + + + END SPECIFICATIONS + + +
C
      DB50 = DB50M
      TMPR = TEMPR * 1.8 + 32.0
C
C     *** fsl....fine sediment (i.e. cohesive sediment or wash) load
C         in mg/liter ***
C
      FERROR= 0
      D50ERR= 0
      HRERR = 0
      VELERR= 0
C
      IF((DB50 .GE. D50G(1)) .AND. (DB50 .LE. D50G(6))) GO TO 10
        FERROR = 1
        D50ERR = 1
 10   CONTINUE
      IF((FHRAD .GE. DG(1)) .AND. (FHRAD .LE. DG(4))) GO TO 20
        FERROR = 1
        HRERR  = 1
 20   CONTINUE
      IF((V .GE. VG(1)) .AND. (V .LE. VG(8))) GO TO 30
        FERROR = 1
        VELERR = 1
 30   CONTINUE
      IF (FERROR .NE. 0) GO TO 400
        IF (TMPR .GE. 32.0) GO TO 40
          TMPR = 32.0
 40     CONTINUE
        IF (TMPR .LE. 100) GO TO 45
          TMPR = 100.0
 45     CONTINUE
        ID1 = 0
        ID2 = 0
        DO 60 I= 1,3
          IF ((FHRAD .LT. DG(I)) .OR. (FHRAD .GT. DG(I+1))) GO TO 50
            ID1 = I
            ID2 = I+1
            GO TO 70
 50       CONTINUE
 60     CONTINUE
 70     CONTINUE
        IV1 = 0
        IV2 = 0
        DO 90 I= 1,7
          IF ((V .LT. VG(I)) .OR. (V .GT. VG(I+1))) GO TO 80
            IV1 = I
            IV2 = I+1
            GO TO 100
 80       CONTINUE
 90     CONTINUE
 100    CONTINUE
        ID501 = 0
        ID502 = 0
        DO 120 I= 1,5
          IF ((DB50 .LT. D50G(I)) .OR.
     $    (DB50 .GT. D50G(I+1))) GO TO 110
            ID501 = I
            ID502 = I+1
            GO TO 130
 110      CONTINUE
 120    CONTINUE
 130    CONTINUE
        II(1) = ID1
        II(2) = ID2
        JJ(1) = IV1
        JJ(2) = IV2
        KK(1) = ID501
        KK(2) = ID502
        DO 200 I= 1,2
          I1    = II(I)
          XX(I) = ALOG10(DG(I1))
          DO 190 J= 1,2
            J1    = JJ(J)
            YY(J) = ALOG10(VG(J1))
            DO 180 K= 1,2
              K1    = KK(K)
              ZZ(K) = ALOG10(D50G(K1))
              IF (G(I1,J1,K1) .GT. 0.0) GO TO 160
              DO 140 J3= J1,7
                IF (G(I1,J3,K1) .GT. 0.0) GO TO 150
 140          CONTINUE
 150          CONTINUE
              X(J,K) = ALOG10(G(I1,J3,K1))+(ALOG10(VG(J1)/VG(J3)))*
     $                  (ALOG10(G(I1,J3+1,K1)/G(I1,J3,K1)))/
     $                  (ALOG10(VG(J3+1)/VG(J3)))
              GO TO 170
 160          CONTINUE
              X(J,K) = ALOG10(G(I1,J1,K1))
 170          CONTINUE
 180        CONTINUE
 190      CONTINUE
          XD    = ALOG10(DB50) - ZZ(1)
          XN1   = X(1,2) - X(1,1)
          XN2   = X(2,2) - X(2,1)
          XDZ   = ZZ(2) - ZZ(1)
          XA(1) = X(1,1) + XN1*XD/XDZ
          XA(2) = X(2,1) + XN2*XD/XDZ
          XV    =ALOG10(V) - YY(1)
          XN3   = XA(2) - XA(1)
          XDY   = YY(2) - YY(1)
          XG(I) = XA(1) + XN3*XV/XDY
 200    CONTINUE
        XN4  = XG(2) - XG(1)
        XR   = ALOG10(FHRAD) - XX(1)
        XDX  = XX(2) - XX(1)
        GTUC = XG(1) + XN4*XR/XDX
        GTUC = 10.**GTUC
C
C       *** gtuc is uncorrected gt in lb/sec/ft ***
C
C       *** next apply fine sediment load and temperature
C                                             corrections ***
C
C       if (tmpr .ne. 60.) go to 210
        IF ((ABS(TMPR-60.)) .GT. 1.0E-5) GO TO 210
          CFT= 1.0
          GO TO 250
 210    CONTINUE
          IT1 = 0
          IT2 = 0
          DO 230 I= 1,6
            IF ((TMPR .LT. TEMP(I)) .OR.
     $      (TMPR .GT. TEMP(I+1))) GO TO 220
              IT1 = I
              IT2 = I+1
              GO TO 240
 220        CONTINUE
 230      CONTINUE
 240      CONTINUE
          XT(1,1) = ALOG10(T(IT1,ID1))
          XT(2,1) = ALOG10(T(IT2,ID1))
          XT(1,2) = ALOG10(T(IT1,ID2))
          XT(2,2) = ALOG10(T(IT2,ID2))
          XNT     = ALOG10(TMPR/TEMP(IT1))/ALOG10(TEMP(IT2)/TEMP(IT1))
          XCT(1)  = XT(1,1) + XNT*(XT(2,1) - XT(1,1))
          XCT(2)  = XT(1,2) + XNT*(XT(2,2) - XT(1,2))
          CFT     = XCT(1) + (XCT(2) - XCT(1))*XR/XDX
          CFT     = 10.**CFT
 250    CONTINUE
C
C       *** fine sediment load correction ***
C
        IF (FSL .GT. 10.) GO TO 260
          CFF= 1.0
          GO TO 350
 260    CONTINUE
          ID1 = 0
          ID2 = 0
          DO 280 I= 1,9
            IF((FHRAD .LT. DF(I)) .OR.
     $      FHRAD .GT. DF(I+1)) GO TO 270
              ID1 = I
              ID2 = I+1
              GO TO 290
 270        CONTINUE
 280      CONTINUE
 290      CONTINUE
          IF (FSL .LE. 1.E+5) GO TO 300
            WRITE(MESSU,2000)
            IF1 = 4
            IF2 = 5
          GO TO 340
 300      CONTINUE
            IF1 = 0
            IF2 = 0
            DO 320 I= 1,4
              IF ((FSL .LT. CF(I)) .OR.
     $        (FSL .GT. CF(I+1))) GO TO 310
                IF1 = I
                IF2 = I+1
                GO TO 330
 310          CONTINUE
 320        CONTINUE
 330        CONTINUE
 340      CONTINUE
          XF(1,1) = ALOG10(F(IF1,ID1))
          XF(2,2) = ALOG10(F(IF2,ID2))
          XF(1,2) = ALOG10(F(IF1,ID2))
          XF(2,1) = ALOG10(F(IF2,ID1))
          XNT     = (FSL - CF(IF1))/(CF(IF2) - CF(IF1))
          XCT(1)  = XF(1,1) + XNT*(XF(2,1) - XF(1,1))
          XCT(2)  = XF(1,2) + XNT*(XF(2,2) - XF(1,2))
          XNT     = ALOG10(FHRAD/DF(ID1))/ALOG10(DF(ID2)/DF(ID1))
          CFF     = XCT(1) + XNT*(XCT(2) - XCT(1))
          CFF     = 10.**CFF
 350    CONTINUE
        TCF = CFT * CFF - 1.0
        CFD = 1.
        IF ((DB50 .GE. 0.20) .AND. (DB50 .LE. 0.30)) GO TO 390
          IP1 = 0
          IP2 = 0
          DO 370 I= 1,10
            IF ((DB50 .LT. DP(I)) .OR.
     $      (DB50 .GT. DP(I+1))) GO TO 360
              IP1 = I
              IP2 = I+1
              GO TO 380
 360        CONTINUE
 370      CONTINUE
 380      CONTINUE
          P2  = ALOG10(P(IP2))
          P1  = ALOG10(P(IP1))
          XNT = ALOG10(DB50/DP(IP1))/ALOG10(DP(IP2)/DP(IP1))
          CFD = P1 + XNT * (P2 -P1)
          CFD = 10.**CFD
 390    CONTINUE
        FFF = CFD * TCF
        FFF = FFF + 1.0
        GSI = FFF * GTUC
C
 400  CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   SANDLD
     I                    (ISAND,VOLS,SROVOL,VOL,EROVOL,SOVOL,EOVOL,
     I                     NEXITS,KSAND,AVVELE,EXPSND,ROM,SANDFG,DB50E,
     I                     HRADE,SLOPE,TW,WSANDE,TWIDE,DB50M,FSL,RCHNO,
     I                     MESSU,MSGFL,DATIM,AVDEPE,
     M                     SAND,RSAND,BDSAND,SDWCT2,
     O                     DEPSCR,ROSAND,OSAND)
C
C     + + + PURPOSE + + +
C     Simulate behavior of sand/gravel
C     variables are r4 unless otherwise stated
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MESSU,NEXITS,RCHNO,SANDFG,SDWCT2(2),MSGFL,DATIM(5)
      REAL        AVVELE,BDSAND,DB50E,DB50M,DEPSCR,EOVOL(5),
     $            EROVOL,EXPSND,FSL,HRADE,ISAND,KSAND,OSAND(5),
     $            ROM,ROSAND,RSAND,SAND,SLOPE,SOVOL(5),SROVOL,
     $            TW,TWIDE,WSANDE,AVDEPE
      DOUBLE PRECISION VOL,VOLS
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ISAND  - ???
C     VOLS   - ???
C     SROVOL - ???
C     VOL    - volume of water in reach above bed
C     EROVOL - ???
C     SOVOL  - ???
C     EOVOL  - ???
C     NEXITS - number of exits from the operation
C     KSAND  - ???
C     AVVELE - ???
C     EXPSND - ???
C     ROM    - ???
C     SANDFG - ???
C     DB50E  - ???
C     HRADE  - ???
C     SLOPE  - ???
C     TW     - water temperature in degrees C
C     WSANDE - ???
C     TWIDE  - ???
C     DB50M  - ???
C     FSL    - ???
C     RCHNO  - ???
C     MESSU  - ftn unit no. to be used for printout of messages
C     MSGFL  - fortran unit number of HSPF message file
C     AVDEPE - ???
C     SAND   - ???
C     RSAND  - ???
C     BDSAND - ???
C     SDWCT2 - ???
C     DEPSCR - ???
C     ROSAND - ???
C     OSAND  - ???
C     DATIM  - date and time
C
C     + + + LOCAL VARIABLES + + +
      INTEGER           D50ERR,FERROR,HRERR,N,VELERR,SCLU,SGRP
      REAL              GSI,PROSND,PSAND,PSCOUR,SANDS,SCOUR
C
C     + + + EXTERNALS + + +
      EXTERNAL   OMSTI,OMSTD,OMSG,OMSTR,COLBY,TOFFAL
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU = 345
C     save starting concentration value
      SANDS= SAND
C
      IF (VOL .GT. 0.0) THEN
C
C       rchres contains water
C
        IF (ROM .GT. 0.0 .AND. AVDEPE .GT. 0.17) THEN
C
C         there is outflow from the rchres- perform advection
C
C         calculate potential value of sand
C
C         casentry sandfg
          GO TO (10,20,50) , SANDFG
C
C         case 1 toffaleti equation
 10         CONTINUE
            CALL TOFFAL
     I                  (AVVELE,DB50E,HRADE,SLOPE,TW,WSANDE,
     O                   GSI)
C           convert potential sand transport rate to a concentration
C           in mg/l
            PSAND= (GSI*TWIDE*10.5)/ROM
            GO TO 60
C
C         case 2 colby equation
 20         CONTINUE
            CALL COLBY
     I                 (AVVELE,DB50M,HRADE,FSL,MESSU,TW,
     O                  GSI,FERROR,D50ERR,HRERR,VELERR)
C
            IF (FERROR .EQ. 1) THEN
C             fatal error ocurred in colby method- one or more
C             variables went outside valid range- warn and switch to
C             toffaleti method
C
              CALL OMSTD (DATIM)
              CALL OMSTI (RCHNO)
              CALL OMSTR (DB50M)
              CALL OMSTR (HRADE)
              CALL OMSTR (AVVELE)
              SGRP = 3
              CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                   SDWCT2(2))
C
C             switch to toffaleti method
C
              CALL TOFFAL
     I                    (AVVELE,DB50E,HRADE,SLOPE,TW,WSANDE,
     O                     GSI)
            END IF
C
C           convert potential sand transport rate to conc in mg/l
            PSAND= (GSI*TWIDE*10.5)/ROM
            GO TO 60
C
C         case 3 input power function
 50         CONTINUE
            PSAND= KSAND*AVVELE**EXPSND
C
 60       CONTINUE
C         endcase
C
C         calculate potential outflow of sand during ivl
C
          PROSND= (SANDS*SROVOL) + (PSAND*EROVOL)
C
C         calculate potential scour from, or to deposition , bed
C         storage
C         scour is expressed as qty.vol/l.ivl
C
          PSCOUR= (VOL*PSAND) - (VOLS*SANDS) + PROSND - ISAND
C
          IF (PSCOUR .LT. BDSAND) THEN
C           potential scour is satisfied by bed storage; new conc.
C           of sandload is potential conc.
C
            SCOUR = PSCOUR
            SAND  = PSAND
            RSAND = SAND * VOL
            BDSAND= BDSAND - SCOUR
          ELSE
C           potential scour cannot be satisfied; all of the
C           available bed storage is scoured
C
            SCOUR = BDSAND
            BDSAND= 0.0
C
C           calculate new conc. of suspended sandload
C
            SAND= (ISAND + SCOUR + SANDS*(VOLS - SROVOL))/(VOL+EROVOL)
C
C           calculate new storage of suspended sandload
            RSAND= SAND * VOL
          END IF
C
C         calculate total amount of sand leaving rchres during ivl
C
          ROSAND= SROVOL*SANDS + EROVOL*SAND
C
          IF (NEXITS .GT. 1) THEN
C           calculate amount of sand leaving through each exit gate;
C           osand is expressed as qty.vol/l.ivl
C
            DO 90 N=1,NEXITS
              OSAND(N)= SOVOL(N)*SANDS + EOVOL(N)*SAND
 90         CONTINUE
          END IF
        ELSE
C         no outflow (still water) or water depth less than two inches
          SAND  = 0.0
          RSAND = 0.0
          SCOUR = -ISAND - (SANDS*VOLS)
          BDSAND= BDSAND - SCOUR
          ROSAND= 0.0
          DO 120 N=1,5
            OSAND(N)=0.0
 120      CONTINUE
        END IF
      ELSE
C       rchres is dry; set sand equal to an undefined number
C
        SAND = -1.0E30
        RSAND= 0.0
C
C       calculate total amount of sand settling out during interval;
C       this is equal to sand inflow + sand initially present
C
        SCOUR= -ISAND - (SANDS*VOLS)
C
C       update bed storage
C
        BDSAND= BDSAND - SCOUR
C
C       assume zero outflow of sand
C
        ROSAND= 0.0
        DO 150 N=1,5
          OSAND(N)=0.0
 150    CONTINUE
      END IF
C
C     calculate depth of bed scour or deposition; positive for
C     deposition
      DEPSCR= -SCOUR
C
      RETURN
      END
C
C
C
      SUBROUTINE   SEDACC
     I                    (FRMROW,TOROW)
C
C     + + + PURPOSE + + +
C     Accumulate fluxes in module section sedtrn for printout
C     handle flux groups dealing with reach-wide variables
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    FRMROW,TOROW
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FRMROW - row containing incremental flux accumulation
C     TOROW  - flux row to be incremented
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION SED2 + + +
      INCLUDE    'crhse.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,I4
C
C     + + + EXTERNALS + + +
      EXTERNAL  ACCVEC
C
C     + + + END SPECIFICATIONS + + +
C
      I4= 4
      CALL ACCVEC
     I            (I4,SDIF(1,FRMROW),
     M             SDIF(1,TOROW))
      CALL ACCVEC
     I            (I4,SDCF1(1,FRMROW),
     M             SDCF1(1,TOROW))
      CALL ACCVEC
     I            (I4,SDCF2(1,FRMROW),
     M             SDCF2(1,TOROW))
C
      IF (NEXITS .GT. 1) THEN
C       handle flux groups dealing with individual exit gates
        DO 10 I=1,4
          CALL ACCVEC
     I                (NEXITS,SDCF3(1,I,FRMROW),
     M                 SDCF3(1,I,TOROW))
 10     CONTINUE
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   SEDPRT
     I                    (UNITFG,LEV,PRINTU,BINU)
C
C     + + + PURPOSE + + +
C     Convert quantities from internal to external units, calculate
C     materials balance and print out results
C     Note: unless otherwise stated, local arrays have same dimensions
C      as corresponding arrays in osv, except for dropping of dimension
C      lev, where applicable
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
C     + + + COMMON BLOCKS- SCRTCH, VERSION SED2 + + +
      INCLUDE    'crhse.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,IX,I0,I1,I2,I3,I4,I10,J,L,JLEN,ACNT,
     $           CLEN(29+NEXITS*4),EXDAT(5)
												
      REAL       DIF,FACTA,FACTB,LFACTA,PBEDDP,PCFLX1(4),PCFLX2(4),
     $           PCFLX3(5,4),PIFLX(4),PRSD(3),PRSDS(3),PRSED(10),
     $           SSEDST,BSEDST,APRINT(29+NEXITS*4)
      CHARACTER*1   CSTR(2)
      CHARACTER*16  UNITID,CSSED(4),CRSED(10),CCFLX3(4),CCFLX1(4),
     $              CCFLX2(4),CIFLX(4)
      CHARACTER*256 CHEAD(29+NEXITS*4)
C
C     + + + EXTERNALS + + +
      EXTERNAL   TRNVEC,BALCHK,INTCHR,EXDATE
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' *** SEDTRN ***')
 2010 FORMAT (/,'   STATE VARIABLES')
 2020 FORMAT ( 47X,'SAND      SILT      CLAY     TOTAL')
 2030 FORMAT (  5X,'CONC. IN SUSPENSION (MG/L)',10X,4G10.3)
 2040 FORMAT (/,5X,'STORAGES (TONS)',27X,'SAND      SILT      CLAY',
     $        '     TOTAL')
 2050 FORMAT (/,5X,'STORAGES (TONNES)',25X,'SAND      SILT      CLAY',
     $        '     TOTAL')
 2060 FORMAT ( 14X,'SUSPENSION',17X,1P4E10.3)
 2070 FORMAT ( 14X,'BED',24X,1P4E10.3)
 2080 FORMAT ( 14X,'TOTAL',22X,1P4E10.3)
 2090 FORMAT (/,5X,'BED DEPTH (FT)',22X,1F10.2)
 2100 FORMAT (/,3X,'FLUXES (TONS)',31X,'SAND      SILT      CLAY',
     $        '     TOTAL')
 2110 FORMAT (/,5X,'BED DEPTH (M)',23X,1F10.2)
 2120 FORMAT (/,3X,'FLUXES (TONNES)',29X,'SAND      SILT      CLAY',
     $        '     TOTAL')
 2130 FORMAT (/,5X,'TOTAL INFLOWS',23X,1P4E10.3)
 2140 FORMAT (  5X,'DEPOSITION/SCOUR',20X,1P4E10.3)
 2150 FORMAT (  5X,'TOTAL OUTFLOWS',22X,1P4E10.3)
 2160 FORMAT (/,5X,'OUTFLOWS FOR EACH EXIT:')
 2170 FORMAT (  7X,'EXIT',I2,28X,1P4E10.3)
 2180 FORMAT (/,31X,'FOR SAND, SILT, CLAY')
C
C     + + + END SPECIFICATIONS + + +
C
      I0= 0
      I1= 1
      I2= 2
      I3 = 3
      I4 = 4
      I10= 10
C
C     initialize array counter for binary printout, store variable
C     names in local strings for use in building binary headers
      ACNT = 0
      CSSED(1)  = 'SSED-SAND'
      CSSED(2)  = 'SSED-SILT'
      CSSED(3)  = 'SSED-CLAY'
      CSSED(4)  = 'SSED-TOT'
      CRSED(1)  = 'RSED-SUSP-SAND'
      CRSED(2)  = 'RSED-SUSP-SILT'
      CRSED(3)  = 'RSED-SUSP-CLAY'
      CRSED(4)  = 'RSED-BED-SAND'
      CRSED(5)  = 'RSED-BED-SILT'
      CRSED(6)  = 'RSED-BED-CLAY'
      CRSED(7)  = 'RSED-TOT-SAND'
      CRSED(8)  = 'RSED-TOT-SILT'
      CRSED(9)  = 'RSED-TOT-CLAY'
      CRSED(10) = 'RSED-TOT-TOT'
      CCFLX3(1) = 'OSED-SAND'
      CCFLX3(2) = 'OSED-SILT'
      CCFLX3(3) = 'OSED-CLAY'
      CCFLX3(4) = 'OSED-TOT'
      CIFLX(1)  = 'ISED-SAND'
      CIFLX(2)  = 'ISED-SILT'
      CIFLX(3)  = 'ISED-CLAY'
      CIFLX(4)  = 'ISED-TOT'
      CCFLX1(1) = 'DEPSCOUR-SAND'
      CCFLX1(2) = 'DEPSCOUR-SILT'
      CCFLX1(3) = 'DEPSCOUR-CLAY'
      CCFLX1(4) = 'DEPSCOUR-TOT'
      CCFLX2(1) = 'ROSED-SAND'
      CCFLX2(2) = 'ROSED-SILT'
      CCFLX2(3) = 'ROSED-CLAY'
      CCFLX2(4) = 'ROSED-TOT'
C
C     Convert quantities from internal to external units, calculate
C     Materials balance and print out results
C
C       assign values to parameters used for conversion from internal to
C       external units
C
      IF (UNITFG .EQ. 1) THEN
C       printout is in english system
        IF (UUNITS .EQ. 1) THEN
C         english to english
C
          FACTA = 3.115E-08
          FACTB = 0.0
          LFACTA= 1.0
        ELSE
C         metric to english
          FACTA = 1.1E-06
          FACTB = 0.0
          LFACTA= 3.28
        END IF
      ELSE
C       printout is in metric system
        IF (UUNITS .EQ. 1) THEN
C         english to metric
          FACTA = 2.83E-08
          FACTB = 0.0
          LFACTA= 0.305
        ELSE
C         metric to metric
          FACTA = 1.0E-06
          FACTB = 0.0
          LFACTA= 1.0
        END IF
      END IF
C
C     convert variables to external units
C
C     rchres-wide variables
C
C     state variables
C
      CALL TRNVEC
     I            (I10,RSED(1),FACTA,FACTB,
     O             PRSED(1))
C
      SSEDST= PRSED(1)+ PRSED(2)+ PRSED(3)
      BSEDST= PRSED(4)+ PRSED(5)+ PRSED(6)
      PBEDDP= BEDDEP*LFACTA
C
C     inflow fluxes
C
      CALL TRNVEC
     I            (I4,SDIF(1,LEV),FACTA,FACTB,
     O             PIFLX(1))
C
C     storages
C
      PRSD(1)= SDST(1,1)*FACTA+ FACTB
      PRSD(2)= SDST(2,1)*FACTA+ FACTB
      PRSD(3)= SDST(3,1)*FACTA+ FACTB
C
      PRSDS(1)= SDST(1,LEV)*FACTA+ FACTB
      PRSDS(2)= SDST(2,LEV)*FACTA+ FACTB
      PRSDS(3)= SDST(3,LEV)*FACTA+ FACTB
C
C     computed fluxes
C
      CALL TRNVEC
     I            (I4,SDCF1(1,LEV),FACTA,FACTB,
     O             PCFLX1(1))
C
      CALL TRNVEC
     I            (I4,SDCF2(1,LEV),FACTA,FACTB,
     O             PCFLX2(1))
C
      IF (NEXITS .GT. 1) THEN
C
C       exit-specific variables
C
        DO 10 I=1,4
          CALL TRNVEC
     I                (NEXITS,SDCF3(1,I,LEV),FACTA,FACTB,
     O                 PCFLX3(1,I))
 10     CONTINUE
      END IF
C
C     do printout on unit printu
C
      IF (PRINTU .GT. 0 .AND. PFLAG(5) .LE. LEV) THEN
        WRITE (PRINTU,2000)
C
        WRITE (PRINTU,2010)
        WRITE (PRINTU,2020)
        WRITE (PRINTU,2030) SSED
C
        IF (UNITFG .EQ. 1) THEN
C         english
          WRITE (PRINTU,2040)
        ELSE
C         metric
          WRITE (PRINTU,2040)
        END IF
C
        WRITE (PRINTU,2060) (PRSED(L),L=1,3),SSEDST
        WRITE (PRINTU,2070) (PRSED(L),L=4,6),BSEDST
        WRITE (PRINTU,2080) (PRSED(L),L=7,10)
C
        IF (UNITFG .EQ. 1) THEN
C         english
          WRITE (PRINTU,2090) PBEDDP
          WRITE (PRINTU,2100)
        ELSE
C         metric
          WRITE (PRINTU,2110) PBEDDP
          WRITE (PRINTU,2120)
        END IF
C
        WRITE (PRINTU,2130) PIFLX
        WRITE (PRINTU,2140) PCFLX1
        WRITE (PRINTU,2150) PCFLX2
      END IF
C
      IF (BINU .GT. 0 .AND. ABS(BFLAG(5)) .LE. LEV) THEN
        DO 20 I= 1, 4
          ACNT = ACNT + 1
          APRINT(ACNT) = SSED(I)
          CHEAD(ACNT) = CSSED(I)
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 20     CONTINUE
        DO 30 I= 1, 3
          ACNT = ACNT + 1
          APRINT(ACNT) = PRSED(I)
          CHEAD(ACNT) = CRSED(I)
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 30     CONTINUE
        ACNT = ACNT + 1
        APRINT(ACNT) = SSEDST
        CHEAD(ACNT) = 'RSED-SUSP-TOT'
        CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
        DO 40 I= 4, 6
          ACNT = ACNT + 1
          APRINT(ACNT) = PRSED(I)
          CHEAD(ACNT) = CRSED(I)
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 40     CONTINUE
        ACNT = ACNT + 1
        APRINT(ACNT) = BSEDST
        CHEAD(ACNT) = 'RSED-BED-TOT'
        CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
        DO 50 I= 7, 10
          ACNT = ACNT + 1
          APRINT(ACNT) = PRSED(I)
          CHEAD(ACNT) = CRSED(I)
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 50     CONTINUE
        ACNT = ACNT + 1
        APRINT(ACNT) = PBEDDP
        CHEAD(ACNT) = 'BEDDEP'
        CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
        DO 60 I= 1, 4
          ACNT = ACNT + 1
          APRINT(ACNT) = PIFLX(I)
          CHEAD(ACNT) = CIFLX(I)
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 60     CONTINUE
        DO 70 I= 1, 4
          ACNT = ACNT + 1
          APRINT(ACNT) = PCFLX1(I)
          CHEAD(ACNT) = CCFLX1(I)
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 70     CONTINUE
        DO 80 I= 1, 4
          ACNT = ACNT + 1
          APRINT(ACNT) = PCFLX2(I)
          CHEAD(ACNT) = CCFLX2(I)
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 80     CONTINUE
      END IF
C
      IF (NEXITS .GT. 1) THEN
C       write outflows by exit
        IF (PRINTU .GT. 0 .AND. PFLAG(5) .LE. LEV) THEN
          WRITE (PRINTU,2160)
          DO 130 I= 1, NEXITS
            WRITE (PRINTU,2170) I,(PCFLX3(I,J),J=1,4)
 130      CONTINUE
        END IF
      END IF
      IF (BINU .GT. 0 .AND. ABS(BFLAG(5)) .LE. LEV) THEN
        DO 160 I= 1, NEXITS
          CALL INTCHR (I, I2, I1,
     O                 JLEN, CSTR)
          DO 150 J= 1, 4
            ACNT = ACNT + 1
            APRINT(ACNT) = PCFLX3(I,J)
            CHEAD(ACNT) = TRIM(CCFLX3(J)) // '-EXIT'
            DO 140 IX= 1, JLEN
              CHEAD(ACNT) = TRIM(CHEAD(ACNT)) // CSTR(IX)
 140        CONTINUE
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 150      CONTINUE
 160    CONTINUE
      END IF
C
C     material balance check
C
      IF (PRINTU .GT. 0 .AND. PFLAG(5) .LE. LEV) THEN
        WRITE (PRINTU,2180)
      END IF
      IF (UNITFG .EQ. 1) THEN
C       english
        UNITID= '    TONS'
      ELSE
C       metric
        UNITID= '  TONNES'
      END IF
C
      DO 170 I= 1, 3
C
C       calculate net quantity of material entering rchres
        DIF= PIFLX(I) - PCFLX2(I)
C
        CALL BALCHK
     I              (I3,RCHNO,DATIM,MESSU,PRINTU,MSGFL,
     I               PRSDS(I),PRSD(I),PIFLX(I),DIF,UNITID,I,
     M               SDWCT1(1,I))
 170  CONTINUE
      IF (BINU .GT. 0 .AND. ABS(BFLAG(5)) .LE. LEV) THEN
        CALL EXDATE(
     I              DATIM,
     O              EXDAT)
C       write binary output
        IF (BFLAG(5) .GT. 0) THEN
C         at start of run, write the header
          WRITE (BINU) I0,'RCHRES  ',RCHNO,'SEDTRN  ',
     1          (CLEN(I),(CHEAD(I)(J:J),J=1,CLEN(I)),I=1,ACNT)
C         set bflag to negative to not write headers anymore
          BFLAG(5) = -BFLAG(5)
        END IF
        WRITE (BINU) I1,'RCHRES  ',RCHNO,'SEDTRN  ',UNITFG,
     1               LEV,(EXDAT(J),J=1,5),(APRINT(J),J=1,ACNT)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   SEDRB
C
C     + + + PURPOSE + + +
C     Handle section sedtrn
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION SED2 + + +
      INCLUDE    'crhse.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,J
C
C     + + + END SPECIFICATIONS + + +
C
      DO 10 I= 1, 4
        IF (DEPSFP(I) .GE. 1) THEN
          PAD(DEPSFP(I)+ IVL1)= DEPSCR(I)
        END IF
        IF (ROSDFP(I) .GE. 1) THEN
          PAD(ROSDFP(I)+ IVL1)= ROSED(I)
        END IF
        IF (RCSDFP(I) .GE. 1) THEN
          PAD(RCSDFP(I)+ IVL1)= ISED(I)
        END IF
 10   CONTINUE
C
      IF (NEXITS .GT. 1) THEN
        DO 30 J= 1,4
          DO 20 I= 1,NEXITS
C           quantities belonging to exit i
            IF (OSEDFP(I,J) .GE. 1) THEN
              PAD(OSEDFP(I,J) + IVL1)= OSED(I,J)
            END IF
 20       CONTINUE
 30     CONTINUE
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   SEDRP
C
C     + + + PURPOSE + + +
C     Handle section sedtrn
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION SED2 + + +
      INCLUDE    'crhse.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I
C
C     + + + END SPECIFICATIONS + + +
C
      DO 10 I= 1,4
        IF (SSEDFP(I) .GE. 1) THEN
          PAD(SSEDFP(I) + IVL1)= SSED(I)
        END IF
 10   CONTINUE
      DO 20 I= 1,10
        IF (RSEDFP(I) .GE. 1) THEN
          PAD(RSEDFP(I) + IVL1)= RSED(I)
        END IF
 20   CONTINUE
      DO 30 I= 1,3
        IF (TSEDFP(I) .GE. 1) THEN
          PAD(TSEDFP(I) + IVL1)= TSED(I)
        END IF
 30   CONTINUE
      IF (BDDPFP .GE. 1) THEN
        PAD(BDDPFP + IVL1)= BEDDEP
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   SEDRST
     I                    (LEV)
C
C     + + + PURPOSE + + +
C     Reset flux and state variables for module section sedtrn
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    LEV
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEV    - current output level (2-pivl,3-day,4-mon,5-ann)
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION SED2 + + +
      INCLUDE    'crhse.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,I4
      REAL       RZERO
C
C     + + + EXTERNALS + + +
      EXTERNAL   SETVEC
C
C     + + + END SPECIFICATIONS + + +
C
C     handle flux groups dealing with reach-wide variables
C
      RZERO= 0.0
      I4=4
      CALL SETVEC
     I            (I4,RZERO,
     O             SDIF(1,LEV))
      CALL SETVEC
     I            (I4,RZERO,
     O             SDCF1(1,LEV))
      CALL SETVEC
     I            (I4,RZERO,
     O             SDCF2(1,LEV))
C
      IF (NEXITS .GT. 1) THEN
C       handle flux groups dealing with individual exit gates
        DO 10 I=1,4
          CALL SETVEC
     I                (NEXITS,RZERO,
     O                 SDCF3(1,I,LEV))
 10     CONTINUE
      END IF
C
C     keep present sediment storages in state variable array used
C     for material balance check
      DO 30 I=1,3
        SDST(I,LEV)= SDST(I,1)
 30   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   TOFFAL
     I                    (V,FDIAM,FHRAD,SLOPE,TEMPR,VSET,
     O                     GSI)
C
C     + + + PURPOSE + + +
C     This subroutine uses toffaleti's method to calculate the capacity
C     of the flow to transport sand.
C      called by: sandld
C
C     + + + DUMMY ARGUMENTS + + +
      REAL       FDIAM,FHRAD,GSI,SLOPE,TEMPR,V,VSET
C
C     + + + ARGUMENT DEFINITIONS + + +
C     V     - average velocity of flow (ft/s)
C     FDIAM - median bed sediment diameter (ft)
C     FHRAD - hydraulic radius (ft)
C     SLOPE - energy or river bed slope
C     TEMPR - water temperature (deg c)
C     VSET  - settling velocity (ft/s)
C     GSI   - total capacity of the rchres (tons/day.ft)
C
C     + + + LOCAL VARIABLES + + +
      REAL       AC,ACK4,AFUNC,CLI,CMI,CNV,CZ,C2D,D65,FD11,
     $           FD25,GSB,GSL,GSM,GSU,K4,K4FUNC,OCZL,OCZM,
     $           OCZU,P1,RPRIME,TMPR,TT,USTAR,VIS,ZI,ZINV,ZM,
     $           ZN,ZO,ZO2,ZP,ZQ
C
C     + + + END SPECIFICATIONS + + +
C
C     Convert water temp from degrees c to degrees f
C
      TMPR= TEMPR*1.80 + 32.0
C
C
C     For water temperatures greater than 32f and less than 100f
C     The kinematic viscosity can be written as the following:
C
      VIS= 4.106E-4*(TMPR**(-0.864))
C
C     Assuming the d50 grain size is approximately equal to the
C     Geometric mean grain size and sigma-g = 1.5, the d65 grain
C     Size can be determined as 1.17*d50.
C
      D65= 1.17*FDIAM
      CNV= 0.1198 + 0.00048*TMPR
      CZ = 260.67 - 0.667*TMPR
      TT = 1.10*(0.051 + 0.00009*TMPR)
      ZI = VSET*V/(CZ*FHRAD*SLOPE)
      IF (ZI .LT. CNV) THEN
        ZI= 1.5*CNV
      END IF
C
C     The manning-strickler equation is used here to
C     Determine the hydraulic radius component due to
C     Grain roughness (r').  taken from the 1975 asce
C     "sedimentation engineering",pg. 128.
C
      RPRIME= ((V**1.5)*(D65**0.25)/(SLOPE**0.75))*0.00349
      USTAR = (RPRIME*SLOPE*32.2)**0.5
      AFUNC = (VIS*1.0E5)**0.333/(10.0*USTAR)
      IF (AFUNC .LE. 0.500) THEN
        AC= (AFUNC/4.89)** (-1.45)
      ELSE IF (AFUNC .LE. 0.660) THEN
        AC= (AFUNC/0.0036)**0.67
      ELSE IF (AFUNC .LE. 0.720) THEN
        AC= (AFUNC/0.29)**4.17
      ELSE IF (AFUNC .LE. 1.25) THEN
        AC= 48.0
      ELSE IF (AFUNC .GT. 1.25) THEN
        AC= (AFUNC/0.304)**2.74
      END IF
C
      K4FUNC= AFUNC*SLOPE*D65*1.0E5
      IF (K4FUNC .LE.  0.24) THEN
        K4= 1.0
      ELSE IF (K4FUNC .LE.  0.35) THEN
        K4= (K4FUNC**1.10)*4.81
      ELSE IF (K4FUNC .GT.  0.35) THEN
        K4= (K4FUNC** (-1.05))*0.49
      END IF
C
      ACK4= AC*K4
      IF (ACK4 - 16.0 .LT. 0.0) THEN
         ACK4= 16.0
         K4= 16.0/AC
      END IF
      OCZU= 1.0 + CNV - 1.5*ZI
      OCZM= 1.0 + CNV - ZI
      OCZL= 1.0 + CNV - 0.756*ZI
      ZINV= CNV - 0.758*ZI
      ZM  = -ZINV
      ZN  = 1.0 + ZINV
      ZO  = -0.736*ZI
      ZP  = 0.244*ZI
      ZQ  = 0.5*ZI
C
C     Cli has been multiplied by 1.0e30 to keep it from
C     Exceeding the computer overflow limit
C
      CLI= 5.6E+22*OCZL*(V**2.333)/FHRAD**(ZM)/
     $     ((TT*AC*K4*FDIAM)**1.667)/(1.0 + CNV)/
     $     ((FHRAD/11.24)**(ZN) - (2.0*FDIAM)**OCZL)
C
      ZO2= ZO/2.0
      P1 = (2.0*FDIAM/FHRAD)**ZO2
      C2D= CLI*P1
      C2D= C2D*P1/1.0E+30
C
C     Check to see if the calculated value is reasonable
C     (< 100.0), and adjust it if it is not.
C
      IF (C2D .GT. 100.0) THEN
        CLI= CLI*100.0/C2D
      END IF
C
C
C     Cmi has been multiplied by 1.0e30 to keep it from
C     Exceeding the computer overflow limit
C
      CMI = 43.2*CLI*(1.0 + CNV)*V*(FHRAD**(ZM))
C
C     Calculate transport capacity of the upper layer
C
      FD11= FHRAD/11.24
      FD25= FHRAD/2.5
      GSU = (CMI*(FD11**(ZP))*(FD25**(ZQ))*
     $     (FHRAD**(OCZU) - (FD25**(OCZU))))/(OCZU*1.0E+30)
C
C     Calculate the capacity of the middle layer
C
      GSM = (CMI*(FD11**(ZP))*((FD25**(OCZM)) -
     $     (FD11**(OCZM))))/(OCZM*1.0E+30)
C
C     Calculate the capacity of the lower layer
C
      GSL = (CMI*((FD11**(ZN)) - ((2.0*FDIAM)**(OCZL))))
     $     /(OCZL*1.0E+30)
C
C     Calculate the capacity of the bed layer
C
      GSB = (CMI*((2.0*FDIAM)**(ZN)))/1.0E+30
C
C     Total capacity of the rchres (gsi has units of tons/day/ft)
C
      GSI = GSU + GSM + GSL + GSB
C
      IF (GSI .LE. 0.0) THEN
        GSI = 0.0
      END IF
C
      RETURN
      END
