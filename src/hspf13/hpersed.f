C
C
C
      SUBROUTINE   PSEDMT
C
C     + + + PURPOSE + + +
C     Process the input for section sedmnt
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION SEDMNT1 + + +
      INCLUDE    'cplse.inc'
      INCLUDE    'crin2.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I1,I2,I4,J
      REAL      RVAL(6)
C
C     + + + EXTERNALS + + +
      EXTERNAL  ITABLE,RTABLE
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' PROCESSING INPUT FOR SECTION SEDMNT')
 2010 FORMAT (/,' FINISHED PROCESSING INPUT FOR SECTION SEDMNT')
C
C     + + + END SPECIFICATIONS + + +
C
      I1=1
C
C     initialize variables not set in SDRST
      DO 10 J= 1, 5
        SDIF(J)= 0.0
 10   CONTINUE
C
      IF (OUTLEV.GT.1) THEN
C       processing message
        WRITE (MESSU,2000)
      END IF
C
      DRYDFG= 1
C
C     process values in table-type sed-parm1
      I2= 41
      I4= 3
      CALL ITABLE (I2,I1,I4,UUNITS,
     M             SEDPM1)
C
C     process values in table-type sed-parm2
      I2= 42
      I4= 6
      CALL RTABLE (I2,I1,I4,UUNITS,
     M             RVAL)
C
      SMPF = RVAL(1)
      KRER = RVAL(2)
      JRER = RVAL(3)
      AFFIX= RVAL(4)
      COVER= RVAL(5)
      NVSI = RVAL(6)*DELT/1440.
C
C     process values in table-type sed-parm3
      I2= 43
      I4= 4
      CALL RTABLE (I2,I1,I4,UUNITS,
     M             RVAL)
C
      KSER= RVAL(1)
      JSER= RVAL(2)
      KGER= RVAL(3)
      JGER= RVAL(4)
C
C     if necessary, set csnofg
      IF (PWATFG.EQ.0) THEN
C       csnofg not available from pwater
C        I2= 26
C        I4= 1
C        CALL ITABLE(I2,I1,I4,UUNITS,
C     M              CSNOFG)
        CSNOFG= 0
      END IF
C
      IF (CRVFG.EQ.1) THEN
C       get monthly values of erosion-related cover -
C       table-type mon-cover
        I2= 44
        I4= 12
        CALL RTABLE (I2,I1,I4,UUNITS,
     M               COVERM)
      END IF
C
      IF (VSIVFG.NE.0) THEN
C       get monthly values of net vertical sediment
C       input - table-type mon-nvsi
        I2= 45
        I4= 12
        CALL RTABLE (I2,I1,I4,UUNITS,
     M               NVSIM)
C       convert to internal units
        DO 20 J= 1,12
          NVSIM(J)= NVSIM(J)*DELT/1440.
 20     CONTINUE
      END IF
C
C     initial detached storage - table-type sed-stor
      I2= 46
      CALL RTABLE (I2,I1,I1,UUNITS,
     M             DETS)
C
C     initialize sediment transport capacity
      IF (SDOPFG .EQ. 1) THEN
        STCAP= DELT60*KSER*(SURS/DELT60)**JSER
      ELSE
        STCAP= 0.0
      END IF
C
      IF (OUTLEV.GT.1) THEN
C       end processing message
        WRITE (MESSU,2010)
      END IF
C
      RETURN
      END
C
C     4.2(1).4
C
      SUBROUTINE SEDMNT
C
C     + + + PURPOSE + + +
C     Produce and remove sediment from the land surface
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION SEDMNT2 + + +
      INCLUDE    'cplse.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     REQFG,TSSUB(2),FLGVAL
      REAL        DUMMY,PREC,RAIN
      CHARACTER*6 OPTYP,TSNAM,SECNAM,MSECNM,OPFGNM
C
C     + + + FUNCTIONS + + +
      REAL       DAYVAL
C
C     + + + EXTERNALS + + +
      EXTERNAL   DAYVAL,DETACH,SOSED1,SOSED2,ATACH,HREQTS
C
C     + + + DATA INITIALIZATIONS + + +
      DATA TSSUB/1,1/
      DATA OPTYP,SECNAM/'PERLND','SEDMNT'/
C
C     + + + HISTORY + + +
C     2/19/2004   BRB - corrected NVSI error  
C
C     + + + END SPECIFICATIONS + + +
C
C     get input time series
CTHJ      PREC= PAD(PRECFP+IVL1)
      REQFG= 2
      TSNAM='PREC  '
      CALL HREQTS (PRECFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,LSNO,
     I             TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,FLGVAL,
     O             PREC)
      IF (CSNOFG.EQ.1) THEN
C       effects of snow are considered
        IF (SNOWFG.EQ.0) THEN
C         get rainfall and snow cover data from inpad
CTHJ          RAIN  = PAD(RNFFP+IVL1)
          REQFG= 3
          MSECNM= 'SNOW  '
          TSNAM='RAINF '
          CALL HREQTS (RNFFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,LSNO,
     I                 TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,FLGVAL,
     O                 RAIN)
CTHJ          SNOCOV= PAD(SNOCFP+IVL1)
          TSNAM='SNOCOV'
          CALL HREQTS (SNOCFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,LSNO,
     I                 TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,FLGVAL,
     O                 SNOCOV)
        ELSE
          RAIN= RAINF
C         snocov is available from snow
        END IF
      ELSE
C       all precipitation is assumed to be rain
        RAIN= PREC
C       snocov is not required
      END IF
C
      IF (SLSDFP.GT.0) THEN
        SLSED= PAD(SLSDFP+IVL1)
      ELSE
        SLSED= 0.0
      END IF
C
      IF (PWATFG.EQ.0) THEN
C       read time series supplied by pwater
CTHJ        SURO= PAD(SOFP+IVL1)
        MSECNM= 'PWATER'
        TSNAM='SURO  '
        CALL HREQTS (SOFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,LSNO,
     I               TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,FLGVAL,
     O               SURO)
CTHJ        SURS= PAD(SSFP+IVL1)
        TSNAM='SURS  '
        CALL HREQTS (SSFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,LSNO,
     I               TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,FLGVAL,
     O               SURS)
C
      ELSE
C       the above time series are available from pwater
      END IF
C
C     estimate the quantity of sediment particles detached from the
C     soil surface by rainfall and augment the detached sediment
C     storage
      CALL DETACH (DAYFG,CRVFG,COVERM,MON,NXTMON,DAY,NDAYS,RAIN,
     I             CSNOFG,SNOCOV,DELT60,SMPF,KRER,JRER,
     M             COVER,DETS,
     O             DET)
C
      IF (DAYFG.EQ.1) THEN
C       it is the first interval of the day
        IF (VSIVFG.NE.0) THEN
C         net vert. input values are allowed to vary throughout the
C         year
C         interpolate for the daily value
C         units are tons/acre-ivl
C         linearly interpolate nvsi between two values from the
C         monthly array nvsim(12)
          NVSI= DAYVAL(NVSIM(MON),NVSIM(NXTMON),DAY,NDAYS)
        ELSE
C         net vert. input values do not vary throughout the year.
C         nvsi value has been supplied by the run interpreter
        END IF
        IF (VSIVFG.EQ.2.AND.DRYDFG.EQ.1) THEN
C         last day was dry, add a whole days load in first interval
C         detailed output will show load added over the whole day.
          DUMMY = NVSI* (1440./DELT)
          DETS = DETS + DUMMY
        ELSE
C         dont accumulate until tomorrow, maybe
C-error:  NVSI = 0.0
C-NVSI should not be set to zero
C-BRB  2/19/2004
        END IF
      END IF
C
C     augment the detached sediment storage by external(vertical)
C     inputs of sediment - dets and detsb units are tons/acre
      DUMMY= SLSED
      IF (VSIVFG.LT.2) DUMMY = DUMMY + NVSI
C
      DETS= DETS+ DUMMY
C
C     washoff of detached sediment from the soil surface
      IF (SDOPFG.EQ.1) THEN
C       use method 1
        CALL SOSED1 (SURO,SURS,DELT60,KSER,JSER,KGER,JGER,
     M               DETS,
     O               WSSD,SCRSD,SOSED,STCAP)
C
      ELSE
C       use method 2
        CALL SOSED2 (SURO,DELT60,KSER,JSER,KGER,JGER,
     M               DETS,
     O               WSSD,SCRSD,SOSED,STCAP)
      END IF
C
C     attach detached sediment on the surface to the soil matrix
C     this code has been modified to allow dry day sed load
C     code modification for chesapeake bay
      IF (DAYFG.EQ.1) THEN
C       first interval of new day
        IF (DRYDFG.EQ.1) THEN
C         yesterday was dry, attach detached sediment
C         on the surface to the soil matrix
          CALL ATACH (AFFIX,
     M                DETS)
        END IF
C       assume today will be dry
        DRYDFG = 1
      END IF
C
      IF (PREC.GT.0.0) THEN
C       today is wet
        DRYDFG = 0
      END IF
C
      RETURN
      END
C
C     4.2(1).4.4
C
      SUBROUTINE ATACH
     I                  (AFFIX,
     M                   DETS)
C
C     + + + PURPOSE + + +
C     Simulate attachment or compaction of detached sediment on the
C     surface.  The calculation is done at the start of each day, if
C     the previous day was dry
C
C     + + + DUMMY ARGUMENTS + + +
      REAL       AFFIX,DETS
C
C     + + + ARGUMENT DEFINITIONS + + +
C     AFFIX  - ???
C     DETS   - ???
C
C     + + + END SPECIFICATIONS + + +
C
C     this subroutine was modified to allow optional
C     sed loading on dry days (chesapeake bay)
C     precipitation did not occur during the previous day
C     the attachment of surface sediment to the soil matrix is
C     taken into account by decreasing the storage of detached
C     sediment
      DETS= DETS*(1.0-AFFIX)
C
      RETURN
      END
C
C     4.2(1).4.1
C
      SUBROUTINE DETACH
     I                  (DAYFG,CRVFG,COVERM,MON,NXTMON,DAY,NDAYS,RAIN,
     I                   CSNOFG,SNOCOV,DELT60,SMPF,KRER,JRER,
     M                   COVER,DETS,
     O                   DET)
C
C     + + + PURPOSE + + +
C     Detach soil by rainfall
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    CRVFG,CSNOFG,DAY,DAYFG,MON,NDAYS,NXTMON
      REAL       COVER,COVERM(12),DELT60,DET,DETS,JRER,KRER,RAIN,SMPF,
     $           SNOCOV
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DAYFG  - flag for first day or day change
C     CRVFG  - ???
C     COVERM - ???
C     MON    - calendar month
C     NXTMON - next calendar month
C     DAY    - day of month
C     NDAYS  - no. of days in this month
C     RAIN   - ???
C     CSNOFG - ???
C     SNOCOV - ???
C     DELT60 - simulation time interval in hours
C     SMPF   - ???
C     KRER   - ???
C     JRER   - ???
C     COVER  - ???
C     DETS   - ???
C     DET    - ???
C
C     + + + LOCAL VARIABLES + + +
      REAL       CR
C
C     + + + FUNCTIONS + + +
      REAL       DAYVAL
C
C     + + + EXTERNALS + + +
      EXTERNAL   DAYVAL
C
C     + + + END SPECIFICATIONS + + +
C
      IF (DAYFG.EQ.1) THEN
C       it is the first interval of the day
        IF (CRVFG.EQ.1) THEN
C         erosion related cover is allowed to vary throughout the
C         year
C         interpolate for the daily value
C         linearly interpolate cover between two values from the
C         monthly array coverm(12)
          COVER= DAYVAL(COVERM(MON),COVERM(NXTMON),DAY,NDAYS)
        ELSE
C         erosion related cover does not vary throughout the year.
C         cover value has been supplied by the run interpreter
        END IF
      END IF
C
      IF (RAIN.GT.0.0) THEN
C       simulate detachment because it is raining
C       find the proportion of area shielded from raindrop impact by
C       snowpack and other cover
        IF (CSNOFG .EQ. 1) THEN
C         snow is being considered
          IF (SNOCOV .GT. 0.0) THEN
C           there is a snowpack
            CR= COVER+ (1.0- COVER)*SNOCOV
          ELSE
            CR= COVER
          END IF
        ELSE
          CR= COVER
        END IF
C
C       calculate the rate of soil detachment, delt60= delt/60 -
C       units are tons/acre-ivl
        DET= DELT60*(1.0-CR)*SMPF*KRER*(RAIN/DELT60)**JRER
C
C       augment detached sediment storage - units are tons/acre
        DETS= DETS+ DET
      ELSE
C       no rain - either it is snowing or it is "dry"
        DET= 0.0
      END IF
C
      RETURN
      END
C
C     4.2(1).15.1.3
C
      SUBROUTINE SDACC
     I                 (FRMROW,TOROW)
C
C     + + + PURPOSE + + +
C     Accumulate fluxes for section sedmnt
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    FRMROW,TOROW
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FRMROW - row containing incremental flux accumulation
C     TOROW  - flux row to be incremented
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION SEDMNT2 + + +
      INCLUDE    'cplse.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I2
C
C     + + + EXTERNALS + + +
      EXTERNAL   ACCVEC
C
C     + + + END SPECIFICATIONS + + +
C
      I2=2
C
      IF (SLSDFP.GT.0) THEN
C       lateral input fluxes are being considered and printed
        SDIF(TOROW)= SDIF(TOROW)+ SDIF(FRMROW)
      END IF
C
      CALL ACCVEC (I2,SDCF1(1,FRMROW),
     M             SDCF1(1,TOROW))
C
      CALL ACCVEC (I2,SDCF3(1,FRMROW),
     M             SDCF3(1,TOROW))
C
      RETURN
      END
C
C     4.2(1).15.2.4
C
      SUBROUTINE SDPRT
     I                 (UNITFG,LEV,PRINTU,BINU)
C
C     + + + PURPOSE + + +
C     Convert quantities from internal to external units
C     and print out results.  the arrays pstat1 through
C     pstat3, piflx, and pcflx1 through pcflx3 have identical
C     structures to sdst1 through sdst3, sdif, and sdcf1 through
C     sdcf3 apart from dropping the dimension lev for fluxes
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
C     + + + COMMON BLOCKS- SCRTCH, VERSION SEDMNT2 + + +
      INCLUDE    'cplse.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER  I,J,I0,I1,I2,ACNT,CLEN(8),EXDAT(5)
      REAL     MFACTA,MFACTB,PCFLX1(2),PCFLX3(2),PDETS,PSDIF,APRINT(8)
      CHARACTER*8   CCFLX1(2),CCFLX3(2)
      CHARACTER*256 CHEAD(8)
C
C     + + + EXTERNALS + + +
      EXTERNAL   TRNVEC,EXDATE
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' *** SEDMNT ***')
 2010 FORMAT (/,'   STATE VARIABLES',26X,'STORAGE')
 2020 FORMAT (' ',46X,'DETS',15X,'COVER')
 2030 FORMAT (' ',43X,'TONS/AC',12X,'FRACTION')
 2040 FORMAT (' ',41X,'TONNES/HA',12X,'FRACTION')
 2050 FORMAT (' ',40X,1PG10.3,10X,G10.3)
 2070 FORMAT (/,'   FLUXES',35X,'WASHOFF',15X,'SCOUR',
     $        5X,'TOTAL',12X,'VERTICAL ADDITIONS',6X,'LATERAL INFLOW')
 2080 FORMAT (' ',46X,'WSSD',15X,'SCRSD',5X,'SOSED',17X,'DET',
     $        6X,'NVSI',15X,'SLSED')
 2090 FORMAT (' ',40X,'   TONS/AC',10X,'   TONS/AC   TONS/AC',10X,
     $        '   TONS/AC   TONS/AC',10X,'   TONS/AC')
 2100 FORMAT (' ',40X,' TONNES/HA',10X,' TONNES/HA TONNES/HA',10X,
     $        ' TONNES/HA TONNES/HA',10X,' TONNES/HA')
 2110 FORMAT (' ',40X,1PG10.3,10X,2G10.3,10X,2G10.3,
     $        10X,G10.3)
 2120 FORMAT (/,'   FLUXES',35X,'WASHOFF',15X,'SCOUR',
     $        5X,'TOTAL',12X,'VERTICAL ADDITIONS')
 2130 FORMAT (' ',46X,'WSSD',15X,'SCRSD',5X,'SOSED',17X,'DET',
     $        6X,'NVSI')
 2140 FORMAT (' ',40X,'   TONS/AC',10X,'   TONS/AC   TONS/AC',10X,
     $        '   TONS/AC   TONS/AC')
 2150 FORMAT (' ',40X,' TONNES/HA',10X,' TONNES/HA TONNES/HA',10X,
     $        ' TONNES/HA TONNES/HA')
 2160 FORMAT (' ',40X,1PG10.3,10X,2G10.3,10X,2G10.3)
C
C     + + + END SPECIFICATIONS + + +
C
      I0= 0
      I1= 1
      I2= 2
C
C     initialize array counter for binary printout, store variable
C     names in local strings for use in building binary headers
      ACNT = 0
      CCFLX1(1) = 'WSSD'
      CCFLX1(2) = 'SCRSD'
      CCFLX3(1) = 'DET'
      CCFLX3(2) = 'NVSI'
C
C     dimensionless variables - do not have to be converted
C
C     assign conversion constant for dimensional variables
      IF (UNITFG.EQ.1) THEN
C       english system
        MFACTA= 1.0
      ELSE
C       metric system
        MFACTA= 2.241
      END IF
C
      MFACTB= 0.0
C
C     convert dimensional variables to external units
C
      PDETS= DETS*MFACTA
C
      IF (SLSDFP.GT.0) THEN
C       lateral inflows are being handled
        PSDIF= SDIF(LEV)*MFACTA
      END IF
C
C     computed fluxes
      CALL TRNVEC (I2,SDCF1(1,LEV),MFACTA,MFACTB,
     O             PCFLX1)
C
      CALL TRNVEC (I2,SDCF3(1,LEV),MFACTA,MFACTB,
     O             PCFLX3)
C
C     write to unit printu
C
      IF (PRINTU .GT. 0 .AND. PFLAG(4) .LE. LEV) THEN
        WRITE (PRINTU,2000)
C
        WRITE (PRINTU,2010)
        WRITE (PRINTU,2020)
C
        IF (UNITFG.EQ.1) THEN
          WRITE (PRINTU,2030)
        ELSE
          WRITE (PRINTU,2040)
        END IF
C
        WRITE (PRINTU,2050)  PDETS, COVER
      END IF
C
      IF (BINU .GT. 0 .AND. ABS(BFLAG(4)) .LE. LEV) THEN
C       compile values for binary printout
        ACNT = ACNT + 1
        APRINT(ACNT) = PDETS
        CHEAD(ACNT) = 'DETS'
        CLEN(ACNT) = 4
        ACNT = ACNT + 1
        APRINT(ACNT) = COVER
        CHEAD(ACNT) = 'COVER'
        CLEN(ACNT) = 5
      END IF
C
C     total segment wide sediment outflow
      SOSED= PCFLX1(1)+ PCFLX1(2)
C
C     fluxes
      IF (SLSDFP.GT.0) THEN
C       lateral inflow is considered
        IF (PRINTU .GT. 0 .AND. PFLAG(4) .LE. LEV) THEN
          WRITE (PRINTU,2070)
          WRITE (PRINTU,2080)
C
          IF (UNITFG.EQ.1) THEN
            WRITE (PRINTU,2090)
          ELSE
            WRITE (PRINTU,2100)
          END IF
C
          WRITE (PRINTU,2110)  PCFLX1, SOSED, PCFLX3, PSDIF
        END IF
C
        IF (BINU .GT. 0 .AND. ABS(BFLAG(4)) .LE. LEV) THEN
C         compile values for binary printout
          DO 10 I = 1, 2
            ACNT = ACNT + 1
            APRINT(ACNT) = PCFLX1(I)
            CHEAD(ACNT) = CCFLX1(I)
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 10       CONTINUE
          ACNT = ACNT + 1
          APRINT(ACNT) = SOSED
          CHEAD(ACNT) = 'SOSED'
          CLEN(ACNT) = 5
          DO 20 I = 1, 2
            ACNT = ACNT + 1
            APRINT(ACNT) = PCFLX3(I)
            CHEAD(ACNT) = CCFLX3(I)
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 20       CONTINUE
          ACNT = ACNT + 1
          APRINT(ACNT) = PSDIF
          CHEAD(ACNT) = 'SLSED'
          CLEN(ACNT) = 5
        END IF
      ELSE
C       no lateral inflow considered
        IF (PRINTU .GT. 0 .AND. PFLAG(4) .LE. LEV) THEN
          WRITE (PRINTU,2120)
          WRITE (PRINTU,2130)
C
          IF (UNITFG.EQ.1) THEN
            WRITE (PRINTU,2140)
          ELSE
            WRITE (PRINTU,2150)
          END IF
C
          WRITE (PRINTU,2160)  PCFLX1, SOSED, PCFLX3
        END IF
C
        IF (BINU .GT. 0 .AND. ABS(BFLAG(4)) .LE. LEV) THEN
C         compile values for binary printout
          DO 30 I = 1, 2
            ACNT = ACNT + 1
            APRINT(ACNT) = PCFLX1(I)
            CHEAD(ACNT) = CCFLX1(I)
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 30       CONTINUE
          ACNT = ACNT + 1
          APRINT(ACNT) = SOSED
          CHEAD(ACNT) = 'SOSED'
          CLEN(ACNT) = 5
          DO 40 I = 1, 2
            ACNT = ACNT + 1
            APRINT(ACNT) = PCFLX3(I)
            CHEAD(ACNT) = CCFLX3(I)
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 40       CONTINUE
        END IF
      END IF
C
      IF (BINU .GT. 0 .AND. ABS(BFLAG(4)) .LE. LEV) THEN
C       write binary output
        CALL EXDATE(
     I              DATIM,
     O              EXDAT)
        IF (BFLAG(4) .GT. 0) THEN
C         at start of run, write the header
          WRITE (BINU) I0,'PERLND  ',LSNO,'SEDMNT  ',
     1          (CLEN(I),(CHEAD(I)(J:J),J=1,CLEN(I)),I=1,ACNT)
C         set bflag to negative to not write headers anymore
          BFLAG(4) = -BFLAG(4)
        END IF
        WRITE (BINU) I1,'PERLND  ', LSNO,'SEDMNT  ',UNITFG,
     1               LEV,(EXDAT(I),I=1,5),(APRINT(I),I=1,ACNT)
      END IF
C
      RETURN
      END
C
C     4.2(1).15.3.3
C
      SUBROUTINE SDRST
     I                 (LEV)
C
C     + + + PURPOSE + + +
C     Reset all flux accumulators and those state variables
C     used in material balance check for section sedmnt
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    LEV
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEV    - current output level (2-pivl,3-day,4-mon,5-ann)
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION SEDMNT2 + + +
      INCLUDE    'cplse.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I2
C
C     + + + EXTERNALS + + +
      EXTERNAL   SETVEC
C
C     + + + END SPECIFICATIONS + + +
C
      I2=2
C
      IF (SLSDFP.GT.0) THEN
C       lateral input fluxes are being printed
        SDIF(LEV)= 0.0
      END IF
C
      CALL SETVEC (I2,0.0,
     O             SDCF1(1,LEV))
C
      CALL SETVEC (I2,0.0,
     O             SDCF3(1,LEV))
C
      RETURN
      END
C
C     4.2(1).4.2
C
      SUBROUTINE SOSED1
     I                  (SURO,SURS,DELT60,KSER,JSER,KGER,JGER,
     M                   DETS,
     O                   WSSD,SCRSD,SOSED,STCAP)
C
C     + + + PURPOSE + + +
C     Warning,  this method of computing sediment removal contains a
C     dimensionally non-homogeneous term (surs+ suro).  this introduces
C     additional dependence of the results on the simulation interval
C     delt.  so far, it has only been used with delt of 15 and 5
C     minutes.
C
C     + + + DUMMY ARGUMENTS + + +
      REAL       DELT60,DETS,JGER,JSER,KGER,KSER,SCRSD,SOSED,SURO,SURS,
     $           WSSD,STCAP
C
C     + + + ARGUMENT DEFINITIONS + + +
C     SURO   - surface output
C     SURS   - ???
C     DELT60 - simulation time interval in hours
C     KSER   - ???
C     JSER   - ???
C     KGER   - ???
C     JGER   - ???
C     DETS   - ???
C     WSSD   - ???
C     SCRSD  - ???
C     SOSED  - ???
C     STCAP  - ???
C
C     + + + LOCAL VARIABLES + + +
      REAL       ARG
C
C     + + + END SPECIFICATIONS + + +
C
C     Remove both detached surface sediment and soil matrix by surface
C     Flow using method 1
C
      IF (SURO.GT.0.0) THEN
C       surface runoff occurs, so sediment and soil matrix
C       particles may be removed, delt60= delt/60
C       get argument used in transport equations
        ARG= SURS+ SURO
C
C       calculate capacity for removing detached sediment - units
C       are tons/acre-ivl
        STCAP= DELT60*KSER*(ARG/DELT60)**JSER
C
        IF (STCAP.GT.DETS) THEN
C         there is insufficient detached storage, base sediment
C         removal on that available, wssd is in tons/acre-ivl
          WSSD= DETS*SURO/ARG
        ELSE
C         there is sufficient detached storage, base sediment
C         removal on the calculated capacity
          WSSD= STCAP*SURO/ARG
        END IF
C
        DETS= DETS- WSSD
C
C       calculate scour of matrix soil by surface runoff -
C       units are tons/acre-ivl
        SCRSD= DELT60*KGER*(ARG/DELT60)**JGER
C
        SCRSD= SCRSD*SURO/ARG
C
C       total removal by runoff
        SOSED= WSSD+ SCRSD
C
      ELSE
C       no runoff occurs, so no removal by runoff
        WSSD = 0.0
        SCRSD= 0.0
        SOSED= 0.0
        STCAP=0.0
      END IF
C
      RETURN
      END
C
C     4.2(1).4.3
C
      SUBROUTINE SOSED2
     I                  (SURO,DELT60,KSER,JSER,KGER,JGER,
     M                   DETS,
     O                   WSSD,SCRSD,SOSED,STCAP)
C
C     + + + PURPOSE + + +
C     Warning,  this method of computing sediment removal has not
C     been tested.  but it is dimensionally homogeneous
C
C     + + + DUMMY ARGUMENTS + + +
      REAL       DELT60,DETS,JGER,JSER,KGER,KSER,SCRSD,SOSED,SURO,WSSD,
     $           STCAP
C
C     + + + ARGUMENT DEFINITIONS + + +
C     SURO   - surface output
C     DELT60 - simulation time interval in hours
C     KSER   - ???
C     JSER   - ???
C     KGER   - ???
C     JGER   - ???
C     DETS   - ???
C     WSSD   - ???
C     SCRSD  - ???
C     SOSED  - ???
C     STCAP  - ???
C
C     + + + END SPECIFICATIONS + + +
C
C     Remove both detached surface sediment and soil matrix by surface
C     Flow using method 2
      IF (SURO.GT.0.0) THEN
C       surface runoff occurs, so sediment and soil matrix
C       particles may be removed, delt60= delt/60
C
C       calculate capacity for removing detached sediment - units
C       are tons/acre-ivl
        STCAP= DELT60*KSER*(SURO/DELT60)**JSER
C
        IF (STCAP.GT.DETS) THEN
C         there is insufficient detached storage, base sediment
C         removal on that available, wssd is in tons/acre-ivl
          WSSD= DETS
          DETS= 0.0
        ELSE
C         there is sufficient detached storage, base sediment
C         removal on the calculated capacity
          WSSD= STCAP
          DETS= DETS- WSSD
        END IF
C
C       calculate scour of matrix soil by surface runoff -
C       units are tons/acre-ivl
        SCRSD= DELT60*KGER*(SURO/DELT60)**JGER
C
C       total removal by runoff
        SOSED= WSSD+ SCRSD
      ELSE
C       no runoff occurs, so no removal by runoff
        WSSD = 0.0
        SCRSD= 0.0
        SOSED= 0.0
        STCAP=0.0
      END IF
C
      RETURN
      END
C
C     4.2(1).14.3
C
      SUBROUTINE SEDTPB
C
C     + + + PURPOSE + + +
C     Handle section SEDMNT.
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION SEDMNT2 + + +
      INCLUDE   'cplse.inc'
      INCLUDE   'cmpad.inc'
C
C     + + + END SPECIFICATIONS + + +
C
C     handle section sedmnt
      IF (WSDFP.GE.1) THEN
        PAD(WSDFP +IVL1)= WSSD
      END IF
C
      IF (CSDFP.GE.1) THEN
        PAD(CSDFP +IVL1)= SCRSD
      END IF
C
      IF (SOSDFP.GE.1) THEN
        PAD(SOSDFP+IVL1)= SOSED
      END IF
C
      IF (DETFP.GE.1) THEN
        PAD(DETFP +IVL1)= DET
      END IF
C
      IF (NVSIFP.GE.1) THEN
        PAD(NVSIFP + IVL1)= NVSI
      END IF
C
      RETURN
      END
C
C     4.2(1).13.4
C
      SUBROUTINE SEDTPT
C
C     + + + PURPOSE + + +
C     Handle section SEDMNT.
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION SEDMNT2 + + +
      INCLUDE   'cplse.inc'
      INCLUDE   'cmpad.inc'
C
C     + + + END SPECIFICATIONS + + +
C
C     handle section sedmnt
C
      IF (DETSFP.GE.1)  PAD(DETSFP+IVL1)= DETS
      IF (STCFP.GE.1) PAD(STCFP + IVL1) =STCAP
      IF (COVRFP.GE.1) PAD(COVRFP + IVL1) =COVER
C
      RETURN
      END
