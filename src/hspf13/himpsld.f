C
C
C
      SUBROUTINE   PSOLID
     I                    (OUTLEV)
C
C     + + + PURPOSE + + +
C     Process the input for section solids1
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   OUTLEV
C
C     + + + ARGUMENT DEFINITIONS + + +
C     OUTLEV - run interp output level
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION SOLIDS1 + + +
      INCLUDE    'cilsl.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   TBNO,TBSB,NVAL,I
      REAL      RVAL(4)
C
C     + + + EXTERNALS + + +
      EXTERNAL  ITABLE,RTABLE
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' PROCESSING INPUT FOR SECTION SOLIDS')
 2010 FORMAT (/,' FINISHED PROCESSING INPUT FOR SECTION SOLIDS')
C
C     + + + END SPECIFICATIONS + + +
C
      IF (OUTLEV.GT.1) THEN
C       processing message
        WRITE (MESSU,2000)
      END IF
C
C     initialize variables not set is SLDRST
      DO 10 I= 1, 5
        SLDIF(I)= 0.0
 10   CONTINUE
C
C     assume day is dry
      DRYDFG= 1
C
C     process values in table-type sld-parm1
      TBNO= 20
      TBSB= 1
      NVAL= 3
      CALL ITABLE (TBNO,TBSB,NVAL,UUNITS,
     M             SLDPM1)
C
C     process values in table-type sld-parm2
      TBNO= 21
      TBSB= 1
      NVAL= 4
      CALL RTABLE (TBNO,TBSB,NVAL,UUNITS,
     M             RVAL)
C
      KEIM  = RVAL(1)
      JEIM  = RVAL(2)
      ACCSDP= RVAL(3)
      REMSDP= RVAL(4)
C
      IF (VASDFG.EQ.1) THEN
C       get monthly values of accumulation rate - table-type mon-accum
        TBNO= 22
        TBSB= 1
        NVAL= 12
        CALL RTABLE (TBNO,TBSB,NVAL,UUNITS,
     M               ACCSDM)
      END IF
C
      IF (VRSDFG.EQ.1) THEN
C       get monthly values of solids removal - table-type mon-remov
        TBNO= 23
        TBSB= 1
        NVAL= 12
        CALL RTABLE (TBNO,TBSB,NVAL,UUNITS,
     M               REMSDM)
      END IF
C
C     initial solids storage - table-type sld-stor
      TBNO= 24
      TBSB= 1
      NVAL= 1
      CALL RTABLE (TBNO,TBSB,NVAL,UUNITS,
     M             RVAL)
      SLDS= RVAL(1)
C
      IF (OUTLEV.GT.1) THEN
C       finished processing message
        WRITE (MESSU,2010)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   SOLIDS
C
C     + + + PURPOSE + + +
C     Accumulate and remove solids from the impervious land segment
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION SOLIDS2 + + +
      INCLUDE   'cilsl.inc'
      INCLUDE   'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     REQFG,TSSUB(2),FLGVAL
      CHARACTER*6 TSNAM,OPTYP,SECNAM,MSECNM,OPFGNM
C
C     + + + EXTERNALS + + +
      EXTERNAL   SOSLD1,SOSLD2,ACCUM,HREQTS
C
C     + + + DATA INITIALIZATIONS + + +
      DATA TSSUB/1,1/
      DATA OPTYP,SECNAM/'IMPLND','SOLIDS'/
C
C     + + + END SPECIFICATIONS + + +
C
C     get input time series
CTHJU      PREC= PAD(PRECFP+IVL1)
      REQFG= 2   
      TSNAM= 'PREC  '
      CALL HREQTS (PRECFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,LSNO,
     I             TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,FLGVAL,
     O             PREC)
C
      IF (SLSDFP.GT.0) THEN
C       lateral input of solids is considered
        SLSLD= PAD(SLSDFP+IVL1)
        SLDS= SLDS+ SLSLD
      ELSE
        SLSLD= 0.0
      END IF
C
      IF (IWATFG.EQ.0) THEN
C       read time series supplied by iwater
CTHJ        SURO= PAD(SOFP+IVL1)
        MSECNM= 'IWATER'
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
C       the above time series are available from iwater
      END IF
C
C     washoff solids
      IF (SDOPFG.EQ.1) THEN
C       use method 1
        CALL SOSLD1 (SURO,SURS,DELT60,KEIM,JEIM,
     M               SLDS,
     O               SOSLD)
      ELSE
C       use method 2
        CALL SOSLD2 (SURO,DELT60,KEIM,JEIM,
     M               SLDS,
     O               SOSLD)
      END IF
C
C     accumulate and remove solids independent of runoff
      CALL ACCUM (DAYFG,PREC,VASDFG,ACCSDM,REMSDM,VRSDFG,
     I            MON,NXTMON,DAY,NDAYS,
     M            DRYDFG,ACCSDP,REMSDP,SLDS)
C
      RETURN
      END
C
C
C
      SUBROUTINE   ACCUM
     I                   (DAYFG,PREC,VASDFG,ACCSDM,REMSDM,VRSDFG,
     I                    MON,NXTMON,DAY,NDAYS,
     M                    DRYDFG,ACCSDP,REMSDP,SLDS)
C
C     + + + PURPOSE + + +
C     Accumulate and remove solids independent of runoff.
C     the calculation is done at the start of each day, if the
C     previous day was dry
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    DAY,DAYFG,DRYDFG,MON,NDAYS,NXTMON,VASDFG,VRSDFG
      REAL       ACCSDM(12),ACCSDP,PREC,REMSDM(12),REMSDP,SLDS
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DAYFG  - flag for first day or day change
C     PREC   - ???
C     VASDFG - ???
C     ACCSDM - ???
C     REMSDM - ???
C     VRSDFG - ???
C     MON    - calendar month
C     NXTMON - next calendar month
C     DAY    - day of month
C     NDAYS  - no. of days in this month
C     DRYDFG - ???
C     ACCSDP - ???
C     REMSDP - ???
C     SLDS   - ???
C
C     + + + FUNCTIONS + + +
      REAL       DAYVAL
C
C     + + + EXTERNALS + + +
      EXTERNAL   DAYVAL
C
C     + + + END SPECIFICATIONS + + +
C
      IF (DAYFG.EQ.0) THEN
C       it is not the first interval of a new day
        IF (PREC.GT.0.0) THEN
C         it is not a dry day
          DRYDFG= 0
        END IF
      ELSE
C       it is the first interval of a new day
        IF (DRYDFG.EQ.1) THEN
C         precipitation did not occur during the previous day
          IF (VASDFG.EQ.1) THEN
C           accumulation rate of solids is allowed to vary
C           throughout the year
C           interpolate for the daily value
C           linearly interpolate accsdp between two values from the
C           monthly array accsdm(12)
            ACCSDP= DAYVAL(ACCSDM(MON),ACCSDM(NXTMON),DAY,NDAYS)
          ELSE
C           the accumulation rate does not vary throughout the year.
C           accsdp value has been supplied by the run interpreter
          END IF
C
          IF (VRSDFG.EQ.1) THEN
C           unit-rate of solids removal is allowed
C           to vary throughout the year
C           interpolate for the daily value
C           linearly interpolate remsdp between two values from the
C           monthly array remsdm(12)
            REMSDP= DAYVAL(REMSDM(MON),REMSDM(NXTMON),DAY,NDAYS)
          ELSE
C           the removal unit-rate does not vary throughout the year.
C           remsdp value has been supplied by the run interpreter
          END IF
C
C         update storage due to accumulation and removal which
C         occurs independent of runoff - units are lbs/acre
          SLDS= ACCSDP+ SLDS*(1.0- REMSDP)
        ELSE
C         precipitation did occur during the previous day, so there
C         is no accumulation or removal independent of runoff
        END IF
C
        IF (PREC.GT.0.0) THEN
C         there is precipitation on the first interval of the new day
          DRYDFG= 0
        ELSE
C         day is dry so far
          DRYDFG= 1
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   SLDACC
     I                    (FRMROW,TOROW)
C
C     + + + PURPOSE + + +
C     Accumulate fluxes for section solids
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   FRMROW,TOROW
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FRMROW - row containing incremental flux accumulation
C     TOROW  - flux row to be incremented
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION SOLIDS2 + + +
      INCLUDE  'cilsl.inc'
      INCLUDE  'cmpad.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      IF (SLSDFP.GT.0) THEN
C       lateral input flux is being considered and printed
        SLDIF(TOROW)= SLDIF(TOROW)+ SLDIF(FRMROW)
      END IF
C
      SDCF1(TOROW) = SDCF1(TOROW)+ SDCF1(FRMROW)
C
      RETURN
      END
C
C
C
      SUBROUTINE   SLDIB
C
C     + + + PURPOSE + + +
C     Handle section solids
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION SOLIDS2 + + +
      INCLUDE 'cilsl.inc'
      INCLUDE 'cmpad.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      IF (SOSDFP.GE.1) THEN
        PAD(SOSDFP+IVL1)= SOSLD
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   SLDIP
C
C     + + + PURPOSE + + +
C     Handle section solids
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION SOLIDS2 + + +
      INCLUDE 'cilsl.inc'
      INCLUDE 'cmpad.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      IF (SLDSFP.GE.1) THEN
        PAD(SLDSFP+IVL1)= SLDS
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   SLDPRT
     I                    (UNITFG,LEV,PRINTU,BINU)
C
C     + + + PURPOSE + + +
C     Convert quantities from internal to external units
C     and print out results.
C     Note: local arrays have identical sizes and structures to the
C     corresponding arrays in the osv apart from dropping the
C     dimension lev for fluxes
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
C     + + + COMMON BLOCKS- SCRTCH, VERSION SOLIDS2 + + +
      INCLUDE    'cilsl.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,J,I0,I1,ACNT,CLEN(3),EXDAT(5)
      REAL       MFACTA,PSLDS,PCFLX1,PSLDIF,APRINT(3)
      CHARACTER*256 CHEAD(3)
C
C     + + + EXTERNALS + + +
      EXTERNAL   EXDATE
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' *** SOLIDS ***')
 2010 FORMAT (/,'   STATE VARIABLES',19X,'SOLIDS STORAGE')
 2020 FORMAT (41X,'      SLDS')
 2030 FORMAT (41X,'   TONS/AC')
 2040 FORMAT (41X,' TONNES/HA')
 2050 FORMAT (41X,F10.3)
 2060 FORMAT (/,'   FLUXES',28X,'SOLIDS WASHOFF      LATERAL INFLOW')
 2070 FORMAT (41X,'     SOSLD',15X,'SLSLD')
 2080 FORMAT (31X,2(10X,'   TONS/AC') )
 2090 FORMAT (31X,2(10X,' TONNES/HA') )
 2100 FORMAT (31X,2(10X,F10.3) )
 2110 FORMAT (/,'   FLUXES',28X,'SOLIDS WASHOFF')
 2120 FORMAT (41X,'     SOSLD')
 2130 FORMAT (41X,'   TONS/AC')
 2140 FORMAT (41X,' TONNES/HA')
C
C     + + + END SPECIFICATIONS + + +
C
      I0= 0
      I1= 1
C
C     initialize array counter for binary printout
      ACNT = 0
C
C     assign conversion constant for variables
      IF (UNITFG.EQ.1) THEN
C       english system
        MFACTA= 1.0
      ELSE
C       metric system
        MFACTA= 2.241
      END IF
C
C     convert dimensional variables to external units
C
C     state variables
      PSLDS= SLDS*MFACTA
C
      IF (SLSDFP.GT.0) THEN
C       lateral inflows are being handled
        PSLDIF= SLDIF(LEV)*MFACTA
      END IF
C
C     computed fluxes
      PCFLX1= SDCF1(LEV)*MFACTA
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
        WRITE (PRINTU,2050)  PSLDS
      END IF
      IF (BINU .GT. 0 .AND. ABS(BFLAG(4)) .LE. LEV) THEN
C       compile values for direct access printout
        ACNT = ACNT + 1
        APRINT(ACNT) = PSLDS
        CHEAD(ACNT) = 'SLDS'
        CLEN(ACNT) = 4
      END IF
C     fluxes
      IF (SLSDFP.GT.0) THEN
C       lateral inflow is considered
        IF (PRINTU .GT. 0 .AND. PFLAG(4) .LE. LEV) THEN
          WRITE (PRINTU,2060)
          WRITE (PRINTU,2070)
C
          IF (UNITFG.EQ.1) THEN
            WRITE (PRINTU,2080)
          ELSE
            WRITE (PRINTU,2090)
          END IF
C
          WRITE (PRINTU,2100)  PCFLX1, PSLDIF
        END IF
        IF (BINU .GT. 0 .AND. ABS(BFLAG(4)) .LE. LEV) THEN
          ACNT = ACNT + 1
          APRINT(ACNT) = PCFLX1
          CHEAD(ACNT) = 'SOSLD'
          CLEN(ACNT) = 5
          ACNT = ACNT + 1
          APRINT(ACNT) = PSLDIF
          CHEAD(ACNT) = 'SLSLD'
          CLEN(ACNT) = 5
        END IF
      ELSE
C       no lateral inflow considered
        IF (PRINTU .GT. 0 .AND. PFLAG(4) .LE. LEV) THEN
          WRITE (PRINTU,2110)
          WRITE (PRINTU,2120)
C
          IF (UNITFG.EQ.1) THEN
            WRITE (PRINTU,2130)
          ELSE
            WRITE (PRINTU,2140)
          END IF
C
          WRITE (PRINTU,2100)  PCFLX1
        END IF
        IF (BINU .GT. 0 .AND. ABS(BFLAG(4)) .LE. LEV) THEN
          ACNT = ACNT + 1
          APRINT(ACNT) = PCFLX1
          CHEAD(ACNT) = 'SOSLD'
          CLEN(ACNT) = 5
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
          WRITE (BINU) I0,'IMPLND  ',LSNO,'SOLIDS  ',
     1          (CLEN(I),(CHEAD(I)(J:J),J=1,CLEN(I)),I=1,ACNT)
C         set bflag to negative to not write headers anymore
          BFLAG(4) = -BFLAG(4)
        END IF
        WRITE (BINU) I1,'IMPLND  ',LSNO,'SOLIDS  ',UNITFG,
     1               LEV,(EXDAT(I),I=1,5),(APRINT(I),I=1,ACNT)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   SLDRST
     I                    (LEV)
C
C     + + + PURPOSE + + +
C     Reset all flux accumulators and those state variables
C     used in material balance check for section solids
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   LEV
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEV    - current output level (2-pivl,3-day,4-mon,5-ann)
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION SOLIDS2 + + +
      INCLUDE  'cilsl.inc'
      INCLUDE  'cmpad.inc'
C
C     + + + END SPECIFICATIONS + + +
C
C     handle flux groups containing segment-wide variables
C
      IF (SLSDFP.GT.0) THEN
C       lateral input fluxes are being printed
        SLDIF(LEV)= 0.0
      END IF
C
      SDCF1(LEV)= 0.0
C
      RETURN
      END
C
C
C
      SUBROUTINE   SOSLD1
     I                    (SURO,SURS,DELT60,KEIM,JEIM,
     M                     SLDS,
     O                     SOSLD)
C
C     + + + PURPOSE + + +
C     Warning: this method of computing solids washoff contains a
C     dimensionally non-homogeneous term (surs + suro). this introduces
C     additional dependence of the results on the simulation interval
C     delt. so far, it has only been used with a delt of 15 minutes
C
C     + + + DUMMY ARGUMENTS + + +
      REAL     DELT60,JEIM,KEIM,SLDS,SOSLD,SURO,SURS
C
C     + + + ARGUMENT DEFINITIONS + + +
C     SURO   - surface output
C     SURS   - ???
C     DELT60 - simulation time interval in hours
C     KEIM   - ???
C     JEIM   - ???
C     SLDS   - ???
C     SOSLD  - ???
C
C     + + + LOCAL VARIABLES + + +
      REAL     ARG,STCAP
C
C     + + + END SPECIFICATIONS + + +
C
C     Washoff solids from the impervious segment using method 1
      IF (SURO.GT.0.0) THEN
C       impervious surface runoff occurs, so solids may be removed;
C       delt60= delt/60
C       get argument used in transport equations
        ARG= SURS+ SURO
C
C       calculate capacity for removing solids - units
C       are tons/acre-ivl
        STCAP= DELT60*KEIM*(ARG/DELT60)**JEIM
C
        IF (STCAP.GT.SLDS) THEN
C         there is insufficient solids storage, base solids
C         removal on that available, sosld is in tons/acre-ivl
          SOSLD= SLDS*SURO/ARG
        ELSE
C         there is sufficient solids storage, base solids
C         removal on the calculated capacity
          SOSLD= STCAP*SURO/ARG
        END IF
C
        SLDS= SLDS- SOSLD
      ELSE
C       no runoff occurs, so no removal by runoff
        SOSLD= 0.0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   SOSLD2
     I                    (SURO,DELT60,KEIM,JEIM,
     M                     SLDS,
     O                     SOSLD)
C
C     + + + PURPOSE + + +
C     Warning: this method of computing solids washoff has not
C     been tested. but it is dimensionally homogeneous
C
C     + + + DUMMY ARGUMENTS + + +
      REAL     DELT60,JEIM,KEIM,SLDS,SOSLD,SURO
C
C     + + + ARGUMENT DEFINITIONS + + +
C     SURO   - surface output
C     DELT60 - simulation time interval in hours
C     KEIM   - ???
C     JEIM   - ???
C     SLDS   - ???
C     SOSLD  - ???
C
C     + + + LOCAL VARIABLES + + +
      REAL     STCAP
C
C     + + + END SPECIFICATIONS + + +
C
C     Washoff solids from the impervious segment using method 2
C
      IF (SURO.GT.0.0) THEN
C       impervious surface runoff occurs, so solids may be removed;
C
C       calculate capacity for removing solids - units
C       are tons/acre-ivl
        STCAP= DELT60*KEIM*(SURO/DELT60)**JEIM
C
        IF (STCAP.GT.SLDS) THEN
C         there is insufficient solids storage, base solids
C         removal on that available, sosld is in tons/acre-ivl
          SOSLD= SLDS
          SLDS = 0.0
        ELSE
C         there is sufficient solids storage, base solids
C         removal on the calculated capacity
          SOSLD= STCAP
          SLDS = SLDS- SOSLD
        END IF
      ELSE
C       no runoff occurs, so no removal by runoff
        SOSLD= 0.0
      END IF
C
      RETURN
      END
