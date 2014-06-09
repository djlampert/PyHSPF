C
C
C
      SUBROUTINE   PMSTLA
C
C     + + + PURPOSE + + +
C     Process input for section mstlay
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION MSTLAY1 + + +
      INCLUDE    'cplms.inc'
      INCLUDE    'crin2.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I1,I2,I4,IVAL(1)
      REAL      RVAL(8)
C
C     + + + EXTERNALS + + +
      EXTERNAL  ITABLE,RTABLE
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' PROCESSING INPUT FOR SECTION MSTLAY')
 2010 FORMAT (/,' TOPSOIL MOISTURE STORAGES')
 2020 FORMAT (/,' FRACTIONAL FLUXES THROUGH TOPSOIL ',
     $            'LAYERS')
 2050 FORMAT (/,' FINISHED PROCESSING INPUT FOR SECTION MSTLAY')
 2060 FORMAT (/,' SUBSOIL MOISTURE STORAGES')
 2070 FORMAT (/,' FRACTIONAL FLUXES THROUGH SUBSOIL')
C
C     + + + END SPECIFICATIONS + + +
C
      I1= 1
      IF (OUTLEV.GT.1) THEN
C       processing section message
        WRITE (MESSU,2000)
      END IF
C
C     factor to convert water from inches to mass/area
      IF (UUNITS.EQ.1) THEN
        CFINMA= 2.264E05
      ELSE
        CFINMA= 2.54E05
      END IF
C
      IF (PWATFG.EQ.0) THEN
C       vuzfg obtained here from user's control input table-type vuzfg
        I2= 79
        I4= 1
        CALL ITABLE (I2,I1,I4,UUNITS,
     M               IVAL)
        VUZFG= IVAL(1)
C
C       uzsn, lzsn and initial surface storages read in table-type uzsn-lzsn
        I2= 80
        I4= 3
        CALL RTABLE (I2,I1,I4,UUNITS,
     M               RVAL)
        UZSN= RVAL(1)
        LZSN= RVAL(2)
        SURS= RVAL(3)
        IF (VUZFG.EQ.1) THEN
C         table-type mon-uzsn
          I2= 22
          I4= 12
          CALL RTABLE (I2,I1,I4,UUNITS,
     M                 UZSNM)
        END IF
      END IF
C
C     table-type mst-parm
      I2= 81
      I4= 3
      CALL RTABLE (I2,I1,I4,UUNITS,
     M             MSTPM)
C
      IF (OUTLEV.GT.2) THEN
C       processing message
        WRITE (MESSU,2010)
      END IF
C     table-type mst-topstor
      I2= 82
      I4= 3
      CALL RTABLE (I2,I1,I4,UUNITS,
     M             MST)
C
      IF (OUTLEV.GT.2) THEN
C       processing message
        WRITE (MESSU,2020)
      END IF
C
C     table-type mst-topflx
      I2= 83
      I4= 5
      CALL RTABLE (I2,I1,I4,UUNITS,
     M             FRAC)
C
C     process input for subsoil layers
      IF (OUTLEV.GT.2) THEN
C       processing message
        WRITE (MESSU,2060)
      END IF
C     table-type mst-substor
      I2= 84
      I4= 2
      CALL RTABLE (I2,I1,I4,UUNITS,
     M             MST(4))
C
      IF (OUTLEV.GT.2) THEN
C       processing message
        WRITE (MESSU,2070)
      END IF
C     table-type mst-subflx
      I2= 85
      I4= 3
      CALL RTABLE (I2,I1,I4,UUNITS,
     M             FRAC(6))
C
      IF (OUTLEV.GT.1) THEN
C       end processing message
        WRITE (MESSU,2050)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   MSTLAY
C
C     + + + PURPOSE + + +
C     Estimate the moisture storages and solute fluxes as
C     fractions of stored material
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION MSTLAY2 + + +
      INCLUDE    'cplms.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     REQFG,TSSUB(2),FLGVAL
      CHARACTER*6 OPTYP,TSNAM,SECNAM,MSECNM,OPFGNM
C
C     + + + FUNCTIONS + + +
      REAL       DAYVAL
C
C     + + + EXTERNALS + + +
      EXTERNAL   DAYVAL,TOPLAY,SUBLAY,HREQTS
C
C     + + + DATA INITIALIZATIONS + + +
      DATA TSSUB/1,1/
      DATA OPTYP,SECNAM,MSECNM/'PERLND','MSTLAY','PWATER'/
C
C     + + + END SPECIFICATIONS + + +
C
      IF (PWATFG.EQ.0) THEN
C
        IF (DAYFG.EQ.1) THEN
C         it is the first interval of the day
          IF (VUZFG.EQ.1) THEN
C           uzsn is allowed to vary throughout the year
C           interpolate for the daily value
C           linearly interpolate uzsn between two values from the
C           monthly array uzsnm(12)
            UZSN= DAYVAL(UZSNM(MON),UZSNM(NXTMON),DAY,NDAYS)
          ELSE
C           uzsn does not vary throughout the year.
C           uzsn value has been supplied by the run interpreter
          END IF
        END IF
C
C       read time series supplied by pwater
CTHJ        SURO = PAD(SOFP+IVL1)
        REQFG= 3
        TSNAM= 'SURO  '
        CALL HREQTS (SOFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,LSNO,
     I               TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,FLGVAL,
     O               SURO)
        SURSS= SURS
CTHJ        SURS = PAD(SSFP+IVL1)
        TSNAM= 'SURS  '
        CALL HREQTS (SSFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,LSNO,
     I               TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,FLGVAL,
     O               SURS)
CTHJ        INFIL= PAD(INFFP+IVL1)
        TSNAM= 'INFIL '
        CALL HREQTS (INFFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,LSNO,
     I               TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,FLGVAL,
     O               INFIL)
CTHJ        IFWI = PAD(IIFP+IVL1)
        TSNAM= 'IFWI  '
        CALL HREQTS (IIFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,LSNO,
     I               TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,FLGVAL,
     O               IFWI)
CTHJ        UZI  = PAD(UZIFP+IVL1)
        TSNAM= 'UZI   '
        CALL HREQTS (UZIFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,LSNO,
     I               TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,FLGVAL,
     O               UZI)
CTHJ        UZS  = PAD(UZSFP+IVL1)
        TSNAM= 'UZS   '
        CALL HREQTS (UZSFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,LSNO,
     I               TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,FLGVAL,
     O               UZS)
CTHJ        PERC = PAD(PCFP+IVL1)
        TSNAM= 'PERC  '
        CALL HREQTS (PCFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,LSNO,
     I               TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,FLGVAL,
     O               PERC)
CTHJ        IFWS = PAD(ISFP+IVL1)
        TSNAM= 'IFWS  '
        CALL HREQTS (ISFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,LSNO,
     I               TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,FLGVAL,
     O               IFWS)
CTHJ        IFWO = PAD(IOFP+IVL1)
        TSNAM= 'IFWO  '
        CALL HREQTS (IOFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,LSNO,
     I               TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,FLGVAL,
     O               IFWO)
CTHJ        SURI= PAD(SURIFP+IVL1)
        TSNAM= 'SURI  '
        CALL HREQTS (SURIFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,LSNO,
     I               TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,FLGVAL,
     O               SURI)
CTHJ        LZS = PAD(LZSFP+IVL1)
        TSNAM= 'LZS   '
        CALL HREQTS (LZSFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,LSNO,
     I               TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,FLGVAL,
     O               LZS)
CTHJ        IGWI= PAD(IGIFP+IVL1)
        TSNAM= 'IGWI  '
        CALL HREQTS (IGIFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,LSNO,
     I               TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,FLGVAL,
     O               IGWI)
CTHJ        AGWI= PAD(AIFP+IVL1)
        TSNAM= 'AGWI  '
        CALL HREQTS (AIFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,LSNO,
     I               TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,FLGVAL,
     O               AGWI)
CTHJ        AGWS= PAD(AGWSFP+IVL1)
        TSNAM= 'AGWS  '
        CALL HREQTS (AGWSFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,LSNO,
     I               TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,FLGVAL,
     O               AGWS)
CTHJ        AGWO= PAD(AOFP+IVL1)
        TSNAM= 'AGWO  '
        CALL HREQTS (AOFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,LSNO,
     I               TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,FLGVAL,
     O               AGWO)
      ELSE
C       the above time series and uzsn are available
C       from pwater
      END IF
C
C     estimate for surface and upper layers
      CALL TOPLAY (SURSS,SURI,CFINMA,SURO,INFIL,IFWI,UZI,SLMPF,UZS,ULPF,
     I             PERC,UZSN,IFWS,IFWO,
     O             MST,FRAC)
C
C     estimate for lower and groundwater layers
      CALL SUBLAY (LZS,IGWI,AGWI,CFINMA,LZSN,
     $             LLPF,AGWS,AGWO,
     O             MST,FRAC)
C
      RETURN
      END
C
C
C
      SUBROUTINE   MSTLPT
C
C     + + + PURPOSE + + +
C     Handle section MSTLAY.
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION MSTLAY2 + + +
      INCLUDE    'cplms.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    J
C
C     + + + END SPECIFICATIONS + + +
C
C     handle section mstlay
      DO 10 J= 1,5
        IF (MSTFP(J).GE.1) THEN
          PAD(MSTFP(J)+IVL1)= MST(J)
        END IF
 10   CONTINUE
C
      DO 20 J= 1,8
        IF (FRACFP(J).GE.1) THEN
          PAD(FRACFP(J)+IVL1)= FRAC(J)
        END IF
 20   CONTINUE
C
      RETURN
      END
C
C     4.2(1).15.2.8
C
      SUBROUTINE MSTPRT
     I                  (LEV,PRINTU,AGMAID,MFACTA,MFACTB,UNITFG,BINU)
C
C     + + + PURPOSE + + +
C     Convert quanities from internal to external units and print out
C     results.  the array pstat2 has identical structure to mst
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     LEV,PRINTU,UNITFG,BINU
      REAL        MFACTA,MFACTB
      CHARACTER*8 AGMAID
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEV    - current output level (2-pivl,3-day,4-mon,5-ann)
C     PRINTU - fortran unit number on which to print output
C     AGMAID - ???
C     MFACTA - ???
C     MFACTB - ???
C     UNITFG - output units   1-english, 2-metric
C     BINU   - fortran unit number on which to write binary output
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION MSTLAY2 + + +
      INCLUDE    'cplms.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I,J,I0,I1,I5,ACNT,CLEN(13),EXDAT(5)
      REAL          PSTAT1(5),APRINT(13)
      CHARACTER*8   CFRAC(8),CSTAT1(5)
      CHARACTER*256 CHEAD(13)
C
C     + + + EXTERNALS + + +
      EXTERNAL   TRNVEC,EXDATE
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' *** MSTLAY ***')
 2010 FORMAT (/,'   STATE VARIABLES',13X,'<--SURFACE LAYER--->',
     $  '<---UPPER LAYER----><----INTERFLOW-----><---LOWER LAYER---->',
     $        '<---GROUNDWATER---->')
 2020 FORMAT (' ',33X,'OUTFLOW    PERCOL  TO INTER    PERCOL',
     $  10X,'   OUTFLOW      PERC DEEP PERC',13X,'OUTFLOW')
 2030 FORMAT (' ','    SOLUTE FRACTIONS',17X,'FSO       FSP',
     $        7X,'FII       FUP',17X,'FIO       FLP      FLDP',
     $        17X,'FAO')
 2040 FORMAT (' ',30X,4F10.4,10X,3F10.4,10X,F10.4)
 2070 FORMAT (/,'     MOISTURE ',A8,9X,5(13X,'STORAGE'))
 2080 FORMAT (' ',45X,'SMSTM',15X,'UMSTM',15X,'IMSTM',15X,'LMSTM',15X,
     $        'AMSTM')
 2090 FORMAT (' ',30X,5(10X,F10.1))
C
C     + + + END SPECIFICATIONS + + +
C
      I0= 0
      I1= 1
      I5= 5
C
C     initialize array counter for binary printout, store variable
C     names in local strings for use in building binary headers
      ACNT = 0
      CSTAT1(1) = 'SMSTM'
      CSTAT1(2) = 'UMSTM'
      CSTAT1(3) = 'IMSTM'
      CSTAT1(4) = 'LMSTM'
      CSTAT1(5) = 'AMSTM'
      CFRAC(1)  = 'FSO'
      CFRAC(2)  = 'FSP'
      CFRAC(3)  = 'FII'
      CFRAC(4)  = 'FUP'
      CFRAC(5)  = 'FIO'
      CFRAC(6)  = 'FLP'
      CFRAC(7)  = 'FLDP'
      CFRAC(8)  = 'FAO'
C
C     mstlay has no fluxes only state variables
C
      IF (PRINTU .GT. 0 .AND. PFLAG(8) .LE. LEV) THEN
C       write heading to unit printu
        WRITE (PRINTU,2000)
        WRITE (PRINTU,2010)
        WRITE (PRINTU,2020)
C
C       write fractions to unit printu
        WRITE (PRINTU,2030)
        WRITE (PRINTU,2040)  FRAC
C
        WRITE (PRINTU,2070)  AGMAID
        WRITE (PRINTU,2080)
      END IF
      IF (BINU .GT. 0 .AND. ABS(BFLAG(8)) .LE. LEV) THEN
C       compile values for binary printout
        DO 10 I = 1, 8
          ACNT = ACNT + 1
          APRINT(ACNT) = FRAC(I)
          CHEAD(ACNT) = CFRAC(I)
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 10     CONTINUE
      END IF
C
C     convert moisture storages to external units
      CALL TRNVEC (I5,MST,MFACTA,MFACTB,
     O             PSTAT1)
C
      IF (PRINTU .GT. 0 .AND. PFLAG(8) .LE. LEV) THEN
        WRITE (PRINTU,2090)  PSTAT1
      END IF
      IF (BINU .GT. 0 .AND. ABS(BFLAG(8)) .LE. LEV) THEN
C       compile values for binary printout
        DO 20 I = 1, 5
          ACNT = ACNT + 1
          APRINT(ACNT) = PSTAT1(I)
          CHEAD(ACNT) = CSTAT1(I)
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 20     CONTINUE
      END IF
C
      IF (BINU .GT. 0 .AND. ABS(BFLAG(8)) .LE. LEV) THEN
C       write binary output
        CALL EXDATE(
     I              DATIM,
     O              EXDAT)
        IF (BFLAG(8) .GT. 0) THEN
C         at start of run, write the header
          WRITE (BINU) I0,'PERLND  ',LSNO,'MSTLAY  ',
     1          (CLEN(I),(CHEAD(I)(J:J),J=1,CLEN(I)),I=1,ACNT)
C         set bflag to negative to not write headers anymore
          BFLAG(8) = -BFLAG(8)
        END IF
        WRITE (BINU) I1,'PERLND  ', LSNO,'MSTLAY  ',UNITFG,
     1               LEV,(EXDAT(I),I=1,5),(APRINT(I),I=1,ACNT)
      END IF
C
      RETURN
      END
C
C     4.2(1).8.2
C
      SUBROUTINE SUBLAY
     I                  (LZS,IGWI,AGWI,CFINMA,LZSN,
     $                   LLPF,AGWS,AGWO,
     O                   MST,FRAC)
C
C     + + + PURPOSE + + +
C     Estimate the moisture and the fraction of solutes being
C     transported in the subsurface layers (lower layer and
C     groundwater layer)
C
C     + + + DUMMY ARGUMENTS + + +
      REAL       AGWI,AGWO,AGWS,CFINMA,FRAC(8),IGWI,LLPF,LZS,
     $           LZSN,MST(5)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LZS    - ???
C     IGWI   - ???
C     AGWI   - ???
C     CFINMA - ???
C     LZSN   - ???
C     LLPF   - ???
C     AGWS   - ???
C     AGWO   - ???
C     MST    - ???
C     FRAC   - ???
C
C     + + + LOCAL VARIABLES + + +
      REAL       AMST,AMSTM,FAO,FLDP,FLP,LLMPF,LMST,LMSTM
C
C     + + + END SPECIFICATIONS + + +
C
C     note that this subroutine only affects elements 4 & 5 of mst,
C     elements 6-8 of frac
C
C     ******************************************************
C     warning,  the equations in this subroutine are based
C     on those in the arm model.  some of the equations are
C     not dimensionally homogeneous.  for example:
C           amst( )= agws(inches) + agwo(inches/interval)
C     thus, the results obtained for solute movement,
C     particularly in the surface & upper layers, will
C     probably be highly dependent on the simulation time
C     interval (delt).  the arm model used delt of 5 mins
C     and (occasionally) of 15 mins.
C     ******************************************************
C
C     find the lower layer moisture storage
      LMST= LZS+ (IGWI+AGWI)
C
      IF (LMST.GT.0.0) THEN
C       there is lower layer moisture storage
C       convert lower layer moisture storage to mass/area units
        LMSTM= LMST*CFINMA
C
C       calculate the percolating solutes retardation factor
C       for lzs < (lzsn*llpf)
        LLMPF= LZS/(LZSN*LLPF)
        IF (LLMPF.GT.1.0)  LLMPF= 1.0
C
C       determine the fraction of the lower layer solute
C       in storage being percolated
C       to active groundwater
        FLP= LLMPF*AGWI/LMST
C       to inactive groundwater by deep percolation
        FLDP= LLMPF*IGWI/LMST
      ELSE
C       there is no lower layer moisture storage so zero variables
        LMSTM= 0.0
        FLP  = 0.0
        FLDP = 0.0
      END IF
C
C     find the active groundwater moisture storage
      AMST= AGWS+ AGWO
C
      IF (AMST.GT.0.0) THEN
C       there is active groundwater moisture storage
C       convert to mass units
        AMSTM= AMST*CFINMA
C
C       determine the fraction of the active groundwater solute
C       in storage being removed by groundwater outflow
        FAO= AGWO/AMST
      ELSE
C       there is no active groundwater moisture storage
        AMSTM= 0.0
        FAO  = 0.0
      END IF
C
C     place computed values in groups
      MST(4) = LMSTM
      MST(5) = AMSTM
      FRAC(6)= FLP
      FRAC(7)= FLDP
      FRAC(8)= FAO
C
      RETURN
      END
C
C     4.2(1).8.1
C
      SUBROUTINE TOPLAY
     I                  (SURSS,SURI,CFINMA,SURO,INFIL,IFWI,UZI,SLMPF,
     I                   UZS,ULPF,PERC,UZSN,IFWS,IFWO,
     O                   MST,FRAC)
C
C     + + + PURPOSE + + +
C     Estimate the moisture and the fraction of solutes being
C     transported in the topsoil (surface layer and upper
C     layer)
C
C     + + + DUMMY ARGUMENTS + + +
      REAL       CFINMA,FRAC(5),IFWI,IFWO,IFWS,INFIL,MST(3),
     $           PERC,SLMPF,SURI,SURO,SURSS,ULPF,UZI,UZS,UZSN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     SURSS  - ???
C     SURI   - ???
C     CFINMA - ???
C     SURO   - surface output
C     INFIL  - ???
C     IFWI   - ???
C     UZI    - ???
C     SLMPF  - ???
C     UZS    - initial upper zone storage
C     ULPF   - ???
C     PERC   - ???
C     UZSN   - upper zone nominal storage
C     IFWS   - ???
C     IFWO   - ???
C     MST    - ???
C     FRAC   - ???
C
C     + + + LOCAL VARIABLES + + +
      REAL       FII,FIO,FSO,FSP,FUP,IMSTM,ISMST,SDOWN,SMST,SMSTM,
     $           UDOWN,ULMPF,UMST,UMSTM
C
C     + + + END SPECIFICATIONS + + +
C
C     note that this subroutine only affects elements 1-3 of mst and
C     elements 1-5 of frac
C
C     ******************************************************
C     warning,  the equations in this subroutine are based
C     on those in the arm model.  some of the equations are
C     not dimensionally homogeneous.  for example:
C           smst( )= surss(inches) + suri(inches/interval)
C     thus, the results obtained for solute movement,
C     particularly in the surface & upper layers, will
C     probably be highly dependent on the simulation time
C     interval (delt).  the arm model used delt of 5 mins
C     and (occasionally) of 15 mins.
C     ******************************************************
C
C     find the surface layer moisture storage
      SMST= SURSS+ SURI
C
      IF (SMST.GT.0.0) THEN
C       there is surface layer moisture storage
C       convert surface layer moisture storage to mass/area units
        SMSTM= SMST*CFINMA
C
C       determine the fraction of the surface layer solute in
C       storage being removed in surface runoff
        FSO= SURO/SMST
C
C       total downward water flux from the surface layer
        SDOWN= INFIL+ IFWI+ UZI
C
C       determine the fraction of the surface layer solute in
C       storage being percolated
C       the percolation multiplier factor (slmpf) is inputed not
C       calculated
        FSP= SLMPF*SDOWN/SMST
      ELSE
C       there is no surface moisture storage, so zero variables
        SMSTM= 0.0
        FSO  = 0.0
        FSP  = 0.0
      END IF
C
C     find the upper layer principal moisture storage
      UMST= UZS+ (IFWI+PERC+INFIL)
C
      IF (UMST.GT.0.0) THEN
C       there is upper layer principal moisture storage
C       convert moisture storage to mass/area units
        UMSTM= UMST*CFINMA
C
C       determine the fraction of the upper layer solute in principal
C       storage going to upper layer transitory (interflow) storage
        FII= IFWI/UMST
C
C       total downward water flux from the upper layer
        UDOWN= INFIL+ PERC
C
C       calculate the percolating solutes retardation factor
C       for uzs < (uzsn*ulpf)
        ULMPF= UZS/(UZSN*ULPF)
        IF (ULMPF.GT.1.0)  ULMPF= 1.0
C
C       determine the fraction of the upper layer solute in storage
C       being percolated
        FUP= ULMPF*UDOWN/UMST
      ELSE
C       there is no upper layer moisture storage, so zero variables
        UMSTM= 0.0
        FII  = 0.0
        FUP  = 0.0
      END IF
C
C     find upper layer transitory (interflow) moisture storage
      ISMST= IFWS+ IFWO
      IMSTM= ISMST*CFINMA
C
      IF (ISMST.GT.0.0) THEN
C       there is upper layer transitory (interflow) moisture storage
C       determine the fraction of the upper layer transitory
C       (interflow) solute in storage being removed by interflow
C       outflow
        FIO= IFWO/ISMST
      ELSE
        FIO= 0.0
      END IF
C
C     place computed values in groups
      MST(1) = SMSTM
      MST(2) = UMSTM
      MST(3) = IMSTM
      FRAC(1)= FSO
      FRAC(2)= FSP
      FRAC(3)= FII
      FRAC(4)= FUP
      FRAC(5)= FIO
C
      RETURN
      END
