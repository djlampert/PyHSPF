C
C
C
      SUBROUTINE   PTRACR
C
C     + + + PURPOSE + + +
C     Process input for section tracer
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION TRACER1 + + +
      INCLUDE    'cpltr.inc'
      INCLUDE    'crin2.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,I1,I2,I4,J,N,RETCOD,K
      REAL      R0
C
C     + + + EXTERNALS + + +
      EXTERNAL  RTABLE,ITABLE
      EXTERNAL  ZIPR,MDATBL
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' PROCESSING INPUT FOR SECTION TRACER')
 2010 FORMAT (/,' SEGMENT-WIDE STORAGES')
 2020 FORMAT (/,' STORAGES IN BLOCK',I3)
 2030 FORMAT (/,'  SURFACE    UPPER   INTERFLOW')
 2040 FORMAT (  '   LAYER     LAYER    STORAGE')
 2050 FORMAT (  ' ',1P3E10.3)
 2060 FORMAT (/,' TOTAL STORAGE IN THE SEGMENT: ',1PE10.3)
 2070 FORMAT (/,' FINISHED PROCESSING INPUT FOR SECTION TRACER')
C
C     + + + END SPECIFICATIONS + + +
C
      I1= 1
      R0 = 0.0
C
      IF (OUTLEV.GT.1) THEN
C       processing message
        WRITE (MESSU,2000)
      END IF
C
C     initialize month-data input
      I= 24
      CALL ZIPR (I,R0,
     O           TRAFXM)
      CALL ZIPR (I,R0,
     O           TRACNM)
C
C     initialize atmospheric deposition fluxes
      I= 10
      CALL ZIPR (I,R0,
     O           TRCFX3)
      CALL ZIPR (I,R0,
     O           TRCFX4)
C
C     warning message counter initialization
      TRWCNT(1)= 0
C
C     get atmospheric deposition flags - table-type trac-ad-flags
      I2= 144
      I4= 4
      CALL ITABLE (I2,I1,I4,UUNITS,
     M             TRADFG)
C
C     read in month-data tables where necessary
      DO 40 J= 1, 2
        N= 2*(J- 1)+ 1
        IF (TRADFG(N) .GT. 0) THEN
C         monthly flux must be read
          CALL MDATBL
     I                (TRADFG(N),
     O                 TRAFXM(1,J),RETCOD)
C         convert units to internal - not done by MDATBL
C         from lb/ac.day to lb/ac.ivl or from kg/ha.day to kg/ha.ivl
          DO 10 K= 1, 12
            TRAFXM(K,J)= TRAFXM(K,J)*DELT60/24.0
 10       CONTINUE
        END IF
        IF (TRADFG(N+1) .GT. 0) THEN
C         monthly ppn conc must be read
          CALL MDATBL
     I                (TRADFG(N+1),
     O                 TRACNM(1,J),RETCOD)
C         convert units to internal - not done by MDATBL
          IF (UUNITS .EQ. 1) THEN
C           convert from mg/l to lb/ac.in
            DO 20 K= 1, 12
              TRACNM(K,J)= TRACNM(K,J)*0.226635
 20         CONTINUE
            ELSE IF (UUNITS .EQ. 2) THEN
C             convert from mg/l to kg/ha.in
              DO 30 K= 1, 12
                TRACNM(K,J)= TRACNM(K,J)*0.01
 30           CONTINUE
            END IF
        END IF
 40   CONTINUE
C
C     table-type trac-id
      I2= 145
      I4= 5
      CALL RTABLE (I2,I1,I4,UUNITS,
     M             TRACID)
C
C     initial storages
      IF (OUTLEV.GT.2) THEN
C       segment wide message
        WRITE (MESSU,2010)
      END IF
      I2= 146
      I4= 3
      CALL RTABLE (I2,I1,I4,UUNITS,
     M             TRST1(1))
C
C     lower and active groundwater layer storages - table-type
C     tra-substor
      I2= 147
      I4= 2
      CALL RTABLE (I2,I1,I4,UUNITS,
     M             TRST1(4))
C
C     determine total storage in the system
      TRSU= 0.0
      DO 80 I= 1,5
        TRSU= TRSU+ TRST1(I)
 80   CONTINUE
C
      IF (OUTLEV.GT.2) THEN
C       storage message
        WRITE (MESSU,2060)  TRSU
      END IF
C
      IF (OUTLEV.GT.1) THEN
C       finished processing message
        WRITE (MESSU,2070)
      END IF
C
      RETURN
      END
C
C     4.2(1).12
C
      SUBROUTINE   TRACER
C
C     + + + PURPOSE + + +
C     Simulate movement of a tracer
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION TRACER2 + + +
      INCLUDE   'cpltr.inc'
      INCLUDE   'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,J,N,REQFG,TSSUB(2),FLGVAL
      REAL        TRADFX,TRADCN
      CHARACTER*6 OPTYP,TSNAM,SECNAM,MSECNM,OPFGNM
C
C     + + + FUNCTIONS + + +
      REAL        DAYVAL
C
C     + + + EXTERNALS + + +
      EXTERNAL  TOPMOV,SUBMOV,DAYVAL,HREQTS
C
C     + + + DATA INITIALIZATIONS + + +
      DATA TSSUB/1,1/
      DATA OPTYP,SECNAM,MSECNM/'PERLND','TRACER','MSTLAY'/
C
C     + + + END SPECIFICATIONS + + +
C
      IF (MSTLFG.EQ.0.AND.PESTFG.EQ.0.AND.NITRFG.EQ.0.AND.PHOSFG.EQ.0)
     $    THEN
C       read time series supplied by mstlay
        REQFG= 3
        TSNAM= 'MST   '
        DO 10 J= 1,5
CTHJ          MST(J)= PAD(MSTFP(J)+IVL1)
          CALL HREQTS (MSTFP(J),IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,
     I                 LSNO,TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,FLGVAL,
     O                 MST(J))
 10     CONTINUE
C
        TSNAM= 'FRAC  '
        DO 20 J= 1,8
CTHJ          FRAC(J)= PAD(FRACFP(J)+IVL1)
          CALL HREQTS (FRACFP(J),IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,
     I                 LSNO,TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,FLGVAL,
     O                 FRAC(J))
 20     CONTINUE
      ELSE
C       the above time series are available from mstlay or
C       other agri-chemical sections
C
      END IF
CTHJ      PREC= PAD(PRECFP+IVL1)
      REQFG= 2
      TSNAM= 'PREC  '
      CALL HREQTS (PRECFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,
     I             LSNO,TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,FLGVAL,
     O             PREC)
C
C     get lateral inflows
      DO 65 I= 1, 5
        IF (LITRSX(I) .GE. 1) THEN
C         lateral inflow of solution phosphate
          LITRS(I)= PAD(LITRSX(I)+IVL1)
        ELSE
          LITRS(I)= 0.0
        END IF
 65   CONTINUE
C
C     compute atmospheric deposition influx
      DO 70 I= 1, 2
        N= 2*(I-1)+ 1
C       dry deposition
        IF (TRADFG(N) .LE. -1) THEN
CTHJ          TRADDR(I)= PAD(TRAFFP(I)+IVL1)
          REQFG= 4
          OPFGNM= 'TRADFG'
          TSNAM= 'TRADFX'
          TSSUB(1)= I
          CALL HREQTS (TRAFFP(I),IVL1,REQFG,MESSU,MSGFL,DATIM,
     I                 OPTYP,LSNO,TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,
     I                 TRADFG(N),
     O                 TRADFX)
          TRADDR(I)= TRADFX
        ELSE IF (TRADFG(N) .GE. 1) THEN
          TRADDR(I)= DAYVAL(TRAFXM(MON,I),TRAFXM(NXTMON,I),DAY,NDAYS)
        ELSE
          TRADDR(I)= 0.0
        END IF
C       wet deposition
        IF (TRADFG(N+1) .LE. -1) THEN
CTHJ          TRADWT(I)= PREC*PAD(TRACFP(I)+IVL1)
          REQFG= 4
          OPFGNM= 'TRADFG'
          TSNAM= 'TRADWT'
          TSSUB(1)= I
          CALL HREQTS (TRACFP(I),IVL1,REQFG,MESSU,MSGFL,DATIM,
     I                 OPTYP,LSNO,TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,
     I                 TRADFG(N+1),
     O                 TRADCN)
          TRADWT(I)= PREC*TRADCN
        ELSE IF (TRADFG(N+1) .GE. 1) THEN
          TRADWT(I)= PREC*DAYVAL(TRACNM(MON,I),TRACNM(NXTMON,I),DAY,
     I                           NDAYS)
        ELSE
          TRADWT(I)= 0.0
        END IF
        TRADEP(I)= TRADDR(I)+ TRADWT(I)
 70   CONTINUE
      TSSUB(1)= 1
C
C     update storages for atmospheric deposition
      STRSU= STRSU+ LITRS(1)+ TRADEP(1)
      UTRSU= UTRSU+ LITRS(2)+ TRADEP(2)
      ITRSU= ITRSU+ LITRS(3)
C
C     move tracer with water in the topsoil layers
      CALL TOPMOV (FRAC,
     M             STRSU,UTRSU,ITRSU,
     O             TSTRS)
C
C     add lateral inflows
      LTRSU= LTRSU+ LITRS(4)
      ATRSU= ATRSU+ LITRS(5)
C
C     transport tracer in the subsurface layers
      CALL SUBMOV (TSTRS(3),FRAC(6),FRAC(7),FRAC(8),
     M             LTRSU,ATRSU,
     O             SSTRS)
C
C     find total tracer outflow
      POTRS= TSTRS(1)+ TSTRS(5)+ SSTRS(3)
C
C     total tracer in storage
      TRSU= STRSU+ UTRSU+ ITRSU+ LTRSU+ ATRSU
C
      RETURN
      END
C
C     4.2(1).15.1.9
C
      SUBROUTINE   TRACC
     I                   (FRMROW,TOROW)
C
C     + + + PURPOSE + + +
C     Accumulate fluxes for section tracer
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    FRMROW,TOROW
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FRMROW - row containing incremental flux accumulation
C     TOROW  - flux row to be incremented
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION TRACER2 + + +
      INCLUDE    'cpltr.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I3,I5,I
C
C     + + + EXTERNALS + + +
      EXTERNAL   ACCVEC
C
C     + + + END SPECIFICATIONS + + +
C
      I3=3
      I5=5
C
      CALL ACCVEC (I5,TRLIF(1,FRMROW),
     M             TRLIF(1,TOROW))
C
      CALL ACCVEC (I5,TRCFX1(1,FRMROW),
     M             TRCFX1(1,TOROW))
C
      CALL ACCVEC (I3,TRCFX2(1,FRMROW),
     M             TRCFX2(1,TOROW))
C
      DO 10 I= 1, 2
        TRCFX3(I,TOROW)= TRCFX3(I,TOROW)+ TRCFX3(I,FRMROW)
        TRCFX4(I,TOROW)= TRCFX4(I,TOROW)+ TRCFX4(I,FRMROW)
 10   CONTINUE
C
      RETURN
      END
C
C     4.2(1).14.9
C
      SUBROUTINE   TRACPB
C
C     + + + PURPOSE + + +
C     Handle section TRACER.
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION TRACER2 + + +
      INCLUDE    'cpltr.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    J
C
C     + + + END SPECIFICATIONS + + +
C
C     handle section tracer
      DO 10 J= 1,5
        IF (TSTRFP(J).GE.1) THEN
          PAD(TSTRFP(J)+IVL1)= TSTRS(J)
        END IF
 10   CONTINUE
C
      DO 20 J= 1,3
        IF (SSTRFP(J).GE.1) THEN
          PAD(SSTRFP(J)+IVL1)= SSTRS(J)
        END IF
 20   CONTINUE
C
      DO 30 J= 1, 2
        IF (TRADDX(J) .GE. 1) THEN
          PAD(TRADDX(J)+IVL1)= TRADDR(J)
        END IF
        IF (TRADWX(J) .GE. 1) THEN
          PAD(TRADWX(J)+IVL1)= TRADWT(J)
        END IF
        IF (TRADPX(J) .GE. 1) THEN
          PAD(TRADPX(J)+IVL1)= TRADEP(J)
        END IF
 30   CONTINUE
C
      IF (POTRFP.GE.1) THEN
        PAD(POTRFP+IVL1)= POTRS
      END IF
C
      RETURN
      END
C
C     4.2(1).13.12
C
      SUBROUTINE   TRACPT
C
C     + + + PURPOSE + + +
C     Handle section TRACER.
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION TRACER2 + + +
      INCLUDE 'cpltr.inc'
      INCLUDE 'cmpad.inc'
C
C     + + + END SPECIFICATIONS + + +
C
C     handle section tracer
      IF (STRSFP.GE.1) THEN
        PAD(STRSFP+IVL1)= STRSU
      END IF
C
      IF (UTRSFP.GE.1) THEN
        PAD(UTRSFP+IVL1)= UTRSU
      END IF
C
      IF (ITRSFP.GE.1) THEN
        PAD(ITRSFP+IVL1)= ITRSU
      END IF
C
      IF (LTRSFP.GE.1) THEN
        PAD(LTRSFP+IVL1)= LTRSU
      END IF
C
      IF (ATRSFP.GE.1) THEN
        PAD(ATRSFP+IVL1)= ATRSU
      END IF
C
      IF (TRSUFP.GE.1) THEN
        PAD(TRSUFP+IVL1)= TRSU
      END IF
C
      RETURN
      END
C
C     4.2(1).15.2.12
C
      SUBROUTINE   TRAPRT
     I                    (LEV,PRINTU,AGMAID,MFACTA,MFACTB,UNITFG,BINU)
C
C     + + + PURPOSE + + +
C     Convert quantities from internal to external units, and
C     produce printout
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
C     UNITFG - ???
C     BINU   - fortran unit number on which to write binary output
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION TRACER2 + + +
      INCLUDE    'cpltr.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I0,I1,I3,I5,I,J,N,ADFG,LATFG,ACNT,CLEN(21),EXDAT(5)
      REAL        MATIN,MATDIF,PCFLX1(5),PCFLX2(3),PPOTRS,PSTAT(5),
     $            PSTOR,PSTORS,TOTAL,PCFLX3(2),PCFLX4(2),PADTOT(2),
     $            PADALL,PLIFX(5),PLITRS,APRINT(21)
      CHARACTER*8   UNITID,CSTAT(5),CCFLX3(2),CCFLX4(2),CADTOT(2),
     $              CCFLX1(5),CCFLX2(3)
      CHARACTER*20  TRACNAME
      CHARACTER*256 CHEAD(21)
C
C     + + + EXTERNALS + + +
      EXTERNAL   TRNVEC,BALCHK,EXDATE
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' ','*** TRACER ***')
 2020 FORMAT (/,' ','  STATE VARIABLES   ',A8)
 2030 FORMAT (' ','    STORAGES BY LAYERS',11X,'SURFACE',5X,
     $        'UPPER PRINCIPAL  UPPER TRANS(INTER)     LOWER',
     $        '  ACTIVE GROUNDWATER',15X,'TOTAL')
 2040 FORMAT (' ',6X,5A4,4X,F10.3,10X,F10.3,10X,2F10.3,
     $        2(10X,F10.3))
 2045 FORMAT (5A4)
 2050 FORMAT (/,'   FLUXES',12X,A8)
 2060 FORMAT (  '     ATMOSPHERIC DEPOSITION    <-------SURFACE',
     #          ' LAYER--------><------UPPER LAYER PRIN------>')
 2070 FORMAT (  31X,'       DRY       WET     TOTAL       DRY',
     #          '       WET     TOTAL')
 2080 FORMAT (  31X,6(1PE10.3))
 2081 FORMAT (  '     LATERAL INFLOWS              SURFACE     UPPER',
     $          ' INTERFLOW     LOWER ACTIVE GW     TOTAL')
 2082 FORMAT (  31X,'    SLITRS    ULITRS    ILITRS    LLITRS',
     $          '    ALITRS    TLITRS')
 2083 FORMAT (  31X,1P6E10.3,/)
 2090 FORMAT (  '     FLOWS (ALL IN SOLUTION)   <--SURFACE LAYER--->',
     $  '<-UPPER LAYER PRIN-> INTERFLOW<---LOWER LAYER----> GRNDWATER',
     $        15X,'TOTAL')
 2100 FORMAT (' ',30X,'   OUTFLOW      PERC      PERC  TO TRANS   ',
     $        'OUTFLOW      PERC DEEP PERC   OUTFLOW',13X,'OUTFLOW')
 2110 FORMAT (' ',6X,5A4,9X,'SOTRS     SPTRS     UPTRS     IITRS',
     $        5X,'IOTRS     LPTRS    LDPTRS     AOTRS',15X,'POTRS')
 2120 FORMAT (' ',30X,8(1PE10.3),10X,(1PE10.3))
C
C     + + + END SPECIFICATIONS + + +
C
      I0= 0
      I1= 1
      I3= 3
      I5= 5
C
C     initialize array counter for binary printout, store variable
C     names in local strings for use in building binary headers
      ACNT = 0
      CSTAT(1)  = ' - SURFACE STORAGE'
      CSTAT(2)  = ' - UPPER PRINCIPAL STORAGE'
      CSTAT(3)  = ' - INTERFLOW STORAGE'
      CSTAT(4)  = ' - LOWER STORAGE'
      CSTAT(5)  = ' - ACTIVE GW STORAGE'
      CCFLX3(1) = 'ATMOSPHERIC DEPOSITION - SURFACE LAYER - DRY'
      CCFLX3(2) = 'ATMOSPHERIC DEPOSITION - UPPER LAYER - DRY'
      CCFLX4(1) = 'ATMOSPHERIC DEPOSITION - SURFACE LAYER - WET'
      CCFLX4(2) = 'ATMOSPHERIC DEPOSITION - UPPER LAYER - WET'
      CADTOT(1) = 'ATMOSPHERIC DEPOSITION - SURFACE LAYER - TOTAL'
      CADTOT(2) = 'ATMOSPHERIC DEPOSITION - UPPER LAYER - TOTAL'
      CCFLX1(1) = 'SOTRS'
      CCFLX1(2) = 'SPTRS'
      CCFLX1(3) = 'UPTRS'
      CCFLX1(4) = 'IITRS'
      CCFLX1(5) = 'IOTRS'
      CCFLX2(1) = 'LPTRS'
      CCFLX2(2) = 'LDPTRS'
      CCFLX2(3) = 'AOTRS'
      WRITE (TRACNAME,2045) (TRACID(I),I=1,5)
      TRACNAME = ADJUSTL(TRACNAME)
C
      IF (PRINTU .GT. 0 .AND. PFLAG(12) .LE. LEV) THEN
        WRITE (PRINTU,2000)
C
C       print headings on unit printu
        WRITE (PRINTU,2020)  AGMAID
        WRITE (PRINTU,2030)
      END IF
C
      CALL TRNVEC (I5,TRST1,MFACTA,MFACTB,
     O             PSTAT)
      TOTAL= PSTAT(1)+ PSTAT(2)+ PSTAT(3)+ PSTAT(4)+ PSTAT(5)
      IF (PRINTU .GT. 0 .AND. PFLAG(12) .LE. LEV) THEN
        WRITE (PRINTU,2040)  TRACID, PSTAT, TOTAL
      END IF
      IF (BINU .GT. 0 .AND. ABS(BFLAG(12)) .LE. LEV) THEN
C       compile values for binary printout
        DO 10 I = 1, 5
          ACNT = ACNT + 1
          APRINT(ACNT) = PSTAT(I)
          CHEAD(ACNT) = TRIM(TRACNAME) // CSTAT(I)
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 10     CONTINUE
        ACNT = ACNT + 1
        APRINT(ACNT) = TOTAL
        CHEAD(ACNT) = TRIM(TRACNAME) // ' - TOTAL STORAGE'
        CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
      END IF
C
      IF (PRINTU .GT. 0 .AND. PFLAG(12) .LE. LEV) THEN
        WRITE (PRINTU,2050)  AGMAID
      END IF
C
      LATFG= 0
      ADFG= 0
      DO 20 N= 1, 5
        IF (LITRSX(N) .GE. 1) THEN
          LATFG= 1
        END IF
 20   CONTINUE
      DO 30 I= 1, 2
        N= 2*(I- 1)+ 1
        IF ( (TRADFG(N) .NE. 0) .OR. (TRADFG(N+1) .NE. 0) ) THEN
          ADFG= 1
        END IF
 30   CONTINUE
C
      PADALL= 0.0
      IF (ADFG .EQ. 1) THEN
        DO 40 I= 1, 2
          N= 2*(I- 1)+ 1
          IF (TRADFG(N) .NE. 0) THEN
            PCFLX3(I)= TRCFX3(I,LEV)*MFACTA
          ELSE
            PCFLX3(I)= 0.0
          END IF
          IF (TRADFG(N+1) .NE. 0) THEN
            PCFLX4(I)= TRCFX4(I,LEV)*MFACTA
          ELSE
            PCFLX4(I)= 0.0
          END IF
          PADTOT(I)= PCFLX3(I)+ PCFLX4(I)
          PADALL= PADALL+ PADTOT(I)
 40     CONTINUE
C
        IF (PRINTU .GT. 0 .AND. PFLAG(12) .LE. LEV) THEN
          WRITE (PRINTU,2060)
          WRITE (PRINTU,2070)
          WRITE (PRINTU,2080) PCFLX3(1),PCFLX4(1),PADTOT(1),
     #                        PCFLX3(2),PCFLX4(2),PADTOT(2)
        END IF
        IF (BINU .GT. 0 .AND. ABS(BFLAG(12)) .LE. LEV) THEN
          DO 50 I = 1, 2
            ACNT = ACNT + 1
            APRINT(ACNT) = PCFLX3(I)
            CHEAD(ACNT) = CCFLX3(I)
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
            ACNT = ACNT + 1
            APRINT(ACNT) = PCFLX4(I)
            CHEAD(ACNT) = CCFLX4(I)
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
            ACNT = ACNT + 1
            APRINT(ACNT) = PADTOT(I)
            CHEAD(ACNT) = CADTOT(I)
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 50       CONTINUE
        END IF
C
      END IF
C
      PLITRS= 0.0
      IF (LATFG .EQ. 1) THEN
C       lateral inflows to print
        DO 60 N= 1, 5
          PLIFX(N)= TRLIF(N,LEV)*MFACTA
          PLITRS= PLITRS+ PLIFX(N)
 60     CONTINUE
C
      END IF
      IF (PRINTU .GT. 0 .AND. PFLAG(12) .LE. LEV) THEN
        WRITE (PRINTU,2090)
        WRITE (PRINTU,2100)
        WRITE (PRINTU,2110)  TRACID
      END IF
      CALL TRNVEC (I5,TRCFX1(1,LEV),MFACTA,MFACTB,
     O             PCFLX1)
C
      CALL TRNVEC (I3,TRCFX2(1,LEV),MFACTA,MFACTB,
     O             PCFLX2)
C
      PPOTRS= PCFLX1(1)+ PCFLX1(5)+ PCFLX2(3)
C
      IF (PRINTU .GT. 0 .AND. PFLAG(12) .LE. LEV) THEN
        WRITE (PRINTU,2120)  PCFLX1, PCFLX2, PPOTRS
      END IF
      IF (BINU .GT. 0 .AND. ABS(BFLAG(12)) .LE. LEV) THEN
        DO 70 I = 1, 5
          ACNT = ACNT + 1
          APRINT(ACNT) = PCFLX1(I)
          CHEAD(ACNT) = CCFLX1(I)
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 70     CONTINUE
        DO 80 I = 1, 3
          ACNT = ACNT + 1
          APRINT(ACNT) = PCFLX2(I)
          CHEAD(ACNT) = CCFLX2(I)
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 80     CONTINUE
        ACNT = ACNT + 1
        APRINT(ACNT) = PPOTRS
        CHEAD(ACNT) = 'POTRS'
        CLEN(ACNT) = 5
      END IF
C
C     tracer balance check and report
      IF (UNITFG .EQ. 1) THEN
C       english
        UNITID= '   LB/AC'
      ELSE
C       metric
        UNITID= '   KG/HA'
      END IF
C
C     convert storages to external units for balance
      PSTORS= TOTTR(LEV)*MFACTA+ MFACTB
      PSTOR = TOTTR(1)*MFACTA+ MFACTB
C
C     find the net output of tracer from the pls
      MATIN= PADALL+ PLITRS
      MATDIF= MATIN- PPOTRS- PCFLX2(2)
C
      CALL BALCHK (I1,LSNO,DATIM,MESSU,PRINTU,MSGFL,
     $             PSTORS,PSTOR,MATIN,MATDIF,UNITID,I1,
     M             TRWCNT(1))
C
      IF (BINU .GT. 0 .AND. ABS(BFLAG(12)) .LE. LEV) THEN
C       write binary output
        CALL EXDATE(
     I              DATIM,
     O              EXDAT)
        IF (BFLAG(12) .GT. 0) THEN
C         at start of run, write the header
          WRITE (BINU) I0,'PERLND  ',LSNO,'TRACER  ',
     1          (CLEN(I),(CHEAD(I)(J:J),J=1,CLEN(I)),I=1,ACNT)
C         set bflag to negative to not write headers anymore
          BFLAG(12) = -BFLAG(12)
        END IF
        WRITE (BINU) I1,'PERLND  ', LSNO,'TRACER  ',UNITFG,
     1               LEV,(EXDAT(I),I=1,5),(APRINT(I),I=1,ACNT)
      END IF
C
      RETURN
      END
C
C     4.2(1).15.3.9
C
      SUBROUTINE   TRRST
     I                   (LEV)
C
C     + + + PURPOSE + + +
C     Reset all flux accumulators and those state variablesc
C     used in material balance check for section tracer
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    LEV
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEV    - current output level (2-pivl,3-day,4-mon,5-ann)
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION TRACER2 + + +
      INCLUDE    'cpltr.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I3,I5,I
C
C     + + + EXTERNALS + + +
      EXTERNAL  SETVEC
C
C     + + + END SPECIFICATIONS + + +
C
      I3=3
      I5=5
C     set flux accumulators to zero
C
      CALL SETVEC (I5,0.0,
     O             TRLIF(1,LEV))
C
      CALL SETVEC (I5,0.0,
     O             TRCFX1(1,LEV))
C
      CALL SETVEC (I3,0.0,
     O             TRCFX2(1,LEV))
C
      DO 10 I= 1, 2
        TRCFX3(I,LEV)= 0.0
        TRCFX4(I,LEV)= 0.0
 10   CONTINUE
C
C     keep storage in state variable used for
C     material balance check
C
      TOTTR(LEV)= TOTTR(1)
C
      RETURN
      END
