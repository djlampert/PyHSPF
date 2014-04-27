C
C
C
      SUBROUTINE   PPEST
C
C     + + + PURPOSE + + +
C     Process input for section pest
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PEST1 + + +
      INCLUDE    'cplpe.inc'
      INCLUDE    'crin2.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      ADOPF4,I1,I2A,I2B,I4A,I4B,I,L,N,SUB,SUBBAS,SUBI,
     #             RETCOD,K
      REAL         RVAL(4),R0
      CHARACTER*60 HEADG
C
C     + + + EXTERNALS + + +
      EXTERNAL     ITABLE,SOLDAT,RTABLE,FIRSTP,SVALP,NONSVP,STORGE
      EXTERNAL     ZIPR,MDATBL
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' PROCESSING INPUT FOR SECTION PEST')
 2010 FORMAT (/,' ',90('-'))
 2020 FORMAT (/,' ANY INITIAL STORAGES OF CRYSTALLINE PESTICIDE ',
     $            'WILL BE MOVED TO ADSORBED',
     $        /,' STATE BECAUSE FIRST-ORDER ',
     $            'ADSORPTION/DESORPTION TECHNIQUE DOES NOT',
     $        /,' HANDLE CRYSTALLINE MATERIAL')
 2030 FORMAT (/,' FINISHED PROCESSING INPUT FOR SECTION PEST')
C
C     + + + END SPECIFICATIONS + + +
C
      I1= 1
      R0= 0.0
C
      IF (OUTLEV.GT.1) THEN
C       processing section message
        WRITE (MESSU,2000)
      END IF
C
C     initialize month-data input
      I= 108
      CALL ZIPR (I,R0,
     O           PEAFXM)
      CALL ZIPR (I,R0,
     O           PEACNM)
C
C     initialize atmospheric deposition fluxes
      I= 45
      CALL ZIPR (I,R0,
     O           PSCFX5)
      CALL ZIPR (I,R0,
     O           PSCFX6)
C
      MSGFL = FILE(15)
C
C     warning and error message counter initialization
      PSWCNT(1)= 0
      PSWCNT(2)= 0
      PSWCNT(3)= 0
      PSWCNT(4)= 0
      PSWCNT(5)= 0
      PSWCNT(6)= 0
C
C     heading for printing out total storages in surface, upper,
C     lower, and active groundwater layers
      HEADG = '     Cryst       Ads      Soln'
C
C     table-type pest-flags
      I2A= 86
      I4A= 7
      CALL ITABLE (I2A,I1,I4A,UUNITS,
     M             PSFLAG)
C
C     get atmospheric deposition flags - table-type pest-ad-flags
      I2A= 87
      I4A= 18
      CALL ITABLE (I2A,I1,I4A,UUNITS,
     M             PEADFG)
C
C     read in month-data tables where necessary
      DO 50 L= 1, 3
        DO 40 I= 1,3
          N= 6*(L-1)+ 2*(I-1)+ 1
          IF (PEADFG(N) .GT. 0) THEN
C           monthly flux must be read
            CALL MDATBL
     I                  (PEADFG(N),
     O                   PEAFXM(1,L,I),RETCOD)
C           convert units to internal - not done by MDATBL
C           from lb/ac.day to lb/ac.ivl or from kg/ha.day to kg/ha.ivl
            DO 10 K= 1, 12
              PEAFXM(K,L,I)= PEAFXM(K,L,I)*DELT60/24.0
 10         CONTINUE
          END IF
          IF (PEADFG(N+1) .GT. 0) THEN
C           monthly ppn conc must be read
            CALL MDATBL
     I                  (PEADFG(N+1),
     O                   PEACNM(1,L,I),RETCOD)
C           convert units to internal - not done by MDATBL
            IF (UUNITS .EQ. 1) THEN
C             convert from mg/l to lb/ac.in
              DO 20 K= 1, 12
                PEACNM(K,L,I)= PEACNM(K,L,I)*0.226635
 20           CONTINUE
            ELSE IF (UUNITS .EQ. 2) THEN
C             convert from mg/l to kg/ha.in
              DO 30 K= 1, 12
                PEACNM(K,L,I)= PEACNM(K,L,I)*0.01
 30           CONTINUE
            END IF
          END IF
 40     CONTINUE
 50   CONTINUE
C
C     table-type soil-data
      CALL SOLDAT (UUNITS,
     O             SOILM,RVAL)
C
C     initialize table-type subscripts
      SUB = 0
      SUBI= 0
C
C     pesticide loop
      DO 90 L= 1,NPST
        IF (OUTLEV.GT.2) THEN
C         delimeter
          WRITE (MESSU,2010)
        END IF
C       table-type pest-id
        I2A= 88
        I4A= 5
        CALL RTABLE (I2A,L,I4A,UUNITS,
     M               GPSPM(1,L))
C
C       get pesticide parameters
        SUBBAS= (L-1)*4
C
C       casentry adopfg(l)
        ADOPF4= ADOPFG(L)
C
        IF (ADOPF4 .EQ. 1) THEN
C         first order kinetics
          I2A= 89
          I2B= 90
          I4A= 2
          CALL FIRSTP (OUTLEV,UUNITS,DELT60,I2A,L,I4A,I2B,SUBBAS,
     $                 I4A,I1,I1,LSNO,MESSU,MSGFL,
     M                 PSWCNT(6),
     O                 GPSPM(6,L),SPSPM(1,L),UPSPM(1,L),
     $                 LPSPM(1,L),APSPM(1,L))
C
        ELSE IF (ADOPF4 .EQ. 2) THEN
C         freundlich isotherm - single valued
          I2A= 91
          I2B= 92
          CALL SVALP (OUTLEV,MESSU,UUNITS,I2A,L,I2B,SUBBAS,
     O                GPSPM(8,L),SPSPM(3,L),UPSPM(3,L),
     $                LPSPM(3,L),APSPM(3,L))
C
        ELSE IF (ADOPF4 .EQ. 3) THEN
C         freundlich isotherm - non single valued
          I2A= 91
          I2B= 93
          CALL NONSVP (OUTLEV,MESSU,UUNITS,I2A,L,I2B,SUBBAS,
     O                 GPSPM(8,L),SPSPM(3,L),UPSPM(3,L),
     $                 LPSPM(3,L),APSPM(3,L))
        END IF
C
C       table-type pst-degrad - degradation parameters
        I2A= 94
        I4A= 4
        CALL RTABLE (I2A,L,I4A,UUNITS,
     M               RVAL)
        SPSPM(8,L)= RVAL(1)
        UPSPM(8,L)= RVAL(2)
        LPSPM(8,L)= RVAL(3)
        APSPM(8,L)= RVAL(4)
C
C       state variables
        SCVFG(L)= 1
        UCVFG(L)= 1
        LCVFG(L)= 1
        ACVFG(L)= 1
C
C       process input of initial storages
        I2A= 95
        I2B= 96
        I4A= 3
        I4B= 1
        CALL STORGE (MESSU,OUTLEV,UUNITS,I2A,I4A,HEADG,I2B,I4B,
     M               SUB,SUBI,
     O               SPS(1,L),UPS(1,L),IPS(L),LPS(1,L),APS(1,L),
     O               TPS(1,L),TOTPST(L))
C
        IF (ADOPFG(L).EQ.1) THEN
          IF (OUTLEV.GT.1) THEN
C           move chry message
            WRITE (MESSU,2020)
          END IF
C
          SPS(2,L)= SPS(2,L)+ SPS(1,L)
          UPS(2,L)= UPS(2,L)+ UPS(1,L)
          LPS(2,L)= LPS(2,L)+ LPS(1,L)
          APS(2,L)= APS(2,L)+ APS(1,L)
C
          SPS(1,L)= 0.0
          UPS(1,L)= 0.0
          LPS(1,L)= 0.0
          APS(1,L)= 0.0
        END IF
C
        IF (ADOPFG(L).EQ.3) THEN
C         set values of "last" adsorbed conc equal to initial value
          SXST(L)=SPS(2,L)
          UXST(L)=UPS(2,L)
          LXST(L)=LPS(2,L)
          AXST(L)=APS(2,L)
        END IF
C       end of pest loop
 90   CONTINUE
C
      IF (OUTLEV.GT.1) THEN
C       end processing message
        WRITE (MESSU,2030)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PEST
C
C     + + + PURPOSE + + +
C     Simulate pesticide behavior in detail
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PEST2 + + +
      INCLUDE     'cplpe.inc'
      INCLUDE     'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,J,L,M,TMPFG,N,REQFG,TSSUB(2),FLGVAL
      REAL        ADEG,APST(3),FSD,IPST,LDEG,LPST(3),SDEG,SDPST(2),
     $            SPST(3),SSPST(3),TSPST(5),UDEG,UPST(3),PEADFX,PEADCN
      CHARACTER*4 LAYID(4)
      CHARACTER*6 OPTYP,TSNAM,SECNAM,MSECNM,OPFGNM
C
C     + + + FUNCTIONS + + +
      REAL        DAYVAL
C
C     + + + EXTERNALS + + +
      EXTERNAL    AGRGET,PSTRXN,SDFRAC,SEDMOV,TOPMOV,SUBMOV,DAYVAL,
     $            HREQTS
C
C     + + + DATA INITIALIZATIONS + + +
      DATA        LAYID/'SURF','UPPR','LOWR','GRND'/
      DATA        TSSUB/1,1/
      DATA        OPTYP,SECNAM/'PERLND','PEST  '/
C
C     + + + END SPECIFICATIONS + + +
C
C     get time series needed for agrichemical sections
C
C     determine if temperature related timseries are needed
      TMPFG= 0
      DO 10 L= 1,NPST
        IF (ADOPFG(L).EQ.1) THEN
          TMPFG= 1
        END IF
  10  CONTINUE
C
      CALL AGRGET (MESSU,MSGFL,DATIM,LSNO,SEDFG,MSTLFG,PSTFG,TMPFG,
     I             IVL1,SOSDFP,MSTFP,FRACFP,SLTFP,ULTFP,LGTFP,SECNAM,
     O             SOSED,MST,FRAC,SLTMP,ULTMP,LGTMP)
CTHJ      PREC= PAD(PRECFP+IVL1)
      REQFG= 2
      TSNAM= 'PREC  '
      CALL HREQTS (PRECFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,LSNO,
     I             TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,FLGVAL,
     O             PREC)
C
C     get lateral inflow
      DO 15 J= 1, 3
        DO 12 I= 1, 5
          IF (LIPSSX(I,J) .GE. 1) THEN
C           lateral inflow of dissolved pesticide
            LIPSS(I,J)= PAD(LIPSSX(I,J)+IVL1)
          ELSE
C           no lateral inflow
            LIPSS(I,J)= 0.0
          END IF
 12     CONTINUE
        DO 13 I= 1, 2
          IF (LISDPX(I,J) .GE. 1) THEN
C           lateral inflow of sediment-associated pesticide
            LISDPS(I,J)= PAD(LISDPX(I,J)+IVL1)
          ELSE
C           no lateral inflow
            LISDPS(I,J)= 0.0
          END IF
 13     CONTINUE
 15   CONTINUE
C
C     compute atmospheric deposition influx
      DO 30 J= 1, 3
        DO 20 I= 1, 3
          N= 6*(J-1)+ 2*(I-1)+ 1
C         dry deposition
          IF (PEADFG(N) .LE. -1) THEN
CTHJ            PEADDR(J,I)= PAD(PEAFFP(J,I)+IVL1)
            REQFG= 4
            OPFGNM= 'PEADFG'
            TSNAM= 'PEADFX'
            TSSUB(1)= J
            TSSUB(2)= I
            CALL HREQTS (PEAFFP(J,I),IVL1,REQFG,MESSU,MSGFL,DATIM,
     I                   OPTYP,LSNO,TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,
     I                   PEADFG(N),
     O                   PEADFX)
            PEADDR(J,I)= PEADFX
          ELSE IF (PEADFG(N) .GE. 1) THEN
            PEADDR(J,I)= DAYVAL(PEAFXM(MON,J,I),PEAFXM(NXTMON,J,I),DAY,
     I                          NDAYS)
          ELSE
            PEADDR(J,I)= 0.0
          END IF
C         wet deposition
          IF (PEADFG(N+1) .LE. -1) THEN
CTHJ            PEADWT(J,I)= PREC*PAD(PEACFP(J,I)+IVL1)
            REQFG= 4
            OPFGNM= 'PEADFG'
            TSNAM= 'PEADCN'
            TSSUB(1)= J
            TSSUB(2)= I
            CALL HREQTS (PEACFP(J,I),IVL1,REQFG,MESSU,MSGFL,DATIM,
     I                   OPTYP,LSNO,TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,
     I                   PEADFG(N+1),
     O                   PEADCN)
            PEADWT(J,I)= PREC*PEADCN
          ELSE IF (PEADFG(N+1) .GE. 1) THEN
            PEADWT(J,I)= PREC*DAYVAL(PEACNM(MON,J,I),
     I                               PEACNM(NXTMON,J,I),DAY,NDAYS)
          ELSE
            PEADWT(J,I)= 0.0
          END IF
          PEADEP(J,I)= PEADDR(J,I)+ PEADWT(J,I)
 20     CONTINUE
 30   CONTINUE
      TSSUB(1)= 1
      TSSUB(2)= 1
C
      DO 230 L= 1,NPST
C
C       add atmospheric deposition and lateral inflow
        SPS(1,L)= SPS(1,L)+ LISDPS(1,L)+ PEADEP(L,1)
        SPS(2,L)= SPS(2,L)+ LISDPS(2,L)+ PEADEP(L,2)
        SPS(3,L)= SPS(3,L)+ LIPSS(1,L)+  PEADEP(L,3)
        UPS(3,L)= UPS(3,L)+ LIPSS(2,L)
        IPS(L)=   IPS(L)+   LIPSS(3,L)
C
C       assign segment-wide topsoil storages to local variables
        DO 40 M= 1,3
          SPST(M)= SPS(M,L)
          UPST(M)= UPS(M,L)
 40     CONTINUE
C
        IPST= IPS(L)
C
C       perform reactions on pesticides in the surface layer storage
        CALL PSTRXN (LSNO,DAYFG,MESSU,MSGFL,DATIM,
     I               SLTMP,MST(1),SLSM,
     I               LAYID(1),ADOPFG(L),ITMXPS(L),
     I               GPSPM(1,L),SPSPM(1,L),
     M               PSWCNT,PSECNT,SCVFG(L),SXJCT(L),SXST(L),SPST,
     O               SDEG)
C
C       perform reactions on pesticides in the upper layer
C       principal storage
        CALL PSTRXN (LSNO,DAYFG,MESSU,MSGFL,DATIM,
     I               ULTMP,MST(2),ULSM,
     I               LAYID(2),ADOPFG(L),ITMXPS(L),
     I               GPSPM(1,L),UPSPM(1,L),
     M               PSWCNT,PSECNT,UCVFG(L),UXJCT(L),UXST(L),UPST,
     O               UDEG)
C
        IF (SOSED.GT.0.0) THEN
C         there is sediment/soil being eroded from the land surface
C
C         determine the fraction of pesticide in the surface layer
C         storage being removed on/with sediment
          CALL SDFRAC  (SOSED,SLME,LSNO,DATIM,MESSU,MSGFL,
     M                  PSWCNT(1),PSWCNT(2),
     O                  FSD)
C
C         transport crystalline pesticide with/on sediment
          CALL SEDMOV (FSD,
     M                 SPST(1),
     O                 SDPST(1))
C
C         transport adsorbed pesticide with/on sediment
          CALL SEDMOV (FSD,
     M                 SPST(2),
     O                 SDPST(2))
        ELSE
C         there is no sediment/soil being eroded from the land
C         surface so zero fluxes
          SDPST(1)= 0.0
          SDPST(2)= 0.0
C
        END IF
C
C       move solution pesticide with water in the topsoil layers
        CALL TOPMOV (FRAC,
     M               SPST(3),UPST(3),IPST,
     O               TSPST)
C
C       subsoil processes
C
C       update storages for lateral inflows
        LPS(3,L)= LPS(3,L)+ LIPSS(4,L)
        APS(3,L)= APS(3,L)+ LIPSS(5,L)
C
C       assign subsoil storages to local variables
        DO 180 M= 1,3
          LPST(M)= LPS(M,L)
          APST(M)= APS(M,L)
 180    CONTINUE
C
C
C       perform reactions on pesticides in the lower layer storage
        CALL PSTRXN (LSNO,DAYFG,MESSU,MSGFL,DATIM,
     I               LGTMP,MST(4),LLSM,LAYID(3),
     I               ADOPFG(L),ITMXPS(L),GPSPM(1,L),
     I               LPSPM(1,L),
     M               PSWCNT,PSECNT,LCVFG(L),LXJCT(L),LXST(L),LPST,
     O               LDEG)
C
C       perform reactions on pesticides in the active
C       groundwater storage
        CALL PSTRXN (LSNO,DAYFG,MESSU,MSGFL,DATIM,
     I               LGTMP,MST(5),ALSM,LAYID(4),
     I               ADOPFG(L),ITMXPS(L),GPSPM(1,L),
     I               APSPM(1,L),
     M               PSWCNT,PSECNT,ACVFG(L),AXJCT(L),AXST(L),APST,
     O               ADEG)
C
C       transport solution pesticide in the subsurface layers
        CALL SUBMOV (TSPST(3),FRAC(6),FRAC(7),FRAC(8),
     M               LPST(3),APST(3),
     O               SSPST)
C
C       assign segment-wide storages and fluxes to permanent storage
C       locations
C       storages
        DO 190 M= 1,3
          SPS(M,L)= SPST(M)
          UPS(M,L)= UPST(M)
          LPS(M,L)= LPST(M)
          APS(M,L)= APST(M)
C         total storage of various forms
          TPS(M,L)= SPST(M)+ UPST(M)+ LPST(M)+ APST(M)
 190    CONTINUE
C
        IPS(L)= IPST
C
C       add solution pesticide in interflow storages to total
C       storage
        TPS(3,L)= TPS(3,L)+ IPST
C
C       fluxes
        DO 200 J= 1,2
          SDPS(J,L)= SDPST(J)
 200    CONTINUE
C
        DO 210 J= 1,5
          TSPSS(J,L)= TSPST(J)
 210    CONTINUE
C
        DO 220 J= 1,3
          SSPSS(J,L)= SSPST(J)
 220    CONTINUE
C
        DEGPS(1,L)= SDEG
        DEGPS(2,L)= UDEG
        DEGPS(3,L)= LDEG
        DEGPS(4,L)= ADEG
C
C       total pesticide degraded
        DEGPS(5,L)= SDEG+ UDEG+ LDEG+ ADEG
C
C       find total pesticide outflow of this pesticide due
C       to water and land surface erosion
        SOSDPS(L)= SDPST(1)+ SDPST(2)
        SOPEST(L)= SOSDPS(L)+ TSPSS(1,L)
        POPST(L) = TSPST(1)+ TSPST(5)+ SSPST(3)
        TOPST(L) = SOSDPS(L)+ POPST(L)
C
C       total pesticide
        TPST(1,L)= SPS(1,L)+ SPS(2,L)+ SPS(3,L)
        TPST(2,L)= UPS(1,L)+ UPS(2,L)+ UPS(3,L)
        TPST(3,L)= IPS(L)
        TPST(4,L)= LPS(1,L)+ LPS(2,L)+ LPS(3,L)
        TPST(5,L)= APS(1,L)+ APS(2,L)+ APS(3,L)
        TOTPST(L)= TPS(1,L)+ TPS(2,L)+ TPS(3,L)
C
 230  CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   NONSVP
     I                    (OUTLEV,MESSU,UUNITS,GNUM,GSUB,NUM,SUBBAS,
     O                     GPARM,SPARM,UPARM,LPARM,APARM)
C
C     + + + PURPOSE + + +
C     Process parameters required for non-single-valued freundlich
C     adsorption/desorption calculations
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    GNUM,GSUB,MESSU,NUM,OUTLEV,SUBBAS,UUNITS
      REAL       APARM(5),GPARM(1),LPARM(5),SPARM(5),UPARM(5)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     OUTLEV - run interpreter output level
C     MESSU  - ftn unit no. to be used for printout of messages
C     UUNITS - system of units   1-english, 2-metric
C     GNUM   - ???
C     GSUB   - ???
C     NUM    - ???
C     SUBBAS - ???
C     GPARM  - ???
C     SPARM  - ???
C     UPARM  - ???
C     LPARM  - ???
C     APARM  - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I1,SUB
      REAL       CMAX
C
C     + + + EXTERNALS + + +
      EXTERNAL   RTABLE,NSVTAB
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' PARAMETERS TO BE USED FOR NON-SINGLE-VALUED ',
     $          'FREUNDLICH ADSORPTION/DESORPTION CALCULATIONS')
 2010 FORMAT (/,' PARAMETERS FOR SURFACE LAYER')
 2020 FORMAT (/,' PARAMETERS FOR THE UPPER LAYER')
 2030 FORMAT (/,' PARAMETERS FOR THE LOWER LAYER')
 2040 FORMAT (/,' PARAMETERS FOR THE ACTIVE GROUNDWATER LAYER')
C
C     + + + END SPECIFICATIONS + + +
C
      I1= 1
C
      IF (OUTLEV.GT.2) THEN
C       processing parameters message
        WRITE (MESSU,2000)
      END IF
C
C     get cmax - maximum solubility
      CALL RTABLE (GNUM,GSUB,I1,UUNITS,
     M             GPARM)
      CMAX= GPARM(1)
C
      IF (OUTLEV.GT.2) THEN
C       processing surface layer message
        WRITE (MESSU,2010)
      END IF
      SUB= SUBBAS + 1
      CALL NSVTAB (NUM,SUB,UUNITS,CMAX,
     O             SPARM)
C
      IF (OUTLEV.GT.2) THEN
C       processing upper layer message
        WRITE (MESSU,2020)
      END IF
      SUB= SUBBAS + 2
      CALL NSVTAB (NUM,SUB,UUNITS,CMAX,
     O             UPARM)
C
      IF (OUTLEV.GT.2) THEN
C       processing lower layer message
        WRITE (MESSU,2030)
      END IF
      SUB= SUBBAS + 3
      CALL NSVTAB (NUM,SUB,UUNITS,CMAX,
     O             LPARM)
C
      IF (OUTLEV.GT.2) THEN
C       processing groundwater layer message
        WRITE (MESSU,2040)
      END IF
      SUB= SUBBAS + 4
      CALL NSVTAB (NUM,SUB,UUNITS,CMAX,
     O             APARM)
C
      RETURN
      END
C
C
C
      SUBROUTINE   NSVTAB
     I                    (TNUM,TSUB,UUNITS,CMAX,
     O                     PARM)
C
C     + + + PURPOSE + + +
C     Process a table containing non-single-valued freundlich reaction
C     parameters
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    TNUM,TSUB,UUNITS
      REAL       CMAX,PARM(5)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     TNUM   - ???
C     TSUB   - ???
C     UUNITS - system of units   1-english, 2-metric
C     CMAX   - ???
C     PARM   - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I4
      REAL      RVAL(4)
C
C     + + + EXTERNALS + + +
      EXTERNAL  RTABLE
C
C     + + + END SPECIFICATIONS + + +
C
      I4= 4
      CALL RTABLE (TNUM,TSUB,I4,UUNITS,
     M               RVAL)
C
C     fixed capacity - xfix
      PARM(1)= RVAL(1)
C
C     freundlich k
      PARM(3)= RVAL(2)
C
C     freundlich n1 - store its inverse
      PARM(4)= 1.0/RVAL(3)
C
C     freundlich n2 - store its inverse
      PARM(5)= 1.0/RVAL(4)
C
C     calculate xmax - max adsorption capacity
      PARM(2)= PARM(3)*(CMAX**PARM(4))+ PARM(1)
C
      RETURN
      END
C
C     4.2(1).9.5.4
C
      SUBROUTINE   DEGRAS
     I                    (DEGCON,
     M                     CMAD,CMCY,CMSU,
     O                     DEGCM)
C
C     + + + PURPOSE + + +
C     Degrade chemical by a simple degradation constant
C     chemical storage units are in mass/area
C
C     + + + DUMMY ARGUMENTS + + +
      REAL       CMAD,CMCY,CMSU,DEGCM,DEGCON
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DEGCON - ???
C     CMAD   - ???
C     CMCY   - ???
C     CMSU   - ???
C     DEGCM  - ???
C
C     + + + LOCAL VARIABLES + + +
      REAL       DEGAD,DEGCY,DEGSU
C
C     + + + END SPECIFICATIONS + + +
C
C     adsorbed
      IF (CMAD.LT.0.000001) THEN
C       not enough to simulate - degrade all
        DEGAD= CMAD
        CMAD = 0.0
      ELSE
C       first order degradation
        DEGAD= CMAD*DEGCON
        CMAD = CMAD- DEGAD
      END IF
C
C     crystalline
      IF (CMCY.LT.0.000001) THEN
C       not enough to simulate - degrade all
        DEGCY= CMCY
        CMCY = 0.0
      ELSE
C       first order degradation
        DEGCY= CMCY*DEGCON
        CMCY = CMCY- DEGCY
      END IF
C
C     solution
      IF (CMSU.LT.0.000001) THEN
C       not enough to simulate - degrade all
        DEGSU= CMSU
        CMSU = 0.0
      ELSE
C       first order degradation
        DEGSU= CMSU*DEGCON
        CMSU = CMSU- DEGSU
      END IF
C
C     total chemical degradation for this layer - units are
C     mass/area-day
      DEGCM= DEGAD+ DEGCY+ DEGSU
C
      RETURN
      END
C
C     4.2(1).9.5.3
C
      SUBROUTINE   NONSV
     I                   (MOISTM,SOILM,TCM,XFIX,CMAX,XMAX,KF1,N1I,LSNO,
     I                    MESSU,MSGFL,DATIM,N2I,ITMAX,CMID,XST,LAYID,
     M                    CMSU,CRV1FG,ECNT,XJCT,
     O                    CMCY,CMAD)
C
C     + + + PURPOSE + + +
C     Calculate the adsorption/desorption of chemicals by the
C     non-single value freundlich method
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     CRV1FG,ECNT,MSGFL,ITMAX,LSNO,MESSU,DATIM(5)
      REAL        CMAD,CMAX,CMCY,CMSU,KF1,MOISTM,N1I,N2I,SOILM,TCM,XFIX,
     $            XJCT,XMAX,XST
      CHARACTER*4 LAYID,CMID(5)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MOISTM - ???
C     SOILM  - ???
C     TCM    - ???
C     XFIX   - ???
C     CMAX   - ???
C     XMAX   - ???
C     KF1    - ???
C     N1I    - ???
C     LSNO   - line number in the opn sequence block of uci
C     MESSU  - ftn unit no. to be used for printout of messages
C     MSGFL  - fortran unit number of HSPF message file
C     N2I    - ???
C     ITMAX  - ???
C     CMID   - ???
C     XST    - ???
C     LAYID  - ???
C     CMSU   - ???
C     CRV1FG - ???
C     ECNT   - ???
C     XJCT   - ???
C     CMCY   - ???
C     CMAD   - ???
C     DATIM  - date and time of day
C
C     + + + LOCAL VARIABLES + + +
      REAL       C,FIXCAP,KF2,MAXAD,MAXSU,REMCM,X,XDIF
C
C     + + + EXTERNALS + + +
      EXTERNAL   ITER
C
C     + + + END SPECIFICATIONS + + +
C
      IF (MOISTM.GE.0.001) THEN
C       there is sufficient moisture for adsorption/desorption to
C       occur
C
C       determine the capacity of the soil to fix the
C       chemical in mass/area
        FIXCAP= XFIX*SOILM*1.0E-06
C
        IF (TCM.GT.FIXCAP) THEN
C         there is more chemical than the fixed capacity, so
C         determine where the surplus resides
C
C         calculate the maximum soluble and adsorbed chemical -
C         units are mass/area
          MAXSU= CMAX*MOISTM*1.0E-06
          MAXAD= XMAX*SOILM*1.0E-06
C
C         determine if maximum adsorption capacity and solubility
C         have been reached
          REMCM= TCM- MAXAD- MAXSU
C
          IF (REMCM.GE.0.0) THEN
C           maximum adsorption capacity and solubilty have been
C           reached, so solution and adsorbed forms are at capacity;
C           the remaining chemical is considered to be in the
C           crystalline form
            CMAD  = MAXAD
            CMSU  = MAXSU
            CMCY  = REMCM
            CRV1FG= 1
          ELSE
C           total amount is less than amount needed to reach
C           capacity.  therefore, no crystalline form exists and
C           adsorption/desorption amounts must be determined from
C           the freundlich isotherm
            CMCY= 0.0
C
C           make initial estimate of the freundlich value for
C           chemical concentration in solution(c) - units are ppm in
C           solution
            IF (CMSU.GT.0.0) THEN
C             use current concentration at the beginning of the
C             interval
              C= CMSU/MOISTM*1.0E06
            ELSE
C             use maximum
              C= CMAX
            END IF
C
            IF (CRV1FG.EQ.1) THEN
C             curve #1 (solely adsorption) curve was used last
C             use an iteration process to determine the freundlich
C             equilibrium solution concentration (c) and adsorbed
C             concentration (x) values on this curve
              CALL ITER (TCM,MOISTM,SOILM,KF1,N1I,XFIX,ITMAX,CMID,
     I                   LAYID,LSNO,MESSU,MSGFL,DATIM,FIXCAP,
     M                   C,ECNT,
     O                   X)
C
              IF (X.LT.XST) THEN
C               new estimate of the adsorbed concentration is less
C               than the freundlich concentration calculated for the
C               end of last interval so desorption should have taken
C               place using curve #2
C
C               locate junction of this curve #2 and curve #1
                XJCT  = XST
                CRV1FG= 0
                XDIF  = XJCT- XFIX
C
C               recompute new freundlich k value
                KF2= ((KF1/XDIF)**(N2I/N1I))*XDIF
C
C               use an iteration process to determine the equilibrium
C               solution concentration (c) and adsorbed concentration
C               (x) values on this curve
                CALL ITER (TCM,MOISTM,SOILM,KF2,N2I,XFIX,ITMAX,CMID,
     I                     LAYID,LSNO,MESSU,MSGFL,DATIM,FIXCAP,
     M                     C,ECNT,
     O                     X)
              ELSE
C               values are ok
C
              END IF
            ELSE
C             curve #2 (branch off desorption/adsorption curve) was
C             used last
C
              XDIF= XJCT- XFIX
C
C             calculate freundlich k for the equation for this curve
              KF2= ((KF1/XDIF)**(N2I/N1I))*XDIF
C
C             use an iteration process to determine the equilibrium
C             solution concentration (c) and adsorbed concentration
C             (x) values on this curve
              CALL ITER (TCM,MOISTM,SOILM,KF2,N2I,XFIX,ITMAX,CMID,
     I                   LAYID,LSNO,MESSU,MSGFL,DATIM,FIXCAP,
     M                   C,ECNT,
     O                   X)
C
              IF (X.GT.XJCT) THEN
C               new estimate of the adsorbed concentration is more
C               than the concentration where curve #2 joins curve #1
C               solution chemical should have been adsorbed using
C               curve #1
C
C               use an iteration process to determine the equilibrium
C               solution concentration (c) and adsorbed concentration
C               (x) values on curve #1
                CALL ITER (TCM,MOISTM,SOILM,KF1,N1I,XFIX,ITMAX,CMID,
     I                     LAYID,LSNO,MESSU,MSGFL,DATIM,FIXCAP,
     M                     C,ECNT,
     O                     X)
C
                CRV1FG= 1
                XJCT  = -1.0E30
              ELSE
C               values are ok
              END IF
            END IF
C
C           convert the freundlich isotherm concentration
C           to mass/area units
            CMAD= X*SOILM*1.0E-06
            IF (CMAD.GT.TCM) CMAD= TCM
            CMSU= TCM-CMAD
          END IF
        ELSE
C         there is insufficient chemical to fullfill the fixed
C         capacity
C         the fixed portion is part of the adsorbed phase
          CMAD  = TCM
          CMCY  = 0.0
          CMSU  = 0.0
          CRV1FG= 1
        END IF
      ELSE
C       there is insufficient moisture for adsorption/desorption to
C       occur
        MAXAD= XMAX*SOILM*1.0E-06
        IF (TCM.GT.MAXAD) THEN
          CMAD= MAXAD
          CMCY= TCM-MAXAD
        ELSE
          CMAD= TCM
          CMCY= 0.0
        END IF
        CMSU  = 0.0
        CRV1FG= 1
      END IF
C
      RETURN
      END
C
C     4.2(1).15.2.9
C
      SUBROUTINE   PESPRT
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
C     BINU   - fortran unit number on which to write binary output
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PEST2 + + +
      INCLUDE   'cplpe.inc'
      INCLUDE   'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I0,I1,I2,I3,I5,J,L,I,N,ADFG,LATFG,ACNT,CLEN(59),
     $          EXDAT(5)
      REAL      MATIN,MATDIF,PCFLX1(2),PCFLX2(5),PCFLX3(3),PCFLX4(5),
     $          PPOPST,PSOPST,PSOSD,PSSTO,PSSTOS,PSTAT(3),TOTAL,
     $          PCFLX5(3),PCFLX6(3),PADTOT(3),PADALL,PLIFX1(5),
     $          PLIFX2(2),PLIPSS,PLIPSD,PLIPST,APRINT(59)
      CHARACTER*8   UNITID,CCFLX1(2),CCFLX2(5),CCFLX3(3),CCFLX4(5),
     $              CLIFX1(5),CLIFX2(2),CATMDEP(3)
      CHARACTER*11  PHASE(3)
      CHARACTER*20  PESTNAME
      CHARACTER*256 CHEAD(59)
C
C     + + + EXTERNALS + + +
      EXTERNAL   TRNVEC,BALCHK,EXDATE
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' *** PEST ***')
 2010 FORMAT (/,' --->',5A4)
 2015 FORMAT (5A4)
 2030 FORMAT (' ','STATE VARIABLES   ',A8)
 2040 FORMAT (/,'     STORAGES BY LAYER',18X,'CRYSTALLINE',
     $        12X,'ADSORBED',12X,'SOLUTION',14X,'TOTALS')
 2050 FORMAT (' ',6X,'SURFACE LAYER',11X,4(10X,F10.3))
 2060 FORMAT (' ',6X,'UPPER PRINCIPAL',9X,4(10X,F10.3))
 2070 FORMAT (' ',6X,'UPPER TRANSITORY(INTER) ',40X,2(10X,F10.3))
 2080 FORMAT (' ',6X,'LOWER LAYER',13X,4(10X,F10.3))
 2090 FORMAT (' ',6X,'ACTIVE GROUNDWATER',6X,4(10X,F10.3))
 2100 FORMAT (/,7X,'TOTALS',18X,4(10X,F10.3))
 2110 FORMAT (/,3X,'FLUXES',12X,A8)
 2120 FORMAT (  '     ATMOSPHERIC DEPOSITION    <--------CRYSTALLINE',
     #          '---------><----------ADSORBED----------><----------',
     #          'SOLUTION---------->')
 2130 FORMAT (  31X,'       DRY       WET     TOTAL       DRY',
     #          '       WET     TOTAL       DRY       WET     TOTAL')
 2140 FORMAT (  31X,9(1PE10.3))
 2141 FORMAT (  '     LATERAL INFLOWS           <------------',
     $          '-------------SOLUTION------------------------->',
     $          '<---------ON SEDIMENT-------->     TOTAL')
 2142 FORMAT (  31X,'   SURFACE     UPPER INTERFLOW     LOWER',
     $          ' ACTIVE GW     TOTAL     CRYST  ADSORBED     TOTAL')
 2143 FORMAT (  31X,'    SLIPSS    ULIPSS    ILIPSS    LLIPSS',
     $          '    ALIPSS    TLIPSS    SDLPSY    SDLPSA    SDLPST',
     $          '    TLIPST')
 2144 FORMAT (  31X,1P10E10.3,/)
 2150 FORMAT (  5X,'OUTFLOWS IN SOLUTION',6X,
     $        '<--SURFACE LAYER---><-UPPER LAYER PRIN-> INTERFLOW',10X,
     $        '<---LOWER LAYER---->',9X,'GROUNDWATER')
 2160 FORMAT (' ',30X,'   OUTFLOW      PERC      PERC  TO TRANS',
     $        '   OUTFLOW',16X,'PERC DEEP PERC',13X,'OUTFLOW')
 2170 FORMAT (' ',35X,'SOPSS     SPPSS     UPPSS     IIPSS',
     $        '     IOPSS',15X,'LPPSS    LDPPSS',15X,'AOPSS')
 2180 FORMAT (' ',30X,1P5E10.3,10X,1P2E10.3,10X,1PE10.3)
 2190 FORMAT (/,'     OTHER OUTFLOWS',12X,
     $        '-SEDIMENT ASSOCIATED OUTFLOW--',10X,
     $        '<--TOTAL OUTFLOW--->')
 2200 FORMAT (' ',32X,'CRYSTALL  ADSORBED     TOTAL',11X,
     $  'FROM SURF  FROM PLS')
 2210 FORMAT(' ',35X,'SDPSY     SDPSA    SOSDPS',14X,'SOPEST    POPEST')
 2220 FORMAT (' ',30X,1P3E10.3,10X,1P2E10.3)
 2230 FORMAT (' ',4X,'DEGRADATION BY LAYER',19X,
     $        'SURFACE',10X,'UPPER PRIN',15X,'LOWER',11X,'ACTIVE GW',
     $        15X,'TOTAL')
 2240 FORMAT (' ',44X,'SDEGPS',14X,'UDEGPS',14X,'LDEGPS',14X,
     $        'ADEGPS',14X,'TDEGPS')
 2250 FORMAT (' ',30X,5(10X,1PE10.3))
C
C     + + + END SPECIFICATIONS + + +
C
      I0=0
      I1=1
      I2=2
      I3=3
      I5=5
C
C     initialize array counter for binary printout, store variable
C     names in local strings for use in building binary headers
      ACNT = 0
      PHASE(1) = 'CRYSTALLINE'
      PHASE(2) = 'ADSORBED'
      PHASE(3) = 'SOLUTION'
      CATMDEP(1) = 'DRY'
      CATMDEP(2) = 'WET'
      CATMDEP(3) = 'TOTAL'
      CLIFX1(1) = 'SLIPSS'
      CLIFX1(2) = 'ULIPSS'
      CLIFX1(3) = 'ILIPSS'
      CLIFX1(4) = 'LLIPSS'
      CLIFX1(5) = 'ALIPSS'
      CLIFX2(1) = 'SDLPSY'
      CLIFX2(2) = 'SDLPSA'
      CCFLX2(1) = 'SOPSS'
      CCFLX2(2) = 'SPPSS'
      CCFLX2(3) = 'UPPSS'
      CCFLX2(4) = 'IIPSS'
      CCFLX2(5) = 'IOPSS'
      CCFLX3(1) = 'LPPSS'
      CCFLX3(2) = 'LDPPSS'
      CCFLX3(3) = 'AOPSS'
      CCFLX1(1) = 'SDPSY'
      CCFLX1(2) = 'SDPSA'
      CCFLX4(1) = 'SDEGPS'
      CCFLX4(2) = 'UDEGPS'
      CCFLX4(3) = 'LDEGPS'
      CCFLX4(4) = 'ADEGPS'
      CCFLX4(5) = 'TDEGPS'
C
      IF (UNITFG .EQ. 1) THEN
C       english
        UNITID= '   LB/AC'
      ELSE
C       metric
        UNITID= '   KG/HA'
      END IF
C
      IF (PRINTU .GT. 0 .AND. PFLAG(9) .LE. LEV) THEN
        WRITE (PRINTU,2000)
      END IF
C
      DO 300 L= 1,NPST
        ACNT = 0
        WRITE (PESTNAME,2015) (GPSPM(J,L),J=1,5)
        PESTNAME = ADJUSTL(PESTNAME)
        PESTNAME = TRIM(PESTNAME) // ' - '
        IF (PRINTU .GT. 0 .AND. PFLAG(9) .LE. LEV) THEN
C         print headings on unit printu
          WRITE (PRINTU,2010)  (GPSPM(J,L),J=1,5)
          WRITE (PRINTU,2030)  AGMAID
          WRITE (PRINTU,2040)
        END IF
C
        CALL TRNVEC (I3,SPS(1,L),MFACTA,MFACTB,
     O               PSTAT)
        TOTAL= PSTAT(1)+ PSTAT(2)+ PSTAT(3)
        IF (PRINTU .GT. 0 .AND. PFLAG(9) .LE. LEV) THEN
          WRITE (PRINTU,2050)  PSTAT, TOTAL
        END IF
        IF (BINU .GT. 0 .AND. ABS(BFLAG(9)) .LE. LEV) THEN
C         compile values for binary printout
          DO 10 I = 1, 3
            ACNT = ACNT + 1
            APRINT(ACNT) = PSTAT(I)
            CHEAD(ACNT) = TRIM(PESTNAME) // PHASE(I)
            CHEAD(ACNT) = TRIM(CHEAD(ACNT)) // ' - SURFACE LAYER'
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 10       CONTINUE
          ACNT = ACNT + 1
          APRINT(ACNT) = TOTAL
          CHEAD(ACNT) = TRIM(PESTNAME) // 'TOTAL - SURFACE LAYER'
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
        END IF
C
        CALL TRNVEC (I3,UPS(1,L),MFACTA,MFACTB,
     O               PSTAT)
        TOTAL= PSTAT(1)+ PSTAT(2)+ PSTAT(3)
        IF (PRINTU .GT. 0 .AND. PFLAG(9) .LE. LEV) THEN
          WRITE (PRINTU,2060)  PSTAT, TOTAL
        END IF
        IF (BINU .GT. 0 .AND. ABS(BFLAG(9)) .LE. LEV) THEN
          DO 20 I = 1, 3
            ACNT = ACNT + 1
            APRINT(ACNT) = PSTAT(I)
            CHEAD(ACNT) = TRIM(PESTNAME) // PHASE(I)
            CHEAD(ACNT) = TRIM(CHEAD(ACNT)) // ' - UPPER PRINCIPAL'
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 20       CONTINUE
          ACNT = ACNT + 1
          APRINT(ACNT) = TOTAL
          CHEAD(ACNT) = TRIM(PESTNAME) // 'TOTAL - UPPER PRINCIPAL'
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
        END IF
C
        PSTAT(1)= IPS(L)*MFACTA+ MFACTB
        IF (PRINTU .GT. 0 .AND. PFLAG(9) .LE. LEV) THEN
          WRITE (PRINTU,2070)  PSTAT(1), PSTAT(1)
        END IF
        IF (BINU .GT. 0 .AND. ABS(BFLAG(9)) .LE. LEV) THEN
          ACNT = ACNT + 1
          APRINT(ACNT) = PSTAT(1)
          CHEAD(ACNT) = TRIM(PESTNAME) // PHASE(1)
          CHEAD(ACNT) = TRIM(CHEAD(ACNT)) // ' - UPPER TRANSITORY'
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          ACNT = ACNT + 1
          APRINT(ACNT) = PSTAT(1)
          CHEAD(ACNT) = TRIM(PESTNAME) // 'TOTAL - UPPER TRANSITORY'
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
        END IF
C
        CALL TRNVEC (I3,LPS(1,L),MFACTA,MFACTB,
     O               PSTAT)
        TOTAL= PSTAT(1)+ PSTAT(2)+ PSTAT(3)
        IF (PRINTU .GT. 0 .AND. PFLAG(9) .LE. LEV) THEN
          WRITE (PRINTU,2080)  PSTAT, TOTAL
        END IF
        IF (BINU .GT. 0 .AND. ABS(BFLAG(9)) .LE. LEV) THEN
          DO 30 I = 1, 3
            ACNT = ACNT + 1
            APRINT(ACNT) = PSTAT(I)
            CHEAD(ACNT) = TRIM(PESTNAME) // PHASE(I)
            CHEAD(ACNT) = TRIM(CHEAD(ACNT)) // ' - LOWER LAYER'
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 30       CONTINUE
          ACNT = ACNT + 1
          APRINT(ACNT) = TOTAL
          CHEAD(ACNT) = TRIM(PESTNAME) // 'TOTAL - LOWER LAYER'
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
        END IF
C
        CALL TRNVEC (I3,APS(1,L),MFACTA,MFACTB,
     O               PSTAT)
        TOTAL= PSTAT(1)+ PSTAT(2)+ PSTAT(3)
        IF (PRINTU .GT. 0 .AND. PFLAG(9) .LE. LEV) THEN
          WRITE (PRINTU,2090)  PSTAT, TOTAL
        END IF
        IF (BINU .GT. 0 .AND. ABS(BFLAG(9)) .LE. LEV) THEN
          DO 40 I = 1, 3
            ACNT = ACNT + 1
            APRINT(ACNT) = PSTAT(I)
            CHEAD(ACNT) = TRIM(PESTNAME) // PHASE(I)
            CHEAD(ACNT) = TRIM(CHEAD(ACNT)) // ' - ACTIVE GW'
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 40       CONTINUE
          ACNT = ACNT + 1
          APRINT(ACNT) = TOTAL
          CHEAD(ACNT) = TRIM(PESTNAME) // 'TOTAL - ACTIVE GW'
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
        END IF
C
        CALL TRNVEC (I3,TPS(1,L),MFACTA,MFACTB,
     O               PSTAT)
        TOTAL= PSTAT(1)+ PSTAT(2)+ PSTAT(3)
        IF (PRINTU .GT. 0 .AND. PFLAG(9) .LE. LEV) THEN
          WRITE (PRINTU,2100)  PSTAT, TOTAL
        END IF
        IF (BINU .GT. 0 .AND. ABS(BFLAG(9)) .LE. LEV) THEN
          DO 50 I = 1, 3
            ACNT = ACNT + 1
            APRINT(ACNT) = PSTAT(I)
            CHEAD(ACNT) = TRIM(PESTNAME) // PHASE(I)
            CHEAD(ACNT) = TRIM(CHEAD(ACNT)) // ' - TOTALS'
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 50       CONTINUE
          ACNT = ACNT + 1
          APRINT(ACNT) = TOTAL
          CHEAD(ACNT) = TRIM(PESTNAME) // 'TOTAL - TOTALS'
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
        END IF
C
        IF (PRINTU .GT. 0 .AND. PFLAG(9) .LE. LEV) THEN
          WRITE (PRINTU,2110)  AGMAID
        END IF
C
        LATFG= 0
        ADFG= 0
        DO 60 I= 1, 3
          DO 58 N= 1, 5
            IF (LIPSSX(N,I) .GE. 1) THEN
              LATFG= 1
            END IF
 58       CONTINUE
          DO 59 N= 1, 2
            IF (LISDPX(N,I) .GE. 1) THEN
              LATFG= 1
            END IF
 59       CONTINUE
C
          N= 6*(L-1)+ 2*(I-1)+ 1
          IF ( (PEADFG(J) .NE. 0) .OR. (PEADFG(J+1) .NE. 0) ) THEN
            ADFG= 1
          END IF
 60     CONTINUE
C
        PADALL= 0.0
        IF (ADFG .EQ. 1) THEN
          DO 70 I= 1, 3
            N= 6*(L-1)+ 2*(I-1)+ 1
            IF (PEADFG(N) .NE. 0) THEN
              PCFLX5(I)= PSCFX5(L,I,LEV)*MFACTA
            ELSE
              PCFLX5(I)= 0.0
            END IF
            IF (PEADFG(N+1) .NE. 0) THEN
              PCFLX6(I)= PSCFX6(L,I,LEV)*MFACTA
            ELSE
              PCFLX6(I)= 0.0
            END IF
            PADTOT(I)= PCFLX5(I)+ PCFLX6(I)
            PADALL= PADALL+ PADTOT(I)
 70       CONTINUE
C
          IF (PRINTU .GT. 0 .AND. PFLAG(9) .LE. LEV) THEN
            WRITE (PRINTU,2120)
            WRITE (PRINTU,2130)
            WRITE (PRINTU,2140) PCFLX5(1),PCFLX6(1),PADTOT(1),
     $                          PCFLX5(2),PCFLX6(2),PADTOT(2),
     $                          PCFLX5(3),PCFLX6(3),PADTOT(3)
          END IF
          IF (BINU .GT. 0 .AND. ABS(BFLAG(9)) .LE. LEV) THEN
            DO 90 I= 1, 3
              DO 80 J= 1, 3
                ACNT = ACNT + 1
                IF (J .EQ. 1) THEN
                  APRINT(ACNT) = PCFLX5(I)
                ELSEIF (J .EQ. 2) THEN
                  APRINT(ACNT) = PCFLX6(I)
                ELSEIF (J .EQ. 3) THEN
                  APRINT(ACNT) = PADTOT(I)
                END IF
                CHEAD(ACNT)=TRIM(PESTNAME)//'ATMOSPHERIC DEPOSITION - '
                CHEAD(ACNT) = TRIM(CHEAD(ACNT)) // PHASE(I)
                CHEAD(ACNT) = TRIM(CHEAD(ACNT)) // ' - '
                CHEAD(ACNT) = TRIM(CHEAD(ACNT)) // CATMDEP(J)
                CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 80           CONTINUE
 90         CONTINUE
          END IF
C
        END IF
C
        PLIPST= 0.0
        IF (LATFG .EQ. 1) THEN
C         lateral inflows to print
          PLIPSS= 0.0
          PLIPSD= 0.0
          DO 95 N= 1, 5
            PLIFX1(N)= PSSIF(N,L,LEV)*MFACTA
            PLIPSS= PLIPSS+ PLIFX1(N)
            PLIPST= PLIPST+ PLIFX1(N)
 95       CONTINUE
          DO 97 N= 1, 2
            PLIFX2(N)= SDPSIF(N,L,LEV)*MFACTA
            PLIPSD= PLIPSD+ PLIFX2(N)
            PLIPST= PLIPST+ PLIFX2(N)
 97       CONTINUE
C
          IF (PRINTU .GT. 0 .AND. PFLAG(9) .LE. LEV) THEN
            WRITE (PRINTU,2141)
            WRITE (PRINTU,2142)
            WRITE (PRINTU,2143)
            WRITE (PRINTU,2144) PLIFX1,PLIPSS,PLIFX2,PLIPSD,PLIPST
          END IF
          IF (BINU .GT. 0 .AND. ABS(BFLAG(9)) .LE. LEV) THEN
            DO 100 I= 1, 5
              ACNT = ACNT + 1
              APRINT(ACNT) = PLIFX1(I)
              CHEAD(ACNT) = TRIM(PESTNAME) // CLIFX1(I)
              CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 100        CONTINUE
            ACNT = ACNT + 1
            APRINT(ACNT) = PLIPSS
            CHEAD(ACNT) = TRIM(PESTNAME) // 'TLIPSS'
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
            DO 110 I= 1, 2
              ACNT = ACNT + 1
              APRINT(ACNT) = PLIFX2(I)
              CHEAD(ACNT) = TRIM(PESTNAME) // CLIFX2(I)
              CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 110        CONTINUE
            ACNT = ACNT + 1
            APRINT(ACNT) = PLIPSD
            CHEAD(ACNT) = TRIM(PESTNAME) // 'SDLPST'
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
            ACNT = ACNT + 1
            APRINT(ACNT) = PLIPST
            CHEAD(ACNT) = TRIM(PESTNAME) // 'TLIPST'
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          END IF
        END IF
C
        IF (PRINTU .GT. 0 .AND. PFLAG(9) .LE. LEV) THEN
          WRITE (PRINTU,2150)
          WRITE (PRINTU,2160)
          WRITE (PRINTU,2170)
        END IF
C
        CALL TRNVEC (I5,PSCFX2(1,L,LEV),MFACTA,MFACTB,
     O               PCFLX2)
C
        CALL TRNVEC (I3,PSCFX3(1,L,LEV),MFACTA,MFACTB,
     O               PCFLX3)
C
        IF (PRINTU .GT. 0 .AND. PFLAG(9) .LE. LEV) THEN
          WRITE (PRINTU,2180)  PCFLX2, PCFLX3
        END IF
        IF (BINU .GT. 0 .AND. ABS(BFLAG(9)) .LE. LEV) THEN
          DO 120 I= 1, 5
            ACNT = ACNT + 1
            APRINT(ACNT) = PCFLX2(I)
            CHEAD(ACNT) = TRIM(PESTNAME) // CCFLX2(I)
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 120      CONTINUE
          DO 130 I= 1, 3
            ACNT = ACNT + 1
            APRINT(ACNT) = PCFLX3(I)
            CHEAD(ACNT) = TRIM(PESTNAME) // CCFLX3(I)
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 130      CONTINUE
        END IF
C
        IF (PRINTU .GT. 0 .AND. PFLAG(9) .LE. LEV) THEN
          WRITE (PRINTU,2190)
          WRITE (PRINTU,2200)
          WRITE (PRINTU,2210)
        END IF
C
        CALL TRNVEC (I2,PSCFX1(1,L,LEV),MFACTA,MFACTB,
     O               PCFLX1)
C
        PSOSD = PCFLX1(1)+ PCFLX1(2)
        PSOPST= PSOSD+ PCFLX2(1)
        PPOPST= PSOPST+ PCFLX2(5)+ PCFLX3(3)
C
        IF (PRINTU .GT. 0 .AND. PFLAG(9) .LE. LEV) THEN
          WRITE (PRINTU,2220)  PCFLX1, PSOSD, PSOPST, PPOPST
C
          WRITE (PRINTU,2230)
          WRITE (PRINTU,2240)
        END IF
        IF (BINU .GT. 0 .AND. ABS(BFLAG(9)) .LE. LEV) THEN
          DO 140 I= 1, 2
            ACNT = ACNT + 1
            APRINT(ACNT) = PCFLX1(I)
            CHEAD(ACNT) = TRIM(PESTNAME) // CCFLX1(I)
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 140      CONTINUE
          ACNT = ACNT + 1
          APRINT(ACNT) = PSOSD
          CHEAD(ACNT) = TRIM(PESTNAME) // 'SOSDPS'
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          ACNT = ACNT + 1
          APRINT(ACNT) = PSOPST
          CHEAD(ACNT) = TRIM(PESTNAME) // 'SOPEST'
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          ACNT = ACNT + 1
          APRINT(ACNT) = PPOPST
          CHEAD(ACNT) = TRIM(PESTNAME) // 'POPEST'
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
        END IF
C
        CALL TRNVEC (I5,PSCFX4(1,L,LEV),MFACTA,MFACTB,
     O               PCFLX4)
C
        IF (PRINTU .GT. 0 .AND. PFLAG(9) .LE. LEV) THEN
          WRITE (PRINTU,2250)  PCFLX4
        END IF
        IF (BINU .GT. 0 .AND. ABS(BFLAG(9)) .LE. LEV) THEN
          DO 150 I= 1, 5
            ACNT = ACNT + 1
            APRINT(ACNT) = PCFLX4(I)
            CHEAD(ACNT) = TRIM(PESTNAME) // CCFLX4(I)
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 150      CONTINUE
        END IF
C
C       pesticide balance check and report
C
C       convert storages to external units for balance
        PSSTOS= TOTPS(L,LEV)*MFACTA+ MFACTB
        PSSTO = TOTPS(L,1)*MFACTA+ MFACTB
C
C       find the net output of pesticide from the pls
        MATIN= PADALL+ PLIPST
        MATDIF= MATIN- (PPOPST+PCFLX4(5)+PCFLX3(2))
C
        J= 1
        CALL BALCHK (J,LSNO,DATIM,MESSU,PRINTU,MSGFL,
     I               PSSTOS,PSSTO,MATIN,MATDIF,UNITID,I1,
     M               PSWCNT(5))
C
        IF (BINU .GT. 0 .AND. ABS(BFLAG(9)) .LE. LEV) THEN
C         write binary output
        CALL EXDATE(
     I              DATIM,
     O              EXDAT)
          IF (BFLAG(9) .GT. 0) THEN
C           at start of run, write the header
            WRITE (BINU) I0,'PERLND  ',LSNO,'PEST    ',
     1            (CLEN(I),(CHEAD(I)(J:J),J=1,CLEN(I)),I=1,ACNT)
C           set bflag to negative to not write headers anymore
            BFLAG(9) = -BFLAG(9)
          END IF
          WRITE (BINU) I1,'PERLND  ', LSNO,'PEST    ',UNITFG,
     1               LEV,(EXDAT(I),I=1,5),(APRINT(I),I=1,ACNT)
        END IF
C
 300  CONTINUE
C
      RETURN
      END
C
C     4.2(1).14.6
C
      SUBROUTINE   PESTPB
C
C     + + + PURPOSE + + +
C     Handle section PEST.
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PEST2 + + +
      INCLUDE    'cplpe.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    L,J
C
C     + + + END SPECIFICATIONS + + +
C
C     handle section pest
      DO 40 L= 1,NPST
        DO 10 J= 1,2
          IF (SDPSFP(J,L).GE.1) THEN
            PAD(SDPSFP(J,L)+IVL1)=SDPS(J,L)
          END IF
 10     CONTINUE
C
        DO 20 J= 1,5
          IF (TSPSSX(J,L).GE.1) THEN
            PAD(TSPSSX(J,L)+IVL1)=TSPSS(J,L)
          END IF
 20     CONTINUE
C
        DO 30 J= 1,3
          IF (SSPSSX(J,L).GE.1) THEN
            PAD(SSPSSX(J,L)+IVL1)=SSPSS(J,L)
          END IF
C
          IF (PEADDX(L,J) .GE. 1) THEN
            PAD(PEADDX(L,J)+IVL1)= PEADDR(L,J)
          END IF
C
          IF (PEADWX(L,J) .GE. 1) THEN
            PAD(PEADWX(L,J)+IVL1)= PEADWT(L,J)
          END IF
          IF (PEADPX(L,J) .GE. 1) THEN
            PAD(PEADPX(L,J)+IVL1)= PEADEP(L,J)
          END IF
 30     CONTINUE
C
        IF (SDEGFP(L).GE.1) THEN
          PAD(SDEGFP(L)+IVL1)= DEGPS(1,L)
        END IF
C
        IF (UDEGFP(L).GE.1) THEN
          PAD(UDEGFP(L)+IVL1)= DEGPS(2,L)
        END IF
C
        IF (LDEGFP(L).GE.1) THEN
          PAD(LDEGFP(L)+IVL1)= DEGPS(3,L)
        END IF
C
        IF (ADEGFP(L).GE.1) THEN
          PAD(ADEGFP(L)+IVL1)= DEGPS(4,L)
        END IF
C
        IF (TDEGFP(L).GE.1) THEN
          PAD(TDEGFP(L)+IVL1)= DEGPS(5,L)
        END IF
C
        IF (OSDPSX(L).GE.1) THEN
          PAD(OSDPSX(L)+IVL1)= SOSDPS(L)
        END IF
C
        IF (SOPSFP(L).GE.1) THEN
          PAD(SOPSFP(L)+IVL1)= SOPEST(L)
        END IF
C
        IF (POPSFP(L).GE.1) THEN
          PAD(POPSFP(L)+IVL1)= POPST(L)
        END IF
C
        IF (TOPSFP(L).GE.1) THEN
          PAD(TOPSFP(L)+IVL1)= TOPST(L)
        END IF
C
 40   CONTINUE
C
      RETURN
      END
C
C     4.2(1).13.9
C
      SUBROUTINE   PESTPT
C
C     + + + PURPOSE + + +
C     Handle section PEST.
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PEST2 + + +
      INCLUDE    'cplpe.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    J,L
C
C     + + + END SPECIFICATIONS + + +
C
C     handle section pest
      DO 20 L= 1, NPST
        DO 10 J= 1, 3
          IF (SPSFP(J,L).GE.1) THEN
            PAD(SPSFP(J,L)+IVL1)= SPS(J,L)
          END IF
C
          IF (UPSFP(J,L).GE.1) THEN
            PAD(UPSFP(J,L)+IVL1)= UPS(J,L)
          END IF
C
          IF (LPSFP(J,L).GE.1) THEN
            PAD(LPSFP(J,L)+IVL1)= LPS(J,L)
          END IF
C
          IF (APSFP(J,L).GE.1) THEN
            PAD(APSFP(J,L)+IVL1)= APS(J,L)
          END IF
C
          IF (TPSFP(J,L).GE.1) THEN
            PAD(TPSFP(J,L)+IVL1)= TPS(J,L)
          END IF
 10     CONTINUE
C
        DO 15 J= 1, 5
          IF (TPESFP(J,L).GE.1) THEN
            PAD(TPESFP(J,L)+IVL1)= TPST(J,L)
          END IF
 15     CONTINUE
C
        IF (IPSFP(L).GE.1) THEN
          PAD(IPSFP(L)+IVL1)  = IPS(L)
        END IF
C
        IF (TPSTFP(L).GE.1) THEN
          PAD(TPSTFP(L)+IVL1)= TOTPST(L)
        END IF
C
 20   CONTINUE
C
      RETURN
      END
C
C     4.2(1).9.5
C
      SUBROUTINE   PSTRXN
     I                    (LSNO,DAYFG,MESSU,MSGFL,DATIM,
     I                     TMP,MOISTM,SOILM,LAYID,
     I                     ADOPFG,ITMXPS,GPSPM,PSPM,
     M                     PSWCNT,PSECNT,CRV1FG,XJCT,XST,PST,
     O                     DEGPS)
C
C     + + + PURPOSE + + +
C     Perform reactions on pesticide
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     ADOPFG,CRV1FG,DAYFG,MSGFL,ITMXPS,
     $            LSNO,MESSU,PSECNT(1),PSWCNT(5),DATIM(5)
      REAL        DEGPS,GPSPM(8),MOISTM,PSPM(8),PST(3),SOILM,
     $            TMP,XJCT,XST
      CHARACTER*4 LAYID
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LSNO   - line number in the opn sequence block of uci
C     DAYFG  - flag for first day or day change
C     MESSU  - ftn unit no. to be used for printout of messages
C     MSGFL  - fortran unit number of HSPF message file
C     TMP    - ???
C     MOISTM - ???
C     SOILM  - ???
C     LAYID  - ???
C     ADOPFG - ???
C     ITMXPS - ???
C     GPSPM  - ???
C     PSPM   - ???
C     PSWCNT - ???
C     PSECNT - ???
C     CRV1FG - ???
C     XJCT   - ???
C     XST    - ???
C     PST    - ???
C     DEGPS  - ???
C     DATIM  - date and time of day
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      M,I4,I20,SCLU,SGRP
      REAL         ADSPS,CMAX,DEGCON,DESPS,FRAC,KADPS,KDSPS,KF1,N1I,N2I,
     $             PSAD,PSCY,PSSU,TFRAC,THADPS,THDSPS,TPS,
     $             TPSAD,TPSSU,XFIX,XMAX
      CHARACTER*4  PESTID(5),CTMP,CHSTR
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (PESTID,CHSTR2)
      CHARACTER*1  CHSTR2(20)
      EQUIVALENCE (CHSTR,CHSTR1)
      CHARACTER*1  CHSTR1(4)
C
C     + + + EXTERNALS + + +
      EXTERNAL    FIRORD,SV,OMSTC,OMSTR,OMSG,NONSV,DEGRAS,OMSTI,OMSTD
C
C     + + + OUTPUT FORMATS + + +
 2030 FORMAT (A4)
C
C     + + + END SPECIFICATIONS + + +
C
      I4   = 4
      I20  = 20
      SCLU = 309
C     assign values to local storage, where necessary
C     general parameters
      DO 10 M= 1,5
        WRITE(CTMP,2030) GPSPM(M)
        PESTID(M)= CTMP
 10   CONTINUE
C
      THDSPS= GPSPM(6)
      THADPS= GPSPM(7)
      CMAX  = GPSPM(8)
C
C     layer-specific parameters
      KDSPS = PSPM(1)
      KADPS = PSPM(2)
      XFIX  = PSPM(3)
      XMAX  = PSPM(4)
      KF1   = PSPM(5)
      N1I   = PSPM(6)
      N2I   = PSPM(7)
      DEGCON= PSPM(8)
C
C     storages
      PSCY= PST(1)
      PSAD= PST(2)
      PSSU= PST(3)
C
C     total pesticide forms in this layer
      TPS= PSAD+ PSCY+ PSSU
C
      IF (TPS.GT.0.001) THEN
C       there is significant pesticide to react
C       adsorb/desorb pesticide
C
C       casentry adopfg
        GO TO (20,90,100),ADOPFG
C
C         case 1
 20       CONTINUE
C           adsorb/desorb pesticide by first order kinetics
C           with this method the adsorption/desorption fluxes are
C           calculated every interval for pesticides
            CALL FIRORD (TMP,MOISTM,KDSPS,KADPS,THDSPS,THADPS,
     $                   PSSU,PSAD,
     O                   ADSPS,DESPS)
C
C           initialize the fraction used to change any negative
C           storages that may have been computed; frac also acts as
C           a flag indicating negative storages were calculated when
C           < 1.0
            FRAC= 1.0
C
C           calculate temporary adsorbed pesticide in storage
            TPSAD= PSAD+ ADSPS- DESPS
C
            IF (TPSAD.LT.0.0) THEN
C             negative storage value is unrealistic
C             calculate that fraction of the flux that is
C             needed to make the storage zero
              FRAC= PSAD/(PSAD-TPSAD)
C
C             write a warning that the adsorbed value of pesticide
C             had to be fixed up so that it did not go negative
C
              CALL OMSTD (DATIM)
              CALL OMSTI (LSNO)
              CALL OMSTC (I20,CHSTR2)
              CALL OMSTR (FRAC)
              CALL OMSTR (PSAD)
              CALL OMSTR (TPSAD)
              CALL OMSTR (ADSPS)
              CALL OMSTR (DESPS)
              CHSTR = LAYID
              CALL OMSTC (I4,CHSTR1)
              SGRP = 1
              CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                   PSWCNT(3))
            END IF
C
C           calculate temporary solution pesticide in storage
            TPSSU= PSSU- ADSPS+ DESPS
C
            IF (TPSSU.LT.0.0) THEN
C             negative storage value is unrealistic
C             calculate that fraction of the flux that is
C             needed to make the storage zero
              TFRAC= PSSU/(PSSU-TPSSU)
C
C             write a warning that the solution value of pesticide
C             had to be fixed up so that it did not go negative
C
              CALL OMSTD (DATIM)
              CALL OMSTI (LSNO)
              CALL OMSTC (I20,CHSTR2)
              CALL OMSTR (FRAC)
              CALL OMSTR (PSSU)
              CALL OMSTR (TPSSU)
              CALL OMSTR (ADSPS)
              CALL OMSTR (DESPS)
              CHSTR = LAYID
              CALL OMSTC (I4,CHSTR1)
              SGRP = 2
              CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                   PSWCNT(4))
C
C             keep the smallest fraction; the smallest fraction
C             of the fluxes will make all the storages either zero
C             or positive
              IF (TFRAC.LT.FRAC)  FRAC= TFRAC
            END IF
C
            IF (FRAC.GE.1.0) THEN
C             no storages would have gone negative; use the
C             temporary values
              PSAD= TPSAD
              PSSU= TPSSU
            ELSE
C             at least one of the storages has gone negative
C             use frac to adjust the fluxes to make all the storages
C             zero or positive
              FRAC = FRAC*0.99
              ADSPS= ADSPS*FRAC
              DESPS= DESPS*FRAC
C
C             recalculate the storages
              PSAD= PSAD+ ADSPS- DESPS
              PSSU= PSSU+ DESPS- ADSPS
            END IF
            GO TO 110
C
C         case 2
 90       CONTINUE
C           adsorb/desorb pesticide by the single value freundlich
C           method
C           with this method the adsorption/desorption is
C           instantaneous and is done every interval for pesticide.
            CALL SV (MOISTM,SOILM,TPS,XFIX,CMAX,XMAX,KF1,N1I,
     I               LSNO,MESSU,MSGFL,DATIM,
     I               ITMXPS,PESTID,LAYID,
     M               PSSU,PSECNT(1),
     O               PSCY,PSAD)
            GO TO 110
C
C         case 3
 100      CONTINUE
C           adsorb/desorb pesticide by the non-single value freundlich
C           method
C           with this method the adsorption/desorption is also
C           instantaneous and is done every interval for pesticide
            CALL NONSV (MOISTM,SOILM,TPS,XFIX,CMAX,XMAX,KF1,N1I,
     I                  LSNO,MESSU,MSGFL,DATIM,
     I                  N2I,ITMXPS,PESTID,XST,LAYID,
     M                  PSSU,CRV1FG,PSECNT(1),XJCT,
     O                  PSCY,PSAD)
            XST=PSAD
C
C       end case
 110    CONTINUE
C
      END IF
C
      IF (DAYFG.EQ.1) THEN
C       it is the first interval of the day
C       degrade pesticide
        CALL DEGRAS (DEGCON,
     M               PSAD,PSCY,PSSU,
     O               DEGPS)
      ELSE
        DEGPS= 0.0
      END IF
C
C     re-assign storages to "permanent" array
      PST(1)= PSCY
      PST(2)= PSAD
      PST(3)= PSSU
C
      RETURN
      END
C
C     4.2(1).15.1.6
C
      SUBROUTINE   PSTACC
     I                    (FRMROW,TOROW)
C
C     + + + PURPOSE + + +
C     Accumulate fluxes for section pest
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    FRMROW,TOROW
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FRMROW - row containing incremental flux accumulation
C     TOROW  - flux row to be incremented
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PEST2 + + +
      INCLUDE    'cplpe.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I2,I3,I5,L,I
C
C     + + + EXTERNALS + + +
      EXTERNAL   ACCVEC
C
C     + + + END SPECIFICATIONS + + +
C
      I2=2
      I3=3
      I5=5
      DO 20 L= 1,NPST
        CALL ACCVEC (I2,PSCFX1(1,L,FRMROW),
     M               PSCFX1(1,L,TOROW))
C
        CALL ACCVEC (I5,PSCFX2(1,L,FRMROW),
     M               PSCFX2(1,L,TOROW))
C
        CALL ACCVEC (I3,PSCFX3(1,L,FRMROW),
     M               PSCFX3(1,L,TOROW))
C
        CALL ACCVEC (I5,PSCFX4(1,L,FRMROW),
     M               PSCFX4(1,L,TOROW))
C
        DO 5 I= 1, 5
          PSSIF(I,L,TOROW)= PSSIF(I,L,TOROW)+ PSSIF(I,L,FRMROW)
 5      CONTINUE
C
        DO 7 I= 1, 2
          SDPSIF(I,L,TOROW)= SDPSIF(I,L,TOROW)+ SDPSIF(I,L,FRMROW)
 7      CONTINUE
C
        DO 10 I= 1, 3
          PSCFX5(L,I,TOROW)= PSCFX5(L,I,TOROW)+ PSCFX5(L,I,FRMROW)
          PSCFX6(L,I,TOROW)= PSCFX6(L,I,TOROW)+ PSCFX6(L,I,FRMROW)
 10     CONTINUE
C
 20   CONTINUE
C
      RETURN
      END
C
C     4.2(1).15.3.6
C
      SUBROUTINE   PSTRST
     I                    (LEV)
C
C     + + + PURPOSE + + +
C     Reset all flux accumulators and those state variables
C     used in material balance check for section pest
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    LEV
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEV    - current output level (2-pivl,3-day,4-mon,5-ann)
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION PEST2 + + +
      INCLUDE    'cplpe.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I2,I3,I5,L,I
C
C     + + + EXTERNALS + + +
      EXTERNAL  SETVEC
C
C     + + + END SPECIFICATIONS + + +
C
      I2=2
      I3=3
      I5=5
C     set flux accumulators to zero
C
      DO 20 L= 1,NPST
C
        CALL SETVEC (I5,0.0,
     O               PSSIF(1,L,LEV))
C
        CALL SETVEC (I2,0.0,
     O               SDPSIF(1,L,LEV))
C
        CALL SETVEC (I2,0.0,
     O               PSCFX1(1,L,LEV))
C
        CALL SETVEC (I5,0.0,
     O               PSCFX2(1,L,LEV))
C
        CALL SETVEC (I3,0.0,
     O               PSCFX3(1,L,LEV))
C
        CALL SETVEC (I5,0.0,
     O               PSCFX4(1,L,LEV))
C
        DO 10 I= 1, 3
          PSCFX5(I,L,LEV)= 0.0
          PSCFX6(I,L,LEV)= 0.0
 10     CONTINUE
C
C       keep storage in state variable used for
C       material balance check
C
        TOTPS(L,LEV)= TOTPS(L,1)
C
 20   CONTINUE
C
      RETURN
      END
