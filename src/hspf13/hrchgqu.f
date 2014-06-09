C
C
C
      SUBROUTINE   PGQUAL
     M                    (OSVREC)
C
C     + + + PURPOSE + + +
C     Process input to section gqual of the rchres module
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   OSVREC
C
C     + + + ARGUMENT DEFINITIONS + + +
C     OSVREC - ???
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION GQUAL 1 + + +
      INCLUDE    'crhgq.inc'
      INCLUDE    'crin2.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,I1,I2,I4,IDELT,IDUM1,IDUM2,IDUM3,IDUM4,IDUM5,
     $             IDUM6,IDUM7,IDUM8,IDUM9,IVAL(5),J,K,
     $             L,LIGHT,NEWDAT(5),NEWMO,N,RETCOD,
     $             SUB1,SUB2,SUB3,SUB4,SUB5,SUB51,SUB6,SUB7,SUBK,
     $             JCITMP,SCLU,SGRP,INITFG,CLEN,CONT
      REAL         RVAL(10),R0
      CHARACTER*80 CHSTR
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (CHSTR,CHSTR1)
      CHARACTER*1  CHSTR1(80)
C
C     + + + INTRINSICS + + +
      INTRINSIC  IABS,MAX
C
C     + + + EXTERNALS + + +
      EXTERNAL     OMSTI,RTABLE,ITABLE,OMSG,ADDTIM,ZIPI
      EXTERNAL     ZIPR,MDATBL,WMSGTT
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (9F8.0)
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' PROCESSING INPUT FOR SECTION GQUAL')
 2010 FORMAT (/,' MATRIX OF PARENT/DAUGHTER COEFFICIENTS FOR',
     $          ' DECAY PROCESS NO.',I5)
 2030 FORMAT (/,' FINISHED PROCESSING INPUT FOR SECTION GQUAL')
C
C     + + + END SPECIFICATIONS + + +
C
      I1= 1
      R0 = 0.0
C
      IF (OUTLEV.GT.1) THEN
        WRITE (MESSU,2000)
      END IF
C
      SCLU = 346
C
C     initialize month-data input
      I= 12*MXGQAL
      CALL ZIPR (I,R0,
     O           GQAFXM)
      CALL ZIPR (I,R0,
     O           GQACNM)
C
C     initialize atmospheric deposition fluxes
      I= 5*MXGQAL
      CALL ZIPR (I,R0,
     O           GQCF10)
      CALL ZIPR (I,R0,
     O           GQCF11)
C
C     initialize table-type subscripts
      SUB1 = 0
      SUB2 = 0
      SUB3 = 0
      SUB4 = 0
      SUB5 = 0
      SUB51= 0
      SUB6 = 0
      SUB7 = 0
      SUBK = 0
C
C     warning and error message offsets and counter initialization
      I= MXGQAL
      J= 0
      CALL ZIPI(I,J,GQWCNT)
      I= 8
      CALL ZIPI(I,J,GQECNT)
C
C     table-type gq-gendata
      I2= 38
      I4=  8
      CALL ITABLE
     I             (I2,I1,I4,UUNITS,
     M              GQPM1(1))
C
C     table-type gq-ad-flags
      I2= 39
      I4=  MXGQ2
      CALL ITABLE
     I             (I2,I1,I4,UUNITS,
     M              GQADFG)
C
C     read in month-data tables where necessary
      DO 30 J= 1, NGQUAL
        N= 2*(J- 1)+ 1
        IF (GQADFG(N) .GT. 0) THEN
C         monthly flux must be read
          CALL MDATBL
     I                (GQADFG(N),
     O                 GQAFXM(1,J),RETCOD)
C         convert units to internal - not done by MDATBL
          IF (UUNITS .EQ. 1) THEN
C           convert from qty/ac.day to qty/ft2.ivl
            DO 10 I= 1, 12
              GQAFXM(I,J)= GQAFXM(I,J)*DELT60/(24.0*43560.0)
 10         CONTINUE
          ELSE IF (UUNITS .EQ. 2) THEN
C           convert from qty/ha.day to qty/m2.ivl
            DO 20 I= 1, 12
              GQAFXM(I,J)= GQAFXM(I,J)*DELT60/(24.0*10000.0)
 20         CONTINUE
          END IF
        END IF
        IF (GQADFG(N+1) .GT. 0) THEN
C         monthly ppn conc must be read
          CALL MDATBL
     I                (GQADFG(N+1),
     O                 GQACNM(1,J),RETCOD)
        END IF
 30   CONTINUE
C
      NGQ3= NGQUAL*3
C
C     quality constituent loop
C
      DO 120 I=1,NGQUAL
C       table-type gq-qaldata
        I2= 40
        I4= 10
        CALL RTABLE
     I               (I2,I,I4,UUNITS,
     M                RVAL(1))
C
        DO 40 K=1,5
          GQID(K,I)= RVAL(K)
 40     CONTINUE
C
        DQAL(I)   = RVAL(6)
        CONCID(I) = RVAL(7)
        CONV(I)   = RVAL(8)
        QTYID(1,I)= RVAL(9)
        QTYID(2,I)= RVAL(10)
        RDQAL(I)  = DQAL(I)*VOL
C
C       get reciprocal of unit conversion factor
        CINV(I)= 1.0/CONV(I)
C
C       process flags for this constituent
C
C       table-type gq-qalfg
        I2= 41
        I4=  7
        CALL ITABLE
     I               (I2,I,I4,UUNITS,
     M                QALFG(1,I))
C
C       table-type gq-flg2
        I2= 42
        I4=  7
        CALL ITABLE
     I               (I2,I,I4,UUNITS,
     M                GQPM2(1,I))
C
C       process parameters for this constituent
C
        IF (QALFG(1,I).EQ.1) THEN
C         qual undergoes hydrolysis
          SUB1= SUB1 + 1
C
C         table-type gq-hydpm
          I2= 43
          I4=  4
          CALL RTABLE
     I                 (I2,SUB1,I4,UUNITS,
     M                  HYDPM(1,I))
C
C         convert rates from /sec to /ivl
          DO 50 K=1,3
            HYDPM(K,I)= HYDPM(K,I)*DELTS
 50       CONTINUE
C
        END IF
C
        IF (QALFG(2,I).EQ.1) THEN
C         qual undergoes oxidation by free radical processes
C
C         table-type gq-roxpm
          SUB2= SUB2 + 1
          I2= 44
          I4=  2
          CALL RTABLE
     I                 (I2,SUB2,I4,UUNITS,
     M                  ROXPM(1,I))
C
C         convert rate from /sec to /ivl
          ROXPM(1,I)= ROXPM(1,I)*DELTS
        END IF
C
        IF (QALFG(3,I).EQ.1) THEN
C         qual undergoes photolysis
C
C         table-type gq-photpm
          SUB3= SUB3 + 1
          I2= 45
          I4= 20
          CALL RTABLE
     I                 (I2,SUB3,I4,UUNITS,
     M                  PHOTPM(1,I))
C
        END IF
C
        IF (QALFG(4,I).EQ.1) THEN
C         qual undergoes volatilization
          SUB4= SUB4 + 1
C
C         table-type gq-cfgas
          I2= 46
          I4=  1
          CALL RTABLE
     I                 (I2,SUB4,I4,UUNITS,
     M                  CFGAS(I))
C
        END IF
C
        IF (QALFG(5,I).EQ.1) THEN
C         qual undergoes biodegradation
          SUB5= SUB5 + 1
C
C         table-type gq-biopm
          I2= 47
          I4=  3
          CALL RTABLE
     I                 (I2,SUB5,I4,UUNITS,
     M                  RVAL(1))
          BIOPM(1,I)= RVAL(1)
          BIOPM(2,I)= RVAL(2)
C
C         convert rate from /day to /ivl
          BIOPM(1,I)= BIOPM(1,I)*DELT60/24.0
C
C         casentry gqpm2(7,i)
          IDUM9=GQPM2(7,I)
          GO TO (60,70,80) , IDUM9
C         specifies source of biomass data
C
C         case 1
 60         CONTINUE
C           time series
            GO TO 90 
C
C         case 2
 70         CONTINUE
C           constant value
            BIO(I)= RVAL(3)
            GO TO 90 
C
C         case 3
 80         CONTINUE
C           monthly values- table-type mon-bio
            SUB51= SUB51 + 1
            I2= 48
            I4= 12
            CALL RTABLE
     I                   (I2,SUB51,I4,UUNITS,
     M                    BIOM(1,I))
C
 90       CONTINUE
C         endcase
C
        END IF
C
        IF (QALFG(6,I).EQ.1) THEN
C         qual undergoes "general" decay
          SUB6= SUB6 + 1
C
C         table-type gq-gendecay
          I2= 49
          I4=  2
          CALL RTABLE
     I                 (I2,SUB6,I4,UUNITS,
     M                  GENPM(1,I))
C
C         convert rate from /day to /ivl
          GENPM(1,I)= GENPM(1,I)*DELT60/24.0
        END IF
C
        IF (QALFG(7,I).EQ.1) THEN
C         constituent is sediment-associated-
C         get all required additional input
          SUB7= SUB7 + 1
C
C         table-type gq-seddecay
          I2= 50
          I4=  4
          CALL RTABLE
     I                 (I2,SUB7,I4,UUNITS,
     M                  ADDCPM(1,I))
C
C         convert rates from /day to /ivl
          ADDCPM(1,I)= ADDCPM(1,I)*DELT60/24.0
          ADDCPM(3,I)= ADDCPM(3,I)*DELT60/24.0
C
C         adsorption/desorption parameters
C
C         table-type gq-kd
          I2= 51
          I4=  6
          CALL RTABLE
     I                 (I2,SUB7,I4,UUNITS,
     M                  ADPM(1,1,I))
C
C         table-type gq-adrate
          I2= 52
          I4=  6
          CALL RTABLE
     I                 (I2,SUB7,I4,UUNITS,
     M                  ADPM(1,2,I))
C
C         convert rates from /day to /ivl
          DO 100 K=1,6
            ADPM(K,2,I)= ADPM(K,2,I)*DELT60/24.0
 100      CONTINUE
C
C         table-type gq-adtheta
          I2= 53
          I4=  6
          CALL RTABLE
     I                 (I2,SUB7,I4,UUNITS,
     M                  ADPM(1,3,I))
C
C         table-type gq-sedconc
          I2= 54
          I4=  6
          CALL RTABLE
     I                 (I2,SUB7,I4,UUNITS,
     M                  SQAL(1,I))
C
C         find the total quantity of material on various forms of
C         sediment
C
          RSQAL(4,I)= 0.0
          RSQAL(8,I)= 0.0
          RSQAL(12,I)= 0.0
C
          DO 110 J=1,3
            RSQAL(J,  I)= SQAL(J,I)*RSED(J)
            RSQAL(J+4,I)= SQAL(J+3,I)*RSED(J+3)
            RSQAL(J+8,I)= RSQAL(J,I)+  RSQAL(J+4,I)
            RSQAL(4,  I)= RSQAL(4,I)+  RSQAL(J,  I)
            RSQAL(8,  I)= RSQAL(8,I)+  RSQAL(J+4,I)
            RSQAL(12, I)= RSQAL(12,I)+ RSQAL(J+8,I)
 110      CONTINUE
C
        ELSE
C         qual not sediment-associated
          RSQAL(12,I)= 0.0
        END IF
C
C       find total quantity of qual in the rchres
        RRQAL(I) = RDQAL(I) + RSQAL(12,I)
        GQST(I,1)= RRQAL(I)
C
 120  CONTINUE
C
C     find values for global flags
C
C     gqalfg indicates whether any qual undergoes each of the decay
C     processes or is sediment-associated
C
      DO 140 K=1,7
        GQALFG(K)= 0
        DO 130 I=1,NGQUAL
          IF (QALFG(K,I).EQ.1)  GQALFG(K)= 1
 130    CONTINUE
 140  CONTINUE
C
C     qalgfg indicates whether a qual undergoes any of the 6 decay
C     processes
      DO 160 I=1,NGQUAL
        QALGFG(I)=0
        DO 150 K=1,6
          IF (QALFG(K,I).EQ.1) QALGFG(I)=1
 150    CONTINUE
 160  CONTINUE
C
C     gdaufg indicates whether any constituent is
C     a "daughter" compound through each of the 6 possible decay
C     processes
C
      DO 180 K=1,6
        GDAUFG(K)= 0
        DO 170 I=1,NGQUAL
          IF (GQPM2(K,I).EQ.1) THEN
            GDAUFG(K)= 1
          END IF
 170    CONTINUE
 180  CONTINUE
C
C     daugfg indicates whether or not a given qual is a daughter
C     compound
      DO 200 I=1,NGQUAL
        DAUGFG(I)=0
        DO 190 K=1,6
          IF (GQPM2(K,I).EQ.1) THEN
            DAUGFG(I)=1
          END IF
 190    CONTINUE
 200  CONTINUE
C
C     get initial value for all inputs which can be constant,
C     vary monthly, or be a time series-some might be over-ridden by
C     monthly values or time series
C
C     table-type gq-values
      I2= 55
      I4=  6
      CALL RTABLE
     I             (I2,I1,I4,UUNITS,
     M              RVAL(1))
C
      TWAT = RVAL(1)
      PHVAL= RVAL(2)
      ROC  = RVAL (3)
      CLD  = RVAL(4)
      SDCNC= RVAL(5)
      PHY  = RVAL (6)
C
      IF (TEMPFG.EQ.3) THEN
C       table-type mon-watemp
        I2= 56
        I4= 12
        CALL RTABLE
     I               (I2,I1,I4,UUNITS,
     M                TEMPM(1))
      END IF
C
      IF (GQALFG(1).EQ.1.AND.PHFLAG.EQ.3) THEN
C       table-type mon-phval
        I2= 94
        I4= 12
        CALL RTABLE
     I               (I2,I1,I4,UUNITS,
     M                PHVALM(1))
      END IF
C
      IF (GQALFG(2).EQ.1.AND.ROXFG.EQ.3) THEN
C       table-type mon-roxygen
        I2= 57
        I4= 12
        CALL RTABLE
     I               (I2,I1,I4,UUNITS,
     M                ROCM(1))
      END IF
C
      IF (GQALFG(3).EQ.1) THEN
C       one or more quals undergoes photolysis-get required input
C
C       table-type gq-alpha
        I2= 58
        I4= 18
        CALL RTABLE
     I               (I2,I1,I4,UUNITS,
     M                ALPH(1))
C
C       table-type gq-gamma
        I2= 59
        I4= 18
        CALL RTABLE
     I               (I2,I1,I4,UUNITS,
     M                GAMM(1))
C
C       table-type gq-delta
        I2= 60
        I4= 18
        CALL RTABLE
     I               (I2,I1,I4,UUNITS,
     M                DEL(1))
C
C       table-type gq-cldfact
        I2= 61
        I4= 18
        CALL RTABLE
     I               (I2,I1,I4,UUNITS,
     M                KCLD(1))
C
C       get any monthly values needed for photolysis
C
C       table-type mon-cloud
        I2= 62
        I4= 12
        IF (CLDFG.EQ.3) THEN
          CALL RTABLE
     I                 (I2,I1,I4,UUNITS,
     M                  CLDM(1))
        END IF
C
C       table-type mon-sedconc
        I2= 63
        I4= 12
        IF (SDFG.EQ.3) THEN
          CALL RTABLE
     I                 (I2,I1,I4,UUNITS,
     M                  SDCNCM(1))
        END IF
C
C       table-type mon-phyto
        I2= 64
        I4= 12
        IF (PHYTFG.EQ.3) THEN
          CALL RTABLE
     I                 (I2,I1,I4,UUNITS,
     M                  PHYM(1))
        END IF
C
C       table-type surf-exposed
C
        IF (HTFG.EQ.0) THEN
          I2= 97
          I4=  1
          CALL RTABLE
     I                (I2,I1,I4,UUNITS,
     M                 RVAL(1))
          CFSAEX= RVAL(1)
        END IF
C
C       fact1 is a pre-calculated value used in photolysis simulation
        FACT1= CFSAEX*DELT60/24.0
C
C       decide which set of light data to use
        JCITMP= LAT
        LIGHT = (IABS(JCITMP) + 5)/10
C
C       no table for equation, so use 10 deg table
        IF (LIGHT.EQ.0) THEN
          LIGHT= 1
        END IF
C
C       read the light data- 9 values to a line,
        SGRP  = 50+ LIGHT
        INITFG= 1
        DO 210 L=1,4
          CLEN  = 80
          CALL WMSGTT (MSGFL,SCLU,SGRP,INITFG,
     M                 CLEN,
     O                 CHSTR1,CONT)
          READ (CHSTR,1000) (LIT(K,L),K=1,9)
          INITFG= 0
          CLEN  = 80
          CALL WMSGTT (MSGFL,SCLU,SGRP,INITFG,
     M                 CLEN,
     O                 CHSTR1,CONT)
          READ (CHSTR,1000) (LIT(K,L),K=10,18)
 210    CONTINUE
C
C       determine which season (set) of data to start with
        LITFG= 0
C
C       look one time-step ahead to see which "month" to use,
C       because we might be on a month boundary, in which case
C       datim will contain the earlier month
C
        IDELT= DELT
        DO 220 I=1,5
          NEWDAT(I)=DATIM(I)
 220    CONTINUE
C
        CALL ADDTIM
     I               (IDELT,NDAY(1),PIVL,PYREND,
     M                NEWDAT(1),IDUM1,
     O                IDUM2,IDUM3,IDUM4,IDUM5,IDUM6,IDUM7,IDUM8)
C
        NEWMO= NEWDAT(2)
        LSET = NEWMO/3
        IF (LSET.EQ.0) THEN
          LSET= 4
        END IF
C
C       southern hemisphere is 2 seasons out of phase
C
        IF (LAT.LT.0) THEN
          LSET= LSET + 2
          IF (LSET.GT.4) THEN
            LSET= LSET - 4
          END IF
        END IF
C
      END IF
C
      IF (GQALFG(4).EQ.1) THEN
C       one or more constituents undergoes volatilization
C       process- input required to compute reaeration coefficient
C
C       flags - table-type ox-flags
        I2= 71
        I4=  1
        CALL ITABLE
     I               (I2,I1,I4,UUNITS,
     M                IVAL)
C
        OXPM1= IVAL(1)
C
        IF (HTFG.EQ.0) THEN
C         get elevation and compute pressure correction factor -
C         table-type elev
          I2= 73
          I4=  1
          CALL RTABLE
     I                 (I2,I1,I4,UUNITS,
     M                  RVAL(1))
          ELEV= RVAL(1)
          CFPRES= ((288.0-0.001981*ELEV)/288.0)**5.256
C
        END IF
C
        IF (LKFG.EQ.1) THEN
C         rchres is a lake - get reaeration parameter from table-type
C         ox-cforea
          I2= 75
          I4=  1
          CALL RTABLE
     I                 (I2,I1,I4,UUNITS,
     M                  RVAL(1))
          CFOREA= RVAL(1)
C
        ELSE
C         rchres is a free-flowing stream
C         casentry reamfg
          GO TO (230,240,250) , REAMFG
C         case 1
 230        CONTINUE
C           tsivoglou method - table-type ox-tsivoglou
            I2= 76
            I4=  2
            CALL RTABLE
     I                   (I2,I1,I4,UUNITS,
     M                    RVAL(1))
C
            REAKT = RVAL(1)
            TCGINV= RVAL(2)
            IF (HYDRFG.EQ.0) THEN
C             read in len, delth - table-type ox-len-delth
              I2= 77
              I4=  2
              CALL RTABLE
     I                     (I2,I1,I4,UUNITS,
     M                      RVAL(1))
C
              LEN  = RVAL(1)
              DELTH= RVAL(2)
C
            ELSE
C             len, delth are available from hydr
C
            END IF
            GO TO 260
C
C         case 2
 240        CONTINUE
C           owen/churchill/o'connor-dobbins - table-type ox-tcginv
            I2= 78
            I4=  1
            CALL RTABLE
     I                   (I2,I1,I4,UUNITS,
     M                    RVAL(1))
C
            TCGINV= RVAL(1)
            GO TO 260
C
C         case 3
 250        CONTINUE
C           user formula - table-type ox-reaparm
            I2= 79
            I4=  4
            CALL RTABLE
     I                   (I2,I1,I4,UUNITS,
     M                    OXPM4(1))
C
 260      CONTINUE
C         endcase
C
        END IF
      END IF
C
C     process tables specifying relationship between
C     "parent" and "daughter" compounds
      SUBK= 0
C
      DO 270 K=1,6
        IF (GDAUFG(K).EQ.1) THEN
C
C         table-type gq-daughter
          IF (OUTLEV.GT.2) THEN
            WRITE (MESSU,2010) K
          END IF
          SUBK= SUBK + 1
          I2= 65
c         read ngqual rows of mxgqal each - in case using old 3-row table
          I4=  NGQUAL*MXGQAL
          CALL RTABLE
     I                 (I2,SUBK,I4,UUNITS,
     M                  C(1,1,K))
        END IF
 270  CONTINUE
C
      IF (GQALFG(7).EQ.1) THEN
C       one or more quals are sediment-associated
        IF (SEDFG.EQ.0) THEN
C         section sedtrn not active
          CALL OMSTI (RCHNO)
          SGRP = 11
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
        END IF
      END IF
C
      IF (HYDRFG.EQ.1) THEN
C       check that required options in section hydr have been selected
        IF (GQALFG(3).EQ.1.AND.AUX1FG.EQ.0) THEN
C         error-simulation of photolysis requires aux1fg to be on to
C         calculate average depth
          SGRP= 12
          CALL OMSTI(RCHNO)
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
        END IF
C
        IF (GQALFG(4).EQ.1) THEN
          IF (LKFG.EQ.0) THEN
            IF (AUX2FG.EQ.0) THEN
C             error-simulation of volatilization
C             in a free flowing stream requires aux3fg on
              SGRP= 13
              CALL OMSTI(RCHNO)
              CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                   ECOUNT)
            END IF
          ELSE
            IF (AUX1FG.EQ.0) THEN
C             error-simulation of volatilization in a lake requires
C             aux1fg on to calculate average depth
              SGRP= 14
              CALL OMSTI(RCHNO)
              CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                   ECOUNT)
            END IF
          END IF
        END IF
      END IF
C
C     determine how many records the osv requires
      OSVREC= MAX(OSVREC,32)
C
      IF (GQALFG(4).EQ.1) THEN
C       need some variables in section oxrx, so osv
C       must include it
        OSVREC= MAX(OSVREC,33)
      END IF
C
      IF (GQALFG(3).EQ.1.AND.PHYTFG.EQ.1) THEN
C       will need variables phyto and pyfp, so include
C       section plank in osv
        OSVREC= MAX(OSVREC,37)
      END IF
C
      IF (OUTLEV.GT.1) THEN
        WRITE (MESSU,2030)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   GQUAL
C
C     + + + PURPOSE + + +
C     Simulate the behavior of a generalized quality constituent
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION GQUAL2 + + +
      INCLUDE    'crhgq.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     FPT,I,IDMK,ITOBE,J,K,L,N,REQFG,TSSUB(2),FLGVAL
      REAL        A,AVDEPE,AVDEPM,AVVELE,CLDL,EXPNT,FACT2(18),KL,
     $            TW20,VOLSP,INDQAL,GQADFX,GQADCN
      DOUBLE PRECISION DPDQAL(MXGQAL)
      CHARACTER*6 OPTYP,TSNAM,SECNAM,MSECNM,OPFGNM
C
C     + + + FUNCTIONS + + +
      REAL       DAYVAL
C
C     + + + INTRINSICS + + +
      INTRINSIC  EXP
C
C     + + + EXTERNALS + + +
      EXTERNAL   DAYVAL,OXREA,ADVECT,DDECAY,ADVQAL,ADECAY,ADSDES,
     $           COPYR,HREQTS
C
C     + + + DATA INITIALIZATIONS + + +
      DATA TSSUB/1,1/
      DATA OPTYP,SECNAM/'RCHRES','GQUAL '/
C
C     + + + END SPECIFICATIONS + + +
C
C     single precision version of vol
      VOLSP= VOL
C
C     get any time series normally supplied by other active module
C     sections
C
      IF (HYDRFG .EQ. 0) THEN
C       read in "hydraulics" time series
CTHJ        AVDEP= PAD(AVDFP + IVL1)
        REQFG= 3
        TSNAM= 'AVDEP '
        CALL HREQTS (AVDFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,RCHNO,
     I               TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,FLGVAL,
     O               AVDEP)
      ELSE
C       it is obtained directly from section hydr
      END IF
C
      IF (UUNITS .EQ. 1) THEN
C       english to metric
        AVDEPM= AVDEP*.3048
        AVDEPE= AVDEP
      ELSE
C       metric to english
        AVDEPM= AVDEP
        AVDEPE= AVDEP*3.28
      END IF
      IF (GQALFG(1) .EQ. 1) THEN
C       doing hydrolysis- we need ph data
C
C       casentry phflag
        GO TO (10,20,30) , PHFLAG
C
C       case 1
 10       CONTINUE
C         time series
          IF (PHFG .EQ. 1) THEN
C           use value computed in last time step
            PHVAL= PH
          ELSE
C           need an input time series
CTHJ            PHVAL= PAD(PHVFP + IVL1)
            REQFG= 5
            TSNAM= 'PHVAL '
            MSECNM= 'PHCARB'
            OPFGNM= 'PHFLAG'
            CALL HREQTS (PHVFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,RCHNO,
     I                   TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,PHFLAG,
     O                   PHVAL)
          END IF
          GO TO 40
C
C       case 2
 20       CONTINUE
C         user-supplied value, read in by run interpreter
          GO TO 40
C
C       case 3
 30       CONTINUE
C         12 user-supplied monthly values
C
          IF (DAYFG .EQ. 1) THEN
C           interpolate a new value
            PHVAL= DAYVAL(PHVALM(MON),PHVALM(NXTMON),DAY,NDAYS)
          END IF
C
 40     CONTINUE
C       endcase
      END IF
C
C     get water temp data
C
C     casentry tempfg
      GO TO (50,60,70) , TEMPFG
C
C     case 1
 50     CONTINUE
C       time series
C
        IF (HTFG .EQ. 1) THEN
C         use value computed by section htrch
        ELSE
C         input time series required
CTHJ          TW= PAD(TWFP + IVL1)
          REQFG= 5
          TSNAM= 'TW    '
          MSECNM= 'HTRCH '
          OPFGNM= 'TEMPFG'
          CALL HREQTS (TWFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,RCHNO,
     I                 TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,TEMPFG,
     O                 TW)
        END IF
C
        TWAT= TW
        GO TO 80 
C
C     case 2
 60     CONTINUE
C       user-supplied value, read in by run interpreter
        GO TO 80
C
C     case 3
 70     CONTINUE
C       12 user-supplied monthly values
C
        IF (DAYFG .EQ. 1) THEN
C         interpolate a new value
          TWAT=DAYVAL(TEMPM(MON),TEMPM(NXTMON),DAY,NDAYS)
        END IF
C
 80   CONTINUE
C     endcase
C
C     this number is used to adjust reaction rates for temperature
      TW20= TWAT - 20.0
C
C     tw20 may be required for bed decay of qual even if tw is
C     undefined (due to vol=0.0)
      IF (TWAT .LE. -10.0) THEN
        TW20= 0.0
      END IF
C
C     correct unrealistically high values of tw calculated in htrch
      IF (TWAT .GE. 50.) THEN
        TW20= 30.
      END IF
C
      IF (GQALFG(2) .EQ. 1) THEN
C       one or more constituents undergo oxidation by free radical
C       processes
C
C       casentry roxfg
        GO TO (90,100,110) , ROXFG
C
C       case 1
 90       CONTINUE
C         time series
CTHJ          ROC= PAD(ROCFP + IVL1)
          REQFG= 4
          TSNAM= 'ROC   '
          OPFGNM= 'ROXFG '
          CALL HREQTS (ROCFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,RCHNO,
     I                 TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,ROXFG,
     O                 ROC)
          GO TO 120
C
C       case 2
 100      CONTINUE
C         user-supplied value, read in by the run interpreter
          GO TO 120
C
C       case 3
 110      CONTINUE
C         12 user-supplied values
C
          IF (DAYFG .EQ. 1) THEN
C           interpolate a new value
            ROC= DAYVAL(ROCM(MON),ROCM(NXTMON),DAY,NDAYS)
          END IF
C
 120    CONTINUE
C       endcase
      END IF
C
      IF (GQALFG(3) .EQ. 1) THEN
C       one or more constituents undergoes photolysis decay
C
        IF (LITFG .EQ. 1) THEN
C         we need the next set of light data
          LSET= LSET + 1
          IF (LSET .GT. 4) THEN
            LSET= 1
          END IF
        END IF
C
        IF (EMONFG .EQ. 1) THEN
C         this is the last time step in the present month- check if
C         we will start a new season on next time step
C
          IF (NXTMON/3 .NE. MON/3) THEN
            LITFG= 1
          ELSE
            LITFG= 0
          END IF
C
        ELSE
          LITFG= 0
        END IF
C
C       casentry sdfg
        GO TO (130,140,150) , SDFG
C
C       case 1
 130      CONTINUE
C         use time series for sediment conc
C
          IF (SEDFG .EQ. 1) THEN
C           use value computed in section sedtrn
            SDCNC= SSED(4)
          ELSE
C           read from the scratch pad
CTHJ            SSED(4)= PAD(SSEDFP(4) + IVL1)
            REQFG= 5
            TSNAM= 'SSED  '
            TSSUB(1)= 4
            MSECNM= 'SEDTRN'
            OPFGNM= 'SDFG  '
            CALL HREQTS (SSEDFP(4),IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,
     I                   RCHNO,TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,SDFG,
     O                   SSED(4))
            TSSUB(1)= 1
            SDCNC  = SSED(4)
          END IF
          GO TO 160
C
C       case 2
 140      CONTINUE
C         user-supplied value, read by run interpreter
          GO TO 160
C
C       case 3
 150      CONTINUE
C         12 user-supplied values
C
          IF (DAYFG .EQ. 1) THEN
C           interpolate a new value
            SDCNC= DAYVAL(SDCNCM(MON),SDCNCM(NXTMON),DAY,NDAYS)
          END IF
C
 160    CONTINUE
C       endcase
C
C       casentry phytfg
        GO TO (170,180,190) , PHYTFG
C
C       case 1
 170      CONTINUE
C         use time series for phyto
          IF (PLKFG .EQ. 1) THEN
C
            IF (PHYFG .EQ. 1) THEN
C             use last simulated value
            ELSE
C             read from scratch pad
CTHJ              PHYTO= PAD(PYFP + IVL1)
              REQFG= 4
              TSNAM= 'PHYTO '
              OPFGNM= 'PHYTFG'
              CALL HREQTS (PYFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,
     I                     RCHNO,TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,
     I                     PHYTFG,
     O                     PHYTO)
            END IF
C
          ELSE
CTHJ            PHYTO= PAD(PYFP + IVL1)
            REQFG= 5
            TSNAM= 'PHYTO '
            MSECNM= 'PLANK '
            OPFGNM= 'PHYTFG'
            CALL HREQTS (PYFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,
     I                   RCHNO,TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,
     I                   PHYTFG,
     O                   PHYTO)
          END IF
C
          PHY= PHYTO
          IF (PHY .LT. 0.0) THEN
            PHY= 0.0
          END IF
          GO TO 200
C
C       case 2
 180      CONTINUE
C         single user-supplied value, read by run interpreter
          GO TO 200
C
C       case 3
 190      CONTINUE
C         12 user-supplied values
C
          IF (DAYFG .EQ. 1) THEN
C           interpolate a new value
            PHY= DAYVAL(PHYM(MON),PHYM(NXTMON),DAY,NDAYS)
          END IF
C
 200    CONTINUE
C       endcase
C
C       casentry cldfg
        GO TO (210,220,230) , CLDFG
C
C       case 1
 210      CONTINUE
C         use time series
CTHJ          CLOUD= PAD(CCFP + IVL1)
          REQFG= 4
          TSNAM= 'CLOUD '
          OPFGNM= 'CLDFG '
          CALL HREQTS (CCFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,RCHNO,
     I                 TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,CLDFG,
     O                 CLOUD)
          CLD  = CLOUD
          GO TO 240
C
C       case 2
 220      CONTINUE
C         single user-supplied value, read by run interpreter
          GO TO 240
C
C       case 3
 230      CONTINUE
C         12 user-supplied values
C
          IF (DAYFG .EQ. 1) THEN
C           interpolate a new value
            CLD= DAYVAL(CLDM(MON),CLDM(NXTMON),DAY,NDAYS)
          END IF
C
 240    CONTINUE
C       endcase
C
C
        IF (AVDEPE .GT. 0.17) THEN
C         depth of water in rchres is greater than two inches -
C         consider photolysis; this criteria will also be applied
C         to other decay processes
C
          DO 250 L=1,18
C
C           evaluate the light extinction exponent- 2.76*klamda*d
            KL   = ALPH(L) + GAMM(L)*SDCNC + DEL(L)*PHY
            EXPNT= 2.76*KL*AVDEPM*100.0
C
C           evaluate the cloud factor
            CLDL= (10.0 - CLD*KCLD(L))/10.0
C
            IF (EXPNT .LE. -20.) THEN
              EXPNT= -20.
            END IF
            IF (EXPNT .GE. 20.) THEN
              EXPNT= 20.
            END IF
C           evaluate the precalculated factors fact2
            FACT2(L)= CLDL*LIT(L,LSET)*(1.0 - EXP(-EXPNT))/EXPNT
C
 250      CONTINUE
C
        ELSE
C         depth of water in rchres is less than two inches -
C         photolysis is not considered
        END IF
C
      END IF
C
      IF (GQALFG(4) .EQ. 1) THEN
C       prepare to simulate volatilization by finding the oxygen
C       reaeration coefficient
C
        IF (LKFG .EQ. 1) THEN
C         get wind movement
CBRB      WIND= PAD(WDFP + IVL1)
          REQFG= 4
          TSNAM= 'WIND  '
          OPFGNM= 'LKFG  '
          CALL HREQTS (WDFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,RCHNO,
     I                 TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,LKFG,
     O                 WIND)
          IF (HYDRFG .EQ. 0) THEN
CTHJ            AVDEP= PAD(AVDFP + IVL1)
            REQFG= 5
            TSNAM= 'AVDEP '
            MSECNM= 'HYDR  '
            OPFGNM= 'LKFG  '
            CALL HREQTS (AVDFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,
     I                   RCHNO,TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,LKFG,
     O                   AVDEP)
          END IF
        ELSE
C         water body is not a lake
C
          IF (HYDRFG .EQ. 0) THEN
            IF (REAMFG .GT. 1) THEN
CTHJ              AVDEP= PAD(AVDFP + IVL1)
              REQFG= 5
              TSNAM= 'AVDEP '
              MSECNM= 'HYDR  '
              OPFGNM= 'REAMFG'
              CALL HREQTS (AVDFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,
     I                     RCHNO,TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,
     I                     REAMFG,
     O                     AVDEP)
            END IF
CTHJ            AVVEL= PAD(AVVFP + IVL1)
            REQFG= 5
            TSNAM= 'AVVEL '
            MSECNM= 'HYDR  '
            OPFGNM= 'LKFG  '
            CALL HREQTS (AVVFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,RCHNO,
     I                   TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,LKFG,
     O                   AVVEL)
          END IF
        END IF
C
        IF (UUNITS .EQ. 1) THEN
          AVDEPE= AVDEP
          AVVELE= AVVEL
        ELSE
          AVDEPE= AVDEP*3.28
          AVVELE= AVVEL*3.28
        END IF
C
        IF (AVDEPE .GT. 0.17) THEN
C         rchres depth is sufficient to consider volatilization
C
C         compute oxygen reaeration rate-korea
          CALL OXREA
     I               (LKFG,WIND,CFOREA,AVVELE,AVDEPE,TCGINV,
     I                REAMFG,REAK,REAKT,EXPRED,EXPREV,LEN,
     I                DELTH,TWAT,DELTS,DELT60,UUNITS,
     O                KOREA)
C
        ELSE
C         rchres depth is not sufficient to consider volatilization
        END IF
C
      END IF
C
      IF (PRECFP .GE. 1) THEN
C       precipitation is input
        PREC= PAD(PRECFP+IVL1)
      ELSE
C       no precipitation
        PREC= 0.0
      END IF
      IF (HYDRFG .NE. 1) THEN
C       section hydr is inactive, so sarea must be on the pad
CTHJ        SAREA= PAD(SAFP+IVL1)
        REQFG= 3
        TSNAM= 'SAREA '
        CALL HREQTS (SAFP,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,RCHNO,
     I               TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,FLGVAL,
     O               SAREA)
      END IF
C
C     main loop-simulate each quality constituent
C
      DO 420 I=1,NGQUAL
C       get data on inflow of dissolved material
        FPT= GQIF1X(I)
C
        IF (FPT .GT. 0) THEN
          A= PAD(FPT + IVL1)
C         convert to internal "concentration" units
          IDQAL(I)= A*CONV(I)
        ELSE
          IDQAL(I)= 0.0
        END IF
C
C       compute atmospheric deposition influx
        N= 2*(I-1)+ 1
C       dry deposition
        IF (GQADFG(N) .LE. -1) THEN
CTHJ          GQADDR(I)= SAREA*CONV(I)*PAD(GQAFFP(I)+IVL1)
          REQFG= 4
          TSNAM= 'GQADFX'
          TSSUB(1)= I
          OPFGNM= 'GQADFG'
          CALL HREQTS (GQAFFP(I),IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,
     I                 RCHNO,TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,
     I                 GQADFG(N),
     O                 GQADFX)
          GQADDR(I)= SAREA*CONV(I)*GQADFX
        ELSE IF (GQADFG(N) .GE. 1) THEN
          GQADDR(I)= SAREA*CONV(I)*DAYVAL(GQAFXM(MON,I),
     I                                    GQAFXM(NXTMON,I),DAY,NDAYS)
        ELSE
          GQADDR(I)= 0.0
        END IF
C       wet deposition
        IF (GQADFG(N+1) .LE. -1) THEN
CTHJ          GQADWT(I)= PREC*SAREA*PAD(GQACFP(I)+IVL1)
          REQFG= 4
          TSNAM= 'GQADCN'
          TSSUB(1)= I
          OPFGNM= 'GQADFG'
          CALL HREQTS (GQACFP(I),IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,
     I                 RCHNO,TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,
     I                 GQADFG(N+1),
     O                 GQADCN)
          GQADWT(I)= PREC*SAREA*GQADCN
        ELSE IF (GQADFG(N+1) .GE. 1) THEN
          GQADWT(I)= PREC*SAREA*DAYVAL(GQACNM(MON,I),GQACNM(NXTMON,I),
     I                                 DAY,NDAYS)
        ELSE
          GQADWT(I)= 0.0
        END IF
        GQADEP(I)= GQADDR(I)+ GQADWT(I)
C
        INDQAL= IDQAL(I)+ GQADEP(I)
C
C       simulate advection of dissolved material
C
        DPDQAL(I)=DQAL(I)
C
        CALL ADVECT
     I              (INDQAL,VOLS,SROVOL,VOL,EROVOL,SOVOL(1),
     I               EOVOL(1),NEXITS,
     M               DPDQAL(I),
     O               RODQAL(I),ODQAL(1,I))
C
        DQAL(I)=DPDQAL(I)
C
C       get biomass input, if required (for degradation)
        IF (QALFG(5,I) .EQ. 1) THEN
C
C         casentry gqpm2(7,i)
          IDMK = GQPM2(7,I)
          GO TO (260,270,280) , IDMK
C         specifies source of biomass data
C
C         case 1
 260        CONTINUE
C           input time series-read from scratch pad
CTHJ            BIO(I)= PAD(BIOFP(I) + IVL1)
            REQFG= 4
            TSNAM= 'BIO   '
            TSSUB(1)= I
            OPFGNM= 'GQPM27'
            CALL HREQTS (BIOFP(I),IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,
     I                   RCHNO,TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,
     I                   IDMK,
     O                   BIO(I))
            TSSUB(1)= 1
            GO TO 290
C
C         case 2
 270        CONTINUE
C           single user-supplied value-read by run interpreter
            GO TO 290
C
C         case 3
 280        CONTINUE
C           12 user-supplied values
C
            IF (DAYFG .EQ. 1) THEN
C             interpolate a new value
              BIO(I)= DAYVAL(BIOM(MON,I),BIOM(NXTMON,I),DAY,NDAYS)
            END IF
 290      CONTINUE
C         endcase
        END IF
C
        IF (AVDEPE .GT. 0.17) THEN
C         simulate decay of dissolved material
C
          CALL DDECAY
     I                (QALFG(1,I),TW20,HYDPM(1,I),PHVAL,ROXPM(1,I),
     I                 ROC,FACT2(1),FACT1,PHOTPM(1,I),KOREA,CFGAS(I),
     I                 BIOPM(1,I),BIO(I),GENPM(1,I),VOLSP,DQAL(I),
     I                 HR,DELT60,
     O                 DDQAL(1,I))
C
          PDQAL(I)= 0.0
C
          DO 310 K=1,6
C
            IF (GQPM2(K,I) .EQ. 1) THEN
C             this compound is a "daughter"-compute the contribution
C             to it from its "parent(s)"
C
              ITOBE= I - 1
              DO 300 J= 1,ITOBE
                PDQAL(I)= PDQAL(I) + DDQAL(K,J)*C(I,J,K)
 300          CONTINUE
            END IF
C
 310      CONTINUE
C
C         update the concentration to account for decay and for input
C         from decay of "parents"- units are conc/l
          DQAL(I)= DQAL(I) + (PDQAL(I) - DDQAL(7,I))/VOLSP
C
        ELSE
C         rchres depth is less than two inches - dissolved decay
C         is not considered
          DO 320 L=1,7
            DDQAL(L,I)= 0.0
 320      CONTINUE
          PDQAL(I)= 0.0
        END IF
C
        IF (QALFG(7,I) .EQ. 1) THEN
C         this constituent is associated with sediment
C
C         zero the accumulators
          ISQAL(4,I)= 0.0
          DSQAL(4,I)= 0.0
          ROSQAL(4,I)= 0.0
          IF (NEXITS .GT. 1) THEN
            DO 330 N= 1, NEXITS
              TOSQAL(N,I)= 0.0
 330        CONTINUE
          END IF
C
C         repeat for each sediment size fraction
          DO 350 J= 1,3
C           get data on inflow of sediment-associated material
            FPT= GQIF2X(J,I)
C
            IF (FPT .GT. 0) THEN
              ISQAL(J,I)= PAD(FPT + IVL1)*CONV(I)
            ELSE
              ISQAL(J,I)= 0.0
            END IF
C
C           advect this material, including calculation of deposition
C           and scour
C
            CALL ADVQAL
     I                  (ISQAL(J,I),RSED(J),RSED(J + 3),
     I                   DEPSCR(J),ROSED(J),OSED(1,J),NEXITS,RCHNO,
     I                   MESSU,MSGFL,DATIM,
     I                   GQID(1,I),J,RSQAL(J,I),RSQAL(J + 4,I),
     M                   GQECNT(1),
     O                   SQAL(J,I),SQAL(J + 3,I),DSQAL(J,I),
     O                   ROSQAL(J,I),OSQAL(1,J,I))
C
            ISQAL(4,I)= ISQAL(4,I)+ ISQAL(J,I)
            DSQAL(4,I)= DSQAL(4,I)+ DSQAL(J,I)
            ROSQAL(4,I)= ROSQAL(4,I) + ROSQAL(J,I)
            IF (NEXITS .GT. 1) THEN
              DO 340 N= 1, NEXITS
                TOSQAL(N,I)= TOSQAL(N,I)+ OSQAL(N,J,I)
 340          CONTINUE
            END IF
C
 350      CONTINUE
C
          TIQAL(I)= IDQAL(I)+ ISQAL(4,I)
          TROQAL(I)= RODQAL(I)+ ROSQAL(4,I)
          IF (NEXITS .GT. 1) THEN
            DO 360 N= 1, NEXITS
              TOQAL(N,I)= ODQAL(N,I)+ TOSQAL(N,I)
 360        CONTINUE
          END IF
C
          IF (AVDEPE .GT. 0.17) THEN
C           simulate decay on suspended sediment
C
            CALL ADECAY
     I                  (ADDCPM(1,I),TW20,RSED(1),
     M                   SQAL((1),I),
     O                   SQDEC((1),I))
C
          ELSE
C           rchres depth is less than two inches - decay of qual
C           associated with suspended sediment is not considered
            DO 370 L=1,3
              SQDEC(L,I)= 0.0
 370        CONTINUE
          END IF
C
C         simulate decay on bed sediment
C
          CALL ADECAY
     I                (ADDCPM(3,I),TW20,RSED(4),
     M                 SQAL((4),I),
     O                 SQDEC((4),I))
C
C         get total decay
          SQDEC(7,I)= 0.0
          DO 380 K=1,6
            SQDEC(7,I)= SQDEC(7,I) + SQDEC(K,I)
 380      CONTINUE
C
          IF (AVDEPE .GT. 0.17) THEN
C           simulate exchange due to adsorption and desorption
C
            CALL ADSDES
     I                  (VOLSP,RSED(1),ADPM(1,1,I),TW20,
     M                   DQAL(I),SQAL(1,I),
     O                   ADQAL(1,I))
C
          ELSE
C           rchres depth is less than two inches - adsorption and
C           desorption of qual is not considered
            DO 390 L=1,7
              ADQAL(L,I)= 0.0
 390        CONTINUE
          END IF
C
C         find total quantity of material on various forms of sediment
          RSQAL(4, I)= 0.0
          RSQAL(8, I)= 0.0
          RSQAL(12,I)= 0.0
C
          DO 400 J=1,3
            RSQAL(J,  I)= SQAL(J,I)*RSED(J)
            RSQAL(J+4,I)= SQAL(J+3,I)*RSED(J+3)
            RSQAL(J+8,I)= RSQAL(J,I)+ RSQAL(J+4,I)
            RSQAL(4,  I)= RSQAL(4, I)+ RSQAL (J,I)
            RSQAL(8,  I)= RSQAL(8, I)+ RSQAL (J+4,I)
            RSQAL(12, I)= RSQAL(12,I)+ RSQAL (J+8,I)
 400      CONTINUE
C
        ELSE
C         qual constituent not associated with sediment-total just
C         above should have been set to zero by run interpreter
          TIQAL(I)= IDQAL(I)
          TROQAL(I)= RODQAL(I)
          IF (NEXITS .GT. 1) THEN
            DO 410 N= 1, NEXITS
              TOQAL(N,I)= ODQAL(N,I)
 410        CONTINUE
          END IF
        END IF
C
C       find total quantity of qual in rchres
        RDQAL(I)= DQAL(I)*VOLSP
C
        IF(QALFG(7,I) .EQ. 1) THEN
          RRQAL(I)= RDQAL(I) + RSQAL(12,I)
        ELSE
          RRQAL(I)= RDQAL(I)
        END IF
        GQST(I,1)= RRQAL(I)
C
 420  CONTINUE
C
      RETURN
      END
C
C     4.2(3).6.3
C
      SUBROUTINE   ADECAY
     I                    (ADDCPM,TW20,RSED,
     M                     SQAL,
     O                     SQDEC)
C
C     + + + PURPOSE + + +
C     Simulate decay of material in adsorbed state
C
C     + + + DUMMY ARGUMENTS + + +
      REAL       ADDCPM(2),RSED(3),SQAL(3),SQDEC(3),TW20
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ADDCPM - ???
C     TW20   - ???
C     RSED   - ???
C     SQAL   - ???
C     SQDEC  - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I
      REAL       DCONC,DK,FACT
C
C     + + + INTRINSICS + + +
      INTRINSIC  EXP
C
C     + + + END SPECIFICATIONS + + +
C
      IF (ADDCPM(1) .GT. 0.0) THEN
C       calculate temp-adjusted decay rate
        DK  = ADDCPM(1)*ADDCPM(2)**TW20
        FACT= 1.0 - EXP(-DK)
C
        DO 10 I=1,3
C         particle size loop
          IF (SQAL(I) .GT. 1.0E - 30) THEN
            DCONC   = SQAL(I)*FACT
            SQAL(I) = SQAL(I) - DCONC
            SQDEC(I)= DCONC*RSED(I)
          ELSE
            SQDEC(I)= 0.0
          END IF
 10     CONTINUE
C
      ELSE
C
        DO 20 I=1,3
          SQDEC(I)= 0.0
 20     CONTINUE
C
      END IF
C
      RETURN
      END
C
C     4.2(3).6.4
C
      SUBROUTINE   ADSDES
     I                    (VOL,RSED,ADPM,TW20,
     M                    DQAL,SQAL,
     O                    ADQAL)
C
C     + + + PURPOSE + + +
C     Simulate exchange of a constituent between the dissolved
C     state and adsorbed state-note that 6 adsorption site classes are
C     considered: 1- suspended sand  2- susp. silt  3- susp. clay
C     4- bed sand  5- bed silt  6- bed clay
C
C     + + + DUMMY ARGUMENTS + + +
      REAL       ADPM(6,3),ADQAL(7),DQAL,RSED(6),SQAL(6),TW20,VOL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     VOL    - volume of water in reach above bed
C     RSED   - ???
C     ADPM   - ???
C     TW20   - ???
C     DQAL   - ???
C     SQAL   - ???
C     ADQAL  - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    J
      REAL       AINV(6),AKJ,CAINV(6),DENOM,NUM,TEMP
C
C     + + + END SPECIFICATIONS + + +
C
      IF (VOL .GT. 0.0) THEN
C       adsorption/desorption can take place
C
C       first find the new dissolved conc.
        NUM  = VOL*DQAL
        DENOM= VOL
C
        DO 10 J=1,6
C
          IF (RSED(J) .GT. 0.0) THEN
C           this sediment class is present-evaluate terms due to it
C
C           transfer rate, corrected for water temp
            AKJ = ADPM(J,2)*ADPM(J,3)**TW20
            TEMP= 1.0/(1.0 + AKJ)
C
C           calculate 1/a and c/a
            AINV(J) = AKJ*ADPM(J,1)*TEMP
            CAINV(J)= SQAL(J)*TEMP
C
C           accumulate terms for numerator and denominator
C           in dqal equation
            NUM  = NUM + (SQAL(J) - CAINV(J))*RSED(J)
            DENOM= DENOM + RSED(J)*AINV(J)
          END IF
C
 10     CONTINUE
C
C       calculate new dissolved concentration-units are conc/l
        DQAL= NUM/DENOM
C
C       calculate new conc on each sed class and the corresponding
C       adsorption/desorption flux
        ADQAL(7)= 0.0
C
        DO 20 J=1,6
C
          IF (RSED(J) .GT. 0.0) THEN
C           this sediment class is present-calculate data
C           pertaining to it
C
C           new concentration
            TEMP= CAINV(J) + DQAL*AINV(J)
C
C           quantity of material transferred
            ADQAL(J)= (TEMP - SQAL(J))*RSED(J)
            SQAL(J) = TEMP
C
C           accumulate total adsorption/desorption flux
            ADQAL(7)= ADQAL(7) + ADQAL(J)
C
          ELSE
C           this sediment class is absent
            ADQAL(J)= 0.0
C           sqal(j) is unchanged-"undefined"
          END IF
 20     CONTINUE
C
      ELSE
C       no water, no adsorption/desorption
C
        DO 30 J=1,7
          ADQAL(J)= 0.0
C         sqal(1 thru 3) and dqal should already have been set to
C         undefined values
 30     CONTINUE
C
      END IF
C
      RETURN
      END
C
C     4.2(3).6.2
C
      SUBROUTINE   ADVQAL
     I                    (ISQAL,RSED,BSED,DEPSCR,ROSED,OSED,NEXITS,
     I                     RCHNO,MESSU,MSGFL,DATIM,
     I                     GQID,J,RSQALS,RBQALS,
     M                     ECNT,
     O                     SQAL,BQAL,DSQAL,ROSQAL,OSQAL)
C
C     + + + PURPOSE + + +
C     Simulate the advective processes, including deposition and
C     scour for the quality constituent attached to one sediment size
C     fraction
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    ECNT(2),MSGFL,J,MESSU,NEXITS,RCHNO,DATIM(5)
      REAL       BSED,BQAL,DEPSCR,DSQAL,GQID(5),ISQAL,OSED(5),OSQAL(5),
     $           RBQALS,ROSED,ROSQAL,RSED,RSQALS,SQAL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ISQAL  - ???
C     RSED   - ???
C     BSED   - ???
C     DEPSCR - ???
C     ROSED  - ???
C     OSED   - ???
C     NEXITS - number of exits from the operation
C     RCHNO  - ???
C     MESSU  - ftn unit no. to be used for printout of messages
C     MSGFL  - fortran unit number of HSPF message file
C     GQID   - ???
C     J      - ???
C     RSQALS - ???
C     RBQALS - ???
C     ECNT   - ???
C     SQAL   - ???
C     BQAL   - ???
C     DSQAL  - ???
C     ROSQAL - ???
C     OSQAL  - ???
C     DATIM  - date and time of day
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    L,N,I20,SCLU,SGRP
      REAL       DENOM,RBQAL
      CHARACTER*20 CHSTR
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (CHSTR,CHSTR1)
      CHARACTER*1  CHSTR1(20)
C
C     + + + INTRINSICS + + +
      INTRINSIC  ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL   OMSTD,OMSTI,OMSTC,OMSG,OMSTR
C
C     + + + OUTPUT FORMATS + + +
 2070 FORMAT (5A4)
C
C     + + + HISTORY + + +
C     4/2007   BRB corrected mass balance errors caused by error in scour calculations
C
C     + + + END SPECIFICATIONS + + +
C
      I20  = 20
      SCLU = 346
      IF (DEPSCR .LT. 0.0) THEN
C       there was scour during the interval
C
        IF ((ABS(BSED)) .LE. 0.0) THEN
C         bed was scoured "clean"
          BQAL = -1.0E30
Cbrb      changed sign of DSQAL; it should be negative for scour; fixed 4/2007 
          DSQAL= -1.0*RBQALS
        ELSE
C         there is still bed material left
          BQAL = RBQALS/(BSED - DEPSCR)
          DSQAL= BQAL*DEPSCR
        END IF
C
C       calculate concentration in suspension-under these conditions,
C       denominator should never be zero
        SQAL  = (ISQAL + RSQALS - DSQAL)/(RSED + ROSED)
        ROSQAL= ROSED*SQAL
C
      ELSE
C       there was deposition or no scour/deposition
C       during the interval
        DENOM= RSED + DEPSCR + ROSED
C
        IF ((ABS(DENOM)) .LE. 0.0) THEN
C         there was no sediment in suspension during the interval
          SQAL  = -1.0E30
          ROSQAL= 0.0
          DSQAL = 0.0
C
          IF ((ABS(ISQAL)) .GT. 0.0 .OR.
     $        (ABS(RSQALS)) .GT. 0.0) THEN
C           error-under these conditions these values should be zero
            CALL OMSTD (DATIM)
            CALL OMSTI (RCHNO)
            CALL OMSTR (ISQAL)
            CALL OMSTR (RSQALS)
            WRITE (CHSTR,2070) GQID
            CALL OMSTC (I20,CHSTR1)
            CALL OMSTI (J)
            SGRP = 1
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 ECNT(1))
          END IF
C
        ELSE
C         there was some suspended sediment during the interval
C
C         calculate conc on suspended sed
          SQAL  = (ISQAL + RSQALS)/DENOM
          ROSQAL= ROSED*SQAL
          DSQAL = DEPSCR*SQAL
C
          IF ((ABS(RSED)) .LE. 0.0) THEN
C           rchres ended up without any suspended sediment-revise
C           value for sqal, but values obtained for rsqal,
C           rosqal, and dsqal are still ok
            SQAL= -1.0E30
          END IF
C
        END IF
C
C       calculate conditions on the bed
C
        IF ((ABS(BSED)) .LE. 0.0) THEN
C         no bed sediments at end of interval
          BQAL= -1.0E30
C
          IF ((ABS(DSQAL)) .GT. 0.0 .OR.
     $        (ABS(RBQALS)) .GT. 0.0) THEN
C           error-under this condition these values should be zero
            CALL OMSTD (DATIM)
            CALL OMSTI (RCHNO)
            CALL OMSTR (DSQAL)
            CALL OMSTR (RBQALS)
            WRITE (CHSTR,2070) GQID
            CALL OMSTC (I20,CHSTR1)
            CALL OMSTI (J)
            SGRP = 2
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 ECNT(2))
          END IF
C
        ELSE
C         there is bed sediment at the end of the interval
          RBQAL= DSQAL + RBQALS
          BQAL = RBQAL/BSED
        END IF
C
      END IF
C
      IF (NEXITS .GT. 1) THEN
C       we need to compute outflow through each individual exit
C
        IF ((ABS(ROSED)) .LE. 0.0) THEN
C         all zero
          DO 10 L=1,5
            OSQAL(L)=0.0
 10       CONTINUE
        ELSE
          DO 20 N= 1, NEXITS
            OSQAL(N)= ROSQAL*OSED(N)/ROSED
 20       CONTINUE
        END IF
C
      END IF
C
      RETURN
      END
C
C     4.2(3).6.1
C
      SUBROUTINE   DDECAY
     I                    (QALFG,TW20,HYDPM,PHVAL,ROXPM,ROC,
     I                     FACT2,FACT1,PHOTPM,KOREA,CFGAS,BIOPM,
     I                     BIO,GENPM,VOLSP,DQAL,HR,DELT60,
     O                     DDQAL)
C
C     + + + PURPOSE + + +
C     Estimate decay of dissolved constituent
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    HR,QALFG(6)
      REAL       BIO,BIOPM(2),CFGAS,DDQAL(7),DELT60,DQAL,FACT1,
     $           FACT2(18),GENPM(2),HYDPM(4),KOREA,PHOTPM(20),PHVAL,
     $           ROC,ROXPM(2),TW20,VOLSP
C
C     + + + ARGUMENT DEFINITIONS + + +
C     QALFG  - ???
C     TW20   - ???
C     HYDPM  - ???
C     PHVAL  - ???
C     ROXPM  - ???
C     ROC    - ???
C     FACT2  - ???
C     FACT1  - ???
C     PHOTPM - ???
C     KOREA  - ???
C     CFGAS  - ???
C     BIOPM  - ???
C     BIO    - ???
C     GENPM  - ???
C     VOLSP  - ???
C     DQAL   - ???
C     HR     - ???
C     DELT60 - simulation time interval in hours
C     DDQAL  - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,L
      REAL       FACT3,K(7),KHYD,KROX
C
C     + + + INTRINSICS + + +
      INTRINSIC  EXP
C
C     + + + END SPECIFICATIONS + + +
C
      IF (DQAL .GT. 1.0E - 25) THEN
C       simulate decay
C
        IF (QALFG(1) .EQ. 1) THEN
C         simulate hydrolysis
          KHYD= HYDPM(1)*10.0**(-PHVAL) + HYDPM(2)*10.0
     $          **(PHVAL - 14.0) + HYDPM(3)
C         adjust for temperature
          K(1)= KHYD*HYDPM(4)**TW20
        ELSE
          K(1)= 0.0
        END IF
C
        IF (QALFG(2) .EQ. 1) THEN
C         simulate oxidation by free radical processes
          KROX= ROXPM(1)*ROC
C         adjust for temperature
          K(2)= KROX*ROXPM(2)**TW20
        ELSE
          K(2)= 0.0
        END IF
C
        IF (QALFG(3) .EQ. 1) THEN
C         simulate photolysis
C         go through summation over 18 wave-length intervals
          FACT3= 0.0
C
          DO 10 L=1,18
            FACT3= FACT3 + FACT2(L)*PHOTPM(L)
 10       CONTINUE
C
          K(3)=FACT1*PHOTPM(19)*FACT3*PHOTPM(20)**TW20
        ELSE
          K(3)= 0.0
        END IF
C
        IF (DELT60 .LT. 24.) THEN
          IF (HR .GE. 6 .AND. HR .LT. 18) THEN
C           it is a daylight hour; photolysis rate is doubled
C           for this interval
            K(3) = 2.0*K(3)
          ELSE
C           it is not a daylight hour; photolysis does not occur
            K(3) = 0.0
          END IF
        ELSE
C         simulation interval is greater than 24 hours;
C         no correction is made to photolysis rate to
C         represent diurnal fluctuation
        END IF
C
        IF (QALFG(4) .EQ. 1) THEN
C         simulate volatilization
          K(4)= KOREA*CFGAS
        ELSE
          K(4)= 0.0
        END IF
C
        IF (QALFG(5) .EQ. 1) THEN
C         simulate biodegradation
          K(5)= BIOPM(1)*BIO*BIOPM(2)**TW20
        ELSE
          K(5)= 0.0
        END IF
C
        IF (QALFG(6) .EQ. 1) THEN
C         simulate simple first-order decay
          K(6)= GENPM(1)*GENPM(2)**TW20
        ELSE
          K(6)= 0.0
        END IF
C
C       get total decay rate
        K(7)= 0.0
C
        DO 20 I=1,6
          K(7)= K(7) + K(I)
 20     CONTINUE
C
C       calculate the total change in material due to decay-units are
C       conc*vol/l.ivl
        DDQAL(7)= DQAL*(1.0 - EXP(-K(7)))*VOLSP
C
C       prorate among the individual decay processes- the method used
C       for proration is linear, which is not strictly correct, but
C       should be a good approximation under most conditions
C
        DO 30 I=1,6
          IF (K(7) .GT. 0.0) THEN
            DDQAL (I)= K(I)/K(7)*DDQAL(7)
          ELSE
            DDQAL(I) = 0.0
          END IF
 30     CONTINUE
C
      ELSE
C       too little dissolved material to simulate decay
        DO 40 I=1,7
          DDQAL(I)=0.0
 40     CONTINUE
      END IF
C
      RETURN
      END
C
C     4.2(3).10.1.5
C
      SUBROUTINE   GQACC
     I                   (FRM,TO)
C
C     + + + PURPOSE + + +
C     Accumulate fluxes in module section gqual for printout
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    FRM,TO
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FRM    - ???
C     TO     - ???
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION GQUAL2 + + +
      INCLUDE    'crhgq.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,I1,I4,I7,L
C
C     + + + EXTERNALS + + +
      EXTERNAL  ACCVEC
C
C     + + + END SPECIFICATIONS + + +
C
      I1=1
      I4=4
      I7=7
        DO 20 L=1,NGQUAL
C         handle flux groups dealing with rchres-wide variables
C
          CALL ACCVEC
     I                (I1,GQIF1(L,FRM),
     M                 GQIF1(L,TO))
C
          CALL ACCVEC
     I                (I1,GQCF1(L,FRM),
     M                 GQCF1(L,TO))
C
          CALL ACCVEC
     I                (I7,GQCF2(1,L,FRM),
     M                 GQCF2(1,L,TO))
C
          CALL ACCVEC
     I                (I1,GQCF3(L,FRM),
     M                 GQCF3(L,TO))
C
          CALL ACCVEC
     I                (I1,GQCF10(L,FRM),
     M                 GQCF10(L,TO))
C
          CALL ACCVEC
     I                (I1,GQCF11(L,FRM),
     M                 GQCF11(L,TO))
C
          IF (QALFG(7,L) .EQ. 1) THEN
C           qual is associated with sediment- accumulate
C           additional fluxes
C
            CALL ACCVEC
     I                  (I4,GQCF4(1,L,FRM),
     M                   GQCF4(1,L,TO))
C
            CALL ACCVEC
     I                  (I4,GQCF5(1,L,FRM),
     M                   GQCF5(1,L,TO))
C
            CALL ACCVEC
     I                  (I7,GQCF6(1,L,FRM),
     M                   GQCF6(1,L,TO))
C
            CALL ACCVEC
     I                  (I7,GQCF7(1,L,FRM),
     M                   GQCF7(1,L,TO))
C
            CALL ACCVEC
     I                  (I4,GQIF2(1,L,FRM),
     M                   GQIF2(1,L,TO))
C
          END IF
C
          IF (NEXITS .GT. 1) THEN
C           handle flux groups dealing with individual exits
C
            CALL ACCVEC
     I                  (NEXITS,GQCF8(1,L,FRM),
     M                   GQCF8(1,L,TO))
C
            IF (QALFG(7,L) .EQ. 1) THEN
              DO 10 I= 1, 3
                CALL ACCVEC
     I                      (NEXITS,GQCF9(1,I,L,FRM),
     M                       GQCF9(1,I,L,TO))
 10           CONTINUE
            END IF
          END IF
 20     CONTINUE
C
      RETURN
      END
C
C
C     4.2(3).10.2.5
C
      SUBROUTINE   GQPRT
     I                   (LEV,PRINTU,UNITFG,BINU)
C
C     + + + PURPOSE + + +
C     Convert quantities from internal to external units, calculate
C     materials balance, and print out results
C     Note:  local arrays have same dimensions as corresponding arraysc
C       in osv, except for dropping of dimension lev, where applicable
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    LEV,PRINTU,UNITFG,BINU
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEV    - current output level (2-pivl,3-day,4-mon,5-ann)
C     PRINTU - fortran unit number on which to print output
C     UNITFG - output units   1-english, 2-metric
C     BINU   - fortran unit number on which to write binary output
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION GQUAL2 + + +
      INCLUDE    'crhgq.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,IX,I0,I1,I2,I3,I4,I7,I12,J,K,N,PADFG,JLEN,ACNT,
     $            CLEN((74+(5*NEXITS))*3),EXDAT(5)
      REAL        FACTA,GQDIF,GQIN,PGQCF1,PGQCF2(7),PGQCF3,PGQCF4(4),
     $            PGQCF5(4),PGQCF6(7),PGQCF7(7),PGQCF8(5),PGQCF9(5,4),
     $            PGQIF1,PGQIF2(4),PGQST,PGQSTS,PRDQAL,PRSQAL(12),
     $            RZERO,TOTIN,TOTOUT,TOTSTO,PGCF10,PGCF11,PADTOT,
     $            APRINT((74+(5*NEXITS))*3)
      CHARACTER*16 UNITID,CSQAL(6),CRSQAL(12),CGQIF2(4),CGQCF2(7),
     $             CGQCF4(4),CGQCF5(4),CGQCF6(7),CGQCF7(7),CGQCF9(4)
      CHARACTER*1   CSTR(2)
      CHARACTER*20  GQNAME
      CHARACTER*256 CHEAD((74+(5*NEXITS))*3)
C
C     + + + EXTERNALS + + +
      EXTERNAL   TRNVEC,BALCHK,INTCHR,EXDATE
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' *** GQUAL ***')
 2010 FORMAT (/,'   GENERALIZED QUALITY',20X,5A4)
 2015 FORMAT (5A4)
 2020 FORMAT (' ','  CONSTITUENT NO.',I3)
 2030 FORMAT (/,'   STATE VARIABLES')
 2040 FORMAT (' ','    DISSOLVED CONCENTRATION',2X,'(',A4,'/L)',3X,
     $        1PE10.3)
 2050 FORMAT (/,'     CONCENTRATION ASSOCIATED',12X,'SUSP. SAND  ',
     $        'SUSP. SILT  SUSP. CLAY    BED SAND    BED SILT    ',
     $        'BED CLAY')
 2060 FORMAT ('     WITH SEDIMENT',2X,'(',A4,'/MG)',12X,1PE10.3,2X,
     $        1PE10.3,2X,1PE10.3,2X,1PE10.3,2X,1PE10.3,2X,1PE10.3)
 2070 FORMAT (/,'     DISSOLVED STORAGE',2X,'(',2A4,')',7X,1PE10.3)
 2080 FORMAT (/,'     STORAGE ON SEDIMENT',2X,'(',2A4,')',8X,'ON SAND',
     $        '     ON SILT     ON CLAY       TOTAL')
 2090 FORMAT (' ',6X,'SUSPENDED',25X,3(1PE10.3,2X),1PE10.3/
     $        ' ',6X,'BED',31X,3(1PE10.3,2X),1PE10.3/
     $        ' ',6X,'TOTAL',29X,3(1PE10.3,2X),1PE10.3)
 2100 FORMAT (/,'     TOTAL STORAGE',2X,'(',2A4,')',11X,1PE10.3)
 2110 FORMAT (/,'   FLUXES',2X,'(',2A4,')')
 2120 FORMAT ('     INFLOW OF DISSOLVED QUAL',12X,1PE10.3)
 2130 FORMAT ('     INFLOW OF DISSOLVED QUAL',2X,'<---ATMOSPHERIC',
     #        ' DEPOSITION--->     OTHER')
 2140 FORMAT (' ',37X,'DRY       WET     TOTAL    INFLOW')
 2150 FORMAT (' ',30X,'    GQADDR    GQADWT   ATM DEP     IDQAL')
 2160 FORMAT (' ',30X,4(1PE10.3))
 2170 FORMAT (/,'     INFLOW OF QUAL ON SEDIMENT',10X,'   ON SAND',
     $        '     ON SILT     ON CLAY       TOTAL')
 2180 FORMAT (' ',38X,4(2X,1PE10.3))
 2190 FORMAT (/,'     TOTAL INFLOW',24X,1PE10.3)
 2200 FORMAT (/,'     INPUT FROM DECAY OF PARENT COMPOUNDS',1PE10.3)
 2210 FORMAT (/,'     DECAY OF DISSOLVED QUAL',13X,'HYDROLYSIS   ',
     $        'OXIDATION  PHOTOLYSIS  VOLATILIZ.   BIODEGRD.  ',
     $        'GEN. DECAY       TOTAL')
 2220 FORMAT (' ',40X,1PE10.3,2X,1PE10.3,2X,1PE10.3,2X,1PE10.3,2X,
     $        1PE10.3,2X,1PE10.3,2X,1PE10.3)
 2230 FORMAT (/,'     DECAY OF QUAL ON SEDIMENT',11X,'SUSP. SAND  ',
     $        'SUSP. SILT  SUSP. CLAY    BED SAND    BED SILT    ',
     $        'BED CLAY       TOTAL')
 2240 FORMAT (' ',40X,1PE10.3,2X,1PE10.3,2X,1PE10.3,2X,1PE10.3,2X,
     $        1PE10.3,2X,1PE10.3,2X,1PE10.3)
 2250 FORMAT (/,'     ADSORPTION/DESORPTION',15X,'SUSP. SAND  ',
     $        'SUSP. SILT  SUSP. CLAY    BED SAND    BED SILT    ',
     $        'BED CLAY       TOTAL')
 2260 FORMAT (' ',6X,'(POSITIVE INDICATES ADSORPTION)',3X,1PE10.3,
     $        2X,1PE10.3,2X,1PE10.3,2X,1PE10.3,2X,1PE10.3,2X,
     $        1PE10.3,2X,1PE10.3)
 2270 FORMAT (/,'     DEPOSITION/SCOUR',20X,'   ON SAND     ',
     $        'ON SILT     ON CLAY       TOTAL')
 2280 FORMAT (' ',6X,'(POSITIVE INDICATES DEPOSITION)',3X,1PE10.3,
     $        2X,1PE10.3,2X,1PE10.3,2X,1PE10.3)
 2290 FORMAT (/,'     TOTAL OUTFLOW OF DISSOLVED QUAL',5X,1PE10.3)
 2300 FORMAT (/,'     TOTAL OUTFLOW OF QUAL ON SEDIMENT',3X,
     $        '   ON SAND     ON SILT     ON CLAY       TOTAL')
 2310 FORMAT (' ',40X,1PE10.3,2X,1PE10.3,2X,1PE10.3,2X,1PE10.3)
 2320 FORMAT (/,'     TOTAL OUTFLOW',23X,1PE10.3)
 2330 FORMAT (/,'     DISSOLVED OUTFLOW FOR EACH EXIT')
 2340 FORMAT (' ',6X,'EXIT',I3,27X,1PE10.3)
 2350 FORMAT (/,'     OUTFLOW ON SEDIMENT FOR EACH EXIT',3X,
     $        '   ON SAND     ON SILT     ON CLAY       TOTAL')
 2360 FORMAT (' ',6X,'EXIT',I3,25X,4(2X,1PE10.3))
 2370 FORMAT (2A4)
C
C     + + + END SPECIFICATIONS + + +
C
      I0   = 0
      I1   = 1
      I2   = 2
      I3   = 3
      I4   = 4
      I7   = 7
      I12  = 12
      RZERO=0.0
C
C     initialize array counter for binary printout
      ACNT = 0
C     store variable
C     names in local strings for use in building binary headers
      CSQAL(1)   = '-SQAL-SUSPSAND'
      CSQAL(2)   = '-SQAL-SUSPSILT'
      CSQAL(3)   = '-SQAL-SUSPCLAY'
      CSQAL(4)   = '-SQAL-BEDSAND'
      CSQAL(5)   = '-SQAL-BEDSILT'
      CSQAL(6)   = '-SQAL-BEDCLAY'
      CRSQAL(1)  = '-RSQAL-SUSPSAND'
      CRSQAL(2)  = '-RSQAL-SUSPSILT'
      CRSQAL(3)  = '-RSQAL-SUSPCLAY'
      CRSQAL(4)  = '-RSQAL-SUSPTOT'
      CRSQAL(5)  = '-RSQAL-BEDSAND'
      CRSQAL(6)  = '-RSQAL-BEDSILT'
      CRSQAL(7)  = '-RSQAL-BEDCLAY'
      CRSQAL(8)  = '-RSQAL-BEDTOT'
      CRSQAL(9)  = '-RSQAL-TOTSAND'
      CRSQAL(10) = '-RSQAL-TOTSILT'
      CRSQAL(11) = '-RSQAL-TOTCLAY'
      CRSQAL(12) = '-RSQAL-TOTTOT'
      CGQIF2(1)  = '-ISQAL-SAND'
      CGQIF2(2)  = '-ISQAL-SILT'
      CGQIF2(3)  = '-ISQAL-CLAY'
      CGQIF2(4)  = '-ISQAL-TOT'
      CGQCF2(1)  = '-DDQAL-HYDROL'
      CGQCF2(2)  = '-DDQAL-OXID'
      CGQCF2(3)  = '-DDQAL-PHOTOL'
      CGQCF2(4)  = '-DDQAL-VOLAT'
      CGQCF2(5)  = '-DDQAL-BIODEG'
      CGQCF2(6)  = '-DDQAL-GEN'
      CGQCF2(7)  = '-DDQAL-TOT'
      CGQCF6(1)  = '-SQDEC-SUSPSAND'
      CGQCF6(2)  = '-SQDEC-SUSPSILT'
      CGQCF6(3)  = '-SQDEC-SUSPCLAY'
      CGQCF6(4)  = '-SQDEC-BEDSAND'
      CGQCF6(5)  = '-SQDEC-BEDSILT'
      CGQCF6(6)  = '-SQDEC-BEDCLAY'
      CGQCF6(7)  = '-SQDEC-BEDTOT'
      CGQCF7(1)  = '-ADQAL-SUSPSAND'
      CGQCF7(2)  = '-ADQAL-SUSPSILT'
      CGQCF7(3)  = '-ADQAL-SUSPCLAY'
      CGQCF7(4)  = '-ADQAL-BEDSAND'
      CGQCF7(5)  = '-ADQAL-BEDSILT'
      CGQCF7(6)  = '-ADQAL-BEDCLAY'
      CGQCF7(7)  = '-ADQAL-TOT'
      CGQCF4(1)  = '-DSQAL-SAND'
      CGQCF4(2)  = '-DSQAL-SILT'
      CGQCF4(3)  = '-DSQAL-CLAY'
      CGQCF4(4)  = '-DSQAL-TOT'
      CGQCF5(1)  = '-ROSQAL-SAND'
      CGQCF5(2)  = '-ROSQAL-SILT'
      CGQCF5(3)  = '-ROSQAL-CLAY'
      CGQCF5(4)  = '-ROSQAL-TOT'
      CGQCF9(1)  = '-OSQAL-SAND-'
      CGQCF9(2)  = '-OSQAL-SILT-'
      CGQCF9(3)  = '-OSQAL-CLAY-'
      CGQCF9(4)  = '-OSQAL-TOT-'
C
C     Zero variables which may be used in balances and totals
      PADTOT     = 0.0
      PGQIF1     = 0.0
      PGQIF2(1)  = 0.0
      PGQIF2(2)  = 0.0
      PGQIF2(3)  = 0.0
      PGQIF2(4)  = 0.0
      PGQCF1     = 0.0
      PGQCF2(7)  = 0.0
      PGQCF6(7)  = 0.0
      PGQCF3     = 0.0
      PGQCF5(4)  = 0.0
      PGQCF3     = 0.0
      PRSQAL(12) = 0.0
C
C
      IF (PRINTU .GT. 0 .AND. PFLAG(6) .LE. LEV) THEN
        WRITE (PRINTU,2000)
      END IF
C
      DO 200 I= 1,NGQUAL
        WRITE (GQNAME, 2015) (GQID(J,I), J= 1,5)
        GQNAME = ADJUSTL(GQNAME)
C
C       convert variables to external units
C
        FACTA = CINV(I)
C
C       rchres-wide variables
C
        PGQIF1= GQIF1(I,LEV)*FACTA
C
        IF (QALFG(7,I) .EQ. 1) THEN
C
          CALL TRNVEC
     I                (I4,GQIF2(1,I,LEV),FACTA,RZERO,
     O                 PGQIF2(1))
C
        ELSE
          DO 10 J= 1, 4
            PGQIF2(J) = 0.0
 10       CONTINUE
        END IF
C
C       storages
C
        PRDQAL= RDQAL(I)*FACTA
C
        IF (QALFG(7,I) .EQ. 1) THEN
C
          CALL TRNVEC
     I                (I12,RSQAL(1,I),FACTA,RZERO,
     O                 PRSQAL(1))
C
        ELSE
          DO 20 J= 1,12
            PRSQAL(J) = 0.0
 20       CONTINUE
        END IF
C
        PGQST = GQST(I,1)*FACTA
        PGQSTS= GQST(I,LEV)*FACTA
C
C       computed fluxes
C
        IF (DAUGFG(I) .EQ. 1) THEN
          PGQCF1 = GQCF1(I,LEV)*FACTA
        ELSE
          PGQCF1 = 0.0
        END IF
C
        IF (QALGFG(I) .EQ. 1) THEN
C
          CALL TRNVEC
     I                (I7,GQCF2(1,I,LEV),FACTA,RZERO,
     O                 PGQCF2(1))
C
        ELSE
          DO 30 J= 1,7
            PGQCF2(J) = 0.0
 30       CONTINUE
        END IF
C
        PGQCF3= GQCF3(I,LEV)*FACTA
C
        IF (QALFG(7,I) .EQ. 1) THEN
C
          CALL TRNVEC
     I                (I4,GQCF4(1,I,LEV),FACTA,RZERO,
     O                 PGQCF4(1))
C
          CALL TRNVEC
     I                (I4,GQCF5(1,I,LEV),FACTA,RZERO,
     O                 PGQCF5(1))
C
          CALL TRNVEC
     I                (I7,GQCF6(1,I,LEV),FACTA,RZERO,
     O                 PGQCF6(1))
C
          CALL TRNVEC
     I                (I7,GQCF7(1,I,LEV),FACTA,RZERO,
     O                 PGQCF7(1))
C
        ELSE
          DO 40 J= 1,4
            PGQCF4(J) = 0.0
            PGQCF5(J) = 0.0
 40       CONTINUE
          DO 50 J= 1,7
            PGQCF6(J) = 0.0
            PGQCF7(J) = 0.0
 50       CONTINUE
        END IF
C
        IF (NEXITS .GT. 1) THEN
C
C         exit-specific variables
C
          CALL TRNVEC
     I                (NEXITS,GQCF8(1,I,LEV),FACTA,RZERO,
     O                 PGQCF8(1))
C
          IF (QALFG(7,I) .EQ. 1) THEN
C           qual is sediment associated
            DO 60 J= 1, 3
              CALL TRNVEC
     I                    (NEXITS,GQCF9(1,J,I,LEV),FACTA,RZERO,
     O                     PGQCF9(1,J))
 60         CONTINUE
            DO 65 N= 1, NEXITS
              PGQCF9(N,4)= PGQCF9(N,1)+ PGQCF9(N,2)+ PGQCF9(N,3)
 65         CONTINUE
          ELSE
C           zero sediment fluxes
            DO 80 J= 1,5
              DO 70 K= 1,4
                PGQCF9(J,K) = 0.0
 70           CONTINUE
 80         CONTINUE
          END IF
        END IF
C
C       do printout on printu
C
        IF (PRINTU .GT. 0 .AND. PFLAG(6) .LE. LEV) THEN
          WRITE (PRINTU,2010) (GQID(J,I), J= 1,5)
          WRITE (PRINTU,2020) I
          WRITE (PRINTU,2030)
          WRITE (PRINTU,2040) CONCID(I),DQAL(I)
C
          IF (QALFG(7,I) .EQ. 1) THEN
            WRITE (PRINTU,2050)
            WRITE (PRINTU,2060) CONCID(I),(SQAL(J,I), J= 1,6)
          END IF
C
          WRITE (PRINTU,2070) (QTYID(J,I), J= 1,2),PRDQAL
C
          IF (QALFG(7,I) .EQ. 1) THEN
            WRITE (PRINTU,2080) (QTYID(J,I), J= 1,2)
            WRITE (PRINTU,2090) PRSQAL
          END IF
        END IF
        IF (BINU .GT. 0 .AND. ABS(BFLAG(6)) .LE. LEV) THEN
          ACNT = ACNT + 1
          APRINT(ACNT) = DQAL(I)
          CHEAD(ACNT) = TRIM(GQNAME) // '-DQAL'
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          IF (QALFG(7,I) .EQ. 1) THEN
            DO 81 J= 1, 6
              ACNT = ACNT + 1
              APRINT(ACNT) = SQAL(J,I)
              CHEAD(ACNT) = TRIM(GQNAME) // CSQAL(J)
              CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 81         CONTINUE
          END IF
          ACNT = ACNT + 1
          APRINT(ACNT) = PRDQAL
          CHEAD(ACNT) = TRIM(GQNAME) // '-RDQAL'
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          IF (QALFG(7,I) .EQ. 1) THEN
            DO 82 J= 1, 12
              ACNT = ACNT + 1
              APRINT(ACNT) = PRSQAL(J)
              CHEAD(ACNT) = TRIM(GQNAME) // CRSQAL(J)
              CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 82         CONTINUE
          END IF
        END IF
C
C       calculate and print total storage of qual in rchres
C
        TOTSTO= PRDQAL + PRSQAL(12)
        IF (PRINTU .GT. 0 .AND. PFLAG(6) .LE. LEV) THEN
          WRITE (PRINTU,2100) (QTYID(J,I), J= 1,2),TOTSTO
C
C         calculate and print input fluxes of qual
          WRITE (PRINTU,2110) (QTYID(J,I), J= 1,2)
        END IF
        IF (BINU .GT. 0 .AND. ABS(BFLAG(6)) .LE. LEV) THEN
          ACNT = ACNT + 1
          APRINT(ACNT) = TOTSTO
          CHEAD(ACNT) = TRIM(GQNAME) // '-RRQAL'
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
        END IF
C
        PADFG= 0
        J= (I-1)*2+ 1
        IF ( (GQADFG(J) .NE. 0) .OR. (GQADFG(J+1) .NE. 0) ) THEN
          PADFG= 1
        END IF
C
        IF (PADFG .EQ. 0) THEN
          IF (PRINTU .GT. 0 .AND. PFLAG(6) .LE. LEV) THEN
            WRITE (PRINTU,2120) PGQIF1
          END IF
          IF (BINU .GT. 0 .AND. ABS(BFLAG(6)) .LE. LEV) THEN
            ACNT = ACNT + 1
            APRINT(ACNT) = PGQIF1
            CHEAD(ACNT) = TRIM(GQNAME) // '-IDQAL'
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          END IF
        ELSE
          IF (GQADFG(J) .NE. 0) THEN
            PGCF10= GQCF10(I,LEV)*FACTA
          ELSE
            PGCF10= 0.0
          END IF
          IF (GQADFG(J+1) .NE. 0) THEN
            PGCF11= GQCF11(I,LEV)*FACTA
          ELSE
            PGCF11= 0.0
          END IF
          PADTOT= PGCF10+ PGCF11
C
          IF (PRINTU .GT. 0 .AND. PFLAG(6) .LE. LEV) THEN
            WRITE (PRINTU,2130)
            WRITE (PRINTU,2140)
            WRITE (PRINTU,2150)
            WRITE (PRINTU,2160) PGCF10, PGCF11, PADTOT, PGQIF1
          END IF
          IF (BINU .GT. 0 .AND. ABS(BFLAG(6)) .LE. LEV) THEN
            ACNT = ACNT + 1
            APRINT(ACNT) = PGCF10
            CHEAD(ACNT) = TRIM(GQNAME) // '-GQADDR'
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
            ACNT = ACNT + 1
            APRINT(ACNT) = PGCF11
            CHEAD(ACNT) = TRIM(GQNAME) // '-GQADWT'
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
            ACNT = ACNT + 1
            APRINT(ACNT) = PADTOT
            CHEAD(ACNT) = TRIM(GQNAME) // '-GQADEP'
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
            ACNT = ACNT + 1
            APRINT(ACNT) = PGQIF1
            CHEAD(ACNT) = TRIM(GQNAME) // '-IDQAL'
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          END IF
        END IF
C
        IF (QALFG(7,I) .EQ. 1) THEN
          IF (PRINTU .GT. 0 .AND. PFLAG(6) .LE. LEV) THEN
            WRITE (PRINTU,2170)
            WRITE (PRINTU,2180) PGQIF2
          END IF
          IF (BINU .GT. 0 .AND. ABS(BFLAG(6)) .LE. LEV) THEN
            DO 83 J= 1, 4
              ACNT = ACNT + 1
              APRINT(ACNT) = PGQIF2(J)
              CHEAD(ACNT) = TRIM(GQNAME) // CGQIF2(J)
              CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 83         CONTINUE
          END IF
        END IF
C
C       calculate and print total inflow of qual
C
        TOTIN= PGQIF1+ PGQIF2(4)+ PADTOT
        IF (PRINTU .GT. 0 .AND. PFLAG(6) .LE. LEV) THEN
          WRITE (PRINTU,2190) TOTIN
C
          IF (DAUGFG(I) .EQ. 1) THEN
            WRITE (PRINTU,2200) PGQCF1
          END IF
C
          IF (QALGFG(I) .EQ. 1) THEN
            WRITE (PRINTU,2210)
            WRITE (PRINTU,2220) PGQCF2
          END IF
C
          IF (QALFG(7,I) .EQ. 1) THEN
            WRITE (PRINTU,2230)
            WRITE (PRINTU,2240) PGQCF6
            WRITE (PRINTU,2250)
            WRITE (PRINTU,2260) PGQCF7
            WRITE (PRINTU,2270)
            WRITE (PRINTU,2280) PGQCF4
          END IF
C
          WRITE (PRINTU,2290) PGQCF3
C
          IF (QALFG(7,I) .EQ. 1) THEN
            WRITE (PRINTU,2300)
            WRITE (PRINTU,2310) PGQCF5
          END IF
        END IF
        IF (BINU .GT. 0 .AND. ABS(BFLAG(6)) .LE. LEV) THEN
          ACNT = ACNT + 1
          APRINT(ACNT) = TOTIN
          CHEAD(ACNT) = TRIM(GQNAME) // '-TIQAL'
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          IF (DAUGFG(I) .EQ. 1) THEN
            ACNT = ACNT + 1
            APRINT(ACNT) = PGQCF1
            CHEAD(ACNT) = TRIM(GQNAME) // '-PDQAL'
            CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          END IF
          IF (QALGFG(I) .EQ. 1) THEN
            DO 84 J= 1, 7
              ACNT = ACNT + 1
              APRINT(ACNT) = PGQCF2(J)
              CHEAD(ACNT) = TRIM(GQNAME) // CGQCF2(J)
              CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 84         CONTINUE
          END IF
          IF (QALFG(7,I) .EQ. 1) THEN
            DO 85 J= 1, 7
              ACNT = ACNT + 1
              APRINT(ACNT) = PGQCF6(J)
              CHEAD(ACNT) = TRIM(GQNAME) // CGQCF6(J)
              CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 85         CONTINUE
            DO 86 J= 1, 7
              ACNT = ACNT + 1
              APRINT(ACNT) = PGQCF7(J)
              CHEAD(ACNT) = TRIM(GQNAME) // CGQCF7(J)
              CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 86         CONTINUE
            DO 87 J= 1, 4
              ACNT = ACNT + 1
              APRINT(ACNT) = PGQCF4(J)
              CHEAD(ACNT) = TRIM(GQNAME) // CGQCF4(J)
              CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 87         CONTINUE
          END IF
          ACNT = ACNT + 1
          APRINT(ACNT) = PGQCF3
          CHEAD(ACNT) = TRIM(GQNAME) // '-RODQAL'
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          IF (QALFG(7,I) .EQ. 1) THEN
            DO 88 J= 1, 4
              ACNT = ACNT + 1
              APRINT(ACNT) = PGQCF5(J)
              CHEAD(ACNT) = TRIM(GQNAME) // CGQCF5(J)
              CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 88         CONTINUE
          END IF
        END IF
C
C       calculate and print total outflow of qual
C
        TOTOUT= PGQCF3 + PGQCF5(4)
        IF (PRINTU .GT. 0 .AND. PFLAG(6) .LE. LEV) THEN
          WRITE (PRINTU,2320) TOTOUT
C
          IF (NEXITS .GT. 1) THEN
            WRITE (PRINTU,2330)
            DO 90 N= 1,NEXITS
              WRITE (PRINTU,2340) N,PGQCF8(N)
 90         CONTINUE
C
            IF (QALFG(7,I) .EQ. 1) THEN
              WRITE (PRINTU,2350)
              DO 100 N= 1,NEXITS
                WRITE (PRINTU,2360) N,(PGQCF9(N,K), K=1,4)
 100          CONTINUE
            END IF
          END IF
        END IF
        IF (BINU .GT. 0 .AND. ABS(BFLAG(6)) .LE. LEV) THEN
          ACNT = ACNT + 1
          APRINT(ACNT) = TOTOUT
          CHEAD(ACNT) = TRIM(GQNAME) // '-TROQAL'
          CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
          IF (NEXITS .GT. 1) THEN
            DO 140 N= 1,NEXITS
              CALL INTCHR (N, I2, I1,
     O                     JLEN, CSTR)
              ACNT = ACNT + 1
              APRINT(ACNT) = PGQCF8(N)
              CHEAD(ACNT) = TRIM(GQNAME) // '-TOSQAL-EXIT'
              DO 110 IX= 1, JLEN
                CHEAD(ACNT) = TRIM(CHEAD(ACNT)) // CSTR(IX)
 110          CONTINUE
              CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
              DO 130 J= 1, 4
                ACNT = ACNT + 1
                APRINT(ACNT) = PGQCF9(N,J)
                CHEAD(ACNT) = TRIM(GQNAME) // CGQCF9(J)
                DO 120 IX= 1, JLEN
                  CHEAD(ACNT) = TRIM(CHEAD(ACNT)) // CSTR(IX)
 120            CONTINUE
                CLEN(ACNT) = LEN_TRIM(CHEAD(ACNT))
 130          CONTINUE
 140        CONTINUE
          END IF
        END IF
C
C       material balance check
C
        IF (PRINTU .GT. 0 .AND. PFLAG(6) .LE. LEV) THEN
          WRITE (UNITID,2370) (QTYID(J,I), J=1,2)
        END IF
C
C       calculate quantity of material entering rchres
C
        GQIN = PADTOT+ PGQIF1+ PGQIF2(4)+ PGQCF1
C
C       calculate net gain or loss of material to the rchres
C
        GQDIF= GQIN - PGQCF2(7) - PGQCF6(7) - PGQCF3 - PGQCF5(4)
C
        CALL BALCHK
     I              (I3,RCHNO,DATIM,MESSU,PRINTU,MSGFL,
     I               PGQSTS,PGQST,GQIN,GQDIF,UNITID,I1,
     M               GQWCNT(I))
 200  CONTINUE
C
      IF (BINU .GT. 0 .AND. ABS(BFLAG(6)) .LE. LEV) THEN
C       write binary output
        CALL EXDATE(
     I              DATIM,
     O              EXDAT)
        IF (BFLAG(6) .GT. 0) THEN
C         at start of run, write the header
          WRITE (BINU) I0,'RCHRES  ',RCHNO,'GQUAL   ',
     1          (CLEN(J),(CHEAD(J)(K:K),K=1,CLEN(J)),J=1,ACNT)
C         set bflag to negative to not write headers anymore
          BFLAG(6) = -BFLAG(6)
        END IF
        WRITE (BINU) I1,'RCHRES  ',RCHNO,'GQUAL   ',UNITFG,
     1         LEV,(EXDAT(J),J=1,5),(APRINT(J),J=1,ACNT)
      END IF
C
      RETURN
      END
C
C     4.2(3).9.5
C
      SUBROUTINE   GQRB
C
C     + + + PURPOSE + + +
C     Handle section gqual
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION GQUAL2 + + +
      INCLUDE    'crhgq.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,K,L,N
      REAL       CC
C
C     + + + END SPECIFICATIONS + + +
C
      DO 70 L= 1,NGQUAL
C
        CC= CINV(L)
        IF (PDFP(L) .GE. 1) THEN
          PAD(PDFP(L)+ IVL1) = PDQAL(L)*CC
        END IF
        IF (GQADDX(L) .GE. 1) THEN
          PAD(GQADDX(L)+ IVL1)= GQADDR(L)*CC
        END IF
        IF (GQADWX(L) .GE. 1) THEN
          PAD(GQADWX(L)+ IVL1)= GQADWT(L)*CC
        END IF
        IF (GQADPX(L) .GE. 1) THEN
          PAD(GQADPX(L)+ IVL1)= GQADEP(L)*CC
        END IF
        DO 10 I= 1,7
          IF (DDFP(I,L) .GE. 1) THEN
            PAD(DDFP(I,L)+ IVL1)= DDQAL(I,L)*CC
          END IF
 10     CONTINUE
        IF (RODFP(L) .GE. 1) THEN
          PAD(RODFP(L)+ IVL1)= RODQAL(L)*CC
        END IF
        IF (TROQFP(L) .GE. 1) THEN
          PAD(TROQFP(L)+ IVL1)= TROQAL(L)*CC
        END IF
        IF (RCDQFP(L) .GE. 1) THEN
          PAD(RCDQFP(L)+ IVL1)= IDQAL(L)*CC
        END IF
        IF (TIQALX(L) .GE. 1) THEN
          PAD(TIQALX(L)+ IVL1)= TIQAL(L)*CC
        END IF
C
        IF (QALFG(7,L) .EQ. 1) THEN
C         qual is associated with sediment, and additional time
C         series can be output
          DO 20 I= 1, 4
            IF (RCSQFP(I,L) .GE. 1) THEN
              PAD(RCSQFP(I,L)+ IVL1)= ISQAL(I,L)*CC
            END IF
            IF (DSFP(I,L) .GE. 1) THEN
              PAD(DSFP(I,L)+ IVL1) = DSQAL(I,L)*CC
            END IF
            IF (ROSFP(I,L) .GE. 1) THEN
              PAD(ROSFP(I,L)+ IVL1)= ROSQAL(I,L)*CC
            END IF
 20       CONTINUE
          DO 30 I= 1,7
            IF (SQDFP(I,L) .GE. 1) THEN
              PAD(SQDFP(I,L) + IVL1)= SQDEC(I,L)*CC
            END IF
            IF (ADFP(I,L) .GE. 1) THEN
              PAD(ADFP(I,L) + IVL1) = ADQAL(I,L)*CC
            END IF
 30       CONTINUE
        END IF
C
        IF (NEXITS .GT. 1) THEN
          DO 40 N= 1,NEXITS
            IF (ODFP(N,L) .GE. 1) THEN
              PAD(ODFP(N,L) + IVL1)= ODQAL(N,L)*CC
            END IF
 40       CONTINUE
          IF (QALFG(7,L) .EQ. 1) THEN
            DO 60 N= 1,NEXITS
              DO 50 K= 1,3
                IF (OSFP(N,K,L) .GE. 1) THEN
                  PAD(OSFP(N,K,L) + IVL1)= OSQAL(N,K,L)*CC
                END IF
 50           CONTINUE
              IF (TOSFP(N,L) .GE. 1) THEN
                PAD(TOSFP(N,L) + IVL1)= TOSQAL(N,L)*CC
              END IF
 60         CONTINUE
          END IF
        END IF
C
 70   CONTINUE
C
      RETURN
      END
C
C     4.2(3).8.5
C
      SUBROUTINE   GQRP
C
C     + + + PURPOSE + + +
C     Handle section gqual
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION GQUAL2 + + +
      INCLUDE    'crhgq.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,L
      REAL       CC
C
C     + + + END SPECIFICATIONS + + +
C
      DO 30 L= 1,NGQUAL
        CC= CINV(L)
        IF (DQFP(L) .GE. 1) THEN
          PAD(DQFP(L) + IVL1)= DQAL(L)
        END IF
        IF (RDFP(L) .GE. 1) THEN
          PAD(RDFP(L) + IVL1)= RDQAL(L)*CC
        END IF
        IF (RRFP(L) .GE. 1) THEN
          PAD(RRFP(L) + IVL1)= RRQAL(L)*CC
        END IF
C
        IF (QALFG(7,L) .EQ. 1) THEN
C         qual is sediment associated, and the following time
C         series may be output
          DO 10 I= 1,6
            IF (SQFP(I,L) .GE. 1) THEN
              PAD(SQFP(I,L) + IVL1)= SQAL(I,L)
            END IF
 10       CONTINUE
          DO 20 I= 1,12
            IF (RSFP(I,L) .GE. 1) THEN
              PAD(RSFP(I,L) + IVL1)= RSQAL(I,L)*CC
            END IF
 20       CONTINUE
        END IF
C
 30   CONTINUE
C
      RETURN
      END
C
C     4.2(3).10.3.5
C
      SUBROUTINE   GQRST
     I                   (LEV)
C
C     + + + PURPOSE + + +
C     Reset flux and state variables for module section gqual
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    LEV
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEV    - current output level (2-pivl,3-day,4-mon,5-ann)
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION GQUAL2 + + +
      INCLUDE    'crhgq.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,I1,I3,I4,I7,L
      REAL       RZERO
C
C     + + + EXTERNALS + + +
      EXTERNAL   SETVEC
C
C     + + + END SPECIFICATIONS + + +
C
      I1=1
      I3=3
      I4=4
      I7=7
      RZERO=0.0
      DO 20 L=1,NGQUAL
C       handle flux groups dealing with rchres-wide variables
C
        CALL SETVEC
     I              (I1,RZERO,
     O               GQIF1(L,LEV))
C
        CALL SETVEC
     I              (I1,RZERO,
     O               GQCF1(L,LEV))
C
        CALL SETVEC
     I              (I7,RZERO,
     O               GQCF2(1,L,LEV))
C
        CALL SETVEC
     I              (I1,RZERO,
     O               GQCF3(L,LEV))
        CALL SETVEC
     I              (I3,RZERO,
     O               GQCF10(1,LEV))
        CALL SETVEC
     I              (I3,RZERO,
     O               GQCF11(1,LEV))
C
        IF (QALFG(7,L) .EQ. 1) THEN
C         qual is sediment associated- zero some additional fluxes
C
          CALL SETVEC
     I                (I4,RZERO,
     O                 GQCF4(1,L,LEV))
C
          CALL SETVEC
     I                (I4,RZERO,
     O                 GQCF5(1,L,LEV))
C
          CALL SETVEC
     I                (I7,RZERO,
     O                 GQCF6(1,L,LEV))
C
          CALL SETVEC
     I                (I7,RZERO,
     O                 GQCF7(1,L,LEV))
C
          CALL SETVEC
     I                (I4,RZERO,
     O                 GQIF2(1,L,LEV))
C
        END IF
C
        IF (NEXITS .GT. 1) THEN
C         handle fluxes associated with individual exits
C
          CALL SETVEC
     I                (NEXITS,RZERO,
     O                 GQCF8(1,L,LEV))
C
          IF (QALFG(7,L) .EQ. 1) THEN
            DO 10 I=1,3
              CALL SETVEC
     I                    (NEXITS,RZERO,
     O                     GQCF9(1,I,L,LEV))
 10         CONTINUE
          END IF
        END IF
C       keep presend gqual storages in state variable array used for
C       for material balance check
C
        GQST(L,LEV)= GQST(L,1)
 20   CONTINUE
C
      RETURN
      END
