C
C     This file is used to pull in all the WDM subroutines needed so that the
C     f2py processor can pull them into Python.  Essentially just points
C     to the WDM library but then tells f2py what the input/output is
C
C     
      SUBROUTINE HSPPY
     I                (FILENAME, HMSNAM,
     O                 RETCOD) 
      CHARACTER*64 FILENAME, HMSNAM
      INTEGER RETCOD
Cf2py intent(in) FILENAME, HMSNAM
Cf2py intent(out) RETCOD
      EXTERNAL HSPFBAT
      CALL HSPFBAT(FILENAME, HMSNAM, RETCOD)
      END
C
      SUBROUTINE SYDATEPY
     O                   ( YR, MO, DA )
      INTEGER YR, MO, DA
Cf2py intent(out) YR
Cf2py intent(out) MO
Cf2py intent(out) DA
      EXTERNAL SYDATE
      CALL SYDATE( YR, MO, DA )
      END
C
      SUBROUTINE TIMCVTPY
     M                   (DATE)
      INTEGER DATE(6)
Cf2py intent(in,out) DATE
      EXTERNAL TIMCVT
      CALL TIMCVT(DATE)
      END
C
      SUBROUTINE TIMDIFPY
     I                    ( DATE1, DATE2, TCODE, TSTEP,
     O                     NVALS )
      INTEGER   DATE1(6),DATE2(6),TCODE,TSTEP
      INTEGER*4 NVALS
Cf2py intent(in) DATE1
Cf2py intent(in) DATE2
Cf2py intent(in) TCODE
Cf2py intent(in) TSTEP
Cf2py intent(out) NVALS
      EXTERNAL  TIMDIF
      CALL TIMDIF(DATE1,DATE2,TCODE,TSTEP,NVALS)
      END
C
      SUBROUTINE WDBFINPY
      EXTERNAL WDBFIN
      CALL WDBFIN
      END
C
      SUBROUTINE WDBOPNPY
     I                    (WDMSFL, WDNAME, RONWFG,
     O                     RETCOD )
      INTEGER      WDMSFL,RONWFG,RETCOD
      CHARACTER*64 WDNAME
Cf2py intent(in) WDMSFL
Cf2py intent(in) WDNAME
Cf2py intent(in) RONWFG
Cf2py intent(out) RETCOD
      EXTERNAL  WDBOPN
      CALL WDBOPN(WDMSFL,WDNAME,RONWFG, RETCOD)
      END
C
      SUBROUTINE WDBSACPY
     I                   (WDMSFL, DSN, MESSFL, SAIND, SALEN, SAVAL,
     O                    RETCOD )
      INTEGER     WDMSFL,DSN,MESSFL,SAIND,SALEN,RETCOD
      CHARACTER*1 SAVAL(SALEN)
Cf2py intent(in) WDMSFL
Cf2py intent(in) DSN
Cf2py intent(in) MESSFL
Cf2py intent(in) SAIND
Cf2py intent(in) SALEN
Cf2py intent(in) SAVAL
Cf2py intent(out) RETCOD
      EXTERNAL  WDBSAC
      CALL WDBSAC(WDMSFL,DSN,MESSFL,SAIND,SALEN,SAVAL,RETCOD)
      END
C
      SUBROUTINE WDBSAIPY
     I                   (WDMSFL, DSN, MESSFL, SAIND, SALEN, SAVAL,
     O                    RETCOD )
      INTEGER   WDMSFL,DSN,MESSFL,SAIND,RETCOD,SALEN
      INTEGER   SAVAL(SALEN)
Cf2py intent(in) WDMSFL
Cf2py intent(in) DSN
Cf2py intent(in) MESSFL
Cf2py intent(in) SAIND
Cf2py intent(in) SALEN
Cf2py intent(in) SAVAL
Cf2py intent(out) RETCOD
      EXTERNAL  WDBSAI
      CALL WDBSAI(WDMSFL,DSN,MESSFL,SAIND,SALEN,SAVAL,RETCOD)
      END
C
      SUBROUTINE WDBSARPY
     I                   (WDMSFL, DSN, MESSFL, SAIND, SALEN, SAVAL,
     O                    RETCOD )
      INTEGER     WDMSFL,DSN,MESSFL,SAIND,SALEN,RETCOD
      REAL        SAVAL(SALEN)
Cf2py intent(in) WDMSFL
Cf2py intent(in) DSN
Cf2py intent(in) MESSFL
Cf2py intent(in) SAIND
Cf2py intent(in) SALEN
Cf2py intent(in) SAVAL
Cf2py intent(out) RETCOD
      EXTERNAL  WDBSAR
      CALL WDBSAR(WDMSFL,DSN,MESSFL,SAIND,SALEN,SAVAL,RETCOD)
      END
C
      SUBROUTINE WDBSGCPY
     I                   (WDMSFL, DSN, SAIND, SALEN,
     O                    SAVAL, RETCOD )
      INTEGER     WDMSFL,SAIND,SALEN,DSN,RETCOD
      CHARACTER*1 SAVAL(SALEN)
Cf2py intent(in) WDMSFL
Cf2py intent(in) DSN
Cf2py intent(in) SAIND 
Cf2py intent(in) SALEN
Cf2py intent(out) SAVAL
Cf2py intent(out) RETCOD
      EXTERNAL  WDBSGC
      CALL WDBSGC(WDMSFL,DSN,SAIND,SALEN,SAVAL,RETCOD)
      END
C
      SUBROUTINE WDBSGIPY
     I                   (WDMSFL, DSN, SAIND, SALEN,
     O                    SAVAL, RETCOD )
      INTEGER   WDMSFL,SAIND,SALEN,DSN,RETCOD
      INTEGER   SAVAL(SALEN)
Cf2py intent(in) WDMSFL
Cf2py intent(in) DSN
Cf2py intent(in) SAIND
Cf2py intent(in) SALEN
Cf2py intent(out) SAVAL
Cf2py intent(out) RETCOD
      EXTERNAL  WDBSGI
      CALL WDBSGI(WDMSFL,DSN,SAIND,SALEN,SAVAL,RETCOD)
      END
C
      SUBROUTINE WDBSGRPY
     I                   (WDMSFL, DSN, SAIND, SALEN,
     O                    SAVAL, RETCOD)
      INTEGER   WDMSFL,SAIND,SALEN,DSN,RETCOD
      REAL      SAVAL(SALEN)
Cf2py intent(in) WDMSFL
Cf2py intent(in) DSN
Cf2py intent(in) SAIND
Cf2py intent(in) SALEN
Cf2py intent(out) SAVAL
Cf2py intent(out) RETCOD
      EXTERNAL  WDBSGR
      CALL WDBSGR(WDMSFL,DSN,SAIND,SALEN,SAVAL,RETCOD)
      END
C
      INTEGER FUNCTION WDCKDTPY
     I                         (WDMSFL,DSN)
C
C     + + + PURPOSE + + +
C     Check data set for existance and type, returns:
C         0 - data set does not exist
C     or data-set type
C         1 - time series      6 - rastor
C         2 - table            7 - space-time
C         3 - schematic        8 - attribute
C         4 - project          9 - message
C         5 - vector
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,DSN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number of WDM file
C     DSN    - data-set number to be checked
C
C     + + + COMMON BLOCKS + + +
C      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
C      INTEGER   RIND,DSNFRC,DSTYPE,RETCOD
C
C     + + + FUNCTIONS + + +
C      INTEGER   WDRCGO
      INTEGER WDCKDT
C
C     + + + EXTERNALS + + +
C      EXTERNAL WDCKDT
C      EXTERNAL  WDDSCK, WDRCGO
C
C     + + + END SPECIFICATIONS + + +
C
C      CALL WDDSCK(WDMSFL,DSN,
C     O            DSNFRC,RETCOD)
C      IF (DSNFRC.GT.0) THEN
C       data set exists
C        RIND  = WDRCGO(WDMSFL,DSNFRC)
C        DSTYPE= WIBUFF(6,RIND)
C      ELSE
C       data set does not exist
C        DSTYPE= 0
C      END IF
C
C      WDCKDTPY= DSTYPE
      WDCKDTPY = WDCKDT(WDMSFL,DSN)
C
      RETURN
      END
C
      SUBROUTINE WDFLCLPY
     I                   (WDMSFL,
     O                    RETCOD )
      INTEGER   WDMSFL,RETCOD
Cf2py intent(in) WDMSFL
Cf2py intent(out) RETCOD
      EXTERNAL  WDFLCL
      CALL WDFLCL(WDMSFL,RETCOD)
      END
C
      SUBROUTINE WDLBAXPY
     I                   (WDMSFL, DSN, DSTYPE, NDN, NUP, NSA, NSASP,NDP,
     O                    PSA )
      INTEGER   WDMSFL,DSN,DSTYPE,NDN,NUP,NSA,NSASP,NDP,PSA
Cf2py intent(in) WDMSFL
Cf2py intent(in) DSN
Cf2py intent(in) DSTYPE
Cf2py intent(in) NDN
Cf2py intent(in) NUP
Cf2py intent(in) NSA
Cf2py intent(in) NSASP
Cf2py intent(in) NDP
Cf2py intent(out) PSA
      EXTERNAL  WDLBAX
      CALL WDLBAX(WDMSFL,DSN,DSTYPE,NDN,NUP,NSA,NSASP,NDP,PSA)
      END
C
      SUBROUTINE WDTGETPY
     I                    (WDMSFL, DSN, DELT, DATES, NVAL,
     I                     DTRAN, QUALFG, TUNITS,
     O                     RVAL, RETCOD )
      INTEGER   WDMSFL,DSN,DELT,DATES(6),NVAL,DTRAN,QUALFG,
     1          TUNITS,RETCOD
      REAL      RVAL(NVAL)
Cf2py intent(in) WDMSFL
Cf2py intent(in) DSN
Cf2py intent(in) DELT
Cf2py intent(in) DATES
Cf2py intent(in) NVAL
Cf2py intent(in) DTRAN
Cf2py intent(in) QUALFG
Cf2py intent(in) TUNITS
Cf2py intent(out) RVAL
Cf2py intent(out) RETCOD
      EXTERNAL  WDTGET
      CALL WDTGET(WDMSFL,DSN,DELT,DATES,NVAL,DTRAN,QUALFG,TUNITS,
     O            RVAL,RETCOD)
      END
C
      SUBROUTINE WDTPUTPY
     I                   (WDMSFL, DSN, DELT, DATES, NVAL,
     I                    DTOVWR, QUALFG, TUNITS, RVAL,
     O                    RETCOD )
      INTEGER   WDMSFL,DSN,DELT,DATES(6),NVAL,DTOVWR,QUALFG,
     1          TUNITS,RETCOD
      REAL      RVAL(NVAL)
Cf2py intent(in) WDMSFL
Cf2py intent(in) DSN
Cf2py intent(in) DELT
Cf2py intent(in) DATES
Cf2py intent(in) NVAL
Cf2py intent(in) DTOVWR
Cf2py intent(in) QUALFG
Cf2py intent(in) TUNITS
Cf2py intent(in) RVAL
Cf2py intent(out) RETCOD
      EXTERNAL  WDTPUT
      CALL WDTPUT(WDMSFL, DSN, DELT, DATES, NVAL, DTOVWR, QUALFG,
     I            TUNITS, RVAL,
     O            RETCOD )
      END
C
      SUBROUTINE WTFNDTPY
     I                    (WDMSFL,DSN,GPFLG,
     O                     TDSFRC,SDAT,EDAT,RETCOD)
C      INTEGER   WDMSFL,DSN,GPFLG,DATES(6),LTSTEP,LTUNIT,
C     1          TDSFRC,TGROUP,TSPTAD,GPOSST,GPOSEN,
C     2          ENDDAT(6),GPSDAT(6),RETCOD
C      INTEGER*4 I4NVAL
C      REAL      TSFILL,TOLR
      INTEGER   WDMSFL,DSN,GPFLG,TDSFRC,SDAT(6),EDAT(6),RETCOD
Cf2py intent(in) WDMSFL
Cf2py intent(in) DSN
Cf2py intent(in) GPFLG
Cf2py intent(out) TDSFRC
Cf2py intent(out) SDAT
Cf2py intent(out) EDAT
Cf2py intent(out) RETCOD
      EXTERNAL  WTFNDT
      CALL WTFNDT(WDMSFL,DSN,GPFLG,TDSFRC,SDAT,EDAT,RETCOD)
C      CALL WTFNDT(WDMSFL,DSN,GPFLG,DATES,LTSTEP,LTUNIT,I4NVAL,
C     O                     TDSFRC,TSFILL,TGROUP,TOLR,TSPTAD,
C     O                     GPOSST,GPOSEN,GPSDAT,ENDDAT,RETCOD)
      END
C
      SUBROUTINE WDDSRNPY
     I                   (WDMSFL, ODSN, NDSN,
     O                    RETCOD)
      INTEGER   WDMSFL,ODSN,NDSN,RETCOD
Cf2py intent(in) WDMSFL
Cf2py intent(in) ODSN
Cf2py intent(in) NDSN
Cf2py intent(out) RETCOD
      EXTERNAL  WDDSRN
      CALL WDDSRN(WDMSFL,ODSN,NDSN,RETCOD)
      END
C
      SUBROUTINE WDDSDLPY
     I                   (WDMSFL, DSN,
     O                    RETCOD )
      INTEGER   WDMSFL,DSN,RETCOD
Cf2py intent(in) WDMSFL
Cf2py intent(in) DSN
Cf2py intent(out) RETCOD
      EXTERNAL  WDDSDL
      CALL WDDSDL(WDMSFL,DSN,RETCOD)
      END
C
      SUBROUTINE WDDSNXPY
     I                   (WDMSFL,
     M                    DSN)
Cf2py intent(in) WDMSFL
Cf2py intent(in,out) DSN
      INTEGER WDMSFL, DSN
      CALL WDDSNX(WDMSFL,DSN)
      END
C
C
C
      SUBROUTINE ERROROPEN
     I                    (NUMBER)
Cf2py intent(in) NUMBER
C
C     + + + PURPOSE + + +
C     Dummy routine to open the Fortran Error file
C
      INTEGER NUMBER
      OPEN(UNIT=NUMBER,FILE='error.fil')
      RETURN
      END
C
C
C
      SUBROUTINE SEQOPEN
     I                  (SEQFILE, NUMBER)
Cf2py intent(in) SEQFILE, NUMBER
C
C     + + + PURPOSE + + +
C     Dummy routine to open a Fortran .SEQ file
C
      INTEGER      NUMBER
      CHARACTER*64 SEQFILE
      OPEN(UNIT=NUMBER,FILE=SEQFILE,STATUS='OLD')
      RETURN
      END
C
C
C
      SUBROUTINE SEQIMPORT
     I                    (WDM, SEQ, ATM)
Cf2py intent(in) WDM, SEQ, ATM
C
C     + + + PURPOSE + + +
C     Dummy routine to import data from a .SEQ file to a .WDM file
C
      INTEGER WDM, SEQ, ATM
C
      CALL WDMIM (SEQ,WDM,ATM)
      RETURN
      END
C
C
C
      SUBROUTINE SEQCLOSE
     I                   (NUMBER)
Cf2py intent(in) NUMBER
C
C     + + + PURPOSE + + +
C     Dummy routine to close a Fortran .SEQ file
C
      INTEGER NUMBER
      CLOSE(UNIT=NUMBER)
      RETURN
      END
C
C Note this is copied directly from LIB3.2/SRC/WDIMEX/WDIMEX.F
C
      SUBROUTINE   WDMIM
     I                   (SUCIFL,WDMSFL,ATMSFL)
C
C     + + + PURPOSE + + +
C     Import data sets (clusters) from sequential file to WDM file.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   SUCIFL,WDMSFL,ATMSFL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     SUCIFL - Fortran unit number of sequential import file
C     WDMSFL - Fortran unit number of WDM file
C     ATMSFL - Fortran unit number of WDM file containing attributes
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,J,K,I3,I4,I6,I10,I80,RETCOD,DONFG,SKIPFG,
     1            CLU,CLUTYP,PSA,NDN,NUP,NSA,NSP,NDP,SAIND,SALEN,
     2            SATYP,IVAL(10),DSFREC
      REAL        RVAL(10)
      CHARACTER*1 BUFF(160),CCLU(3),CDSN(3),CEND(3),CLAB(3),CDAT(3),
     1            CRESP,CDSTYP(36)
C
C     + + + FUNCTIONS + + +
      INTEGER     STRFND, CHRINT, LENSTR
      REAL        CHRDEC
C
C     + + + EXTERNALS + + +
      EXTERNAL    STRFND, CHRINT, WDDSCK, CHRDEC, LENSTR, PRWMSI
      EXTERNAL    WDDSDL, WDLBAX, PRWMAI, PRWMDI, PRWMTI, PRWMXI
      EXTERNAL    WDBSGX, WDBSAI, WDBSAR, WDBSAC
C
C     + + + DATA INITIALIZATIONS + + +
      DATA I3,I4,I6,I10,I80/3,4,6,10,80/
      DATA CCLU,CDSN,CEND/'C','L','U','D','S','N','E','N','D'/
      DATA CLAB,CDAT/'L','A','B','D','A','T'/
      DATA CDSTYP/'T','I','M','E','T','A','B','L','S','C','H','E',
     1            'P','R','O','J','V','E','C','T','R','A','S','T',
     2            'S','P','T','I','A','T','T','R','M','E','S','S'/
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (80A1)
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (' General information from the IMPORT file:')
 2002 FORMAT (/,' Importing DSN/CLU number ',I5,'.')
 2005 FORMAT (' This DSN/CLU already exists.')
 2010 FORMAT (' *** Error on LABEL/ADD, return code:',I5,'.')
 2015 FORMAT (' *** Error, no attrib. for new DSN on IMPORT file.')
 2020 FORMAT (1X,80A1)
 2040 FORMAT (/,' Import of DSN/CLUs complete.',/)
C
C     + + + END SPECIFICATIONS + + +
C
C     general info
      WRITE (*,2000)
      WRITE(99,2000)
C
C     loop to write out general comments
      DONFG= 0
 10   CONTINUE
        READ (SUCIFL,1000) (BUFF(I),I=1,I80)
        IF (STRFND(I4,BUFF,I3,CCLU).EQ.0 .AND.
     1      STRFND(I4,BUFF,I3,CDSN).EQ.0) THEN
C         general comment line, write it to terminal and log file
          I= LENSTR(I80,BUFF)
          WRITE (*,2020) (BUFF(J),J=1,I)
          WRITE(99,2020) (BUFF(J),J=1,I)
        ELSE
          DONFG= 1
        END IF
      IF (DONFG.EQ.0) GO TO 10
C
 15   CONTINUE
C       process a cluster
        CLU= CHRINT(I6,BUFF(11))
        I= 36
        J= STRFND(I,CDSTYP,I4,BUFF(27))
        CLUTYP= 1+ ((J-1)/4)
        NDN= CHRINT(I3,BUFF(38))
        IF (NDN.EQ.0) NDN= 10
        NUP= CHRINT(I3,BUFF(48))
        IF (NUP.EQ.0) NUP= 20
        NSA= CHRINT(I3,BUFF(58))
        IF (NSA.EQ.0) NSA= 20
        NSP= CHRINT(I3,BUFF(68))
        IF (NSP.EQ.0) NSP= 50
        NDP= CHRINT(I3,BUFF(78))
        IF (NDP.EQ.0) NDP= 100
C
        WRITE (*,2002) CLU
        WRITE(99,2002) CLU
C
C       loop to find valid cluster to write or skip
        SKIPFG= 0
        CALL WDDSCK (WDMSFL,CLU,
     O               DSFREC,RETCOD)
        IF (DSFREC.GT.0) THEN
C         cluster exists, change, skip, add, or overwrite data?
          WRITE (*,2005)
          WRITE(99,2005)
 25       CONTINUE
            WRITE (*,*) 'Do you want to Skip data, ',
     1                  'Overwrite data or Abort import',
     2                  ' (Enter S, O, or A) '
            READ (*,1000) CRESP
          IF (CRESP.NE.'S'.AND.CRESP.NE.'s'.AND.CRESP.NE.'O'.AND.
     1      CRESP.NE.'o'.AND.CRESP.NE.'A'.AND.CRESP.NE.'a') GO TO 25
          IF (CRESP.EQ.'S' .OR. CRESP.EQ.'s') THEN
C           skip cluster
            WRITE(99,*) 'Skipping DSN/CLU'
            SKIPFG= 2
          ELSE IF (CRESP.EQ.'O' .OR. CRESP.EQ.'o') THEN
C           overwrite cluster
            CALL WDDSDL (WDMSFL,CLU,
     O                   RETCOD)
            WRITE(99,*) 'Overwrite DSN/CLU'
            SKIPFG= 0
          ELSE
C           aborting import
            WRITE(99,*) 'Abort import'
            SKIPFG= 5
          END IF
        END IF
C
        IF (SKIPFG.EQ.0) THEN
C         copy label from import file to new cluster, first add label
          CALL WDLBAX (WDMSFL,CLU,CLUTYP,NDN,NUP,NSA,NSP,NDP,
     O                 PSA)
C         next attributes
 30       CONTINUE
            READ (SUCIFL,1000) (BUFF(I),I=1,I80)
            DONFG= STRFND(I3,BUFF(3),I3,CDAT)
          IF (STRFND(I3,BUFF(3),I3,CLAB).EQ.0.AND.DONFG.EQ.0)
     1      GOTO 30
C
          IF (DONFG.EQ.0) THEN
C           'LABEL' found in 30 loop, now in attributes
            RETCOD= 0
 40         CONTINUE
              READ (SUCIFL,1000) (BUFF(I),I=1,I80)
C             are we at end?
              IF (STRFND(I3,BUFF(3),I3,CEND).GT.0) THEN
C               yes, get out of this loop
                DONFG= 2
                SAIND= 0
              ELSE
C               which attribute
                CALL WDBSGX (ATMSFL,BUFF(5),
     O                       SAIND,SATYP,SALEN)
              END IF
              IF (SAIND.GT.0) THEN
C               valid attribute type
                GOTO (41,43,45), SATYP
C
 41             CONTINUE
C                 integer attribute
                  J= 1
                  DO 42 I= 1,SALEN
                    J= J+ 10
                    IF (J.GT.I80) THEN
                      J= J+ 10
                      READ(SUCIFL,1000) (BUFF(K),K=81,160)
                    END IF
                    IVAL(I)= CHRINT(I10,BUFF(J))
 42               CONTINUE
                  CALL WDBSAI (WDMSFL,CLU,ATMSFL,SAIND,SALEN,IVAL,
     O                         RETCOD)
                  GO TO 50
C
 43             CONTINUE
C                 real attribute
                  J= 1
                  DO 44 I= 1,SALEN
                    J= J+ 10
                    IF (J.GT.I80) THEN
                      J= J+ 10
                      READ(SUCIFL,1000) (BUFF(K),K=81,160)
                    END IF
                    RVAL(I)= CHRDEC(I10,BUFF(J))
 44               CONTINUE
                  CALL WDBSAR (WDMSFL,CLU,ATMSFL,SAIND,SALEN,RVAL,
     O                         RETCOD)
                  GO TO 50
C
 45             CONTINUE
C                 character attribute
                  IF ((SALEN+12).GT.I80) THEN
                    READ (SUCIFL,1000) (BUFF(K),K=81,148)
                  END IF
                  CALL WDBSAC (WDMSFL,CLU,ATMSFL,
     I                         SAIND,SALEN,BUFF(13),
     O                         RETCOD)
                  GO TO 50
C
 50             CONTINUE
              ELSE IF (DONFG.EQ.0) THEN
C               unknown attribute
                RETCOD= 1
              END IF
C
              IF (RETCOD.NE.0) THEN
C               problem writing out attributes
                WRITE (*,2010) RETCOD
                WRITE(99,2010) RETCOD
                RETCOD= 0
              END IF
            IF (DONFG.EQ.0) GO TO 40
C
          ELSE
C           no attributes found to write on a new cluster, skip
            WRITE (*,2015)
            WRITE(99,2015)
            SKIPFG= 2
          END IF
C
C         **** ADD POINTERS HERE SOMEDAY ****
C
        END IF
C
        IF (SKIPFG.NE.2 .AND. SKIPFG.NE.5) THEN
C         skip to data
          DONFG= 0
 60       CONTINUE
            READ (SUCIFL,1000) (BUFF(I),I=1,I80)
            IF (STRFND(I3,BUFF,I3,CEND).GT.0) DONFG= 1
          IF (STRFND(I3,BUFF(3),I3,CDAT).EQ.0.AND.DONFG.EQ.0) GOTO 60
C
          IF (DONFG.EQ.0) THEN
C           message file data data exists, now input it
            IF (CLUTYP.EQ.1) THEN
C             timeseries type data
              CALL PRWMTI (WDMSFL,SUCIFL,CLU,
     O                     RETCOD)
            ELSE IF (CLUTYP.EQ.2) THEN
C             table type data
              CALL PRWMXI (ATMSFL,WDMSFL,SUCIFL,CLU,
     O                     RETCOD)
            ELSE IF (CLUTYP.EQ.5) THEN
C             vector (DLG) type data
              CALL PRWMDI (WDMSFL,SUCIFL,CLU,
     O                     RETCOD)
            ELSE IF (CLUTYP.EQ.8) THEN
C             attribute type data
              CALL PRWMAI (WDMSFL,SUCIFL,CLU,
     O                     RETCOD)
            ELSE IF (CLUTYP.EQ.9) THEN
C             message type data
              CALL PRWMSI (WDMSFL,SUCIFL,CLU,
     O                     RETCOD)
            END IF
            IF (RETCOD .NE. 0) THEN
C             problems with import, let user know
              WRITE(*,*) '*** Problems with import, return code:',
     1                    RETCOD
              WRITE(*,*) '    review file "error.fil" for details'
            ELSE
              WRITE(*,*) 'DSN/CLU data import complete - no problems.'
            END IF
          END IF
        END IF
C
        IF (SKIPFG.NE.5) THEN
C         not aborting, look for more clusters
 800      CONTINUE
            READ (SUCIFL,1000,END=900) (BUFF(I),I=1,I80)
C           WRITE (*,1000) (BUFF(I),I=1,I80)
          IF (STRFND(I4,BUFF,I3,CCLU).EQ.0 .AND.
     1        STRFND(I4,BUFF,I3,CDSN).EQ.0) GO TO 800
        END IF
      IF (SKIPFG.NE.5) GO TO 15
 900  CONTINUE
C     close import file
      CLOSE (SUCIFL)
C     end of file, all clusters copied
      WRITE (*,2040)
      WRITE(99,2040)
C
      RETURN
      END
