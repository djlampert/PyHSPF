C
C
C
      SUBROUTINE   FILBLK
     I                    (USRFL,
     M                     FILES,
     O                     RETCOD)
C
C     + + + PURPOSE + + +
C     Process HSPF Files Block.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       USRFL,FILES(15),RETCOD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     USRFL   - fortran unit number for user's input file
C     FILES   - integer array of hspf file unit numbers
C     RETCOD  - error return code
C
C     + + + COMMON BLOCKS + + +
      INCLUDE       'cfilbk.inc'
      INCLUDE       'cifltb.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I,RCL,FILX,I0,ISTAT,ID,FILFND,RET,BASE
      CHARACTER*6   CNOP,CMESSU,CTSS,CKEY,CDSS,CUNIT,CMUNIT,CMLEVL,
     $              PRGNAM
      CHARACTER*12  ACC,STAT,CPROG
      CHARACTER*30  FRM
      CHARACTER*64  FILNAM,DEFNAM
C
C     + + + INTRINSICS + + +
      INTRINSIC IABS
C
C     + + + FUNCTIONS + + +
      INTEGER   CHRDIG
C
C     + + + EXTERNALS + + +
      EXTERNAL  FILOPN,RDFLBK,WDBOPN,OMSINI,ZSET,ATTACH,ATTEND,ZOPEN,
     #          ZIPI,XGTRCL,XPLFRM,CHRDIG
C
C     + + + DATA INITIALIZATIONS + + +
      DATA CNOP/'NOP   '/,CMESSU/'MESSU '/,CTSS/'TSS   '/,
     $     CMLEVL/'MLEVEL'/,CUNIT/'UNIT  '/,CMUNIT/'MUNIT '/,
     $     CPROG/'PROGRAM     '/
      DATA PRGNAM/'HSPF12'/
      DATA I0/0/
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT ('DSS',I1,'  ')
 2010 FORMAT ('DSS',I2,' ')
 2020 FORMAT ('UNIT0',I1)
 2030 FORMAT ('UNIT',I2)
C
C     + + + END SPECIFICATIONS + + +
C
      IF (ABS(USRFL) .GT. 0) THEN
C       read files block from users input file and store in arrays
        FILFND= 0
        CALL RDFLBK
     I              (ABS(USRFL),FILES(15),
     M               FILFND,
     O               NUMFIL,FTYPE,FNAME,FUNIT,RETCOD)
      END IF
C
C     process non-scratch disk files
C
      IF (FILES(1) .EQ. 0) THEN
C       run interpreter output file (messu)
        FILX= 0
        I   = 0
 10     CONTINUE
          I= I+ 1
          IF (FTYPE(I) .EQ. 'MESSU') THEN
            FILX= I
          END IF
        IF (FILX.EQ.0 .AND. I.LT.NUMFIL) GO TO 10
C
        IF (FILX .GT. 0) THEN
          DEFNAM  = FNAME(FILX)
          FILES(1)= FUNIT(FILX)
        ELSE
          DEFNAM  = 'hspfecho.out'
          FILES(1)= 14
        END IF
C
        CALL ATTACH (I0,CMESSU,DEFNAM,CNOP,
     O               FILNAM,ISTAT)
        IF (ISTAT .NE. -1) THEN
C         default name didn't change - reset to avoid truncation at 30
C         characters
          FILNAM= DEFNAM
        END IF
C
        IF (ISTAT .LE. 0) THEN
C         try to open file
          ACC=  'SEQUENTIAL'
          FRM=  'FORMATTED'
          RCL=   132
          STAT= 'UNKNOWN'
          CALL FILOPN
     I                (ACC,FRM,RCL,STAT,FILES(1),FILNAM,
     O                 RET)
        END IF
        RETCOD= RETCOD+ RET
      END IF
C
C     wdm files
      DO 20 FILX= 1, NUMFIL
        IF (FTYPE(FILX)(1:3) .EQ. 'WDM') THEN
C         process this entry
          I= CHRDIG(FTYPE(FILX)(4:4))
          IF (I .LT. 1) THEN
C           blank or invalid
            I= 1
          END IF
C
          IF (FILES(10+I) .EQ. 0) THEN
C           try to open file
C
C           see if user specified alternate name on command line
            CALL ATTACH (I0,FTYPE(FILX),FNAME(FILX),CNOP,
     O                   FILNAM,ISTAT)
            IF (ISTAT .NE. -1) THEN
C             default name didn't change - reset to avoid truncation at 30
C             characters
              FILNAM= FNAME(FILX)
            END IF
C
            IF (ISTAT .LE. 0) THEN
C             try to open file
              FILES(10+I)= FUNIT(FILX)
              CALL WDBOPN (FILES(10+I),FILNAM,I0,
     O                     RET)
            END IF
          END IF
          RETCOD= RETCOD+ IABS(RET)
        END IF
 20   CONTINUE
C
C     dss files
C
      CALL ZIPI (MAXDSS,I0,
     O           DSSOPN)
C
      DO 40 ID= 1, MAXDSS
C
C       construct dss character key
        IF (ID .LE. 9) THEN
          WRITE (CDSS,2000) ID
        ELSE IF (ID .LE. 99) THEN
          WRITE (CDSS,2010) ID
        END IF
C
        FILX= 0
        DO 30 I= 1, NUMFIL
          IF (FTYPE(I) .EQ. CDSS) THEN
            FILX= I
          END IF
 30     CONTINUE
C
        IF (FILX .GT. 0) THEN
          CALL ATTACH (I0,CDSS,FNAME(FILX),CNOP,
     O                 FILNAM,ISTAT)
          IF (ISTAT .NE. -1) THEN
C           default name didn't change - reset to avoid truncation at 30
C           characters
            FILNAM= FNAME(FILX)
          END IF
C
          IF (ISTAT .LE. 0) THEN
C           try to open file
            CALL ZSET (CPROG,PRGNAM,I0)
            CALL ZSET (CMUNIT,CMUNIT,FILES(1))
            CALL ZSET (CUNIT,CUNIT,FUNIT(FILX))
            CALL ZSET (CMLEVL,CMLEVL,I0)
            CALL ZOPEN (IFLTAB(1,ID),FILNAM,
     O                  RET)
            IF (RET .EQ. 0) THEN
              DSSOPN(ID)= 1
            END IF
          END IF
          RETCOD= RETCOD+ IABS(RET)
        END IF
 40   CONTINUE
C
      IF (FILES(10) .EQ. 0) THEN
C       tss file
        FILX= 0
        DO 50 I= 1, NUMFIL
          IF (FTYPE(I) .EQ. 'TSS') THEN
            FILX= I
          END IF
 50     CONTINUE
C
        IF (FILX .GT. 0) THEN
          CALL ATTACH (I0,CTSS,FNAME(FILX),CNOP,
     O                 FILNAM,ISTAT)
          IF (ISTAT .NE. -1) THEN
C           default name didn't change - reset to avoid truncation at 30
C           characters
            FILNAM= FNAME(FILX)
          END IF
C
          IF (ISTAT .LE. 0) THEN
C           try to open file
            FILES(10)= FUNIT(FILX)
            ACC=  'DIRECT'
            FRM=  'UNFORMATTED'
            STAT= 'OLD'
            BASE     = 512
            CALL XGTRCL (BASE,
     O                   RCL)
            CALL FILOPN
     I                  (ACC,FRM,RCL,STAT,FILES(10),FILNAM,
     O                   RET)
          END IF
          RETCOD= RETCOD+ RET
        END IF
      END IF
C
C     initialize message output storage
      CALL OMSINI
C
C     binary output files
      IF (USRFL.GT.0) THEN
C       neg unit number used to indicate dont want to open bin file
        DO 60 I= 1, NUMFIL
          IF (FTYPE(I) .EQ. 'BINO ') THEN
C           construct file unit character key
            IF (FUNIT(I) .LE. 9) THEN
              WRITE (CKEY,2020) FUNIT(I)
            ELSE IF (FUNIT(I) .LE. 99) THEN
              WRITE (CKEY,2030) FUNIT(I)
            END IF
C
            CALL ATTACH (I0,CKEY,FNAME(I),CNOP,
     O                   FILNAM,ISTAT)
            IF (ISTAT .NE. -1) THEN
C             default name didn't change - reset to avoid truncation at 30
C             characters
              FILNAM= FNAME(I)
            END IF
C
            IF (ISTAT .LE. 0) THEN
C             try to open file
              ACC=  'SEQUENTIAL'
              FRM=  'UNFORMATTED'
              RCL = 0
              STAT= 'UNKNOWN'
              CALL FILOPN
     I                    (ACC,FRM,RCL,STAT,FUNIT(I),FILNAM,
     O                     RET)
              IF (RET.EQ.0) THEN
C               write an endfile to clear out anything existing
                ENDFILE (FUNIT(I))
                CLOSE (FUNIT(I))
                CALL FILOPN
     I                      (ACC,FRM,RCL,STAT,FUNIT(I),FILNAM,
     O                       RET)
              END IF
            END IF
            RETCOD= RETCOD+ RET
          END IF
 60     CONTINUE
      END IF
C
C     other input/output files
      DO 70 I= 1, NUMFIL
        IF (FTYPE(I) .EQ. '     ') THEN
C         construct file unit character key
          IF (FUNIT(I) .LE. 9) THEN
            WRITE (CKEY,2020) FUNIT(I)
          ELSE IF (FUNIT(I) .LE. 99) THEN
            WRITE (CKEY,2030) FUNIT(I)
          END IF
C
          CALL ATTACH (I0,CKEY,FNAME(I),CNOP,
     O                 FILNAM,ISTAT)
          IF (ISTAT .NE. -1) THEN
C           default name didn't change - reset to avoid truncation at 30
C           characters
            FILNAM= FNAME(I)
          END IF
C
          IF (ISTAT .LE. 0) THEN
C           try to open file
            ACC=  'SEQUENTIAL'
            FRM=  'FORMATTED'
            RCL=   132
            STAT= 'UNKNOWN'
            CALL FILOPN
     I                  (ACC,FRM,RCL,STAT,FUNIT(I),FILNAM,
     O                   RET)
          END IF
          RETCOD= RETCOD+ RET
        END IF
 70   CONTINUE
C
C     do pltgen files, if vax
      CALL XPLFRM
     O           (FRM)
      IF (FRM .EQ. 'FORMATTED,CARRIAGECONTROL=LIST') THEN
C       yes, this is a vax, so process
        DO 80 I= 1, NUMFIL
          IF (FTYPE(I) .EQ. 'PLTGEN') THEN
C           this is a pltgen file
            ACC = 'SEQUENTIAL'
            RCL = 132
            STAT= 'UNKNOWN'
            CALL FILOPN
     I                  (ACC,FRM,RCL,STAT,FUNIT(I),FNAME(I),
     O                   RET)
            RETCOD= RETCOD + RET
          ENDIF
 80     CONTINUE
      END IF
C
C     do unformatted dtsf/ptsf files
      DO 90 I= 1, NUMFIL
        IF (FTYPE(I) .EQ. 'FEQTSF') THEN
C         this is an unformatted file
          ACC = 'SEQUENTIAL'
          FRM = 'UNFORMATTED'
          RCL = 0
          STAT= 'REPLACE'
          CALL FILOPN
     I                (ACC,FRM,RCL,STAT,FUNIT(I),FNAME(I),
     O                 RET)
          RETCOD= RETCOD + RET
        END IF
 90   CONTINUE
C
C     do state variable status file
      DO 100 I= 1, NUMFIL
        IF (FTYPE(I) .EQ. 'STASAV') THEN
C          WRITE(99,*) 'HFILES:found STASAV:',FUNIT(I)
          FILES(9) = FUNIT(I)
          ACC = 'SEQUENTIAL'
          FRM = 'FORMATTED'
          RCL = 0
          STAT= 'OLD'
          CALL FILOPN
     I                (ACC,FRM,RCL,STAT,FUNIT(I),FNAME(I),
     O                 RET)
        END IF
 100  CONTINUE
C
C     do update file
      DO 110 I= 1, NUMFIL
        IF (FTYPE(I) .EQ. 'UPDATE') THEN
C          WRITE(99,*) 'HFILES:found UPDATE:',FUNIT(I)
          FILES(8) = FUNIT(I)
          ACC = 'SEQUENTIAL'
          FRM = 'FORMATTED'
          RCL = 0
          STAT= 'OLD'
          CALL FILOPN
     I                (ACC,FRM,RCL,STAT,FUNIT(I),FNAME(I),
     O                 RET)
        END IF
 110  CONTINUE
C
C     do PEST supplemental file
      DO 120 I= 1, NUMFIL
        IF (FTYPE(I) .EQ. 'PESTSU') THEN
          FILES(7)= FUNIT(I)
          ACC     = 'SEQUENTIAL'
          FRM     = 'FORMATTED'
          RCL     = 0
          STAT    = 'OLD'
          CALL FILOPN
     I                (ACC,FRM,RCL,STAT,FUNIT(I),FNAME(I),
     O                 RET)
        END IF
 120  CONTINUE

C
      CALL ATTEND
C
      RETURN
      END
C
C
C
      SUBROUTINE   FILOPN
     I                    (ACC,FRM,RCL,STAT,UNITNO,FILNAM,
     O                     RETCOD)
C
C     + + + PURPOSE + + +
C     Open a file for HSPF.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       RCL,UNITNO,RETCOD
      CHARACTER*12  ACC,STAT
      CHARACTER*30  FRM
      CHARACTER*64  FILNAM
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ACC    - access code: "sequential" or "direct"
C     FRM    - format code: "formatted" or "unformatted"
C     RCL    - record length
C     STAT   - status code: "old", "new", "unknown", or "scratch"
C     UNITNO - file unit number
C     FILNAM - file name to be opened
C     RETCOD - error return code
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       IOS
C
C     + + + OUTPUT FORMATS + + +
 2000   FORMAT
     $         ('**************************************',
     $          '**************************************',
     $        /,'* ERROR OPENING FOLLOWING FILE:',44X,'*',
     $        /,'*',74X,'*',
     $        /,'* UNIT   = ',I5,59X,'*',
     $        /,'* FILE   = ',A64,'*',
     $        /,'* STATUS = ',A12,52X,'*',
     $        /,'* ACCESS = ',A12,52X,'*',
     $        /,'* FORMAT = ',A12,52X,'*',
     $        /,'* RECLEN = ',I5,59X,'*',
     $        /,'* IOS    = ',I5,59X,'*',
     $        /,'**************************************',
     $          '**************************************')
C
C     + + + END SPECIFICATIONS + + +
C
      RETCOD = 0
      IF (ACC .EQ. 'SEQUENTIAL  ') THEN
C       sequential
        OPEN (UNIT=UNITNO,FILE=FILNAM,STATUS=STAT,ACCESS=ACC,
     $        FORM=FRM,ERR=10,IOSTAT=IOS)
C
      ELSE
C       direct access
        IF (STAT .EQ. 'SCRATCH     ') THEN
C         scratch file
          OPEN (UNIT=UNITNO,STATUS=STAT,ACCESS=ACC,
     $          FORM=FRM,RECL=RCL,ERR=10,IOSTAT=IOS)
        ELSE
C         permanent file
          OPEN (UNIT=UNITNO,FILE=FILNAM,STATUS=STAT,ACCESS=ACC,
     $          FORM=FRM,RECL=RCL,ERR=10,IOSTAT=IOS)
        END IF
      END IF
C
      GO TO 20
 10   CONTINUE
C       write error message to terminal
        RETCOD = IOS
        WRITE (*,2000) UNITNO,FILNAM,STAT,ACC,FRM,RCL,IOS
 20   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   HSCKFL
     I                    (UNITNO)
C
C     + + + PURPOSE + + +
C     Check whether a file encountered in the run interpreter
C     is open.  If not, then open the file with the name
C     'hspfxx.dat', where xx is the file unit number.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      UNITNO
C
C     + + + ARGUMENT DEFINITIONS + + +
C     UNITNO  - fortran unit number for file
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      RCL
C
C     + + + EXTERNALS + + +
      EXTERNAL        HSCKFLX
C
C     + + + END SPECIFICATIONS + + +
C
      RCL = 132
      CALL HSCKFLX (RCL,UNITNO)
C
      RETURN
      END
C
C
C
      SUBROUTINE   HSCKFLX
     I                     (RCL,UNITNO)
C
C     + + + PURPOSE + + +
C     Check whether a file encountered in the run interpreter
C     is open.  If not, then open the file with the name
C     'hspfxx.dat', where xx is the file unit number.
C     Exteneded to include RCL as input argument.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      UNITNO,RCL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     UNITNO  - fortran unit number for file
C     RCL     - record length
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      RET
      LOGICAL      OPND
      CHARACTER*12 ACC,STAT
      CHARACTER*30 FRM
      CHARACTER*64 FILNAM
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT ('hspf0',I1,'.dat')
 2010 FORMAT ('hspf',I2,'.dat')
 2020 FORMAT ('hspf0',I1,'.bin')
 2030 FORMAT ('hspf',I2,'.bin')
C
C     + + + EXTERNALS + + +
      EXTERNAL        FILOPN, XPLFRM
C
C     + + + END SPECIFICATIONS + + +
C
C     check for pltgen file
      IF (UNITNO .GT. 9999) THEN
C       pltgen file
        UNITNO= UNITNO - 9999
C       check how format should read
        CALL XPLFRM
     O             (FRM)
      ELSE IF (UNITNO .LT. 0) THEN
C       dtsf or ptsf file
        UNITNO= UNITNO+ 9999
        FRM= "UNFORMATTED"
      ELSE
        IF (RCL.EQ.0) THEN
C         binary output file
          FRM= "UNFORMATTED"
        ELSE
C         regular sequential formatted file
          FRM= 'FORMATTED'
        END IF
      END IF
C
      INQUIRE (UNIT=UNITNO,OPENED=OPND)
C
      IF (OPND) THEN
C       a file is open with this unit number - do nothing
      ELSE
C       file not open - open a sequential file with name
C       hspfxx.dat where xx = unit number
        IF (RCL.EQ.0) THEN
          IF (UNITNO .LE. 9) THEN
            WRITE (FILNAM,2020) UNITNO
          ELSE IF (UNITNO .LE. 99) THEN
            WRITE (FILNAM,2030) UNITNO
          END IF
        ELSE
          IF (UNITNO .LE. 9) THEN
            WRITE (FILNAM,2000) UNITNO
          ELSE IF (UNITNO .LE. 99) THEN
            WRITE (FILNAM,2010) UNITNO
          END IF
        END IF
C
        ACC = 'SEQUENTIAL'
        STAT= 'UNKNOWN'
        CALL FILOPN
     I              (ACC,FRM,RCL,STAT,UNITNO,FILNAM,
     O               RET)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   RDFLBK
     I                   (USRFL,MESSFL,
     M                    FILFND,
     O                    NUMFIL,FTYPE,FNAME,FUNIT,RETCOD)
C
C     + + + PURPOSE + + +
C     Read HSPF Files Block and store entries in arrays.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       USRFL,NUMFIL,FUNIT(100),FILFND,RETCOD,MESSFL
      CHARACTER*6   FTYPE(100)
      CHARACTER*64  FNAME(100)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     USRFL   - Fortran unit number for user's input file
C     FILFND  - flag indicating current position in file:
C               0 : FILES keyword not yet read (used by HSPF)
C               1 :               already read (used by HYDRO2)
C     FILES   - integer array of HSPF file unit numbers
C     RETCOD  - error return code
C     MESSFL  - message file unit number
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       FIL,I,I3,I80,SCLU,SGRP,IOPT
      CHARACTER*80  RECBUF,BLANKL
      CHARACTER*12  FILSTR
      CHARACTER*1   BLANK1(80),BLANK,CMNT(3),RECBF1(80)
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (A80)
 1010 FORMAT (A10)
 1020 FORMAT (A6,2X,I5,3X,A64)
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE  (BLANKL,BLANK1(1))
      EQUIVALENCE  (RECBUF,RECBF1(1))
C
C     + + + FUNCTIONS + + +
      INTEGER  STRFND
C
C     + + + EXTERNALS + + +
      EXTERNAL STRFND,HDMEST
C
C     + + + END SPECIFICATIONS + + +
C
      RETCOD= 0
      I3    = 3
      I80   = 80
      SCLU  = 201
      BLANK = ' '
      DO 10 I = 1, 80
        BLANK1(I) = BLANK
 10   CONTINUE
C
      CMNT(1)= '*'
      CMNT(2)= '*'
      CMNT(3)= '*'
      NUMFIL= 0
 20   CONTINUE
        READ (USRFL,1000,END=50) RECBUF
C       check whether a comment line or blank
        IF ((STRFND(I80,RECBF1,I3,CMNT) .EQ. 0) .AND.
     #      (RECBUF .NE. BLANKL)) THEN
C         noncomment line - process
          READ (RECBUF,1010) FILSTR
          IF (FILSTR .EQ. 'FILES       ') THEN
C           found FILES block
            FILFND= 1
            GO TO 20
          ELSE IF (FILSTR .EQ. 'END FILES ') THEN
C           found end of FILES block
            IF (FILFND.EQ.0) THEN
C             never found start of files block, fatal error
              SGRP = 60
              IOPT = 1
              CALL HDMEST (IOPT,MESSFL,SCLU,SGRP)
              RETCOD = 1
            END IF
            FILFND= 2
          END IF
          IF (FILFND .EQ. 1 ) THEN
C           currently in FILES block - process a line
            FIL= NUMFIL + 1
            NUMFIL= FIL
            READ (RECBUF,1020,ERR=30) FTYPE(FIL),FUNIT(FIL),FNAME(FIL)
C
            GO TO 40
 30         CONTINUE
              WRITE (*,*) ' ERROR READING ENTRY IN FILES BLOCK'
 40         CONTINUE
          END IF
        END IF
        GO TO 60
 50     CONTINUE
          IF (FILFND.EQ.0) THEN
            IOPT = 1
            SGRP = 61
            CALL HDMEST (IOPT,MESSFL,SCLU,SGRP)
            RETCOD = 2
          ELSE IF (FILFND.EQ.1) THEN
            IOPT = 1
            SGRP = 62
            CALL HDMEST (IOPT,MESSFL,SCLU,SGRP)
            RETCOD = 3
          END IF
 60     CONTINUE
      IF (FILFND.LE.1 .AND. NUMFIL.LT.100 .AND. RETCOD.EQ.0) GO TO 20
C
      RETURN
      END
C
C
C
      SUBROUTINE   FILCLO
C
C     + + + PURPOSE + + +
C     Close files in HSPF Files Block (except wdm files)
C
C     + + + COMMON BLOCKS + + +
      INCLUDE       'cfilbk.inc'
      INCLUDE       'cifltb.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,FNUM
      LOGICAL     OPFG
      CHARACTER*3 FTYP
C
C     + + + END SPECIFICATIONS + + +
C
C     WRITE(99,*) 'HFILES:FILCLO:',NUMFIL
      DO 100 I = 1,NUMFIL
        FNUM = FUNIT(I)
        FTYP = FTYPE(I)(1:3)
        IF (FTYP .NE. 'WDM') THEN
          INQUIRE(UNIT=FNUM,OPENED=OPFG)
          IF (OPFG) THEN
            CLOSE(UNIT=FNUM)
C           WRITE(99,*) 'HFILES:FILCLO:close unit:',FNUM
          ELSE
C           WRITE(99,*) 'HFILES:FILCLO:not open  :',FNUM
          END IF
        ELSE
C         WRITE(99,*) 'HFILES:FILCLO:wdm not cl:',FNUM
        END IF
100   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   FILTSF
C
C     + + + PURPOSE + + +
C     Close and reopen tsf files
C
C     + + + COMMON BLOCKS + + +
      INCLUDE       'cfilbk.inc'
      INCLUDE       'cifltb.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I,RCL,RET
      CHARACTER*12  ACC,STAT
      CHARACTER*30  FRM
C
C     + + + EXTERNALS + + +
      EXTERNAL  FILOPN
C
C     + + + END SPECIFICATIONS + + +
C
      DO 100 I = 1,NUMFIL
        IF (FTYPE(I) .EQ. 'FEQTSF') THEN
C         this is an feq tsf file
          CLOSE(UNIT=FUNIT(I))
          ACC = 'SEQUENTIAL'
          FRM = 'UNFORMATTED'
          RCL = 0
          STAT= 'UNKNOWN'
          CALL FILOPN
     I                (ACC,FRM,RCL,STAT,FUNIT(I),FNAME(I),
     O                 RET)
        END IF
100   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   REPFIL
     I                   (OLDFIL,NEWFIL)
C
C     + + + PURPOSE + + +
C     Replace a file name in common block with a new name from a new
C     scenario.  Called from NEWAQT in GenScn, resides here to use
C     the files common block.
C
C     + + + DUMMY ARGUMENTS + + +
      CHARACTER*64  OLDFIL,NEWFIL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     OLDFIL  - name of file in old scenario
C     NEWFIL  - name of file in new scenario
C
C     + + + COMMON BLOCKS + + +
      INCLUDE       'cfilbk.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I
C
C     + + + END SPECIFICATIONS + + +
C
      I = 1
 10   CONTINUE
        IF (FNAME(I).EQ.OLDFIL) THEN
C         this is the one to change
          FNAME(I) = NEWFIL
          I = 101
        ELSE
          I = I + 1
        END IF
      IF (I.LE.100) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE   HSVRSN
C
C     + + + PURPOSE + + +
C     Dummy routine to include unix what version information for the
C     hspf library.
C
C     + + + LOCAL VARIABLES + + +
      CHARACTER*64  VERSN
C
C     + + + END SPECIFICATIONS + + +
C
      INCLUDE 'fversn.inc'
C
      RETURN
      END
