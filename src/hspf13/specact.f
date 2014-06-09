C
C
C
      SUBROUTINE   PSPECL
     I                    (KEYST,KEYND,NDAMON,SDATIM,EDATIM,SPOUT,
     I                     STFIL,
     M                     RUNWID,
     O                     CONDFG)
C
C     + + + PURPOSE + + +
C     Process the Special Actions block from UCI file.
C
C     + + + HISTORY + + +
C     12/17/2004 jlk&pbd - added call to new subroutine to handle
C         MFACT change action
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    KEYST,KEYND,NDAMON(12),SDATIM(5),EDATIM(5),SPOUT,
     $           STFIL,RUNWID,CONDFG
C
C     + + + ARGUMENT DEFINITIONS + + +
C     KEYST  - starting record number
C     KEYND  - ending record number
C     NDAMON - no. of days in each month of calendar year
C     SDATIM - starting date/time
C     EDATIM - ending date/time
C     SPOUT  - runtime Special Action output level
C     STFIL  - unit number of status file
C     RUNWID - maximum run span width allowed by user-defined variable
C              quantities - 0 if no restrictions
C     CONDFG - flag indicating whether conditinal special actions are
C              used - 1 if yes, 2 if no
C
C     + + + COMMON BLOCKS- SPEC + + +
      INCLUDE     'cspec.inc'
C     special action file in memory
      INCLUDE     'pspins.inc'
      INCLUDE     'cspins.inc'
C
C     + + + PARAMETERS + + +
      INTEGER      MXSPBF,MXVACC
      PARAMETER   (MXSPBF=200000)
      PARAMETER   (MXVACC=50)
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      MESSU,MSGFL,SCLU,SGRP,BGRP,I1,ITYP,I,INITFG,CLEN,
     $             CONT,LOCDIR,NVNMS,N,J,NVACC,VACFST(10),VACCNT(10),
     $             TOPTYP,ERRFLG,DUMSUB(3),VRFADD(MXVACC),
     $             VACADD(MXVACC),SPOS,SPBF(LENPSP,MXSPBF),
     $             SPBDAT(MXSPBF),SPBPOS(MXSPBF),IOPT
      CHARACTER*6  DUMNAM
      CHARACTER*15 CRFADD(2),CACADD(2)
      CHARACTER*80 CHSTR
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (SPBF,SPBFR)
      REAL         SPBFR(LENPSP,MXSPBF)
      EQUIVALENCE (CHSTR,CHSTR1)
      CHARACTER*1  CHSTR1(80)
C
C     + + + EXTERNALS + + +
      EXTERNAL     ZIPI,HDMES2,WMSGTT,MKADDR,PSPUCI,PSPKEY,PSPUMK
C
C     + + + DATA INITIALIZATIONS + + +
      DATA I1/1/
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (4X,A6,7I10)
 1010 FORMAT (I10,10X,I10)
 1020 FORMAT (3(A6,3I4,1X,I5,1X))
 1030 FORMAT (5X,2I5)
 1040 FORMAT (4(A15,1X))
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' ',132('='),
     $        /,' PROCESSING SPEC-ACTIONS BLOCK')
 2010 FORMAT (/,' ',132('='),
     $        /,' SPEC-ACTIONS BLOCK NOT FOUND')
 2020 FORMAT (/,' NO SPECIAL ACTION ENTRIES FOUND TO PROCESS')
 2030 FORMAT (/,' FINISHED PROCESSING SPEC-ACTIONS BLOCK',
     $        /,' ',132('='))
C
C     + + + END SPECIFICATIONS + + +
C
      MESSU= FILE(1)
      MSGFL= FILE(15)
C
      SCLU= 203
      BGRP= 100
C
      CONDFG= 0
C
      IF (SPOUT .GE. 10) THEN
C       echo keys
        WRITE (MESSU,*) 'KEYST,KEYND:',KEYST,KEYND
      END IF
C
      IF (KEYST .LE. 0) THEN
C       no special action block to process
        IF (OUTLEV .GT. 0) THEN
C         message to that effect
          WRITE (MESSU,2010)
        END IF
      ELSE
C       special action uci instructions available
        IOPT= 2
        ITYP= 9
        CALL HDMES2 (IOPT,ITYP,I1)
        IF (OUTLEV .GT. 0) THEN
C         processing message
          WRITE (MESSU,2000)
        END IF
C
        IF (SPOUT .GE. 10) THEN
C         echo dates
          WRITE (MESSU,*) ' SDATIM:',SDATIM
          WRITE (MESSU,*) ' EDATIM:',EDATIM
        END IF
      END IF
C
      IF ( (KEYST .GE. 1) .OR. (STFIL .GE. 1) ) THEN
C       either a block to process or need variable library for
C       status file
C
C       read info about which operations do special actions
        I= 0
        SGRP= 51
        INITFG= 1
 10     CONTINUE
          CLEN= 80
          I= I+ 1
          CALL WMSGTT (MSGFL,SCLU,SGRP,INITFG,
     M                 CLEN,
     O                 CHSTR1,CONT)
          READ (CHSTR,1000) OPTYLB(I),LOSPFL(I),LOSPST(I),LOSPKY(I),
     $                      LOSPLV(I),LOSPNU(I),LONAM(I),LONUM(I)
          INITFG= 0
        IF (CONT .EQ. 1) GO TO 10
C
C       valid variable names from msgfl
        SGRP=  52
        INITFG= 1
        CLEN=  80
        CALL WMSGTT (MSGFL,SCLU,SGRP,INITFG,
     M               CLEN,
     O               CHSTR1,CONT)
        READ (CHSTR,1010) NVNMS,LOCDIR
        IF (SPOUT .GE. 10) THEN
C         echo info
          WRITE (MESSU,*) '  NVNMS,LOCDIR:',NVNMS,LOCDIR
        END IF
C
C       LOCDIR specs method(s) available for spec. actions input
C          0 - variable name required;
C          1 - either variable name or address required;
C          2 - address required
C
        IF (LOCDIR .LE. 1) THEN
C         read special actions variable name library from msgfl
          SGRP= 53
          INITFG= 1
          DO 20 N= 1, NVNMS, 3
            CLEN= 80
            CALL WMSGTT (MSGFL,SCLU,SGRP,INITFG,
     M                   CLEN,
     O                   CHSTR1,CONT)
            READ (CHSTR,1020) (VNAMLB(I),(VDIM(J,I),J=1,3),VLOC(I),
     $                         I= N, N+ 2)
            INITFG= 0
            IF ( (CONT .EQ. 0) .AND. (SGRP .EQ. 53) ) THEN
C             go to next screen group
              SGRP= 54
              INITFG= 1
            END IF
 20       CONTINUE
        END IF
      END IF
C
      IF (KEYST .GE. 1) THEN
C       block is present - continue processing
C
C       read special-action accumulator info and library
        SGRP= 55
        INITFG= 1
        NVACC= 0
        I= 0
 30     CONTINUE
          CLEN= 80
          I= I+ 1
          CALL WMSGTT (MSGFL,SCLU,SGRP,INITFG,
     M                 CLEN,
     O                 CHSTR1,CONT)
          READ (CHSTR,1030) VACFST(I),VACCNT(I)
          NVACC= NVACC+ VACCNT(I)
          INITFG= 0
        IF (CONT .EQ. 1) GO TO 30
C
        SGRP= 56
        INITFG= 1
        DO 50 N= 1, NVACC, 2
          CLEN= 80
          CALL WMSGTT (MSGFL,SCLU,SGRP,INITFG,
     M                 CLEN,
     O                 CHSTR1,CONT)
          READ (CHSTR,1040) CRFADD(1),CACADD(1),CRFADD(2),CACADD(2)
C
C         process each address pair
          DO 40 I= 1, 2
            J= N+ I- 1
            IF (J .LE. NVACC) THEN
C             process this pair
C
C             get reference address
              TOPTYP= 0
              ERRFLG= 0
              CALL MKADDR (LOCDIR,CRFADD(I),MESSU,MSGFL,SCLU,BGRP,
     M                     TOPTYP,ERRFLG,
     O                     DUMNAM,DUMSUB,VRFADD(J))
              IF (ERRFLG .EQ. 0) THEN
C               get accumulator address
                CALL MKADDR (LOCDIR,CACADD(I),MESSU,MSGFL,SCLU,BGRP,
     M                       TOPTYP,ERRFLG,
     O                       DUMNAM,DUMSUB,VACADD(J))
              END IF
              IF (ERRFLG .NE. 0) THEN
C               don't use this reference
                VRFADD(J)= 0
                VACADD(J)= 0
              END IF
            END IF
 40       CONTINUE
          INITFG= 0
 50     CONTINUE
C
C       read and process lines from special actions block
C
        CALL PSPUCI (MESSU,MSGFL,SCLU,KEYST,KEYND,LOCDIR,SDATIM,EDATIM,
     I               NDAMON,MXSPBF,MXVACC,VACFST,VACCNT,VRFADD,VACADD,
     I               LENPSP,SPOUT,
     M               RUNWID,CONDFG,
     O               SPOS,SPBF,SPBFR,SPBDAT)
C
        IF (SPOS.GT.0 .OR. SPUMPT.GT.0) THEN
C         at least one spec act is in memory.
          IF (SPOS.GT.0) THEN
C           sort special actions and generate keys
            CALL PSPKEY (MESSU,MSGFL,SCLU,SPOS,MXSPBF,SPBDAT,SPBF,
     I                   LENPSP,SPOUT,
     O                   SPBPOS)
          END IF
          IF (SPUMPT.GT.0) THEN
C           sort mult update instructions
            CALL PSPUMK (SDATIM,NDAMON)
          END IF
        ELSE
C         no entries found to process
          IF (OUTLEV .GT. 0) THEN
C           message to this effect
            WRITE (MESSU,2020)
          END IF
        END IF
      END IF
C
      IF (SPOUT .GE. 10) THEN
C       echo run width
        WRITE (MESSU,*) ' runwid at end of pspecl',RUNWID
      END IF
C
C     all done special actions
      IF (OUTLEV .GT. 0) THEN
C       end processing message
        WRITE (MESSU,2030)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PSPUCI
     I                    (MESSU,MSGFL,SCLU,KEYST,KEYND,LOCDIR,SDATIM,
     I                     EDATIM,NDAMON,MXSPBF,MXVACC,VACFST,VACCNT,
     I                     VRFADD,VACADD,LLNPSP,SPOUT,
     M                     RUNWID,CONDFG,
     O                     SPOS,SPBF,SPBFR,SPBDAT)
C
C     + + + PURPOSE + + +
C     Read and process Special Actions lines.
C
C     + + + HISTORY + + +
C     4/14/2005 jlk - added initialization for I0
C     12/17/2004 jlk&pbd - added call to new subroutine to handle
C         MFACT change action
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    MESSU,MSGFL,SCLU,KEYST,KEYND,LOCDIR,SDATIM(5),
     $           EDATIM(5),NDAMON(12),MXSPBF,MXVACC,VACFST(10),
     $           VACCNT(10),VRFADD(MXVACC),VACADD(MXVACC),LLNPSP,SPOUT,
     $           RUNWID,CONDFG,SPOS,SPBF(LLNPSP,MXSPBF),SPBDAT(MXSPBF)
      REAL       SPBFR(LLNPSP,MXSPBF)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSU  - unit number to write messages on
C     MSGFL  - unit number for file containing error messages
C     SCLU   - cluster in file containing error text
C     KEYST  - starting record number
C     KEYND  - ending record number
C     LOCDIR - specs method(s) available for spec. actions input
C     SDATIM - starting date/time
C     EDATIM - ending date/time
C     NDAMON - no. of days in each month of calendar year
C     MXSPBF - max size of special actions buffer
C     MXVACC - maximum number of variable accumulator references
C     VACFST - first variable accumulator reference for each operation type
C     VACCNT - number of variable accumulator references for each operation type
C     VRFADD - variable accumulator reference addresses
C     VACADD - variable accumulator addresses
C     LLNPSP - local length of special action in buffer
C     SPOUT  - runtime Special Action output level
C     RUNWID - maximum run span width allowed by user-defined variable
C              quantities - 0 if no restrictions
C     CONDFG - flag indicating whether conditinal special actions are
C              used - 1 if yes, 2 if no
C     SPOS   - position in special actions instr buffer
C     SPBF   - special action instruction buffer (integer version)
C     SPBFR  - special action instruction buffer (real version)
C     SPBDAT - special action instruction date
C
C     + + + COMMON BLOCKS- SPEC + + +
      INCLUDE     'cspec.inc'
C     special action distributions
      INCLUDE     'pspdst.inc'
      INCLUDE     'cspdst.inc'
C     special action file in memory
      INCLUDE     'pspins.inc'
      INCLUDE     'cspins.inc'
C     user defined variable names
      INCLUDE     'pspuvr.inc'
      INCLUDE     'cspuvr.inc'
C     special action conditions
      INCLUDE     'pspcnd.inc'
      INCLUDE     'cspcnd.inc'
C     user defined variable quantity definitions
      INCLUDE     'pspvqd.inc'
      INCLUDE     'cspvqd.inc'
C
C     + + + PARAMETERS + + +
      INTEGER      MXBKLV
      PARAMETER   (MXBKLV=25)
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      CURBLK,CURLVL,PREBLK(MXBKLV),ELSEFG(MXBKLV),I0,DELT,
     $             KEY,I,STWORD,SGRP,FIRSTH,NUMHDR,ALLHDR,ALLFTR,LREPT,
     $             IOPT,DCNT
      CHARACTER*80 UCIBUF
C
C     + + + FUNCTIONS + + +
      INTEGER     CKNBLV
C
C     + + + EXTERNALS + + +
      EXTERNAL    ZIPI,GETUCI,PSPUVQ,PSPCON,PSPDIS,PSPUVN,PSPACT,OMSG
      EXTERNAL    CKNBLV,PSPIPS,PSPHDR,HDMES3,PSPUML
C
C     + + + DATA INITIALIZATIONS + + +
      DATA I0/0/

C     + + + END SPECIFICATIONS + + +
C
C     start with no actions stored
      SPOS= 0
      FIRSTH= 0
      NUMHDR= 0
      ALLHDR= 0
      ALLFTR= 0
      LREPT= 1
C
C     no conditions stored
      NCOND= 0
      NCHAIN= 0
      NBLOCK= 0
      CALL ZIPI (MXSPCH,I0,CHNUVQ)
C
C     initialize logical blocks
      CURBLK= 0
      CURLVL= 0
      CALL ZIPI (MXBKLV,I0,PREBLK)
      CALL ZIPI (MXBKLV,I0,ELSEFG)
C
C     no distributions stored
      DCNT= 0
      CALL ZIPI (MXSPDS,I0,SPDCNT)
C
C     no user defined special actions
      SPUCNT= 0
C     info about first referenced variable starts at first position
      SPUPOS(1)= 1
C
C     no user defined variable quantities
      NVQD= 0
      CALL ZIPI (MXSPVQ,I0,
     O           UVQOPX)
C
C     no update mfact instructions stored
      SPUMPT = 0
C
C     set run time step
      DELT= GRPTAB(3,1)
C
C     where to start
      KEY= KEYST
C     output header position
      IOPT = 4
C
C     begin whiledo to read lines
 10   CONTINUE
C
C       read a uci entry
        CALL GETUCI (I0,
     M               KEY,
     O               UCIBUF)
        IF (KEY .NE. KEYND) THEN
C         this is not the last line in block
          IF (SPOUT .GE. 10) THEN
C           echo key
            WRITE (MESSU,*)
              WRITE (MESSU,*) 'read UCI:',KEY
          END IF
C
          IF (UCIBUF(1:4) .EQ. 'MULT') THEN
C           this is an mfact update line
            CALL HDMES3 (IOPT,'MULT')
            CALL PSPUML (UCIBUF,MESSU,OUTLEV,MSGFL,SCLU,SPOUT,
     M                   ECOUNT)
          ELSE
C           find first word in case line is a free-form conditional line
            I= 80
            STWORD= CKNBLV (I,UCIBUF)
            IF (STWORD .LT. 1) THEN
C             set dummy stword on blank line
              STWORD= 1
            END IF
C
            IF (UCIBUF(STWORD:STWORD+2) .EQ. '@@@') THEN
C             special action echo file header for next action line
              CALL PSPHDR (UCIBUF,
     M                     FIRSTH,NUMHDR,ALLHDR)
C
            ELSE IF (UCIBUF(3:8) .EQ. 'UVQUAN') THEN
C             user defined variable quantity name
              CALL HDMES3 (IOPT,'UVQUAN')
              CALL PSPUVQ (UCIBUF,MESSU,OUTLEV,MSGFL,SCLU,LOCDIR,
     I                     DELT,MAXOPN,OPNTAB,NOPNS,SPOUT,OPTYL1,
     M                     ECOUNT,RUNWID)
C
            ELSE IF ( (UCIBUF(STWORD:STWORD+2) .EQ. 'IF ') .OR.
     $                (UCIBUF(STWORD:STWORD+3) .EQ. 'ELSE') .OR.
     $                (UCIBUF(STWORD:STWORD+5) .EQ. 'END IF') ) THEN
C             conditional
              CALL HDMES3 (IOPT,'CONDITIONAL')
              CALL PSPCON (OUTLEV,MESSU,MSGFL,SCLU,MXBKLV,STWORD,LREPT,
     I                     LLNPSP,MXSPBF,SPOS,
     M                     SPBF,ECOUNT,UCIBUF,KEY,CURBLK,CURLVL,PREBLK,
     M                     ELSEFG,ALLFTR,RUNWID)
              CONDFG= 1
C
            ELSE IF (UCIBUF(3:8) .EQ. 'DISTRB') THEN
C             distribute action definition
              CALL HDMES3 (IOPT,'DISTRB')
              CALL PSPDIS (UCIBUF,MESSU,MSGFL,SCLU,MXSPDS,OUTLEV,SPOUT,
     M                     DCNT,SPDCNT,SPDTST,SPDTCD,SPDDFG,SPDFRC,
     M                     ECOUNT)
C
            ELSE IF (UCIBUF(3:8) .EQ. 'UVNAME') THEN
C             user defined action name
              CALL HDMES3 (IOPT,'UVNAME')
              CALL PSPUVN (MESSU,MSGFL,SCLU,OUTLEV,LOCDIR,SPOUT,
     M                     ECOUNT,UCIBUF,KEY)
C
            ELSE
C             old style action
              CALL HDMES3 (IOPT,'CLASSIC')
              CALL PSPACT (UCIBUF,MESSU,MSGFL,SCLU,LOCDIR,SDATIM,
     I                     EDATIM,NDAMON,MXSPBF,MXSPDS,SPDCNT,DELT,
     I                     CURBLK,VACFST,VACCNT,MXVACC,VRFADD,VACADD,
     I                     SPOUT,LENPSP,
     M                     FIRSTH,NUMHDR,LREPT,SPOS,SPBF,SPBFR,SPBDAT,
     M                     RUNWID)
C
            END IF
          END IF
C
        END IF
      IF (KEY .NE. KEYND) GO TO 10
C
      IF (SPOUT .GE. 10) THEN
C       echo number of special actions
        WRITE (MESSU,*) 'finished processing special action entries',
     $                   SPOS
      END IF
C
C     check to make sure logic blocks were correctly closed
      IF ( (CURLVL .NE. 0) .OR. (CURBLK .NE. 0) ) THEN
C       error - ifs and endifs don't match up
        SGRP= 28
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             ECOUNT)
      END IF
C
      IF (NVQD .GE. 1) THEN
C       handle pipes for user-defined variable quantities
        CALL PSPIPS (MESSU,MSGFL,SCLU,SPOUT,
     M               RUNWID)
      END IF
C
C      write (99,*) 'numhdr,mxsphf',NUMHDR,' of',MXSPHF
C      write (99,*) 'spos  ,mxspin',SPOS,  ' of',MXSPIN
C      write (99,*) 'nvqd  ,mxspvq',NVQD,  ' of',MXSPVQ
C      write (99,*) 'ncond ,mxspcn',NCOND, ' of',MXSPCN
C      write (99,*) 'nblock,mxspbk',NBLOCK,' of',MXSPBK
C      write (99,*) 'nchain,mxspch',NCHAIN,' of',MXSPCH
C      IF (NBLOCK .GE. 1) THEN
C        I= BLKPOS(NBLOCK)+ BLKCNT(NBLOCK)- 1
C      ELSE
C        I= 0
C      END IF
C      write (99,*) 'nchref,mxspcr',I,     ' of',MXSPCR
C      write (99,*) 'spucnt,mxspuv',SPUCNT,' of',MXSPUV
C      IF (SPUCNT .GE. 1) THEN
C        I= SPUPOS(SPUCNT+1)
C      ELSE
C        I= 0
C      END IF
C      write (99,*) 'spuptr,mxspux',I,     ' of',MXSPUX
C      I= 0
C      DO 991 KEY= 1, NBLOCK
C        IF (BLKLVL(KEY) .GT. I) THEN
C          I= BLKLVL(KEY)
C        END IF
C 991  CONTINUE
C      write (99,*) 'deeplv,mxbklv',I,     ' of',MXBKLV
C
      RETURN
      END
C
C
C
      SUBROUTINE   PSPKEY
     I                    (MESSU,MSGFL,SCLU,SPOS,MXSPBF,SPBDAT,SPBF,
     I                     LLNPSP,SPOUT,
     O                     SPBPOS)
C
C     + + + PURPOSE + + +
C     Sort Special Actions for each operation and set OSV keys.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER MESSU,MSGFL,SCLU,SPOS,MXSPBF,SPBDAT(MXSPBF),LLNPSP,
     $        SPBF(LLNPSP,MXSPBF),SPOUT,SPBPOS(MXSPBF)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSU  - unit number to write messages on
C     MSGFL  - unit number for file containing error messages
C     SCLU   - cluster in file containing error text
C     SPOS   - position in special actions instr buffer
C     MXSPBF - max size of special actions buffer
C     SPBDAT - special action instruction date
C     SPBF   - special action instruction buffer
C     LLNPSP - local length of special action in buffer
C     SPOUT  - runtime Special Action output level
C     SPBPOS - special action sorted position
C
C     + + + COMMON BLOCKS- SPEC + + +
      INCLUDE     'cspec.inc'
C     special action file in memory
      INCLUDE     'pspins.inc'
      INCLUDE     'cspins.inc'
C     osv in scratch pad
      INCLUDE     'cmosv.inc'
      INCLUDE     'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      SPAKEY,SORTOP,TOPTYP,SPAKST,OSVKEY,OSVSIZ,XPOS,IPOS,
     $             TOPFST,TOPLST,FIT,ADDR,I,J,LOGBLK,I0,I1,CONDCK,SGRP,
     $             SKEY,DATED,UNDAT,PKEY,SPNUND
      CHARACTER*6  OPTYP
C
C     + + + FUNCTIONS + + +
      INTEGER      OPNNO
C
C     + + + EXTERNALS + + +
      EXTERNAL     ASRTI,GETOSV,OPNNO,OMSG,PUTOSV
C
C     + + + DATA INITIALIZATIONS + + +
      DATA I0,I1/0,1/
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (A4,A2)
C
C     + + + END SPECIFICATIONS + + +
C
C     no special actions saved
      SPAKEY= 0
C
      IF (SPOUT .GE. 10) THEN
C       echo header
        WRITE (MESSU,*)
        WRITE (MESSU,*) '------ sorting special actions -------'
      END IF
C     sort special action instructions in place by date
      SORTOP= 0
      CALL ASRTI (SORTOP,SPOS,SPBDAT,
     O            SPBPOS)
C
C     process special actions through each operation in the run
      SPWSIV= 0
      DO 100 OPNO= 1,NOPNS
        SPNUND= 0
        TOPTYP= OPNTAB(4,OPNO)
        IF (SPOUT .GE. 10) THEN
C         echo operaton info
          WRITE (MESSU,*)
          WRITE (MESSU,*) '*** OPNO  :',OPNO,TOPTYP,LOSPFL(TOPTYP)
        END IF
        IF (LOSPFL(TOPTYP) .GT. 0) THEN
C         this operation is capable of handling special actions
C         assume no special actions in this operation
          SPAKST= 0
C         how big is osv for this operation
          OSVKEY= OPNTAB(7,OPNO)
          OSVSIZ= (1 + OPNTAB(8,OPNO) - OSVKEY)*500
          IF (SPOUT .GE. 10) THEN
C           echo osv info
            WRITE (MESSU,*) '    OSVKEY:',OSVKEY,OSVSIZ
          END IF
C         read in the first osv-chunk for this operation
          CALL GETOSV (OSVKEY,OSVKEY,MAXOSV,
     O                 OSV)
C         loop thru instructions
          DO 30 XPOS= 1, SPOS
C           see if current operation fits in the given range
            IPOS= SPBPOS(XPOS)
            WRITE (OPTYP,2000) SPBF(15,IPOS),SPBF(16,IPOS)
            TOPFST  = SPBF(17,IPOS)
            TOPLST  = SPBF(18,IPOS)
            FIT= OPNNO (OPTYP,TOPFST,TOPLST,MAXOPN,OPNTAB,OPNO,OPNO)
            IF (SPOUT .GE. 10) THEN
C             echo info
              WRITE (MESSU,*) '       FIT:',FIT,TOPFST,TOPLST,XPOS,
     $                                   IPOS
            END IF
            IF (FIT .GT. 0) THEN
C             the entry does apply to this opn
              ADDR= SPBF(7,IPOS)
              IF (ADDR .LE. OSVSIZ) THEN
C               special action within active osv space for opn
C               write instruction to runtime buffer
                SPAKEY= SPAKEY+ 1
                IF (SPAKST .EQ. 0) THEN
C                 first instruction for this opn, set flag pointer
                  SPAKST= SPAKEY
                  IF (SPOUT .GE. 10) THEN
C                   echo first key
                    WRITE (MESSU,*) '  first at:',SPAKEY
                  END IF
                END IF
                IF (SPOUT .GE. 10) THEN
C                 echo instruction
                  WRITE (MESSU,*) '  instr at:',SPAKEY,
     $                          (SPBF(J,IPOS),J=1,5)
                END IF
                DO 10 J= 1,14
                  SPINS(J,SPAKEY)= SPBF(J,IPOS)
 10             CONTINUE
                DO 20 J= 15, LENSPI
                  I= J+ 4
                  SPINS(J,SPAKEY)= SPBF(I,IPOS)
 20             CONTINUE
C
C               set initial sort pointer to use if no undated
                SPPTR(SPAKEY)= SPAKEY
C
                IF (SPINS(1,SPAKEY) .EQ. 0) THEN
C                 this action undated
                  SPNUND= SPNUND+ 1
                END IF
C
C               check condition if present
                LOGBLK= SPINS(18,SPPTR(SPAKEY))
                IF (LOGBLK .GT. 0) THEN
C                 check condition for proper formation
                  CALL SPBKCK (LOGBLK,I1,OPNO,I0,MESSU,
     O                         CONDCK)
                  IF (CONDCK .EQ. -1) THEN
C                   error - stack overflow
                    SGRP= 25
                    CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                         ECOUNT)
                  ELSE IF (CONDCK .EQ. -2) THEN
C                   error - stack underflow
                    SGRP= 26
                    CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                         ECOUNT)
                  ELSE IF (CONDCK .EQ. -3) THEN
C                   error - program bug - stack not cleared
                    SGRP= 27
                    CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                         ECOUNT)
                  END IF
                END IF
              ELSE
C               not part of active osv
                IF (SPOUT .GE. 10) THEN
C                 echo no instruction message
                  WRITE (MESSU,*) '  NO instr:',ADDR,OSVSIZ
                END IF
              END IF
            END IF
 30       CONTINUE
C
          IF (SPNUND .GT. 0) THEN
C           separate into dated and undated pools and rewrite pointers
            DATED= 0
            UNDAT= 0
            IF (SPOUT .GE. 10) THEN
              WRITE (MESSU,*) 'opno,spakst,spakey,spnund',OPNO,SPAKST,
     $                         SPAKEY,SPNUND
            END IF
            DO 40 SKEY= SPAKST, SPAKEY
              IF (SPINS(1,SKEY) .EQ. 0) THEN
C               place in undated pool
                UNDAT= UNDAT+ 1
                PKEY= SPAKEY- SPNUND+ UNDAT
              ELSE
C               place in dated pool
                DATED= DATED+ 1
                PKEY= SPAKST- 1+ DATED
              END IF
              SPPTR(PKEY)= SKEY
 40         CONTINUE
            I= UNDAT+ DATED- (SPAKEY- SPAKST+ 1)
            IF (SPOUT .GE. 10) THEN
              WRITE (MESSU,*) '  should be zero:',I
              DO 45 PKEY= SPAKST,SPAKEY
                WRITE (MESSU,*) '  pkey,spptr(pkey)',PKEY,SPPTR(PKEY)
 45           CONTINUE
            END IF
          END IF
C
          IF (SPAKST .GT. 0) THEN
C           space key to this instruction
C      WRITE(99,*)'KEYS',TOPTYP,LOSPST(TOPTYP),LOSPKY(TOPTYP),
C     $            LOSPLV(TOPTYP),LOSPNU(TOPTYP)
            IPOS= LOSPST(TOPTYP)
            OSV(IPOS)= SPAKST
C           final key
            IPOS= LOSPKY(TOPTYP)
            OSV(IPOS)= SPAKEY
C           special action output level
            IPOS= LOSPLV(TOPTYP)
            OSV(IPOS)= SPOUT
C           number of undated actions
            IPOS= LOSPNU(TOPTYP)
            OSV(IPOS)= SPNUND
            IF (SPOUT .GE. 10) THEN
C             echo keys
              WRITE (MESSU,*) '  into OSV:',SPAKST,SPAKEY
            END IF
          ELSE
C           no instructions written, update osv to indicate that
            IPOS= LOSPST(TOPTYP)
            OSV(IPOS)= 0
            IPOS= LOSPKY(TOPTYP)
            OSV(IPOS)= 0
            IPOS= LOSPLV(TOPTYP)
            OSV(IPOS)= 0
            IPOS= LOSPNU(TOPTYP)
            OSV(IPOS)= 0
            IF (SPOUT .GE. 10) THEN
C             echo no actions message
              WRITE (MESSU,*) ' ** no actions for this operation'
            END IF
          END IF
C
C         write osv-chunk back to disc
          CALL PUTOSV (OSVKEY,OSVKEY,MAXOSV,OSV)
        ELSE
C         special actions not allowed for this operation type
          IF (SPOUT .GE. 10) THEN
C           echo not allowed message
            WRITE (MESSU,*) '    no spec actions op type',TOPTYP
          END IF
        END IF
C       end operation loop
 100  CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   PSPIPS
     I                    (MESSU,MSGFL,SCLU,SPOUT,
     M                     RUNWID)
C
C     + + + PURPOSE + + +
C     Determine lengths of pipes for user-defined variable quantities
C     and initialize from starting OSV value of base variable
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER MESSU,MSGFL,SCLU,SPOUT,RUNWID
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSU  - unit number to write messages on
C     MSGFL  - unit number for file containing error messages
C     SCLU   - cluster in file containing error text
C     SPOUT  - runtime Special Action output level
C     RUNWID - maximum run span width allowed by user-defined variable
C              quantities - 0 if no restrictions
C
C     + + + COMMON BLOCKS + + +
      INCLUDE     'cspec.inc'
C     user-defined variable quantity
      INCLUDE     'pspvqd.inc'
      INCLUDE     'cspvqd.inc'
C     osv in scratch pad
      INCLUDE     'cmosv.inc'
      INCLUDE     'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER          LSTPTR,MINPIP,ERRFLG,EXTRA,SGRP,I,KEYST,KEYND,
     $                 OSVLEN,I6,PTR
      DOUBLE PRECISION DVAL
      CHARACTER*6      OBUFF
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (OBUFF,OBUF1)
      CHARACTER*1  OBUF1(6)
C
C     + + + FUNCTIONS + + +
      INTEGER     DADDR
C
C     + + + INTRINSICS + + +
      INTRINSIC   MAX,SNGL
C
C     + + + EXTERNALS + + +
      EXTERNAL    OMSTI,OMSG,OMSTC,GETOSV,DADDR
C
C     + + + DATA INITIALIZATIONS + + +
      DATA I6/6/
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' USER-DEFINED VARIABLE QUANTITIES - POINTERS AND',
     $        ' PIPE',/,/,'  NAME    POS  LEN       VALUE')
 2010 FORMAT (2X,A6,2I5,I12)
 2020 FORMAT (2X,A6,2I5,G12.5)
C
C     + + + END SPECIFICATIONS + + +
C
      ERRFLG= 0
      LSTPTR= 0
      EXTRA= 0
C
C     check run width restriction
      IF (RUNWID .EQ. -1) THEN
C       run width restriction still must be determined
C       first try evenly dividing availiable pipe among all uvquans
        RUNWID= MXPIPE / NVQD
C
C       check to make sure all pipes fit
        DO 10 I= 1, NVQD
          MINPIP= UVQLAG(I)+ UVQAGG(I)
          IF (MINPIP .GT. RUNWID) THEN
C           must allocate extra space to this pipe
            EXTRA= EXTRA+ MINPIP- RUNWID
          END IF
 10     CONTINUE
        IF (EXTRA .GT. 0) THEN
C         reduce run width to make room for extra space
          RUNWID= RUNWID- (EXTRA- 1)/NVQD - 1
          IF (RUNWID .LT. 1) THEN
C           error - too many lags and aggs for so many uvquans
            CALL OMSTI (MXPIPE)
            SGRP= 90
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 ECOUNT)
            ERRFLG= 1
          END IF
        END IF
      END IF
C
      IF (ERRFLG .EQ. 0) THEN
C       calculate pointers and initialize pipe
        DO 50 I= 1, NVQD
          UVQPOS(I)= LSTPTR+ 1
          UVQLEN(I)= MAX (RUNWID,UVQLAG(I)+ UVQAGG(I))
          LSTPTR= UVQPOS(I)+ UVQLEN(I)- 1
          IF (LSTPTR .GT. MXPIPE) THEN
C           error - pipe overflow
            CALL OMSTI (MXPIPE)
            SGRP= 90
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 ECOUNT)
            ERRFLG= 1
          ELSE
C           this uvquan fits in pipe
            IF (SPOUT .GE. 10) THEN
C             echo pipe info
              WRITE (MESSU,*) '  pipe first,length,last',UVQPOS(I),
     $                        UVQLEN(I),LSTPTR
            END IF
          END IF
C
          IF (ERRFLG .EQ. 0) THEN
C           initialize pipe to initial value in OSVM
            IF (UVQOPX(I) .EQ. 0) THEN
C             base variable is in workspace - set to zero
              DO 20 PTR= UVQPOS(I), LSTPTR
                IVQPIP(PTR)= 0
 20           CONTINUE
            ELSE IF (UVQOPX(I) .EQ. -1) THEN
C             base variable is boolean value of a logic chain - set to undefined
              DO 30 PTR= UVQPOS(I), LSTPTR
                IVQPIP(PTR)= -1
 30           CONTINUE
            ELSE
C             base variable is in an osv
              KEYST= OPNTAB(7,UVQOPX(I))
              KEYND= OPNTAB(8,UVQOPX(I))
              OSVLEN= (KEYND- KEYST+ 1)* 500
              IF (UVQADD(I) .LE. OSVLEN) THEN
C               base variable is in active osv - not necessarily
C               in active section!
                CALL GETOSV (KEYST,KEYND,MAXOSV,
     O                       OSV)
                DO 40 PTR= UVQPOS(I), LSTPTR
                  IF (UVQTYP(I) .EQ. 2) THEN
C                   integer
                    IVQPIP(PTR)= IPAD(UVQADD(I))
                  ELSE IF (UVQTYP(I) .EQ. 3) THEN
C                   real
                    IF (IPAD(UVQADD(I)) .NE. -999) THEN
C                     valid real
                      UVQPIP(PTR)= PAD(UVQADD(I))
                    ELSE
C                     undefined - set to zero
                      UVQPIP(PTR)= 0.0
                    END IF
                  ELSE IF (UVQTYP(I) .EQ. 4) THEN
C                   double precision
                    IF (IPAD(UVQADD(I)) .NE. -999) THEN
C                     valid double precision
                      DVAL= DPPAD(DADDR (UVQADD(I)))
                      UVQPIP(PTR)= SNGL (DVAL)
                    ELSE
C                     undefined - set to zero
                      UVQPIP(PTR)= 0.0
                    END IF
                  END IF
C
 40             CONTINUE
              ELSE
C               error - base variable address is outside of osv
                OBUFF= UVQNAM(I)
                CALL OMSTC (I6,OBUF1)
                CALL OMSTI (UVQADD(I))
                CALL OMSTI (OSVLEN)
                SGRP= 91
                CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                     ECOUNT)
                ERRFLG= 1
              END IF
            END IF
            IF (SPOUT .GE. 10) THEN
C             echo entire pipe
              IF (UVQTYP(I) .EQ. 2) THEN
C               integer
                WRITE (MESSU,*) '  pipe',(IVQPIP(PTR), PTR= UVQPOS(I),
     $                                    LSTPTR)
              ELSE
C               real or dp
                WRITE (MESSU,*) '  pipe',(UVQPIP(PTR), PTR= UVQPOS(I),
     $                                    LSTPTR)
              END IF
            END IF
          END IF
C
          IF ( (ERRFLG .EQ. 0) .AND. (UVQOPX(I) .GE. 0) ) THEN
C           echo pointers and pipes for all but internal boolean pipes
            IF (OUTLEV .GT. 2) THEN
C             echo to message unit
              IF (I .EQ. 1) THEN
C               echo header lines
                WRITE (MESSU,2000)
              END IF
              IF (UVQTYP(I) .EQ. 2) THEN
C               integer
                WRITE (MESSU,2010)  UVQNAM(I),UVQPOS(I),
     $                              UVQLEN(I),
     $                              IVQPIP(UVQPOS(I))
              ELSE
C               real or real from dp
                WRITE (MESSU,2020)  UVQNAM(I),UVQPOS(I),
     $                              UVQLEN(I),
     $                              UVQPIP(UVQPOS(I))
              END IF
            END IF
          END IF
C
 50     CONTINUE
      END IF
C
C      write (99,*) 'lstptr,mxpipe',LSTPTR,' of',MXPIPE
C
      RETURN
      END
C
C
C
      SUBROUTINE   PSPHDR
     I                    (UCIBUF,
     M                     FIRSTH,NUMHDR,ALLHDR)
C
C     + + + PURPOSE + + +
C     Store new special action header and update pointers
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      FIRSTH,NUMHDR,ALLHDR
      CHARACTER*80 UCIBUF
C
C     + + + ARGUMENT DEFINITIONS + + +
C     UCIBUF - buffer containing current record from uci file
C     FIRSTH - index of first header for upcoming action line
C     NUMHDR - number of header lines for upcoming action line
C     ALLHDR - total number of header lines read so far
C
C     + + + COMMON BLOCKS + + +
C     special action file in memory
      INCLUDE     'pspins.inc'
      INCLUDE     'cspins.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      IF (ALLHDR .LT. MXSPHF) THEN
C       room to store header
        ALLHDR= ALLHDR+ 1
        IF (NUMHDR .EQ. 0) THEN
C         this is first header
          FIRSTH= ALLHDR
        END IF
        NUMHDR= NUMHDR+ 1
        SPHDR(ALLHDR)= UCIBUF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PSPFTR
     I                    (UCIBUF,SPOS,LLNPSP,MXSPBF,LREPT,
     M                     SPBF,ALLFTR)
C
C     + + + PURPOSE + + +
C     Store new special action footer and update pointers
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      SPOS,LLNPSP,MXSPBF,LREPT,SPBF(LLNPSP,MXSPBF),ALLFTR
      CHARACTER*80 UCIBUF
C
C     + + + ARGUMENT DEFINITIONS + + +
C     UCIBUF - buffer containing current record from uci file
C     SPOS   - position in special actions instr buffer
C     LLNPSP - local length of special action in buffer
C     MXSPBF - max size of special actions buffer
C     LREPT  - number of times last action was repeated
C     SPBF   - special action instruction buffer
C     ALLFTR - total number of footer lines read so far
C
C     + + + COMMON BLOCKS + + +
C     special action file in memory
      INCLUDE     'pspins.inc'
      INCLUDE     'cspins.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER POS
C
C     + + + END SPECIFICATIONS + + +
C
      IF (ALLFTR .LT. MXSPHF) THEN
C       room to store footer
        ALLFTR= ALLFTR+ 1
        SPFTR(ALLFTR)= UCIBUF(1:20)
C
C       update pointers for all applicable actions
        DO 10 POS= SPOS- LREPT+ 1, SPOS
          IF (SPBF(30,POS) .EQ. 0) THEN
C           this is first footer
            SPBF(29,POS)= ALLFTR
          END IF
          SPBF(30,POS)= SPBF(30,POS)+ 1
 10     CONTINUE
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PSPUVQ
     I                    (UCIBUF,MESSU,OUTLEV,MSGFL,SCLU,LOCDIR,
     I                     DELT,MAXOPN,OPNTAB,NOPNS,SPOUT,OPTYL1,
     M                     ECOUNT,RUNWID)
C
C     + + + PURPOSE + + +
C     read and process user-defined variable quantity from ucifl
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      MESSU,OUTLEV,MSGFL,SCLU,LOCDIR,DELT,MAXOPN,
     $             OPNTAB(20,MAXOPN),NOPNS,SPOUT,ECOUNT,RUNWID
      CHARACTER*1  OPTYL1(60)
      CHARACTER*80 UCIBUF
C
C     + + + ARGUMENT DEFINITIONS + + +
C     UCIBUF - buffer containing current record from uci file
C     MESSU  - unit number to write messages on
C     OUTLEV - output level
C     MSGFL  - unit number for file containing error messages
C     SCLU   - cluster in file containing error text
C     LOCDIR - specs method(s) available for spec. actions input
C              0 - variable name required;
C              1 - either variable name or address required;
C              2 - address required
C     DELT   - simulation interval in minutes
C     MAXOPN - maximum number of operations
C     OPNTAB - information on operations
C     NOPNS  - number of operations
C     SPOUT  - runtime Special Action output level
C     OPTYL1 - operation type name library
C     ECOUNT - error count
C     RUNWID - maximum run span width allowed by user-defined variable
C              quantities - 0 if no restrictions
C
C     + + + COMMON BLOCKS + + +
C     user defined variable quantity definitions
      INCLUDE     'pspvqd.inc'
      INCLUDE     'cspvqd.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      TSTEP(2),I80,SGRP,ERRFLG,I1,TOPX,TCODE,TCMIN(4),I,
     $             I6,IMIN,IMAX,BGRP,SDELT,I4,I2,J,I60
      CHARACTER*2  CTCODE(2)
      CHARACTER*4  CTRAN
      CHARACTER*6  COPTYP,OBUFF
      CHARACTER*15 CADDR
      CHARACTER*80 EBUFF
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE     (COPTYP,COPTY1(1)),(OBUFF,OBUF1(1)),(EBUFF,EBUF1)
      CHARACTER*1      COPTY1(6),OBUF1(6),EBUF1(80)
C
C     + + + FUNCTIONS + + +
      INTEGER      OPNNO,CHKSTR
C
C     + + + INTRINSICS + + +
      INTRINSIC    MAX
C
C     + + + EXTERNALS + + +
      EXTERNAL     OMSTC,OMSG,OMSTI,OPNNO,CHKSTR,CKTCOD,MKADDR
C
C     + + + DATA INITIALIZATIONS + + +
      DATA I1,I2,I4,I6,I60,I80/1,2,4,6,60,80/
      DATA TCMIN/0,1,60,1440/
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (9X,A6,1X,A6,1X,I3,1X,A15,I3,F10.0,2(1X,A2,I3),1X,A4)
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT ('  UVQUAN :',A6,': COPTYP :',A6,': OPTNO ',I3,
     $        ' CADDR :',A15,': UVQTYP',I2,' UVQMUL ',G10.4,
     $        ' LAGCOD :',A2,': LAGSTP ',I3,/,'  AGGCOD :',A2,
     $        ': AGGSTP ',I3,' TRNCOD :',A4,':')
 2010 FORMAT ('  UVQBNM,UVQSUB,ADDR: ',A6,3I3,I6)
 2020 FORMAT (/,' USER-DEFINED VARIABLE QUANTITIES',/,/,'  NAME OPTYP',
     $        ' OPTNO OPNO      ADDR TYP  MULTIPLY LAGIVL AGGIVL ',
     $        'AGGCD')
 2030 FORMAT (A6,2I6,I5,I10,I4,G10.3,2I7,I6)
C
C     + + + END SPECIFICATIONS + + +
C
      ERRFLG= 0
      BGRP= 102
C
      NVQD= NVQD+ 1
      IF (NVQD .GT. MXSPVQ) THEN
C       error - too many uvquans
        ERRFLG= 1
        CALL OMSTI (MXSPVQ)
        SGRP= 77
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             ECOUNT)
      END IF
C
      IF (ERRFLG .EQ. 0) THEN
C       read line
        READ (UCIBUF,1000,ERR=10)  UVQNAM(NVQD),COPTYP,UVQOPN(2,NVQD),
     $                             CADDR,UVQTYP(NVQD),UVQMUL(NVQD),
     $                             CTCODE(1),TSTEP(1),CTCODE(2),
     $                             TSTEP(2),CTRAN
        GO TO 20
 10     CONTINUE
C         error - read format
          EBUFF= UCIBUF
          CALL OMSTC (I80,EBUF1)
          SGRP= 60
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
          ERRFLG= 1
 20     CONTINUE
        IF (SPOUT .GE. 10) THEN
C         echo initial read
          WRITE (MESSU,2000)  UVQNAM(NVQD),COPTYP,
     $                        UVQOPN(2,NVQD),CADDR,UVQTYP(NVQD),
     $                        UVQMUL(NVQD),CTCODE(1),TSTEP(1),
     $                        CTCODE(2),TSTEP(2),CTRAN
        END IF
      END IF
C
      IF ( (ERRFLG .EQ. 0) .AND. (NVQD .GT. 1) ) THEN
C       check name for uniqueness
        I= NVQD- 1
        OBUFF(1:6)= UVQNAM(NVQD)
        J= CHKSTR (I6,I,OBUF1,UVQNM1)
        IF (J .NE. 0) THEN
C         error - repeated name
          ERRFLG= 1
          OBUFF(1:6)= UVQNAM(NVQD)
          CALL OMSTC (I6,OBUF1)
          SGRP= 61
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
        END IF
      END IF

      IF (ERRFLG .EQ. 0) THEN
C       check operation type and store in integer version
C
        IF (COPTYP .EQ. 'GLOBAL') THEN
C         special case for global workspace variable
          UVQOPN(1,NVQD)= 0
          UVQOPN(2,NVQD)= 0
        ELSE
C         check for legal operation type          
          UVQOPN(1,NVQD)= CHKSTR (I6,I60,COPTY1,OPTYL1)
          IF (UVQOPN(1,NVQD) .LE. 0) THEN
C           error - invalid operation type
            OBUFF(1:6)= UVQNAM(NVQD)
            CALL OMSTC (I6,OBUF1)
            CALL OMSTC (I6,COPTY1)
            SGRP= 62
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 ECOUNT)
            ERRFLG= 1
          END IF
        END IF
        IF (SPOUT .GE. 10) THEN
C         echo operation type
          WRITE (MESSU,*) '  operation type',UVQOPN(1,NVQD)
        END IF
      END IF
C
      IF (ERRFLG .EQ. 0) THEN
C       check opid and find its row in operation table
        IF (UVQOPN(1,NVQD) .EQ. 0) THEN
C         workspace - no operation
          UVQOPX(NVQD)= 0
        ELSE
C         see if base operation is active
          TOPX= OPNNO (COPTYP,UVQOPN(2,NVQD),UVQOPN(2,NVQD),MAXOPN,
     $                 OPNTAB,I1,NOPNS)
          IF (TOPX .LE. 0) THEN
C           error - base variable not available
            OBUFF(1:6)= UVQNAM(NVQD)
            CALL OMSTC (I6,OBUF1)
            CALL OMSTC (I6,COPTY1)
            CALL OMSTI (UVQOPN(2,NVQD))
            SGRP= 63
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 ECOUNT)
            ERRFLG= 1
            UVQOPX(NVQD)= 0
          ELSE
C           store row number
            UVQOPX(NVQD)= TOPX
          END IF
        END IF
        IF (SPOUT .GE. 10) THEN
C         echo operation index
          WRITE (MESSU,*) '  operation index',UVQOPX(NVQD)
        END IF
      END IF
C
      IF (ERRFLG .EQ. 0) THEN
C       get address of base variable
        CALL MKADDR (LOCDIR,CADDR,MESSU,MSGFL,SCLU,BGRP,
     M               UVQOPN(1,NVQD),ERRFLG,
     O               UVQBNM(NVQD),UVQSUB(1,NVQD),UVQADD(NVQD))
        IF (SPOUT .GE. 10) THEN
C         echo base variable info
          WRITE (MESSU,2010) UVQBNM(NVQD),(UVQSUB(I,NVQD), I= 1, 3),
     $                       UVQADD(NVQD)
        END IF
      END IF
C
      IF (ERRFLG .EQ. 0) THEN
C       check type code
        IF (UVQTYP(NVQD) .EQ. 0) THEN
C         default to real
          UVQTYP(NVQD)= 3
        END IF
        IMIN= 2
        IMAX= 4
        IF ( (UVQTYP(NVQD) .LT. IMIN) .OR.
     $       (UVQTYP(NVQD) .GT. IMAX) ) THEN
C         error - bad type code
          OBUFF(1:6)= UVQNAM(NVQD)
          CALL OMSTC (I6,OBUF1)
          CALL OMSTI(UVQTYP(NVQD))
          CALL OMSTI(IMIN)
          CALL OMSTI(IMAX)
          SGRP= 64
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
          ERRFLG= 1
          UVQTYP(NVQD)= 3
        END IF
        IF (SPOUT .GE. 10) THEN
C         echo type
          WRITE (MESSU,*) '  type',UVQTYP(NVQD)
        END IF
      END IF
C
      IF (ERRFLG .EQ. 0) THEN
C       check multiplier
        IF (UVQMUL(NVQD) .EQ. 0.0) THEN
C         default to one
          UVQMUL(NVQD)= 1.0
        END IF
        IF (SPOUT .GE. 10) THEN
C         echo multiplier
          WRITE (MESSU,*) '  multiplier',UVQMUL(NVQD)
        END IF
      END IF
C 
      IF (ERRFLG .EQ. 0) THEN
C       check lag and aggregate intervals
C
        DO 30 I= 1, 2
C         check tcode
          CALL CKTCOD (CTCODE(I),MESSU,MSGFL,SCLU,
     M                 ECOUNT,ERRFLG,
     O                 TCODE)
          IF (TCODE .GT. 4) THEN
C           error - tcode cannot be months or years
            OBUFF(1:6)= UVQNAM(NVQD)
            CALL OMSTC (I6,OBUF1)
            CALL OMSTC (I2,CTCODE(I))
            SGRP= 65
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 ECOUNT)
            ERRFLG= 1
            TCODE= 4
          ELSE
C           check tstep
            IF (TSTEP(I) .LT. 0) THEN
C             tstep out of range
              OBUFF(1:6)= UVQNAM(NVQD)
              CALL OMSTC (I6,OBUF1)
              CALL OMSTI (TSTEP(I))
              SGRP= 66
              CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                   ECOUNT)
              ERRFLG= 1
              TSTEP(I)= 1
            ELSE
C             calculate intervals
              SDELT= TSTEP(I)*(TCMIN(TCODE)/DELT)
              IF (I .EQ. 1) THEN
C               lag defaults to zero
                UVQLAG(NVQD)= SDELT
                IF (SPOUT .GE. 10) THEN
C                 echo lag interval
                  WRITE (MESSU,*) '  lag intervals',UVQLAG(NVQD)
                END IF
              ELSE IF (I .EQ. 2) THEN
C               aggregation defaults to one
                UVQAGG(NVQD)= MAX (SDELT,I1)
                IF (SPOUT .GE. 10) THEN
C                 echo aggregate interval
                  WRITE (MESSU,*) '  ag intervals',UVQAGG(NVQD)
                END IF
              END IF
            END IF
          END IF
 30     CONTINUE
      END IF
C
      IF (ERRFLG .EQ. 0) THEN
C       check aggregation transormation function
        IF ( (CTRAN .EQ. 'SUM ') .OR. (CTRAN .EQ. '    ') ) THEN
C         sum over intervals - default
          UVQAFG(NVQD)= 1
        ELSE IF ( (CTRAN .EQ. 'AVER') .AND.
     $            (UVQTYP(NVQD) .GE. 3) ) THEN
C         average over intervals - real or dp only
          UVQAFG(NVQD)= 2
        ELSE IF (CTRAN .EQ. 'MAX ') THEN
C         maximum over intervals
          UVQAFG(NVQD)= 3
        ELSE IF (CTRAN .EQ. 'MIN ') THEN
C         minimum over intervals
          UVQAFG(NVQD)= 4
        ELSE
C         error - invalid aggregation code
          CALL OMSTC (I4,CTRAN)
          OBUFF(1:6)= UVQNAM(NVQD)
          CALL OMSTC (I6,OBUF1)
          SGRP= 67
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
          ERRFLG= 1
        END IF
        IF (SPOUT .GE. 10) THEN
C         echo aggregation function code
          WRITE (MESSU,*) '  aggregation code',UVQAFG(NVQD)
        END IF
      END IF
C
      IF (ERRFLG .EQ. 0) THEN
C       condition was legal
C
        IF (RUNWID .EQ. 0) THEN
C         run width must be determined in pspips
          RUNWID= -1
        END IF
C
C       echo definition
        IF (OUTLEV .GT. 2) THEN
C         echo to message unit
          IF (NVQD .EQ. 1) THEN
C           echo header lines
            WRITE (MESSU,2020)
          END IF
          WRITE (MESSU,2030) UVQNAM(NVQD),UVQOPN(1,NVQD),
     $                       UVQOPN(2,NVQD),UVQOPX(NVQD),
     $                       UVQADD(NVQD),UVQTYP(NVQD),UVQMUL(NVQD),
     $                       UVQLAG(NVQD),UVQAGG(NVQD),UVQAFG(NVQD)
        END IF
      ELSE
C       ignore condition
        NVQD= NVQD- 1
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PSPCON
     I                    (OUTLEV,MESSU,MSGFL,SCLU,MXBKLV,STWORD,LREPT,
     I                     LLNPSP,MXSPBF,SPOS,
     M                     SPBF,ECOUNT,UCIBUF,KEY,CURBLK,CURLVL,PREBLK,
     M                     ELSEFG,ALLFTR,RUNWID)
C
C     + + + PURPOSE + + +
C     Read and process special action condition from ucifl.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      OUTLEV,MESSU,MSGFL,SCLU,MXBKLV,STWORD,LREPT,LLNPSP,
     $             MXSPBF,SPOS,SPBF(LLNPSP,MXSPBF),ECOUNT,KEY,CURBLK,
     $             CURLVL,PREBLK(MXBKLV),ELSEFG(MXBKLV),ALLFTR,RUNWID
      CHARACTER*80 UCIBUF
C
C     + + + ARGUMENT DEFINITIONS + + +
C     OUTLEV - output level
C     MESSU  - unit number to write messages on
C     MSGFL  - unit number for file containing error messages
C     SCLU   - cluster in file containing error text
C     MXBKLV - maximum number of nesting levels for logic blocks
C     STWORD - position of keyword (IF, ELSE, or END IF) in text buffer
C     LLNPSP - local length of special action in buffer
C     MXSPBF - max size of special actions buffer
C     SPOS   - position in special actions instr buffer
C     SPBF   - special action instruction buffer
C     ECOUNT - error count
C     UCIBUF - buffer containing current record from uci file
C     KEY    - next record number in uci file
C     CURBLK - current logic block
C     CURLVL - current nesting level
C     PREBLK - logic block for previous nesting level(s)
C     ELSEFG - flag indicating whether an ELSE (not ELSE IF) has occurred
C              in each nesting level
C     ALLFTR - total number of footer lines read so far
C     RUNWID - maximum run span width allowed by user-defined variable
C              quantities - 0 if no restrictions
C
C     + + + COMMON BLOCKS + + +
C     special action file in memory
      INCLUDE     'pspins.inc'
      INCLUDE     'cspins.inc'
C     special action conditions
      INCLUDE     'pspcnd.inc'
      INCLUDE     'cspcnd.inc'
C     user defined variable quantity definitions
      INCLUDE     'pspvqd.inc'
      INCLUDE     'cspvqd.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER  PREPOS,PRECNT,I,J,NEWCHN,SGRP,STLINE,CHNFLG,LSTREF,
     $         NUMLIN,KEYST
C
C     + + + EXTERNALS + + +
      EXTERNAL OMSTI,OMSG,SPARSE,PSPFTR
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,'  IF: NESTING LEVEL',I3)
 2010 FORMAT (  '  ELSE IF')
 2020 FORMAT (  '  ELSE')
 2030 FORMAT (  '  END IF: NESTING LEVEL',I3,/)
C
C     + + + END SPECIFICATIONS + + +
C
      CHNFLG= 0
      KEYST= KEY
C
      IF (UCIBUF(STWORD:STWORD+2) .EQ. 'IF ') THEN
C       new block and nesting level
C
        IF (CURLVL .GE. MXBKLV) THEN
C         error - nesting too deep
          CALL OMSTI (KEY)
          CALL OMSTI (MXBKLV)
          SGRP= 70
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
        ELSE IF (NBLOCK .GE. MXSPBK) THEN
C         error - too many logic blocks
          CALL OMSTI (KEY)
          CALL OMSTI (MXSPBK)
          SGRP= 74
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
        ELSE
C         process block
C
C         increase nesting level and store block for previous level
          CURLVL= CURLVL+ 1
          PREBLK(CURLVL)= CURBLK
          ELSEFG(CURLVL)= 0
C
          IF (OUTLEV .GT. 2) THEN
C           echo beginning of IF block
            WRITE (MESSU,2000) CURLVL
          END IF
C
C         current logic block is new logic block
          NBLOCK= NBLOCK+ 1
C
C         define chains for new block
C
          IF (CURLVL .EQ. 1) THEN
C           no previous level to copy chains from
            IF (NBLOCK .LE. 1) THEN
C             no previous blocks
              BLKPOS(NBLOCK)= 1
            ELSE
C             position pointer after last block
              BLKPOS(NBLOCK)= BLKPOS(NBLOCK- 1)+ BLKCNT(NBLOCK- 1)
            END IF
            BLKCNT(NBLOCK)= 1
            PRECNT= 0
          ELSE
C           copy chain references from previous level
C
C           set pointers
C
C           position and count for previous level
            PREPOS= BLKPOS(PREBLK(CURLVL))
            PRECNT= BLKCNT(PREBLK(CURLVL))
C
C           current position depends on previous block, not level
            BLKPOS(NBLOCK)= BLKPOS(NBLOCK- 1)+ BLKCNT(NBLOCK- 1)
C           current count is that for previous level plus one for new chain
            BLKCNT(NBLOCK)= PRECNT+ 1
C
C           copy chain references
            J= BLKPOS(NBLOCK)
            DO 10 I= PREPOS, PREPOS+ PRECNT- 1
              BLKCHN(J)= BLKCHN(I)
              J= J+ 1
 10         CONTINUE
          END IF
C
C         add new chain
          STLINE= STWORD+ 3
          CALL SPARSE (STLINE,OUTLEV,MESSU,MSGFL,SCLU,
     M                 ECOUNT,UCIBUF,KEY,
     O                 NEWCHN,NUMLIN)
          IF (NEWCHN .GT. 0) THEN
C           condition processed properly
            NCHAIN= NCHAIN+ 1
            LSTREF= BLKPOS(NBLOCK)+ PRECNT
            IF (NCHAIN .GT. MXSPCH) THEN
C             error - too many chains
              NEWCHN= 0
              CALL OMSTI (KEY)
              CALL OMSTI (MXSPCH)
              SGRP= 75
              CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                   ECOUNT)
            ELSE IF (LSTREF .GT. MXSPCR) THEN
C             error - too many chain references
              NEWCHN= 0
              CALL OMSTI (KEY)
              CALL OMSTI (MXSPCR)
              SGRP= 76
              CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                   ECOUNT)
            ELSE
C             store new chain
              CHNCND(NCHAIN)= NEWCHN
              BLKCHN(LSTREF)= NCHAIN
              CHNFLG= NCHAIN
              CHNKEY(NCHAIN)= KEYST
            END IF
          END IF
          IF (NEWCHN .LE. 0) THEN
C           error occurred in condition - reset block pointers
            BLKCNT(NBLOCK)= 0
            BLKPOS(NBLOCK)= 0
            NBLOCK= NBLOCK- 1
            PREBLK(CURLVL)= 0
            CURLVL= CURLVL- 1
          ELSE
C           this block ok
            CURBLK= NBLOCK
            BLKLVL(CURBLK)= CURLVL
          END IF
        END IF
C
      ELSE IF (UCIBUF(STWORD:STWORD+3) .EQ. 'ELSE') THEN
C       new block
C
        IF (CURLVL .LT. 1) THEN
C         error - else without matching if
          CALL OMSTI (KEY)
          SGRP= 71
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
        ELSE IF (ELSEFG(CURLVL) .EQ. 1) THEN
C         error - an ELSE (not ELSE IF) has already occurred at this level
          CALL OMSTI (KEY)
          SGRP= 73
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
        ELSE IF (NBLOCK .GE. MXSPBK) THEN
C         error - too many logic blocks
          CALL OMSTI (KEY)
          CALL OMSTI (MXSPBK)
          SGRP= 74
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
        ELSE
C         process block
C
C         set pointers
          PREPOS= BLKPOS(CURBLK)
          PRECNT= BLKCNT(CURBLK)
          NBLOCK= NBLOCK+ 1
          BLKPOS(NBLOCK)= BLKPOS(NBLOCK- 1)+ BLKCNT(NBLOCK- 1)
          BLKCNT(NBLOCK)= PRECNT
          LSTREF= BLKPOS(NBLOCK)+ BLKCNT(NBLOCK)- 1
C
C         copy chain references
          J= BLKPOS(NBLOCK)
          DO 20 I= PREPOS, PREPOS+ PRECNT- 1
            BLKCHN(J)= BLKCHN(I)
            J= J+ 1
 20       CONTINUE
C
C         negate last chain reference to require that it is false
          BLKCHN(LSTREF)= -BLKCHN(LSTREF)
C
          IF (UCIBUF(STWORD:STWORD+7) .EQ. 'ELSE IF ') THEN
C           add new chain
C
            IF (OUTLEV .GT. 2) THEN
C             echo beginning of ELSE IF block
              WRITE (MESSU,2010)
            END IF
C
C           update pointers
            BLKCNT(NBLOCK)= BLKCNT(NBLOCK)+ 1
            LSTREF= LSTREF+ 1
C           parse rest of line
            STLINE= STWORD+ 8
            CALL SPARSE (STLINE,OUTLEV,MESSU,MSGFL,SCLU,
     M                   ECOUNT,UCIBUF,KEY,
     O                   NEWCHN,NUMLIN)
            IF (NEWCHN .GT. 0) THEN
C             condition processed properly
              NCHAIN= NCHAIN+ 1
              IF (NCHAIN .GT. MXSPCH) THEN
C               error - too many chains
                NEWCHN= 0
                CALL OMSTI (KEY)
                CALL OMSTI (MXSPCH)
                SGRP= 75
                CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                     ECOUNT)
              ELSE IF (LSTREF .GT. MXSPCR) THEN
C               error - too many chain references
                NEWCHN= 0
                CALL OMSTI (KEY)
                CALL OMSTI (MXSPCR)
                SGRP= 76
                CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                     ECOUNT)
              ELSE
C               store new chain
                CHNCND(NCHAIN)= NEWCHN
                BLKCHN(LSTREF)= NCHAIN
                CHNFLG= NCHAIN
                CHNKEY(NCHAIN)= KEYST
              END IF
            END IF
            IF (NEWCHN .LE. 0) THEN
C             error occurred in condition - reset block pointers
              BLKCNT(NBLOCK)= 0
              BLKPOS(NBLOCK)= 0
              NBLOCK= NBLOCK- 1
            ELSE
C             this block ok
              CURBLK= NBLOCK
            END IF
          ELSE
C           plain else is last possible block at this level
            ELSEFG(CURLVL)= 1
            CURBLK= NBLOCK
C
            IF (OUTLEV .GT. 2) THEN
C             echo beginning of ELSE IF block
              WRITE (MESSU,2020)
            END IF
C
C           store footer
            CALL PSPFTR (UCIBUF,SPOS,LLNPSP,MXSPBF,LREPT,
     M                   SPBF,ALLFTR)
          END IF
          BLKLVL(CURBLK)= CURLVL
        END IF
      ELSE IF (UCIBUF(STWORD:STWORD+5) .EQ. 'END IF') THEN
C       end of nesting level
        IF (CURLVL .LT. 1) THEN
C         error - endif without matching if
          CALL OMSTI (KEY)
          SGRP= 72
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
        ELSE
C         update pointers
          CURBLK= PREBLK(CURLVL)
          PREBLK(CURLVL)= 0
          ELSEFG(CURLVL)= 0
C
          IF (OUTLEV .GT. 2) THEN
C           echo end of IF block
            WRITE (MESSU,2030) CURLVL
          END IF
          CURLVL= CURLVL- 1
C
C         store footer
          CALL PSPFTR (UCIBUF,SPOS,LLNPSP,MXSPBF,LREPT,
     M                 SPBF,ALLFTR)
        END IF
      ELSE
C       program bug
        WRITE(*,*) 'BUG ON CONDITION KEYWORDS ON LINE',KEY
        WRITE(*,*) UCIBUF
        STOP
      END IF
C
      IF (CHNFLG .GT. 0) THEN
C       need an internal uvquan for boolean value
        NVQD= NVQD+ 1
        IF (NVQD .GT. MXSPVQ) THEN
C         error - too many uvquans
          CALL OMSTI (MXSPVQ)
          SGRP= 77
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
        ELSE
C         construct uvquan
          CHNUVQ(NCHAIN)= NVQD
          UVQOPN(1,NVQD)= -1
          UVQOPN(2,NVQD)= -1
          UVQOPX(NVQD)= -1
          UVQNAM(NVQD)= 'intrnl'
          UVQBNM(NVQD)= 'intrnl'
          UVQSUB(1,NVQD)= 0
          UVQSUB(2,NVQD)= 0
          UVQSUB(3,NVQD)= 0
          UVQTYP(NVQD)= 2
          UVQADD(NVQD)= -NCHAIN
          UVQMUL(NVQD)= 1.0
          UVQLAG(NVQD)= 0
          UVQAGG(NVQD)= 1
          UVQAFG(NVQD)= 1
C
          IF (RUNWID .EQ. 0) THEN
C           run width must be determined in pspips
            RUNWID= -1
          END IF
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PSPDIS
     I                    (UCIBUF,MESSU,MSGFL,SCLU,MXSPDS,OUTLEV,SPOUT,
     M                     DCNT,SPDCNT,SPDTST,SPDTCD,SPDDFG,SPDFRC,
     M                     ECOUNT)
C
C     + + + PURPOSE + + +
C     read and process special action distribution from ucifl
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      MESSU,MSGFL,SCLU,MXSPDS,OUTLEV,SPOUT,DCNT,
     $             SPDCNT(MXSPDS),SPDTST(MXSPDS),SPDTCD(MXSPDS),
     $             SPDDFG(MXSPDS),ECOUNT
      REAL         SPDFRC(10,MXSPDS)
      CHARACTER*80 UCIBUF
C
C     + + + ARGUMENT DEFINITIONS + + +
C     UCIBUF - buffer containing current record from uci file
C     MESSU  - unit number to write messages on
C     MSGFL  - unit number for file containing error messages
C     SCLU   - cluster in file containing error text
C     MXSPDS - maximum number of distributions
C     OUTLEV - output level
C     SPOUT  - runtime Special Action output level
C     DCNT   - number of distributions processed so far
C     SPDCNT - count of fractions for this distribution
C     SPDTST - timestep between fractions for this distribution
C     SPDTCD - time code for this distribution
C     SPDDFG - deferral code for this distribution 1 - skip
C                                                  2 - shift
C                                                  3 - accum
C     SPDFRC - fraction of action at each timestep of distribution
C     ECOUNT - error count
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,I1,IND,CNT,TSTEP,TCODE,DEFFG,IMAX,ERRFLG,SGRP
      REAL         FRACT(10)
      CHARACTER*2  CTCODE
      CHARACTER*5  CDEFFG
      CHARACTER*1  CUBUF1(80)
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (CDEFFG,CDEFF1)
      CHARACTER*1  CDEFF1(5)
C
C     + + + EXTERNALS + + +
      EXTERNAL     CKTCOD,OMSTI,OMSTC,OMSG,CVARAR
C
C     + + + DATA INITIALIZATIONS + + +
      DATA I1/1/
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (8X,I3,1X,I3,1X,A2,1X,I3,1X,A5,2X,10F5.2)
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' DISTRIBUTIONS',/,
     $          '   INDEX TSTEP CTCODE TCODE CDEFFG DEFFG  CNT',
     $          ' FRACTIONS')
 2010 FORMAT (3X,I5,I6,5X,A2,I6,2X,A5,I6,I5,1X,10F8.5)
C
C     + + + END SPECIFICATIONS + + +
C
      TCODE = 0
      DEFFG = 0
      ERRFLG= 0
C
      READ(UCIBUF,1000,ERR=20) IND,CNT,CTCODE,TSTEP,CDEFFG,FRACT
      IF (SPOUT .GE. 10) THEN
C       echo initial read
        WRITE (MESSU,*) '  DISTRB:',IND,CNT,CTCODE,TSTEP,CDEFFG,
     $                    (FRACT(I),I=1,CNT)
      END IF
C
      IF ( (IND .LT. I1) .OR. (IND .GT. MXSPDS) ) THEN
C       error - distribution index number out of range
        CALL OMSTI (IND)
        CALL OMSTI (I1)
        CALL OMSTI (MXSPDS)
        SGRP= 30
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             ECOUNT)
        ERRFLG= 1
      ELSE
C       index in range
        IF (SPDCNT(IND) .NE. 0) THEN
C         error - this distribution index already in use
          CALL OMSTI (IND)
          SGRP= 31
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
          ERRFLG= 1
          IND= 0
        END IF
        IF (SPOUT .GE. 10) THEN
C         echo index
          WRITE (MESSU,*) '  index',IND
        END IF
C
C       check time code
        CALL CKTCOD (CTCODE,MESSU,MSGFL,SCLU,
     M               ECOUNT,ERRFLG,
     O               TCODE)
        IF (SPOUT .GE. 10) THEN
C         echo tcode
          WRITE (MESSU,*) '  tcode',TCODE
        END IF
C
C       check deferral code
        IF (CDEFFG .EQ. ' SKIP' .OR. CDEFFG .EQ. '     ') THEN
C         this is the default
          DEFFG= 1
        ELSE IF (CDEFFG .EQ. 'SHIFT') THEN
          DEFFG= 2
        ELSE IF (CDEFFG .EQ. 'ACCUM') THEN
          DEFFG= 3
        ELSE
C         unknown deferral flag
          I= 5
          CALL OMSTC(I,CDEFF1)
          SGRP= 32
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
          ERRFLG= 1
        END IF
        IF (SPOUT .GE. 10) THEN
C         echo deferral code
          WRITE (MESSU,*) '  deferral code',DEFFG
        END IF
C
        IF (TSTEP .LT. I1) THEN
C         bad timestep
          CALL OMSTI (TSTEP)
          SGRP= 33
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
          ERRFLG= 1
          TSTEP= 1
        END IF
        IF (SPOUT .GE. 10) THEN
C         echo time step
          WRITE (MESSU,*) '  time step',TSTEP
        END IF
C
        IMAX= 10
        IF ( (CNT .LT. I1) .OR. (CNT .GT. IMAX) ) THEN
C         bad fraction count
          CALL OMSTI (CNT)
          CALL OMSTI (I1)
          CALL OMSTI (IMAX)
          SGRP= 34
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
          ERRFLG= 1
          CNT= 0
        END IF
        IF (SPOUT .GE. 10) THEN
C         echo count
          WRITE (MESSU,*) '  count',CNT
        END IF
C
        IF (ERRFLG .EQ. 0) THEN
C         fill in info for this distribution
          SPDCNT(IND)= CNT
          SPDTST(IND)= TSTEP
          SPDTCD(IND)= TCODE
          SPDDFG(IND)= DEFFG
          DO 10 I= 1,10
            IF (I .LE. CNT) THEN
              SPDFRC(I,IND)= FRACT(I)
            ELSE
              SPDFRC(I,IND)= 0.0
            END IF
 10       CONTINUE
        END IF
      END IF
C
      GO TO 30
 20   CONTINUE
C       read error
        I= 80
        CALL CVARAR (I,UCIBUF,I,
     O               CUBUF1)
        CALL OMSTC (I,CUBUF1)
        SGRP= 35
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             ECOUNT)
        ERRFLG= 1
 30   CONTINUE
C
      IF (OUTLEV .GT. 2) THEN
C       echo distribution
        DCNT= DCNT+ 1
        IF (DCNT .EQ. 1) THEN
C         distribution echo header
          WRITE (MESSU,2000)
        END IF
C       output all parms of distribution
        WRITE (MESSU,2010) IND,SPDTST(IND),CTCODE,SPDTCD(IND),CDEFFG,
     $                   SPDDFG(IND),SPDCNT(IND),(SPDFRC(I,IND),I=1,CNT)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PSPUVN
     I                    (MESSU,MSGFL,SCLU,OUTLEV,LOCDIR,SPOUT,
     M                     ECOUNT,UCIBUF,KEY)
C
C     + + + PURPOSE + + +
C     read and process a user defined special action
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      MESSU,MSGFL,SCLU,OUTLEV,LOCDIR,SPOUT,ECOUNT,KEY
      CHARACTER*80 UCIBUF
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSU  - unit number to write messages on
C     MSGFL  - unit number of file containg message text
C     SCLU   - cluster in file containing error text
C     OUTLEV - output level
C     LOCDIR - specs method(s) available for spec. actions input
C              0 - variable name required;
C              1 - either variable name or address required;
C              2 - address required
C     SPOUT  - runtime Special Action output level
C     ECOUNT - error count
C     UCIBUF - buffer containing current record from uci file
C
C     + + + COMMON BLOCKS + + +
C     user defined special actions
      INCLUDE     'pspuvr.inc'
      INCLUDE     'cspuvr.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      COUNT,UPOS,I,J,LOPTYP,ERRFLG,I6,I0,SGRP,BGRP,INDENT,
     $             I3,I80
      CHARACTER*1  SPACE
      CHARACTER*6  ECHBUF
      CHARACTER*30 BTMP
      CHARACTER*80 TBUFF
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (ECHBUF,ECHBU1),(BTMP,BTMP1),(TBUFF,TBUFF1)
      CHARACTER*1  ECHBU1(6),BTMP1(30),TBUFF1(80)
C
C     + + + INTRINSICS + + +
      INTRINSIC    MOD
C
C     + + + EXTERNALS + + +
      EXTERNAL     MKADDR,GETUCI,GETIND,OMSTI,OMSG,OMSTC,CHRINS
C
C     + + + DATA INITIALIZATIONS + + +
      DATA I0,I3,I6,I80/0,3,6,80/
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (16X,I3)
 1010 FORMAT (A6)
 1020 FORMAT (F5.2)
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' USER-DEFINED VARIABLE NAMES',/,/,'  UVNAME OPTYP',
     $        ' VNAME  S1 S2 S3      ADDR ACT    FRAC')
 2010 FORMAT (2X,A6,I6,1X,A6,3I3,I10,I4,F8.5)
C
C     + + + END SPECIFICATIONS + + +
C
      ERRFLG= 0
      BGRP= 104
      SPACE= ' '
C
C     don't know what type of operation
      LOPTYP= 0
C
C     how many variables associated with user defined special action
      READ (UCIBUF,1000,ERR=5) COUNT
        GO TO 7
 5    CONTINUE
C       error - cannot read count
        ECHBUF(1:6)= UCIBUF(11:16)
        CALL OMSTC (I6,ECHBU1)
        ECHBUF(1:3)= UCIBUF(17:19)
        I= 3
        CALL OMSTC (I,ECHBU1)
        SGRP= 43
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             ECOUNT)
 7    CONTINUE
      IF (SPOUT .GE. 10) THEN
C       echo initial read
        WRITE (MESSU,*) '  UVNAME: ',UCIBUF(11:16),' count is',COUNT
      END IF
C
      IF (SPUCNT .GE. MXSPUV-1) THEN
C       error - too many user defined special actions
        I= MXSPUV- 1
        CALL OMSTI (I)
        SGRP= 40
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             ECOUNT)
        ERRFLG= 1
C       position ucifl at right spot
        IF (COUNT .GT. 2) THEN
 10       CONTINUE
C           need to skip records in uci file
            CALL GETUCI (I0,
     M                   KEY,
     O                   UCIBUF)
            COUNT= COUNT- 2
          IF (COUNT .GT. 2) GO TO 10
        END IF
      END IF
C
      IF (ERRFLG .EQ. 0) THEN
C       save this user defined special action, location
        SPUCNT= SPUCNT+ 1
C
C       user defined name
        READ (UCIBUF(11:16),1010) SPUVNM(SPUCNT)
C       position where next will start
        SPUPOS(SPUCNT+1)= SPUPOS(SPUCNT)+ COUNT
        IF (SPUPOS(SPUCNT+1) .GT. MXSPUX) THEN
C         error - too many total variables
          ECHBUF= SPUVNM(SPUCNT)
          CALL OMSTC (I6,ECHBU1)
          CALL OMSTI (MXSPUX)
          SGRP= 41
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
          ERRFLG= 1
        END IF
      END IF
C
      IF (ERRFLG .EQ. 0) THEN
C       process references to variables
        UPOS= SPUPOS(SPUCNT)
C
        DO 40 I= 1, COUNT
          IF (MOD(I,2) .EQ. 0) THEN
            BTMP= UCIBUF(51:80)
          ELSE
            BTMP= UCIBUF(21:50)
          END IF
C         need to determine address of referenced name
          CALL MKADDR (LOCDIR,BTMP(1:15),MESSU,MSGFL,SCLU,BGRP,
     M                 LOPTYP,ERRFLG,
     O                 SPUNAM(UPOS),SPUSUB(1,UPOS),SPUADD(UPOS))
          IF (SPOUT .GE. 10) THEN
C           echo address
            WRITE (MESSU,*) ' ref var',I,' at addr ',SPUADD(UPOS)
          END IF
          IF (I .EQ. 1) THEN
C           save type
            SPUTYP(SPUCNT)= LOPTYP
          ELSE IF (SPUTYP(SPUCNT) .NE. LOPTYP) THEN
C           error - not same as other types
            ECHBUF= SPUNAM(UPOS)
            CALL OMSTC (I6,ECHBU1)
            CALL OMSTI (LOPTYP)
            ECHBUF= SPUVNM(SPUCNT)
            CALL OMSTC (I6,ECHBU1)
            CALL OMSTI (SPUTYP(SPUCNT))
            SGRP= 42
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 ECOUNT)
            ERRFLG= 1
          END IF
C
          IF (ERRFLG .EQ. 0) THEN
C           fraction
            READ(BTMP(17:21),1020,ERR=20) SPUFRC(UPOS)
              GO TO 30
 20         CONTINUE
C             error - fraction is invalid
              ECHBUF= SPUVNM(SPUCNT)
              CALL OMSTC (I6,ECHBU1)
              ECHBUF= SPUNAM(UPOS)
              CALL OMSTC (I6,ECHBU1)
              J= 21
              CALL OMSTC (J,BTMP1)
              SGRP= 44
              CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                   ECOUNT)
              ERRFLG= 1
              SPUFRC(UPOS)= 0.0
 30         CONTINUE
            IF (SPOUT .GE. 10) THEN
C             echo fraction
              WRITE (MESSU,*) '  fraction',I,' is',SPUFRC(UPOS)
            END IF
C
C           action
            IF (BTMP(23:26) .EQ. 'MOVT') THEN
C             act on total of this and next location
              SPUACT(UPOS)= 1
            ELSE IF (BTMP(23:26) .EQ. 'MOV1') THEN
C             act on value in this location, move remainder to next
              SPUACT(UPOS)= 2
            ELSE IF (BTMP(23:26) .EQ. 'MOV2') THEN
C             act on value in next location, move remainder to next
              SPUACT(UPOS)= 3
            ELSE
C             no action code
              SPUACT(UPOS)= 0
            END IF
            IF (SPOUT .GE. 10) THEN
C             echo action code
              WRITE (MESSU,*) '  action code',I,' is',SPUACT(UPOS)
            END IF
C
            IF ( (I .LT. COUNT) .AND. (MOD(I,2) .EQ. 0) ) THEN
C             next user defined name record
              CALL GETUCI (I0,
     M                     KEY,
     O                     UCIBUF)
              CALL GETIND (KEY,
     O                     INDENT)
              IF (INDENT .GT. 0) THEN
C               reinsert indentation for continuation line
                TBUFF= UCIBUF
                DO 35 J= 1, INDENT
                  CALL CHRINS (I80,I3,SPACE,
     M                         TBUFF1(1))
                UCIBUF= TBUFF
 35             CONTINUE
              END IF
            END IF
C           increment position for details
            UPOS= UPOS+ 1
          END IF
 40     CONTINUE
      END IF
C
      IF (ERRFLG .EQ. 0) THEN
C       no errors in processing
        IF (OUTLEV .GT. 2) THEN
C         echo user name
          IF (SPUCNT .EQ. 1) THEN
C           echo header
            WRITE (MESSU,2000)
          END IF
C
          DO 50 UPOS= SPUPOS(SPUCNT),SPUPOS(SPUCNT+1)- 1
            WRITE (MESSU,2010) SPUVNM(SPUCNT),SPUTYP(SPUCNT),
     $                         SPUNAM(UPOS),(SPUSUB(I,UPOS),I=1,3),
     $                         SPUADD(UPOS),SPUACT(UPOS),SPUFRC(UPOS)
 50       CONTINUE
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PSPACT
     I                    (UCIBUF,MESSU,MSGFL,SCLU,LOCDIR,SDATIM,EDATIM,
     I                     NDAMON,MXSPBF,MXSPDS,SPDCNT,DELT,CURBLK,
     I                     VACFST,VACCNT,MXVACC,VRFADD,VACADD,SPOUT,
     I                     LLNPSP,
     M                     FIRSTH,NUMHDR,LREPT,SPOS,SPBF,SPBFR,SPBDAT,
     M                     RUNWID)
C
C     + + + PURPOSE + + +
C     read and process an old style special action from ucifl
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      MESSU,MSGFL,SCLU,LOCDIR,SDATIM(5),EDATIM(5),
     $             NDAMON(12),MXSPBF,MXSPDS,SPDCNT(MXSPDS),DELT,CURBLK,
     $             VACFST(10),VACCNT(10),MXVACC,VRFADD(MXVACC),
     $             VACADD(MXVACC),SPOUT,LLNPSP,FIRSTH,NUMHDR,LREPT,
     $             SPOS,SPBF(LLNPSP,MXSPBF),SPBDAT(MXSPBF),RUNWID
      REAL         SPBFR(LLNPSP,MXSPBF)
      CHARACTER*80 UCIBUF
C
C     + + + ARGUMENT DEFINITIONS + + +
C     UCIBUF - buffer containing current record from uci file
C     MESSU  - unit number to write messages on
C     MSGFL  - unit number of file containing error text
C     SCLU   - cluster in file containing error text
C     LOCDIR - specs method(s) available for spec. actions input
C              0 - variable name required;
C              1 - either variable name or address required;
C              2 - address required
C     SDATIM - starting date/time
C     EDATIM - ending date/time
C     NDAMON - no. of days in each month of calendar year
C     MXSPBF - max size of special actions buffer
C     MXSPDS - maximum number of distributions
C     SPDCNT - count of fractions for this distribution
C     DELT   - simulation interval in minutes
C     CURBLK - current logic block
C     VACFST - first variable accumulator reference for each operation type
C     VACCNT - number of variable accumulator references for each operation type
C     MXVACC - maximum number of variable accumulator references
C     VRFADD - variable accumulator reference addresses
C     VACADD - variable accumulator addresses
C     SPOUT  - runtime Special Action output level
C     LLNPSP - local length of special action in buffer
C     FIRSTH - index of first header for upcoming action line
C     NUMHDR - number of header lines for upcoming action line
C     LREPT  - number of times last action was repeated
C     SPOS   - position in special actions instr buffer
C     SPBF   - special action instruction buffer (integer version)
C     SPBFR  - special action instruction buffer (real version)
C     SPBDAT - special action instruction date
C     RUNWID - maximum run span width allowed by user-defined variable
C              quantities - 0 if no restrictions
C
C     + + + COMMON BLOCK- SPEC + + +
      INCLUDE     'cspec.inc'
C     user defined variable names
      INCLUDE     'pspuvr.inc'
      INCLUDE     'cspuvr.inc'
C     conditions
      INCLUDE     'pspcnd.inc'
      INCLUDE     'cspcnd.inc'
C     user defined variable quantity definitions
      INCLUDE     'pspvqd.inc'
      INCLUDE     'cspvqd.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       ERRFLG,TOPFST,TOPLST,TSTEP(2),DATIM(5),DSIND,TYPCOD,
     $              ACTCD,NUMINC,I,SGRP,TOPTYP,OECNT,I6,UVQ,BGRP,IVAL,
     $              ACTUVQ,TCMIN(4),DSPOS,UKWDNO,NSUB(3),ADDR,XKWDNO,
     $              UPOS,MOVFLG,TACTCD,I2,EXDAT(5),RLEN,SDIG,DECP,I0,
     $              I5,TCODE(2),ALLDEL,IMIN,IMAX,DIF1,DIF2,CNDIND,
     $              ICHAIN,UVQIND,ACCADD,HDRPTR,HDRNUM,I60
      REAL          RVAL,TRVAL
      CHARACTER*2   CTCODE(2),CODES(6)
      CHARACTER*3   CACUSR
      CHARACTER*4   CACTCD(16)
      CHARACTER*6   COPTYP,OBUFF,BLANK,VNAME,UVNAME
      CHARACTER*10  CACTVL
      CHARACTER*120 ECHBUF
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE  (COPTYP,COPTY1(1)),(ECHBUF,ECHBU1)
      EQUIVALENCE  (CACUSR,CACUS1),(OBUFF,OBUF1)
      CHARACTER*1   COPTY1(6),ECHBU1(120),CACUS1(3),OBUF1(6)
C
C     + + + FUNCTIONS + + +
      INTEGER       CHKSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL      OMSTC,OMSG,TOPTNO,MKQUAN,CKRWID,CKTCOD,OMSTI,CHKSTR,
     $              MKADDR,COPYI,STDATE,DIFTIM,EXDATE,DECCHX,SPNXDT,
     $              LFTSTR,QUPCAS
C
C     + + + DATA INITIALIZATIONS + + +
      DATA I0,I2,I5,I6,I60/0,2,5,6,60/
      DATA CACTCD/'=   ','+=  ','-=  ','*=  ','/=  ','MIN ','MAX ',
     $            'ABS ','INT ','^=  ','LN  ','LOG ','MOD ','MOVT',
     $            'MOV1','MOV2'/
      DATA CODES/'  ','MI','HR','DY','MO','YR'/
      DATA RLEN,SDIG,DECP/10,5,-5/
      DATA TCMIN/0,1,60,1440/
      DATA BLANK/'      '/
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (2X,A6,I3,I4,A2,I3,I4,4(1X,I2),2I2,2X,15X,A3,A10,1X,A2,1X,
     $        2I3)
 1010 FORMAT (I3)
 1020 FORMAT (A6)
 1030 FORMAT (A4,A2)
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (A6)
 2010 FORMAT (/,' ACTION DEFINITIONS',
     $        /,'   OPERATION       DEFERRAL DATE AND TIME     TYPE',
     $          '  DIST VARIABLE SUBSCRPTS        ADDR   ACT      VALUE',
     $        /,'   TYPE     #   # STEP CODE YEAR/MO/DY HR:MN  CODE',
     $          '       NAME   #1 #2 #3                 CODE')
 2020 FORMAT (2X,A6,I4,I4,1X,I4,3X,A2,1X,I4,4(1X,I2),2X,I4,2X,I4,21X,
     $        I10,2X,A4)
 2030 FORMAT (I10)
 2040 FORMAT (76X,I10)
 2050 FORMAT (I3)
 2060 FORMAT (' ',A120)
C
C     + + + END SPECIFICATIONS + + +
C
C     assume no error
      ERRFLG= 0
      BGRP= 106
C
C     get needed info from uci record buffer
      READ (UCIBUF,1000,ERR=10) COPTYP,TOPFST,TOPLST,CTCODE(1),
     $                          TSTEP(1),(DATIM(I),I=1,5),DSIND,TYPCOD,
     $                          CACUSR,CACTVL,CTCODE(2),TSTEP(2),NUMINC
      GO TO 20
 10   CONTINUE
C       error - read format error
        ECHBUF= UCIBUF
        I= 80
        CALL OMSTC(I,ECHBU1)
        SGRP= 10
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             ECOUNT)
        ERRFLG= 1
 20   CONTINUE
      IF (SPOUT .GE. 10) THEN
C       echo initial read
        WRITE (MESSU,*) ':',COPTYP,':',TOPFST,TOPLST,':',CTCODE(1),':',
     $                  TSTEP(1),(DATIM(I),I=1,5),DSIND,TYPCOD,':',
     $                  CACUSR,':',CTCODE(2),':',TSTEP(2),NUMINC
      END IF
C
      IF (ERRFLG .EQ. 0) THEN
C       check operation type and store in integer version
        TOPTYP= CHKSTR (I6,I60,COPTY1,OPTYL1)
        IF (TOPTYP .LE. 0) THEN
C         error - operation type not recognized
          ERRFLG= 1
        ELSE IF (LOSPFL(TOPTYP) .EQ. 0) THEN
C         error - specified operation type not supported for spec actions
          ERRFLG= 1
        END IF
        IF (ERRFLG .EQ. 1) THEN
C         write error message
          I= 6
          CALL OMSTC(I,COPTY1)
          SGRP= 11
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
          TOPTYP= 0
        END IF
        IF (SPOUT .GE. 10) THEN
C         echo operation type
          WRITE (MESSU,*) '  operation type',TOPTYP
        END IF
      END IF
C
      IF (ERRFLG .EQ. 0) THEN
C       check that specified operation-type range is valid
        OECNT= ECOUNT
        CALL TOPTNO (MESSU,MSGFL,
     M               TOPFST,TOPLST,ECOUNT)
        IF (OECNT .NE. ECOUNT) THEN
C         an error was found
          ERRFLG= 1
        END IF
        IF (SPOUT .GE. 10) THEN
C         echo target operation range
          WRITE (MESSU,*) '  topfst,toplst',TOPFST,TOPLST
        END IF
      END IF
C
      DO 30 I= 1,2
C       check time units code
        CALL CKTCOD (CTCODE(I),MESSU,MSGFL,SCLU,
     M               ECOUNT,ERRFLG,
     O               TCODE(I))
        IF (SPOUT .GE. 10) THEN
C         echo time code
          WRITE (MESSU,*) '  time code',TCODE(I)
        END IF
C
        IF (ERRFLG .EQ. 0) THEN
          IF ( (I .EQ. 1) .AND. (TCODE(I) .GT. 4) ) THEN
C           error - tcode cannot be months or years for deferral
            CALL OMSTC (I2,CTCODE(1))
            SGRP= 12
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 ECOUNT)
            ERRFLG= 1
          ELSE
C           check time step
            IF ( (TSTEP(I) .EQ. 0) .AND. (I .EQ. 2) ) THEN
C             repeat time step defaults to one
              TSTEP(I)= 1
            END IF
C           timestep
            IF (TSTEP(I) .LT. 0) THEN
              CALL OMSTI (TSTEP(I))
              SGRP= 16
              CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                   ECOUNT)
              ERRFLG= 1
            END IF
            IF (SPOUT .GE. 10) THEN
C             echo time step
              WRITE (MESSU,*) '  time step',TSTEP
            END IF
          END IF
        END IF
 30   CONTINUE
C
      IF ( (ERRFLG .EQ. 0) .AND. (TSTEP(1) .GT. 0) )THEN
C       convert deferral time code and step to minutes
        ALLDEL= -TSTEP(1)*(TCMIN(TCODE(1)))
      ELSE
C       no deferral
        ALLDEL= 0
      END IF
C
      IF (ERRFLG .EQ. 0) THEN
C       check parameters, first type code
        IF (TYPCOD .EQ. 0) THEN
C         default to real
          TYPCOD= 3
        END IF
        IMIN= 2
        IMAX= 4
        IF ( (TYPCOD .LT. IMIN) .OR. (TYPCOD .GT. IMAX) ) THEN
C         error - bad type code
          CALL OMSTI(TYPCOD)
          CALL OMSTI(IMIN)
          CALL OMSTI(IMAX)
          SGRP= 13
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
          ERRFLG= 1
        END IF
        IF (SPOUT .GE. 10) THEN
C         echo type code
          WRITE (MESSU,*) '  type code',TYPCOD
        END IF
C       number of times to repeat
        IF (NUMINC .EQ. 0) THEN
C         number of repeats defaults to one
          NUMINC= 1
        END IF
        IF (NUMINC .LT. 1) THEN
C         error - repeat value was negative
          CALL OMSTI (NUMINC)
          SGRP= 15
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
          ERRFLG= 1
        END IF
        IF (SPOUT .GE. 10) THEN
C         echo number of repeats
          WRITE (MESSU,*) '  number of repeats',NUMINC
        END IF
      END IF
C
      IF (ERRFLG .EQ. 0) THEN
C       read value - format depends on type.
        RVAL= 0.0
        IVAL=   0
        ACTUVQ= 0
        I= MXSPVQ* 6
        CALL MKQUAN (MESSU,MSGFL,SCLU,I,UVQNM1,NVQD,TYPCOD,
     M               CACTVL,ERRFLG,ECOUNT,IVAL,RVAL,ACTUVQ)
        IF (SPOUT .GE. 10) THEN
C         echo action info
          IF (ACTUVQ .NE. 0) THEN
C           variable
            WRITE (MESSU,*) '  action uvq',ACTUVQ
          ELSE IF (TYPCOD .EQ. 2) THEN
C           integer
            WRITE (MESSU,*) '  action value',IVAL
          ELSE
C           real
            WRITE (MESSU,*) '  action value',RVAL
          END IF
        END IF
      END IF
C
      IF ( (ERRFLG .EQ. 0) .AND. (RUNWID .NE. 1) ) THEN
C       check for tighter run width restriction
        IF (SPOUT .GE. 10) THEN
C         echo mesage for beginning of run width check
          WRITE (MESSU,*) '  Beginning to calculate RUNWID',RUNWID
        END IF
        IF (ACTUVQ .GT. 0) THEN
C         check action quantity
          IF (UVQOPX(ACTUVQ) .GT. 0) THEN
C           base variable is in osv, not workspace
            CALL CKRWID (UVQOPX(ACTUVQ),UVQLAG(ACTUVQ),COPTYP,TOPFST,
     I                   TOPLST,MAXOPN,OPNTAB,NOPNS,
     M                   RUNWID)
          END IF
        END IF
        IF ( (CURBLK .GT. 0) .AND. (RUNWID .NE. 1) ) THEN
C         check all logic chains for current logic block
C
          ICHAIN= 1
 40       CONTINUE
C           do-while no more links
            CNDIND= CHNCND(BLKPOS(CURBLK)+ ICHAIN- 1)
            IF (CNDIND .GT. 0) THEN
C             check chain
 50           CONTINUE
C               check user-defined quantities in individual condition
                IF ( (CNDIND .GT. 0) .AND. (RUNWID .NE. 1) ) THEN
C                 find index of possible user-defined quantities
                  DO 60 I= 1, 2
C                   check comparison values
                    UVQIND= CNDUVQ(I,CNDIND)
                    IF (UVQIND .GT. 0) THEN
C                     quantity is a user-defined variable quantity
                      IF (UVQOPX(UVQIND) .GT. 0) THEN
C                       base variable is in osv, not workspace
                        CALL CKRWID (UVQOPX(UVQIND),UVQLAG(UVQIND),
     I                               COPTYP,TOPFST,TOPLST,MAXOPN,OPNTAB,
     I                               NOPNS,
     M                               RUNWID)
                      END IF
                    END IF
 60               CONTINUE
                END IF
C
C               check link
                CNDIND= CNDLNK(CNDIND)
C             end of do-until loop on links
              IF ( (CNDIND .GT. 0) .AND. (RUNWID .NE. 1) ) GO TO 50
            END IF
            IF (SPOUT .GE. 10) THEN
C             echo message for end of run width calculating
              WRITE (MESSU,*) '  Finished calculating RUNWID',RUNWID
            END IF
C
C         end if do-until loop on chains
          ICHAIN= ICHAIN+ 1
          IF ( (ICHAIN .LE. BLKCNT(CURBLK)) .AND.
     $         (RUNWID .NE. 1) ) GO TO 40
        END IF
      END IF
C
      IF (ERRFLG .EQ. 0) THEN
C       action code
C
        READ (CACUSR,1010,ERR=70) ACTCD
C       number was input
C
C       check action code
        IMIN= 1
        IMAX= 16
        IF ( (ACTCD .LT. IMIN) .OR. (ACTCD .GT. IMAX) ) THEN
C         error - bad action code
          CALL OMSTI (ACTCD)
          CALL OMSTI (IMIN)
          CALL OMSTI (IMAX)
          SGRP= 14
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
          ERRFLG= 1
        END IF
        GO TO 90
C
 70     CONTINUE
C       character was input - left justify, make uppercase, and evaluate
        I= 3
        CALL LFTSTR (I,
     M               CACUS1)
        CALL QUPCAS (I,
     M               CACUS1)
        ACTCD= 0
        DO 80 I= 1, 13
          IF (CACUSR .EQ. CACTCD(I)(1:3) ) THEN
C           found character action code
            ACTCD= I
          END IF
 80     CONTINUE
        IF (ACTCD .EQ. 0) THEN
C         error - bad character action code
          I= 3
          CALL OMSTC (I,CACUS1)
          SGRP= 9
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
          ERRFLG= 1
        END IF
C
 90     CONTINUE
        IF (SPOUT .GE. 10) THEN
C         echo action code
          WRITE (MESSU,*) '  action code',ACTCD
        END IF
      END IF
C
      IF ( (ERRFLG .EQ. 0) .AND. (DSIND .NE. 0) ) THEN
C       check distribution stuff
        IF ( (DSIND .GT. 0) .AND. (DSIND .LT. MXSPDS) ) THEN
C         index in allowable range
          IF (SPDCNT(DSIND) .EQ. 0) THEN
C           a distribution does not exist
            DSPOS= 0
          ELSE
C           start with first fraction
            DSPOS= 1
          END IF
        ELSE
C         distribution number out of range
          DSPOS= 0
        END IF
        IF (DSPOS .EQ. 0) THEN
C         problem with distribution
          CALL OMSTI (DSIND)
          SGRP= 17
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
          ERRFLG= 1
        END IF
      ELSE
C       no distribution for this action
        DSPOS= 0
      END IF
      IF (SPOUT .GE. 10) THEN
C       echo distribution index
        WRITE (MESSU,*) '  distribution ID',DSIND
      END IF
C
      IF (ERRFLG .EQ. 0) THEN
        IF (SPUCNT .GT. 0) THEN
C         check for user defined name
          READ (UCIBUF(43:48),1020) UVNAME
          OBUFF(1:6)= UVNAME
          UKWDNO= CHKSTR (I6,SPUCNT,OBUF1,SPUVN1)
          IF (UKWDNO .NE. 0) THEN
C           the variable specified is user defined
            IF (SPOUT .GE. 10) THEN
C             echo uvname index
              WRITE (MESSU,*) '  user defined target variable:',UKWDNO
            END IF
C
            IF (SPUTYP(UKWDNO) .EQ. 0) THEN
C             target variable is in workspace
              TOPTYP= 0
            ELSE IF (TOPTYP .NE. SPUTYP(UKWDNO)) THEN
C             wrong operation type
              ECHBUF(1:6)= UCIBUF(43:48)
              ECHBUF(7:12)= COPTYP
              I= 12
              CALL OMSTC(I,ECHBU1)
              SGRP= 18
              CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                   ECOUNT)
              ERRFLG= 1
            END IF
          END IF
        ELSE
C         no user names available
          UKWDNO= 0
        END IF
        IF (UKWDNO .EQ. 0) THEN
C         need to determine address
          CALL MKADDR (LOCDIR,UCIBUF(43:57),MESSU,MSGFL,SCLU,BGRP,
     M                 TOPTYP,ERRFLG,
     O                 VNAME,NSUB,ADDR)
        END IF
        IF (SPOUT .GE. 10) THEN
C         echo address of target variable
          WRITE (MESSU,*) '  target address',ADDR
        END IF
      END IF
C
      IF (ERRFLG .EQ. 0) THEN
C       process headers
        HDRPTR= FIRSTH
        HDRNUM= NUMHDR
        NUMHDR= 0
        FIRSTH= 0
        LREPT=  NUMINC
      END IF
C
      IF (ERRFLG .EQ. 0) THEN
C       process dates
C       loop here for repeat instruction
 100    CONTINUE
          IF (DATIM(1) .EQ. 0) THEN
C           action is undated
            IF (NUMINC .GT. 1) THEN
C             error - undated action doesn't need to be repeated
              CALL OMSTI (NUMINC)
              SGRP= 19
              CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                   ECOUNT)
              ERRFLG= 1
            ELSE IF (DSIND .NE. 0) THEN
C             error - undated action cannot be distributed
              CALL OMSTI (DSIND)
              SGRP= 20
              CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                   ECOUNT)
              ERRFLG= 1
            ELSE IF (ACTCD .GE. 14) THEN
C             error - undated action incompatible with MOV* action
              WRITE (OBUFF,2000) UVNAME
              I= 6
              CALL OMSTC (I,OBUF1(1))
              CALL OMSTI (ACTCD)
              SGRP= 21
              CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                   ECOUNT)
              ERRFLG= 1
            ELSE IF (CURBLK .GT. 0) THEN
C             undated conditional - check if deferral is specified
              IF (ALLDEL .LT. 0) THEN
C               error - undated action cannot be deferred
                SGRP= 22
                CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                     ECOUNT)
                ERRFLG= 1
              END IF
            END IF
            IF (ERRFLG .EQ. 0) THEN
C             store delt as flag and temporarily use run start date
              ALLDEL= DELT
              CALL COPYI (I5,SDATIM,
     O                    DATIM)
            END IF
            IF (SPOUT .GE. 10) THEN
C             echo undated message
              WRITE (MESSU,*) '  undated, alldel',ALLDEL,' datim',
     $                           (DATIM(I),I=1,5)
            END IF
          ELSE
C           check supplied date/time and convert to internal format
            OECNT = ECOUNT
            CALL STDATE (NDAMON,MESSU,MSGFL,
     M                   ECOUNT,DATIM)
            IF (OECNT .NE. ECOUNT) THEN
C             an error was found
              ERRFLG= 1
            END IF
            IF (SPOUT .GE. 10) THEN
C             echo date
              WRITE (MESSU,*) '  date',DATIM
            END IF
          END IF
C
          IF (ERRFLG .EQ. 0) THEN
C           if the time for this entry is outside run span, ignore it
            CALL DIFTIM (SDATIM,DATIM,NDAMON,
     O                   DIF1)
            CALL DIFTIM (DATIM,EDATIM,NDAMON,
     O                   DIF2)
            IF (SPOUT .GE. 10) THEN
C             echo time differences
              WRITE (MESSU,*) '      DIF1:',DIF1,DIF2
            END IF
            IF ( (DIF1 .GE. 0) .AND. (DIF2 .GE. 0) ) THEN
C             this entry in run span, echo and save instruction
C             local copy of user defined variable index
              XKWDNO= UKWDNO
              IF (UKWDNO .GT. 0) THEN
                UPOS= SPUPOS(UKWDNO)
              END IF
C             assume no movement between locations
              MOVFLG= 0
 110          CONTINUE
C               may loop thru variables referenced by user variables
                IF (XKWDNO .NE. 0) THEN
C                 user defined variable name
                  IF (SPUTYP(UKWDNO) .EQ. 0) THEN
C                   target variable is in workspace
                    VNAME= SPUVNM(UPOS)
                    NSUB(1)= 0
                    NSUB(2)= 0
                    NSUB(3)= 0
                  ELSE
C                   osv variable
                    VNAME= SPUNAM(UPOS)
                    NSUB(1)= SPUSUB(1,UPOS)
                    NSUB(2)= SPUSUB(2,UPOS)
                    NSUB(3)= SPUSUB(3,UPOS)
                  END IF
                  ADDR= SPUADD(UPOS)
                  IF (TYPCOD .GE. 3) THEN
C                   adjust value by fraction
                    TRVAL= RVAL* SPUFRC(UPOS)
                  END IF
                  IF (SPUACT(UPOS) .GE. 1) THEN
C                   need to move between values
                    IF (DSIND .NE. 0) THEN
C                     movement not compatible with distribution
                      SGRP= 23
                      CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                           ECOUNT)
                      ERRFLG= 1
                    END IF
                    IF (CURBLK .NE. 0) THEN
C                     check condition for deferral on failure
                      IF (ALLDEL .LT. 0) THEN
C                       movement not compatible with deferral on condition
C                       failure
                        SGRP= 24
                        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                             ECOUNT)
                        ERRFLG= 1
                      END IF
                    END IF
                    IF (ERRFLG .EQ. 0) THEN
C                     movement is ok - modify instruction
                      TACTCD= SPUACT(UPOS)+ 13
                      TRVAL = SPUFRC(UPOS)
                      MOVFLG= 1
                    END IF
                  ELSE
C                   use general action code from instruction
                    TACTCD= ACTCD
                  END IF
                  UPOS= UPOS+ 1
                  IF (UPOS .GE. SPUPOS(XKWDNO+1)) THEN
C                   all done user defined name
                    XKWDNO= 0
                  END IF
                ELSE
C                 not user defined - no adjustment to action code or value
                  TACTCD= ACTCD
                  IF (TYPCOD .GE. 3) THEN
C                   real or double precision
                    TRVAL = RVAL
                  END IF
                END IF
C
C               check for accumulator address
                ACCADD= 0
                IF ( (TACTCD .EQ. 2) .AND. (TOPTYP .GT. 0) ) THEN
C                 check this "increment" action to see if it requires
C                 updating an accumulator
                  IF (VACFST(TOPTYP) .GT. 0) THEN
C                   there are accumulators defined for this operation type
                    I= VACFST(TOPTYP)- 1
                    IMAX= I+ VACCNT(TOPTYP)
 120                CONTINUE
                      I= I+ 1
                      IF (ADDR .EQ. VRFADD(I)) THEN
C                       this action must be accumulated in osv
                        ACCADD= VACADD(I)
                      END IF
                    IF ( (ACCADD .EQ. 0) .AND. (I .LT. IMAX) ) GO TO 120
                  END IF
                END IF
C
C               echo and store an individual action in the buffer
                SPOS= SPOS+ 1
                IF (SPOUT .GE. 10) THEN
C                 echo instruction position
                  WRITE (MESSU,*) '   save at:',SPOS
                END IF
C
                IF (OUTLEV .GT. 2) THEN
C                 echo instruction
                  IF (SPOS .EQ. 1) THEN
C                   details echo header
                    WRITE (MESSU,2010)
                  END IF
C
C                 convert time to external format
                  CALL EXDATE (DATIM,
     O                         EXDAT)
                  IF (MOVFLG .LE. 1) THEN
C                   full output
                    WRITE (ECHBUF,2020) COPTYP,TOPFST,TOPLST,TSTEP(1),
     $                                  CODES(TCODE(1)),EXDAT,TYPCOD,
     $                                  DSIND,ADDR,CACTCD(TACTCD)
                    IF (ACTUVQ .GE. 1) THEN
C                     user-defined variable quantity
                      WRITE (ECHBUF(98:103),2000) UVQNAM(ACTUVQ)
                    ELSE IF (TYPCOD .EQ. 2) THEN
C                     integer
                      WRITE (ECHBUF(94:103),2030) IVAL
                    ELSE
C                     real or double
                      CALL DECCHX (TRVAL,RLEN,SDIG,DECP,
     O                             ECHBU1(94))
                    END IF
                    IF (MOVFLG .EQ. 1) THEN
C                     partial output next time
                      MOVFLG= 2
                    END IF
                  ELSE
C                   partial output with second variable of move
                    WRITE (ECHBUF,2040) ADDR
C                   partial output complete
                    MOVFLG= 0
                  END IF
C
                  IF (VNAME .NE. BLANK) THEN
C                   output variable name
                    ECHBUF(57:62)= VNAME
                    IF (NSUB(1) .GE. 1) THEN
C                     at least one subscript
                      WRITE (ECHBUF(63:65),2050) NSUB(1)
                    END IF
                    IF (NSUB(2) .GE. 1) THEN
C                     at least two subscripts
                      WRITE (ECHBUF(66:68),2050) NSUB(2)
                    END IF
                    IF (NSUB(3) .GE. 1) THEN
C                     at least three subscripts
                      WRITE (ECHBUF(69:71),2050) NSUB(3)
                    END IF
                  END IF
C                 write the echo buffer
                  WRITE (MESSU,2060) ECHBUF
                END IF
C
                IF (ADDR .LT. 0) THEN
C                 target variable is in workspace
                  UVQ= 0
 130              CONTINUE
                    UVQ= UVQ+ 1
                    IF (UVQ .LE. NVQD) THEN
C                     check this
                      IF (ADDR .EQ. UVQADD(UVQ)) THEN
C                       put index into last subscript with negative value
                        NSUB(3)= -UVQ
                      END IF
                    END IF
                  IF ((NSUB(3) .EQ. 0) .AND. (UVQ .LT. NVQD)) GO TO 130
C
                  IF (NSUB(3) .EQ. 0) THEN
C                   error - no uvquan found at target address
                    WRITE (OBUFF,2000) UVNAME
                    I= 6
                    CALL OMSTC (I,OBUF1(1))
                    I= -ADDR
                    CALL OMSTI (I)
                    SGRP= 29
                    CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                         ECOUNT)
                    ERRFLG= 1
                  END IF
                END IF
C
C               save instruction in memory
                IF (ALLDEL .LE. 0) THEN
C                 dated
                  DO 140 I= 1,5
                    SPBF(I,SPOS)= DATIM(I)
 140              CONTINUE
                ELSE
C                 undated
                  DO 150 I= 1,5
                    SPBF(I,SPOS)= 0
 150              CONTINUE
                END IF
                SPBF(6,SPOS)= TYPCOD
                SPBF(7,SPOS)= ADDR
                SPBF(8,SPOS)= TACTCD
                IF (TYPCOD .EQ. 2) THEN
C                 integer type
                  SPBF(9,SPOS)= IVAL
                ELSE
C                 real or double precision
                  SPBFR(9,SPOS)= TRVAL
                END IF
                READ (VNAME,1030) SPBF(10,SPOS),SPBF(11,SPOS)
                SPBF(12,SPOS) = NSUB(1)
                SPBF(13,SPOS) = NSUB(2)
                SPBF(14,SPOS) = NSUB(3)
                READ (COPTYP,1030) SPBF(15,SPOS),SPBF(16,SPOS)
                SPBF(17,SPOS) = TOPFST
                SPBF(18,SPOS) = TOPLST
C               distribution index
                SPBF(19,SPOS) = DSIND
C               current positon within distribution
                SPBF(20,SPOS) = DSPOS
C               deferred fraction of distribuiton
                SPBFR(21,SPOS)= 0.0
C               logic block
                SPBF(22,SPOS)= CURBLK
C               uvquan for action value
                SPBF(23,SPOS)= ACTUVQ
C               tstep for continuous action or deferral
                SPBF(24,SPOS)= ALLDEL
C               address for accumulator
                SPBF(25,SPOS)= ACCADD
C               uci order key for sorting mixed dated and undated actions
                SPBF(26,SPOS)= SPOS
C               pointer to first header line in buffer
                SPBF(27,SPOS)= HDRPTR
C               number of header lines in buffer
                SPBF(28,SPOS)= HDRNUM
C               initialize footer pointers
                SPBF(29,SPOS)= 0
                SPBF(30,SPOS)= 0
C               offset from beginning of run - will be zero for undated
                SPBDAT(SPOS)= DIF1
C             loop back for more references from user defined name
              IF (XKWDNO .GT. 0) GO TO 110
            ELSE
C             this date is not active
              IF (LREPT .GT. 1) THEN
C               don't repeat footer for this action
                LREPT= LREPT- 1
              END IF
            END IF
C           one less action to do
            NUMINC= NUMINC- 1
            IF (NUMINC .GT. 0) THEN
              CALL SPNXDT (TCODE(2),TSTEP(2),I0,
     M                     DATIM)
              IF (SPOUT .GE. 10) THEN
C               echo new date for repeated action
                WRITE (MESSU,*) '     new DATIM:',DATIM,NUMINC
              END IF
            END IF
          END IF
        IF ( (NUMINC .GT. 0) .AND. (ERRFLG .EQ. 0) ) GO TO 100
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   CKTCOD
     I                    (CTCODE,MESSU,MSGFL,SCLU,
     M                     ECOUNT,ERRFLG,
     O                     TCODE)
C
C     + + + PURPOSE + + +
C     ckeck a character time code and return its integer value,
C     report error if problem
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MESSU,MSGFL,SCLU,ECOUNT,ERRFLG,TCODE
      CHARACTER*2 CTCODE
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CTCODE - character time code
C     MESSU  - unit number of file to write error message on
C     MSGFL  - unit number of file containing error text
C     SCLU   - cluster in file containing error text
C     TCODE  - integer time code
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,SGRP
      CHARACTER*1 CTCOD1(2)
C
C     + + + EXTERNALS + + +
      EXTERNAL    OMSTI,OMSTC,OMSG,CVARAR
C
C     + + + END SPECIFICATIONS + + +
C
      IF (CTCODE .EQ. '  ') THEN
C       default to minutes
        TCODE= 2
      ELSE IF (CTCODE .EQ. 'MI') THEN
C       minutes
        TCODE= 2
      ELSE IF (CTCODE .EQ. 'HR') THEN
C       hours
        TCODE= 3
      ELSE IF (CTCODE .EQ. 'DY') THEN
C       days
        TCODE= 4
      ELSE IF (CTCODE .EQ. 'MO') THEN
C       months
        TCODE= 5
      ELSE IF (CTCODE .EQ. 'YR') THEN
C       years
        TCODE= 6
      ELSE
C       bad value
        TCODE= -1
        I= 2
        CALL CVARAR(I,CTCODE,I,
     O              CTCOD1)
        CALL OMSTC(I,CTCOD1)
        CALL OMSTI(TCODE)
        SGRP = 6
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             ECOUNT)
        ERRFLG= 1
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   MKADDR
     I                    (LOCDIR,INPBUF,MESSU,MSGFL,SCLU,BGRP,
     M                     TOPTYP,ERRFLG,
     O                     VNAME,NSUB,ADDR)
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      LOCDIR,MESSU,MSGFL,SCLU,BGRP,TOPTYP,ERRFLG,NSUB(3),
     $             ADDR
      CHARACTER*6  VNAME
      CHARACTER*15 INPBUF
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LOCDIR - specs method(s) available for spec. actions input
C              0 - variable name required;
C              1 - either variable name or address required;
C              2 - address required
C     INPBUF - buffer containing variable name portion of current record
C     MESSU  - unit number to write messages on
C     MSGFL  - unit number of file containg HSPF message text
C     SCLU   - cluster in file containing error text
C     TOPTYP - operation type, 0 means look to find it
C     ERRFLG - flag indicating whether an error has occurred processing line
C     VNAME  - variable name
C     NSUB   - subscripts associated with variable name
C     ADDR   - address of location specified
C
C     + + + COMMON BLOCK- SPEC + + +
      INCLUDE     'cspec.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,SUBCN1,SUBCN2,SUBFG,IND,VKWDNO,LOPTYP,I0,
     $             SGRP,I6,SIZE,SRANGE
      CHARACTER*3  CSUB(3)
      CHARACTER*6  BLANK,CWRKSP
      CHARACTER*15 CHSTR
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (CHSTR,CHSTR1)
      CHARACTER*1  CHSTR1(15)
C
C     + + + FUNCTIONS + + +
      INTEGER       CHKSTR
C
C     + + + INTRINSICS + + +
      INTRINSIC    ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL     OMSG,OMSTI,OMSTC,CHKSTR,TAGVAL
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (A6,3A3)
 1010 FORMAT (I3)
 1020 FORMAT (I8)
C
C     + + + END SPECIFICATIONS + + +
C
C     initialize
      I6= 6
      I0= 0
      BLANK= '      '
      CWRKSP= 'WORKSP'
C
C     read variable name
      READ (INPBUF,1000) VNAME,CSUB
C
      IF (VNAME(1:3) .NE. BLANK(1:3)) THEN
C       read subscripts
        DO 50 I= 1, 3
          READ (CSUB(I),1010,ERR=30) NSUB(I)
C           number read okay
          GO TO 40
 30       CONTINUE
C         not a valid number - try reading as a character tag
          CALL TAGVAL (CSUB(I)(2:3),I0,MESSU,MSGFL,SCLU,BGRP,
     M                 ECOUNT,
     O                 NSUB(I))
          IF (NSUB(I) .EQ. -999) THEN
C           an invalid tag was specified as subscript
            ERRFLG= 1
          END IF
 40       CONTINUE
 50     CONTINUE
      END IF
C
      IF (ERRFLG .EQ. 0) THEN
C       continue processing
        IF (VNAME .EQ. CWRKSP) THEN
C         variable is in workspace
          SUBFG= 0
          DO 55 I= 1, 3
            IF (NSUB(I) .LT. 0) THEN
C             error - negative subscript
              SUBFG= 1
            END IF
 55       CONTINUE
          IF (SUBFG .EQ. 1) THEN
C           error - at least one of the specified
C           subscripts exceeds the corresponding
C           dimension of the variable array
            CHSTR(1:6)= VNAME
            CALL OMSTC (I6,CHSTR1)
            SGRP= 5
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 ECOUNT)
            ADDR  = -999
            ERRFLG= 1
          ELSE IF (NSUB(1) .EQ. 0) THEN
C           error - number of nonzero subscripts specified
C           does not correspond to number of dimensions of
C           variable
            CHSTR(1:6)= VNAME
            CALL OMSTC (I6,CHSTR1)
            I= 3
            CALL OMSTI (I)
            SGRP= 4
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 ECOUNT)
            ADDR  = -999
            ERRFLG= 1
          ELSE
C           compute address from as many subscripts as are found
            ADDR= -NSUB(1)
            IF (NSUB(2) .GT. 1) THEN
C             second subscript
              ADDR= ADDR- 1000*NSUB(2)
            END IF
            IF (NSUB(3) .GT. 1) THEN
C             third subscript
              ADDR= ADDR- 1000000*NSUB(3)
            END IF
          END IF
        ELSE IF (VNAME(1:3) .EQ. BLANK(1:3)) THEN
C         assume user has supplied address
          IF (LOCDIR .EQ. 0) THEN
C           error - attempting to read an address when the
C           variable name is required
            SGRP= 1
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 ECOUNT)
            ADDR  = -999
            ERRFLG= 1
          ELSE
C           read address directly from the ucifl (old format)
            READ (INPBUF,1020,ERR=60) ADDR
              GO TO 70
 60         CONTINUE
C             read format error
              I= 15
              CHSTR= INPBUF
              CALL OMSTC (I,CHSTR1)
              SGRP= 8
              CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                   ECOUNT)
              ADDR  = -999
              ERRFLG= 1
 70         CONTINUE
          END IF
        ELSE IF (LOCDIR .EQ. 2) THEN
C         error - attempting to read a variable name when
C         the address is required
          SGRP= 2
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
          ADDR= -999
C         fill in name and subs with dummy walues
          VNAME= BLANK
          NSUB(1) = 0
          NSUB(2) = 0
          NSUB(3) = 0
          ERRFLG  = 1
        ELSE
C         search variable name library for the input name
          LOPTYP= 0
 80       CONTINUE
            IF (TOPTYP .EQ. 0) THEN
C             try the next type
              LOPTYP= LOPTYP+ 1
            ELSE
C             try the specified type
              LOPTYP= TOPTYP
            END IF
            IND= LONAM(LOPTYP)
            IF (IND .GT. 0) THEN
C             the library exists- check it
              CHSTR= VNAME
              IND= (IND-1)*6+ 1
              VKWDNO= CHKSTR (I6,LONUM(LOPTYP),CHSTR1,VNAML1(IND))
            ELSE
C             no library, cant determine name
              VKWDNO= 0
            END IF
          IF (TOPTYP.EQ.0 .AND. VKWDNO.EQ.0 .AND. LOPTYP.LT.10) GO TO 80
C
          IF (TOPTYP.EQ.0 .AND. VKWDNO.NE.0) THEN
C           found the name, save the operation type
            TOPTYP= LOPTYP
          END IF
C
          IF (VKWDNO .EQ. 0) THEN
C           error - variable name not found in vnamlb
            CHSTR(1:6)= VNAME
            CALL OMSTC (I6,CHSTR1)
            SGRP= 3
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 ECOUNT)
            ADDR  = -999
            ERRFLG= 1
          ELSE
C           variable name found - correct index and check subscripts
C           if the first subscript is negative, the variable is
C           double precision.  If it is -1, then it takes no subscripts
            VKWDNO= VKWDNO+ LONAM(TOPTYP)- 1
            SUBFG = 0
            SUBCN1= 0
            SUBCN2= 0
            DO 90 I=1,3
              IF ( (VDIM(I,VKWDNO) .NE. 0) .AND.
     $             (VDIM(I,VKWDNO) .NE. -1) .AND.
     $             (NSUB(I) .EQ. 0) ) THEN
C               default to one
                NSUB(I)= 1
              END IF
              IF (NSUB(I) .GE. 1) THEN
C               count this nonzero subscript
                SUBCN1= SUBCN1 + 1
              END IF
              IF ( (VDIM(I,VKWDNO) .NE. 0) .AND.
     $             (VDIM(I,VKWDNO) .NE. -1) ) THEN
C               count this nonzero possible subscript
                SUBCN2= SUBCN2 + 1
              END IF
              IF (VDIM(I,VKWDNO) .EQ. -1) THEN
C               double precision - no subscripts
                SRANGE= 0
              ELSE
C               subscript needed
                SRANGE= ABS(VDIM(I,VKWDNO))
              END IF
              IF (NSUB(I) .GT. SRANGE) THEN
C               this subscript is out of range
                SUBFG= 1
              END IF
 90         CONTINUE
            IF (SUBCN1 .NE. SUBCN2) THEN
C             error - number of nonzero subscripts specified
C             does not correspond to number of dimensions of
C             variable
              CHSTR(1:6)= VNAME
              CALL OMSTC (I6,CHSTR1)
              CALL OMSTI (SUBCN2)
              SGRP= 4
              CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                   ECOUNT)
              ADDR  = -999
              ERRFLG= 1
            ELSE IF (SUBFG .GT. 0) THEN
C             error - at least one of the specified
C             subscripts exceeds the corresponding
C             dimension of the variable array
              CHSTR(1:6)= VNAME
              CALL OMSTC (I6,CHSTR1)
              SGRP= 5
              CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                   ECOUNT)
              ADDR  = -999
              ERRFLG= 1
            ELSE
C             looks good, set base address
              ADDR= VLOC(VKWDNO)
              IF (VDIM(1,VKWDNO) .LT. 0) THEN
C               negative first subscript means double precision
                SIZE= 2
              ELSE
C               integer or real
                SIZE= 1
              END IF
              IF (SUBCN2 .EQ. 1) THEN
C               one dimensional array
                ADDR= ADDR+ SIZE * (NSUB(1)- 1)
              ELSE IF (SUBCN2 .EQ. 2) THEN
C               two dimensional array
                ADDR= ADDR+ SIZE * (NSUB(1)- 1 +
     $                ABS(VDIM(1,VKWDNO))*(NSUB(2)- 1))
              ELSE IF (SUBCN2 .EQ. 3) THEN
C               three dimensional array
                ADDR= ADDR+ SIZE * (NSUB(1)- 1 +
     $                ABS(VDIM(1,VKWDNO))*((NSUB(2)- 1) +
     $                ABS(VDIM(2,VKWDNO))*(NSUB(3)- 1)))
              END IF
            END IF
          END IF
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   SPNXDT
     I                    (TCODE,TSTEP,INTFG,
     M                     DATIM)
C
C     + + + PURPOSE + + +
C     increment hspf format date
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   TCODE,TSTEP,INTFG,DATIM(5)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     TCODE  - time code
C     TSTEP  - time step
C     INTFG  - internal date flag - 0: external format date, 1: internal
C     DATIM  - current special action date
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,CURDAT(5),STPOS,CARRY,DPM,DONFG
C
C     + + + FUNCTIONS + + +
      INTEGER   DAYMON
C
C     + + + EXTERNALS + + +
      EXTERNAL  DAYMON,EXDATE
C
C     + + + END SPECIFICATIONS + + +
C
C     make working copy
      DO 10 I= 1,5
        CURDAT(I)= DATIM(I)
 10   CONTINUE
C
C     increment time
C
      CARRY= TSTEP
      STPOS= TCODE- 1
C     add the time, not changing insig. parts
      GO TO (20,30,40,60,70), STPOS
C
 20   CONTINUE
C       minutes
C
C       add minutes carried
        CURDAT(5)= CURDAT(5)+ CARRY
C
C       determine extra hours to carry
        CARRY    = (CURDAT(5)-1)/ 60
C
C       subtract extra hours from minutes
        CURDAT(5)= CURDAT(5)- (CARRY*60)
C
      IF (CARRY .EQ. 0) GO TO 80
 30   CONTINUE
C       hours
C
C       add hours carried
        CURDAT(4)= CURDAT(4)+ CARRY
C
C       determine extra days to carry
        CARRY    = (CURDAT(4)-1)/ 24
C
C       subtract extra days from hours
        CURDAT(4)= CURDAT(4)- (CARRY*24)
C
      IF (CARRY .EQ. 0) GO TO 80
 40   CONTINUE
C       days
C
C       add days carried
        CURDAT(3)= CURDAT(3)+ CARRY
C
        DONFG= 0
C       do-until loop
 50     CONTINUE
          DPM= DAYMON (CURDAT(1),CURDAT(2))
          IF (CURDAT(3).GT.DPM) THEN
C
C           subtract month from days
            CURDAT(3)= CURDAT(3)- DPM
C
C           add month
            CURDAT(2)= CURDAT(2)+ 1
            IF (CURDAT(2) .GT. 12) THEN
C             new year
              CURDAT(1)= CURDAT(1)+ 1
              CURDAT(2)= 1
            END IF
          ELSE
            DONFG= 1
          END IF
        IF (DONFG.EQ.0) GO TO 50
C
C       years updated already, so done
        CARRY= 0
      GO TO 80
 60   CONTINUE
C       months
C
C       add months carried
        CURDAT(2)= CURDAT(2)+ CARRY
C
C       determine years to carry
        CARRY    = (CURDAT(2)-1)/ 12
C
C       subtract years carried
        CURDAT(2)= CURDAT(2)- (CARRY*12)
C
      IF (CARRY .EQ. 0) GO TO 80
 70   CONTINUE
C       years
        CURDAT(1)= CURDAT(1)+ CARRY
C
C     end computed go to
 80   CONTINUE
C
      IF (TCODE .GE. 5) THEN
C       check days/month when adding months or years only
        DPM= DAYMON (CURDAT(1),CURDAT(2))
        IF (DAYMON (DATIM(1),DATIM(2)) .EQ. DATIM(3)) THEN
C         stay at end of month regardless of length
          CURDAT(3)= DPM
        ELSE IF (DPM .LT. CURDAT(3)) THEN
C         this month shorter than previous - use end of month
          CURDAT(3)= DPM
        END IF
      END IF
C
      IF (INTFG .EQ. 0) THEN
C       convert new time to external format and make it current
        CALL EXDATE (CURDAT,
     O               DATIM)
        IF (DATIM(4) .EQ. 24) THEN
C         too many hours
          DATIM(4)= 0
          DATIM(3)= DATIM(3)+ 1
          IF (DATIM(3).GT.DAYMON(DATIM(1),DATIM(2))) THEN
C           too many days
            DATIM(3)= 1
            DATIM(2)= DATIM(2)+ 1
            IF (DATIM(2) .GT. 12) THEN
C             too many months
              DATIM(2)= 1
              DATIM(1)= DATIM(1)+ 1
            END IF
          END IF
        END IF
      ELSE
C       leave in internal format
        DO 110 I= 1,5
          DATIM(I)= CURDAT(I)
 110    CONTINUE
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   SPCKDT
     I                    (DATE1,DATE2,
     O                     FLAG)
C
C     + + + PURPOSE + + +
C     determine order of two dates
C     (adapted from CKDATE)
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   DATE1(5),DATE2(5),FLAG
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DATE1  - first date
C     DATE2  - second date
C     FLAG   - order flag -1: 1<2, 0: 1=2, 1: 1>2
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,IDAT
C
C     + + + END SPECIFICATIONS + + +
C
C     check it
      FLAG= -99
      I= 0
 10   CONTINUE
        I= I+ 1
        IDAT= DATE2(I)
        IF (DATE1(I) .LT. IDAT) THEN
C         first date is sooner
          FLAG= -1
        ELSE IF (DATE1(I) .GT. IDAT) THEN
C         first date is later
          FLAG= 1
        ELSE IF (I .EQ. 5) THEN
C         all parts of date are equal
          FLAG= 0
        END IF
      IF (FLAG .EQ. -99) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE   SPWSUP
     I                    (SPIVL)
C
C     + + + PURPOSE + + +
C     Update user-defined quantities for Global Workspace variables.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     SPIVL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     SPIVL  - interval of run
C
C     + + + COMMON BLOCKS + + +
C     user defined variable quantity definitions
      INCLUDE     'pspvqd.inc'
      INCLUDE     'cspvqd.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER          UVQ,PTR1,PTR2
C
C     + + + INTRINSICS + + +
      INTRINSIC    MOD
C
C     + + + END SPECIFICATIONS + + +
C
      DO 10 UVQ= 1, NVQD
        IF (UVQOPX(UVQ) .LE. 0) THEN
C         this is a workspace variable or an internal boolean
          PTR2= MOD (SPIVL- 1,UVQLEN(UVQ))+ UVQPOS(UVQ)
          IF (UVQOPX(UVQ) .EQ. 0) THEN
C           workspace
            IF (SPIVL .GT. 1) THEN
C             update value from previous interval
              PTR1= MOD (SPIVL- 2+ UVQLEN(UVQ),UVQLEN(UVQ))+
     $              UVQPOS(UVQ)
              IF (UVQTYP(UVQ) .EQ. 2) THEN
C               integer
                IVQPIP(PTR2)= IVQPIP(PTR1)
              ELSE
C               real or dp
                UVQPIP(PTR2)= UVQPIP(PTR1)
              END IF
            END IF
          ELSE
C           internal boolean
            IVQPIP(PTR2)= -1
          END IF
        END IF
 10   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   SPDIST
     I                    (MESSU,SPOUT,DSIND,DSPOS,
     M                     RVAL,DEFFRC,
     O                     CFRAC)
C
C     + + + PURPOSE + + +
C     Handle distributed Special Action fractions.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MESSU,SPOUT,DSIND,DSPOS
      REAL        RVAL,DEFFRC,CFRAC
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSU  - ftn unit no. to be used for printout of messages
C     SPOUT  - runtime Special Action output level
C     DSIND  - index of current distribution
C     DSPOS  - position in current distribution this interval
C     RVAL   - action value for this interval
C     DEFFRC - deferred fraction of action value
C
C     + + + COMMON BLOCKS + + +
C     special action distributions
      INCLUDE     'pspdst.inc'
      INCLUDE     'cspdst.inc'
C
C     + + + LOCAL VARIABLES + + +
      REAL         MVFRAC
C
C     + + + INTRINSICS + + +
      INTRINSIC    FLOAT
C
C     + + + END SPECIFICATIONS + + +
C
C     get current fraction
      CFRAC= SPDFRC(DSPOS,DSIND)
      IF (SPOUT .GE. 9) THEN
C       echo distrib info
        WRITE (MESSU,*) 'use distribut',DSIND,DSPOS,CFRAC,RVAL
      END IF
C
      IF (DEFFRC .GT. 0.0) THEN
C       adjust cfrac if any deferred fraction
        IF (SPDDFG(DSIND) .EQ. 2) THEN
C         shift
          MVFRAC= DEFFRC / (FLOAT (SPDCNT(DSIND)- DSPOS+ 1))
          CFRAC= CFRAC+ MVFRAC
          DEFFRC= DEFFRC- MVFRAC
        ELSE IF (SPDDFG(DSIND) .EQ. 3) THEN
C         accum
          CFRAC= CFRAC+ DEFFRC
          DEFFRC= 0.0
        END IF
      END IF
C
C     adjust value
      RVAL= RVAL*CFRAC
      IF (SPOUT .GE. 9) THEN
C       echo new value
        WRITE (MESSU,*) 'adjusted RVAL,CFRAC',RVAL,CFRAC
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   SPVQUP
     I                    (SPIVL,SPOPNO,ADDR,ADDR2,ACCADD,INEWVL,RVAL,
     I                     RNEWVL,RNEWV2,DNEWVL,PIPEFG)
C
C     + + + PURPOSE + + +
C     Update User-defined Variable Quantity pipes after a Special Action
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER SPIVL,SPOPNO,ADDR,ADDR2,ACCADD,INEWVL,PIPEFG
      REAL    RVAL,RNEWVL,RNEWV2
      DOUBLE PRECISION DNEWVL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     SPIVL  - interval of run
C     SPOPNO - index of target operation
C     ADDR   - address of target location
C     ADDR2  - address of secondary target location
C     ACCADD - address of accumulator location
C     INEWVL - new integer value for pipe
C     RVAL   - action value for this interval
C     RNEWVL - new real value for pipe
C     RNEWV2 - second new real value for pipe
C     DNEWVL - new double precision value for pipe
C     PIPEFG - flag indicating whether to fill entire pipe, not just one value
C
C     + + + COMMON BLOCKS + + +
C     user defined variable quantity definitions
      INCLUDE     'pspvqd.inc'
      INCLUDE     'cspvqd.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,N,UVQ,PTR
C
C     + + + INTRINSICS + + +
      INTRINSIC    MOD,SNGL
C
C     + + + END SPECIFICATIONS + + +
C
      DO 10 UVQ= 1, NVQD
        IF ( ( (UVQOPX(UVQ) .EQ. SPOPNO) .OR.
     $         (UVQOPX(UVQ) .EQ. 0) ) .AND.
     $       ( (UVQADD(UVQ) .EQ. ADDR) .OR.
     $         (UVQADD(UVQ) .EQ. ADDR2) .OR.
     $         (UVQADD(UVQ) .EQ. ACCADD) ) ) THEN
C         update pipe
C
          IF (PIPEFG .EQ. 1) THEN
C           must fill entire pipe, not just single value
            N= UVQLEN(UVQ)
            PTR= UVQPOS(UVQ)
          ELSE
C           only update last value
            N= 1
C
C           compute pointer for current interval
            IF (UVQOPX(UVQ) .EQ. 0) THEN
C             workspace - look at current interval
              PTR= MOD (SPIVL- 1,UVQLEN(UVQ))+ UVQPOS(UVQ)
            ELSE
C             current operation - look at previous interval
              PTR= MOD (SPIVL- 2+ UVQLEN(UVQ),UVQLEN(UVQ))+
     $             UVQPOS(UVQ)
            END IF
          END IF
C
C         reset value(s) in pipe
          DO 5 I= 1, N
            IF (UVQTYP(UVQ) .EQ. 2) THEN
C             integer
              IVQPIP(PTR)= INEWVL
            ELSE IF (UVQTYP(UVQ) .EQ. 3) THEN
C             real
              IF (UVQADD(UVQ) .EQ. ADDR) THEN
C               update first value
                UVQPIP(PTR)= RNEWVL
              ELSE IF (UVQADD(UVQ) .EQ. ADDR2) THEN
C               update second value
                UVQPIP(PTR)= RNEWV2
              ELSE IF (UVQADD(UVQ) .EQ. ACCADD) THEN
C               update accumulator
                UVQPIP(PTR)= RVAL
              END IF
            ELSE IF (UVQTYP(UVQ) .EQ. 4) THEN
C             double precision
              IF (UVQADD(UVQ) .EQ. ADDR) THEN
C               update first value
                UVQPIP(PTR)= SNGL (DNEWVL)
              ELSE IF (UVQADD(UVQ) .EQ. ACCADD) THEN
C               update accumulator
                UVQPIP(PTR)= RVAL
              END IF
            END IF
            PTR= PTR+ 1
 5        CONTINUE
        END IF
 10   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   SPSORT
     I                   (MESSU,SORTFG,SPAFP,SPAKND,SPOUT)
C
C     + + + PURPOSE + + +
C     Sort Special Actions after an undated, deferred, or distributed
C     Action has been re-dated.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER MESSU,SORTFG,SPAFP,SPAKND,SPOUT
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSU  - ftn unit no. to be used for printout of messages
C     SORTFG - flag indicating how sort should take place
C     SPAFP  - current special action instruction record number
C     SPAKND - last special act. instr. record number for this operation
C     SPOUT  - runtime Special Action output level
C
C     + + + COMMON BLOCKS + + +
C     special action file in memory
      INCLUDE     'pspins.inc'
      INCLUDE     'cspins.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER  I,J,LSPAFP,LAST,FLAG,LSPPTR
C
C     + + + EXTERNALS + + +
      EXTERNAL SPCKDT,COPYI
C
C     + + + END SPECIFICATIONS + + +
C
      IF (SORTFG .EQ. 2) THEN
C       undated - try skipping past all undated before checking dates
C       go to end and work back until an undated is found
        LSPAFP= SPAKND+ 1
 10     CONTINUE
          LSPAFP= LSPAFP- 1
        IF ( (SPINS(20,SPPTR(LSPAFP)) .LE. 0) .AND.
     $       (LSPAFP .GT. (SPAFP+ 1)) ) GO TO 10
      ELSE
C       dated - assume likely to stay near the top
        LSPAFP= SPAFP+ 1
      END IF
C
C     now find how far down this action goes in the list
Cthj      LAST= SPAKND
      LAST= SPAFP
 20   CONTINUE
        CALL SPCKDT (SPINS(1,SPPTR(SPAFP)),SPINS(1,SPPTR(LSPAFP)),
     O               FLAG)
        IF (SPOUT .GE. 9) THEN
C         echo swap flag
          WRITE (MESSU,*) ' swap flag',FLAG
        END IF
        IF ( (FLAG .EQ. 0) .AND.
     $    (SPINS(22,SPPTR(SPAFP)) .GT. SPINS(22,SPPTR(LSPAFP))) ) THEN
          FLAG= 1
          IF (SPOUT .GE. 9) THEN
C           echo swap flag
            WRITE (MESSU,*) ' corrected swap flag',FLAG
          END IF
        END IF
        IF (FLAG .GT. 0) THEN
C         date after next action or date is same but later in uci
C         than next action
          LAST= LSPAFP
          IF (SPOUT .GE. 9) THEN
C           echo swap pointers
            WRITE (MESSU,*) '  swap instr at',SPAFP,LSPAFP
          END IF
C
C         update sort pointer
          LSPAFP= LSPAFP+ 1
        END IF
      IF ( (FLAG .GT. 0) .AND. (LSPAFP .LE. SPAKND) ) GO TO 20
C
      IF (LAST .GT. SPAFP) THEN
C       finally, move the actions
        LSPPTR= SPPTR(SPAFP)
        DO 30 I= SPAFP+ 1, LAST
          SPPTR(I-1)= SPPTR(I)
 30     CONTINUE
        SPPTR(LAST)= LSPPTR
      END IF
C
      IF (SPOUT .GE. 9) THEN
C       echo new instruction order
        WRITE (MESSU,*) 'after sorting'
        DO 40 I= SPAFP, SPAKND
          WRITE (MESSU,3000) I,(SPINS(J,SPPTR(I)),J=1,LENSPI)
 3000     FORMAT ('instr',I6,':',I5,4I3,I3,I10,I3,G10.4,A4,A2,3I3,2I3,
     $            G10.4,3I5,I10,4I5)
 40     CONTINUE
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   SPNEWV
     I                    (MESSU,SPOUT,ACTCOD,TYPCOD,ICURVL,RCURVL,
     I                     RCURV2,DCURVL,IVAL,RVAL,CACTVA,CACTV2,
     O                     INEWVL,RNEWVL,RNEWV2,DNEWVL)
C
C     + + + PURPOSE + + +
C     Compute new value for Special Action target and echo
C     to message file if needed.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER          MESSU,SPOUT,ACTCOD,TYPCOD,ICURVL,IVAL,INEWVL
      REAL             RCURVL,RCURV2,RVAL,RNEWVL,RNEWV2
      DOUBLE PRECISION DCURVL,DNEWVL
      CHARACTER*20     CACTVA,CACTV2
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSU  - ftn unit no. to be used for printout of messages
C     SPOUT  - runtime Special Action output level
C     ACTCOD - action code
C     TYPCOD - type code: 2=integer, 3=real, 4=double precision
C     ICURVL - integer current value
C     RCURVL - real current value
C     RCURV2 - second real current value
C     DCURVL - double precision current value
C     IVAL   - integer action value
C     RVAL   - real action value
C     CACTVL - variable name and subscripts
C     CACTV2 - second variable name and subscripts
C     INEWVL - integer new value
C     RNEWVL - real new value
C     RNEWV2 - second real new value
C     DNEWVL - double precision new value
C
C     + + + LOCAL VARIABLES + + +
      INTEGER          RLEN,SDIG,DECP
      REAL             RTOT,UNDEF
      CHARACTER*3      CACTCD(13)
      CHARACTER*4      CMVTP(3)
      CHARACTER*13     CVAL1,CVAL2,CVAL3
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (CVAL1,CVAL11),(CVAL2,CVAL21),(CVAL3,CVAL31)
      CHARACTER*1  CVAL11(13),CVAL21(13),CVAL31(13)
C
C     + + + INTRINSICS + + +
      INTRINSIC    FLOAT,INT,MIN,MAX,DBLE,MOD,ABS,LOG,LOG10
C
C     + + + EXTERNALS + + +
      EXTERNAL     DECCHX
C
C     + + + DATA INITIALIZATIONS + + +
      DATA RLEN,SDIG,DECP/13,5,-5/
      DATA CMVTP/'MOVT','MOV1','MOV2'/
      DATA UNDEF/-1.0E+30/
      DATA CACTCD/' = ',' + ',' - ',' * ',' / ','MIN','MAX',
     $            'ABS','INT',' ^ ','LN ','LOG','MOD'/
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (3X,I10)
 2010 FORMAT (1PE13.5)
 2020 FORMAT (3X,A20,' ORIG:',A13,'          RESET TO = ',A13)
 2030 FORMAT (3X,A20,' ORIG:',A13,' ',A3,' ',A13,' = ',A13)
 2040 FORMAT (3X,A20,' ORIG:',A13,'    ',A4,'  RESET TO = ',A13)
 2050 FORMAT (3X,A20,'      ',A13,'     ',A13,'   ',A13)
C
C     + + + END SPECIFICATIONS + + +
C
      IF (SPOUT .GE. 2) THEN
C       write current value
        IF (TYPCOD .EQ. 2) THEN
C         integer type
          WRITE (CVAL1,2000) ICURVL
        ELSE IF (TYPCOD .EQ. 3) THEN
C         real
          CALL DECCHX (RCURVL,RLEN,SDIG,DECP,
     O                 CVAL11)
        ELSE IF (TYPCOD .EQ. 4) THEN
C         double prec
          WRITE (CVAL1,2010) DCURVL
        END IF
      END IF
C
      IF (ACTCOD .EQ. 1) THEN
C       reset
        IF (TYPCOD .EQ. 2) THEN
C         integer type
          INEWVL= IVAL
          IF (SPOUT .GE. 2) THEN
C           prepare echo buffer
            WRITE (CVAL3,2000) IVAL
          END IF
        ELSE
          IF (TYPCOD .EQ. 3) THEN
C           real type
            RNEWVL= RVAL
          ELSE
C           dp type
            DNEWVL= DBLE (RVAL)
          END IF
          IF (SPOUT .GE. 2) THEN
C           prepare echo buffer
            CALL DECCHX (RVAL,RLEN,SDIG,DECP,
     O                   CVAL31)
          END IF
        END IF
C       details about this action
        IF (SPOUT .GE. 2) THEN
C         echo action
          WRITE (MESSU,2020) CACTVA,CVAL1,CVAL3
        END IF
      ELSE IF (ACTCOD .LE. 13) THEN
C       arithmetic operator
        IF (TYPCOD .EQ. 2) THEN
C         integer type
C
C         save increment for output
          IF (SPOUT .GE. 2) THEN
C           prepare echo buffer
            WRITE (CVAL2,2000) IVAL
          END IF
          INEWVL= ICURVL
          IF (IVAL .NE. -999) THEN
C           value is defined
            IF (ACTCOD .EQ. 2) THEN
C             add
              INEWVL= ICURVL+ IVAL
            ELSE IF (ACTCOD .EQ. 3) THEN
C             subtract
              INEWVL= ICURVL- IVAL
            ELSE IF (ACTCOD .EQ. 4) THEN
C             multiply
              INEWVL= ICURVL*IVAL
            ELSE IF (ACTCOD .EQ. 5) THEN
C             divide
              INEWVL= ICURVL/IVAL
            ELSE IF (ACTCOD .EQ. 6) THEN
C             minimum
              INEWVL= MIN (ICURVL,IVAL)
            ELSE IF (ACTCOD .EQ. 7) THEN
C             maximum
              INEWVL= MAX (ICURVL,IVAL)
            ELSE IF (ACTCOD .EQ. 8) THEN
C             absolute value
              INEWVL= ABS (IVAL)
            ELSE IF (ACTCOD .EQ. 9) THEN
C             integer truncation
              INEWVL= IVAL
            ELSE IF (ACTCOD .EQ. 10) THEN
C             exponentiation
              INEWVL= INT (FLOAT (ICURVL)**FLOAT (IVAL))
            ELSE IF (ACTCOD .EQ. 11) THEN
C             natural logarithm
              IF (IVAL .GE. 1) THEN
C               valid log
                INEWVL= INT (LOG (FLOAT (IVAL)))
              ELSE
C               undefined
                INEWVL= -999
              END IF
            ELSE IF (ACTCOD .EQ. 12) THEN
C             logarithm base ten
              IF (IVAL .GE. 1) THEN
C               valid log
                INEWVL= INT (LOG10 (FLOAT (IVAL)))
              ELSE
C               undefined
                INEWVL= -999
              END IF
            ELSE IF (ACTCOD .EQ. 13) THEN
C             modulus
              INEWVL= MOD (ICURVL,IVAL)
            END IF
          END IF
          IF (SPOUT .GE. 2) THEN
C           prepare echo buffer
            WRITE (CVAL3,2000) INEWVL
          END IF
        ELSE
C         save increment for output
          IF (SPOUT .GE. 2) THEN
C           prepare echo buffer
            CALL DECCHX (RVAL,RLEN,SDIG,DECP,
     O                   CVAL21)
          END IF
          IF (TYPCOD .EQ. 3) THEN
C           real type
            RNEWVL= RCURVL
            IF (RVAL .GT. UNDEF) THEN
C             value is defined
              IF (ACTCOD .EQ. 2) THEN
C               add
                RNEWVL= RCURVL+ RVAL
              ELSE IF (ACTCOD .EQ. 3) THEN
C               subtract
                RNEWVL= RCURVL- RVAL
              ELSE IF (ACTCOD .EQ. 4) THEN
C               multiply
                RNEWVL= RCURVL*RVAL
              ELSE IF (ACTCOD .EQ. 5) THEN
C               divide
                RNEWVL= RCURVL/RVAL
              ELSE IF (ACTCOD .EQ. 6) THEN
C               minimum
                RNEWVL= MIN (RCURVL,RVAL)
              ELSE IF (ACTCOD .EQ. 7) THEN
C               maximum
                RNEWVL= MAX (RCURVL,RVAL)
              ELSE IF (ACTCOD .EQ. 8) THEN
C               absolute value
                RNEWVL= ABS (RVAL)
              ELSE IF (ACTCOD .EQ. 9) THEN
C               integer truncation
                RNEWVL= INT (RVAL)
              ELSE IF (ACTCOD .EQ. 10) THEN
C               exponentiation
                RNEWVL= RCURVL**RVAL
              ELSE IF (ACTCOD .EQ. 11) THEN
C               natural logarithm
                IF (RVAL .GT. 0.0) THEN
C                 valid log
                  RNEWVL= LOG (RVAL)
                ELSE
C                 undefined
                  RNEWVL= UNDEF
                END IF
              ELSE IF (ACTCOD .EQ. 12) THEN
C               logarithm base ten
                IF (RVAL .GT. 0.0) THEN
C                 valid log
                  RNEWVL= LOG10 (RVAL)
                ELSE
C                 undefined
                  RNEWVL= UNDEF
                END IF
              ELSE IF (ACTCOD .EQ. 13) THEN
C               modulus
                RNEWVL= MOD (RCURVL,RVAL)
              END IF
            END IF
            IF (SPOUT .GE. 2) THEN
C             prepare echo buffer
              CALL DECCHX (RNEWVL,RLEN,SDIG,DECP,
     O                     CVAL31)
            END IF
          ELSE
C           dp type
            DNEWVL= DCURVL
            IF (RVAL .GT. UNDEF) THEN
C             value is defined
              IF (ACTCOD .EQ. 2) THEN
C               add
                DNEWVL= DCURVL + RVAL
              ELSE IF (ACTCOD .EQ. 3) THEN
C               subtract
                DNEWVL= DCURVL - RVAL
              ELSE IF (ACTCOD .EQ. 4) THEN
C               multiply
                DNEWVL= DCURVL * RVAL
              ELSE IF (ACTCOD .EQ. 5) THEN
C               divide
                DNEWVL= DCURVL / RVAL
              ELSE IF (ACTCOD .EQ. 6) THEN
C               minimum
                DNEWVL= MIN (DCURVL,DBLE (RVAL))
              ELSE IF (ACTCOD .EQ. 7) THEN
C               maximum
                DNEWVL= MAX (DCURVL,DBLE (RVAL))
              ELSE IF (ACTCOD .EQ. 8) THEN
C               absolute value
                DNEWVL= DBLE (ABS (RVAL))
              ELSE IF (ACTCOD .EQ. 9) THEN
C               integer truncation
                DNEWVL= DBLE (INT (RVAL))
              ELSE IF (ACTCOD .EQ. 10) THEN
C               exponentiation
                DNEWVL= DCURVL**DBLE (RVAL)
              ELSE IF (ACTCOD .EQ. 11) THEN
C               natural logarithm
                IF (RVAL .GT. 0.0) THEN
C                 valid log
                  DNEWVL= DBLE (LOG (RVAL))
                ELSE
C                 undefined
                  DNEWVL= UNDEF
                END IF
              ELSE IF (ACTCOD .EQ. 12) THEN
C               logarithm base ten
                IF (RVAL .GT. 0.0) THEN
C                 valid log
                  DNEWVL= DBLE (LOG10 (RVAL))
                ELSE
C                 undefined
                  DNEWVL= UNDEF
                END IF
              ELSE IF (ACTCOD .EQ. 13) THEN
C               modulus
                DNEWVL= MOD (DCURVL,DBLE (RVAL))
              END IF
            END IF
            IF (SPOUT .GE. 2) THEN
C             prepare echo buffer
              WRITE (CVAL3,2010) DNEWVL
            END IF
          END IF
        END IF
C       details about this action
        IF (SPOUT .GE. 2) THEN
C         echo action
          WRITE (MESSU,2030) CACTVA,CVAL1,CACTCD(ACTCOD),CVAL2,
     $                       CVAL3
        END IF
      ELSE
C       mix two locations up
        RTOT= RCURVL+ RCURV2
        IF (ACTCOD .EQ. 14) THEN
C         act on total
          RNEWVL= RVAL* RTOT
        ELSE IF (ACTCOD .EQ. 15) THEN
C         act on amount in first loc
          RNEWVL= RVAL* RCURVL
        ELSE IF (ACTCOD .EQ. 16) THEN
C         act on amount in second loc
          RNEWVL= RVAL* RCURV2
        END IF
        RNEWV2= RTOT- RNEWVL
C
        IF (SPOUT .GE. 2) THEN
C         write details for first loc
          CALL DECCHX (RCURVL,RLEN,SDIG,DECP,
     O                 CVAL11)
          CALL DECCHX (RNEWVL,RLEN,SDIG,DECP,
     O                 CVAL21)
          WRITE (MESSU,2040) CACTVA,CVAL1,CMVTP(ACTCOD-13),CVAL2
C         write details for second loc
          CALL DECCHX (RCURV2,RLEN,SDIG,DECP,
     O                 CVAL11)
          CALL DECCHX (RVAL,RLEN,SDIG,DECP,
     O                 CVAL21)
          CALL DECCHX (RNEWV2,RLEN,SDIG,DECP,
     O                 CVAL31)
          WRITE (MESSU,2050) CACTV2,CVAL1,CVAL2,CVAL3
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   SPACT
     I                   (MESSU,SPIVL,SPOPNO,SPOUT,
     M                    LSPPTR,SPAFP,
     O                    SORTFG)
C
C     + + + PURPOSE + + +
C     Perform a Special Action.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MESSU,SPIVL,SPOPNO,SPOUT,LSPPTR,SPAFP,SORTFG
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSU  - ftn unit no. to be used for printout of messages
C     SPIVL  - interval of run
C     SPOPNO - index of target operation
C     SPOUT  - runtime Special Action output level
C     LSPPTR - pointer to current Special Action
C     SPAFP  - current Special Action instruction record number
C     SORTFG - flag indicating whether resort is needed
C
C     + + + COMMON BLOCKS + + +
      INCLUDE     'cmdum.inc'
      INCLUDE     'cmpad.inc'
C     special action file in memory
      INCLUDE     'pspins.inc'
      INCLUDE     'cspins.inc'
C     special action distributions
      INCLUDE     'pspdst.inc'
      INCLUDE     'cspdst.inc'
C
C    + + + LOCAL VARIABLES + + +
      INTEGER          LSPAC(LENSPI),ADDR2,I,UVTYP,EXDAT(5),ICURVL,
     $                 CONDCK,INEWVL,DADD,I0,I1,I2
      REAL             R,CFRAC,RCURVL,RCURV2,RNEWVL,RNEWV2
      DOUBLE PRECISION DCURVL,DNEWVL
      CHARACTER*20     CACTVA,CACTV2
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (LSPAC(6),TYPCOD),(LSPAC(7),ADDR),(LSPAC(8),ACTCOD),
     $            (LSPAC(9),RVAL),(LSPAC(9),IVAL),(LSPAC(14),TARUVQ),
     $            (LSPAC(15),DSIND),(LSPAC(16),DSPOS),
     $            (LSPAC(17),DEFFRC),(LSPAC(18),LOGBLK),
     $            (LSPAC(19),ACTUVQ),(LSPAC(20),ALLDEL),
     $            (LSPAC(21),ACCADD),(LSPAC(23),HDRPTR),
     $            (LSPAC(24),HDRNUM),(LSPAC(25),FTRPTR),
     $            (LSPAC(26),FTRNUM)
      INTEGER      TYPCOD,ADDR,ACTCOD,IVAL,TARUVQ,DSIND,DSPOS,LOGBLK,
     $             ACTUVQ,ALLDEL,ACCADD,HDRPTR,HDRNUM,FTRPTR,FTRNUM
      REAL         RVAL,DEFFRC
C
C     + + + FUNCTIONS + + +
      INTEGER      DADDR
C
C     + + + INTRINSICS + + +
      INTRINSIC    FLOAT,INT,DBLE
C
C     + + + EXTERNALS + + +
      EXTERNAL     SPCKDT,GTQUAN,SPDIST,EXDATE,DADDR,SPONAM,
     $             SPBKCK,SPNEWV,SPVQUP,SPNXDT
C
C     + + + DATA INITIALIZATIONS + + +
      DATA I0,I1,I2/0,1,2/
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT ('    SPECIAL ACTION SKIPPED - CONDITION FALSE ')
 2010 FORMAT ('    DEFERRING UNTIL',I5,'/',I2,'/',I2,1X,I2,':',I2)
 2020 FORMAT ('WARNING: ON',I5,4('/',I2),' USING DISTRIBUTION',I3,
     $        ' A FRACTION OF',F8.4,' WAS LOST DUE TO FAILURE OF ',
     $        'LOGICAL CONDITIONS ON THE LAST INTERVAL')
 2030 FORMAT ('    DEFERRING A FRACTION OF',F8.4,' FROM DISTRIBUTION',
     $         I4,' UNTIL',I5,'/',I2,'/',I2,1X,I2,':',I2)
 2040 FORMAT (A80)
 2050 FORMAT (A20)
C
C     + + + END SPECIFICATIONS + + +
C
C     make a local copy of current action to use equiv
      DO 10 I= 1, LENSPI
        LSPAC(I)= SPINS(I,LSPPTR)
 10   CONTINUE
      ADDR2= 0
C
      IF (ACTUVQ .GT. 0) THEN
C       action value is a variable, so fetch current value
        IF (SPOUT .GE. 9) THEN
C         echo actuvq
          WRITE (MESSU,*) 'fetching variable action value',ACTUVQ
        END IF
        CALL GTQUAN (ACTUVQ,SPIVL,SPOPNO,
     O               I,R,UVTYP)
        IF (UVTYP .EQ. 2) THEN
C         quantity is integer
          IF (SPOUT .GE. 9) THEN
C           echo integer value
            WRITE (MESSU,*) 'value is',I
          END IF
          IF (TYPCOD .EQ. 2) THEN
C           use integer value
            IVAL= I
          ELSE
C           convert integer value to real
            RVAL= FLOAT (I)
          END IF
        ELSE
C         quantity is real
          IF (SPOUT .GE. 9) THEN
C           echo real value
            WRITE (MESSU,*) 'value is',R
          END IF
          IF (TYPCOD .EQ. 2) THEN
C           convert real value to integer
            IVAL= INT (R)
          ELSE
C           use real value
            RVAL= R
          END IF
        END IF
      END IF
C
      IF (DSIND .NE. 0) THEN
C       a distribution is in use
        CALL SPDIST (MESSU,SPOUT,DSIND,DSPOS,
     M               RVAL,DEFFRC,
     O               CFRAC)
C       update instruction with new deferred fraction
        SPINSR(17,LSPPTR)= DEFFRC
      END IF
C
      IF (ADDR .LT. 0) THEN
C       get current value from pipes
        CALL GTQUAN (TARUVQ,SPIVL,SPOPNO,
     O               ICURVL,RCURVL,UVTYP)
        IF (TYPCOD .EQ. 4) THEN
C         convert real to double
          DCURVL= DBLE (RCURVL)
        END IF
      ELSE
C       current value from pad
        ICURVL= IPAD(ADDR)
        IF (TYPCOD .EQ. 3) THEN
C         real
          IF (ICURVL .EQ. -999) THEN
C           current value is undefined - set to zero
            RCURVL= 0.0
          ELSE
C           current value is good
            RCURVL= PAD(ADDR)
          END IF
        ELSE IF (TYPCOD .EQ. 4) THEN
C         double prec
          IF (ICURVL .EQ. -999) THEN
C           current value is undefined - set to zero
            DCURVL= 0.0
          ELSE
C           current value is good
            DCURVL= DPPAD(DADDR (ADDR))
          END IF
        END IF
      END IF
C
      IF (SPOUT .GE. 2) THEN
C       describe variable being acted on
        CALL SPONAM (LSPAC(10),LSPAC(12),LSPAC(7),LSPAC(6),
     O               CACTVA)
      END IF
C
      IF (LOGBLK .EQ. 0) THEN
C       no conditions to check
        CONDCK= 1
      ELSE
C       check condition
        CALL SPBKCK (LOGBLK,SPIVL,SPOPNO,SPOUT,MESSU,
     O               CONDCK)
      END IF
C
      SORTFG= 0
C
      IF ( (SPOUT .GE. 4) .AND. (HDRNUM .GE. 1) ) THEN
C       echo headers
        DO 20 I= HDRPTR, HDRPTR+ HDRNUM- 1
          WRITE (MESSU,2040) SPHDR(I)
 20     CONTINUE
      END IF
C
      IF (CONDCK .GE. 1) THEN
C       take the special action
C
        IF (ACTCOD .GE. 14) THEN
C         mix two locations up, need next action
          SPAFP= SPAFP+ 1
          LSPPTR= SPPTR(SPAFP)
          ADDR2= SPINS(7,LSPPTR)
          IF (ADDR2 .LT. 0) THEN
C           get current value from pipes
            CALL GTQUAN (SPINS(14,LSPPTR),SPIVL,SPOPNO,
     O                   I,RCURV2,UVTYP)
          ELSE
C           get current value from pad
            RCURV2= PAD(ADDR2)
          END IF
          CALL SPONAM (SPINS(10,LSPPTR),SPINS(12,LSPPTR),
     I                 SPINS(7,LSPPTR),SPINS(6,LSPPTR),
     O                 CACTV2)
        END IF
C
C       compute new value
        CALL SPNEWV (MESSU,SPOUT,ACTCOD,TYPCOD,ICURVL,RCURVL,
     I               RCURV2,DCURVL,IVAL,RVAL,CACTVA,CACTV2,
     O               INEWVL,RNEWVL,RNEWV2,DNEWVL)
C
C       update values in osv and uvquans as necessary
C
        IF (ADDR .GT. 0) THEN
C         update in osv
          IF (TYPCOD .EQ. 2) THEN
C           integer
            IPAD(ADDR)= INEWVL
          ELSE IF (TYPCOD .EQ. 3) THEN
C           real
            PAD(ADDR)= RNEWVL
            IF (ACCADD .GT. 0) THEN
C             increment accumulator
              PAD(ACCADD)= PAD(ACCADD)+ RVAL
            END IF
          ELSE IF (TYPCOD .EQ. 4) THEN
C           double precision
            DADD= DADDR (ADDR)
            DPPAD(DADD)= DNEWVL
            IF (ACCADD .GT. 0) THEN
C             increment accumulator
              DADD= DADDR (ACCADD)
              DPPAD(DADD)= DPPAD(DADD)+ DBLE (RVAL)
            END IF
          END IF
        END IF
        IF (ADDR2 .GT. 0) THEN
C         second value in osv
          PAD(ADDR2)= RNEWV2
        END IF
C
C       check all osv and workspace uvquans to see if their base
C       variable was changed
        CALL SPVQUP (SPIVL,SPOPNO,ADDR,ADDR2,ACCADD,INEWVL,RVAL,
     I               RNEWVL,RNEWV2,DNEWVL,I0)
C
      ELSE IF (CONDCK .EQ. 0) THEN
C       condition not satisfied - action skipped
        IF (SPOUT .GE. 5) THEN
C         echo message for skipped action
          WRITE (MESSU,2000)
        END IF
        IF (ACTCOD .GE. 14) THEN
C         must also skip second instruction
          SPAFP= SPAFP+ 1
        ELSE IF (DSIND .GT. 0) THEN
C         action is distributed
          IF (SPDDFG(DSIND) .GE. 2) THEN
C           defer current fraction in distribution
            DEFFRC= DEFFRC+ CFRAC
            SPINSR(17,LSPPTR)= DEFFRC
          END IF
        ELSE IF (ALLDEL .LT. 0) THEN
C         action is deferred on failure
          I= -ALLDEL
          CALL SPNXDT (I2,I,I1,
     M                 SPINS(1,LSPPTR))
          IF (SPOUT .GE. 2) THEN
C           echo new date
            WRITE (MESSU,2010) (SPINS(I,LSPPTR),I=1,5)
          END IF
          SORTFG= 1
        END IF
      ELSE
C       error in spbkck
        WRITE (*,*) 'PROGRAM BUG - ERROR IN SPBKCK'
      END IF
C
      IF (DSIND .GT. 0) THEN
C       check if done with distribution
        IF (DSPOS .EQ. SPDCNT(DSIND)) THEN
C         all done with current distribution
          IF (SPOUT .GE. 9) THEN
C           echo distrib done
            WRITE (MESSU,*) 'done distrib:',DSIND,DSPOS,
     $                       SPDCNT(DSIND),DEFFRC
          END IF
C
          IF (DEFFRC .GT. 0.0) THEN
C           notify user of lost fraction
            WRITE (MESSU,2020) (LSPAC(I),I=1,5),DSIND,DEFFRC
          END IF
        ELSE
C         more to do fraction in this distribution
          SPINS(16,LSPPTR)= DSPOS+ 1
C         increment date to next one, internal format
          CALL SPNXDT (SPDTCD(DSIND),SPDTST(DSIND),I1,
     M                 SPINS(1,LSPPTR))
          IF ( (CONDCK .LE. 0) .AND. (SPDDFG(DSIND) .GE. 2) .AND.
     $         (SPOUT .GE. 2) ) THEN
C           write deferral message
            CALL EXDATE (SPINS(1,LSPPTR),
     O                   EXDAT)
            WRITE (MESSU,2030) DEFFRC,DSIND,EXDAT
          END IF
          IF (SPOUT .GE. 9) THEN
C           echo next distrib
            WRITE (MESSU,*) 'next distrib:',DSIND,
     $                       SPINS(16,LSPPTR),
     $                      (SPINS(I,LSPPTR),I=1,5)
          END IF
          SORTFG= 1
        END IF
      END IF
C
      IF ( (SPOUT .GE. 1) .AND. (FTRNUM .GE. 1) ) THEN
C       echo footers
        DO 30 I= FTRPTR, FTRPTR+ FTRNUM- 1
          WRITE (MESSU,2050) SPFTR(I)
 30     CONTINUE
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   SPECL
     I                   (COPTYP,PUNO,SPAKND,SPOPNO,DATIM,MESSU,SPIVL,
     I                    SPOUT,SPNUND,
     M                    SPAFP)
C
C     + + + PURPOSE + + +
C     Perform all Special Actions for current operation for current interval.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     PUNO,SPAKND,SPOPNO,DATIM(5),MESSU,SPIVL,SPOUT,SPNUND,
     $            SPAFP
      CHARACTER*6 COPTYP
C
C     + + + ARGUMENT DEFINITIONS + + +
C     COPTYP - operation type as character string
C     PUNO   - operation number
C     SPAKND - last special act. instr. record number for this operation
C     SPOPNO - index of target operation
C     DATIM  - current date and time of day
C     MESSU  - ftn unit no. to be used for printout of messages
C     SPIVL  - interval of run
C     SPOUT  - runtime Special Action output level
C     SPNUND - number of undated special actions
C     SPAFP  - current special action instruction record number
C
C     + + + COMMON BLOCKS + + +
C     special action file in memory
      INCLUDE     'pspins.inc'
      INCLUDE     'cspins.inc'
C
C    + + + LOCAL VARIABLES + + +
      INTEGER          ACTCNT,NUNREM,DLAST,DPTR,DPOS,UPTR,UPOS,DOITFG,
     $                 DATFG,LSPPTR,LSPAFP,EXDAT(5),SORTFG
C
C     + + + EXTERNALS + + +
      EXTERNAL     SPWSUP,SPCKDT,EXDATE,SPACT,SPSORT
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' SPEC-ACT: ',A6,2X,I3,' AT ',I4,2('/',I2),I3,':',I2)
C
C     + + + END SPECIFICATIONS + + +
C
C      write (99,*) "spivl coptyp puno spopno spwsiv",SPIVL,COPTYP,
C     $              PUNO,SPOPNO,SPWSIV
      IF (SPIVL .GT. SPWSIV) THEN
C       update workspace uvquans if not already done this interval
        CALL SPWSUP (SPIVL)
        SPWSIV = SPIVL
C       write (99,*) "updated workspace at interval",SPWSIV
      END IF
C
C     initialize action counter
      ACTCNT= 0
C
C     initialize number of undated actions remaining
      NUNREM= SPNUND
C
C     calculate pointer to last dated action
      DLAST= SPAKND- NUNREM
C
C     loop back here to check next
 10   CONTINUE
C
        IF (SPAFP .LE. DLAST) THEN
C         check to see if next dated action is due this interval
          DPTR= SPPTR(SPAFP)
          CALL SPCKDT (DATIM,SPINS(1,DPTR),
     O                 DOITFG)
C         >>>>> change gt to ge in next line and in goto 10 to do
C         >>>>> special action on the time interval where it occurs
          IF (DOITFG .GT. 0) THEN
C           due this interval
            DPOS= SPINS(22,DPTR)
          ELSE
C           no dated actions this interval
            DPOS= 0
          END IF
        ELSE
C         no dated actions remaining in run
          DPOS= 0
        END IF
C
        IF (NUNREM .GT. 0) THEN
C         there are undated actions remaining this interval
          UPTR= SPPTR(SPAKND-NUNREM+1)
          UPOS= SPINS(22,UPTR)
        ELSE
C         no remaining undated actions this interval
          UPOS= 0
        END IF
C
        IF (DPOS .EQ. 0) THEN
C         no dated actions remaining this interval
          IF (UPOS .EQ. 0) THEN
C           done for this interval
            DATFG= -1
          ELSE
C           only undated remaining
            DATFG= 0
          END IF
        ELSE
C         dated actions remain
          IF (UPOS .EQ. 0) THEN
C           only dated actions left
            DATFG= 1
          ELSE
C           need to check
            IF (DPOS .LT. UPOS) THEN
C             do dated next
              DATFG= 1
            ELSE
C             do undated next
              DATFG= 0
            END IF
          END IF
        END IF
C
        IF (DATFG .EQ. -1) THEN
C         no action to take
          LSPPTR= 0
        ELSE IF (DATFG .EQ. 0) THEN
C         take undated action
          LSPPTR= UPTR
          LSPAFP= SPPTR(LSPPTR)
        ELSE IF (DATFG .EQ. 1) THEN
C         take dated action
          LSPPTR= DPTR
          LSPAFP= SPAFP
        END IF
C
        IF (LSPPTR .GT. 0) THEN
C         do the special action
          ACTCNT= ACTCNT+ 1
C
          IF ( (SPOUT .GE. 1) .AND. (ACTCNT .EQ. 1) ) THEN
C           operation info, date and type of variable being acted on
C
C           convert time to external format
            CALL EXDATE (DATIM,
     O                   EXDAT)
            WRITE (MESSU,2000) COPTYP,PUNO,EXDAT
          END IF
C
C         perform action
          CALL SPACT (MESSU,SPIVL,SPOPNO,SPOUT,
     M                LSPPTR,LSPAFP,
     O                SORTFG)
C
          IF (DATFG .EQ. 1) THEN
C           took dated action
            IF (SORTFG .EQ. 0) THEN
C             set pointer to get next special action
              SPAFP= LSPAFP+ 1
            ELSE
C             date was modified - check for change in order
              SPAFP= LSPAFP
              IF (SPAFP .LT. DLAST) THEN
C               there are later dated special actions to check
                CALL SPSORT (MESSU,SORTFG,SPAFP,DLAST,SPOUT)
              END IF
            END IF
          ELSE
C           took undated action
            NUNREM= NUNREM- 1
          END IF
C
        END IF
C       check to see if need next action also
      IF ( ( (SPAFP .LE. DLAST) .AND. (DOITFG .GT. 0) ) .OR.
     $     (NUNREM .GT. 0) ) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE   SPONAM
     I                    (VNAM,VSUB,ADDR,TYPCOD,
     O                     CVNAME)
C
C     + + + PURPOSE + + +
C     build special action variable name string
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      VNAM(2),VSUB(3),ADDR,TYPCOD
      CHARACTER*20 CVNAME
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      BLANK
      CHARACTER*4  CVTYP(4),CBLK
C
C     + + + DATA INITIALZATIONS + + +
      DATA CBLK/'    '/
      DATA CVTYP/'unkn','INTG','REAL','DBLE'/
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (A4)
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (A4,' LOC:',I8)
 2010 FORMAT (A4,A2,3I4)
C
C     + + + END SPECIFICATIONS + + +
C
      READ (CBLK,1000) BLANK
      IF (VNAM(1) .EQ. BLANK) THEN
C       dont know variable name
        WRITE (CVNAME,2000) CVTYP(TYPCOD),ADDR
      ELSE IF (VSUB(1) .EQ. 0) THEN
C       name of variable known and no subscripts
        WRITE (CVNAME,2010) VNAM
      ELSE IF (VSUB(2) .EQ. 0 .AND. VSUB(1) .NE. 0) THEN
C       one subscript
        WRITE (CVNAME,2010) VNAM,VSUB(1)
      ELSE IF (VSUB(3) .EQ. 0 .AND. VSUB(2) .NE. 0) THEN
C       two subscripts
        WRITE (CVNAME,2010) VNAM,VSUB(1),VSUB(2)
      ELSE
C       three subscripts
        WRITE (CVNAME,2010) VNAM,VSUB
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   SPBKCK
     I                    (LOGBLK,SPIVL,SPOPNO,SPOUT,MESSU,
     O                     CONDCK)
C
C     + + + PURPOSE + + +
C     Determine whether a conditional special action should be taken.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER LOGBLK,SPIVL,SPOPNO,SPOUT,MESSU,CONDCK
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LOGBLK - index of condition to check
C     SPIVL  - interval of run
C     SPOPNO - index of target operation
C     SPOUT  - runtime Special Action output level
C     MESSU  - ftn unit no. to be used for printout of messages
C     CONDCK - flag indicating result of check: 0=fail, 1=pass
C
C     + + + PARAMETERS + + +
      INTEGER    MXSTAK
      PARAMETER (MXSTAK=100)
C
C     + + + COMMON BLOCKS + + +
C     special action conditions
      INCLUDE     'pspcnd.inc'
      INCLUDE     'cspcnd.inc'
C     user defined variable quantity definitions
      INCLUDE     'pspvqd.inc'
      INCLUDE     'cspvqd.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,IQUAN(2),TYPFG,IND,STACK(MXSTAK),POINTR,OPAND1,
     $          OPAND2,OPERAT,POPS,ERRFLG,CURCHN,CURREF,LSTREF,PTR,
     $          CHVAL,EPOS,KEY,I0,UVQ,RLEN,SDIG,DECP
      REAL      R,QUAN(2)
      CHARACTER*2   CMPCOD(6)
      CHARACTER*5   CTRUTH(2)
      CHARACTER*132 ECHBUF
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (ECHBUF,ECHBU1)
      CHARACTER*1  ECHBU1(132)
C
C     + + + FUNCTIONS + + +
      INTEGER   LENSTR
C
C     + + + INTRINSICS + + +
      INTRINSIC FLOAT,MIN,MAX,ABS,MOD
C
C     + + + EXTERNALS + + +
      EXTERNAL  GTQUAN,ZIPI,GETUCI,LENSTR,DECCHX
C
C     + + + DATA INITIALIZATIONS + + +
      DATA I0/0/
      DATA RLEN,SDIG,DECP/13,5,-5/
      DATA CMPCOD/'= ','!=','> ','>=','< ','<='/
      DATA CTRUTH/'FALSE','TRUE '/
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (I10)
 2005 FORMAT (132A1)
 2010 FORMAT (A6,':')
 2020 FORMAT (A2)
 2030 FORMAT (A55,A5)
 2040 FORMAT (10X,'CONDITION IS ',A5)
C
C     + + + END SPECIFICATIONS + + +
C
      CONDCK= 1
      ERRFLG= 0
C
      IF (LOGBLK .GT. 0) THEN
C       check condition
C
C       initialize chain pointers
        CURREF= BLKPOS(LOGBLK)
        LSTREF= CURREF+ BLKCNT(LOGBLK)- 1
C
C       begin do-until loop on chain references
 10     CONTINUE
C
C         get current value of chain
          CURCHN= ABS(BLKCHN(CURREF))
          CALL GTQUAN (CHNUVQ(CURCHN),SPIVL,SPOPNO,
     O                 CHVAL,R,I)
          IF (CHVAL .EQ. -1) THEN
C           must compute value this interval
C
C           initialize stack and link pointers
            IND= CHNCND(CURCHN)
            POINTR= 0
            CALL ZIPI (MXSTAK,I0,
     O                 STACK)
            IF (SPOUT .GE. 1) THEN
C             echo chain
              KEY= -CHNKEY(CURCHN)
 12           CONTINUE
                CALL GETUCI (I0,
     M                       KEY,
     O                       ECHBUF(1:80))
                EPOS= 80
                EPOS= MAX (4,LENSTR (EPOS,ECHBUF))
                WRITE (ECHBUF(EPOS+1:EPOS+10),2000) CHNKEY(CURCHN)
                WRITE (MESSU,2005) (ECHBU1(I), I= 1, EPOS+ 10)
                IF (ECHBUF(EPOS- 3:EPOS) .EQ. 'THEN') THEN
C                 last line of chain
                  KEY= 0
                END IF
              IF (KEY .NE. 0) GO TO 12
            END IF            
C
C           do-until loop for each link
 20         CONTINUE
C
C             evaluate comparison and push result
C
              POINTR= POINTR+ 1
              IF (SPOUT .GE. 3) THEN
C               initialize echo buffer
                EPOS= 11
                ECHBUF= ' '
              END IF
              IF (POINTR .LE. MXSTAK) THEN
C               get quantities - all converted to real
                DO 30 I= 1, 2
                  UVQ= CNDUVQ(I,IND)
                  IF (UVQ .GT. 0) THEN
C                   get variable quantity
                    IF (SPOUT .GE. 3) THEN
C                     echo uvquan names
                      WRITE (ECHBUF(EPOS:EPOS+6),2010) UVQNAM(UVQ)
                    END IF
                    CALL GTQUAN (UVQ,SPIVL,SPOPNO,
     O                           IQUAN(I),QUAN(I),TYPFG)
                    IF (TYPFG .EQ. 2) THEN
C                     convert integer to real
                      QUAN(I)= FLOAT (IQUAN(I))
                    END IF
                  ELSE
C                   get constant quantity
                    QUAN(I)= CNDVAL(I,IND)
                  END IF
                  IF (SPOUT .GE. 3) THEN
C                   echo quantity
                    EPOS= EPOS+ 7
                    CALL DECCHX (QUAN(I),RLEN,SDIG,DECP,
     O                           ECHBU1(EPOS))
                    EPOS= EPOS+ RLEN+ 4
                  END IF
 30             CONTINUE
                IF (SPOUT .GE. 3) THEN
C                 echo comparison operator
                  WRITE (ECHBUF(32:33),2020) CMPCOD(CNDCOD(IND))
                END IF
C
C               compare quantities
                IF ( (CNDCOD(IND) .EQ. 1) .AND.
     $               (QUAN(1) .EQ. QUAN(2)) ) THEN
C                 equal
                  STACK(POINTR)= 1
                ELSE IF ( (CNDCOD(IND) .EQ. 2) .AND.
     $                    (QUAN(1) .NE. QUAN(2)) ) THEN
C                 not equal
                  STACK(POINTR)= 1
                ELSE IF ( (CNDCOD(IND) .EQ. 3) .AND.
     $                    (QUAN(1) .GT. QUAN(2)) ) THEN
C                 greater
                  STACK(POINTR)= 1
                ELSE IF ( (CNDCOD(IND) .EQ. 4) .AND.
     $                    (QUAN(1) .GE. QUAN(2)) ) THEN
C                 greater or equal
                  STACK(POINTR)= 1
                ELSE IF ( (CNDCOD(IND) .EQ. 5) .AND.
     $                    (QUAN(1) .LT. QUAN(2)) ) THEN
C                 less
                  STACK(POINTR)= 1
                ELSE IF ( (CNDCOD(IND) .EQ. 6) .AND.
     $                    (QUAN(1) .LE. QUAN(2)) ) THEN
C                 less or equal
                  STACK(POINTR)= 1
                END IF
                IF (SPOUT .GE. 3) THEN
C                 echo truth value
                  WRITE (MESSU,2030) ECHBUF(1:56),
     $                               CTRUTH(STACK(POINTR)+1)
                END IF
              ELSE
C               stack overflow
                CHVAL= -1
                ERRFLG= 1
              END IF
C
              IF (ERRFLG .GE. 0) THEN
C               handle stack
C
                IF (CNDLNK(IND) .LE. 0) THEN
C                 end of chain - clear stack
                  POPS= (POINTR- 1) / 2
                ELSE
C                 user has specified proper number of pops
                  POPS= CNDPOP(IND)
                END IF
C
                DO 40 I= 1, POPS
                  IF (ERRFLG .EQ. 1) THEN
C                   skip rest of stack pops
                  ELSE IF (POINTR .LT. 3) THEN
C                   stack underflow
                    CHVAL= -2
                    ERRFLG= 1
                  ELSE
C                   pop and evaluate an operator and its operands, and
C                   push the result
                    OPAND1= STACK(POINTR)
                    OPERAT= STACK(POINTR-1)
                    OPAND2= STACK(POINTR-2)
                    STACK(POINTR)= 0
                    STACK(POINTR-1)= 0
                    POINTR= POINTR- 2
                    IF (OPERAT .EQ. 1) THEN
C                     and
                      STACK(POINTR)= OPAND1*OPAND2
                    ELSE IF (OPERAT .EQ. 2) THEN
C                     or
                      STACK(POINTR)= MIN ((OPAND1+ OPAND2), 1)
                    END IF
                  END IF
 40             CONTINUE
C
                IF ( (ERRFLG .EQ. 0) .AND. (CNDLNK(IND) .GT. 0) ) THEN
C                 chain not complete
                  POINTR= POINTR+ 1
                  IF (POINTR .LE. MXSTAK) THEN
C                   push next operator
                    STACK(POINTR)= CNDLOG(IND)
                  ELSE
C                   stack overflow
                    CHVAL= -1
                    ERRFLG= 1
                  END IF
                END IF
C
C               update index to point to next link, if necessary
                IND= CNDLNK(IND)
C
              END IF
C
C           end do-until on links
            IF ( (ERRFLG .EQ. 0) .AND. (IND .GT. 0) ) GO TO 20
C
            IF (ERRFLG .EQ. 0) THEN
C             no error so far
              IF (POINTR .NE. 1) THEN
C               error - program bug - stack not complete
                CHVAL= -3
                ERRFLG= 1
              ELSE
C               get result from stack
                CHVAL= STACK(1)
              END IF
            END IF
C
            IF ( (SPOUT .GE. 1) .AND. (CHVAL .GE. 0) ) THEN
C             echo final truth value
             WRITE (MESSU,2040) CTRUTH(CHVAL+1)
            END IF
C
C           store value in internal uvquan
            PTR= MOD (SPIVL- 1,UVQLEN(CHNUVQ(CURCHN)))+
     $           UVQPOS(CHNUVQ(CURCHN))
            IVQPIP(PTR)= CHVAL
          END IF
C
          IF ( (CONDCK .EQ. 1) .AND. (ERRFLG .EQ. 0) ) THEN
C           check chain result against desired result
            IF ( (BLKCHN(CURREF) .GT. 0) .AND. (CHVAL .EQ. 1) ) THEN
C             need true - got true
              CONDCK= 1
            ELSE IF ( (BLKCHN(CURREF) .LT. 0) .AND.
     $                (CHVAL .EQ. 0) ) THEN
C             need false - got false
              CONDCK= 1
            ELSE
C             condition fails
              CONDCK= 0
            END IF
C         ELSE
C           block is already false - continue checking only to force
C           evaluation of last chain if necessary
          END IF
C
C         update pointer
          CURREF= CURREF+ 1
C       end do-until loop on chains
        IF ( (ERRFLG .EQ. 0) .AND. (CURREF .LE. LSTREF) ) GO TO 10
C
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   MKQUAN
     I                    (MESSU,MSGFL,SCLU,MXSPV1,UVQNM1,NVQD,TYPCOD,
     M                     QUAN,ERRFLG,ECOUNT,IVAL,RVAL,UVQIDX)
C
C     + + + PURPOSE + + +
C     Determine if a quantity is constant or a variable and return
C     the value or a pointer as applicable.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      MESSU,MSGFL,SCLU,MXSPV1,NVQD,TYPCOD,ERRFLG,ECOUNT,
     $             IVAL,UVQIDX
      REAL         RVAL
      CHARACTER*1  UVQNM1(MXSPV1)
      CHARACTER*10 QUAN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSU  - unit number to write messages on
C     MSGFL  - unit number for file containing error messages
C     SCLU   - cluster in file containing error text
C     MXSPV1 - maximum number of characters in variable quantity names
C     UVQNM1 - library of variable quantity names
C     NVQD   - number of active variable quantity names
C     TYPCOD - type code: 2=integer, 3=real, 4=double precision
C     QUAN   - String containing either a real number or the name of
C              a user-defined variable quantity
C     ERRFLG - error flag
C     ECOUNT - error count
C     IVAL   - integer value in QUAN, or left unaltered
C     RVAL   - real value in QUAN, or left unaltered
C     UVQIDX - index of UVQUAN name, or 0 if invalid, or unaltered
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,I6,I10,SGRP
      REAL         R
      CHARACTER*6  NAME
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (NAME,NAME1)
      CHARACTER*1  NAME1(6)
C
C     + + + FUNCTIONS + + +
      INTEGER      CHKSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL     CHKSTR,OMSTC,OMSG,LFTSTR
C
C     + + + DATA INITIALIZATIONS + + +
      DATA I6,I10/6,10/
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (I10)
 1010 FORMAT (F10.0)
C
C     + + + END SPECIFICATIONS + + +
C
      UVQIDX= 0
C
      IF (ERRFLG .EQ. 0) THEN
C       try to read as a number
        IF (TYPCOD .EQ. 2) THEN
C         read as integer
          READ (QUAN,1000,ERR=10) I
          IVAL= I
        ELSE
C         read as real
          READ (QUAN,1010,ERR=10) R
          RVAL= R
        END IF
        GO TO 20
 10     CONTINUE
C         assume quan contains a user-defined name
          IF (NVQD .GT. 0) THEN
C           compare against list of names
            CALL LFTSTR (I10,
     M                   QUAN)
            NAME= QUAN(1:6)
            UVQIDX= CHKSTR (I6,NVQD,NAME1,UVQNM1)
          END IF
          IF (UVQIDX .EQ. 0) THEN
C           invalid user-defined name given
            ERRFLG= 1
            CALL OMSTC (I10,QUAN)
            SGRP= 7
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 ECOUNT)
          END IF
 20     CONTINUE
      END IF
C 
      RETURN
      END
C
C
C
      SUBROUTINE   UPQUAN
     I                    (SPIVL,SPOPNO)
C
C     + + + PURPOSE + + +
C     Update all pipes for user-defined variable quantities based
C     on variables in an operation.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER SPIVL,SPOPNO
C
C     + + + ARGUMENT DEFINITIONS + + +
C     SPOPNO - index of target operation
C     SPIVL  - interval of run
C
C     + + + COMMON BLOCKS + + +
      INCLUDE     'cmdum.inc'
      INCLUDE     'cmpad.inc'
C     user defined variable quantity definitions
      INCLUDE     'pspvqd.inc'
      INCLUDE     'cspvqd.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   UVQ,PTR
C
C     + + + FUNCTIONS + + +
      INTEGER   DADDR
C
C     + + + INTRINSICS + + +
      INTRINSIC SNGL,MOD
C
C     + + + EXTERNALS + + +
      EXTERNAL  DADDR
C
C     + + + END SPECIFICATIONS + + +
C
      DO 10 UVQ= 1, NVQD
        IF (UVQOPX(UVQ) .EQ. SPOPNO) THEN
C         condition depends on this operation
C
C         compute pointer for current interval
          PTR= MOD (SPIVL- 1,UVQLEN(UVQ))+ UVQPOS(UVQ)
C
C         update pipe
          IF (UVQTYP(UVQ) .EQ. 2) THEN
C           integer
            IVQPIP(PTR)= IPAD(UVQADD(UVQ))
          ELSE IF (UVQTYP(UVQ) .EQ. 3) THEN
C           real
            UVQPIP(PTR)= PAD(UVQADD(UVQ))
          ELSE IF (UVQTYP(UVQ) .EQ. 4) THEN
C           double precision
            UVQPIP(PTR)= SNGL (DPPAD(DADDR (UVQADD(UVQ))))
          END IF
C
        END IF
 10   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   GTQUAN
     I                    (UVQ,SPIVL,SPOPNO,
     O                     IVAL,RVAL,TYPFG)
C
C     + + + PURPOSE + + +
C     Calculate a user-defined variable quantity
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER  UVQ,SPIVL,SPOPNO,IVAL,TYPFG
      REAL     RVAL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     UVQ    - index of quantity to calculate, if negative, then get
C            - value of workspace slot
C     SPIVL  - interval of run
C     SPOPNO - index of target operation
C     IVAL   - integer value of quantity, or left unaltered
C     RVAL   - real value of quantity, or left unaltered
C     TYPFG  - type of quantity: 2=integer, 3=real
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'pspvqd.inc'
      INCLUDE 'cspvqd.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER  CURRENT,FIRST,LAST,PTR,ENDPIP,LUVQ
      REAL     UNDEF
C
C     + + + INTRINSICS + + +
      INTRINSIC MOD,MAX,MIN,FLOAT,INT,ABS
C
C     + + + DATA INITIALIZATIONS + + +
      DATA UNDEF/-1.0E+30/
C
C     + + + END SPECIFICATIONS + + +
C
      LUVQ= ABS (UVQ)
C
C     determine first and last values to accumulate
C
C     find current interval in pipe
      CURRENT= MOD (SPIVL- 1,UVQLEN(LUVQ))+ UVQPOS(LUVQ)
C
      IF (SPOPNO .LE. UVQOPX(LUVQ)) THEN
C       quantity is downstream - point back one interval to get last update
        CURRENT= CURRENT- 1
      END IF
C
      IF (UVQ .LT. 0) THEN
C       just get current value
        LAST= CURRENT
      ELSE
C       step back intervals for lag
        LAST= CURRENT- UVQLAG(UVQ)
      END IF
C
C     make sure pointer is within range of pipe
      IF (LAST .LT. UVQPOS(LUVQ)) THEN
C       wrap around
        LAST= LAST+ UVQLEN(LUVQ)
      END IF
C
      IF (UVQ .LT. 0) THEN
C       go ahead and get value
        IF (UVQTYP(LUVQ) .EQ. 2) THEN
C         integer
          IVAL= IVQPIP(LAST)
          RVAL= 0.0
          TYPFG= 2
        ELSE
C         real
          RVAL= UVQPIP(LAST)
          IVAL= 0
          TYPFG= 3
        END IF
      ELSE
C       finish setting up and retrieving overall value
C
C       count back for multiple intervals to aggregate
        FIRST= LAST- UVQAGG(UVQ)+ 1
C
C       make sure pointer is within range of pipe
        IF (FIRST .LT. UVQPOS(UVQ)) THEN
C         wrap around
          FIRST= FIRST+ UVQLEN(UVQ)
        END IF
C
C       initialize values
        IF (UVQAFG(UVQ) .EQ. 3) THEN
C         maximum
          IVAL= -1000000
          RVAL= -1.0E+30
        ELSE IF (UVQAFG(UVQ) .EQ. 4) THEN
C         minimum
          IVAL= 1000000
          RVAL= 1.0E+30
        ELSE
C         sum or average
          IVAL= 0
          RVAL= 0.0
        END IF
C
C       initialize pointer
        PTR= FIRST- 1
        ENDPIP= UVQPOS(UVQ)+ UVQLEN(UVQ)- 1
C
C       accumulate values
 10     CONTINUE
C
C         update pointer
          PTR= PTR+ 1
          IF (PTR .GT. ENDPIP) THEN
C           reset to beginning
            PTR= UVQPOS(UVQ)
          END IF
C
          IF (UVQTYP(UVQ) .EQ. 2) THEN
C           integer
            TYPFG= 2
            IF (IVAL .NE. -999) THEN
C             aggregated value is still defined
              IF (UVQAFG(UVQ) .EQ. 3) THEN
C               max
                IVAL= MAX (IVAL,IVQPIP(PTR))
              ELSE IF (UVQAFG(UVQ) .EQ. 4) THEN
C               min
                IVAL= MIN (IVAL,IVQPIP(PTR))
              ELSE IF (IVQPIP(PTR) .EQ. -999) THEN
C               new value is undefined - make sum undefined
                IVAL= -999
              ELSE IF (UVQAFG(UVQ) .EQ. 1) THEN
C               sum
                IVAL= IVAL+ IVQPIP(PTR)
              END IF
            END IF
          ELSE
C           real or double precision
            TYPFG= 3
            IF (RVAL .NE. UNDEF) THEN
C             aggregated value is still defined
              IF (UVQAFG(UVQ) .EQ. 3) THEN
C               max
                RVAL= MAX (RVAL,UVQPIP(PTR))
              ELSE IF (UVQAFG(UVQ) .EQ. 4) THEN
C               min
                RVAL= MIN (RVAL,UVQPIP(PTR))
              ELSE IF (UVQPIP(PTR) .EQ. UNDEF) THEN
C               new value is undefined - make sum or aver undefined
                RVAL= UNDEF
              ELSE IF (UVQAFG(UVQ) .EQ. 1) THEN
C               sum
                RVAL= RVAL+ UVQPIP(PTR)
              ELSE IF (UVQAFG(UVQ) .EQ. 2) THEN
C               aver
                RVAL= RVAL+ UVQPIP(PTR) / FLOAT (UVQAGG(UVQ))
              END IF
            END IF
          END IF
C
        IF (PTR .NE. LAST) GO TO 10
C
        IF (UVQTYP(UVQ) .EQ. 2) THEN
C         integer
          IF (IVAL .NE. -999) THEN
C           defined value
            IVAL= INT (FLOAT (IVAL) * UVQMUL(UVQ))
          END IF
        ELSE
C         real
          IF (RVAL .NE. UNDEF) THEN
C           defined value
            RVAL= RVAL* UVQMUL(UVQ)
          END IF
        END IF
      END IF
C
      RETURN
      END
C
C
C
      INTEGER FUNCTION   DADDR
     I                         (IADDR)
C     + + + PURPOSE + + +
C     Calculate the DPPAD address from the PAD address.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER IADDR
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IADDR  - address in PAD/IPAD array of a variable - must be odd
C
C     + + + END SPECIFICATIONS + + +
C
      DADDR= (IADDR- 1)/2+ 1
C
      RETURN
      END
C
C
C
      SUBROUTINE   CKRWID
     I                    (UVQOPX,UVQLAG,OPTYP,TOPFST,TOPLST,MAXOPN,
     I                     OPNTAB,NOPNS,
     M                     RUNWID)
C
C     + + + PURPOSE + + +
C     Checks the target operations of a special action to
C     determine any restriction in run width caused by
C     user-defined variable quantities.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     UVQOPX,UVQLAG,TOPFST,TOPLST,MAXOPN,OPNTAB(20,MAXOPN),
     $            NOPNS,RUNWID
      CHARACTER*6 OPTYP
C
C     + + + ARGUMENT DEFINITIONS + + +
C     UVQOPX - operation index of base variable for user-defined
C              variable quantity
C     UVQLAG - lag in intervals for user-defined variable quantity
C     OPTYP  - operation type of special action target(s)
C     TOPFST - first target operation ID number of special action target(s)
C     TOPLST - last target operation ID number of special action target(s)
C     MAXOPN - maximum number of operations
C     OPNTAB - information on operations
C     NOPNS  - number of operations
C     RUNWID - maximum run span width allowed by user-defined variable
C              quantities - 0 if no restrictions
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   START,TOPX,RSTRIC
C
C     + + + FUNCTIONS + + +
      INTEGER   OPNNO
C
C     + + + INTRINSICS + + +
      INTRINSIC MIN
C
C     + + + EXTERNALS + + +
      EXTERNAL  OPNNO
C
C     + + + END SPECIFICATIONS + + +
C
C     do-until checked all target ops
      START= 1
 10   CONTINUE
        TOPX= OPNNO (OPTYP,TOPFST,TOPLST,MAXOPN,OPNTAB,START,
     $               NOPNS)
        IF (TOPX .GT. 0) THEN
C         check this operation
          IF (TOPX .LT. UVQOPX) THEN
C           quantity is downstream of target - can run lag+1 ivls
            RSTRIC= UVQLAG+ 1
            IF (RUNWID .LE. 0) THEN
C             this is first restriction
              RUNWID= RSTRIC
            ELSE
C             lowest restriction applies
              RUNWID= MIN (RUNWID,RSTRIC)
            END IF
          ELSE IF (TOPX .GT. UVQOPX) THEN
C           quantity is upstream of target
            IF (RUNWID .EQ. 0) THEN
C             run width is determined later in pspips
C             set flag value
              RUNWID= -1
            END IF
          ELSE
C           quantity is in same operation as target
C           no run width restriction
          END IF
        END IF
C
C       increment operation pointer
        START= START+ 1
C
C     end of loop on target operations
      IF (TOPX .GT. 0) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE   SPARSE
     I                    (STLINE,OUTLEV,MESSU,MSGFL,SCLU,
     M                     ECOUNT,UCIBUF,KEY,
     O                     NEWCHN,NUMLIN)
C
C     + + + PURPOSE + + +
C     Parse free-form special action line(s) into a logic chain.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      STLINE,OUTLEV,MESSU,MSGFL,SCLU,ECOUNT,KEY,NEWCHN,
     $             NUMLIN
      CHARACTER*80 UCIBUF
C
C     + + + ARGUMENT DEFINITIONS + + +
C     STLINE - position of beginning of line after any keywords
C     OUTLEV - run interpreter output level
C     MESSU  - unit number to write messages on
C     MSGFL  - unit number for file containing error messages
C     SCLU   - cluster in file containing error text
C     ECOUNT - error count
C     UCIBUF - buffer containing current record from uci file
C     KEY    - current record number in uci file
C     NEWCHN - pointer to beginning of new condition chain
C     NUMLIN - number of UCI lines in chain specification
C
C     + + + PARAMETERS + + +
      INTEGER MXITEM
      PARAMETER (MXITEM = 200)
C
C     + + + COMMON BLOCKS + + +
C     special action conditions
      INCLUDE     'pspcnd.inc'
      INCLUDE     'cspcnd.inc'
C     user defined variable quantity definitions
      INCLUDE     'pspvqd.inc'
      INCLUDE     'cspvqd.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      NITEM,PARLEV(MXITEM),ITMTYP(MXITEM),POS,CURLEV,
     $             DONEFG,ERRFLG,PRECON,I,J,SGRP,I0,I3,BUFLEN,PREPOS,
     $             RLEN,SDIG,DECP,RPARS,LPARS,ECHPOS,MAX
      CHARACTER*1  SPACE
      CHARACTER*4  CLOGOP(3)
      CHARACTER*10 CITEM
      CHARACTER*80 ECHBUF
C
C     + + + FUNCTIONS + + +
      INTEGER      LENSTR,CKNBLV
C
C     + + + EXTERNALS + + +
      EXTERNAL     ZIPI,LENSTR,ZIPC,SPITEM,OMSTI,OMSG,OMSTC,MKQUAN,
     $             DECCHX,GETUCI,CKNBLV
C
C     + + + DATA INITIALIZATIONS + + +
      DATA I0,I3/0,3/
      DATA RLEN,SDIG,DECP/10,5,-5/
      DATA SPACE/' '/
      DATA CLOGOP/'AND ','OR  ','THEN'/
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (A6)
 2010 FORMAT (A2)
 2020 FORMAT (A4)
 2030 FORMAT (I3)
 2040 FORMAT ('(')
 2050 FORMAT (')')
 2060 FORMAT (A80)
C
C     + + + END SPECIFICATIONS + + +
C
C     initialize variables
      NUMLIN= 1
      PRECON= NCOND
      NITEM= 0
      CALL ZIPI (MXITEM,NITEM,PARLEV)
      CALL ZIPI (MXITEM,NITEM,ITMTYP)
      POS= STLINE
      CURLEV= 0
      DONEFG= 0
      I= 80
      BUFLEN= LENSTR (I,UCIBUF)
      RPARS= 0
      LPARS= 0
      CALL ZIPC (I,SPACE,ECHBUF)
C
C     begin do-until loop on items
 10   CONTINUE
C
        IF (POS .LE. BUFLEN) THEN
C         more to do on this line
C
C         get next item
          NITEM= NITEM+ 1
          PREPOS= POS
          CALL SPITEM (UCIBUF,BUFLEN,
     M                 POS,
     O                 CITEM,ITMTYP(NITEM))
C         process item by code
          GO TO (100,100,100,110,110,110,120,130,140,140,150)
     $           ITMTYP(NITEM)
C
C           error - program bug
            WRITE(*,*) 'ERROR PARSING LINE',KEY,' AT POS',PREPOS
          GO TO 200
C
C         left parenthesis
 100      CONTINUE
            IF (NITEM .GT. 1) THEN
C             make sure previous item was a logic operator or a
C             left parenthesis
              IF ( (ITMTYP(NITEM-1) .NE. 9) .AND.
     $             (ITMTYP(NITEM-1) .NE. 10) ) THEN
C               previous item was not a logic operator
                IF ( (ITMTYP(NITEM-1) .LT. 1) .OR.
     $               (ITMTYP(NITEM-1) .GT. 3) ) THEN
C                 error - left parenthesis in improper position
                  CALL OMSTI (PREPOS)
                  SGRP= 80
                  CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                       ECOUNT)
                  DONEFG= -1
                END IF
              END IF
            END IF
            IF (DONEFG .EQ. 0) THEN
C             proper position
              CURLEV= CURLEV+ 1
              PARLEV(NITEM)= CURLEV
              LPARS= LPARS+ 1
            END IF
          GO TO 200
C
C         right parenthesis
 110      CONTINUE
            IF (NITEM .LT. 3) THEN
C             too early
              DONEFG= -1
            ELSE IF ( (ITMTYP(NITEM-1) .NE. 7) .OR.
     $                (ITMTYP(NITEM-2) .NE. 8) ) THEN
C             right paren doesn't follow a compop-quantity sequence
              IF ( (ITMTYP(NITEM-1) .LT. 4) .OR.
     $             (ITMTYP(NITEM-1) .GT. 6) ) THEN
C               right paren doesn't follow another right paren
                DONEFG= -1
              END IF
            END IF
            IF (DONEFG .EQ. -1) THEN
C             error - right parenthesis in improper position
              CALL OMSTI (PREPOS)
              SGRP= 81
              CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                   ECOUNT)
            ELSE
C             proper position
              IF (CURLEV .LT. 1) THEN
C               error - too many right parentheses
                CALL OMSTI (PREPOS)
                SGRP= 82
                CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                     ECOUNT)
                DONEFG= -1
              ELSE
C               calculate number of logic stack pops
                I= NITEM- 1
 115            CONTINUE
                  IF ( (ITMTYP(I) .EQ. 9) .OR.
     $                 (ITMTYP(I) .EQ. 10) ) THEN
C                   intervening logical operator
                    IF (PARLEV(I) .LE. CURLEV) THEN
C                     this operator to be popped from this parenthesis
                      CNDPOP(NCOND)= CNDPOP(NCOND)+ 1
                    END IF
                  ELSE IF ( (ITMTYP(I) .GE. 1) .AND.
     $                      (ITMTYP(I) .LE. 3) ) THEN
C                   found a left parenthesis
                    IF (PARLEV(I) .EQ. CURLEV) THEN
C                     found matching left parenthesis
                      IF (ITMTYP(I) .EQ. (ITMTYP(NITEM)-3)) THEN
C                       type of parenthesis matches
                        PARLEV(NITEM)= CURLEV
                        I= -1
                      ELSE
C                       error - right parenthesis doesn't match left
                        CALL OMSTI (PREPOS)
                        SGRP= 83
                        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                             ECOUNT)
                        DONEFG= -1
                      END IF
                    END IF
                  END IF
                  I= I- 1
                IF ( (I .GT. 0) .AND. (DONEFG .EQ. 0) ) GO TO 115
C
                IF (DONEFG .GE. 0) THEN
C                 proper right parenthesis
                  RPARS= RPARS+ 1
                  CURLEV= CURLEV- 1
                END IF
C
              END IF
            END IF
          GO TO 200
C
C         quantity
 120      CONTINUE
            IF (NITEM .EQ. 1) THEN
C             left operand at beginning of line
              I= 1
              NCOND= NCOND+ 1
              CNDPOP(NCOND)= 0
            ELSE IF ( (ITMTYP(NITEM-1) .GE. 1) .AND.
     $                (ITMTYP(NITEM-1) .LE. 3) ) THEN
C             left operand after left parenthesis
              I= 1
              NCOND= NCOND+ 1
              CNDPOP(NCOND)= 0
            ELSE IF ( (ITMTYP(NITEM-1) .EQ. 9) .OR.
     $                (ITMTYP(NITEM-1) .EQ. 10) ) THEN
C             left operand after logical operator
              I= 1
              NCOND= NCOND+ 1
              CNDPOP(NCOND)= 0
            ELSE IF (ITMTYP(NITEM-1) .EQ. 8) THEN
C             right operand after comparison operator
              I= 2
            ELSE
C             error - quantity not in proper position
              CALL OMSTI (PREPOS)
              I= 10
              CALL OMSTC (I,CITEM(1:10))
              SGRP= 84
              CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                   ECOUNT)
              DONEFG= -1
            END IF
            IF (DONEFG .EQ. 0) THEN
C             get quantity
              ERRFLG= 0
              MAX= MXSPVQ*6
              CALL MKQUAN (MESSU,MSGFL,SCLU,MAX,UVQNM1,NVQD,I3,
     M                     CITEM,ERRFLG,ECOUNT,J,CNDVAL(I,NCOND),
     M                     CNDUVQ(I,NCOND))
              IF (ERRFLG .NE. 0) THEN
C               error occurred reading a quantity
                DONEFG= -1
              END IF
              IF ( (DONEFG .EQ. 0) .AND. (OUTLEV .GT. 2) ) THEN
C               prepare echo buffer
                IF (I .EQ. 1) THEN
C                 first quantity
                  ECHPOS= 11
                ELSE
C                 second quantity
                  ECHPOS= 25
                END IF
                IF (CNDUVQ(I,NCOND) .GT. 0) THEN
C                 variable quantity
                  WRITE (ECHBUF(ECHPOS:ECHPOS+9),2000) CITEM(1:6)
                ELSE
C                 constant quantity
                  CALL DECCHX (CNDVAL(I,NCOND),RLEN,SDIG,DECP,
     O                         ECHBUF(ECHPOS:ECHPOS+9)) 
                END IF
              END IF
            END IF
          GO TO 200
C
C         comparison operator
 130      CONTINUE
            IF (NITEM .EQ. 1) THEN
C             too early 
              DONEFG= -1
            ELSE IF (ITMTYP(NITEM-1) .NE. 7) THEN
C             previous item is not a quantity
              DONEFG= -1
            ELSE IF (NITEM .GT. 2) THEN
C             check before previous item - must be a logical operator or
C             a left parenthesis
              IF ( (ITMTYP(NITEM-2) .NE. 9) .AND.
     $             (ITMTYP(NITEM-2) .NE. 10) .AND.
     $             (ITMTYP(NITEM-2) .GE. 4) ) THEN
C               previous quantity is not a left operand
                DONEFG= -1
              END IF
            END IF
            IF (DONEFG .NE. 0) THEN
C             error - comparison operator not in proper position
              CALL OMSTI (PREPOS)
              I= 2
              CALL OMSTC (I,CITEM(1:2))
              SGRP= 85
              CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                   ECOUNT)
            ELSE
C             process comparison operator
              IF (CITEM(1:3) .EQ. '!= ') THEN
C               not equal
                CNDCOD(NCOND)= 2
              ELSE IF (CITEM(1:3) .EQ. '>= ') THEN
C               greater or equal
                CNDCOD(NCOND)= 4
              ELSE IF (CITEM(1:3) .EQ. '<= ') THEN
C               less or equal
                CNDCOD(NCOND)= 6
              ELSE IF (CITEM(1:2) .EQ. '= ') THEN
C               equal
                CNDCOD(NCOND)= 1
              ELSE IF (CITEM(1:2) .EQ. '> ') THEN
C               greater
                CNDCOD(NCOND)= 3
              ELSE IF (CITEM(1:2) .EQ. '< ') THEN
C               less
                CNDCOD(NCOND)= 5
              ELSE
C               error - unrecognized comparison code
                CALL OMSTI (PREPOS)
                I= 10
                CALL OMSTC (I,CITEM(1:10))
                SGRP= 86
                CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                     ECOUNT)
                DONEFG= -1
              END IF
              IF ( (DONEFG .EQ. 0) .AND. (OUTLEV .GT. 2) ) THEN
C               prepare echo buffer
                WRITE (ECHBUF(22:23),2010) CITEM(1:2)
              END IF
            END IF
          GO TO 200
C
C         logical operator
 140      CONTINUE
            IF (NITEM .LT. 4) THEN
C             too early
              DONEFG= -1
            ELSE IF ( ( (ITMTYP(NITEM-1) .NE. 7) .OR.
     $                  (ITMTYP(NITEM-2) .NE. 8) ) .AND.
     $                ( (ITMTYP(NITEM-1) .LT. 4) .OR.
     $                  (ITMTYP(NITEM-1) .GT. 6) ) ) THEN
C             does not follow either a right parenthesis or a
C             compop-quantity sequence
              DONEFG= -1
            END IF
            IF (DONEFG .NE. 0) THEN
C             error - logical operator not in proper position
              CALL OMSTI (PREPOS)
              I= 3
              CALL OMSTC (I,CITEM(1:3))
              SGRP= 87
              CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                   ECOUNT)
            ELSE
C             process logic operator
              CNDLOG(NCOND)= ITMTYP(NITEM)- 8
              CNDLNK(NCOND)= NCOND+ 1
              PARLEV(NITEM)= CURLEV
              IF (OUTLEV .GT. 2) THEN
C               write output buffer
                WRITE (ECHBUF(41:44),2020) CLOGOP(CNDLOG(NCOND))
                WRITE (ECHBUF(6:8),2030) LPARS
                WRITE (ECHBUF(9:9),2040)
                WRITE (ECHBUF(37:39),2030) RPARS
                WRITE (ECHBUF(36:36),2050)
                WRITE (MESSU,2060) ECHBUF
                LPARS= 0
                RPARS= 0
                ECHBUF= ' '
              END IF
            END IF
          GO TO 200
C
C         delimiter found
 150      CONTINUE
            IF (NITEM .LT. 4) THEN
C             too early
              DONEFG= -1
            ELSE IF ( ( (ITMTYP(NITEM-1) .NE. 7) .OR.
     $                  (ITMTYP(NITEM-2) .NE. 8) ) .AND.
     $                ( (ITMTYP(NITEM-1) .LT. 4) .OR.
     $                  (ITMTYP(NITEM-1) .GT. 6) ) ) THEN
C             does not follow either a right parenthesis or a
C             compop-quantity sequence
              DONEFG= -1
            END IF
            IF (DONEFG .NE. 0) THEN
C             error - THEN not in proper position
              CALL OMSTI (PREPOS)
              SGRP= 88
              CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                   ECOUNT)
            ELSE IF (CURLEV .NE. 0) THEN
C             error - not enough right parentheses
              SGRP= 89
              CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                   ECOUNT)
              DONEFG= -1
            END IF
            IF (DONEFG .EQ. 0) THEN
C             done - last link pointer is null
              DONEFG= 1
              CNDLNK(NCOND)= 0
              IF (OUTLEV .GT. 2) THEN
C               write output buffer
                WRITE (ECHBUF(41:44),2020) CLOGOP(3)
                WRITE (ECHBUF(6:8),2030) LPARS
                WRITE (ECHBUF(9:9),2040)
                WRITE (ECHBUF(37:39),2030) RPARS
                WRITE (ECHBUF(36:36),2050)
                WRITE (MESSU,2060) ECHBUF
                LPARS= 0
                RPARS= 0
                ECHBUF= ' '
              END IF
            END IF
C
C         end computed goto
 200      CONTINUE
        ELSE
C         end of line - need to read new line
          CALL GETUCI (I0,
     M                 KEY,
     O                 UCIBUF)
          I= 80
          BUFLEN= LENSTR (I,UCIBUF)
          POS= CKNBLV (I,UCIBUF)
          NUMLIN= NUMLIN+ 1
        END IF
C     end of do-until loop on items
      IF (DONEFG .EQ. 0) GO TO 10
C
      IF (DONEFG .LT. 0) THEN
C       an error ocurred - omit new chain
        NCOND= PRECON
        NEWCHN= 0
      ELSE
C       chain is valid
        NEWCHN= PRECON+ 1
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   SPITEM
     I                    (UCIBUF,BUFLEN,
     M                     BUFPOS,
     O                     CITEM,ITMTYP)
C
C     + + + PURPOSE + + +
C     Read next item from a line in buffer, and return a type code.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      BUFLEN,BUFPOS,ITMTYP
      CHARACTER*10 CITEM
      CHARACTER*80 UCIBUF
C
C     + + + ARGUMENT DEFINITIONS + + +
C     UCIBUF - buffer containing current record from uci file
C     BUFLEN - actual length of buffer, excluding trailing blanks
C     BUFPOS - starting position of next item in buffer
C     CITEM  - character variable for next item
C     ITMTYP - item type code:
C               1  (
C               2  [
C               3  {
C               4  )
C               5  ]
C               6  }
C               7  quantity, either number or user-defined quantity name
C               8  comparison operator: =, !=, >, <, >=, <=
C               9  logical operator AND
C              10  logical operator OR
C              11  delimiter: THEN
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,DONEFG,ITPOS,ENDFG,CHARFG
      CHARACTER*1 PAREN(6),COMPOP(4),SPACE
C
C     + + + DATA INITIALIZATIONS + + +
      DATA PAREN  /'(','[','{',')',']','}'/
      DATA COMPOP /'=','/','<','>'/
      DATA SPACE  /' '/
C
C     + + + END SPECIFICATIONS + + +
C
C     initialize
      DONEFG= 0
      ENDFG= 0
      CHARFG= 0
      ITMTYP= 0
      ITPOS= 1
      CITEM= '          '
C
C     check for parenthesis
      I= 0
 10   CONTINUE
        I= I+ 1
        IF (UCIBUF(BUFPOS:BUFPOS) .EQ. PAREN(I)) THEN
C         found parenthesis
          ITMTYP= I
          CITEM(ITPOS:ITPOS)= PAREN(I)
          ENDFG= 1
          BUFPOS= BUFPOS+ 1
        END IF
      IF ( (I .LT. 6) .AND. (ENDFG .EQ. 0) ) GOTO 10
C
      IF (ENDFG .EQ. 0) THEN
C       check for comparison operator
        I= 0
 20     CONTINUE
          I= I+ 1
          IF (UCIBUF(BUFPOS:BUFPOS) .EQ. COMPOP(I)) THEN
C           found comparison operator
            ITMTYP= 8
            CITEM(ITPOS:ITPOS)= COMPOP(I)
            BUFPOS= BUFPOS+ 1
            ITPOS= ITPOS+ 1
          END IF
        IF ( (I .LT. 4) .AND. (ITMTYP .EQ. 0) ) GO TO 20
      END IF
C
C     loop until find end of item (and trailing spaces if any)
 30   CONTINUE
C
        IF (ENDFG .EQ. 1) THEN
C         check for space
          IF (UCIBUF(BUFPOS:BUFPOS) .EQ. SPACE) THEN
C           still in trailing spaces
            BUFPOS= BUFPOS+ 1
          ELSE
C           found end of trailing spaces
            DONEFG= 1
          END IF
        ELSE
C         process next character
C
C         check for space
          IF (UCIBUF(BUFPOS:BUFPOS) .EQ. SPACE) THEN
C           found end of item
            ENDFG= 1
            BUFPOS= BUFPOS+ 1
          END IF
C
          IF ( (ENDFG .EQ. 0) .AND. (DONEFG .EQ. 0) ) THEN
C           check for parenthesis
            I= 0
 40         CONTINUE
              I= I+ 1
              IF (UCIBUF(BUFPOS:BUFPOS) .EQ. PAREN(I)) THEN
C               found parenthesis
                DONEFG= 1
              END IF
            IF ( (I .LT. 6) .AND. (DONEFG .EQ. 0) ) GO TO 40
          END IF
C
          IF ( (ENDFG .EQ. 0) .AND. (DONEFG .EQ. 0) ) THEN
C           check for comparison operator
            CHARFG= 0
            I= 0
 50         CONTINUE
              I= I+ 1
              IF (UCIBUF(BUFPOS:BUFPOS) .EQ. COMPOP(I)) THEN
C               found part of a comparison operator
                IF (ITMTYP .EQ. 8) THEN
C                 comparison operator continues
                  CITEM(ITPOS:ITPOS)= COMPOP(I)
                  BUFPOS= BUFPOS+ 1
                  ITPOS= ITPOS+ 1
                ELSE
C                 was reading a word or number, so found end
                 DONEFG= 1
                END IF
                CHARFG= 1
              END IF
            IF ( (I .LT. 4) .AND. (CHARFG .EQ. 0) ) GO TO 50
          END IF
C
          IF ( (CHARFG .EQ. 0) .AND. (ENDFG .EQ. 0) .AND.
     $         (DONEFG.EQ. 0) ) THEN
C           current character is not a special character
            IF (ITMTYP .EQ. 0) THEN
C             word continues
              CITEM(ITPOS:ITPOS)= UCIBUF(BUFPOS:BUFPOS)
              BUFPOS= BUFPOS+ 1
              ITPOS= ITPOS+ 1
            ELSE
C             comparison code is over
              DONEFG= 1
            END IF
          END IF
C
        END IF
C
        IF (ITPOS .GT. 10) THEN
C         filled up item
          DONEFG= 1
          ITPOS= 10
        END IF
        IF (BUFPOS .GT. BUFLEN) THEN
C         reached end of line
          DONEFG= 1
        END IF
C
C     end do-until loop
      IF (DONEFG .EQ. 0) GO TO 30
C
      IF (ITMTYP .EQ. 0) THEN
C       must determine type of word
        IF (CITEM(1:ITPOS) .EQ. 'AND') THEN
C         found logical operator AND
          ITMTYP= 9
        ELSE IF (CITEM(1:ITPOS) .EQ. 'OR') THEN
C         found logical operator OR
          ITMTYP= 10
        ELSE IF (CITEM(1:ITPOS) .EQ. 'THEN') THEN
C         found delimiter
          ITMTYP= 11
        ELSE
C         assume quantity
          ITMTYP= 7
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PSPUML
     I                    (UCIBUF,MESSU,OUTLEV,MSGFL,SCLU,SPOUT,
     M                     ECOUNT)
C
C     + + + PURPOSE + + +
C     read and process mfact change record from ucifl
C    
C     + + + HISTORY + + +
C     2004/12/20 - jlk&pbd - initial implementation
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      MESSU,OUTLEV,MSGFL,SCLU,ECOUNT,SPOUT
      CHARACTER*80 UCIBUF
C
C     + + + ARGUMENT DEFINITIONS + + +
C     UCIBUF - buffer containing current record from uci file
C     MESSU  - unit number to write messages on
C     OUTLEV - output level
C     MSGFL  - unit number for file containing error messages
C     SCLU   - cluster in file containing error text
C     SPOUT  - runtime Special Action output level
C     ECOUNT - error count
C
C     + + + COMMON BLOCKS + + +
      INCLUDE     'pspins.inc'
      INCLUDE     'cspins.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      ERRFLG,SGRP,I80
      CHARACTER*80 EBUFF
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE     (EBUFF,EBUF1)
      CHARACTER*1     EBUF1(80)
C
C     + + + EXTERNALS + + +
      EXTERNAL     OMSG,OMSTI,OMSTC
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (5X,A4,A2,I4,2(A4,A2),2I2,F10.0,A1,1X,A4,A2,I4,2(A4,A2),
     $        2I2,I4,4I2)
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT ('  MFACT UPDATE record processed')
 2010 FORMAT ('   SVOL :',A4,A2,': SNUM :',I4,': SGRPN ',A4,A2,
     $        ': SGRPN :',A4,A2,': SMEMSB :',2I3)
 2020 FORMAT ('   TVOL :',A4,A2,': TNUM :',I4,': TGRPN ',A4,A2,
     $        ': TGRPN :',A4,A2,': TMEMSB :',2I3)
 2030 FORMAT ('   MDATE :',I4,4I3,': MFACT :',G10.4,': FUNC :',A1)
C
C     + + + END SPECIFICATIONS + + +
C
      I80   = 80
      ERRFLG= 0
C     save special action output level in common
      SPOUTM= SPOUT
C
      SPUMPT = SPUMPT + 1
      IF (SPUMPT .GT. MXSPUM) THEN
C       error - too many mfact update records
        ERRFLG= 1
        CALL OMSTI (MXSPUM)
        SGRP= 109
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             ECOUNT)
      END IF
C
      IF (ERRFLG .EQ. 0) THEN
C       read line
C       WRITE(99,*) 'PSPUML:about to read uci'
        READ (UCIBUF,1000,ERR=10)  (SPUMIN(I,SPUMPT),I=1,9),
     1                              SPUMIR(10,SPUMPT),
     1                             (SPUMIN(I,SPUMPT),I=11,25)
C       WRITE(99,*) ' PSPUML:read uci into',SPUMPT
        GO TO 20
 10     CONTINUE
C         error - read format
          EBUFF= UCIBUF
          CALL OMSTC (I80,EBUF1)
          SGRP= 108
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
          ERRFLG= 1
 20     CONTINUE
C
        IF (SPOUT .GE. 0 .AND. ERRFLG .EQ. 0) THEN
C         echo initial read
          WRITE (MESSU,2000)
          WRITE (MESSU,2010) (SPUMIN(I,SPUMPT),I=1,9)
          WRITE (MESSU,2020) (SPUMIN(I,SPUMPT),I=12,20)
          WRITE (MESSU,2030) (SPUMIN(I,SPUMPT),I=21,25),
     1           SPUMIR(10,SPUMPT),SPUMIN(11,SPUMPT)
        END IF
      END IF
C
      IF (ERRFLG .NE. 0) THEN
C       ignore this record
        SPUMPT = SPUMPT - 1
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PSPUMK
     I                    (SDATIM,NDAMON)
C
C     + + + PURPOSE + + +
C     sort the mult update instructions by date
C
C     + + + HISTORY + + +
C     2004/12/20 - jlk&pbd - initial implementation
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   SDATIM(5),NDAMON(12)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     SDATIM - starting date/time
C     NDAMON - no. of days in each month of calendar year
C
C     + + + COMMON BLOCKS + + +
      INCLUDE     'pspins.inc'
      INCLUDE     'cspins.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,SORTOP,SPUMDX(MXSPUM)
C
C     + + + EQUIVALENCES + + +
C
C     + + + EXTERNALS + + +
      EXTERNAL  DIFTIM, ASRTI
C
C     + + + INPUT FORMATS + + +
C
C     + + + OUTPUT FORMATS + + +
2000  FORMAT(A,(10I8))
C
C     + + + END SPECIFICATIONS + + +
C
      NSPUM = SPUMPT
C
C     determine offset from start of run for each instruction
      DO 10 I = 1, NSPUM
        CALL  DIFTIM(SDATIM,SPUMIN(21,I),NDAMON,
     O               SPUMDX(I))
 10   CONTINUE
C
C     sort special action mult instructions in place by date
      SORTOP= 0
      CALL ASRTI (SORTOP,NSPUM,SPUMDX,
     O            SPUMPO)
C    
C     determine where to start
      SPUMPT= 1
      DO WHILE (SPUMDX(SPUMPO(SPUMPT)) .LT. 0) 
C       start with instructions at or after beginning of run
        SPUMPT= SPUMPT+ 1
      END DO
C
C      WRITE(99,*)    ' PSPUMK:NSPUM,SPUMPT',NSPUM,SPUMPT
C      WRITE(99,2000) ' PSPUMK:SPUMDX:',(SPUMDX(I),I=1,NSPUM)
C      WRITE(99,2000) ' PSPUMK:SPUMPO:',(SPUMPO(I),I=1,NSPUM)
C
      RETURN
      END
C
C
C
      SUBROUTINE   SPECLM
     I                    (MESSU,DATIM,
     I                     FOPKEY,LOPKEY,OSUPM,
     O                     MUPFG)
C
C     + + + PURPOSE + + +
C     look for update to mfactr from special actions, update if found
C
C     + + + HISTORY + + +
C     2004/12/20 - jlk&pbd - initial implementation
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      MESSU,MUPFG,DATIM(6),
     1             FOPKEY,LOPKEY,OSUPM(11,LOPKEY)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSU  - unit number to write messages on
C     DATIM  - current date and time of day
C     FOPKEY - pointer to first operation in osuper file
C     LOPKEY - pointer to last operation in osuper file
C     OSUPM  - osuper file
C     MUPFG  - flag indicating if we just updated an mfactr
C
C     + + + COMMON BLOCKS + + +
      INCLUDE     'pspins.inc'
      INCLUDE     'cspins.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       CHKNXT,I0,I1,I2,I5,IMFACT,J,NVALS,
     1              SNUM,SMEMSB(2),TNUM,TMEMSB(2),TMPDAT(6),
     2              SPUMPX
      CHARACTER*1   FUNC
      CHARACTER*6   SVOL,SGRPN,SMEMN,TVOL,TGRPN,TMEMN
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE   (MULT,IMFACT)
      REAL          MULT
C
C     + + + EXTERNALS + + +
      EXTERNAL     UPDMFT, TIMDIF
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (A4,A2)
 2010 FORMAT (A1)
 2020 FORMAT (1X,A,2I4,I5,5(I3))
C
C     + + + END SPECIFICATIONS + + +
C
      I0    = 0
      I1    = 1
      I2    = 2
      I5    = 5
      MUPFG = 0
C
C      WRITE(99,2020) 'SPECLM:entry:SPUMPT,SPUMPO,DATIM:',
C     1                       SPUMPT,SPUMPO(SPUMPT),DATIM
C
      IF (SPUMPT.GT.0) THEN
C       have some of these types of special actions
C
 10     CONTINUE
          CHKNXT = 0
          SPUMPX = SPUMPO(SPUMPT)
C          WRITE(99,2020) 'SPECLM:next ',SPUMPT,SPUMPX,
C     1               (SPUMIN(J,SPUMPX),J=21,25)
C         see if the next one applies to this date
          CALL COPYI(I5,SPUMIN(21,SPUMPX),TMPDAT)
          TMPDAT(6) = 0
          CALL TIMDIF (DATIM,TMPDAT,I2,I1,NVALS)
C          WRITE(99,*) ' SPECLM:NVALS:',NVALS
          IF (NVALS .EQ. 0) THEN
C           this change is to occur now
C
C           convert source and target from the update request 
            WRITE(SVOL,2000) SPUMIN(1,SPUMPX),SPUMIN(2,SPUMPX)
            SNUM = SPUMIN(3,SPUMPX)
            WRITE(SGRPN,2000) SPUMIN(4,SPUMPX),SPUMIN(5,SPUMPX)
            WRITE(SMEMN,2000) SPUMIN(6,SPUMPX),SPUMIN(7,SPUMPX)
            SMEMSB(1) = SPUMIN(8,SPUMPX)
            SMEMSB(2) = SPUMIN(9,SPUMPX)
C
            WRITE(TVOL,2000) SPUMIN(12,SPUMPX),SPUMIN(13,SPUMPX)
            TNUM = SPUMIN(14,SPUMPX)
            WRITE(TGRPN,2000) SPUMIN(15,SPUMPX),SPUMIN(16,SPUMPX)
            WRITE(TMEMN,2000) SPUMIN(17,SPUMPX),SPUMIN(18,SPUMPX)
            TMEMSB(1) = SPUMIN(19,SPUMPX)
            TMEMSB(2) = SPUMIN(20,SPUMPX)
C      
            IMFACT = SPUMIN(10,SPUMPX)
            WRITE(FUNC,2010) SPUMIN(11,SPUMPX)
C
            CALL UPDMFT
     I                 (MESSU,I0,FOPKEY,LOPKEY,OSUPM,
     I                  SVOL,SNUM,SGRPN,SMEMN,SMEMSB,
     I                  TVOL,TNUM,TGRPN,TMEMN,TMEMSB,
     I                  MULT,FUNC,DATIM,SPOUTM)
C
C            WRITE(99,*) 'SPECLM:afterUpdate:',SPUMPT,SPUMPX,NSPUM
C
            IF (SPUMPT.LT.NSPUM) THEN
C             there are more to come
              SPUMPT = SPUMPT + 1
              CHKNXT = 1
            ELSE
C             this was the last one
              SPUMPT = 0
            END IF
            MUPFG  = 1
          END IF
C         if the next one is on the same date, update it too
        IF (CHKNXT.EQ.1) GOTO 10
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   UPDMFT
     I                    (MESSU,ARCFIL,FOPKEY,LOPKEY,OSUPM,
     I                     SVOL,SNUM,SGRPN,SMEMN,SMEMSB,
     I                     TVOL,TNUM,TGRPN,TMEMN,TMEMSB,
     I                     MULT,FUNC,DATIM,SPOUTM)
C
C     + + + PURPOSE + + +
C     update mfactor for the input source and target
C
C     + + + HISTORY + + +
C     2004/12/20 - jlk&pbd - initial implementation
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       MESSU,ARCFIL,FOPKEY,LOPKEY,OSUPM(11,LOPKEY),
     1              SNUM,SMEMSB(2),TNUM,TMEMSB(2),DATIM(6),SPOUTM
      REAL          MULT
      CHARACTER*1   FUNC
      CHARACTER*6   SVOL,SGRPN,SMEMN,TVOL,TGRPN,TMEMN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSU  - unit number to write messages on
C     ARCFIL - unit number of archive file
C     FOPKEY - pointer to first operation in osuper file
C     LOPKEY - pointer to last operation in osuper file
C     OSUPM  - osuper file
C     SVOL   - source volume name
C     SNUM   - source volume number (eg 101)
C     SGRPN  - source group name
C     SMEMN  - source member name
C     SMEMSB - source member subscripts
C     TVOL   - target volume name
C     TNUM   - target volume number
C     TGRPN  - target group name
C     TMEMN  - target member name
C     TMEMSB - target member subscripts
C     MULT   - new multiplication factor 
C     FUNC   - mfact change function
C     DATIM  - current date/time 
C     SPOUTM - special action output level
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       SKEY,EKEY,KEY,IMATCH,OMCODE,J,SID,TID,SOPIN,TOPIN,
     1              I220,REC(220),I,K,ISTEP
      REAL          OLDA
      CHARACTER*6   OPLIB(11),SVOLX,SGRPNX,SMEMNX,TVOLX,TGRPNX,TMEMNX
      CHARACTER*255 FMSG,SMSG
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (REC(135),A),(REC(210),ARATIO)
      REAL         A,ARATIO
C
C     + + + EXTERNALS + + +
      EXTERNAL   GETTSI,PUTTSI,HDMES3
C
C     + + + DATA INITIALIZATIONS + + +
      DATA OPLIB/'PERLND','IMPLND','RCHRES',
     $           'COPY  ','PLTGEN','DISPLY','DURANL','GENER ',
     $           'MUTSIN','BMPRAC','REPORT'/
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (A4,A2)
 2005 FORMAT (/,' SPEC-ACT: MFACT AT ',I10,'/',I2,'/',I2,I3,':',I2)
 2010 FORMAT (2X,2(1X,A6,I4,1X,A6,1X,A6,2I2),1X,A1,4E12.4,I5)
 2020 FORMAT (I5,4I3,2(1X,A6,I4,1X,A6,1X,A6,2I2),1X,A1,4E12.4,I5)
 2030 FORMAT (1X,A,3(1X,G10.4),1X,A,I6)
C
C     + + + END SPECIFICATIONS + + +
C
C     to status monitor
      WRITE(SMSG,*) ' UPDMFT:entry:FOPKEY,LOPKEY',FOPKEY,LOPKEY
      CALL HDMES3(6,SMSG)
C
      I220 = 220      
C
C     keep original source and target id from update request
      SID = SNUM
      TID = TNUM
C     dummy source and target operation index
      SOPIN = 0
      TOPIN = 0
C
C     the snum and tnum that we are looking for are the true
C     operation numbers such as 101 for PERLND 101.  if svol or
C     tvol are hspf operation names, then look thru osupm for
C     that operation type and number.  the index of osupm is
C     the number that is used in the time series instruction.
C
      OMCODE = 0
      DO 20 J= 1,11
        IF (SVOL .EQ. OPLIB(J)) THEN
C         it is an operation name
          OMCODE= J
        END IF
 20   CONTINUE
      IF (OMCODE.GT.0) THEN
        DO 30 J= FOPKEY, LOPKEY
          IF ( (OSUPM(1,J) .EQ. OMCODE) .AND.
     $         (OSUPM(2,J) .EQ. SNUM) ) THEN
C           this is the jth operation, use to match tsins
            SID  = J - FOPKEY + 1
            SOPIN= J
C           WRITE(99,*) "  SOP:",OMCODE,SNUM,SID
            GO TO 35
          END IF
 30     CONTINUE
 35     CONTINUE
      END IF
C
C     to status monitor
      WRITE(SMSG,*) ' UPDMFT:found SID:',SID,SOPIN
      CALL HDMES3(6,SMSG)
C
      OMCODE = 0
      DO 40 J= 1,11
        IF (TVOL .EQ. OPLIB(J)) THEN
C         it is an operation name
          OMCODE= J
        END IF
 40   CONTINUE
C
      IF (OMCODE.GT.0) THEN
        DO 50 J= FOPKEY, LOPKEY
          IF ( (OSUPM(1,J) .EQ. OMCODE) .AND.
     $         (OSUPM(2,J) .EQ. TNUM) ) THEN
C           this is the jth operation, use to match tsins
            TID  = J - FOPKEY + 1
            TOPIN= J
C           WRITE(99,*) "  TOP:",OMCODE,TNUM,TID
            GOTO 55
          END IF
 50     CONTINUE
 55     CONTINUE
      END IF
C
C     to status monitor
      WRITE(SMSG,*) ' UPDMFT:found TID:',TID,TOPIN
      CALL HDMES3(6,SMSG)
C
C     check instructions related to source and target operation only
      IF (SOPIN .EQ. 0) THEN 
C       no appropriate sources
        SOPIN = TOPIN
      END IF
      IF (TOPIN .EQ. 0) THEN
C       no appropriate targets
        TOPIN = SOPIN
      END IF
C      
      IF (SOPIN .EQ. 0) THEN
C        WRITE(99,*) "  FAIL-NO MATCH"
      ELSE
C       look for time series instructions matching this source/target
C       DO 100 I= FOPKEY, LOPKEY
C
C       to status monitor
        WRITE(SMSG,*) ' UPDMFT:loop to match instructions,SOPIN,TOPIN:',
     $                 SOPIN,TOPIN
        CALL HDMES3(6,SMSG)
C
        ISTEP = TOPIN - SOPIN
        IF (ISTEP .EQ. 0) THEN
          ISTEP = 1
        END IF
        DO 100 I= SOPIN,TOPIN,ISTEP
          SKEY = OSUPM(3,I)
          EKEY = OSUPM(4,I)
          KEY  = SKEY
 10       CONTINUE
            IF (KEY .EQ. 0) THEN
C              WRITE(99,*) 'UPDMFT:KEY:0:SKIP',I
            ELSE
C              WRITE(99,*) ' UPDMFT:GETTSI:KEY',KEY
              CALL GETTSI (I220,KEY,
     O                     REC)
C 
              WRITE(SVOLX,2000)  REC(201),REC(202)
              WRITE(TVOLX,2000)  REC(211),REC(212)
C
              IF (SVOLX.EQ.'      ' .OR. TVOLX.EQ.'      ') THEN
C               skip the blanks, they have been set to blanks
C               because they are not the timeseries instructions
C               we want to modify
C                WRITE(99,*) "  FAIL:BLANKS"
              ELSE
C               see if this one matches the one we're looking for
C                WRITE(99,*) 'SVOL: ' // SVOL      // ":" // SVOLX
C                WRITE(99,*) 'SNUM: ' ,  SID       ,  ":" , REC(203)
C                WRITE(99,*) 'SGRPN:' // SGRPN     // ":" // SGRPNX
C                WRITE(99,*) 'SMEMN:' // SMEMN     // ":" // SMEMNX
C                WRITE(99,*) 'SMSB1:' ,  SMEMSB(1) ,  ":" , REC(208)
C                WRITE(99,*) 'SMSB2:' ,  SMEMSB(2) ,  ":" , REC(209)
C                WRITE(99,*) 'TVOL: ' // TVOL      // ":" // TVOLX
C                WRITE(99,*) 'TNUM: ' ,  TID       ,  ":" , REC(213)
C                WRITE(99,*) 'TGRPN:' // TGRPN     // ":" // TGRPNX
C                WRITE(99,*) 'TMEMN:' // TMEMN     // ":" // TMEMNX
C                WRITE(99,*) 'TMSB1:' ,  TMEMSB(1) ,  ":" , REC(218)
C                WRITE(99,*) 'TMSB2:' ,  TMEMSB(2) ,  ":" , REC(219)
C
                IMATCH = 1
                IF (SVOL.NE.SVOLX .AND. SVOL.NE.'      ') THEN
                  IMATCH = 0
                  FMSG = "  FAIL:SVOL: " // SVOL  // ":" // SVOLX
                  GOTO 80
                END IF
C  
                IF (SID.NE.REC(203) .AND. SID.NE.0) THEN
                  IMATCH = 0
                  WRITE(FMSG,*) "  FAIL:SNUM:",SID,":",REC(203)
                  GOTO 80
                END IF
C
                WRITE(SGRPNX,2000) REC(204),REC(205)
                IF (SGRPN.NE.SGRPNX .AND. SGRPN.NE.'      ') THEN
                  IMATCH = 0
                  FMSG = "  FAIL:SGRPN:" // SGRPN // ":" // SGRPNX
                  GOTO 80               
                END IF
C 
                WRITE(SMEMNX,2000) REC(206),REC(207)
                IF (SMEMN.NE.SMEMNX .AND. SMEMN.NE.'      ') THEN
                  IMATCH = 0
                  FMSG = "  FAIL:SMEMNM:" // SMEMN // ":" // SMEMNX
                  GOTO 80               
                END IF
C
                IF (SMEMSB(1).NE.REC(208) .AND. SMEMSB(1).NE.0) THEN
                  IMATCH = 0
                  WRITE(FMSG,*)"  FAIL:SMEMSB1:",SMEMSB(1),":",REC(208)
                  GOTO 80               
                END IF
C
                IF (SMEMSB(2).NE.REC(209) .AND. SMEMSB(2).NE.0) THEN
                  IMATCH = 0
                  WRITE(FMSG,*)"  FAIL:SMEMSB2:",SMEMSB(2),":",REC(209)
                  GOTO 80               
                END IF
C  
                IF (TVOL.NE.TVOLX .AND. TVOL.NE.'      ') THEN
                  IMATCH = 0
                  FMSG = "  FAIL:TVOL" // TVOL  // ":" // TVOLX
                  GOTO 80               
                END IF
C
                IF (TID.NE.REC(213) .AND. TID.NE.0) THEN
                  IMATCH = 0
                  WRITE(FMSG,*)"  FAIL:TNUM:",TID,":",REC(213)
                  GOTO 80               
                END IF
C
                WRITE(TGRPNX,2000) REC(214),REC(215)
                IF (TGRPN.NE.TGRPNX .AND. TGRPN.NE.'      ') THEN
                  IMATCH = 0
                  FMSG = "  FAIL:TGRPN:" // TGRPN // ":" // TGRPNX
                  GOTO 80               
                END IF
C
                WRITE(TMEMNX,2000) REC(216),REC(217)
                IF (TMEMN.NE.TMEMNX .AND. TMEMN.NE.'      ') THEN
                  IMATCH = 0
                  FMSG = "  FAIL:TMEMN:" // TMEMN // ":" // TMEMNX
                  GOTO 80               
                END IF
C
                IF (TMEMSB(1).NE.REC(218) .AND. TMEMSB(1).NE.0) THEN
                  IMATCH = 0
                  WRITE(FMSG,*)"  FAIL:TMEMSB1:",TMEMSB(1),":",REC(218)
                  GOTO 80               
                END IF
C
                IF (TMEMSB(2).NE.REC(219) .AND. TMEMSB(2).NE.0) THEN
                  IMATCH = 0
                  WRITE(FMSG,*)"  FAIL:TMEMSB2:",TMEMSB(2),":",REC(219)
                  GOTO 80               
                END IF
C
                IF (IMATCH.EQ.1) THEN
C                 it matches, so update accordingly
C                  WRITE(99,2030) '  SPECACT:UPDMFT:b4 upd mfact, ' //
C     $               'a, aratio, mult, func ',a,aratio,mult,func
                  OLDA = A
                  IF (FUNC.EQ.'=') THEN
                    A = ARATIO * MULT
                  ELSEIF (FUNC.EQ.'+') THEN
                    A = A + (ARATIO * MULT)
                  ELSEIF (FUNC.EQ.'-') THEN
                    A = A - (ARATIO * MULT)
                  ELSEIF (FUNC.EQ.'*') THEN
                    A = A * MULT
                  ELSE
C                    WRITE(99,*) '  unknown mfactr function "',
C     $                               FUNC,'" cant update'
                    IF (MESSU.GT.0) THEN
                      WRITE(MESSU,*) 'Unknown MFACTR function"',
     $                                 FUNC,'" cant update'
                    END IF
                    GOTO 110
                  END IF
C                 WRITE(99,*) ' UPDMFT:PUTTSI:KEY',KEY
                  CALL PUTTSI (I220,KEY,REC)
C
                  IF (MESSU.GT.0 .AND. SPOUTM.GE.1) THEN
                    WRITE (MESSU,2005) (DATIM(K),K=1,5)
                    WRITE (MESSU,2010) 
     $                      SVOL,SNUM,SGRPNX,SMEMNX,REC(208),REC(209),
     $                      TVOL,TNUM,TGRPNX,TMEMNX,REC(218),REC(219),
     $                      FUNC,MULT,OLDA,OLDA/ARATIO,ARATIO,KEY
C
                  END IF
C
                  IF (ARCFIL.GT.0) THEN
C                   write to archive file
                    WRITE (ARCFIL,2020) DATIM(1),DATIM(2),DATIM(3),
     $                      DATIM(4),DATIM(5),
     $                      SVOL,SNUM,SGRPNX,SMEMNX,REC(208),REC(209),
     $                      TVOL,TNUM,TGRPNX,TMEMNX,REC(218),REC(219),
     $                      FUNC,MULT,OLDA,OLDA/ARATIO,ARATIO,KEY
                  END IF
C
                  WRITE(SMSG,2030) '  SPECACT:UPDMFT:af upd mfact,' //
     $               ' a, aratio, mult, func ',a,aratio,mult,func,key
C                  WRITE(99,*) SMSG
                  CALL HDMES3(6,SMSG)
                END IF
C
                GO TO 90
C
  80          CONTINUE 
C               quick exit on no match
C               WRITE(99,*) FMSG
  90          CONTINUE
C
              END IF
            END IF
            KEY = KEY + 1
          IF (KEY.LE.EKEY) GOTO 10
 100    CONTINUE
C
 110    CONTINUE
      END IF
C
C     WRITE(99,*) ' UPDMFT:exit'
C
      RETURN
      END
C
C
C
      SUBROUTINE   SPECLX 
     I                    (CURDAT,DELT,
     O                     INSPAC)

C
C     + + + PURPOSE + + +
C     see if we need to update an mfact in this next run interval
C
C     + + + HISTORY + + +
C     2004/12/20 - jlk&pbd - initial implementation
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      CURDAT(6),DELT,INSPAC
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CURDAT - current date within this run
C     DELT   - time step of run in minutes
C     INSPAC - number of intervals to get to next mfact special action
C
C     + + + COMMON BLOCKS + + +
      INCLUDE     'pspins.inc'
      INCLUDE     'cspins.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I2,DATIM(6),SPUMPX
C
C     + + + EXTERNALS + + +
      EXTERNAL     TIMDIF
C
C     + + + END SPECIFICATIONS + + +
C
      I2    = 2
C
C      WRITE(99,*) ' SPECLX:entry', SPUMPT
C
      INSPAC = -1
C
      IF (SPUMPT.GT.0) THEN
C       have some of these types of special actions
C
        SPUMPX = SPUMPO(SPUMPT)
C
        DATIM(1) = SPUMIN(21,SPUMPX)
        DATIM(2) = SPUMIN(22,SPUMPX)
        DATIM(3) = SPUMIN(23,SPUMPX)
        DATIM(4) = SPUMIN(24,SPUMPX)
        DATIM(5) = SPUMIN(25,SPUMPX)
        DATIM(6) = 0
C
        CALL TIMDIF (CURDAT,DATIM,I2,DELT,
     O               INSPAC)
      END IF
C
      WRITE(99,*) ' SPECLX:INSPAC:', INSPAC
C
      RETURN
      END
