C     File: djl.f
C
C     Adapted by David J. Lampert
C     Last updated 08/10/2014
C
C     Purpose: this file contains some "substition" subroutines for compiling
C     HSPF that remove system and compiler dependency and make it easy to
C     create a path independent code for a shared library (DLL or SO).
C
C     Created a subroutine to the main program HSPFBAT to allow access to the
C     routine outside of a standard executable (for my purposes so I could
C     call it from Python).
C
C     HSPFBAT
C
C     Copied the HSPF status routines from HSPF V11 (V12 routines are designed
C     for interaction with Visual Basic and Windows), changed only to 
C     introduce dummy IOPT variable for consistency with HSPF calls
C
C     HSPSTA
C     HDMESC
C     HDMESI
C     HDMEST
C     HDMES2
C     HDMES3
C     HDMESN
C
C     Re-wrote the time/date modules to make appropriate Gfortran calls
C
C     SYDATE
C     SYTIME
C
C     Copied some needed subroutines from the HSPF 12.2 source files because 
C     source files introduced new unneeded dependencies
C
C     SYDATM from UTIL/DTTMDG.FOR
C     SCPRST from UTIL/UTSCXX.FOR
C     ZFMTWR from UTIL/UTSCXX.FOR
C     ZWRSCR from UTIL/UTSCXX.FOR
C     SCPRBF from UTIL/UTSCXX.FOR
C     COLSET from UTIL/UTSCXX.FOR
C     QFDPRS from AIDE/QFDPRS.FOR
C
C     Created dummy routines to replace frivolous screen I/O, interactions
C     with Windows and Visual Basic 6
C
C     SCPRBN      normally from UTIL/UTSCXX.FOR
C     CKUSER      new HSPF 12 subroutine in Newaqt12/HSPSTA.FOR
C     UPDWIN      new HSPF 12 subroutine in Newaqt12/HSPSTA.FOR
C     SDELAY      new HSPF 12 subroutine in Newaqt12/HSPSTA.FOR
C     HSPF_INI    new HSPF 12 subroutine in Newaqt12/HSPSTA.FOR
C     EXT_UPDATE  new HSPF 12 subroutine in Newaqt12/HSPSTA.FOR
C     LOG_MSG     new HSPF 12 subroutine in Newaqt12/HSPSTA.FOR
C
      SUBROUTINE HSPFBAT
     I                  (FILNAM, HMSNAM,
     O                   SGRP)
C
C     + + + PURPOSE + + +
C     Batch HSPF call
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmesfl.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      FILES(15),I,I0,USRFL,MESSU,
     $             RETCOD,RCL,SCLU,SGRP
      CHARACTER*64 FILNAM,VERSN,HMSNAM,FNAME
      CHARACTER*12 ACC,STAT
      CHARACTER*30 FRMT
C
C     + + + EXTERNALS + + +
      EXTERNAL     WDBFIN, FILOPN, FILBLK, HSPF, ZIPI, UCIINP, WDBOPN 
C
C     + + + END SPECIFICATIONS + + +
C
C     version info and unix what info
      INCLUDE 'versn.inc'
C
C     initialize wdm file common block
      CALL WDBFIN
C     open message file
      I= 1
C      INCLUDE 'fhsmes.inc'
      CALL WDBOPN (MESSFL,HMSNAM,I,
     O             RETCOD)
      IF (RETCOD .NE. 0) THEN
C       problem with opening message file, prompt for other file name
        WRITE (*,*) 'Problem:',RETCOD,MESSFL,I,' with ',HMSNAM
      END IF
C
      IF (RETCOD .EQ. 0) THEN
        SCLU= 201
        SGRP= 50
        USRFL= 7
        ACC= 'SEQUENTIAL'
        FRMT= 'FORMATTED'
        RCL= 0
        STAT= 'OLD'
        CALL FILOPN
     I              (ACC,FRMT,RCL,STAT,USRFL,FILNAM,
     O               RETCOD)
        IF (RETCOD .EQ. 0) THEN
C         input file opened, process files block in input file
C         initialize files to closed
          I= 14
          I0= 0
          CALL ZIPI (I,I0,
     O               FILES)
          FILES(15)= MESSFL
          CALL FILBLK
     I                (USRFL,
     M                 FILES,
     O                 RETCOD)
C         back to beginning of input file
          REWIND (USRFL)
C
          IF (RETCOD .EQ. 0) THEN
C           read users uci file
            MESSU= FILES(1)
            CALL UCIINP (USRFL,MESSFL,MESSU)
C           close users input file
            CLOSE (UNIT=USRFL)
C           proceed to run model
            CALL HSPF (FILES,
     O                 RETCOD)
C           simulation complete
            IF (RETCOD .EQ. 0) THEN
              SGRP= 52
            ELSE IF (RETCOD .EQ. 1) THEN
C             runfg=0 in global block - must stop
              SGRP= 53
            ELSE IF (RETCOD .EQ. 2) THEN
C             errors in input file -  must stop
              SGRP= 54
            ELSE IF (RETCOD .EQ. 3) THEN
C             no run keyword found in input file
              SGRP= 55
            END IF
          ELSE
C           error in files block
            SGRP= 56
          END IF
        ELSE
C         error opening uci file
          SGRP= 57
        END IF
C
      END IF
C
C     DJL - close WDM or any other open files
      DO 100 I = 1,15
         IF (FILES(I) .GT. 0) THEN
            CLOSE(UNIT=FILES(I))
         END IF
 100     CONTINUE
C      CLOSE(UNIT=99)
C      CLOSE(UNIT=11)
C      CLOSE(UNIT=12)
      RETURN
      END
C
C
C
      SUBROUTINE   SYDATE
     O                   ( YR, MO, DA )
C
C     + + + PURPOSE + + +
C     This subroutine is used to retrieve the system date.
C     This version of SYDATE calls the DG system routine IDATE.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   YR, MO, DA
C
C     + + + ARGUMENT DEFINITIONS + + +
C     YR     - year
C     MO     - month
C     DA     - day
C
C     + + + EXTERNALS + + +
C      EXTERNAL IDATE
C
C     + + + LOCAL VARIABLES + + +
C
      INTEGER DATE(3)
C
C     + + + END SPECIFICATIONS + + +
C
      CALL IDATE ( DATE )
      DA = DATE(1)
      MO = DATE(2)
      YR = DATE(3)
C
      RETURN
      END
C
C
C
      SUBROUTINE   SYTIME
     O                   ( HR, MN, SC )
C
C     + + + PURPOSE + + +
C     This subroutine is used to retrieve the system time.
C     This version of SYTIME calls the DG system routine SECNDS.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   HR, MN, SC
C
C     + + + ARGUMENT DEFINITIONS + + +
C     HR     - Number of hours since midnight
C     MN     - Number of minutes since hour
C     SC     - Number of seconds since minute
C
C     + + + FUNCTIONS + + +
      REAL      SECNDS
C
C     + + + INTRINSICS + + +
C     INTRINSIC INT
C
C     + + + EXTERNALS + + +
C      EXTERNAL  SECNDS
C
C     + + + END SPECIFICATIONS + + +
C
C     SECNDS returns the difference, in seconds, between the current
C     system time and the user supplied time.  Supplying a value of
C     zero (midnight) causes SECNDS to return the current system time.
C
      SC = INT ( SECNDS(0.0) + 0.005 )
      HR = SC / 3600
      MN = (SC  -  HR * 3600)  /  60
      SC = SC  -  HR * 3600  -  MN * 60
C
      RETURN
      END
C
C
C
      SUBROUTINE   HSPSTA
     I                   (IOPT,NOPNS,LAST,COUNT,OPN,OMCODE,OPTNO)
C
C     + + + PURPOSE + + +
C     routine to show run status for HSPF
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     IOPT,NOPNS,LAST,COUNT,OPN,OMCODE,OPTNO
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NOPNS  - total number of options
C     LAST   - last time interval
C     COUNT  - number of current time interval
C     OPN    - number of current operation number
C     OMCODE - code number of current operation
C     OPTNO  - number for this operation
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I1,I6,I80,J,SKIP,JUST,LEN,POS,SCOL,ROW,COL1,
     1            FORE,BACK,ALEN,APOS
      REAL        COLWID
      CHARACTER*1 OPNAM(6,9),WBUFF(33),OBUFF(80),BK(1),CH1,CH2,CH3
C
      SAVE        COLWID,SKIP,ALEN,APOS,ROW
C
C     + + + FUNCTIONS + + +
      INTEGER     LENSTR
C
C     + + + INTRINSICS + + +
      INTRINSIC   FLOAT,MOD
C
C     + + + EXTERNALS + + +
      EXTERNAL    ZIPC,CHRCHR,SCCUMV,SCPRST,LENSTR,SCCLAL,COLSET,
     #            INTCHR,XGTCHR
C
C     + + + DATA INITIALIZATIONS + + +
      DATA I1,I6,I80,JUST,SCOL/1,6,80,0,13/
      DATA WBUFF/'O','P','x','x','x',' ','O','F','x','x','x',' ',' ',
     1           ' ','T','I','M','E',' ','P','A','D','x','x','x','x',
     2           ' ','O','F','x','x','x','x'/
      DATA OPNAM,BK(1)/'P','E','R','L','N','D','I','M','P','L','N','D',
     1                 'R','C','H','R','E','S','C','O','P','Y',' ',' ',
     2                 'P','L','T','G','E','N','D','I','S','P','L','Y',
     3                 'D','U','R','A','N','L','G','E','N','E','R',' ',
     4                 'M','U','T','S','I','N',' '/
C
C     + + + END SPECIFICATIONS + + +
C
      CALL XGTCHR
     O            (CH1,CH2,CH3)
C
      FORE= 7
      BACK= 0
      CALL COLSET (FORE,BACK)
C
      IF (COUNT.EQ.1 .AND. OPN.EQ.1) THEN
C       determine how many lines to display
        SKIP= (NOPNS- 1)/22+ 1
C        SKIP= 1
C        IF (NOPNS.GT.66) THEN
C          SKIP= 4
C        ELSE IF (NOPNS.GT.44) THEN
C          SKIP= 3
C        ELSE IF (NOPNS.GT.22) THEN
C          SKIP= 2
C        END IF
C       determine column width
        COLWID= 68./FLOAT(LAST)
        ALEN = COLWID
        APOS = 0
        ROW  = 0
        IF (ALEN.EQ.0) ALEN= 1
C       clear screen
        CALL SCCLAL
      END IF
C
C     update where we are
      LEN= 3
      CALL INTCHR (OPN,LEN,JUST,J,WBUFF(3))
      CALL INTCHR (NOPNS,LEN,JUST,J,WBUFF(9))
      LEN= 4
      CALL INTCHR (COUNT,LEN,JUST,J,WBUFF(23))
      CALL INTCHR (LAST,LEN,JUST,J,WBUFF(30))
      CALL SCCUMV (I1,I1)
      LEN= 33
      CALL SCPRST (LEN,WBUFF)
      CALL COLSET (FORE,BACK)
C
      IF (MOD(OPN-1,SKIP).EQ.0.OR.OPN.EQ.1) THEN
C       make previous active section inactive
        IF (COUNT.GT.1 .OR. OPN.GT.1) THEN
          CALL ZIPC (ALEN,CH1,OBUFF)
C         WRITE (*,*) 'MOVE CURSOR TO LROW, POS ',LROW,POS
          CALL SCCUMV (ROW,APOS)
          CALL SCPRST (ALEN,OBUFF)
          CALL COLSET (FORE,BACK)
        END IF
C       display this line
        ROW = 3+ (OPN-1)/SKIP
        CALL ZIPC (I80,BK(1),OBUFF)
C       put operation name in buffer
        CALL CHRCHR (I6,OPNAM(1,OMCODE),OBUFF)
C       put operation number in buffer
        LEN = 4
        CALL INTCHR (OPTNO,LEN,JUST,
     O               J,OBUFF(7))
C       put completed portion in buffer
        COL1= (COUNT-1)* COLWID+ SCOL
        LEN = COL1- SCOL
        IF (LEN.GT.66) LEN= 66
        IF (LEN.GT.0) CALL ZIPC (LEN,CH1,OBUFF(SCOL))
C       write beginning of line and completed portion
        CALL SCCUMV (ROW,I1)
        LEN = LENSTR(I80,OBUFF)
        IF (LEN.LT.13) LEN= 12
        CALL SCPRST (LEN,OBUFF)
        APOS= LEN+ 1
C       write active portion
        CALL ZIPC (ALEN,CH2,OBUFF(APOS))
        CALL SCPRST (ALEN,OBUFF(APOS))
        POS = APOS+ ALEN
        LEN = I80- POS
        IF (LEN.GT.0) THEN
          CALL ZIPC (LEN,CH3,OBUFF(POS))
C         write remaining portion
          CALL SCPRST (LEN,OBUFF(POS))
        END IF
        CALL COLSET (FORE,BACK)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   HDMESC
     I                   (MESSFL,SCLU,SGRP,STRING)
C
C     + + + PURPOSE + + +
C     put message with character string to output unit
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       MESSFL,SCLU,SGRP
      CHARACTER*64  STRING
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - message file unit number
C     SCLU   - screen cluster number
C     SGRP   - screen message group
C     STRING - character string to add to message
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I1,CLEN(1),I,J,OFUN
      CHARACTER*1  STRIN1(64)
C
C     + + + EXTERNALS + + +
      EXTERNAL    CVARAR,PMXTFC,SCCUMV
C
C     + + + END SPECIFICATIONS + + +
C
      I1     = 1
      CLEN(1)= 64
C     set cursor in right place
      I = 6
      J = 12
      CALL SCCUMV (I,J)
C
C     convert character string to array
      CALL CVARAR (CLEN(1),STRING,CLEN(1),STRIN1)
C     send message to screen
      OFUN = -1
      CALL PMXTFC (MESSFL,OFUN,SCLU,SGRP,I1,CLEN,STRIN1)
C
      RETURN
      END
C
C
C
      SUBROUTINE   HDMESI
     I                   (IOPT,MESSFL,SCLU,SGRP,I)
C
C     + + + PURPOSE + + +
C     put message with integer to output unit
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       IOPT,MESSFL,SCLU,SGRP,I
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - message file unit number
C     SCLU   - screen cluster number
C     SGRP   - screen message group
C     I      - integer value to add to message
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I1,IVAL(1),K,J,OFUN
C
C     + + + EXTERNALS + + +
      EXTERNAL    PMXTFI,SCCUMV
C
C     + + + END SPECIFICATIONS + + +
C
      I1     = 1
      IVAL(1)= I
C     set cursor in right place
      K = 6
      J = 12
      CALL SCCUMV (K,J)
C
C     send message to screen
      OFUN = -1
      CALL PMXTFI (MESSFL,OFUN,SCLU,SGRP,I1,IVAL)
C
      RETURN
      END
C
C
C
      SUBROUTINE   HDMEST
     I                   (IOPT,MESSFL,SCLU,SGRP)
C
C     + + + PURPOSE + + +
C     put message (text only) to output unit
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       IOPT,MESSFL,SCLU,SGRP
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - message file unit number
C     SCLU   - screen cluster number
C     SGRP   - screen message group
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,J,OFUN
C
C     + + + EXTERNALS + + +
      EXTERNAL    PMXTFT,SCCUMV
C
C     + + + END SPECIFICATIONS + + +
C
C     set cursor in right place
      I = 6
      J = 12
      CALL SCCUMV (I,J)
C     send message to screen
      OFUN = -1
      CALL PMXTFT (MESSFL,OFUN,SCLU,SGRP)
C
      RETURN
      END
C
C
C
      SUBROUTINE   HDMES2
     I                   (IOPT,KTYP,OCCUR)
C
C     + + + PURPOSE + + +
C     put current operation or block to screen during interp
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       IOPT,KTYP,OCCUR
C
C     + + + ARGUMENT DEFINITIONS + + +
C     KTYP    - type of keyword
C     OCCUR   - number of occurances
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,J
      CHARACTER*12 KNAME
C
C     + + + EXTERNALS + + +
      EXTERNAL    GETKNM,ZWRSCR
C
C     + + + END SPECIFICATIONS + + +
C
C     get this keyword
      CALL GETKNM (KTYP,OCCUR,
     O             KNAME)
C     write to screen
      I= 7
      J= 15
      CALL ZWRSCR (KNAME,I,J)
C
      RETURN
      END
C
C
C
      SUBROUTINE   HDMES3
     I                   (IOPT,TABNAM)
C
C     + + + PURPOSE + + +
C     put current operation table name to screen during interp
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       IOPT
      CHARACTER*12  TABNAM
C
C     + + + ARGUMENT DEFINITIONS + + +
C     TABNAM  - table name
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,J
C
C     + + + EXTERNALS + + +
      EXTERNAL    ZWRSCR
C
C     + + + END SPECIFICATIONS + + +
C
C     write to screen
      I= 7
      J= 34
      CALL ZWRSCR (TABNAM,I,J)
C
      RETURN
      END
C
C
C
      SUBROUTINE   HDMESN
     I                   (IOPT,OPTNO)
C
C     + + + PURPOSE + + +
C     put current operation number to screen during interp
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       IOPT,OPTNO
C
C     + + + ARGUMENT DEFINITIONS + + +
C     OPTNO  - operation number
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,J,K,I1,I5
      CHARACTER*1  CHRST1(5)
      CHARACTER*5  COPTNO
C
C     + + + EXTERNALS + + +
      EXTERNAL    ZWRSCR,INTCHR,CARVAR
C
C     + + + END SPECIFICATIONS + + +
C
      I1 = 1
      I5 = 5
      IF (OPTNO.EQ.0) THEN
        COPTNO = '     '
      ELSE
        CALL INTCHR (OPTNO,I5,I1,K,CHRST1)
        CALL CARVAR (I5,CHRST1,I5,COPTNO)
      END IF
C
C     write to screen
      I= 7
      J= 28
      CALL ZWRSCR (COPTNO,I,J)
C
      RETURN
      END
C
C
C
      SUBROUTINE   SCPRST
     I                     (LEN,STR)
C
C     + + + PURPOSE + + +
C     prints a string to the terminal with no cr/lf
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     LEN
      CHARACTER*1 STR(*)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEN    - length of string to write (characters)
C     STR    - characters to write
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   CRFLG,RMFLG
C
C     + + + EXTERNALS + + +
      EXTERNAL  SCPRBF
C
C     + + + END SPECIFICATIONS + + +
C
      CRFLG= 0
      RMFLG= 0
      CALL SCPRBF (LEN,RMFLG,CRFLG,STR)
C
      RETURN
      END
C
C
C
      SUBROUTINE   ZFMTWR
     I                    (STRING)
C
C     + + + PURPOSE + + +
C     write string using a language-dependent format statement
C
C     + + + DUMMY ARGUMENTS + + +
      CHARACTER STRING*(*)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     STRING - character string to write
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I1,I2
      CHARACTER*255 LSTR
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (LSTR1,LSTR)
      CHARACTER*1  LSTR1(255)
C
C     + + + INTRINSICS + + +
      INTRINSIC   LEN
C
C     + + + EXTERNALS + + +
      EXTERNAL   SCPRBF
C
C     + + + END SPECIFICATIONS + + +
C
      I1  = 1
      I2  = 2
      LSTR= STRING
      CALL SCPRBF (LEN(STRING),I1,I2,LSTR1)
C
      RETURN
      END
C
C
C
      SUBROUTINE   ZWRSCR
     I                    (STRING,LINE,COLUMN)
C
C     + + + PURPOSE + + +
C     write a string to specific screen position
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   LINE,COLUMN
      CHARACTER STRING*(*)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     STRING - character string to write
C     LINE   - starting line number to write
C     COLUMN - starting column number to write
C
C     + + + INTRINSICS + + +
      INTRINSIC  LEN
C
C     + + + EXTERNALS + + +
      EXTERNAL   ZFMTWR, SCCUMV
C
C     + + + END SPECIFICATIONS + + +
C
      CALL SCCUMV(LINE,COLUMN)
      CALL ZFMTWR(STRING(1:LEN(STRING)))
C
      RETURN
      END
C
C
C
      SUBROUTINE   SCPRBF
     I                    (LEN,RMFLG,CRFLG,STR)
C
C     + + + PURPOSE + + +
C     builds an output string buffer and writes it out
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     LEN,RMFLG,CRFLG
      CHARACTER*1 STR(*)
C
C     + + + ARGUMENTS DEFINITIONS + + +
C     LEN    - length of string to write
C     RMFLG  - relative movement flag 0-check, 1-yes
C     CRFLG  - carriage return/line feed flag, 0-dont,
C              0 - don't
C              1 - do always
C              2 - do if something in buffer
C     STR    - string to write(or store in buffer)
C
C     + + + SAVES + + +
      INTEGER      STRPOS
      CHARACTER*1  OBUFF(255)
      SAVE         STRPOS,OBUFF
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,LCRFLG
      CHARACTER*1 CTMP
C
C     + + + EXTERNALS + + +
      EXTERNAL    COPYC,SCPRBN
C
C     + + + INTRINSICS + + +
      INTRINSIC   ICHAR
C
C     + + + DATA INITIALIZATIONS + + +
      DATA STRPOS/0/
C
C     + + + OUTPUT FORMATS + + +
2000  FORMAT (A1)
C
C     + + + END SPECIFICATIONS + + +
C
      IF (LEN.GT.0) THEN
C       save the new characters
        CALL COPYC (LEN,STR,OBUFF(STRPOS+1))
        STRPOS= STRPOS+ LEN
      END IF
      IF (STRPOS.GT.0.AND.CRFLG.NE.0.AND.RMFLG.EQ.0) THEN
C       look for relative cursor movement
        I    = 0
 10     CONTINUE
          I= I+ 1
          IF (ICHAR(OBUFF(I)).EQ.27) THEN
C           found esc
            I= I+ 4
            IF (I.LE.LEN) THEN
              CTMP= OBUFF(I)
              IF (CTMP.EQ.'A'.OR.CTMP.EQ.'B'.OR.
     1            CTMP.EQ.'C'.OR.CTMP.EQ.'D') THEN
C               start from where we are
                RMFLG= 1
              END IF
            END IF
          END IF
        IF (I.LT.LEN) GO TO 10
      END IF
C
      IF ((CRFLG.EQ.2.AND.STRPOS.GT.0) .OR. STRPOS.GT.128) THEN
C       time to write out the string before a character input or color change
        LCRFLG= 0
        CALL SCPRBN(STRPOS,RMFLG,LCRFLG,OBUFF)
        STRPOS= 0
      ELSE IF (CRFLG.EQ.1) THEN
C       time to write out the string
        IF (STRPOS.EQ.0) THEN
C         just need a cr/lf
          WRITE (*,2000)
        ELSE
          LCRFLG= 1
          CALL SCPRBN(STRPOS,RMFLG,LCRFLG,OBUFF)
        END IF
        STRPOS= 0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   COLSET
     I                    (FOR,BAC)
C
C     + + + PURPOSE + + +
C     routine to set the foreground and background colors
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    FOR,BAC
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FOR    - color to set foreground to
C     BAC    - color to set background to
C
C     + + + COMMON BLOCK + + +
      INCLUDE 'color.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     LEN,RMFLG,CRFLG
      CHARACTER*1 STR(255)
C
C     + + + EXTERNALS + + +
      EXTERNAL    SCPRBF
C
C     + + + END SPECIFICATIONS + + +
C
C     force write of anything in buffer with old color
      LEN   = 0
      RMFLG = 2
      CRFLG = 2
      STR(1)= ' '
      CALL SCPRBF(LEN,RMFLG,CRFLG,STR)
C
C     change the colors
      FORE= FOR
      BACK= BAC
C
      RETURN
      END
C
C
C
      SUBROUTINE   SYDATM
     O                   ( YR, MO, DY, HR, MN, SC )
C
C     + + + PURPOSE + + +
C     Returns the current date and time.  Calls the system dependent
C     subroutines SYDATE for the date and SYTIME for the time.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   YR, MO, DY, HR, MN, SC
C
C     + + + ARGUMENT DEFINITIONS
C     YR     - year
C     MO     - month
C     DA     - day
C     HR     - hour
C     MN     - minute
C     SC     - second
C
C     + + + EXTERNALS + + +
      EXTERNAL   SYDATE, SYTIME
C
C     + + + END SPECIFICATIONS + + +
C
C     get date
      CALL SYDATE ( YR, MO, DY )
C
C     get time
      CALL SYTIME ( HR, MN, SC )
C
      RETURN
      END
C
C
C
      SUBROUTINE QFDPRS( TSTR, 
     o                   WRKDIR, IFNAME )
C
C     PURPOSE
C     
C     SPLITS A FILENAME AND PATH INTO THE PATH AND THE FILENAME
C
C     DUMMY VARIABLES
C
      CHARACTER*78 TSTR
      CHARACTER*64 WRKDIR, IFNAME
C     
C     LOCAL VARIABLES
C
      INTEGER I
      CHARACTER STRING
C      EXTERNAL ADJUSTL
C
C     FIGURE OUT WHERE THE RIGHTMOST '/' IS
      TSTR = ADJUSTL(TSTR)
      I=78
 100  IF (TSTR(I:I) .NE. '/') THEN
         IF (I .GT. 1) THEN 
            I=I-1
         ELSE
            GOTO 101
         ENDIF
         GOTO 100
      ELSE
         GOTO 101
      ENDIF
 101  CONTINUE
      IF (I .EQ. 1) THEN
         WRKDIR = './'
         IFNAME = TSTR
      ELSE
         WRKDIR = TSTR(1:I)
         IFNAME = TSTR(I+1:LEN(TSTR))
      ENDIF
      RETURN
      END
C
C
C
      SUBROUTINE   SCPRBN
     I                    (LEN,RMFLG,CRFLG,STR)
      INTEGER     LEN,RMFLG,CRFLG
      CHARACTER*1 STR(LEN)
      RETURN
      END
C
C
C
      INTEGER FUNCTION CKUSER ()
C     + + + LOCAL VARIABLES + + +
C      INTEGER     IOPT,IRET
C      INTEGER*1   JTXT(1)
      CKUSER = 0
      RETURN
      END
C
C
C
      SUBROUTINE UPDWIN(IOPT,ILEN,ATXT)
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      IOPT, ILEN
      CHARACTER*1  ATXT(ILEN)
C
C
      RETURN
      END
C
C
C
      SUBROUTINE SDELAY
     I                 (HUNSEC)
C
C     + + + PURPOSE + + +
C     delay specified amount of time
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     HUNSEC
C
      RETURN
      END
C
C
C
      SUBROUTINE   HSPF_INI
     I                      (DELT,OPST,OPND,OPNTAB,
     O                       EXUPFG,EXTWID)
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   DELT,OPST,OPND,OPNTAB(20,OPND),EXUPFG,EXTWID
      RETURN
      END
C
C
C
      SUBROUTINE   EXT_UPDATE
     I                        (WDMSFL,FOPKEY,LOPKEY,OSUPM)
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER WDMSFL(5),FOPKEY,LOPKEY,OSUPM(11,LOPKEY)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - array of WDM file unit numbers, 5th is message file
C     FOPKEY - pointer to first operation in osuper file
C     LOPKEY - pointer to last operation in osuper file
C     OSUPM  - osuper file
C
      RETURN
      END
C
C
C
      SUBROUTINE  LOG_MSG
     I                   (MSG)
      CHARACTER*80 MSG
      RETURN
      END
