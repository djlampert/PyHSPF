C
C
C
      SUBROUTINE   DELUCI
     M                   (LDREC)
C
C     + + + PURPOSE + + +
C     delete a UCI line and assoc info
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      LDREC
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LDREC  - record to delete, returned as previous record
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cucim.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER  TNXT,TPRE
C
C     + + + END SPECIFICATIONS + + +
C
C     save some stuff
      TNXT= NXTREC(LDREC)
      TPRE= PREREC(LDREC)
C
      IF (TPRE .GT. 0) THEN
C       get prev rec straight
        NXTREC(TPRE)= TNXT
      END IF
      IF (TNXT .GT. 0) THEN
C       get next rec straight
        PREREC(TNXT)= TPRE
      END IF
C     get the free rec straight
      PREREC(FREREC)= LDREC
C     get the deleted rec straight
      UCIM(LDREC)   = ' '
      UCINDT(LDREC) = 0
      NXTREC(LDREC) = FREREC
      PREREC(LDREC) = 0
      TYPREC(LDREC) = -3
      FREREC        = LDREC
C     get previous record number
      LDREC= TPRE
C
      RETURN
      END
C
C
C
      SUBROUTINE   DMPKEY
C
C     + + + PURPOSE + + +
C     dump keyword info known about uci
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cucim.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,J,DPOS
C
C     + + + OUTPUT FORMATS + + +
C2000  FORMAT (2X,A12,3I4,2I6)
C2010  FORMAT (22X,I4,2I6)
C
C     + + + END SPECIFICATIONS + + +
C
C      WRITE(99,*) 'Keywords:',NUKWD,NUKWLC
      DO 20 I= 1,NUKWD
        DPOS= UKWDFP(I)
        J   = 1
C        WRITE(99,2000) UKWD(I),UKWDTY(I),UKWDCT(I),
C     $                 J,UKWSRC(DPOS),UKWERC(DPOS)
        IF (UKWPTN(DPOS) .GT. 0) THEN
C         more to dump
 10       CONTINUE
            DPOS= UKWPTN(DPOS)
            J   = J+ 1
C            WRITE(99,2010) J,UKWSRC(DPOS),UKWERC(DPOS)
          IF (UKWPTN(DPOS) .GT. 0) GO TO 10
        END IF
 20   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   DUMPER
     I                   (KEYST,KEYND,MESSU)
C
C     + + + PURPOSE + + +
C     Dump a specified range of the user's control input to messu
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    KEYND,KEYST,MESSU
C
C     + + + ARGUMENT DEFINITIONS + + +
C     KEYST  - starting record numberending record number
C     KEYND  - ending record number
C     MESSU  - ftn unit no. to be used for printout of messages
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cucim.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       KEY,IM2,SCLU,SGRP,INDENT
      CHARACTER*80  UCIBF
      CHARACTER*132 LONGBF
C
C     + + + EXTERNALS + + +
      EXTERNAL     GETUCI,GETIND,PMXTFT
C
C     + + + OUTPUT FORMATS + + +
 2010 FORMAT (5X,A80)
 2020 FORMAT (5X,A132)
C
C     + + + END SPECIFICATIONS + + +
C
      IM2= -2
C
C     dumping message
      SCLU= 210
      SGRP= 10
      CALL PMXTFT (UMSGFL,MESSU,SCLU,SGRP)
C
C     whiledo key<= keynd (force write of 1st record)
      KEY= -KEYST
 10   CONTINUE
        CALL GETUCI (IM2,
     M               KEY,
     O               UCIBF)
        CALL GETIND (KEY,
     O               INDENT)
        IF (INDENT .LE. 0) THEN
C         no indent - write uci buffer as stored
          WRITE (MESSU,2010) UCIBF
        ELSE
C         add indent first
          LONGBF= ' '
          WRITE (LONGBF(INDENT+1:INDENT+80),2010) UCIBF
          WRITE (MESSU,2020) LONGBF
        END IF
      IF (KEY .LT. KEYND) GO TO 10
C     end whiledo
C
      RETURN
      END
C
C
C
      SUBROUTINE   GETEND
     O                   (CEND)
C
C     + + + PURPOSE + + +
C     get the end keyword
C
C     + + + DUMMY ARGUMENTS + + +
      CHARACTER*4 CEND
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CEND    - end keyword
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cucim.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      CEND = UEND
C
      RETURN
      END
C
C
C
      SUBROUTINE   GETKNM
     I                  (KTYP,KRPT,
     O                   KNAME)
C
C     + + + PURPOSE + + +
C     get the next keyword associated with a type
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      KTYP,KRPT
      CHARACTER*12 KNAME
C
C     + + + ARGUMENT DEFINITIONS + + +
C     KTYP    - type of keyword
C     KRPT    - repeat of keyword
C     KNAME   - associated name
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cucim.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,J
C
C     + + + END SPECIFICATIONS + + +
C
      KNAME= ' '
C
      IF (NUKWD .GT. 0) THEN
C       keys to check
        I= 0
        J= 0
 10     CONTINUE
          I= I+ 1
          IF (UKWDTY(I) .EQ. KTYP) THEN
C           right type
            J= J+ 1
            IF (J .EQ. KRPT) THEN
              KNAME= UKWD(I)
              I    = NUKWD
            END IF
          END IF
        IF (I .LT. NUKWD) GOTO 10
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   GETSE
     I                 (ITYPE,IRPT,
     O                  SREC,EREC)
C
C     + + + PURPOSE + + +
C     find start and end position in a uci for a type of key
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   ITYPE,IRPT,SREC,EREC
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ITYPE  - type of key - 1:RUN, 2:GLOBAL, 100:OPN, etc.
C     IRPT   - which occurance of key
C     SREC   - starting record
C     EREC   - ending record
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cucim.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER I,ICNT,PTR,FOUND
C
C     + + + END SPECIFICATIONS + + +
C
      SREC= 0
      EREC= 0
      FOUND= 0
      PTR= 0
C
      IF (NUKWD .GT. 0) THEN
C       keys to check
        ICNT= 0
        I   = 0
 10     CONTINUE
          I = I+ 1
          IF (UKWDTY(I) .EQ. ITYPE) THEN
C           this is the correct type
C
            IF (ITYPE .EQ. 100) THEN
C             operation-type keyword
              ICNT= ICNT+ 1
              IF (IRPT .EQ. ICNT) THEN
C               found correct repetition
                FOUND= 1
C               set pointer
                PTR= UKWDFP(I)
              END IF
            ELSE
C             other keyword type
              FOUND= 1
              IF (IRPT .LE. UKWDCT(I)) THEN
C               keyword occurs in file enough times
C
C               set pointer to first occurrence
                PTR= UKWDFP(I)
C
                IF (IRPT .GT. 1) THEN
C                 count through repeat occurrences as necessary
C                 to get pointer to desired occurrence
                  DO 20 ICNT = 1, IRPT- 1
                    PTR= UKWPTN(PTR)
 20               CONTINUE
                END IF
              END IF
            END IF
C
            IF (PTR .GT. 0) THEN
C             get start and records using pointer
              SREC= UKWSRC(PTR)
              EREC= UKWERC(PTR)
            END IF
          END IF
        IF (I.LT.NUKWD .AND. FOUND.EQ.0) GO TO 10
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   GETUCI
     I                  (LTYPRC,
     M                   NREC,
     O                   UCIBUF)
C
C     + + + PURPOSE + + +
C     Retrieve a UCI line
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      LTYPRC,NREC
      CHARACTER*80 UCIBUF
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LTYPRC - type of record: -2:blank line,
C                              -1:comment,
C                               0:unknown,
C                              >0:something useful
C     NREC   - record number to start looking after and record found
C     UCIBUF - UCI line
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   RETCOD
C
C     + + + EXTERNALS + + +
      EXTERNAL  GETUCIEX
C
C     + + + END SPECIFICATIONS + + +
C
      CALL GETUCIEX (LTYPRC,
     M               NREC,
     O               UCIBUF,RETCOD)
C
      RETURN
      END
C
C
C
      SUBROUTINE   GETUCIEX
     I                     (LTYPRC,
     M                      NREC,
     O                      UCIBUF,RETCOD)
C
C     + + + PURPOSE + + +
C     Retrieve a UCI line, and return type of record found
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      LTYPRC,NREC,RETCOD
      CHARACTER*80 UCIBUF
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LTYPRC - type of record: -2:blank line,
C                              -1:comment,
C                               0:unknown,
C                              >0:something useful
C     NREC   - record number to start looking after and record found
C     UCIBUF - UCI line
C     RETCOD - type of record found
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cucim.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   DONFG
C
C     + + + END SPECIFICATIONS + + +
C
      DONFG = 0
      IF (NREC .GT. 0) THEN
C       start with next record
        NREC = NXTREC(NREC)
      ELSE
C       start with this record
        NREC = -NREC
      END IF
C
      IF (NREC .GT. 0) THEN
C       something to look thru
 10     CONTINUE
C         set next record
          IF (TYPREC(NREC) .GE. LTYPRC) THEN
C           a match
            UCIBUF= UCIM(NREC)
            RETCOD= TYPREC(NREC)
            DONFG = 1
          ELSE
C           not a match, look at next
            NREC= NXTREC(NREC)
          END IF
        IF (DONFG.EQ.0 .AND. NREC.GT.0) GO TO 10
      END IF
C
      IF (NREC .EQ. 0) THEN
C       didnt find record match
        UCIBUF= ' '
        RETCOD= 0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   GETIND
     M                    (NREC,
     O                     INDENT)
C
C     + + + PURPOSE + + +
C     Retrieve the number of spaces to indent a UCI line.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      NREC,INDENT
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NREC   - record number
C     INDENT - number of spaces to indent line
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cucim.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      IF (NREC .GT. 0) THEN
C       valid record - return indent
        INDENT= UCINDT(NREC)
      ELSE
C       invalid record - no indent
        INDENT= 0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   INIUCI
     I                   (MSGFL,MESSU,
     O                    CEND)
C
C     + + + PURPOSE + + +
C     Initialize in memory uci buffer
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MSGFL,MESSU
      CHARACTER*4 CEND
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MSGFL  - unit number of message file containing info needed
C     MESSU  - unit number of output file
C     CEND   - block delimiter string
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cucim.inc'
C
C     + + + LOCAL VARIABLES + ++
      INTEGER     I,SCLU,SGRP,CLEN
      CHARACTER*1 CEND1(4)
C
C     + + + EXTERNALS + + +
      EXTERNAL    WMSGTT, INIKEY
C
C     + + + OUTPUT FORMATS + + +
2000  FORMAT(4A1)
C
C     + + + END SPECIFICATIONS + + +
C
C     save unit numbers
      UMSGFL= MSGFL
      UMESSU= MESSU
C     assume lots of output
      UOUTLV= 10
C     records in uci, dummy first rec
      NXTREC(1)= 0
      PREREC(1)= 0
      TYPREC(1)= -2
      UCIM(1)  = ' '
      UCINDT(1)= 0
C     the rest are free
      FREREC= 2
      DO 10 I = 2,MXUCI
        NXTREC(I)= I+ 1
        PREREC(I)= I- 1
C       unknown
        TYPREC(I)= -3
C       no indent
        UCINDT(I)= 0
 10   CONTINUE
C
C     keywords
      CALL INIKEY
C
C     end string
      SCLU  = 210
      SGRP  = 1
      CLEN  = 4
      CALL WMSGTT (MSGFL,SCLU,SGRP,SGRP,
     M             CLEN,
     O             CEND1,I)
      WRITE(UEND,2000) CEND1
      CEND= UEND
C
      RETURN
      END
C
C
C
      SUBROUTINE   INIKEY
C
C     + + + PURPOSE + + +
C     Initialize keyword locations
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cucim.inc'
C
C     + + + END SPECIFICATIONS + + +
C
C     no keywords
      NUKWD = 0
      NUKWLC= 0
C
      RETURN
      END
C
C
C
      SUBROUTINE   KEYUCI
     I                   (NKWD,LKWD,PKWD,IKWD,IRPT,
     I                    KWDLIB,KWDDIM,KWDTYP,
     M                    ECNT,
     O                    KCNT)
C
C     + + + PURPOSE + + +
C     Look for keywords in a UCI file, save results with uci
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     NKWD,LKWD,PKWD,IKWD,IRPT,KWDDIM(NKWD),KWDTYP(NKWD),
     $            ECNT,KCNT,RETCOD
      CHARACTER*1 KWDLIB(*)
C
C     + + + ARGUEMENT DEFINITIONS + + +
C     NKWD   - number of keywords to look for
C     LKWD   - length of keywords
C     PKWD   - offset to position of keyword on input
C     IKWD   - type of base keyword
C     IRPT   - which start at which copy of keyword
C     KWDLIB - keyword values
C     KWDDIM - max number of keyword occurences allowed
C     KWDTYP - type of each keyword
C     ECNT   - errors found count
C     KCNT   - keyword found count
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cucim.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      NREC,LPOS,RPOS,SCLU,SGRP,LOUT,LMOV,I,EREC,
     #             OPNREC,OPNKWD,KWDNO,ENDFLG,I4,I1,I0
      CHARACTER*80 UCIBUF
      CHARACTER*1  TXT1(20),COUT(12),CKEY(12),BLNK(1),CEND1(4)
C
C     + + + EQUIVALENCE + + +
      EQUIVALENCE (CEND1,CEND)
      CHARACTER*4  CEND
C
C     + + + FUNCTIONS + + +
      INTEGER      CHKSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL     OMSG,OMSTC,OMSTI
      EXTERNAL     GETSE,GETEND,ZIPC,GETUCI,CHKSTR,PUTKWD,CHRCHR
C
C     + + + DATA INITIALIZATIONS + + +
      DATA BLNK/' '/
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (80A1)
C
C     + + + OUTPUT FORMATS + + +
 2010 FORMAT (' FOUND ',A)
C
C     + + + END SPECIFICATIONS + + +
C
C     end keyword
      CALL GETEND(CEND)
C     no keywords found yet
      KCNT= 0
C
      I0= 0
      I1= 1
      I4= 4
C
      IF (IKWD .EQ. 0) THEN
C       start looking at first record
        NREC= 1
        EREC= 0
      ELSE
C       start looking where base is
        CALL GETSE(IKWD,IRPT,
     O             NREC,EREC)
      END IF
C
      LOUT  = 12
      CALL ZIPC (LOUT,BLNK,COUT)
      CALL ZIPC (LOUT,BLNK,CKEY)
      IF (LKWD .LT. LOUT) THEN
C       just update whats needed
        LMOV = LKWD
      ELSE
C       truncate
        LMOV = LOUT
      END IF
C
C     cluster for problem messages
      SCLU  = 210
C     no keyword open
      OPNKWD= 0
C     where to look for keyword in uci
      LPOS  = PKWD + 1
      RPOS  = PKWD + LKWD
C
 10   CONTINUE
C       get next non comment or blank record
        CALL GETUCI (I0,
     M               NREC,
     O               UCIBUF)
        READ (UCIBUF(LPOS:RPOS),1000) (TXT1(I),I=1,LKWD)
C       see if this a wanted keyword
        KWDNO= CHKSTR(LKWD,NKWD,TXT1,KWDLIB)
C
        IF (KWDNO .GT. 0) THEN
C         recognized, save the name
          I = (KWDNO-1)* LKWD + 1
          CALL CHRCHR(LMOV,KWDLIB(I),CKEY)
          IF (UMESSU .GT. 0) THEN
            IF (IKWD.LT.100 .OR. UOUTLV.GT.4) THEN
C             echo it
              WRITE (UMESSU,2010) UCIBUF(LPOS:RPOS)
            END IF
          END IF
          IF (OPNKWD .GT. 0) THEN
C           error - two data set headings found without an end between
            CALL OMSTC(LOUT,CKEY)
            I = (OPNKWD-1)* LKWD + 1
            CALL CHRCHR(LMOV,KWDLIB(I),COUT)
            CALL OMSTC(LOUT,COUT)
            SGRP= 2
            CALL OMSG (UMESSU,UMSGFL,SCLU,SGRP,
     M                 ECNT)
          END IF
C         ok - remember the key to this line
          OPNKWD= KWDNO
          OPNREC= NREC
        ELSE
C         unrecognized, check for end
          ENDFLG = CHKSTR(I4,I1,TXT1,CEND1)
          IF (ENDFLG .EQ. 1) THEN
C           found an end delimiter
            READ(UCIBUF(LPOS+I4:RPOS+I4),1000) (TXT1(I),I=1,LKWD)
            KWDNO= CHKSTR(LKWD,NKWD,TXT1,KWDLIB)
            IF (KWDNO .GT. 0) THEN
C             recognized
              IF (UMESSU .GT. 0) THEN
                IF (IKWD.LT.100 .OR. UOUTLV.GT.4) THEN
C                 echo it
                  WRITE (UMESSU,2010) UCIBUF(LPOS:RPOS+4)
                END IF
              END IF
C             check if delimiter matches the heading
              IF (OPNKWD .EQ. 0) THEN
C               there was no preceding keyword
                I = (KWDNO-1)* LKWD + 1
                CALL CHRCHR(LMOV,KWDLIB(I),COUT)
                CALL OMSTC(LOUT,COUT)
                SGRP= 5
                CALL OMSG (UMESSU,UMSGFL,SCLU,SGRP,
     M                     ECNT)
              ELSE IF (KWDNO .NE. OPNKWD) THEN
C               end doesn't match heading
                I = (KWDNO-1)* LKWD + 1
                CALL CHRCHR(LMOV,KWDLIB(I),COUT)
                CALL OMSTC(LOUT,COUT)
                CALL OMSTC(LOUT,CKEY)
                SGRP= 6
                CALL OMSG (UMESSU,UMSGFL,SCLU,SGRP,
     M                     ECNT)
              ELSE
C               complete
                CALL PUTKWD (KWDDIM(KWDNO),KWDTYP(KWDNO),
     I                       CKEY,OPNREC,NREC,
     O                       RETCOD)
                IF (RETCOD .NE. 0) THEN
C                 too many occurances
                  I = (KWDNO-1)* LKWD + 1
                  CALL CHRCHR(LMOV,KWDLIB(I),COUT)
                  CALL OMSTC(LOUT,COUT)
                  CALL OMSTI(KWDDIM(KWDNO))
                  SGRP= 3
                  CALL OMSG (UMESSU,UMSGFL,SCLU,SGRP,
     M                       ECNT)
                END IF
                OPNKWD= 0
                KCNT  = KCNT+ 1
              END IF
            END IF
          ELSE
C           line is neither a data set heading or delimiter
          END IF
        END IF
      IF (NREC .NE. EREC) GO TO 10
C
      IF (OPNKWD .GT. 0) THEN
C       end of block was encountered before the end keyword was found
        CALL OMSTC(LOUT,CKEY)
        SGRP= 7
        CALL OMSG (UMESSU,UMSGFL,SCLU,SGRP,
     M             ECNT)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PUTKWD
     I                   (KWDDIM,KWDTYP,KNAM1,SREC,EREC,
     O                    RETCOD)
C
C     + + + PURPOSE + + +
C     save info about where a keyword was found
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     KWDDIM,KWDTYP,SREC,EREC,RETCOD
      CHARACTER*1 KNAM1(12)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     KWDDIM - max number of occurances allowed
C     KWDTYP - type of keyword
C     KNAM   - name of keyword
C     SREC   - start rec of block
C     EREC   - end rec of block
C     RETCOD - return code - 0:added ok, 1:too many
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cucim.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       NEWFLG,I,NXTPOS
      CHARACTER*12  KNAME
C
C     + + + OUTPUT FORMATS + + +
2000  FORMAT(12A1)
C
C     + + + END SPECIFCATIONS + + +
C
C     save name
      WRITE(KNAME,2000) KNAM1
C     record type
      TYPREC(SREC)= KWDTYP
C
      RETCOD= 0
C     assume a new keyword
      NEWFLG= 1
C
      IF (NUKWD .GT. 0) THEN
C       check if not new
        I= 0
 10     CONTINUE
          I= I+ 1
          IF (KNAME.EQ.UKWD(I) .AND. KWDTYP.EQ.UKWDTY(I)) THEN
C           already have this one
            NEWFLG= 0
            NXTPOS= UKWDFP(I)
 20         CONTINUE
              IF (UKWPTN(NXTPOS) .EQ. 0) THEN
C               at the end of the chain
                IF (UKWDCT(I) .LT. KWDDIM) THEN
C                 room for another
                  UKWDCT(I) = UKWDCT(I)+ 1
                  NUKWLC        = NUKWLC+ 1
                  UKWPTN(NXTPOS)= NUKWLC
                  UKWSRC(NUKWLC)= SREC
                  UKWERC(NUKWLC)= EREC
                  UKWPTN(NUKWLC)= 0
                ELSE
C                 no room for this one
                  RETCOD= 1
                END IF
C               force exit of this inner loop
                NXTPOS = 0
              ELSE
                NXTPOS = UKWPTN(NXTPOS)
              END IF
            IF (NXTPOS .GT. 0) GOTO 20
          END IF
        IF (NEWFLG.EQ.1 .AND. I.LT.NUKWD) GO TO 10
      END IF
C
      IF (NEWFLG .EQ. 1) THEN
C       a new keyword
        NUKWD         = NUKWD+ 1
        UKWD(NUKWD)   = KNAME
        UKWDCT(NUKWD) = 1
        UKWDTY(NUKWD) = KWDTYP
        NUKWLC        = NUKWLC+ 1
        UKWDFP(NUKWD) = NUKWLC
        UKWSRC(NUKWLC)= SREC
        UKWERC(NUKWLC)= EREC
        UKWPTN(NUKWLC)= 0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   DELKWD
     I                   (KWDTYP)
C
C     + + + PURPOSE + + +
C     delete info about where a keyword was found because this
C     keyword no longer exists
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     KWDTYP
C
C     + + + ARGUMENT DEFINITIONS + + +
C     KWDTYP - type of keyword
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cucim.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I,FOUND
C
C     + + + END SPECIFCATIONS + + +
C
      IF (KWDTYP.LT.1000) THEN
C       look for this keyword
        I = 0
        FOUND = 0
 10     CONTINUE
          I = I + 1
          IF (UKWDTY(I).EQ.KWDTYP) THEN
C           this is the keyword to get rid of
            FOUND = 1
            UKWDTY(I) = 0
          END IF
        IF (FOUND.EQ.0) GO TO 10
      ELSE
C       look for any table of this kwdtyp
        I = 0
 20     CONTINUE
          I = I + 1
          IF (UKWDTY(I).GT.KWDTYP .AND. UKWDTY(I).LT.KWDTYP+1000) THEN
C           get rid of this keyword
            UKWDTY(I) = 0
          END IF
        IF (I.LT.MXSKWD) GO TO 20
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PUTOLV
     I                   (OUTLEV)
C
C     + + + PURPOSE + + +
C     Save output level
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      OUTLEV
C
C     + + + ARGUMENT DEFINITIONS + + +
C     OUTLEV - output level
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cucim.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      UOUTLV= OUTLEV
C
      RETURN
      END
C
C
C
      SUBROUTINE   PUTUCI
     I                   (UCIBUF,LTYPRC,LPRERC)
C
C     + + + PURPOSE + + +
C     Save a UCI line and assoc info
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      LTYPRC,LPRERC
      CHARACTER*80 UCIBUF
C
C     + + + ARGUMENT DEFINITIONS + + +
C     UCIBUF - UCI record
C     LTYPRC - type of record: -2:blank line,
C                              -1:comment,
C                               0:unknown,
C                              >0:something useful
C     LPRERC - record to write this one after
C
C     + + + LOCAL VARIABLES + + +
      INTEGER  I
C
C     + + + EXTERNALS + + +
      EXTERNAL   PUTUCX
C
C     + + + END SPECIFICATIONS + + +
C
      I= -1
      CALL PUTUCX (UCIBUF,LTYPRC,LPRERC,I)
C
      RETURN
      END
C
C
C
      SUBROUTINE   PUTUCX
     I                   (UCIBUF,LTYPRC,LPRERC,INDENT)
C
C     + + + PURPOSE + + +
C     Save a UCI line and assoc info
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      LTYPRC,LPRERC,INDENT
      CHARACTER*80 UCIBUF
C
C     + + + ARGUMENT DEFINITIONS + + +
C     UCIBUF - UCI record
C     LTYPRC - type of record: -2:blank line,
C                              -1:comment,
C                               0:unknown,
C                              >0:something useful
C     LPRERC - record to write this one after
C     INDENT - number of indented spaces removed from special action line
C              or -1 if no change is to be made
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cucim.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER  TNXT,TPRE,TCUR
C
C     + + + END SPECIFICATIONS + + +
C
C     save some stuff
      TNXT= NXTREC(LPRERC)
      TPRE= LPRERC
      TCUR= FREREC
C
      IF (TPRE .GT. 0) THEN
C       get prev rec straight
        NXTREC(TPRE)= TCUR
      END IF
      IF (TNXT .GT. 0) THEN
C       get next rec straight
        PREREC(TNXT)= TCUR
      END IF
C     get the free rec straight
      FREREC        = NXTREC(FREREC)
      PREREC(FREREC)= 0
C     get the new rec straight
      UCIM(TCUR)    = UCIBUF
      NXTREC(TCUR)  = TNXT
      PREREC(TCUR)  = TPRE
      TYPREC(TCUR)  = LTYPRC
      IF (INDENT .GE. 0) THEN
C       update indentation
        UCINDT(TCUR)  = INDENT
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PREUCI
     M                   (LKEY)
C
C     + + + PURPOSE + + +
C     given a key, return the key of the previous uci line
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      LKEY
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LKEY   - uci key
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cucim.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      LKEY = PREREC(LKEY)
C
      RETURN
      END
C
C
C
      SUBROUTINE   REPUCI
     I                   (LREC,UCIBUF)
C
C     + + + PURPOSE + + +
C     replace a UCI line, no change to assoc info
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      LREC
      CHARACTER*80 UCIBUF
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LREC   - record to replace
C     UCIBUF - new record
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cucim.inc'
C
C     + + + END SPECIFICATIONS + + +
C
C     replace record
      UCIM(LREC)= UCIBUF
C
      RETURN
      END
C
C
C
      SUBROUTINE   REPUCX
     I                   (LREC,UCIBUF,INDENT)
C
C     + + + PURPOSE + + +
C     replace a UCI line and its indentation, no change to other info
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      LREC,INDENT
      CHARACTER*80 UCIBUF
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LREC   - record to replace
C     UCIBUF - new record
C     INDENT - number of spaces to indent line
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cucim.inc'
C
C     + + + END SPECIFICATIONS + + +
C
C     replace record
      UCIM(LREC)= UCIBUF
      UCINDT(LREC)= INDENT
C
      RETURN
      END
C
C
C
      SUBROUTINE   COMKEY
     I                   (LREC)
C
C     + + + PURPOSE + + +
C     comment out a UCI line
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      LREC
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LREC   - record to replace
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cucim.inc'
C
C     + + + END SPECIFICATIONS + + +
C
C     comment out record
      TYPREC(LREC)= -1
C
      RETURN
      END
C
C
C
      SUBROUTINE   UCIINP
     I                   (USRFL,MSGFL,MESSU)
C
C     + + + PURPOSE + + +
C     Read a complete data set from the user's control input,
C     identify comments and blank lines, save it in memory
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   USRFL,MSGFL,MESSU
C
C     + + + ARGUMENT DEFINITIONS + + +
C     USRFL  - fortran unit number of file containing hspf input
C     MSGFL  - unit number of message file containing info needed
C     MESSU  - unit number of output file
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       CMTFG,I3,I132,SPACFG,PREREC,TYPREC,BLNKFG,
     $              GLOBFG,DONEFG,I,INDENT,START,NUMMOV
      CHARACTER*1   STARS(3)
      CHARACTER*4   CEND
      CHARACTER*12  GLOKWD,SPAKWD
      CHARACTER*132 TXT
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (TXT,TXT1)
      CHARACTER*1  TXT1(132)
C
C     + + + FUNCTIONS + + +
      INTEGER    CKNBLV,STRFND
C
C     + + + EXTERNAL + + +
      EXTERNAL   CKNBLV,STRFND,INIUCI,ZIPC,PUTUCX
C
C     + + + DATA INITIALIZATIONS + + +
      DATA GLOKWD/'GLOBAL'/
      DATA SPAKWD/'SPEC-ACTIONS'/
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (A132)
C
C     + + + END SPECIFICATIONS + + +
C
      I3= 3
      I132= 132
      CALL ZIPC (I3,'*',STARS)
C
C     initialize in memory uci
      CALL INIUCI (MSGFL,MESSU,
     O             CEND)
C
C     read until end of uci file
      SPACFG= 0
      GLOBFG= 0
      PREREC= 1
 10   CONTINUE
        READ (USRFL,1000,END=100) TXT
C       unknown type
        TYPREC= 0
        INDENT= 0
C       see if this is a blank line
        BLNKFG= CKNBLV (I132,TXT1)
        IF (BLNKFG .EQ. 0) THEN
C         it is
          TYPREC= -2
        ELSE
C         see if this is a comment line
          CMTFG= STRFND (I132,TXT1,I3,STARS)
          IF (CMTFG .GT. 0) THEN
C           it is
            TYPREC= -1
          END IF
        END IF
C
        IF (TYPREC .EQ. 0) THEN
C         check for special actions block
          DONEFG= 0
C
C         if the SPEC-ACTIONS keyword is found outside of the
C         global block and at the start of a line, then all lines
C         until the end of the block will be moved to start at no
C         later than the third character.
C
C         first check to see if we're in global block
          IF ( (GLOBFG .EQ. 0) .AND. (TXT(1:12) .EQ. GLOKWD) ) THEN
C           we are now entering global block - start ignoring spakwd
            GLOBFG= 1
            DONEFG= 1
          ELSE IF ( (GLOBFG .EQ. 1) .AND. (TXT(1:4) .EQ. CEND) .AND.
     $              (TXT(5:16) .EQ. GLOKWD) ) THEN
C           now leaving global block - start looking for spakwd
            GLOBFG= 0
            DONEFG= 1
          ELSE IF (GLOBFG .EQ. 0) THEN
C           look for spakwd
            IF ( (SPACFG .EQ. 0) .AND. (TXT(1:12) .EQ. SPAKWD) ) THEN
C             found start of block
              SPACFG= 1
              DONEFG= 1
            ELSE IF ( (SPACFG .EQ. 1) .AND. (TXT(1:4) .EQ. CEND) .AND.
     $                (TXT(5:16) .EQ. SPAKWD) ) THEN
C             now leaving special actions block
              SPACFG= -1
              DONEFG= 1
            END IF
          END IF
C
          IF ( (DONEFG .EQ. 0) .AND. (SPACFG .EQ. 1) ) THEN
C           this line is inside special action block
C
            START= CKNBLV (I132,TXT1)
            IF (START .GT. 3) THEN
C             move to third character
              INDENT= START- 3
              NUMMOV= 78
              IF (START .GT. 55) THEN
C               move less than full 80 characters
                NUMMOV= 133- START
              END IF
C             move rest of active line
              DO 20 I= 3, NUMMOV+ 2
                TXT1(I)= TXT1(I+ INDENT)
                TXT1(I+ INDENT)= ' '
 20           CONTINUE
              IF (INDENT .GT. 52) THEN
C               set to max
                INDENT= 52
              END IF
            END IF
          END IF
        END IF
C
C       save the record
        CALL PUTUCX (TXT(1:80),TYPREC,PREREC,INDENT)
        PREREC= PREREC+ 1
      GO TO 10
C
C     end of file
 100  CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   DELKWO
     I                   (KWDTYP,KNAME)
C
C     + + + PURPOSE + + +
C     delete info about where a keyword was found because this
C     keyword no longer exists -- special case for an operation
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      KWDTYP
      CHARACTER*12 KNAME
C
C     + + + ARGUMENT DEFINITIONS + + +
C     KWDTYP - type of keyword
C     KNAME  - name of keyword
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cucim.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I,FOUND
C
C     + + + END SPECIFCATIONS + + +
C
C     look for this keyword
      I = 0
      FOUND = 0
 10   CONTINUE
        I = I + 1
        IF (KNAME.EQ.UKWD(I) .AND. KWDTYP.EQ.UKWDTY(I)) THEN
C         this is the keyword to get rid of
          FOUND = 1
          UKWDTY(I) = 0
        END IF
      IF (FOUND.EQ.0) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE   CLOUCI
C
C     + + + PURPOSE + + +
C     close in memory uci file - free up space - dummy for f77
C
C     + + + END SPECIFICATIONS + + +
C
      RETURN
      END
