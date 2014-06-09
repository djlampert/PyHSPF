C
C
C
      SUBROUTINE   ITABLE
     I                    (TABNO,TABSUB,NVALS,UUNITS,
     M                     IVAL)
C
C     + + + PURPOSE + + +
C     Process information normally in table containing integer values
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   NVALS
      INTEGER   TABNO,TABSUB,UUNITS,IVAL(NVALS)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     TABNO  - table id number
C     TABSUB - table subscript (occurance number)
C     NVALS  - number of values in table
C     UUNITS - system of units   1-english, 2-metric
C     IVAL   - values from table
C
C     + + + PARAMETERS + + +
      INCLUDE     'pmxfld.inc'
C
C     + + + COMMON BLOCKS- INTERP2 + + +
      INCLUDE     'crin2.inc'
      INCLUDE     'crin2c.inc'
C
C     + + + SAVE VARIABLES + + +
      INTEGER   TABCOD,TABGRP(4,MAXTTP),WRNCNT
      SAVE      TABCOD,TABGRP,WRNCNT
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      MESSU,MSGFL,ERRFG,HEADCT,NONBLK,
     $             I,J,I0,KEY,KEYND,KEYST,TCLU,SCLU,SGRP,CPOS,LVALS,
     $             INITFG,CONT,IPOS,IFLD,LDEF,LMIN,LMAX,LLEN,LCOL,CLEN,
     $             NFLDS,LFLDS,SCOL(MXFLD),FLEN(MXFLD),APOS(MXFLD),
     $             IMIN(MXFLD),IMAX(MXFLD),IDEF(MXFLD),NMHDRW,RETCOD,
     $             OMCODE,NUMREC,OLEN,SFLD,CNUM,CLNX(1),IOPT
      REAL         RMIN(MXFLD),RMAX(MXFLD),RDEF(MXFLD)
      CHARACTER*1  INBUF1(80),WRFMT(120),HDRBUF(78,10),HEADG(120,4),
     $             FTYP(MXFLD),OBUF1(80)
      CHARACTER*4  C4BUF
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (INBUF1,INBUFF),(OBUF1,OBUFF)
      CHARACTER*80 INBUFF,OBUFF
C
C     + + + FUNCTIONS + + +
      INTEGER      TABREC,CHRINT,LENSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL     TABREC,CHRINT,LENSTR,WMSGTT,WMSGTX,ZIPI
      EXTERNAL     OMSTC,OMSTI,OMSG,GETUCI,PMXTFT,PMXTFC,HDMES3
C
C     + + + INTRINSICS + + +
      INTRINSIC    MOD
C
C     + + + DATA INITIALIZATIONS + + +
      DATA   TABCOD/0/
      DATA   WRNCNT/0/
C
C     + + + INPUT FORMATS + + +
 1010 FORMAT (A4)
 1020 FORMAT (22X,4(4X,I3))
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (A12,I4)
 2010 FORMAT (' ')
 2020 FORMAT (' ',120A1)
C
C     + + + HISTORY + + +
C     7/2004 - pbd, check for valid integers, report error if not
C
C     + + + END SPECIFICATIONS + + +
C
C     file units needed
      MESSU= FILE(1)
      MSGFL= FILE(15)
C
      I0= 0
      SCLU= 218
C
      IF (OUTLEV .GT. 3) THEN
C       processing message
        WRITE (OBUFF,2000) TABNAM(TABNO),TABSUB
        SGRP= 8
        CNUM= 1
        CLNX(1)= 16
        CALL PMXTFC (MSGFL,MESSU,SCLU,SGRP,CNUM,CLNX,OBUF1)
      END IF
      IOPT= 4
      CALL HDMES3 (IOPT,TABNAM(TABNO))
C
C     get type of operation for this table
      OMCODE= OPNTAB(4,OPNO)
C
      TCLU= OMCODE+ 120
      IF (OMCODE .NE. TABCOD) THEN
C       first read addresses for each table, english and metric
        TABCOD= OMCODE
        SGRP= 1
        INITFG= 1
        NUMREC= 0
 10     CONTINUE
          OLEN= 80
          CALL WMSGTT (MSGFL,TCLU,SGRP,INITFG,
     M                 OLEN,
     O                 OBUF1,CONT)
          NUMREC= NUMREC+ 1
          READ (OBUFF,1020) (TABGRP(I,NUMREC),I= 1, 4)
          INITFG= 0
C         loop back if more
        IF (CONT .EQ. 1) GO TO 10
      END IF
C
C     read info for this type of table from message file
      SGRP= TABGRP(UUNITS,TABNO)
C     write (99,*)'TCLU,SGRP',TCLU,SGRP
      CALL WMSGTX (MSGFL,TCLU,SGRP,
     O             NFLDS,SCOL,FLEN,FTYP,APOS,IMIN,IMAX,IDEF,
     O             RMIN,RMAX,RDEF,NMHDRW,HDRBUF,RETCOD)
      IF (FLEN(1) .EQ. 4) THEN
C       1st two fields made smaller for editing in AIDE,
C       adjust their widths and starting columns for all fields
        FLEN(1)= 5
        FLEN(2)= 5
        SCOL(2)= 6
        DO 20 I= 3, NFLDS
          SCOL(I)= SCOL(I)+ 2
 20     CONTINUE
      ELSE IF (FLEN(1) .EQ. 8) THEN
C       1st field made smaller for editing in AIDE,
C       adjust its width and starting columns for all fields
        FLEN(1)= 10
        DO 30 I= 2, NFLDS
          SCOL(I)= SCOL(I)+ 2
 30     CONTINUE
      END IF
C     new style, first data from second field
      SFLD= 2
C
C     output heading and formats
      SGRP= TABGRP(UUNITS+2,TABNO)
      INITFG= 1
      CLEN= 60
      CALL WMSGTT (MSGFL,TCLU,SGRP,INITFG,
     M             CLEN,
     O             WRFMT,CONT)
      HEADCT= 0
      INITFG= 0
C
 40   CONTINUE
C       output header
        HEADCT= HEADCT+ 1
        CLEN= 120
        CALL WMSGTT (MSGFL,TCLU,SGRP,INITFG,
     M               CLEN,
     O               HEADG(1,HEADCT),CONT)
      IF ( (CONT .EQ. 1) .AND. (HEADCT .LT. 4) ) GO TO 40
C
      IF (RESMFG .EQ. 0) THEN
C       not in resume mode, set values to undefined
        I= -999
        CALL ZIPI (NVALS,I,
     O             IVAL)
      END IF
C
C     look for the record in ucifl which contains values to be processed
      J= KYST(TABNO)+ TABSUB- 1
      KEYST= TABKST(J)
      KEYND= TABKND(J)
C
      KEY= -TABREC (KEYST,KEYND,TABNM1(1,TABNO),
     $              TABSUB,OPTNO,
     $              MESSU,MSGFL,
     $              ECOUNT)
      IF (KEY .EQ. 0) THEN
C       no uci info info for this table
        INBUFF= ' '
        IF (OUTLEV .GT. 3) THEN
C         indicate that fact
          SGRP= 4
          CALL PMXTFT (MSGFL,MESSU,SCLU,SGRP)
        END IF
        IF (RESMFG .EQ. 1) THEN
C         operating in resume mode, so existing values will be kept
          IF (OUTLEV .GT. 3) THEN
C           indicate that
            SGRP= 5
            CALL PMXTFT (MSGFL,MESSU,SCLU,SGRP)
          END IF
        ELSE
C         will use defaults
          IF (OUTLEV .GT. 3) THEN
C           indicate that
            SGRP= 6
            CALL PMXTFT (MSGFL,MESSU,SCLU,SGRP)
          END IF
        END IF
      END IF
C
C     loop to check values and use defaults as needed
      IPOS= 0
C     loop back here for mult records
 50   CONTINUE
        IF (KEY .NE. 0) THEN
C         read data supplied by user from uci file
          CALL GETUCI (I0,
     M                 KEY,
     O                 INBUFF)
        END IF
C
C       values from one record
        LVALS= 0
        LFLDS= SFLD- 1
        DO 60 I= SFLD,NFLDS
          IF (FTYP(I) .EQ. 'I') THEN
C           integer value
            LVALS= LVALS+ 1
          ELSE IF (FTYP(I) .EQ. 'C') THEN
C           character value
            LLEN= FLEN(I)
            CLEN= LLEN/4
            IF (MOD (LLEN,4) .GT. 0) THEN
C             extra characters
              CLEN= CLEN+ 1
            END IF
            LVALS= LVALS+ CLEN
          END IF
          IF (IPOS+ LVALS .LE. NVALS) THEN
C           this field needed
            LFLDS= LFLDS+ 1
          END IF
 60     CONTINUE
C
        DO 80 IFLD= SFLD,LFLDS
C         stuff for this field
          LCOL= SCOL(IFLD)
          LLEN= FLEN(IFLD)
          LMIN= IMIN(APOS(IFLD))
          LMAX= IMAX(APOS(IFLD))
          LDEF= IDEF(APOS(IFLD))
C
          IF (FTYP(IFLD) .EQ. 'I') THEN
C           integer field
            IPOS= IPOS+ 1
            I= LENSTR (LLEN,INBUF1(LCOL))
            IF (I .LE. 0) THEN
C             supply default value
              IVAL(IPOS)= LDEF
            ELSE
C             make sure string converts to valid integer
              ERRFG= 0
              IF (I.GE.3) THEN
                NONBLK = 0
                DO 85 J= 1,I-1
                  IF (INBUF1(LCOL+J-1).NE.' ') THEN
                    NONBLK = J
                  ELSE IF (NONBLK.GT.0) THEN
                    ERRFG= 2
                  END IF
 85             CONTINUE
              END IF

              IF (ERRFG .EQ. 2) THEN
C               warning, cant have blank between 2 non-blank chars
                J= 6
                CALL OMSTC (J,OPTYP1)
                CALL OMSTI (OPTNO)
                J= 12
                CALL OMSTC (J,TABNM1(1,TABNO))
                CALL OMSTI (TABSUB)
                CALL OMSTI (IFLD-1)
                J= 5
                CALL OMSTC (J,INBUF1(LCOL))
                SGRP= 13
                CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                     WRNCNT)
              END IF
C
C             get integer value from user input
              IVAL(IPOS)= CHRINT (I,INBUF1(LCOL))
C
C             check for range
              IF (LMIN .NE. -999) THEN
C               check against minimum
                IF (IVAL(IPOS) .LT. LMIN) THEN
C                 less than min allowed
                  ERRFG= 1
                END IF
              END IF
C
              IF (LMAX .NE. -999) THEN
C               check against maximum
                IF (IVAL(IPOS) .GT. LMAX) THEN
C                 greated than max allowed
                  ERRFG= 1
                END IF
              END IF
C
              IF ( (ERRFG .EQ. 1) .AND. (IVAL(IPOS) .EQ. 0) .AND.
     $             (LDEF .NE. -999) ) THEN
C               value was zero, which is out of range - use default
                IVAL(IPOS)= LDEF
                ERRFG= 0
              END IF
C
              IF (ERRFG .EQ. 1) THEN
C               error - outside of valid range
                I= 6
                CALL OMSTC (I,OPTYP1)
                CALL OMSTI (OPTNO)
                I= 12
                CALL OMSTC (I,TABNM1(1,TABNO))
                CALL OMSTI (TABSUB)
                CALL OMSTI (IFLD-1)
                CALL OMSTI (IVAL(IPOS))
                CALL OMSTI (LMIN)
                CALL OMSTI (LMAX)
                CALL OMSTI (LDEF)
                SGRP= 1
                CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                     ECOUNT)
C               set to default value
                IVAL(IPOS)= LDEF
              END IF
            END IF
          ELSE
C           character field, default may apply to many fields
            CLEN= LLEN/4
            IF (MOD (LLEN,4) .GT. 0) THEN
C             extra characters
              CLEN= CLEN+ 1
            END IF
            IF ( (RESMFG .EQ. 0) .OR. (KEY .NE. 0) ) THEN
C             get the characters
              DO 70 J= 1, CLEN
C               put character values into buffer
                CPOS= LCOL+ 4*(J-1)
                C4BUF= INBUFF(CPOS:CPOS+3)
                IPOS= IPOS+ 1
                READ (C4BUF,1010) IVAL(IPOS)
 70           CONTINUE
            END IF
          END IF
C
          IF ( (ERRFG .EQ. 0) .AND. (IVAL(IPOS) .EQ. -999) ) THEN
C           need default but none available
            I= 6
            CALL OMSTC (I,OPTYP1)
            CALL OMSTI (OPTNO)
            I= 12
            CALL OMSTC (I,TABNM1(1,TABNO))
            CALL OMSTI (TABSUB)
            CALL OMSTI (IFLD-1)
            SGRP= 2
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 ECOUNT)
          END IF
C
C       end of loop on fields
 80     CONTINUE
C
C     end of loop on multiple lines
      IF (IPOS .LT. NVALS) GO TO 50
C
      IF (OUTLEV .GT. 2) THEN
C       report values that were read in, supplied by default, or re-used
        WRITE (MESSU,2010)
        CLEN= 120
        DO 90 I= 1, HEADCT
          WRITE (MESSU,2020) (HEADG(J,I),J=1,LENSTR(CLEN,HEADG(1,I)))
 90     CONTINUE
        WRITE (MESSU,WRFMT) IVAL
      END IF
C
      IF (OUTLEV .GT. 3) THEN
C       done table message
        SGRP= 7
        CALL PMXTFT (MSGFL,MESSU,SCLU,SGRP)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   RTABLE
     I                    (TABNO,TABSUB,NVALS,UUNITS,
     M                     RVAL)
C
C     + + + PURPOSE + + +
C     Process information normally in table containing real values
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     NVALS,TABNO,TABSUB,UUNITS
      REAL        RVAL(NVALS)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     TABNO  - table id number
C     TABSUB - table subscript (occurance number)
C     NVALS  - number of values in table
C     UUNITS - system of units   1-english, 2-metric
C     RVAL   - values from table
C
C     + + + PARAMETERS + + +
      INCLUDE     'pmxfld.inc'
C
C     + + + COMMON BLOCKS- INTERP2 + + +
      INCLUDE     'crin2.inc'
      INCLUDE     'crin2c.inc'
C
C     + + + PARAMETERS + + +
      INTEGER    MXCNV
      PARAMETER (MXCNV=100)
C
C     + + + SAVE VARIABLES + + +
      INTEGER CNVTYP,CNVCNT,CNVTAB(MXCNV),CNVFLD(MXCNV),TABCOD,
     $        TABGRP(4,MAXTTP),WRNCNT
      REAL    CNVADD(MXCNV),CNVMLT(MXCNV)
C
      SAVE    CNVTYP,CNVCNT,CNVTAB,CNVFLD,CNVADD,CNVMLT,TABCOD,
     $        TABGRP,WRNCNT
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      MESSU,MSGFL,ERRFG,HEADCT,OLEN,I10,I0,NONBLK,
     $             I,J,KEY,KEYND,KEYST,TCLU,SCLU,SGRP,CPOS,LVALS,
     $             INITFG,CONT,IPOS,IFLD,LLEN,LCOL,CLEN,
     $             NFLDS,LFLDS,SCOL(MXFLD),FLEN(MXFLD),APOS(MXFLD),
     $             IMIN(MXFLD),IMAX(MXFLD),IDEF(MXFLD),NMHDRW,RETCOD,
     $             OMCODE,NUMREC,DEFFG,EPOS,EWID,SFLD,CNUM,CLNX(1),IOPT,
     $             VALSET
      REAL         RMIN(MXFLD),RMAX(MXFLD),RDEF(MXFLD),LDEF,LMIN,LMAX,
     $             L999,RTMP,ADD(MXFLD),MULT(MXFLD)
      CHARACTER*1  INBUF1(80),WRFMT(120),HDRBUF(78,10),HEADG(120,4),
     $             FTYP(MXFLD),EBUF1(132),OBUF1(80)
      CHARACTER*4  C4BUF
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE   (INBUF1,INBUFF),(OBUFF,OBUF1),(EBUF1,EBUFF)
      CHARACTER*80   INBUFF,OBUFF
      CHARACTER*132  EBUFF
C
C     + + + FUNCTIONS + + +
      INTEGER        TABREC,LENSTR
      REAL           CHRDEC
C
C     + + + INTRINSICS + + +
      INTRINSIC      MOD,ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL       TABREC,CHRDEC,LENSTR,WMSGTT,WMSGTX,ZIPR,OMSTC
      EXTERNAL       HDMES3
      EXTERNAL       OMSTI,OMSTR,OMSG,GETUCI,RHTSTR,COPYC,PMXTFT,PMXTFC
      EXTERNAL       GETVEC
C
C     + + + DATA INITIALIZATIONS + + +
      DATA   CNVTYP/0/
      DATA   TABCOD/0/
      DATA   WRNCNT/0/
C
C     + + + INPUT FORMATS + + +
 1010 FORMAT (A4)
 1020 FORMAT (2I3,2F10.0)
 1030 FORMAT (22X,4(4X,I3))
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (A12,I4)
 2010 FORMAT (' ')
 2020 FORMAT (' ',120A1)
 2030 FORMAT (1PG10.3)
 2040 FORMAT (1X,132A1)
C
C     + + + HISTORY + + +
C     7/2004 - pbd, add write of values as interpreted
C     7/2004 - pbd, check for valid reals, report error if not
C
C     + + + END SPECIFICATIONS + + +
C
C     file units needed
      MESSU= FILE(1)
      MSGFL= FILE(15)
C
      I0= 0
      I10= 10
      SCLU= 218
C
      IF (OUTLEV .GT. 3) THEN
C       processing message
        WRITE (OBUFF,2000) TABNAM(TABNO),TABSUB
        SGRP= 8
        CNUM= 1
        CLNX(1)= 16
        CALL PMXTFC (MSGFL,MESSU,SCLU,SGRP,CNUM,CLNX,OBUF1)
      END IF
      IOPT = 4
      CALL HDMES3 (IOPT,TABNAM(TABNO))
C
C     get type of operation for this table
      OMCODE= OPNTAB(4,OPNO)
C
      TCLU= OMCODE+ 120
      IF (OMCODE .NE. TABCOD) THEN
C       first read addresses for each table, english and metric
        TABCOD= OMCODE
        SGRP= 1
        INITFG= 1
        NUMREC= 0
 10     CONTINUE
          OLEN= 80
          CALL WMSGTT (MSGFL,TCLU,SGRP,INITFG,
     M                 OLEN,
     O                 OBUF1,CONT)
          NUMREC= NUMREC+ 1
          READ (OBUFF,1030) (TABGRP(I,NUMREC),I= 1, 4)
          INITFG= 0
C         loop back if more
        IF (CONT .EQ. 1) GO TO 10
      END IF
C
      IF ( (OMCODE .LE. 3) .AND. (OMCODE .NE. CNVTYP) ) THEN
C       may need to change to internal units with PERLND,IMPLND,RCHRES
C       get info about what needs changes
        SGRP= 71
        INITFG= 1
        CNVCNT= 0
        CNVTYP= OMCODE
 20     CONTINUE
          OLEN= 80
          CALL WMSGTT (MSGFL,TCLU,SGRP,INITFG,
     M                 OLEN,
     O                 INBUF1,CONT)
          CNVCNT= CNVCNT+ 1
          READ (INBUFF,1020) CNVTAB(CNVCNT),CNVFLD(CNVCNT),
     $                       CNVADD(CNVCNT),CNVMLT(CNVCNT)
          INITFG= 0
C         loop back if more
        IF (CONT .EQ. 1) GO TO 20
      END IF
C     default add and mult
      RTMP= 0.0
      CALL ZIPR (NVALS,RTMP,
     O           ADD)
      RTMP= 1.0
      CALL ZIPR (NVALS,RTMP,
     O           MULT)
C     adjust as needed
      DO 30 I= 1, CNVCNT
        IF (CNVTAB(I) .EQ. TABGRP(UUNITS,TABNO)) THEN
C         conversion applies to this table
          IF (CNVFLD(I) .EQ. 0) THEN
C           all fields
            CALL ZIPR (NVALS,CNVADD(I),
     O                 ADD)
            CALL ZIPR (NVALS,CNVMLT(I),
     O                 MULT)
          ELSE
C           single field
            ADD(CNVFLD(I))= CNVADD(I)
            MULT(CNVFLD(I))= CNVMLT(I)
          END IF
        END IF
 30   CONTINUE
C
C     constant for no min, max, or default
      L999= 999.0
C
C     read info for this type of table from message file
      SGRP= TABGRP(UUNITS,TABNO)
C     write (99,*)'TCLU,SGRP',TCLU,SGRP
      CALL WMSGTX (MSGFL,TCLU,SGRP,
     O             NFLDS,SCOL,FLEN,FTYP,APOS,IMIN,IMAX,IDEF,
     O             RMIN,RMAX,RDEF,NMHDRW,HDRBUF,RETCOD)
      IF (FLEN(1) .EQ. 4) THEN
C       1st two fields made smaller for editing in AIDE,
C       adjust their widths and starting columns for all fields
        FLEN(1)= 5
        FLEN(2)= 5
        SCOL(2)= 6
        DO 40 I= 3, NFLDS
          SCOL(I)= SCOL(I)+ 2
 40     CONTINUE
      ELSE IF (FLEN(1) .EQ. 8) THEN
C       1st field made smaller for editing in AIDE,
C       adjust its width and starting columns for all fields
        FLEN(1)= 10
        DO 50 I= 2, NFLDS
          SCOL(I)= SCOL(I)+ 2
 50     CONTINUE
      END IF
C     new style, first data from second field
      SFLD= 2
C
C     output heading and formats
      SGRP= TABGRP(UUNITS+2,TABNO)
      INITFG= 1
      CLEN= 60
      CALL WMSGTT (MSGFL,TCLU,SGRP,INITFG,
     M             CLEN,
     O             WRFMT,CONT)
      HEADCT= 0
      INITFG= 0
C
 60   CONTINUE
C       output header
        HEADCT= HEADCT+ 1
        CLEN= 120
        CALL WMSGTT (MSGFL,TCLU,SGRP,INITFG,
     M               CLEN,
     O               HEADG(1,HEADCT),CONT)
      IF ( (CONT .EQ. 1) .AND. (HEADCT .LT. 4) ) GO TO 60
C
      IF (RESMFG .EQ. 0) THEN
C       not in resume mode, set values to undefined
        RTMP= -999.0
        CALL ZIPR (NVALS,RTMP,
     O             RVAL)
      END IF
C
C     look for the record in ucifl which contains values to be processed
      J= KYST(TABNO)+ TABSUB- 1
      KEYST= TABKST(J)
      KEYND= TABKND(J)
C
      KEY= -TABREC (KEYST,KEYND,TABNM1(1,TABNO),
     $              TABSUB,OPTNO,
     $              MESSU,MSGFL,
     $              ECOUNT)
      IF (KEY .EQ. 0) THEN
C       no uci info info for this table
        INBUFF= ' '
        IF (OUTLEV .GT. 3) THEN
C         indicate that fact
          SGRP= 4
          CALL PMXTFT (MSGFL,MESSU,SCLU,SGRP)
        END IF
        IF (RESMFG .EQ. 1) THEN
C         operating in resume mode, so existing values will be kept
          IF (OUTLEV .GT. 3) THEN
C           indicate that
            SGRP= 5
            CALL PMXTFT (MSGFL,MESSU,SCLU,SGRP)
          END IF
        ELSE
C         will use defaults
          IF (OUTLEV .GT. 3) THEN
C           indicate that
            SGRP= 6
            CALL PMXTFT (MSGFL,MESSU,SCLU,SGRP)
          END IF
        END IF
      END IF
C
      IF (OUTLEV .GT. 2) THEN
C       write header to echo file
        WRITE (MESSU,2010)
        CLEN= 120
        DO 70 I= 1, HEADCT
          WRITE (MESSU,2020) (HEADG(J,I),J=1,LENSTR (CLEN,HEADG(1,I)))
 70     CONTINUE
      END IF
C
C     loop to check values and use defaults as needed
      IPOS= 0
      EPOS= 1
      EBUFF= ' '
C
C     loop back here for mult records
 80   CONTINUE
        IF (KEY .NE. 0) THEN
C         read data supplied by user from uci file
          CALL GETUCI (I0,
     M                 KEY,
     O                 INBUFF)
        END IF
C   
        IF (FILE(7) .GT. 0) THEN
C         pest supplemental file in use, see if this record needs updating
          CALL GETVEC (MESSU, MSGFL, NVALS, IPOS, OUTLEV,
     M                 INBUFF, RVAL, ECOUNT,
     O                 VALSET)          
        ELSE
C         no pest supplemental file in use
          VALSET = 0
        END IF
C
C       values from one record
        LVALS= 0
        LFLDS= SFLD- 1
        DO 90 I= SFLD,NFLDS
          IF (FTYP(I) .EQ. 'R') THEN
C           real value
            LVALS= LVALS+ 1
          ELSE IF (FTYP(I) .EQ. 'C') THEN
C           character value
            LLEN= FLEN(I)
            CLEN= LLEN/4
            IF (MOD (LLEN,4) .GT. 0) THEN
C             extra characters
              CLEN= CLEN+ 1
            END IF
            LVALS= LVALS+ CLEN
          END IF
          IF (IPOS+ LVALS .LE. NVALS) THEN
C           this field needed
            LFLDS= LFLDS+ 1
          END IF
 90     CONTINUE
C
        DO 110 IFLD= SFLD, LFLDS
C         stuff for this field
          LCOL= SCOL(IFLD)
          LLEN= FLEN(IFLD)
          LMIN= RMIN(APOS(IFLD))
          LMAX= RMAX(APOS(IFLD))
          LDEF= RDEF(APOS(IFLD))
C         field default flag
          DEFFG= 0
C
          IF (FTYP(IFLD) .EQ. 'R') THEN
C           real field
            IPOS= IPOS+ 1
            I= LENSTR (LLEN,INBUF1(LCOL))
            IF (I .LE. 0) THEN
              IF (VALSET .EQ. 0) THEN
C               supply default value
                RVAL(IPOS)= LDEF
                DEFFG= 1
              END IF
            ELSE
C             make sure string converts to valid real number
              ERRFG= 0
              IF (I.GE.3) THEN
                NONBLK = 0
                DO 85 J= 1,I-1
                  IF (INBUF1(LCOL+J-1).NE.' ') THEN
                    NONBLK = J
                  ELSE IF (NONBLK.GT.0) THEN
                    ERRFG= 2
                  END IF
 85             CONTINUE
              END IF

              IF (ERRFG .EQ. 2) THEN
C               warning, cant have blank between 2 non-blank chars
                J= 6
                CALL OMSTC (J,OPTYP1)
                CALL OMSTI (OPTNO)
                J= 12
                CALL OMSTC (J,TABNM1(1,TABNO))
                CALL OMSTI (TABSUB)
                CALL OMSTI (IFLD-1)
                J= 10
                CALL OMSTC (J,INBUF1(LCOL))
                SGRP= 14
                CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                     WRNCNT)
              END IF
C
              IF (VALSET .EQ. 0) THEN
C               get real value from user input
                RVAL(IPOS)= CHRDEC (I,INBUF1(LCOL))
              END IF
C
C             check for range
              IF (ABS (LMIN+L999) .GT. 1.0E-5) THEN
C               check against minimum with slack
                IF (RVAL(IPOS) .LT. (LMIN* 0.99999)) THEN
C                 less than min allowed
                  ERRFG= 1
                END IF
              END IF
C
              IF (ABS (LMAX+L999) .GT. 1.0E-5) THEN
C               check against maximum with slack
                IF (RVAL(IPOS) .GT. (LMAX* 1.00001)) THEN
C                 greated than max allowed
                  ERRFG= 1
                END IF
              END IF
C
              IF ( (ERRFG .EQ. 1) .AND.
     $             (ABS(RVAL(IPOS)) .LT. 1.0E-30) .AND.
     $             (ABS(LDEF+L999) .GT. 1.0E-5) ) THEN
C               value was zero, which is out of range - use default
                RVAL(IPOS)= LDEF
                DEFFG= 1
                ERRFG= 0
              END IF
C
              IF (ERRFG .EQ. 1) THEN
C               error - outside of valid range
                I= 6
                CALL OMSTC (I,OPTYP1)
                CALL OMSTI (OPTNO)
                I= 12
                CALL OMSTC (I,TABNM1(1,TABNO))
                CALL OMSTI (TABSUB)
                CALL OMSTI (IFLD-1)
                CALL OMSTR (RVAL(IPOS))
                CALL OMSTR (LMIN)
                CALL OMSTR (LMAX)
                CALL OMSTR (LDEF)
                SGRP= 3
                CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                     ECOUNT)
C               set to default value
                RVAL(IPOS)= LDEF
                DEFFG= 1
              END IF
            END IF
C
C           handle echo buffer
            EWID= 10
            IF (DEFFG .EQ. 0 .AND. VALSET .EQ. 0) THEN
C             copy user's input
              CALL COPYC (LLEN,INBUF1(LCOL),
     O                    EBUF1(EPOS))
            ELSE
C             write default or pest update value
              WRITE (EBUFF(EPOS:EPOS+EWID-1),2030) RVAL(IPOS)
            END IF
C           right justify in field of width EWID
            CALL RHTSTR (EWID,
     O                   EBUF1(EPOS))
            EPOS= EPOS+ EWID
          ELSE
C           character field, default may apply to many fields
            CLEN= LLEN/4
            IF (MOD (LLEN,4).GT.0) THEN
C             extra characters
              CLEN= CLEN+ 1
            END IF
            IF ( (RESMFG .EQ. 0) .OR. (KEY .NE. 0) ) THEN
C             pad out to a multiple of ten spaces
              EWID= CLEN*4- 1
              EPOS= EPOS+ 10- MOD (EWID,I10)- 1
C             get the characters
              EWID= 4
              DO 100 J= 1, CLEN
C               put character values into buffer
                CPOS= LCOL+ 4*(J-1)
                C4BUF= INBUFF(CPOS:CPOS+3)
                IPOS= IPOS+ 1
                READ (C4BUF,1010) RVAL(IPOS)
C               copy into echo buffer
                CALL COPYC (EWID,C4BUF,
     O                      EBUF1(EPOS))
                EPOS= EPOS+ EWID
C
C               check if echo buffer is full
                IF (EPOS .GE. 123) THEN
C                 full buffer and more to come - write and reset
                  WRITE (MESSU,2040) (EBUF1(I),I=1,EPOS- 1)
                  EBUFF= ' '
                  EPOS= 1
                END IF
 100          CONTINUE
            END IF
          END IF
C
          IF ( (ERRFG .EQ. 0) .AND.
     $         (ABS(RVAL(IPOS)+L999) .LE. 1.0E-5) ) THEN
C           need default but none available
            I= 6
            CALL OMSTC (I,OPTYP1)
            CALL OMSTI (OPTNO)
            I= 12
            CALL OMSTC (I,TABNM1(1,TABNO))
            CALL OMSTI (TABSUB)
            CALL OMSTI (IFLD-1)
            SGRP= 2
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 ECOUNT)
          END IF
C
C         check if echo buffer is full
          IF ( (EPOS .GE. 123) .AND. (IFLD .LT. LFLDS) ) THEN
C           full buffer and more to come - write and reset
            WRITE (MESSU,2040) (EBUF1(I),I=1,EPOS- 1)
            EBUFF= ' '
            EPOS= 1
          END IF
C
C       end of loop on fields
 110    CONTINUE
C
C     end of loop on multiple lines
      IF (IPOS .LT. NVALS) GO TO 80
C
      IF (OUTLEV .GT. 2) THEN
C       report values that were read in, supplied by default, or re-used
        WRITE (MESSU,2040) (EBUF1(I),I=1,EPOS- 1)
        IF (OUTLEV .GT. 4) THEN
C         also report in interpreted form
          WRITE (MESSU,WRFMT) RVAL
        END IF
      END IF
C
C     adjust if needed
      DO 120 I= 1,NVALS
        IF ( (ADD(I) .NE. 0.0) .OR. (MULT(I) .NE. 1.0) ) THEN
C         convert field
          RVAL(I)= (RVAL(I)+ADD(I))*MULT(I)
        END IF
 120  CONTINUE
C
      IF (OUTLEV .GT. 3) THEN
C       done table message
        SGRP= 7
        CALL PMXTFT (MSGFL,MESSU,SCLU,SGRP)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   MDATBL
     I                    (MDATNO,
     O                     MTHVAL,RETCOD)
C
C     + + + PURPOSE + + +
C     Process an MONTH-DATA table referred to in an input module.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    MDATNO,RETCOD
      REAL       MTHVAL(12)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MDATNO - month-data table number
C     MTHVAL - month-data table stored as a scalar
C     RETCOD - return code
C
C     + + + COMMON BLOCKS- INTERP1 + + +
      INCLUDE    'crin1.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      KEY,KEYND,KEYST,MESSU,NO,SCLU,SGRP,
     $             MSGFL,I,I0
      CHARACTER*80 UCIBF
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (UCIBF,UCIBF1)
      CHARACTER*1  UCIBF1(80)
C
C     + + + FUNCTIONS + + +
      INTEGER    VALNO
C
C     + + + EXTERNALS + + +
      EXTERNAL   VALNO,OMSG,OMSTI,OMSTC,DUMPER,GETUCI,OMSTR
C
C     + + + INPUT FORMATS + + +
 1020 FORMAT (12F6.0)
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' PROCESSING MONTH-DATA NO. ',I3)
 2060 FORMAT (/,' FINISHED PROCESSING MONTH-DATA NO. ',I3)
C
C     + + + END SPECIFICATIONS + + +
C
      I0= 0

      MESSU= FILE(1)
      MSGFL= FILE(15)
C
      SCLU= 218
C
      RETCOD= 0
C
      IF (OUTLEV .GT. 3) THEN
C       processing message
        WRITE (MESSU,2000) MDATNO
      END IF
C
C     check that the reference is valid
      NO= VALNO (NMDATS,MDTINX,MDATNO)
      IF (NO .EQ. 0) THEN
C       error - month-data table referred to not found in month-data table index
        CALL OMSTI (MDATNO)
        SGRP= 10
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             ECOUNT)
        RETCOD= 1
      ELSE
C       month-data table found - process it
        KEYST= MDTINX(NO,2)
        KEYND= MDTINX(NO,3)
        IF (OUTLEV .GT. 5) THEN
C         dump month-data table records
          CALL DUMPER (KEYST,KEYND,MESSU)
        END IF
C
C       get past the month-data table heading
        KEY= KEYST
        CALL GETUCI (I0,
     M               KEY,
     O               UCIBF)
        READ (UCIBF,1020,ERR=5) MTHVAL
          GO TO 8
 5      CONTINUE
C         error - invalid numeric input
          CALL OMSTI (MDATNO)
          I= 72
          CALL OMSTC (I,UCIBF1)
          SGRP= 12
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               ECOUNT)
          RETCOD= 2
 8      CONTINUE
C
        IF (RETCOD .EQ. 0) THEN
C         check for negative values
          DO 10 I= 1, 12
            IF (MTHVAL(I) .LT. 0.0) THEN
C             error - month-data table cannot contain negative values
              CALL OMSTI (MDATNO)
              CALL OMSTI (I)
              CALL OMSTR (MTHVAL(I))
              SGRP= 11
              CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                   ECOUNT)
              RETCOD= 2
              MTHVAL(I)= 0.0
            END IF
 10       CONTINUE
        END IF
      END IF
C
      IF (OUTLEV .GT. 3) THEN
        WRITE (MESSU,2060) MDATNO
      END IF
C
      RETURN
      END
