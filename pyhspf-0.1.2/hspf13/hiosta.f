C
C
C
      SUBROUTINE   GSTVFL
     I                   (MESSU,MSGFL,STFIL,SDATIM)
C
C     + + + PURPOSE + + +
C     Read file containing status of state variables.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSU,MSGFL,STFIL,SDATIM(5)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSU  - unit number to write messages on
C     MSGFL  - unit number of message file
C     STFIL  - unit number of status file
C     SDATIM - starting date/time
C     NDELT  - simulation time interval in minutes
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'ciosta.inc'
      INCLUDE 'cspec.inc'
C
      INCLUDE 'pspuvr.inc'
      INCLUDE 'cspuvr.inc'
C
      INCLUDE 'pspvqd.inc'
      INCLUDE 'cspvqd.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER        LOPNUM,NSUB(3),ENDFG,LADDR,LOPTYP,ERRFLG,LEN,J,I0,
     $               LOIND,LTYPE,IVAL,DADD,I6,I60,SCLU,SGRP,BGRP,I,I1,
     $               UKWD,UPOS,UVQ,LINE,HDRFG,UADD
      REAL           RVAL,R0
      DOUBLE PRECISION DVAL
      CHARACTER*6    LOPNAM,OPVAR
      CHARACTER*12   CVAL(MAXSTC)
      CHARACTER*15   CADDR
      CHARACTER*1000 CBUFF
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (LOPNAM,LOPNA1),(CADDR,CADDR1),(CBUFF,CBUF1),
     $            (CVAL,CVAL1)
      CHARACTER*1  LOPNA1(6),CADDR1(15),CBUF1(1000),CVAL1(12,MAXSTC)
C
C     + + + FUNCTIONS + + +
      INTEGER        CHKSTR,OPNNO,ZLNTXT,DADDR
C
C     + + + EXTERNALS + + +
      EXTERNAL       WMSGTT,GETOSV,PUTOSV,CHKSTR,OPNNO,ZLNTXT,DADDR,
     $               SPNXDT,OMSTC,OMSG
C
C     + + + DATA INITIALIZATIONS + + +
      DATA I0,I1,I6,I60/0,1,6,60/
      DATA R0/0.0/
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (A1000)
 1010 FORMAT (A15,I5,3X,2A1,I5)
 1020 FORMAT (30X,75(I4,4I2))
 1030 FORMAT (2X,A6,I3,1X,A15,I3,75A12)
 1040 FORMAT (I12)
 1050 FORMAT (F12.0)
C
C     + + + END SPECIFICATIONS + + +
C
C      WRITE(99,*) 'HIOSTA:GSTVFL:NOPNS:',NOPNS
C
      SCLU= 220
C
      LINE= 0
      HDRFG= 0
      ENDFG= 0
C
C     initialize common block
      COLCNT= 0
      STACNT= 0
      REPCOD= 0
      REPSTP= 0
      DO 10 I= 1, 5
        REPDAT(I)= SDATIM(I)
        RSTDAT(I)= SDATIM(I)
 10   CONTINUE
C
 20   CONTINUE
C       read records until end of file
        ERRFLG= 0
        LADDR= 0
        READ (STFIL,1000,END=30,ERR=30) CBUFF
          GO TO 40
 30     CONTINUE
          ENDFG= 1
 40     CONTINUE
        IF (ENDFG .EQ. 0) THEN
C         line read successfully
          LINE= LINE+ 1
          IF (LINE .EQ. 1) THEN
C           first line of file
            IF (CBUFF(1:2) .NE. '  ') THEN
C             first line can specify periodic status saves
              HDRFG= 1
              READ (CBUFF,1010,ERR=50) STNAM,COLCNT,CRCOD,REPSTP
                GO TO 60
 50           CONTINUE
C               error reading header line
                I= 30
                CALL OMSTC (I,CBUF1(1))
                SGRP= 11
                CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                     ECOUNT)
                ERRFLG= 1
 60           CONTINUE
C
              IF (ERRFLG .EQ. 0) THEN
C               check repeat code
                CALL CKTCOD (CRCOD,MESSU,MSGFL,SCLU,
     M                       ECOUNT,ERRFLG,
     O                       REPCOD)
              END IF
C
              IF (ERRFLG .EQ. 0) THEN
C               get column header dates
                READ (CBUFF,1020,ERR=70) ((COLDAT(I,J),I= 1,5),
     $                                     J=1,COLCNT)
                  GO TO 80
 70             CONTINUE
C                 error reading column header dates
                  SGRP= 12
                  CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                       ECOUNT)
                  ERRFLG= 1
 80             CONTINUE
              END IF
C
              IF (ERRFLG .EQ. 0) THEN
C               check for periodic dates
                IF (REPCOD .GE. 3) THEN
C                 periodic status saves requested
                  CALL SPNXDT (REPCOD,REPSTP,I1,
     M                         REPDAT)
                END IF
              END IF
            END IF
          END IF
C
          IF (HDRFG .EQ. 0) THEN
C           this should be a normal status line
C
            IF (STACNT .LT. MAXSTA) THEN
C             process this line of status file
              READ (CBUFF,1030,ERR=90)  LOPNAM,LOPNUM,CADDR,LTYPE,
     $                                 (CVAL(I),I=1,COLCNT)
                GO TO 100
 90           CONTINUE
C               error reading status line
                I= 30
                CALL OMSTC (I,CBUF1(1))
                SGRP= 13
                CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                     ECOUNT)
                ERRFLG= 1
 100          CONTINUE
C
              IF (ERRFLG .EQ. 0) THEN
C               valid line
C
C               first check operation type
                IF (LOPNAM .EQ. 'GLOBAL') THEN
C                 workspace variable
C
                  OPVAR= CADDR(1:6)
                  IF (SPUCNT .GE. 1) THEN
C                   check for workspace variable name
                    UKWD= CHKSTR (I6,SPUCNT,CADDR1,SPUVN1)
                    IF (UKWD .GE. 1) THEN
C                     variable name exists
                      UPOS= SPUPOS(UKWD)
                      IF (SPUADD(UPOS) .LT. 0) THEN
C                       found workspace variable
C
C                       find target uvquan with this address
                        UVQ= 0
 110                    CONTINUE
                          UVQ= UVQ+ 1
                          IF (UVQ .LE. NVQD) THEN
C                           check this
                            IF (SPUADD(UPOS) .EQ. UVQADD(UVQ)) THEN
C                             put index into address with negative value
                              LADDR= -UVQ
                            END IF
                          END IF
                        IF ( (LADDR .EQ. 0) .AND.
     $                       (UVQ .LT. NVQD)) GO TO 110
C
                        IF (LADDR .EQ. 0) THEN
C                         error - no uvquan found at target address
                          I= 6
                          CALL OMSTC (I,CADDR1(1))
                          I= -LADDR
                          CALL OMSTI (I)
                          SGRP= 14
                          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                               ECOUNT)
                          ERRFLG= 1
                        END IF
                      ELSE
C                       variable name not a workspace variable
                        ERRFLG= 1
                        I= 6
                        CALL OMSTC (I,CADDR1(1))
                        SGRP= 15
                        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                             ECOUNT)
                      END IF
                    ELSE
C                     workspace variable name not recognized
                      ERRFLG= 1
                      I= 6
                      CALL OMSTC (I,CADDR1(1))
                      SGRP= 16
                      CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                           ECOUNT)
                    END IF
                  ELSE
C                   no workspace variables defined
                    ERRFLG= 1
                    I= 6
                    CALL OMSTC (I,CADDR1(1))
                    SGRP= 17
                    CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                         ECOUNT)
                  END IF
                ELSE
C                 should be osv variable
                  LOPTYP= CHKSTR (I6,I60,LOPNA1,OPTYL1)
                  IF (LOPTYP .LE. 0) THEN
C                   invalid operation type
                    ERRFLG= 1
                    I= 6
                    CALL OMSTC (I,LOPNA1)
                    SGRP= 18
                    CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                         ECOUNT)
                  END IF
C
                  IF (ERRFLG .EQ. 0) THEN
C                   operation type valid - check for active operation
                    LOIND= OPNNO (LOPNAM,LOPNUM,LOPNUM,MAXOPN,OPNTAB,I1,
     I                            NOPNS)
                    IF (LOIND .LE. 0) THEN
C                     operation not active - skip this variable
                      ERRFLG= 1
                    END IF
                  END IF
C
                  IF (ERRFLG .EQ. 0) THEN
C                   active operation - check variable name
                    BGRP= 9
                    CALL MKADDR (I1,CADDR,MESSU,MSGFL,SCLU,BGRP,
     M                           LOPTYP,ERRFLG,
     O                           OPVAR,NSUB,LADDR)
                  END IF
                END IF
C
                IF (ERRFLG.EQ. 0) THEN
C                 check variable type
                  IF (LTYPE .EQ. 0) THEN
C                   variable type defaults to real
                    LTYPE= 3
                  END IF
                  IF ( (LTYPE .LE. 1) .OR. (LTYPE .GE. 5) ) THEN
C                   error - invalid type code
                    ERRFLG= 1
                    I= 6
                    CALL OMSTC (I,CADDR1(1))
                    CALL OMSTI (LTYPE)
                    SGRP= 19
                    CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                         ECOUNT)
                  END IF
                END IF
C
                IF (ERRFLG .EQ. 0) THEN
C                 valid variable - process
                  STACNT= STACNT+ 1
                  LEN= ZLNTXT (CVAL(COLCNT))
                  IF (LEN .GE. 1) THEN
C                   non-blank value used first time to set variable
                    IF (LTYPE .EQ. 2) THEN
C                     read integer value
                      READ (CVAL(COLCNT),1040,ERR=120) IVAL
                        GO TO 130
 120                  CONTINUE
C                       error reading integer value
                        I= 6
                        CALL OMSTC (I,CADDR1(1))
                        I= 12
                        CALL OMSTC (I,CVAL1(1,COLCNT))
                        SGRP= 20
                        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                             ECOUNT)
                        ERRFLG= 1
 130                  CONTINUE
                    ELSE
C                     read real value
                      READ (CVAL(COLCNT),1050,ERR=140) RVAL
                        GO TO 150
 140                  CONTINUE
C                       error reading real value
                        I= 6
                        CALL OMSTC (I,CADDR1(1))
                        I= 12
                        CALL OMSTC (I,CVAL1(1,COLCNT))
                        SGRP= 21
                        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                             ECOUNT)
                        ERRFLG= 1
 150                  CONTINUE
                    END IF
C
                    IF (LADDR .GE. 1) THEN
C                     update current value in osv
                      CALL GETOSV (OPNTAB(7,LOIND),OPNTAB(8,LOIND),
     I                             MAXOSV,
     O                             OSV)
                      IF (LTYPE .EQ. 2) THEN
C                       update integer value
                        OSV(LADDR)= IVAL
                      ELSE IF (LTYPE .EQ. 3) THEN
C                       update real value
                        OSVR(LADDR)= RVAL
                      ELSE IF (LTYPE .EQ. 4) THEN
C                       update double precision value
                        DADD= DADDR (LADDR)
                        OSVD(DADD)= RVAL
                      END IF
                      CALL PUTOSV (OPNTAB(7,LOIND),OPNTAB(8,LOIND),
     I                             MAXOSV,OSV)
                      DVAL= RVAL
                      CALL SPVQUP (I1,LOIND,LADDR,I0,I0,IVAL,R0,RVAL,
     I                             R0,DVAL,I1)
                    ELSE IF (LADDR .LE. -1) THEN
C                     update uvquans based on this address
                      DVAL= RVAL
                      UADD= UVQADD(-LADDR)
                      CALL SPVQUP (I1,I0,UADD,I0,I0,IVAL,R0,RVAL,R0,
     I                             DVAL,I1)
                    END IF
                  END IF
C
                  IF (ERRFLG .EQ. 0) THEN
C                   save info for write
                    OPNAM(STACNT) = LOPNAM
                    OPNUM(STACNT) = LOPNUM
                    VARNAM(STACNT)= OPVAR
                    TYP(STACNT)   = LTYPE
                    ADDR(STACNT)  = LADDR
                    SUB(1,STACNT) = CADDR(7:9)
                    SUB(2,STACNT) = CADDR(10:12)
                    SUB(3,STACNT) = CADDR(13:15)
                    IF (LADDR .GE. 1) THEN
C                     osv variable - save keys
                      OSVRST(STACNT)= OPNTAB(7,LOIND)
                      OSVREN(STACNT)= OPNTAB(8,LOIND)
                    ELSE
C                     workspace variable - dummy keys
                      OSVRST(STACNT)= 0
                      OSVREN(STACNT)= 0
                    END IF
                  END IF
                END IF
              END IF
            ELSE
C             warning - too many variables
              SGRP= 22
              CALL OMSTI (MAXSTA)
              CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                   ECOUNT)
              ENDFG= 1
            END IF
          END IF
        END IF
        HDRFG= 0
      IF (ENDFG .EQ. 0) GO TO 20
C
C     reinitialize column counter for write
      COLCNT= 0
C
      RETURN
      END
C
C
C
      SUBROUTINE   PSTVFL
     I                    (STFIL,CURIVL,DELT,NOPNS,LSTCAL,MESSU,MSGFL,
     M                     ECOUNT)
C
C     + + + PURPOSE + + +
C     Write file containing status of state variables.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   STFIL,CURIVL,DELT,NOPNS,LSTCAL,MESSU,MSGFL,ECOUNT
C
C     + + + ARGUMENT DEFINITIONS + + +
C     STFIL  - unit number of status file
C     CURIVL - current interval of run
C     DELT   - interval of run in minutes
C     NOPNS  - total number of operations in run
C     LSTCAL - flag indicating last interval of run
C     MESSU  - unit number to write messages on
C     MSGFL  - unit number for file containing error messages
C     ECOUNT - count of status file errors
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'ciosta.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER        I,J,I1,I2,I5,LEN,SIGDIG,DECPLA,DADD,OSVSAV,TYPFG,
     $               DOITFG,CURDAT(5),ST,EN,DONEFG,SCLU,SGRP,IVAL
      REAL           RVAL
      CHARACTER*1000 OBUFF
C
C     + + + EQUIVALENCE + + +
      EQUIVALENCE  (OBUFF,OBUF1)
      CHARACTER*1   OBUF1(1000)
C
C     + + + FUNCTIONS + + +
      INTEGER       DADDR
C
C     + + + EXTERNALS + + +
      EXTERNAL      COPYI,SPNXDT,SPCKDT,EXDATE,GETOSV,DADDR,GTQUAN,
     $              DECCHX,OMSTC,OMSTI,OMSG
C
C     + + + DATA INITIALIZATIONS + + +
      DATA I1,I2,I5/1,2,5/
      DATA LEN,SIGDIG,DECPLA/12,6,5/
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (2X,A6,I3,1X,A6,3A3,I3)
 2010 FORMAT (1000A1)
 2020 FORMAT (I12)
 2030 FORMAT (A15,I5,3X,2A1,I5,75(2X,5I2.2))
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU= 220
C
      DOITFG= -1
C
      IF ( (LSTCAL .EQ. 1) .OR. (REPCOD .GE. 3) ) THEN
C       compute current date
        CALL COPYI (I5,RSTDAT,
     O              CURDAT)
        I= CURIVL*DELT
        CALL SPNXDT (I2,I,I1,
     M               CURDAT)
      END IF
      IF (LSTCAL .EQ. 1) THEN
C       last interval of run - always write
        DOITFG= 1
      ELSE IF (REPCOD .GE. 3) THEN
C       compare against repeat date
        CALL SPCKDT (CURDAT,REPDAT,
     O               DOITFG)
        IF (DOITFG .GE. 0) THEN
C         update repeat date until beyond current date
          DONEFG= -1
 3        CONTINUE
            CALL SPNXDT (REPCOD,REPSTP,I1,
     M                   REPDAT)
            CALL SPCKDT (REPDAT,CURDAT,
     O                   DONEFG)
          IF (DONEFG .LE. 0) GO TO 3
        END IF
      END IF
C
      IF (DOITFG .GE. 0) THEN
C       status file must be written
        COLCNT= COLCNT+ 1
        IF (COLCNT .LE. MAXSTC) THEN
C         there is room to add a column
C
C         initialize file and data
          REWIND (STFIL)
C
C         write header
          CALL EXDATE (CURDAT,
     O                 COLDAT(1,COLCNT))
          COLDAT(1,COLCNT) = MOD (COLDAT(1,COLCNT),100)
          WRITE (STFIL,2030)   STNAM,COLCNT,CRCOD,REPSTP,
     $                       ((COLDAT(I,J),I=1,5),J=1,COLCNT)
C
C         begin loop on status file lines
          OSVSAV= -1
          DO 10 I= 1, STACNT
C
C           initialize output buffer for current line
C
C            WRITE(99,*) 'HIOSTA:PSTVFL',I,OSVRST(I),OSVREN(I),ADDR(I)
            WRITE(OBUFF,2000)  OPNAM(I),OPNUM(I),VARNAM(I),
     $                        (SUB(J,I),J=1,3),TYP(I)
            IF (OBUFF(9:11)  .EQ. '  0') OBUFF(9:11)  = '   '
            IF (OBUFF(19:21) .EQ. '  0') OBUFF(19:21) = '   '
            IF (OBUFF(22:24) .EQ. '  0') OBUFF(22:24) = '   '
            IF (OBUFF(25:27) .EQ. '  0') OBUFF(25:27) = '   '
C
C           now fetch current value of variable
C
            IF (ADDR(I) .GE. 1) THEN
C             this is an osv variable
C
              IF (OSVRST(I) .NE. OSVSAV) THEN
C               need different osv than last osv variable
                CALL GETOSV (OSVRST(I),OSVREN(I),MAXOSV,
     O                       OSV)
              END IF
              IF (TYP(I) .EQ. 2) THEN
C               write integer value
                STVAL(I,COLCNT)= OSV(ADDR(I))
              ELSE IF (TYP(I) .EQ. 3) THEN
C               write real value
                STVALR(I,COLCNT)= OSVR(ADDR(I))
              ELSE IF (TYP(I) .EQ. 4) THEN
C               write double precision value
                DADD= DADDR (ADDR(I))
                STVALR(I,COLCNT)= OSVD(DADD)
              END IF
            ELSE
C             workspace variable
              J= NOPNS+ 1
              CALL GTQUAN (ADDR(I),CURIVL,J,
     O                     IVAL,RVAL,TYPFG)
              IF (TYPFG .NE. TYP(I)) THEN
C               error bug - types should match
                SGRP= 31
                J= 6
                CALL OMSTC (J,VARNAM(I))
                CALL OMSTI (TYP(I))
                CALL OMSTI (TYPFG)
                CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                     ECOUNT)
              ELSE IF (TYPFG .EQ. 2) THEN
C               integer
                STVAL(I,COLCNT)= IVAL
              ELSE IF (TYPFG .GE. 3) THEN
C               real
                STVALR(I,COLCNT)= RVAL
              END IF
            END IF
C
C           finally write out buffer
            DO 5 J= 1, COLCNT
              EN= 30+ J*LEN
              ST= EN- LEN+ 1
              IF (TYP(I) .EQ. 2) THEN
C               integer values
                WRITE (OBUFF(ST:EN),2020) STVAL(I,J)
C                WRITE(99,*) 'HIOSTA:PSTVFL,stval:',STVAL(I,J)
              ELSE
C               real values
                CALL DECCHX (STVALR(I,J),LEN,SIGDIG,DECPLA,
     O                       OBUF1(ST))
C                WRITE(99,*) 'HIOSTA:PSTVFL,stvalr:',STVALR(I,J)
              END IF
 5          CONTINUE
            WRITE (STFIL,2010) (OBUF1(J),J=1,EN)
C
 10       CONTINUE
        END IF
      END IF
C
      RETURN
      END
