C
C
C
      SUBROUTINE   ACCVEC
     I                   (DIM,FRMVEC,
     M                    TOVEC)
C
C     + + + PURPOSE + + +
C     Add the elements of FRMVEC to the corresponding elements of
C     TOVEC.  Both vectors of dimension dim are one dimensional.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    DIM
      REAL       FRMVEC(DIM),TOVEC(DIM)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DIM    - ???
C     FRMVEC - ???
C     TOVEC  - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I
C
C     + + + END SPECIFICATIONS + + +
C
      DO 10 I= 1,DIM
        TOVEC(I)= TOVEC(I) + FRMVEC(I)
 10   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   BALCHK
     I                   (OPTYP,OPNO,DATIM,MESSU,PRINTU,MSGFL,
     I                    STORS,STOR,MATIN,MATDIF,UNITID,HDRFG,
     M                    WCOUNT)
C
C     + + + PURPOSE + + +
C     Perform a material balance check and report the result.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      OPTYP,DATIM(5),MESSU,PRINTU,OPNO,MSGFL,HDRFG,WCOUNT
      REAL         MATDIF,MATIN,STOR,STORS
      CHARACTER*8  UNITID
C
C     + + + ARGUMENT DEFINITIONS + + +
C     OPTYP  - operation type
C     OPNO   - operation number
C     DATIM  - date and time of day
C     MESSU  - ftn unit no. to be used for printout of messages
C     PRINTU - fortran unit number on which to print output
C     MSGFL  - fortran unit number of HSPF message file
C     STORS  - storage of material at start of print interval
C     STOR   - storage of material at end of print interval
C     MATIN  - total inflow of material during print interval
C     MATDIF - net inflow(in-out) of material during print interval
C     UNITID - units of reference value
C     HDRFG  - flag indicating whether to print header - 1=yes else no
C     WCOUNT - count of times this message has been produced
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      SCLU,SGRP,I6
      REAL         ERROR,REFVAL,RELERR,PCTERR
      CHARACTER*6  COPTYP(3)
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (COPTYP,COPTY1)
      CHARACTER*1  COPTY1(6,3)
C
C     + + + INTRINSICS + + +
      INTRINSIC    ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL     OMSTR,OMSG,OMSTI,OMSTD,OMSTC
C
C     + + + DATA INITIALIZATIONS + + +
      DATA COPTYP/'PERLND','IMPLND','RCHRES'/
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,'   BALANCE',22X,'% ERROR IN ',A8)
 2010 FORMAT (29X,F10.3,2X,1PE10.3)
C
C     + + + END SPECIFICATIONS + + +
C
C     the "reference" value is taken as the sum of initial storage
C     and inflow because, at one extreme, storage might completely
C     dominate throughput over a printout interval, and at the
C     other extreme, the reverse might be the case
      I6= 6
      REFVAL= STORS+ MATIN
      ERROR= (STOR- STORS)- MATDIF
      IF (REFVAL .GT. 0.0) THEN
C       compute relative error
        RELERR= ERROR/REFVAL
        PCTERR= 100.0*RELERR
      ELSE
C       cannot compute
        RELERR= 0.0
        PCTERR= 0.0
      END IF
C
      IF (PRINTU .GT. 0) THEN
C       write to output file
        IF (HDRFG .EQ. 1) THEN
C         write header
          WRITE (PRINTU,2000) UNITID
        END IF
        WRITE (PRINTU,2010)  PCTERR, REFVAL
      END IF
C
      IF (ABS (RELERR) .GT. 0.001) THEN
C       issue a warning
        CALL OMSTD (DATIM)
        CALL OMSTC (I6,COPTY1(1,OPTYP))
        CALL OMSTI (OPNO)
        CALL OMSTR (RELERR)
        CALL OMSTR (STORS)
        CALL OMSTR (STOR)
        CALL OMSTR (MATIN)
        CALL OMSTR (MATDIF)
        SCLU= 238
        SGRP= 1
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             WCOUNT)
      END IF
C
      RETURN
      END
C
C
C
      REAL   FUNCTION   DAYVAL
     I                        (MVAL1,MVAL2,DAY,NDAYS)
C
C     + + + PURPOSE + + +
C     Linearly interpolate a value for this day (DAYVAL), given
C     values for the start of this month and next month (MVAL1 and
C     MVAL2).  ndays is the number of days in this month.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    DAY,NDAYS
      REAL       MVAL1,MVAL2
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MVAL1  - ???
C     MVAL2  - ???
C     DAY    - day of month
C     NDAYS  - no. of days in this month
C
C     + + + LOCAL VARIABLES + + +
      REAL       RDAY,RNDAYS
C
C     + + + INTRINSICS + + +
      INTRINSIC  FLOAT
C
C     + + + END SPECIFICATIONS + + +
C
      RDAY  = FLOAT(DAY)
      RNDAYS= FLOAT(NDAYS)
      DAYVAL= MVAL1 + (MVAL2 - MVAL1)*(RDAY - 1)/RNDAYS
C
      RETURN
      END
C
C
C
      SUBROUTINE   TRNVEC
     I                   (DIM,FROM,FACTA,FACTB,
     O                   TO)
C
C     + + + PURPOSE + + +
C     Multiply the elements of a vector FROM, with dimension DIM, by
C     FACTA and add FACTB.  Store the results in a vector to of
C     similar size, i.e. perform a linear transform.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    DIM
      REAL       FACTA,FACTB,FROM(DIM),TO(DIM)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DIM    - ???
C     FROM   - ???
C     FACTA  - ???
C     FACTB  - ???
C     TO     - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I
C
C     + + + INTRINSICS + + +
      INTRINSIC  ABS
C
C     + + + END SPECIFICATIONS + + +
C
      IF ((ABS(FACTB)) .LE. 0.0) THEN
C       factb is zero
        IF ((ABS(FACTA - 1.0)) .GE. 1.0E-5) THEN
C         facta is not unity
          DO 10 I= 1,DIM
            IF (FROM(I) .GT. -1.0E+29) THEN
C             transform value
              TO(I)= FROM(I)*FACTA
            ELSE
C             value is undefined - leave alone
              TO(I)= FROM(I)
            END IF
 10       CONTINUE
        ELSE
C         facta is unity
          DO 20 I= 1,DIM
            TO(I)= FROM(I)
 20       CONTINUE
        END IF
      ELSE
C       factb must be added
        IF ((ABS(FACTA - 1.0)) .GE. 1.0E-5) THEN
C         facta is not unity
          DO 30 I= 1,DIM
            IF (FROM(I) .GT. -1.0E+29) THEN
C             transform value
              TO(I)= FROM(I)*FACTA + FACTB
            ELSE
C             value is undefined - leave alone
              TO(I)= FROM(I)
            END IF
 30       CONTINUE
        ELSE
C         facta is unity
          DO 40 I= 1,DIM
            IF (FROM(I) .GT. -1.0E+29) THEN
C             transform value
              TO(I)= FROM(I) + FACTB
            ELSE
C             value is undefined - leave alone
              TO(I)= FROM(I)
            END IF
 40       CONTINUE
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   SETVEC
     I                   (DIM,VALUE,
     O                    VEC)
C
C     + + + PURPOSE + + +
C     Assign the value VALUE to all the elements of a vector VEC.
C     Dimension is DIM.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    DIM
      REAL       VALUE,VEC(DIM)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DIM    - ???
C     VALUE  - ???
C     VEC    - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I
C
C     + + + END SPECIFICATIONS + + +
C
      DO 10 I= 1,DIM
        VEC(I)= VALUE
 10   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   HREQTS
     I                    (FLAGPT,IVL1,REQFG,MESSU,MSGFL,DATIM,OPTYP,
     I                     OPTNO,TSNAM,TSSUB,SECNAM,MSECNM,OPFGNM,
     I                     FLGVAL,
     O                     VALUE)
C
C     + + + PURPOSE + + +
C     Fetches value of a required timeseries, or generates an error
C     message and returns -999.0 if the flag pointer is not set.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     FLAGPT,IVL1,REQFG,MESSU,MSGFL,DATIM(5),OPTNO,
     $            TSSUB(2),FLGVAL
      REAL        VALUE
      CHARACTER*6 OPTYP,TSNAM,SECNAM,MSECNM,OPFGNM
C
C     + + + COMMON BLOCKS + + +
      INCLUDE "cmdum.inc"
      INCLUDE "cmpad.inc"
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FLAGPT - flag pointer
C     IVL1   - interval pointer
C     REQFG  - flag indicating when timeseries is required
C                   1 - always when operation is on
C                   2 - always when section is on
C                   3 - only when another certain section is off
C                   4 - only when a flag set to a certain value
C                   5 - only when both 3 and 4 are true
C                   6 - only when another timeseries is requested
C     MESSU  - ftn unit no. to be used for printout of messages
C     MSGFL  - fortran unit number of HSPF message file
C     DATIM  - date and time of day
C     OPTYP  - operation type
C     OPTNO  - operation number
C     TSNAM  - name of timeseries
C     TSSUB  - timeseries subscripts
C     SECNAM - name of module section containing t.s.
C     MSECNM - name of turned-off module section making t.s. required 
C     OPFGNM - name of option flag or other t.s. making t.s. required
C     FLGVAL - value of option flag making t.s. required
C     VALUE  - new value of timeseries
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,ECOUNT,SCLU,SGRP
      CHARACTER*6 OBUFF
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (OBUFF,OBUFF1)
      CHARACTER*1  OBUFF1(6)
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU= 238
      ECOUNT= 0
C
      IF (FLAGPT .GE. 1) THEN
C       timeseries has been supplied
        VALUE= PAD(FLAGPT+ IVL1)
      ELSE
C       required timeseries is missing
        CALL OMSTD (DATIM)
        I= 6
        OBUFF= OPTYP
        CALL OMSTC (I,OBUFF1)
        CALL OMSTI (OPTNO)
        OBUFF= TSNAM
        CALL OMSTC (I,OBUFF1)
        CALL OMSTI (TSSUB(1))
        CALL OMSTI (TSSUB(2))
        IF (REQFG .EQ. 1) THEN
C         timeseries is always required by the operation
        ELSE IF (REQFG .EQ. 2) THEN
C         timeseries is always required by the section
          OBUFF= SECNAM
          CALL OMSTC (I,OBUFF1)
        ELSE IF (REQFG .EQ. 3) THEN
C         timeseries is required when missing section turned off
          OBUFF= SECNAM
          CALL OMSTC (I,OBUFF1)
          OBUFF= MSECNM
          CALL OMSTC (I,OBUFF1)
        ELSE IF (REQFG .EQ. 4) THEN
C         timeseries is required when option flag is set
          OBUFF= SECNAM
          CALL OMSTC (I,OBUFF1)
          OBUFF= OPFGNM
          CALL OMSTC (I,OBUFF1)
          CALL OMSTI (FLGVAL)
        ELSE IF (REQFG .EQ. 5) THEN
C         timeseries is required when option flag is set plus a
C         missing section turned off
          OBUFF= SECNAM
          CALL OMSTC (I,OBUFF1)
          OBUFF= MSECNM
          CALL OMSTC (I,OBUFF1)
          OBUFF= OPFGNM
          CALL OMSTC (I,OBUFF1)
          CALL OMSTI (FLGVAL)
        ELSE IF (REQFG .EQ. 6) THEN
C         timeseries is required when another timeseries is requested
          OBUFF= SECNAM
          CALL OMSTC (I,OBUFF1)
          OBUFF= MSECNM
          CALL OMSTC (I,OBUFF1)
          OBUFF= OPFGNM
          CALL OMSTC (I,OBUFF1)
        END IF
        SGRP= 10+ REQFG
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             ECOUNT)
        VALUE= -999.0
      END IF
C
      RETURN
      END
