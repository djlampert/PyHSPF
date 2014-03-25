C
C     include file CSPUVR.INC - common block to store special action
C     user defined variable names
      COMMON /CSPUVR/ SPUCNT,
     $                SPUPOS,SPUTYP,SPUSUB,SPUADD,SPUACT,SPUFRC
C
      INTEGER         SPUCNT,
     $                SPUPOS(MXSPUV),SPUTYP(MXSPUV),
     $                SPUSUB(3,MXSPUX),SPUADD(MXSPUX),
     $                SPUACT(MXSPUX)
      REAL            SPUFRC(MXSPUX)
C
      COMMON /SPUVNC/ SPUVNM,SPUNAM
      CHARACTER*6     SPUVNM(MXSPUV),SPUNAM(MXSPUX)
C
      EQUIVALENCE    (SPUVNM,SPUVN1),(SPUNAM,SPUNA1)
      CHARACTER*1     SPUVN1(MXSPUV*6),SPUNA1(MXSPUX*6)
C
C     SPUCNT - number of UVNAMEs
C     SPUPOS - position in buffer of first member variable in UVNAME
C     SPUTYP - OMCODE of all member variables in UVNAME
C     SPUSUB - subscripts of member variable in buffer
C     SPUADD - address of member variable in buffer
C     SPUACT - action code of member variable: 1-MOVT; 2-MOV1; 3-MOV2
C     SPUFRC - fraction of member variable in buffer
C
C     SPUVNM - name of UVNAME
C     SPUNAM - name of member variable
C
