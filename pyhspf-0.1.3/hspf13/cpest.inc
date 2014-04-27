C
C     information from pest supplemental file
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxpst.inc'
C
      COMMON/CPEST/ USED,NUM,        ID,       
     $              SUPVAR
      INTEGER       USED,NUM(MAXVEC),ID(MAXVEC)
      REAL          SUPVAR(MAXNUM,MAXVEC)
C