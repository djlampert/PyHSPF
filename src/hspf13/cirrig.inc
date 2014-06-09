C
C     include file CIRRIG.INC - common block to store irrigation
C     data
C
      COMMON /CIRRIG/IRRWFG,IRVADD,IRMADD,IRWADD,IRDADD,IRSADD,
     $               IROSV
      INTEGER        IRRWFG,IRVADD,IRMADD,IRWADD,IRDADD,IRSADD,
     $               IROSV(500)
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (IROSV,IROSVR),(IROSV,IROSVD)
      REAL         IROSVR(500)
      DOUBLE PRECISION IROSVD(250)
C
C     IRRWFG - flag indicating whether irrigation withdrawals
C              from any RCHRES are being made
C     IRVADD - address in RCHRES OSV for VOL
C     IRMADD - address in RCHRES OSV for IRMINV
C     IRWADD - address in RCHRES OSV for IRRWDL
C     IRDADD - address in RCHRES OSV for RIRDEM
C     IRSADD - address in RCHRES OSV for RIRSHT
C     IROSV  - workspace for OSV chunks
C     IROSVR - real version of workspace 
C     IROSVD - double precision version of workspace 
C
