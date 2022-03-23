C*PGQIDS -- inquire list of open devices
C%void cpgqids(int nmax, int *nopen, int *list);
C+
      SUBROUTINE PGQIDS (NMAX, NOPEN, LIST)
      INTEGER NMAX, NOPEN, LIST(NMAX)
C
C This subroutine returns the number of open PGPLOT devices, and
C a list of their identifiers.  The identifier is an integer
C greater than zero that is assigned when PGOPEN is called to open
C the device, and it may be used as an argument to PGSLCT.  
C
C [This routine was added to PGPLOT in Version 5.3.0.]
C
C Arguments:
C  NMAX   (input)  : maximum number of identifiers to return
C  NOPEN  (output) : the number of open devices
C  LIST   (output) : the identifiers of the NOPEN open devices
C                    are returned in LIST(1) ... LIST(NOPEN);
C                    if NOPEN is greater than NMAX, only the first
C                    NMAX are returned. LIST must be an array with
C                    dimension NMAX or greater.
C--
C  9-Jul-1998 - new routine [TJP].
C-----------------------------------------------------------------------
      INCLUDE  'pgplot.inc'
      INTEGER I
C
      NOPEN = 0
      DO 10 I=1,PGMAXD
         IF (PGDEVS(I) .GT. 0) THEN
            NOPEN = NOPEN + 1
            IF (NOPEN.LE.NMAX) LIST(NOPEN) = I + 7770
         END IF
 10   CONTINUE
      DO 20 I=NOPEN+1,NMAX
         LIST(I) = 0
 20   CONTINUE
C
      END
