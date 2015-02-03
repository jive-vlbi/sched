C$Procedure      SPKS20 ( S/P Kernel, subset, type 20 )
 
      SUBROUTINE SPKS20 ( HANDLE, BADDR, EADDR, BEGIN, END )
      IMPLICIT NONE
 
C$ Abstract
C
C     Extract a subset of the data in a SPK segment of type 20
C     into a new segment.
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C     SPK
C
C$ Keywords
C
C     EPHEMERIS
C
C$ Declarations
 
      INTEGER               HANDLE
      INTEGER               BADDR
      INTEGER               EADDR
      DOUBLE PRECISION      BEGIN
      DOUBLE PRECISION      END
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle of source segment.
C     BADDR      I   Beginning address of source segment.
C     EADDR      I   Ending address of source segment.
C     BEGIN      I   Beginning (initial epoch) of subset.
C     END        I   End (final epoch) of subset.
C
C$ Detailed_Input
C
C     HANDLE,
C     BADDR,
C     EADDR       are the file handle assigned to a SPK file, and the
C                 beginning and ending addresses of a segment within
C                 the file.  Together they determine a complete set of
C                 ephemeris data, from which a subset is to be
C                 extracted.
C
C     BEGIN,
C     END         are the initial and final epochs (ephemeris time)
C                 of the subset to be extracted.
C
C$ Detailed_Output
C
C     None. This routine writes data to the SPK file currently
C     open for write access.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  Any errors that occur while reading data from the source SPK
C         file will be diagnosed by routines in the call tree of this
C         routine.
C
C     2)  Any errors that occur while writing data to the output SPK
C         file will be diagnosed by routines in the call tree of this
C         routine.
C
C$ Files
C
C     See argument HANDLE.
C
C$ Particulars
C
C     The exact structure of a segment of data type 20 is detailed in
C     the SPK Required Reading file.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     NAIF Document 168.0, "S- and P- Kernel (SPK) Specification and
C     User's Guide"
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C     R.E. Thurman    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 23-DEC-2013 (NJB) (RET)
C
C-&
 
C$ Index_Entries
C
C     subset type_20 spk segment
C
C-&
 
 
 
 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      J2000
      DOUBLE PRECISION      SPD

      LOGICAL               RETURN
 
C
C     Local parameters
C
      INTEGER               BUFSIZ
      PARAMETER           ( BUFSIZ = 100 )

C
C     Local variables
C
      DOUBLE PRECISION      BTIME
      DOUBLE PRECISION      DATA   ( BUFSIZ )
      DOUBLE PRECISION      DSCALE
      DOUBLE PRECISION      TSCALE
      DOUBLE PRECISION      INITJD
      DOUBLE PRECISION      INITFR
      DOUBLE PRECISION      INTLEN
      DOUBLE PRECISION      INTRVL
      DOUBLE PRECISION      SUBBEG
      DOUBLE PRECISION      SUBIFR
      DOUBLE PRECISION      SUBIJD
      
      INTEGER               RECSIZ
      INTEGER               NREC
      INTEGER               FIRST
      INTEGER               LAST
      INTEGER               REMAIN
      INTEGER               ADDR
      INTEGER               MOVE
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'SPKS20' )
 
C
C     The segment is made up of a number of logical records, each
C     having the same size, and covering the same length of time.
C
C     We can determine which record to return using the input epoch,
C     the integer and fractional parts of the initial time of the first
C     record's coverage interval, and the length of the interval
C     covered by each record. These constants are located at the end of
C     the segment, along with the size of each logical record and the
C     total number of records.
C
C     For convenience, we'll fetch the segment's distance and time
C     scales in the same call.
C
      CALL DAFGDA ( HANDLE, EADDR-6, EADDR, DATA )
 
      DSCALE =      DATA( 1 )
      TSCALE =      DATA( 2 )
      INITJD =      DATA( 3 )
      INITFR =      DATA( 4 )
      INTLEN =      DATA( 5 )
      RECSIZ = INT( DATA( 6 ) )
      NREC   = INT( DATA( 7 ) )
 
      BTIME  = ( ( INITJD - J2000() ) + INITFR ) * SPD()
      INTRVL = INTLEN * SPD()

      FIRST = INT( (BEGIN - BTIME) / INTRVL ) + 1
      FIRST = MAX( 1,  MIN( FIRST, NREC )  )
 
      LAST  = INT( (END   - BTIME) / INTRVL ) + 1
      LAST  = MAX( 1,  MIN( LAST,  NREC )  )
 
C
C     The number of records to be moved.
C
      NREC = LAST - FIRST + 1
 
C
C     We're going to move the data in chunks of BUFSIZ d.p. words.
C     Compute the number of words left to move, the address of the
C     beginning of the records to move, and the number to move this
C     time.
C
      REMAIN = NREC * RECSIZ
      ADDR   = BADDR + ( FIRST - 1 )*RECSIZ
      MOVE   = MIN( BUFSIZ, REMAIN )
 
      DO WHILE ( REMAIN .GT. 0 )
 
         CALL DAFGDA ( HANDLE, ADDR, ADDR + MOVE - 1, DATA )
         CALL DAFADA ( DATA, MOVE )
         REMAIN = REMAIN - MOVE
         ADDR   = ADDR   + MOVE
         MOVE   = MIN( BUFSIZ, REMAIN )
 
      END DO
 
C
C     That's all the records we have to move. But there are still seven
C     final numbers left to write:
C
C        1)  The distance scale (DSCALE).
C        2)  The time scale (TSCALE).
C        3)  The initial integer Julian date of the start time of the
C            first record.
C        4)  The fractional part of the state time of the first 
C            record.
C        5)  The time interval length for each polynomial in days
C            (INTLEN).
C        6)  The record size (RECSIZ).
C        7)  The number of records (NREC).
C
C
C
C     Let SUBBEG be the subset begin time expressed as a TDB Julian
C     date.
C      
      SUBBEG =  J2000()  +  (  BTIME + ( FIRST - 1 )*INTRVL  ) / SPD()

      SUBIJD =  AINT( SUBBEG )
      SUBIFR =  SUBBEG - SUBIJD

      DATA( 1 ) = DSCALE
      DATA( 2 ) = TSCALE
      DATA( 3 ) = SUBIJD
      DATA( 4 ) = SUBIFR
      DATA( 5 ) = INTLEN
      DATA( 6 ) = DBLE( RECSIZ )
      DATA( 7 ) = DBLE( NREC   )

      CALL DAFADA ( DATA, 7 )
 
      CALL CHKOUT ( 'SPKS20' )
      RETURN
      END
