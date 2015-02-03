C$Procedure      SPKS03 ( S/P Kernel, subset, type 3 )
 
      SUBROUTINE SPKS03 ( HANDLE, BADDR, EADDR, BEGIN, END )
      IMPLICIT NONE
 
C$ Abstract
C
C     Extract a subset of the data in a SPK segment of type 3 (Chebyshev
C     polynomials, position and velocity) into a new segment.
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
C     None.
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
C     The exact structure of a segment of data type 3 (Chebyshev
C     polynomials, position and velocity) is detailed in the SPK
C     Required Reading file.
C
C     On not so close inspection, it will be noted that SPKS03 is
C     identical to SPKS02.
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
C     R.E. Thurman    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.1, 31-DEC-2013 (NJB)
C
C        Enhanced header documentation.
C
C-    SPICELIB Version 1.1.0, 07-SEP-2001 (EDW)
C
C        Replaced DAFRDA call with DAFGDA.
C        Added IMPLICIT NONE.
C
C-    SPICELIB Version 1.0.3, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.2, 23-AUG-1991 (HAN)
C
C        SPK03 was removed from the Required_Reading section of the
C        header. The information in the SPK03 Required Reading file
C        is now part of the SPK Required Reading file.
C
C-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
C
C        Literature references added to the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (RET)
C
C-&
 
C$ Index_Entries
C
C     subset type_3 spk segment
C
C-&
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
C
C     Local variables
C
      DOUBLE PRECISION      DATA    ( 50 )
      DOUBLE PRECISION      INIT
      DOUBLE PRECISION      INTLEN
 
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
      ELSE
         CALL CHKIN ( 'SPKS03' )
      END IF
 
C
C     The segment is made up of a number of logical records, each
C     having the same size, and covering the same length of time.
C
C     We can determine which records to extract by comparing the input
C     epochs with the initial time of the segment and the length of the
C     interval covered by each record.  These final two constants are
C     located at the end of the segment, along with the size of each
C     logical record and the total number of records.
C
      CALL DAFGDA ( HANDLE, EADDR-3, EADDR, DATA )
 
      INIT   =      DATA( 1 )
      INTLEN =      DATA( 2 )
      RECSIZ = INT( DATA( 3 ) )
      NREC   = INT( DATA( 4 ) )
 
      FIRST = INT( (BEGIN - INIT) / INTLEN ) + 1
      FIRST = MIN( FIRST, NREC )
 
      LAST = INT( (END - INIT) / INTLEN ) + 1
      LAST = MIN( LAST, NREC )
 
C
C     The number of records to be moved.
C
      NREC = LAST - FIRST + 1
 
C
C     We're going to move the data in chunks of 50 d.p. words.  Compute
C     the number of words left to move, the address of the beginning
C     of the records to move, and the number to move this time.
C
      REMAIN = NREC * RECSIZ
      ADDR   = BADDR + ( FIRST - 1 )*RECSIZ
      MOVE   = MIN( 50, REMAIN )
 
      DO WHILE ( REMAIN .GT. 0 )
 
         CALL DAFGDA ( HANDLE, ADDR, ADDR + MOVE - 1, DATA )
         CALL DAFADA ( DATA, MOVE )
         REMAIN = REMAIN - MOVE
         ADDR   = ADDR   + MOVE
         MOVE   = MIN( 50, REMAIN )
 
      END DO
 
C
C     That's all the records we have to move. But there are still four
C     final numbers left to write:
C
C        1)  The initial time for the polynomials (INIT).
C        2)  The time interval length for each polynomial (INTLEN).
C        3)  The record size (RECSIZ).
C        4)  The number of records (NREC).
C
C     INIT and NREC will probably be different for the new segment (in
C     fact, NREC has already been changed), the other two will not.
C
      INIT = INIT + ( FIRST - 1 )*INTLEN
 
      DATA( 1 ) = INIT
      DATA( 2 ) = INTLEN
      DATA( 3 ) = DBLE( RECSIZ )
      DATA( 4 ) = DBLE( NREC   )
      CALL DAFADA ( DATA, 4 )
 
      CALL CHKOUT ( 'SPKS03' )
      RETURN
      END
