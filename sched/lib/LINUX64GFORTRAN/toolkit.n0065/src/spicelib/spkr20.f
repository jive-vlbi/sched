C$Procedure      SPKR20 ( SPK, read record from segment, type 20 )
 
      SUBROUTINE SPKR20 ( HANDLE, DESCR, ET, RECORD )
      IMPLICIT NONE
 
C$ Abstract
C
C     Read a single SPK data record from a segment of type 20
C     (Chebyshev, velocity coefficients only).
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
      DOUBLE PRECISION      DESCR    ( 5 )
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      RECORD   ( * )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   File handle.
C     DESCR      I   Segment descriptor.
C     ET         I   Evaluation epoch.
C     RECORD     O   Data record.
C
C$ Detailed_Input
C
C     HANDLE,
C     DESCR       are the file handle and segment descriptor for an SPK
C                 segment of type 20.
C
C     ET          is an epoch for which a data record from the specified
C                 segment is required. ET is expressed as seconds past
C                 J2000 TDB.
C
C$ Detailed_Output
C
C     RECORD      is an array of data from the specified segment which,
C                 when evaluated at epoch ET, will give the state
C                 (position and velocity) of the target body identified
C                 by the input segment descriptor. The descriptor
C                 specifies the center of motion and reference frame of
C                 the state.
C
C                 The structure of the record is as follows:
C
C                    +--------------------------------------+
C                    | record size (excluding this element) |
C                    +--------------------------------------+
C                    | Coverage interval midpoint           |
C                    +--------------------------------------+
C                    | Coverage interval radius             |
C                    +--------------------------------------+
C                    | Coeffs for X velocity component      |
C                    +--------------------------------------+
C                    | Coeffs for Y velocity component      |
C                    +--------------------------------------+
C                    | Coeffs for Z velocity component      |
C                    +--------------------------------------+
C                    | X position component                 |
C                    +--------------------------------------+
C                    | Y position component                 |
C                    +--------------------------------------+
C                    | Z position component                 |
C                    +--------------------------------------+
C
C                 In the above record
C
C                    - Times are expressed as seconds past J2000 TDB.
C                    - Position components have units of km.
C                    - Velocity coefficients have units of km/s.
C
C                 RECORD must be declared by the caller with size large
C                 enough to accommodate the largest record that can be
C                 returned by this routine. See the INCLUDE file
C                 spkrec.inc for the correct record length.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) Any errors that occur while looking up SPK data will be
C        diagnosed by routines in the call tree of this routine.
C
C$ Files
C
C     See argument HANDLE.
C
C$ Particulars
C
C     See the SPK Required Reading file for a description of the
C     structure of a data type 20 (Chebyshev polynomials, velocity
C     only) segment.
C
C$ Examples
C
C     The data returned by the SPKRnn routine is in its rawest form,
C     taken directly from the segment.  As such, it will be meaningless
C     to a user unless he/she understands the structure of the data type
C     completely.  Given that understanding, however, the SPKRxx
C     routines might be used to "dump" and check segment data for a
C     particular epoch.
C
C
C     C
C     C     Get a segment applicable to a specified body and epoch.
C     C
C           CALL SPKSFS ( BODY, ET, HANDLE, DESCR, IDENT, FOUND )
C
C     C
C     C     Look at parts of the descriptor.
C     C
C           CALL DAFUS ( DESCR, 2, 6, DCD, ICD )
C           CENTER = ICD( 2 )
C           REF    = ICD( 3 )
C           TYPE   = ICD( 4 )
C
C           IF ( TYPE .EQ. 20 ) THEN
C              CALL SPKR20 ( HANDLE, DESCR, ET, RECORD )
C                  .
C                  .  Look at the RECORD data.
C                  .
C           END IF
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
C     I.M. Underwood  (JPL)
C
C$ Version
C
C     Version 1.0.0 17-JAN-2014 (NJB) (IMU)
C
C-&
 
C$ Index_Entries
C
C     read record from type_20 spk segment
C
C-&
 
 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      J2000
      DOUBLE PRECISION      SPD

      LOGICAL               RETURN
      
C
C     Local variables
C
      DOUBLE PRECISION      DC       ( 2 )
      DOUBLE PRECISION      DSCALE
      DOUBLE PRECISION      INIT
      DOUBLE PRECISION      INITFR
      DOUBLE PRECISION      INITJD
      DOUBLE PRECISION      INTLEN
      DOUBLE PRECISION      INTRVL
      DOUBLE PRECISION      MID
      DOUBLE PRECISION      POS      ( 3 )
      DOUBLE PRECISION      RADIUS
      DOUBLE PRECISION      RECBEG
      DOUBLE PRECISION      TSCALE

      INTEGER               BEGIN
      INTEGER               END
      INTEGER               I
      INTEGER               IC       ( 6 )
      INTEGER               LOC
      INTEGER               NREC
      INTEGER               NTERMS
      INTEGER               RECADR
      INTEGER               RECNO
      INTEGER               RECSIZ
      INTEGER               SIZE
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'SPKR20' )
C
C     Unpack the segment descriptor.
C
      CALL DAFUS ( DESCR, 2, 6, DC, IC )
 
      BEGIN = IC( 5 )
      END   = IC( 6 )
 
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
      CALL DAFGDA ( HANDLE, END-6, END, RECORD )
 
      DSCALE = RECORD( 1 )
      TSCALE = RECORD( 2 )
      INITJD = RECORD( 3 )
      INITFR = RECORD( 4 )
      INTLEN = RECORD( 5 ) 
      RECSIZ = INT( RECORD( 6 ) )
      NREC   = INT( RECORD( 7 ) )

C
C     NTERMS is the number of velocity coefficients per component,
C     plus 1 (for the position component).
C
      NTERMS = RECSIZ / 3

C
C     Convert the initial epoch and interval length to
C     seconds past J2000 TDB.
C
      INIT   = (  ( INITJD - J2000() )  +  INITFR  ) * SPD()
      INTRVL = INTLEN * SPD()

C
C     Locate the record containing the coefficients to use.
C
      RECNO  = INT( (ET - INIT) / INTRVL ) + 1
      RECNO  = MAX( 1,  MIN( RECNO, NREC )  )
 
C
C     Compute the midpoint and radius of the record at
C     index RECNO. We want to compute the midpoint in such
C     a way that we take advantage of interval lengths that
C     are exactly representable, when we have them.
C
C     RECBEG is the record start time, minus the fractional
C     part of the segment start time, expressed as seconds
C     past J2000. We'll account for the fractional part of the
C     start time below when we compute MID.
C
      RECBEG = (  ( INITJD - J2000() )  +  (RECNO-1)*INTLEN  ) * SPD()

      RADIUS = INTRVL / 2.D0

      MID    = RECBEG + ( INITFR * SPD() ) + RADIUS

C
C     Compute the address of the desired record.
C
      RECADR = ( RECNO - 1 )*RECSIZ + BEGIN
 
C
C     Along with the record, return the size, midpoint, and
C     radius of the record.
C
      RECORD( 1 ) = DBLE( RECSIZ + 2 )
      RECORD( 2 ) = MID
      RECORD( 3 ) = RADIUS
      CALL DAFGDA ( HANDLE, RECADR, RECADR + RECSIZ - 1, RECORD( 4 ) )

C
C     We're going to re-arrange the record: the position components
C     will be transferred to the end of the record, and the record
C     contents will be left-shifted to fill in the free elements.
C 
      DO I = 1, 3
         POS( I ) = RECORD ( 3  +  I*NTERMS )
      END DO


      SIZE = RECSIZ + 3
C
C     Remove the position elements from the record.
C
      DO I = 1, 3
C
C        LOC is the index of the element to delete. After the first
C        removal, we must account for the resulting left shift when
C        calculating the indices of subsequent elements to be removed.
C        
         LOC = 3 + I*NTERMS - (I-1)

         CALL REMLAD ( 1, LOC, RECORD, SIZE )
C
C        Note that SIZE is an in-out argument; on output it indicates
C        the size of the array after removal of the indicated
C        element(s).
C
      END DO

C
C     Convert the position vector to km.
C
      CALL VSCLIP ( DSCALE, POS )

C
C     Append the position to the record. Since we inserted three
C     elements at the start of the record and deleted three position
C     elements, the target index is the same as if we had copied the
C     record directly to the output array.
C
      CALL MOVED ( POS, 3, RECORD(RECSIZ+1) )

C
C     Convert the velocity Chebyshev coefficients to units of km/s.
C
      DO I = 4, RECSIZ
         RECORD(I) = RECORD(I) * (DSCALE/TSCALE)
      END DO

      CALL CHKOUT ( 'SPKR20' )
      RETURN
      END
