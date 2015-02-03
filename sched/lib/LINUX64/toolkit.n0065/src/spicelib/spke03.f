C$Procedure      SPKE03 ( S/P Kernel, evaluate, type 3 )
 
      SUBROUTINE SPKE03 ( ET, RECORD, STATE )
 
C$ Abstract
C
C     Evaluate a single SPK data record from a segment of type 3
C     (Chebyshev Polynomials, position and velocity).
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
 
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      RECORD   ( * )
      DOUBLE PRECISION      STATE    ( 6 )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     ET         I   Evaluation epoch.
C     RECORD     I   Data record.
C     STATE      O   State (position and velocity).
C
C$ Detailed_Input
C
C     ET          is the epoch at which a state vector or Euler angle
C                 state is to be computed. The epoch is represented as
C                 seconds past J2000 TDB.
C
C     RECORD      is a data record which, when evaluated at epoch ET,
C                 will yield three function components and their
C                 derivatives with respect to time. The record
C                 structure for SPK type 3 data is:
C
C                    +--------------------------------------+
C                    | record size (excluding this element) |
C                    +--------------------------------------+
C                    | Coverage interval midpoint           |
C                    +--------------------------------------+
C                    | Coverage interval radius             |
C                    +--------------------------------------+
C                    | Coeffs for X position component      |
C                    +--------------------------------------+
C                    | Coeffs for Y position component      |
C                    +--------------------------------------+
C                    | Coeffs for Z position component      |
C                    +--------------------------------------+
C                    | Coeffs for X velocity component      |
C                    +--------------------------------------+
C                    | Coeffs for Y velocity component      |
C                    +--------------------------------------+
C                    | Coeffs for Z velocity component      |
C                    +--------------------------------------+
C
C                 In the above record
C
C                    - Times are expressed as seconds past J2000 TDB.
C                    - Position components have units of km.
C                    - Velocity components have units of km/s.
C
C                 RECORD must be declared by the caller with size large
C                 enough to accommodate the largest record that can be
C                 returned by this routine. See the INCLUDE file
C                 spkrec.inc for the correct record length.
C
C$ Detailed_Output
C
C     STATE       is the state. In order, X, Y, Z, X', Y', and Z'.
C                 Units are km and km/sec.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If the input record contains an invalid coefficient count, 
C        the error SPICE(INVALIDCOUNT) will be signaled.
C
C     2) If the input record contains invalid domain transformation
C        parameters, the error will be diagnosed by a routine in the
C        call tree of this routine.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     The exact format and structure of type 3 (Chebyshev polynomials,
C     position and velocity) segments are described in the SPK
C     Required Reading file.
C
C     A type 3 segment contains six sets of Chebyshev coefficients,
C     one set each for the position coordinates X, Y, and Z, and one
C     set each for the velocity coordinates X', Y', and Z'.  SPKE03
C     calls the routine CHBVAL to evaluate each polynomial, and arrive
C     at the complete state.
C
C$ Examples
C
C     The SPKEnn routines are almost always used in conjunction with
C     the corresponding SPKRnn routines, which read the records from
C     SPK files.
C
C     The data returned by the SPKRnn routine is in its rawest form,
C     taken directly from the segment.  As such, it will be meaningless
C     to a user unless he/she understands the structure of the data type
C     completely.  Given that understanding, however, the SPKRnn
C     routines might be used to examine raw segment data before
C     evaluating it with the SPKEnn routines.
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
C           IF ( TYPE .EQ. 3 ) THEN
C
C              CALL SPKR03 ( HANDLE, DESCR, ET, RECORD )
C                  .
C                  .  Look at the RECORD data.
C                  .
C              CALL SPKE03 ( ET, RECORD, STATE )
C                  .
C                  .  Check out the evaluated state.
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
C     R.E. Thurman    (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.0.0, 31-DEC-2013 (NJB)
C
C        Added error checks for invalid coefficient counts
C        and invalid interval radius. Changed error handling
C        style to "discovery." Enhanced header documentation.
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
C     evaluate type_3 spk segment
C
C-&
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
C
C     Local variables
C
      INTEGER               NCOF
      INTEGER               DEGP
      INTEGER               COFLOC
      INTEGER               I
 
C
C     Use discovery check-in.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF
 
C
C     The first number in the record is the record size.  Following it
C     are two numbers that will be used later, then the six sets of
C     coefficients.  The number of coefficients for each quantity can
C     be determined from the record size, since there are the same
C     number of coefficients for each quantity.
C
      NCOF = ( INT( RECORD( 1 ) ) - 2 ) / 6
 
      IF ( NCOF .LT. 1 ) THEN

         CALL CHKIN  ( 'SPKE03'                                     )
         CALL SETMSG ( 'The input record''s coefficient count NCOF '
     .   //            'should be positive but was #.'              )
         CALL ERRINT ( '#', NCOF                                    )
         CALL SIGERR ( 'SPICE(INVALIDCOUNT)'                        )
         CALL CHKOUT ( 'SPKE03'                                     )
         RETURN

      END IF

C
C     Check the radius of the domain interval.
C
      IF ( RECORD(3) .LE. 0.D0 )  THEN

         CALL CHKIN  ( 'SPKE03'                            )
         CALL SETMSG ( 'Interval radius must be positive '
     .   //            'but was #.'                        )
         CALL ERRDP  ( '#', RECORD(3)                      )
         CALL SIGERR ( 'SPICE(INVALIDRADIUS)'              )
         CALL CHKOUT ( 'SPKE03'                            )
         RETURN

      END IF

C
C     The degree of each polynomial is one less than the number of
C     coefficients.
C
      DEGP = NCOF - 1
 
C
C     Call CHBVAL once for each quantity to evaluate the position
C     and velocity values.
C
      DO I = 1, 6
C
C        The coefficients for each variable are located contiguously,
C        following the first three words in the record.
C
         COFLOC = NCOF*( I - 1 ) + 4
 
C
C        CHBVAL needs as input the coefficients, the degree of the
C        polynomial, the epoch, and also two variable transformation
C        parameters, which are located, in our case, in the second and
C        third slots of the record.
C
         CALL CHBVAL ( RECORD( COFLOC ), DEGP, RECORD( 2 ), ET,
     .                 STATE( I )                               )
      END DO
 
      RETURN
      END
