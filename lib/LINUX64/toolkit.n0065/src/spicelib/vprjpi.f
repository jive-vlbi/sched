C$Procedure      VPRJPI ( Vector projection onto plane, inverted )
 
      SUBROUTINE VPRJPI ( VIN, PROJPL, INVPL, VOUT, FOUND )
 
C$ Abstract
C
C     Find the vector in a specified plane that maps to a specified
C     vector in another plane under orthogonal projection.
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
C     PLANES
C
C$ Keywords
C
C     GEOMETRY
C     MATH
C     PLANE
C     VECTOR
C
C$ Declarations
 
      INTEGER               UBPL
      PARAMETER           ( UBPL   =    4 )
 
      DOUBLE PRECISION      VIN    (    3 )
      DOUBLE PRECISION      PROJPL ( UBPL )
      DOUBLE PRECISION      INVPL  ( UBPL )
      DOUBLE PRECISION      VOUT   (    3 )
      LOGICAL               FOUND
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     VIN        I   The projected vector.
C     PROJPL     I   Plane containing VIN.
C     INVPL      I   Plane containing inverse image of VIN.
C     VOUT       O   Inverse projection of VIN.
C     FOUND      O   Flag indicating whether VOUT could be calculated.
C
C$ Detailed_Input
C
C     VIN,
C     PROJPL,
C     INVPL          are, respectively, a 3-vector, a SPICELIB plane
C                    containing the vector, and a SPICELIB plane
C                    containing the inverse image of the vector under
C                    orthogonal projection onto PROJPL.
C
C$ Detailed_Output
C
C     VOUT           is the inverse orthogonal projection of VIN.  This
C                    is the vector lying in the plane INVPL whose
C                    orthogonal projection onto the plane PROJPL is
C                    VIN.  VOUT is valid only when FOUND (defined below)
C                    is .TRUE.  Otherwise, VOUT is undefined.
C
C     FOUND          indicates whether the inverse orthogonal projection
C                    of VIN could be computed.  FOUND is .TRUE. if so,
C                    .FALSE. otherwise.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the geometric planes defined by PROJPL and INVPL are
C         orthogonal, or nearly so, the inverse orthogonal projection
C         of VIN may be undefined or have magnitude too large to
C         represent with double precision numbers.  In either such
C         case, FOUND will be set to .FALSE.
C
C     2)  Even when FOUND is .TRUE., VOUT may be a vector of extremely
C         large magnitude, perhaps so large that it is impractical to
C         compute with it.  It's up to you to make sure that this
C         situation does not occur in your application of this routine.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     Projecting a vector orthogonally onto a plane can be thought of
C     as finding the closest vector in the plane to the original vector.
C     This `closest vector' always exists; it may be coincident with the
C     original vector.  Inverting an orthogonal projection means finding
C     the vector in a specified plane whose orthogonal projection onto
C     a second specified plane is a specified vector.  The vector whose
C     projection is the specified vector is the inverse projection of
C     the specified vector, also called the `inverse image under
C     orthogonal projection' of the specified vector.  This routine
C     finds the inverse orthogonal projection of a vector onto a plane.
C
C     Related routines are VPRJP, which projects a vector onto a plane
C     orthogonally, and VPROJ, which projects a vector onto another
C     vector orthogonally.
C
C$ Examples
C
C     1)   Suppose
C
C             VIN    =  ( 0.0, 1.0, 0.0 ),
C
C          and that PROJPL has normal vector
C
C             PROJN  =  ( 0.0, 0.0, 1.0 ).
C
C          Also, let's suppose that INVPL has normal vector and constant
C
C             INVN   =  ( 0.0, 2.0, 2.0 )
C             INVC   =    4.0.
C
C          Then VIN lies on the y-axis in the x-y plane, and we want to
C          find the vector VOUT lying in INVPL such that the orthogonal
C          projection of VOUT the x-y plane is VIN.  Let the notation
C          < a, b > indicate the inner product of vectors a and b.
C          Since every point X in INVPL satisfies the equation
C
C             <  X,  (0.0, 2.0, 2.0)  >  =  4.0,
C
C          we can verify by inspection that the vector
C
C             ( 0.0, 1.0, 1.0 )
C
C          is in INVPL and differs from VIN by a multiple of PROJN.  So
C
C             ( 0.0, 1.0, 1.0 )
C
C          must be VOUT.
C
C          To find this result using SPICELIB, we can create the
C          SPICELIB planes PROJPL and INVPL using the code fragment
C
C             CALL NVP2PL  ( PROJN,  VIN,   PROJPL )
C             CALL NVC2PL  ( INVN,   INVC,  INVPL  )
C
C          and then perform the inverse projection using the call
C
C             CALL VPRJPI ( VIN, PROJPL, INVPL, VOUT )
C
C          VPRJPI will return the value
C
C             VOUT = ( 0.0, 1.0, 1.0 )
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     [1] `Calculus and Analytic Geometry', Thomas and Finney.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.0.0, 17-FEB-2004 (NJB)
C
C        Computation of LIMIT was re-structured to avoid 
C        run-time underflow warnings on some platforms.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 01-NOV-1990 (NJB)
C
C-&
 
C$ Index_Entries
C
C     vector projection onto plane inverted
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 2.0.0, 17-FEB-2004 (NJB)
C
C        Computation of LIMIT was re-structured to avoid 
C        run-time underflow warnings on some platforms.
C        In the revised code, BOUND/DPMAX() is never
C        scaled by a number having absolute value < 1.
C
C-&
 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      DPMAX
      DOUBLE PRECISION      VDOT
 
      LOGICAL               RETURN
 
 
C
C     Local parameters
C
 
C
C     BOUND is used to bound the magnitudes of the numbers that we
C     try to take the reciprocal of, since we can't necessarily invert
C     any non-zero number.  We won't try to invert any numbers with
C     magnitude less than
C
C        BOUND / DPMAX().
C
C     BOUND is chosen somewhat arbitrarily....
C
      DOUBLE PRECISION      BOUND
      PARAMETER           ( BOUND = 10.D0 )
 
 
C
C     Local variables
C
      DOUBLE PRECISION      DENOM
      DOUBLE PRECISION      INVC
      DOUBLE PRECISION      INVN  ( 3 )
      DOUBLE PRECISION      LIMIT
      DOUBLE PRECISION      MULT
      DOUBLE PRECISION      NUMER
      DOUBLE PRECISION      PROJC
      DOUBLE PRECISION      PROJN ( 3 )
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'VPRJPI' )
      END IF
 
C
C     Unpack the planes.
C
      CALL PL2NVC ( PROJPL, PROJN, PROJC )
      CALL PL2NVC ( INVPL,  INVN,  INVC  )

C
C     We'll first discuss the computation of VOUT in the nominal case,
C     and then deal with the exceptional cases.
C
C     When PROJPL and INVPL are not orthogonal to each other, the
C     inverse projection of VIN will differ from VIN by a multiple of
C     PROJN, the unit normal vector to PROJPL.  We find this multiple
C     by using the fact that the inverse projection VOUT satisfies the
C     plane equation for the inverse projection plane INVPL.
C
C        We have
C
C           VOUT = VIN  +  MULT * PROJN;                           (1)
C
C        since VOUT satisfies
C
C           < VOUT, INVN >  =  INVC
C
C        we must have
C
C           <  VIN  +  MULT * PROJN,  INVN  > = INVC
C
C        which in turn implies
C
C
C                     INVC  -  < VIN, INVN >
C           MULT  =  ------------------------.                     (2)
C                        < PROJN, INVN >
C
C        Having MULT, we can compute VOUT according to equation (1).
C
C     Now, if the denominator in the above expression for MULT is zero
C     or just too small, performing the division would cause a
C     divide-by-zero error or an overflow of MULT.  In either case, we
C     will avoid carrying out the division, and we'll set FOUND to
C     .FALSE.
C
 
C
C     Compute the numerator and denominator of the right side of (2).
C
      NUMER  =  INVC - VDOT ( VIN,   INVN )
      DENOM  =         VDOT ( PROJN, INVN )

C
C     If the magnitude of the denominator is greater than the absolute
C     value of
C
C                    BOUND
C        LIMIT  =  --------- * NUMER,
C                   DPMAX()
C
C     we can safely divide the numerator by the denominator, and the
C     magnitude of the result will be no greater than
C
C         DPMAX()
C        --------- .
C          BOUND
C
C     Note that we have ruled out the case where NUMER and DENOM are
C     both zero by insisting on strict inequality in the comparison of
C     DENOM and LIMIT.
C
C     We never set LIMIT smaller than BOUND/DPMAX(), since
C     the computation using NUMER causes underflow to be signaled
C     on some systems.
C
      IF ( ABS(NUMER) .LT. 1.D0 ) THEN
         LIMIT  =           BOUND / DPMAX()
      ELSE
         LIMIT  =  ABS (  ( BOUND / DPMAX() ) * NUMER  )
      END IF

 
      IF (  ABS (DENOM) .GT. LIMIT  ) THEN
C
C        We can find VOUT after all.
C
         MULT = NUMER / DENOM

         CALL VLCOM ( 1.0D0, VIN, MULT, PROJN, VOUT )
 
         FOUND = .TRUE.
 
      ELSE
C
C        No dice.
C
         FOUND = .FALSE.
 
      END IF
 
 
      CALL CHKOUT ( 'VPRJPI' )
      RETURN
      END
