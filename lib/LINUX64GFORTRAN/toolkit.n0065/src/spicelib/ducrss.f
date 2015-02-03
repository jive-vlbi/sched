C$Procedure      DUCRSS ( Unit Normalized Cross Product and Derivative )
 
      SUBROUTINE DUCRSS ( S1, S2, SOUT )
 
C$ Abstract
C
C     Compute the unit vector parallel to the cross product of
C     two 3-dimensional vectors and the derivative of this unit vector.
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
C     None.
C
C$ Keywords
C
C     VECTOR
C     DERIVATIVE
C
C$ Declarations
 
      DOUBLE PRECISION    S1   ( 6 )
      DOUBLE PRECISION    S2   ( 6 )
      DOUBLE PRECISION    SOUT ( 6 )
 
C$ Brief_I/O
C
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     S1        I   Left hand state for cross product and derivative.
C     S2        I   Right hand state for cross product and derivative.
C     SOUT      O   Unit vector and derivative of the cross product.
C
C$ Detailed_Input
C
C     S1       This may be any state vector.  Typically, this
C              might represent the apparent state of a planet or the
C              Sun, which defines the orientation of axes of
C              some coordinate system.
C
C     S2       Any state vector.
C
C$ Detailed_Output
C
C     SOUT     This variable represents the unit vector parallel to the
C              cross product of the position components of S1 and S2
C              and the derivative of the unit vector.
C
C              If the cross product of the position components is
C              the zero vector, then the position component of the
C              output will be the zero vector.  The velocity component
C              of the output will simply be the derivative of the
C              cross product of the position components of S1 and S2.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C     1) If the position components of S1 and S2 cross together to
C        give a zero vector, the position component of the output
C        will be the zero vector.  The velocity component of the
C        output will simply be the derivative of the cross product
C        of the position vectors.
C
C     2) If S1 and S2 are large in magnitude (taken together,
C        their magnitude surpasses the limit allowed by the
C        computer) then it may be possible to generate a
C        floating point overflow from an intermediate
C        computation even though the actual cross product and
C        derivative may be well within the range of double
C        precision numbers.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     DUCRSS calculates the unit vector parallel to the cross product
C     of two vectors and the derivative of that unit vector.
C
C$ Examples
C
C     One often constructs non-inertial coordinate frames from
C     apparent positions of objects.  However, if one wants to convert
C     states in this non-inertial frame to states in an inertial
C     reference frame, the derivatives of the axes of the non-inertial
C     frame are required.  For example consider an Earth meridian
C     frame defined as follows.
C
C        The z-axis of the frame is defined to be the vector
C        normal to the plane spanned by the position vectors to the
C        apparent Sun and to the apparent body as seen from an observer.
C
C        Let SUN be the apparent state of the Sun and let BODY be the
C        apparent state of the body with respect to the observer.  Then
C        the unit vector parallel to the z-axis of the Earth meridian
C        system and its derivative are given by the call:
C
C        CALL DUCRSS ( SUN, BODY, ZZDOT )
C
C$ Restrictions
C
C     No checking of S1 or S2 is done to prevent floating point
C     overflow. The user is required to determine that the magnitude
C     of each component of the states is within an appropriate range
C     so as not to cause floating point overflow. In almost every case
C     there will be no problem and no checking actually needs to be
C     done.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C     W.L. Taber      (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.2.0, 08-APR-2014 (NJB)
C
C        Now scales inputs to reduce chance of numeric
C        overflow.
C
C-    SPICELIB Version 1.1.1, 22-APR-2010 (NJB)
C
C        Header correction: assertions that the output
C        can overwrite the input have been removed.
C
C-    SPICELIB Version 1.1.0, 30-AUG-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in DVHAT call.
C
C-    SPICELIB Version 1.0.0, 15-JUN-1995 (WLT)
C
C
C-&
 
 
C$ Index_Entries
C
C     Compute a unit cross product and its derivative
C
C-&
 
C
C     Local variables
C
      DOUBLE PRECISION      F1
      DOUBLE PRECISION      F2
      DOUBLE PRECISION      SCLS1  ( 6 )
      DOUBLE PRECISION      SCLS2  ( 6 )
      DOUBLE PRECISION      TMPSTA ( 6 )

C
C     Scale the components of the input states so the states have the
C     same direction and angular rates, but their largest position
C     components have absolute value equal to 1. Do not modify states
C     that have all position components equal to zero.
C      
      F1 = MAX( ABS(S1(1)), ABS(S1(2)), ABS(S1(3)) )
      F2 = MAX( ABS(S2(1)), ABS(S2(2)), ABS(S2(3)) )

      IF ( F1 .GT. 0.D0 ) THEN

         CALL VSCLG ( 1.D0/F1, S1, 6, SCLS1 )
      ELSE
         CALL MOVED ( S1,          6, SCLS1 )
      END IF

      IF ( F2 .GT. 0.D0 ) THEN

         CALL VSCLG ( 1.D0/F2, S2, 6, SCLS2 )
      ELSE
         CALL MOVED ( S2,          6, SCLS2 )
      END IF

C
C     Not much to this.  Just get the cross product and its derivative.
C     Using that, get the associated unit vector and its derivative.
C
      CALL DVCRSS ( SCLS1,  SCLS2, TMPSTA )
      CALL DVHAT  ( TMPSTA,        SOUT   ) 
 
      RETURN
      END
 
