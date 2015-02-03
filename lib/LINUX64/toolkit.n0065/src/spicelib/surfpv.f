C$Procedure      SURFPV ( Surface point and velocity )
 
      SUBROUTINE SURFPV ( STVRTX, STDIR, A, B, C, STX, FOUND )
      IMPLICIT NONE
 
C$ Abstract
C
C     Find the state (position and velocity) of the surface intercept 
C     defined by a specified ray, ray velocity, and ellipsoid.
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
C     ELLIPSOID
C     GEOMETRY
C
C$ Declarations

      DOUBLE PRECISION      STVRTX ( 6 )
      DOUBLE PRECISION      STDIR  ( 6 )
      DOUBLE PRECISION      A
      DOUBLE PRECISION      B
      DOUBLE PRECISION      C
      DOUBLE PRECISION      STX    ( 6 )
      LOGICAL               FOUND
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     STVRTX     I   State of ray's vertex.
C     STDIR      I   State of ray's direction vector.
C     A          I   Length of ellipsoid semi-axis along the x-axis.
C     B          I   Length of ellipsoid semi-axis along the y-axis.
C     C          I   Length of ellipsoid semi-axis along the z-axis.
C     STX        O   State of surface intercept.
C     FOUND      O   Flag indicating whether intercept state was found.
C
C$ Detailed_Input
C
C     STVRTX         is the state of a ray's vertex. The first three
C                    components of STVRTX are the vertex's x, y, and z
C                    position components; the vertex's x, y, and z
C                    velocity components follow.
C
C                    The reference frame relative to which STVRTX is
C                    specified has axes aligned with with those of a
C                    triaxial ellipsoid. See the description below of
C                    the arguments A, B, and C.
C
C                    The vertex may be inside or outside of this
C                    ellipsoid, but not on it, since the surface 
C                    intercept is a discontinuous function at
C                    vertices on the ellipsoid's surface.
C
C                    No assumption is made about the units of length
C                    and time, but these units must be consistent with
C                    those of the other inputs.
C
C
C     STDIR          is the state of the input ray's direction vector.
C                    The first three components of STDIR are a non-zero
C                    vector giving the x, y, and z components of the
C                    ray's direction; the direction vector's x, y, and
C                    z velocity components follow.
C
C                    STDIR is specified relative to the same reference
C                    frame as is STVRTX.
C
C
C     A,
C     B,
C     C              are, respectively, the lengths of a triaxial
C                    ellipsoid's semi-axes lying along the x, y, and
C                    z axes of the reference frame relative to which
C                    STVRTX and STDIR are specified.
C                    
C$ Detailed_Output
C
C     STX            is the state of the intercept of the input ray on
C                    the surface of the input ellipsoid. The first
C                    three components of STX are the intercept's x, y,
C                    and z position components; the intercept's x, y,
C                    and z velocity components follow.
C
C                    STX is specified relative to the same reference
C                    frame as are STVRTX and STDIR.
C
C                    STX is defined if and only if both the intercept
C                    and its velocity are computable, as indicated by
C                    the output argument FOUND.
C
C                    The position units of STX are the same as those of
C                    STVRTX, STDIR, and A, B, and C. The time units are
C                    the same as those of STVRTX and STDIR.
C
C
C     FOUND          is a logical flag indicating whether STX is
C                    defined. FOUND is .TRUE. if and only if both the
C                    intercept and its velocity are computable. Note
C                    that in some cases the intercept may computable
C                    while the velocity is not; this can happen for
C                    near-tangency cases.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the input ray's direction vector is the zero vector, then
C         a routine in the call tree of this routine will signal
C         an error.
C
C     2)  If any of the ellipsoid's axis lengths is nonpositive,
C         a routine in the call tree of this routine will signal
C         an error.
C
C     3)  If the vertex of the ray is on the ellipsoid,
C         the error SPICE(INVALIDVERTEX) is signaled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     The position and velocity of the ray's vertex as well as the
C     ray's direction vector and velocity vary with time. The
C     inputs to SURFPV may be considered the values of these
C     vector functions at a particular time, say t0. Thus
C
C        State of vertex:            STVRTX = ( V(t0), V'(t0) )
C
C        State of direction vector:  STDIR  = ( D(t0), D'(t0) )
C
C     To determine the intercept point, W(t0), we simply compute the
C     intersection of the ray originating at V(t0) in the direction of
C     D(t0) with the ellipsoid
C
C          2      2      2
C         x      y      z
C        --- +  --- +  ---  =  1
C          2      2      2
C         A      B      C
C
C     W(t) is the path of the intercept point along the surface of
C     the ellipsoid. To determine the velocity of the intercept point,
C     we need to take the time derivative of W(t), and evaluate it at
C     t0. Unfortunately W(t) is a complicated expression, and its
C     derivative is even more complicated.
C
C     However, we know that the derivative of W(t) at t0, W'(t0), is
C     tangent to W(t) at t0. Thus W'(t0) lies in the plane that is
C     tangent to the ellipsoid at t0. Let X(t) be the curve in the
C     tangent plane that represents the intersection of the ray
C     emanating from V(t0) with direction D(t0) with that tangent
C     plane.
C
C        X'(t0) = W'(t0)
C
C     The expression for X'(t) is much simpler than that of W'(t);
C     SURFPV evaluates X'(t) at t0.
C
C
C     Derivation of X(t) and X'(t)
C     ----------------------------------------------------------------
C
C     W(t0) is the intercept point. Let N be a surface normal at I(t0).
C     Then the tangent plane at W(t0) is the set of points X(t) such
C     that
C
C        < X(t) - I(t0), N > = 0
C
C     X(t) can be expressed as the vector sum of the vertex
C     and some scalar multiple of the direction vector,
C
C        X(t) = V(t) + s(t) * D(t)
C
C     where s(t) is a scalar function of time. The derivative of
C     X(t) is given by
C
C        X'(t) = V'(t)  +  s(t) * D'(t)  +  s'(t) * D(t)
C
C     We have V(t0), V'(t0), D(t0), D'(t0), W(t0), and N, but to
C     evaluate X'(t0), we need s(t0) and s'(t0). We derive an
C     expression for s(t) as follows.
C
C     Because X(t) is in the tangent plane, it must satisfy
C
C        < X(t) - W(t0), N > = 0.
C
C     Substituting the expression for X(t) into the equation above
C     gives
C
C        < V(t) + s(t) * D(t) - W(t0), N > = 0.
C
C     Thus
C
C        < V(t) - W(t0), N >  +  s(t) * < D(t), N > = 0,
C
C     and
C                    < V(t) - W(t0), N >
C        s(t)  =  -  -------------------
C                        < D(t), N >
C
C     The derivative of s(t) is given by
C
C        s'(t) =
C
C            < D(t),N > * < V'(t),N >  -  < V(t)-W(t0),N > * < D'(t),N >
C        -   -----------------------------------------------------------
C                                             2
C                                  < D(t), N >
C
C$ Examples
C
C
C     The numerical results shown for these examples may differ across
C     platforms. The results depend on the compiler and supporting
C     libraries, and the machine specific arithmetic implementation.
C
C
C     1) Illustrate the role of the ray vertex velocity and 
C        ray direction vector velocity via several simple cases. Also
C        show the results of a near-tangency computation.
C
C
C     Example code begins here.
C
C
C           PROGRAM EX1
C           IMPLICIT NONE
C
C           CHARACTER*(*)         F1
C           PARAMETER           ( F1 = '(A,3E23.16)' )
C
C           DOUBLE PRECISION      A
C           DOUBLE PRECISION      B
C           DOUBLE PRECISION      C
C           DOUBLE PRECISION      STVRTX ( 6 )
C           DOUBLE PRECISION      STDIR  ( 6 )
C           DOUBLE PRECISION      STX    ( 6 )
C
C           INTEGER               I
C
C           LOGICAL               FOUND
C
C           A      = 1.D0
C           B      = 2.D0
C           C      = 3.D0
C
C           WRITE (*,*) ' '
C           WRITE (*,*) 'Ellipsoid radii:'
C           WRITE (*,*) '     A = ', A
C           WRITE (*,*) '     B = ', B
C           WRITE (*,*) '     C = ', C
C
C           WRITE (*,*) ' '
C           WRITE (*,*) 'Case 1: Vertex varies, direction is constant'
C           WRITE (*,*) ' '
C
C           STVRTX( 1 ) =  2.D0
C           STVRTX( 2 ) =  0.D0
C           STVRTX( 3 ) =  0.D0
C           STVRTX( 4 ) =  0.D0
C           STVRTX( 5 ) =  0.D0
C           STVRTX( 6 ) =  3.D0
C
C
C           STDIR ( 1 ) = -1.D0
C           STDIR ( 2 ) =  0.D0
C           STDIR ( 3 ) =  0.D0
C           STDIR ( 4 ) =  0.D0
C           STDIR ( 5 ) =  0.D0
C           STDIR ( 6 ) =  0.D0
C
C           WRITE (*,* ) 'Vertex:'
C           WRITE (*,F1) ' ', ( STVRTX(I), I = 1,3 )
C           WRITE (*,* ) 'Vertex velocity:'
C           WRITE (*,F1) ' ', ( STVRTX(I), I = 4,6 )
C           WRITE (*,* ) 'Direction:'
C           WRITE (*,F1) ' ', ( STDIR(I),  I = 1,3 )
C           WRITE (*,* ) 'Direction velocity:'
C           WRITE (*,F1) ' ', ( STDIR(I),  I = 4,6 )
C
C           CALL SURFPV ( STVRTX, STDIR, A, B, C, STX, FOUND )
C
C           IF ( .NOT. FOUND ) THEN
C              WRITE (*,*) ' No intercept state found.'
C           ELSE
C              WRITE (*,* ) 'Intercept:'
C              WRITE (*,F1) ' ', ( STX(I),  I = 1,3 )
C              WRITE (*,* ) 'Intercept velocity:'
C              WRITE (*,F1) ' ', ( STX(I),  I = 4,6 )
C              WRITE (*,* ) ' '
C           END IF
C
C           WRITE (*,*) ' '
C           WRITE (*,*) 'Case 2: Vertex and direction both vary'
C           WRITE (*,*) ' '
C
C           STDIR ( 6 ) =  4.D0
C
C           WRITE (*,* ) 'Vertex:'
C           WRITE (*,F1) ' ', ( STVRTX(I), I = 1,3 )
C           WRITE (*,* ) 'Vertex velocity:'
C           WRITE (*,F1) ' ', ( STVRTX(I), I = 4,6 )
C           WRITE (*,* ) 'Direction:'
C           WRITE (*,F1) ' ', ( STDIR(I),  I = 1,3 )
C           WRITE (*,* ) 'Direction velocity:'
C           WRITE (*,F1) ' ', ( STDIR(I),  I = 4,6 )
C
C           CALL SURFPV ( STVRTX, STDIR, A, B, C, STX, FOUND )
C
C           IF ( .NOT. FOUND ) THEN
C              WRITE (*,*) ' No intercept state found.'
C           ELSE
C              WRITE (*,* ) 'Intercept:'
C              WRITE (*,F1) ' ', ( STX(I),  I = 1,3 )
C              WRITE (*,* ) 'Intercept velocity:'
C              WRITE (*,F1) ' ', ( STX(I),  I = 4,6 )
C              WRITE (*,* ) ' '
C           END IF
C
C           WRITE (*,*) ' '
C           WRITE (*,*) 'Case 3: Vertex and direction both vary;'
C           WRITE (*,*) '        near-tangent case.'
C           WRITE (*,*) ' '
C
C           STVRTX( 3 ) =  C - 1.D-15
C           STVRTX( 6 ) =  1.D299
C           STDIR ( 6 ) =  1.D299
C
C           WRITE (*,* ) 'Vertex:'
C           WRITE (*,F1) ' ', ( STVRTX(I), I = 1,3 )
C           WRITE (*,* ) 'Vertex velocity:'
C           WRITE (*,F1) ' ', ( STVRTX(I), I = 4,6 )
C           WRITE (*,* ) 'Direction:'
C           WRITE (*,F1) ' ', ( STDIR(I),  I = 1,3 )
C           WRITE (*,* ) 'Direction velocity:'
C           WRITE (*,F1) ' ', ( STDIR(I),  I = 4,6 )
C
C           CALL SURFPV ( STVRTX, STDIR, A, B, C, STX, FOUND )
C
C           IF ( .NOT. FOUND ) THEN
C              WRITE (*,*) ' No intercept state found.'
C           ELSE
C              WRITE (*,* ) 'Intercept:'
C              WRITE (*,F1) ' ', ( STX(I),  I = 1,3 )
C              WRITE (*,* ) 'Intercept velocity:'
C              WRITE (*,F1) ' ', ( STX(I),  I = 4,6 )
C              WRITE (*,* ) ' '
C           END IF
C
C           END
C
C
C     When this program was executed on a PC/Linux/g77 platform, the
C     output was: 
C
C
C  Ellipsoid radii:
C       A =   1.
C       B =   2.
C       C =   3.
C
C  Case 1: Vertex varies, direction is constant
C
C  Vertex:
C   0.2000000000000000E+01 0.0000000000000000E+00 0.0000000000000000E+00
C  Vertex velocity:
C   0.0000000000000000E+00 0.0000000000000000E+00 0.3000000000000000E+01
C  Direction:
C  -0.1000000000000000E+01 0.0000000000000000E+00 0.0000000000000000E+00
C  Direction velocity:
C   0.0000000000000000E+00 0.0000000000000000E+00 0.0000000000000000E+00
C  Intercept:
C   0.1000000000000000E+01 0.0000000000000000E+00 0.0000000000000000E+00
C  Intercept velocity:
C   0.0000000000000000E+00 0.0000000000000000E+00 0.3000000000000000E+01
C
C
C  Case 2: Vertex and direction both vary
C
C  Vertex:
C   0.2000000000000000E+01 0.0000000000000000E+00 0.0000000000000000E+00
C  Vertex velocity:
C   0.0000000000000000E+00 0.0000000000000000E+00 0.3000000000000000E+01
C  Direction:
C  -0.1000000000000000E+01 0.0000000000000000E+00 0.0000000000000000E+00
C  Direction velocity:
C   0.0000000000000000E+00 0.0000000000000000E+00 0.4000000000000000E+01
C  Intercept:
C   0.1000000000000000E+01 0.0000000000000000E+00 0.0000000000000000E+00
C  Intercept velocity:
C   0.0000000000000000E+00 0.0000000000000000E+00 0.7000000000000000E+01
C
C
C  Case 3: Vertex and direction both vary;
C          near-tangent case.
C
C  Vertex:
C   0.2000000000000000E+01 0.0000000000000000E+00 0.2999999999999999E+01
C  Vertex velocity:
C   0.0000000000000000E+00 0.0000000000000000E+00 0.1000000000000000+300
C  Direction:
C  -0.1000000000000000E+01 0.0000000000000000E+00 0.0000000000000000E+00
C  Direction velocity:
C   0.0000000000000000E+00 0.0000000000000000E+00 0.1000000000000000+300
C  Intercept:
C   0.2580956827951785E-07 0.0000000000000000E+00 0.2999999999999999E+01
C  Intercept velocity:
C  -0.3874532036207665+307 0.0000000000000000E+00 0.2999999974190432+300
C
C
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C     J.E. McLean    (JPL)
C     W.L. Taber     (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 31-MAR-2009 (NJB) (JEM) (WLT)
C
C-&
 
C$ Index_Entries
C
C     ellipsoid surface point and velocity
C
C-&
 
C
C     SPICELIB functions
C
      LOGICAL               FAILED
      LOGICAL               RETURN
 
      DOUBLE PRECISION      DPMAX
      DOUBLE PRECISION      VDOT
      DOUBLE PRECISION      VNORM
 
C
C     Local parameters
C 
      DOUBLE PRECISION      MARGIN
      PARAMETER           ( MARGIN = 10.D0 )

C
C     Local variables
C
 
      DOUBLE PRECISION      DSNUM
      DOUBLE PRECISION      DU     ( 3 )
      DOUBLE PRECISION      DV     ( 3 )
      DOUBLE PRECISION      LEVEL
      DOUBLE PRECISION      M
      DOUBLE PRECISION      N      ( 3 )
      DOUBLE PRECISION      R
      DOUBLE PRECISION      SECOND ( 3 )
      DOUBLE PRECISION      STDHAT ( 6 )
      DOUBLE PRECISION      THIRD  ( 3 )
      DOUBLE PRECISION      U      ( 3 )
      DOUBLE PRECISION      UDN
      DOUBLE PRECISION      V      ( 3 )
      DOUBLE PRECISION      VMX    ( 3 )
      DOUBLE PRECISION      X      ( 3 )
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'SURFPV' )

C
C     Determine the ellipsoid surface intercept point of the ray
C     emanating from the observer in the direction of D. We'll call it
C     X and it will go in the first three elements of STX once we
C     determine the velocity. If there is no intersection, we check
C     out.
C
C     SURFPT takes care of some error checking too. It signals an error
C     if D is the zero vector or if A, B, or C are bad axis lengths.
C
      CALL SURFPT ( STVRTX, STDIR, A, B, C, X, FOUND )
 
      IF ( FAILED()  .OR.  ( .NOT. FOUND )  ) THEN
         CALL CHKOUT ( 'SURFPV' )
         RETURN
      END IF

C
C     No result has been found, since we don't know whether the
C     intercept velocity is computable.
C     
      FOUND = .FALSE.
 
C
C     Compute the state of a unit vector parallel to the ray's
C     direction "D." We know that the norm of D is not zero because
C     SURFPT checked it.
C
      CALL DVHAT ( STDIR, STDHAT )
 
C
C     The velocity vector of the intercept point goes in the last three
C     elements of STX. Let
C
C        X = W(t0)               DX = dX/dt at t0
C        V = V(t0)               DV = dV/dt at t0
C        U = D(t0) / ||D(t0)||   DU = d ( D(t)/||D(t)|| )/dt at t0
C
C     and N be the unit normal to the ellipsoid surface at X.
C     Then, from the derivation in $ Particulars above,
C
C           DX  =
C
C
C            < V-X,N >       < U,N > < DV,N > - < V-X,N > < DU,N >
C      DV -  --------- DU -  ------------------------------------- U
C             < U,N >                            2
C                                          < U,N >
C
C     Compute the unit normal at the intercept point, and unpack
C     the input states into V, U, DV, and DU. Let V-X = VMX.
C
      CALL SURFNM ( A, B, C, X, N )
 
      CALL VEQU ( STVRTX,    V )
      CALL VEQU ( STDHAT,    U )
 
      CALL VEQU ( STVRTX(4), DV )
      CALL VEQU ( STDHAT(4), DU )
 
      CALL VSUB ( V, X, VMX )

C
C     Reject the vertex if it's on the ellipsoid. 
C     We check this by determining whether the transformed
C     vertex is on or in the unit sphere.
C
      LEVEL =  ( V(1)/A )**2  +  ( V(2)/B )**2  +  ( V(3)/C )**2

      IF ( LEVEL .EQ. 1.D0 ) THEN

         CALL SETMSG ( 'Ray''s vertex (# # #) has '
     .   //            'level surface parameter #. Vertex '
     .   //            'must not be on the ellipsoid.'     )
         CALL ERRDP  ( '#', V(1)                           )
         CALL ERRDP  ( '#', V(2)                           )
         CALL ERRDP  ( '#', V(3)                           )
         CALL ERRDP  ( '#', LEVEL                          )
         CALL SIGERR ( 'SPICE(INVALIDVERTEX)'              )
         CALL CHKOUT ( 'SURFPV'                            )
         RETURN

      END IF
 
C
C     As the intercept point nears the limb, its velocity may tend to
C     infinity. We must check the value of < U,N > before dividing by
C     it. If the intercept point is on the limb, then < U,N > = 0. If
C     it is near the limb, < U,N > may be so small that dividing by it
C     would result in a number that is greater than the maximum double
C     precision number for the computer.
C
      UDN = VDOT( U, N )

      IF ( UDN .EQ. 0.D0 ) THEN
C
C        The intercept point is on the limb, so its velocity
C        is not defined. This means we can't "find" the state
C        of the intercept point.
C
         CALL CHKOUT ( 'SURFPV' )
         RETURN

      END IF
 
C
C     Evaluate the second term of the equation for DX, but don't
C     divide by < U,N > just yet.
C
      CALL VSCL ( VDOT( VMX, N ),  DU,  SECOND )

C
C                                                         2
C     Evaluate the third term, but don't divide by < U,N >  just yet.
C
      DSNUM = UDN * VDOT( DV, N )  -  VDOT( VMX, N ) * VDOT( DU, N )
 
      CALL VSCL ( DSNUM, U, THIRD )
 
C
C     We'll use the following test.
C
      M = MAX (  VNORM( SECOND ),  VNORM( THIRD ),  1.0D0  )

C
C     If
C
C           M          DPMAX()
C        -------   >   -------
C               2      MARGIN
C        < U,N >       
C
C
C     or equivalently
C
C                               2
C        M  >  DPMAX() * < U,N >  / MARGIN
C
C
C     then the velocity is probably too large to compute. We know that
C     we can perform the multiplication above because U and N are both
C     unit vectors, so the dot product of U and N is less than or equal
C     to one.
C
      IF (  M  .GT.  ( DPMAX() / MARGIN ) * UDN**2  ) THEN

         CALL CHKOUT ( 'SURFPV' )
         RETURN

      END IF
  
C
C     If < U,N > passed the tests above, we can solve for the 
C     intercept velocity.
C
C                                                         2
C        DX =  DV  -  SECOND / < U,N >  -  THIRD / < U,N >
C
C
      R = 1.0D0 / UDN
 
      CALL VLCOM3( 1.0D0, DV, -R, SECOND, -(R**2), THIRD, STX(4) )
 
C
C     Since we could compute the velocity, we can assign the
C     intercept point, and set the found flag to .TRUE.
C
      CALL VEQU ( X, STX )
 
      FOUND = .TRUE.
 
      CALL CHKOUT ( 'SURFPV' )
      RETURN
      END
 
