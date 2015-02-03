C$Procedure ZZNRDDP ( Shell for deep space entry points )

      SUBROUTINE ZZNRDDP ( AO , ELEMS , EM, OMGASM, OMGDOT, T, XINC,
     .                     XLL, XLLDOT, XN, XNODES, XNODOT, XNODP  )
      IMPLICIT NONE

C$ Abstract
C
C    This subroutine is a shell for the routines needed by DPSPCE
C    for calculating deep space effects on a vehicle.
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
C     TWO LINE ELEMENTS
C     SPACETRACK
C     DEEP SPACE
C
C$ Declarations

      DOUBLE PRECISION      AO
      DOUBLE PRECISION      ELEMS ( 10 )
      DOUBLE PRECISION      EM
      DOUBLE PRECISION      OMGASM
      DOUBLE PRECISION      OMGDOT
      DOUBLE PRECISION      T
      DOUBLE PRECISION      XINC
      DOUBLE PRECISION      XLL
      DOUBLE PRECISION      XLLDOT
      DOUBLE PRECISION      XN
      DOUBLE PRECISION      XNODES
      DOUBLE PRECISION      XNODOT
      DOUBLE PRECISION      XNODP


C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     AO         I   Entry ZZDPINIT, Original semimajor axis
C     ELEMS      I   Entry ZZDPINIT, Array of orbit elements
C                      "   ZZDPSEC
C     EM         I   Entry ZZDPSEC,  Perturbed eccentricity of the orbit
C                      "   ZZDPPER   at time T
C     OMGASM     I   Entry ZZDPSEC   Perturbed argument of perigee
C                      "   ZZDPPER
C     OMGDOT     I   Entry ZZDPINIT, Time rate of change of arg of
C                      "   ZZDPSEC   perigee
C     T          I   Entry ZZDPSEC,  Time of state evaluation
C                      "   ZZDPPER
C     XINC       I   Entry ZZDPSEC,  Perturbed inclination of the orbit
C                      "   ZZDPPER   plane at time T
C     XLL        I   Entry ZZDPSEC   Long-period periodic term
C     XLLDOT     I   Entry ZZDPINIT, Time rate of change of XLL
C     XN         I   Entry ZZDPSEC   Perturbed mean motion of the orbit
C                                    at time T
C     XNODES     I   Entry ZZDPSEC,  Perturbed argument of ascending
C                      "   ZZDPPER   node
C     XNODOT     I   Entry ZZDPINIT, Time rate of change of mean motion
C     XNODP      I   Entry ZZDPINIT, Original mean motion
C
C$ Detailed_Input
C
C     AO          the original semimajor axis of the orbit.
C
C     ELEMS       is an array containing two-line element data
C                 as prescribed below. The elements XNDD6O and BSTAR
C                 must already be scaled by the proper exponent stored
C                 in the two line elements set.  Moreover, the
C                 various items must be converted to the units shown
C                 here.
C
C                    ELEMS (  1 ) = XNDT2O in radians/minute**2
C                    ELEMS (  2 ) = XNDD6O in radians/minute**3
C                    ELEMS (  3 ) = BSTAR
C                    ELEMS (  4 ) = XINCL  in radians
C                    ELEMS (  5 ) = XNODEO in radians
C                    ELEMS (  6 ) = EO
C                    ELEMS (  7 ) = OMEGAO in radians
C                    ELEMS (  8 ) = XMO    in radians
C                    ELEMS (  9 ) = XNO    in radians/minute
C                    ELEMS ( 10 ) = EPOCH of the elements in seconds
C                                   past ephemeris epoch J2000.
C
C     EM          is the perturbed eccentricity from the mean
C                 eccentricity at epoch at time T.
C
C     OMGASM      the value of the argument of perigee after the
C                 perturbations at the time of interest are
C                 added
C
C     OMGDOT      the time derivative of the argument of perigee
C
C     T           is the total time from the epoch, in minutes, of the
C                 element set at which to calculate the state.
C
C     XINC        is the perturbed inclination of the orbit plane from
C                 the mean inclination at the epoch at time T
C
C     XLL         a long-period periodic term dependent on inclination,
C                 eccentricity and argument of periapsis
C
C     XLLDOT      the time derivative of the XLL long-period term
C
C     XN          is the perturbed mean motion from the 'mean' mean
C                 motion at epoch at time T.
C
C     XNODES      is the value of the argument of the ascending node
C                 after the perturbations at the time of interest are
C                 added.
C
C     XNODOT      the time derivative of the mean motion
C
C     XNODP       original mean motion of the orbit.
C
C$ Detailed_Output
C
C     None
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This subroutine is a shell for the entry points used by the
C     propagator for deep space orbits, where a deep space orbit is one
C     which has a period greater the 225 minutes.  The entry points
C     are
C
C     ZZDPINIT - initialize variables for the deep space regime
C     ZZDPSEC  - calculates and updates secular perturbation terms
C     ZZDPPER  - calculates and updates periodic perturbation terms
C                particularly as caused by the sun and the moon
C
C     The names of several constants defined in the Spacetrack 3 report
C     have been changed.
C
C     D2201 to DG( 1  )
C     D2211 to DG( 2  )
C     D3210 to DG( 3  )
C     D3222 to DG( 4  )
C     D4410 to DG( 5  )
C     D4422 to DG( 6  )
C     D5220 to DG( 7  )
C     D5232 to DG( 8  )
C     D5421 to DG( 9  )
C     D5433 to DG( 10 )
C
C     The names of variables changed from the Spacetrack 3 report
C
C     DEL1  to  DEL( 1 )
C     DEL2  to  DEL( 2 )
C     DEL3  to  DEL( 3 )
C     SSL   to  SSX( 1 )
C     SSG   to  SSX( 2 )
C     SSH   to  SSX( 3 )
C     SSE   to  SSX( 4 )
C     SSI   to  SSX( 5 )
C     OMGDT to  OMGDOT
C
C     The entry point ZZDPPER was modified to insure that the
C     perturbations on the elements are zero at the epoch.  This was
C     not correctly handled in the Spacetrack 3 report.
C
C$ Examples
C
C     Never call this subroutine directly.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     Hoots, Felix R., Ronald L. Roehrich (31 December 1988). "Models
C     for Propagation of NORAD Element Sets". United States Department
C     of Defense Spacetrack Report (3).
C
C     Vallado, David A., Paul Crawford, Richard Hujsak, and
C     Kelso, T. S., "Revisiting Spacetrack Report #3," AIAA/AAS
C     Astrodynamics Specialist Conference, Keystone, CO, Aug 2006.
C
C$ Author_and_Institution
C
C     E.D. Wright      (JPL)
C     W.L. Taber       (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.0.0, 20-JAN-2012 (EDW)
C
C        Eliminated use of the DOPERT boolean in ZZDPINIT.
C        Refer to that entry point "Version" section for details.
C
C        Added proper citation for Spacetrack 3 (Hoots) and
C        Revisiting Spacetrak 3 (Vallado).
C
C        Added proper Declarations section.
C
C-    SPICELIB Version 1.5.1, 19-SEP-2006 (EDW)
C
C        Added text to previously empty Declarations section
C        in ZZDPINIT, ZZDPPER, ZZDPSEC.
C
C-    SPICELIB Version 1.5.0, 20-JAN-1999 (EDW) (WLT)
C
C        OMGDOT, named in an ENTRY point argument list
C        was not passed via an argument list. Solaris exhibited a
C        bus error because of this situation. All ENTRY point
C        arguments are passed only by argument lists and are declared
C        in the umbrella subroutine's, ZZNRDDP, argument list.
C
C        Combined the various SSL, SSG, SSH, SSI, SSE variables into
C        the vector SSX.
C
C        Removed the dependency upon the UTC/ET leapsecond kernel.
C
C        Alphabetized all variable declaration lists.
C
C        All arguments passed through entry points listed as arguments
C        of ZZNRDDP. OMGDT renamed OMGDOT to be consistent with other
C        deep space two line element routines.
C
C-    SPICELIB Version 1.0.0, 1-APR-1997 (EDW)
C
C-&

C$ Index_Entries
C
C     two line element set
C
C-&

C
C     Local variables
C
      DOUBLE PRECISION      A1
      DOUBLE PRECISION      A10
      DOUBLE PRECISION      A2
      DOUBLE PRECISION      A3
      DOUBLE PRECISION      A4
      DOUBLE PRECISION      A5
      DOUBLE PRECISION      A6
      DOUBLE PRECISION      A7
      DOUBLE PRECISION      A8
      DOUBLE PRECISION      A9
      DOUBLE PRECISION      AINV2
      DOUBLE PRECISION      ALFDP
      DOUBLE PRECISION      AQNV
      DOUBLE PRECISION      ATIME
      DOUBLE PRECISION      BETDP
      DOUBLE PRECISION      BFACT
      DOUBLE PRECISION      BSQ
      DOUBLE PRECISION      C
      DOUBLE PRECISION      CC
      DOUBLE PRECISION      COSIQ
      DOUBLE PRECISION      COSIS
      DOUBLE PRECISION      COSOK
      DOUBLE PRECISION      COSOMO
      DOUBLE PRECISION      COSQ
      DOUBLE PRECISION      COSQ2
      DOUBLE PRECISION      CTEM
      DOUBLE PRECISION      DAY
      DOUBLE PRECISION      DELT
      DOUBLE PRECISION      DS50
      DOUBLE PRECISION      E3
      DOUBLE PRECISION      EE2
      DOUBLE PRECISION      EO
      DOUBLE PRECISION      EOC
      DOUBLE PRECISION      EQ
      DOUBLE PRECISION      EQSQ
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      F2
      DOUBLE PRECISION      F220
      DOUBLE PRECISION      F221
      DOUBLE PRECISION      F3
      DOUBLE PRECISION      F311
      DOUBLE PRECISION      F321
      DOUBLE PRECISION      F322
      DOUBLE PRECISION      F330
      DOUBLE PRECISION      F441
      DOUBLE PRECISION      F442
      DOUBLE PRECISION      F522
      DOUBLE PRECISION      F523
      DOUBLE PRECISION      F542
      DOUBLE PRECISION      F543
      DOUBLE PRECISION      FT
      DOUBLE PRECISION      G200
      DOUBLE PRECISION      G201
      DOUBLE PRECISION      G211
      DOUBLE PRECISION      G300
      DOUBLE PRECISION      G310
      DOUBLE PRECISION      G322
      DOUBLE PRECISION      G410
      DOUBLE PRECISION      G422
      DOUBLE PRECISION      G520
      DOUBLE PRECISION      G521
      DOUBLE PRECISION      G532
      DOUBLE PRECISION      G533
      DOUBLE PRECISION      GAM
      DOUBLE PRECISION      OMEGAO
      DOUBLE PRECISION      OXNODE
      DOUBLE PRECISION      PE
      DOUBLE PRECISION      PE0
      DOUBLE PRECISION      PGH
      DOUBLE PRECISION      PGH0
      DOUBLE PRECISION      PH
      DOUBLE PRECISION      PH0
      DOUBLE PRECISION      PINC
      DOUBLE PRECISION      PINC0
      DOUBLE PRECISION      PIX1
      DOUBLE PRECISION      PIX2
      DOUBLE PRECISION      PL
      DOUBLE PRECISION      PL0
      DOUBLE PRECISION      PREEP
      DOUBLE PRECISION      RTEQSQ
      DOUBLE PRECISION      S1
      DOUBLE PRECISION      S2
      DOUBLE PRECISION      S3
      DOUBLE PRECISION      S4
      DOUBLE PRECISION      S5
      DOUBLE PRECISION      S6
      DOUBLE PRECISION      S7
      DOUBLE PRECISION      SE
      DOUBLE PRECISION      SE2
      DOUBLE PRECISION      SE3
      DOUBLE PRECISION      SEL
      DOUBLE PRECISION      SES
      DOUBLE PRECISION      SGH
      DOUBLE PRECISION      SGH2
      DOUBLE PRECISION      SGH3
      DOUBLE PRECISION      SGH4
      DOUBLE PRECISION      SGHL
      DOUBLE PRECISION      SGHS
      DOUBLE PRECISION      SH
      DOUBLE PRECISION      SH2
      DOUBLE PRECISION      SH3
      DOUBLE PRECISION      SHL
      DOUBLE PRECISION      SHS
      DOUBLE PRECISION      SI
      DOUBLE PRECISION      SI2
      DOUBLE PRECISION      SI3
      DOUBLE PRECISION      SIL
      DOUBLE PRECISION      SINI2
      DOUBLE PRECISION      SINIQ
      DOUBLE PRECISION      SINIS
      DOUBLE PRECISION      SINOK
      DOUBLE PRECISION      SINOMO
      DOUBLE PRECISION      SINQ
      DOUBLE PRECISION      SINZF
      DOUBLE PRECISION      SIS
      DOUBLE PRECISION      SL
      DOUBLE PRECISION      SL2
      DOUBLE PRECISION      SL3
      DOUBLE PRECISION      SL4
      DOUBLE PRECISION      SLL
      DOUBLE PRECISION      SLS
      DOUBLE PRECISION      STEM
      DOUBLE PRECISION      STEPN
      DOUBLE PRECISION      STEPP
      DOUBLE PRECISION      TEMP
      DOUBLE PRECISION      TEMP1
      DOUBLE PRECISION      THETA
      DOUBLE PRECISION      THGR
      DOUBLE PRECISION      X1
      DOUBLE PRECISION      X2
      DOUBLE PRECISION      X3
      DOUBLE PRECISION      X4
      DOUBLE PRECISION      X5
      DOUBLE PRECISION      X6
      DOUBLE PRECISION      X7
      DOUBLE PRECISION      X8
      DOUBLE PRECISION      XFACT
      DOUBLE PRECISION      XGH2
      DOUBLE PRECISION      XGH3
      DOUBLE PRECISION      XGH4
      DOUBLE PRECISION      XH2
      DOUBLE PRECISION      XH3
      DOUBLE PRECISION      XI2
      DOUBLE PRECISION      XI3
      DOUBLE PRECISION      XINCL
      DOUBLE PRECISION      XL
      DOUBLE PRECISION      XL2
      DOUBLE PRECISION      XL3
      DOUBLE PRECISION      XL4
      DOUBLE PRECISION      XLAMO
      DOUBLE PRECISION      XLDOT
      DOUBLE PRECISION      XLI
      DOUBLE PRECISION      XLS
      DOUBLE PRECISION      XMAO
      DOUBLE PRECISION      XMO
      DOUBLE PRECISION      XNDDT
      DOUBLE PRECISION      XNDOT
      DOUBLE PRECISION      XNI
      DOUBLE PRECISION      XNO2
      DOUBLE PRECISION      XNODCE
      DOUBLE PRECISION      XNODEO
      DOUBLE PRECISION      XNOI
      DOUBLE PRECISION      XNQ
      DOUBLE PRECISION      XQNCL
      DOUBLE PRECISION      Z1
      DOUBLE PRECISION      Z11
      DOUBLE PRECISION      Z12
      DOUBLE PRECISION      Z13
      DOUBLE PRECISION      Z2
      DOUBLE PRECISION      Z21
      DOUBLE PRECISION      Z22
      DOUBLE PRECISION      Z23
      DOUBLE PRECISION      Z3
      DOUBLE PRECISION      Z31
      DOUBLE PRECISION      Z32
      DOUBLE PRECISION      Z33
      DOUBLE PRECISION      ZCOSG
      DOUBLE PRECISION      ZCOSGL
      DOUBLE PRECISION      ZCOSH
      DOUBLE PRECISION      ZCOSHL
      DOUBLE PRECISION      ZCOSI
      DOUBLE PRECISION      ZCOSIL
      DOUBLE PRECISION      ZE
      DOUBLE PRECISION      ZF
      DOUBLE PRECISION      ZM
      DOUBLE PRECISION      ZMOL
      DOUBLE PRECISION      ZMOS
      DOUBLE PRECISION      ZN
      DOUBLE PRECISION      ZSING
      DOUBLE PRECISION      ZSINGL
      DOUBLE PRECISION      ZSINH
      DOUBLE PRECISION      ZSINHL
      DOUBLE PRECISION      ZSINI
      DOUBLE PRECISION      ZSINIL
      DOUBLE PRECISION      ZX
      DOUBLE PRECISION      ZY
      DOUBLE PRECISION      DEL   ( 3  )
      DOUBLE PRECISION      DG    ( 10 )
      DOUBLE PRECISION      SSX   ( 5  )
      DOUBLE PRECISION      JDTDB
      DOUBLE PRECISION      JDUT50

      INTEGER               I
      INTEGER               IRESFL
      INTEGER               ISYNFL

      LOGICAL               CONT

C
C     SPICELIB functions
C
      DOUBLE PRECISION      J1950
      DOUBLE PRECISION      SPD
      DOUBLE PRECISION      J2000
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      TWOPI

      LOGICAL               RETURN


C
C     Define rather a large number of local parameters.
C
      DOUBLE PRECISION      ZNS
      PARAMETER           ( ZNS    = 1.19459D-5   )

      DOUBLE PRECISION      C1SS
      PARAMETER           ( C1SS   = 2.9864797D-6 )


      DOUBLE PRECISION      ZES
      PARAMETER           ( ZES    = 0.01675D0    )

      DOUBLE PRECISION      ZNL
      PARAMETER           ( ZNL    = 1.5835218D-4 )

      DOUBLE PRECISION      C1L
      PARAMETER           ( C1L    = 4.7968065D-7 )

      DOUBLE PRECISION      ZEL
      PARAMETER           ( ZEL    = .05490D0     )

      DOUBLE PRECISION      ZCOSIS
      PARAMETER           ( ZCOSIS = .91744867D0  )

      DOUBLE PRECISION      ZSINIS
      PARAMETER           ( ZSINIS = .39785416D0  )

      DOUBLE PRECISION      ZSINGS
      PARAMETER           ( ZSINGS = -.98088458D0 )

      DOUBLE PRECISION      ZCOSGS
      PARAMETER           ( ZCOSGS = .1945905D0   )

      DOUBLE PRECISION      Q22
      PARAMETER           ( Q22    = 1.7891679D-6 )

      DOUBLE PRECISION      Q31
      PARAMETER           ( Q31    = 2.1460748D-6 )

      DOUBLE PRECISION      Q33
      PARAMETER           ( Q33    = 2.2123015D-7 )

      DOUBLE PRECISION      ROOT22
      PARAMETER           ( ROOT22 = 1.7891679D-6 )

      DOUBLE PRECISION      ROOT32
      PARAMETER           ( ROOT32 = 3.7393792D-7 )

      DOUBLE PRECISION      ROOT44
      PARAMETER           ( ROOT44 = 7.3636953D-9 )

      DOUBLE PRECISION      ROOT52
      PARAMETER           ( ROOT52 = 1.1428639D-7 )

      DOUBLE PRECISION      ROOT54
      PARAMETER           ( ROOT54 = 2.1765803D-9 )

      DOUBLE PRECISION      THDT
      PARAMETER           ( THDT   = 4.3752691D-3 )

      DOUBLE PRECISION      STEP2
      PARAMETER           ( STEP2 = 259200.D0 )

      INTEGER               LUNAR
      PARAMETER           ( LUNAR = 1 )

      DOUBLE PRECISION      ZERO
      PARAMETER           ( ZERO    =  0.0D0    )

      DOUBLE PRECISION      ONE
      PARAMETER           ( ONE     =  1.0D0    )


C
C     Save everything just to be sure.
C
      SAVE


C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'NRDDP' )
      END IF

C
C     This routine should never be called. If this routine is called,
C     an error is signalled.
C
      CALL SETMSG ( 'NRDDP: You called an entry which '  //
     .              'performs no run-time function. This may '        //
     .              'indicate a bug. Please check the documentation ' //
     .              'for the subroutine ZZNRDDP.' )

      CALL SIGERR ( 'SPICE(EVILBOGUSENTRY)' )

      CALL CHKOUT ( 'NRDDP' )

      RETURN





C$Procedure ZZDPINIT (Initialize deep space algorithm and variables )

      ENTRY ZZDPINIT( AO, XLLDOT, OMGDOT, XNODOT, XNODP, ELEMS )

C$ Abstract
C
C     Entrance for deep space initialization.  This section is called
C     once per element set.
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
C     KEYWORD
C
C$ Declarations
C
C     None.
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     AO         I   Original semimajor axis
C     XLLDOT     I   Time rate of change of XLL
C     OMGDOT     I   Time rate of change of argument of perigee
C     XNODOT     I   Time rate of change of mean motion
C     XNODP      I   Original mean motion
C     ELEMS      I   Array of orbit elements
C
C$ Detailed_Input
C
C     AO          the original semimajor axis of the orbit.
C
C     XLLDOT      the time derivative of the XLL long-period term
C
C     OMGDOT      the time derivative of the argument of perigee
C
C     XNODOT      the time derivative of the mean motion
C
C     XNODP       original mean motion of the elements
C
C     ELEMS       is an array containing two-line element data
C                 as prescribed below. The elements XNDD6O and BSTAR
C                 must already be scaled by the proper exponent stored
C                 in the two line elements set.  Moreover, the
C                 various items must be converted to the units shown
C                 here.
C
C                    ELEMS (  1 ) = XNDT2O in radians/minute**2
C                    ELEMS (  2 ) = XNDD6O in radians/minute**3
C                    ELEMS (  3 ) = BSTAR
C                    ELEMS (  4 ) = XINCL  in radians
C                    ELEMS (  5 ) = XNODEO in radians
C                    ELEMS (  6 ) = EO
C                    ELEMS (  7 ) = OMEGAO in radians
C                    ELEMS (  8 ) = XMO    in radians
C                    ELEMS (  9 ) = XNO    in radians/minute
C                    ELEMS ( 10 ) = EPOCH of the elements in seconds
C                                   past ephemeris epoch J2000.
C
C$ Detailed_Output
C
C     No direct output.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine only initializes non-time dependent variables and
C     sets flags concerning whether the orbit is synchronous or
C     experiences resonance effects.  It should be called once per
C     element set.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     1)  This routine should only be called by DPSPCE when propagating
C         two line element sets.
C
C$ Literature_References
C
C     Hoots, Felix R., Ronald L. Roehrich (31 December 1988). "Models 
C     for Propagation of NORAD Element Sets". United States Department
C     of Defense Spacetrack Report (3).
C
C     Vallado, David A., Paul Crawford, Richard Hujsak, and
C     Kelso, T. S., "Revisiting Spacetrack Report #3," AIAA/AAS
C     Astrodynamics Specialist Conference, Keystone, CO, Aug 2006.
C
C$ Author_and_Institution
C
C     E.D. Wright      (JPL)
C     W.L. Taber       (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.0.0, 02-MAR-2011 (EDW)
C
C        Eliminated use of the DOPERT boolean. This algorithm used
C        that variable to zero-out the perturbations at epoch (T = 0).
C        The original code implementation introduced a logic error
C        such that the state of DOPERT was correct only for
C        runs involving a single vehicle TLE.
C
C        The "Revisiting Spacetrack Report #3" by Vallado et. al.
C        indicates the perturbation zeroing is a mistake. That
C        operation was removed.
C
C-    SPICELIB Version 1.5.1, 19-SEP-2006 (EDW)
C
C        Added text to previously empty Declarations section
C        in ZZDPINIT, ZZDPPER, ZZDPSEC.
C
C-    SPICELIB Version 1.5.0, 20-JAN-1999 (EDW) (WLT)
C
C        OMGDOT, named in an ENTRY point argument list
C        was not passed via an argument list.  Solaris exhibited a
C        bus error because of this situation.  All ENTRY point
C        arguments are passed only by argument lists and are declared
C        in the umbrella subroutine's, ZZNRDDP, argument list.
C
C        Combined the various SSL, SSG, SSH, SSI, SSE variables into
C        the vector SSX.
C
C        Removed the dependency upon the UTC/ET leapsecond kernel.
C
C        Alphabetized all variable declaration lists.
C
C        All arguments passed through entry points listed as arguments
C        of ZZNRDDP.  OMGDT renamed OMGDOT to be consistent with other
C        deep space two line element routines.
C
C-    SPICELIB Version 1.0.0, APR-30-1997 (EDW)
C
C
C-&

C$ Index_Entries
C
C     two line elements, deep space, initialize
C
C-&

      PIX1 = PI()
      PIX2 = TWOPI()

C
C     Unpack the elements array.
C
      XINCL  = ELEMS( 4  )
      XNODEO = ELEMS( 5  )
      EO     = ELEMS( 6  )
      OMEGAO = ELEMS( 7  )
      XMO    = ELEMS( 8  )

C
C     Calculate intermediate values
C
      EQSQ   = EO**2
      BSQ    = ONE - EQSQ
      RTEQSQ = DSQRT( BSQ   )
      SINIQ  = DSIN( XINCL  )
      COSIQ  = DCOS( XINCL  )
      COSQ2  = COSIQ**2
      SINOMO = DSIN( OMEGAO )
      COSOMO = DCOS( OMEGAO )


C
C     This section of code was previously performed by the THETAG
C     function.  The epoch of the elements is defined in seconds since
C     J2000.  It is necessary to calculate the number of days which have
C     elapsed since the Jan 0.0 1950 reference date which is
C     Dec 31 1949 00:00:00 UTC ( J1950 - 1 ).  First extract the epoch
C     from the ELEMS array and place it in the first entry of a working
C     array.
C
      ET = ELEMS( 10 )

C
C     Convert the ET seconds past the J2000 epoch to the Julian
C     date TDB.
C
      JDTDB = J2000() + ET/SPD()

C
C     How many days since the 1950 reference? Using SPICE standard
C     leapseconds the difference between TDB and UTC in 1950 is 32.184
C     seconds.  So we compute JDTDB corresponding to the UTC 1950
C     epoch.  We call this JDTDB epoch ---JDUT50. Then we get the days
C     since 1950 by simple arithmetic.
C
      JDUT50 = J1950() - ONE    + (32.184D0/SPD())
      DS50   = JDTDB   - JDUT50

C
C     What is the Earth's right ascension of the epoch?  We know the
C     value at the JD1950-1 reference date, so add the number of radians
C     the Earth has rotated through since then.  MOD this value with
C     2*PI to get the right ascension for the epoch.  This technique may
C     not be the best way to get this value.
C
      THETA  = 1.72944494D0 + 6.3003880987D0 * DS50
      THGR   = DMOD( THETA, PIX2 )


C
C     THGR should have a domain between 0 and 2 Pi.
C
      IF ( THGR .LT. ZERO ) THEN
         THGR = THGR + PIX2
      END IF

C
C     Set some operation variables.
C
      EQ     = EO
      XNQ    = XNODP
      AQNV   = ONE/AO
      XQNCL  = XINCL
      XMAO   = XMO
      SINQ   = DSIN( XNODEO )
      COSQ   = DCOS( XNODEO )


C
C     Initialize lunar solar terms
C

      DAY = DS50 + 18261.5D0


      IF ( DAY .NE. PREEP ) THEN

         PREEP  = DAY
         XNODCE = 4.5236020D0  -  9.2422029D-04 * DAY
         STEM   = DSIN( XNODCE )
         CTEM   = DCOS( XNODCE )
         ZCOSIL = 0.91375164D0 - .03568096D0*CTEM
         ZSINIL = DSQRT ( ONE - ZCOSIL**2 )
         ZSINHL = 0.089683511D0 * STEM / ZSINIL
         ZCOSHL = DSQRT ( ONE - ZSINHL**2 )
         C      = 4.7199672D0 + .22997150D0 * DAY
         GAM    = 5.8351514D0 + .0019443680D0 * DAY
         ZMOL   = DMOD( (C - GAM), PIX2 )

         IF ( ZMOL .LT. ZERO ) THEN
            ZMOL = ZMOL + PIX2
         END IF

         ZX = .39785416D0 * STEM/ZSINIL
         ZY = ZCOSHL*CTEM + 0.91744867D0 * ZSINHL*STEM

C
C        Compute the angle from the x-axis of the point
C
         IF (      ZX .NE. ZERO
     .        .OR. ZY .NE. ZERO ) THEN

            ZX = DATAN2 ( ZX, ZY)

            IF ( ZX .LT. ZERO ) THEN
               ZX = ZX + PIX2
            END IF

         ELSE

            ZX = ZERO

         END IF

         ZX     = GAM + ZX - XNODCE
         ZCOSGL = DCOS( ZX )
         ZSINGL = DSIN( ZX )
         ZMOS   = 6.2565837D0 + .017201977D0 * DAY
         ZMOS   = DMOD( ZMOS, PIX2 )

         IF ( ZMOS .LT. ZERO ) THEN
            ZMOS = ZMOS + PIX2
         END IF

      END IF



C
C     Do solar terms.  Start with the constant values.
C

      ZCOSG  = ZCOSGS
      ZSING  = ZSINGS
      ZCOSI  = ZCOSIS
      ZSINI  = ZSINIS
      ZCOSH  = COSQ
      ZSINH  = SINQ
      CC     = C1SS
      ZN     = ZNS
      ZE     = ZES
      XNOI   = ONE/XNQ


C
C     Initialize solar and lunar terms.  The procedure will
C     first initialize just the solar, then the lunar, then
C     reinitialize the solar with the added lunar effect.
C
      DO I = 1, 2

C
C        Solar.
C
         A1  = ZCOSG*ZCOSH + ZSING*ZCOSI*ZSINH
         A3  =-ZSING*ZCOSH + ZCOSG*ZCOSI*ZSINH
         A7  =-ZCOSG*ZSINH + ZSING*ZCOSI*ZCOSH
         A8  = ZSING*ZSINI
         A9  = ZSING*ZSINH + ZCOSG*ZCOSI*ZCOSH
         A10 = ZCOSG*ZSINI
         A2  = COSIQ*A7 + SINIQ*A8
         A4  = COSIQ*A9 + SINIQ*A10
         A5  =-SINIQ*A7 + COSIQ*A8
         A6  =-SINIQ*A9 + COSIQ*A10

         X1  = A1*COSOMO + A2*SINOMO
         X2  = A3*COSOMO + A4*SINOMO
         X3  =-A1*SINOMO + A2*COSOMO
         X4  =-A3*SINOMO + A4*COSOMO
         X5  = A5*SINOMO
         X6  = A6*SINOMO
         X7  = A5*COSOMO
         X8  = A6*COSOMO

         Z31 = 12.D0*X1**2 - 3.D0*X3**2
         Z32 = 24.D0*X1*X2 - 6.D0*X3*X4
         Z33 = 12.D0*X2**2 - 3.D0*X4**2

         Z1  = 3.D0*(A1**2 + A2**2) + Z31*EQSQ
         Z2  = 6.D0*(A1*A3 + A2*A4) + Z32*EQSQ
         Z3  = 3.D0*(A3**2 + A4**2) + Z33*EQSQ

         Z11 =-6.D0*A1*A5 + EQSQ*(-24.D0*X1*X7 - 6.D0*X3*X5)
         Z12 =-6.D0*(A1*A6 + A3*A5) + EQSQ*
     .         ( -24.D0*(X2*X7 + X1*X8) - 6.D0*(X3*X6 + X4*X5))
         Z13 =-6.D0*A3*A6+EQSQ *(-24.D0*X2*X8-6.D0*X4*X6)
         Z21 = 6.D0*A2*A5+EQSQ *(24.D0*X1*X5-6.D0*X3*X7)
         Z22 = 6.D0*(A4*A5+A2*A6)+EQSQ *(24.D0*(X2*X5+X1*X6) -
     .         6.D0*(X4*X7+X3*X8))
         Z23 = 6.D0*A4*A6 + EQSQ*
     .         ( 24.D0*X2*X6 - 6.D0*X4*X8 )

         Z1  = Z1 + Z1 + BSQ*Z31
         Z2  = Z2 + Z2 + BSQ*Z32
         Z3  = Z3 + Z3 + BSQ*Z33

         S3  = CC * XNOI
         S2  =-0.5D0 * S3/RTEQSQ
         S4  = S3 * RTEQSQ
         S1  =-15.D0 * EQ * S4
         S5  = X1*X3 + X2*X4
         S6  = X2*X3 + X1*X4
         S7  = X2*X4 - X1*X3

         SE  = S1 * ZN * S5
         SI  = S2 * ZN * ( Z11 + Z13 )
         SL  =-ZN * S3 * ( Z1  + Z3 - 14.D0 - 6.D0*EQSQ )
         SGH = S4 * ZN * ( Z31 + Z33 - 6.D0 )
         SH  =-ZN * S2 * ( Z21 + Z23 )

         IF ( XQNCL .LT. 5.2359877D-2 ) THEN
            SH = ZERO
         END IF


         EE2  =  2.D0 * S1 * S6
         E3   =  2.D0 * S1 * S7
         XI2  =  2.D0 * S2 * Z12
         XI3  =  2.D0 * S2 *(Z13-Z11)
         XL2  = -2.D0 * S3 * Z2
         XL3  = -2.D0 * S3 *(Z3-Z1)
         XL4  = -2.D0 * S3 * (-21.D0-9.D0*EQSQ)*ZE
         XGH2 =  2.D0 * S4 * Z32
         XGH3 =  2.D0 * S4 *(Z33-Z31)
         XGH4 =-18.D0 * S4 * ZE
         XH2  = -2.D0 * S2 * Z22
         XH3  = -2.D0 * S2 *(Z23-Z21)


         IF ( I .EQ. LUNAR ) THEN

C
C           Do lunar terms after solar terms, but only once.
C
            SSX(1)   = SL
            SSX(3)   = SH/SINIQ
            SSX(2)   = SGH  -  COSIQ * SSX(3)
            SSX(4)   = SE
            SSX(5)   = SI
            SE2      = EE2
            SI2      = XI2
            SL2      = XL2

            SGH2     = XGH2
            SH2      = XH2
            SE3      = E3
            SI3      = XI3
            SL3      = XL3

            SGH3     = XGH3
            SH3      = XH3
            SL4      = XL4
            SGH4     = XGH4

            ZCOSG    = ZCOSGL
            ZSING    = ZSINGL
            ZCOSI    = ZCOSIL
            ZSINI    = ZSINIL
            ZCOSH    = ZCOSHL * COSQ    +  ZSINHL * SINQ
            ZSINH    = SINQ   * ZCOSHL  -  COSQ   * ZSINHL

            ZN       = ZNL
            CC       = C1L
            ZE       = ZEL

         END IF


      END DO


      SSX(1) = SSX(1) + SL
      SSX(2) = SSX(2) + SGH - COSIQ/SINIQ * SH
      SSX(3) = SSX(3) + SH/SINIQ
      SSX(4) = SSX(4) + SE
      SSX(5) = SSX(5) + SI

C
C     Geopotential resonance initialization for 12 hour orbits
C

      IRESFL = 0
      ISYNFL = 0

      IF( XNQ .LT. (.0052359877D0) .AND.
     .    XNQ .GT. (.0034906585D0)       ) THEN

C
C        Synchronous resonance terms initialization
C

         IRESFL = 1
         ISYNFL = 1
         G200   = ONE + EQSQ*(-2.5D0 +.8125D0 * EQSQ)
         G310   = ONE + 2.D0*EQSQ
         G300   = ONE + EQSQ*(-6.0D0 + 6.60937D0 * EQSQ)
         F220   = 0.75D0 * (ONE + COSIQ)**2
         F311   =.9375D0 * SINIQ*SINIQ*(ONE + 3.D0*COSIQ) -
     .            0.75D0*(ONE + COSIQ)
         F330   = 1.875D0 * ( ONE + COSIQ )**3
         DEL(1) = 3.D0*(XNQ**2)*(AQNV**2)
         DEL(2) = 2.D0*DEL(1)*F220*G200*Q22
         DEL(3) = 3.D0*DEL(1)*F330*G300*Q33*AQNV
         DEL(1) =      DEL(1)*F311*G310*Q31*AQNV
         XLAMO  = XMAO   + XNODEO + OMEGAO - THGR
         BFACT  = XLLDOT + OMGDOT + XNODOT - THDT
         BFACT  = BFACT  + SSX(1) + SSX(2) + SSX(3)

      ELSE

         IF ( XNQ .LT.(8.26D-3) .OR.
     .        XNQ .GT.(9.24D-3) .OR.
     .        EQ  .LT. 0.5D0        ) THEN

            RETURN

         END IF


         IRESFL = 1
         EOC    = EQ*EQSQ
         G201   = -.306D0 - ( EQ - .64D0 )*0.440D0

C
C     Looks icky doesn't it?
C

         IF( EQ. GT. (.65D0) ) THEN

            G211 =-72.099D0  + 331.819D0 * EQ - 508.738D0 * EQSQ +
     .             266.724D0 * EOC
            G310 =-346.844D0 + 1582.851D0 * EQ - 2415.925D0 * EQSQ +
     .             1246.113D0 * EOC
            G322 =-342.585D0 + 1554.908D0 * EQ - 2366.899D0 * EQSQ +
     .             1215.972D0 * EOC
            G410 =-1052.797D0 + 4758.686D0 * EQ - 7193.992D0 * EQSQ +
     .             3651.957D0 * EOC
            G422 =-3581.69D0 + 16178.11D0 * EQ - 24462.77D0 * EQSQ +
     .             12422.52D0 * EOC

C
C           Decide on the G520 coefficient.
C
            IF ( EQ .GT. (.715D0) ) THEN

               G520 = -5149.66D0 + 29936.92D0 * EQ -
     .                54087.36D0 * EQSQ + 31324.56D0 * EOC

            ELSE

               G520 = 1464.74D0 - 4664.75D0 * EQ + 3763.64D0 * EQSQ

            END IF

         ELSE

            G211 = 3.616D0 - 13.247D0*EQ + 16.290D0*EQSQ
            G310 = -19.302D0 + 117.390D0*EQ -
     .             228.419D0 * EQSQ + 156.591D0*EOC
            G322 = -18.9068D0 + 109.7927D0*EQ -
     .             214.6334D0 * EQSQ + 146.5816D0*EOC
            G410 = -41.122D0 + 242.694D0*EQ - 471.094D0*EQSQ +
     .             313.953D0 * EOC
            G422 = -146.407D0 + 841.880D0*EQ - 1629.014D0*EQSQ +
     .             1083.435D0 * EOC
            G520 = -532.114D0 + 3017.977D0*EQ - 5740.D0*EQSQ +
     .             3708.276D0 * EOC

         END IF


         IF( EQ .GE.(.7D0) ) THEN

            G533  = -37995.78D0 + 161616.52D0*EQ -
     .              229838.2D0*EQSQ + 109377.94D0*EOC
            G521  = -51752.104D0 + 218913.95D0*EQ -
     .              309468.16D0*EQSQ + 146349.42D0*EOC
            G532  = -40023.88D0 + 170470.89D0*EQ -
     .              242699.48D0*EQSQ + 115605.82D0*EOC

         ELSE

            G533  = -919.2277D0 + 4988.61D0*EQ -
     .              9064.77D0*EQSQ + 5542.21D0*EOC
            G521  = -822.71072D0 + 4568.6173D0*EQ -
     .              8491.4146D0*EQSQ + 5337.524D0*EOC
            G532  = -853.666D0 + 4690.25D0*EQ -
     .              8624.77D0*EQSQ + 5341.4D0*EOC

         END IF


         SINI2    = SINIQ*SINIQ

         F220     = .75D0 * (ONE + 2.D0*COSIQ + COSQ2)
         F221     = 1.5D0*SINI2
         F321     = 1.875D0 * SINIQ * (ONE-2.D0*COSIQ-3.D0*COSQ2)
         F322     =-1.875D0 * SINIQ * (ONE+2.D0*COSIQ-3.D0*COSQ2)
         F441     = 35.D0 * SINI2 * F220
         F442     = 39.3750D0 * SINI2 * SINI2

         F522     = 9.84375D0 * SINIQ * (SINI2 *
     .              (ONE - 2.D0*COSIQ-5.D0*COSQ2)
     .              +.33333333D0 * (-2.D0 + 4.D0 * COSIQ +
     .              6.D0 *COSQ2))

         F523     = SINIQ * (4.92187512D0 * SINI2 *
     .              ( -2.D0 - 4.D0 * COSIQ + 10.D0 * COSQ2 )
     .              + 6.56250012D0 * ( ONE + 2.D0 * COSIQ -
     .              3.D0 * COSQ2 ) )

         F542     = 29.53125D0 * SINIQ * (2.D0 - 8.D0 * COSIQ +
     .              COSQ2 * ( -12.D0+8.D0*COSIQ + 10.D0 * COSQ2 ) )

         F543     = 29.53125D0 * SINIQ * (-2.D0 - 8.D0 * COSIQ +
     .              COSQ2 * (12.D0 + 8.D0*COSIQ - 10.D0*COSQ2 ) )

         XNO2     = XNQ   * XNQ
         AINV2    = AQNV  * AQNV
         TEMP1    = 3.D0  * XNO2 * AINV2
         TEMP     = TEMP1 * ROOT22
         DG( 1  ) = TEMP  * F220 * G201
         DG( 2  ) = TEMP  * F221 * G211
         TEMP1    = TEMP1 * AQNV
         TEMP     = TEMP1 * ROOT32
         DG( 3  ) = TEMP  * F321 * G310
         DG( 4  ) = TEMP  * F322 * G322
         TEMP1    = TEMP1 * AQNV
         TEMP     = 2.D0  * TEMP1 * ROOT44
         DG( 5  ) = TEMP  * F441 * G410
         DG( 6  ) = TEMP  * F442 * G422
         TEMP1    = TEMP1 * AQNV
         TEMP     = TEMP1 * ROOT52
         DG( 7  ) = TEMP  * F522  * G520
         DG( 8  ) = TEMP  * F523  * G532
         TEMP     = 2.D0  * TEMP1 * ROOT54
         DG( 9  ) = TEMP  * F542  * G521
         DG( 10 ) = TEMP  * F543  * G533

         XLAMO    = XMAO   + XNODEO + XNODEO - THGR - THGR
         BFACT    = XLLDOT + XNODOT + XNODOT - THDT - THDT
         BFACT    = BFACT  + SSX(1)    + SSX(3)    + SSX(3)

      END IF



      XFACT = BFACT - XNQ

C
C     Initialize integrator
C
      XLI   = XLAMO
      XNI   = XNQ
      ATIME = ZERO

      RETURN





C$Procedure ZZDPSEC (Calculate secular perturbations )

      ENTRY ZZDPSEC( XLL , OMGASM, XNODES, EM, XINC, XN, T, ELEMS,
     .               OMGDOT )

C$ Abstract
C
C     Entrance for deep space secular effects
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
C     SECULAR PERTURBATION
C
C$ Declarations
C
C     None.
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     XLL        I    Long-period periodic term
C     OMGASM     I    Perturbed argument of perigee
C     XNODES     I    Perturbed argument of ascending node
C     T          I    Time to calculate perturbation
C     ELEMS      I    The two line elements array
C     XN         O    Perturbed mean motion of the orbit at time T
C     EM         O    Perturbed eccentricity of the orbit at time T
C     XINC       O    Perturbed inclination of the orbit plane at time T
C
C$ Detailed_Input
C
C     XLL         a long-period periodic term dependent on inclination,
C                 eccentricity and argument of periapsis
C
C     OMGASM      the value of the argument of perigee after the
C                 perturbations at the time of interest are
C                 added
C
C     XNODES      is the value of the argument of the ascending node
C                 after the perturbations at the time of interest are
C                 added.
C
C     T           is the total time from the epoch, in minutes, of the
C                 element set at which to calculate the perturbation.
C
C     ELEMS       is an array containing two-line element data
C                 as prescribed below. The elements XNDD6O and BSTAR
C                 must already be scaled by the proper exponent stored
C                 in the two line elements set.  Moreover, the
C                 various items must be converted to the units shown
C                 here.
C
C                    ELEMS (  1 ) = XNDT2O in radians/minute**2
C                    ELEMS (  2 ) = XNDD6O in radians/minute**3
C                    ELEMS (  3 ) = BSTAR
C                    ELEMS (  4 ) = XINCL  in radians
C                    ELEMS (  5 ) = XNODEO in radians
C                    ELEMS (  6 ) = EO
C                    ELEMS (  7 ) = OMEGAO in radians
C                    ELEMS (  8 ) = XMO    in radians
C                    ELEMS (  9 ) = XNO    in radians/minute
C                    ELEMS ( 10 ) = EPOCH of the elements in seconds
C                                   past ephemeris epoch J2000.
C
C$ Detailed_Output
C
C     XN          is the perturbed mean motion from the 'mean' mean
C                 motion at epoch at time T.
C
C     EM          is the perturbed eccentricity from the mean
C                 eccentricity at epoch at time T.
C
C     XINC        is the perturbed inclination of the orbit plane from
C                 the mean inclination at the epoch at time T
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     None.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     The operation of this routine is to calculate the current secular
C     perturbations of the 'mean' orbit elements.  The extent of the
C     perturbations is determined by the state of the IRESFL flag.  This
C     flag indicates whether the resonance effects will or will not be
C     calculated for the vehicle.  Resonance will be calculated when
C     mean motion is between 0.8 to 1.2 orbits per day (approximately
C     geosynch), or between 1.9 and 2.1 orbits per days.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     1)  This routine should only be called by DPSPCE when propagating
C         two line element sets.
C
C$ Literature_References
C
C     Hoots, Felix R., Ronald L. Roehrich (31 December 1988). "Models
C     for Propagation of NORAD Element Sets". United States Department
C     of Defense Spacetrack Report (3).
C
C$ Author_and_Institution
C
C     E.D. Wright      (JPL)
C     W.L. Taber       (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.5.1, 19-SEP-2006 (EDW)
C
C        Added text to previously empty Declarations section
C        in ZZDPINIT, ZZDPPER, ZZDPSEC.
C
C-    SPICELIB Version 1.5.0, 20-JAN-1999 (EDW) (WLT)
C
C        OMGDOT, named in an ENTRY point argument list
C        was not passed via an argument list.  Solaris exhibited a
C        bus error because of this situation.  All ENTRY point
C        arguments are passed only by argument lists and are declared
C        in the umbrella subroutine's, ZZNRDDP, argument list.
C
C        Combined the various SSL, SSG, SSH, SSI, SSE variables into
C        the vector SSX.
C
C        Removed the dependency upon the UTC/ET leapsecond kernel.
C
C        Alphabetized all variable declaration lists.
C
C        All arguments passed through entry points listed as arguments
C        of ZZNRDDP.  OMGDT renamed OMGDOT to be consistent with other
C        deep space two line element routines.
C
C-    SPICELIB Version 1.0.0, MAY-2-1997 (EDW)
C
C
C-&

C$ Index_Entries
C
C     two line elements, secular perturbation
C
C-&

      STEPP  = 720.D0
      STEPN  =-720.D0

      XINCL  = ELEMS( 4 )
      EO     = ELEMS( 6 )
      XLL    = XLL     +  SSX(1) * T
      OMGASM = OMGASM  +  SSX(2) * T
      XNODES = XNODES  +  SSX(3) * T
      EM     = EO      +  SSX(4) * T
      XINC   = XINCL   +  SSX(5) * T


C
C     Check for a positive inclination and the state of the
C     resonance flag.
C
      IF ( XINC .GE. 0. ) THEN

C
C        If the resonance flag is not set return.
C
         IF( IRESFL .EQ. 0) THEN
            RETURN
         END IF

      ELSE

C
C        A negative inclination.  Fix that and reset XNODES and
C        OMGASM then check the resonance flag.
C
         XINC   = -XINC
         XNODES = XNODES + PIX1
         OMGASM = OMGASM - PIX1

         IF( IRESFL .EQ. 0) THEN
            RETURN
         END IF

      END IF



C
C     If we got down here then the resonance effects need to be
C     calculated.  Continue to loop until the CONT flag is set to false.
C
      CONT = .TRUE.

      DO WHILE( CONT )

         IF ( ( ATIME .EQ. ZERO )                   .OR.
     .        ( T .GE. ZERO .AND. ATIME .LT. ZERO ) .OR.
     .        ( T .LT. ZERO .AND. ATIME .GE. ZERO )     ) THEN

C
C           Epoch restart
C
            IF ( T .GE. ZERO )  THEN
               DELT = STEPP
            ELSE
               DELT = STEPN
            END IF


            ATIME = ZERO
            XNI   = XNQ
            XLI   = XLAMO

            CONT = .FALSE.


         ELSE IF ( DABS( T ). GE. DABS(ATIME) ) THEN

            DELT = STEPN

            IF ( T .GT. ZERO ) THEN
               DELT = STEPP
            END IF

            CONT = .FALSE.


         ELSE

            DELT = STEPP

            IF ( T .GE. ZERO ) THEN
               DELT = STEPN
            END IF


            CALL ZZSECPRT ( ISYNFL, DG   , DEL   , XNI,
     .                      OMEGAO, ATIME, OMGDOT, XLI,
     .                      XFACT , XLDOT, XNDOT , XNDDT )

            XLI   = XLI   +  XLDOT * DELT  +  XNDOT * STEP2
            XNI   = XNI   +  XNDOT * DELT  +  XNDDT * STEP2
            ATIME = ATIME +  DELT

            CONT = .TRUE.

         END IF

      END DO


C
C     Do this loop while the time interval is greater than STEPP
C

      DO WHILE ( DABS( T - ATIME ). GE. STEPP )

         CALL ZZSECPRT ( ISYNFL, DG   , DEL   , XNI,
     .                   OMEGAO, ATIME, OMGDOT, XLI,
     .                   XFACT , XLDOT, XNDOT , XNDDT )

         XLI   = XLI  +  XLDOT * DELT  +  XNDOT * STEP2
         XNI   = XNI  +  XNDOT * DELT  +  XNDDT * STEP2
         ATIME = ATIME + DELT

      END DO


C
C     Calculate the time interval and determine the secular
C     perturbations
C

      FT = T - ATIME

      CALL ZZSECPRT ( ISYNFL, DG   , DEL   , XNI,
     .                OMEGAO, ATIME, OMGDOT, XLI,
     .                XFACT , XLDOT, XNDOT , XNDDT )

      XN    = XNI    + XNDOT * FT + XNDDT * FT * FT * 0.5D0
      XL    = XLI    + XLDOT * FT + XNDOT * FT * FT * 0.5D0
      TEMP  =-XNODES + THGR       + T     * THDT
      XLL   = XL     - OMGASM     + TEMP


      IF ( ISYNFL .EQ. 0) THEN
         XLL = XL + TEMP + TEMP
      END IF


      RETURN





C$Procedure ZZDPPER ( Calculate periodic perturbations )

      ENTRY ZZDPPER( T, EM, XINC, OMGASM, XNODES, XLL)

C$ Abstract
C
C     Entrances for lunar-solar periodics
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
C     PERIODIC PERTURBATION
C
C$ Declarations
C
C     None.
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     T          I   Time to calculate perturbations
C     EM         O   Perturbed eccentricity of the orbit at time T
C     XINC       O   Perturbed inclination of the orbit plane at time T
C     OMGASM     O   Perturbed argument of perigee
C     XNODES     O   Perturbed argument of ascending node
C     XLL        0   Long-period periodic term
C
C$ Detailed_Input
C
C     T           the time from the epoch in minutes of the element set
C                 at which to calculate the perturbation.
C
C$ Detailed_Output
C
C     EM          is the perturbed eccentricity from the mean
C                 eccentricity at epoch at time T.
C
C     XINC        is the perturbed inclination of the orbit plane from
C                 the mean inclination at the epoch at time T.
C
C     OMGASM      the value of the argument of perigee after the
C                 perturbations at the time of interest are
C                 added.
C
C     XNODES      is the value of the argument of the ascending node
C                 after the perturbations at the time of interest are
C                 added.
C
C     XLL         a long-period periodic term dependent on inclination,
C                 eccentricity and argument of periapsis.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine calculates the current time dependent periodic
C     perturbations values due to the sun and the moon.  The original
C     version, as taken from the Spacetrack 3 report, had a number of
C     bugs.
C
C     XNODES could be evaluated as being in the wrong quadrant due to
C     a failure to insure a domain of 0 to 2 Pi.
C
C     The SIN and COS of the perturbed inclination, XINCL, were
C     calculated before the perturbed value.
C
C     EM & XINC are input and output values.  The input value is updated
C     by the addition of a perturbation value.
C
C     The original report did not recalculate perturbation terms if two
C     consecutive epoch times were less than 30 minutes apart.  This
C     condition has been removed.  Perturbation terms are always
C     calculated.
C
C$ Examples
C
C     None needed.
C
C$ Restrictions
C
C     1)  This routine should only be called by DPSPCE when propagating
C         two line element sets.
C
C     2)  This routine should be initialized prior to use by making
C         a call with the time epoch set to 0.  Failure to do so
C         invalidates the perturbation calculation.
C
C$ Literature_References
C
C     Hoots, Felix R., Ronald L. Roehrich (31 December 1988). "Models
C     for Propagation of NORAD Element Sets". United States Department
C     of Defense Spacetrack Report (3).
C
C$ Author_and_Institution
C
C     E.D. Wright      (JPL)
C     W.L. Taber       (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.5.1, 19-SEP-2006 (EDW)
C
C        Added text to previously empty Declarations section
C        in ZZDPINIT, ZZDPPER, ZZDPSEC.
C
C-    SPICELIB Version 1.5.0, 20-JAN-1999 (EDW) (WLT)
C
C        OMGDOT, named in an ENTRY point argument list
C        was not passed via an argument list.  Solaris exhibited a
C        bus error because of this situation.  All ENTRY point
C        arguments are passed only by argument lists and are declared
C        in the umbrella subroutine's, ZZNRDDP, argument list.
C
C        Combined the various SSL, SSG, SSH, SSI, SSE variables into
C        the vector SSX.
C
C        Removed the dependency upon the UTC/ET leapsecond kernel.
C
C        Alphabetized all variable declaration lists.
C
C        All arguments passed through entry points listed as arguments
C        of ZZNRDDP.  OMGDT renamed OMGDOT to be consistent with other
C        deep space two line element routines.
C
C-    SPICELIB Version 1.0.0, MAY-17-1997 (EDW)
C
C
C-&

C$ Index_Entries
C
C     two line elements, periodic perturbation
C
C-&


C
C     Time varying periodic terms.
C

C
C     Update for solar perts at time T.
C
      ZM     = ZMOS + ZNS * T
      ZF     = ZM   + 2.D0 * ZES * DSIN(ZM)
      SINZF  = DSIN(ZF)
      F2     = 0.5D0 * SINZF * SINZF - 0.25D0
      F3     =-0.5D0*SINZF*DCOS( ZF )
      SES    =  SE2*F2 +  SE3*F3
      SIS    =  SI2*F2 +  SI3*F3
      SLS    =  SL2*F2 +  SL3*F3 +  SL4*SINZF
      SGHS   = SGH2*F2 + SGH3*F3 + SGH4*SINZF
      SHS    =  SH2*F2 +  SH3*F3


C
C     Update for lunar perts at time T.
C
      ZM     = ZMOL + ZNL*T
      ZF     = ZM + 2.D0*ZEL * DSIN(ZM)
      SINZF  = DSIN( ZF )
      F2     = 0.5D0*SINZF*SINZF - 0.25D0
      F3     =-0.5D0*SINZF*DCOS(ZF)
      SEL    =  EE2*F2 +   E3*F3
      SIL    =  XI2*F2 +  XI3*F3
      SLL    =  XL2*F2 +  XL3*F3 +  XL4*SINZF
      SGHL   = XGH2*F2 + XGH3*F3 + XGH4*SINZF
      SHL    =  XH2*F2 +  XH3*F3

C
C     Sum of solar and lunar perts
C
      PE     = SES + SEL
      PINC   = SIS + SIL
      PL     = SLS + SLL
      PGH    = SGHS + SGHL
      PH     = SHS  + SHL


C
C     Subtract the epoch perturbations off the calculated values.
C
      PE     = PE   - PE0
      PINC   = PINC - PINC0
      PL     = PL   - PL0
      PGH    = PGH  - PGH0
      PH     = PH   - PH0

      XINC  = XINC + PINC
      EM    = EM   + PE


C
C     Sin and Cos of the perturbed inclination.  The original
C     Spacetrack 3 report calculated the values before the
C     perturbation.
C
      SINIS = DSIN( XINC )
      COSIS = DCOS( XINC )


      IF ( XQNCL. GT. (.2D0) ) THEN

         PH     = PH/SINIQ
         PGH    = PGH    - COSIQ * PH
         OMGASM = OMGASM + PGH
         XNODES = XNODES + PH
         XLL    = XLL    + PL

      ELSE

C
C        Apply periodics with Lyddane modification
C
         SINOK = DSIN( XNODES )
         COSOK = DCOS( XNODES )
         ALFDP = SINIS * SINOK
         BETDP = SINIS * COSOK
         ALFDP = ALFDP + PH*COSOK + PINC*COSIS*SINOK
         BETDP = BETDP - PH*SINOK + PINC*COSIS*COSOK

C
C        Force a 0 - 2Pi domain on XNODES.
C
         IF ( XNODES .LT. ZERO ) THEN
            XNODES = XNODES + PIX2
         END IF

         XLS = XLL + OMGASM + PL + PGH +  COSIS * XNODES
     .                                 -  SINIS * XNODES * PINC


C
C        Compute the angle from the x-axis of the point
C
         IF (      ALFDP .NE. ZERO
     .        .OR. BETDP .NE. ZERO ) THEN

C
C           Save the old value of XNODES, then compute the current value
C           From ALFDP and BETDP
C
            OXNODE = XNODES
            XNODES = DATAN2( ALFDP, BETDP )

C
C           Force a 0 - 2Pi domain on XNODES
C
            IF ( XNODES .LT. ZERO ) THEN
               XNODES = XNODES + PIX2
            END IF


C
C           XNODES should be the angular difference between the previous
C           value of XNODES and that just calculated.  This is a
C           correction to the standard SDP4 routine which did not
C           calculate this term correctly if XNODES passes over the
C           branch cut at 2*Pi.
C
            IF ( DABS( XNODES - OXNODE ). GT. PIX1 ) THEN

               IF ( XNODES. GT. OXNODE ) THEN
                  XNODES = XNODES - PIX2
               ELSE
                  XNODES = XNODES + PIX2
               END IF

            END IF


         ELSE
            XNODES = ZERO

         END IF

         XLL    = XLL  +  PL
         OMGASM = XLS  -  XLL  -  XNODES * DCOS( XINC )

      END IF


      RETURN
      END
