C$Procedure      UNITIM ( Uniform time scale transformation )
 
      DOUBLE PRECISION FUNCTION UNITIM ( EPOCH, INSYS, OUTSYS )
 
C$ Abstract
C
C     Transform time from one uniform scale to another.  The uniform
C     time scales are TAI, TDT, TDB, ET, JED, JDTDB, JDTDT.
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
C     TIME
C
C$ Keywords
C
C     TIME
C     CONVERSION
C     UTILITY
C
C$ Declarations
 
      IMPLICIT NONE
 
      INCLUDE               'zzctr.inc'

      DOUBLE PRECISION      EPOCH
      CHARACTER*(*)         INSYS
      CHARACTER*(*)         OUTSYS
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     EPOCH      I   An epoch.
C     INSYS      I   The time scale associated with the input EPOCH.
C     OUTSYS     I   The time scale associated with the function value.
C
C     The function returns the d.p. in OUTSYS that is equivalent to the
C     EPOCH on the INSYS time scale.
C
C$ Detailed_Input
C
C     EPOCH      is an epoch relative to the INSYS time scale.
C
C     INSYS      is a time scale.  Acceptable values are:
C
C                'TAI'     International Atomic Time.
C                'TDB'     Barycentric Dynamical Time.
C                'TDT'     Terrestrial Dynamical Time.
C                'ET'      Ephemeris time (in the SPICE system, this is
C                          equivalent to TDB).
C                'JDTDB'   Julian Date relative to TDB.
C                'JDTDT'   Julian Date relative to TDT.
C                'JED'     Julian Ephemeris date (in the SPICE system
C                          this is equivalent to JDTDB).
C
C                The routine is not sensitive to the case of the
C                characters in INSYS;  'tai' 'Tai' and 'TAI' are
C                all equivalent from the point of view of this routine.
C
C     OUTSYS     is the time scale to which EPOCH should be converted.
C                Acceptable values are the same as for INSYS.  The
C                routine is not sensitive to the case of OUTSYS.
C
C$ Detailed_Output
C
C     The function returns the time in the system specified by OUTSYS
C     that is equivalent to the EPOCH in the INSYS time scale.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) The kernel pool must contain the variables:
C
C           'DELTET/DELTA_T_A'
C           'DELTET/K'
C           'DELTET/EB'
C           'DELTET/M'
C
C        If these are not present, the error 'SPICE(MISSINGTIMEINFO)'
C        will be signalled.  (These variables are typically inserted
C        into the kernel pool by loading a leapseconds kernel with
C        the SPICE routine FURNSH.)
C
C     2) If the names of either the input or output time types are
C        unrecognized, the error 'SPICE(BADTIMETYPE)' will be signalled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     We use the term uniform time scale to refer to those
C     representations of time that are numeric (each epoch is
C     represented by a number) and additive.  A numeric time
C     system is additive if given the representations, E1 and E2,
C     of any pair of successive epochs, the time elapsed between
C     the epochs is given by E2 - E1.
C
C     Given an epoch in one of the uniform time scales
C     specified by INSYS, the function returns the equivalent
C     representation in the scale specified by OUTSYS.  A list
C     of the recognized uniform time scales is given in the
C     detailed input for INSYS.
C
C$ Examples
C
C     To convert an epoch with respect to the International Atomic
C     Time (TAI) scale to ET (Barycentric Dynamical Time), make the
C     following assignment.
C
C           ET = UNITIM ( TAI, 'TAI', 'ET' )
C
C$ Restrictions
C
C     The appropriate variable must be loaded into the SPICE kernel pool
C     (normally by loading a leapseconds kernel with FURNSH) prior to
C     calling this routine.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C     H.A. Neilan    (JPL)
C     W.L. Taber     (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.4.0, 09-SEP-2013 (BVS)
C
C        Updated to keep track of the POOL counter and call ZZCVPOOL.
C
C-    SPICELIB Version 1.3.0, 05-MAR-2009 (NJB)
C
C        This routine now keeps track of whether its kernel pool
C        look-up failed. If so, a kernel pool lookup is attempted on
C        the next call to this routine. This change is an enhancement,
C        not a bug fix (unlike similar modifications in SCLK routines).
C
C-    SPICELIB Version 1.2.1, 15-NOV-2006 (EDW) (NJB)
C
C        Replaced references to LDPOOL with references to FURNSH.
C        Replaced references to RTPOOL with references to GDPOOL.
C        Enhanced long error message associated with missing kernel
C        variables.
C
C-    SPICELIB Version 1.2.0, 17-FEB-1999 (WLT)
C
C        Added a second call to SWPOOL in the event some required
C        kernel pool variable is not supplied. 
C        
C-    SPICELIB Version 1.1.0, 17-MAY-1994 (HAN)
C
C        If the value of the function RETURN is TRUE upon execution of
C        this module, this function is assigned a default value of
C        either 0, 0.0D0, .FALSE., or blank depending on the type of
C        the function.
C
C-    SPICELIB Version 1.0.0, 28-MAR-1992 (WLT)
C
C-&
 
C$ Index_Entries
C
C     Transform between two uniform numeric time systems
C     Transform between two additive numeric time systems
C     Convert one uniform numeric time system to another
C     Convert one additive numeric time system to another
C
C-&
 
 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      J2000
      DOUBLE PRECISION      SPD

      LOGICAL               ELEMC
      LOGICAL               FAILED
      LOGICAL               RETURN
      LOGICAL               SETC
      LOGICAL               SOMFLS
 
C
C     Local parameters
C
 
C
C     LBCELL is the bottom slot of a cell.
C
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )
C
C     NEEDED is the number of kernel pool variables needed by this
C     routine.
C
      INTEGER               NEEDED
      PARAMETER           ( NEEDED = 4 )
 
C
C     LNGVAR is the length of the longest kernel pool variable name
C     that is used by this routine.
C
      INTEGER               LNGVAR
      PARAMETER           ( LNGVAR = 16 )
 
C
C     MISLEN is the length required by the MISSED array of strings
C     used for error messages.
C
      INTEGER               MISLEN
      PARAMETER           ( MISLEN = 20 )
C
C     TYPLEN is the maximum length allowed for names of uniform
C     time types.
C
      INTEGER               TYPLEN
      PARAMETER           ( TYPLEN = 8 )
 
C
C     NTDT is the number of time types based on terrestrial dynamical
C     time (TDT).
C
      INTEGER               NTDT
      PARAMETER           ( NTDT   = 3 )
 
C
C     NTDB is the number of time types base on barycentric dynamical
C     time (TDB).
C
      INTEGER               NTDB
      PARAMETER           ( NTDB   = 4 )
 
 
C
C     NRECOG is the total number of recognized types.
C
      INTEGER               NRECOG
      PARAMETER           ( NRECOG = NTDB + NTDT )
 
C
C     Local variables
C
      CHARACTER*(1)         BSLASH
      CHARACTER*(LNGVAR)    VARS   ( NEEDED )
      CHARACTER*(MISLEN)    MISSED ( NEEDED )
      CHARACTER*(TYPLEN)    MYIN
      CHARACTER*(TYPLEN)    MYOUT
      CHARACTER*(TYPLEN)    RECOG  ( LBCELL : NRECOG    )
      CHARACTER*(TYPLEN)    TYPES  ( LBCELL : 2         )
      CHARACTER*(TYPLEN)    TYPTDB ( LBCELL : NTDB      )
      CHARACTER*(TYPLEN)    TYPTDT ( LBCELL : NTDT      )

      DOUBLE PRECISION      DTA
      DOUBLE PRECISION      EB
      DOUBLE PRECISION      JD2000
      DOUBLE PRECISION      K
      DOUBLE PRECISION      M     ( 0:1 )
      DOUBLE PRECISION      MYTIME
      DOUBLE PRECISION      SECSPD
      DOUBLE PRECISION      TDB
      DOUBLE PRECISION      TDT

      INTEGER               I
      INTEGER               N
      INTEGER               USRCTR ( CTRSIZ ) 

      LOGICAL               FIRST
      LOGICAL               FOUND    ( NEEDED )
      LOGICAL               INTDB
      LOGICAL               INTDT
      LOGICAL               NODATA
      LOGICAL               OUTTDB
      LOGICAL               OUTTDT
      LOGICAL               UPDATE
 
 
C
C     Saved variables
C
      SAVE                  BSLASH
      SAVE                  DTA
      SAVE                  EB
      SAVE                  FIRST
      SAVE                  JD2000
      SAVE                  K
      SAVE                  M
      SAVE                  MISSED
      SAVE                  NODATA
      SAVE                  RECOG
      SAVE                  SECSPD
      SAVE                  TYPTDB
      SAVE                  TYPTDT
      SAVE                  VARS
      SAVE                  USRCTR
 
C
C     Initial values
C
      DATA                  FIRST    / .TRUE. /
 
      DATA                  MISSED   / 'DELTET/DELTA_T_A, #',
     .                                 'DELTET/K, #',
     .                                 'DELTET/EB, #',
     .                                 'DELTET/M, #'           /

      DATA                  NODATA   / .TRUE. /
 
      DATA                  VARS     / 'DELTET/DELTA_T_A',
     .                                 'DELTET/K',
     .                                 'DELTET/EB',
     .                                 'DELTET/M'              /
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         UNITIM = 0.0D0
         RETURN
      END IF

      CALL CHKIN ( 'UNITIM' )
 
 
      IF ( FIRST ) THEN
 
         FIRST  = .FALSE. 
C
C        Initialize the backslash character.  We use this for error
C        message construction.
C
         BSLASH = CHAR(92)

C
C        Set up the parameters that we are going to need often.
C
         SECSPD  = SPD()
         JD2000  = J2000()
 
C
C        Initialize the sets that we will use.
C
         TYPTDT(1) = 'JDTDT'
         TYPTDT(2) = 'TAI'
         TYPTDT(3) = 'TDT'
 
         TYPTDB(1) = 'ET'
         TYPTDB(2) = 'JDTDB'
         TYPTDB(3) = 'JED'
         TYPTDB(4) = 'TDB'
 
         CALL VALIDC ( NTDT,    NTDT,   TYPTDT )
         CALL VALIDC ( NTDB,    NTDB,   TYPTDB )
 
         CALL SSIZEC ( NTDT  +  NTDB,   RECOG  )
         CALL UNIONC ( TYPTDT,  TYPTDB, RECOG  )
 
C
C        Initialize the local POOL counter to user value.
C
         CALL ZZCTRUIN( USRCTR )

C
C        Set up the kernel pool watchers
C
         CALL SWPOOL ( 'UNITIM', NEEDED, VARS )
 
      END IF
 
C
C     Check to see if any of the kernel items required by this
C     routine have been updated since the last call to this
C     entry point.
C
      CALL ZZCVPOOL ( 'UNITIM', USRCTR, UPDATE )
 
      IF ( UPDATE .OR. NODATA ) THEN
C
C        Fetch all of the time parameters from the kernel pool.
C
         CALL GDPOOL ( 'DELTET/DELTA_T_A', 1, 1, N, DTA,  FOUND(1) )
         CALL GDPOOL ( 'DELTET/K',         1, 1, N, K,    FOUND(2) )
         CALL GDPOOL ( 'DELTET/EB',        1, 1, N, EB,   FOUND(3) )
         CALL GDPOOL ( 'DELTET/M',         1, 2, N, M,    FOUND(4) )

         IF ( FAILED() ) THEN

            NODATA = .TRUE.
            UNITIM = 0.D0

            CALL CHKOUT ( 'UNITIM' )
            RETURN

         END IF
            
C
C        If anything wasn't found, it's an error dude.
C
         IF ( SOMFLS ( FOUND, NEEDED ) ) THEN
 
            NODATA = .TRUE.

C
C           If we didn't get all of the things we needed for time
C           conversion, we need to reset the watch.  Otherwise
C           subsequent calls to this routine will never have the
C           needed data.
C
            CALL SWPOOL ( 'UNITIM', NEEDED, VARS )
 
            CALL SETMSG ( 'The following, needed to convert '         //
     .                    'between the input uniform time scales, '   //
     .                    'were not found in the kernel pool: # '     //
     .                    'Your program may have failed to load a '   //
     .                    'leapseconds kernel.  Other possible '      //
     .                    'causes of this problem include loading an '//
     .                    'invalid leapseconds kernel---one that '    //
     .                    'lacks an initial '// BSLASH //'begindata ' //
     .                    'marker or final newline character, or is ' //
     .                    'otherwise corrupted---or deleting previous'//
     .                    'ly loaded kernel pool variables via calls '//
     .                    'to UNLOAD, KCLEAR, or CLPOOL. '            //
     .                    'Use the SPICE routine FURNSH (in Fortran ' //
     .                    'Toolkits, FURNSH is an entry point of '    //
     .                    'KEEPER) to load a leapseconds kernel; '    //
     .                    'make sure the kernel is up to date. '      //
     .                    'See the Kernel and Time Required Reading ' //
     .                    'or the "Intro to Kernels" and "LSK and '   //
     .                    'SCLK" SPICE Tutorials for details.'        )
 
            DO I = 1, NEEDED
               IF ( .NOT. FOUND(I) ) THEN
                  CALL ERRCH ( '#', MISSED(I) )
               END IF
            END DO
 
            CALL ERRCH  ( ', #', '.' )
            CALL SIGERR ( 'SPICE(MISSINGTIMEINFO)' )
 
            CALL CHKOUT ( 'UNITIM' )
            UNITIM = EPOCH
 
            RETURN
 
         END IF

C
C        At this point the kernel data checks are done.
C
         NODATA = .FALSE.

      END IF
 
C
C     Normalize the IN and OUT scale variables
C
      CALL UCASE  ( INSYS,  MYIN  )
      CALL UCASE  ( OUTSYS, MYOUT )
 
      CALL SSIZEC ( 2,      TYPES )
      CALL INSRTC ( MYIN,   TYPES )
      CALL INSRTC ( MYOUT,  TYPES )
 
C
C     We will work with a local copy of EPOCH.
C
      MYTIME = EPOCH
 
C
C     First make sure both types are recognized.
C
      IF      ( .NOT. SETC ( TYPES, '<', RECOG ) ) THEN
 
         CALL SETMSG ( 'The time types recognized by UNITIM are: '    //
     .                 'TAI, TDT, JDTDT, TDB, ET, JED, JDTDB.  At '   //
     .                 'least one of the inputs (#, #) was not in '   //
     .                 'the list of recognized types. '               )
 
         CALL ERRCH  ( '#', MYIN            )
         CALL ERRCH  ( '#', MYOUT           )
         CALL SIGERR ( 'SPICE(BADTIMETYPE)' )
         CALL CHKOUT ( 'UNITIM'             )
 
         UNITIM = EPOCH
         RETURN
 
      END IF
 
C
C     If the input and output types are the same, just copy the input
C     epoch to the output and call it quits.
C
      IF ( MYIN .EQ. MYOUT ) THEN
 
         UNITIM = MYTIME
 
         CALL CHKOUT ( 'UNITIM' )
         RETURN
      END IF
 
 
C
C     Determine the base types of the input and output types.
C
      INTDT  = ELEMC ( MYIN,  TYPTDT )
      OUTTDT = ELEMC ( MYOUT, TYPTDT )
      INTDB  = .NOT. INTDT
      OUTTDB = .NOT. OUTTDT
 
C
C     The two types, TDT and TDB, will be used as the fundamental
C     base used in conversions.
C
C        TAI and JDTDT will be converted to TDT
C        JED and JDTDB will be converted to TDB.
C        (ET is already TDB.)
C
C
      IF      ( MYIN .EQ. 'TAI'   ) THEN
 
         MYTIME = MYTIME + DTA
 
      ELSE IF ( MYIN .EQ. 'JDTDT' ) THEN
 
         MYTIME = ( MYTIME - JD2000 ) * SECSPD
 
      ELSE IF ( MYIN .EQ. 'JED'   ) THEN
 
         MYTIME = ( MYTIME - JD2000 ) * SECSPD
 
      ELSE IF ( MYIN .EQ. 'JDTDB' ) THEN
 
         MYTIME = ( MYTIME - JD2000 ) * SECSPD
 
      END IF
 
C
C     At this point, MYTIME has been converted from its input
C     to one of the base types.
C
C     Next change type from TDB to TDT or vice versa, if
C     required.  (The time is already in TDT or TDB).
C
      IF      ( INTDT .AND. OUTTDB ) THEN
 
         TDT    = MYTIME
         TDB    = TDT + K*SIN(           M(0) + M(1)*TDT
     .                         + EB*SIN( M(0) + M(1)*TDT ) )
 
         MYTIME = TDB
 
      ELSE IF ( INTDB .AND. OUTTDT ) THEN
 
C
C        What we have to do here is invert the formula used to get
C        TDB from TDT that was used above.
C
C        Of course solving the equation
C
C           TDB = TDT + K*SIN { M0 + M1*TDT + EB*SIN( MO + M1*TDT ) }
C
C        analytically for TDT if given TDB is no piece of cake.
C        However, we can get as close as we want to TDT if
C        we notice a few tricks.  First, let's let f(t) denote the
C        function
C
C           f(t) = SIN( M0 + M1*t + EB*SIN( M0 + M1*t ) )
C
C        With this simpler notation we can rewrite our problem
C        as that of solving the equation
C
C           y = t + K*f(t)
C
C        for t given y.  Whichever t satisfies this equation will be
C        unique. The uniqueness of the solution is ensured because the
C        expression on the right-hand side of the equation is
C        monotone increasing in t.
C
C        Let's suppose that t is the solution, then the following
C        is true.
C
C           t = y - K*f(t)
C
C        but we can also replace the t on the right hand side of the
C        equation by y - K*f(t).  Thus
C
C           t = y - K*f( y - K*f(t))
C
C             = y - K*f( y - K*f( y - K*f(t)))
C
C             = y - K*f( y - K*f( y - K*f( y - K*f(t))))
C
C             = y - K*f( y - K*f( y - K*f( y - K*f( y - K*f(t)))))
C             .
C             .
C             .
C             = y - K*f( y - K*f( y - K*f( y - K*f( y - K*f(y - ... )))
C
C        and so on, for as long as we have patience to perform the
C        substitutions.
C
C        The point of doing this recursive substitution is that we
C        hope to move t to an insignificant part of the computation.
C        This would seem to have a reasonable chance of success since
C        K is a small number and f is bounded by 1.
C
C        Following this idea, we will attempt to solve for t using
C        the recursive method outlined below.
C
C        We will make our first guess at t, call it t_0.
C
C         t_0 = y
C
C        Our next guess, t_1, is given by:
C
C         t_1 = y - K*f(t_0)
C
C        And so on:
C
C         t_2 = y - K*f(t_1)        [ = y - K*f(y - K*f(y))            ]
C         t_3 = y - K*f(t_2)        [ = y - K*f(y - K*f(y - K*f(y)))   ]
C             .
C             .
C             .
C         t_n = y - K*f(t_(n-1))    [ = y - K*f(y - K*f(y - K*f(y...)))]
C
C        The questions to ask at this point are:
C
C           1) Do the t_i's converge?
C           2) If they converge, do they converge to t?
C           3) If they converge to t, how fast do they get there?
C
C        1) The sequence of approximations converges.
C
C           | t_n - t_(n-1) | =    [ y - K*f( t_(n-1) ) ]
C                               -  [ y - K*f( t_(n-2) ) ]
C
C                             =  K*[ f( t_(n-2) ) - f( t_(n-1) ) ]
C
C           The function f has an important property. The absolute
C           value of its derivative is always less than M1*(1+EB).
C           This means that for any pair of real numbers s,t
C
C              | f(t) - f(s) |  < M1*(1+EB)*| t - s |.
C
C           From this observation, we can see that
C
C             | t_n - t_(n-1) | < K*M1*(1+EB)*| t_(n-1) - t_(n-2) |
C
C           With this fact available, we could (with a bit more work)
C           conclude that the sequence of t_i's converges and that
C           it converges at a rate that is at least as fast as the
C           sequence L, L**2, L**3, ....
C
C           Where L = K*M1*(1+EB) << 1.
C
C         2) If we let t be the limit of the t_i's then it follows
C            that
C
C               t = y - K*f(t).
C
C            or that
C
C               y = t + K*f(t).
C
C         3) As we already pointed out, the sequence of t_i's
C            converges at least as fast as the geometric series
C            L, L**2, ...
C
C
C        Since K*M1*(1+EB) is quite small (on the order of 10**-9)
C        3 iterations should get us as close as we can get to the
C        solution for TDT
C
 
         TDB = MYTIME
         TDT = TDB
 
         DO I = 1, 3
 
            TDT = TDB - K*SIN(           M(0) + M(1)*TDT
     .                         + EB*SIN( M(0) + M(1)*TDT ))
         END DO
 
         MYTIME = TDT
 
      END IF
 
C
C     Now MYTIME is in the base type of the requested output.
C     If further conversion is required, we do it here.
C
      IF      ( MYOUT .EQ. 'TAI'   ) THEN
 
         MYTIME = MYTIME - DTA
 
      ELSE IF ( MYOUT .EQ. 'JDTDT' ) THEN
 
         MYTIME = MYTIME/SECSPD + JD2000
 
      ELSE IF ( MYOUT .EQ. 'JED'   ) THEN
 
         MYTIME = MYTIME/SECSPD + JD2000
 
      ELSE IF ( MYOUT .EQ. 'JDTDB' ) THEN
 
         MYTIME = MYTIME/SECSPD + JD2000
 
      END IF
 
      UNITIM = MYTIME
 
      CALL CHKOUT ( 'UNITIM' )
      RETURN
      END
