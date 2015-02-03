C$Procedure GFSTOL ( GF, set a tolerance value for GF )

      SUBROUTINE GFSTOL ( VALUE )

C$ Abstract
C
C     Override the default GF convergence value used in the high
C     level GF routines.
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
C     GF
C
C$ Keywords
C
C     GEOMETRY
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE               'zzholdd.inc'

      DOUBLE PRECISION      VALUE

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     ZZPUT      P   ZZHOLDD stores a DP value.
C     GF_TOL     P   ZZHOLDD acts on the GF subsystem tolerance.
C     VALUE      I   Double precision value returned or to store.
C
C$ Detailed_Input
C
C     VALUE       The scalar double precision value to use as the GF
C                 subsystem convergence tolerance. This value will
C                 override the default tolerance, CNVTOL, defined in
C                 gf.inc. Units are TDB seconds.
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
C     1)  The error SPICE(INVALIDTOLERANCE) signals if VALUE is not
C         strictly greater-than-zero.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     The high level GF routines (see gf.req for a listing) use a
C     default value for the convergence tolerance, CNVTOL, defined in
C     gf.inc. It may occur that a GF search run needs a different
C     convergence tolerance. GFSTOL programmatically changes the
C     tolerance used by those routines.
C
C$ Examples
C
C     The numerical results shown for these examples may differ across
C     platforms. The results depend on the SPICE kernels used as
C     input, the compiler and supporting libraries, and the machine
C     specific arithmetic implementation.
C
C        Use the meta-kernel shown below to load the required SPICE
C        kernels.
C
C           KPL/MK
C
C           File name: standard.tm
C
C           This meta-kernel is intended to support operation of SPICE
C           example programs. The kernels shown here should not be
C           assumed to contain adequate or correct versions of data
C           required by SPICE-based user applications.
C
C           In order for an application to use this meta-kernel, the
C           kernels referenced here must be present in the user's
C           current working directory.
C
C           The names and contents of the kernels referenced
C           by this meta-kernel are as follows:
C
C              File name                     Contents
C              ---------                     --------
C              de421.bsp                     Planetary ephemeris
C              pck00009.tpc                  Planet orientation and
C                                            radii
C              naif0009.tls                  Leapseconds
C
C           \begindata
C
C              KERNELS_TO_LOAD = ( 'de421.bsp',
C                                  'pck00009.tpc',
C                                  'naif0009.tls'  )
C
C           \begintext
C
C        Example:
C
C        Perform a search for occultation events of the sun by earth as
C        observed from the Moon center. Search during the interval from
C        14 A.D. SEP 1 to 14 A.D. SEP 30 (Julian).
C
C           PROGRAM GFSTOL_T
C
C           IMPLICIT NONE
C
C           INTEGER               WNCARD
C
C           CHARACTER*(*)         TIMFMT
C           PARAMETER           ( TIMFMT =
C          .                   'YYYY ERA MON DD HR:MN:SC.#### ::JCAL' )
C
C           INTEGER               MAXWIN
C           PARAMETER           ( MAXWIN = 2 * 100 )
C
C           INTEGER               TIMLEN
C           PARAMETER           ( TIMLEN = 40 )
C
C           INTEGER               LBCELL
C           PARAMETER           ( LBCELL = -5 )
C
C           CHARACTER*(TIMLEN)    WIN0
C           CHARACTER*(TIMLEN)    WIN1
C           CHARACTER*(TIMLEN)    BEGSTR
C           CHARACTER*(TIMLEN)    ENDSTR
C
C           DOUBLE PRECISION      CNFINE ( LBCELL : MAXWIN )
C           DOUBLE PRECISION      RESULT ( LBCELL : MAXWIN )
C           DOUBLE PRECISION      ET0
C           DOUBLE PRECISION      ET1
C           DOUBLE PRECISION      LEFT
C           DOUBLE PRECISION      RIGHT
C           DOUBLE PRECISION      STEP
C
C           INTEGER               I
C
C           LOGICAL               OK
C
C
C     C
C     C     Load kernels.
C     C
C           CALL FURNSH ( 'standard.tm' )
C
C     C
C     C     Use an SPK covering year 14 AD.
C     C
C           CALL FURNSH ( 'de408.bsp' )
C
C     C
C     C     Initialize the confinement and result windows.
C     C
C           CALL SSIZED ( MAXWIN, CNFINE )
C           CALL SSIZED ( MAXWIN, RESULT )
C
C     C
C     C     Obtain the TDB time bounds of the confinement
C     C     window, which is a single interval in this case.
C     C
C           WIN0 = '14 A.D. SEP 1  00:00:00'
C           WIN1 = '14 A.D. SEP 30 00:00:00'
C
C           CALL STR2ET ( WIN0, ET0 )
C           CALL STR2ET ( WIN1, ET1 )
C
C     C
C     C     Insert the time bounds into the confinement
C     C     window.
C     C
C           CALL WNINSD ( ET0, ET1, CNFINE )
C
C     C
C     C     Select a 3-minute step. We'll ignore any occultations
C     C     lasting less than 3 minutes.
C     C
C           STEP = 180.D0
C
C     C
C     C     Perform the search. ET0 and ET1 have values ~-6*10^10,
C     C     CNVTOL has value 10^-6, so double precision addition or
C     C     subtraction of ET0 and ET1 with CNVTOL returns a result
C     C     indistinguishable from ET0 and ET1.
C     C
C     C     Reduce the GF convergence tolerance by an order of
C     C     magnitude to resolve this condition.
C     C
C           CALL GFSTOL ( 1D-5 )
C
C           CALL GFOCLT ( 'ANY',
C          .              'EARTH', 'ellipsoid', 'IAU_EARTH',
C          .              'SUN',   'ellipsoid', 'IAU_SUN',
C          .              'LT',    'MOON',       STEP,
C          .               CNFINE,  RESULT  )
C
C
C           IF ( WNCARD(RESULT) .EQ. 0 ) THEN
C
C              WRITE (*,*) 'No occultation was found.'
C
C           ELSE
C
C              DO I = 1, WNCARD(RESULT)
C
C     C
C     C           Fetch and display each occultation interval.
C     C
C                 CALL WNFETD ( RESULT, I, LEFT, RIGHT )
C
C                 CALL TIMOUT ( LEFT,  TIMFMT, BEGSTR )
C                 CALL TIMOUT ( RIGHT, TIMFMT, ENDSTR )
C
C                 WRITE (*,*) 'Interval ', I
C                 WRITE (*,*) '   Start time: '//BEGSTR
C                 WRITE (*,*) '   Stop time:  '//ENDSTR
C
C              END DO
C
C           END IF
C
C           END
C
C    The program outputs:
C
C       Interval            1
C          Start time:   14 A.D. SEP 27 05:02:02.8250
C          Stop time:    14 A.D. SEP 27 09:33:31.6995
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
C     E.D. Wright    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0  18-APR-2014 (EDW)
C
C-&

C$ Index_Entries
C
C     change default convergence tolerance for GF routines
C
C-&

C
C     SPICELIB functions
C

      LOGICAL               RETURN
      
C
C     Local variables.
C

      LOGICAL               OK

C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF

C
C     This routine wraps a call to ZZHOLDD with the appropriate ID and
C     OP value.
C

C
C     Check tolerance value.
C

      IF ( VALUE .LE. 0.D0 ) THEN

         CALL CHKIN  ( 'GFSTOL'                              )
         CALL SETMSG ( 'Convergence tolerance must be '
     .              // 'greater-than zero. Input VALUE = #.' )
         CALL ERRDP  ( '#', VALUE                            )
         CALL SIGERR ( 'SPICE(INVALIDTOLERANCE)'             )
         CALL CHKOUT ( 'GFSTOL'                              )
         RETURN

      ELSE

         CALL ZZHOLDD ( ZZPUT, GF_TOL, OK, VALUE )

      END IF


      END

