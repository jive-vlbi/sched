C$Procedure GFSTEP ( GF, step size )

      SUBROUTINE GFSTEP ( TIME, STEP )

C$ Abstract
C
C     Return the time step set by the most recent call to GFSSTP.
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
C     TIME
C
C$ Keywords
C
C     GEOMETRY
C     SEARCH
C     UTILITY
C
C$ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION      STEP
      DOUBLE PRECISION      TIME

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     TIME       I   Ignored ET value.
C     STEP       O   Time step to take.
C
C$ Detailed_Input
C
C     TIME      is an ignored double precision number. This argument
C               is present so the argument list of this routine is
C               compatible with the GF step size routine argument list
C               specification.
C
C               When this routine is called from within the GF
C               root-finding system, either the initial ET value of the
C               current interval of the confinement window, or the
C               value resulting from the last search step, is passed in
C               via the TIME argument.
C
C$ Detailed_Output
C
C     STEP      is the output step size. This is the value set by the
C               most recent call to GFSSTP. Units are TDB seconds.
C
C               STEP is used in the GF search root-bracketing process.
C               STEP indicates how far to advance TIME so that TIME and
C               TIME+STEP may bracket a state transition and definitely
C               do not bracket more than one state transition.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If this routine is called before a step size has been
C        set via a call to GFSSTP, the error SPICE(NOTINITIALIZED)
C        is signaled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine returns the time step set by the most recent call to
C     GFSSTP.
C
C$ Examples
C
C     1) In normal usage of a high-level GF API routine, the caller
C        will pass in a constant step size STEP. The API routine will
C        then make the call
C
C           CALL GFSSTP ( STEP )
C
C        Subsequent calls to GFSTEP during the search process conducted
C        by the API routine will return STEP.
C
C
C     2) User applications can pass GFSTEP to mid-level GF API routines
C        expecting a step size routine as an input argument. For
C        example, the GF API routine GFOCCE can be called as follows:
C
C
C           Set the step size.
C
C           CALL GFSSTP ( STEP )
C
C
C           Look for solutions. (GFSTEP is the 11th argument.)
C
C           CALL GFOCCE ( OCCTYP,  FRONT,   FSHAPE,  FFRAME,
C          .              BACK,    BSHAPE,  BFRAME,  ABCORR,
C          .              OBSRVR,  CNVTOL,  GFSTEP,  GFREFN,
C          .              RPT,     GFREPI,  GFREPU,  GFREPF,
C          .              BAIL,    GFBAIL,  CNFINE,  RESULT )
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
C     L.S. Elson     (JPL)
C     W.L. Taber     (JPL)
C     I.M. Underwood (JPL)
C     E.D. Wright    (JPL)
C
C$ Version
C
C-    SPICELIB version 1.1.0 31-AUG-2010 (EDW)
C
C        Expanded error message on STEP for clarity.
C
C        Added TIME = TIME declaration to eliminate unused dummy
C        variable warning during compilation.
C
C-    SPICELIB Version 1.0.0 05-MAR-2009 (NJB) (LSE) (IMU) (WLT) (EDW)
C
C-&

C$ Index_Entries
C
C     GF get constant step size
C
C-&

C
C     SPICELIB functions
C

C
C     Local variables
C
      DOUBLE PRECISION      SVSTEP

      LOGICAL               SVINIT

C
C     Saved variables
C
      SAVE                  SVSTEP
      SAVE                  SVINIT

C
C     Initial values
C
      DATA                  SVINIT / .FALSE. /
      DATA                  SVSTEP / -1.0D0  /


C
C     Discovery check-in.
C
      IF ( .NOT. SVINIT ) THEN

         CALL CHKIN  ( 'GFSTEP'                           )
         CALL SETMSG ( 'Step size was never initialized.' )
         CALL SIGERR ( 'SPICE(NOTINITIALIZED)'            )
         CALL CHKOUT ( 'GFSTEP'                           )
         RETURN

      END IF

C
C     Set STEP to the saved value from the last call to GFSSTP.
C
      STEP = SVSTEP
      TIME = TIME

      RETURN



C$Procedure GFSSTP ( Geometry finder set step size )

      ENTRY GFSSTP ( STEP )

C$ Abstract
C
C     Set the step size to be returned by GFSTEP.
C
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
C     TIME
C
C$ Keywords
C
C     GEOMETRY
C     SEARCH
C     UTILITY
C
C$ Declarations
C
C     DOUBLE PRECISION      STEP
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     STEP       I   Time step to take.
C
C$ Detailed_Input
C
C     STEP      is the output step size to be returned by the next call
C               GFSTEP. Units are TDB seconds.
C
C               STEP is used in the GF search root-bracketing process.
C               STEP indicates how far to advance TIME so that TIME and
C               TIME+STEP may bracket a state transition and definitely
C               do not bracket more than one state transition.
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
C     1) If the input step size is non-positive, the error
C        SPICE(INVALIDSTEP) is signaled. The stored step value
C        is not updated.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     See the header of GFSTEP above.
C
C$ Examples
C
C     See the header of GFSTEP above.
C
C$ Restrictions
C
C     This routine must be called before the first time
C     GFSTEP is called during a program run.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C     L.S. Elson     (JPL)
C     W.L. Taber     (JPL)
C     I.M. Underwood (JPL)
C
C$ Version
C
C-    SPICELIB version 1.1.0 31-AUG-2010 (EDW)
C
C        Expanded error message on STEP for clarity.
C
C-    SPICELIB version 1.0.0 15-APR-2009 (LSE) (NJB)
C
C-&

C$ Index_Entries
C
C     GF set constant step size
C
C-&

C
C     Check the step size.
C
      IF ( STEP .LE. 0.D0 ) THEN

         CALL CHKIN  ( 'GFSSTP'                                       )
         CALL SETMSG ( 'Step has value #; step size must be positive.')
         CALL ERRDP  ( '#',  STEP                                     )
         CALL SIGERR ( 'SPICE(INVALIDSTEP)'                           )
         CALL CHKOUT ( 'GFSSTP'                                       )
         RETURN

      END IF

      SVSTEP = STEP
      SVINIT = .TRUE.

      RETURN
      END
