C$Procedure DASINE (arc sine of bracketed argument)
 
      DOUBLE PRECISION FUNCTION DASINE ( ARG, TOL )
 
C$ Abstract
C
C     This routine produces a SPICE error if the |argument| exceeds
C     1.D0 by more than TOL. If ARG exceeds 1.D0, the argument is
C     evaluated as if it equaled 1.D0, if ARG is less than -1.,
C     the argument is evaluated as if it equaled -1.D0.
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
C      INTERVALS,  NUMBERS,  UTILITY, INVERSE TRIGONOMETRIC FUNCTION
C
C$ Declarations
 
      DOUBLE PRECISION   ARG
      DOUBLE PRECISION   TOL
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      ARG        I   Argument to be evaluated.
C      TOL        I   Tolerance.
C      DASINE     O   The function returns the arc sine of ARG.
C
C$ Detailed_Input
C
C      ARG         is the arc sine argument that is to be evaluated
C                  such that if it is less than -1.D0 by more than TOL
C                  or greater than 1.D0 by more than TOL, an error
C                  results.
C
C      TOL         is a tolerance such that |ARG| is considered to be
C                  equal to 1.D0 if |ARG| <= 1.D0 + TOL. TOL must be
C                  non-negative.
C
C$ Detailed_Output
C
C      DASINE      The function returns the arc sine of ARG. If |ARG|
C                  >= 1.D0, it returns DASIN (1.D0) or DASIN (-1.D0) as
C                  appropriate. Values range from -PI/2 to PI/2.
C
C$ Parameters
C
C      None.
C
C$ Exceptions
C
C     1) If |ARG| > 1.D0 + TOL, the error SPICE(INPUTOUTOFBOUNDS) is
C        signaled.
C
C     2) If TOL is less than zero, the error SPICE(VALUEOUTOFRANGE) is
C        signaled.
C
C$ Files
C
C      None.
C
C$ Particulars
C
C      This routine determines whether |ARG| > 1.D0 + TOL. If
C      it is, an error will be flagged. In addition, 
C      the values of ARG are constrained to [-1.D0, 1.D0].
C
C$ Examples
C
C      The following illustrate the operation of DASINE.
C
C            DASINE (  -1.D0,        1.D-7 )  =  -PI/2
C            DASINE (  -1.00001D0,   1.D-3 )  =  -PI/2 
C            DASINE (  -1.00001D0,   1.D-7 )  =  -PI/2 (error flagged)
C            DASINE (   0.D0,        1.D-7 )  =   0.D0
C            DASINE (   1.00001D0,   1.D-3 )  =   PI/2
C            DASINE (   1.00001D0,   1.D-7 )  =   PI/2 (error flagged)
C
C$ Restrictions
C
C      None.
C
C$ Author_and_Institution
C
C     L.S. Elson      (JPL)
C
C$ Literature_References
C
C      None.
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 28-FEB-2006 (LSE)
C
C-&
 
C$ Index_Entries
C
C     check a d.p. argument for ASIN before evaluation
C
C-& 

C
C     Bracket ARG.
C

      DASINE      =  ASIN ( MAX ( - 1.D0, MIN ( 1.D0, ARG ) ) )

C
C     Check that tolerance is non negative.
C

      IF ( TOL .LT. 0.D0 ) THEN

         CALL CHKIN  ( 'DASINE'                           )
         CALL SETMSG ( 'TOL was #; must be non-negative.' )
         CALL ERRDP  ( '#',  TOL                          )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'           )
         CALL CHKOUT ( 'DASINE'                           )
         RETURN

      END IF

C
C     Check to see if |ARG| is within TOL of 1.D0. Signal error if
C     appropriate.
C

      IF  ( ( ABS ( ARG ) - TOL ) .GT. 1.D0) THEN 

         CALL CHKIN  ( 'DASINE'                           )
         CALL SETMSG ( 'The |argument| specified was greater '//
     .                 'than 1.D0 by more than #. The value '//
     .                 'of the argument is #. '           )
         CALL ERRDP  ( '#',  TOL                          )
         CALL ERRDP  ( '#',  ARG                          )
         CALL SIGERR ( 'SPICE(INPUTOUTOFBOUNDS)'          )
         CALL CHKOUT ( 'DASINE'                           )
         RETURN

      END IF

      RETURN
      END
