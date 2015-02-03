C$Procedure      RMAINI ( Remainder --- integer )
 
      SUBROUTINE RMAINI ( NUM, DENOM, Q, REM )
 
C$ Abstract
C
C     Compute the integer quotient and non-negative remainder
C     of NUM and DENOM.
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
C     MATH
C     UTILITY
C
C$ Declarations
 
      INTEGER               NUM
      INTEGER               DENOM
      INTEGER               Q
      INTEGER               REM
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     NUM        I   Numerator used to compute quotient and remainder.
C     DENOM      I   Denominator used to compute quotient and remainder.
C     Q          O   Integer portion of the quotient NUM/DENOM.
C     REM        O   Remainder of the quotient NUM/DENOM.
C
C$ Detailed_Input
C
C     NUM        is the numerator of a quotient
C
C     DENOM      is the denominator of a quotient
C
C$ Detailed_Output
C
C     Q          is the largest integer less than or equal to the
C                quotient NUM/DENOM
C
C     REM        is the remainder of the integer division NUM/DENOM
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If DENOM is zero, the error 'SPICE(DIVIDEBYZERO)' will be
C        signalled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     Given the integer inputs NUM and DENOM, this routine
C     finds integers Q and REM that satisfy the following conditions:
C
C         1) NUM = DENOM * Q + REM
C
C         2) REM is a non negative integer less than the absolute
C            value of DENOM.
C
C     This routine serves as a macro.  In this way the code to perform
C     this task can be written and maintained in a single location.
C
C$ Examples
C
C     One frequently needs to compute the  ``360 modulus'' of a
C     number.  For positive numbers the FORTRAN intrinsic mod
C     function works well.  However, for negative numbers the
C     intrinsic will return a negative modulus.  This routine
C     can be used to compute the positive 360 pi modulus (MOD360) for
C     any integer I by the call:
C
C         CALL RMAINI ( I, 360, Q, MOD360 )
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
C     W.L. Taber     (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 01-DEC-1995 (WLT)
C
C-&
 
C$ Index_Entries
C
C     Compute the remainder of an integer division
C
C-&
 
C
C     Take care of the zero-denominator case first
C
      IF ( DENOM .EQ. 0.0D0 ) THEN
         CALL CHKIN  ( 'RMAINI'                                       )
         CALL SETMSG ( 'Attempting to compute a quotient with a '     //
     .                 'divide by zero.'                              )
         CALL SIGERR ( 'SPICE(DIVIDEBYZERO)'                          )
         CALL CHKOUT ( 'RMAINI'                                       )
         RETURN
      END IF
 
      Q   = NUM / DENOM
      REM = NUM - DENOM*Q
 
      IF ( REM .LT. 0 ) THEN
         Q   = Q   - 1
         REM = REM + DENOM
      END IF
 
      RETURN
      END
