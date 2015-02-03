C$Procedure      RMAIND ( Remainder --- double precision )
 
      SUBROUTINE RMAIND ( NUM, DENOM, Q, REM )
 
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
 
      DOUBLE PRECISION      NUM
      DOUBLE PRECISION      DENOM
      DOUBLE PRECISION      Q
      DOUBLE PRECISION      REM
 
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
C     Given the double precision inputs NUM and DENOM, this routine
C     finds double precision numbers Q and REM that satisfy the
C     following conditions:
C
C         1) NUM = DENOM * Q + REM
C
C         2) DENOM has integer value.
C
C         3) REM belongs to the half open interval [0, ABS(DENOM) )
C
C     This routine serves as a macro.  In this way the code to perform
C     this task can be written and maintained in a single location.
C
C$ Examples
C
C     One frequently needs to compute the  ``Two pi modulus'' of a
C     number.  For positive numbers the FORTRAN intrinsic mod
C     function works well.  However, for negative numbers the
C     intrinsic will return a negative modulus.  This routine
C     can be used to compute the positive two pi modulus (MOD2PI) for
C     any number X by the call:
C
C         CALL RMAIND ( X, TWOPI(), I, MOD2PI )
C
C$ Restrictions
C
C     Arithmetic overflows are not trapped or detected by this routine.
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
C
C-&
 
C$ Index_Entries
C
C     Compute the remainder of a double precision division
C
C-&
 
      DOUBLE PRECISION      MYNUM
      DOUBLE PRECISION      MYDNOM
 
C
C     Take care of the zero-denominator case first
C
      IF ( DENOM .EQ. 0.0D0 ) THEN
         CALL CHKIN  ( 'RMAIND'                                       )
         CALL SETMSG ( 'Attempting to compute a quotient with a '     //
     .                 'divide by zero.'                              )
         CALL SIGERR ( 'SPICE(DIVIDEBYZERO)'                          )
         CALL CHKOUT ( 'RMAIND'                                       )
         RETURN
      END IF
 
      MYDNOM = DENOM
      MYNUM  = NUM
 
      Q   = DINT ( MYNUM / MYDNOM )
      REM = MYNUM -    Q * MYDNOM
 
      IF ( REM .LT. 0.0D0 ) THEN
         Q   = Q   - 1.0D0
         REM = REM + MYDNOM
      END IF
 
      RETURN
      END
