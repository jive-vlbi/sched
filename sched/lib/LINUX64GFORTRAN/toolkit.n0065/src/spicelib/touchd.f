C$Procedure      TOUCHD ( Touch a variable )
 
      DOUBLE PRECISION FUNCTION TOUCHD ( DP )
 
C$ Abstract
C
C     Return the value of a double precision number.
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
C      None.
C
C$ Keywords
C
C       UTILITY
C
C$ Declarations
 
      IMPLICIT NONE
      DOUBLE PRECISION      DP
 
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      DP        I   any double precision number
C
C       The function returns the value of DP.
C
C$ Detailed_Input
C
C      DP          is any double precision number
C
C$ Detailed_Output
C
C     The function returns the input d.p.
C
C$ Parameters
C
C      None.
C
C$ Files
C
C      None.
C
C$ Exceptions
C
C     Error free.
C
C
C$ Particulars
C
C     This is a utility routine so that formal arguments to a routine
C     that are never used can be given the appearance of being used
C     to a compiler.  In this way it is possible to leave "hooks" in
C     a calling sequence even if those variables are for the moment
C     unused.  Similarly, variables declared for future use can be left
C     in place so that they don't need to be commented out
C
C$ Examples
C
C     Suppose that a routine takes as an argument a
C     fortran structure implemented as a set of parallel arrays.
C     But that one of the arrays is not needed for the purposes of
C     the routine.  This routine allows you to touch that array
C     without changing it.
C
C
C        SUBROUTINE INCPTR ( N, PTR, VALUES )
C
C        This routine increments the current pointer into a circular
C        array of double precision numbers.
C
C        INTEGER               N
C        INTEGER               PTR
C        DOUBLE PRECISION      VALUES ( * )
C
C        Even though we don't need to do anything with the values
C        array, it's passed for the sake of uniformity in calling
C        sequences.  Touch the VALUES array so that the compiler
C        will think it's been used.
C
C        VALUES(1) = TOUCHD ( VALUES(1) )
C
C        PTR = PTR + 1
C
C        IF ( PTR .GT. N ) THEN
C           PTR = 1
C        END IF
C        RETURN
C
C
C
C$ Restrictions
C
C     If you use this routine, it would be a very good idea to
C     write down why you are using it in the calling routine.
C
C$ Author_and_Institution
C
C      W.L. Taber      (JPL)
C
C$ Literature_References
C
C      None.
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 6-MAy-1994 (WLT)
C
C
C-&
 
C$ Index_Entries
C
C     touch a d.p. number
C
C-&
 
      TOUCHD = DP
      RETURN
      END
