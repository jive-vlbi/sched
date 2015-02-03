C$Procedure             ODD ( Is a number odd? )
 
      LOGICAL FUNCTION  ODD ( I )
 
C$ Abstract
C
C     Determine whether an integer is odd.
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
C     NUMBERS
C
C$ Declarations
 
      INTEGER          I
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     I          I   The integer in question.
C     ODD        O   True if I is odd, otherwise false.
C
C$ Detailed_Input
C
C     I           is the integer to be tested for oddness.
C
C$ Detailed_Output
C
C     ODD is returned as true if I is odd, false if I is even.
C
C$ Parameters
C
C     None.
C
C$ Particulars
C
C     Divide I by two. If the remainder is one, I is odd.
C
C$ Examples
C
C     Let ENDPTS contain a series of endpoints,
C
C        a , b , ..., a , b
C         1   1        n   n
C
C     representing an ordered collection of disjoint intervals,
C
C        a   <  b   < a
C         i  -   i     i+1
C
C     The following code fragment uses ODD to determine whether
C     an arbitrary value X is contained in any of the intervals.
C
C        CONTAINED = .FALSE.
C
C        DO I = 1, N-1
C           IF ( X .GE. ENDPTS(I)  .AND.  X .LE. ENDPTS(I+1) ) THEN
C              CONTAINED = ( ODD ( I ) )
C           END IF
C        END DO
C
C$ Restrictions
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
C$ Author_and_Institution
C
C     I.M. Underwood  (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 1.0.2, 07-NOV-2005 (BVS)
C
C        Fixed a few typos in the header.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
C
C-&
 
C$ Index_Entries
C
C     test whether an integer is odd
C
C-&
 
 
 
C$ Revisions
C
C-    Beta Version 1.0.1, 27-JAN-1989 (IMU)
C
C        Examples section completed.
C
C-&
 
 
 
C
C     Self-explanatory.
C
      ODD = ( MOD (I,2) .NE. 0 )
 
      RETURN
      END
