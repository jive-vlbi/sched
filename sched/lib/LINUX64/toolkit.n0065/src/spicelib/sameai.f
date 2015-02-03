C$Procedure            SAMEAI ( Are two integer arrays the same? )
 
      LOGICAL FUNCTION SAMEAI ( A1, A2, NDIM )
 
C$ Abstract
C
C     Indicate whether two integer arrays are equal.
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
C     ARRAY
C
C$ Declarations
 
      INTEGER               A1 ( * )
      INTEGER               A2 ( * )
      INTEGER               NDIM
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     A1         I    First array to be compared.
C     A2         I    Second array to be compared.
C     NDIM       I    Dimension of A1 and A2.
C
C     The function returns the value .TRUE. if and only if A1 = A2.
C
C$ Detailed_Input
C
C     A1,
C     A2              are two integer arrays to be compared.  A1 and
C                     A2 must have the same dimension.
C
C     NDIM            is the common dimension of A1 and A2.
C
C$ Detailed_Output
C
C     The function takes the value .TRUE. if and only if A1 equals A2.
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
C     This function can be thought of as a macro.  It replaces the
C     loop
C
C        SAME  = .TRUE.
C        I     =  1
C
C        DO WHILE (  ( I .LE. NDIM )  .AND.  SAME  )
C
C           IF ( A1(I) .NE.  A2(I)  )
C              SAME  = .FALSE.
C           ELSE
C              I     =  I + 1
C           END IF
C
C        END DO
C
C
C$ Examples
C
C
C     1)  Test two integer arrays A1 and A2 for equality, where both
C         arrays have declared length 10:
C
C            SAME  =  SAMEAI ( A1, A2, 10 )
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
C
C$ Version
C
C-    Beta Version 1.0.0, 19-DEC-1995  (NJB)
C
C-&
 
C$ Index_Entries
C
C     test two integer arrays for equality
C
C-&
 
C
C     Local variables
C
      INTEGER               I
 
C
C     Executable code
C
      SAMEAI = .TRUE.
 
      DO I = 1, NDIM
 
         IF (  A1(I)  .NE.  A2(I)  ) THEN
            SAMEAI = .FALSE.
            RETURN
         END IF
 
      END DO
 
      RETURN
      END
