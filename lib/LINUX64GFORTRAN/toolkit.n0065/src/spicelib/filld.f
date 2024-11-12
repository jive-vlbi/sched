C$Procedure      FILLD ( Fill a double precision array )
 
      SUBROUTINE FILLD ( VALUE, NDIM, ARRAY )
 
C$ Abstract
C
C      Fill a double precision array with a specified value.
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
C      ARRAY,  ASSIGNMENT
C
C$ Declarations
 
      DOUBLE PRECISION    VALUE
      INTEGER             NDIM
      DOUBLE PRECISION    ARRAY ( * )
 
C$ Brief_I/O
C
C  VARIABLE  I/O              DESCRIPTION
C  --------  ---  ------------------------------------------------------
C   VALUE     I     Double precision value to be placed in all the
C                   elements of ARRAY.
C   NDIM      I     The number of elements in ARRAY.
C   ARRAY     O     Double precision array which is to be filled.
C
C$ Detailed_Input
C
C      VALUE     is the value to be assigned to the array elements
C                1 through NDIM.
C
C      NDIM      is the number of elements in the array.
C
C$ Detailed_Output
C
C      ARRAY     is a double precision array whose elements are to be
C                set to VALUE.
C
C$ Parameters
C
C      None.
C
C$ Particulars
C
C      None.
C
C$ Examples
C
C       Let  VALUE = 1.0D0
C            NDIM  =  4
C
C       then the contents of ARRAY are:
C
C            ARRAY (1) = 1.0D0
C            ARRAY (2) = 1.0D0
C            ARRAY (3) = 1.0D0
C            ARRAY (4) = 1.0D0
C
C$ Restrictions
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C     1) If NDIM < 1 the array is not modified.
C
C$ Files
C
C      None.
C
C$ Author_and_Institution
C
C      W.M. Owen       (JPL)
C
C$ Literature_References
C
C      None.
C
C$ Version
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
C
C-&
 
C$ Index_Entries
C
C     fill a d.p. array
C
C-&
 
 
 
 
C
C     Local variables
C
      INTEGER I
 
 
      DO I = 1, NDIM
        ARRAY(I) = VALUE
      END DO
 
 
      RETURN
      END
