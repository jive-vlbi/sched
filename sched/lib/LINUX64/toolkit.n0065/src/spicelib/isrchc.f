C$Procedure             ISRCHC  ( Search in a character array )
 
      INTEGER FUNCTION  ISRCHC ( VALUE, NDIM, ARRAY )
 
C$ Abstract
C
C      Search for a given value within a character string array. Return
C      the index of the first matching array entry, or zero if the key
C      value was not found.
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
C      ARRAY,  SEARCH
C
C$ Declarations
 
      CHARACTER*(*)    VALUE
      INTEGER          NDIM
      CHARACTER*(*)    ARRAY ( * )
 
C$ Brief_I/O
C
C      VARIABLE  I/O              DESCRIPTION
C      --------  ---  --------------------------------------------------
C       VALUE     I     Key value to be found in ARRAY.
C       NDIM      I     Dimension of ARRAY.
C       ARRAY     I     Character string array to search.
C
C       The function returns the index of the first matching array
C       element or zero if the value is not found.
C
C$ Detailed_Input
C
C      VALUE      is the key value to be found in the array.
C
C      NDIM       is the dimension of the array.
C
C      ARRAY      is the character array to be searched.
C
C$ Detailed_Output
C
C      The function returns the index of the first matching array
C      element in ARRAY. If VALUE is not found, ISRCHC is zero.
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
C      The following table shows the value of ISRCHC given the contents
C      of ARRAY and VALUE:
C
C        ARRAY                 VALUE     ISRCHC
C      -----------------       -----     ------
C      '1', '0', '4', '2'       '4'        3
C      '1', '0', '4', '2'       '2'        4
C      '1', '0', '4', '2'       '3'        0
C
C$ Restrictions
C
C      None.
C
C$ Exceptions
C
C     Error free.
C
C     1) If NDIM < 1 the function value is zero.
C
C$ Files
C
C      None
C
C$ Author_and_Institution
C
C      W.M. Owen       (JPL)
C
C$ Literature_References
C
C      None
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
C     search in a character array
C
C-&
 
 
 
C
C     Local variables
C
      INTEGER I
 
 
      ISRCHC = 0
 
      DO I = 1, NDIM
 
         IF ( ARRAY(I) .EQ. VALUE ) THEN
            ISRCHC = I
            RETURN
         END IF
 
      END DO
 
      RETURN
      END
