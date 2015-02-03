C$Procedure      SOMFLS ( Some entries false? )
 
      LOGICAL FUNCTION SOMFLS ( LOGCLS, N )
 
C$ Abstract
C
C     Determine if some of the entries in an array of logicals are
C     .FALSE.
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
C     UTILITY
C
C$ Declarations
 
      LOGICAL               LOGCLS ( * )
      INTEGER               N
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     LOGCLS     I   An array of logicals.
C     N          I   Number of elements in the array LOGCLS.
C
C     The function returns .TRUE. if some of the values in the array
C     LOGCLS are false.
C
C$ Detailed_Input
C
C     LOGCLS     is an array of logicals.
C
C     N          is the number of elements in the array LOGCLS
C
C$ Detailed_Output
C
C     The function returns true if the value of some entry of LOGCLS
C     is .FALSE.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C     If N is less than 1, the function returns a value of .FALSE.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This function examines each element of LOGCLS until
C     a .FALSE. value is found or until all values have been
C     examined.
C
C$ Examples
C
C     Suppose you need to confirm that a character set
C     WORDS does not contain at least one of the words in the phrase
C
C       'EVERY GOOD BOY DOES FINE'
C
C     You might execute the following block of code.
C
C           FOUND(1) = ELEMC  ( 'EVERY', WORDS )
C           FOUND(2) = ELEMC  ( 'GOOD',  WORDS )
C           FOUND(3) = ELEMC  ( 'BOY',   WORDS )
C           FOUND(4) = ELEMC  ( 'DOES',  WORDS )
C           FOUND(5) = ELEMC  ( 'FINE',  WORDS )
C
C           OK       = SOMFLS ( FOUND,   5     )
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
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 12-JUL-1991 (WLT)
C
C-&
 
C$ Index_Entries
C
C     test whether some logicals in an array are false
C
C-&
 
      INTEGER               I
 
C
C     Just do it.
C
 
      DO I = 1, N
 
         IF ( .NOT. LOGCLS(I) ) THEN
 
            SOMFLS = .TRUE.
            RETURN
 
         END IF
 
      END DO
 
      SOMFLS = .FALSE.
      RETURN
 
 
      END
 
