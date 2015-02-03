C$Procedure      SDIFFC ( Symmetric difference of two character sets )
 
      SUBROUTINE SDIFFC ( A, B, C )
 
C$ Abstract
C
C      Take the symmetric difference of two character sets to form
C      a third set.
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
C      SETS
C
C$ Keywords
C
C      CELLS, SETS
C
C$ Declarations
 
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )
 
      CHARACTER*(*)      A      ( LBCELL:* )
      CHARACTER*(*)      B      ( LBCELL:* )
      CHARACTER*(*)      C      ( LBCELL:* )
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      A          I   First input set.
C      B          I   Second input set.
C      C          O   Symmetric difference of A and B.
C
C$ Detailed_Input
C
C
C      A           is a set.
C
C
C      B           is a set, distinct from A.
C
C$ Detailed_Output
C
C      C           is a set, distinct from sets A and B, which
C                  contains the symmetric difference of A and B
C                  (that is, all of the elements which are in A
C                  OR in B, but NOT in both).
C
C                  If the size (maximum cardinality) of C is smaller
C                  than the cardinality of the symmetric difference of
C                  A and B, then only as many items as will fit in C
C                  are included, and an error is signalled.
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
C      The SYMMETRIC DIFFERENCE of two sets contains every
C      element which is in the first set OR in the second set,
C      but NOT in both sets.
C
C            {a,b}      sym. difference {c,d}     =  {a,b,c,d}
C            {a,b,c}                    {b,c,d}      {a,d}
C            {a,b,c,d}                  {}           {a,b,c,d}
C            {}                         {a,b,c,d}    {a,b,c,d}
C            {}                         {}           {}
C
C      The following call
C
C            CALL SDIFFC  ( PLANETS, ASTEROIDS, RESULT )
C
C      places the symmetric difference of the character sets PLANETS and
C      ASTEROIDS into the character set RESULT.
C
C      The output set must be distinct from both of the input sets.
C      For example, the following calls are invalid.
C
C            CALL SDIFFI ( CURRENT,     NEW, CURRENT )
C            CALL SDIFFI (     NEW, CURRENT, CURRENT )
C
C      In each of the examples above, whether or not the subroutine
C      signals an error, the results will almost certainly be wrong.
C      Nearly the same effect can be achieved, however, by placing the
C      result into a temporary set, which is immediately copied back
C      into one of the input sets, as shown below.
C
C            CALL SDIFFI ( CURRENT, NEW,  TEMP )
C            CALL COPYI  ( TEMP,    NEW        )
C
C$ Restrictions
C
C      None.
C
C$ Exceptions
C
C      1) If the symmetric difference of the two sets causes an excess
C         of elements, the error SPICE(SETEXCESS) is signalled.
C
C      2) If length of the elements of the output set is < the
C         maximum of the lengths of the elements of the input
C         sets, the error SPICE(ELEMENTSTOOSHORT) is signalled.
C
C$ Files
C
C      None.
C
C$ Literature_References
C
C      None.
C
C$ Author_and_Institution
C
C      N.J. Bachman    (JPL)
C      C.A. Curzon     (JPL)
C      W.L. Taber      (JPL)
C      I.M. Underwood  (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 18-JUN-1999 (WLT)
C
C        Modified call to CHKOUT to be consistent with CHKIN.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (CAC) (WLT) (IMU)
C
C-&
 
C$ Index_Entries
C
C     symmetric difference of two character sets
C
C-&
 
 
C$ Revisions
C
C-    Beta Version 2.0.0, 05-JAN-1989 (NJB)
C
C        Error signalled if output set elements are not long enough.
C        Length must be at least max of lengths of input elements.
C        Also, calling protocol for EXCESS has been changed.
C-&
 
 
C
C     SPICELIB functions
C
      INTEGER              CARDC
      INTEGER              SIZEC
      LOGICAL              RETURN
C
C     Local variables
C
 
      INTEGER             APOINT
      INTEGER             BPOINT
 
      INTEGER             CSIZE
      INTEGER             ACARD
      INTEGER             BCARD
      INTEGER             CCARD
 
      INTEGER             OVER
 
 
 
 
C
C     Set up the error processing.
C
      IF ( RETURN () ) RETURN
      CALL CHKIN ( 'SDIFFC' )
 
C
C     Make sure output set elements are long enough.
C
      IF (               LEN( C(LBCELL) )
     .      .LT.  MAX (  LEN( A(LBCELL) ), LEN( B(LBCELL) )  )   ) THEN
 
         CALL SETMSG ( 'Length of output cell is #.  Length required' //
     .                 ' to contain result is #.'  )
 
         CALL ERRINT ( '#', LEN( C(LBCELL) )  )
         CALL ERRINT ( '#', MAX (  LEN(A(LBCELL)), LEN(B(LBCELL)) )  )
 
         CALL SIGERR ( 'SPICE(ELEMENTSTOOSHORT)' )
 
         CALL CHKOUT ( 'SDIFFC' )
         RETURN
 
      END IF
C
C     Find the cardinality of the input sets, and the allowed size
C     of the output set.
C
      ACARD = CARDC ( A )
      BCARD = CARDC ( B )
      CSIZE = SIZEC ( C )
 
C
C     Begin with the input pointers at the first elements of the
C     input sets. The cardinality of the output set is zero.
C     And there is no overflow so far.
C
      APOINT = 1
      BPOINT = 1
 
      CCARD  = 0
      OVER   = 0
 
 
 
C
C     When the end of both input sets are reached, we're done.
C
      DO WHILE ( APOINT .LE. ACARD .OR. BPOINT .LE. BCARD )
 
C
C        If there is still space in the output set, fill it
C        as necessary.
C
         IF ( CCARD .LT. CSIZE ) THEN
 
            IF ( APOINT .GT. ACARD ) THEN
 
               CCARD    = CCARD + 1
               C(CCARD) = B(BPOINT)
               BPOINT   = BPOINT + 1
 
            ELSE IF ( BPOINT .GT. BCARD ) THEN
 
               CCARD    = CCARD + 1
               C(CCARD) = A(APOINT)
               APOINT   = APOINT + 1
 
            ELSE IF ( A(APOINT) .EQ. B(BPOINT) ) THEN
 
               APOINT = APOINT + 1
               BPOINT = BPOINT + 1
 
            ELSE IF ( LLT ( A(APOINT), B(BPOINT) ) ) THEN
 
               CCARD    = CCARD + 1
               C(CCARD) = A(APOINT)
               APOINT   = APOINT + 1
 
            ELSE IF ( LLT ( B(BPOINT), A(APOINT) ) ) THEN
 
               CCARD    = CCARD + 1
               C(CCARD) = B(BPOINT)
               BPOINT   = BPOINT + 1
 
            END IF
 
 
 
C
C        Otherwise, stop folling the array, but continue to count the
C        number of elements in excess of the size of the output set.
C
         ELSE
 
            IF ( APOINT .GT. ACARD ) THEN
 
               OVER   = OVER   + 1
               BPOINT = BPOINT + 1
 
            ELSE IF ( BPOINT .GT. BCARD ) THEN
 
               OVER   = OVER   + 1
               APOINT = APOINT + 1
 
            ELSE IF ( A(APOINT) .EQ. B(BPOINT) ) THEN
 
               APOINT = APOINT + 1
               BPOINT = BPOINT + 1
 
            ELSE IF ( LLT ( A(APOINT), B(BPOINT) ) ) THEN
 
               OVER   = OVER   + 1
               APOINT = APOINT + 1
 
            ELSE IF ( LLT ( B(BPOINT), A(APOINT) ) ) THEN
 
               OVER   = OVER   + 1
               BPOINT = BPOINT + 1
 
            END IF
 
         END IF
 
      END DO
 
C
C     Set the cardinality of the output set.
C
      CALL SCARDC ( CCARD, C )
 
C
C     Report any excess.
C
      IF ( OVER .GT. 0 ) THEN
         CALL EXCESS ( OVER, 'set' )
         CALL SIGERR ( 'SPICE(SETEXCESS)' )
      END IF
 
      CALL CHKOUT ( 'SDIFFC' )
 
      RETURN
      END
 
