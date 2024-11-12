C$Procedure      DIFFD ( Difference of two double precision sets )
 
      SUBROUTINE DIFFD ( A, B, C )
 
C$ Abstract
C
C      Take the difference of two double precision sets to form
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
C     None.
C
C$ Keywords
C
C      CELLS, SETS
C
C$ Declarations
 
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )
 
      DOUBLE PRECISION   A      ( LBCELL:* )
      DOUBLE PRECISION   B      ( LBCELL:* )
      DOUBLE PRECISION   C      ( LBCELL:* )
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      A          I   First input set.
C      B          I   Second input set.
C      C          O   Difference of A and B.
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
C                  contains the difference of A and B (that is,
C                  all of the elements which are in A, but NOT
C                  in B).
C
C                  If the size (maximum cardinality) of C is smaller
C                  than the cardinality of the difference of A and B,
C                  then only as many items as will fit in C are
C                  included, and an error is returned.
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
C      The DIFFERENCE of two sets contains every element which is
C      in the first set, but NOT in the second.
C
C            {a,b}      difference  {c,d}     =  {a,b}
C            {a,b,c}                {b,c,d}      {a}
C            {a,b,c,d}              {}           {a,b,c,d}
C            {}                     {a,b,c,d}    {}
C            {}                     {}           {}
C
C      The following call
C
C            CALL DIFFC  ( PLANETS, ASTEROIDS, RESULT )
C
C      places the difference of the character sets PLANETS and
C      ASTEROIDS into the character set RESULT.
C
C      The output set must be distinct from both of the input sets.
C      For example, the following calls are invalid.
C
C            CALL DIFFI  ( CURRENT,     NEW, CURRENT )
C            CALL DIFFI  (     NEW, CURRENT, CURRENT )
C
C      In each of the examples above, whether or not the subroutine
C      signals an error, the results will almost certainly be wrong.
C      Nearly the same effect can be achieved, however, by placing the
C      result into a temporary set, which is immediately copied back
C      into one of the input sets, as shown below.
C
C            CALL DIFFI  ( CURRENT, NEW,  TEMP )
C            CALL COPYI  ( TEMP,    NEW       )
C
C$ Restrictions
C
C      None.
C
C$ Exceptions
C
C     1) If the difference of the two sets causes an excess of
C        elements, the error SPICE(SETEXCESS) is signalled.
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
C     difference of two d.p. sets
C
C-&
 
 
C$ Revisions
C
C-    Beta Version 1.1.0, 06-JAN-1989 (NJB)
C
C        Calling protocol of EXCESS changed.  Call to SETMSG removed.
C
C        Also, in the overflow case, the number of excess elements was
C        computed incorrectly; this has been fixed.  The problem was
C        that OVER was incremented in all cases of the overflow IF
C        block, rather than only in the cases where the cardinality of
C        the output cell would have been incremented if there were
C        enough room.
C-&
 
 
C
C     SPICELIB functions
C
      INTEGER             CARDD
      INTEGER             SIZED
      LOGICAL             RETURN
 
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
      CALL CHKIN ( 'DIFFD' )
 
 
C
C     Find the cardinality of the input sets, and the allowed size
C     of the output set.
C
      ACARD = CARDD ( A )
      BCARD = CARDD ( B )
      CSIZE = SIZED ( C )
 
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
C     When the end of the first input set is reached, we're done.
C
      DO WHILE ( APOINT .LE. ACARD )
 
C
C        If there is still space in the output set, fill it
C        as necessary.
C
         IF ( CCARD .LT. CSIZE ) THEN
 
            IF ( BPOINT .GT. BCARD) THEN
 
               CCARD    = CCARD + 1
               C(CCARD) = A(APOINT)
               APOINT   = APOINT + 1
 
            ELSE IF ( A(APOINT) .EQ. B(BPOINT)) THEN
 
               APOINT = APOINT + 1
               BPOINT = BPOINT + 1
 
            ELSE IF ( A(APOINT) .LT. B(BPOINT) ) THEN
 
               CCARD    = CCARD + 1
               C(CCARD) = A(APOINT)
               APOINT   = APOINT + 1
 
            ELSE IF ( B(BPOINT) .LT. A(APOINT) ) THEN
 
               BPOINT = BPOINT + 1
 
            END IF
 
 
C
C        Otherwise, stop filling the array, but continue to count the
C        number of elements in excess of the size of the output set.
C
         ELSE
 
            IF ( BPOINT .GT. BCARD ) THEN
 
               OVER   = OVER   + 1
               APOINT = APOINT + 1
 
            ELSE IF ( A(APOINT) .EQ. B(BPOINT) ) THEN
 
               APOINT = APOINT + 1
               BPOINT = BPOINT + 1
 
            ELSE IF ( A(APOINT) .LT. B(BPOINT) ) THEN
 
               OVER   = OVER   + 1
               APOINT   = APOINT + 1
 
            ELSE IF ( B(BPOINT) .LT. A(APOINT) ) THEN
 
               BPOINT = BPOINT + 1
 
            END IF
 
         END IF
 
      END DO
 
C
C     Set the cardinality of the output set.
C
      CALL SCARDD ( CCARD, C )
 
C
C     Report any excess.
C
      IF ( OVER .GT. 0 ) THEN
         CALL EXCESS ( OVER, 'set' )
         CALL SIGERR ( 'SPICE(SETEXCESS)' )
      END IF
 
 
      CALL CHKOUT ( 'DIFFD' )
 
      RETURN
      END
 
