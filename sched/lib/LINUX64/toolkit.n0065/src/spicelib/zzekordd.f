C$Procedure      ZZEKORDD ( Order of a double precision EK column )
 
      SUBROUTINE ZZEKORDD ( DVALS, NULLOK, NLFLGS, NVALS, IORDER )
 
C$ Abstract
C
C     Determine the order of elements in a double precision EK column,
C     using dictionary ordering on d.p. data values and array
C     indices.
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
C     EK
C
C$ Keywords
C
C     EK
C     SORT
C
C$ Declarations
 
      DOUBLE PRECISION      DVALS  ( * )
      LOGICAL               NULLOK
      LOGICAL               NLFLGS ( * )
      INTEGER               NVALS
      INTEGER               IORDER ( * )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     DVALS      I   Array of d.p. column values.
C     NULLOK     I   Logical flag indicating whether nulls are allowed.
C     NLFLGS     I   Flags indicating whether column entries are null.
C     NVALS      I   Dimension of DVALS.
C     IORDER     O   Order vector for DVALS.
C
C$ Detailed_Input
C
C     DVALS          is an array of double precision EK column values,
C                    some of which may be null, if null values are
C                    permitted.  See the description of the input
C                    arguments NULLOK and NLFLGS below.
C
C     NULLOK         is a logical flag indicating whether column
C                    elements may be null.  If NULLOK is TRUE, then
C                    NLFLGS must be set to indicate the status of each
C                    element of DVALS.
C
C     NLFLGS         is an array of logical flags that indicate whether
C                    the corresponding elements of DVALS are null.
C                    NLFLGS is meaningful only when NULLOK is .TRUE.
C                    When NULLOK is .TRUE., the Ith element of DVALS is
C                    null if and only if the Ith element of NLFLGS
C                    is .TRUE.
C
C                    When NULLOK is .FALSE., all elements of DVALS are
C                    considered to be non-null.
C
C     NVALS          is the number of elements in the input array.
C
C$ Detailed_Output
C
C     IORDER         is the order vector for the input array.
C                    IORDER(1) is the index of the smallest element
C                    of DVALS; IORDER(2) is the index of the next
C                    smallest; and so on.  Null values, if allowed, are
C                    considered to be less than all non-null values.
C                    The order relation between equal values is
C                    determined by the indices of the values in the
C                    input array; values with lower indices are
C                    considered to be smaller.
C
C$ Parameters
C
C     None.
C
C$ Particulars
C
C     ZZEKORDD creates an order vector for an array of double precision
C     column values.  Null values are allowed.  The order relation used
C     is dictionary ordering on ordered pairs consisting of data
C     values and array indices:  if two input data values are equal,
C     the associated array indices determine the order relation of the
C     values, where the smaller index is considered to precede the
C     greater.
C
C$ Examples
C
C      1)  Sort the following list of values, some of which are
C          null:
C
C                  Value                         Null?
C             --------------             ---------------------
C             DVALS(1)  =  3             NLFLGS(1)  =  .FALSE.
C             DVALS(2)  =  1             NLFLGS(2)  =  .FALSE.
C             DVALS(3)  =  4             NLFLGS(3)  =  .TRUE.
C             DVALS(4)  =  5             NLFLGS(4)  =  .FALSE.
C             DVALS(5)  =  2             NLFLGS(5)  =  .TRUE.
C
C
C          The subroutine call
C
C              CALL ZZEKORDD ( DVALS, .TRUE., NLFLGS, 5, IORDER )
C
C          generates the output
C
C             IORDER(1)  =  3
C             IORDER(2)  =  5
C             IORDER(3)  =  2
C             IORDER(4)  =  1
C             IORDER(5)  =  4
C
C
C      2)  Given the same inputs values of DVALS and NLFLGS, the
C          subroutine call
C
C             CALL ZZEKORDD ( DVALS, .FALSE., NLFLGS, 5, IORDER )
C
C          generates the output
C
C             IORDER(1)  =  2
C             IORDER(2)  =  5
C             IORDER(3)  =  1
C             IORDER(4)  =  3
C             IORDER(5)  =  4
C
C$ Restrictions
C
C      None.
C
C$ Exceptions
C
C      Error free.
C
C$ Files
C
C      None.
C
C$ Author_and_Institution
C
C      N.J. Bachman    (JPL)
C      I.M. Underwood  (JPL)
C
C$ Literature_References
C
C      None.
C
C$ Version
C
C-     Beta Version 3.0.0, 08-SEP-1995 (NJB)
C
C         Re-written to use dictionary ordering on values and input
C         array indices.
C
C-     Beta Version 2.0.0, 13-FEB-1995 (NJB)
C
C         Renamed as a private routine.
C
C-     Beta Version 1.0.0, 13-APR-1994 (NJB) (IMU)
C
C-&
 
 
C
C     Local variables
C
      DOUBLE PRECISION      D1
      DOUBLE PRECISION      D2
 
      INTEGER               GAP
      INTEGER               I
      INTEGER               I1
      INTEGER               I2
      INTEGER               J
      INTEGER               JG
 
      LOGICAL               N1
      LOGICAL               N2
      LOGICAL               OK
 
 
C
C     Statement functions
C
      LOGICAL               CMP1
      LOGICAL               CMP2
      LOGICAL               CMP3
      LOGICAL               CMP4
      LOGICAL               LE
 
      CMP1 ( D1, D2, I1, I2 )          =          ( D1 .LT. D2 )
     .                                    .OR. (  ( D1 .EQ. D2 ) .AND.
     .                                            ( I1 .LT. I2 )       )
 
      CMP2 ( I1, I2, N1, N2 )          =       ( N1 .AND. .NOT. N2  )
     .                                    .OR. (      ( N1 .AND. N2 )
     .                                          .AND. ( I1 .LT. I2  ) )
 
 
      CMP3 ( D1, D2, I1, I2, N1, N2 )  =        ( .NOT. ( N1 .OR. N2 ) )
     .                                   .AND. CMP1 (  D1, D2, I1, I2  )
 
 
      CMP4 ( D1, D2, I1, I2, N1, N2 )  = CMP2 ( I1, I2, N1, N2 )
     .                                   .OR.
     .                                   CMP3 ( D1, D2, I1, I2, N1, N2 )
 
 
      LE (  D1,  D2,  I1,  I2,  OK,  N1,  N2  )  =
     .
     .        (  (.NOT. OK ) .AND.  CMP1 ( D1, D2, I1, I2 )          )
     .  .OR.  (  (      OK ) .AND.  CMP4 ( D1, D2, I1, I2, N1, N2 )  )
 
 
C
C     Begin with the initial ordering.
C
      DO I = 1, NVALS
         IORDER(I) = I
      END DO
 
C
C     Find the smallest element, then the next smallest, and so on.
C     This uses the Shell Sort algorithm, but swaps the elements of
C     the order vector instead of the array itself.
C
      GAP = NVALS / 2
 
      DO WHILE ( GAP .GT. 0 )
 
         DO I = GAP+1, NVALS
 
 
            J = I - GAP
 
            DO WHILE ( J .GT. 0 )
 
               JG = J + GAP
 
               IF (   LE (   DVALS ( IORDER(J)  ),
     .                       DVALS ( IORDER(JG) ),
     .                       IORDER(J),
     .                       IORDER(JG),
     .                       NULLOK,
     .                       NLFLGS( IORDER(J)  ),
     .                       NLFLGS( IORDER(JG) )   )    ) THEN
C
C                 Getting here means that
C
C                    DVALS(IORDER(J)) .LE. DVALS(IORDER(JG))
C
C                 according to our order relation.
C
                  J = 0
 
               ELSE
 
                  CALL SWAPI ( IORDER(J), IORDER(JG) )
 
               END IF
 
               J = J - GAP
 
            END DO
 
 
         END DO
 
         GAP = GAP / 2
 
      END DO
 
      RETURN
      END
