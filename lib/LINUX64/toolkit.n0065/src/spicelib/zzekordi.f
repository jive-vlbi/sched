C$Procedure      ZZEKORDI ( Order of an integer EK column )
 
      SUBROUTINE ZZEKORDI ( IVALS, NULLOK, NLFLGS, NVALS, IORDER )
 
C$ Abstract
C
C     Determine the order of elements in an integer EK column, using
C     dictionary ordering on integer data values and array indices.
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
 
      INTEGER               IVALS  ( * )
      LOGICAL               NULLOK
      LOGICAL               NLFLGS ( * )
      INTEGER               NVALS
      INTEGER               IORDER ( * )
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      IVALS      I   Array of integer column values.
C      NULLOK     I   Logical flag indicating whether nulls are allowed.
C      NLFLGS     I   Flags indicating whether column entries are null.
C      NVALS      I   Dimension of IVALS.
C      IORDER     O   Order vector for IVALS.
C
C$ Detailed_Input
C
C      IVALS          is an array of integer EK column values,
C                     some of which may be null, if null values are
C                     permitted.  See the description of the input
C                     arguments NULLOK and NLFLGS below.
C
C      NULLOK         is a logical flag indicating whether column
C                     elements may be null.  If NULLOK is TRUE, then
C                     NLFLGS must be set to indicate the status of each
C                     element of IVALS.
C
C      NLFLGS         is an array of logical flags that indicate whether
C                     the corresponding elements of IVALS are null.
C                     NLFLGS is meaningful only when NULLOK is .TRUE.
C                     When NULLOK is .TRUE., the Ith element of IVALS is
C                     null if and only if the Ith element of NLFLGS
C                     is .TRUE.
C
C                     When NULLOK is .FALSE., all elements of IVALS are
C                     considered to be non-null.
C
C      NVALS          is the number of elements in the input array.
C
C$ Detailed_Output
C
C      IORDER      is the order vector for the input array.
C                  IORDER(1) is the index of the smallest element
C                  of IVALS; IORDER(2) is the index of the next
C                  smallest; and so on.  Null values, if allowed, are
C                  considered to be less than all non-null values.  The
C                  order relation between equal values is determined
C                  by the indices of the values in the input array;
C                  values with lower indices are considered to be
C                  smaller.
C
C$ Parameters
C
C     None.
C
C$ Particulars
C
C     ZZEKORDI creates an order vector for an array of integer
C     column values.  Null values are allowed.  The order
C     relation used is dictionary ordering on ordered pairs consisting
C     of data values and array indices:  if two input data values
C     are equal, the associated array indices determine the order
C     relation of the values, where the smaller index is considered
C     to precede the greater.
C
C$ Examples
C
C      1)  Sort the following list of values, some of which are
C          null:
C
C                  Value                         Null?
C             --------------             ---------------------
C             IVALS(1)  =  3             NLFLGS(1)  =  .FALSE.
C             IVALS(2)  =  1             NLFLGS(2)  =  .FALSE.
C             IVALS(3)  =  4             NLFLGS(3)  =  .TRUE.
C             IVALS(4)  =  5             NLFLGS(4)  =  .FALSE.
C             IVALS(5)  =  2             NLFLGS(5)  =  .TRUE.
C
C
C          The subroutine call
C
C              CALL ZZEKORDI ( IVALS, .TRUE., NLFLGS, 5, IORDER )
C
C          generates the output
C
C             IORDER(1)  =  3
C             IORDER(2)  =  5
C             IORDER(3)  =  2
C             IORDER(4)  =  1
C             IORDER(5)  =  4
C
C          Note that the order of the null values is determined by
C          their indices in the input array.
C
C
C      2)  Given the same inputs values of IVALS and NLFLGS, the
C          subroutine call
C
C             CALL ZZEKORDI ( IVALS, .FALSE., NLFLGS, 5, IORDER )
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
C-     Beta Version 3.0.0, 26-MAY-1995 (NJB)
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
      INTEGER               I1
      INTEGER               I2
      INTEGER               I3
      INTEGER               I4
 
      INTEGER               GAP
      INTEGER               I
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
 
 
      CMP1 ( I1, I2, I3, I4 )          =          ( I1 .LT. I2 )
     .                                    .OR. (  ( I1 .EQ. I2 ) .AND.
     .                                            ( I3 .LT. I4 )       )
 
 
      CMP2 ( I3, I4, N1, N2 )          =       ( N1 .AND. .NOT. N2  )
     .                                    .OR. (      ( N1 .AND. N2 )
     .                                          .AND. ( I3 .LT. I4  ) )
 
 
      CMP3 ( I1, I2, I3, I4, N1, N2 )  =        ( .NOT. ( N1 .OR. N2 ) )
     .                                   .AND. CMP1 (  I1, I2, I3, I4  )
 
 
      CMP4 ( I1, I2, I3, I4, N1, N2 )  = CMP2 ( I3, I4, N1, N2 )
     .                                   .OR.
     .                                   CMP3 ( I1, I2, I3, I4, N1, N2 )
 
 
      LE (  I1,  I2, I3, I4, OK,   N1,   N2  )  =
     .
     .        (  (.NOT. OK ) .AND.  CMP1 ( I1, I2, I3, I4 )          )
     .  .OR.  (  (      OK ) .AND.  CMP4 ( I1, I2, I3, I4, N1, N2 )  )
 
 
 
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
 
               IF (   LE (   IVALS ( IORDER(J)  ),
     .                       IVALS ( IORDER(JG) ),
     .                       IORDER(J),
     .                       IORDER(JG),
     .                       NULLOK,
     .                       NLFLGS( IORDER(J)  ),
     .                       NLFLGS( IORDER(JG) )   )    ) THEN
C
C                 Getting here means that
C
C                    IVALS(IORDER(J)) .LE. IVALS(IORDER(JG))
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
