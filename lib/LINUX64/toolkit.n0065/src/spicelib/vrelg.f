C$Procedure  VRELG ( Vector relative difference, general dimension )
 
      DOUBLE PRECISION FUNCTION VRELG ( V1, V2, NDIM )
 
C$ Abstract
C
C   Return the relative difference between two vectors of general
C   dimension.
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
C   None.
C
C$ Keywords
C
C     MATH
C     VECTOR
C
C$ Declarations
 
      DOUBLE PRECISION      V1 ( * )
      DOUBLE PRECISION      V2 ( * )
      INTEGER               NDIM
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C      V1,V2     I   Input vectors.
C      NDIM      I   Dimension of V1 and V2.
C
C$ Detailed_Input
C
C      V1, V2        are two vectors for which the relative difference
C                    is to be computed.
C
C      NDIM          is the dimension of V1 and V2.
C
C$ Detailed_Output
C
C      VRELG         is the relative difference between V1 and V2.
C                    It is defined as:
C                                             || V1 - V2 ||
C                              VRELG  =   ----------------------
C                                         MAX ( ||V1||, ||V2|| )
C
C                    where || X || indicates the Euclidean norm of
C                    the vector X.
C
C                    VRELG assumes values in the range [0,2]. If both
C                    V1 and V2 are zero vectors then VRELG is defined
C                    to be zero.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C     If both V1 and V2 are zero vectors then VRELG is defined to be
C     zero.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This function computes the relative difference between two vectors
C     of general dimension as defined above.
C
C     The function VREL may be used to find the relative difference
C     for two 3-dimensional vectors.
C
C$ Examples
C
C     This example determines if the state of Jupiter, with respect
C     to Voyager 2, for a set of times is the same for two different
C     ephemeris files. Instead of insisting on absolute equality
C     between the state vectors, the program will check if the relative
C     difference between the vectors is greater than a fixed tolerance.
C
C     C
C     C     The NAIF code for Jupiter is 599 and for Voyager 2 is -32.
C     C     Set the tolerance to be 0.0005.
C     C
C           INTEGER               JUP
C           PARAMETER           ( JUP = 599 )
C
C           INTEGER               VG2
C           PARAMETER           ( VG2 = -32 )
C
C           INTEGER               NUM
C           PARAMETER           ( NUM = 500 )
C
C           DOUBLE PRECISION      TOL
C           PARAMETER           ( TOL = 5.D-04 )
C
C     C
C     C     Spicelib function
C     C
C           DOUBLE PRECISION      VRELG
C     C
C     C     Local variables
C     C
C           DOUBLE PRECISION      STATE1 ( 6, NUM )
C           DOUBLE PRECISION      STATE2 ( 6, NUM )
C           DOUBLE PRECISION      ET     (    NUM )
C           DOUBLE PRECISION      LT
C           DOUBLE PRECISION      DIFF
C
C           INTEGER               HANDLE
C           INTEGER               I
C
C           .
C           .
C           .
C
C           C
C           C     Load  the first SPK file.
C           C
C                 CALL SPKLEF ( 'VG2_SOURCE_1.BSP', HANDLE )
C           C
C           C     Find the states for each time in the array ET.
C           C     This example assumes that the SPK file can
C           C     provide states for all of the times in the array.
C           C
C                 DO I = 1, NUM
C
C                    CALL SPKEZ ( JUP, ET(I),      'J2000', 'LT',
C                .                VG2, STATE1(1,I), LT           )
C
C                 END DO
C           C
C           C     Unload the first file and load the second one.
C           C
C                 CALL SPKUEF ( HANDLE )
C
C                 CALL SPKLEF ( 'VG2_SOURCE_2.BSP', HANDLE )
C           C
C           C     Find the states from the new file.
C           C
C                 DO I = 1, NUM
C
C                    CALL SPKEZ ( JUP, ET(I),      'J2000', 'LT',
C                .                VG2, STATE2(1,I), LT           )
C
C                 END DO
C           C
C           C     Now compare the two state vectors for each time.
C           C
C                 DO I = 1, NUM
C
C                    DIFF = VRELG ( STATE1(1,I), STATE2(1,I), 6 )
C
C                    IF ( DIFF .GT. TOL ) THEN
C
C                       .
C                       .
C                       .
C
C                    END IF
C
C                 END DO
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
C     J.M. Lynch     (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 15-JUN-1992 (JML)
C
C-&
 
C$ Index_Entries
C
C     relative difference of n-dimensional vectors
C
C-&
 
 
 
 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      VNORMG
      DOUBLE PRECISION      VDISTG
 
 
C
C     Local variables
C
      DOUBLE PRECISION      NUNORM
      DOUBLE PRECISION      DENORM
 
 
 
 
 
C
C     If the numerator is zero then set VRELG equal to zero. Otherwise,
C     perform the rest of the calculation.
C
C     This handles the case where both vectors are zero vectors since
C     the distance between them will be zero.
C
      NUNORM = VDISTG ( V1, V2, NDIM )
 
      IF ( NUNORM .EQ. 0.D0 ) THEN
 
         VRELG = 0.D0
 
      ELSE
 
         DENORM = MAX (  VNORMG(V1,NDIM), VNORMG(V2,NDIM)  )
 
         VRELG = NUNORM / DENORM
 
       END IF
 
 
      RETURN
      END
