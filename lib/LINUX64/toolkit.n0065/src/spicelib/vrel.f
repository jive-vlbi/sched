C$Procedure  VREL ( Vector relative difference, 3 dimensions )
 
      DOUBLE PRECISION FUNCTION VREL ( V1, V2  )
 
C$ Abstract
C
C   Return the relative difference between two 3-dimensional vectors.
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
 
      DOUBLE PRECISION      V1 ( 3 )
      DOUBLE PRECISION      V2 ( 3 )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C      V1,V2     I   Input vectors.
C
C$ Detailed_Input
C
C      V1, V2        are two 3-dimensional vectors for which the
C                    relative difference is to be computed.
C
C$ Detailed_Output
C
C      VREL          is the relative difference between V1 and V2.
C                    It is defined as:
C                                             || V1 - V2 ||
C                              VREL   =   ----------------------
C                                         MAX ( ||V1||, ||V2|| )
C
C                    where || X || indicates the Euclidean norm of
C                    the vector X.
C
C                    VREL assumes values in the range [0,2]. If both
C                    V1 and V2 are zero vectors then VREL is defined
C                    to be zero.
C
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C     If both V1 and V2 are zero vectors then VREL is defined
C     to be zero.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This function computes the relative difference between two
C     3-dimensional vectors as defined above.
C
C     The function VRELG may be used to find the relative difference
C     for two vectors of general dimension.
C
C$ Examples
C
C     This example code fragment computes the relative difference
C     between the geometric and light time corrected state of Io
C     with respect to Voyager 2 at a given UTC time.
C
C     C
C     C     The NAIF integer code for Io is 501 and the code for
C     C     Voyager 2 is -32.
C     C
C
C           INTEGER               IO
C           PARAMETER           ( IO  = 501 )
C
C           INTEGER               VG2
C           PARAMETER           ( VG2 = -32 )
C
C     C
C     C     Spicelib function
C     C
C           DOUBLE PRECISION      VREL
C     C
C     C     Local variables
C     C
C           DOUBLE PRECISION      STATE ( 6 )
C           DOUBLE PRECISION      POS1  ( 3 )
C           DOUBLE PRECISION      POS2  ( 3 )
C           DOUBLE PRECISION      DIFF
C           DOUBLE PRECISION      LT
C           DOUBLE PRECISION      ET
C
C           INTEGER               HANDLE
C
C           CHARACTER*(20)        UTC
C
C           DATA                  UTC / '1979 JUN 25 12:00:00' /
C
C     C
C     C     Load the sample SPK ephemeris file.
C     C
C           CALL SPKLEF ( 'VG2_JUP.BSP', HANDLE )
C     C
C     C     Convert the UTC time string to ephemeris time.
C     C
C           CALL UTC2ET ( UTC, ET )
C     C
C     C     First calculate the geometric state and then the light
C     C     time corrected state.
C     C
C           CALL SPKEZ ( IO, ET, 'J2000', 'NONE', VG2, STATE, LT )
C
C           CALL VEQU  ( STATE, POS1 )
C
C           CALL SPKEZ ( IO, ET, 'J2000', 'LT', VG2, STATE, LT )
C
C           CALL VEQU  ( STATE, POS2 )
C     C
C     C     Call VREL to find the relative difference between the
C     C     two states.
C     C
C           DIFF = VREL ( POS1, POS2 )
C
C           .
C           .
C           .
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
C     relative difference of 3-dimensional vectors
C
C-&
 
 
 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      VNORM
      DOUBLE PRECISION      VDIST
 
 
C
C     Local variables
C
      DOUBLE PRECISION      NUNORM
      DOUBLE PRECISION      DENORM
 
 
 
 
 
 
C
C     If the numerator is zero then set VREL equal to zero. Otherwise,
C     perform the rest of the calculation.
C
C     This handles the case where both vectors are zero vectors since
C     the distance between them will be zero.
C
      NUNORM = VDIST ( V1, V2 )
 
      IF ( NUNORM .EQ. 0.D0 ) THEN
 
         VREL = 0.D0
 
      ELSE
 
         DENORM = MAX (  VNORM(V1), VNORM(V2)  )
 
         VREL = NUNORM / DENORM
 
       END IF
 
 
      RETURN
      END
