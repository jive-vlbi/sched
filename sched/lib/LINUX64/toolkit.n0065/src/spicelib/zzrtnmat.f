C$Procedure ZZRTNMAT ( RTN transformation matrix )

      SUBROUTINE ZZRTNMAT ( V, M )
 
C$ Abstract
C
C     Given a vector, return a transformation matrix that maps from the
C     vector's base reference frame to the RTN
C     (radial-tangential-normal) frame associated with the vector.
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
C     ROTATION
C
C$ Keywords
C
C     FRAMES
C     MATRIX
C     ROTATION
C
C$ Declarations
 
      IMPLICIT NONE

      DOUBLE PRECISION      V ( 3 )
      DOUBLE PRECISION      M ( 3, 3 )

 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     V          I   A 3-dimensional vector.
C     M          O   Base frame to RTN frame rotation matrix.
C
C$ Detailed_Input
C
C     V              is any vector that does not lie on the Z-axis
C                    of the reference frame relative to which the
C                    vector is expressed: at least one of V's X or
C                    Y components must be non-zero.
C
C$ Detailed_Output
C
C     M              is a rotation matrix that transforms vectors
C                    from the base frame of V---that is, the reference
C                    frame relative to which V is expressed---to
C                    the RTN (radial, tangential, normal) frame
C                    defined by V.
C
C                    The basis vectors of the RTN frame are defined
C                    as follows:
C
C                       Axis 1: radial direction R. This axis is
C                               parallel to V.
C
C                       Axis 2: tangential direction T. This axis
C                               is parallel to Z x V, where Z is 
C                               the third axis of V's base frame.
C                               
C                       Axis 3: normal direction N. This axis is 
C                               parallel to R x T.
C
C                   The unit vectors R, T, N are, respectively, the 
C                   first, second and third rows of M.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If the input vector V has X and Y components equal to zero,
C        the error SPICE(DEGENERATECASE) is signaled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     The RTN frame supported this routine is a generalization of
C     the frame defined by a solar system object's spin axis and
C     and a position vector (often the position of a spacecraft
C     relative to the center of the object).
C
C     If the base frame of the input vector V is the body-fixed,
C     body-centered planetocentric frame associated with a solar system
C     object such as a planet or satellite, then the R, T, N directions
C     correspond to the "up", "East," and "North" directions at the
C     location indicated by V.
C
C$ Examples
C
C     1) Get the RTN transformation matrix for the vector ( 1, 0, 1 ):
C
C
C           IMPLICIT NONE
C
C           DOUBLE PRECISION      V ( 3 )
C           DOUBLE PRECISION      M ( 3, 3 )
C           INTEGER               I
C           INTEGER               J
C
C           CALL VPACK    ( 1.D0, 0.D0, 1.D0, V )
C
C           CALL ZZRTNMAT ( V, M )           
C
C           DO I = 1, 3
C              WRITE(*,'(3E15.7)') ( M(I,J), J = 1, 3 )
C           END DO 
C
C           END           
C
C        When this program was executed on a PC/Linux/g77 system, the 
C        output was
C
C            0.7071068E+00  0.0000000E+00  0.7071068E+00
C            0.0000000E+00  0.1000000E+01  0.0000000E+00
C           -0.7071068E+00  0.0000000E+00  0.7071068E+00
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
C-    SPICELIB Version 1.0.0, 05-MAR-2009 (NJB)
C
C-&
 
C$ Index_Entries
C
C     transformation to radial, tangential, normal frame
C     transformation to rtn frame
C
C-&
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN

C
C     Local variables
C
C     Internally, we're going to use the more
C     descriptive names EAST for the "tangential"
C     direction and NORTH for the "normal" direction.
C
      DOUBLE PRECISION      EAST   ( 3 )
      DOUBLE PRECISION      LON
      DOUBLE PRECISION      NORTH  ( 3 )
      DOUBLE PRECISION      RAD    ( 3 )
      DOUBLE PRECISION      VLON   ( 3 )
      DOUBLE PRECISION      Z ( 3 )

      INTEGER               I

C
C     Saved variables
C
      SAVE                  Z

C
C     Initial values
C
      DATA                  Z / 0.D0, 0.D0, 1.D0 /

C
C     Use discovery check-in. Just test the RETURN status.
C     
      IF ( RETURN() ) THEN
         RETURN
      END IF


      IF (  ( V(1) .EQ. 0.D0 ) .AND. ( V(2) .EQ. 0.D0 )  ) THEN 

         CALL CLEARD ( 9, M )

         CALL CHKIN  ( 'ZZRTNMAT'                              )
         CALL SETMSG ( 'Input vector (# # #) lies on Z-axis; '
     .   //            'tangential and normal directions are '
     .   //            'undefined.'                            )
         CALL ERRDP  ( '#', V(1)                               )
         CALL ERRDP  ( '#', V(2)                               )
         CALL ERRDP  ( '#', V(3)                               )
         CALL SIGERR ( 'SPICE(DEGENERATECASE)'                 )
         CALL CHKOUT ( 'ZZRTNMAT'                              )
         RETURN

      ELSE
C
C        The two-argument arctangent function gives us a
C        robust way of determining the longitude of V, even
C        when the magnitude of V is very small.
C
         LON = DATAN2 ( V(2), V(1) )

C
C        Let VLON be a unit vector in the x-y plane whose
C        longitude is LON.
C
         VLON(1) = COS( LON )
         VLON(2) = SIN( LON )
         VLON(3) = 0.D0

C
C        We can compute the East and North vectors
C        without much loss of precision, since VLON is
C        orthogonal to Z and EAST is orthogonal to V.
C        
         CALL UCRSS ( Z, VLON, EAST  )
         CALL UCRSS ( V, EAST, NORTH )
         
         CALL VHAT  ( V, RAD )

C
C        The rows of M are the basis vectors of
C        the radial/East/North frame:
C         
         DO I = 1, 3

            M(1,I) = RAD  (I)
            M(2,I) = EAST (I)
            M(3,I) = NORTH(I)

         END DO

      END IF

      END 

