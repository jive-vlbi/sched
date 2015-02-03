C$Procedure      SETELM ( Calculate set of SPICE conic elements )

      SUBROUTINE SETELM ( CNGMVL, DVL, DSTCOF, ANGCOF, 
     .                    PARAM,  NPARAM, ELTS, ERRFLG, ERRTXT )
     
C$ Abstract
C
C     Calculate set of standard conic elements used by SPICELIB 
C     procedure CONIC from a set of different parameters allowed for
C     the input data type ELEMENTS.
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
C     MKSPK User's Guide
C
C$ Keywords
C
C     None.
C
C$ Declarations 

      IMPLICIT   NONE

      DOUBLE PRECISION      CNGMVL
      DOUBLE PRECISION      DVL    ( * )
      DOUBLE PRECISION      ANGCOF
      DOUBLE PRECISION      DSTCOF
      INTEGER               PARAM  ( * )
      INTEGER               NPARAM 
      DOUBLE PRECISION      ELTS   ( 8 )
      LOGICAL               ERRFLG 
      CHARACTER*(*)         ERRTXT ( 2 )

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     CNGMVL     I   Gravitational parameter (GM) of primary body
C     DVL        I   Array of input parameters values
C     PARAM      I   Array of parameter indexes 
C     NPARAM     I   Number elements in PARAM
C     ANGCOF     I   Angle unit coefficient
C     DSTCOF     I   Distance unit coefficient
C     ELTS       O   Set of conic elements
C     ERRFLG     O   Logical error flag
C     ERRTXT     O   Error explanation text
C
C$ Detailed_Input 
C
C                                                       3    2
C     CNGMVL      is the gravitational parameter (GM, km /sec ) of
C                 the primary body.
C
C     DVL         is a set of parameter values obtained by parsing
C                 input text data of type ELEMENTS according 
C                 to the specified DATA_ORDER. The parameters in the 
C                 arrays are the following order (see array DOVAL in 
C                 the main module):
C
C                    'EPOCH', 'X', 'Y', 'Z', 'VX' , 'VY', 'VZ',
C                    'A', 'E', 'RP', 'T', 'P', 'INC', 'PER', 'NOD',
C                    'MEAN', 'EXAN', 'TRAN', 'EPOCHP', 'TAU',
C                    'EQ_A', 'EQ_H', 'EQ_K', 'EQ_ML', 'EQ_P', 'EQ_Q',
C                    'DPER/DT', 'DMPN/DT', 'DNOD/DT' , 'SKIP'
C
C     PARAM       is an array of input parameter indexes in DVL 
C                 arranged in accordance with the specified DATA_ORDER 
C                 value. Index of each parameter is as ordinal number 
C                 of the corresponding word in array DOVAL.   
C
C     NPARAM      is the number of elements in array PARAM.
C
C     ANGCOF      is the coefficient that converts input angle units
C                 to radians.
C
C     DSTCOF      is the coefficient that converts input distance units 
C                 to kilometers.
C
C$ Detailed_Output
C
C     ELTS        are conic elements describing the orbit of an object
C                 around a primary body. The elements are, in order:
C
C                            Perifocal distance.
C                            Eccentricity.
C                            Inclination.
C                            Longitude of the ascending node.
C                            Argument of periapsis.
C                            Mean anomaly at epoch.
C                            Epoch.
C                            Gravitational parameter.
C
C                 The epoch of the elements is the epoch of the input
C                 state. Units are km, rad, rad/sec. The same elements
C                 are used to describe all three types (elliptic,
C                 hyperbolic, and parabolic) of conic orbit.
C
C     ERRFLG      is the logical flag of the error. Set to .TRUE. value 
C                 if calculation of the comic elements from a given 
C                 input parameters wasn't possible.
C     
C     ERRTXT      is the error explanation text.
C 
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     If input elements are inconsistent or incomplete, this routine 
C     doesn't signal an error but rather returns long anf short 
C     error messages through its output parameters.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     None.
C
C$ Examples
C
C     None.
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
C     N.G. Khavenson (IKI RAS, Russia)
C     B.V. Semenov   (NAIF, JPL)
C
C$ Version
C
C-    Version 1.0.4, 18-JAN-2000 (BVS).
C
C        Added CHKOUT calls before each RETURN.
C
C-    Version 1.0.3, 19-MAR-1999 (BVS).
C
C        Corrected comments.
C
C-    Version 1.0.2, 13-JAN-1999 (BVS).
C
C        Modified error messages.
C
C-    Version 1.0.1, 22-NOV-1998 (NGK).
C
C
C-    Version 1.0.0, 8-SEP-1998 (NGK).
C
C-&
 
C$ Index_Entries
C
C     Calculate set of conic elements from MKSPK input parameters.
C
C-&                       

C
C     SPICELIB functions
C
      INTEGER               ISRCHI
      DOUBLE PRECISION      TWOPI
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      DACOSH 
      LOGICAL               RETURN 

C
C     Local variables
C      

      DOUBLE PRECISION      TWOTHR
      DOUBLE PRECISION      SENSE
      DOUBLE PRECISION      WRKEL
      DOUBLE PRECISION      MEANM
          
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SETELM' )
      END IF 

C
C     Initialize error flag.
C
      ERRFLG = .FALSE.

C
C     Assign constant value.
C
      TWOTHR = 2.D0 / 3.D0

C
C     All listed below DVL are present in the input data. We already
C     checked this when we checked specified DATA_ORDER against input
C     output type in CHCKDO.
C
      ELTS (2) = DVL (9)
      ELTS (3) = DVL (13) * ANGCOF
      ELTS (4) = DVL (15) * ANGCOF
      ELTS (5) = DVL (14) * ANGCOF
      ELTS (7) = DVL (1)
      ELTS (8) = CNGMVL

C
C     First we need to compute the distance of the periapsis.      
C
      IF ( ISRCHI ( 10, NPARAM, PARAM ) .NE. 0 ) THEN

C
C        Distance of the peripsis is defined in input file.
C
         ELTS (1) = DVL (10) * DSTCOF

      ELSE IF ( ISRCHI ( 8, NPARAM, PARAM ) .NE. 0) THEN

C
C        Semi-major axis is defined in input file.
C
         IF ( DVL (9) .NE. 1.D0  ) THEN

C           Case of elliptic or hyperbolic orbits.
C
            ELTS (1) = DABS ( ( DVL (9) - 1.D0 ) * DVL (8) ) * DSTCOF

         ELSE

C
C           Case of parabolic orbit. Complain and exit.
C
            ERRTXT (1) = 'Semi-major axis can not be used as ' //
     .                   'input parameter in case of a parabolic ' //
     .                   'orbit (ecc = 1.0).'
            ERRTXT (2) = 'SPICE(INCONSISTELEMENTS)'
            
            ERRFLG = .TRUE.
            
            CALL CHKOUT ( 'SETELM' )
            RETURN

         END IF

      ELSE IF ( ISRCHI( 12, NPARAM, PARAM ) .NE. 0 ) THEN
      
C
C        Semi-latus rectum is defined in input file.
C 
         ELTS (1) = DVL (12) / ( 1.D0 + DVL (9) ) * DSTCOF

      ELSE IF ( ISRCHI( 11, NPARAM, PARAM ) .NE. 0 ) THEN

C
C        Orbital period is defined in input file.
C
         IF ( DVL (9) .LT. 1.D0 )  THEN

C
C           Case of elliptic orbit.
C
            ELTS (1) = ( 1.D0 - DVL (9) ) *
     .            ( DVL (11) * DSQRT ( CNGMVL ) / TWOPI () ) ** TWOTHR
     .             * DSTCOF

         ELSE

C
C           Case of parabolic or hyperbolic orbit. Complain and exit.
C
            ERRTXT (1) = 'Orbital period can not be used as input ' //
     .                   'parameter in case of a parabolic ' //
     .                   '(ecc = 1.0) or hyperbolic (ecc > 1.0) ' //
     .                   'orbit.'
            ERRTXT (2) = 'SPICE(INCONSISTELEMENTS)'
            
            ERRFLG = .TRUE.

            CALL CHKOUT ( 'SETELM' )
            RETURN

         END IF

      ELSE

C
C        Not enough input data to calculate the periapsis distance.
C        Complain and exit.
C
         ERRTXT (1) = 'Perifocal distance can not be computed ' //
     .                'from given set of orbital elements.' 
         ERRTXT (2) = 'SPICE(INCOMPLETEELEMENTS)'
         
         ERRFLG = .TRUE.

         CALL CHKOUT ( 'SETELM' )
         RETURN

      END IF

C
C     Second we need to compute the mean anomaly.      
C
      IF ( ISRCHI ( 16, NPARAM, PARAM ) .NE. 0 ) THEN

C
C        Mean anomaly is defined in the input file.
C
         ELTS (6) = DVL (16) * ANGCOF

      ELSE IF ( ISRCHI( 17, NPARAM, PARAM ) .NE. 0 ) THEN

C
C        Eccentric anomaly is defined in the input file.
C
         WRKEL = DVL (17) * ANGCOF

         IF ( DVL (9) .LT. 1.D0  ) THEN

C
C           Case of elliptic orbit.
C
            ELTS (6) =  WRKEL - DVL (9) * DSIN ( WRKEL )

         ELSE IF ( DVL (9) .GT. 1.D0  ) THEN 
         
C
C           Case of hyperbolic orbit.
C
            ELTS (6) = DVL (9) * DSINH ( WRKEL ) - WRKEL

         ELSE

C
C           Case of parabolic  orbit. Complain and exit.
C
            ERRTXT (1) = 'Eccentric anomaly can not be used as ' //
     .                   'input parameter in case of a parabolic ' //
     .                   'orbit (ecc = 1.0).'
            ERRTXT (2) = 'SPICE(INCONSISTELEMENTS)'
            ERRFLG = .TRUE.

            CALL CHKOUT ( 'SETELM' )
            RETURN

         END IF

      ELSE IF ( ISRCHI( 18, NPARAM, PARAM ) .NE. 0 ) THEN
      
C
C        True anomaly is defined in input file.
C
         WRKEL = DVL (18) * ANGCOF

         IF ( WRKEL .LT. PI () ) THEN

            SENSE = 1.D0

         ELSE

            SENSE = -1.D0

         END IF

         IF ( DVL (9) .LT. 1.D0  ) THEN

C
C           Case of elliptic orbit.
C
            WRKEL = DACOS (
     .           (   DVL (9) + DCOS ( WRKEL )      ) /
     .           ( 1.D0 + DVL (9) * DCOS ( WRKEL ) )   )
            ELTS (6) = SENSE * ( WRKEL - DVL (9) * DSIN ( WRKEL ) )

            IF ( ELTS (6) .LT. 0.D0 ) THEN

               ELTS (6) = ELTS (6) + TWOPI ()

            END IF

         ELSE  IF ( DVL (9) .GT. 1.D0  ) THEN

C
C           Case of hyperbolic orbit.
C
            WRKEL = DACOSH (
     .           (   DVL (9) + DCOS ( WRKEL )     ) /
     .           ( 1.D0 + DVL (9) * DCOS( WRKEL ) )    )
            ELTS (6) = SENSE*( ( DVL (9) * DSINH ( WRKEL ) - WRKEL ) )

         ELSE

C
C           Case of parabolic orbit.
C
            WRKEL = DTAN ( WRKEL / 2.D0 )
            ELTS (6) = SENSE * ( WRKEL + WRKEL ** 3 / 3.D0 )

         END IF

      ELSE IF ( ISRCHI( 19, NPARAM, PARAM ) .NE. 0 ) THEN
      
C
C        Epoch of periapsis is defined in input file.
C
         IF ( DVL (9) .LT. 1.D0 ) THEN

C
C           Case of elliptic orbit.
C
            WRKEL = ( 1.D0 - DVL (9) ) / ELTS (1)
            MEANM = DSQRT ( ELTS (8) * WRKEL ) *  WRKEL
            ELTS (6) = MEANM * ( DVL (1) - DVL (19) )

         ELSE  IF ( DVL (9) .GT. 1.D0  ) THEN

C
C           Case of hyperbolic orbit.
C
            WRKEL = ( DVL (9) - 1.D0 ) / ELTS (1)
            MEANM = DSQRT ( ELTS (8) * WRKEL ) * WRKEL
            ELTS (6) = MEANM * ( DVL (1) - DVL (19) )

         ELSE

C
C           Case of parabolic orbit.
C
            MEANM = DSQRT ( ELTS (8) / 2.D0 / ELTS (1) ) / ELTS (1)
            ELTS (6) = MEANM * ( DVL (1) - DVL (19) )

         END IF

      ELSE IF ( ISRCHI( 20, NPARAM, PARAM ) .NE. 0 ) THEN

C
C        Time interval between current epoch and periapsis epoch
C        is defined in input file.
C
         IF ( DVL (9) .LT. 1.D0 ) THEN

C
C           Case of elliptic orbit.
C
            WRKEL = ( 1.D0 - DVL (9) ) / ELTS (1)
            MEANM = DSQRT ( ELTS (8) * WRKEL ) * WRKEL
            ELTS (6) = MEANM * DVL (20)

         ELSE  IF ( DVL (9) .GT. 1.D0  ) THEN

C
C           Case of hyperbolic orbit.
C
            WRKEL = ( DVL (9) - 1.D0 ) / ELTS (1)
            MEANM = DSQRT ( ELTS (8) * WRKEL ) * WRKEL
            ELTS ( 6 ) = MEANM * DVL (20)

         ELSE

C
C           Case of parabolic orbit.
C
            MEANM = DSQRT ( ELTS (8) / 2.D0 / ELTS (1) ) / ELTS (1)
            ELTS (6) = MEANM * DVL (20)

         END IF

      ELSE

C
C        Not enough input data to calculate mean anomaly. 
C        Complain and exit.
C
         ERRTXT (1) = 'Mean anomaly can not be computed ' //
     .                'from given set of orbital elements.' 
         ERRTXT (2) = 'SPICE(INCOMPLETEELEMENTS)'
         ERRFLG = .TRUE.
         
         CALL CHKOUT ( 'SETELM' )
         RETURN

      END IF

      CALL CHKOUT ( 'SETELM' )
      RETURN
      
      END
