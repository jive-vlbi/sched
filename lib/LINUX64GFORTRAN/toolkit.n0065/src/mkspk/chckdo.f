C$Procedure      CHCKDO ( Check presence of required input parameters )

      SUBROUTINE CHCKDO ( INDTVL, OUTTVL, PARAM, NPARAM, DOVAL )

C$ Abstract
C
C     This routine is a module of the MKSPK program. It checks whether
C     set of input parameters specified in the DATA_ORDER value
C     contains all parameters required for a given input data type and
C     output SPK type.
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

      IMPLICIT              NONE
      INCLUDE               'mkspk.inc' 

      CHARACTER*(*)         INDTVL
      INTEGER               OUTTVL
      INTEGER               PARAM  ( * )
      INTEGER               NPARAM
      CHARACTER*(*)         DOVAL  ( * )

C$ Brief_I/O        
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     INDTVL     I   Input data type.
C     OUTTVL     I   Output spk type.
C     PARAM      I   Array of DATA_ORDER parameter IDs 
C     NPARAM     I   Number of not zero parameter IDs in PARAM
C     DOVAL      I   Array of parameter values acceptable in DATA_ORDER
C
C$ Detailed_Input
C
C     INDTVL      is the input data type. See MKSPK.INC for the 
C                 current list of supported input data types.
C
C     OUTTVL      is the output SPK type. Currently supported output 
C                 SPK types are 5, 8, 9, 12, 13, 15 and 17. 
C
C     PARAM       is an integer array containing indexes of the 
C                 recognizable input parameters present in the
C                 DATA_ORDER keyword value in the order in which they
C                 were provided in that value.
C
C     NPARAM      is the number of elements in PARAM.
C
C     DOVAL       is an array containing complete set recognizable 
C                 input parameters. (see main module for the current
C                 list) 
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     If the set of input parameters does not contain some of the 
C     required tokens, then the error 'SPICE(MISSINGDATAORDERTK)'
C     will be signalled.
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
C-    Version 1.0.3, 29-MAR-1999 (NGK).
C
C        Added comments.
C
C-    Version 1.0.2, 18-MAR-1999 (BVS).
C
C        Corrected comments.
C
C-    Version 1.0.1, 13-JAN-1999 (BVS).
C
C        Modified error messages.
C
C-    Version 1.0.0, 08-SEP-1998 (NGK).
C
C-&
 
C$ Index_Entries
C
C     Check adequacy of the DATA_ORDER defined in MKSPK setup
C
C-&                       
 
C
C     SPICELIB functions
C
      INTEGER               RTRIM 
      INTEGER               ISRCHI 
      LOGICAL               RETURN
      
C
C     Parameters INELTP, INSTTP, INEQTP containing supported 
C     input data type names and keyword parameter KDATOR are declared
C     in the include file.
C

C
C     Local variables
C
      INTEGER               L
      LOGICAL               FOUND
C
C     Error line variable. Size LINLEN declared in the include file.
C

      CHARACTER*(LINLEN)    ERRLIN
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'CHCKDO' )
      END IF 

C
C     Check if EPOCH is present among specified input parameters.
C
      IF ( ISRCHI ( 1, NPARAM, PARAM ) .EQ. 0  ) THEN 

         CALL SETMSG ( 'Set of input data parameters specified ' //
     .                 'in the setup file keyword ''#'' must ' //
     .                 'contain token ''#'' designating epoch ' //
     .                 'position in the input data records.'      )
         CALL ERRCH  ( '#', KDATOR                       )
         CALL ERRCH  ( '#', DOVAL (1)                    )
         CALL SIGERR ( 'SPICE(MISSINGEPOCHTOKEN)'        )
 
      END IF

C
C     Check whether all necessary input parameters are present 
C     according to the input data type.
C 
      FOUND = .TRUE.
      
      ERRLIN = 'The following token(s) designating input '        //
     .         'parameter(s) required when input data type is '   //
     .         '''#'' is(are) missing in the value of the setup ' //
     .         'file keyword ''#'':'
       
      IF ( INDTVL ( : RTRIM ( INDTVL ) ) .EQ. INELTP ) THEN

C
C        Input type is ELEMENTS. Check whether eccentricity,
C        inclination, argument of periapsis and longitude of ascending
C        node are present in the input data.
C
         CALL REPMC ( ERRLIN, '#', INELTP, ERRLIN )
         CALL REPMC ( ERRLIN, '#', KDATOR, ERRLIN )

         IF ( ISRCHI ( 9, NPARAM, PARAM ) .EQ. 0  ) THEN 
          
            CALL INSSUB( ERRLIN, ' ''#'',', RTRIM(ERRLIN)+1, ERRLIN )
            CALL REPMC ( ERRLIN, '#', DOVAL (9), ERRLIN )
            FOUND = .FALSE.
            
         END IF
         
         DO L = 13, 15
                         
            IF ( ISRCHI ( L , NPARAM, PARAM ) .EQ. 0  ) THEN 
         
               CALL INSSUB( ERRLIN, ' ''#'',', RTRIM(ERRLIN)+1, ERRLIN)
               CALL REPMC ( ERRLIN, '#', DOVAL ( L ), ERRLIN )
               FOUND = .FALSE.
            
            END IF                          
         
         END DO
         
      ELSE IF ( INDTVL ( : RTRIM ( INDTVL  ) ) . EQ. INSTTP ) THEN

C
C        Input type is STATES. Check whether all state vector 
C        components are present in the input data.
C      
         CALL REPMC ( ERRLIN, '#', INSTTP, ERRLIN )
         CALL REPMC ( ERRLIN, '#', KDATOR, ERRLIN )
         
         DO L = 2,7
                         
            IF ( ISRCHI ( L , NPARAM, PARAM ) .EQ. 0  ) THEN 
         
               CALL INSSUB( ERRLIN, ' ''#'',', RTRIM(ERRLIN)+1, ERRLIN)
               CALL REPMC ( ERRLIN, '#', DOVAL ( L ), ERRLIN )
               FOUND = .FALSE.
            
            END IF                          
         
         END DO
         
      ELSE IF ( INDTVL ( : RTRIM ( INDTVL  ) ) . EQ. INEQTP ) THEN
 
C
C        Input type is EQ_ELEMENTS. Check whether all equinoctial
C        elements are present in the input data.
C
         CALL REPMC ( ERRLIN, '#', INEQTP, ERRLIN )
         CALL REPMC ( ERRLIN, '#', KDATOR, ERRLIN )

         DO L = 21, 29
                         
            IF ( ISRCHI ( L , NPARAM, PARAM ) .EQ. 0  ) THEN 
         
               CALL INSSUB( ERRLIN, ' ''#'',', RTRIM(ERRLIN)+1, ERRLIN)
               CALL REPMC ( ERRLIN, '#', DOVAL ( L ), ERRLIN )
               FOUND = .FALSE.
            
            END IF                          
         
         END DO 
         
      END IF 
      
C
C     Signal the error if any of the required parameters wasn't found.
C     
      IF ( .NOT. FOUND )  THEN

         ERRLIN(RTRIM(ERRLIN):RTRIM(ERRLIN)) = '.'
         CALL SETMSG ( ERRLIN )
         CALL SIGERR ( 'SPICE(MISSINGDATAORDERTK)' )
               
      END IF  

C
C     Check whether all necessary input parameters are present 
C     according to the output SPK type.
C 
      FOUND = .TRUE.               
      
      IF ( OUTTVL .EQ. 17 ) THEN
                       
C
C        Output type is 17. Verify if dM/dt, dNOD/dt, dPER/dt
C        exist in input data.
C      
         ERRLIN = 'The following token(s) designating input ' //
     .            'parameter(s) required when output SPK type is ' //
     .            '17 is(are) missing in the value of the setup ' //
     .            'file keyword ''#'':'
       
         DO L = 27, 29
                         
            IF ( ISRCHI ( L , NPARAM, PARAM ) .EQ. 0  ) THEN 
         
               CALL INSSUB( ERRLIN, ' ''#'',', RTRIM(ERRLIN)+1, ERRLIN)
               CALL REPMC ( ERRLIN, '#', DOVAL ( L ), ERRLIN )
               FOUND = .FALSE.
            
            END IF                          
         
         END DO 
          
      END IF
      
C
C     Signal the error if any of the required parameters wasn't found.
C     
      IF ( .NOT. FOUND )  THEN
         
         ERRLIN(RTRIM(ERRLIN):RTRIM(ERRLIN)) = '.'
         CALL SETMSG ( ERRLIN )
         CALL SIGERR ( 'SPICE(MISSINGDATAORDERTK)' )
         
      END IF

      CALL CHKOUT ( 'CHCKDO' )
            
      RETURN

      END

