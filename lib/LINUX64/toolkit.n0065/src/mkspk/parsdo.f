C$Procedure      PARSDO ( Parsing of DATA_ORDER string )

      SUBROUTINE PARSDO ( LINE, DOVAL, NVAL, PARAM, NPARAM )

C$ Abstract
C
C     This routine is a module of the MKSPK program. It parses the
C     DATA_ORDER value provided in a setup file and forms an array 
C     of indexes of recognizable input parameters contaned in it.
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
C     PARSING
C
C$ Declarations

      IMPLICIT              NONE
      
      INCLUDE               'mkspk.inc'

      CHARACTER*(*)         LINE
      CHARACTER*(*)         DOVAL (*)
      INTEGER               NVAL
      INTEGER               PARAM (*)
      INTEGER               NPARAM 

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  ----------------------------------------------
C     LINE       I   DATA_ORDER string
C     DOVAL      I   Array of recognizable input parameter names
C     NVAL       I   Number of recognizable input parameters 
C     PARAM      O   Array of parameter IDs present in DATA_ORDER 
C     NPARAM     O   Number of elements in PARAM
C
C$ Detailed_Input
C
C     LINE        is the DATA_ORDER value that will be parsed.
C
C     DOVAL       is an array containing complete set recognizable 
C                 input parameters (see main module for the current
C                 list). 
C
C     NVAL        is the total number of recognizable input parameters
C                 (number of elements in DOVAL).
C
C$ Detailed_Output
C
C     PARAM       is an integer array containing indexes of the 
C                 recognizable input parameters present in the input 
C                 DATA_ORDER value in the order in which they are 
C                 provided in that value.
C
C     NPARAM      is the number of elements in PARAM.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If token in the data order is not recognized, then the 
C        error 'SPICE(BADDATAORDERTOKEN)' will be signalled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This subroutine parses DATA_ORDER string containing names of
C     input data record parameters in the order in which they appear
C     in the input records and returns an integer array of the indexes
C     of the parameters that were found in the string.
C
C$ Examples
C
C     Let DATA_ORDER has following value: 
C
C        LINE      = 'EPOCH X Y Z SKIP VX VY VZ'
C
C     and DOVAL array contains the following values:
C
C        DOVAL(1)  =  'EPOCH'
C        DOVAL(2)  =  'X'                   
C        DOVAL(3)  =  'Y' 
C        DOVAL(4)  =  'Z'
C        DOVAL(5)  =  'VX'
C        DOVAL(6)  =  'VY' 
C        DOVAL(7)  =  'VZ'
C        ...
C        DOVAL(30) =  'SKIP' 
C
C     Then after parsing we will have on the output:
C
C        NPARAM    = 8 
C 
C        PARAM     = 1, 2, 3, 4, 30, 5, 6, 7
C
C$ Restrictions
C
C     Because search for a parameter in the DATA_ORDER value is case 
C     sensitive, the DATA_ORDER value and parameter names must be 
C     in the same case (nominally uppercase).
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
C        Corrected examples section.
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
C     Parse MKSPK setup DATA_ORDER string.                  
C
C-&
 
C
C     SPICELIB functions
C
      INTEGER               ISRCHC 
      INTEGER               LASTNB
      LOGICAL               RETURN
      
C
C     Local variables
C
      INTEGER               I 
      INTEGER               L 
      
C
C     Size VALUEL declared in the include file.
C       
      CHARACTER*( VALUEL )  VALUE     
      
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'PARSDO' ) 
      END IF

C
C     Assign zero to PARAM array.
C
      DO L = 1, NVAL 
      
         PARAM ( L ) = 0
         
      END DO  

C
C     Reset counter of words on line.
C       
      NPARAM = 0
          
      DO WHILE ( LASTNB ( LINE ) .NE. 0 )

C     
C        Get next word from the line. Value is already uppercase.
C
         CALL NEXTWD ( LINE, VALUE, LINE )  
         I = ISRCHC  ( VALUE, NVAL, DOVAL ) 

C         
C        Look whether this value is one of the reserved values.
C         
         IF ( I .NE. 0 ) THEN
         
C                   
C           This value is OK. Memorize it.
C
            NPARAM = NPARAM + 1
            PARAM ( NPARAM ) =  I
               
         ELSE 
          
C
C           We can not recognize this value.
C              
            CALL SETMSG ( 'Can not recognize token ''#'' in '     // 
     .                    'the value of the setup file '          //
     .                    'keyword ''#''. Refer to the User''s '  //
     .                    'Guide for the program for '            //
     .                    'complete list of allowed tokens.'      )
            CALL ERRCH  ( '#', VALUE                              )
            CALL ERRCH  ( '#', KDATOR                             )
            CALL SIGERR ( 'SPICE(BADDATAORDERTOKEN)'              )
            
         END IF            
      
      END DO 
      
      CALL CHKOUT ( 'PARSDO' )
      
      RETURN 
      
      END
      
