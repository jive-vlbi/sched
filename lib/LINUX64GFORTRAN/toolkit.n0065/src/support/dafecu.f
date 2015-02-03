C$Procedure      DAFECU( DAF extract comments to a logical unit )

      SUBROUTINE DAFECU ( HANDLE, COMLUN, COMNTS )

C$ Abstract
C
C     Extract comments from a previously opened binary DAF file to a
C     previously opened text file attached to a Fortran logical unit.
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
C     None.
C
C$ Declarations

      INTEGER               HANDLE
      INTEGER               COMLUN
      LOGICAL               COMNTS

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C      HANDLE    I   Handle of a DAF file opened with read access.
C      COMLUN    I   Logical unit of an opened text file.
C      COMNTS    O   Logical flag, indicating comments were found.
C
C$ Detailed_Input
C
C     HANDLE   The file handle for a binary DAF file that has been
C              opened with read access.
C
C     COMLUN   The Fortran logical unit of a previously opened text
C              file to which the comments from a binary DAF file are
C              to be written.
C              
C              The comments will be placed into the text file beginning
C              at the current location in the file and continuing
C              until all of the comments from the comment area of the 
C              DAF file have been written.
C              
C$ Detailed_Output
C
C     COMNTS   A logical flag indicating whether or not any comments
C              were found in the comment area of a DAF file. COMNTS will
C              have the value .TRUE. if there were some comments, and
C              the value .FALSE. otherwise.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)   If the input logical unit COMLUN is not positive or there
C          is not an opened file attached to it, the error 
C          SPICE(INVALIDARGUMENT) will be signalled.
C
C     2)   If the INQUIRE on the logical unit to see if there is a
C          file attached fails, the error SPICE(INQUIREFAILED) will
C          be signalled.
C
C     3)   If an error occurs while reading from the binary DAF file
C          attached to HANDLE, a routine called by this routine will
C          signal an error.
C          
C     4)   If an error occurs while writing to the text file attached
C          to COMLUN, a routine called by this routine will signal an
C          error.
C
C$ Files
C
C     See parameters COMLUN and HANDLE in the $ Detailed_Inputs section.
C
C$ Particulars
C
C     This routine will extract all of the comments from the comment
C     area of a binary DAF file, placing them into a text file
C     attached to COMLUN beginning at the current position in the 
C     text file. If there are no comments in the DAF file, nothing is 
C     written to the text file attached to COMLUN.
C
C$ Examples
C
C      Let
C      
C         HANDLE   be the DAF file handle of a previously opened binary
C                  DAF file.
C                  
C         COMLUN   be the Fortran logical unit of a previously opened
C                  text file that is to accept the comments from the 
C                  DAF comment area.
C                  
C      The subroutine call
C      
C         CALL DAFECU ( HANDLE, COMLUN, COMNTS )
C         
C      will extract the comments from the comment area of the binary
C      DAF file attached to HANDLE, if there are any, and write them
C      to the logical unit COMLUN. Upon successful completion, the
C      value of COMNTS will be .TRUE. if there were some comments
C      in the comment area of the DAF file and .FALSE. otherwise.
C
C$ Restrictions
C
C     The maximum length of a single comment line in the comment area is
C     specified by the parameter LINLEN defined below. Currently this
C     value is 1000 characters.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     K.R. Gehringer (JPL)
C
C$ Version
C
C-    Beta Version 1.1.1, 08-MAY-2001 (BVS)
C
C        Buffer line size (LINLEN) was increased from 255 to 1000
C        characters to make it consistent the line size in SPC 
C        routines.
C
C-    Beta Version 1.1.0, 18-JAN-1996 (KRG)
C
C        Added a test and errors for checking to see whether COMLUN
C        was actually attached to an ASCII text file when this routine 
C        was called.
C
C-    Beta Version 1.0.0, 23-SEP-1994 (KRG)
C
C-&

C$ Index_Entries
C
C      extract comments from a DAF to a logical unit
C
C-&

C
C     SPICELIB functions
C
      LOGICAL               FAILED
      LOGICAL               RETURN
C
C     Local parameters
C
C     Set the value for the maximum length of a text line.
C     
      INTEGER               LINLEN
      PARAMETER           ( LINLEN = 1000 )
C
C     Set the size of the comment buffer.
C     
      INTEGER               BUFSIZ
      PARAMETER           ( BUFSIZ = 22 )
C
C     Local variables
C
      CHARACTER*(LINLEN)    COMBUF(BUFSIZ)
      
      INTEGER               IOSTAT
      INTEGER               NUMCOM
      
      LOGICAL               EOC
      LOGICAL               GOTSOM
      LOGICAL               OPENED

C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DAFECU' )
      END IF
C
C     Verify that the DAF file attached to HANDLE is opened for reading.
C     
      CALL DAFSIH ( HANDLE, 'READ' )
      
      IF ( FAILED() ) THEN
      
         CALL CHKOUT ( 'DAFECU' )
         RETURN
         
      END IF
C
C     Logical units must be positive. If it is not, signal an error.
C
      IF ( COMLUN .LE. 0 ) THEN

         CALL SETMSG ( '# is not a valid logical unit. Logical'
     .   //            ' units must be positive.'               )
         CALL ERRINT ( '#', COMLUN                              )
         CALL SIGERR ( 'SPICE(INVALIDARGUMENT)'                 )
         CALL CHKOUT ( 'DAFECU' )
         RETURN

      END IF
C
C     Verify that there is an open ASCII text file attached to COMLUN.
C
      INQUIRE ( UNIT=COMLUN, OPENED=OPENED, IOSTAT=IOSTAT )

      IF ( IOSTAT .NE. 0 ) THEN

         CALL SETMSG ( 'The INQUIRE on logical unit # failed. The'
     .   //            ' value of IOSTAT was #.'                   )
         CALL ERRINT ( '#', COMLUN                                 )
         CALL ERRINT ( '#', IOSTAT                                 )
         CALL SIGERR ( 'SPICE(INQUIREFAILED)'                      )
         CALL CHKOUT ( 'DAFECU' )
         RETURN

      END IF

      IF ( .NOT. OPENED ) THEN

         CALL SETMSG ( 'There is no open file attached to logical' 
     .   //            ' unit #, so no comments could be written.' )
         CALL ERRINT ( '#', COMLUN                                 )
         CALL SIGERR ( 'SPICE(INVALIDARGUMENT)'                    )
         CALL CHKOUT ( 'DAFECU' )
         RETURN

      END IF
C
C     Initialize some things before the loop.
C     
      NUMCOM = 0
      EOC    = .FALSE.
      GOTSOM = .FALSE.
      
      DO WHILE ( .NOT. EOC )
C
C        While we have not reached the end of the comments, get some
C        more.
C        
         CALL DAFEC ( HANDLE, BUFSIZ, NUMCOM, COMBUF, EOC )
         
         IF ( FAILED() ) THEN
         
            CALL CHKOUT( 'DAFECU' )
            RETURN
         
         END IF
         
         IF ( NUMCOM .GT. 0 ) THEN
C   
C           If NUMCOM .GT. 0 then we did get some comments, and we need
C           to write them out, but first, set the flag indicating that
C           we got some comments.
C     
            IF ( .NOT. GOTSOM ) THEN
            
               GOTSOM = .TRUE.
               
            END IF

            CALL WRITLA ( NUMCOM, COMBUF, COMLUN )
            
            IF ( FAILED() ) THEN
         
               CALL CHKOUT( 'DAFECU' )
               RETURN
            
            END IF
     
         END IF
         
      END DO
C
C     Set the output flag indicating whether or not we got any comments.
C     
      COMNTS = GOTSOM
      
      CALL CHKOUT ( 'DAFECU' )
      RETURN
      END
