C$Procedure      DASECU ( DAS extract comments to a logical unit )
 
      SUBROUTINE DASECU ( HANDLE, COMLUN, COMNTS )
 
C$ Abstract
C
C     Extract comments from a previously opened binary DAS file to a
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
C      HANDLE    I   Handle of a DAS file opened with read access.
C      COMLUN    I   Logical unit of an opened text file.
C      COMNTS    O   Logical flag, indicating comments were found.
C
C$ Detailed_Input
C
C     HANDLE   The file handle for a binary DAS file that has been
C              opened with read access.
C
C     COMLUN   The Fortran logical unit of a previously opened text
C              file to which the comments from a binary DAS file are
C              to be written.
C
C              The comments will be placed into the text file beginning
C              at the current location in the file, and continuing
C              until all of the comments have been written.
C
C$ Detailed_Output
C
C     COMNTS   A logical flag indicating whether or not any comments
C              were found in the comment area of a DAS file. COMNTS will
C              have the value .TRUE. if there were some comments, and
C              the value .FALSE. otherwise.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)   If an error occurs while reading from the binary DAS file
C          attached to HANDLE, a routine called by this routine will
C          signal an error.
C
C     2)   If an error occurs while writing to the text file attached
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
C     area of a binary DAS file, placing them into a text file
C     attached to COMLUN, beginning at the current position in the
C     text file. If there are no comments in the DAS file, nothing is
C     written to the text file attached to COMLUN.
C
C$ Examples
C
C      Let
C
C         HANDLE   be the DAS file handle of a previously opened binary
C                  DAS file.
C
C         COMLUN   be the Fortran logical unit of a previously opened
C                  text file that is to accept the comments from the
C                  DAS comment area.
C
C      The subroutine call
C
C         CALL DASECU ( HANDLE, COMLUN, COMNTS )
C
C      will extract the comments from the comment area of the binary
C      DAS file attached to HANDLE, if there are any, and write them
C      to the logical unit COMLUN. Upun successfur completion, the
C      value of COMNTS will be .TRUE. if there were some comments
C      in the comment area and .FALSE. otherwise.
C
C$ Restrictions
C
C     The maximum length of a single line comment in the comment area is
C     specified by the parameter LINLEN defined below. Currently this
C     value is 255 characters.
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
C-    Beta Version 1.0.0, 5-JAN-1993 (KRG)
C
C-&
 
C$ Index_Entries
C
C      extract comments from a DAS file to a logical unit
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
      PARAMETER           ( LINLEN = 255 )
C
C     Set the size of the comment buffer.
C
      INTEGER               BUFSIZ
      PARAMETER           ( BUFSIZ = 22 )
C
C     Local variables
C
      CHARACTER*(LINLEN)    COMBUF(BUFSIZ)
 
      INTEGER               NUMCOM
 
      LOGICAL               EOC
      LOGICAL               GOTSOM
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DASECU' )
      END IF
C
C     Verify that the DAS file attached to HANDLE is opened for reading.
C
      CALL DASSIH ( HANDLE, 'READ' )
 
      IF ( FAILED() ) THEN
 
         CALL CHKOUT ( 'DASECU' )
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
         CALL DASEC ( HANDLE, BUFSIZ, NUMCOM, COMBUF, EOC )
 
         IF ( FAILED() ) THEN
 
            CALL CHKOUT( 'DASECU' )
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
 
               CALL CHKOUT( 'DASECU' )
               RETURN
 
            END IF
 
         END IF
 
      END DO
C
C     Set the output flag indicating whether or not we got any comments.
C
      COMNTS = GOTSOM
 
      CALL CHKOUT ( 'DASECU' )
      RETURN
      END
