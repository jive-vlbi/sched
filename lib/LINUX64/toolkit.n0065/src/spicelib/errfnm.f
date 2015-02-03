C$Procedure      ERRFNM ( Insert filename into long error message text )
 
      SUBROUTINE ERRFNM ( MARKER, UNIT )
 
C$ Abstract
C
C     Substitute the first occurrence of a marker in the current long
C     error message with the name of the file attached to the logical
C     unit number.
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
C     ERROR
C
C$ Keywords
C
C     ERROR
C     STRING
C     UNITS
C
C$ Declarations
 
      CHARACTER*(*)         MARKER
 
      INTEGER               UNIT
 
      INTEGER               FILEN
      PARAMETER           ( FILEN = 128 )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     MARKER     I   A substring in the error message that is to be
C                    replaced.
C     UNIT       I   Logical unit number attached to a file.
C     FILEN      P   Maximum length of filename.
C
C$ Detailed_Input
C
C     MARKER     is a character string which marks a position in
C                the long error message where a character string
C                is to be substituted.  Leading and trailing blanks
C                in MARKER are not significant.
C
C                Case IS significant;  'XX' is considered to be
C                a different marker from 'xx'.
C
C     UNIT       is the logical unit number attached to a file.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     FILEN      is the maximum file name length that can be
C                accommodated by this routine.
C
C$ Exceptions
C
C     Error free.
C
C     1) If the logical unit number is not attached to a file, the
C        string inserted into the long error message is:
C
C        '<unavailable from the system>'
C
C     2) If the FORTRAN INQUIRE statement fails to execute properly,
C        the string inserted into the long error message is:
C
C        '<unavailable from the system>'
C
C$ Files
C
C     See "Detailed_Input" description of the variable UNIT.
C
C$ Particulars
C
C     None.
C
C$ Examples
C
C     1. The following code fragment reads a record from a file
C        then checks to see if the read was successful. If the
C        read failed, an error message is constructed that
C        specifies the record number, the filename and the value
C        of IOSTAT.
C
C        ERRFNM is used to replace the marker in the long error
C        message with the name of the file.
C
C
C        READ ( UNIT, REC=RECNUM, IOSTAT=IOSTAT ) RECORD
C
C         IF ( IOSTAT .NE. 0 ) THEN
C
C            CALL SETMSG ( 'Error reading record number # from ' //
C      .                   'file FILENAME. The value of IOSTAT ' //
C      .                   'was #.'                              )
C
C            CALL ERRINT ( '#',         RECNUM   )
C            CALL ERRFNM ( 'FILENAME',  UNIT     )
C            CALL ERRINT ( '#',         IOSTAT   )
C            CALL SIGERR ( 'SPICE(READFAILURE)'  )
C            CALL CHKOUT ( 'SAMPLE'              )
C            RETURN
C
C         END IF
C
C
C         If the unit is attached to the file SAMPLE.DAT, RECNUM
C         is 15 and IOSTAT is 36, and the INQUIRE statement in
C         this routine executed successfully, the long error
C         message is:
C
C           'Error reading record number 15 from file SAMPLE.DAT.
C            The value of IOSTAT was 36.'
C
C
C         If the unit is not attached to a file or if the INQUIRE
C         statement in this routine failed to execute successfully,
C         the long error message is:
C
C           'Error reading record number 15 from file
C           <unavailable from the system>. The value of IOSTAT
C           was 36.'
C
C
C     2. Note that the case of the marker is significant. The following
C        code fragment contains a call to ERRFNM using a marker
C        that does not appear in the long error message.
C
C
C        READ ( UNIT, REC=RECNUM, IOSTAT=IOSTAT ) RECORD
C
C         IF ( IOSTAT .NE. 0 ) THEN
C
C            CALL SETMSG ( 'Error reading record number # from ' //
C      .                   'file FILENAME. The value of IOSTAT ' //
C      .                   'was #.'                              )
C
C            CALL ERRINT ( '#',         RECNUM   )
C            CALL ERRFNM ( 'filename',  UNIT     )
C            CALL ERRINT ( '#',         IOSTAT   )
C            CALL SIGERR ( 'SPICE(READFAILURE)'  )
C            CALL CHKOUT ( 'SAMPLE'              )
C            RETURN
C
C         END IF
C
C
C         If the marker is not found, ERRFNM does not substitute
C         the filename for the marker. The long error message in
C         this case is:
C
C           'Error reading record number 15 from file FILENAME.
C            The value of IOSTAT was 36.'
C
C$ Restrictions
C
C     The filename length is restricted by the parameter FILEN.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     H.A. Neilan    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 05-APR-1991 (HAN)
C
C-&
 
C$ Index_Entries
C
C     insert filename into long error message
C
C-&
 
 
 
 
 
 
C
C     Local variables
C
      CHARACTER*(FILEN)     NAME
 
      INTEGER               IOSTAT
 
 
 
 
C
C     Initialize the variables.
C
      NAME = ' '
 
C
C     Get the name of the file attached to the logical unit number.
C
 
      INQUIRE ( UNIT   = UNIT,
     .          NAME   = NAME,
     .          IOSTAT = IOSTAT )
 
 
C
C     If the INQUIRE statement executed successfully and the unit
C     was attached to a file, we have a filename.
C
C     If the INQUIRE statement didn't execute successfully the value
C     of IOSTAT is not equal to zero. If the unit is not connected to
C     a file the filename is blank. If either of these two things
C     are true, we must construct a string indicating that the
C     filename was unavailable from the system.
C
 
      IF ( ( IOSTAT .NE. 0 ) .OR. ( NAME .EQ. ' ' ) ) THEN
 
         NAME = '<unavailable from the system>'
 
      END IF
 
C
C     Let the error handling routine take it from here.
C
      CALL ERRCH ( MARKER, NAME )
 
      RETURN
      END
