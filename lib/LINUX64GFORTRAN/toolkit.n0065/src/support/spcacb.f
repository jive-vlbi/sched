C$ Procedure  SPCACB  ( SPK and CK add comments from a buffer  )
 
      SUBROUTINE SPCACB( DAFHDL, BUFFER )
 
C$ Abstract
C
C     Store text from a line buffer in the comment area of a binary SPK
C     or CK file, appending it to whatever text may already have
C     been stored there.
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
C     SPC
C
C$ Keywords
C
C     FILES
C     UTILITY
C
C$ Declarations
C
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )
 
      INTEGER               MXCREC
      PARAMETER           ( MXCREC = 1000 )
 
      INTEGER               LINLEN
      PARAMETER           ( LINLEN = 255 )
 
      INTEGER               DAFHDL
      CHARACTER*(*)         BUFFER(LBCELL:*)
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C      LBCELL    P    Lower bound for the CELL 'data type'
C      MXCREC    P    Maximum length of a character record in a DAF
C      LINLEN    P    The maximum length of an input line
C      DAFHDL    I    DAF file handle for output
C      BUFFER    I    Buffer of comment lines to be written
C
C$ Detailed_Input
C
C     DAFHDL   The NAIF DAF file handle for accessing a DAF file.
C
C     BUFFER   A list of comment lines which are to be added to the
C              comment area of the binary DAF file attached to the
C              DAF file handle DAFHDL.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     MXCREC   This is the maximum length of a character record in a
C              DAF file.
C
C     LBCELL   This is the lower bound for the CELL data type which
C              is supported by SPICELIB.
C
C     LINLEN   This is the maximum length of a single text record in
C              a text file.
C
C$ Exceptions
C
C     1)   If the length of the cell buffer is not positive, the error
C          SPICE(NONPOSBUFLENGTH) will be signalled.
C
C     2)   If the end of of comment marker is not found, then the error
C          SPICE(MISSINGEOT) will be signalled. ( NOTE: the end comment
C          marker is also referred to as the end of transmission
C          character. )
C
C     3)   If the comment area of the file exists, i.e., the number of
C          comment records is greater than zero, and the last comment
C          record is not the last reserved record, then the error
C          SPICE(BADCOMMENTAREA) will be signalled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine will take a character CELL buffer of text lines and
C     append them to the comment area of a binary SPK or CK DAF file.
C     The lines of text in the buffer will be 'packed' into a DAF
C     character record, and when the character record is full it will be
C     written to the comment area of the file. This is repeated until
C     all of the lines in the buffer have been processed.
C
C     If there are no comments in the comment area, then space will
C     be allocated in the file and the text lines in BUFFER will be
C     written into the file. Blank text lines are allowed. If there
C     are already comments in the comment area, then the text lines
C     in BUFFER will be appended to these comments, with a single
C     blank line separating the two comment blocks.
C
C$ Examples
C
C     Let
C           DAFHDL = The DAF handle for an SPK or CK file
C
C           BUFFER = A list of text lines to be added to the comment
C                    area of the SPK or CK file.
C
C     The call
C
C           CALL SPCACB( DAFHDL, BUFFER )
C
C     will append the text line(s) in BUFFER to the comment area of
C     the SPK or CK file.
C
C$ Restrictions
C
C     The conventions for the comment area specified by the SPC family
C     of routines is used. Any SPK or CK files which do not conform
C     to these conventions may not have 'readable' comment areas. Only
C     comments are to be placed into the comment area, where a comment
C     consists of only ASCII printable characters.
C
C     NOTE: The SPC family of routines should be the only routines used
C           to write to and read from the comment area of SPK or CK
C           files.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     K.R. Gehringer (JPL)
C     B.V. Semenov   (JPL)
C
C$ Version
C
C-    Beta Version 1.1.0, 18-MAY-2004 (BVS)
C
C        Removed check requiring the number of comment records to be
C        one less than the number of reserved records. Fixed logic
C        adding the end-of-comment marker to handle cases when it
C        "rolls" over to the next reserved record.
C
C-    Beta Version 1.0.1, 30-MAR-1999 (BVS)
C
C        Changed LINLEN to 255 (was 80). 
C
C-    Beta Version 1.0.0, 23-APR-1992 (KRG)
C
C-&
 
C$ Index_Entries
C
C      WRITE A LINE BUFFER TO AN SPK OR CK COMMENT AREA
C
C-&
 
 
 
C
C     SPICELIB functions
C
      INTEGER               CPOS
      INTEGER               LASTNB
      INTEGER               CARDC
 
      LOGICAL               RETURN
C
C     Local parameters
C
      INTEGER               INTEOC
      PARAMETER           ( INTEOC = 4 )
 
      INTEGER               INTEOL
      PARAMETER           ( INTEOL = 0 )
 
      INTEGER               CASTRT
      PARAMETER           ( CASTRT = 2 )
C
C     Local variables
C
      CHARACTER*(LINLEN)    LINE
      CHARACTER*(MXCREC)    CRECRD
      CHARACTER*(1)         EOCMRK
      CHARACTER*(1)         EOLMRK
C
C     This is needed for the call to DAFRFR to get some of the
C     information needed. It is not used anywhere else.
C
      CHARACTER*(LINLEN)    IFNAME
 
      INTEGER               I
      INTEGER               J
      INTEGER               LENGTH
      INTEGER               CURPOS
      INTEGER               EOCPOS
      INTEGER               NCRECS
      INTEGER               NNRECS
      INTEGER               NRRECS
      INTEGER               RECNO
      INTEGER               NCHARS
      INTEGER               NLINES
      INTEGER               SPACE
C
C     These are needed to call DAFRFR to get some of the information
C     needed. Only FIRST will be used, and this is to determine the
C     number of reserved records which exist.
C
      INTEGER               ND
      INTEGER               NI
      INTEGER               FIRST
      INTEGER               LAST
      INTEGER               FREE
C
C     Initial values
C
      EOCMRK = CHAR( INTEOC )
      EOLMRK = CHAR( INTEOL )
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SPCACB' )
      END IF
C
C     Give some of the variables a value so that they have one.
C
      NCRECS = 0
      NNRECS = 0
      NRRECS = 0
C
C     First, extract the number of lines in the buffer
C
      NLINES = CARDC( BUFFER )
C
C     Check for a nonpositive number of lines.
C
      IF ( NLINES .LE. 0 ) THEN
         CALL SETMSG ( 'An invalid buffer length was found: #' )
         CALL ERRINT ( '#', NLINES )
         CALL SIGERR ( 'SPICE(NONPOSBUFLENGTH)' )
         CALL CHKOUT ( 'SPCACB' )
         RETURN
      END IF
C
C     Count the number of characters in the buffer, ignoring leading
C     and trailing blanks on nonblank lines. Blank lines will not count
C     here, their contribution to the size of the comment area will be
C     incorporated later. This is for determining the number of
C     character records to add to the file attached to handle DAFHDL.
C
      NCHARS = 0
      I = 0
      DO WHILE ( I .LT. NLINES )
         I = I + 1
         LINE = BUFFER(I)
         LENGTH = LASTNB( LINE )
         NCHARS = NCHARS + LENGTH
      END DO
C
C     Add NLINES + 1 to NCHARS to allow for the end of line markers
C     ( EOLMRK ) and the end of comments marker ( EOCMRK ).
C
      NCHARS = NCHARS + NLINES + 1
C
C     Get the number of reserved records from the file.
C
      CALL DAFRFR( DAFHDL, ND, NI, IFNAME, FIRST, LAST, FREE )

C
C     Subtract 1 from FIRST to obtain the number of reserved records.
C
C     Note that this should be one more than the number of comment
C     records in the comment area for the SPK or CK file comment area
C     to conform to the SPC comment area conventions. That is, the
C     number of reserved records = the number of comment records + 1.
C
      NRRECS = FIRST - 1

C
C     If the number of reserved records, NRRECS, is greater then 1,
C     determine the number of comment records in the comment area.
C     The comments begin on record CASTRT and should continue to record
C     NRRECS - 1. The comments are terminated by and end of comment
C     marker EOCMRK = CHAR(4).
C
      EOCPOS = 0
      I = 0
      DO WHILE ( (I .LT. NRRECS - 1 ) .AND. ( EOCPOS .EQ. 0 ) )
         RECNO = CASTRT + I
         CALL DAFRCR ( DAFHDL, RECNO, CRECRD )
         EOCPOS = CPOS ( CRECRD, EOCMRK, 1 )
         I = I + 1
      END DO
 
      IF ( ( EOCPOS .EQ. 0 ) .AND. ( NRRECS .GT. 1 ) ) THEN
         CALL SETMSG ( 'End-of-transmission character missing in ' //
     .                 'comment area of binary file.' )
         CALL SIGERR ( 'SPICE(MISSINGEOT)' )
         CALL CHKOUT ( 'SPCACB' )
         RETURN
      END IF
 
      NCRECS = I

C
C     Check to see if the number of comment records is one less than
C     the number of reserved records. If not, signal an error.
C
C      IF ( NCRECS .NE. NRRECS - 1 ) THEN
C         CALL SETMSG ( 'The number of comment records and the'//
C     .                 ' number of reserved records do not agree.'//
C     .                 ' The comment area could be bad.' )
C         CALL SIGERR ( 'SPICE(BADCOMMENTAREA)' )
C         CALL CHKOUT ( 'SPCACB' )
C         RETURN
C      END IF
C
C     Determine the amount of free space in the comment area. This
C     will be the space remaining on the last comment record, i.e.,
C     the maximum length of a DAF character record - the position
C     of the end of comments marker - 1.
C
      IF ( NCRECS .GT. 0 ) THEN
         SPACE = MXCREC - EOCPOS
      ELSE
         SPACE = 0
      END IF
C
C     Determine the number of extra reserved records which are
C     necessary to store the comments in the buffer.
C
      IF ( NCHARS .GT. SPACE ) THEN
         NNRECS = 1 + ( NCHARS - SPACE ) / MXCREC
      ELSE
         NNRECS = 0
      END IF
C
C     Now call the DAF routine to add reserved records to the file,
C     if we need to.
C
      IF ( NNRECS .GT. 0 ) THEN
         CALL DAFARR ( DAFHDL, NNRECS )
      END IF
C
C     At this point, we know that we have enough space to write the
C     comments in the buffer to the comment area. Either there was
C     enough space already there, or we figured out how many new
C     character records were needed, and we added them to the file.
C     So, now we begin 'packing' the comments into the character record.
C
C     We begin by reading the last comment record if there is one,
C     otherwise we just initialize the appropriate variables.
C
      IF ( NCRECS .EQ. 0 ) THEN
         RECNO = CASTRT
         CURPOS = 0
         CRECRD = ' '
      ELSE
         RECNO = CASTRT + NCRECS - 1
         CALL DAFRCR( DAFHDL, RECNO, CRECRD )
C
C        Find the end of comment marker again. This is really not
C        necessary, but it is here to localize all the info needed.
C
         EOCPOS = CPOS( CRECRD, EOCMRK, 1 )
C
C        Set the current record position
C
         CURPOS = EOCPOS
C
C        Put an end of line marker here to separate the new comments
C        from the old ones, and increment the current record position.
C
         CRECRD(CURPOS:CURPOS) = EOLMRK
      END IF
 
      I = 0
      DO WHILE ( I .LT. NLINES )
         I = I + 1
         LINE = BUFFER(I)
         LENGTH = LASTNB( LINE )
         J = 0
         DO WHILE ( J .LT. LENGTH )
            IF ( CURPOS .LT. MXCREC ) THEN
               J = J + 1
               CURPOS = CURPOS + 1
               CRECRD(CURPOS:CURPOS) = LINE(J:J)
            ELSE
               CALL DAFWCR( DAFHDL, RECNO, CRECRD )
               RECNO = RECNO + 1
               CURPOS = 0
               CRECRD = ' '
            END IF
         END DO
C
C        Check to see if we happened to get exactly MXCREC characters
C        when we stopped moving characters from LINE. If we did, then
C        we need to write out the current record and appropriately
C        adjust the necessary variables.
C
         IF ( CURPOS .EQ. MXCREC ) THEN
            CALL DAFWCR( DAFHDL, RECNO, CRECRD )
            RECNO = RECNO + 1
            CURPOS = 0
            CRECRD = ' '
         END IF
         CURPOS = CURPOS + 1
         CRECRD(CURPOS:CURPOS) = EOLMRK
      END DO
C
C     We have now finished processing all of the lines, so we
C     need to append the end of comment marker to the current
C     record and write it to the file.
C
      IF ( CURPOS .EQ. MXCREC ) THEN
         CALL DAFWCR( DAFHDL, RECNO, CRECRD )
         RECNO = RECNO + 1
         CURPOS = 0
         CRECRD = ' '
      END IF

      CURPOS = CURPOS + 1
      CRECRD(CURPOS:CURPOS) = EOCMRK
      CALL DAFWCR( DAFHDL, RECNO, CRECRD )
       
      CALL CHKOUT ( 'SPCACB' )
      RETURN
      END
