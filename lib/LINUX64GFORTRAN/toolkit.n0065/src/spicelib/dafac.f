C$Procedure DAFAC ( DAF add comments )
 
      SUBROUTINE DAFAC ( HANDLE, N, BUFFER )
 
C$ Abstract
C
C     Add comments from a buffer of character strings to the comment
C     area of a binary DAF file, appending them to any comments which
C     are already present in the file's comment area.
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
C     DAF
C
C$ Keywords
C
C     FILES
C     UTILITY
C
C$ Declarations
 
      INTEGER               HANDLE
      INTEGER               N
      CHARACTER*(*)         BUFFER(*)
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C      HANDLE    I    handle of a DAF opened with write access.
C      N         I    Number of comments to put into the comment area.
C      BUFFER    I    Buffer of comments to put into the comment area.
C
C$ Detailed_Input
C
C     HANDLE   The file handle of a binary DAF which has been opened
C              with write access.
C
C     N        The number of comments in BUFFER that are to be added to
C              the comment area of the binary DAF attached to HANDLE.
C
C     BUFFER   A buffer containing comments which are to be added
C              to the comment area of the binary DAF attached to HANDLE.
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
C     1)   If the number of comments to be added is not positive, the
C          error SPICE(INVALIDARGUMENT) will be signalled.
C
C     2)   If a non printing ASCII character is encountered in the
C          comments, the error SPICE(ILLEGALCHARACTER) will be
C          signalled.
C
C     3)   If the binary DAF file attached to HANDLE is not open with
C          write access an error will be signalled by a routine called
C          by this routine.
C
C     4)   If the end of the comments cannot be found, i.e., the end of
C          comments marker is missing on the last comment record, the
C          error SPICE(BADCOMMENTAREA) will be signalled.
C
C$ Files
C
C     See argument HANDLE in $ Detailed_Input.
C
C$ Particulars
C
C     A binary DAF contains a data area which is reserved for storing
C     annotations or descriptive textual information about the data
C     contained in a file. This area is referred to as the ``comment
C     area'' of the file. The comment area of a DAF is a line oriented
C     medium for storing textual information. The comment area
C     preserves leading or embedded white space in the line(s) of text
C     which are stored so that the appearance of the information will
C     be unchanged when it is retrieved (extracted) at some other time.
C     Trailing blanks, however, are NOT preserved, due to the way that
C     character strings are represented in standard Fortran 77.
C
C     This routine will take a buffer of text lines and add (append)
C     them to the comment area of a binary DAF. If there are no
C     comments in the comment area of the file, then space will be
C     allocated and the text lines in BUFFER will be placed into the
C     comment area. The text lines may contain only printable ASCII
C     characters (decimal values 32 - 126).
C
C     There is NO maximum length imposed on the significant portion
C     of a text line that may be placed into the comment area of a
C     DAF. The maximum length of a line stored in the comment area
C     should be reasonable, however, so that they may be easily
C     extracted. A good maximum value for this would be 255 characters,
C     as this can easily accommodate ``screen width'' lines as well as
C     long lines which may contain some other form of information.
C
C$ Examples
C
C     Let
C
C           HANDLE   be the handle for a DAF which has been opened with
C                    write access.
C
C           N        be the number of lines of text to be added to the
C                    comment area of the binary DAF attached to HANDLE.
C
C           BUFFER   is a list of text lines to be added to the comment
C                    area of the binary DAF attached to HANDLE.
C
C     The call
C
C           CALL DAFAC ( HANDLE, N, BUFFER )
C
C     will append the first N line(s) in BUFFER to the comment area
C     of the binary DAF attached to HANDLE.
C
C$ Restrictions
C
C     1) This routine uses constants that are specific to the ASCII
C        character sequence. The results of using this routine with
C        a different character sequence are unpredictable.
C
C     2) This routine is only used to extract records on environments
C        whose characters are a single byte in size.  Updates to this
C        routine and routines in its call tree may be required to
C        properly handle other cases.
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
C-    Support Version 2.0.0, 16-NOV-2001 (FST)
C
C        Updated this routine to utilize the new handle manager
C        interfaces.
C
C-    Beta Version 1.0.0, 26-JUL-1994 (KRG)
C
C-&
 
C$ Index_Entries
C
C     add comments to a binary daf file
C     append comments to a daf file comment area
C
C-&
 
C$ Revisions
C
C-    Support Version 2.0.0, 16-NOV-2001 (FST)
C
C        The call to DAFHLU has been replaced with a call to ZZDDHHLU,
C        the handle manager interface for retrieving a logical unit.
C        DAFHLU is no longer used, since it locks the unit returned to
C        its HANDLE, tying up resources in the handle manager.
C
C-&
 
C
C     SPICELIB functions
C
      INTEGER               CPOS
      INTEGER               LASTNB
      INTEGER               NCPOS
 
      LOGICAL               FAILED
      LOGICAL               RETURN
C
C     Local parameters
C
C     Length of a DAF file internal filename.
C
      INTEGER               IFNLEN
      PARAMETER           ( IFNLEN = 60 )
C
C     Decimal value for the DAF comment area end-of-comment (EOC)
C     marker.
C
      INTEGER               INTEOC
      PARAMETER           ( INTEOC = 4 )
C
C     Decimal value for the DAF comment area end-of-line (EOL) marker.
C
      INTEGER               INTEOL
      PARAMETER           ( INTEOL = 0 )
C
C     Length of a DAF character record, in characters.
C
      INTEGER               MXCREC
      PARAMETER           ( MXCREC = 1000 )
C
C     Maximum and minimum decimal values for the printable ASCII
C     characters.
C
      INTEGER               MAXPCH
      PARAMETER           ( MAXPCH = 126 )
 
      INTEGER               MINPCH
      PARAMETER           ( MINPCH = 32 )
C
C     Local variables
C
      CHARACTER*(MXCREC)    CRECRD
      CHARACTER*(1)         EOCMRK
      CHARACTER*(1)         EOLMRK
      CHARACTER*(IFNLEN)    IFNAME
 
      INTEGER               CURPOS
      INTEGER               DAFLUN
      INTEGER               EOCPOS
      INTEGER               I
      INTEGER               J
      INTEGER               LENGTH
      INTEGER               NCHARS
      INTEGER               NCOMC
      INTEGER               NCOMR
      INTEGER               NELPOS
      INTEGER               NEWREC
      INTEGER               NOTUSD
      INTEGER               RECNO
      INTEGER               RINUSE
      INTEGER               SPACE
 
      INTEGER               IOSTAT
      INTEGER               BWARD
      INTEGER               FWARD
      INTEGER               FREE
      INTEGER               ND
      INTEGER               NI
 
      LOGICAL               EMPTY
      LOGICAL               FIRST
      LOGICAL               FOUND
C
C     Saved variables
C
      SAVE EOLMRK
      SAVE EOCMRK
      SAVE FIRST
C
C     Initial values
C
      DATA                  FIRST   / .TRUE. /
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DAFAC' )
      END IF
C
C     The lines of text in BUFFER will be ``packed'' into DAF comment
C     records: the significant portion of each comment line from BUFFER
C     will be terminated using the special character EOLMRK to indicate
C     the end of the line. When a comment record is full or all of the
C     comments have been added, the comment record will be written to
C     the comment area of the binary DAF file.
C
C     If this is the first time that this routine has been called,
C     we need to initialize the character value for the end-of-line
C     marker and the character value for the end of comments marker.
C
      IF ( FIRST ) THEN
 
         FIRST  = .FALSE.
         EOCMRK = CHAR ( INTEOC )
         EOLMRK = CHAR ( INTEOL )
 
      END IF
C
C     Verify that the DAF file attached to HANDLE is opened with write
C     access.
C
      CALL DAFSIH ( HANDLE, 'WRITE' )
 
      IF ( FAILED() ) THEN
 
         CALL CHKOUT ( 'DAFAC' )
         RETURN
 
      END IF
C
C     Convert the DAF file handle to its corresponding Fortran logical
C     unit number for reading and writing comment records.
C
      CALL ZZDDHHLU ( HANDLE, 'DAF', .FALSE., DAFLUN )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'DAFAC' )
         RETURN
      END IF
C
C     Check for a nonpositive number of lines in the buffer.
C
      IF ( N .LE. 0 ) THEN
 
         CALL SETMSG ( 'The number of comment lines to be added'  //
     .                 ' to the binary DAF file ''#'' was not'    //
     .                 ' positive: #.'                             )
         CALL ERRFNM ( '#', DAFLUN                                 )
         CALL ERRINT ( '#', N                                      )
         CALL SIGERR ( 'SPICE(INVALIDARGUMENT)'                    )
         CALL CHKOUT ( 'DAFAC'                                     )
         RETURN
 
      END IF
C
C     Count the number of characters in the buffer ignoring trailing
C     blanks on nonblank lines and blank lines. The count will be
C     modified to include the contribution of blank lines later. This
C     count is used to determine the number of character records to be
C     added to the binary DAF file attached to HANDLE.
C
      NCHARS = 0
 
      DO I = 1, N
C
C        Get the length of the significant portion of a comment line.
C
         LENGTH = LASTNB ( BUFFER(I) )
C
C        Scan the comment line for non printing characters.
C
         DO J = 1, LENGTH
C
C           Check to see that the characters in the buffer are all
C           printing ASCII characters. The bounds for printing ASCII
C           characters are given by MINPCH and MAXPCH, which are
C           defined in the $ Local Parameters section of the header.
C
            IF ( ( ICHAR ( BUFFER(I)(J:J) ) .GT. MAXPCH ) .OR.
     .           ( ICHAR ( BUFFER(I)(J:J) ) .LT. MINPCH )      ) THEN
 
                  CALL SETMSG ( 'A nonprinting character was'      //
     .                          ' encountered in the comment'      //
     .                          ' buffer. Value: #'                 )
                  CALL ERRINT ( '#', ICHAR( BUFFER(I)(J:J) )        )
                  CALL SIGERR ( 'SPICE(ILLEGALCHARACTER)'           )
                  CALL CHKOUT ( 'DAFAC'                             )
                  RETURN
 
            END IF
 
         END DO
C
C        Increment the number of characters by the length of the
C        significant portion of the current line in the buffer.
C
         NCHARS = NCHARS + LENGTH
 
      END DO
C
C     We need to include the number of end of line markers in the
C     number of characters, so add the number of comment lines to
C     be added, N, to the number of characters, NCHARS. This is where
C     the contribution of any blank lines gets added to the character
C     count. We also need to have space for the end of comments marker.
C
      NCHARS = NCHARS + N + 1
C
C     Get the current number of comment records and comment characters
C     from the DAF file attached to HANDLE. We will also get back some
C     extra stuff that we do not use.
C
      CALL DAFRFR ( HANDLE, ND, NI, IFNAME, FWARD, BWARD, FREE )
 
      IF ( FAILED() ) THEN
 
         CALL CHKOUT ( 'DAFAC' )
         RETURN
 
      END IF
C
C     Compute the number of comment records and the number of comment
C     characters. In order to perform these calculations, we assume
C     that we have a valid comment area in the DAF file attached to
C     HANDLE.
C
      NCOMR = FWARD - 2
 
      IF ( NCOMR .GT. 0 ) THEN
C
C        The starting record number is the number of comment records + 1
C        where the 1 skips the file record.
C
         EMPTY  = .TRUE.
         FOUND  = .FALSE.
         NOTUSD = 0
 
         DO WHILE ( ( NCOMR .GT. 0 ) .AND. ( .NOT. FOUND ) .AND. EMPTY )
 
            RECNO = NCOMR + 1
 
            READ ( DAFLUN, REC=RECNO, IOSTAT=IOSTAT ) CRECRD
 
            IF ( IOSTAT .NE. 0 ) THEN
 
               CALL SETMSG ( 'Error reading comment area of binary' //
     .                       ' file named ''#''.  IOSTAT = #.'       )
               CALL ERRFNM ( '#', DAFLUN                             )
               CALL ERRINT ( '#', IOSTAT                             )
               CALL SIGERR ( 'SPICE(FILEREADFAILED)'                 )
               CALL CHKOUT ( 'DAFAC'                                 )
               RETURN
 
            END IF
C
C           Scan the comment record looking for the end of comments
C           marker.
C
            EOCPOS = CPOS ( CRECRD(1:MXCREC), EOCMRK, 1 )
 
            IF ( EOCPOS .GT. 0 ) THEN
 
               FOUND = .TRUE.
 
            ELSE
 
               NELPOS = NCPOS ( CRECRD(1:MXCREC), EOLMRK, 1 )
 
               IF ( NELPOS .NE. 0 ) THEN
 
                  EMPTY = .FALSE.
 
               ELSE
 
                  NCOMR  = NCOMR  - 1
                  NOTUSD = NOTUSD + 1
 
               END IF
 
            END IF
 
         END DO
C
C        If we do not find the end of comments marker and the comment
C        area is not empty, then it is an error.
C
         IF ( ( .NOT. FOUND ) .AND. ( .NOT. EMPTY ) ) THEN
 
            CALL SETMSG ( 'The comment area in the DAF file'   //
     .                    ' ''#'' may be damaged. The end of'  //
     .                    ' the comments could not be found.'   )
            CALL ERRFNM ( '#', DAFLUN                           )
            CALL SIGERR ( 'SPICE(BADCOMMENTAREA)'               )
            CALL CHKOUT ( 'DAFAC'                               )
            RETURN
 
         ELSE IF ( FOUND ) THEN
 
            NCOMC = MXCREC * ( NCOMR - 1 ) + EOCPOS - 1
 
         ELSE IF ( EMPTY ) THEN
 
            NCOMC = 0
 
         END IF
 
      ELSE
 
         NCOMC  = 0
         NOTUSD = 0
 
      END IF
C
C     Determine the amount of free space in the comment area. If
C     there are some comment records allocated, the space available
C     is the number of comment records allocated times the length of
C     a comment record, minus the number of comment characters already
C     used. Otherwise, the space available is zero.
C
      IF ( NCOMR + NOTUSD .GT. 0 ) THEN
 
         SPACE = NOTUSD * MXCREC + NCOMR * MXCREC - NCOMC
 
      ELSE
 
         SPACE = 0
 
      END IF
C
C     Determine the number of new comment records which are necessary
C     to store all of the comments from the buffer.
C
      IF ( NCHARS .GT. SPACE ) THEN
C
C        If there are more characters to store than available space
C        we need at least one new record.
C
         NEWREC = 1  +  (  ( NCHARS - SPACE )  - 1  ) / MXCREC
 
      ELSE
C
C        Otherwise, we do not need any new records.
C
         NEWREC = 0
 
      END IF
C
C     Now add the necessary number of comment records to the file,
C     if we need to add any.
C
      IF ( NEWREC .GT. 0 ) THEN
 
         CALL DAFARR ( HANDLE, NEWREC )
 
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'DAFAC' )
            RETURN
         END IF
 
      END IF
C
C     At this point, we know that we have enough space to write all of
C     the comments in BUFFER to the comment area. Either there was
C     enough space already there, or we calculated how many new comment
C     records were needed, and we added them to the file. So, now we
C     begin ``packing'' the comments into DAF comment records and
C     writing them to the file.
C
C     We begin initializing the appropriate variables.
C
      IF ( NCOMC .EQ. 0 ) THEN
C
C        If there are no comments in the comment area, then we need
C        to skip the file record. The first available comment record
C        is therecord immediately after the file record, so we set
C        RECNO accordingly. We also initialize the current position in
C        the comment record, and the comment record itself.
C
         RECNO  = 2
         CURPOS = 1
         CRECRD = ' '
 
      ELSE
C
C        If there are comments in the comment area, then we need to
C        skip the file record and any comment records which have been
C        filled. The first comment record with space available is the
C        record immediately following the last completely filled
C        comment record. So calculate the number of comment records
C        in use, and set RECNO appropriately. Finally calculate the
C        initial position.
C
         RINUSE = 1 + NCOMC / MXCREC
         RECNO  = 1 + RINUSE
         CURPOS = NCOMC - MXCREC * ( RINUSE - 1 ) + 1
 
      END IF
C
C     Begin ``packing'' the comments from the input buffer into the
C     comment records, writing the comment records to the file as they
C     become filled.
C
      DO I = 1, N
C
C        Get the length of the significant portion of comment line I.
C
         LENGTH = LASTNB ( BUFFER(I) )
C
C        Process the comment line.
C
         DO J = 1, LENGTH
C
C           If we have filled the comment record while processing
C           comment line BUFFER(I), write out the comment record,
C           increment the record number, RECNO, and reset the values
C           of the current position and the comment record.
C
            IF ( CURPOS .GT. MXCREC ) THEN
 
               WRITE ( DAFLUN, REC=RECNO, IOSTAT=IOSTAT ) CRECRD
 
               IF ( IOSTAT .NE. 0 ) THEN
 
                  CALL SETMSG ( 'Error writing to record # of the'  //
     .                          ' binary file named ''#''. IOSTAT'  //
     .                          ' = #.'                              )
                  CALL ERRINT ( '#', RECNO                           )
                  CALL ERRFNM ( '#', DAFLUN                          )
                  CALL ERRINT ( '#', IOSTAT                          )
                  CALL SIGERR ( 'SPICE(FILEWRITEFAILED)'             )
                  CALL CHKOUT ( 'DAFAC'                              )
                  RETURN
 
               END IF
 
               RECNO  = RECNO + 1
               CURPOS = 1
               CRECRD = ' '
 
            END IF
 
            CRECRD(CURPOS:CURPOS) = BUFFER(I)(J:J)
            CURPOS                = CURPOS + 1
 
         END DO
C
C        Check to see if we happened to exactly fill the comment record
C        when we finished processing comment line BUFFER(I). If we
C        did, CURPOS will be 1 greater than MXCREC, and we will need
C        to write the comment record to the file, increment the record
C        number, RECNO, and reset the values of the current position
C        and the comment record.
C
         IF ( CURPOS .GT. MXCREC ) THEN
 
               WRITE ( DAFLUN, REC=RECNO, IOSTAT=IOSTAT ) CRECRD
 
               IF ( IOSTAT .NE. 0 ) THEN
 
                  CALL SETMSG ( 'Error writing to record # of the'  //
     .                          ' binary file named ''#''. IOSTAT'  //
     .                          ' = #.'                              )
                  CALL ERRINT ( '#', RECNO                           )
                  CALL ERRFNM ( '#', DAFLUN                          )
                  CALL ERRINT ( '#', IOSTAT                          )
                  CALL SIGERR ( 'SPICE(FILEWRITEFAILED)'             )
                  CALL CHKOUT ( 'DAFAC'                              )
                  RETURN
 
               END IF
 
            RECNO  = RECNO + 1
            CURPOS = 1
            CRECRD = ' '
 
         END IF
C
C        Append the end-of-line marker to the comment line that we just
C        placed into the comment record.
C
         CRECRD(CURPOS:CURPOS) = EOLMRK
         CURPOS                = CURPOS + 1
 
      END DO
C
C     We have now finished processing all of the comment lines in
C     BUFFER, so we need write the current record to the file. This
C     record will always contain something, so we always need to write
C     it.
C
      IF ( CURPOS .GT. MXCREC ) THEN
C
C        If we have completely filled the comment record, the last
C        character of the last line n the buffer coincides with the
C        last character in the comment record, then we need to write
C        the record and get set up to add the end of comments mark on
C        the next record.
C
         WRITE ( DAFLUN, REC=RECNO, IOSTAT=IOSTAT ) CRECRD
 
         IF ( IOSTAT .NE. 0 ) THEN
 
            CALL SETMSG ( 'Error writing to record # of the'    //
     .                    ' binary file named ''#''. IOSTAT'    //
     .                    ' = #.'                                )
            CALL ERRINT ( '#', RECNO                             )
            CALL ERRFNM ( '#', DAFLUN                            )
            CALL ERRINT ( '#', IOSTAT                            )
            CALL SIGERR ( 'SPICE(FILEWRITEFAILED)'               )
            CALL CHKOUT ( 'DAFAC'                                )
            RETURN
 
         END IF
 
         RECNO  = RECNO + 1
         CURPOS = 1
         CRECRD = ' '
 
      END IF
C
C     Add the end of comments mark to the final comment record and
C     write it to the file.
C
      CRECRD(CURPOS:CURPOS) = EOCMRK
 
      WRITE ( DAFLUN, REC=RECNO, IOSTAT=IOSTAT ) CRECRD
 
      IF ( IOSTAT .NE. 0 ) THEN
 
         CALL SETMSG ( 'Error writing to record # of the'    //
     .                 ' binary file named ''#''. IOSTAT'    //
     .                 ' = #.'                                )
         CALL ERRINT ( '#', RECNO                             )
         CALL ERRFNM ( '#', DAFLUN                            )
         CALL ERRINT ( '#', IOSTAT                            )
         CALL SIGERR ( 'SPICE(FILEWRITEFAILED)'               )
         CALL CHKOUT ( 'DAFAC'                                )
         RETURN
 
      END IF
C
C     Check out and leave DAFAC.
C
      CALL CHKOUT ( 'DAFAC' )
      RETURN
      END
