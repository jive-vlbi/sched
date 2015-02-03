C$Procedure     DASAC ( DAS add comments )
 
      SUBROUTINE DASAC ( HANDLE, N, BUFFER )
 
C$ Abstract
C
C     Add comments from a buffer of character strings to the comment
C     area of a binary DAS file, appending them to any comments which
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
C     DAS
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
C      HANDLE    I    DAS handle of a file opened with write access.
C      N         I    Number of comments to put into the comment area.
C      BUFFER    I    Buffer of lines to be put into the comment area.
C
C$ Detailed_Input
C
C     HANDLE   The file handle of a binary DAS file which has been
C              opened with write access.
C
C     N        The number of comments in BUFFER that are to be
C              added to the comment area of the binary DAS file
C              attached to HANDLE.
C
C     BUFFER   A buffer containing comments which are to be added
C              to the comment area of the binary DAS file attached
C              to HANDLE.
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
C     3)   If the binary DAS file attached to HANDLE is not open with
C          write access an error will be signalled by a routine called
C          by this routine.
C
C$ Files
C
C     See argument HANDLE in $ Detailed_Input.
C
C$ Particulars
C
C     Binary DAS files contain a data area which is reserved for storing
C     annotations or descriptive textual information about the data
C     contained in a file. This area is referred to as the ``comment
C     area'' of the file. The comment area of a DAS file is a line
C     oriented medium for storing textual information. The comment
C     area preserves any leading or embedded white space in the line(s)
C     of text which are stored so that the appearance of the
C     information will be unchanged when it is retrieved (extracted) at
C     some other time. Trailing blanks, however, are NOT preserved,
C     due to the way that character strings are represented in
C     standard Fortran 77.
C
C     This routine will take a buffer of text lines and add (append)
C     them to the comment area of a binary DAS file. If there are no
C     comments in the comment area of the file, then space will be
C     allocated and the text lines in BUFFER will then placed into the
C     comment area. The text lines may contain only printable ASCII
C     characters (decimal values 32 - 126).
C
C     There is NO maximum length imposed on the significant portion
C     of a text line that may be placed into the comment area of a
C     DAS file. The maximum length of a line stored in the comment
C     area should be reasonable, however, so that they may be easily
C     extracted. A good value for this would be 255 characters, as
C     this can easily accommodate ``screen width'' lines as well as
C     long lines which may contain some other form of information.
C
C$ Examples
C
C     Let
C
C           HANDLE   be the handle for a DAS file which has been opened
C                    with write access.
C
C           N        be the number of lines of text to be added to the
C                    comment area of the binary DAS file attached to
C                    HANDLE.
C
C           BUFFER   is a list of text lines to be added to the comment
C                    area of the binary DAS file attached to HANDLE.
C
C     The call
C
C           CALL DASAC ( HANDLE, N, BUFFER )
C
C     will append the first N line(s) in BUFFER to the comment area
C     of the binary DAS file attached to HANDLE.
C
C$ Restrictions
C
C     1) This routine uses constants that are specific to the ASCII
C        character sequence. The results of using this routine with
C        a different character sequence are unpredictable.
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
C-    Beta Version 1.0.1, 12-MAY-1994 (KRG)
C
C        Fixed a typo in the $ Particulars section.
C
C-    Beta Version 1.0.0, 23-NOV-1992 (KRG)
C
C-&
 
C$ Index_Entries
C
C      add comments to a binary das file
C      append comments to a das file comment area
C
C-&
 
C$ Revisions
C
C-    Beta Version 1.0.1, 12-MAY-1994 (KRG)
C
C        Fixed a typo in the $ Particulars section.
C
C-    Beta Version 1.0.0, 23-NOV-1992 (KRG)
C
C-&
 
 
 
C
C     SPICELIB functions
C
      INTEGER               LASTNB
 
      LOGICAL               FAILED
      LOGICAL               RETURN
C
C     Local parameters
C
C     Length of a DAS character record, in characters.
C
      INTEGER               MXCREC
      PARAMETER           ( MXCREC = 1024 )
C
C     Maximum and minimum decimal values for the printable ASCII
C     characters.
C
      INTEGER               MAXPCH
      PARAMETER           ( MAXPCH = 126 )
 
      INTEGER               MINPCH
      PARAMETER           ( MINPCH = 32 )
C
C     Decimal value for the DAS comment area end-of-line (EOL) marker.
C
      INTEGER               INTEOL
      PARAMETER           ( INTEOL = 0 )
C
C     Length of a DAS file ID word.
C
      INTEGER               IDWLEN
      PARAMETER           ( IDWLEN = 8 )
C
C     Length of a DAS file internal filename.
C
      INTEGER               IFNLEN
      PARAMETER           ( IFNLEN = 60 )
C
C     Local variables
C
      CHARACTER*(MXCREC)    CRECRD
      CHARACTER*(1)         EOLMRK
      CHARACTER*(IDWLEN)    IDWORD
      CHARACTER*(IFNLEN)    IFNAME
 
      INTEGER               CURPOS
      INTEGER               DASLUN
      INTEGER               I
      INTEGER               J
      INTEGER               LENGTH
      INTEGER               NCHARS
      INTEGER               NCOMC
      INTEGER               NCOMR
      INTEGER               NEWREC
      INTEGER               NRESVC
      INTEGER               NRESVR
      INTEGER               RECNO
      INTEGER               RINUSE
      INTEGER               SPACE
 
      LOGICAL               FIRST
C
C     Saved variables
C
      SAVE EOLMRK
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
         CALL CHKIN ( 'DASAC' )
      END IF
C
C     The lines of text in BUFFER will be ``packed'' into DAS comment
C     records: the significant portion of each comment line from BUFFER
C     will be terminated by the special character EOLMRK to indicate the
C     end of the line. When a comment record is full or all of the
C     comments have been added to the file, the comment record will be
C     written to the comment area of the binary DAS file.
C
C     If this is the first time that this routine has been called,
C     we need to initialize the character value for the end-of-line
C     marker.
C
      IF ( FIRST ) THEN
 
         FIRST  = .FALSE.
         EOLMRK = CHAR ( INTEOL )
 
      END IF
C
C     Verify that the DAS file attached to HANDLE is opened with write
C     access.
C
      CALL DASSIH ( HANDLE, 'WRITE' )
 
      IF ( FAILED() ) THEN
 
         CALL CHKOUT ( 'DASAC' )
         RETURN
 
      END IF
C
C     Convert the DAS file handle to its corresponding Fortran logical
C     unit number for reading and writing comment records.
C
      CALL DASHLU ( HANDLE, DASLUN )
 
      IF ( FAILED() ) THEN
 
         CALL CHKOUT ( 'DASAC' )
         RETURN
 
      END IF
C
C     Check for a nonpositive number of lines in the buffer.
C
      IF ( N .LE. 0 ) THEN
 
         CALL SETMSG ( 'The number of comment lines to be added to'  //
     .                 ' the binary DAS file # was not positive: #.'  )
         CALL ERRFNM ( '#', DASLUN                                    )
         CALL ERRINT ( '#', N                                         )
         CALL SIGERR ( 'SPICE(INVALIDARGUMENT)'                       )
         CALL CHKOUT ( 'DASAC'                                        )
         RETURN
 
      END IF
C
C     Count the number of characters in the buffer ignoring trailing
C     blanks on nonblank lines and blank lines. The count will be
C     modified to include the contribution of blank lines later. This
C     count is used to determine the number of character records to be
C     added to the binary DAS file attached to HANDLE.
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
C           characters are given by MAXPCH and MINPCH, which are
C           defined in the $ Local Parameters section of the header.
C
            IF ( ( ICHAR ( BUFFER(I)(J:J) ) .GT. MAXPCH ) .OR.
     .           ( ICHAR ( BUFFER(I)(J:J) ) .LT. MINPCH )      ) THEN
 
                  CALL SETMSG ( 'A nonprinting character was'      //
     .                          ' encountered in the comment'      //
     .                          ' buffer. Value: #'                 )
                  CALL ERRINT ( '#', ICHAR( BUFFER(I)(J:J) )        )
                  CALL SIGERR ( 'SPICE(ILLEGALCHARACTER)'           )
                  CALL CHKOUT ( 'DASAC'                             )
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
C     count.
C
      NCHARS = NCHARS + N
C
C     Get the current number of comment records and comment characters
C     from the DAS file attached to HANDLE. We will also get back some
C     extra stuff that we do not use.
C
      CALL DASRFR ( HANDLE,
     .              IDWORD, IFNAME,
     .              NRESVR, NRESVC,
     .              NCOMR,  NCOMC   )
 
      IF ( FAILED() ) THEN
 
         CALL CHKOUT ( 'DASAC' )
         RETURN
 
      END IF
C
C     Determine the amount of free space in the comment area. If
C     there are some comment records allocated, the space available
C     is the number of comment records allocated times the length of
C     a comment record, minus the number of comment characters already
C     used. Otherwise, the space available is zero.
C
      IF ( NCOMR .GT. 0 ) THEN
 
         SPACE = NCOMR * MXCREC - NCOMC
 
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
 
         CALL DASACR ( HANDLE, NEWREC )
 
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'DASAC' )
            RETURN
         END IF
 
C
C        Update the value for the number of comment records to include
C        those that were just added. We need this value when we write
C        the file record at the end of the routine to update the number
C        comment characters, NCOMC.
C
         NCOMR = NCOMR + NEWREC
 
      END IF
C
C     At this point, we know that we have enough space to write all of
C     the comments in BUFFER to the comment area. Either there was
C     enough space already there, or we figured out how many new comment
C     records were needed, and we added them to the file. So, now we
C     begin ``packing'' the comments into DAS character records and
C     writing them to the file.
C
C     We begin by reading the last comment record if there is one.
C     Otherwise we just initialize the appropriate variables.
C
      IF ( NCOMC .EQ. 0 ) THEN
C
C        If there are no comments in the comment area, then we need to
C        skip the file record and the reserved records, if any. The
C        first available comment record is the record immediately
C        after the last reserved record, so we set RECNO accordingly.
C        We also initialize the current position in the comment record,
C        and the comment record itself.
C
         RECNO  = 1 + NRESVR + 1
         CURPOS = 1
         CRECRD = ' '
 
      ELSE
C
C        If there are comments in the comment area, then we need to skip
C        the file record, the reserved records, if any, and any comment
C        records which have been filled. The first comment record
C        with space available is the record immediately following the
C        last completely filled comment record. So calculate the number
C        of comment records in use, and set RECNO appropriately. Then
C        calculate the initial position and read in the comment record.
C
         RINUSE = 1 + NCOMC / MXCREC
         RECNO  = 1 + NRESVR + RINUSE
         CURPOS = NCOMC - MXCREC * ( RINUSE - 1 ) + 1
 
         CALL DASIOC ( 'READ', DASLUN, RECNO, CRECRD )
 
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'DASAC' )
            RETURN
         END IF
 
      END IF
C
C     Begin ``packing'' the comments from the input buffer into the
C     comment records, writing the comment records to the DAS file
C     as they become filled.
C
      DO I = 1, N
C
C        Get the length of the significant portion of a comment line.
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
 
               CALL DASIOC ( 'WRITE', DASLUN, RECNO, CRECRD )
 
               IF ( FAILED() ) THEN
                  CALL CHKOUT ( 'DASAC' )
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
 
            CALL DASIOC ( 'WRITE', DASLUN, RECNO, CRECRD )
 
            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'DASAC' )
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
      CALL DASIOC ( 'WRITE', DASLUN, RECNO, CRECRD )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'DASAC' )
         RETURN
      END IF
 
C
C     And finally, we need to update the number of comment characters
C     in the file record by adding NCHARS, and writing the file record.
C
      NCOMC = NCOMC + NCHARS
 
      CALL DASWFR ( HANDLE,
     .              IDWORD, IFNAME,
     .              NRESVR, NRESVC,
     .              NCOMR,  NCOMC  )
C
C     Check out and leave DASAC. A test of FAILED should be done by
C     the calling routine to catch an error that may occur during
C     the call to DASWFR.
C
      CALL CHKOUT ( 'DASAC' )
      RETURN
      END
