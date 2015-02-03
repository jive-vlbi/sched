 
C$Procedure  DASEC  ( DAS extract comments )
 
      SUBROUTINE DASEC ( HANDLE, BUFSIZ, N, BUFFER, DONE )
 
C$ Abstract
C
C     Extract comments from the comment area of a binary DAS file.
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
      INTEGER               BUFSIZ
      INTEGER               N
      CHARACTER*(*)         BUFFER(*)
      LOGICAL               DONE
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C      HANDLE    I   Handle of binary DAS file open with read access.
C      BUFSIZ    I   Maximum size, in lines, of BUFFER.
C      N         O   Number of comments extracted from the DAS file.
C      BUFFER    O   Buffer in which extracted comments are placed.
C      DONE      O   Indicates whether all comments have been extracted.
C
C$ Detailed_Input
C
C     HANDLE   The file handle of a binary DAS file which has been
C              opened with read access.
C
C     BUFSIZ   The maximum number of comments that may be placed into
C              BUFFER. This would typically be the declared array size
C              for the Fortran character string array passed into this
C              routine.
C
C$ Detailed_Output
C
C     N        The number of comment lines extracted from the comment
C              area of the binary DAS file attached to HANDLE. This
C              number will be <= BUFSIZ on output. If N = BUFSIZ and
C              DONE <> .TRUE. then there are more comments left to to
C              extract. If N = 0, then DONE = .TRUE., i.e., there were
C              no comments in the comment area. If there are comments
C              in the comment area, or comments remaining after the
C              extraction process has begun, N > 0, always.
C
C     BUFFER   A list of at most BUFSIZ comments which have been
C              extracted from the comment area of the binary DAS
C              file attached to HANDLE.
C
C     DONE     A logical flag indicating whether or not all of the
C              comment lines from the comment area of the DAS file have
C              been read. This variable has the value .TRUE. after the
C              last comment line has been read. It will have the value
C              .FALSE. otherwise.
C
C              If there are no comments in the comment area, this
C              variable will have the value .TRUE., and N = 0.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)   If the size of the output line buffer is is not positive,
C          the error SPICE(INVALIDARGUMENT) will be signalled.
C
C     3)   If a comment line in a DAS file is longer than the length
C          of a character string array element of BUFFER, the error
C          SPICE(COMMENTTOOLONG) will be signalled.
C
C     3)   If there is a mismatch between the number of comment
C          characters found and the number of comment characters
C          expected, the error SPICE(BADDASCOMMENTAREA) will be
C          signalled.
C
C     4)   If the binary DAS file attached to HANDLE is not open for
C          reading, an error will be signalled by a routine called by
C          this routine.
C
C$ Files
C
C     See argument HANDLE in $ Detailed_Input.
C
C$ Particulars
C
C     Binary DAS files contain an area which is reserved for storing
C     annotations or descriptive textual information describing the data
C     contained in a file. This area is referred to as the ``comment
C     area'' of the file. The comment area of a DAS file is a line
C     oriented medium for storing textual information. The comment
C     area preserves any leading or embedded white space in the line(s)
C     of text which are stored, so that the appearance of the of
C     information will be unchanged when it is retrieved (extracted) at
C     some other time. Trailing blanks, however, are NOT preserved,
C     due to the way that character strings are represented in
C     standard Fortran 77.
C
C     This routine will read the comments from the comment area of
C     a binary DAS file, placing them into a line buffer. If the line
C     buffer is not large enough to hold the entire comment area,
C     the portion read will be returned to the caller, and the DONE
C     flag will be set to .FALSE.. This allows the comment area to be
C     read in ``chunks,'' a buffer at a time. After all of the comment
C     lines have been read, the DONE flag will be set to .TRUE..
C
C     This routine can be used to ``simultaneously'' extract comments
C     from the comment areas of multiple binary DAS files. See Example
C     2 in the $ Examples section.
C
C$ Examples
C
C     Example 1
C     ---------
C
C     The following example will extract the entire comment area of a
C     binary DAS file attached to HANDLE, displaying the comments on
C     the terminal screen.
C
C     Let
C
C        BUFFER  have the following declaration:
C
C           CHARACTER*(80)  BUFFER(25)
C
C        HANDLE  be the handle of an open binary DAS file.
C
C     then
C
C        BUFSIZ = 25
C        DONE   = .FALSE.
C
C        DO WHILE ( .NOT. DONE )
C
C           CALL DASEC( HANDLE, BUFSIZ, N, BUFFER, DONE )
C
C           DO I = 1, N
C
C              WRITE (*,*) BUFFER(I)
C
C           END DO
C
C        END DO
C
C     Example 2
C     ---------
C
C     The following example demonstrates the use of this routine to
C     simultaneously read the comment areas of multiple DAS files.
C     For each file, the comments will be displayed on the screen as
C     they are extracted.
C
C     Let
C
C        BUFFER  have the following declaration:
C
C           CHARACTER*(80)  BUFFER(25)
C
C        NUMFIL     be the number of binary DAS files that are to have
C                   their comment areas displayed.
C
C        DASNAM(I)  Be a list of filenames for the DAS files which are
C                   to have their comment areas displayed.
C
C        HANDLE(I)  be a list of handles for the DAS files which are
C                   to have their comment areas displayed.
C
C        DONE(I)    be a list of logical flags indicating whether
C                   we are done extracting the comment area from the
C                   DAS file attached to HANDLE(I)
C
C     then
C
C            BUFSIZ = 25
C
C            DO I = 1, NUMFIL
C
C               DONE(I)   = .FALSE.
C               HANDLE(I) = 0
C
C            END DO
C     C
C     C      Open the DAS files.
C     C
C            DO I = 1, NUMFIL
C
C               CALL DASOPR ( DASNAM(I), HANDLE(I) )
C
C            END DO
C     C
C     C      While there are still some comments left to read in at
C     C      least one of the files, read them and display them.
C     C
C            DO WHILE ( .NOT. ALLTRU( DONE, NUMFIL ) )
C
C               DO I = 1, NUMFIL
C
C                  IF ( .NOT. DONE(I) ) THEN
C
C                     WRITE (*,*)
C                     WRITE (*,*) 'File: ', DASNAM(I)(:RTRIM(DASNAM(I)))
C                     WRITE (*,*)
C                     N = 0
C
C                     CALL DASEC ( HANDLE(I),
C           .                      BUFSIZ,
C           .                      N,
C           .                      BUFFER,
C           .                      DONE(I) )
C
C                     DO J = 1, N
C
C                        WRITE (*,*) BUFFER(J)(:RTRIM(BUFFER(J)))
C
C                     END DO
C
C                  END IF
C
C               END DO
C
C            END DO
C
C$ Restrictions
C
C     1) The comment area may consist only of printing ASCII characters,
C        decimal values 32 - 126. See the MAXPCH and MINPCH parameters
C        defined in the $ Local Parameters section.
C
C     2) There is NO maximum length imposed on the significant portion
C        of a text line that may be placed into the comment area of a
C        DAS file. The maximum length of a line stored in the comment
C        area should be kept reasonable, so that they may be easily
C        extracted. A good value for this would be 255 characters, as
C        this can easily accomodate ``screen width'' lines as well as
C        long lines which may contain some other form of information.
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
C-    SPICELIB Version 1.3.0, 18-JUN-1999 (WLT)
C
C        Changed name used in CHKOUT to be consistent with the CHKIN
C        value.
C
C-    SPICELIB Version 1.2.0, 04-AUG-1994 (KRG)
C
C        Rearranged some of the code to avoid always reading the file
C        record. Now we look for the input HANDLE in the file table
C        first, and only read the file record if we do not find it. Also
C        added a new array to be saved: FILCNT. This is the number of
C        comment characters in a file; we save it now rather than
C        reading it every time.
C
C        Fixed a bug. If the Fortran character string array elements
C        have exactly the same length as a comment in the comment area,
C        this routine would halt rather unexpectedly from a memory over
C        run.
C
C-    SPICELIB Version 1.1.0, 22-NOV-1993 (KRG)
C
C        Changed the value of the parameter FTSIZE from 20 to 21. This
C        change makes the value of FTSIZE in DASEC compatible with the
C        value in DASFM. See DASFM for a discussion of the reasons for
C        the increase in the value.
C
C-    SPICELIB Version 1.0.0, 23-NOV-1992 (KRG)
C
C-&
 
C$ Index_Entries
C
C      extract comments from a das file
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 1.2.0, 04-AUG-1994 (KRG)
C
C        Rearranged some of the code to avoid always reading the file
C        record. Now we look for the input HANDLE in the file table
C        first, and only read the file record if we do not find it. Also
C        added a new array to be saved: FILCNT. This is the number of
C        comment characters in a file; we save it now rather than
C        reading it every time.
C
C        Fixed a bug. If the Fortran character string array elements
C        have exactly the same length as a comment in the comment area,
C        this routine would halt rather unexpectedly from a memory over
C        run. This occurred when attempting to clear, i.e., blank pad,
C        the portion of a character string element that extended beyond
C        the text in a comment line. A test has been added to verify
C        that blank padding can be performed.
C
C-    SPICELIB Version 1.1.0, 22-NOV-1993 (KRG)
C
C        Changed the value of the parameter FTSIZE from 20 to 21. This
C        change makes the value of FTSIZE in DASEC compatible with the
C        value in DASFM. See DASFM for a discussion of the reasons for
C        the increase in the value.
C
C-    SPICELIB Version 1.0.0, 23-NOV-1992 (KRG)
C
C-&
 
C
C     SPICELIB functions
C
      INTEGER               ISRCHI
 
      LOGICAL               FAILED
      LOGICAL               RETURN
C
C     Local parameters
C
C     The maximum number of DAS files that may be open simultaneously.
C
      INTEGER               FTSIZE
      PARAMETER           ( FTSIZE = 21 )
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
C     Maximum length of a filename.
C
      INTEGER               FNMLEN
      PARAMETER           ( FNMLEN = 128 )
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
      CHARACTER*(1)         CH
      CHARACTER*(MXCREC)    CRECRD
      CHARACTER*(IDWLEN)    IDWORD
      CHARACTER*(IFNLEN)    IFNAME
 
      INTEGER               CURPOS
      INTEGER               DASLUN
      INTEGER               I
      INTEGER               INDEX
      INTEGER               J
      INTEGER               K
      INTEGER               LINLEN
      INTEGER               NCHARS
      INTEGER               NCOMC
      INTEGER               NCOMR
      INTEGER               NRESVC
      INTEGER               NRESVR
      INTEGER               NUMCOM
      INTEGER               RECNO
C
C     The file table declarations for keeping track of which files
C     are currently in the process of having comments extracted.
C
      INTEGER               FILCNT(FTSIZE)
      INTEGER               FILCHR(FTSIZE)
      INTEGER               FILHAN(FTSIZE)
      INTEGER               LSTHAN
      INTEGER               LSTPOS(FTSIZE)
      INTEGER               LSTREC(FTSIZE)
      INTEGER               NFILES
 
      LOGICAL               EOL
      LOGICAL               FIRST
C
C     Saved variables
C
      SAVE CRECRD
C
C     Save all of the file table information.
C
      SAVE FILCNT
      SAVE FILCHR
      SAVE FILHAN
      SAVE LSTHAN
      SAVE LSTPOS
      SAVE LSTREC
      SAVE NFILES
 
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
         CALL CHKIN ( 'DASEC' )
      END IF
C
C     If this is the first time that this routine has been called,
C     we need to initialize the character value of the end-of-line
C     marker, and the file table variables.
C
      IF ( FIRST ) THEN
 
         FIRST  = .FALSE.
         NFILES = 0
         LSTHAN = -1
 
         DO I = 1, FTSIZE
 
            FILCNT(I) = 0
            FILCHR(I) = 0
            FILHAN(I) = 0
            LSTREC(I) = 0
            LSTPOS(I) = 0
 
         END DO
 
      END IF
C
C     Verify that the DAS file attached to HANDLE is opened for reading
C     by calling the routine to signal an invalid access mode on a
C     handle.
C
      CALL DASSIH ( HANDLE, 'READ' )
 
      IF ( FAILED() ) THEN
 
         CALL CHKOUT ( 'DASEC' )
         RETURN
 
      END IF
C
C     Check for a nonpositive BUFFER size.
C
      IF ( BUFSIZ .LE. 0 ) THEN
 
         CALL SETMSG ( 'The output buffer size was not positive: #.' )
         CALL ERRINT ( '#', BUFSIZ                                   )
         CALL SIGERR ( 'SPICE(INVALIDARGUMENT)'                      )
         CALL CHKOUT ( 'DASEC'                                       )
         RETURN
 
      END IF
C
C     Convert the DAS file handle to its corresponding Fortran logical
C     unit number for reading the comment records.
C
      CALL DASHLU ( HANDLE, DASLUN )
 
      IF ( FAILED() ) THEN
 
         CALL CHKOUT ( 'DASEC' )
         RETURN
 
      END IF
C
C     Get the length of a single character string in the buffer.
C
      LINLEN = LEN ( BUFFER(1) )
C
C     If we have extracted comments from at least one file and we
C     didn't finish, get the index for that file in the file table.
C
      IF ( NFILES .GT. 0 ) THEN
 
         INDEX = ISRCHI ( HANDLE, NFILES, FILHAN )
 
      ELSE
 
         INDEX = 0
 
      END IF
C
C     Check to see if we found HANDLE in the file handle table. If
C     we did, INDEX will be > 0.
C
      IF ( INDEX .GT. 0 ) THEN
C
C        Set the record number and the starting position accordingly,
C        i.e., where we left off when we last read from that file.
C
         RECNO  = LSTREC(INDEX)
         CURPOS = LSTPOS(INDEX)
         NCHARS = FILCHR(INDEX)
         NCOMC  = FILCNT(INDEX)
 
      ELSE
C
C        We have not yet read any comments from this file, so start at
C        the start. To get to the first comment record, we need to skip
C        the file record and any reserved records that are in the file.
C        The first comment record immediately follows the last reserved
C        record.
C
C        Get the current number of comment records and comment
C        characters from the DAS file attached to HANDLE. We will also
C        get back some extra stuff that we do not use.
C
         CALL DASRFR ( HANDLE,
     .                 IDWORD, IFNAME,
     .                 NRESVR, NRESVC,
     .                 NCOMR,  NCOMC   )
 
         IF ( FAILED() ) THEN
 
            CALL CHKOUT ( 'DASEC' )
            RETURN
 
         END IF
C
C        If the number of comment characters, NCOMC, is equal to zero,
C        then we have no comments to read, so set the number of comments
C        to zero, set DONE to .TRUE., check out,  and return.
C
         IF ( NCOMC .EQ. 0 ) THEN
 
            N    = 0
            DONE = .TRUE.
            CALL CHKOUT ( 'DASEC' )
            RETURN
 
         END IF
 
         RECNO  = 1 + NRESVR + 1
         CURPOS = 1
         NCHARS = 0
 
      END IF
C
C     Begin reading the comment area into the buffer.
C
      IF ( HANDLE .NE. LSTHAN ) THEN
C
C        If the current DAS handle is not the same as the handle on
C        the last call, then we need to read in the appropriate record
C        from the DAS file comment area. Otherwise the record was saved,
C        so we don't need to read it in.
C
         CALL DASIOC ( 'READ', DASLUN, RECNO, CRECRD )
 
      END IF
C
C     Initialize the BUFFER line counter, I, and the line position
C     counter, J.
C
      I = 1
      J = 1
 
      DONE = .FALSE.
      DO WHILE ( ( I .LE. BUFSIZ ) .AND. ( .NOT. DONE ) )
 
         EOL = .FALSE.
         DO WHILE ( .NOT. EOL )
 
            NCHARS = NCHARS + 1
            CH     = CRECRD(CURPOS:CURPOS)
 
            IF ( ICHAR( CH ) .EQ. INTEOL ) THEN
 
               EOL           = .TRUE.
 
               IF ( J .LE. LINLEN ) THEN
 
                  BUFFER(I)(J:) = ' '
 
               END IF
 
            ELSE
 
               IF ( J .LE. LINLEN ) THEN
 
                  BUFFER(I)(J:J) = CH
                  J              = J + 1
 
               ELSE
 
                  CALL SETMSG ( 'The output buffer line length (#)'  //
     .                          ' was not long enough to contain'    //
     .                          ' a comment line with length #.'      )
                  CALL ERRINT ( '#', LINLEN                           )
                  CALL ERRINT ( '#', I                                )
                  CALL SIGERR ( 'SPICE(COMMENTTOOLONG)'               )
                  CALL CHKOUT ( 'DASEC'                               )
                  RETURN
 
               END IF
 
            END IF
C
C           If we have reached the end of the current comment record,
C           read in the next one and reset the current position.
C           Otherwise, just increment the current position.
C
            IF ( CURPOS .EQ. MXCREC ) THEN
 
               RECNO = RECNO + 1
               CALL DASIOC ( 'READ', DASLUN, RECNO, CRECRD )
               CURPOS = 1
 
            ELSE
 
               CURPOS = CURPOS + 1
 
            END IF
C
C           Check to make sure that it is safe to continue, i.e.,
C           that the number of comment characters we have processed
C           has not exceeded the number of comment characters in the
C           comment area of the DAS file.
C
            IF ( NCHARS .GT. NCOMC ) THEN
 
               CALL SETMSG ( 'Count of comment characters (#)'     //
     .                       ' exceeds the number of comment'      //
     .                       ' characters (#) in the DAS file #.'   )
               CALL ERRINT ( '#',NCHARS                             )
               CALL ERRINT ( '#',NCOMC                              )
               CALL ERRFNM ( '#',DASLUN                             )
               CALL SIGERR ( 'SPICE(BADDASCOMMENTAREA)'             )
               CALL CHKOUT ( 'DASEC'                                )
               RETURN
 
            END IF
 
         END DO
C
C        We have just completed a comment line, so we save the comment
C        number, increment the buffer line counter, I, and reset the
C        buffer line position counter, J.
C
         NUMCOM = I
         I      = I + 1
         J      = 1
C
C        Check for the end of the comments.
C
         IF ( NCHARS .EQ. NCOMC ) THEN
C
C           If we have reached the end of the comments, signalled
C           by having processed all of the comment characters, NCOMC,
C           then we are done. So, set DONE to .TRUE. and remove the
C           entry for this file from the file table.
C
            DONE   = .TRUE.
            LSTHAN = -1
C
C           0 <= INDEX <= NFILES, and we only want to remove things
C           from the file table if:
C
C              1) There are files in the file table, NFILES > 0
C              2) The file we are currently reading from is in the
C                 file table, INDEX > 0.
C
C           So, if INDEX > 0, we know that there are files in the file
C           table, and that we are currently reading from one of them.
C
            IF ( INDEX .GT. 0 ) THEN
 
               DO K = INDEX, NFILES - 1
 
                  FILCNT(K) = FILCNT(K+1)
                  FILCHR(K) = FILCHR(K+1)
                  FILHAN(K) = FILHAN(K+1)
                  LSTREC(K) = LSTREC(K+1)
                  LSTPOS(K) = LSTPOS(K+1)
 
               END DO
 
               NFILES = NFILES - 1
 
            END IF
 
         END IF
 
      END DO
C
C     Set the number of comment lines in the buffer
C
      N = NUMCOM
C
C     At this point, we have either filled the buffer or we have
C     finished reading in the comment area. Find out what has
C     happened and act accordingly.
C
      IF ( .NOT. DONE ) THEN
C
C        If we are not done, then we have filled the buffer, so save
C        everything that needs to be saved in the file table before
C        exiting.
C
         IF ( INDEX .EQ. 0 ) THEN
C
C           This was the first time that the comment area of this file
C           has been read, so add it to the file table and save all of
C           its information if there is room in the file table.
C
            IF ( NFILES .GE. FTSIZE ) THEN
 
               CALL SETMSG ( 'The file table is full with # files,'  //
     .                       ' and another file could not be added.'  )
               CALL ERRINT ( '#',FTSIZE                               )
               CALL SIGERR ( 'SPICE(FILETABLEFULL)'                   )
               CALL CHKOUT ( 'DASEC'                                  )
               RETURN
 
            END IF
 
            NFILES         = NFILES + 1
            FILCNT(NFILES) = NCOMC
            FILCHR(NFILES) = NCHARS
            FILHAN(NFILES) = HANDLE
            LSTREC(NFILES) = RECNO
            LSTPOS(NFILES) = CURPOS
            LSTHAN         = HANDLE
 
         ELSE
C
C           The comment area of this file is already in the file table,
C           so just update its information.
C
            FILCHR(INDEX) = NCHARS
            LSTREC(INDEX) = RECNO
            LSTPOS(INDEX) = CURPOS
            LSTHAN        = HANDLE
 
         END IF
 
      END IF
 
      CALL CHKOUT ( 'DASEC' )
      RETURN
      END
