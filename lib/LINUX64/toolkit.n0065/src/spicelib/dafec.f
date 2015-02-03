C$Procedure DAFEC ( DAF extract comments )
 
      SUBROUTINE DAFEC ( HANDLE, BUFSIZ, N, BUFFER, DONE )
 
C$ Abstract
C
C     Extract comments from the comment area of a binary DAF.
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
      INTEGER               BUFSIZ
      INTEGER               N
      CHARACTER*(*)         BUFFER(*)
      LOGICAL               DONE
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C      HANDLE    I   Handle of binary DAF opened with read access.
C      BUFSIZ    I   Maximum size, in lines, of BUFFER.
C      N         O   Number of extracted comment lines.
C      BUFFER    O   Buffer where extracted comment lines are placed.
C      DONE      O   Indicates whether all comments have been extracted.
C
C$ Detailed_Input
C
C     HANDLE   The file handle of a binary DAF which has been opened
C              with read access.
C
C     BUFSIZ   The maximum number of comments that may be placed into
C              BUFFER. This would typically be the declared array size
C              for the Fortran character string array passed into this
C              routine.
C
C$ Detailed_Output
C
C     N        The number of comment lines extracted from the comment
C              area of the binary DAF attached to HANDLE. This number
C              will be <= BUFSIZ on output. If N = BUFSIZ and DONE <>
C              .TRUE., then there are more comments left to to extract.
C              If N = 0, then DONE = .TRUE., i.e., there were no
C              comments in the comment area or we have extracted all
C              of the comments. If there are comments in the comment
C              area, or comments remaining after the extraction process
C              has begun, N > 0, always.
C
C     BUFFER   A array of at most BUFSIZ comments which have been
C              extracted from the comment area of the binary DAF
C              attached to HANDLE.
C
C     DONE     A logical flag indicating whether or not all of the
C              comment lines from the comment area of the DAF have
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
C          the error SPICE(INVALIDARGUMENT) will be signaled.
C
C     3)   If a comment line in a DAF is longer than the length
C          of a character string array element of BUFFER, the error
C          SPICE(COMMENTTOOLONG) will be signaled.
C
C     3)   If the end of the comments cannot be found, i.e., the end of
C          comments marker is missing on the last comment record, the
C          error SPICE(BADCOMMENTAREA) will be signaled.
C
C     4)   If the number of comment characters scanned exceeds the
C          number of comment characters computed, the error
C          SPICE(BADCOMMENTAREA) will be signaled.
C
C     5)   If the binary DAF attached to HANDLE is not open for
C          reading,an error will be signaled by a routine called by
C          this routine.
C
C$ Files
C
C     See argument HANDLE in $ Detailed_Input.
C
C$ Particulars
C
C     A binary DAF contains an area which is reserved for storing
C     annotations or descriptive textual information describing the data
C     contained in a file. This area is referred to as the ``comment
C     area'' of the file. The comment area of a DAF is a line
C     oriented medium for storing textual information. The comment
C     area preserves any leading or embedded white space in the line(s)
C     of text which are stored, so that the appearance of the of
C     information will be unchanged when it is retrieved (extracted) at
C     some other time. Trailing blanks, however, are NOT preserved,
C     due to the way that character strings are represented in
C     standard Fortran 77.
C
C     This routine will read the comments from the comment area of
C     a binary DAF, placing them into a line buffer. If the line
C     buffer is not large enough to hold the entire comment area,
C     the portion read will be returned to the caller, and the DONE
C     flag will be set to .FALSE.. This allows the comment area to be
C     read in ``chunks,'' a buffer at a time. After all of the comment
C     lines have been read, the DONE flag will be set to .TRUE..
C
C     This routine can be used to ``simultaneously'' extract comments
C     from the comment areas of multiple binary DAFs. See Example
C     2 in the $ Examples section.
C
C$ Examples
C
C     Example 1
C     ---------
C
C     The following example will extract the entire comment area of a
C     binary DAF attached to HANDLE, displaying the comments on the
C     terminal screen.
C
C     Let
C
C        BUFFER  have the following declaration:
C
C           CHARACTER*(80)  BUFFER(25)
C
C        HANDLE  be the handle of an open binary DAF file.
C
C     then
C
C        BUFSIZ = 25
C        DONE   = .FALSE.
C
C        DO WHILE ( .NOT. DONE )
C
C           CALL DAFEC( HANDLE, BUFSIZ, N, BUFFER, DONE )
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
C     simultaneously read the comment areas of multiple DAFs. For each
C     file, the comments will be displayed on the screen as they are
C     extracted.
C
C     Let
C
C        BUFFER  have the following declaration:
C
C           CHARACTER*(80)  BUFFER(25)
C
C        NUMFIL     be the number of binary DAFs that are to have their
C                   comment areas displayed.
C
C        DAFNAM(I)  Be a list of filenames for the DAFs which are to
C                   have their comment areas displayed.
C
C        HANDLE(I)  be a list of handles for the DAFs which are to have
C                   their comment areas displayed.
C
C        DONE(I)    be a list of logical flags indicating whether
C                   we are done extracting the comment area from the
C                   DAF attached to HANDLE(I)
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
C     C      Open the DAFs.
C     C
C            DO I = 1, NUMFIL
C
C               CALL DAFOPR ( DAFNAM(I), HANDLE(I) )
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
C                     WRITE (*,*) 'File: ', DAFNAM(I)(:RTRIM(DAFNAM(I)))
C                     WRITE (*,*)
C                     N = 0
C
C                     CALL DAFEC ( HANDLE(I),
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
C        decimal values 32 - 126.
C
C     2) There is NO maximum length imposed on the significant portion
C        of a text line that may be placed into the comment area of a
C        DAF. The maximum length of a line stored in the comment area
C        should be kept reasonable, so that they may be easily
C        extracted. A good value for this would be 1000 characters, as
C        this can easily accomodate ``screen width'' lines as well as
C        long lines which may contain some other form of information.
C
C     3) This routine is only used to read records on environments
C        whose characters are a single byte in size.  Updates
C        to this routine and routines in its call tree may be
C        required to properly handle other cases.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     K.R. Gehringer (JPL)
C     F.S. Turner    (JPL)
C     N.J. Bachman   (JPL)
C     B.V. Semenov   (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0 12-APR-2012 (BVS)
C
C        Increased FTSIZE (from 1000 to 5000).
C
C-    SPICELIB Version 1.0.0 08-NOV-2006 (NJB) (KRG) (FST)
C
C        Based on Support Version 2.0.0, 16-NOV-2001 (FST)
C
C-&
 
C$ Index_Entries
C
C      extract comments from a DAF
C
C-&
 
C
C     SPICELIB functions
C
      INTEGER               CPOS
      INTEGER               ISRCHI
      INTEGER               NCPOS
 
      LOGICAL               FAILED
      LOGICAL               RETURN

C
C     Local parameters
C
C     Length of a DAF internal filename.
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
C     The maximum number of DAFs that may be open simultaneously.
C
      INTEGER               FTSIZE
      PARAMETER           ( FTSIZE = 5000 )

C
C     Length of a DAF character record, in characters.
C
      INTEGER               MXCREC
      PARAMETER           ( MXCREC = 1000 )
C
C     Local variables
C
      CHARACTER*(1)         CH
      CHARACTER*(MXCREC)    CRECRD
      CHARACTER*(1)         EOCMRK
      CHARACTER*(1)         EOLMRK
      CHARACTER*(IFNLEN)    IFNAME
 
      INTEGER               CURPOS
      INTEGER               DAFLUN
      INTEGER               EOCPOS
      INTEGER               I
      INTEGER               INDEX
      INTEGER               J
      INTEGER               K
      INTEGER               LINLEN
      INTEGER               NCHARS
      INTEGER               NCOMC
      INTEGER               NCOMR
      INTEGER               NELPOS
      INTEGER               NUMCOM
      INTEGER               RECNO
 
      INTEGER               IOSTAT
      INTEGER               BWARD
      INTEGER               FWARD
      INTEGER               FREE
      INTEGER               ND
      INTEGER               NI
C
C     The file table declarations for keeping track of which files
C     are currently in the process of having comments extracted.
C
      INTEGER               FILCHR(FTSIZE)
      INTEGER               FILCNT(FTSIZE)
      INTEGER               FILHAN(FTSIZE)
      INTEGER               LSTHAN
      INTEGER               LSTPOS(FTSIZE)
      INTEGER               LSTREC(FTSIZE)
      INTEGER               NFILES
 
      LOGICAL               EMPTY
      LOGICAL               EOL
      LOGICAL               FIRST
      LOGICAL               FOUND

C
C     Saved variables
C
      SAVE CRECRD
      SAVE EOCMRK
      SAVE EOLMRK
C
C     Save all of the file table information.
C
      SAVE FILCHR
      SAVE FILCNT
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
         CALL CHKIN ( 'DAFEC' )
      END IF
C
C     If this is the first time that this routine has been called,
C     we need to initialize the character value of the end-of-line
C     marker, and the file table variables.
C
      IF ( FIRST ) THEN
 
         FIRST  = .FALSE.
         NFILES = 0
         LSTHAN = 0
         EOCMRK = CHAR ( INTEOC )
         EOLMRK = CHAR ( INTEOL )
 
         DO I = 1, FTSIZE
 
            FILCHR(I) = 0
            FILCNT(I) = 0
            FILHAN(I) = 0
            LSTPOS(I) = 0
            LSTREC(I) = 0
 
         END DO
 
      END IF
C
C     Verify that the DAF attached to HANDLE is opened for reading
C     by calling the routine to signal an invalid access mode on a
C     handle.
C
      CALL DAFSIH ( HANDLE, 'READ' )
 
      IF ( FAILED() ) THEN
 
         CALL CHKOUT ( 'DAFEC' )
         RETURN
 
      END IF
C
C     Check for a nonpositive BUFFER size.
C
      IF ( BUFSIZ .LE. 0 ) THEN
 
         CALL SETMSG ( 'The output buffer size was not positive: #.' )
         CALL ERRINT ( '#', BUFSIZ                                   )
         CALL SIGERR ( 'SPICE(INVALIDARGUMENT)'                      )
         CALL CHKOUT ( 'DAFEC'                                       )
         RETURN
 
      END IF
C
C     Convert the DAF handle to its corresponding Fortran logical
C     unit number for reading the comment records.
C
      CALL ZZDDHHLU ( HANDLE, 'DAF', .FALSE., DAFLUN )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'DAFEC' )
         RETURN
      END IF
C
C     Get the length of a single character string in the buffer.
C
      LINLEN = LEN ( BUFFER(1) )
C
C     If we have extracted comments from at least one file and we
C     didn't finish, check to see if HANDLE is in the file table.
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
C        the file record. We also need to count the number of comment
C        characters.
C
C        Read the file record from the DAF attached to HANDLE. We will
C        get back some stuff that we do not use.
C
         CALL DAFRFR ( HANDLE, ND, NI, IFNAME, FWARD, BWARD, FREE )
 
         IF ( FAILED() ) THEN
 
            CALL CHKOUT ( 'DAFEC' )
            RETURN
 
         END IF
C
C        Compute the number of comment records and the number of
C        comment characters. In order to perform these calculations,
C        we assume that we have a valid comment area in the DAF
C        attached to HANDLE.
C
         NCOMR = FWARD - 2
 
         IF ( NCOMR .GT. 0 ) THEN
C
C           The starting record number is the number of comment records
C           + 1 where the 1 skips the file record.
C
            EMPTY  = .TRUE.
            FOUND  = .FALSE.
 
            DO WHILE (    ( NCOMR .GT. 0 ) .AND. ( .NOT. FOUND )
     .                .AND. EMPTY                                )
 
               RECNO = NCOMR + 1
 
               READ ( DAFLUN, REC=RECNO, IOSTAT=IOSTAT ) CRECRD
 
               IF ( IOSTAT .NE. 0 ) THEN
 
                  CALL SETMSG ( 'Error reading comment area of' //
     .                          ' binary file named ''#''.'     //
     .                          ' IOSTAT = #.'                   )
                  CALL ERRFNM ( '#', DAFLUN                      )
                  CALL ERRINT ( '#', IOSTAT                      )
                  CALL SIGERR ( 'SPICE(FILEREADFAILED)'          )
                  CALL CHKOUT ( 'DAFEC'                          )
                  RETURN
 
               END IF
C
C              Scan the comment record looking for the end of comments
C              marker.
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
 
                  END IF
 
               END IF
 
            END DO
C
C           If we do not find the end of comments marker and the
C           comment area is not empty, then it is an error.
C
            IF ( ( .NOT. FOUND ) .AND. ( .NOT. EMPTY ) ) THEN
 
               CALL SETMSG ( 'The comment area in the DAF file'   //
     .                       ' ''#'' may be damaged. The end of'  //
     .                       ' the comments could not be found.'   )
               CALL ERRFNM ( '#', DAFLUN                           )
               CALL SIGERR ( 'SPICE(BADCOMMENTAREA)'               )
               CALL CHKOUT ( 'DAFEC'                               )
               RETURN
 
            ELSE IF ( FOUND ) THEN
 
               NCOMC = MXCREC * ( NCOMR - 1 ) + EOCPOS - 1
 
            ELSE IF ( EMPTY ) THEN
 
               NCOMC = 0
 
            END IF
 
         ELSE
 
            NCOMC = 0
 
         END IF
C
C        If the number of comment characters, NCOMC, is equal to zero,
C        then we have no comments to read, so set the number of comments
C        to zero, set DONE to .TRUE., check out,  and return.
C
         IF ( NCOMC .EQ. 0 ) THEN
 
            N    = 0
            DONE = .TRUE.
            CALL CHKOUT ( 'DAFEC' )
            RETURN
 
         END IF
C
C        Otherwise, set the initial position  in the comment area.
C
         RECNO  = 2
         CURPOS = 1
         NCHARS = 0
 
      END IF
C
C     Begin reading the comment area into the buffer.
C
      IF ( HANDLE .NE. LSTHAN ) THEN
C
C        If the current DAF handle is not the same as the handle on
C        the last call, then we need to read in the appropriate record
C        from the DAF comment area. Otherwise the record was saved and
C        so we don't need to read it in.
C
         READ ( DAFLUN, REC=RECNO, IOSTAT=IOSTAT ) CRECRD
 
         IF ( IOSTAT .NE. 0 ) THEN
 
            CALL SETMSG ( 'Error reading comment area of binary' //
     .                    ' file named FILE.  IOSTAT = *.'        )
            CALL ERRINT ( '*', IOSTAT                             )
            CALL ERRFNM ( 'FILE', DAFLUN                          )
            CALL SIGERR ( 'SPICE(FILEREADFAILED)'                 )
            CALL CHKOUT ( 'DAFEC'                                 )
            RETURN
 
         END IF
 
      END IF
C
C     Initialize the BUFFER line counter, I, and the line position
C     counter, J.
C
      I = 1
      J = 1
C
C     Start filling up the BUFFER.
C
      NUMCOM = 0
      DONE   = .FALSE.

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
     .                          ' comment line #.'                    )
                  CALL ERRINT ( '#', LINLEN                           )
                  CALL ERRINT ( '#', I                                )
                  CALL SIGERR ( 'SPICE(COMMENTTOOLONG)'               )
                  CALL CHKOUT ( 'DAFEC'                               )
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
               READ ( DAFLUN, REC=RECNO, IOSTAT=IOSTAT ) CRECRD
 
               IF ( IOSTAT .NE. 0 ) THEN
 
                  CALL SETMSG ( 'Error reading comment area of'  //
     .                          ' binary file named #.'          //
     .                          '  IOSTAT = #.'                   )
                  CALL ERRFNM ( '#', DAFLUN                       )
                  CALL ERRINT ( '#', IOSTAT                       )
                  CALL SIGERR ( 'SPICE(FILEREADFAILED)'           )
                  CALL CHKOUT ( 'DAFEC'                           )
                  RETURN
 
               END IF
 
               CURPOS = 1
 
            ELSE
 
               CURPOS = CURPOS + 1
 
            END IF
C
C           Check to make sure that it is safe to continue, i.e.,
C           that the number of comment characters we have processed
C           has not exceeded the number of comment characters in the
C           comment area of the DAF file. This should never happen.
C
            IF ( NCHARS .GT. NCOMC ) THEN
 
               CALL SETMSG ( 'Count of comment characters (#)'     //
     .                       ' exceeds the number of comment'      //
     .                       ' characters (#) in the DAF file #.'   )
               CALL ERRINT ( '#',NCHARS                             )
               CALL ERRINT ( '#',NCOMC                              )
               CALL ERRFNM ( '#',DAFLUN                             )
               CALL SIGERR ( 'SPICE(BADCOMMENTAREA)'                )
               CALL CHKOUT ( 'DAFEC'                                )
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
C           If we have reached the end of the comments, signaled
C           by having processed all of the comment characters, NCOMC,
C           then we are done. So, set DONE to .TRUE. and remove the
C           entry for this file from the file table.
C
            DONE = .TRUE.
            LSTHAN = 0
C
C           0 <= INDEX <= NFILES, and we only want to remove things
C           from the file table if:
C
C              The file we are currently reading from is in the
C              file table, INDEX > 0, which implies NFILES > 0.
C
C           So, if INDEX > 0, we know that there are files in the file
C           table, and that we are currently reading from one of them.
C
            IF ( INDEX .GT. 0 ) THEN
 
               DO K = INDEX, NFILES - 1
 
                  FILCHR(K) = FILCHR(K+1)
                  FILCNT(K) = FILCNT(K+1)
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
               CALL CHKOUT ( 'DAFEC'                                  )
               RETURN
 
            END IF
 
            NFILES         = NFILES + 1
            FILCHR(NFILES) = NCHARS
            FILCNT(NFILES) = NCOMC
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
 
      CALL CHKOUT ( 'DAFEC' )
      RETURN
      END
