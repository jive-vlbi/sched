C$Procedure SPCRFL ( SPK and CK, read first line of comments )
 
      SUBROUTINE SPCRFL ( HANDLE, LINE, EOC )
 
C$ Abstract
C
C     Read the first line of text from the comment area
C     of a binary SPK or CK file.
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
C
C$ Declarations
 
      INTEGER               HANDLE
      CHARACTER*(*)         LINE
      LOGICAL               EOC
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle assigned to binary SPK or CK file.
C     LINE       O   First line of text from the comment area.
C     EOC        O   End of comments?
C
C$ Detailed_Input
C
C     HANDLE      is the handle assigned to the binary SPK or CK file
C                 which has been opened for read access.
C
C$ Detailed_Output
C
C     LINE        is the first line of text from the comment area of
C                 the SPK or CK file specified by HANDLE.  LINE may
C                 be blank.
C
C     EOC         is true if the comment area is empty.  If there
C                 are comments in the comment area, then EOC is false.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If the comment area of the SPK or CK file is empty, LINE
C        will be blank.
C
C     2) If the first line of comments in the comment area is longer
C        than the declared length of LINE, it will be truncated to
C        fit into the variable.
C
C     3) If there is a problem reading from the comment area, the error
C        SPICE(FILEREADFAILED) is signalled.
C
C     4) If the comments are not in the correct format, the error
C        SPICE(FORMATERROR) is signalled.
C
C$ Files
C
C     HANDLE      is the handle assigned to the binary SPK or CK file.
C                 Use DAFOPR to open it for read access and get its
C                 handle unless SPKLEF or CKLPF has already been called
C                 and returned the handle.  This file is unchanged by
C                 calling SPCRFL.
C
C$ Particulars
C
C     The structure of SPK and CK files accommodates comments in
C     addition to data.  The following routines are available
C     for accessing the comment area of a binary SPK or CK file:
C
C           SPCAC           add comments
C
C           SPCEC           extract comments
C
C           SPCDC           delete comments
C
C           SPCRFL          read first line of comments
C
C           SPCRNL          read next line of comments
C
C     Note that comments must consist of only text, that is, printable
C     ASCII characters, specifically ASCII 32-126.  This excludes
C     tabs (ASCII 9) and control characters.
C
C     The SPC conversion routines---SPCB2A, SPCA2B, SPCB2T, and
C     SPCT2B---include these comments when converting SPK and CK
C     files between binary and text formats.
C
C$ Examples
C
C     Suppose we have a binary SPK file called A.BSP.  The following
C     code fragment searches the comment area for a lines containing
C     the character string `SOURCE' and writes the lines to standard
C     output.
C
C      C
C      C     Open the binary SPK file and get its handle.
C      C
C            CALL DAFOPR ( 'A.BSP', HANDLE )
C
C      C
C      C     Read the first line of comments.
C      C
C            CALL SPCRFL ( HANDLE, LINE, EOC )
C
C      C
C      C     Search for the string 'SOURCE' in the line.  If
C      C     it is found, write the line.  Then get the next
C      C     line of comments and repeat as long as we're not
C      C     at the end.
C      C
C            DO WHILE ( .NOT. EOC )
C
C               IF (  POS ( LINE, 'SOURCE', 1 ) .NE. 0  ) THEN
C                  WRITE (*,*) LINE
C               END IF
C
C               CALL SPCRNL ( LINE, EOC )
C
C            END DO
C
C$ Restrictions
C
C     1)  This routine assumes that the comment area of the binary SPK
C         or CK file contains only text stored by SPCAC.  Comments
C         written any other way may not be handled properly.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     K.R. Gehringer (JPL)
C     J.E. McLean    (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.0.0, 16-NOV-2001 (FST)
C
C        Updated this routine to utilize the new handle manager
C        interfaces.
C
C-    SPICELIB Version 1.1.0, 27-JUL-1992 (KRG)
C
C        Removed a call to the SPICELIB subroutine SUFFIX() which
C        was used to join two parts of a comment line that may be
C        broken across two comment records. The problem was, SUFFIX
C        cannot know about leading/imbedded blanks when it appends, so
C        blanks were inadvertantly removed when they happened to be
C        stored at the end of comment record.
C
C        Added the variable TMPLEN to record the length of the first
C        part of a comment line that may be broken across comment
C        records.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 15-APR-1991 (JEM)
C
C-&
 
C$ Index_Entries
C
C     read the first comment line of an spk or ck file
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 2.0.0, 16-NOV-2001 (FST)
C
C        Calls to DAFHLU now lock handles to their logical units.
C        While at first glance it may seem this is the appropriate
C        course of action due to the buffering of the logical unit
C        by this routine for its entry point, adding a call to
C        ZZDDHUNL in the entry point removes the need to lock DAFU
C        to its handle.  The value of HANDLE is now buffered in
C        HANBUF, to allow the entry point to retrieve a logical
C        unit.
C
C-&
 
 
C
C     SPICELIB functions
C
      INTEGER               POS
 
      LOGICAL               FAILED
      LOGICAL               RETURN
 
C
C     Local parameters
C
C     IFNLEN      is the length of a DAF internal file name.
C
C     MAXCPR      is the maximum number of characters per DAF record and
C                 hence the maximum comment line length.
C
      INTEGER               IFNLEN
      PARAMETER           ( IFNLEN = 60 )
 
      INTEGER               MAXCPR
      PARAMETER           ( MAXCPR = 1000   )
 
C
C     Local variables
C
      CHARACTER*(1)         EOT
      CHARACTER*(IFNLEN)    IFNAME
      CHARACTER*(1)         NULL
      CHARACTER*(MAXCPR)    RECORD
      CHARACTER*(MAXCPR)    TEMP
 
      INTEGER               BOL
      INTEGER               BWARD
      INTEGER               DAFU
      INTEGER               EOL
      INTEGER               FREE
      INTEGER               FWARD
      INTEGER               HANBUF
      INTEGER               IOSTAT
      INTEGER               ND
      INTEGER               NI
      INTEGER               NRR
      INTEGER               POSNUL
      INTEGER               REC
      INTEGER               TMPLEN
 
      LOGICAL               CALLED
      LOGICAL               EOCSAV
 
C
C     Saved variables
C
      SAVE
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SPCRFL' )
      END IF
 
C
C     SPCRFL has been called for this file.
C
      CALLED = .TRUE.
 
 
C
C     Read the file record to find out if the DAF contains any
C     reserved records.  The reserved records in an array file
C     are stored between the first record and the first summary
C     record.  FWARD is the record number of that first summary
C     record, and NRR is the number of reserved records in the file.
C
C     If there are no reserved records, there are no comments.
C
      CALL DAFRFR ( HANDLE, ND, NI, IFNAME, FWARD, BWARD, FREE )
      NRR = FWARD - 2
 
      IF ( NRR .EQ. 0 ) THEN
         LINE   = ' '
         EOC    = .TRUE.
         EOCSAV = EOC
         CALL CHKOUT ( 'SPCRFL' )
         RETURN
      END IF
 
C
C     We need to read directly from the SPK/CK file, using a logical
C     unit instead of a handle.
C
      CALL ZZDDHHLU ( HANDLE, 'DAF', .FALSE., DAFU )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'SPCRFL' )
         RETURN
      END IF
 
C
C     Buffer the value of HANDLE.
C
      HANBUF = HANDLE
 
C
C     In the comment area, NULL means end-of-line, and EOT means
C     end-of-transmission, or in other words, end-of-comments.
C
      NULL = CHAR ( 0 )
      EOT  = CHAR ( 4 )
 
C
C     Read the first reserved record.
C
      REC = 2
 
      READ ( DAFU, REC=REC, IOSTAT=IOSTAT ) RECORD
 
      IF ( IOSTAT .NE. 0 ) THEN
         CALL SETMSG ( 'Error reading comment area of the binary ' //
     .                 'file named FNM at record #.  Value of '    //
     .                 'IOSTAT is #.'                              )
         CALL ERRINT ( '#', REC                                    )
         CALL ERRINT ( '#', IOSTAT                                 )
         CALL ERRFNM ( 'FNM', DAFU                                 )
         CALL SIGERR ( 'SPICE(FILEREADFAILED)'                     )
         CALL CHKOUT ( 'SPCRFL'                                    )
         RETURN
      END IF
 
C
C     The first line of comments begins with the first character
C     of the record.  A NULL character specifies the end.
C
      POSNUL = POS ( RECORD, NULL, 1 )
 
      IF ( POSNUL .EQ. 0 ) THEN
C
C        No NULL is in the record, so LINE is just the whole
C        record.  (The maximum length of a line written to
C        the comment area by SPCAC is MAXCPR characters).
C
         EOL = MAXCPR
 
      ELSE
C
C        The end of the line precedes the NULL character.
C
         EOL = POSNUL - 1
 
      END IF
 
C
C     Now we have the position of the end of the first line.
C     Assign it to LINE.  We're not yet at the end of comments,
C     since we have a line to return.  If the first character
C     was a NULL, the line is blank.
C
      IF ( EOL .EQ. 0 ) THEN
         LINE = ' '
      ELSE
         LINE = RECORD ( 1: EOL )
      END IF
 
      EOC     = .FALSE.
      EOCSAV  = EOC
 
      CALL CHKOUT ( 'SPCRFL' )
      RETURN
 
 
 
 
C$Procedure SPCRNL ( SPK and CK, read next line of comments )
 
      ENTRY SPCRNL ( LINE, EOC )
 
C$ Abstract
C
C     Continue reading lines from the comment area of a binary
C     SPK or CK file specified by the most recent call to
C     the routine SPCRFL.
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
C
C$ Declarations
C
C     CHARACTER*(*)         LINE
C     LOGICAL               EOC
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     LINE       O   Next line of text from the comment area.
C     EOC        O   End of comments?
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     LINE        is the next line of text from the comment area of
C                 the SPK or CK file.  LINE may be blank.
C                 SPCRFL reads the first line of comments from
C                 a specified binary SPK or CK file.  Once SPCRFL
C                 has been called, SPCRNL may be called repetitively
C                 to read the next lines of the comment area until
C                 the end.
C
C     EOC         is true if there are no more comments to read.
C                 Otherwise, EOC is false.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If SPCRFL is not called prior to calling SPCRNL, the error
C        SPICE(SPCRFLNOTCALLED).
C
C     2) If the most recent call to SPCRFL returned EOC with the value
C        true, then SPCRNL will return EOC with the same value.
C
C     3) If EOC is true, LINE will be blank.
C
C     4) If the first line of comments in the comment area is longer
C        than the declared length of LINE, it will be truncated to
C        fit into the variable.
C
C     5) If there is a problem reading from the comment area, the error
C        SPICE(FILEREADFAILED) is signalled.
C
C     6) If the comments are not in the correct format, the error
C        SPICE(FORMATERROR) is signalled.
C
C$ Files
C
C     The handle of the binary SPK or CK is specified with the routine
C     SPCRFL.
C
C$ Particulars
C
C     The structure of SPK and CK files accommodates comments in
C     addition to data.  The following five routines are available
C     for accessing the comment area of a binary SPK or CK file:
C
C           SPCAC           add comments
C
C           SPCEC           extract comments
C
C           SPCDC           delete comments
C
C           SPCRFL          read first line of comments
C
C           SPCRNL          read next line of comments
C
C     Note that comments must consist of only text, that is, printable
C     ASCII characters, specifically ASCII 32-126.  This excludes
C     tabs (ASCII 9) and control characters.
C
C     The SPC conversion routines---SPCB2A, SPCA2B, SPCB2T, and
C     SPCT2B---include these comments when converting SPK and CK
C     files between binary and text formats.
C
C$ Examples
C
C     Suppose we have a binary SPK file called A.BSP.  The following
C     code fragment searches the comment area for a lines containing
C     the character string `SOURCE' and writes the lines to standard
C     output.
C
C      C
C      C     Open the binary SPK file and get its handle.
C      C
C            CALL DAFOPR ( 'A.BSP', HANDLE )
C
C      C
C      C     Read the first line of comments.
C      C
C            CALL SPCRFL ( HANDLE, LINE, EOC )
C
C      C
C      C     Search for the string 'SOURCE' in the line.  If
C      C     it is found, write the line.  Then get the next
C      C     line of comments and repeat as long as we're not
C      C     at the end.
C      C
C            DO WHILE ( .NOT. EOC )
C
C               IF (  POS ( LINE, 'SOURCE', 1 ) .NE. 0  ) THEN
C                  WRITE (*,*) LINE
C               END IF
C
C               CALL SPCRNL ( LINE, EOC )
C
C            END DO
C
C$ Restrictions
C
C     1)  This routine assumes that the comment area of the binary SPK
C         or CK file contains only text stored by SPCAC.  Comments
C         written any other way may not be handled properly.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     K.R. Gehringer (JPL)
C     J.E. McLean    (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.0.0, 16-NOV-2001 (FST)
C
C        Updated this entry point to utilize the handle manager
C        interfaces.  See the Revisions section of the subroutine
C        header above for a detailed discussion of the changes.
C
C-    SPICELIB Version 1.1.0, 27-JUL-1992 (KRG)
C
C        Removed a call to the SPICELIB subroutine SUFFIX() which
C        was used to join two parts of a comment line that may be
C        broken across two comment records. The problem was, SUFFIX
C        cannot know about leading/imbedded blanks when it appends, so
C        blanks were inadvertantly removed when they happened to be
C        stored at the end of comment record.
C
C        Added the variable TMPLEN to record the length of the first
C        part of a comment line that may be broken across comment
C        records.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 15-APR-1991 (JEM)
C
C-&
 
C$ Index_Entries
C
C     continue reading comments from an spk or ck file
C     read the next comment line of an spk or ck file
C
C-&
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SPCRNL' )
      END IF
 
 
C
C     If SPCRFL hasn't been called, then we don't know which
C     file to read from.
C
      IF ( .NOT. CALLED ) THEN
         CALL SETMSG ( 'You must call SPCRFL to read the first '      //
     .                 'line of comments before calling SPCRNL '      //
     .                 'to read the next line.'                       )
         CALL SIGERR ( 'SPICE(SPCRFLNOTCALLED)'                       )
         CALL CHKOUT ( 'SPCRNL'                                       )
         RETURN
      END IF
 
C
C     If we were at the end of comments before, then we're still
C     at the end.
C
      IF ( EOCSAV ) THEN
         LINE = ' '
         EOC  = .TRUE.
         CALL CHKOUT ( 'SPCRNL' )
         RETURN
      END IF
 
C
C     Retrieve a logical unit for HANBUF.
C
      CALL ZZDDHHLU ( HANBUF, 'DAF', .FALSE., DAFU )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'SPCRNL' )
         RETURN
      END IF
 
C
C     RECORD contains the last line and EOL is the position of
C     the end of that line.  Now we need to determine the
C     position of the beginning of the next line (BOL).  There
C     is a NULL between EOL and BOL, so BOL is two more than
C     EOL.  If that puts BOL off the end of the current RECORD,
C     then we have to go to the next record.
C
      BOL = EOL + 2
 
      IF ( BOL .GT. MAXCPR ) THEN
 
         BOL = BOL - MAXCPR
 
         REC = REC + 1
 
C
C        Check to make sure that we're not reading past the
C        reserved records.  FWARD is the "forward list pointer".
C        It is the number of the first record after the reserved
C        records.
C
         IF ( REC .GE. FWARD ) THEN
            CALL SETMSG ( 'The comment area of the binary file '      //
     .                    'named FNM is formatted incorrectly. '      //
     .                    'The end of the comments is not marked '    //
     .                    'as it should be in record #. Calling '     //
     .                    'SPCDC or DAFRRR will remove the comment '  //
     .                    'area and eliminate this format error. '    //
     .                    'Comments should be written ONLY by SPCAC.' )
            CALL ERRINT ( '#', REC-1                                  )
            CALL ERRFNM ( 'FNM', DAFU                                 )
            CALL SIGERR ( 'SPICE(FORMATERROR)'                        )
            CALL CHKOUT ( 'SPCRNL'                                    )
            RETURN
         END IF
 
C
C        All clear to read the record.
C
         READ ( DAFU, REC=REC, IOSTAT=IOSTAT ) RECORD
 
         IF ( IOSTAT .NE. 0 ) THEN
            CALL SETMSG ( 'Error reading comment area of the binary ' //
     .                    'file named FNM at record #.  Value of '    //
     .                    'IOSTAT is #.'                              )
            CALL ERRINT ( '#', REC                                    )
            CALL ERRINT ( '#', IOSTAT                                 )
            CALL ERRFNM ( 'FNM', DAFU                                 )
            CALL SIGERR ( 'SPICE(FILEREADFAILED)'                     )
            CALL CHKOUT ( 'SPCRNL'                                    )
            RETURN
         END IF
 
      END IF
 
C
C     RECORD is now the record of the file that contains the
C     beginning of the next line (BOL).  The line may not
C     exist or may be blank or may be a character string.
C
 
      IF ( RECORD ( BOL: BOL ) .EQ. EOT ) THEN
C
C        There isn't a next line to get.  We're at the end of
C        the comments.
C
         LINE   = ' '
         EOC    = .TRUE.
         EOCSAV = EOC
         CALL CHKOUT ( 'SPCRNL' )
         RETURN
      END IF
 
 
      IF ( RECORD ( BOL: BOL ) .EQ. NULL ) THEN
C
C        Just a NULL means a blank line.
C
         EOL    = BOL - 1
         LINE   = ' '
         EOC    = .FALSE.
         EOCSAV = EOC
         CALL CHKOUT ( 'SPCRNL' )
         RETURN
      END IF
 
C
C     The beginning of the next line is a character.  Now we have
C     to find the end.  It precedes the next NULL.
C
      POSNUL = POS ( RECORD, NULL, BOL )
 
      IF ( POSNUL .NE. 0 ) THEN
 
         EOL    = POSNUL - 1
         LINE   = RECORD ( BOL: EOL )
         EOC    = .FALSE.
         EOCSAV = EOC
 
      ELSE
C
C        There is no NULL in the rest of the record, so we have to
C        read the next record to find it.  Save the first part
C        of the line in TEMP.
C
         TEMP = RECORD ( BOL: MAXCPR )
         TMPLEN = MAXCPR - BOL + 1
 
         REC = REC + 1
 
C
C        Check to make sure that we're not reading past the
C        reserved records.  FWARD is the "forward list pointer".
C        It is the number of the first record after the reserved
C        records.
C
         IF ( REC .GE. FWARD ) THEN
            CALL SETMSG ( 'The comment area of the binary file '      //
     .                    'named FNM is formatted incorrectly. '      //
     .                    'The end of the comments is not marked '    //
     .                    'as it should be in record #. Calling '     //
     .                    'SPCDC or DAFRRR will remove the comment '  //
     .                    'area and eliminate this format error. '    //
     .                    'Comments should be written ONLY by SPCAC.' )
            CALL ERRINT ( '#', REC-1                                  )
            CALL ERRFNM ( 'FNM', DAFU                                 )
            CALL SIGERR ( 'SPICE(FORMATERROR)'                        )
            CALL CHKOUT ( 'SPCRNL'                                    )
            RETURN
         END IF
 
C
C        All clear to read the record.
C
         READ ( DAFU, REC=REC, IOSTAT=IOSTAT ) RECORD
 
         IF ( IOSTAT .NE. 0 ) THEN
            CALL SETMSG ( 'Error reading comment area of the binary ' //
     .                    'file named FNM at record #.  Value of '    //
     .                    'IOSTAT is #.'                              )
            CALL ERRINT ( '#', REC                                    )
            CALL ERRINT ( '#', IOSTAT                                 )
            CALL ERRFNM ( 'FNM', DAFU                                 )
            CALL SIGERR ( 'SPICE(FILEREADFAILED)'                     )
            CALL CHKOUT ( 'SPCRNL'                                    )
            RETURN
         END IF
 
C
C        There should be a null in this new record.  If there isn't
C        then something is wrong.  The maximum length of a line is
C        MAXCPR characters according to SPCAC.  So BOL and the NULL
C        should be in the same record or in adjacent records.
C
         POSNUL = POS ( RECORD, NULL, 1 )
 
         IF ( POSNUL .EQ. 0 ) THEN
            CALL SETMSG ( 'Cannot find the end of the line.  There '  //
     .                    'is something wrong with the format of the' //
     .                    'comments.'                                 )
            CALL SIGERR ( 'SPICE(FORMATERROR)'                        )
            CALL CHKOUT ( 'SPCRNL'                                    )
            RETURN
         END IF
 
         EOL = POSNUL - 1
 
C
C        EOL is zero if the NULL was the first character of the
C        new record.  Otherwise, concatenate the two parts of
C        the line from the two adjacent records.  Then assign the
C        values of LINE and EOC.
C
         IF ( EOL .NE. 0 ) THEN
            TEMP( TMPLEN+1: ) = RECORD( 1 : EOL )
         END IF
 
         LINE   = TEMP
         EOC    = .FALSE.
         EOCSAV = EOC
 
      END IF
 
 
      CALL CHKOUT ( 'SPCRNL' )
      RETURN
      END
