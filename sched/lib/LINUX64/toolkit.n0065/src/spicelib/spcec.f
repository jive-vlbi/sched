C$Procedure SPCEC ( SPK and CK, extract comments )
 
      SUBROUTINE SPCEC ( HANDLE, UNIT )
 
C$ Abstract
C
C     Extract the text from the comment area of a binary SPK or CK file
C     and write it to a text file.
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
      INTEGER               UNIT
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle assigned to binary SPK or CK file.
C     UNIT       I   Logical unit connected to text file.
C
C$ Detailed_Input
C
C     HANDLE      is the handle assigned to the binary SPK or CK file
C                 which has been opened for read access.
C
C     UNIT        is the logical unit connected to the text file to
C                 which the contents of the comment area of the SPK
C                 or CK file will be written, beginning at the current
C                 position of the file pointer.
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
C     1) If the comment area of the SPK or CK file is empty, nothing
C        will be written to the text file.
C
C     2) If there is a problem reading from the comment area, the error
C        SPICE(FILEREADFAILED) is signalled.
C
C     3) If there is a problem writing to the text file, the error
C        SPICE(FILEWRITEFAILED) is signalled.
C
C$ Files
C
C     HANDLE      is the handle assigned to the binary SPK or CK file.
C                 Use DAFOPR to open it for read access and get its
C                 handle unless SPKLEF or CKLPF has already been called
C                 and returned the handle.  This file is unchanged by
C                 calling SPCEC.
C
C     UNIT        is the logical unit connected to the text file which
C                 has been opened for write access.  Use TXTOPN to
C                 open the file and get its logical unit.  Upon exit,
C                 this file will contain the text from the comment
C                 area of the binary SPK or CK file, beginning at
C                 the line that was the position of the file pointer
C                 when SPCEC was called.  In other words, SPCEC does
C                 not rewind or backspace this file before writing
C                 the text to it.
C
C$ Particulars
C
C     The structure of SPK and CK files accommodates comments in
C     addition to data.  The following three routines are available
C     for accessing the comment area of a binary SPK or CK file:
C
C           SPCAC           add comments
C
C           SPCEC           extract comments
C
C           SPCDC           delete comments
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
C     code fragment stores the contents of the comment area of A.BSP
C     in a text file called COMMENTS.TXT and surrounds the comments
C     with markers.
C
C            CALL DAFOPR ( 'A.BSP', HANDLE )
C
C            CALL TXTOPN ( 'COMMENTS.TXT', UNIT )
C
C            WRITE (UNIT,*) '\begincomments'
C
C            CALL SPCEC  ( HANDLE, UNIT )
C
C            WRITE (UNIT,*) '\endcomments'
C
C$ Restrictions
C
C     1)  Use TXTOPN to open new text files and get their logical unit.
C         There are system dependencies regarding opening text files,
C         and these have been isolated in the routines TXTOPN and
C         TXTOPR.
C
C     2)  This routine assumes that the comment area of the binary SPK
C         or CK file contains only text stored by SPCAC.  Comments
C         written any other way may not be handled properly.
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
C     J.E. McLean (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.0.0, 16-NOV-2001 (FST)
C
C        Updated this routine to utilize new handle manager
C        interfaces.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 05-APR-1991 (JEM)
C
C-&
 
C$ Index_Entries
C
C     extract comments from spk or ck file
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 2.0.0, 16-NOV-2001 (FST)
C
C        The call to DAFHLU has been replaced with a call to
C        ZZDDHHLU, the handle manager interface for retrieving
C        a logical unit.  DAFHLU is no longer used, since it
C        locks the unit returned to its HANDLE, tying up resources
C        in the handle manager.  A call to DAFSIH was inserted to
C        make certain that HANDLE is present in DAFAH's file table.
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
      CHARACTER*(IFNLEN)    IFNAME
      CHARACTER*(MAXCPR)    LINE
      CHARACTER*(MAXCPR)    RECORD
      CHARACTER*(1)         EOT
      CHARACTER*(1)         NULL
 
      INTEGER               BWARD
      INTEGER               C
      INTEGER               DAFU
      INTEGER               FREE
      INTEGER               FWARD
      INTEGER               IOSTAT
      INTEGER               ND
      INTEGER               NI
      INTEGER               NRR
      INTEGER               POS
      INTEGER               REC
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SPCEC' )
      END IF
 
C
C     First, check to see if HANDLE is a legitimate DAF handle.
C
      CALL DAFSIH ( HANDLE, 'READ' )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'SPCEC' )
         RETURN
      END IF
 
C
C     Read the file record to find out if the DAF contains any
C     reserved records.  The reserved records in an array file
C     are stored between the first record and the first summary
C     record.  FWARD is the record number of that first summary
C     record, and NRR is the number of reserved records in the file.
C     If there are no reserved records, there's nothing to be done.
C
      CALL DAFRFR ( HANDLE, ND, NI, IFNAME, FWARD, BWARD, FREE )
      NRR = FWARD - 2
 
      IF ( NRR .EQ. 0 ) THEN
         CALL CHKOUT ( 'SPCEC' )
         RETURN
      END IF
 
C
C     We need to read directly from the SPK or CK file, using a logical
C     unit instead of a handle.
C
      CALL ZZDDHHLU ( HANDLE, 'DAF', .FALSE., DAFU )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'SPCEC' )
         RETURN
      END IF
 
C
C     Load the contents of the reserved records into individual lines,
C     for printing.  Keep adding characters to the current line until
C     it has been filled, then write it to the text file, and
C     begin a new line.
C
C     In the comment area, NULL means end-of-line, and EOT means
C     end-of-transmission, or in other words, end-of-comments.
C
      NULL = CHAR ( 0 )
      EOT  = CHAR ( 4 )
 
      LINE   = ' '
      RECORD = ' '
      POS    = 0
 
      DO REC = 1, NRR
 
         READ ( DAFU, REC=REC+1, IOSTAT=IOSTAT ) RECORD
 
         IF ( IOSTAT .NE. 0 ) THEN
            CALL SETMSG ( 'Error reading comment area of the binary ' //
     .                    'file named FNM.  Value of IOSTAT is #.'    )
            CALL ERRINT ( '#', IOSTAT                                 )
            CALL ERRFNM ( 'FNM', DAFU                                 )
            CALL SIGERR ( 'SPICE(FILEREADFAILED)'                     )
            CALL CHKOUT ( 'SPCEC'                                     )
            RETURN
         END IF
 
         DO C = 1, MAXCPR
 
C
C           End-of-transmission means we're done.
C
            IF ( RECORD(C:C) .EQ. EOT ) THEN
 
               CALL CHKOUT ( 'SPCEC' )
               RETURN
 
C
C           NULL means that the current line is ready to be written to
C           the text file.  The end-of-line character itself does not
C           get written.  After this, the current line should be empty
C           again.
C
            ELSE IF ( RECORD(C:C) .EQ. NULL ) THEN
 
               IF ( POS .EQ. 0 ) THEN
                  WRITE ( UNIT, *, IOSTAT=IOSTAT )
               ELSE
                  WRITE ( UNIT, FMT='(A)', IOSTAT=IOSTAT ) LINE( :POS)
               END IF
 
               IF ( IOSTAT .NE. 0 ) THEN
                  CALL SETMSG ( 'Error writing to the text file '     //
     .                          'named FNM.  Value of IOSTAT is #.'   )
                  CALL ERRINT ( '#', IOSTAT                           )
                  CALL SIGERR ( 'SPICE(FILEWRITEFAILED)'              )
                  CALL CHKOUT ( 'SPCEC'                               )
                  RETURN
               END IF
 
               LINE = ' '
               POS  = 0
 
C
C           If this a normal character, add it to the current line.
C
            ELSE
 
               POS           = POS + 1
               LINE(POS:POS) = RECORD(C:C)
 
            END IF
 
         END DO
 
      END DO
 
      CALL CHKOUT ( 'SPCEC' )
      RETURN
      END
