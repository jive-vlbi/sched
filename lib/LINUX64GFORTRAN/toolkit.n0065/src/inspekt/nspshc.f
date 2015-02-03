C$Procedure      NSPSHC ( Inspekt Show comments)
 
      SUBROUTINE NSPSHC ( HANDLE, QUIT )
 
C$ Abstract
C
C     Send the contents of the comment section of a file to
C     the Inspekt output ports (via NICEPR).
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
C      None.
C
C$ Keywords
C
C       INSPEKT
C       COMMENTS
C
C$ Declarations
 
      IMPLICIT NONE
      INTEGER               HANDLE
      LOGICAL               QUIT
 
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      HANDLE     I   The handle of a DAS file open for reading.
C      QUIT       O   Flag indicating whether user quit reading comments
C$ Detailed_Input
C
C     HANDLE      is the handle of a DAS file open for reading.
C
C$ Detailed_Output
C
C     QUIT        is a logical indicating that the user interrupted
C                 the output of comments.
C
C$ Parameters
C
C      None.
C
C$ Files
C
C      None.
C
C$ Exceptions
C
C     Error free.
C
C     1)
C
C$ Particulars
C
C     This is a utility program for Inspekt (although with minor
C     modifications other programs might find this useful.
C
C     It takes the handle of an open DAS and sends the comments
C     a line at at time to the page manager.
C
C     If the page manager has been set up to pause after completion
C     of a page, the program determines if the user typed Q indicating
C     that s(he)'d like to stop viewing comments.
C
C     If Q is returned by the page manager, this routine abandons
C     reading and display of comments for the current file.
C
C$ Examples
C
C     Suppose you had a bunch of DAS files queued up for dumping
C     comments, and that you wanted to just send them to
C     Inspekt's output devices.  The following shows
C     how you could address the problem of a user abandoning
C     output.
C
C        DO WHILE ( more DAS files )
C
C           Get the next DAS file handle
C
C           CALL NSPSHC ( HANDLE, QUIT )
C
C           IF ( QUIT ) THEN
C              set more DAS files to false.
C           ELSE
C              determine if there are more DAS files
C           END IF
C
C        END DO
C
C     You could always get more creative and see if the user
C     was abandoning all reading or maybe just the last file.
C
C$ Restrictions
C
C     You must set up the page manager with whatever page size
C     and widths you find to be desirable.  However, this routine
C     assumes that it can get the current page width from the
C     NSPMRG routine and uses this to format strings.
C
C$ Author_and_Institution
C
C      W.L. Taber      (JPL)
C
C$ Literature_References
C
C      None.
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 24-AUG-1995 (WLT)
C
C
C-&
 
C$ Index_Entries
C
C     Display DAS comments in Inspekt.
C
C-&
C
C     SPICELIB Functions
C
      INTEGER               CARDC
      INTEGER               LTRIM
      LOGICAL               FAILED
 
 
 
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )
 
      INTEGER               BSIZE
      PARAMETER           ( BSIZE = 12 )
 
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )
 
      INTEGER               MXCREC
      PARAMETER           ( MXCREC = 1024 )
 
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 132 )
 
      INTEGER               LONGSZ
      PARAMETER           ( LONGSZ = 255 )
 
 
      INTEGER               IDSIZE
      PARAMETER           ( IDSIZE = 8 )
 
      INTEGER               IFNSIZ
      PARAMETER           ( IFNSIZ = 60 )
 
      CHARACTER*(IDSIZE)    IDWORD
      CHARACTER*(IFNSIZ)    IFNAME
      CHARACTER*(LNSIZE)    BUFFER ( LBCELL: BSIZE )
      CHARACTER*(LNSIZE)    STYLE
      CHARACTER*(LONGSZ)    LINE
      CHARACTER*(MXCREC)    CRECRD
      CHARACTER*(WDSIZE)    RESPNS
 
      INTEGER               DASLUN
      INTEGER               FETCH
      INTEGER               I
      INTEGER               J
      INTEGER               NCOMR
      INTEGER               NREC
      INTEGER               NRESVC
      INTEGER               NRESVR
      INTEGER               NULL
      INTEGER               PUTAT
      INTEGER               RECNO
      INTEGER               TOGET
 
      LOGICAL               DIDPMT
      LOGICAL               SEND
      LOGICAL               SOME
 
C
 
C
C     First determine the logical unit attached to this DAS and
C     get the attributes stored in the file record.
C
      CALL DASHLU ( HANDLE, DASLUN )
      CALL DASRFR ( HANDLE,
     .              IDWORD, IFNAME,
     .              NRESVR, NRESVC,
     .              NCOMR,  TOGET   )
 
 
 
      QUIT = .FALSE.
      NULL = 0
 
      IF ( FAILED() ) THEN
         RETURN
      END IF
C
C     The first comment record is located just after the
C     file and reserved records.
C
      RECNO  = 1 + NRESVR + 1
C
C     There is currently nothing to output.
C
      LINE   = ' '
      PUTAT  =  1
      SOME   = .FALSE.
      SEND   = .FALSE.
C
C     Get the current margins and use the null character for the
C     hard space.
C
      CALL NSPMRG ( STYLE )
      CALL SUFFIX ( 'HARDSPACE', 1, STYLE )
      CALL SUFFIX ( CHAR(NULL),  1, STYLE )
C
C     Finally, all comments go to the body section of the
C     page.
C
      CALL PAGSCN ( 'BODY' )
 
 
      DO NREC = 1, NCOMR
 
         CALL DASIOC ( 'READ', DASLUN, RECNO, CRECRD )
 
         IF ( FAILED() ) THEN
            RETURN
         END IF
 
         RECNO = RECNO + 1
 
         FETCH = MIN  (  MXCREC, TOGET )
         TOGET = TOGET - FETCH
 
         DO I = 1, FETCH
C
C           Lines in the comment area are delimited by the
C           null character (CHAR(0)).  See if we've reached
C           the end of a line.
C
            IF ( ICHAR(CRECRD(I:I)) .EQ. NULL ) THEN
 
               DO J = 1, LTRIM(LINE)-1
                  LINE(J:J) = CHAR(NULL)
               END DO
 
               CALL SSIZEC   ( BSIZE,       BUFFER )
               CALL NICEBT_1 ( LINE, STYLE, BUFFER )
 
               LINE  = ' '
               PUTAT = 1
               SOME  = .FALSE.
               SEND  = .TRUE.
 
            ELSE IF ( PUTAT .EQ. LNSIZE ) THEN
C
C              The line might fill up.  If it does we just have
C              to shop out what we have.
C
               DO J = 1, LTRIM(LINE)-1
                  LINE(J:J) = CHAR(NULL)
               END DO
 
               CALL SSIZEC   ( BSIZE,       BUFFER )
               CALL NICEBT_1 ( LINE, STYLE, BUFFER )
 
 
               LINE      = ' '
               LINE(1:1) = CRECRD(I:I)
               PUTAT     = 2
               SOME      = .TRUE.
               SEND      = .TRUE.
 
            ELSE
C
C              This is what usually happens.  Tack the next character
C              on the the end of the line and set the pointer for
C              the next place to put things.
C
               LINE(PUTAT:PUTAT) = CRECRD(I:I)
               PUTAT             = PUTAT + 1
               SOME = .TRUE.
               SEND = .FALSE.
 
            END IF
 
 
            IF ( SEND ) THEN
 
               IF ( CARDC(BUFFER) .EQ. 0 ) THEN
                  BUFFER(1) = ' '
                  CALL SCARDC ( 1, BUFFER )
               END IF
 
               DO J = 1, CARDC(BUFFER)
                  CALL PAGPUT(BUFFER(J))
                  CALL PAGPMT( DIDPMT, RESPNS )
                  CALL UCASE ( RESPNS, RESPNS )
C
C                 We give the user the chance to bail out of
C                 a dump of comments.
C
                  IF ( DIDPMT .AND. RESPNS .EQ. 'Q' ) THEN
                     QUIT = .TRUE.
                     RETURN
                  END IF
               END DO
 
            END IF
 
 
         END DO
 
 
      END DO
 
C
C     If there are some comments that have been accumulated
C     we need to send them to the page manager.
C
      IF ( SOME ) THEN
C
C        We keep the spaces at the beginning of the line
C        as hard spaces.
C
         DO J = 1, LTRIM(LINE)-1
            LINE(J:J) = CHAR(NULL)
         END DO
C
C        Use NICEBT to format this line.
C
         CALL SSIZEC   ( BSIZE,       BUFFER )
         CALL NICEBT_1 ( LINE, STYLE, BUFFER )
C
C        Now ship out the contents of BUFFER.
C
         DO J = 1, CARDC(BUFFER)
 
            CALL PAGPUT(BUFFER(J))
            CALL PAGPMT( DIDPMT, RESPNS )
            CALL UCASE ( RESPNS, RESPNS )
C
C           We give the user the chance to bail out of
C           a dump of comments.
C
            IF ( DIDPMT .AND. RESPNS .EQ. 'Q' ) THEN
               QUIT = .TRUE.
               RETURN
            END IF
         END DO
 
      END IF
 
      RETURN
      END
 
