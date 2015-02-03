C$Procedure      SUMEK ( Summarize the tables in a binary EK file )
 
      SUBROUTINE SUMEK ( HANDLE, BINFNM, LOGFIL, LOGLUN )
 
C$ Abstract
C
C     Summarize the tables in a binary E-Kernel file sequentially.
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
C     EK Required Reading
C
C$ Keywords
C
C     None.
C
C$ Declarations

      IMPLICIT NONE
 
      INTEGER               HANDLE
      CHARACTER*(*)         BINFNM
      LOGICAL               LOGFIL
      INTEGER               LOGLUN
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C      HANDLE    I   File handle for the E-kernal to be summarized.
C      LOGFIL    I   Write the summary to a log file and to screen?
C      LOGLUN    I   Logical unit connected to the log file.
C
C$ Detailed_Input
C
C     HANDLE   The file handle of a previously opened binary E-Kernel
C              file whose summary is desired.
C
C     LOGFIL     if TRUE means that the summary will be written to
C                a log file as well as displayed on the terminal
C                screen.  Otherwise, the summary will not be written
C                to a file.
C
C     LOGLUN     is the logical unit connected to a log file to which
C                the summary is to be written if LOGFIL is TRUE.
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
C     1)   If an unrecognized data type is encountered in the file,
C          the error SPICE(UNKNOWNDATATYPE) will be signalled.
C
C     2)   If the binary file attached to HANDLE is not open for
C          reading, an error will be signalled by a routine called by
C          this routine.
C
C     3)   If an error occurs while writing the summary to UNIT, an
C          error will be signalled by a routine called by this routine.
C
C     4)   If a leapseconds file has not been loaded before this routine
C          is called, an error will be signalled by a routine called
C          by this routine.
C
C$ Files
C
C     See parameters HANDLE and STDOUT in the $ Detailed_Inputs section.
C
C$ Particulars
C
C     This routine provides a mechanism for summarizing a binary
C     E-Kernel file. For each table in the file a brief summary
C     of its characteristics will be written to UNIT.
C
C     This routine is for use with the SPACIT utility program.
C
C$ Examples
C
C     Assume that a SPICE leapseconds file has been loaded into the
C     SPICELIB kernel pool, and let
C
C           HANDLE   be the file handle of a previously opened binary
C                    E-Kernel file.
C
C           STDOUT   be the logical unit to which the summary is to be
C                    written.
C
C     Then the subroutine call:
C
C        CALL SUMEK ( HANDLE, STDOUT )
C
C     will collect summary information for each table in a binary
C     E-Kernel file and write it to the logical unit STDOUT.
C
C$ Restrictions
C
C     1) Beware having more columns in an E-Kernel table than you have
C        array storage allocated. The E-Kernel sugment summary routine
C        called by this routine does not know about the amount of space
C        the caller has allocated for storing the columns that it gets.
C        Currently, there is room for MAXCOL columns. See the $ Local
C        Parameters section below.
C
C     2) This routine assumes that a SPICE leapseconds file has
C        already been loaded into the SPICELIB kernel pool.
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
C-    Beta Version 1.1.0, 25-APR-1994 (KRG)
C
C        Added two now calling arguments and modified the dispaly 
C        algorithm to display a table to the logfile immediately after 
C        it displays one to the screen.
C
C-    Beta Version 1.1.0, 25-APR-1994 (KRG)
C
C        Removed the unused variable EKNAME from the declarations.
C
C-    Beta Version 1.0.0, 06-JAN-1993 (KRG)
C
C-&
 
C$ Index_Entries
C
C      summarize the tables in a binary ek file
C
C-&
 
C$ Revisions
C
C-    Beta Version 1.1.0, 25-APR-1994 (KRG)
C
C        Removed the unused variable EKNAME from the declarations.
C
C-    Beta Version 1.0.0, 06-JAN-1993 (KRG)
C
C-&
 
C
C     SPICELIB functions
C
      LOGICAL               FAILED
      LOGICAL               RETURN
C
C     Other functions
C
      INTEGER               EKNSEG
C
C     Local parameters
C
C     Parameters from the following files are used. See these files for 
C     descriptions of their parameters.
C
      INCLUDE 'ekbool.inc'
      INCLUDE 'ekcnamsz.inc'
      INCLUDE 'ekcoldsc.inc'
      INCLUDE 'eksegdsc.inc'
      INCLUDE 'ektnamsz.inc'
C
C     Set the maximum number of columns we allow in a single EK table.
C
      INTEGER               MAXCOL
      PARAMETER           ( MAXCOL = 100 )
C
C     Set the length of a text line.
C
      INTEGER               LINLEN
      PARAMETER           ( LINLEN = 255 )
C
C     Set the size for the character type equivalents.
C
      INTEGER               TYPLEN
      PARAMETER           ( TYPLEN = 4 )
C
C     Set up the page size for displaying headings for the EK column
C     description table.
C
      INTEGER               LINPPG
      PARAMETER           ( LINPPG = 22 )
C
C     Set up a mnemonic for displaying the table description data.
C
      INTEGER               DSCIDX
      PARAMETER           ( DSCIDX = 16 )
C
C     Set up some mnemonics for the display of the table's columnar
C     data
C
      INTEGER               MYNMID
      PARAMETER           ( MYNMID = 4 )

      INTEGER               MYTPID
      PARAMETER           ( MYTPID = 37 )

      INTEGER               MYLNID
      PARAMETER           ( MYLNID = 42 )

      INTEGER               MYFXID
      PARAMETER           ( MYFXID = 49 )

      INTEGER               MYSZID
      PARAMETER           ( MYSZID = 55 )

      INTEGER               MYINID
      PARAMETER           ( MYINID = 66 )

      INTEGER               MYNLID
      PARAMETER           ( MYNLID = 72 )
C
C     Set up some mnemonics for the data types used in an EK file.
C
      INTEGER               CHR
      PARAMETER           ( CHR    = 1       )
 
      INTEGER               DP
      PARAMETER           ( DP     = CHR + 1 )
 
      INTEGER               INT
      PARAMETER           ( INT    = DP + 1  )
 
      INTEGER               TIME
      PARAMETER           ( TIME   = INT + 1 )
C
C     Set the maximum number of data types.
C
      INTEGER               MAXTYP
      PARAMETER           ( MAXTYP = TIME )
C
C     Parameter for the standard output unit.
C
      INTEGER               STDOUT
      PARAMETER           ( STDOUT = 6 )
C
C     Set the length for an output line.
C
      INTEGER               OUTLEN
      PARAMETER           ( OUTLEN = 80 )
C
C     Set value for a separator
C
      CHARACTER*(*)         STARS
      PARAMETER           ( STARS  = '********************' )
C
C     Set up some character string equivalents for some things.
C
      CHARACTER*(*)         YES
      PARAMETER           ( YES    = 'Y'    )
 
      CHARACTER*(*)         NO
      PARAMETER           ( NO     = 'N'    )
 
      CHARACTER*(*)         NOTAPP
      PARAMETER           ( NOTAPP = 'n/a'  )
C
C     Set up labels for various output things.
C
      CHARACTER*(*)         FNMLBL
      PARAMETER           ( FNMLBL = 'Summary for EK file: #' )
C
C     Local variables
C
      CHARACTER*(CNAMSZ)    CNAMES(MAXCOL)
      CHARACTER*(LINLEN)    LINE
      CHARACTER*(TNAMSZ)    TABNAM
      CHARACTER*(TYPLEN)    STRTYP(MAXTYP)
      CHARACTER*(OUTLEN)    SUMSEP
      CHARACTER*(OUTLEN)    SEPAR
      CHARACTER*(LINLEN)    FNMOUT
 
      INTEGER               CDSCRS(CDSCSZ,MAXCOL)
      INTEGER               I
      INTEGER               J
      INTEGER               LINCNT
      INTEGER               MARK
      INTEGER               NCOLS
      INTEGER               NROWS
      INTEGER               NSEGS
      INTEGER               SEGDSC(SDSCSZ)
C
C     Initial Values
C
C     Set up some character string equivalents for the data types.
C
      DATA                  STRTYP(CHR ) / 'CHR ' /
      DATA                  STRTYP(DP  ) / 'DP  ' /
      DATA                  STRTYP(INT ) / 'INT ' /
      DATA                  STRTYP(TIME) / 'TIME' /
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SUMEK' )
      END IF
C
C     Initialize the separator.
C
      SEPAR = STARS // STARS // STARS // STARS
C
C     Initialize the table separator.
C
      SUMSEP = '----------------------------------------' //
     .         '----------------------------------------'
C
C     Verify that the file attached to HANDLE is opened for reading
C     by calling the routine to signal an invalid access mode on a
C     handle.
C
      CALL DASSIH ( HANDLE, 'READ' )
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'SUMEK' )
         RETURN
      END IF

      CALL REPMC ( FNMLBL, '#', BINFNM, FNMOUT )
      CALL WRITLN ( ' ',   STDOUT )
      CALL WRITLN ( SEPAR, STDOUT )
      CALL WRITLN ( ' ',   STDOUT )
      CALL WRITLN ( FNMOUT, STDOUT )
      CALL WRITLN ( ' ',    STDOUT )

      IF ( LOGFIL ) THEN
         CALL WRITLN ( ' ',    LOGLUN )
         CALL WRITLN ( SEPAR,  LOGLUN )
         CALL WRITLN ( ' ',    LOGLUN )
         CALL WRITLN ( FNMOUT, LOGLUN )
         CALL WRITLN ( ' ',    LOGLUN )
      END IF
C
C     Get the number of tables in the EK file, and write it.
C
      NSEGS = EKNSEG ( HANDLE )

      LINE  = 'Number of Tables: #'
      CALL REPMI  ( LINE, '#', NSEGS, LINE )

      CALL WRITLN ( LINE, STDOUT           )
      CALL WRITLN ( ' ',  STDOUT           )
      IF ( LOGFIL ) THEN
         CALL WRITLN ( LINE, LOGLUN )
         CALL WRITLN ( ' ',  LOGLUN )
      END IF

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'SUMEK' )
         RETURN
      END IF
C
C     Loop through all of the tables in the EK file, writing out the
C     summary information for each of them.
C
      DO I = 1, NSEGS
C
C        Get the summary information for the EK table with index I.
C
         CALL ZZEKSINF ( HANDLE, I, TABNAM, SEGDSC, CNAMES, CDSCRS  )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'SUMEK' )
            RETURN
         END IF
C
C        Write out the current table number.
C
         LINE = 'Table number: #'
         CALL REPMI  ( LINE, '#', I, LINE )
         CALL WRITLN ( LINE, STDOUT       )
         IF ( LOGFIL ) THEN
            CALL WRITLN ( LINE, LOGLUN )
         END IF
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'SUMEK' )
            RETURN
         END IF
C
C        Write out a line that marks the beginning of the table.
C
         CALL WRITLN ( SUMSEP, STDOUT )
         IF ( LOGFIL ) THEN
            CALL WRITLN ( SUMSEP, LOGLUN )
         END IF

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'SUMEK' )
            RETURN
         END IF
C
C        Write out the table name.
C
         MARK        = DSCIDX
         LINE        = '   Table Name:'
         LINE(MARK:) = '#'
         CALL REPMC  ( LINE, '#', TABNAM, LINE )
         CALL WRITLN ( LINE, STDOUT           )
         IF ( LOGFIL ) THEN
            CALL WRITLN ( LINE, LOGLUN )
         END IF
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'SUMEK' )
            RETURN
         END IF
C
C        Write out the number of rows in the table.
C
         MARK        = DSCIDX
         NROWS       = SEGDSC(NRIDX)
         LINE        = '   Rows      :'
         LINE(MARK:) = '#'
         CALL REPMI  ( LINE, '#', NROWS, LINE )
         CALL WRITLN ( LINE, STDOUT           )
         IF ( LOGFIL ) THEN
            CALL WRITLN ( LINE, LOGLUN )
         END IF
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'SUMEK' )
            RETURN
         END IF
C
C        Write out the number of columns in the table.
C
         MARK        = DSCIDX
         NCOLS       = SEGDSC(NCIDX)
         LINE        = '   Columns   :'
         LINE(MARK:) = '#'
         CALL REPMI  ( LINE, '#', NCOLS, LINE )
         CALL WRITLN ( LINE, STDOUT           )
         IF ( LOGFIL ) THEN
            CALL WRITLN ( LINE, LOGLUN)
         END IF
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'SUMEK' )
            RETURN
         END IF
C
C        Begin writing out the columns for this table.
C
         LINCNT = 0
         DO J = 1, NCOLS
C
C           If we need to, write out some headings for the EK column
C           description table.
C
            IF ( LINCNT .EQ. 0 ) THEN

               CALL WRITLN( ' ', STDOUT )

               LINE = ' '
               LINE(MYNMID:) = '#'
               CALL REPMC ( LINE, '#', 'Column Name', LINE )

               LINE(MYTPID:) = '#'
               CALL REPMC ( LINE, '#', 'Type', LINE )

               LINE(MYLNID:) = '#'
               CALL REPMC ( LINE, '#', 'Length', LINE )

               LINE(MYFXID:) = '#'
               CALL REPMC ( LINE, '#', 'Fixed', LINE )

               LINE(MYSZID:) = '#'
               CALL REPMC ( LINE, '#', 'Size', LINE )

               LINE(MYINID:) = '#'
               CALL REPMC ( LINE, '#', 'Index', LINE )

               LINE(MYNLID:) = '#'
               CALL REPMC ( LINE, '#', 'Null', LINE )

               CALL WRITLN ( LINE, STDOUT )
               IF ( LOGFIL ) THEN
                  CALL WRITLN ( LINE, LOGLUN)
               END IF
C
C              Write out a seperator line for the table headings.
C
               LINE = '   -------------------------------- ----' //
     .                ' ------ ----- ---------- ----- ----'

               CALL WRITLN ( LINE, STDOUT )
               IF ( LOGFIL ) THEN
                  CALL WRITLN ( LINE, LOGLUN )
               END IF
               IF ( FAILED() ) THEN
                  CALL CHKOUT ( 'SUMEK' )
                  RETURN
               END IF

            END IF

            LINCNT = LINCNT + 1
C
C           Place the column name into the output line.
C
            LINE(MYNMID:) = '#'
            CALL REPMC( LINE, '#', CNAMES(J), LINE )
C
C           Place the column data type and length, if applicable, into
C           the output line.
C
            LINE(MYTPID:) = '#'
            IF ( CDSCRS(TYPIDX,J) .EQ. CHR ) THEN

               CALL REPMC ( LINE, '#', STRTYP(CHR), LINE )
               LINE(MYLNID:) = '#'
               IF ( CDSCRS(LENIDX,J) .EQ. IFALSE ) THEN
                  CALL REPMC ( LINE, '#', 'Var', LINE )
               ELSE
                  CALL REPMI ( LINE, '#', CDSCRS(LENIDX,J), LINE )
               END IF

            ELSE IF ( CDSCRS(TYPIDX,J) .EQ. DP ) THEN

               CALL REPMC ( LINE, '#', STRTYP(DP), LINE )
               LINE(MYLNID:) = '#'
               CALL REPMC ( LINE, '#', NOTAPP, LINE )

            ELSE IF ( CDSCRS(TYPIDX,J) .EQ. INT ) THEN

               CALL REPMC ( LINE, '#', STRTYP(INT), LINE )
               LINE(MYLNID:) = '#'
               CALL REPMC ( LINE, '#', NOTAPP, LINE )

            ELSE IF ( CDSCRS(TYPIDX,J) .EQ. TIME ) THEN

               CALL REPMC ( LINE, '#', STRTYP(TIME), LINE )
               LINE(MYLNID:) = '#'
               CALL REPMC ( LINE, '#', NOTAPP, LINE )

            ELSE

               CALL SETMSG ( 'Unknown data type encountered,'   //
     .                       ' value: #'                         )
               CALL ERRINT ( '#', CDSCRS(TYPIDX,J)               )
               CALL SIGERR ( 'SPICE(UNKNOWNDATATYPE)'            )
               CALL CHKOUT ( 'SUMEK'                             )
               RETURN

            END IF
C
C           Place the fixed length column indicator and the length, when
C           applicable, into the output line.
C
            LINE(MYFXID:) = '#'
            IF ( CDSCRS(SIZIDX,J) .EQ. IFALSE ) THEN

               CALL REPMC ( LINE, '#', NO, LINE )
               LINE(MYSZID:) = '#'
               CALL REPMC ( LINE, '#', NOTAPP, LINE )

            ELSE

               CALL REPMC ( LINE, '#', YES, LINE )
               LINE(MYSZID:) = '#'
               CALL REPMI ( LINE, '#', CDSCRS(SIZIDX,J), LINE )

            END IF
C
C           Place the indexed column indicator into the output line.
C
            LINE(MYINID:) = '#'
            IF ( CDSCRS(IXTIDX,J) .NE. IFALSE ) THEN
               CALL REPMC ( LINE, '#', YES, LINE )
            ELSE
               CALL REPMC ( LINE, '#', NO, LINE )
            END IF
C
C           Place the null values column indicator into the output line.
C
            LINE(MYNLID:) = '#'
            IF ( CDSCRS(NFLIDX,J) .NE. IFALSE ) THEN
               CALL REPMC ( LINE, '#', YES, LINE )
            ELSE
               CALL REPMC ( LINE, '#', NO, LINE )
            END IF
C
C           Finally, write out the column description.
C
            CALL WRITLN ( LINE, STDOUT )
            IF ( LOGFIL ) THEN
               CALL WRITLN ( LINE, LOGLUN)
            END IF
            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'SUMEK' )
               RETURN
            END IF
C
C           Reset the line counter if we need to so that we will use the
C           table headings again.
C
            IF ( LINCNT .EQ. LINPPG ) THEN
               LINCNT = 0
            END IF
C
C        If there was some sort of errer, then check out and return.
C
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'SUMEK' )
            RETURN
         END IF

         END DO
C
C        Write out a line that marks the end of the table, and then
C        skip a line.
C
         CALL WRITLN ( SUMSEP, STDOUT )
         CALL WRITLN ( ' ',    STDOUT )
         IF ( LOGFIL ) THEN
            CALL WRITLN ( SUMSEP, LOGLUN )
            CALL WRITLN ( ' ',    LOGLUN )
         END IF
C
C        If there was some sort of error, then check out and return.
C
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'SUMEK' )
            RETURN
         END IF

      END DO

      CALL WRITLN ( SEPAR, STDOUT )
      CALL WRITLN ( ' ',   STDOUT )
      IF ( LOGFIL ) THEN
         CALL WRITLN ( SEPAR, LOGLUN )
         CALL WRITLN ( ' ',   LOGLUN )
      END IF
 
      CALL CHKOUT ( 'SUMEK' )
      RETURN
      END
