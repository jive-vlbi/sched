      SUBROUTINE WRTNPR ( IDLIST, N, DEL, QUOTE )
C
C$ Abstract
C
C     Write out the current row of the current selection set preserving
C     components in separate rows.
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
C     None.
C
C$ Keywords
C
C     INSPEKT
C
C$ Declarations
 
      IMPLICIT NONE
      INTEGER               IDLIST ( 0: * )
      INTEGER               N
      CHARACTER*(1)         DEL
      CHARACTER*(1)         QUOTE
 
      INCLUDE              'longline.inc'
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     IDLIST     I   List of column ids for use in this report.
C     N          I   Number of columns to be output.
C     DEL        I   Character used to delimit colums.
C     QUOTE      I   Character used to quote strings.
C
C$ Detailed_Input
C
C     IDLIST     is the list of column idcodes that are used to
C                for the various subrows of this row of the matching
C                query.
C
C     N          The number of columns in IDLIST
C
C     DEL        Delimiter to use between columns.  You can use a space
C                but it kind of defeats the purpose of this routine.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     None.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C$ Particulars
C
C     This is simply a formatter.  Each component appears in an output
C     row by itself.
C
C$ Examples
C
C     Nope.
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     W.L. Taber      (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 27-MAR--2003 (WLT)
C
C
C-&
C
C     SPICELIB Functions
C
      INTEGER               LASTNB
      INTEGER               RTRIM
 
      INTEGER               CH
      PARAMETER           ( CH   = 1 )
 
      INTEGER               DP
      PARAMETER           ( DP   = 2 )
 
      INTEGER               INT
      PARAMETER           ( INT  = 3 )
 
      INTEGER               TIME
      PARAMETER           ( TIME = 4 )
 
      CHARACTER*(LNGSIZ)    ROW
      CHARACTER*(LNGSIZ)    LINE
      CHARACTER*(LNGSIZ)    TEMP
 
      INTEGER               NUM
      INTEGER               J
      INTEGER               I
      INTEGER               LIMIT
      INTEGER               MYID
      INTEGER               PUT
      INTEGER               K
      INTEGER               WIDTH
      INTEGER               DUMMY
      INTEGER               THSTYP
 
      LOGICAL               DOQUOT
      LOGICAL               NULL
 
 
      ROW = ' '
      PUT = 1
 
      DO I = 1, N

         CALL CLQ2ID ( IDLIST(I),  MYID )
         CALL CLNCMP ( IDLIST(I),  NUM )
         CALL CLGAI  ( MYID,  'TYPE', DUMMY, THSTYP )
 
         DOQUOT = THSTYP .NE. INT .AND. THSTYP .NE. DP
         TEMP    = ' '
C
C        Get the print value for each component of the current column of
C        this row.  Append all of the print values together
C
         IF ( DOQUOT ) THEN
            CALL SETCHR( QUOTE, PUT, ROW )
         END IF

         NULL = .FALSE.
         
         DO J = 1, NUM
            LINE = ' '
            CALL CLPVAL ( IDLIST(I), J,  LINE, WIDTH  )

C            
C           If we get a null or absent value, we have to quote this
C           item.  If we haven't already done so, we still have time.
C
            IF ( LINE .EQ. '<null>' .OR. LINE .EQ. '<absent>' ) THEN
               IF ( .NOT. DOQUOT ) THEN
                  DOQUOT = .TRUE.
                  CALL SETCHR( QUOTE, PUT, ROW )
               END IF
               NULL = .TRUE.
            END IF

            IF ( J .EQ. 1 ) THEN
               LIMIT = RTRIM(LINE)
            ELSE
               LIMIT = LASTNB(LINE)               
            END IF

               
            DO K = 1, LIMIT
               CALL SETCHR( LINE(K:K), PUT, ROW )
               IF ( DOQUOT .AND. LINE(K:K) .EQ. QUOTE ) THEN
                  CALL SETCHR( QUOTE, PUT, ROW )
               END IF 
            END DO
            
            IF ( J .NE. NUM .AND. LIMIT .NE. 0 .AND. .NOT. NULL ) THEN
               CALL SETCHR ( ' ', PUT, ROW )
            END IF
 
         END DO
C
C        Move the column value constructed above into the row we've
C        been constructing --- one character at a time, doubling the
C        quote character if necessary.
C
         IF ( DOQUOT ) THEN
            CALL SETCHR( QUOTE, PUT, ROW )
         END IF
 
C
C        If this is not the last column, we need to place a delimiter
C        onto the end of the row we've constructed so far.
C
         IF ( I .NE. N ) THEN
            CALL SETCHR( DEL, PUT, ROW )
         END IF
 
      END DO
 
      CALL NSPWLN ( ROW )
 
      RETURN
      END
