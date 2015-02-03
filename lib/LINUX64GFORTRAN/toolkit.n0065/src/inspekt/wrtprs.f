      SUBROUTINE WRTPRS ( IDLIST, N, DEL, QUOTE )
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
      INTEGER               LASTNB

      INTEGER               CH
      PARAMETER           ( CH   = 1 )
 
      INTEGER               DP
      PARAMETER           ( DP   = 2 )
 
      INTEGER               INT
      PARAMETER           ( INT  = 3 )
 
      INTEGER               TIME
      PARAMETER           ( TIME = 4 )
      
      CHARACTER*(LNGSIZ)    SUBROW
      CHARACTER*(LNGSIZ)    LINE
      CHARACTER*(LNGSIZ)    TEMP
 
 
 
      INTEGER               DUMMY
      INTEGER               GET
      INTEGER               I
      INTEGER               MYID
      INTEGER               LAST
      INTEGER               NROWS
      INTEGER               NUM
      INTEGER               PUT
      INTEGER               ROW
      INTEGER               THSTYP
      INTEGER               WIDTH
      
      LOGICAL               DOQUOT
      LOGICAL               EMPTY

C
C     In this case we are going to print out a sub-row at a time
C     Each new component of a column will appear in a subsequent
C     row.  If there is no component with the specified index,
C     we will leave a blank in that spot.
C
C     Determine how many sub-rows are required for this matching
C     row of  the query.
C
      NROWS = 0
 
      DO I = 1, N
         CALL CLNCMP ( IDLIST(I), NUM )
         NROWS = MAX( NROWS, NUM )
      END DO
C
C     Now form each sub-row.
C
      DO ROW = 1, NROWS
C
C        This subrow starts out empty.
C
         SUBROW = ' '
         PUT    = 1
         
C
C        Keep track of whether we put any real data into this line.
C
         EMPTY  = .TRUE.
 
         DO I = 1,N
C
C           Fetch the number of components of the next column to
C           see if we are going to leave a blank or not.
C
            CALL CLNCMP( IDLIST(I),  NUM )
C
C           Also determine which type of column we are dealing with so
C           we can determine whether or not to quote it.
C
            CALL CLQ2ID ( IDLIST(I),  MYID )
            CALL CLGAI  ( MYID,  'TYPE', DUMMY, THSTYP )
 
            DOQUOT = THSTYP .NE. INT .AND. THSTYP .NE. DP
 
            IF ( NUM .GE. ROW ) THEN
               CALL CLPVAL( IDLIST(I), ROW, LINE, WIDTH )

               IF ( LINE .EQ. '<null>' .OR. LINE .EQ. '<absent>' ) THEN
                  DOQUOT = .TRUE.
               END IF
               
               LAST = LASTNB( LINE )

C               
C              An empty string is allowed for non-null character
C              coumns, but not for any others.
C
               IF ( THSTYP .EQ. CH .AND. ROW .GT. 1 ) THEN
                  CALL CLPVAL( IDLIST(I), 1, TEMP, WIDTH )
                  IF (       TEMP .NE. '<null>'
     .                 .AND. TEMP .NE. '<absent>' ) THEN
                     EMPTY = .FALSE.
                  END IF
               ELSE
                  EMPTY = EMPTY .AND. LINE .EQ. ' '
               END IF
            ELSE
               LINE = ' '
               LAST = 1
            END IF
 
 
 
            IF ( DOQUOT ) THEN
               CALL SETCHR( QUOTE, PUT, SUBROW )
            END IF
 
            DO GET = 1, LAST
 
               CALL SETCHR( LINE(GET:GET), PUT, SUBROW )
 
               IF ( DOQUOT .AND. LINE(GET:GET) .EQ. QUOTE ) THEN
                  CALL SETCHR( QUOTE, PUT, SUBROW )
               END IF
 
            END DO
 
            IF ( DOQUOT ) THEN
               CALL SETCHR( QUOTE, PUT, SUBROW )
            END IF
 
            IF ( I .NE. N ) THEN
               CALL SETCHR( DEL, PUT, SUBROW )
            END IF
 
         END DO
C
C        Ship this subrow out if it is non-empty.
C
         IF ( .NOT. EMPTY ) THEN
            CALL NSPWLN ( SUBROW )
         END IF
C
C        Get the next one (if there is one )
C
      END DO
 
      RETURN
      END
