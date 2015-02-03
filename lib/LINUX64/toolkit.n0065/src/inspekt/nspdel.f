      SUBROUTINE NSPDEL( IDLIST, N, FROM, TO, EVERY )
C
C$ Abstract
C
C     Print a delimited report from Inspekt
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
      INTEGER               IDLIST ( 0 : * )
      INTEGER               N
      INTEGER               FROM
      INTEGER               TO
      INTEGER               EVERY
 
      CHARACTER*(*)         RNAME
      PARAMETER           ( RNAME = 'NSPDEL' )
 
      INCLUDE              'longline.inc'
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     IDLIST     I   Idcodes for the columns to be reported
C     N          I   Number of matching rows
C     FROM       I   First row to print
C     TO         I   Last  row to print
C     EVERY      I   Increment between rows
C
C
C$ Detailed_Input
C
C     IDLIST     is an array of id codes for the various columns that
C                are to be printed in the report.
C
C     N          is the number of columns to appear in the report.
C
C     FROM       is the first row of the set of matching rows to
C                display
C
C     TO         is the last row of the set of matching rows to
C                display.
C
C     EVERY      is the number of matching rows to skip between the
C                rows that are displayed.
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
C     None.
C
C
C$ Particulars
C
C     This routine prints the results of an Inspekt query in a delimited
C     format suitable for importing into excel  or other program.
C
C$ Examples
C
C    Sorry Charlie, you're on your own here.
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
C-    Inspekt Version 1.0.0, 26-MAR-2003
C
C
C-&
 
C
C     Spicelib Functions
C
      LOGICAL               RETURN
      INTEGER               LASTNB
      INTEGER               POS
C
C     Parameters
C
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE  = 4 )
 
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE  = 80 )
 
      CHARACTER*(LNSIZE)    FMT
      CHARACTER*(LNSIZE)    NAME
      CHARACTER*(LNGSIZ)    TITLE
      CHARACTER*(WDSIZE)    DELIM
      CHARACTER*(WDSIZE)    QUOTE
      CHARACTER*(WDSIZE)    DEL
 
      INTEGER               K
      INTEGER               PUT
      INTEGER               I
      INTEGER               J
      INTEGER               COUNT
      INTEGER               SKIP
 
      LOGICAL               MERGE
      LOGICAL               FOUND
 
 
      IF ( RETURN() ) THEN
         RETURN
      END IF
 
      CALL CHKIN ( RNAME )
C
C     Look up the delimiter to use as well as the character to use to
C     quote strings.
C
      CALL BBGETC_1 ( 'COPY', 'DELIMITER', K, DELIM )
      CALL BBGETC_1 ( 'COPY', 'QUOTE',     K, QUOTE )
      CALL BBGETC_1 ( 'COPY', 'FORMAT',    K, FMT   )
C
C     Determine whether or not to put each componenent in
C     a separate row.
      MERGE = POS( FMT, 'PRESERVED', 0 ) .EQ. 0
 
      IF ( DELIM .EQ. 'TAB' ) THEN
         DEL = CHAR(9)
      ELSE IF ( DELIM .EQ. 'SPACE' ) THEN
         DEL = ' '
      ELSE
         DEL = DELIM
      END IF
C
C     Write out the line with the column headings.
C
      TITLE = ' '
      PUT   = 1
      DO I  = 1, N
 
         CALL CLGQAL ( IDLIST(I), NAME )
         CALL SETCHR( QUOTE, PUT, TITLE )

         DO J = 1, LASTNB(NAME)
 
            CALL SETCHR( NAME(J:J), PUT, TITLE )
            IF ( NAME(J:J) .EQ. QUOTE ) THEN
               CALL SETCHR( QUOTE, PUT, TITLE )
            END IF
         END DO
 
         CALL SETCHR( QUOTE, PUT, TITLE )
         IF ( I .NE. N ) THEN
            CALL SETCHR( DEL,   PUT, TITLE )
         END IF
         
      END DO
 
      CALL NSPWLN ( TITLE )
 
C
C     Advance to the first row of the current scope of the query.
C
 
 
      DO I = 1, FROM
         CALL CLADV ( FOUND )
      END DO
 
      COUNT = FROM
 
      DO WHILE ( FOUND )
 
         IF ( .NOT. MERGE ) THEN 
            CALL WRTPRS ( IDLIST, N, DEL, QUOTE )
         ELSE
            CALL WRTNPR ( IDLIST, N, DEL, QUOTE )
         END IF
 
 
         SKIP = 0
         DO WHILE ( FOUND .AND. SKIP .LT. EVERY )
            CALL CLADV ( FOUND )
            SKIP  = SKIP  + 1
            COUNT = COUNT + 1
         END DO
 
         IF ( COUNT .GT. TO ) THEN
            FOUND = .FALSE.
         END IF
 
      END DO
 
      CALL CHKOUT ( RNAME )
      RETURN
      END
