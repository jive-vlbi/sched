C$Procedure      RDKVAR ( Read the next variable from a kernel file )
 
      SUBROUTINE RDKVAR ( TABSYM,
     .                    TABPTR,
     .                    TABVAL, NAME, EOF )
 
C$ Abstract
C
C     Read the next variable from a SPICE ASCII kernel file into a
C     double precision symbol table.
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
C     KERNEL, SYMBOLS
C
C$ Keywords
C
C     FILES
C
C$ Declarations
 
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )
 
      CHARACTER*(*)         TABSYM     ( LBCELL:* )
      INTEGER               TABPTR     ( LBCELL:* )
      DOUBLE PRECISION      TABVAL     ( LBCELL:* )
      CHARACTER*(*)         NAME
      LOGICAL               EOF
 
      INTEGER               LINLEN
      PARAMETER           ( LINLEN = 80 )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     TABSYM,
C     TABPTR,
C     TABVAL    I/O  Symbol table.
C     NAME       O   Name of the variable.
C     EOF        O   End of file indicator.
C     LINLEN     P   Maximum line length.
C
C$ Detailed_Input
C
C     TABSYM,
C     TABPTR,
C     TABVAL      are the components of a double precision symbol
C                 table. On input, the table may or may not contain
C                 any variables.
C
C$ Detailed_Output
C
C     TABSYM,
C     TABPTR,
C     TABVAL      on output, contains the name and values of the next
C                 variable in kernel file. Depending on the assignment
C                 directive, the values in the file may replace or
C                 augment any existing values.
C
C      NAME       is the name of the variable. NAME is blank if
C                 no variable is read.
C
C      EOF        is true when the end of the kernel file has been
C                 reached, and is false otherwise. The kernel file
C                 is closed automatically when the end of the file
C                 is reached.
C
C$ Parameters
C
C      LINLEN      is the maximum length of a line in the kernel file.
C
C
C$ Files
C
C     RDKVAR reads from the file most recently opened by RDKNEW.
C
C$ Exceptions
C
C     1) If an error occurs parsing a date from the kernel file, the
C        error SPICE(DATEEXPECTED) is signalled.
C
C     2) If an error occurs parsing a numeric value from the kernel
C        file, the error SPICE(NUMBEREXPECTED) is signalled.
C
C$ Particulars
C
C     None.
C
C$ Examples
C
C     In the following example, RDKNEW and RDKVAR are used to read
C     the contents of two kernel files into a single symbol table.
C     First, the table is cleared.
C
C         CALL SCARDC ( 0, TABSYM )
C         CALL SCARDI ( 0, TABPTR )
C         CALL SCARDD ( 0, TABVAL )
C
C     Next, the files are opened and read individually.
C
C         DO I = 1, 2
C            CALL RDKNEW ( KERNEL(I), EOF )
C
C            DO WHILE ( .NOT. EOF )
C               CALL RDKVAR ( TABSYM, TABPTR, TABVAL, NAME, EOF )
C            END DO
C         END DO
C
C     Let the files KERNEL(1) and KERNEL(2) contain
C
C         ===========================================================
C
C         \begindata
C         DELTA_T_A       =   32.184
C         K               =    1.657D-3
C         ORBIT_ECC       =    1.671D-2
C         MEAN_ANOM       = (  6.239996D0,  1.99096871D-7 )
C
C         ===========================================================
C
C     and
C
C         ===========================================================
C         \begindata
C          K               =    0.0D0
C         ===========================================================
C
C     respectively. Then the contents of the symbol table are
C
C          DELTA_T_A  -->   32.184
C          K          -->    0.0D0
C          MEAN_ANOM  -->    6.239996D0
C                            1.99096871D-7
C          ORBIT_ECC  -->    1.671D-2
C
C     In particular, the value of K read from the second file replaces
C     the value read from the first file.
C
C$ Restrictions
C
C     The input file must be opened and initialized by RDKNEW prior
C     to the first call to RDKVAR.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     H.A. Neilan     (JPL)
C     I.M. Underwood  (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 10-MAR-1992 (WLT)
C
C        Changed the length of the local character variable ERROR so 
C        that it would always have a length greater than the lengths of 
C        the character strng values placed into it.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
C
C-&
 
C$ Index_Entries
C
C     read the next variable from a kernel file
C
C-&
 
 
C$ Revisions
C
C-    SPICELIB Version 1.1.0, 10-MAR-1992 (WLT)
C
C        Changed the length of the local character variable ERROR so 
C        that it would always have a length greater than the lengths of 
C        the character strng values placed into it.
C
C        The length of the character variable ERROR was changed from 30 
C        to 80.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C-    Beta Version 2.0.0, 23-OCT-1989 (HAN)
C
C        Added a test to FAILED in the main DO-loop to prevent
C        infinite looping. If the error mode was set to 'RETURN'
C        and an error occurred, the same line could be processed
C        forever.
C
C-    Beta Version 1.1.0, 13-JAN-1989 (IMU)
C
C        Variable name may now take up an entire line. The previous
C        maximum length (32 characters) was tied to the known length
C        used by POOL. That length is now parameterized. Rather than
C        have two parameters, which could get out of synch, RDKVAR
C        now assumes that a variable name can be as long as an input
C        line.
C
C-&
 
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
      LOGICAL               FAILED
 
C
C     Local variables
C
      CHARACTER*1           COMMA
      PARAMETER           ( COMMA = ',' )
 
      CHARACTER*1           BLANK
      PARAMETER           ( BLANK = ' ' )
 
      CHARACTER*1           LPAREN
      PARAMETER           ( LPAREN = '(' )
 
      CHARACTER*1           RPAREN
      PARAMETER           ( RPAREN = ')' )
 
      CHARACTER*(LINLEN)    LINE
      CHARACTER*(LINLEN)    VARNAM
      CHARACTER*3           DIRCTV
      CHARACTER*6           STATUS
 
      CHARACTER*30          CVALUE
      DOUBLE PRECISION      DVALUE
      INTEGER               I
 
      CHARACTER*80          ERROR
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'RDKVAR' )
      END IF
 
C
C     No variable yet.
C
      NAME  = ' '
 
 
C
C     No parsing error has occurred yet.
C
      ERROR = ' '
 
 
C
C     Get the next data line. Unless something is terribly wrong,
C     this will begin a new variable definition. We have to read
C     the whole variable, unless we luck out and get an error, in
C     which case we can quit.
C
      STATUS = 'BEGIN'
 
      DO WHILE ( ( STATUS .NE. 'DONE' ) .AND. ( .NOT. FAILED() ) )
 
         CALL RDKDAT ( LINE, EOF )
 
         IF ( EOF ) THEN
            CALL CHKOUT ( 'RDKVAR' )
            RETURN
         END IF
 
C
C        Replace commas with blanks. We make no distinctions between
C        the two.
C
         CALL REPLCH ( LINE, COMMA, BLANK, LINE )
 
C
C        The first word on the first line should be the name of a
C        variable. The second word should be a directive: = or +=.
C
         IF ( STATUS .EQ. 'BEGIN' ) THEN
 
            CALL NEXTWD ( LINE, VARNAM, LINE )
            CALL NEXTWD ( LINE, DIRCTV, LINE )
 
C
C           If this is replacement (=) and not an addition (+=),
C           delete the values currently associated with the variable.
C           They will be replaced later.
C
            IF ( DIRCTV .EQ. '=' ) THEN
               CALL SYDELD ( VARNAM, TABSYM, TABPTR, TABVAL )
            END IF
 
C
C           If this is a vector, the next thing on the line will be a
C           left parenthesis. Otherwise, assume that this is a scalar.
C           If it's a vector, get the first value. If it's a scalar,
C           plant a bogus right parenthesis, to make the following loop
C           terminate after one iteration.
C
            CALL NEXTWD ( LINE, CVALUE, LINE )
 
            IF ( CVALUE .EQ. LPAREN ) THEN
               CALL NEXTWD ( LINE, CVALUE, LINE )
            ELSE
               LINE = RPAREN
            END IF
 
C
C        For subsequent lines, treat everything as a new value.
C
         ELSE
 
            CALL NEXTWD ( LINE, CVALUE, LINE )
 
         END IF
 
C
C        We have a value anyway. Store it in the table.
C
C        Keep going until the other shoe (the right parenthesis)
C        drops, or until the end of the line is reached.
C
C        Dates begin with @; anything else is presumed to be a number.
C
         DO WHILE ( CVALUE .NE. RPAREN  .AND.  CVALUE .NE. BLANK )
 
            IF ( CVALUE(1:1) .EQ. '@' ) THEN
 
               CALL TPARSE ( CVALUE(2: ), DVALUE, ERROR )
 
               IF ( ERROR .NE. ' ' ) THEN
                  ERROR  =  'Encountered : ' // CVALUE(2: )
                  CALL SETMSG ( ERROR    )
                  CALL SIGERR ( 'SPICE(DATEEXPECTED)' )
                  CALL CHKOUT ( 'RDKVAR' )
                  RETURN
               END IF
 
            ELSE
 
               CALL NPARSD ( CVALUE, DVALUE, ERROR, I )
 
               IF ( ERROR .NE. ' ' ) THEN
                  ERROR =  'Encountered : ' // CVALUE
                  CALL SETMSG ( ERROR    )
                  CALL SIGERR ( 'SPICE(NUMBEREXPECTED)' )
                  CALL CHKOUT ( 'RDKVAR' )
                  RETURN
               END IF
 
            END IF
 
            CALL SYENQD ( VARNAM, DVALUE, TABSYM,
     .                                    TABPTR,
     .                                    TABVAL )
 
            CALL NEXTWD ( LINE, CVALUE, LINE )
 
         END DO
 
         IF ( CVALUE .EQ. RPAREN ) THEN
            STATUS = 'DONE'
         ELSE
            STATUS = 'INVAR'
         END IF
 
      END DO
 
C
C     Return the name of the variable, but only if everything went okay.
C
      NAME = VARNAM
 
 
      CALL CHKOUT ( 'RDKVAR' )
      RETURN
      END
