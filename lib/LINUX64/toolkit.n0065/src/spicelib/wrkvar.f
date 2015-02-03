C$Procedure      WRKVAR ( Write a variable to a kernel file )
 
      SUBROUTINE WRKVAR ( UNIT, NAME, DIRCTV, TABSYM,
     .                                        TABPTR,
     .                                        TABVAL  )
 
C$ Abstract
C
C     Write the value of a variable in a double precision symbol
C     table to a NAIF ASCII kernel file.
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
 
      INTEGER               UNIT
      CHARACTER*(*)         NAME
      CHARACTER*(*)         DIRCTV
      CHARACTER*(*)         TABSYM     ( LBCELL:* )
      INTEGER               TABPTR     ( LBCELL:* )
      DOUBLE PRECISION      TABVAL     ( LBCELL:* )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     UNIT       I   Output logical unit.
C     NAME       I   Name of the variable.
C     DIRCTV     I   Kernel directive: '=' or '+='.
C     TABSYM,
C     TABPTR,
C     TABVAL    I/O  Symbol table.
C
C$ Detailed_Input
C
C     UNIT        is the logical unit to which the variable will be
C                 written. This is usually the logical unit to which
C                 the output kernel file is connected.
C
C     NAME        is the name of the variable to be written to UNIT.
C
C     DIRCTV      is the directive linking NAME and its associated
C                 values in the kernel file. This may be any of the
C                 directives recognized by RDKVAR.
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
C     TABVAL      are the components of a double precision symbol
C                 table. This subroutine does not change the components;
C                 they contain the same values on output as they did
C                 on input.
C
C$ Parameters
C
C      None.
C
C$ Files
C
C     If the variable is to be written to an output kernel file, the
C     file should be opened with a logical unit determined by the
C     calling program.
C
C$ Exceptions
C
C     1) If an error occurs writing the variable to UNIT, the
C        error SPICE(WRITEERROR) is signalled.
C
C$ Particulars
C
C     If the table symbol table does not contain any variables, nothing
C     will be written to UNIT.
C
C$ Examples
C
C     If  NAME   = 'MEAN_ANOM'
C         DIRCTV = '='
C
C     And the contents of the symbol table are:
C
C          DELTA_T_A  -->   32.184
C          K          -->    0.D0
C          MEAN_ANOM  -->    6.239996D0
C                            1.99096871D-7
C          ORBIT_ECC  -->    1.671D-2
C
C     The output to UNIT might look like this, depending on the
C     length of the symbol table variables:
C
C     MEAN_ANOM   = ( 6.239996D0,
C                     1.99096871D-7  )
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C     I.M. Underwood  (JPL)
C
C$ Version
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
C     write a variable to a kernel file
C
C-&
 
 
C$ Revisions
C
C-    Beta Version 1.1.0, 20-DEC-1988 (NJB)
C
C        Call to IOERR changed to be consistent with new calling
C        protocol.  SETMSG call deleted, since IOERR now calls SETMSG.
C
C-&
 
 
 
C
C     SPICELIB functions
C
      INTEGER               SYDIMD
      LOGICAL               RETURN
 
C
C     Local variables
C
      CHARACTER*1           BLANK
      PARAMETER           ( BLANK = ' '    )
 
      CHARACTER*1           LPAREN
      PARAMETER           ( LPAREN = '('   )
 
      CHARACTER*3           RPAREN
      PARAMETER           ( RPAREN = '  )' )
 
      CHARACTER*2           COMMA
      PARAMETER           ( COMMA  = ','   )
 
      INTEGER               VARLEN
      INTEGER               MARGIN
      INTEGER               VARDIM
 
      CHARACTER*132         LINE
      DOUBLE PRECISION      DVALUE
 
      LOGICAL               FOUND
      INTEGER               IOSTAT
      INTEGER               I
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'WRKVAR' )
      END IF
 
 
C
C     Preliminary measurements.
C
      VARLEN = LEN ( TABSYM(1) )
      MARGIN = VARLEN + 6
      VARDIM = SYDIMD ( NAME, TABSYM, TABPTR, TABVAL )
 
C
C     One value per line.
C
      DO I = 1, VARDIM
 
         CALL SYNTHD ( NAME, I, TABSYM, TABPTR, TABVAL, DVALUE, FOUND )
 
C
C        The first line contains the variable name, the directive,
C        an optional left parenthesis, and the first value. The values
C        of a multi-dimensional variable are separated by commas.
C
         IF ( I .EQ. 1 ) THEN
 
            CALL LJUST ( NAME,   LINE                    )
            CALL RJUST ( DIRCTV, LINE(MARGIN-4:MARGIN-3) )
 
            IF ( VARDIM .GT. 1 ) THEN
 
               LINE(MARGIN-1:MARGIN-1) = LPAREN
               WRITE (UNIT,*,IOSTAT=IOSTAT) LINE(1:MARGIN), DVALUE,
     .                                      COMMA
 
            ELSE
 
               WRITE (UNIT,*,IOSTAT=IOSTAT) LINE(1:MARGIN), DVALUE
 
            END IF
 
 
C
C        The last line of a multi-dimensional variable ends with a
C        right parenthesis.
C
         ELSE IF ( I .GT. 1  .AND.  I .EQ. VARDIM ) THEN
 
            LINE = ' '
            WRITE (UNIT,*,IOSTAT=IOSTAT) LINE(1:MARGIN), DVALUE, RPAREN
 
         ELSE
 
            LINE = ' '
            WRITE (UNIT,*,IOSTAT=IOSTAT) LINE(1:MARGIN), DVALUE, COMMA
 
         END IF
 
      END DO
 
      IF ( IOSTAT .NE. 0 ) THEN
         CALL IOERR ( 'writing a variable to the output kernel file',
     .                ' ',
     .                IOSTAT                                          )
 
         CALL SIGERR ( 'SPICE(WRITEERROR)' )
 
      END IF
 
 
      CALL CHKOUT ( 'WRKVAR' )
      RETURN
      END
