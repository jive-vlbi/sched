C$Procedure      EXPAND ( Expand a SUBTeX buffer )
 
      SUBROUTINE EXPAND ( BUFFER )
 
C$ Abstract
C
C     Expand a buffer containing SUBTeX source lines by replacing
C     @input commands with the contents of the indicated files.
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
C     SUBTeX
C
C$ Keywords
C
C     SUBTeX
C
C$ Declarations
 
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )
 
      CHARACTER*(*)         BUFFER         ( LBCELL:* )
 
C$ Detailed_Input
C
C     BUFFER      on input is a buffer containing lines of SUBTeX
C                 source text, which may include commands of the form
C
C                    @input <filename>
C
C$ Detailed_Output
C
C     BUFFER      on output contains the original lines, with any
C                 input lines replaced by the contents of the specified
C                 files.
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     BUFFER    I/O  Original, expanded SUBTeX source lines.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     1) If an input file cannot be opened or read, the error
C        'SUBTeX(BADINPUTFILE)' is signalled.
C
C     2) If the expanded buffer becomes too large, the error
C        'SUBTeX(CANNOTEXPAND)' is signalled.
C
C$ Particulars
C
C     The @input control sequence and the name of the file are
C     assumed to be the first two words of the command file.
C     For example,
C
C        @input gloss.tex
C        @input gloss.tex  % glossary of terms
C
C     are all legitimate input commands; however,
C
C        @input gloss.tex%glossary of terms
C
C     is not; and in the following line
C
C        @input gloss.tex  @input appendix.tex ... @input authors.tex
C
C     all files but the first are ignored. Note that EXPAND does NOT
C     assumes a file type of 'TEX' if none is supplied. (This may be
C     remedied in later versions.
C
C$ Examples
C
C
C
C$ Restrictions
C
C     1) The file name may contain up to 128 characters.
C
C     2) All files are opened with the READONLY qualifier, which is
C        specific to VAX Fortran. This must be changed or removed
C        when porting to non-VAX environments.
C
C$ Literature_References
C
C$Include SUBTeX.REFS
C
C$ Author_and_Institution
C
C     I.M. Underwood (JPL)
C
C$ Version
C
C     Beta Version 1.0.0, 11-JUN-1988 (IMU)
C
C-&
 
 
C
C     SPICELIB functions
C
      INTEGER               CARDC
      LOGICAL               RETURN
      INTEGER               SIZEC
 
C
C     Local variables
C
      CHARACTER*12          CSEQ
      CHARACTER*128         FILE
      CHARACTER*132         TEXT
 
      INTEGER               LINE
      INTEGER               LOC
 
      INTEGER               SIZE
      INTEGER               CARD
      INTEGER               END
      INTEGER               NEW
 
      LOGICAL               MORE
      LOGICAL               EOF
 
C
C     Standard SPICE error handling
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'EXPAND' )
      END IF
 
 
C
C     The size of the buffer determines the number of replacement
C     lines that can be added.
C
      SIZE = SIZEC ( BUFFER )
 
 
C
C     Rather than trying to nest expansions, we will expand one level
C     at a time.
C
      MORE = .TRUE.
 
      DO WHILE ( MORE )
 
C
C        Check each line in the buffer for an input command.
C        (Remember that expanding a line can change both the
C        cardinality of the buffer and the location of the
C        current line.)
C
         LINE = 0
         MORE = .FALSE.
 
         DO WHILE ( LINE .LT. CARDC ( BUFFER ) )
 
            LINE = LINE + 1
            CALL NTHWD ( BUFFER(LINE), 1, CSEQ, LOC )
 
            IF ( CSEQ .EQ. '@input' ) THEN
 
C
C              We have a bite. Get the name of the file.
C
               CALL NTHWD ( BUFFER(LINE), 2, FILE, LOC )
 
C
C              Put lines read from the file at the end of the buffer.
C              We will exchange the new lines for the @input command
C              line later.
C
               CARD = CARDC ( BUFFER )
               END  = CARD
 
               CALL RDTEXT ( FILE, TEXT, EOF )
 
               DO WHILE ( .NOT. EOF )
                  IF ( END .LT. SIZE ) THEN
                     END         = END + 1
                     BUFFER(END) = TEXT
 
                  ELSE
                     CALL SIGERR ( 'SUBTeX(CANNOTEXPAND)' )
                     CALL CHKOUT ( 'EXPAND' )
                     RETURN
                  END IF
 
                  CALL RDTEXT ( FILE, TEXT, EOF )
               END DO
 
C
C              The big switch. (Remember that the current line
C              will also be incremented at the beginning of the
C              loop. Also that the command line, now at the
C              end of the buffer, is not part of the expanded
C              buffer.)
C
               NEW = END - CARD
               CALL SWAPAC ( 1, LINE, NEW, CARD+1, BUFFER(1) )
 
               LINE = LINE + NEW - 1
               CALL SCARDC ( CARD+NEW-1, BUFFER )
 
C
C              Because the expanded file may contain its own
C              input commands, we will need to run through the
C              buffer again.
C
               MORE = .TRUE.
 
            END IF
 
         END DO
 
      END DO
 
      CALL CHKOUT ( 'EXPAND' )
      RETURN
      END
 
