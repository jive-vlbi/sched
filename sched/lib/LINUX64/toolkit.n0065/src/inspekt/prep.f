C$Procedure      PREP ( Preprocess the input SUBTeX buffer )
 
      SUBROUTINE PREP ( BUFFER )
 
C$ Abstract
C
C     Preprocess a SUBTeX buffer. That is, replace tab characters
C     with spaces and remove all other non-printing characters.
C     Also remove index lines.
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
 
      CHARACTER*(*)         BUFFER  ( LBCELL:* )
 
C$ Detailed_Input
C
C     BUFFER      on input contains lines of SUBTeX source text.
C
C$ Detailed_Output
C
C     BUFFER      on output contains the same source text, with
C                 all tab characters replaced by spaces, and with
C                 all other non-printing characters removed.
C                 Index lines are also removed.
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     BUFFER    I/O  Input source, output source.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     None.
C
C$ Particulars
C
C
C
C$ Examples
C
C
C
C
C$ Version
C
C     Beta Version 1.0.0, 11-JUN-1988 (IMU)
C
C-&
 
 
C
C     Entry points
C
 
C
C     SPICELIB functions
C
      INTEGER               CARDC
      LOGICAL               MATCHW
      LOGICAL               RETURN
 
C
C     Local variables
C
      INTEGER               NUL
      INTEGER               TAB
      INTEGER               US
      INTEGER               DEL
 
      PARAMETER           ( NUL  =   0 )
      PARAMETER           ( TAB  =   9 )
      PARAMETER           ( US   =  31 )
      PARAMETER           ( DEL  = 127 )
 
      INTEGER               CARD
      INTEGER               I
 
 
C
C     Standard SPICE error handling
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'PREP' )
      END IF
 
C
C     Replace tabs first, else they'll be stripped. Replace index
C     lines by blank lines.
C
      CARD = CARDC ( BUFFER )
 
      DO I = 1, CARD
 
         CALL REPLCH ( BUFFER(I), CHAR(TAB), ' ',       BUFFER(I) )
         CALL ASTRIP ( BUFFER(I), CHAR(NUL), CHAR(US),  BUFFER(I) )
         CALL ASTRIP ( BUFFER(I), CHAR(DEL), CHAR(DEL), BUFFER(I) )
 
         IF ( MATCHW ( BUFFER(I), '@Index*', '*', '%' ) ) THEN
            BUFFER(I) = ' '
         END IF
 
      END DO
 
C
C     Remove trailing blank lines.
C
      DO WHILE ( BUFFER(CARD) .EQ. ' ' )
         CARD = CARD - 1
      END DO
 
      CALL SCARDC ( CARD, BUFFER )
 
      CALL CHKOUT ( 'PREP' )
      RETURN
      END
 
 
