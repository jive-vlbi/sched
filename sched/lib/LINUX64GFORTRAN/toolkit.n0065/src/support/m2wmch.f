C$Procedure            M2WMCH ( Match a word against a META/2 class )
 
      LOGICAL FUNCTION M2WMCH ( STRING, WORDB, WORDE, CLASS )
      IMPLICIT NONE
 
C$ Abstract
C
C     Determine whether or not the WORD is a member of a META/2
C     class.
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
C     META/2 a language specification language.
C
C$ Keywords
C
C     PARSING
C     UTILITY
C     WORD
C
C$ Declarations
 
      CHARACTER*(*)         STRING
      INTEGER               WORDB
      INTEGER               WORDE
      CHARACTER*(*)         CLASS
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     STRING     I   A string containing words.
C     WORDB      I   The beginning of a word.
C     WORDE      I   The ending of the same word.
C     CLASS      I   A META/2 specification keyword or META-KEY
C
C     The function is returned as .TRUE. if WORD is a member of CLASS.
C
C$ Detailed_Input
C
C      STRING    is any character string.  It is expected to be composed
C                of words.
C
C      WORDB     is the beginning of some word in STRING.
C
C      WORDE     is the ending of the same word of STRING.
C
C                The word of interest is STRING(WORDB:WORDE).
C
C      CLASS     is one of the recognized classes of words in META/2 or
C                a META-KEY.  CLASS is expected to be right justified.
C                This class may be modified by a restriction template.
C                The possible classes are:
C
C                @word            @number
C                @alpha           @int
C                @name            @body
C                @english         @unit
C                @epoc
C                @day
C                @time
C                @month
C                @year
C                @calendar
C
C                Of these, the following can be modified by a
C                restriction template.
C
C                @word            @number
C                @alpha           @int
C                @name            @unit
C                @english
C                @month
C
C                If CLASS is not one of these words ( possibly qualified
C                by a restriction template ) it is assumed to be a
C                specification keyword.
C
C$ Detailed_Output
C
C      M2WMCH is returned as .TRUE. if
C
C         1.)  CLASS is a META-KEY and STRINB(WORDB:WORDE) falls into
C              the category specified by this META-KEY
C
C         or
C
C         2.) CLASS is determined to be a specification keyword and
C             STRING(WORDB:WORDE) is equal to this keyword.
C
C         Otherwise, it is returned as .FALSE.
C
C$ Error_Handling
C
C     None.
C
C$ Input_Files
C
C     None.
C
C$ Output_Files
C
C     None.
C
C$ Particulars
C
C      This is a utility routine for use by META/2.  It determines
C      whether or not a word from a candidate sentence matches
C      a desired class.
C
C$ Examples
C
C      The following table gives a sample of the results that
C      are returned by this function.
C
C      WORD          CLASS               M2WMCH
C      ---------     ---------           ------
C      SEPARATION    OBJECT                F
C      SEPARATION    @english              T
C      SEPARATION    @english(T*)          F
C      SEPARATION    @english(T*|S*)       T
C      12:15:15      @number               F
C      12:15:15      @time                 T
C      44:12:18      @time                 F
C      PIG           @english              T
C      PIG           @int                  T
C      12.182        NUMBER                F
C      12.182        @number               T
C      12.182        @int                  F
C
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
C     W.L. Taber     (JPL)
C     I.M. Underwood (JPL)
C
C$ Version
C
C-     META/2 Configured Version 3.1.0, 07-NOV-2005 (BVS)
C
C         Fixed the way ZZTOKNS is called.
C
C-     META/2 Configured Version 3.0.0, 23-MAR-2000 (WLT)
C
C         Extended the routine so that it can now check the keyword
C         @unit and @unit(unitspec).
C
C-     META/2 Configured Version 2.0.0, 9-MAY-1994 (WLT)
C
C         This is the configured version of the Command Loop
C         software as of May 9, 1994
C
C
C-     META/2 Configured Version 1.0.0, 3-MAY-1994 (WLT)
C
C         This is the configured version of META/2
C         software as of May 3, 1994
C
C
C     Version B1.0.0, 31-MAR-1988 (WLT) (IMU)
C
C-&
 
 
 
C
C     SPICELIB functions
C
      LOGICAL               EQSTR
      LOGICAL               M2ALPH
      LOGICAL               M2NAME
      LOGICAL               M2ENGL
      LOGICAL               M2TIME
      LOGICAL               M2MON
      LOGICAL               M2NUMB
      LOGICAL               M2INT
      LOGICAL               M2YEAR
      LOGICAL               M2DAY
      LOGICAL               M2EPOC
      LOGICAL               M2BODY
      LOGICAL               M2UNIT
      LOGICAL               MATCHM
      LOGICAL               ZZTOKNS
 
C
C     Local variables
C
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 160 )
 
      INTEGER               BEG
      INTEGER               END
      INTEGER               WB
      INTEGER               WE
      CHARACTER*(32)        BASE
      CHARACTER*(LNSIZE)    ERROR
      LOGICAL               KEY
      LOGICAL               TEMP
 
      DOUBLE PRECISION      XIN
      DOUBLE PRECISION      XOUT
      DOUBLE PRECISION      X
      DOUBLE PRECISION      Y
      DOUBLE PRECISION      V
 
 
      INTEGER               PNTR
 
      INTEGER               NB
      INTEGER               NE
      INTEGER               L
      INTEGER               LBRACE
      INTEGER               RBRACE
      INTEGER               I
      INTEGER               STATUS
 
      LOGICAL               NAMFND
      LOGICAL               TMPLOG
 
 
      CHARACTER*(*)         DELIMS
      PARAMETER           ( DELIMS = '() ,:/-' )
 
 
      SAVE
 
 
C
C     There are some obvious things we can handle right now.
C     Note that if we input a substring that is completely outside
C     the range (1, LEN(STRING)), then WB will be greater than WE.
C     Otherwize we will have trimmed the substring to lie within
C     the bounds of the string.
C
      WB = MAX( WORDB, 1           )
      WE = MIN( WORDE, LEN(STRING) )
 
      IF ( WB .GT. WE ) THEN
         M2WMCH = .FALSE.
         RETURN
      END IF
 
 
C
C     Get the first and last characters of CLASS
C     These are EXPECTED to be the first and last characters of
C     CLASS.
C
      BEG    = 1
      L      = LEN  (CLASS)
      LBRACE = ICHAR( '[' )
      RBRACE = ICHAR( ']' )
 
 
C
C     Next see if there is a name attached to which we will write the
C     results of successful matches.
C
      NAMFND = .FALSE.
      END    =  L
C
C     If the length is not at least 4 or the last character is not
C     a right brace, there is no name associated with this word.
C
      IF (      ( ICHAR(CLASS(L:L)) .EQ. RBRACE )
     .    .AND. ( L                 .GE. 4      ) ) THEN
C
C        Ok. We have a chance at getting a name.  Look for
C        a left brace and if found set the name and class end.
C
         I = 2
 
         DO WHILE (I .LT. L - 1)
 
            IF ( ICHAR(CLASS(I:I)) .EQ. LBRACE ) THEN
               NB     = I + 1
               NE     = L-1
               END    = I - 1
               I      = L
               NAMFND = .TRUE.
            END IF
 
            I = I + 1
         END DO
 
 
      END IF
 
C
C     See if CLASS represents a specification keyword or a META-KEY.
C
      CALL M2TRAN ( CLASS, BEG, END, BASE, KEY, TEMP )
 
 
C
C     If we have a specification keyword, the input WORD must match
C     exactly.
C
      IF ( KEY ) THEN
 
         M2WMCH = EQSTR ( CLASS(1:END), STRING(WB:WE) )
 
C
C     See if we are trying to match a numeric string.
C
      ELSE IF (     ( BASE .EQ. '@int'    )
     .         .OR. ( BASE .EQ. '@number' ) ) THEN
 
 
         IF      ( BASE .EQ. '@int'    ) THEN
 
            M2WMCH = M2INT ( STRING(WB:WE) )
 
         ELSE IF ( BASE .EQ. '@number' ) THEN
 
            M2WMCH = M2NUMB(STRING(WB:WE))
 
         END IF
 
 
         IF ( M2WMCH .AND. TEMP ) THEN
 
C
C           Parse the number and see if it is in bounds.
C
            CALL M2NTEM ( CLASS,          BASE, BEG,   END, X, Y )
            CALL NPARSD ( STRING(WB:WE),  V,    ERROR, PNTR      )
 
            M2WMCH =  ( V .LE. Y ) .AND.
     .                ( V .GE. X )
 
         END IF
 
 
         IF ( M2WMCH .AND. NAMFND ) THEN
            CALL M2SAVE ( CLASS(NB:NE), WB, WE )
         END IF
 
         RETURN
 
      ELSE IF ( BASE .EQ. '@unit'    ) THEN
 
         M2WMCH = M2UNIT( STRING(WB:WE) )
 
         IF ( M2WMCH .AND. TEMP ) THEN
 
            XIN = 1.0D0
            CALL CONVRT_3 ( XIN,
     .                      STRING(WB:WE),
     .                      CLASS(BEG+1:END-1),
     .                      XOUT,
     .                      STATUS )
 
            M2WMCH = STATUS .EQ. 0
 
         END IF
 
 
         IF ( M2WMCH .AND. NAMFND ) THEN
            CALL M2SAVE ( CLASS(NB:NE), WB, WE )
         END IF
 
         RETURN
 
      ELSE IF ( BASE .EQ. '@name'    ) THEN
 
         M2WMCH = M2NAME ( STRING(WB:WE) )
 
      ELSE IF ( BASE .EQ. '@body' ) THEN
 
         M2WMCH = M2BODY ( STRING(WB:WE) )
 
      ELSE IF ( BASE .EQ. '@english' ) THEN
 
         M2WMCH = M2ENGL ( STRING(WB:WE) )
 
      ELSE IF ( BASE .EQ. '@alpha'   ) THEN
 
         M2WMCH = M2ALPH ( STRING(WB:WE)  )
 
      ELSE IF ( BASE .EQ. '@time'    ) THEN
 
         M2WMCH = M2TIME ( STRING(WB:WE)  )
 
      ELSE IF ( BASE .EQ. '@epoch'   ) THEN
 
         M2WMCH = M2EPOC ( STRING(WB:WE)  )
 
      ELSE IF ( BASE .EQ. '@day'     ) THEN
 
         M2WMCH = M2DAY ( STRING(WB:WE)  )
 
      ELSE IF ( BASE .EQ. '@year'    ) THEN
 
         M2WMCH = M2YEAR ( STRING(WB:WE)  )
 
      ELSE IF ( BASE .EQ. '@month'   ) THEN
 
         M2WMCH = M2MON ( STRING(WB:WE)  )
 
      ELSE IF ( BASE .EQ. '@calendar' ) THEN
 
         TMPLOG = ZZTOKNS ( STRING(WB:WE), ERROR )
 
         M2WMCH = ERROR .EQ. ' '
 
      ELSE IF ( BASE .EQ. '@word'     ) THEN
 
         M2WMCH =  .TRUE.
 
      END IF
 
 
      IF ( M2WMCH .AND. TEMP ) THEN
 
         M2WMCH = MATCHM (  STRING(WB:WE),
     .                      CLASS(BEG+1:END-1),
     .                      '*', '%', '~', '|'  )
 
 
      END IF
 
 
      IF ( M2WMCH .AND. NAMFND ) THEN
         CALL M2SAVE ( CLASS(NB:NE), WB, WE )
      END IF
 
 
      RETURN
 
      END
