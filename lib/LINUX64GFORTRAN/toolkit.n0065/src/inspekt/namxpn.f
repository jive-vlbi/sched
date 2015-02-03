C$Procedure      NAMXPN ( name expansion )
 
      SUBROUTINE NAMXPN ( COMMND, PREF, ERROR )
 
C$ Abstract
C
C    The routine examines the unquoted words in a string to
C    see if any contain wildcard characters.  For each word
C    for which such a character is present, the routine
C    replaces the word by a matching table/column name.
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
      CHARACTER*(*)         COMMND
      CHARACTER*(*)         PREF
      CHARACTER*(*)         ERROR  ( 2 )
 
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     COMMND     I   A string that may have column patterns.
C     PREF       I   Search for table/column if word is unqualified
C     COMMND     O   The same string with patterns replaced by names.
C     ERROR      O   Blank unless something goes wrong.
C
C$ Detailed_Input
C
C     COMMND     is a string that may be part or all of an INSPEKT
C                command.  The string is searched for unquoted
C                words that contain wildcard characters '*' and '%'.
C
C     PREF       if a word is not a qualified column name (i.e.
C                does not contain a period) PREF indicates how
C                to attempt to match it against known items.
C
C                If PREF equals 'COLUMN' unqualified words are
C                matched only against known columns.  If PREF
C                is equal to 'TABLE', unqualified words are matched
C                only against known tables.  If PREF is any other
C                value, both table and column names are checked
C                for matches.
C
C                The routine is not sensitive to the case of PREF.
C
C$ Detailed_Output
C
C     COMMND     is the input string with patterns replaced by
C                matching column names.  If a pattern cannot be
C                uniquely matched it is left unchanged.  However,
C                this is regarded as an error condition and will
C                be diagnosed in ERROR.
C
C     ERROR      is a string indicating whether or not everything
C                went well in the attempt to match patterns against
C                known column names.  If every pattern could be
C                uniquely matched, ERROR is returned blank. Otherwise
C                a diagnosis of the problem is returned in ERROR(1).
C                ERROR(2) is not altered by this routine.
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
C     1) If a pattern appears in the input string, but cannot
C        be uniquely matched against a known table/column name, a
C        diagnosis of the failure to match is recorded in ERROR(1)
C
C$ Particulars
C
C     This is a utility routine for INSPEKT that allows the processing
C     of words that are regarded as patterns.  Patterns are assumed
C     to have two or more characters.  One of these must be either
C     the asterisk '*' or the percent '%' character.  Moreover if a
C     period is present in the word, these characters must follow
C     the period.
C
C     If such a word is present the portion of it that follows
C     a period '.' is matched against the list of known column names.
C     If there is a unique match, the portion of the word that follows
C     the period is replaced by the matching name.
C
C     Patterns and column names are case insensitive as far as
C     matching is concerned.
C
C$ Examples
C
C     Suppose that there are 4 known columns present in the
C     column manager:
C
C        EMPLOYEE, MANAGER, EMPLOYEE_SALARY, DEPARTMENT.
C
C     If the query is entered
C
C        SELECT EMP*E, MAN*, *SAL*, DEP* FROM TABLE
C        WHERE *SAL* > 10000
C        ORDER BY DEP*, EMP*E;
C
C     It will be expanded to
C
C        SELECT EMPLOYEE, MANAGER, EMPLOYEE_SALARY, DEPARTMENT
C        FROM TABLE WHERE EMPLOYEE_SALARY > 10000 ORDER
C        BY DEPARTMENT, EMPLOYEE;
C
C     Note that the query
C
C        SELECT EMP*, MAN*, *SAL*, DEP* FROM TABLE;
C
C     Results in an error diagnosis because both 'EMPLOYEE' and
C     'EMPLOYEE_SALARY' match the pattern 'EMP*'
C
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C       W.L. Taber      (JPL)
C
C$ Literature_References
C
C       None.
C
C$ Version
C
C-    Inspekt Version 1.0.0, 1-NOV-1995 (WLT)
C
C
C-&
C
C     SPICELIB Functions
C
      INTEGER               POS
      INTEGER               CPOS
      INTEGER               RTRIM
C
C     Inspekt Functions
C
      LOGICAL               MATCHI
 
 
 
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 64 )
 
C
C     Local Variables
C
 
      CHARACTER*(1)         PERIOD
      CHARACTER*(WDSIZE)    ITEM ( 2 )
      CHARACTER*(WDSIZE)    NAME
      CHARACTER*(WDSIZE)    PART ( 2 )
      CHARACTER*(WDSIZE)    SUB  ( 2 )
      CHARACTER*(WDSIZE)    WORD
 
 
      INTEGER               B
      INTEGER               COMP
      INTEGER               E
      INTEGER               I
      INTEGER               J
      INTEGER               LOC
      INTEGER               MATCHS
      INTEGER               N
      INTEGER               NID
      INTEGER               Q
      INTEGER               R
      INTEGER               START
 
      LOGICAL               FOUND
      LOGICAL               SUBST
 
C
C     Look up the number of columns that are currently
C     active.
C
      CALL CLNUM  ( N )
C
C     For each unquoted word in the command see if it contains
C     one of the wildcard characters.  If it does we see if there
C     is a matching column name.
C
 
      I = 1
      CALL NTHUQT ( COMMND, I, ' ', WORD, LOC )
 
 
 
      DO WHILE ( WORD .NE. ' ' )
C
C        Get the last non-blank character.
C
         R     = RTRIM(WORD)
 
         IF ( R .GT. 1 ) THEN
C
C           We need to take apart the word to see if there
C           are one or two components.  We use the period (.)
C           to make this determination.  The stuff before
C           the period is always a table, the stuff after is
C           the column.  If there isn't a period, we let the
C           user decide if we need to look in the columns, tables
C           or both for resolution of the pattern match.
C
            START = POS(WORD, '.', 1)
            B     = LOC
            E     = LOC + R - 1
C
C           Until we learn differently, we assume we don't have
C           to perform any substitutions.
C
            SUBST = .FALSE.
 
            IF ( START .EQ. 0 ) THEN
 
               PART(1) = WORD
               PERIOD  = ' '
               PART(2) = ' '
               CALL UCASE ( PREF, ITEM(1) )
               CALL UCASE ( PREF, ITEM(2) )
 
            ELSE IF ( START .EQ. 1 ) THEN
 
               PART(1) = ' '
               PERIOD  = '.'
               PART(2) = WORD(START+1:)
               ITEM(1) = 'TABLE '
               ITEM(2) = 'COLUMN'
 
            ELSE IF ( START .LT. R ) THEN
 
               PART(1) = WORD(1:START-1)
               PERIOD  = '.'
               PART(2) = WORD(START+1:)
               ITEM(1) = 'TABLE '
               ITEM(2) = 'COLUMN'
 
            ELSE
 
               PART(1) = WORD(1:START-1)
               PERIOD  = '.'
               PART(2) = ' '
               ITEM(1) = 'TABLE '
               ITEM(2) = 'COLUMN'
 
            END IF
C
C           Now for each of the components, look for
C           matching names. (Provide of course that a pattern
C           is in fact a template).
C
 
            DO COMP = 1, 2
 
               SUB(COMP) = PART(COMP)
               MATCHS    = 0
 
               IF ( CPOS ( PART(COMP), '%*', 1 ) .GT. 0 ) THEN
C
C                 Wildcards are present in this component so we
C                 regard it as a template.
C
C                 Unless we specifically said that this component
C                 should be regarded as a table name, we look in
C                 the columns to see if we have a match there.
C
                  IF ( ITEM(COMP) .NE. 'TABLE' ) THEN
                     DO J = 1, N
                        CALL CLNID ( J,    NID,     FOUND )
                        CALL CLGAC ( NID, 'COLNAM', NAME  )
 
                        IF ( MATCHI( NAME, PART(COMP), '*', '%' ) ) THEN
 
                           IF ( NAME .NE. SUB(COMP) ) THEN
                              MATCHS    = MATCHS+1
                              SUB(COMP) = NAME
                              Q         = RTRIM(NAME)
                           END IF
 
                        END IF
                     END DO
                  END IF
C
C                 Unless we specifically ask for just columns
C                 we look up possible matching taable names.
C
                  IF ( ITEM(COMP) .NE. 'COLUMN' ) THEN
                     DO J = 1, N
 
                        CALL CLNID ( J,    NID,     FOUND )
                        CALL CLGAC ( NID, 'TABLE', NAME  )
 
                        IF ( MATCHI( NAME, PART(COMP), '*', '%' ) ) THEN
 
                           IF ( NAME .NE. SUB(COMP) ) THEN
                              MATCHS    = MATCHS+1
                              SUB(COMP) = NAME
                              Q         = RTRIM(NAME)
                           END IF
 
                        END IF
                     END DO
                  END IF
C
C                 Now see how many matches we had.  If not exactly
C                 one, this word can't be expanded.  We regard this
C                 as an error since the only unquoted words that
C                 can have wild cards in a query are columns
C                 and table names.
C
                  IF ( MATCHS .EQ. 0 ) THEN
 
                     ERROR(1) = 'The pattern ''#'' does not '
     .               //         'match the name of any currently '
     .               //         'available @s. '
 
 
                  ELSE IF ( MATCHS .GT. 1 ) THEN
 
                     ERROR(1) = 'The pattern ''#'' matches more '
     .               //         'than one @ name.  The pattern '
     .               //         'needs to be more specific. '
 
 
                  END IF
 
                  IF ( MATCHS .NE. 1 ) THEN
 
                     CALL LCASE ( ITEM(COMP), ITEM(COMP) )
                     CALL REPMC ( ERROR(1), '@', ITEM(COMP), ERROR(1))
                     CALL REPMC ( ERROR(1), '#', PART(COMP), ERROR(1) )
                     RETURN
 
                  ELSE
 
                     SUBST = .TRUE.
 
                  END IF
 
               END IF
 
            END DO
C
C           That takes care of all checks for the current word.  If
C           we need to make a substitution, now is the time to do it.
C
            IF ( SUBST ) THEN
 
               WORD = SUB(1)
               CALL SUFFIX ( PERIOD, 0, WORD )
               CALL SUFFIX ( SUB(2), 0, WORD )
 
               Q = RTRIM(WORD)
 
               CALL REPSUB ( COMMND, B, E, WORD(1:Q), COMMND )
 
            END IF
 
         END IF
C
C        Now fetch the next word in the command.
C
         I = I+1
         CALL NTHUQT ( COMMND, I, ' ', WORD, LOC )
 
      END DO
C
C     For debugging for the moment we write out the command
C     so we can see what's going on.
C
      RETURN
      END
