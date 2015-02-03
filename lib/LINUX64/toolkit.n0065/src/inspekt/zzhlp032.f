C$Procedure      ZZHLP032 ( private help text )
 
      SUBROUTINE ZZHLP032 ( BEGIN, FINISH, TEXT )
 
C$ Abstract
C
C     Fill out a portion of the help text needed by percy.
C
C     Private routine intended solely for the support of Inspekt
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
C     PRIVATE
C
C$ Declarations
 
      IMPLICIT NONE
      INTEGER               BEGIN ( * )
      INTEGER               FINISH( * )
      CHARACTER*(*)         TEXT  ( * )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     BEGIN      O   Indexes of begins of text help
C     FINISH     O   Indexes of ends of text help
C     TEXT       O   A block of text help.
C
C$ Exceptions
C
C     Error free.
C
C$ Particulars
C
C     This routine simply fills begin and end markers as well
C     as actual text for a block of help text for percy.
C
C$ Examples
C
C     None.
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
C-    Inspekt Version 1.0.0, 1-AUG-1997 (WLT)
C
C
C-&
 
      INTEGER               I
      INTEGER               J
 
      J = FINISH ( 1 )
      I = BEGIN  ( 1 )
 
      FINISH(1) = J
      BEGIN (1) = I
 
      CALL REPMC ( TEXT(1), '*', '*', TEXT(1) )
      TEXT( 2979 ) = 'SET PAGE TITLE @MYFMT@;'
      TEXT( 2980 ) = '|endliteral'
      TEXT( 2981 ) = ' '
      TEXT( 2982 ) = '@subsection Examining Symbols'
      TEXT( 2983 ) = ' '
      TEXT( 2984 ) = 'After working in Inspekt for some time,'
     .//             ' you may want to see what'
      TEXT( 2985 ) = 'symbols you''ve created.  Type'
      TEXT( 2986 ) = '@literal'
      TEXT( 2987 ) = 'SHOW SYMBOL pattern'
      TEXT( 2988 ) = '|endliteral'
      TEXT( 2989 ) = 'to see the definition and full evaluati'
     .//             'on of all symbols whose names'
      TEXT( 2990 ) = 'match the supplied pattern.'
      TEXT( 2991 ) = '@@Using Symbols'
      TEXT( 2992 ) = 'Quit Help'
      TEXT( 2993 ) = 'Help'
      TEXT( 2994 ) = 'Echoing Translated Commands'
      TEXT( 2995 ) = 'Collecting Commands In Files'
      TEXT( 2996 ) = 'Special Symbols --- Queries'
      FINISH( 74 ) = 2996
 
      BEGIN ( 75 ) = 2997
      TEXT( 2997 ) = 'The VERBATIM format allows you to captu'
     .//             're in a report the explicit'
      TEXT( 2998 ) = 'components that were supplied to each c'
     .//             'olumn at the the time an'
      TEXT( 2999 ) = 'event was recorded.  Each component wil'
     .//             'l appear on a separate line'
      TEXT( 3000 ) = 'in the report.  An example appears belo'
     .//             'w:'
      TEXT( 3001 ) = '@literal'
      TEXT( 3002 ) = '--- TIME     ---'
      TEXT( 3003 ) = 'JAN 25 18:29:59'
      TEXT( 3004 ) = '--- ACTIVITY ---'
      TEXT( 3005 ) = 'OBSERVATION'
      TEXT( 3006 ) = '--- SUBCLASS ---'
      TEXT( 3007 ) = 'GOOD AIR QUALITY'
      TEXT( 3008 ) = '--- COMMENTS ---'
      TEXT( 3009 ) = 'The air quality is good.'
      TEXT( 3010 ) = 'Smog has not been a problem for several'
     .//             ' days now.'
      TEXT( 3011 ) = '|endliteral'
      TEXT( 3012 ) = '@@Verbatim Format'
      TEXT( 3013 ) = 'Quit Help'
      TEXT( 3014 ) = 'Help'
      TEXT( 3015 ) = 'Reports'
      FINISH( 75 ) = 3015
 
      BEGIN ( 76 ) = 3016
      TEXT( 3016 ) = 'The "WHERE" clause of a SELECT command '
     .//             'allows you to narrow focus on a'
      TEXT( 3017 ) = 'particular set of events.  The form of '
     .//             'the WHERE-clause is:'
      TEXT( 3018 ) = '@literal'
      TEXT( 3019 ) = 'WHERE condition'
      TEXT( 3020 ) = 'AND/OR (NOT) condition'
      TEXT( 3021 ) = '         ...'
      TEXT( 3022 ) = 'AND/OR (NOT) condition'
      TEXT( 3023 ) = '|endliteral'
      TEXT( 3024 ) = 'With one exception'
      TEXT( 3025 ) = 'A condition is an expression of the for'
     .//             'm:'
      TEXT( 3026 ) = '@literal'
      TEXT( 3027 ) = 'column_name  binary_relation  value/col'
     .//             'umn'
      TEXT( 3028 ) = 'or'
      TEXT( 3029 ) = 'column_name  [NOT] BETWEEN value1/colum'
     .//             'n AND value2/column'
      TEXT( 3030 ) = '|endliteral'
      TEXT( 3031 ) = ' '
      TEXT( 3032 ) = 'Only'
      TEXT( 3033 ) = 'names of columns from loaded kernels ma'
     .//             'y be specified. The columns'
      TEXT( 3034 ) = 'must be scalar valued. If the column sp'
     .//             'ecified in a relation is'
      TEXT( 3035 ) = 'a character or time column, VALUE must '
     .//             'be enclosed in single ('')'
      TEXT( 3036 ) = 'or double (") quotes.'
      TEXT( 3037 ) = ' '
      TEXT( 3038 ) = 'The binary relations are: NE, LT, LE, E'
     .//             'Q, GE, GT, LIKE, NOT LIKE.'
      TEXT( 3039 ) = 'The short hand equivalents of the first'
     .//             ' 6 of these are !=, <, <=,'
      TEXT( 3040 ) = '=, >=, >.  All but the LIKE relation ca'
     .//             'n be used with any column.'
      TEXT( 3041 ) = 'The LIKE relation can be used only with'
     .//             ' character columns; it is'
      TEXT( 3042 ) = 'the pattern matching relation. The valu'
     .//             'e to the  right of a LIKE'
      TEXT( 3043 ) = 'relation is a pattern that the column m'
     .//             'ust match.  The character'
      TEXT( 3044 ) = '''*'' matches any substring  of of any '
     .//             'string.  The character ''% '''
      TEXT( 3045 ) = 'matches  any single character.'
      TEXT( 3046 ) = ' '
      TEXT( 3047 ) = 'Conditions may be grouped using parenth'
     .//             'eses.  These groups may'
      TEXT( 3048 ) = 'in turn be grouped using parentheses an'
     .//             'd the connecting words'
      TEXT( 3049 ) = 'AND and OR.  Any condition can be negat'
     .//             'ed using the word NOT.'
      TEXT( 3050 ) = ' '
      TEXT( 3051 ) = '@@Where Clause'
      TEXT( 3052 ) = 'Quit Help'
      TEXT( 3053 ) = 'Help'
      TEXT( 3054 ) = 'Example Where Clause'
      TEXT( 3055 ) = 'Conditional Operators'
      TEXT( 3056 ) = 'Looking at Data    --- SELECT'
      FINISH( 76 ) = 3056
 
      RETURN
      END
