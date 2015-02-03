C$Procedure      ZZHLP031 ( private help text )
 
      SUBROUTINE ZZHLP031 ( BEGIN, FINISH, TEXT )
 
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
      TEXT( 2879 ) = '@literal'
      TEXT( 2880 ) = 'DEFINE ES EDIT SELECT;'
      TEXT( 2881 ) = '|endliteral'
      TEXT( 2882 ) = ' '
      TEXT( 2883 ) = 'When you type'
      TEXT( 2884 ) = '@literal'
      TEXT( 2885 ) = 'Inspekt> ES;'
      TEXT( 2886 ) = '|endliteral'
      TEXT( 2887 ) = 'The command is translated to'
      TEXT( 2888 ) = '@literal'
      TEXT( 2889 ) = 'EDIT SELECT'
      TEXT( 2890 ) = '|endliteral'
      TEXT( 2891 ) = 'If you have recently issued a select co'
     .//             'mmand, the command will be copied'
      TEXT( 2892 ) = 'into the editor so that you may perform'
     .//             ' any desired modifications.'
      TEXT( 2893 ) = ' '
      TEXT( 2894 ) = '@subsection Creating Symbols'
      TEXT( 2895 ) = ' '
      TEXT( 2896 ) = 'You may use symbols to stand for any gr'
     .//             'oup of words that you would'
      TEXT( 2897 ) = 'like to avoid typing repeatedly. To def'
     .//             'ine a symbol type:'
      TEXT( 2898 ) = '@literal'
      TEXT( 2899 ) = 'DEFINE symbol_name value;'
      TEXT( 2900 ) = '|endliteral'
      TEXT( 2901 ) = 'where "symbol_name" is the name of the '
     .//             'symbol you want to define and'
      TEXT( 2902 ) = '"value" is the associated list of words'
     .//             ' and characters. The value of'
      TEXT( 2903 ) = 'a symbol may be blank.  Symbol names mu'
     .//             'st begin with a letter and'
      TEXT( 2904 ) = 'can not end with a question mark ''?''.'
     .//             '  Also the case of letters'
      TEXT( 2905 ) = 'in a symbol is insignificant.  For exam'
     .//             'ple, the commands'
      TEXT( 2906 ) = '@literal'
      TEXT( 2907 ) = 'DEFINE MYSYM a string of words;'
      TEXT( 2908 ) = 'DEFINE MySym a string of words;'
      TEXT( 2909 ) = 'DEFINE mysym a string of words;'
      TEXT( 2910 ) = '|endliteral'
      TEXT( 2911 ) = 'all define the symbol MYSYM.  Moreover,'
     .//             ' when symbols appear in'
      TEXT( 2912 ) = 'a command, the case of the letters is i'
     .//             'nsignificant.  The'
      TEXT( 2913 ) = 'commands'
      TEXT( 2914 ) = '@literal'
      TEXT( 2915 ) = 'SET FORMAT MYSYM;'
      TEXT( 2916 ) = 'SET FORMAT MySym;'
      TEXT( 2917 ) = 'SET FORMAT mysym;'
      TEXT( 2918 ) = '|endliteral'
      TEXT( 2919 ) = 'will all be recognized as containing th'
     .//             'e symbol MYSYM.'
      TEXT( 2920 ) = ' '
      TEXT( 2921 ) = '@subsection Removing Symbols'
      TEXT( 2922 ) = ' '
      TEXT( 2923 ) = 'To remove a symbol definitions type:'
      TEXT( 2924 ) = '@literal'
      TEXT( 2925 ) = 'UNDEFINE symbol_name'
      TEXT( 2926 ) = '|endliteral'
      TEXT( 2927 ) = 'Note that'
      TEXT( 2928 ) = 'you can "undefine" a symbol that has no'
     .//             't been defined.  This is useful'
      TEXT( 2929 ) = 'in Inspekt Procedure Files when you wan'
     .//             't to make sure that some previously'
      TEXT( 2930 ) = 'defined symbol will not accidentally al'
     .//             'ter the meaning of a procedure command.'
      TEXT( 2931 ) = ' '
      TEXT( 2932 ) = '@subsection Symbols Used in Other Symbo'
     .//             'ls'
      TEXT( 2933 ) = ' '
      TEXT( 2934 ) = 'You may use symbols in the definition o'
     .//             'f another symbol.  For example,'
      TEXT( 2935 ) = '@literal'
      TEXT( 2936 ) = 'DEFINE P PRESERVED;'
      TEXT( 2937 ) = 'DEFINE M MARKED;'
      TEXT( 2938 ) = 'DEFINE TAB TABULAR;'
      TEXT( 2939 ) = 'DEFINE SETF SET FORMAT'
      TEXT( 2940 ) = 'DEFINE MYFMT SETF M TAB P;'
      TEXT( 2941 ) = '|endliteral'
      TEXT( 2942 ) = 'When you type'
      TEXT( 2943 ) = '@literal'
      TEXT( 2944 ) = 'Inspekt> MYFMT;'
      TEXT( 2945 ) = '|endliteral'
      TEXT( 2946 ) = 'The effect will be the same as if you h'
     .//             'ad typed'
      TEXT( 2947 ) = '@literal'
      TEXT( 2948 ) = 'SET FORMAT MARKED TABULAR PRESERVED;'
      TEXT( 2949 ) = '|endliteral'
      TEXT( 2950 ) = 'Symbols are not evaluated until they ar'
     .//             'e encountered in a command.'
      TEXT( 2951 ) = 'Moreover, symbol substitution is perfor'
     .//             'med until no more symbols'
      TEXT( 2952 ) = 'are found in the string.'
      TEXT( 2953 ) = ' '
      TEXT( 2954 ) = 'As a result of this "evaluate when enco'
     .//             'untered" strategy you can change'
      TEXT( 2955 ) = 'the value of MYFMT to mean SET FORMAT M'
     .//             'ARKED TABULAR by'
      TEXT( 2956 ) = 'redefining P to be a blank.'
      TEXT( 2957 ) = '@literal'
      TEXT( 2958 ) = 'DEFINE P  ;'
      TEXT( 2959 ) = '|endliteral'
      TEXT( 2960 ) = 'In addition to saving you typing, symbo'
     .//             'ls offer a mechanism for'
      TEXT( 2961 ) = 'passing information to Inspekt Procedur'
     .//             'es.  If a procedure needs'
      TEXT( 2962 ) = 'some variable piece of information, you'
     .//             ' can write the procedure so'
      TEXT( 2963 ) = 'that this information is expected to be'
     .//             ' present in some symbol.'
      TEXT( 2964 ) = 'Then create the symbol with the appropr'
     .//             'iate information before'
      TEXT( 2965 ) = 'starting the procedure that needs it.'
      TEXT( 2966 ) = ' '
      TEXT( 2967 ) = ' '
      TEXT( 2968 ) = '@subsection Suppressing Symbols Substit'
     .//             'ution'
      TEXT( 2969 ) = ' '
      TEXT( 2970 ) = 'Occasionally you may want to make sure '
     .//             'that a word does not get mistaken'
      TEXT( 2971 ) = 'for a symbol.  If the word is enclosed '
     .//             'in single ('') or double (") quotes'
      TEXT( 2972 ) = 'it is automatically invisible to the sy'
     .//             'mbol translation subsystem.  Thus'
      TEXT( 2973 ) = 'words in quoted strings are never mista'
     .//             'ken for symbols.  In addition'
      TEXT( 2974 ) = 'to these words you can make one or more'
     .//             ' words invisible to the symbol'
      TEXT( 2975 ) = 'resolver by placing it between consecut'
     .//             'ive ''@'' characters.  Thus the command'
      TEXT( 2976 ) = 'below sets the page title to be "MYFMT"'
     .//             ' even though MYFMT is a symbol defined'
      TEXT( 2977 ) = 'above'
      TEXT( 2978 ) = '@literal'
 
      RETURN
      END
