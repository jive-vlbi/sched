C$Procedure      ZZHLP011 ( private help text )
 
      SUBROUTINE ZZHLP011 ( BEGIN, FINISH, TEXT )
 
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
      TEXT( 1041 ) = 'process.  There are several causes of s'
     .//             'uch errors: typos,'
      TEXT( 1042 ) = 'misunderstanding of commands syntax, or'
     .//             ' inadequate preparation of'
      TEXT( 1043 ) = 'Inspekt''s environment.  Inspekt attemp'
     .//             'ts to diagnose errors and'
      TEXT( 1044 ) = 'provides two levels of error diagnosis.'
     .//             ' All error diagnostics are'
      TEXT( 1045 ) = 'written to Inspekt''s log file.'
      TEXT( 1046 ) = ' '
      TEXT( 1047 ) = 'When an error is detected, Inspekt will'
     .//             ' display a message and then'
      TEXT( 1048 ) = 'prompt for a new command.  If the error'
     .//             ' message does not seem to'
      TEXT( 1049 ) = 'explain what went wrong, you can get th'
     .//             'e second level of error'
      TEXT( 1050 ) = 'diagnosis by entering the command "?;".'
     .//             '  If second level of error'
      TEXT( 1051 ) = 'diagnosis  is not available, you will g'
     .//             'et a  message stating that'
      TEXT( 1052 ) = 'there is no more information available.'
     .//             '  If a second level is'
      TEXT( 1053 ) = 'available, the diagnosis will be displa'
     .//             'yed.  This second level'
      TEXT( 1054 ) = 'will usually list the chain of subrouti'
     .//             'ne calls that led to the'
      TEXT( 1055 ) = 'error diagnosis and other information. '
     .//             ' If you can''t determine the'
      TEXT( 1056 ) = 'cause of the error, send a description '
     .//             'of what you were attempting'
      TEXT( 1057 ) = 'to do along  with the Inspekt.log file '
     .//             'to:'
      TEXT( 1058 ) = '@literal'
      TEXT( 1059 ) = 'btaber@spice.jpl.nasa.gov'
      TEXT( 1060 ) = '|endliteral'
      TEXT( 1061 ) = 'and I will do what I can to help out.'
      TEXT( 1062 ) = '@literal'
      TEXT( 1063 ) = '-Bill Taber'
      TEXT( 1064 ) = '|endliteral'
      TEXT( 1065 ) = ' '
      TEXT( 1066 ) = '@@Errors'
      TEXT( 1067 ) = 'Quit Help'
      TEXT( 1068 ) = 'Help'
      TEXT( 1069 ) = 'Typing Commands'
      FINISH( 20 ) = 1069
 
      BEGIN ( 21 ) = 1070
      TEXT( 1070 ) = 'Below is a collection of sample time fo'
     .//             'rmats and the time strings that would b'
     .//             'e'
      TEXT( 1071 ) = 'formed for noon on the fifteenth of Feb'
     .//             'ruary 1993.'
      TEXT( 1072 ) = '@literal'
      TEXT( 1073 ) = 'Format:  YYYY-MON-DD WKD HR:MN:SC'
      TEXT( 1074 ) = 'Result:  1993-FEB-15 MON 12:00:00'
      TEXT( 1075 ) = ' '
      TEXT( 1076 ) = 'Format:  Day of year DDD, MON-DD YYYY H'
     .//             'R:MN:SC.####'
      TEXT( 1077 ) = 'Result:  Day of year 046, FEB-15 1993 1'
     .//             '2:00:00.0000'
      TEXT( 1078 ) = ' '
      TEXT( 1079 ) = 'Format:  YYYY-DDD.### ::RND'
      TEXT( 1080 ) = 'Result:  1993-046.500'
      TEXT( 1081 ) = '|endliteral'
      TEXT( 1082 ) = '@@Example Time Formats'
      TEXT( 1083 ) = 'Quit Help'
      TEXT( 1084 ) = 'Help'
      TEXT( 1085 ) = 'Time Formats'
      TEXT( 1086 ) = 'Custom Formats'
      FINISH( 21 ) = 1086
 
      BEGIN ( 22 ) = 1087
      TEXT( 1087 ) = 'The WHERE clause in the SELECT command '
     .//             'below narrows down a selection to those'
      TEXT( 1088 ) = 'events that occurred later than 1 Jan 1'
     .//             '993 but before 1 MAR 1993.  It further'
      TEXT( 1089 ) = 'restricts the selection to those events'
     .//             ' that have "PLATFORM" occurring in a'
      TEXT( 1090 ) = 'substring in the SUBSYSTEM column of th'
     .//             'e event.'
      TEXT( 1091 ) = '@literal'
      TEXT( 1092 ) = 'select time event_type subsystem notes'
      TEXT( 1093 ) = 'from  events'
      TEXT( 1094 ) = 'WHERE TIME GT "1 JAN 1993"'
      TEXT( 1095 ) = '  AND TIME LT "1 MAR 1993"'
      TEXT( 1096 ) = '  AND SUBSYSTEM LIKE "*PLATFORM*";'
      TEXT( 1097 ) = '|endliteral'
      TEXT( 1098 ) = 'Note that the times used in the TIME co'
     .//             'ndition'
      TEXT( 1099 ) = 'and the pattern used in the SUBSYSTEM c'
     .//             'ondition'
      TEXT( 1100 ) = 'must match is enclosed in double (") or'
     .//             ' single ('') quotes.'
      TEXT( 1101 ) = '@@Example Where Clause'
      TEXT( 1102 ) = 'Quit Help'
      TEXT( 1103 ) = 'Help'
      FINISH( 22 ) = 1103
 
      BEGIN ( 23 ) = 1104
      TEXT( 1104 ) = 'Flagged formats are presented as shown '
     .//             'here'
      TEXT( 1105 ) = '@literal'
      TEXT( 1106 ) = 'First Column : Value of column for firs'
     .//             't event'
      TEXT( 1107 ) = 'Second Column: Value of the second colu'
     .//             'mn with'
      TEXT( 1108 ) = '               the output wrapped if it'
     .//             ' will'
      TEXT( 1109 ) = '               not fit on a single line'
     .//             ' of text.'
      TEXT( 1110 ) = 'Third Column : Value of the third colum'
     .//             'n'
      TEXT( 1111 ) = '               possibly wrapped in an u'
     .//             'neven'
      TEXT( 1112 ) = '               fashion due to'
      TEXT( 1113 ) = '               the means of specifying '
     .//             'an event.'
      TEXT( 1114 ) = ' '
      TEXT( 1115 ) = 'First Column : Value of this for second'
     .//             ' event'
      TEXT( 1116 ) = '      ...            ...'
      TEXT( 1117 ) = '|endliteral'
      TEXT( 1118 ) = 'There are two forms of flagged format. '
     .//             ' PRESERVED and un-PRESERVED'
      TEXT( 1119 ) = '(default).  If a format is a preserved,'
     .//             ' each component of a column'
      TEXT( 1120 ) = 'is begun on a new line of the report.  '
     .//             'Otherwise it is considered'
      TEXT( 1121 ) = 'to be simply FLAGGED format. Note  a bl'
     .//             'ank line is inserted  between'
      TEXT( 1122 ) = 'consecutive events in the output.'
      TEXT( 1123 ) = '@@Flagged Format'
      TEXT( 1124 ) = 'Quit Help'
      TEXT( 1125 ) = 'Help'
      TEXT( 1126 ) = 'Reports'
      TEXT( 1127 ) = ' '
      FINISH( 23 ) = 1127
 
      BEGIN ( 24 ) = 1128
      TEXT( 1128 ) = 'An E-kernel may contain many different '
     .//             'tables.  Moreover, different'
 
      RETURN
      END
