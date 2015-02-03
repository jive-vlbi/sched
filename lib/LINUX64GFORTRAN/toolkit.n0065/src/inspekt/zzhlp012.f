C$Procedure      ZZHLP012 ( private help text )
 
      SUBROUTINE ZZHLP012 ( BEGIN, FINISH, TEXT )
 
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
      TEXT( 1129 ) = 'tables may have columns having the same'
     .//             ' name.  For this reason'
      TEXT( 1130 ) = 'when you select items to be displayed i'
     .//             'n a report via a'
      TEXT( 1131 ) = 'SELECT or SAMPLE ... SELECT statement y'
     .//             'ou must specify which'
      TEXT( 1132 ) = 'table the report is to be drawn from.  '
     .//             'You do this via the'
      TEXT( 1133 ) = 'FROM clause of the SELECT statement'
      TEXT( 1134 ) = '@literal'
      TEXT( 1135 ) = 'select  ...'
      TEXT( 1136 ) = 'FROM    TABLE_1 [alias_1]'
      TEXT( 1137 ) = '     [, TABLE_2 [alias_2] ...]'
      TEXT( 1138 ) = 'where   ...'
      TEXT( 1139 ) = 'oder by ...'
      TEXT( 1140 ) = '|endliteral'
      TEXT( 1141 ) = 'The simplest commands involve only one '
     .//             'table.  In such cases'
      TEXT( 1142 ) = 'there is usually no need to supply an a'
     .//             'lias for the'
      TEXT( 1143 ) = 'table specified in the FROM clause.'
      TEXT( 1144 ) = '@@From Clause'
      TEXT( 1145 ) = 'Quit Help'
      TEXT( 1146 ) = 'Help'
      TEXT( 1147 ) = 'Combining Tables'
      TEXT( 1148 ) = 'Looking at Data    --- SELECT'
      FINISH( 24 ) = 1148
 
      BEGIN ( 25 ) = 1149
      TEXT( 1149 ) = 'We anticipate that E-kernels may become'
     .//             ' quite large. As a result'
      TEXT( 1150 ) = 'the number of events that satisfy some '
     .//             'matching criteria given in'
      TEXT( 1151 ) = 'a  SELECT command might be very large. '
     .//             ' Since Inspekt does not yet'
      TEXT( 1152 ) = 'support  a UNIX-like "more" function an'
     .//             'd does not allow you to'
      TEXT( 1153 ) = 'interrupt some task (via a key sequence'
     .//             ' such as CTRL-C),  Inspekt'
      TEXT( 1154 ) = 'has a user adjustable DATA-DELUGE WARNI'
     .//             'NG level.  Reports will  be'
      TEXT( 1155 ) = 'generated automatically in response to '
     .//             'a SELECT command only if'
      TEXT( 1156 ) = 'the number  of matching events is less '
     .//             'than the data-deluge warning'
      TEXT( 1157 ) = 'level.  If the number  of matching even'
     .//             'ts is greater than this'
      TEXT( 1158 ) = 'level, you will be notified and  given '
     .//             'the option of producing the'
      TEXT( 1159 ) = 'report, viewing a subsample of the repo'
     .//             'rt, or cancelling the report.'
      TEXT( 1160 ) = ' '
      TEXT( 1161 ) = 'To set the deluge warning level, type t'
     .//             'he command.'
      TEXT( 1162 ) = '@literal'
      TEXT( 1163 ) = '   SET DELUGE WARNING integer'
      TEXT( 1164 ) = '|endliteral'
      TEXT( 1165 ) = 'If you take no action the warning level'
     .//             ' has value 100.'
      TEXT( 1166 ) = ' '
      TEXT( 1167 ) = 'To see the current data-deluge warning '
     .//             'level type the command'
      TEXT( 1168 ) = '@literal'
      TEXT( 1169 ) = 'SHOW FORMAT'
      TEXT( 1170 ) = '|endliteral'
      TEXT( 1171 ) = ' '
      TEXT( 1172 ) = ' '
      TEXT( 1173 ) = '@@Getting Too Much Data'
      TEXT( 1174 ) = 'Quit Help'
      TEXT( 1175 ) = 'Help'
      TEXT( 1176 ) = 'Sampling Data'
      TEXT( 1177 ) = 'SHOW FORMAT   ...'
      FINISH( 25 ) = 1177
 
      BEGIN ( 26 ) = 1178
      TEXT( 1178 ) = '@@Glossary'
      TEXT( 1179 ) = 'Quit Help'
      TEXT( 1180 ) = 'Help'
      TEXT( 1181 ) = 'Column'
      TEXT( 1182 ) = 'Patterns'
      TEXT( 1183 ) = 'Query'
      TEXT( 1184 ) = 'Reports'
      TEXT( 1185 ) = 'Symbol'
      FINISH( 26 ) = 1185
 
      BEGIN ( 27 ) = 1186
      TEXT( 1186 ) = 'The only attribute that you can set tha'
     .//             't globally affects headers'
      TEXT( 1187 ) = 'is the header frequency.  To see the cu'
     .//             'rrent frequency type the'
      TEXT( 1188 ) = 'command'
      TEXT( 1189 ) = '@literal'
      TEXT( 1190 ) = 'SHOW PAGE;'
      TEXT( 1191 ) = '|endliteral'
      TEXT( 1192 ) = '@@Headers'
      TEXT( 1193 ) = 'Quit Help'
      TEXT( 1194 ) = 'Help'
      FINISH( 27 ) = 1194
 
      BEGIN ( 28 ) = 1195
      TEXT( 1195 ) = '@@Help'
      TEXT( 1196 ) = 'Quit Help'
      TEXT( 1197 ) = 'About Help'
      TEXT( 1198 ) = 'Typing Commands'
      TEXT( 1199 ) = 'Kernels            --- LOAD'
      TEXT( 1200 ) = 'Looking at Data    --- SELECT'
      TEXT( 1201 ) = 'Setting up Inspekt --- SET'
      TEXT( 1202 ) = 'Current Settings   --- SHOW'
      TEXT( 1203 ) = 'Saving Work        --- SAVE TO'
      TEXT( 1204 ) = 'Errors'
      TEXT( 1205 ) = 'Syntax Summaries'
      TEXT( 1206 ) = 'Limits'
      TEXT( 1207 ) = 'Glossary'
      TEXT( 1208 ) = 'Problems, Suggestions'
      FINISH( 28 ) = 1208
 
      BEGIN ( 29 ) = 1209
      TEXT( 1209 ) = 'SPICE data is stored in data files call'
     .//             'ed kernels.  To make the'
      TEXT( 1210 ) = 'data in these kernels available for ret'
     .//             'rieval and manipulation by'
      TEXT( 1211 ) = 'Inspekt, you need to "load" the kernels'
     .//             '.  When you load a kernel,'
      TEXT( 1212 ) = 'Inspekt opens the file, reads the file,'
     .//             ' and stores some (or all) of'
      TEXT( 1213 ) = 'its contents in Inspekt''s memory.'
 
      RETURN
      END
