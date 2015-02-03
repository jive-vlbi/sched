C$Procedure      ZZHLP001 ( private help text )
 
      SUBROUTINE ZZHLP001 ( BEGIN, FINISH, TEXT )
 
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
      TEXT( 92 ) = 'or a command that evaluates to one of the'
     .//           'se commands as a result of'
      TEXT( 93 ) = 'symbol substitution.'
      TEXT( 94 ) = ' '
      TEXT( 95 ) = 'If there is information that a procedure '
     .//           'needs, you can pass that'
      TEXT( 96 ) = 'information to the procedure by creating '
     .//           'one or more symbols that'
      TEXT( 97 ) = 'evaluate to the needed information.'
      TEXT( 98 ) = '@@Collecting Commands In Files'
      TEXT( 99 ) = 'Quit Help'
      TEXT( 100 ) = 'Help'
      TEXT( 101 ) = 'Using Symbols'
      FINISH( 3 ) = 101
 
      BEGIN ( 4 ) = 102
      TEXT( 102 ) = 'When we collect data about events (or an'
     .//            'y other set of objects)'
      TEXT( 103 ) = 'a decision must be made about what aspec'
     .//            'ts of the event shall'
      TEXT( 104 ) = 'be recorded.  Usually some of these aspe'
     .//            'cts (attributes) of events'
      TEXT( 105 ) = 'will change from one event to the next. '
     .//            ' It is the variation in these'
      TEXT( 106 ) = 'attributes that allow us to distinguish '
     .//            'one event from another.'
      TEXT( 107 ) = 'An individual attribute of the event is '
     .//            'called a column of the event.'
      TEXT( 108 ) = 'This term arises from the common way in '
     .//            'which data is presented'
      TEXT( 109 ) = 'on a page.  If we list all of the events'
     .//            ' we have'
      TEXT( 110 ) = 'recorded on a sheet of paper (or a termi'
     .//            'nal) so that the attributes'
      TEXT( 111 ) = 'for each event are always listed in the '
     .//            'same order from left to right,'
      TEXT( 112 ) = 'then the attributes for different events'
     .//            ' appear in columns on'
      TEXT( 113 ) = 'the page.'
      TEXT( 114 ) = ' '
      TEXT( 115 ) = '@@Column'
      TEXT( 116 ) = 'Quit Help'
      TEXT( 117 ) = 'Help'
      FINISH( 4 ) = 117
 
      BEGIN ( 5 ) = 118
      TEXT( 118 ) = 'When someone creates a table and column '
     .//            'names for the table they'
      TEXT( 119 ) = 'are presented with the following problem'
     .//            '.  The various names'
      TEXT( 120 ) = 'should be meaningful and they should be '
     .//            'easy to type.  If an easy'
      TEXT( 121 ) = 'to type name is not meaningful, it won'''
     .//            't be very useful in describing'
      TEXT( 122 ) = 'the table or a column.  Since tables and'
     .//            ' columns are usually meant'
      TEXT( 123 ) = 'to exist for a long time, table producer'
     .//            's often err on the side'
      TEXT( 124 ) = 'of creating names that are meaningful bu'
     .//            't are a bit difficult'
      TEXT( 125 ) = 'to type.'
      TEXT( 126 ) = ' '
      TEXT( 127 ) = 'Inspekt helps you deal with the problem '
     .//            'of minimizing the amount'
      TEXT( 128 ) = 'of text you have to type to specify a ta'
     .//            'ble or column by allowing'
      TEXT( 129 ) = 'you to use a pattern instead of the full'
     .//            ' name.  For example'
      TEXT( 130 ) = 'suppose that a column has the name EMPLO'
     .//            'YEE_SALARY.  If there are no'
      TEXT( 131 ) = 'other columns that start with "EMP", you'
     .//            ' can specify this column by'
      TEXT( 132 ) = 'typing "EMP*".  If there are several col'
     .//            'umn names that start with'
      TEXT( 133 ) = '"EMPLOYEE_" but only one column name end'
     .//            's with "SALARY" you can'
      TEXT( 134 ) = 'specify the column by typing "*_SAL*".  '
     .//            'As long as only one name'
      TEXT( 135 ) = 'matches the pattern, Inspekt will recogn'
     .//            'ize the name and treat the'
      TEXT( 136 ) = 'pattern as if you had typed the full nam'
     .//            'e.'
      TEXT( 137 ) = ' '
      TEXT( 138 ) = 'For example suppose that you have loaded'
     .//            ' an E-kernel that contains'
      TEXT( 139 ) = 'two tables "EMPLOYEE_DATA"  and "DEPARTM'
     .//            'ENT_DATA".  Moreover'
      TEXT( 140 ) = 'suppose that the first table contains th'
     .//            'e following columns:'
      TEXT( 141 ) = '"EMPLOYEE_NAME",  "SUPERVISOR", "HIRE_DA'
     .//            'TE", "SALARY", "DEPARTMENT".'
      TEXT( 142 ) = 'Suppose the second table has the followi'
     .//            'ng columns:'
      TEXT( 143 ) = '"DEPARTMENT_NAME", "OPERATING_EXPENSES",'
     .//            ' "MANAGER", "LOCATION".'
      TEXT( 144 ) = ' '
      TEXT( 145 ) = 'Then the following query'
      TEXT( 146 ) = '@literal'
      TEXT( 147 ) = 'SELECT EMP*, SAL*, DEP* FROM EMP* WHERE '
     .//            'SAL* > 30000;'
      TEXT( 148 ) = '|endliteral'
      TEXT( 149 ) = 'is equivalent to the much longer command'
      TEXT( 150 ) = '@literal'
      TEXT( 151 ) = 'SELECT EMPLOYEE_NAME, SALARY, DEPARTMENT'
      TEXT( 152 ) = 'FROM EMPLOYEE_DATA'
      TEXT( 153 ) = 'WHERE SALARY > 30000;'
      TEXT( 154 ) = '|endliteral'
      TEXT( 155 ) = ' '
      TEXT( 156 ) = 'You can patterns only for column and tab'
     .//            'le names.  You may not'
      TEXT( 157 ) = 'use them to abbreviate other words of th'
     .//            'e Inspekt command language.'
      TEXT( 158 ) = 'If some language words or phrases seem t'
     .//            'o be a bit long to type'
      TEXT( 159 ) = 'you should consider creating a symbol fo'
     .//            'r those words or phrases.'
      TEXT( 160 ) = ' '
      TEXT( 161 ) = '@@Column and Table Abbreviations'
      TEXT( 162 ) = 'Quit Help'
      TEXT( 163 ) = 'Pattern Matching'
      TEXT( 164 ) = 'Using Symbols'
      TEXT( 165 ) = 'Special Symbols --- Queries'
      FINISH( 5 ) = 165
 
      BEGIN ( 6 ) = 166
      TEXT( 166 ) = 'To see a list of all columns that are cu'
     .//            'rrently available'
      TEXT( 167 ) = 'in Inspekt, type the command'
      TEXT( 168 ) = '@literal'
      TEXT( 169 ) = 'SHOW SUMMARY'
      TEXT( 170 ) = '|endliteral'
      TEXT( 171 ) = 'You will be presented with a list of col'
     .//            'umn names in the form'
      TEXT( 172 ) = '"table_name.column_name" where "table_na'
     .//            'me" is the name of the'
      TEXT( 173 ) = 'table to which  the column belongs and "'
     .//            'column_name" is the name'
      TEXT( 174 ) = 'of the column.  To see attributes for a '
     .//            'particular column, type'
      TEXT( 175 ) = '@literal'
      TEXT( 176 ) = '  SHOW COLUMN column_name'
      TEXT( 177 ) = 'or'
      TEXT( 178 ) = '  SHOW COLUMN table_name.column_name'
      TEXT( 179 ) = '|endliteral'
      TEXT( 180 ) = 'The second form is required only if ther'
     .//            'e are two or more loaded'
      TEXT( 181 ) = 'tables that have the same column.'
      TEXT( 182 ) = ' '
 
      RETURN
      END
