C$Procedure      ZZHLP021 ( private help text )
 
      SUBROUTINE ZZHLP021 ( BEGIN, FINISH, TEXT )
 
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
      TEXT( 1942 ) = 'where topic title is the title that app'
     .//             'ears in one of the help menus.'
      TEXT( 1943 ) = ' '
      TEXT( 1944 ) = 'You don''t have to enter the title exac'
     .//             'tly as it appears in the help system'
      TEXT( 1945 ) = 'menus.  The help topic search does not '
     .//             'depend upon the case of the letters'
      TEXT( 1946 ) = 'you use.  Also, you may use a wild card'
     .//             ' pattern for the topic title.'
      TEXT( 1947 ) = 'This way you don''t have to remember'
      TEXT( 1948 ) = 'the exact topic.  However, you do run a'
     .//             ' slight risk that some other help'
      TEXT( 1949 ) = 'topic will match your pattern.  If more'
     .//             ' than one topic matches the pattern,'
      TEXT( 1950 ) = 'Inspekt will choose the one that occurs'
     .//             ' first in an alphabetical listing'
      TEXT( 1951 ) = 'of the help topics.'
      TEXT( 1952 ) = ' '
      TEXT( 1953 ) = 'Once you are in the help system, you mu'
     .//             'st use the menus to navigate the'
      TEXT( 1954 ) = 'various help topics.  You can not enter'
     .//             ' the name of some other topic and'
      TEXT( 1955 ) = 'display it directly.'
      TEXT( 1956 ) = '@@Short Cut to Topics'
      TEXT( 1957 ) = 'Quit Help'
      TEXT( 1958 ) = 'Help'
      FINISH( 53 ) = 1958
 
      BEGIN ( 54 ) = 1959
      TEXT( 1959 ) = 'You can get a snapshot of all of the at'
     .//             'tributes of a column (both'
      TEXT( 1960 ) = 'the user adjustable attributes and fixe'
     .//             'd attributes) by issuing'
      TEXT( 1961 ) = 'the command.'
      TEXT( 1962 ) = '@literal'
      TEXT( 1963 ) = 'SHOW COLUMN column_name;'
      TEXT( 1964 ) = '|endliteral'
      TEXT( 1965 ) = 'If more than one table possess a column'
     .//             ' with your name'
      TEXT( 1966 ) = 'you must specify which column you are t'
     .//             'alking about.'
      TEXT( 1967 ) = 'Do this by prefixing the table name to '
     .//             'the column name as'
      TEXT( 1968 ) = 'in'
      TEXT( 1969 ) = '@literal'
      TEXT( 1970 ) = 'SHOW COLUMN table.column_name'
      TEXT( 1971 ) = '|endliteral'
      TEXT( 1972 ) = 'where <table> is the name of the column'
     .//             ' of interest .'
      TEXT( 1973 ) = 'Below is a possible result of the comma'
     .//             'nd SHOW COLUMN ACTIVITY;'
      TEXT( 1974 ) = '@literal'
      TEXT( 1975 ) = 'Attributes of column:     :  ACTIVITY'
      TEXT( 1976 ) = 'Type                      :  CHARACTER*'
     .//             '(32)'
      TEXT( 1977 ) = 'Indexed                   :  YES'
      TEXT( 1978 ) = 'Number of Components      :  1'
      TEXT( 1979 ) = ' '
      TEXT( 1980 ) = 'User Adjustable Attributes'
      TEXT( 1981 ) = 'Column justification      :  LEFT'
      TEXT( 1982 ) = 'Column width              :  32'
      TEXT( 1983 ) = 'Column heading            :  ACTIVITY'
      TEXT( 1984 ) = '|endliteral'
      TEXT( 1985 ) = ' '
      TEXT( 1986 ) = '@@SHOW COLUMN   ...'
      TEXT( 1987 ) = 'Quit Help'
      TEXT( 1988 ) = 'Help'
      TEXT( 1989 ) = 'SET COLUMN ...'
      FINISH( 54 ) = 1989
 
      BEGIN ( 55 ) = 1990
      TEXT( 1990 ) = 'Every SPICE kernel provides a mechanism'
     .//             ' for the creator of the'
      TEXT( 1991 ) = 'product to attach documentation to the '
     .//             'kernel.  This documentation is stored i'
     .//             'n'
      TEXT( 1992 ) = 'of the file called the "comments" area.'
      TEXT( 1993 ) = 'All kernels should'
      TEXT( 1994 ) = 'have a non-empty comment section. The c'
     .//             'omments typically will'
      TEXT( 1995 ) = 'provide information on one or more of t'
     .//             'he following items:'
      TEXT( 1996 ) = '@newlist'
      TEXT( 1997 ) = '@numitem the date the kernel was create'
     .//             'd,'
      TEXT( 1998 ) = ' '
      TEXT( 1999 ) = '@numitem who created it,'
      TEXT( 2000 ) = ' '
      TEXT( 2001 ) = '@numitem who to contact if you have que'
     .//             'stions about the kernel,'
      TEXT( 2002 ) = ' '
      TEXT( 2003 ) = '@numitem the intended set of users of t'
     .//             'he kernel,'
      TEXT( 2004 ) = ' '
      TEXT( 2005 ) = '@numitem special notes regarding the co'
     .//             'ntents of the kernel.'
      TEXT( 2006 ) = ' '
      TEXT( 2007 ) = 'To see the comments stored in a loaded '
     .//             'E-kernel issue the command'
      TEXT( 2008 ) = '@literal'
      TEXT( 2009 ) = 'SHOW COMMENTS pattern'
      TEXT( 2010 ) = '|endliteral'
      TEXT( 2011 ) = 'If the name of a loaded kernel'
      TEXT( 2012 ) = 'matches this pattern, the comments for '
     .//             'that kernel will be'
      TEXT( 2013 ) = 'displayed.'
      TEXT( 2014 ) = ' '
      TEXT( 2015 ) = 'If none of the kernel names match this '
     .//             'pattern, a message informing'
      TEXT( 2016 ) = 'you of this condition will be displayed'
     .//             '.  If a kernel matches the'
      TEXT( 2017 ) = 'pattern, but no comments are in the ker'
     .//             'nel a message will be displayed'
      TEXT( 2018 ) = 'indicating that no comments are availab'
     .//             'le.'
      TEXT( 2019 ) = ' '
      TEXT( 2020 ) = '@@SHOW COMMENTS ...'
      TEXT( 2021 ) = 'Quit Help'
      TEXT( 2022 ) = 'Help'
      FINISH( 55 ) = 2022
 
      BEGIN ( 56 ) = 2023
      TEXT( 2023 ) = 'There are a number of more or less glob'
     .//             'al features of'
      TEXT( 2024 ) = 'an Inspekt session that affect how Insp'
     .//             'ekt carries out'
      TEXT( 2025 ) = 'the commands you issue.  These items ar'
     .//             'e grouped together'
      TEXT( 2026 ) = 'under the term Environment.  They inclu'
     .//             'de:'
      TEXT( 2027 ) = '@newlist'
      TEXT( 2028 ) = '@numitem The editor used when you EDIT '
     .//             'a command.'
      TEXT( 2029 ) = ' '
      TEXT( 2030 ) = '@numitem Whether or not Help waits for '
     .//             'you to'
      TEXT( 2031 ) = '         finish reading a page before i'
     .//             't prints'
      TEXT( 2032 ) = '         the next screen out material'
 
      RETURN
      END
