C$Procedure      ZZHLP010 ( private help text )
 
      SUBROUTINE ZZHLP010 ( BEGIN, FINISH, TEXT )
 
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
      TEXT( 950 ) = 'You can see the current status of comman'
     .//            'd echoing by issuing'
      TEXT( 951 ) = 'a show environment command.'
      TEXT( 952 ) = '@literal'
      TEXT( 953 ) = 'SHOW ENVIRONMENT'
      TEXT( 954 ) = '|endliteral'
      TEXT( 955 ) = '@@Echoing Translated Commands'
      TEXT( 956 ) = 'Quit Help'
      TEXT( 957 ) = 'Help'
      FINISH( 17 ) = 957
 
      BEGIN ( 18 ) = 958
      TEXT( 958 ) = 'Inspekt maintains a "stack" of the twent'
     .//            'y most recently entered'
      TEXT( 959 ) = 'commands.  Each new command is placed on'
     .//            ' "top" of the other commands.'
      TEXT( 960 ) = 'Once a command has twenty or more comman'
     .//            'ds on top of it, it is lost'
      TEXT( 961 ) = 'and can not be retrieved.'
      TEXT( 962 ) = ' '
      TEXT( 963 ) = 'Commands in the stack are numbered from '
     .//            '1 to 20.  The most recently'
      TEXT( 964 ) = 'issued command (the one on top) is numbe'
     .//            'r 1.  The command issued'
      TEXT( 965 ) = 'just prior to number 1,  is number 2, an'
     .//            'd so on.'
      TEXT( 966 ) = ' '
      TEXT( 967 ) = 'You can manipulate this stack with the c'
     .//            'ommands below.  Most'
      TEXT( 968 ) = '"stack" manipulation commands require th'
     .//            'at you specify a particular'
      TEXT( 969 ) = 'command.  You can do this by specifying '
     .//            'the number of the command'
      TEXT( 970 ) = '(the number provide by RECALL) or by spe'
     .//            'cifying a pattern. If you'
      TEXT( 971 ) = 'specify a command via a number, the numb'
     .//            'er must be between 1 and'
      TEXT( 972 ) = '20. If you specify the command via a pat'
     .//            'tern, you are specifying'
      TEXT( 973 ) = 'the last command whose first line matche'
     .//            's the pattern.  As a'
      TEXT( 974 ) = 'convenience, the wild card ''*'' is auto'
     .//            'matically appended to the'
      TEXT( 975 ) = 'specification pattern.  You can not supp'
     .//            'ress the appended wild'
      TEXT( 976 ) = 'card.'
      TEXT( 977 ) = ' '
      TEXT( 978 ) = '@setparamsize{RECALL pattern}'
      TEXT( 979 ) = '@param  RECALL.'
      TEXT( 980 ) = 'displays the commands in the reverse ord'
     .//            'er from the way you have'
      TEXT( 981 ) = 'entered them.  The commands are numbered'
     .//            ' so that you may easily'
      TEXT( 982 ) = 'refer to them.  The stack is left unchan'
     .//            'ged.'
      TEXT( 983 ) = ' '
      TEXT( 984 ) = ' '
      TEXT( 985 ) = '@param RECALL number @cr'
      TEXT( 986 ) = '       RECALL pattern.'
      TEXT( 987 ) = 'displays a specific command.  The stack '
     .//            'is left unchanged.'
      TEXT( 988 ) = ' '
      TEXT( 989 ) = ' '
      TEXT( 990 ) = '@param DO number @cr'
      TEXT( 991 ) = '       DO pattern.'
      TEXT( 992 ) = 're-executes the command specified. The r'
     .//            'e-executed command is placed'
      TEXT( 993 ) = 'on the top of the command stack.'
      TEXT( 994 ) = ' '
      TEXT( 995 ) = ' '
      TEXT( 996 ) = '@param EDIT number @cr'
      TEXT( 997 ) = '       EDIT pattern.'
      TEXT( 998 ) = 'invokes your system editor and allows yo'
     .//            'u to use the editor'
      TEXT( 999 ) = 'to modify the command.  When you exit th'
     .//            'e editor, the new command'
      TEXT( 1000 ) = 'is executed and placed on the top of th'
     .//             'e command stack.'
      TEXT( 1001 ) = ' '
      TEXT( 1002 ) = '@@Editing Commands'
      TEXT( 1003 ) = 'Quit Help'
      TEXT( 1004 ) = 'Help'
      TEXT( 1005 ) = 'Pattern Matching'
      TEXT( 1006 ) = 'Setting The Editor'
      FINISH( 18 ) = 1006
 
      BEGIN ( 19 ) = 1007
      TEXT( 1007 ) = 'Inspekt recognizes two environment vari'
     .//             'ables: LEAPSECONDS and SCLK.'
      TEXT( 1008 ) = 'If you want to make them available to I'
     .//             'nspekt, you should create'
      TEXT( 1009 ) = 'one or both of them prior to running In'
     .//             'spekt.'
      TEXT( 1010 ) = '@setparamsize{LEAPSECONDS}'
      TEXT( 1011 ) = '@param LEAPSECONDS.'
      TEXT( 1012 ) = 'should point to a SPICE leapseconds ker'
     .//             'nel.'
      TEXT( 1013 ) = ' '
      TEXT( 1014 ) = '@param SCLK.'
      TEXT( 1015 ) = 'should point to a SPICE Spacecraft Cloc'
     .//             'k kernel.'
      TEXT( 1016 ) = ' '
      TEXT( 1017 ) = 'Inspekt looks for these environment var'
     .//             'iables at program initialization.'
      TEXT( 1018 ) = 'If they are present, Inspekt will load '
     .//             'the kernels pointed to by'
      TEXT( 1019 ) = 'the environment variables.  This is the'
     .//             ' only time these variables'
      TEXT( 1020 ) = 'can be used by Inspekt.  You may not us'
     .//             'e them in a LOAD command.'
      TEXT( 1021 ) = ' '
      TEXT( 1022 ) = 'If you should decide to use these envir'
     .//             'onment variables make sure'
      TEXT( 1023 ) = 'that they evaluate to the full path nam'
     .//             'e of the kernels.  You may'
      TEXT( 1024 ) = 'set these  variables with C-shell comma'
     .//             'nd "setenv"'
      TEXT( 1025 ) = '@literal'
      TEXT( 1026 ) = 'setenv LEAPSECONDS (full path of leapse'
     .//             'conds kernel)'
      TEXT( 1027 ) = 'setenv SCLK        (full path of spacec'
     .//             'raft clock kernel)'
      TEXT( 1028 ) = '|endliteral'
      TEXT( 1029 ) = ' '
      TEXT( 1030 ) = 'In DCL you use logical variables instea'
     .//             'd of environment variables.'
      TEXT( 1031 ) = 'You set the logical variables using the'
     .//             ' define command.'
      TEXT( 1032 ) = '@literal'
      TEXT( 1033 ) = 'DEFIND LEAPSECONDS DISK:[DIR.ECT.ORY]le'
     .//             'ap_file.ker'
      TEXT( 1034 ) = 'DEFINE SCLK        DISK:[DIR.ECT.ORY]sc'
     .//             'lk_file.ker'
      TEXT( 1035 ) = '|endliteral'
      TEXT( 1036 ) = ' '
      TEXT( 1037 ) = '@@Environment Variables'
      TEXT( 1038 ) = 'Quit Help'
      TEXT( 1039 ) = 'Help'
      FINISH( 19 ) = 1039
 
      BEGIN ( 20 ) = 1040
      TEXT( 1040 ) = 'Every now and then, you will type a com'
     .//             'mand that Inspekt cannot'
 
      RETURN
      END
