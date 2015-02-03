C$Procedure      ZZHLP009 ( private help text )
 
      SUBROUTINE ZZHLP009 ( BEGIN, FINISH, TEXT )
 
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
      TEXT( 859 ) = 'There are two versions of Delimited repo'
     .//            'rts: preserved and'
      TEXT( 860 ) = 'non-preserved. The two forms are identic'
     .//            'al if all of the'
      TEXT( 861 ) = 'columns of the report are scalar valued.'
     .//            ' However, when some'
      TEXT( 862 ) = 'column has more than one component, the '
     .//            'two reports are'
      TEXT( 863 ) = 'different.'
      TEXT( 864 ) = ' '
      TEXT( 865 ) = 'If you specify the report to be DELIMITE'
     .//            'D, all componets of a column'
      TEXT( 866 ) = 'are placed together in the output separa'
     .//            'ted by a space character.'
      TEXT( 867 ) = ' '
      TEXT( 868 ) = 'If you specify the report to be DELIMITE'
     .//            'D PRESERVED, each column is'
      TEXT( 869 ) = 'regarded as having the same number of co'
     .//            'mponents -- the maximum number'
      TEXT( 870 ) = 'of components of all columns in the row '
     .//            'of the report. If n is the'
      TEXT( 871 ) = 'maximum number of components for any req'
     .//            'uested column of a matching'
      TEXT( 872 ) = 'row, the row is written out in n-lines. '
     .//            'The first line contains the'
      TEXT( 873 ) = 'first component of each column, the seco'
     .//            'nd line contains the second'
      TEXT( 874 ) = 'component of each column, and so on. If '
     .//            'a column does not have an i''th'
      TEXT( 875 ) = 'component, a blank character (unquoted) '
     .//            'is written for that component.'
      TEXT( 876 ) = ' '
      TEXT( 877 ) = '@@Delimited Format'
      TEXT( 878 ) = 'Quit Help'
      TEXT( 879 ) = 'Help'
      TEXT( 880 ) = 'Reports'
      FINISH( 14 ) = 880
 
      BEGIN ( 15 ) = 881
      TEXT( 881 ) = 'To see the current data-deluge warning l'
     .//            'evel type the command'
      TEXT( 882 ) = '@literal'
      TEXT( 883 ) = 'SHOW FORMAT'
      TEXT( 884 ) = '|endliteral'
      TEXT( 885 ) = ' '
      TEXT( 886 ) = 'To set the deluge warning level, type th'
     .//            'e command.'
      TEXT( 887 ) = '@literal'
      TEXT( 888 ) = 'SET DELUGE WARNING integer'
      TEXT( 889 ) = '|endliteral'
      TEXT( 890 ) = 'If you take no action the warning level '
     .//            'has value 100.'
      TEXT( 891 ) = ' '
      TEXT( 892 ) = '@@Deluge Warning'
      TEXT( 893 ) = 'Quit Help'
      TEXT( 894 ) = 'Help'
      TEXT( 895 ) = 'Getting Too Much Data'
      TEXT( 896 ) = 'Sampling Data'
      FINISH( 15 ) = 896
 
      BEGIN ( 16 ) = 897
      TEXT( 897 ) = 'The area of the terminal or terminal win'
     .//            'dow where reports are'
      TEXT( 898 ) = 'displayed is called the output page. Rep'
     .//            'orts may span several'
      TEXT( 899 ) = 'output pages.  Inspekt allows you to adj'
     .//            'ust the shape of the output'
      TEXT( 900 ) = 'page and to control the titles and heade'
     .//            'rs that appear on the'
      TEXT( 901 ) = 'page. The commands that allow you to man'
     .//            'ipulate the output page'
      TEXT( 902 ) = 'are shown below.'
      TEXT( 903 ) = '@literal'
      TEXT( 904 ) = '   SET PAGE WIDTH number'
      TEXT( 905 ) = '   SET PAGE HEIGHT number'
      TEXT( 906 ) = '   SET PAGE TITLE title'
      TEXT( 907 ) = '   SET TITLE JUSTIFICATION justification'
      TEXT( 908 ) = '   SET TITLE FREQUENCY frequency'
      TEXT( 909 ) = '   SET HEADER FREQUENCY frequency'
      TEXT( 910 ) = '|endliteral'
      TEXT( 911 ) = ' '
      TEXT( 912 ) = 'The output page is described in terms of'
     .//            ' printable fixed size'
      TEXT( 913 ) = 'characters. By default it is 80 characte'
     .//            'rs wide and 24 characters'
      TEXT( 914 ) = 'tall. The output page is a subset of  a '
     .//            'page region that is'
      TEXT( 915 ) = 'between 40 and 132 characters wide and a'
     .//            't least 22 characters tall.'
      TEXT( 916 ) = '(The maximum page height allowed is the '
     .//            'maximum positive integer'
      TEXT( 917 ) = 'that your computer supports)'
      TEXT( 918 ) = ' '
      TEXT( 919 ) = 'The page width is used to determine how '
     .//            'much text can fit across'
      TEXT( 920 ) = 'your terminal window.  The page height i'
     .//            's used to determine how'
      TEXT( 921 ) = 'many lines of text can be written before'
     .//            ' displaying a page title'
      TEXT( 922 ) = 'or report header.'
      TEXT( 923 ) = ' '
      TEXT( 924 ) = '@@Display Area'
      TEXT( 925 ) = 'Quit Help'
      TEXT( 926 ) = 'Help'
      TEXT( 927 ) = 'Reports'
      TEXT( 928 ) = 'SET TIME   ...'
      TEXT( 929 ) = 'SET HEADER ...'
      TEXT( 930 ) = 'SET PAGE   ...'
      FINISH( 16 ) = 930
 
      BEGIN ( 17 ) = 931
      TEXT( 931 ) = 'Although symbols can be a big help when '
     .//            'used well, they can'
      TEXT( 932 ) = 'also interfere with what you think you'''
     .//            've typed.  Usually you'
      TEXT( 933 ) = 'discover this problem as the result of a'
     .//            'n error message.'
      TEXT( 934 ) = ' '
      TEXT( 935 ) = 'You may want to see how Inspekt translat'
     .//            'ed what you typed before it'
      TEXT( 936 ) = 'gets around to acting on the command.  T'
     .//            'o do this you can'
      TEXT( 937 ) = '"turn on" command echoing.  Do this by t'
     .//            'yping'
      TEXT( 938 ) = '@literal'
      TEXT( 939 ) = 'ECHO'
      TEXT( 940 ) = '|endliteral'
      TEXT( 941 ) = 'Having issued this command, when a new c'
     .//            'ommand is issued that'
      TEXT( 942 ) = 'contains symbols, Inspekt will "echo" th'
     .//            'e translated command'
      TEXT( 943 ) = 'so that you can see how Inspekt has inte'
     .//            'rpreted your command.'
      TEXT( 944 ) = ' '
      TEXT( 945 ) = 'To disable the echoing of commands type:'
      TEXT( 946 ) = '@literal'
      TEXT( 947 ) = 'NO ECHO'
      TEXT( 948 ) = '|endliteral'
      TEXT( 949 ) = ' '
 
      RETURN
      END
