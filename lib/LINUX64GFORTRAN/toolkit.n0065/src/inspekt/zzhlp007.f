C$Procedure      ZZHLP007 ( private help text )
 
      SUBROUTINE ZZHLP007 ( BEGIN, FINISH, TEXT )
 
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
      TEXT( 674 ) = 'Time Formats'
      TEXT( 675 ) = 'Titles'
      TEXT( 676 ) = 'Other Settings'
      TEXT( 677 ) = 'Setting up Inspekt --- SET'
      FINISH( 9 ) = 677
 
      BEGIN ( 10 ) = 678
      TEXT( 678 ) = 'To create a custom time format for a col'
     .//            'umn, enter the command:'
      TEXT( 679 ) = '@literal'
      TEXT( 680 ) = 'SET COLUMN <column_name> FORMAT <format>'
     .//            ';'
      TEXT( 681 ) = '|endliteral'
      TEXT( 682 ) = 'where <column_name> is the name of the c'
     .//            'olumn and <format> is the'
      TEXT( 683 ) = 'custom format you desire.'
      TEXT( 684 ) = ' '
      TEXT( 685 ) = 'Custom formats work as follows.  Given a'
     .//            ' time, there are associated'
      TEXT( 686 ) = 'with it the current year, month, day, da'
     .//            'y of year, hour, minutes,'
      TEXT( 687 ) = 'seconds, current julian date, current nu'
     .//            'mber of seconds past the'
      TEXT( 688 ) = 'epoch of J2000, etc.  When a time is to '
     .//            'be displayed, the custom'
      TEXT( 689 ) = 'format you have provided is used as a re'
     .//            'cipe for constructing the'
      TEXT( 690 ) = 'time string.  Reading from left to right'
     .//            ' the string formatter looks'
      TEXT( 691 ) = 'for special substrings (listed below).  '
     .//            'Unrecognized substrings'
      TEXT( 692 ) = 'are simply copied into the output string'
     .//            '.  (This allows you to add'
      TEXT( 693 ) = 'any label you might like to the output t'
     .//            'imes.)  However, when a'
      TEXT( 694 ) = 'recognized substring is found, the time '
     .//            'formatter determines the'
      TEXT( 695 ) = 'corresponding component of time and appe'
     .//            'nds this to the output time'
      TEXT( 696 ) = 'string that is under construction.'
      TEXT( 697 ) = ' '
      TEXT( 698 ) = 'NOTE THAT TIME FORMATS ARE CASE SENSITIV'
     .//            'E.  To get a particular'
      TEXT( 699 ) = 'component of the time into the output st'
     .//            'ring you must use exactly'
      TEXT( 700 ) = 'the substring given in the list below. F'
     .//            'or example, if you wish'
      TEXT( 701 ) = 'have the 3 letter abbreviation for the m'
     .//            'onth appear in your output'
      TEXT( 702 ) = 'times, you must use "MON";  the string "'
     .//            'mon" will simply be copied'
      TEXT( 703 ) = 'as is into any of your time strings.'
      TEXT( 704 ) = ' '
      TEXT( 705 ) = '(Substrings beginning with "::" do not a'
     .//            'ffect the appearance of'
      TEXT( 706 ) = 'the format only the time system or round'
     .//            'ing)'
      TEXT( 707 ) = ' '
      TEXT( 708 ) = '@setparamsize{::UTC,::TDB,::TDT}'
      TEXT( 709 ) = '@param ::UTC,::TDB,::TDT.'
      TEXT( 710 ) = ' use the time system UTC, TDB, TDT respe'
     .//            'ctively  (default UTC)'
      TEXT( 711 ) = ' '
      TEXT( 712 ) = '@param ::RND, ::TRUNC.'
      TEXT( 713 ) = ' Round or Truncate time respectively (de'
     .//            'fault truncate)'
      TEXT( 714 ) = ' '
      TEXT( 715 ) = '@param YYYY.'
      TEXT( 716 ) = 'year'
      TEXT( 717 ) = ' '
      TEXT( 718 ) = '@param MON, MM.'
      TEXT( 719 ) = ' 3 letter abbreviation, 2 digit number f'
     .//            'or month resp.'
      TEXT( 720 ) = ' '
      TEXT( 721 ) = '@param DD, DOY.'
      TEXT( 722 ) = ' day of month, day of year respectively'
      TEXT( 723 ) = ' '
      TEXT( 724 ) = '@param WKD.'
      TEXT( 725 ) = ' 3 letter abbreviation for day of week'
      TEXT( 726 ) = ' '
      TEXT( 727 ) = '@param HR, MN, SC.'
      TEXT( 728 ) = ' hour, minutes, seconds respectively'
      TEXT( 729 ) = ' '
      TEXT( 730 ) = '@param JD,SP1950, SP2000.'
      TEXT( 731 ) = ' Julian date, seconds past 1950 or 2000 '
     .//            'respectively'
      TEXT( 732 ) = ' '
      TEXT( 733 ) = ' '
      TEXT( 734 ) = '@param ##---#.'
      TEXT( 735 ) = 'when these follow  a decimal point, they'
     .//            ' indicate the number of'
      TEXT( 736 ) = 'decimal places to use in the representat'
     .//            'ion of the'
      TEXT( 737 ) = 'preceding numeric component.  For exampl'
     .//            'e ''SC.###'' indicates that'
      TEXT( 738 ) = 'the seconds component of a time should b'
     .//            'e presented with'
      TEXT( 739 ) = '3 decimal points.'
      TEXT( 740 ) = '@@Custom Formats'
      TEXT( 741 ) = 'Quit Help'
      TEXT( 742 ) = 'Help'
      TEXT( 743 ) = 'Example Time Formats'
      FINISH( 10 ) = 743
 
      BEGIN ( 11 ) = 744
      TEXT( 744 ) = 'When printing a double precision number '
     .//            'in a report,'
      TEXT( 745 ) = 'Inspekt first examines'
      TEXT( 746 ) = 'the column attributes to determine if yo'
     .//            'u have specified'
      TEXT( 747 ) = 'a particular format for that column.  If'
     .//            ' you have that format'
      TEXT( 748 ) = 'is used to create the text that is prese'
     .//            'nted in the report.'
      TEXT( 749 ) = 'If you have not specified a particular f'
     .//            'ormat, Inspekt looks'
      TEXT( 750 ) = 'up the "default floating format" and use'
     .//            's this to create the text'
      TEXT( 751 ) = 'to be used in the report.  You may adjus'
     .//            't the default floating'
      TEXT( 752 ) = 'format.  To do this issue the command'
      TEXT( 753 ) = ' '
      TEXT( 754 ) = '@literal'
      TEXT( 755 ) = 'SET DEFAULT FLOATING FORMAT format;'
      TEXT( 756 ) = '|endliteral'
      TEXT( 757 ) = ' '
      TEXT( 758 ) = 'where "format" is the format you''d like'
     .//            ' Inspekt to use'
      TEXT( 759 ) = 'when you have not specified a particular'
     .//            ' format for a column.'
      TEXT( 760 ) = ' '
      TEXT( 761 ) = 'If you''ve specified a format for a doub'
     .//            'le precision column, and would'
      TEXT( 762 ) = 'like to return to using the default floa'
     .//            'ting format issue the'
      TEXT( 763 ) = 'command'
      TEXT( 764 ) = '@literal'
      TEXT( 765 ) = 'SET COLUMN column_name FORMAT DEFAULT;'
      TEXT( 766 ) = '|endliteral'
      TEXT( 767 ) = 'Until you change the format for the spec'
     .//            'ified column again,'
 
      RETURN
      END
