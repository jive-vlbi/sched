C$Procedure      ZZHLP006 ( private help text )
 
      SUBROUTINE ZZHLP006 ( BEGIN, FINISH, TEXT )
 
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
      TEXT( 577 ) = 'EQ             column EQ   expression'
      TEXT( 578 ) = 'GE             column GE   expression'
      TEXT( 579 ) = 'GT             column GT   expression'
      TEXT( 580 ) = 'NE             column NE   expression'
      TEXT( 581 ) = '<              column <    expression'
      TEXT( 582 ) = '<=             column <=   expression'
      TEXT( 583 ) = '=              column =    expression'
      TEXT( 584 ) = '>=             column >=   expression'
      TEXT( 585 ) = '>              column >    expression'
      TEXT( 586 ) = '!=             column <>   expression'
      TEXT( 587 ) = '<>             column !=   expression'
      TEXT( 588 ) = 'LIKE           column LIKE expression'
      TEXT( 589 ) = 'NOT LIKE       column NOT LIKE expressio'
     .//            'n'
      TEXT( 590 ) = ' '
      TEXT( 591 ) = 'BETWEEN        column BETWEEN expression'
      TEXT( 592 ) = '                      AND     expression'
      TEXT( 593 ) = ' '
      TEXT( 594 ) = 'NOT BETWEEN    column NOT BETWEEN expres'
     .//            'sion'
      TEXT( 595 ) = '                      AND         expres'
     .//            'sion'
      TEXT( 596 ) = '|endliteral'
      TEXT( 597 ) = 'where "column" is the name of some colum'
     .//            'n and "expression"'
      TEXT( 598 ) = 'is the name of a column or a literal val'
     .//            'ue such as 1 or'
      TEXT( 599 ) = '"A Literal String".'
      TEXT( 600 ) = ' '
      TEXT( 601 ) = 'Conditions listed above are true if:'
      TEXT( 602 ) = ' '
      TEXT( 603 ) = '@setparamsize{NOT BETWEEN}'
      TEXT( 604 ) = '@param LT or <.'
      TEXT( 605 ) = 'the value column is less than the value '
     .//            'of expression.'
      TEXT( 606 ) = ' '
      TEXT( 607 ) = '@param LE or <=.'
      TEXT( 608 ) = 'the value of column is less than or equa'
     .//            'l to the value'
      TEXT( 609 ) = 'of expression'
      TEXT( 610 ) = ' '
      TEXT( 611 ) = '@param EQ or =.'
      TEXT( 612 ) = 'the value of column is equal to the valu'
     .//            'e of expression.'
      TEXT( 613 ) = 'Note that for strings, the case of chara'
     .//            'cters is significant.'
      TEXT( 614 ) = 'The strings ''A'' and ''a'' are not equa'
     .//            'l.'
      TEXT( 615 ) = ' '
      TEXT( 616 ) = '@param GE or >=.'
      TEXT( 617 ) = 'the value of column is greater than or e'
     .//            'qual to the value'
      TEXT( 618 ) = 'of expression.'
      TEXT( 619 ) = ' '
      TEXT( 620 ) = '@param GT or >.'
      TEXT( 621 ) = 'the value of column is greater than the '
     .//            'value of expression.'
      TEXT( 622 ) = ' '
      TEXT( 623 ) = '@param NE or != or <>.'
      TEXT( 624 ) = 'the value of column is not equal to the '
     .//            'value of expression.'
      TEXT( 625 ) = ' '
      TEXT( 626 ) = '@param LIKE.'
      TEXT( 627 ) = 'the value of column matches the value of'
     .//            ' expression when'
      TEXT( 628 ) = 'expression is interpreted as a pattern.'
      TEXT( 629 ) = ' '
      TEXT( 630 ) = '@param NOT LIKE.'
      TEXT( 631 ) = 'The value of column does not match the v'
     .//            'alue of expression'
      TEXT( 632 ) = 'when expression is interpreted as a patt'
     .//            'ern.'
      TEXT( 633 ) = ' '
      TEXT( 634 ) = '@param BETWEEN.'
      TEXT( 635 ) = 'The value of column is greater than or e'
     .//            'qual to the smaller of'
      TEXT( 636 ) = 'the two expressions AND less than or equ'
     .//            'al to the larger of the'
      TEXT( 637 ) = 'two expressions.'
      TEXT( 638 ) = ' '
      TEXT( 639 ) = '@param NOT BETWEEN'
      TEXT( 640 ) = 'The value of column is less than the sma'
     .//            'ller of the two expression'
      TEXT( 641 ) = 'OR greater than the larger of the two ex'
     .//            'pression.'
      TEXT( 642 ) = ' '
      TEXT( 643 ) = 'A WHERE clause is composed of the word "'
     .//            'WHERE" followed by'
      TEXT( 644 ) = 'a logical expression made up of conditio'
     .//            'ns connected by'
      TEXT( 645 ) = 'AND''s, OR''s and NOT''s and grouped usi'
     .//            'ng parentheses.'
      TEXT( 646 ) = ' '
      TEXT( 647 ) = '@@Conditional Operators'
      TEXT( 648 ) = 'Quit Help'
      TEXT( 649 ) = 'Help'
      TEXT( 650 ) = 'Specifying Strings'
      TEXT( 651 ) = 'Specifying Times'
      TEXT( 652 ) = 'Where Clause'
      FINISH( 8 ) = 652
 
      BEGIN ( 9 ) = 653
      TEXT( 653 ) = 'Inspekt can display any of it''s current'
     .//            ' setting.  However, these'
      TEXT( 654 ) = 'settings are grouped together in various'
     .//            ' groupings.  To see one'
      TEXT( 655 ) = 'of these grouping you type'
      TEXT( 656 ) = '@literal'
      TEXT( 657 ) = 'SHOW item'
      TEXT( 658 ) = '|endliteral'
      TEXT( 659 ) = 'See the help topics below for more speci'
     .//            'fic descriptions.'
      TEXT( 660 ) = '@@Current Settings   --- SHOW'
      TEXT( 661 ) = 'Quit Help'
      TEXT( 662 ) = 'Help'
      TEXT( 663 ) = 'SHOW COLUMN      ...'
      TEXT( 664 ) = 'SHOW COMMENTS    ...'
      TEXT( 665 ) = 'SHOW ENVIRONMENT ...'
      TEXT( 666 ) = 'SHOW FORMAT      ...'
      TEXT( 667 ) = 'SHOW INDEXED     ...'
      TEXT( 668 ) = 'SHOW KERNELS     ...'
      TEXT( 669 ) = 'SHOW PAGE        ...'
      TEXT( 670 ) = 'SHOW SUMMARY     ...'
      TEXT( 671 ) = 'Columns'
      TEXT( 672 ) = 'Deluge Warning'
      TEXT( 673 ) = 'Headers'
 
      RETURN
      END
