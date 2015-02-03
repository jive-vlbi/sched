C$Procedure      ZZHLP005 ( private help text )
 
      SUBROUTINE ZZHLP005 ( BEGIN, FINISH, TEXT )
 
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
      TEXT( 480 ) = 'To select columns you simply use the ali'
     .//            'ased names as'
      TEXT( 481 ) = 'presented in the table above.  Thus to f'
     .//            'ind all of the grandfathers'
      TEXT( 482 ) = 'in our original table RELATIONS we would'
     .//            ' issue the following'
      TEXT( 483 ) = 'command'
      TEXT( 484 ) = '@literal'
      TEXT( 485 ) = 'SELECT  Y.FATHER'
      TEXT( 486 ) = 'FROM   RELATIONS X, RELATIONS Y'
      TEXT( 487 ) = 'WHERE   X.FATHER = Y.CHILD'
      TEXT( 488 ) = 'OR      X.MOTHER = Y.CHILD'
      TEXT( 489 ) = '|endliteral'
      TEXT( 490 ) = 'Here is how the conditions narrow down t'
     .//            'he join.'
      TEXT( 491 ) = ' '
      TEXT( 492 ) = '@literal'
      TEXT( 493 ) = 'Join of RELATIONS X, RELATIONS Y'
      TEXT( 494 ) = 'WHERE   X.FATHER = Y.CHILD'
      TEXT( 495 ) = 'OR      X.MOTHER = Y.CHILD'
      TEXT( 496 ) = ' '
      TEXT( 497 ) = 'X.CHILD  X.FATHER X.MOTHER   Y.CHILD Y.F'
     .//            'ATHER  Y.MOTHER'
      TEXT( 498 ) = '----------------------------------------'
     .//            '-----------------'
      TEXT( 499 ) = 'CINDY    GEORGE   ALICE      GEORGE  WIL'
     .//            'LIAM   MARGARET'
      TEXT( 500 ) = 'CINDY    GEORGE   ALICE      ALICE   JOH'
     .//            'N      KATHERINE'
      TEXT( 501 ) = 'SALLY    GEORGE   MARTHA     GEORGE  WIL'
     .//            'LIAM   MARGARET'
      TEXT( 502 ) = 'SALLY    GEORGE   MARTHA     MARTHA  ROB'
     .//            'ERT    CONSTANCE'
      TEXT( 503 ) = '|endliteral'
      TEXT( 504 ) = ' '
      TEXT( 505 ) = 'We get all the names in the Y.FATHER col'
     .//            'umn'
      TEXT( 506 ) = 'where the CHILD appearing in the Y copy '
     .//            'of RELATIONS appears'
      TEXT( 507 ) = 'as a parent in the X copy of RELATIONS.'
      TEXT( 508 ) = ' '
      TEXT( 509 ) = 'To find every child that is a grandchild'
     .//            ','
      TEXT( 510 ) = 'we can simply pick out the children whos'
     .//            'e mothers'
      TEXT( 511 ) = 'are listed as children in the second cop'
     .//            'y of RELATIONS.'
      TEXT( 512 ) = ' '
      TEXT( 513 ) = '@literal'
      TEXT( 514 ) = 'SELECT X.CHILD'
      TEXT( 515 ) = 'FROM RELATIONS X, RELATION Y'
      TEXT( 516 ) = 'WHERE X.MOTHER = Y.CHILD'
      TEXT( 517 ) = '|endliteral'
      TEXT( 518 ) = ' '
      TEXT( 519 ) = '@subsection Using Aliases'
      TEXT( 520 ) = ' '
      TEXT( 521 ) = 'Even if you do not join a table to itsel'
     .//            'f, you may wish to'
      TEXT( 522 ) = 'use an alias for a table name.  For inst'
     .//            'ance, in the second'
      TEXT( 523 ) = 'example where the tables MUSIC and PEOPL'
     .//            'E had a column name'
      TEXT( 524 ) = 'in column, you can use an alias as shown'
     .//            ' below in order to'
      TEXT( 525 ) = 'simplify the typing.'
      TEXT( 526 ) = '@literal'
      TEXT( 527 ) = 'Original Select Command Without Aliases'
      TEXT( 528 ) = ' '
      TEXT( 529 ) = 'select song'
      TEXT( 530 ) = 'from music, people'
      TEXT( 531 ) = 'where MUSIC.COMPOSER = PEOPLE.COMPOSER'
      TEXT( 532 ) = 'and   country  = ''IRELAND'''
      TEXT( 533 ) = ' '
      TEXT( 534 ) = 'Same Select Command but with Aliases for'
     .//            ' the Tables'
      TEXT( 535 ) = ' '
      TEXT( 536 ) = 'select song'
      TEXT( 537 ) = 'from music M, people P'
      TEXT( 538 ) = 'where M.COMPOSER = P.COMPOSER'
      TEXT( 539 ) = 'and   country  = ''IRELAND'''
      TEXT( 540 ) = '|endliteral'
      TEXT( 541 ) = ' '
      TEXT( 542 ) = '@subsection Combining More than Two Tabl'
     .//            'es'
      TEXT( 543 ) = ' '
      TEXT( 544 ) = 'You may join more than two tables if the'
     .//            ' need arises.  The'
      TEXT( 545 ) = 'result is the cartesian product of the c'
     .//            'ontents of the contributing'
      TEXT( 546 ) = 'tables.'
      TEXT( 547 ) = ' '
      TEXT( 548 ) = 'The syntax for specifying such a join is'
     .//            ':'
      TEXT( 549 ) = '@literal'
      TEXT( 550 ) = 'FROM  TABLE_1 [alias_1],'
      TEXT( 551 ) = '      TABLE_2 [alias_2],'
      TEXT( 552 ) = '      ...,'
      TEXT( 553 ) = '      TABLE_N [alias_N]'
      TEXT( 554 ) = '|endliteral'
      TEXT( 555 ) = ' '
      TEXT( 556 ) = 'Inspekt and the E-kernel system require '
     .//            'that N be no more than 10.'
      TEXT( 557 ) = 'As you might imagine, the join of many t'
     .//            'ables has the potential'
      TEXT( 558 ) = 'for creating an enormous table from whic'
     .//            'h data will be selected.'
      TEXT( 559 ) = 'As a result Inspekt''s speed may degrade'
     .//            ' significantly if you'
      TEXT( 560 ) = 'join many tables.'
      TEXT( 561 ) = '@@Combining Tables'
      TEXT( 562 ) = 'Quit Help'
      TEXT( 563 ) = 'Help'
      TEXT( 564 ) = 'From Clause'
      TEXT( 565 ) = 'Looking at Data    --- SELECT'
      FINISH( 7 ) = 565
 
      BEGIN ( 8 ) = 566
      TEXT( 566 ) = 'Normally, when you select data from an e'
     .//            'vents kernels, you place'
      TEXT( 567 ) = 'conditions upon the various columns that'
     .//            ' will be displayed.  This'
      TEXT( 568 ) = 'is done via conditional operators (also '
     .//            'called relational operators).'
      TEXT( 569 ) = ' '
      TEXT( 570 ) = 'The following conditionals are supported'
     .//            ' by the E-kernel system.'
      TEXT( 571 ) = ' '
      TEXT( 572 ) = '@literal'
      TEXT( 573 ) = 'Operator       Usage in a condition'
      TEXT( 574 ) = '--------       -------------------------'
     .//            '--'
      TEXT( 575 ) = 'LT             column LT   expression'
      TEXT( 576 ) = 'LE             column LE   expression'
 
      RETURN
      END
