C$Procedure      ZZHLP004 ( private help text )
 
      SUBROUTINE ZZHLP004 ( BEGIN, FINISH, TEXT )
 
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
      TEXT( 380 ) = 'where MUSIC.COMPOSER = PEOPLE.COMPOSER'
      TEXT( 381 ) = 'and   country  = ''IRELAND'''
      TEXT( 382 ) = '|endliteral'
      TEXT( 383 ) = ' '
      TEXT( 384 ) = '@subsection Joining a Table to Itself'
      TEXT( 385 ) = ' '
      TEXT( 386 ) = 'Sometimes it is useful to be able to joi'
     .//            'n a table to itself.'
      TEXT( 387 ) = 'This is particularly true if the table c'
     .//            'ontains some kind'
      TEXT( 388 ) = 'of hierarchical information such as in c'
     .//            'hild-parent pairs.'
      TEXT( 389 ) = 'For example here is a list of family mem'
     .//            'bers and their'
      TEXT( 390 ) = 'parents'
      TEXT( 391 ) = '@literal'
      TEXT( 392 ) = 'Table RELATIONS'
      TEXT( 393 ) = 'CHILD    FATHER   MOTHER'
      TEXT( 394 ) = '------------------------'
      TEXT( 395 ) = 'CINDY    GEORGE   ALICE'
      TEXT( 396 ) = 'SALLY    GEORGE   MARTHA'
      TEXT( 397 ) = 'GEORGE   WILLIAM  MARGARET'
      TEXT( 398 ) = 'ALICE    JOHN     KATHERINE'
      TEXT( 399 ) = 'MARTHA   ROBERT   CONSTANCE'
      TEXT( 400 ) = '|endliteral'
      TEXT( 401 ) = ' '
      TEXT( 402 ) = 'The grandparents in this table are paren'
     .//            'ts of parents.  We can'
      TEXT( 403 ) = 'join the table to itself to get a list o'
     .//            'f all the grandfathers.'
      TEXT( 404 ) = 'The join is simple enough.  Here it is.'
      TEXT( 405 ) = ' '
      TEXT( 406 ) = '@literal'
      TEXT( 407 ) = 'Join of RELATIONS, RELATIONS'
      TEXT( 408 ) = ' '
      TEXT( 409 ) = 'CHILD    FATHER   MOTHER     CHILD   FAT'
     .//            'HER    MOTHER'
      TEXT( 410 ) = '----------------------------------------'
     .//            '-----------------'
      TEXT( 411 ) = 'CINDY    GEORGE   ALICE      CINDY   GEO'
     .//            'RGE    ALICE'
      TEXT( 412 ) = 'CINDY    GEORGE   ALICE      SALLY   GEO'
     .//            'RGE    MARTHA'
      TEXT( 413 ) = 'CINDY    GEORGE   ALICE      GEORGE  WIL'
     .//            'LIAM   MARGARET'
      TEXT( 414 ) = 'CINDY    GEORGE   ALICE      ALICE   JOH'
     .//            'N      KATHERINE'
      TEXT( 415 ) = 'CINDY    GEORGE   ALICE      MARTHA  ROB'
     .//            'ERT    CONSTANCE'
      TEXT( 416 ) = 'SALLY    GEORGE   MARTHA     CINDY   GEO'
     .//            'RGE    ALICE'
      TEXT( 417 ) = 'SALLY    GEORGE   MARTHA     SALLY   GEO'
     .//            'RGE    MARTHA'
      TEXT( 418 ) = 'SALLY    GEORGE   MARTHA     GEORGE  WIL'
     .//            'LIAM   MARGARET'
      TEXT( 419 ) = 'SALLY    GEORGE   MARTHA     ALICE   JOH'
     .//            'N      KATHERINE'
      TEXT( 420 ) = 'SALLY    GEORGE   MARTHA     MARTHA  ROB'
     .//            'ERT    CONSTANCE'
      TEXT( 421 ) = 'GEORGE   WILLIAM  MARGARET   CINDY   GEO'
     .//            'RGE    ALICE'
      TEXT( 422 ) = 'GEORGE   WILLIAM  MARGARET   SALLY   GEO'
     .//            'RGE    MARTHA'
      TEXT( 423 ) = 'GEORGE   WILLIAM  MARGARET   GEORGE  WIL'
     .//            'LIAM   MARGARET'
      TEXT( 424 ) = 'GEORGE   WILLIAM  MARGARET   ALICE   JOH'
     .//            'N      KATHERINE'
      TEXT( 425 ) = 'GEORGE   WILLIAM  MARGARET   MARTHA  ROB'
     .//            'ERT    CONSTANCE'
      TEXT( 426 ) = 'ALICE    JOHN     KATHERINE  CINDY   GEO'
     .//            'RGE    ALICE'
      TEXT( 427 ) = 'ALICE    JOHN     KATHERINE  SALLY   GEO'
     .//            'RGE    MARTHA'
      TEXT( 428 ) = 'ALICE    JOHN     KATHERINE  GEORGE  WIL'
     .//            'LIAM   MARGARET'
      TEXT( 429 ) = 'ALICE    JOHN     KATHERINE  ALICE   JOH'
     .//            'N      KATHERINE'
      TEXT( 430 ) = 'ALICE    JOHN     KATHERINE  MARTHA  ROB'
     .//            'ERT    CONSTANCE'
      TEXT( 431 ) = 'MARTHA   ROBERT   CONSTANCE  CINDY   GEO'
     .//            'RGE    ALICE'
      TEXT( 432 ) = 'MARTHA   ROBERT   CONSTANCE  SALLY   GEO'
     .//            'RGE    MARTHA'
      TEXT( 433 ) = 'MARTHA   ROBERT   CONSTANCE  GEORGE  WIL'
     .//            'LIAM   MARGARET'
      TEXT( 434 ) = 'MARTHA   ROBERT   CONSTANCE  ALICE   JOH'
     .//            'N      KATHERINE'
      TEXT( 435 ) = 'MARTHA   ROBERT   CONSTANCE  MARTHA  ROB'
     .//            'ERT    CONSTANCE'
      TEXT( 436 ) = '|endliteral'
      TEXT( 437 ) = ' '
      TEXT( 438 ) = 'But this table has a clear problem.  If '
     .//            'I want to select a'
      TEXT( 439 ) = 'CHILD  from this table how do I specify '
     .//            'which'
      TEXT( 440 ) = 'CHILD column I''m talking about? It clea'
     .//            'rly is not enough'
      TEXT( 441 ) = 'to simply type RELATIONS.CHILD  because '
     .//            'both CHILD columns'
      TEXT( 442 ) = 'come from the same table.'
      TEXT( 443 ) = 'We handle the problem of ambiguous colum'
     .//            'ns names by creating'
      TEXT( 444 ) = 'a unique alias for the table when you sp'
     .//            'ecify the join.  Thus'
      TEXT( 445 ) = 'if I want to be able to select a column '
     .//            'from this table I specify'
      TEXT( 446 ) = 'the join in the from clause as shown bel'
     .//            'ow.'
      TEXT( 447 ) = ' '
      TEXT( 448 ) = '@literal'
      TEXT( 449 ) = 'select ...'
      TEXT( 450 ) = 'FROM   RELATIONS X, RELATIONS Y'
      TEXT( 451 ) = 'where  ...'
      TEXT( 452 ) = '|endliteral'
      TEXT( 453 ) = 'This join creates the following table.'
      TEXT( 454 ) = '@literal'
      TEXT( 455 ) = 'Join of RELATIONS X, RELATIONS Y'
      TEXT( 456 ) = ' '
      TEXT( 457 ) = 'X.CHILD  X.FATHER X.MOTHER   Y.CHILD Y.F'
     .//            'ATHER  Y.MOTHER'
      TEXT( 458 ) = '----------------------------------------'
     .//            '-----------------'
      TEXT( 459 ) = 'CINDY    GEORGE   ALICE      CINDY   GEO'
     .//            'RGE    ALICE'
      TEXT( 460 ) = 'CINDY    GEORGE   ALICE      SALLY   GEO'
     .//            'RGE    MARTHA'
      TEXT( 461 ) = 'CINDY    GEORGE   ALICE      GEORGE  WIL'
     .//            'LIAM   MARGARET'
      TEXT( 462 ) = 'CINDY    GEORGE   ALICE      ALICE   JOH'
     .//            'N      KATHERINE'
      TEXT( 463 ) = 'CINDY    GEORGE   ALICE      MARTHA  ROB'
     .//            'ERT    CONSTANCE'
      TEXT( 464 ) = 'SALLY    GEORGE   MARTHA     CINDY   GEO'
     .//            'RGE    ALICE'
      TEXT( 465 ) = '  .         .       .          .        '
     .//            '.        .'
      TEXT( 466 ) = '  .         .       .          .        '
     .//            '.        .'
      TEXT( 467 ) = '  .         .       .          .        '
     .//            '.        .'
      TEXT( 468 ) = '|endliteral'
      TEXT( 469 ) = ' '
      TEXT( 470 ) = 'As you can see in the example FROM claus'
     .//            'e above, an alias is'
      TEXT( 471 ) = 'specified by following the name of a tab'
     .//            'le by a second word'
      TEXT( 472 ) = 'and a comma.  The second word is the ali'
     .//            'as for the table.'
      TEXT( 473 ) = 'Thus the alias for the first copy of REL'
     .//            'ATIONS is X,  the'
      TEXT( 474 ) = 'alias for the second copy of RELATIONS i'
     .//            's Y.  There is nothing'
      TEXT( 475 ) = 'special about the aliases X and Y.  We c'
     .//            'ould just as easily'
      TEXT( 476 ) = 'have used EGG and SPAM respectively.  Th'
     .//            'e letters X and Y are'
      TEXT( 477 ) = 'just easier to type. Aliases are case in'
     .//            'sensitive.  The names'
      TEXT( 478 ) = ' X.FATHER and x.FATHER refer to the same'
     .//            ' column.'
      TEXT( 479 ) = ' '
 
      RETURN
      END
