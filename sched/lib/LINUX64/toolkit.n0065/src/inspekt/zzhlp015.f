C$Procedure      ZZHLP015 ( private help text )
 
      SUBROUTINE ZZHLP015 ( BEGIN, FINISH, TEXT )
 
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
      TEXT( 1402 ) = ' '
      TEXT( 1403 ) = 'If picture does NOT have a leading zero'
     .//             ' and'
      TEXT( 1404 ) = 'the integer portion is not large enough'
     .//             ' to fill up'
      TEXT( 1405 ) = 'the space specified by the picture, out'
     .//             'put will be blank'
      TEXT( 1406 ) = 'padded between the sign (if one is requ'
     .//             'ired'
      TEXT( 1407 ) = 'and the first character of the integer '
     .//             'part of the'
      TEXT( 1408 ) = 'number.'
      TEXT( 1409 ) = ' '
      TEXT( 1410 ) = 'If a decimal point ( ''.'' ) is present'
     .//             ' in the picture it'
      TEXT( 1411 ) = 'will be present following the integer p'
     .//             'ortion of'
      TEXT( 1412 ) = 'output. Moreover, the decimal portion o'
     .//             'f the output will'
      TEXT( 1413 ) = 'contain the same number of digits as th'
     .//             'ere are'
      TEXT( 1414 ) = 'non-blank characters following the deci'
     .//             'mal point in'
      TEXT( 1415 ) = 'the picture.  However, if the picture c'
     .//             'ontains more than'
      TEXT( 1416 ) = '14 characters following the decimal poi'
     .//             'nt, the'
      TEXT( 1417 ) = 'characters in output that follow the 14'
     .//             ' position to'
      TEXT( 1418 ) = 'the right of the decimal point will be '
     .//             'zeros.'
      TEXT( 1419 ) = ' '
      TEXT( 1420 ) = '@@Numeric Formats'
      TEXT( 1421 ) = 'Quit Help'
      TEXT( 1422 ) = 'Help'
      FINISH( 33 ) = 1422
 
      BEGIN ( 34 ) = 1423
      TEXT( 1423 ) = 'You may control the order in which even'
     .//             'ts are reported by a SELECT'
      TEXT( 1424 ) = 'command through use of the "ORDER BY" c'
     .//             'lause that is an optional'
      TEXT( 1425 ) = 'part of all SELECT commands. The defaul'
     .//             't ordering is by the TIME'
      TEXT( 1426 ) = 'column that is present in every E-kerne'
     .//             'l. To order your output'
      TEXT( 1427 ) = 'based upon some other column issue the '
     .//             'SELECT command with the'
      TEXT( 1428 ) = 'clause'
      TEXT( 1429 ) = '@literal'
      TEXT( 1430 ) = 'ORDER BY column(s) of choice'
      TEXT( 1431 ) = '|endliteral'
      TEXT( 1432 ) = 'as part of your command.  Column names '
     .//             'should be separated by commas'
      TEXT( 1433 ) = '(,).  Note that the "ORDER BY" clause w'
     .//             'orks faster with indexed'
      TEXT( 1434 ) = 'columns.  To see which columns are inde'
     .//             'xed you can use any of'
      TEXT( 1435 ) = 'the commands:'
      TEXT( 1436 ) = '@literal'
      TEXT( 1437 ) = 'SHOW INDEXES'
      TEXT( 1438 ) = 'SHOW KERNELS'
      TEXT( 1439 ) = 'SHOW SUMMARY'
      TEXT( 1440 ) = '|endliteral'
      TEXT( 1441 ) = ' '
      TEXT( 1442 ) = '@@Order By'
      TEXT( 1443 ) = 'Quit Help'
      TEXT( 1444 ) = 'Help'
      TEXT( 1445 ) = 'Looking at Data    --- SELECT'
      FINISH( 34 ) = 1445
 
      BEGIN ( 35 ) = 1446
      TEXT( 1446 ) = '@@Other Settings'
      TEXT( 1447 ) = 'Quit Help'
      TEXT( 1448 ) = 'Help'
      TEXT( 1449 ) = 'Autoadjust'
      TEXT( 1450 ) = 'Deluge Warning'
      TEXT( 1451 ) = 'Echoing Translated Commands'
      TEXT( 1452 ) = 'Making Help Wait'
      TEXT( 1453 ) = 'Setting the Editor'
      TEXT( 1454 ) = 'Default Floating Format'
      TEXT( 1455 ) = 'Default Integer Format'
      TEXT( 1456 ) = 'Default Time Format'
      TEXT( 1457 ) = ' '
      FINISH( 35 ) = 1457
 
      BEGIN ( 36 ) = 1458
      TEXT( 1458 ) = 'It is often easier to describe a set of'
     .//             ' character by specifying some'
      TEXT( 1459 ) = 'common pattern shared by all of the str'
     .//             'ings of interest.  For this reason'
      TEXT( 1460 ) = 'you can use a LIKE relation in the WHER'
     .//             'E clause of a SELECT command.'
      TEXT( 1461 ) = 'You specify a like condition as shown b'
     .//             'elow:'
      TEXT( 1462 ) = '@literal'
      TEXT( 1463 ) = 'column_name LIKE "pattern"'
      TEXT( 1464 ) = '|endliteral'
      TEXT( 1465 ) = 'where "pattern" is a pattern that the c'
     .//             'olumn must match for the'
      TEXT( 1466 ) = 'LIKE-condition to be satisfied. Note th'
     .//             'at the pattern must be enclosed'
      TEXT( 1467 ) = 'in quotes (").   There are two special '
     .//             'characters that may appear in a'
      TEXT( 1468 ) = 'pattern. The asterisk ''*'' matches any'
     .//             ' substring. The percent mark ''%'''
      TEXT( 1469 ) = 'matches any character.  If a pattern do'
     .//             'es not begin/end with the asterisk,'
      TEXT( 1470 ) = 'the column value must begin/end with th'
     .//             'e the pattern character in order'
      TEXT( 1471 ) = 'to match the pattern.  pattern matching'
     .//             ' is case insensitive.'
      TEXT( 1472 ) = '@literal'
      TEXT( 1473 ) = 'Examples:  SPECIAL          matches S*C'
     .//             '%%L'
      TEXT( 1474 ) = '           SPECIAL does not match   S*C'
     .//             '%%L%'
      TEXT( 1475 ) = '           SPECIAL          matches %PE'
     .//             '%%AL'
      TEXT( 1476 ) = '           SPECIAL          matches S*L'
      TEXT( 1477 ) = '|endliteral'
      TEXT( 1478 ) = '@@Pattern Matching'
      TEXT( 1479 ) = 'Quit Help'
      TEXT( 1480 ) = 'Help'
      FINISH( 36 ) = 1480
 
      BEGIN ( 37 ) = 1481
      TEXT( 1481 ) = 'Patterns are sequences of characters th'
     .//             'at are used to'
      TEXT( 1482 ) = 'select a word or phrase from some list '
     .//             'of words or phrases.'
      TEXT( 1483 ) = 'The pattern begins at the first non-bla'
     .//             'nk character in'
      TEXT( 1484 ) = 'the character sequence.  The pattern en'
     .//             'ds at the last non-blank'
      TEXT( 1485 ) = 'character of the sequence.'
      TEXT( 1486 ) = ' '
      TEXT( 1487 ) = 'There are two special characters that m'
     .//             'ay appear in a pattern'
      TEXT( 1488 ) = 'they are ''*'' the substring "wild card'
     .//             '" and ''%'' the'
      TEXT( 1489 ) = 'character "wild card".'
 
      RETURN
      END
