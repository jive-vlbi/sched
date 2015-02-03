C$Procedure      ZZHLP014 ( private help text )
 
      SUBROUTINE ZZHLP014 ( BEGIN, FINISH, TEXT )
 
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
      TEXT( 1308 ) = '[NOT] column <relation> value'
      TEXT( 1309 ) = '|endliteral'
      TEXT( 1310 ) = 'If'
      TEXT( 1311 ) = 'the column is a character or time colum'
     .//             'n, the value must be enclosed in either'
     .//             ' s'
      TEXT( 1312 ) = 'Allowed relations are EQ NE LT LE GT GE'
     .//             ' and LIKE (used for pattern'
      TEXT( 1313 ) = 'matching).'
      TEXT( 1314 ) = ' '
      TEXT( 1315 ) = '@@Looking at Data    --- SELECT'
      TEXT( 1316 ) = 'Quit Help'
      TEXT( 1317 ) = 'Help'
      TEXT( 1318 ) = 'Column and Table Abbreviations'
      TEXT( 1319 ) = 'Select Clause'
      TEXT( 1320 ) = 'From Clause'
      TEXT( 1321 ) = 'Where Clause'
      TEXT( 1322 ) = 'Order By'
      TEXT( 1323 ) = 'Combining Tables'
      TEXT( 1324 ) = 'Reports'
      TEXT( 1325 ) = 'Getting Too Much Data'
      TEXT( 1326 ) = ' '
      FINISH( 31 ) = 1326
 
      BEGIN ( 32 ) = 1327
      TEXT( 1327 ) = 'When you select a help topic for which '
     .//             'there is some text that'
      TEXT( 1328 ) = 'should be displayed, the help system be'
     .//             'gins sending this text'
      TEXT( 1329 ) = 'to your display.  If there is a lot of '
     .//             'text, some of it may'
      TEXT( 1330 ) = 'scroll by before you have a chance to r'
     .//             'ead it.  There'
      TEXT( 1331 ) = 'are two ways to deal with this.'
      TEXT( 1332 ) = '@newlist'
      TEXT( 1333 ) = '@numitem You can hit CTRL-S on your key'
     .//             'pad to cause output'
      TEXT( 1334 ) = 'to your display to be temporarily disab'
     .//             'led. Hit CTRL-Q to restart'
      TEXT( 1335 ) = 'the output.  This works on most termina'
     .//             'ls and terminal emulators.'
      TEXT( 1336 ) = ' '
      TEXT( 1337 ) = '@numitem You can tell Inspekt to wait o'
     .//             'nce it finishes displaying'
      TEXT( 1338 ) = 'a page full of text.'
      TEXT( 1339 ) = ' '
      TEXT( 1340 ) = 'To do this at the Inspekt prompt type:'
      TEXT( 1341 ) = '@literal'
      TEXT( 1342 ) = 'Inspekt> SET HELP WAIT;'
      TEXT( 1343 ) = '|endliteral'
      TEXT( 1344 ) = 'The Inspekt help system will now pause '
     .//             'after each page of'
      TEXT( 1345 ) = 'text it displays and wait for you to hi'
     .//             't a carriage return before'
      TEXT( 1346 ) = 'it displays the next page or related to'
     .//             'pics menu.'
      TEXT( 1347 ) = ' '
      TEXT( 1348 ) = 'To return to the original help system b'
     .//             'ehaviour, type the command'
      TEXT( 1349 ) = '@literal'
      TEXT( 1350 ) = 'Inspekt> SET HELP NO WAIT;'
      TEXT( 1351 ) = '|endliteral'
      TEXT( 1352 ) = ' '
      TEXT( 1353 ) = 'Note that the size of a page is also un'
     .//             'der your control.  You may'
      TEXT( 1354 ) = 'set the number of lines that will fit o'
     .//             'n a page by using the'
      TEXT( 1355 ) = 'command "SET PAGE HEIGHT".  This comman'
     .//             'd is described in the'
      TEXT( 1356 ) = '"SET PAGE ..." help topic.'
      TEXT( 1357 ) = ' '
      TEXT( 1358 ) = '@@Making Help Wait'
      TEXT( 1359 ) = 'Quit Help'
      TEXT( 1360 ) = 'Help'
      TEXT( 1361 ) = 'SET PAGE   ...'
      FINISH( 32 ) = 1361
 
      BEGIN ( 33 ) = 1362
      TEXT( 1362 ) = 'A numeric format is specified by creati'
     .//             'ng a picture of the'
      TEXT( 1363 ) = 'format.  For example to specify that a '
     .//             'number should start'
      TEXT( 1364 ) = 'with 3 digits and be displayed to 3 dec'
     .//             'imal places use'
      TEXT( 1365 ) = 'a picture such as this:'
      TEXT( 1366 ) = '@literal'
      TEXT( 1367 ) = '###.###'
      TEXT( 1368 ) = '|endliteral'
      TEXT( 1369 ) = ' '
      TEXT( 1370 ) = 'If the first character of the picture i'
     .//             's a minus sign,'
      TEXT( 1371 ) = 'the first character in the output strin'
     .//             'g will be'
      TEXT( 1372 ) = 'a blank if the number is non-negative, '
     .//             'a minus sign'
      TEXT( 1373 ) = 'if the number is negative.'
      TEXT( 1374 ) = ' '
      TEXT( 1375 ) = 'If the first character of the picture i'
     .//             's a plus sign,'
      TEXT( 1376 ) = 'the first character of the output strin'
     .//             'g will be a'
      TEXT( 1377 ) = 'plus if the number is positive, a blank'
     .//             ' if the number'
      TEXT( 1378 ) = 'is zero, and a minus sign if the number'
     .//             ' is negative.'
      TEXT( 1379 ) = ' '
      TEXT( 1380 ) = 'If the first character of the string is'
     .//             ' NOT a sign'
      TEXT( 1381 ) = '(plus or minus) the first character of '
     .//             'the output'
      TEXT( 1382 ) = 'string will be a minus sign if the numb'
     .//             'er is negative'
      TEXT( 1383 ) = 'and will be the first character of the '
     .//             'integer part'
      TEXT( 1384 ) = 'of the number otherwise.'
      TEXT( 1385 ) = ' '
      TEXT( 1386 ) = 'The integer portion of STRING will cont'
     .//             'ain at least'
      TEXT( 1387 ) = 'as many characters as appear before the'
     .//             ' decimal point'
      TEXT( 1388 ) = '(or last character if there is no decim'
     .//             'al point) but'
      TEXT( 1389 ) = 'after a leading + or -. There will ALWA'
     .//             'YS be at least'
      TEXT( 1390 ) = 'one digit output in integer portion of '
     .//             'STRING.'
      TEXT( 1391 ) = ' '
      TEXT( 1392 ) = 'If the picture begins with a any of the'
     .//             ' following'
      TEXT( 1393 ) = '@literal'
      TEXT( 1394 ) = '   ''+0'', ''-0'', or ''0'''
      TEXT( 1395 ) = '|endliteral'
      TEXT( 1396 ) = 'it is said to have a leading zero.  If '
     .//             'a picture has'
      TEXT( 1397 ) = 'a leading zero and the integer portion '
     .//             'is not large'
      TEXT( 1398 ) = 'enough to fill up the integer space spe'
     .//             'cified by'
      TEXT( 1399 ) = 'the picture, the output will be zero pa'
     .//             'dded from the sign (if'
      TEXT( 1400 ) = 'one is required) up to the first charac'
     .//             'ter of the'
      TEXT( 1401 ) = 'integer part of the number.'
 
      RETURN
      END
