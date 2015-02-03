C$Procedure      ZZHLP003 ( private help text )
 
      SUBROUTINE ZZHLP003 ( BEGIN, FINISH, TEXT )
 
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
      TEXT( 280 ) = '|endliteral'
      TEXT( 281 ) = 'The tables we want to join are specified'
     .//            ' in the FROM clause'
      TEXT( 282 ) = 'of the select command.  They must be sep'
     .//            'arated by commas.'
      TEXT( 283 ) = ' '
      TEXT( 284 ) = 'Notice that every row of the first table'
     .//            ', MUSIC, has every'
      TEXT( 285 ) = 'row of the second table, PEOPLE appended'
     .//            ' to it to produce'
      TEXT( 286 ) = 'a new row.   This table becomes useful w'
     .//            'hen we apply constraints'
      TEXT( 287 ) = 'to it.  In our case we want to find out '
     .//            'which songs were written'
      TEXT( 288 ) = 'by Irish song writers.'
      TEXT( 289 ) = ' '
      TEXT( 290 ) = 'The country of origin of the composer'
      TEXT( 291 ) = 'is present in a row of the join table'
      TEXT( 292 ) = 'if the NAME and COMPOSER columns have th'
     .//            'e'
      TEXT( 293 ) = 'same value.  The SELECT command'
      TEXT( 294 ) = ' '
      TEXT( 295 ) = '@literal'
      TEXT( 296 ) = 'select song, composer, name, country'
      TEXT( 297 ) = 'from music, people'
      TEXT( 298 ) = 'WHERE COMPOSER = NAME'
      TEXT( 299 ) = '|endliteral'
      TEXT( 300 ) = ' '
      TEXT( 301 ) = 'gives us the following rows.'
      TEXT( 302 ) = '@literal'
      TEXT( 303 ) = 'SONG              COMPOSER   NAME       '
     .//            'COUNTRY'
      TEXT( 304 ) = '----------------------------------------'
     .//            '-------'
      TEXT( 305 ) = 'YESTERDAY         MCCARTNEY  MCCARTNEY  '
     .//            'ENGLAND'
      TEXT( 306 ) = 'LIVE AND LET DIE  MCCARTNEY  MCCARTNEY  '
     .//            'ENGLAND'
      TEXT( 307 ) = 'BLOODY SUNDAY     HEUSSEN    HEUSSEN    '
     .//            'IRELAND'
      TEXT( 308 ) = 'ONE TREE HILL     HEUSSEN    HEUSSEN    '
     .//            'IRELAND'
      TEXT( 309 ) = 'SATISFACTION      JAGGER     JAGGER     '
     .//            'ENGLAND'
      TEXT( 310 ) = 'BOYS OF SUMMER    HENLEY     HENLEY     '
     .//            'USA'
      TEXT( 311 ) = '|endliteral'
      TEXT( 312 ) = ' '
      TEXT( 313 ) = 'This report is a lot closer to what we w'
     .//            'ant.'
      TEXT( 314 ) = ' '
      TEXT( 315 ) = 'To see just the titles of songs written '
     .//            'by Irish'
      TEXT( 316 ) = 'song writers we add modify the SELECT co'
     .//            'mmand as shown'
      TEXT( 317 ) = 'below.'
      TEXT( 318 ) = '@literal'
      TEXT( 319 ) = 'select SONG'
      TEXT( 320 ) = 'from music, people'
      TEXT( 321 ) = 'where composer = name'
      TEXT( 322 ) = 'AND   COUNTRY  = ''IRELAND'''
      TEXT( 323 ) = '|endliteral'
      TEXT( 324 ) = 'This results in the following report.'
      TEXT( 325 ) = '@literal'
      TEXT( 326 ) = 'SONG'
      TEXT( 327 ) = '--------------'
      TEXT( 328 ) = 'BLOODY SUNDAY'
      TEXT( 329 ) = 'ONE TREE HILL'
      TEXT( 330 ) = '|endliteral'
      TEXT( 331 ) = ' '
      TEXT( 332 ) = '@subsection Equi-joins'
      TEXT( 333 ) = ' '
      TEXT( 334 ) = 'In the previous example we had the condi'
     .//            'tion'
      TEXT( 335 ) = ' '
      TEXT( 336 ) = '@literal'
      TEXT( 337 ) = 'WHERE COMPOSER = NAME'
      TEXT( 338 ) = '|endliteral'
      TEXT( 339 ) = ' '
      TEXT( 340 ) = 'This is a common condition when selectin'
     .//            'g data from a join'
      TEXT( 341 ) = 'of two tables.  The general situation lo'
     .//            'oks something like'
      TEXT( 342 ) = 'this.'
      TEXT( 343 ) = '@literal'
      TEXT( 344 ) = 'SELECT ...'
      TEXT( 345 ) = 'FROM   table_1, table_2'
      TEXT( 346 ) = 'WHERE  column_from_table_1 = column_from'
     .//            '_table_2'
      TEXT( 347 ) = '|endliteral'
      TEXT( 348 ) = 'When you join two or more tables and add'
     .//            ' an equality a condition'
      TEXT( 349 ) = 'to the rows from different tables, the j'
     .//            'oin is'
      TEXT( 350 ) = 'called an "equi-join".  You will'
      TEXT( 351 ) = 'almost always create equi-joins when joi'
     .//            'ning tables.'
      TEXT( 352 ) = ' '
      TEXT( 353 ) = '@subsection Ambiguous Columns'
      TEXT( 354 ) = ' '
      TEXT( 355 ) = 'Suppose in the following example the tab'
     .//            'le PEOPLE had the columns'
      TEXT( 356 ) = 'COMPOSER and COUNTRY instead of NAME and'
     .//            ' COUNTRY. The simple view'
      TEXT( 357 ) = 'of the join now becomes:'
      TEXT( 358 ) = '@literal'
      TEXT( 359 ) = 'SONG              COMPOSER   COMPOSER   '
     .//            'COUNTRY'
      TEXT( 360 ) = '----------------------------------------'
     .//            '-------'
      TEXT( 361 ) = 'YESTERDAY         MCCARTNEY  MCCARTNEY  '
     .//            'ENGLAND'
      TEXT( 362 ) = 'YESTERDAY         MCCARTNEY  HEUSSEN    '
     .//            'IRELAND'
      TEXT( 363 ) = 'YESTERDAY         MCCARTNEY  JAGGER     '
     .//            'ENGLAND'
      TEXT( 364 ) = 'YESTERDAY         MCCARTNEY  HENLEY     '
     .//            'USA'
      TEXT( 365 ) = 'LIVE AND LET DIE  MCCARTNEY  MCCARTNEY  '
     .//            'ENGLAND'
      TEXT( 366 ) = '        .            .           .      '
     .//            '  .'
      TEXT( 367 ) = '        .            .           .      '
     .//            '  .'
      TEXT( 368 ) = '        .            .           .      '
     .//            '  .'
      TEXT( 369 ) = '|endliteral'
      TEXT( 370 ) = 'If I want to talk about the column first'
     .//            ' column COMPOSER'
      TEXT( 371 ) = 'how do I distinguish it from the second '
     .//            'column COMPOSER?'
      TEXT( 372 ) = 'The first column comes from the table MU'
     .//            'SIC  the second'
      TEXT( 373 ) = 'comes from the table PEOPLE.  To unambig'
     .//            'uously specify'
      TEXT( 374 ) = 'either prefix the column name by the nam'
     .//            'e of its parent'
      TEXT( 375 ) = 'table as in  MUSIC.COMPOSER  or PEOPLE.C'
     .//            'OMPOSER.  The'
      TEXT( 376 ) = 'select command would then be issued as:'
      TEXT( 377 ) = '@literal'
      TEXT( 378 ) = 'select song'
      TEXT( 379 ) = 'from music, people'
 
      RETURN
      END
