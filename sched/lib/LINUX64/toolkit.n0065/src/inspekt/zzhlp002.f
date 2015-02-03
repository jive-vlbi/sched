C$Procedure      ZZHLP002 ( private help text )
 
      SUBROUTINE ZZHLP002 ( BEGIN, FINISH, TEXT )
 
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
      TEXT( 183 ) = 'To see a list of indexed columns type:'
      TEXT( 184 ) = '@literal'
      TEXT( 185 ) = 'SHOW INDEXED'
      TEXT( 186 ) = '|endliteral'
      TEXT( 187 ) = '@@Columns'
      TEXT( 188 ) = 'Quit Help'
      TEXT( 189 ) = 'Help'
      TEXT( 190 ) = 'SET COLUMN ...'
      TEXT( 191 ) = 'Reports'
      TEXT( 192 ) = 'Time Formats'
      TEXT( 193 ) = 'Autoadjust'
      FINISH( 6 ) = 193
 
      BEGIN ( 7 ) = 194
      TEXT( 194 ) = 'Data in one row of a table can be connec'
     .//            'ted with data in'
      TEXT( 195 ) = 'a row of a second (possibly the same) ta'
     .//            'ble by a process'
      TEXT( 196 ) = 'called "joining" the tables.'
      TEXT( 197 ) = ' '
      TEXT( 198 ) = 'The "join" of two tables is the cartesia'
     .//            'n product of the'
      TEXT( 199 ) = 'two tables.  For example suppose that th'
     .//            'e tables MUSIC and PEOPLE have'
      TEXT( 200 ) = 'been loaded into Inspekt.  Table MUSIC'
      TEXT( 201 ) = 'has two columns SONG and COMPOSER, Table'
     .//            ' PEOPLE has columns'
      TEXT( 202 ) = 'NAME and COUNTRY.  Below is a sample of '
     .//            'the data in these'
      TEXT( 203 ) = 'two tables.'
      TEXT( 204 ) = ' '
      TEXT( 205 ) = '@literal'
      TEXT( 206 ) = 'Table MUSIC'
      TEXT( 207 ) = ' '
      TEXT( 208 ) = 'SONG              COMPOSER'
      TEXT( 209 ) = '--------------------------'
      TEXT( 210 ) = 'YESTERDAY         MCCARTNEY'
      TEXT( 211 ) = 'LIVE AND LET DIE  MCCARTNEY'
      TEXT( 212 ) = 'BLOODY SUNDAY     HEUSSEN'
      TEXT( 213 ) = 'ONE TREE HILL     HEUSSEN'
      TEXT( 214 ) = 'SATISFACTION      JAGGER'
      TEXT( 215 ) = 'BOYS OF SUMMER    HENLEY'
      TEXT( 216 ) = ' '
      TEXT( 217 ) = ' '
      TEXT( 218 ) = 'Table PEOPLE'
      TEXT( 219 ) = ' '
      TEXT( 220 ) = 'NAME             COUNTRY'
      TEXT( 221 ) = '--------------------------'
      TEXT( 222 ) = 'MCCARTNEY        ENGLAND'
      TEXT( 223 ) = 'HEUSSEN          IRELAND'
      TEXT( 224 ) = 'JAGGER           ENGLAND'
      TEXT( 225 ) = 'HENLEY           USA'
      TEXT( 226 ) = '|endliteral'
      TEXT( 227 ) = 'The two tables have sufficient informati'
     .//            'on to tell us'
      TEXT( 228 ) = 'the names of all the songs written by Ir'
     .//            'ish composers.'
      TEXT( 229 ) = 'But, how do we get Inspekt to tell us th'
     .//            'is information.'
      TEXT( 230 ) = ' '
      TEXT( 231 ) = 'We "join" the two tables so that everyth'
     .//            'ing is in one'
      TEXT( 232 ) = 'large table.  Below is the join of the t'
     .//            'ables MUSIC and'
      TEXT( 233 ) = 'PEOPLE. (We''ve included blank lines to '
     .//            'help illustrate'
      TEXT( 234 ) = 'how rows are combined to make the join t'
     .//            'able. Also for'
      TEXT( 235 ) = 'purpose of illustration only, we''ve pre'
     .//            'sented the data'
      TEXT( 236 ) = 'from the second table in lower case.  In'
     .//            ' the actual join'
      TEXT( 237 ) = 'the case of data does not change.)'
      TEXT( 238 ) = '@literal'
      TEXT( 239 ) = 'Join of tables MUSIC, PEOPLE'
      TEXT( 240 ) = ' '
      TEXT( 241 ) = 'SONG              COMPOSER   NAME       '
     .//            'COUNTRY'
      TEXT( 242 ) = '----------------------------------------'
     .//            '-------'
      TEXT( 243 ) = 'YESTERDAY         MCCARTNEY  mccartney  '
     .//            'england'
      TEXT( 244 ) = 'YESTERDAY         MCCARTNEY  heussen    '
     .//            'ireland'
      TEXT( 245 ) = 'YESTERDAY         MCCARTNEY  jagger     '
     .//            'england'
      TEXT( 246 ) = 'YESTERDAY         MCCARTNEY  henley     '
     .//            'usa'
      TEXT( 247 ) = ' '
      TEXT( 248 ) = 'LIVE AND LET DIE  MCCARTNEY  mccartney  '
     .//            'england'
      TEXT( 249 ) = 'LIVE AND LET DIE  MCCARTNEY  heussen    '
     .//            'ireland'
      TEXT( 250 ) = 'LIVE AND LET DIE  MCCARTNEY  jagger     '
     .//            'england'
      TEXT( 251 ) = 'LIVE AND LET DIE  MCCARTNEY  henley     '
     .//            'usa'
      TEXT( 252 ) = ' '
      TEXT( 253 ) = 'BLOODY SUNDAY     HEUSSEN    mccartney  '
     .//            'england'
      TEXT( 254 ) = 'BLOODY SUNDAY     HEUSSEN    heussen    '
     .//            'ireland'
      TEXT( 255 ) = 'BLOODY SUNDAY     HEUSSEN    jagger     '
     .//            'england'
      TEXT( 256 ) = 'BLOODY SUNDAY     HEUSSEN    henley     '
     .//            'usa'
      TEXT( 257 ) = ' '
      TEXT( 258 ) = 'ONE TREE HILL     HEUSSEN    mccartney  '
     .//            'england'
      TEXT( 259 ) = 'ONE TREE HILL     HEUSSEN    heussen    '
     .//            'ireland'
      TEXT( 260 ) = 'ONE TREE HILL     HEUSSEN    jagger     '
     .//            'england'
      TEXT( 261 ) = 'ONE TREE HILL     HEUSSEN    HENLEY     '
     .//            'USA'
      TEXT( 262 ) = ' '
      TEXT( 263 ) = 'SATISFACTION      JAGGER     mccartney  '
     .//            'england'
      TEXT( 264 ) = 'SATISFACTION      JAGGER     heussen    '
     .//            'ireland'
      TEXT( 265 ) = 'SATISFACTION      JAGGER     jagger     '
     .//            'england'
      TEXT( 266 ) = 'SATISFACTION      JAGGER     henley     '
     .//            'usa'
      TEXT( 267 ) = ' '
      TEXT( 268 ) = 'BOYS OF SUMMER    HENLEY     mccartney  '
     .//            'england'
      TEXT( 269 ) = 'BOYS OF SUMMER    HENLEY     heussen    '
     .//            'ireland'
      TEXT( 270 ) = 'BOYS OF SUMMER    HENLEY     jagger     '
     .//            'england'
      TEXT( 271 ) = 'BOYS OF SUMMER    HENLEY     henley     '
     .//            'usa'
      TEXT( 272 ) = '|endliteral'
      TEXT( 273 ) = ' '
      TEXT( 274 ) = 'To select something from this join of tw'
     .//            'o tables you'
      TEXT( 275 ) = 'construct your SELECT command as shown h'
     .//            'ere:'
      TEXT( 276 ) = '@literal'
      TEXT( 277 ) = 'select item, ... , item'
      TEXT( 278 ) = 'FROM   MUSIC, PEOPLE'
      TEXT( 279 ) = 'where ...'
 
      RETURN
      END
