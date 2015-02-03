C$Procedure      ZZHLP013 ( private help text )
 
      SUBROUTINE ZZHLP013 ( BEGIN, FINISH, TEXT )
 
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
      TEXT( 1214 ) = ' '
      TEXT( 1215 ) = 'E-kernels can be "unloaded." When an E-'
     .//             'kernel is unloaded,'
      TEXT( 1216 ) = 'Inspekt "forgets" about the existence o'
     .//             'f the kernel. Data in the kernel'
      TEXT( 1217 ) = 'can not be retrieved or manipulated wit'
     .//             'hout first'
      TEXT( 1218 ) = 're-loading it.'
      TEXT( 1219 ) = ' '
      TEXT( 1220 ) = 'To load or unload an E-kernel, type'
      TEXT( 1221 ) = '@literal'
      TEXT( 1222 ) = 'LOAD EK (filename of E-kernel)'
      TEXT( 1223 ) = 'UNLOAD  (filename of previously loaded '
     .//             'E-kernel).'
      TEXT( 1224 ) = '|endliteral'
      TEXT( 1225 ) = 'The filename used in the UNLOAD command'
     .//             ' must be the same as the filename used '
     .//             'to'
      TEXT( 1226 ) = ' '
      TEXT( 1227 ) = 'Two other files may be loaded: a leapse'
     .//             'conds kernel and an SCLK kernel. These '
     .//             'ar'
      TEXT( 1228 ) = '@literal'
      TEXT( 1229 ) = 'LOAD LEAPSECONDS (filename of leapsecon'
     .//             'ds kernel)'
      TEXT( 1230 ) = 'LOAD SCLK KERNEL (filename of SCLK kern'
     .//             'el)'
      TEXT( 1231 ) = '|endliteral'
      TEXT( 1232 ) = 'You can avoid having to load leapsecond'
     .//             's or SCLK kernels by'
      TEXT( 1233 ) = 'setting up the environment variables SC'
     .//             'LK and LEAPSECONDS to'
      TEXT( 1234 ) = 'point to the corresponding kernel prior'
     .//             ' to starting Inspekt.'
      TEXT( 1235 ) = ' '
      TEXT( 1236 ) = 'Leapsecond and SCLK kernels cannot be u'
     .//             'nloaded.'
      TEXT( 1237 ) = 'However, you can load a different leaps'
     .//             'econds or SCLK kernel.'
      TEXT( 1238 ) = 'When a new SLCK or leapseconds kernel i'
     .//             's loaded Inspekt behaves'
      TEXT( 1239 ) = 'as if you had never loaded the previous'
     .//             ' SCLK'
      TEXT( 1240 ) = 'or leapseconds kernel. Only the data in'
     .//             ' the freshly loaded kernel'
      TEXT( 1241 ) = 'will be used by Inspekt.'
      TEXT( 1242 ) = ' '
      TEXT( 1243 ) = 'There are two commands for determining '
     .//             'what kernels have been loaded.'
      TEXT( 1244 ) = '@literal'
      TEXT( 1245 ) = 'SHOW KERNELS;'
      TEXT( 1246 ) = '|endliteral'
      TEXT( 1247 ) = 'displays which E-kernels have been load'
     .//             'ed'
      TEXT( 1248 ) = ' '
      TEXT( 1249 ) = '@literal'
      TEXT( 1250 ) = 'SHOW ENVIRONMENT'
      TEXT( 1251 ) = '|endliteral'
      TEXT( 1252 ) = 'displays which leapseconds and SCLK ker'
     .//             'nels (if any) have been'
      TEXT( 1253 ) = 'loaded along with other information abo'
     .//             'ut the current Inspekt'
      TEXT( 1254 ) = 'settings.'
      TEXT( 1255 ) = '@@Kernels            --- LOAD'
      TEXT( 1256 ) = 'Quit Help'
      TEXT( 1257 ) = 'Help'
      TEXT( 1258 ) = 'Environment Variables'
      FINISH( 29 ) = 1258
 
      BEGIN ( 30 ) = 1259
      TEXT( 1259 ) = 'Listed below are the limits for various'
     .//             ' aspects of Inspekt.'
      TEXT( 1260 ) = '@setparamsize{Total number of Columns}'
      TEXT( 1261 ) = ' '
      TEXT( 1262 ) = '@param  Loaded E-kernels.'
      TEXT( 1263 ) = 'a maximum of 20 E-kernels may be loaded'
     .//             ' at once.'
      TEXT( 1264 ) = ' '
      TEXT( 1265 ) = '@param Total number of columns.'
      TEXT( 1266 ) = 'a maximum of 500 columns may'
      TEXT( 1267 ) = 'be present in all of the loaded kernels'
      TEXT( 1268 ) = ' '
      TEXT( 1269 ) = '@param Page Width.'
      TEXT( 1270 ) = 'the page must be at least 40 characters'
     .//             ' wide and'
      TEXT( 1271 ) = 'no  more than 132 characters wide.'
      TEXT( 1272 ) = ' '
      TEXT( 1273 ) = '@param  Column width.'
      TEXT( 1274 ) = 'a column must be at least 8 characters '
     .//             'wide and no'
      TEXT( 1275 ) = 'more than 80 characters wide.'
      TEXT( 1276 ) = ' '
      TEXT( 1277 ) = '@param  Command Size.'
      TEXT( 1278 ) = 'A command can contain no more than 1760'
     .//             ' character'
      TEXT( 1279 ) = '(it should fit on one 24 by 80 characte'
     .//             'r screen).'
      TEXT( 1280 ) = ' '
      TEXT( 1281 ) = '@param Events per Report.'
      TEXT( 1282 ) = 'A default limit of 100 rows will be pre'
     .//             'sented in'
      TEXT( 1283 ) = 'any report.  You can override this limi'
     .//             't with the'
      TEXT( 1284 ) = 'SET DELUGE command or at the time a rep'
     .//             'ort is'
      TEXT( 1285 ) = 'ready for output.'
      TEXT( 1286 ) = ' '
      TEXT( 1287 ) = '@@Limits'
      TEXT( 1288 ) = 'Quit Help'
      TEXT( 1289 ) = 'Help'
      FINISH( 30 ) = 1289
 
      BEGIN ( 31 ) = 1290
      TEXT( 1290 ) = 'To view events in a report, you need to'
     .//             ' issue a "SELECT" command. The form of'
      TEXT( 1291 ) = 'this command is shown below ("WHERE" an'
     .//             'd "ORDER BY" clauses are optional).'
      TEXT( 1292 ) = '@literal'
      TEXT( 1293 ) = 'SELECT a comma delimited'
      TEXT( 1294 ) = '       list of unambiguous'
      TEXT( 1295 ) = '       column names'
      TEXT( 1296 ) = 'FROM   a comma delimited'
      TEXT( 1297 ) = '       list of tables-alias pairs'
      TEXT( 1298 ) = 'WHERE    condition_1'
      TEXT( 1299 ) = '   AND/OR condition_2'
      TEXT( 1300 ) = '   ...'
      TEXT( 1301 ) = '   AND/OR condition_n'
      TEXT( 1302 ) = 'ORDER BY a comma delimited'
      TEXT( 1303 ) = '         list of unambiguous'
      TEXT( 1304 ) = '         column names'
      TEXT( 1305 ) = '|endliteral'
      TEXT( 1306 ) = 'All but one of the conditions in the "W'
     .//             'HERE" clause have the form:'
      TEXT( 1307 ) = '@literal'
 
      RETURN
      END
