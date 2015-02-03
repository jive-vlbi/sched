C$Procedure   STCC01 ( STAR catalog type 1, check whether type 1 )
 
      SUBROUTINE STCC01 ( CATFNM, TABNAM, ISTYP1, ERRMSG )
 
C$ Abstract
C
C     Check whether a file is a type 1 star catalog and return the
C     catalog's table name if it is.
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
C     EK
C
C$ Keywords
C
C     None.
C
C$ Declarations
 
      INCLUDE 'ekcnamsz.inc'
      INCLUDE 'ektnamsz.inc'
      INCLUDE 'ekglimit.inc'
 
      CHARACTER*(*)         CATFNM
      CHARACTER*(*)         TABNAM
      LOGICAL               ISTYP1
      CHARACTER*(*)         ERRMSG
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     CATFNM      I   Catalog file name.
C     TABNAM      O   Catalog table name.
C     ISTYP1      O   True when file is type 1 star catalog.
C     ERRMSG      O   Error message.
C
C$ Detailed_Input
C
C     CATFNM      is the name of the catalog file.
C
C$ Detailed_Output
C
C     TABNAM      is the name of the data table contained in the
C                 catalog. Set to blank if file is not a type 1 star
C                 catalog.
C
C     ISTYP1      is TRUE when the file is a type 1 star catalog. FALSE
C                 otherwise.
C
C     ERRMSG      is a diagnostic message indicating why the file is
C                 not a type 1 star catalog. Set to blank if the file
C                 is a type 1 star catalog.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the indicated file cannot be opened, the error will be
C         diagnosed by routines called by this routine.
C
C     2)  If the indicated file has the wrong architecture version, the
C         error will be diagnosed by routines called by this routine.
C
C     3)  If an I/O error occurs while reading the indicated file, the
C         error will be diagnosed by routines called by this routine.
C
C$ Files
C
C     This routine checks whether file is really SPICE type 1 star
C     catalog file.
C
C     SPICE type 1 star catalog files MUST contain a single data table.
C     It can occupy a single segment or it can spread across multiple
C     segments. This table MUST include the following columns:
C
C        column name                data type          units
C     -------------------------------------------------------
C        RA                   DOUBLE PRECISION        DEGREES
C        DEC                  DOUBLE PRECISION        DEGREES
C        RA_SIGMA             DOUBLE PRECISION        DEGREES
C        DEC_SIGMA            DOUBLE PRECISION        DEGREES
C        CATALOG_NUMBER       INTEGER
C        SPECTRAL_TYPE        CHARACTER*(4)
C        VISUAL_MAGNITUDE     DOUBLE PRECISION
C
C     Nulls are not allowed in any of the columns.
C     Other columns can also be present in the table but their data
C     will NOT be accessible through type 1 star catalog access
C     routines. Note that the names and attributes of these additional
C     columns must be identical for all segments containing this table.
C
C$ Particulars
C
C     This routine does not need to be called by the user's program.
C     It is used by star catalog loader routines to check
C     whether a particular file is a type 1 star catalog before loading
C     the file.
C
C$ Examples
C
C     In the following code fragment, STCC01 is used to determine
C     whether a file is a SPICE type 1 star catalog.
C
C     C
C     C     Call STCC01 to determine whether the file is type 1 star
C     C     catalog file.
C     C
C           CALL STCC01 ( CATFNM, TABNAM, ISTYP1, ERRMSG )
C
C     C
C     C     Check ISTYP1 flag and stop execution and report an
C     C     error if file is not type 1 star catalog file.
C     C
C           IF ( .NOT. ISTYP1 ) THEN
C          .   WRITE (*,*) 'The file:'
C          .   WRITE (*,*) '  ',CATFNM(1:RTRIM(CATFNM))
C          .   WRITE (*,*) 'is not a type 1 star catalog.'
C          .   WRITE (*,*) ERRMSG
C              STOP
C           END IF
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     B.V. Semenov    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 15-MAY-1996 (BVS)
C
C-&
 
C$ Index_Entries
C
C     check whether a file is a type 1 star catalog
C
C-&
 
C
C
C     SPICELIB functions
C
      LOGICAL               RETURN
      INTEGER               EKNSEG
      INTEGER               NBLEN
      INTEGER               ISRCHC
      
 
C
C     Local parameters.
C
      INTEGER               CAT1NC
      PARAMETER           ( CAT1NC = 7 )
 
C
C     Local variables
C
 
      CHARACTER*(4)         CAT1DT ( CAT1NC )
      CHARACTER*(4)         DTYPES ( MXCLSG )
      CHARACTER*(CNAMSZ)    CAT1NM ( CAT1NC )
      CHARACTER*(CNAMSZ)    CNAMES ( MXCLSG )
      CHARACTER*(TNAMSZ)    TMPTNM
      CHARACTER*(TNAMSZ)    TNMPRV
 
      INTEGER               I
      INTEGER               J
      INTEGER               NCOLS
      INTEGER               NROWS
      INTEGER               NUMSEG
      INTEGER               SIZES  ( MXCLSG )
      INTEGER               STRLNS ( MXCLSG )
      INTEGER               TMPHND
 
      LOGICAL               FOUND
      LOGICAL               INDEXD ( MXCLSG )
      LOGICAL               NULLOK ( MXCLSG )
 
      SAVE
 
C
C     Initial values.
C
      DATA                  CAT1NM  /  'CATALOG_NUMBER',
     .                                 'RA',
     .                                 'DEC',
     .                                 'RA_SIGMA',
     .                                 'DEC_SIGMA',
     .                                 'VISUAL_MAGNITUDE',
     .                                 'SPECTRAL_TYPE' /
 
      DATA                  CAT1DT  /  'INT',
     .                                 'DP',
     .                                 'DP',
     .                                 'DP',
     .                                 'DP',
     .                                 'DP',
     .                                 'CHR' /
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'STCC01' )
      END IF
 
C
C     More initial values.
C
      TABNAM = ' '
      ERRMSG = ' '
      ISTYP1 = .TRUE.
 
C
C     Open star catalog file with low level "open for read access"
C     EK routine.
C
      CALL EKOPR ( CATFNM, TMPHND )
 
C
C     Get the number of segments in the file and check whether it is
C     greater than 0 (i.e. some data are is present in the file). If
C     not then set an error message and return to the calling routine.
C
      NUMSEG = EKNSEG ( TMPHND )
 
      IF ( NUMSEG .LE. 0 ) THEN
         ERRMSG = 'File contains no data.'
         ISTYP1 = .FALSE.
         CALL CHKOUT ( 'STCC01' )
         RETURN
      END IF
 
C
C     Loop through the segments to find out whether all of them
C     contain pieces of the same table. If not then set
C     an error message and return to the calling routine.
C
      DO I = 1, NUMSEG
 
         CALL EKSSUM (  TMPHND,  I,       TMPTNM,  NROWS,
     .                  NCOLS,   CNAMES,  DTYPES,  SIZES,
     .                  STRLNS,  INDEXD,  NULLOK          )
 
         IF ( I .GT. 1 ) THEN
 
            IF ( TMPTNM .NE. TNMPRV ) THEN
               ERRMSG = 'File contains more than one data table.'
               ISTYP1 = .FALSE.
               CALL CHKOUT ( 'STCC01' )
               RETURN
            END IF
 
         END IF
 
         TNMPRV = TMPTNM
 
      END DO
 
C
C     Check whether the  number of columns is less than it
C     is supposed to be in type 1 star catalogs. If so then set
C     an error message and return to a calling routine.
C
 
      IF ( NCOLS .LT. CAT1NC ) THEN
         ERRMSG = 'File contains too few data columns.'
         ISTYP1 = .FALSE.
         CALL CHKOUT ( 'STCC01' )
         RETURN
      END IF
 
C
C     Check whether all columns that will be used in catalog search and
C     star data fetching are present in the data table. If not
C     then set an error message and return to a calling routine.
C
      DO I = 1, CAT1NC
 
         FOUND = .FALSE.
 
         J = ISRCHC ( CAT1NM(I), NCOLS, CNAMES )
 
         IF ( J .GT. 0  ) THEN
            FOUND = CAT1DT(I) .EQ. DTYPES(J) .AND. .NOT. NULLOK(J)
         END IF
 
         IF ( .NOT. FOUND ) THEN
            ERRMSG = ' Column '                             //
     .               CAT1NM(I)(1:NBLEN(CAT1NM(I)))          //
     .               ' is not found or'                     //
     .               ' improperly declared in the file.'
            ISTYP1 = .FALSE.
            CALL CHKOUT ( 'STCC01' )
            RETURN
         END IF
 
      END DO
 
C
C     If we got to this point then all checks were passed successfully
C     and the file can be processed as a type 1 star catalog. We
C     "return" the table name and close the file with the EK close
C     routine.
C
      TABNAM = TMPTNM
 
      CALL EKCLS ( TMPHND )
 
      CALL CHKOUT ( 'STCC01' )
      RETURN
      END
