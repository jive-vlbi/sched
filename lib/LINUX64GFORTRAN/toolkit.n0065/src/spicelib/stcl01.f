C$Procedure   STCL01 ( STAR catalog type 1, load catalog file )
 
      SUBROUTINE STCL01 ( CATFNM, TABNAM, HANDLE )
 
C$ Abstract
C
C     Load SPICE type 1 star catalog and return the catalog's
C     table name.
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
 
      CHARACTER*(*)         CATFNM
      CHARACTER*(*)         TABNAM
      INTEGER               HANDLE
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     CATFNM      I   Catalog file name.
C     TABNAM      O   Catalog table name.
C     HANDLE      O   Catalog file handle.
C
C$ Detailed_Input
C
C     CATFNM      is the name of the catalog file.
C
C$ Detailed_Output
C
C     TABNAM      is the name of the table loaded from the catalog
C                 file. This name must be provided as an input argument
C                 to STCF01 catalog search routine. Multiple catalogs
C                 contaning the table TABNAM may be loaded. Sets of
C                 columns, column names and attribites must be
C                 identical through all these files.
C
C     HANDLE      is the integer handle of the catalog file.
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
C     4)  If the catalog file is not a type 1 star catalog file
C         then the error 'SPICE(BADCATALOGFILE)' is signalled.
C
C$ Files
C
C     This routine loads a SPICE type 1 star catalog file.
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
C     will NOT be accessible through STCF01 and STCG01 --
C     the interface used to access data in the catalog. Note
C     that the names and attributes of these additional columns
C     must be identical for all segments containing this table.
C
C$ Particulars
C
C     This STCL01 routine is intended to be part of the user 
C     interface to the SPICE type 1 star catalog. It loads a 
C     SPICE type 1 star catalog file and makes its data available 
C     for searches and retrieval.
C
C     Other routines in SPICE type 1 star catalog access family are:
C
C        STCF01  search through the catalog for all stars within
C                a specified RA-DEC rectangle.
C
C        STCG01  retrieve position and characteristics for
C                every single star found.
C
C$ Examples
C
C     In the following code fragment, STCL01 is used to load
C     a SPICE type 1 star catalog.
C
C     C
C     C     Load catalog file.
C     C
C           CALL STCL01 ( CATFN, TABNAM, HANDLE )
C     C
C     C     Search through the loaded catalog.
C     C
C           CALL STCF01 ( TABNAM, RAMIN,  RAMAX,
C          .              DECMIN, DECMAX, NSTARS )
C     C
C     C     Retrieve data for every star that matched the
C     C     search criteria.
C     C
C           DO I = 1, NSTARS
C
C              CALL STCG01 ( I, RA, DEC, RASIG, DECSIG,
C          .                 CATNUM, SPTYPE, VMAG )
C
C           END DO
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
C-    SPICELIB Version 1.1.0, 18-JUN-1999 (WLT)
C
C        Balanced calls to CHKIN/CHKOUT.
C
C-    SPICELIB Version 1.0.0, 15-MAY-1996 (BVS)
C
C-&
 
C$ Index_Entries
C
C     load a type 1 star catalog file
C
C-&
 
C
C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
C
C     Local variables
C
      LOGICAL               ISTYP1
      CHARACTER*(256)       ERRMSG
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'STCL01' )
      END IF
 
C
C     Check whether the file is really a type 1 star catalog file.
C     If not then signal an error.
C
      CALL STCC01 ( CATFNM, TABNAM, ISTYP1, ERRMSG )
 
      IF ( .NOT. ISTYP1 ) THEN
         CALL SETMSG ('File # is not type 1 star catalog ' //
     .                   'file.' // ERRMSG                  )
         CALL ERRCH  ( '#', CATFNM                          )
         CALL SIGERR ( 'SPICE(BADCATALOGFILE)'              )
         CALL CHKOUT ( 'STCL01' )
         RETURN
      END IF
 
C
C     Load the catalog file with the high level EK loader.
C
      CALL EKLEF  ( CATFNM, HANDLE )
 
      CALL CHKOUT ( 'STCL01' )
      RETURN
      END
