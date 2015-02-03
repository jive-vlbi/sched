C$ Abstract
C
C     This file contains public, global parameter declarations
C     for the SPICELIB Geometry Finder (GF) subsystem.
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
C     GF
C
C$ Keywords
C
C     GEOMETRY
C     ROOT
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman      (JPL)
C     L.E. Elson        (JPL)
C     E.D. Wright       (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 1.3.0, 01-OCT-2011 (NJB) 
C
C       Added NWILUM parameter.
C
C-    SPICELIB Version 1.2.0, 14-SEP-2010 (EDW) 
C
C       Added NWPA parameter.
C
C-    SPICELIB Version 1.1.0, 08-SEP-2009 (EDW)
C
C       Added NWRR parameter.
C       Added NWUDS parameter.
C
C-    SPICELIB Version 1.0.0, 21-FEB-2009 (NJB) (LSE) (EDW)
C
C-&


C
C     Root finding parameters:
C
C     CNVTOL is the default convergence tolerance used by the
C     high-level GF search API routines. This tolerance is
C     used to terminate searches for binary state transitions:
C     when the time at which a transition occurs is bracketed
C     by two times that differ by no more than CNVTOL, the
C     transition time is considered to have been found.
C
C     Units are TDB seconds.
C
      DOUBLE PRECISION      CNVTOL
      PARAMETER           ( CNVTOL = 1.D-6 )


C
C     NWMAX is the maximum number of windows allowed for user-defined
C     workspace array.
C     
C        DOUBLE PRECISION      WORK   ( LBCELL : MW, NWMAX )
C
C     Currently no more than twelve windows are required; the three 
C     extra windows are spares.
C   
C     Callers of GFEVNT can include this file and use the parameter
C     NWMAX to declare the second dimension of the workspace array
C     if necessary.
C
      INTEGER               NWMAX
      PARAMETER           ( NWMAX = 15 )

C
C     Callers of GFIDST should declare their workspace window
C     count using NWDIST.
C
      INTEGER               NWDIST
      PARAMETER           ( NWDIST = 5 )


C
C     Callers of GFSEP should declare their workspace window
C     count using NWSEP.
C
      INTEGER               NWSEP
      PARAMETER           ( NWSEP  = 5 )


C
C     Callers of GFRR should declare their workspace window
C     count using NWRR.
C
      INTEGER               NWRR
      PARAMETER           ( NWRR   = 5 )


C
C     Callers of GFUDS should declare their workspace window
C     count using NWUDS.
C
      INTEGER               NWUDS
      PARAMETER           ( NWUDS  = 5 )


C
C     Callers of GFPA should declare their workspace window
C     count using NWPA.
C
      INTEGER               NWPA
      PARAMETER           ( NWPA  = 5 )

C
C     Callers of GFILUM should declare their workspace window
C     count using NWILUM.
C
      INTEGER               NWILUM
      PARAMETER           ( NWILUM = 5 )


C
C     ADDWIN is a parameter used to expand each interval of the search
C     (confinement) window by a small amount at both ends in order to
C     accommodate searches using equality constraints. The loaded
C     kernel files must accommodate these expanded time intervals.
C
      DOUBLE PRECISION      ADDWIN
      PARAMETER           ( ADDWIN = 0.5D0 )

C
C     FRMNLN is a string length for frame names.
C

      INTEGER               FRMNLN
      PARAMETER           ( FRMNLN = 32 )

C
C     NVRMAX is the maximum number of vertices if FOV type is "POLYGON"
C
      INTEGER               NVRMAX
      PARAMETER           ( NVRMAX = 50 )

C
C     FOVTLN -- maximum length for FOV string.
C
      INTEGER               FOVTLN
      PARAMETER           ( FOVTLN = 40 )
 
C
C     Specify the character strings that are allowed in the
C     specification of field of view shapes.
C

      
      CHARACTER*(*)         FTCIRC
      PARAMETER           ( FTCIRC = 'CIRCLE' )

      CHARACTER*(*)         FTELLI
      PARAMETER           ( FTELLI = 'ELLIPSE' )

      CHARACTER*(*)         FTPOLY
      PARAMETER           ( FTPOLY = 'POLYGON' )

      CHARACTER*(*)         FTRECT
      PARAMETER           ( FTRECT = 'RECTANGLE' )
 
C
C     Character strings that are allowed in the
C     specification of occultation types:
C
      CHARACTER*(*)         ANNULR
      PARAMETER           ( ANNULR = 'ANNULAR' )

      CHARACTER*(*)         ANY
      PARAMETER           ( ANY    = 'ANY'     ) 

      CHARACTER*(*)         PARTL
      PARAMETER           ( PARTL  = 'PARTIAL' )

      CHARACTER*(*)         FULL
      PARAMETER           ( FULL   = 'FULL'    ) 

C
C     Occultation target shape specifications:
C
      CHARACTER*(*)         EDSHAP
      PARAMETER           ( EDSHAP = 'ELLIPSOID'   ) 

      CHARACTER*(*)         PTSHAP
      PARAMETER           ( PTSHAP = 'POINT'   ) 

      CHARACTER*(*)         RYSHAP
      PARAMETER           ( RYSHAP = 'RAY' )

      CHARACTER*(*)         SPSHAP
      PARAMETER           ( SPSHAP = 'SPHERE' )
      
C
C     Specify the number of supported occultation types and occultation
C     type string length:
C 
      INTEGER               NOCTYP
      PARAMETER           ( NOCTYP = 4 )

      INTEGER               OCLLN
      PARAMETER           ( OCLLN  = 7 )

      INTEGER               SHPLEN
      PARAMETER           ( SHPLEN = 9 )

C
C     Instrument field-of-view (FOV) parameters
C
C     Maximum number of FOV boundary vectors:
C
      INTEGER               MAXVRT
      PARAMETER           ( MAXVRT = 10000 )

C
C     FOV shape parameters:
C
C        circle
C        ellipse
C        polygon
C        rectangle
C
      CHARACTER*(*)         CIRFOV
      PARAMETER           ( CIRFOV = 'CIRCLE' )

      CHARACTER*(*)         ELLFOV
      PARAMETER           ( ELLFOV = 'ELLIPSE' )

      CHARACTER*(*)         POLFOV
      PARAMETER           ( POLFOV = 'POLYGON' )

      CHARACTER*(*)         RECFOV
      PARAMETER           ( RECFOV = 'RECTANGLE' )


C
C     End of file gf.inc.
C 
