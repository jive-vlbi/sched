C$ Abstract
C
C     MKSPK Include File.
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
C$ Author_and_Institution
C
C     N.G. Khavenson (IKI RAS, Russia)
C     B.V. Semenov   (NAIF, JPL)
C
C$ Version
C
C-    Version 1.3.0, 08-FEB-2012 (BVS).
C
C        Added TLE coverage and ID keywords. Added default TLE pad
C        parameter.
C
C-    Version 1.2.0, 16-JAN-2008 (BVS).
C
C        Added ETTMWR parameter
C
C-    Version 1.1.0, 05-JUN-2001 (BVS).
C
C        Added MAXDEG parameter.
C
C-    Version 1.0.4, 21-MAR-2001 (BVS).
C
C        Added parameter for command line flag '-append' indicating
C        that appending to an existing output file was requested.
C        Added corresponding setup file keyword ('APPEND_TO_OUTPUT'.)
C        Added parameters for yes and no values of this keyword.
C
C-    Version 1.0.3, 28-JAN-2000 (BVS).
C
C        Added parameter specifying number of supported input data
C        types and parameter specifying number of supported output SPK
C        types
C
C-    Version 1.0.2, 22-NOV-1999 (NGK).
C
C        Added parameters for two-line elements processing.
C
C-    Version 1.0.1, 18-MAR-1999 (BVS).
C
C        Added usage, help and template displays. Corrected comments.
C
C-    Version 1.0.0,  8-SEP-1998 (NGK).
C
C-&

C
C     Begin Include Section:  MKSPK generic parameters.
C

C
C     Maximum number of states allowed per one segment.
C
      INTEGER               MAXSTA
      PARAMETER           ( MAXSTA = 10000 )

C
C     String size allocation parameters
C
C
C     Length of buffer for input text processing
C
      INTEGER               BUFLEN
      PARAMETER           ( BUFLEN = 1024 )

C
C     Length of a input text line
C
      INTEGER               LINLEN
      PARAMETER           ( LINLEN = 512 )

C
C     Length of file name and comment line
C
      INTEGER               FILSIZ
      PARAMETER           ( FILSIZ = 255 )

C
C     Length of string for keyword value processing
C
      INTEGER               SHRTLN
      PARAMETER           ( SHRTLN = 80 )

C
C     Length of string for word processing
C
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )
C
C     Length of data order parameters string
C
      INTEGER               VALUEL
      PARAMETER           ( VALUEL = 12 )

C
C     Length of string reserved as delimiter
C
      INTEGER               DLMSIZ
      PARAMETER           ( DLMSIZ = 3 )


C
C     Numbers of different parameters
C
C
C
C     Maximum number of allowed comment lines.
C
      INTEGER               MXCMNT
      PARAMETER           ( MXCMNT = 500 )

C
C     Reserved number of input parameters
C
      INTEGER               MXNVAL
      PARAMETER           ( MXNVAL = 50 )

C
C     Full number of delimiters
C
      INTEGER               ALLNMB
      PARAMETER           ( ALLNMB = 5 )

C
C     Number of delimiters that may appear in time string
C
      INTEGER               TMDNMB
      PARAMETER           ( TMDNMB = 2 )

C
C     Command line flags
C
      CHARACTER*(*)         CMLSET
      PARAMETER           ( CMLSET = '-setup'  )

      CHARACTER*(*)         CMLINP
      PARAMETER           ( CMLINP = '-input'  )

      CHARACTER*(*)         CMLOUT
      PARAMETER           ( CMLOUT = '-output' )

      CHARACTER*(*)         CMLUSS
      PARAMETER           ( CMLUSS = '-u' )

      CHARACTER*(*)         CMLUSL
      PARAMETER           ( CMLUSL = '-usage' )

      CHARACTER*(*)         CMLHLS
      PARAMETER           ( CMLHLS = '-h' )

      CHARACTER*(*)         CMLHLL
      PARAMETER           ( CMLHLL = '-help' )

      CHARACTER*(*)         CMLTMS
      PARAMETER           ( CMLTMS = '-t' )

      CHARACTER*(*)         CMLTML
      PARAMETER           ( CMLTML = '-template' )

      CHARACTER*(*)         CMLAPP
      PARAMETER           ( CMLAPP = '-append' )

C
C     Setup file keywords reserved values
C
      CHARACTER*(*)         KPROID
      PARAMETER           ( KPROID   =   'PRODUCER_ID' )

      CHARACTER*(*)         KSTATM
      PARAMETER           ( KSTATM   =   'START_TIME' )

      CHARACTER*(*)         KSTOTM
      PARAMETER           ( KSTOTM   =   'STOP_TIME' )

      CHARACTER*(*)         KINDFN
      PARAMETER           ( KINDFN   =   'INPUT_DATA_FILE' )

      CHARACTER*(*)         KOSPFN
      PARAMETER           ( KOSPFN   =   'OUTPUT_SPK_FILE' )

      CHARACTER*(*)         KIGNFL
      PARAMETER           ( KIGNFL   =   'IGNORE_FIRST_LINE' )

      CHARACTER*(*)         KINDTP
      PARAMETER           ( KINDTP   =   'INPUT_DATA_TYPE' )

      CHARACTER*(*)         KOSPTP
      PARAMETER           ( KOSPTP   =   'OUTPUT_SPK_TYPE' )

      CHARACTER*(*)         KOBJID
      PARAMETER           ( KOBJID   =   'OBJECT_ID' )

      CHARACTER*(*)         KOBJNM
      PARAMETER           ( KOBJNM   =   'OBJECT_NAME' )

      CHARACTER*(*)         KCENID
      PARAMETER           ( KCENID   =   'CENTER_ID' )

      CHARACTER*(*)         KCENNM
      PARAMETER           ( KCENNM   =   'CENTER_NAME' )

      CHARACTER*(*)         KRFRNM
      PARAMETER           ( KRFRNM   =   'REF_FRAME_NAME' )

      CHARACTER*(*)         KDATOR
      PARAMETER           ( KDATOR   =   'DATA_ORDER' )

      CHARACTER*(*)         KDATDL
      PARAMETER           ( KDATDL   =   'DATA_DELIMITER' )

      CHARACTER*(*)         KINUNI
      PARAMETER           ( KINUNI   =   'INPUT_DATA_UNITS' )

      CHARACTER*(*)         KELENG
      PARAMETER           ( KELENG   =   'EPOCH_STR_LENGTH' )

      CHARACTER*(*)         KTIMWR
      PARAMETER           ( KTIMWR   =   'TIME_WRAPPER' )

      CHARACTER*(*)         LSKFIL
      PARAMETER           ( LSKFIL   =   'LEAPSECONDS_FILE' )

      CHARACTER*(*)         PCKFIL
      PARAMETER           ( PCKFIL   =   'PCK_FILE' )

      CHARACTER*(*)         CMTFIL
      PARAMETER           ( CMTFIL   =   'COMMENT_FILE' )

      CHARACTER*(*)         FRKFIL
      PARAMETER           ( FRKFIL   =   'FRAME_DEF_FILE' )

      CHARACTER*(*)         KSEGID
      PARAMETER           ( KSEGID   =   'SEGMENT_ID' )

      CHARACTER*(*)         KPLDEG
      PARAMETER           ( KPLDEG   =   'POLYNOM_DEGREE' )

      CHARACTER*(*)         KPRTYP
      PARAMETER           ( KPRTYP   =   'PRECESSION_TYPE' )

      CHARACTER*(*)         KCENGM
      PARAMETER           ( KCENGM   =   'CENTER_GM' )

      CHARACTER*(*)         KCEPRA
      PARAMETER           ( KCEPRA   =   'CENTER_POLE_RA' )

      CHARACTER*(*)         KCPDEC
      PARAMETER           ( KCPDEC   =   'CENTER_POLE_DEC' )

      CHARACTER*(*)         KCENJ2
      PARAMETER           ( KCENJ2   =   'CENTER_J2' )

      CHARACTER*(*)         KCNEQR
      PARAMETER           ( KCNEQR   =   'CENTER_EQ_RADIUS' )

      CHARACTER*(*)         KLNPRC
      PARAMETER           ( KLNPRC   =   'LINES_PER_RECORD' )

      CHARACTER*(*)         KAPPND
      PARAMETER           ( KAPPND   =   'APPEND_TO_OUTPUT' )

      CHARACTER*(*)         KTLTID
      PARAMETER           ( KTLTID   =   'TLE_INPUT_OBJ_ID' )

      CHARACTER*(*)         KTLSID
      PARAMETER           ( KTLSID   =   'TLE_SPK_OBJ_ID' )

      CHARACTER*(*)         KTLBPD
      PARAMETER           ( KTLBPD   =   'TLE_START_PAD' )

      CHARACTER*(*)         KTLEPD
      PARAMETER           ( KTLEPD   =   'TLE_STOP_PAD' )

C
C     Standard YES and NO values for setup file keywords.
C
      CHARACTER*(*)         YESVAL
      PARAMETER           ( YESVAL   = 'YES' )

      CHARACTER*(*)         NOVAL
      PARAMETER           ( NOVAL    = 'NO'  )

C
C     Number of supported input data types and input DATA TYPE
C     reserved values.
C
      INTEGER               ITYPNM
      PARAMETER           ( ITYPNM   =  4 )

      CHARACTER*(*)         INSTTP
      PARAMETER           ( INSTTP   =  'STATES' )

      CHARACTER*(*)         INELTP
      PARAMETER           ( INELTP   =  'ELEMENTS' )

      CHARACTER*(*)         INEQTP
      PARAMETER           ( INEQTP   =  'EQ_ELEMENTS' )

      CHARACTER*(*)         INTLTP
      PARAMETER           ( INTLTP   =  'TL_ELEMENTS' )

C
C     Number of supported output SPK data types -- this version
C     supports SPK types 5, 8, 9, 10, 12, 13, 15 and 17.
C
      INTEGER               OTYPNM
      PARAMETER           ( OTYPNM   =  8 )

C
C     End of input record marker
C
      CHARACTER*(*)         EORMRK
      PARAMETER           ( EORMRK   = '$' )

C
C     Maximum allowed polynomial degree. The value of this parameter
C     is consistent with the ones in SPKW* routines.
C
      INTEGER               MAXDEG
      PARAMETER           ( MAXDEG   =  15 )

C
C     Special time wrapper tag for input times given as ET seconds past
C     J2000
C
      CHARACTER*(*)         ETTMWR
      PARAMETER           ( ETTMWR   =  'ETSECONDS' )

C
C     Default TLE pad, 1/2 day in seconds.
C
      DOUBLE PRECISION      DTLEPD
      PARAMETER           ( DTLEPD   =  43200.D0 )

C
C     End Include Section:  MKSPK generic parameters.
C
