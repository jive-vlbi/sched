C$Procedure ZZIDMAP ( Private --- SPICE body ID/name assignments )

      SUBROUTINE  ZZIDMAP( BLTCOD, BLTNAM )
      IMPLICIT NONE

C$ Abstract
C
C     The default SPICE body/ID mapping assignments available
C     to the SPICE library.
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
C     naif_ids.req
C
C$ Keywords
C
C     Body mappings.
C
C$ Declarations

      INCLUDE              'zzbodtrn.inc'

      INTEGER              BLTCOD(NPERM)
      CHARACTER*(MAXL)     BLTNAM(NPERM)

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     BLTCOD     O  List of default integer ID codes
C     BLTNAM     O  List of default names
C     NPERM      P  Number of name/ID mappings
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     BLTCOD     The array of NPERM elements listing the body ID codes.
C
C     BLTNAM     The array of NPERM elements listing the body names 
C                corresponding to the ID entry in BLTCOD
C
C$ Parameters
C
C     NPERM      The length of both BLTCOD, BLTNAM
C                (read from zzbodtrn.inc).
C
C$ Exceptions
C
C     None.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     Each ith entry of BLTCOD maps to the ith entry of BLTNAM.
C
C$ Examples
C
C     Simple to use, a call the ZZIDMAP returns the arrays defining the
C     name/ID mappings.
C
C
C        INCLUDE            'zzbodtrn.inc'
C
C        INTEGER             ID  ( NPERM )
C        CHARACTER*(MAXL)    NAME( NPERM )
C
C        CALL ZZIDMAP( ID, NAME )
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
C     E.D. Wright, 07-MAY-2014 (JPL)
C
C$ Version
C
C-    SPICELIB 1.0.8 06-MAY-2014 (EDW)
C
C         Edited text comments in Asteroids section and Comets section.
C
C         Eliminated "PI" IAU Number from "CHARON" description.
C
C         HYROKKIN (644) spelling corrected to HYRROKKIN.
C
C     Added:
C
C             -750   SPRINT-AS
C             -189   NSYT
C             -189   INSIGHT
C             -170   JWST
C             -170   JAMES WEBB SPACE TELESCOPE
C             -144   SOLO
C             -144   SOLAR ORBITER
C              -96   SPP
C              -96   SOLAR PROBE PLUS
C              -64   ORX
C              -64   OSIRIS-REX
C              -54   ARM
C              -54   ASTEROID RETRIEVAL MISSION
C              -12   LADEE
C               -3   MOM
C               -3   MARS ORBITER MISSION
C                0   SOLAR_SYSTEM_BARYCENTER
C                1   MERCURY_BARYCENTER
C                2   VENUS_BARYCENTER
C                3   EARTH_BARYCENTER
C                4   MARS_BARYCENTER
C                5   JUPITER_BARYCENTER
C                6   SATURN_BARYCENTER
C                7   URANUS_BARYCENTER
C                8   NEPTUNE_BARYCENTER
C                9   PLUTO_BARYCENTER
C              644   HYRROKKIN
C              904   KERBEROS
C              905   STYX
C          1003228   C/2013 A1
C          1003228   SIDING SPRING
C          2000002   PALLAS
C          2000511   DAVIDA
C
C     Removed assignments:
C
C             -486   HERSCHEL
C             -489   PLANCK
C             -187   SOLAR PROBE
C
C-    SPICELIB 1.0.7 20-MAY-2010 (EDW)
C
C        Edit to vehicle ID list to correct -76 not in proper
C        numerical (descending) order.
C
C     Added:
C
C               -5   AKATSUKI
C               -5   VCO
C             -121   BEPICOLOMBO
C             -177   GRAIL-A
C             -181   GRAIL-B
C             -202   MAVEN
C             -205   SOIL MOISTURE ACTIVE AND PASSIVE
C             -205   SMAP
C             -362   RADIATION BELT STORM PROBE A
C             -362   RBSP_A
C             -363   RADIATION BELT STORM PROBE B
C             -363   RBSP_B
C              550   HERSE
C              653   AEGAEON
C          1000093   TEMPEL_1
C          2000021   LUTETIA
C          2004179   TOUTATIS
C
C-    SPICELIB 1.0.6 08-APR-2009 (EDW)
C
C     Added:
C
C               -5   PLC
C               -5   PLANET-C
C              -68   MMO
C              -68   MERCURY MAGNETOSPHERIC ORBITER
C              -69   MPO
C              -69   MERCURY PLANETARY ORBITER
C          2002867   STEINS
C             -140   EPOCH
C             -140   DIXI
C
C-    SPICELIB 1.0.5 09-JAN-2008 (EDW)
C
C     Added:
C
C              -18   LCROSS
C              -29   NEXT
C              -86   CH1
C              -86   CHANDRAYAAN-1
C             -131   KAGUYA
C             -140   EPOXI
C             -151   CHANDRA
C             -187   SOLAR PROBE
C              636   AEGIR
C              637   BEBHIONN
C              638   BERGELMIR
C              639   BESTLA
C              640   FARBAUTI
C              641   FENRIR
C              642   FORNJOT
C              643   HATI
C              644   HYROKKIN
C              645   KARI
C              646   LOGE
C              647   SKOLL
C              648   SURTUR
C              649   ANTHE
C              650   JARNSAXA
C              651   GREIP
C              652   TARQEQ
C              809   HALIMEDE
C              810   PSAMATHE
C              811   SAO
C              812   LAOMEDEIA
C              813   NESO
C
C     NAIF modified the Jovian system listing to conform to the
C     current (as of this date) name/body mapping.
C
C              540   MNEME
C              541   AOEDE
C              542   THELXINOE
C              543   ARCHE
C              544   KALLICHORE
C              545   HELIKE
C              546   CARPO
C              547   EUKELADE
C              548   CYLLENE
C              549   KORE
C
C     Removed assignments:
C
C             -172   SPACETECH-3 COMBINER
C             -174   PLUTO-KUIPER EXPRESS
C             -175   PLUTO-KUIPER EXPRESS SIMULATION
C             -205   SPACETECH-3 COLLECTOR
C              514   1979J2
C              515   1979J1
C              516   1979J3
C              610   1980S1
C              611   1980S3
C              612   1980S6
C              613   1980S13
C              614   1980S25
C              615   1980S28
C              616   1980S27
C              617   1980S26
C              706   1986U7
C              707   1986U8
C              708   1986U9
C              709   1986U4
C              710   1986U6
C              711   1986U3
C              712   1986U1
C              713   1986U2
C              714   1986U5
C              715   1985U1
C              718   1986U10
C              901   1978P1
C
C     Spelling correction:
C
C        MAGACLITE to MEGACLITE
C
C     Rename:
C
C        ERRIAPO to ERRIAPUS
C        STV-1 to STV51
C        STV-2 to STV52
C        STV-3 to STV53
C
C
C-    SPICELIB 1.0.4 01-NOV-2006 (EDW)
C
C     NAIF removed several provisional name/ID mappings from
C     the Jovian system listing:
C
C     539         'HEGEMONE'              JXXXIX
C     540         'MNEME'                 JXL
C     541         'AOEDE'                 JXLI
C     542         'THELXINOE'             JXLII
C     543         'ARCHE'                 JXLIII
C     544         'KALLICHORE'            JXLIV
C     545         'HELIKE'                JXLV
C     546         'CARPO'                 JXLVI
C     547         'EUKELADE'              JXLVII
C     548         'CYLLENE'               JXLVIII
C
C     The current mapping set for the range 539-561:
C
C              540   ARCHE
C              541   EUKELADE
C              546   HELIKE
C              547   AOEDE
C              548   HEGEMONE
C              551   KALLICHORE
C              553   CYLLENE
C              560   CARPO
C              561   MNEME
C
C     The new mapping leaves the IDs 539, 542-545, 549, 550, 552,
C     554-559 unassigned.
C
C     Added:
C
C              635   DAPHNIS
C              722   FRANCISCO
C              723   MARGARET
C              724   FERDINAND
C              725   PERDITA
C              726   MAB
C              727   CUPID
C              -61   JUNO
C              -76   MSL
C              -76   MARS SCIENCE LABORATORY
C             -212   STV-1
C             -213   STV-2
C             -214   STV-3
C              902   NIX
C              903   HYDRA
C             -85    LRO
C             -85    LUNAR RECON ORBITER
C             -85    LUNAR RECONNAISSANCE ORBITER
C
C     Spelling correction
C
C              632   METHODE to METHONE
C
C-    SPICELIB 1.0.3 14-NOV-2005 (EDW)
C
C     Added:
C
C              539   HEGEMONE
C              540   MNEME
C              541   AOEDE
C              542   THELXINOE
C              543   ARCHE
C              544   KALLICHORE
C              545   HELIKE
C              546   CARPO
C              547   EUKELADE
C              548   CYLLENE
C              631   NARVI
C              632   METHODE
C              633   PALLENE
C              634   POLYDEUCES
C          2025143   ITOKAWA
C              -98   NEW HORIZONS
C             -248   VENUS EXPRESS, VEX
C             -500   RSAT, SELENE Relay Satellite, SELENE Rstar, Rstar
C             -502   VSAT, SELENE VLBI Radio Satellite,
C                    SELENE VRAD Satellite, SELENE Vstar
C           399064   DSS-64
C
C      Change in spelling:
C
C              623   SUTTUNG to SUTTUNGR
C              627   SKADI   to SKATHI
C              630   THRYM   to THRYMR
C
C-    SPICELIB 1.0.2 20-DEC-2004 (EDW)
C
C     Added:
C
C           Due to the previous definition of Parkes with DSS-05,
C           the Parkes ID remains 399005.
C
C             -486   HERSCHEL
C             -489   PLANCK
C           399049   DSS-49
C           399055   DSS-55
C             -203   DAWN
C          1000012   67P/CHURYUMOV-GERASIMENKO (1969 R1)
C          1000012   CHURYUMOV-GERASIMENKO
C          398989    NOTO
C             -84    PHOENIX
C            -131    SELENE
C            -238    SMART-1, S1, SM1, SMART1
C            -130    HAYABUSA
C
C-    SPICELIB 1.0.1 19-DEC-2003 (EDW)
C
C     Added:
C              -79   SPITZER
C          2000216   KLEOPATRA
C
C-    SPICELIB 1.0.0 27-JUL-2003 (EDW)
C
C     Added:
C              -47   GNS
C              -74   MRO
C              -74   MARS RECON ORBITER
C             -130   MUSES-C
C             -142   TERRA
C             -154   AQUA
C             -159   EUROPA ORBITER
C             -190   SIM
C             -198   INTEGRAL
C             -227   KEPLER
C             -234   STEREO AHEAD
C             -235   STEREO BEHIND
C             -253   OPPORTUNITY
C             -254   SPIRIT
C              528   AUTONOE
C              529   THYONE
C              530   HERMIPPE
C              531   AITNE
C              532   EURYDOME
C              533   EUANTHE
C              534   EUPORIE
C              535   ORTHOSIE
C              536   SPONDE
C              537   KALE
C              538   PASITHEE
C              619   YMIR
C              620   PAALIAQ
C              621   TARVOS
C              622   IJIRAQ
C              623   SUTTUNG
C              624   KIVIUQ
C              625   MUNDILFARI
C              626   ALBIORIX
C              627   SKADI
C              628   ERRIAPO
C              629   SIARNAQ
C              630   THRYM
C              718   PROSPERO
C              719   SETEBOS
C              720   STEPHANO
C              721   TRINCULO
C           398990   NEW NORCIA
C          2431011   DACTYL
C          2000001   CERES
C          2000004   VESTA
C
C     Renamed:
C
C              -25   LPM to
C              -25   LP
C
C             -180   MUSES-C to
C             -130   MUSES-B
C
C             -172   STARLIGHT COMBINER to
C             -172   SPACETECH-3 COMBINER
C
C             -205   STARLIGHT COLLECTOR to
C             -205   SPACETECH-3 COLLECTOR
C
C      Removed:
C             -172   SLCOMB
C
C
C-&

C$ Index_Entries
C
C     body ID mapping
C
C-&

C
C     A script generates this file. Do not edit by hand.
C     Edit the creation script to modify the contents of
C     ZZIDMAP.
C

      BLTCOD(1) =   0
      BLTNAM(1) =  'SOLAR_SYSTEM_BARYCENTER'

      BLTCOD(2) =   0
      BLTNAM(2) =  'SSB'

      BLTCOD(3) =   0
      BLTNAM(3) =  'SOLAR SYSTEM BARYCENTER'

      BLTCOD(4) =   1
      BLTNAM(4) =  'MERCURY_BARYCENTER'

      BLTCOD(5) =   1
      BLTNAM(5) =  'MERCURY BARYCENTER'

      BLTCOD(6) =   2
      BLTNAM(6) =  'VENUS_BARYCENTER'

      BLTCOD(7) =   2
      BLTNAM(7) =  'VENUS BARYCENTER'

      BLTCOD(8) =   3
      BLTNAM(8) =  'EARTH_BARYCENTER'

      BLTCOD(9) =   3
      BLTNAM(9) =  'EMB'

      BLTCOD(10) =   3
      BLTNAM(10) =  'EARTH MOON BARYCENTER'

      BLTCOD(11) =   3
      BLTNAM(11) =  'EARTH-MOON BARYCENTER'

      BLTCOD(12) =   3
      BLTNAM(12) =  'EARTH BARYCENTER'

      BLTCOD(13) =   4
      BLTNAM(13) =  'MARS_BARYCENTER'

      BLTCOD(14) =   4
      BLTNAM(14) =  'MARS BARYCENTER'

      BLTCOD(15) =   5
      BLTNAM(15) =  'JUPITER_BARYCENTER'

      BLTCOD(16) =   5
      BLTNAM(16) =  'JUPITER BARYCENTER'

      BLTCOD(17) =   6
      BLTNAM(17) =  'SATURN_BARYCENTER'

      BLTCOD(18) =   6
      BLTNAM(18) =  'SATURN BARYCENTER'

      BLTCOD(19) =   7
      BLTNAM(19) =  'URANUS_BARYCENTER'

      BLTCOD(20) =   7
      BLTNAM(20) =  'URANUS BARYCENTER'

      BLTCOD(21) =   8
      BLTNAM(21) =  'NEPTUNE_BARYCENTER'

      BLTCOD(22) =   8
      BLTNAM(22) =  'NEPTUNE BARYCENTER'

      BLTCOD(23) =   9
      BLTNAM(23) =  'PLUTO_BARYCENTER'

      BLTCOD(24) =   9
      BLTNAM(24) =  'PLUTO BARYCENTER'

      BLTCOD(25) =   10
      BLTNAM(25) =  'SUN'

      BLTCOD(26) =   199
      BLTNAM(26) =  'MERCURY'

      BLTCOD(27) =   299
      BLTNAM(27) =  'VENUS'

      BLTCOD(28) =   399
      BLTNAM(28) =  'EARTH'

      BLTCOD(29) =   301
      BLTNAM(29) =  'MOON'

      BLTCOD(30) =   499
      BLTNAM(30) =  'MARS'

      BLTCOD(31) =   401
      BLTNAM(31) =  'PHOBOS'

      BLTCOD(32) =   402
      BLTNAM(32) =  'DEIMOS'

      BLTCOD(33) =   599
      BLTNAM(33) =  'JUPITER'

      BLTCOD(34) =   501
      BLTNAM(34) =  'IO'

      BLTCOD(35) =   502
      BLTNAM(35) =  'EUROPA'

      BLTCOD(36) =   503
      BLTNAM(36) =  'GANYMEDE'

      BLTCOD(37) =   504
      BLTNAM(37) =  'CALLISTO'

      BLTCOD(38) =   505
      BLTNAM(38) =  'AMALTHEA'

      BLTCOD(39) =   506
      BLTNAM(39) =  'HIMALIA'

      BLTCOD(40) =   507
      BLTNAM(40) =  'ELARA'

      BLTCOD(41) =   508
      BLTNAM(41) =  'PASIPHAE'

      BLTCOD(42) =   509
      BLTNAM(42) =  'SINOPE'

      BLTCOD(43) =   510
      BLTNAM(43) =  'LYSITHEA'

      BLTCOD(44) =   511
      BLTNAM(44) =  'CARME'

      BLTCOD(45) =   512
      BLTNAM(45) =  'ANANKE'

      BLTCOD(46) =   513
      BLTNAM(46) =  'LEDA'

      BLTCOD(47) =   514
      BLTNAM(47) =  'THEBE'

      BLTCOD(48) =   515
      BLTNAM(48) =  'ADRASTEA'

      BLTCOD(49) =   516
      BLTNAM(49) =  'METIS'

      BLTCOD(50) =   517
      BLTNAM(50) =  'CALLIRRHOE'

      BLTCOD(51) =   518
      BLTNAM(51) =  'THEMISTO'

      BLTCOD(52) =   519
      BLTNAM(52) =  'MAGACLITE'

      BLTCOD(53) =   520
      BLTNAM(53) =  'TAYGETE'

      BLTCOD(54) =   521
      BLTNAM(54) =  'CHALDENE'

      BLTCOD(55) =   522
      BLTNAM(55) =  'HARPALYKE'

      BLTCOD(56) =   523
      BLTNAM(56) =  'KALYKE'

      BLTCOD(57) =   524
      BLTNAM(57) =  'IOCASTE'

      BLTCOD(58) =   525
      BLTNAM(58) =  'ERINOME'

      BLTCOD(59) =   526
      BLTNAM(59) =  'ISONOE'

      BLTCOD(60) =   527
      BLTNAM(60) =  'PRAXIDIKE'

      BLTCOD(61) =   528
      BLTNAM(61) =  'AUTONOE'

      BLTCOD(62) =   529
      BLTNAM(62) =  'THYONE'

      BLTCOD(63) =   530
      BLTNAM(63) =  'HERMIPPE'

      BLTCOD(64) =   531
      BLTNAM(64) =  'AITNE'

      BLTCOD(65) =   532
      BLTNAM(65) =  'EURYDOME'

      BLTCOD(66) =   533
      BLTNAM(66) =  'EUANTHE'

      BLTCOD(67) =   534
      BLTNAM(67) =  'EUPORIE'

      BLTCOD(68) =   535
      BLTNAM(68) =  'ORTHOSIE'

      BLTCOD(69) =   536
      BLTNAM(69) =  'SPONDE'

      BLTCOD(70) =   537
      BLTNAM(70) =  'KALE'

      BLTCOD(71) =   538
      BLTNAM(71) =  'PASITHEE'

      BLTCOD(72) =   539
      BLTNAM(72) =  'HEGEMONE'

      BLTCOD(73) =   540
      BLTNAM(73) =  'MNEME'

      BLTCOD(74) =   541
      BLTNAM(74) =  'AOEDE'

      BLTCOD(75) =   542
      BLTNAM(75) =  'THELXINOE'

      BLTCOD(76) =   543
      BLTNAM(76) =  'ARCHE'

      BLTCOD(77) =   544
      BLTNAM(77) =  'KALLICHORE'

      BLTCOD(78) =   545
      BLTNAM(78) =  'HELIKE'

      BLTCOD(79) =   546
      BLTNAM(79) =  'CARPO'

      BLTCOD(80) =   547
      BLTNAM(80) =  'EUKELADE'

      BLTCOD(81) =   548
      BLTNAM(81) =  'CYLLENE'

      BLTCOD(82) =   549
      BLTNAM(82) =  'KORE'

      BLTCOD(83) =   550
      BLTNAM(83) =  'HERSE'

      BLTCOD(84) =   699
      BLTNAM(84) =  'SATURN'

      BLTCOD(85) =   601
      BLTNAM(85) =  'MIMAS'

      BLTCOD(86) =   602
      BLTNAM(86) =  'ENCELADUS'

      BLTCOD(87) =   603
      BLTNAM(87) =  'TETHYS'

      BLTCOD(88) =   604
      BLTNAM(88) =  'DIONE'

      BLTCOD(89) =   605
      BLTNAM(89) =  'RHEA'

      BLTCOD(90) =   606
      BLTNAM(90) =  'TITAN'

      BLTCOD(91) =   607
      BLTNAM(91) =  'HYPERION'

      BLTCOD(92) =   608
      BLTNAM(92) =  'IAPETUS'

      BLTCOD(93) =   609
      BLTNAM(93) =  'PHOEBE'

      BLTCOD(94) =   610
      BLTNAM(94) =  'JANUS'

      BLTCOD(95) =   611
      BLTNAM(95) =  'EPIMETHEUS'

      BLTCOD(96) =   612
      BLTNAM(96) =  'HELENE'

      BLTCOD(97) =   613
      BLTNAM(97) =  'TELESTO'

      BLTCOD(98) =   614
      BLTNAM(98) =  'CALYPSO'

      BLTCOD(99) =   615
      BLTNAM(99) =  'ATLAS'

      BLTCOD(100) =   616
      BLTNAM(100) =  'PROMETHEUS'

      BLTCOD(101) =   617
      BLTNAM(101) =  'PANDORA'

      BLTCOD(102) =   618
      BLTNAM(102) =  'PAN'

      BLTCOD(103) =   619
      BLTNAM(103) =  'YMIR'

      BLTCOD(104) =   620
      BLTNAM(104) =  'PAALIAQ'

      BLTCOD(105) =   621
      BLTNAM(105) =  'TARVOS'

      BLTCOD(106) =   622
      BLTNAM(106) =  'IJIRAQ'

      BLTCOD(107) =   623
      BLTNAM(107) =  'SUTTUNGR'

      BLTCOD(108) =   624
      BLTNAM(108) =  'KIVIUQ'

      BLTCOD(109) =   625
      BLTNAM(109) =  'MUNDILFARI'

      BLTCOD(110) =   626
      BLTNAM(110) =  'ALBIORIX'

      BLTCOD(111) =   627
      BLTNAM(111) =  'SKATHI'

      BLTCOD(112) =   628
      BLTNAM(112) =  'ERRIAPUS'

      BLTCOD(113) =   629
      BLTNAM(113) =  'SIARNAQ'

      BLTCOD(114) =   630
      BLTNAM(114) =  'THRYMR'

      BLTCOD(115) =   631
      BLTNAM(115) =  'NARVI'

      BLTCOD(116) =   632
      BLTNAM(116) =  'METHONE'

      BLTCOD(117) =   633
      BLTNAM(117) =  'PALLENE'

      BLTCOD(118) =   634
      BLTNAM(118) =  'POLYDEUCES'

      BLTCOD(119) =   635
      BLTNAM(119) =  'DAPHNIS'

      BLTCOD(120) =   636
      BLTNAM(120) =  'AEGIR'

      BLTCOD(121) =   637
      BLTNAM(121) =  'BEBHIONN'

      BLTCOD(122) =   638
      BLTNAM(122) =  'BERGELMIR'

      BLTCOD(123) =   639
      BLTNAM(123) =  'BESTLA'

      BLTCOD(124) =   640
      BLTNAM(124) =  'FARBAUTI'

      BLTCOD(125) =   641
      BLTNAM(125) =  'FENRIR'

      BLTCOD(126) =   642
      BLTNAM(126) =  'FORNJOT'

      BLTCOD(127) =   643
      BLTNAM(127) =  'HATI'

      BLTCOD(128) =   644
      BLTNAM(128) =  'HYRROKKIN'

      BLTCOD(129) =   645
      BLTNAM(129) =  'KARI'

      BLTCOD(130) =   646
      BLTNAM(130) =  'LOGE'

      BLTCOD(131) =   647
      BLTNAM(131) =  'SKOLL'

      BLTCOD(132) =   648
      BLTNAM(132) =  'SURTUR'

      BLTCOD(133) =   649
      BLTNAM(133) =  'ANTHE'

      BLTCOD(134) =   650
      BLTNAM(134) =  'JARNSAXA'

      BLTCOD(135) =   651
      BLTNAM(135) =  'GREIP'

      BLTCOD(136) =   652
      BLTNAM(136) =  'TARQEQ'

      BLTCOD(137) =   653
      BLTNAM(137) =  'AEGAEON'

      BLTCOD(138) =   799
      BLTNAM(138) =  'URANUS'

      BLTCOD(139) =   701
      BLTNAM(139) =  'ARIEL'

      BLTCOD(140) =   702
      BLTNAM(140) =  'UMBRIEL'

      BLTCOD(141) =   703
      BLTNAM(141) =  'TITANIA'

      BLTCOD(142) =   704
      BLTNAM(142) =  'OBERON'

      BLTCOD(143) =   705
      BLTNAM(143) =  'MIRANDA'

      BLTCOD(144) =   706
      BLTNAM(144) =  'CORDELIA'

      BLTCOD(145) =   707
      BLTNAM(145) =  'OPHELIA'

      BLTCOD(146) =   708
      BLTNAM(146) =  'BIANCA'

      BLTCOD(147) =   709
      BLTNAM(147) =  'CRESSIDA'

      BLTCOD(148) =   710
      BLTNAM(148) =  'DESDEMONA'

      BLTCOD(149) =   711
      BLTNAM(149) =  'JULIET'

      BLTCOD(150) =   712
      BLTNAM(150) =  'PORTIA'

      BLTCOD(151) =   713
      BLTNAM(151) =  'ROSALIND'

      BLTCOD(152) =   714
      BLTNAM(152) =  'BELINDA'

      BLTCOD(153) =   715
      BLTNAM(153) =  'PUCK'

      BLTCOD(154) =   716
      BLTNAM(154) =  'CALIBAN'

      BLTCOD(155) =   717
      BLTNAM(155) =  'SYCORAX'

      BLTCOD(156) =   718
      BLTNAM(156) =  'PROSPERO'

      BLTCOD(157) =   719
      BLTNAM(157) =  'SETEBOS'

      BLTCOD(158) =   720
      BLTNAM(158) =  'STEPHANO'

      BLTCOD(159) =   721
      BLTNAM(159) =  'TRINCULO'

      BLTCOD(160) =   722
      BLTNAM(160) =  'FRANCISCO'

      BLTCOD(161) =   723
      BLTNAM(161) =  'MARGARET'

      BLTCOD(162) =   724
      BLTNAM(162) =  'FERDINAND'

      BLTCOD(163) =   725
      BLTNAM(163) =  'PERDITA'

      BLTCOD(164) =   726
      BLTNAM(164) =  'MAB'

      BLTCOD(165) =   727
      BLTNAM(165) =  'CUPID'

      BLTCOD(166) =   899
      BLTNAM(166) =  'NEPTUNE'

      BLTCOD(167) =   801
      BLTNAM(167) =  'TRITON'

      BLTCOD(168) =   802
      BLTNAM(168) =  'NEREID'

      BLTCOD(169) =   803
      BLTNAM(169) =  'NAIAD'

      BLTCOD(170) =   804
      BLTNAM(170) =  'THALASSA'

      BLTCOD(171) =   805
      BLTNAM(171) =  'DESPINA'

      BLTCOD(172) =   806
      BLTNAM(172) =  'GALATEA'

      BLTCOD(173) =   807
      BLTNAM(173) =  'LARISSA'

      BLTCOD(174) =   808
      BLTNAM(174) =  'PROTEUS'

      BLTCOD(175) =   809
      BLTNAM(175) =  'HALIMEDE'

      BLTCOD(176) =   810
      BLTNAM(176) =  'PSAMATHE'

      BLTCOD(177) =   811
      BLTNAM(177) =  'SAO'

      BLTCOD(178) =   812
      BLTNAM(178) =  'LAOMEDEIA'

      BLTCOD(179) =   813
      BLTNAM(179) =  'NESO'

      BLTCOD(180) =   999
      BLTNAM(180) =  'PLUTO'

      BLTCOD(181) =   901
      BLTNAM(181) =  'CHARON'

      BLTCOD(182) =   902
      BLTNAM(182) =  'NIX'

      BLTCOD(183) =   903
      BLTNAM(183) =  'HYDRA'

      BLTCOD(184) =   904
      BLTNAM(184) =  'KERBEROS'

      BLTCOD(185) =   905
      BLTNAM(185) =  'STYX'

      BLTCOD(186) =   -1
      BLTNAM(186) =  'GEOTAIL'

      BLTCOD(187) =   -3
      BLTNAM(187) =  'MOM'

      BLTCOD(188) =   -3
      BLTNAM(188) =  'MARS ORBITER MISSION'

      BLTCOD(189) =   -5
      BLTNAM(189) =  'AKATSUKI'

      BLTCOD(190) =   -5
      BLTNAM(190) =  'VCO'

      BLTCOD(191) =   -5
      BLTNAM(191) =  'PLC'

      BLTCOD(192) =   -5
      BLTNAM(192) =  'PLANET-C'

      BLTCOD(193) =   -6
      BLTNAM(193) =  'P6'

      BLTCOD(194) =   -6
      BLTNAM(194) =  'PIONEER-6'

      BLTCOD(195) =   -7
      BLTNAM(195) =  'P7'

      BLTCOD(196) =   -7
      BLTNAM(196) =  'PIONEER-7'

      BLTCOD(197) =   -8
      BLTNAM(197) =  'WIND'

      BLTCOD(198) =   -12
      BLTNAM(198) =  'VENUS ORBITER'

      BLTCOD(199) =   -12
      BLTNAM(199) =  'P12'

      BLTCOD(200) =   -12
      BLTNAM(200) =  'PIONEER 12'

      BLTCOD(201) =   -12
      BLTNAM(201) =  'LADEE'

      BLTCOD(202) =   -13
      BLTNAM(202) =  'POLAR'

      BLTCOD(203) =   -18
      BLTNAM(203) =  'MGN'

      BLTCOD(204) =   -18
      BLTNAM(204) =  'MAGELLAN'

      BLTCOD(205) =   -18
      BLTNAM(205) =  'LCROSS'

      BLTCOD(206) =   -20
      BLTNAM(206) =  'P8'

      BLTCOD(207) =   -20
      BLTNAM(207) =  'PIONEER-8'

      BLTCOD(208) =   -21
      BLTNAM(208) =  'SOHO'

      BLTCOD(209) =   -23
      BLTNAM(209) =  'P10'

      BLTCOD(210) =   -23
      BLTNAM(210) =  'PIONEER-10'

      BLTCOD(211) =   -24
      BLTNAM(211) =  'P11'

      BLTCOD(212) =   -24
      BLTNAM(212) =  'PIONEER-11'

      BLTCOD(213) =   -25
      BLTNAM(213) =  'LP'

      BLTCOD(214) =   -25
      BLTNAM(214) =  'LUNAR PROSPECTOR'

      BLTCOD(215) =   -27
      BLTNAM(215) =  'VK1'

      BLTCOD(216) =   -27
      BLTNAM(216) =  'VIKING 1 ORBITER'

      BLTCOD(217) =   -29
      BLTNAM(217) =  'STARDUST'

      BLTCOD(218) =   -29
      BLTNAM(218) =  'SDU'

      BLTCOD(219) =   -29
      BLTNAM(219) =  'NEXT'

      BLTCOD(220) =   -30
      BLTNAM(220) =  'VK2'

      BLTCOD(221) =   -30
      BLTNAM(221) =  'VIKING 2 ORBITER'

      BLTCOD(222) =   -30
      BLTNAM(222) =  'DS-1'

      BLTCOD(223) =   -31
      BLTNAM(223) =  'VG1'

      BLTCOD(224) =   -31
      BLTNAM(224) =  'VOYAGER 1'

      BLTCOD(225) =   -32
      BLTNAM(225) =  'VG2'

      BLTCOD(226) =   -32
      BLTNAM(226) =  'VOYAGER 2'

      BLTCOD(227) =   -40
      BLTNAM(227) =  'CLEMENTINE'

      BLTCOD(228) =   -41
      BLTNAM(228) =  'MEX'

      BLTCOD(229) =   -41
      BLTNAM(229) =  'MARS EXPRESS'

      BLTCOD(230) =   -44
      BLTNAM(230) =  'BEAGLE2'

      BLTCOD(231) =   -44
      BLTNAM(231) =  'BEAGLE 2'

      BLTCOD(232) =   -46
      BLTNAM(232) =  'MS-T5'

      BLTCOD(233) =   -46
      BLTNAM(233) =  'SAKIGAKE'

      BLTCOD(234) =   -47
      BLTNAM(234) =  'PLANET-A'

      BLTCOD(235) =   -47
      BLTNAM(235) =  'SUISEI'

      BLTCOD(236) =   -47
      BLTNAM(236) =  'GNS'

      BLTCOD(237) =   -47
      BLTNAM(237) =  'GENESIS'

      BLTCOD(238) =   -48
      BLTNAM(238) =  'HUBBLE SPACE TELESCOPE'

      BLTCOD(239) =   -48
      BLTNAM(239) =  'HST'

      BLTCOD(240) =   -53
      BLTNAM(240) =  'MARS PATHFINDER'

      BLTCOD(241) =   -53
      BLTNAM(241) =  'MPF'

      BLTCOD(242) =   -53
      BLTNAM(242) =  'MARS ODYSSEY'

      BLTCOD(243) =   -53
      BLTNAM(243) =  'MARS SURVEYOR 01 ORBITER'

      BLTCOD(244) =   -54
      BLTNAM(244) =  'ARM'

      BLTCOD(245) =   -54
      BLTNAM(245) =  'ASTEROID RETRIEVAL MISSION'

      BLTCOD(246) =   -55
      BLTNAM(246) =  'ULYSSES'

      BLTCOD(247) =   -58
      BLTNAM(247) =  'VSOP'

      BLTCOD(248) =   -58
      BLTNAM(248) =  'HALCA'

      BLTCOD(249) =   -59
      BLTNAM(249) =  'RADIOASTRON'

      BLTCOD(250) =   -61
      BLTNAM(250) =  'JUNO'

      BLTCOD(251) =   -64
      BLTNAM(251) =  'ORX'

      BLTCOD(252) =   -64
      BLTNAM(252) =  'OSIRIS-REX'

      BLTCOD(253) =   -66
      BLTNAM(253) =  'VEGA 1'

      BLTCOD(254) =   -67
      BLTNAM(254) =  'VEGA 2'

      BLTCOD(255) =   -68
      BLTNAM(255) =  'MMO'

      BLTCOD(256) =   -68
      BLTNAM(256) =  'MERCURY MAGNETOSPHERIC ORBITER'

      BLTCOD(257) =   -69
      BLTNAM(257) =  'MPO'

      BLTCOD(258) =   -69
      BLTNAM(258) =  'MERCURY PLANETARY ORBITER'

      BLTCOD(259) =   -70
      BLTNAM(259) =  'DEEP IMPACT IMPACTOR SPACECRAFT'

      BLTCOD(260) =   -74
      BLTNAM(260) =  'MRO'

      BLTCOD(261) =   -74
      BLTNAM(261) =  'MARS RECON ORBITER'

      BLTCOD(262) =   -76
      BLTNAM(262) =  'MSL'

      BLTCOD(263) =   -76
      BLTNAM(263) =  'MARS SCIENCE LABORATORY'

      BLTCOD(264) =   -77
      BLTNAM(264) =  'GLL'

      BLTCOD(265) =   -77
      BLTNAM(265) =  'GALILEO ORBITER'

      BLTCOD(266) =   -78
      BLTNAM(266) =  'GIOTTO'

      BLTCOD(267) =   -79
      BLTNAM(267) =  'SPITZER'

      BLTCOD(268) =   -79
      BLTNAM(268) =  'SPACE INFRARED TELESCOPE FACILITY'

      BLTCOD(269) =   -79
      BLTNAM(269) =  'SIRTF'

      BLTCOD(270) =   -81
      BLTNAM(270) =  'CASSINI ITL'

      BLTCOD(271) =   -82
      BLTNAM(271) =  'CAS'

      BLTCOD(272) =   -82
      BLTNAM(272) =  'CASSINI'

      BLTCOD(273) =   -84
      BLTNAM(273) =  'PHOENIX'

      BLTCOD(274) =   -85
      BLTNAM(274) =  'LRO'

      BLTCOD(275) =   -85
      BLTNAM(275) =  'LUNAR RECON ORBITER'

      BLTCOD(276) =   -85
      BLTNAM(276) =  'LUNAR RECONNAISSANCE ORBITER'

      BLTCOD(277) =   -86
      BLTNAM(277) =  'CH1'

      BLTCOD(278) =   -86
      BLTNAM(278) =  'CHANDRAYAAN-1'

      BLTCOD(279) =   -90
      BLTNAM(279) =  'CASSINI SIMULATION'

      BLTCOD(280) =   -93
      BLTNAM(280) =  'NEAR EARTH ASTEROID RENDEZVOUS'

      BLTCOD(281) =   -93
      BLTNAM(281) =  'NEAR'

      BLTCOD(282) =   -94
      BLTNAM(282) =  'MO'

      BLTCOD(283) =   -94
      BLTNAM(283) =  'MARS OBSERVER'

      BLTCOD(284) =   -94
      BLTNAM(284) =  'MGS'

      BLTCOD(285) =   -94
      BLTNAM(285) =  'MARS GLOBAL SURVEYOR'

      BLTCOD(286) =   -95
      BLTNAM(286) =  'MGS SIMULATION'

      BLTCOD(287) =   -96
      BLTNAM(287) =  'SPP'

      BLTCOD(288) =   -96
      BLTNAM(288) =  'SOLAR PROBE PLUS'

      BLTCOD(289) =   -97
      BLTNAM(289) =  'TOPEX/POSEIDON'

      BLTCOD(290) =   -98
      BLTNAM(290) =  'NEW HORIZONS'

      BLTCOD(291) =   -107
      BLTNAM(291) =  'TROPICAL RAINFALL MEASURING MISSION'

      BLTCOD(292) =   -107
      BLTNAM(292) =  'TRMM'

      BLTCOD(293) =   -112
      BLTNAM(293) =  'ICE'

      BLTCOD(294) =   -116
      BLTNAM(294) =  'MARS POLAR LANDER'

      BLTCOD(295) =   -116
      BLTNAM(295) =  'MPL'

      BLTCOD(296) =   -121
      BLTNAM(296) =  'BEPICOLOMBO'

      BLTCOD(297) =   -127
      BLTNAM(297) =  'MARS CLIMATE ORBITER'

      BLTCOD(298) =   -127
      BLTNAM(298) =  'MCO'

      BLTCOD(299) =   -130
      BLTNAM(299) =  'MUSES-C'

      BLTCOD(300) =   -130
      BLTNAM(300) =  'HAYABUSA'

      BLTCOD(301) =   -131
      BLTNAM(301) =  'SELENE'

      BLTCOD(302) =   -131
      BLTNAM(302) =  'KAGUYA'

      BLTCOD(303) =   -135
      BLTNAM(303) =  'DRTS-W'

      BLTCOD(304) =   -140
      BLTNAM(304) =  'EPOCH'

      BLTCOD(305) =   -140
      BLTNAM(305) =  'DIXI'

      BLTCOD(306) =   -140
      BLTNAM(306) =  'EPOXI'

      BLTCOD(307) =   -140
      BLTNAM(307) =  'DEEP IMPACT FLYBY SPACECRAFT'

      BLTCOD(308) =   -142
      BLTNAM(308) =  'TERRA'

      BLTCOD(309) =   -142
      BLTNAM(309) =  'EOS-AM1'

      BLTCOD(310) =   -144
      BLTNAM(310) =  'SOLO'

      BLTCOD(311) =   -144
      BLTNAM(311) =  'SOLAR ORBITER'

      BLTCOD(312) =   -146
      BLTNAM(312) =  'LUNAR-A'

      BLTCOD(313) =   -150
      BLTNAM(313) =  'CASSINI PROBE'

      BLTCOD(314) =   -150
      BLTNAM(314) =  'HUYGENS PROBE'

      BLTCOD(315) =   -150
      BLTNAM(315) =  'CASP'

      BLTCOD(316) =   -151
      BLTNAM(316) =  'AXAF'

      BLTCOD(317) =   -151
      BLTNAM(317) =  'CHANDRA'

      BLTCOD(318) =   -154
      BLTNAM(318) =  'AQUA'

      BLTCOD(319) =   -159
      BLTNAM(319) =  'EUROPA ORBITER'

      BLTCOD(320) =   -164
      BLTNAM(320) =  'YOHKOH'

      BLTCOD(321) =   -164
      BLTNAM(321) =  'SOLAR-A'

      BLTCOD(322) =   -165
      BLTNAM(322) =  'MAP'

      BLTCOD(323) =   -166
      BLTNAM(323) =  'IMAGE'

      BLTCOD(324) =   -170
      BLTNAM(324) =  'JWST'

      BLTCOD(325) =   -170
      BLTNAM(325) =  'JAMES WEBB SPACE TELESCOPE'

      BLTCOD(326) =   -177
      BLTNAM(326) =  'GRAIL-A'

      BLTCOD(327) =   -178
      BLTNAM(327) =  'PLANET-B'

      BLTCOD(328) =   -178
      BLTNAM(328) =  'NOZOMI'

      BLTCOD(329) =   -181
      BLTNAM(329) =  'GRAIL-B'

      BLTCOD(330) =   -183
      BLTNAM(330) =  'CLUSTER 1'

      BLTCOD(331) =   -185
      BLTNAM(331) =  'CLUSTER 2'

      BLTCOD(332) =   -188
      BLTNAM(332) =  'MUSES-B'

      BLTCOD(333) =   -189
      BLTNAM(333) =  'NSYT'

      BLTCOD(334) =   -189
      BLTNAM(334) =  'INSIGHT'

      BLTCOD(335) =   -190
      BLTNAM(335) =  'SIM'

      BLTCOD(336) =   -194
      BLTNAM(336) =  'CLUSTER 3'

      BLTCOD(337) =   -196
      BLTNAM(337) =  'CLUSTER 4'

      BLTCOD(338) =   -198
      BLTNAM(338) =  'INTEGRAL'

      BLTCOD(339) =   -200
      BLTNAM(339) =  'CONTOUR'

      BLTCOD(340) =   -202
      BLTNAM(340) =  'MAVEN'

      BLTCOD(341) =   -203
      BLTNAM(341) =  'DAWN'

      BLTCOD(342) =   -205
      BLTNAM(342) =  'SOIL MOISTURE ACTIVE AND PASSIVE'

      BLTCOD(343) =   -205
      BLTNAM(343) =  'SMAP'

      BLTCOD(344) =   -212
      BLTNAM(344) =  'STV51'

      BLTCOD(345) =   -213
      BLTNAM(345) =  'STV52'

      BLTCOD(346) =   -214
      BLTNAM(346) =  'STV53'

      BLTCOD(347) =   -226
      BLTNAM(347) =  'ROSETTA'

      BLTCOD(348) =   -227
      BLTNAM(348) =  'KEPLER'

      BLTCOD(349) =   -228
      BLTNAM(349) =  'GLL PROBE'

      BLTCOD(350) =   -228
      BLTNAM(350) =  'GALILEO PROBE'

      BLTCOD(351) =   -234
      BLTNAM(351) =  'STEREO AHEAD'

      BLTCOD(352) =   -235
      BLTNAM(352) =  'STEREO BEHIND'

      BLTCOD(353) =   -236
      BLTNAM(353) =  'MESSENGER'

      BLTCOD(354) =   -238
      BLTNAM(354) =  'SMART1'

      BLTCOD(355) =   -238
      BLTNAM(355) =  'SM1'

      BLTCOD(356) =   -238
      BLTNAM(356) =  'S1'

      BLTCOD(357) =   -238
      BLTNAM(357) =  'SMART-1'

      BLTCOD(358) =   -248
      BLTNAM(358) =  'VEX'

      BLTCOD(359) =   -248
      BLTNAM(359) =  'VENUS EXPRESS'

      BLTCOD(360) =   -253
      BLTNAM(360) =  'OPPORTUNITY'

      BLTCOD(361) =   -253
      BLTNAM(361) =  'MER-1'

      BLTCOD(362) =   -254
      BLTNAM(362) =  'SPIRIT'

      BLTCOD(363) =   -254
      BLTNAM(363) =  'MER-2'

      BLTCOD(364) =   -362
      BLTNAM(364) =  'RADIATION BELT STORM PROBE A'

      BLTCOD(365) =   -362
      BLTNAM(365) =  'RBSP_A'

      BLTCOD(366) =   -363
      BLTNAM(366) =  'RADIATION BELT STORM PROBE B'

      BLTCOD(367) =   -363
      BLTNAM(367) =  'RBSP_B'

      BLTCOD(368) =   -500
      BLTNAM(368) =  'RSAT'

      BLTCOD(369) =   -500
      BLTNAM(369) =  'SELENE Relay Satellite'

      BLTCOD(370) =   -500
      BLTNAM(370) =  'SELENE Rstar'

      BLTCOD(371) =   -500
      BLTNAM(371) =  'Rstar'

      BLTCOD(372) =   -502
      BLTNAM(372) =  'VSAT'

      BLTCOD(373) =   -502
      BLTNAM(373) =  'SELENE VLBI Radio Satellite'

      BLTCOD(374) =   -502
      BLTNAM(374) =  'SELENE VRAD Satellite'

      BLTCOD(375) =   -502
      BLTNAM(375) =  'SELENE Vstar'

      BLTCOD(376) =   -502
      BLTNAM(376) =  'Vstar'

      BLTCOD(377) =   -550
      BLTNAM(377) =  'MARS-96'

      BLTCOD(378) =   -550
      BLTNAM(378) =  'M96'

      BLTCOD(379) =   -550
      BLTNAM(379) =  'MARS 96'

      BLTCOD(380) =   -550
      BLTNAM(380) =  'MARS96'

      BLTCOD(381) =   -750
      BLTNAM(381) =  'SPRINT-A'

      BLTCOD(382) =   50000001
      BLTNAM(382) =  'SHOEMAKER-LEVY 9-W'

      BLTCOD(383) =   50000002
      BLTNAM(383) =  'SHOEMAKER-LEVY 9-V'

      BLTCOD(384) =   50000003
      BLTNAM(384) =  'SHOEMAKER-LEVY 9-U'

      BLTCOD(385) =   50000004
      BLTNAM(385) =  'SHOEMAKER-LEVY 9-T'

      BLTCOD(386) =   50000005
      BLTNAM(386) =  'SHOEMAKER-LEVY 9-S'

      BLTCOD(387) =   50000006
      BLTNAM(387) =  'SHOEMAKER-LEVY 9-R'

      BLTCOD(388) =   50000007
      BLTNAM(388) =  'SHOEMAKER-LEVY 9-Q'

      BLTCOD(389) =   50000008
      BLTNAM(389) =  'SHOEMAKER-LEVY 9-P'

      BLTCOD(390) =   50000009
      BLTNAM(390) =  'SHOEMAKER-LEVY 9-N'

      BLTCOD(391) =   50000010
      BLTNAM(391) =  'SHOEMAKER-LEVY 9-M'

      BLTCOD(392) =   50000011
      BLTNAM(392) =  'SHOEMAKER-LEVY 9-L'

      BLTCOD(393) =   50000012
      BLTNAM(393) =  'SHOEMAKER-LEVY 9-K'

      BLTCOD(394) =   50000013
      BLTNAM(394) =  'SHOEMAKER-LEVY 9-J'

      BLTCOD(395) =   50000014
      BLTNAM(395) =  'SHOEMAKER-LEVY 9-H'

      BLTCOD(396) =   50000015
      BLTNAM(396) =  'SHOEMAKER-LEVY 9-G'

      BLTCOD(397) =   50000016
      BLTNAM(397) =  'SHOEMAKER-LEVY 9-F'

      BLTCOD(398) =   50000017
      BLTNAM(398) =  'SHOEMAKER-LEVY 9-E'

      BLTCOD(399) =   50000018
      BLTNAM(399) =  'SHOEMAKER-LEVY 9-D'

      BLTCOD(400) =   50000019
      BLTNAM(400) =  'SHOEMAKER-LEVY 9-C'

      BLTCOD(401) =   50000020
      BLTNAM(401) =  'SHOEMAKER-LEVY 9-B'

      BLTCOD(402) =   50000021
      BLTNAM(402) =  'SHOEMAKER-LEVY 9-A'

      BLTCOD(403) =   50000022
      BLTNAM(403) =  'SHOEMAKER-LEVY 9-Q1'

      BLTCOD(404) =   50000023
      BLTNAM(404) =  'SHOEMAKER-LEVY 9-P2'

      BLTCOD(405) =   1000001
      BLTNAM(405) =  'AREND'

      BLTCOD(406) =   1000002
      BLTNAM(406) =  'AREND-RIGAUX'

      BLTCOD(407) =   1000003
      BLTNAM(407) =  'ASHBROOK-JACKSON'

      BLTCOD(408) =   1000004
      BLTNAM(408) =  'BOETHIN'

      BLTCOD(409) =   1000005
      BLTNAM(409) =  'BORRELLY'

      BLTCOD(410) =   1000006
      BLTNAM(410) =  'BOWELL-SKIFF'

      BLTCOD(411) =   1000007
      BLTNAM(411) =  'BRADFIELD'

      BLTCOD(412) =   1000008
      BLTNAM(412) =  'BROOKS 2'

      BLTCOD(413) =   1000009
      BLTNAM(413) =  'BRORSEN-METCALF'

      BLTCOD(414) =   1000010
      BLTNAM(414) =  'BUS'

      BLTCOD(415) =   1000011
      BLTNAM(415) =  'CHERNYKH'

      BLTCOD(416) =   1000012
      BLTNAM(416) =  '67P/CHURYUMOV-GERASIMENKO (1969 R1)'

      BLTCOD(417) =   1000012
      BLTNAM(417) =  'CHURYUMOV-GERASIMENKO'

      BLTCOD(418) =   1000013
      BLTNAM(418) =  'CIFFREO'

      BLTCOD(419) =   1000014
      BLTNAM(419) =  'CLARK'

      BLTCOD(420) =   1000015
      BLTNAM(420) =  'COMAS SOLA'

      BLTCOD(421) =   1000016
      BLTNAM(421) =  'CROMMELIN'

      BLTCOD(422) =   1000017
      BLTNAM(422) =  'D''ARREST'

      BLTCOD(423) =   1000018
      BLTNAM(423) =  'DANIEL'

      BLTCOD(424) =   1000019
      BLTNAM(424) =  'DE VICO-SWIFT'

      BLTCOD(425) =   1000020
      BLTNAM(425) =  'DENNING-FUJIKAWA'

      BLTCOD(426) =   1000021
      BLTNAM(426) =  'DU TOIT 1'

      BLTCOD(427) =   1000022
      BLTNAM(427) =  'DU TOIT-HARTLEY'

      BLTCOD(428) =   1000023
      BLTNAM(428) =  'DUTOIT-NEUJMIN-DELPORTE'

      BLTCOD(429) =   1000024
      BLTNAM(429) =  'DUBIAGO'

      BLTCOD(430) =   1000025
      BLTNAM(430) =  'ENCKE'

      BLTCOD(431) =   1000026
      BLTNAM(431) =  'FAYE'

      BLTCOD(432) =   1000027
      BLTNAM(432) =  'FINLAY'

      BLTCOD(433) =   1000028
      BLTNAM(433) =  'FORBES'

      BLTCOD(434) =   1000029
      BLTNAM(434) =  'GEHRELS 1'

      BLTCOD(435) =   1000030
      BLTNAM(435) =  'GEHRELS 2'

      BLTCOD(436) =   1000031
      BLTNAM(436) =  'GEHRELS 3'

      BLTCOD(437) =   1000032
      BLTNAM(437) =  'GIACOBINI-ZINNER'

      BLTCOD(438) =   1000033
      BLTNAM(438) =  'GICLAS'

      BLTCOD(439) =   1000034
      BLTNAM(439) =  'GRIGG-SKJELLERUP'

      BLTCOD(440) =   1000035
      BLTNAM(440) =  'GUNN'

      BLTCOD(441) =   1000036
      BLTNAM(441) =  'HALLEY'

      BLTCOD(442) =   1000037
      BLTNAM(442) =  'HANEDA-CAMPOS'

      BLTCOD(443) =   1000038
      BLTNAM(443) =  'HARRINGTON'

      BLTCOD(444) =   1000039
      BLTNAM(444) =  'HARRINGTON-ABELL'

      BLTCOD(445) =   1000040
      BLTNAM(445) =  'HARTLEY 1'

      BLTCOD(446) =   1000041
      BLTNAM(446) =  'HARTLEY 2'

      BLTCOD(447) =   1000042
      BLTNAM(447) =  'HARTLEY-IRAS'

      BLTCOD(448) =   1000043
      BLTNAM(448) =  'HERSCHEL-RIGOLLET'

      BLTCOD(449) =   1000044
      BLTNAM(449) =  'HOLMES'

      BLTCOD(450) =   1000045
      BLTNAM(450) =  'HONDA-MRKOS-PAJDUSAKOVA'

      BLTCOD(451) =   1000046
      BLTNAM(451) =  'HOWELL'

      BLTCOD(452) =   1000047
      BLTNAM(452) =  'IRAS'

      BLTCOD(453) =   1000048
      BLTNAM(453) =  'JACKSON-NEUJMIN'

      BLTCOD(454) =   1000049
      BLTNAM(454) =  'JOHNSON'

      BLTCOD(455) =   1000050
      BLTNAM(455) =  'KEARNS-KWEE'

      BLTCOD(456) =   1000051
      BLTNAM(456) =  'KLEMOLA'

      BLTCOD(457) =   1000052
      BLTNAM(457) =  'KOHOUTEK'

      BLTCOD(458) =   1000053
      BLTNAM(458) =  'KOJIMA'

      BLTCOD(459) =   1000054
      BLTNAM(459) =  'KOPFF'

      BLTCOD(460) =   1000055
      BLTNAM(460) =  'KOWAL 1'

      BLTCOD(461) =   1000056
      BLTNAM(461) =  'KOWAL 2'

      BLTCOD(462) =   1000057
      BLTNAM(462) =  'KOWAL-MRKOS'

      BLTCOD(463) =   1000058
      BLTNAM(463) =  'KOWAL-VAVROVA'

      BLTCOD(464) =   1000059
      BLTNAM(464) =  'LONGMORE'

      BLTCOD(465) =   1000060
      BLTNAM(465) =  'LOVAS 1'

      BLTCOD(466) =   1000061
      BLTNAM(466) =  'MACHHOLZ'

      BLTCOD(467) =   1000062
      BLTNAM(467) =  'MAURY'

      BLTCOD(468) =   1000063
      BLTNAM(468) =  'NEUJMIN 1'

      BLTCOD(469) =   1000064
      BLTNAM(469) =  'NEUJMIN 2'

      BLTCOD(470) =   1000065
      BLTNAM(470) =  'NEUJMIN 3'

      BLTCOD(471) =   1000066
      BLTNAM(471) =  'OLBERS'

      BLTCOD(472) =   1000067
      BLTNAM(472) =  'PETERS-HARTLEY'

      BLTCOD(473) =   1000068
      BLTNAM(473) =  'PONS-BROOKS'

      BLTCOD(474) =   1000069
      BLTNAM(474) =  'PONS-WINNECKE'

      BLTCOD(475) =   1000070
      BLTNAM(475) =  'REINMUTH 1'

      BLTCOD(476) =   1000071
      BLTNAM(476) =  'REINMUTH 2'

      BLTCOD(477) =   1000072
      BLTNAM(477) =  'RUSSELL 1'

      BLTCOD(478) =   1000073
      BLTNAM(478) =  'RUSSELL 2'

      BLTCOD(479) =   1000074
      BLTNAM(479) =  'RUSSELL 3'

      BLTCOD(480) =   1000075
      BLTNAM(480) =  'RUSSELL 4'

      BLTCOD(481) =   1000076
      BLTNAM(481) =  'SANGUIN'

      BLTCOD(482) =   1000077
      BLTNAM(482) =  'SCHAUMASSE'

      BLTCOD(483) =   1000078
      BLTNAM(483) =  'SCHUSTER'

      BLTCOD(484) =   1000079
      BLTNAM(484) =  'SCHWASSMANN-WACHMANN 1'

      BLTCOD(485) =   1000080
      BLTNAM(485) =  'SCHWASSMANN-WACHMANN 2'

      BLTCOD(486) =   1000081
      BLTNAM(486) =  'SCHWASSMANN-WACHMANN 3'

      BLTCOD(487) =   1000082
      BLTNAM(487) =  'SHAJN-SCHALDACH'

      BLTCOD(488) =   1000083
      BLTNAM(488) =  'SHOEMAKER 1'

      BLTCOD(489) =   1000084
      BLTNAM(489) =  'SHOEMAKER 2'

      BLTCOD(490) =   1000085
      BLTNAM(490) =  'SHOEMAKER 3'

      BLTCOD(491) =   1000086
      BLTNAM(491) =  'SINGER-BREWSTER'

      BLTCOD(492) =   1000087
      BLTNAM(492) =  'SLAUGHTER-BURNHAM'

      BLTCOD(493) =   1000088
      BLTNAM(493) =  'SMIRNOVA-CHERNYKH'

      BLTCOD(494) =   1000089
      BLTNAM(494) =  'STEPHAN-OTERMA'

      BLTCOD(495) =   1000090
      BLTNAM(495) =  'SWIFT-GEHRELS'

      BLTCOD(496) =   1000091
      BLTNAM(496) =  'TAKAMIZAWA'

      BLTCOD(497) =   1000092
      BLTNAM(497) =  'TAYLOR'

      BLTCOD(498) =   1000093
      BLTNAM(498) =  'TEMPEL_1'

      BLTCOD(499) =   1000093
      BLTNAM(499) =  'TEMPEL 1'

      BLTCOD(500) =   1000094
      BLTNAM(500) =  'TEMPEL 2'

      BLTCOD(501) =   1000095
      BLTNAM(501) =  'TEMPEL-TUTTLE'

      BLTCOD(502) =   1000096
      BLTNAM(502) =  'TRITTON'

      BLTCOD(503) =   1000097
      BLTNAM(503) =  'TSUCHINSHAN 1'

      BLTCOD(504) =   1000098
      BLTNAM(504) =  'TSUCHINSHAN 2'

      BLTCOD(505) =   1000099
      BLTNAM(505) =  'TUTTLE'

      BLTCOD(506) =   1000100
      BLTNAM(506) =  'TUTTLE-GIACOBINI-KRESAK'

      BLTCOD(507) =   1000101
      BLTNAM(507) =  'VAISALA 1'

      BLTCOD(508) =   1000102
      BLTNAM(508) =  'VAN BIESBROECK'

      BLTCOD(509) =   1000103
      BLTNAM(509) =  'VAN HOUTEN'

      BLTCOD(510) =   1000104
      BLTNAM(510) =  'WEST-KOHOUTEK-IKEMURA'

      BLTCOD(511) =   1000105
      BLTNAM(511) =  'WHIPPLE'

      BLTCOD(512) =   1000106
      BLTNAM(512) =  'WILD 1'

      BLTCOD(513) =   1000107
      BLTNAM(513) =  'WILD 2'

      BLTCOD(514) =   1000108
      BLTNAM(514) =  'WILD 3'

      BLTCOD(515) =   1000109
      BLTNAM(515) =  'WIRTANEN'

      BLTCOD(516) =   1000110
      BLTNAM(516) =  'WOLF'

      BLTCOD(517) =   1000111
      BLTNAM(517) =  'WOLF-HARRINGTON'

      BLTCOD(518) =   1000112
      BLTNAM(518) =  'LOVAS 2'

      BLTCOD(519) =   1000113
      BLTNAM(519) =  'URATA-NIIJIMA'

      BLTCOD(520) =   1000114
      BLTNAM(520) =  'WISEMAN-SKIFF'

      BLTCOD(521) =   1000115
      BLTNAM(521) =  'HELIN'

      BLTCOD(522) =   1000116
      BLTNAM(522) =  'MUELLER'

      BLTCOD(523) =   1000117
      BLTNAM(523) =  'SHOEMAKER-HOLT 1'

      BLTCOD(524) =   1000118
      BLTNAM(524) =  'HELIN-ROMAN-CROCKETT'

      BLTCOD(525) =   1000119
      BLTNAM(525) =  'HARTLEY 3'

      BLTCOD(526) =   1000120
      BLTNAM(526) =  'PARKER-HARTLEY'

      BLTCOD(527) =   1000121
      BLTNAM(527) =  'HELIN-ROMAN-ALU 1'

      BLTCOD(528) =   1000122
      BLTNAM(528) =  'WILD 4'

      BLTCOD(529) =   1000123
      BLTNAM(529) =  'MUELLER 2'

      BLTCOD(530) =   1000124
      BLTNAM(530) =  'MUELLER 3'

      BLTCOD(531) =   1000125
      BLTNAM(531) =  'SHOEMAKER-LEVY 1'

      BLTCOD(532) =   1000126
      BLTNAM(532) =  'SHOEMAKER-LEVY 2'

      BLTCOD(533) =   1000127
      BLTNAM(533) =  'HOLT-OLMSTEAD'

      BLTCOD(534) =   1000128
      BLTNAM(534) =  'METCALF-BREWINGTON'

      BLTCOD(535) =   1000129
      BLTNAM(535) =  'LEVY'

      BLTCOD(536) =   1000130
      BLTNAM(536) =  'SHOEMAKER-LEVY 9'

      BLTCOD(537) =   1000131
      BLTNAM(537) =  'HYAKUTAKE'

      BLTCOD(538) =   1000132
      BLTNAM(538) =  'HALE-BOPP'

      BLTCOD(539) =   1003228
      BLTNAM(539) =  'C/2013 A1'

      BLTCOD(540) =   1003228
      BLTNAM(540) =  'SIDING SPRING'

      BLTCOD(541) =   9511010
      BLTNAM(541) =  'GASPRA'

      BLTCOD(542) =   2431010
      BLTNAM(542) =  'IDA'

      BLTCOD(543) =   2431011
      BLTNAM(543) =  'DACTYL'

      BLTCOD(544) =   2000001
      BLTNAM(544) =  'CERES'

      BLTCOD(545) =   2000002
      BLTNAM(545) =  'PALLAS'

      BLTCOD(546) =   2000004
      BLTNAM(546) =  'VESTA'

      BLTCOD(547) =   2000021
      BLTNAM(547) =  'LUTETIA'

      BLTCOD(548) =   2000216
      BLTNAM(548) =  'KLEOPATRA'

      BLTCOD(549) =   2000433
      BLTNAM(549) =  'EROS'

      BLTCOD(550) =   2000511
      BLTNAM(550) =  'DAVIDA'

      BLTCOD(551) =   2000253
      BLTNAM(551) =  'MATHILDE'

      BLTCOD(552) =   2002867
      BLTNAM(552) =  'STEINS'

      BLTCOD(553) =   2009969
      BLTNAM(553) =  '1992KD'

      BLTCOD(554) =   2009969
      BLTNAM(554) =  'BRAILLE'

      BLTCOD(555) =   2004015
      BLTNAM(555) =  'WILSON-HARRINGTON'

      BLTCOD(556) =   2004179
      BLTNAM(556) =  'TOUTATIS'

      BLTCOD(557) =   2025143
      BLTNAM(557) =  'ITOKAWA'

      BLTCOD(558) =   398989
      BLTNAM(558) =  'NOTO'

      BLTCOD(559) =   398990
      BLTNAM(559) =  'NEW NORCIA'

      BLTCOD(560) =   399001
      BLTNAM(560) =  'GOLDSTONE'

      BLTCOD(561) =   399002
      BLTNAM(561) =  'CANBERRA'

      BLTCOD(562) =   399003
      BLTNAM(562) =  'MADRID'

      BLTCOD(563) =   399004
      BLTNAM(563) =  'USUDA'

      BLTCOD(564) =   399005
      BLTNAM(564) =  'DSS-05'

      BLTCOD(565) =   399005
      BLTNAM(565) =  'PARKES'

      BLTCOD(566) =   399012
      BLTNAM(566) =  'DSS-12'

      BLTCOD(567) =   399013
      BLTNAM(567) =  'DSS-13'

      BLTCOD(568) =   399014
      BLTNAM(568) =  'DSS-14'

      BLTCOD(569) =   399015
      BLTNAM(569) =  'DSS-15'

      BLTCOD(570) =   399016
      BLTNAM(570) =  'DSS-16'

      BLTCOD(571) =   399017
      BLTNAM(571) =  'DSS-17'

      BLTCOD(572) =   399023
      BLTNAM(572) =  'DSS-23'

      BLTCOD(573) =   399024
      BLTNAM(573) =  'DSS-24'

      BLTCOD(574) =   399025
      BLTNAM(574) =  'DSS-25'

      BLTCOD(575) =   399026
      BLTNAM(575) =  'DSS-26'

      BLTCOD(576) =   399027
      BLTNAM(576) =  'DSS-27'

      BLTCOD(577) =   399028
      BLTNAM(577) =  'DSS-28'

      BLTCOD(578) =   399033
      BLTNAM(578) =  'DSS-33'

      BLTCOD(579) =   399034
      BLTNAM(579) =  'DSS-34'

      BLTCOD(580) =   399042
      BLTNAM(580) =  'DSS-42'

      BLTCOD(581) =   399043
      BLTNAM(581) =  'DSS-43'

      BLTCOD(582) =   399045
      BLTNAM(582) =  'DSS-45'

      BLTCOD(583) =   399046
      BLTNAM(583) =  'DSS-46'

      BLTCOD(584) =   399049
      BLTNAM(584) =  'DSS-49'

      BLTCOD(585) =   399053
      BLTNAM(585) =  'DSS-53'

      BLTCOD(586) =   399054
      BLTNAM(586) =  'DSS-54'

      BLTCOD(587) =   399055
      BLTNAM(587) =  'DSS-55'

      BLTCOD(588) =   399061
      BLTNAM(588) =  'DSS-61'

      BLTCOD(589) =   399063
      BLTNAM(589) =  'DSS-63'

      BLTCOD(590) =   399064
      BLTNAM(590) =  'DSS-64'

      BLTCOD(591) =   399065
      BLTNAM(591) =  'DSS-65'

      BLTCOD(592) =   399066
      BLTNAM(592) =  'DSS-66'



      RETURN
      END

