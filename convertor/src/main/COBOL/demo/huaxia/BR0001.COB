001280******************************************************************
001290*   (C) COPYRIGHT FINANCIAL NETWORK SERVICES PTY LTD. 1982       *
001300*       ALL RIGHTS RESERVED. NO PART OF THIS PROGRAM MAY         *
001310*       BE PHOTOCOPIED, REPRODUCED, TRANSLATED TO ANOTHER        *
001320*       PROGRAM LANGUAGE OR USED IN ANY WAY WITHOUT THE          *
001330*       PRIOR WRITTEN CONSENT OF FINANCIAL NETWORK SERVICES      *
001340*       OR THE AUTHORISED SELLING AGENT.                         *
001350*----------------------------------------------------------------*
001360*             FINANCIAL NETWORK SERVICES PTY LTD.                *
001370*                 UNIT 6/70 ROSEHILL STREET                      *
001380*                     REDFERN, AUSTRALIA                         *
001390*                           2016.                                *
001400*                      TEL. 02-3181088                           *
001410*                      FAX. 02-3190008                           *
001420******************************************************************
001430******************************************************************
001440*                                                                *
001450***                                                            ***
001460***  SYSTEM:-   LOANS PRIMARY UPDATE                           ***
001470***                                                            ***
001480***  MODULE:-   BR0000                                         ***
001490***                                                            ***
001500***  PROGRAM:-  BR0001                                         ***
001510***                                                            ***
TB0169***  NOTE:-     THIS PROGRAM CALLS UT0001 FOR TIB LOAN ACCOUNT ***
TB0169***             CREATION / AMEND                               ***
TB0169***                                                            ***
001550***  PURPOSE:-  THIS PROGRAM CREATES NEW CUSTOMERS             ***
001560***              RECORDS AND HAS THE FACILITY TO               ***
001570***              CHANGE THE DETAILS OF OLD                     ***
001580***              RECORDS                                       ***
001590***             FOR CREATING NEW RECORDS                       ***
001610***             FOR MODIFYING OLD RECORDS                      ***
001620***               TRN-NO :- 17001 17002            ***
001630***             FOR MODIFYING SECURITY ADDRESS                 ***
001640***               TRN-NO :- 17450 17451                        ***
001650***             FOR ADD DELETE MODIFY ACCOUNT NAMES            ***
001660***             LONG & SHORT     TRN-NO :- 17465               ***
001670******************************************************************
001680*                 P R O G R A M    H I S T O R Y
001690*  PROGRAMMERS INITIALS TOGETHER WITH THE "SPR" NUMBER MUST BE
001700*  REFLECTED IN COLUMNS 73-80 OF EVERY LINE/S CHANGED OR ADDED.
001710*
001720* PROGRAMMER : DATE :SPR NO :COMMENTS
COR003*----------------------------------------------------------------
COR003* Xu Guangxian:03012007:COR003:FIELDS WILL BE EXPANDED
COR003*----------------------------------------------------------------
S10099* Jacky Li    :20070417:TLON009-01 : FOR MM&DCF PROVISION
S10099*-----------------------------------------------------------------
S10093* Claire Xu   :20070313 :TLON013-01:added new functions to PL/ NPL
S10093*                                    adjustment processing
S10093*-----------------------------------------------------------------
S10046* VIVI WEI   :150507:S10046:FIELDS EXPANSION
S10046*-----------------------------------------------------------------
S10113* Li Zhipeng  :20070323: FIX FOR TRF FROM ACCOUNT SYSTEM FIELD
S10113*                      : IN SCREEN TRXN 010915
S10113*-----------------------------------------------------------------
S10169* W WILLIAM   :07062007:MVSWLW: FIX MVS COMPILE ERROR
S10175*-----------------------------------------------------------------
S10052*
S10052* SPECS 3-1 , BOC DON'T NEED TXN 12008 , CANCEL THIS TXN CODE.
S10052*
S10052*-----------------------------------------------------------------
S10108* Claire Xu : 11062007 :TLN056-01: ACCOUNT MAINTENANCE & VOUCHER
S10108*                                PRINTING
S10108*-----------------------------------------------------------------
S10113* JAKSON XU:  27062007 : S10113  : FIX FOR BASE PROBLEM
S10113*-----------------------------------------------------------------
S10258*  SONG JZ   :16062007:S10258   : LON TEAM FOR ID EXPANSION
S10258*----------------------------------------------------------------
R10100* R AGUILAR  :310507:R10100 : TLON005-3 INTEREST CALCULATION
R10100*            :      :       : DO NOT ALLOW BACKDATED INT START DAT
R10100*            :      :       : AND VALIDATE FLOAT CYCLE AGAINST FIX
R10100*            :      :       : RATE DETAILS
R10100*            :050607:R10100 : TLON005-3 ADD FLOAT CYCLE FIELDS ON
R10100*            :      :       : SCREEN 17001.
R10100*-----------------------------------------------------------------
S10538* Shelly Z.  :110707:S10538:move cusm no when call UT0500 for TS13
S10538*-----------------------------------------------------------------
S11194* Claire Xu  :27072007:S11194:EXPANSION FOR STARAREA
S11194*-----------------------------------------------------------------
S11261* Shelly Z.  :270707:S11261:Change the call code to '05' when call
S11261*            :      :      :UT0400.
S11261*-----------------------------------------------------------------
S10211* BRIAN YANG :200807 :S10211: INTERFACE CHANGED OF MI0090&MI0095
S10211*-----------------------------------------------------------------
S12173* SHELLY ZHANG:22082007:S12173:fix base version issue
S12173*-----------------------------------------------------------------
S12209* SHELLY ZHANG:24082007:S12209:fix IR 20070808-006
S12209*-----------------------------------------------------------------
S12311* ZHUDI: 20070831: S12311: FIX THE ABEND OF UT0073
S12311*-----------------------------------------------------------------
S12576* SHELLY ZHANG:190907:S12576:FIX IR 20070915-017
S12576*-----------------------------------------------------------------
S12692* BERVE SON   :270907:S12692:INITIALIZE VARIABLES CAUSING
S12692*                           :ABDENS IN MVS.
S12692*-----------------------------------------------------------------
S12693* XIAOBO ZHANG:270907:S12693:FIX IR MISCELLANEOUS-20070814-101
S12693*-----------------------------------------------------------------
S12727* GUANGXIAN   :290907:S12727:FIX REPORT TEAM IR 20070815-004
S12727*-----------------------------------------------------------------
S12763* MANISH R    :021007:S12763:FIX IR 20070920-021
S12763*-----------------------------------------------------------------
S12772* GUANGXIAN   :031007:S12772:FIX IR 20070828-005
S12772*-----------------------------------------------------------------
S12802* BERVE SON   :121007:S12802:LOAN EXTENSION.
S12802*-----------------------------------------------------------------
S12997* MANISH R    :201007:S12997:FIX NPL TO PL CONV IR
S12997*-----------------------------------------------------------------
S12695*CHINTA SASTRY:19102007:S12695:EDIT MASK LENGTH ADJUSTMENT
S12695*---------------------------------------------------------
S12787* SHELLY ZHANG:231007:S12787: ADD PL/NPL PROCESS
S12787*-----------------------------------------------------------------
S13026* MANISH R    :231007:S13026:FIX ABEND ISSUE
S13026*-----------------------------------------------------------------
S13064*  ZYM       :26102007:S13064: CHANGE BORP-PRIORITY TO
S13064*                                     BOIS-PRIORITY
S13064*-----------------------------------------------------------------
S13252* MANISH R    :151107:S13252: REMOVE VALIDATION OF PRIORITY
S13252*                             CODE
S13252*-----------------------------------------------------------------
S13203* Shelly Zhang:121107:S13203:for Product and Branch Change
S13203*-----------------------------------------------------------------
S13300* MANISH R    :211107:S13300:ADD VALIDATION FOR NPL PRODUCTS
S13300*-----------------------------------------------------------------
S13624* Shelly Zhang:160108:S13624:CHANGE VALIDATION FOR PRODUCT CHANGE
S13624*-----------------------------------------------------------------
S13645* Shelly Zhang:190108:S13645:FIX IR 20080117-001
S13645*-----------------------------------------------------------------
S13668* MANISH :CHANGES FOR PRODUCT BRANCH CHANGE
S13668*-----------------------------------------------------------------
S13708* SONG JZ     :21JAN08: S13708:LOAN RESTRUCTION
S13708*-----------------------------------------------------------------
S13794* MANISH      :220208:S13794:REMOVE VALIDATION DURING PROD CHANGE
S13794*-----------------------------------------------------------------
S13801* MANISH      :260208:S13801:FIX BORV ACCESS ERROR
S13801*-----------------------------------------------------------------
S13827* MANISH      :280208:S13827:FIX VALIDATION FOR RATE CHANGE
S13827*-----------------------------------------------------------------
S13836* MANISH      :290208:S13836:RESTRICT NO OF TIMES OF PROD CHANGE
S13836*-----------------------------------------------------------------
S13888* MANISH      :110308:S13888:NEW ACCT CAT SHOULD NOT BE ZERO
S13888*-----------------------------------------------------------------
S14015* HAN   :15042008:S14015: AMEND THE INPUT AND OUTPUT LENGTH
S14015*                       : FOR DESC FOR BAD-DEBT-DESC
S14015*-----------------------------------------------------------------
S14011* SONG JZ     :160408:S14011:FIX IR 20080331-003
S14011*-----------------------------------------------------------------
S13300* MANISH :230408:S13300: ADD NEW ERROR CODE FOR STATUS 50,52,54
S13300*-----------------------------------------------------------------
S13300* MANISH :240408:S13300: FIX ABEND IN 17003 TXN
S13300*-----------------------------------------------------------------
S14066* MANISH RAI : S14066 : FIX THE QUALIFIER ACCESS ERROR FOR 98922
S14066*-----------------------------------------------------------------
S14071* MANISH : 300408:S14071 : FIX GLIF ENTRIES FOR 13656 TXN
S14071*-----------------------------------------------------------------
S14071* MANISH : 010508 :S14071: REMOVE QUALIFIER VALIDATION
S14071*-----------------------------------------------------------------
S14073* ASIM MUND :060508 :S14073: SELECT LOAN ACCOUNT FROM RELM TO CHNG
S14073*           :       :      : STATUS IF CNTRT STAT IS CHNGD IN 17053
S14073*-----------------------------------------------------------------
S14107* MANISH RAI : 070508 : S14107: CHANGE FOR WRITEOFF ADD NEW TXNS
S14107*-----------------------------------------------------------------
LON032* SONG JZ : 23052008: LON032 : FIX IR 20080424-004
LON032*                              REFORMAT THE INPUT MESSAGE 17475
LON032*----------------------------------------------------------------
LON051* MANISH  : 29052008: LON051 : FOR CONVERSION FROM NPL TO PL
LON051*                            : CHECK IF VALUE IN FINE1,2 OR 4
LON051*                            : REJECT THE TXN
LON051*----------------------------------------------------------------
LON077* MANISH  : 05062008: LON077 : DURING CONVERSION FROM NPL TO
LON077*                            : PL THE PROPER TXN HISTORY SHOULD
LON077*                            : BE WRITTEN TO BOCT
LON077*----------------------------------------------------------------
LON120* MANISH : 20080613: LON120:CORRECTION TXN FOR NPL
LON120*        : 20080614        :CORRECT GLIF ISSUE
LON120*----------------------------------------------------------------
LON135* MANISH : 20080617 :LON0135 : ROUND THE AMOUNT DURING FORCED
LON135*                              CAPN DURING WRITEOFF
LON135*----------------------------------------------------------------
LON140* MANISH : 20080619 : LON140 : ENSURE PROPER BILLING TO RPYV
LON140*                              DURING WRITEOFF
LON140*----------------------------------------------------------------
LON152* MANISH : 21/06/2008 :LON152 : ADD ROUNDING FOR FINE 1,2,4
LON152*----------------------------------------------------------------
LON155* SONG JZ: 21/06/2008 :LON155 : FIX 20080527-005
LON155*                               CHANGE THE CONDITION FOR 771
LON155*----------------------------------------------------------------
LON157* ASIM MUND:20080624  :LON157 : FIX FOR 13655 AND 13656 TRAN
LON157*          :          :       : DURING PL TO NPL CORRECTION TRAN
LON157*----------------------------------------------------------------
LON168* AMALAZARTE:20080625 :LON168 : FIX CODE FOR GETTING THE GLCC OF
LON168*                     :       : A LOAN ACCOUNT TURNED TO WRITE OFF
LON168*                     :       : IF CGL QUALIFIER IS NOT SET FOR
LON168*                     :       : CONTROL THEN GET FROM CONTRACT
LON168*                     :       : PRODUCT. IF CGL QUALIFIER CONTRA
LON168*                     :       : IS NOT SET FOR CONTRA THEN GET
LON168*                     :       : FROM ED2.
LON168*                     :       : FIX THE CODE FOR CHANGING TRANS
LON168*                     :       : AMT OF BAD DEBT FOR WRITE OFF.
LON168*----------------------------------------------------------------
LON182* MANISH : 20080627: WRITE TO BOCT DURING FORCED BILLING
LON182*----------------------------------------------------------------
LON189* MANISH : 20080628 : LON189: COMMENT UNUSED CALL FOR NPL TO PL
LON189*                     LON189: FIX CORRECTION FOR CONTRACT
LON189*----------------------------------------------------------------
LON237* Li Li      :20080715:LON237: FIX INVALID GO TO
LON237*----------------------------------------------------------------
LON286* MANISH : 20080729 : LON286: FIX PROVISION AMT CORRECTION
LON286*                           : FOR WRITEOFF CORRECTION
LON286*----------------------------------------------------------------
LON236* GUANGXIAN  :20080729:LON236: CHANGE THE 17050 TRAN TYPE
LON236*            :        :      : ONLY USE 1:17000/17116/17117
LON236*            :        :      : 2:17465 3:17046
LON236*            :        :      : CLEAR ALL THE USENESS VARI TRAN-CD
LON236*            :        :      : CLEAR YOUBE 921-FLAG
LON236*            :        :      : ADD RATE-HISTORY REPAY-SCHEDULE
LON236*----------------------------------------------------------------
LON344* GUANGXIAN  :20080810:LON344: FIX NPL CORRECTION PROBLEM
LON344*----------------------------------------------------------------
LON362* MANISH     :20080814:LON362: REVERT CHANGES FOR MULTIPLE
LON362*                              PRODUCTS IN A CONTRACT
LON362*----------------------------------------------------------------
LON327* GUANGXIAN : 20080819:LON327:CLEAR DISPLAY AND UNUSED VARIABLES
LON327*           :         :      :KEEP THE USEFUL DISPLAY STDMSG
LON327*-----------------------------------------------------------------
LON387* MANISH    : 20080921:LON387:CHANGE THE LOGIC OF FORCED BILLING
LON387*                             DURING WRITEOFF
LON387*-----------------------------------------------------------------
LON435* MANISH    : 20080904:LON435:CHANGE FOR BRANCH AND PRODUCT
LON435*                            :TUNING REMOVE READ OF BOCT
LON435*-----------------------------------------------------------------
LON460* ASIM MUND : 20080918:LON460:MODIFY BR0001 FOR INVALID GO TO
LON460*-----------------------------------------------------------------
LON467* Jacky  : 20080920:LON467:when ACCR is ZERO,skip writing RPYV
LON467*-----------------------------------------------------------------
LON512*  RUBY PATEL : REMOVE DISPLAYS
LON512*-----------------------------------------------------------------
LON527* GUANGXIAN : 20081021:LON527:CHECK RETURN CODE
LON527*-----------------------------------------------------------------
LON531* MANISH : 20081024:LON531:ADD LOGIC OF STOP ACCRUAL FOR
LON531*                          WRITEOFF
LON531*-----------------------------------------------------------------
LON546* Li Li      : 20081030 : LON546 : Change For Perform Thru
LON546*-----------------------------------------------------------------
LON582* SONG JZ    : 20081210 : LON520 : CHANGE ACCT TYPE CHG FOR 17475
LON582*-----------------------------------------------------------------
LON615* GUANGXIAN  : 20081216 : LON615 : FIX THE CALL UT0250 AND UT0251
LON615*            :          :        : RETURN STATUS PROBLEM
LON615*-----------------------------------------------------------------
LON619* GUANGXIAN  : 20081217 : LON619 : CHECK MISMATCH LINKAGE SECTION
LON619*-----------------------------------------------------------------
LON645* RUBY PATEL : THE JOURNAL AMOUNT SHOUDL BE WRITTEN WITH A +VE
LON645* 20090107     SIGN FOR ALL TXNS AND THE GLIF SHOUDL BE GENERATED
LON645*              ON BASIS OF THE ED1/ED2/GLCPARAM SIGN
LON645*-----------------------------------------------------------------
013620 IDENTIFICATION DIVISION.
013630 PROGRAM-ID. BR0001.
013640 AUTHOR. I. M. HASSAN.
013650*
013660* DESC. LOANS MORTG TRACK CREATE/AMEND
013670*
013680 INSTALLATION. FINANCIAL NETWORK SERVICES PTY LTD.
013690*
013700 DATE-WRITTEN. JUL 01 1983.
013710 DATE-COMPILED. XXXXXXXXXX.
013720/
013740 ENVIRONMENT DIVISION.
013750*
013760 CONFIGURATION SECTION.
013770*
013780 SOURCE-COMPUTER. VAX.
013790 OBJECT-COMPUTER. VAX.
013800*
013810 SPECIAL-NAMES.
013820*
013830*    SWITCH-1 ON STATUS IS ATM-ONLY-REFRESH
013840*    SWITCH-2 OFF STATUS IS NO-REFRESH
013850     CONSOLE IS CONS.
013860*
013870*INPUT-OUTPUT SECTION.
013880*FILE-CONTROL.
013890*
013900 DATA DIVISION.
013910*
013920*FILE SECTION.
013930/
013940 WORKING-STORAGE SECTION.
013950/
LON236 01 WA-ALL-VARIABLE.
MVSWLW    03 WA-CONB-BAD-DEBT-IND                  PIC 99 VALUE 0.
S14066    03 WA-TMP-AMT-DI                         PIC 9(18) VALUE 0.
LON362    03 WA-TMP-AMT                            PIC S9(14)V999 COMP.
S13827    03 WA-EFR-IND                            PIC X  VALUE SPACES.
S13827    03 WA-RATE-OPT-APPLIC                    PIC X  VALUE SPACES.
INITWS  03 WA-AMOUNT-ROUND                       PIC S9(14)V999 VALUE 0.
INITWS    03 WA-RESULT-INTEGER                     PIC S9(14) VALUE 0.
INITWS  03 WA-RES                                PIC S9(14)V999 VALUE 0.
INITWS    03 WA-ROUNDING                           PIC S999V999 VALUE 0.
INITWS    03 ROUNDING-FACT                         PIC S999V999 VALUE 0.
INITWS  03 WA-REMD                               PIC S9(14)V999 VALUE 0.
LON120    03 WA-SAVE-BOCT-REC                   PIC X(500) VALUE SPACES.
LON120    03 WA-REC-NO                          PIC 9(09) VALUE ZEROES.
LON236* THE OTHER VARIABLE
LON387 01 WA-RPYV-TMP.
LON387    03 WA-RPYV-AMT                        OCCURS 4 TIMES
LON387                                          PIC S9(14)V9(3).
LON387    03 WA-RPYV-CODE                       OCCURS 4 TIMES
LON387                                          PIC X(02).
LON387    03 WA-RPYV-BUK                        OCCURS 4 TIMES
LON387                                          PIC 9.
S13815    03 WA-TMP-AMT-T                       OCCURS 5 TIMES
S13815                                          PIC S9(14)V999.
LON387 01 WA-DISP-AMT                           PIC 9(18).
INITWS 01 WA-ACCR-TMP                           PIC S9(14)V999 VALUE 0.
LON140 01 WA-WRITE-RPYV-FLAG  PIC 9.
LON140    88  WA-RPYV-NO      VALUE 0.
LON140    88  WA-RPYV-YES     VALUE 1.
S13668 01  XXCT-RECORD-AREA.
S13668     COPY BOCTVARV IN LIBRYBOR REPLACING ==(BOCT)==
S13668                   BY ==XXCT==.
S13668     COPY BOCTALT  IN LIBRYBOR REPLACING ==(BOCT)==
S13668                      BY                 ==XXCT==.
S10093 01  WA-GLCC-VARS.
S10093     05  WA-GLCC-XFER-AMOUNT             PIC S9(14)V999 COMP
S10093                                         VALUE 0.
S10093     05  WA-SIGNED-TXN-AMOUNT            PIC S9(14)V999 COMP
S10093                                         VALUE 0.
S10093     05  WA-SOUR-GL-CLASS-CODE           PIC X(25) VALUE SPACES.
S10093     05  WA-DEST-GL-CLASS-CODE           PIC X(25) VALUE SPACES.
S10093 01  WA-SAVE-INPUT-MSG                   PIC X(983).
S10093 01  WA-TMP-GL-P                         PIC S9(14)V999 COMP.
LON327*S10093 01  WS-BORM-GL-CLASS-CODE      PIC X(25) VALUE SPACES.
LON327*S10093 01  WS-VOUCHER-VARIABLES.
LON327*S10093     03 WS-VCHR-ORG-TRANCODE        PIC 9(6)  VALUE 0.
LON327*S10093 01  WA-ROUND-AMNT-0                     PIC S9(17) COMP.
S10093 01  PROGNAME                            PIC X(6) VALUE "BR0001".
S10093 01  WA-MULTI-BASE-CURR-AMT              PIC S9(14)V999
INITWS                                 TRAILING SEPARATE VALUE 0.
S10093 01  WA-SAVE-ST-AREA-DATA                PIC X(1132).
S10093 01  WA-SAVE-ST-AREA-TRN-NO              PIC 9(6).
LON327*S10093 01  WA-SAVE-BORM-RECORD-AREA            PIC X(983).
S12787 01  WA-SAVE-POSTING-IND                 PIC X    VALUE SPACE.
S10093 01  WA-PASSALONG.
S10093     COPY PASALONG IN LIBRYMIS.
LON327*S10093 01  ARRS-RECORD-AREA.
LON327*S10093     COPY ARRSMAST IN LIBRYBOR.
S10093 01  WA-UT28-PARAM.
S10093     COPY UT28PARM IN LIBRYCHG.
TB0159*
LON327*S10093 01  UT0096CA-AREA.
LON327*S10093     COPY UT0096CA IN LIBRYMIS
LON327*S10093     REPLACING ==(UT0096CA)== BY ==UT0096CA==.
LON327*S10093     COPY MISERR01 IN LIBRYMIS.
S10093 01  UT0251CA-CALL-AREA.
S10093     COPY UT0251CA IN LIBRYMIS.
LON327*S10093     COPY UT4001CA IN LIBRYMIS.
LON327*S10093 01  WA-GL-TXN-AREA.
LON327*S10093     COPY INPUTMSL IN LIBRYGEN.
LON327*S10093 01  WA-UTINSPCA-AREA.
LON327*S10093     COPY UTINSPCA IN LIBRYMIS.
LON327*S10093 01  FAGD-RECORD.
LON327*S10093     COPY FAGDMAST IN LIBRYMIS.
LON327*S10093 01  GL-PASSALONG.
LON327*S10093     COPY GLPASSWS IN LIBRYGEN.
LON327*S10093 01  MFNP-RECORD.
LON327*S10093     COPY MFNPARM IN LIBRYMIS.
S10093 01  WA-OTHER-SAVE-AREAS.
S10093     COPY UT80010 IN LIBRYMIS.
LON327*S10093 01  SUSP-RECORD-AREA.
LON327*S10093     COPY SUSPMAST IN LIBRYMIS.
LON327*S10093 01  CASHIER-CALL-AREA.
LON327*S10093     COPY CASHCALL IN LIBRYMIS.
LON327*015570 01  WA-SPECIAL-CHARACTERS.
LON327*015580     COPY WASPCHAR IN LIBRYMIS.
015590/
015600 01  CUSM-RECORD.
015610     COPY CUSMMAST IN LIBRYMIS.
015620*
S10093 01  SY0010CA-COMMON-AREA.
S10093     COPY SY0010CA IN LIBRYMIS.
015700/
S10093 01  WA-JRNL-CALL-AREA.
S10093     COPY JNRST IN LIBRYMIS.
015760 01   UT0310CA-AREA.
015770      COPY UT0310CA IN LIBRYMIS.
S13420 01  SYPR-RECORD.
S13420     COPY SYPRMAST IN LIBRYMIS REPLACING ==(SYPR)==
S13420        BY                                 ==SYPR==.
TB0069/
TB0069     COPY DATECODE IN LIBRYMIS
TB0069          REPLACING ==(DATE)== BY ==DATE==.
NLB115/
S10139 01  UT0060CA-AREA.
S10139     COPY UT0060CA IN LIBRYMIS.
S10139 01  WK-UT0060-WORK-AREA.
S10139     03  LOCATED-FLAG                     PIC X.
S10139     03  WK-TELLER-INDEX                  PIC 99.
S10139     03  WK-ERROR-INDEX                   PIC 99.
S10139     03  WK-ERROR-OVERRIDE                PIC XX.
S10139     03  WK-BATCH-TANDEM                  PIC X.
S10139     03  FILLER                           PIC X(42).
S10139 01  WA-DISPLAY-NUMBER                    PIC 9(17).
S10093 01  WS-SAVE-TXN-CURRENCY                PIC X(3).
016080 01  UT0250-CALL-AREA.
016090     COPY SYSPCALL IN LIBRYMIS.
016100/
016110 01  UT0079-LINKAGE.
016120     COPY UT0079CA IN LIBRYBOR.
016130/
016140 01  WA-SAVE-UT0296CA-AREA.
016150     COPY UT0296SS IN LIBRYMIS.
016160 01  UT0296-CALL-AREA.
016170     COPY UT0296CA IN LIBRYMIS.
016180/
LON327*016190     COPY UT0681CA IN LIBRYMIS.
016200/
LON327*TB0023     COPY UT0683CA IN LIBRYMIS.
TB0023/
LON327*S11323     COPY UT1020CA IN LIBRYMIS.
S11323/
SC5100 01 WA-NPL-COMPONENT-1 PIC 9(17).
SC5100 01 WA-NPL-COMPONENT-2 PIC 9(10).
SC5100 01 WA-53-54-FLAG      PIC X(1).
SC6343 01 WA-53-52-FLAG      PIC X(1).
S10093 01 WS-FOUND                             PIC X.
S13349 01 WA-GFIL-BAD-DEBT-ARRAY.
S13349    03 WA-GFIL-BAD-DEBT  OCCURS 5.
S13349       05 GLIF-GL-BUCKET-CDE          PIC XX.
S13349       05 GLIF-GL-BUCKET-DUE          PIC S9(14)V999 COMP.
SC5014
INITWS 01 WA-NPL-INT-RATE                   PIC S99V9999 VALUE 0.
SC7860 01 WA-BAD-DEBT-DT                    PIC S9(9) BINARY.
LON120 01 WA-TEMP-DT                        PIC 9(9).
INITWS 01 WA-RATE-NPL                       PIC S99V9999 VALUE 0.
ST0094 01 WA-BAD-DEBT-IND-BEF               PIC XX.
ST0094 01 WA-BAD-DEBT-IND-AFT               PIC XX.
ST0094 01 WA-TRANSFER-NPL-VARS.
ST0094    03 WA-NPL-STOR-RT                 PIC S9(3)V9(4) COMP.
ST0094    03 WA-NPL-INT-RT                  PIC S9(3)V9(4) COMP.
ST0094    03 WA-NPL-INT-ADJ                 PIC S9(12)V9(5) COMP.
ST0094    03 WA-NPL-INT-DUE                 PIC S9(14)V999 COMP.
ST0094    03 WA-INT-BUCKET-CD               PIC S9(14)V999 COMP.
ST0094    03 WA-TOTAL-I-BUCKET              PIC S9(14)V999 COMP.
ST0094    03 WA-NEGO-RATE-IND               PIC X.
SC7560    03 WA-NEGO-RATE-FLAG              PIC X.
SC7560    03 WA-EFR-CHNG-IND                PIC X.
SC7560    03 WA-DUES-RECALC-IND             PIC X.
ST0094    03 WA-RATE-INCR                   PIC S99V9(5) COMP.
SC7850    03 WA-NPL-JRNL-NO                 PIC S9(7)    COMP.
SC8133 01 WA-PREV-CYCLE-NO                  PIC 9(03).
LON120 01 WA-PREV-BAD-DEBT-IND              PIC XX.
LON120 01 WA-NEXT-BAD-DEBT-IND              PIC XX.
SC8314 01 WA-OLD-RATE                       PIC S999V9999 COMP.
SC9829 01 WA-STOP-READ-FLAG         PIC 9 VALUE 0.
SC9829    88 WA-CONTINUE                VALUE 0.
SC9829    88 WA-STOP                    VALUE 1.
SC9829 01  WA-LATEST-TXN                     PIC X.

016940 01  REJM-RECORD-AREA.
TB0258     COPY REJMMAST IN LIBRYMIS.
016960*
LON327*016970 01  TPPR-RECORD-AREA.
LON327*016980     COPY TPPRMAST IN LIBRYMIS.
LON327*016990     COPY UTTPPRCA IN LIBRYMIS.
017040/
LON327*TB0165 01  CTAM-RECORD-AREA.
LON327*TB0165     COPY CTAMMAST IN LIBRYCTA.

TB0165     COPY UTCTAPCA IN LIBRYCTA.
TB0165     COPY CTAPMAST IN LIBRYCTA.
TB0157/
LON327*TB0157 01  WA-INSM-AREA.
LON327*TB0157     COPY INSMMAST IN LIBRYMIS.
TB0173/
LON327*TB0173 01  REFM-RECORD-AREA.
LON327*TB0173     COPY REFMMAST IN LIBRYCHG
LON327*TB0173     REPLACING ==(REFM)== BY ==REFM==.
TB0173*
LON327*TB0190 01  UTAO1PCA-CALL-PARAM.
LON327*TB0190     COPY UTAO1PCA IN LIBRYMIS.
TB0190
LON327*TB0190 01  UTAO2PCA-CALL-PARAM.
LON327*TB0190     COPY UTAO2PCA IN LIBRYMIS.
TB0190*
LON327*TB0219 01  INOD-RECORD-AREA.
LON327*TB0219     COPY INODMAST IN LIBRYINV.
LON327*TB0219 01  INOD-ALTERNATE-KEY.
LON327*TB0219     COPY INODALT  IN LIBRYINV.
LON327*S10093 01  UTVTACCA-CALL-PARAM.
LON327*S10093     COPY UTVTACCA IN LIBRYINV.
017550 01  UT0400-CALL-AREA.
017560     COPY UT0400CA IN LIBRYMIS.
017570     COPY RELMMAST IN LIBRYMIS.
S12787 01  UT0145CA-CALL-AREA.
S12787     COPY UT0145CA IN LIBRYMIS.
017580*
017590* RELMVALS COPY AREA.
017600     COPY RELMVALS IN LIBRYMIS.
017610/
TB0255 01  WA-POST-DATE                  PIC S9(9) BINARY.
LB0113 01  WA-XX                         PIC 99.
LB0113 01  WA-YY                         PIC 99.
017850 01  WA-FOUND-IND                     PIC X.
017860     88 FOUND-NEW-RATE                   VALUE "Y".
017870     88 NEW-RATE-NOT-FOUND               VALUE "N".
017880     88 NEW-RATE-IN-PROGRESS             VALUE "P".
017890 01  WA-NEW-RATE-FOUND                PIC X VALUE "Y".
017900 01  WA-NEW-RATE-NOT-FOUND            PIC X VALUE "N".
017910 01  WA-IN-PROGRESS                   PIC X VALUE "P".
017920 01  WA-CAFM-TYPE                     PIC X.
017930     88 CAP-IN-FORCE                  VALUE "1" "3".
017940     88 FLOOR-IN-FORCE                VALUE "2" "3".
017950     88 NO-CAP-OR-COLLARS             VALUE "0".
017960     88 FIXED-IN-FORCE                VALUE "4".
017970     88 CAP-COLLARS-ALLOWED           VALUE "5".
018020 01  WA-SEARCH-RATE-DATE              PIC S9(9) BINARY.
018030 01  WA-WORK-AMOUNT-0                 PIC S9(17) COMP.
018120 01  WA-WORK-AMOUNT-1                 PIC S9(16)V9 COMP.
018130 01  WA-WORK-AMOUNT-2                 PIC S9(15)V99 COMP.
018140 01  WA-WORK-AMOUNT-3                 PIC S9(14)V999 COMP.
TB0084 01  WA-CTR                           PIC 99 VALUE 0.
TB0082*
TB0082 01  WA-FLAGS.
TB0104     05  WA-BOIS-AUTO-DBT-ACCT-NO-FLAG PIC X VALUE "N".
TB0104         88  WA-BOIS-AUTO-DBT-ACCT-UPDTD VALUE "Y".
TB0143 01  WA-ACCT-NO-16                   PIC 9(16) COMP.
TB0082*
SC1448 01  WA-BR0001-BAD-DEBT-DATE          PIC S9(9) BINARY.
SC5861 01  WA-REJM-REASON-CODE              PIC 99 VALUE 0.
S13708 01  WA-PROD-CHAGE-VAR.
S13708      05  WA-OLD-ACCT-TYPE            PIC 9(4).
S13708      05  WA-OLD-INT-CAT              PIC 9(4).
S13708      05  WA-NEW-ACCT-TYPE            PIC 9(4).
S13708      05  WA-NEW-INT-CAT              PIC 9(4).
SC5572
SC5572 01  WA-STORED-TEXT-LN.
SC5572     03  WA-ORIG-LOAN-STAT               PIC 99.
SC5572     03  FILLER                          PIC XX.
SC5572     03  WA-ORIG-TXN-CODE                PIC 9(5).
SC5572     03  FILLER                          PIC XX.
SC5572     03  WA-ORIG-OUSTD-LOAN-BAL          PIC S9(14)V999 VALUE 0.
SC5572     03  FILLER                          PIC XX.
SC5572     03  WA-REASON-CODE-20               PIC XX.
SC5572     03  FILLER                          PIC X(17).
SC5572
SC1795 01  WA-TOTAL-BUCKET-INT              PIC S9(14)V9(3) COMP.
TB0161 01  WA-FLAG-CORR                     PIC X.
SC4083 01  WA-BUCKET-NO                     PIC 99.
SC4083 01  WA-BUCKET-CODE                   PIC XX.
S13349 01  WA-PRIO-NUM-ARRAY.
S13349     03  WA-PRIO-NUM   OCCURS 15.
S13349         05 WA-PRIONUM                PIC 9(2).
S13349         05 WA-PRIOCDE                PIC X.
SC4083 01  WA-PRIO-INDX                     PIC 9(2).
S10614 01  WS-OTHER-OVERRIDE                PIC X.
S10614 01  WS-CHANGE-OVERRIDE               PIC X VALUE "N".
SC4083 01  WA-INDX                          PIC 99 VALUE 0.
TB0161 01  WA-GL-INDX                       PIC 99 VALUE 0.

019510 01  WA-VARIABLES.
019530     03  WA-DISP-KEY.
019540         05  WA-DISP-SOC-NO           PIC 999.
019550         05  WA-DISP-TXCODE           PIC 9(6).
019560     03  WA-TRAN-STATUS               PIC 9(02) VALUE ZEROES.
019570     03  WA-NEW-TRAN                  PIC 9(06) VALUE ZEROES.
019580     03  WA-ZERO                      PIC S9(5) COMP VALUE 0.
S13349     03  WA-ZEROES                    PIC X(5)  VALUE "00000".
019600     03  WA-PTR                       PIC 99.
019610     03  WA-BORV-TRAN-DATE            PIC 9(08).
019620     03  WA-TODAYS-DATE               PIC 9(08).
019630     03  WA-BORV-KEY.
019640         05 WA-BORV-SOC-NO            PIC 9(03).
019650         05 WA-BORV-ACT-NO            PIC 9(16).
SC7850     03  WA-BORV-53-KEY.
SC7850         05 WA-BORV-53-SOC-NO         PIC 9(03).
SC7850         05 WA-BORV-53-ACT-NO         PIC 9(16).
SC7779         05 WA-BORV-53-JRNL-NO        PIC S9(7) COMP.
SC7779     03  WA-NPL-PREV-DUE-DATE         PIC S9(9) BINARY.
019710     03  WA-BD-DATE.
019720         05  WA-BD-CC                 PIC 9(02).
019730         05  WA-BD-YY                 PIC 9(02).
019740         05  WA-BD-MM                 PIC 9(02).
019750         05 WA-BD-DD                 PIC 9(02).
019760     03 WA-PRECEEDING-TRANS          PIC 9(07) VALUE ZEROES.
019770     03  WA-FOUND-MANUAL-TAG          PIC X(01) VALUE SPACES.
019780     03  WA-END-OF-FILE               PIC X(01) VALUE SPACES.
019790     03  WA-READ                      PIC 9(09) VALUE ZEROES.
SC7850     03  WA-BORV-READ                 PIC 9(09) VALUE ZEROES.
SC3055     03  W100-TODAY.
019820         05  W100-TODAY-CCYY               PIC 9(4).
019830         05  FILLER  REDEFINES  W100-TODAY-CCYY.
019840             07  W100-TODAY-CC             PIC 99.
019850             07  W100-TODAY-YY             PIC 99.
019860         05  W100-TODAY-MM                 PIC 99.
019870         05  W100-TODAY-DD                 PIC 99.

019890 01  WA-EDIT-32.
019900     03  WA-EDIT-32-BIT PIC X
019910         OCCURS 32 TIMES.
TB0173*
020010 01  WA-INT-DUE-DT                    PIC S9(9) BINARY.
020020 01  WA-INT-FREQ                      PIC 99.
020030 01  WA-TXN-AMOUNT                    PIC S9(14)V999  COMP.
020040 01  WA-PROC-DDMMCCYY.
020050      05  WA-PROC-DD                  PIC 99.
020060      05  WA-PROC-MM                  PIC 99.
020070      05  WA-PROC-CCYY.
020080          07  WA-PROC-CC              PIC 99.
020090          07  WA-PROC-YY              PIC 99.
020100      05  WA-PROC-CCYY-R REDEFINES WA-PROC-CCYY
020110                                      PIC 9(4).
020230 01  WA-LEAP-REM                      PIC 9.
020240     88  WA-LEAP-YEAR                 VALUE 0.
020250 01  WA-LEAP-REM1                     PIC 9(3).
020270 01  WA-PROC-DT-BINARY                PIC S9(9) BINARY.
020280 01  WA-WORK-DDMMCCYY.
020290      05  WA-WORK-DD                  PIC 99.
020300      05  WA-WORK-MM                  PIC 99.
020310      05  WA-WORK-CCYY.
020320          07  WA-WORK-CC              PIC 99.
020330          07  WA-WORK-YY              PIC 99.
020340      05  WA-WORK-CCYY-R REDEFINES WA-WORK-CCYY
020350                                      PIC 9(4).
LON327*S10093 01  WA-TAB-MASK.
LON327*S10093     03  WA-INPUT-PARAM-TABLE    PIC X    OCCURS 32 TIMES.
S10093     COPY GLCCWORK IN LIBRYMIS.
020390 01  WA-EDIT-MASK.
020400     03  FILLER                   PIC X(3).
020410     03  BIT-4                    PIC X.
020420     03  FILLER                   PIC X(5).
020430     03  BIT-10                   PIC X.
020440     03  FILLER                   PIC X(22).
SC2386 01  WA-DEF-BUCKET-AMT            PIC S9(12)V9(5) COMP.
SC1121 01  WA-BORM-INT-ACCR             PIC S9(12)V9(5) COMP.
SC1121 01  WA-REASON-CODE               PIC 9(4) VALUE ZEROES.
020580 01  WA-ACCRUED-INT                      PIC S9(14)V999 VALUE 0.
020590 01  WA-TOM-ACRU-AMT                     PIC S9(12)V9(5) VALUE 0.
020610 01  WA-INT-ST-DATE                      PIC S9(9) BINARY VALUE 0.
020760 01  WA-GLD-CODE.
020770     COPY GLDCODES IN LIBRYBOR.
S10099 01  PROM-RECORD-AREA.
S10099     COPY PROMMAST IN LIBRYMIS.
S10099 01  WA-ENTRY-DATE-TIME.
S10099     03   WA-ENTRY-DATE-CC                PIC 99.
S10099     03   WA-ENTRY-DATE-YY                PIC 99.
S10099     03   WA-ENTRY-DATE-MM                PIC 99.
S10099     03   WA-ENTRY-DATE-DD                PIC 99.
S10099     03   WA-ENTRY-TIME-HH                PIC 99.
S10099     03   WA-ENTRY-TIME-MM                PIC 99.
S10099     03   WA-ENTRY-TIME-SS                PIC 99.
020880*
020940 01  WA-FLAGS.
020950   03  WA-NO-MORE-FLAG                   PIC X VALUE "N".
020960     88  WA-NO-MORE                      VALUE "Y".
020970   03  WA-ERROR-FLAG                     PIC X VALUE "N".
020980     88  WA-ERROR                        VALUE "Y".
020990   03  WA-FOUND-R-FLAG                   PIC X VALUE "N".
021000     88  WA-FOUND                        VALUE "Y".
TB0023     88  WA-FOUND-P                      VALUE "P".
TB0069   03  WA-STORE-RATE-CH-IND              PIC X VALUE "N".
TB0069     88  WA-STORE-RATE-CH                VALUE "Y".
TB0069   03  WA-NEGO-RATE-IND-CH               PIC X VALUE "N".
TB0069   03  WA-RATE-CH-IND                    PIC X VALUE "N".
TB0069     88  WA-RATE-CH                      VALUE "Y".
TB0069   03  WA-NO-NPB-ADD                     PIC X VALUE "N".
021260/
LON327*021270 01  PCOD-RECORD-AREA.
LON327*021280     COPY PCODMAST     IN LIBRYMIS
LON327*021290     REPLACING ==(PCOD)== BY ==PCOD==.
021300/
LON327*021310 01  WA-FACD-AREA.
LON327*021320     COPY FACDMAST IN LIBRYMIS.
LON327*TB0143     COPY FACDALT  IN LIBRYMIS.
021340/
LON327*TB0023 01  WA-XPFD-AREA.
LON327*TB0023     COPY XPFDMAST IN LIBRYMIS.
LON327*TB0023     COPY XPFDALT  IN LIBRYMIS.
TB0023/
LON327*021390 01  BRHM-RECORD.
LON327*021400     COPY BRHMMAST IN LIBRYMIS.
TB0055 01  UTBRHMCA-CALL-AREA.
TB0055     COPY UTBRHMCA IN LIBRYMIS
TB0055     REPLACING ==(UTBRHMCA)== BY UTBRHMCA.
TB0055 01  UTBRHMRA-CALL-AREA.
TB0055     COPY UTBRHMRA IN LIBRYMIS
TB0055     REPLACING ==(UTBRHMRA)== BY UTBRHMRA.
021470/
LON327*021480 01  CUSV-AARECORD-AREA.
LON327*021490     COPY CUSVAA IN LIBRYMIS.
021500/
LON327*021510 01  CUSV-BBRECORD-AREA.
LON327*021520     COPY CUSVBB IN LIBRYMIS.
021530/
021540 01  CUSV-DDRECORD-AREA.
021550     COPY CUSVDD IN LIBRYMIS.
021560/
LON327*021570 01  CAFM-RECORD.
LON327*021580     COPY CAFMMAST IN LIBRYMIS.

021600/
LON327*021610 01  CUSV-EERECORD-AREA.
LON327*021620     COPY CUSVEE IN LIBRYMIS.
021630/
LON327*021670 01  BLDV-SL-RECORD-AREA.
LON327*021680     COPY BLDVSL IN LIBRYBOR
LON327*021690     REPLACING ==(BLDVSL)== BY ==BLDV-SL==.
021700/
LON327*021710 01  BLDV-XX-RECORD-AREA.
LON327*021720     COPY BLDVXX IN LIBRYBOR.
LON327*021730 01  BLDV-YY-RECORD-AREA.
LON327*021740     COPY BLDVYY IN LIBRYBOR.
021750/
021760 01  BLDV-RECORD.
021770     COPY BLDVWW IN LIBRYBOR.
021780/
LON327*021840 01  MICM-RECORD-AREA.
LON327*021850     COPY MICMMAST IN LIBRYMIS.
021860/
LON327*021870 01  MICV-RECORD-AREA.
LON327*021880     COPY MICVVARV IN LIBRYMIS.
021890/
021900 01  LNGM-AREA.
021910     COPY LNGMMAST IN LIBRYMIS.
021920/
LON327*ST0170 01  WA-OFSR-AREA.
LON327*ST0170     COPY OFSRMAST   IN LIBRYBOR.
ST0170/
LON327*022050 01  PINT-RECORD.
LON327*022060     COPY PINT IN LIBRYBOR.
LON327*022070 01  FADT-RECORD.
LON327*022080     COPY FADT IN LIBRYBOR.
TB0023/
LON236* NEED THE BADDEBT HERE
LON327*TB0082 01  COLM-AREA.
LON327*TB0082     COPY COLMMAST IN LIBRYMIS.
TB0082/
TB0092     COPY BADDEBT  IN LIBRYBOR.
TB0092/
LON327*022420 01  PORT-DB-AREA.
LON327*022430     COPY PORTPARM IN LIBRYMIS.
022440/
LON327*022450 01  RPRT-DB-AREA.
LON327*022460     COPY RPRTPARM IN LIBRYMIS.
022470/
022480     COPY UPTELLCA IN LIBRYMIS.
022490/
022500     COPY UT0020CA IN LIBRYMIS.
022510/
022520     COPY DBIOMODCA IN LIBRYMIS.
022530/
022540     COPY BBLC IN LIBRYBOR.
022550/
LON327*022560 01  DBIOMOD-RECORD2.
LON327*022570     COPY BINS IN LIBRYBOR.
LON327*022580 01  DBIOMOD-RECORD3.
LON327*022590      COPY ABEXMAST IN LIBRYMIS.
022600     COPY DBIOORAST IN LIBRYMIS.
022610/
022620     COPY DBIOMODNAME IN LIBRYMIS.
022630     COPY RGRPMAST IN LIBRYMIS.
TB0092/
TB0092 01  WA-REASON-CODE-REC.
TB0092     COPY RSNCMAST IN LIBRYMIS.
TB0092/
022850 01  ACLN-RECORD-AREA.
022860     COPY ACLN IN LIBRYMIS.
022870/
022880 01  ACSN-RECORD-AREA.
022890     COPY ACSN IN LIBRYMIS.
022900/
LON327*022910 01  UT0200-CALL-PARAMS.
LON327*022920     COPY UT0200CA IN LIBRYINV.
022930   03  WA-ACCT-PARM                    PIC X(1810)
022940                                       VALUE SPACES.
022950       COPY UT0200RT IN LIBRYINV.
022960/
LON327*022970 01  ACCT-PARAM-REC.
LON327*022980     COPY DEPPARM IN LIBRYINV.
023000/
LON327*023010 01  INVM-RECORD-AREA.
LON327*023020     COPY INVMMAST IN LIBRYINV.
LON327*S10093 01  CHECK-RESPONSE-AREA.
LON327*S10093      COPY CHECKRES IN LIBRYMIS.
LON327*S10093      COPY PBOOKCA IN LIBRYMIS
LON327*S10093      REPLACING ==(PBOOKCA)== BY ==PBOOKCA==.

LON327*S10093 01  BIRM-RECORD.
LON327*S10093     COPY BIRM IN LIBRYBOR.
023110/
023120 01  SAVE-ACSN-RECORD-AREA             PIC X(82).
023130/
023140 01  SAVE-ACLN-RECORD-AREA             PIC X(502).
023150/
023160 01  PUAREA.
023170     05  FIELD-A                        PIC X(32).
023180     05  FIELD-B                        PIC X(4).

023460 01  WA-SUB                             PIC 99   VALUE 0.
023460 01  WA-SUB1                            PIC 99   VALUE 0.
023480*01  W-PROGRAM-NAME                     PIC X(6) VALUE "BR0001".
023500 01  WA-ACSN-KEY.
023510     03  WA-ACSN-SOC-NO                 PIC 9(3).
023520     03  WA-ACSN-SYS-ID                 PIC X(3).
023530     03  WA-ACSN-ACCT-NO                PIC 9(16).
023540     03  WA-FILLER                      PIC X(09).
S10093 01  WA-WORK-SYSTEM.
S10093     03  WA-WORK-SYSTEM-ID              PIC X.
S10093     03  WA-WORK-SECTION-ID             PIC XX.
S10093 01  UT0017-CALL-AREA.
S10093     COPY UT0017CA IN LIBRYMIS.
S10093 01  WA-WORK-AREA-BACKDATED.
INITWS     03 WA-BASE-INT-ACCR-BEFORE          PIC S9(14)V999 VALUE 0.
INITWS     03 WA-MARG-INT-ACCR-BEFORE          PIC S9(14)V999 VALUE 0.
INITWS     03 WA-MARG-INT-ACCR-AFTER           PIC S9(14)V999 VALUE 0.
INITWS     03 WA-BASE-INT-ACCR-AFTER           PIC S9(14)V999 VALUE 0.
023660 01  WA-WORK-AREA.
S10093     03  WA-SAVE-TXN-AMT                 PIC S9(14)V999 VALUE 0.
S10093     03  WA-DUMMY-TRN-NO                 PIC 9(6).
S10093     03  WA-DUMMY-TRN-NO-R  REDEFINES WA-DUMMY-TRN-NO.
S10093         05  FILLER                      PIC 9.
S10093         05  WA-DUMMY-FST-ONE            PIC 9.
S10093           88  WA-DUMMY-LOAN-TXN         VALUE 1.
S10093         05  FILLER                      PIC X(2).
S10093         05  WA-DUMMY-LST-TWO            PIC 99.
S10093           88  WA-DUMMY-CR-DB-TXN            VALUE 30, 32, 40, 47,
S10093                                                   50, 52, 53, 90.
S10093           88  WA-REPAYMENT-TXN              VALUE 10, 20, 45, 46,
S10093                                                   11, 30, 49.
S10093     03  WA-REPAY-TRCODE                 PIC 9(6).
S10093         88  WA-CUST-INC-EXP-FEES            VALUE 11054 11154
S10093                                                   13012 13112
S10093                                                   13013 13113
S10093                                                   13042 13142
S10093                                                   13043 13143
S10093                                                   13052 13152
S10093                                                   13058 13158
S10093                                                   13061 13161
S10093                                                   13062 13162.
S10093     03  WA-ORIG-EFT-TRN-NO              PIC 9(6) VALUE 0.
S10093     03  WA-STORE-TXN-DT                 PIC S9(9) BINARY.
S10093     03  4XSTARS                         PIC X(4) VALUE "****".
S10093 01  ALL-WORKAREAS.
S10093     03  WA-DMY                          PIC S9(9) BINARY.
INITWS     03  WA-WORK-INT-0                   PIC S9(12) VALUE 0.
S10093 01  WA-CAL-CODE.
S10093     05 WA-DDMMCCYY.
S10093        10 WA-CAL1-DD              PIC 9(2).
S10093        10 WA-CAL1-MM              PIC 9(2).
S10093        10 WA-CAL1-CC              PIC 9(2).
S10093        10 WA-CAL1-YY              PIC 9(2).
S10093     05 WA-CCYYMMDD.
S10093        10 WA-CAL2-CC              PIC 9(2).
S10093        10 WA-CAL2-YY              PIC 9(2).
S10093        10 WA-CAL2-MM              PIC 9(2).
S10093        10 WA-CAL2-DD              PIC 9(2).
S10093*
S10093     03 LNGM-DATA-REC-R.
024210        05  LNGM-KEY-AREA.
024220            07  WA-INST-NO                  PIC 999.
024230            07  WA-LANG-CODE                PIC 99.
024240     03  WA-INPUT-ACCT.
024250         05  FILLER                      PIC XXXX.
024260         05  WA-INPUT-BR                 PIC 999.
024270         05  FILLER                      PIC X(9).
024280     03  WA-DUMMY-KEY.
024290         05  WA-SOC-NO                   PIC 999.
024300         05  WA-CUST-NO                  PIC 9(16).
024310         05  FILLER                      PIC X(12).
TB0069     03  WA-BOR-FIELD-CHANGE             PIC 9 VALUE ZERO.
024750     03  WA-RESULT                       PIC 9(5).
024840     03  WA-FIRST-TIME-FLAG              PIC 9          VALUE 0.
024870     03  WA-RATE                         PIC X(4).
025000     03  WA-STORE-TRN-NO                 PIC 9(6).
TB0069     03  WA-BUCV-INT-ACCR                PIC S9(12)V9(5) COMP.
TB0069     03  WA-NEXT-RT-INCR                 PIC S99V9(5) COMP.
TB0069     03  WA-INT-ACCR                     PIC S9(12)V9(5) COMP.
S11323     03  WA-TXN-DATE                     PIC S9(9) BINARY.
INITWS 01  WA-RT-INCR                          PIC S99V9(5) VALUE 0.
025910 01  WA-INT-DATE-A                       PIC S9(9) BINARY.
INITWS 01  WA-INT-RATE-A                       PIC S99V9999 VALUE 0.
026000 01  WA-TIERS.
026010     03 WA-TIER-METHOD                   PIC 99.
026020     03 WA-TIER-METH REDEFINES WA-TIER-METHOD.
026030        05 WA-TIER-METH-1                PIC 9.
026040        05 WA-TIER-METH-2                PIC 9.
026050 01  WA-TIER-LEVEL                       PIC 9(4).
LON327*026060 01  WA-TIER-SUB                         PIC 9(4).
TB0104 01  WA-UT0014-INPUT-FREQ                PIC 99.
TB0104     88 DAILY-INPUT-FREQ                 VALUE 15.
TB0104     88 MONTHLY-INPUT-FREQ               VALUES 01 03 06
TB0104                                                12 99.
026160*********************************************************
026170***  INPUT AMOUNT DECIMAL CHECK FOR INVALIDATION ********
026180 01  WA-VA-DUMMY-AMOUNT.
026190     03  WA-VA-AMT                       PIC S9(14)V999
INITWS                                 TRAILING SEPARATE VALUE 0.
NLB14M     03  WA-VA-AMT-S   REDEFINES  WA-VA-AMT.
NLB14M         05  FILLER                      PIC X(17).
NLB14M         05  WA-VA-AMT-SIGN              PIC X.
026240     03  WA-VA-AMT-0   REDEFINES  WA-VA-AMT.
026250         05  FILLER                      PIC X(14).
026260         05  WA-DECS-0                   PIC 999.
026270         05  FILLER                      PIC X.
026280     03  WA-VA-AMT-1   REDEFINES  WA-VA-AMT.
026290         05  FILLER                      PIC X(15).
026300         05  WA-DECS-1                   PIC 99.
026310         05  FILLER                      PIC X.
026320     03  WA-VA-AMT-2   REDEFINES  WA-VA-AMT.
026330         05  FILLER                      PIC X(16).
026340         05  WA-DECS-2                   PIC 9.
026350         05  FILLER                      PIC X.
026590/
026600     COPY UT0030CA IN LIBRYMIS.
026620*
026640 01  WA-12000-PROCESS-DATE               PIC S9(9) BINARY.
026650 01  WA-MAX-TERM                         PIC 9(6).
026680*
026690 01  WA-UBITES-AREA.
026700     03  WA-EDIT-MASK-TABLE.
026710         05  WA-EDIT-MASK-BIT            OCCURS 32
026720                                         PIC X.
026730     03  WA-UBITES-BITS                  PIC X(4).
026740*
026870 01  WA-BAD-DEBT-DESCRIPTION.
026880     03  FILLER                          PIC X(80).
026890     03  FILLER                          PIC X(80).
026900     03  FILLER                          PIC X(80).
026910     03  FILLER                          PIC X(80).
026920     03  FILLER                          PIC X(80).
026930     03  FILLER                          PIC X(80).
026940     03  FILLER                          PIC X(80).
TB0092     03  FILLER                          PIC X(80).
TB0092     03  FILLER                          PIC X(80).
TB0092     03  FILLER                          PIC X(80).
TB0092     03  FILLER                          PIC X(80).
TB0092     03  FILLER                          PIC X(80).
TB0258     03  FILLER                          PIC X(80).
027010 01  WA-BAD-DEBT-DESC-R REDEFINES WA-BAD-DEBT-DESCRIPTION.
TB0258     03  WA-BAD-DEBT-ITEMS OCCURS 13 TIMES PIC X(80).
027050/
LON327*027060 01  MS0010CA-REC.
LON327*027070     COPY MS0010CA IN LIBRYMIS.
TB0084/
LON327*TB0084 01  UT0075CA-CALL-AREA.
LON327*TB0084     COPY UT0075CA IN LIBRYBOR.
027110/
SC4083 01  UTHISTCA-CALL-PARAM.
SC4083     COPY UTHISTCA IN LIBRYBOR.
SC0592/
LON327*SC0592 01  UTGTRJCA-CALL-AREA.
LON327*SC0592     COPY UTGTRJCA IN LIBRYMIS.
TB0156/
027200 01  UT0078CA-CALL-PARAM.
027210     COPY UT0078CA IN LIBRYBOR.
027220/
TB0069 01  UT0094CA-CALL-PARAM.
TB0069     COPY UT0094CA IN LIBRYBOR.
TB0069/
027260 01  WA-UT0220-CALL-AREA.
027270     COPY UT0220CA IN LIBRYMIS.
027280     COPY INSPARM IN LIBRYMIS.
027290/
027300 01  WA-UT0280-CALL-AREA.
027310   02  WA-UT0280-CALL-CODE                 PIC 99.
027320   02  WA-UT0280-CALL-SOC-NO               PIC 999.
027330   02  WA-UT0280-CALL-USERCODE             PIC 99.
027340   02  WA-UT0280-CALL-STATUS               PIC XXXX.
027350   02  WA-UT0280-CALL-DATA.
027360       COPY USRPARM IN LIBRYMIS.
027370*
LON327*027380 01  WA-USERCODE-INDEX               PIC 9999 COMP VALUE 0.
027390/
027400 01  UT0298CA-CALL-AREA.
027410     COPY UT0298CA IN LIBRYMIS.
027420/
027430 01  UT0500-CALL-PARAM.
027440     COPY UT0500CA IN LIBRYMIS.
027450/
LON327*027460 01  UT0510CA-CALL-PARAM.
LON327*027470     COPY UT0510CA IN LIBRYBOR.
027480/
LON327*027490     COPY UT0671CA IN LIBRYMIS.
LON619*027500     COPY FACRMAST IN LIBRYMIS.
027510/
027520     COPY UT2000CA IN LIBRYBOR.
027530     03  WA-UT2000-CALL-REC.
S10046         05  FILLER                      PIC X(1800).
027560/
027570*
027580* THE FOLLOWING FIELD SHOULD BE THE SAME LENGTH AS BORPARM
027590*
S10046 01  WA-SAVE-BORPARM-AREA                PIC X(1800).
S10454 01  WA-SAVE-LOAN-CLASS                  PIC X.
027630 01  WA-SAVE-CAPN-METHOD                 PIC X.
027640 01  WA-SAVE-CAPN-FREQ-EOFY              PIC X.
027650 01  WA-SAVE-CAPN-FREQ                   PIC 99.
027660 01  WA-SAVE-RESIDUAL-IND                PIC X.
027670 01  WA-SAVE-ACCRUE-FREQ                 PIC 99.
027680 01  WA-SAVE-REPAY-FREQ                  PIC 99.
027690 01  WA-SAVE-INT-ADV                     PIC 99   VALUE 0.
027710 01  WA-SAVE-CURRENCY-IND                PIC X(3) VALUE SPACES.
S10237 01  WA-OLD-PRO-REPAY-DAY                PIC 99 COMP.
LON327*S10237 01  WA-NEW-LOAN-TERM                    PIC S9(5) COMP.
LON327*S10237 01  WA-OLD-LOAN-TERM                    PIC S9(5) COMP.
LON327*S10237 01  WA-DUE-START-DATE-BIN               PIC S9(9) BINARY.
027760 01  WA-SAVE-TOM-RATE-TBL-ID             PIC X(4) VALUE SPACES.
027770 01  WA-SAVE-REPAY-METHOD                PIC X    VALUE SPACES.
027780     88  WA-SAVE-SERIAL-REPAY-METHOD              VALUE "1" THRU
027790                                                        "9".
027800*
S13203 01  WA-CON-REQD-IND                     PIC X    VALUE SPACES.
S13203 01  WA-PROVISION                        PIC X    VALUE SPACES.
S13203 01  WA-COMMITMENT                       PIC X    VALUE SPACES.
S13203 01  WA-PL-NPL-APPL                      PIC X    VALUE SPACES.
S13203 01  WA-AUTO-RISK-IND                    PIC X    VALUE SPACES.
S13203 01  WA-MANUAL-RISK-IND                  PIC X    VALUE SPACES.
S13203 01  WA-INT-CAT-REQD                     PIC X    VALUE SPACES.
SC1580 01  WA-SAV-PREV-BAD-DEBT-TRF-DT         PIC S9(9) COMP.
FG1580 01  WA-SAV-PREV-BAD-DEBT-AMT            PIC S9(14)V999 COMP.
FG1580 01  WA-SAV-PREV-GL-INT-AMT              PIC S9(14)V999 COMP.
FG1580 01  WA-SAV-PREV-GL-PRIN-AMT             PIC S9(14)V999 COMP.
FG1580 01  WA-SAV-PREV-TOT-BAD-AMT             PIC S9(14)V999 COMP.
LON327*FG1580 01  WA-BAD-DEBT                   PIC S9(14)V999 COMP.
S11215 01  WA-SAVE-FINE-METH                   PIC 99.
S11215 01  WA-SAVE-FINE-CONDT                  PIC 99.
027920/
027930 01  UT9000-CALL-AREA.
027940     COPY UT9000CA IN LIBRYMIS.
027950/
ST0125 01  LS-MI0095CA.
ST0125     COPY MI0095CA IN LIBRYATM.
027980/
027990 01  LS-RFSH-AREA.
028000     COPY PASARFSH IN LIBRYMIS.
028010/
TB0104 01  WA-UT0014-CALL-CODE                 PIC 99.
TB0104/
TB0104 01  WA-UT0014-PASSALONG.
TB0104     COPY UT0014CA IN LIBRYBOR.
TB0104     03  IN0014-INPUT-MESSAGE.
TB0104         05  IN0014-INPUT-ACCT-NO        PIC 9(16).
TB0104         05  IN0014-INPUT-AMOUNT         PIC S9(14)V999
INITWS                                 SIGN TRAILING SEPARATE VALUE 0.
TB0104         05  IN0014-INPUT-FREQ           PIC 99.
TB0104         05  IN0014-INPUT-CODE           PIC X.
TB0104             88 VALID-INPUT-CODES        VALUES "D"
TB0104                                                "M"
TB0104                                                "E".
TB0104             88 DAILY-INPUT-CODE         VALUE "D".
TB0104             88 MONTHLY-INPUT-CODE       VALUE "M".
tb0104             88 EOM-INPUT-CODE           VALUE "E".
TB0104         05  IN0014-CHARGE-INDICATOR     PIC 9.
TB0104         05  IN0014-INPUT-CDATE          PIC 9(8).
TB0104         05  FILLER                      PIC X(8).
TB0104         05  IN0014-INPUT-RECACC-NO      PIC 9(17).
TB0104         05  IN0014-INPUT-REC-SYS        PIC X(3).
TB0104         05  IN0014-INPUT-COMMENTS       PIC X(25).
TB0104         05  FILLER                      PIC X(25).
TB0104         05  IN0014-INPUT-ADD-1          PIC X(40).
TB0104         05  FILLER                      PIC X(10).
TB0104         05  IN0014-INPUT-NAME           PIC X(32).
TB0104         05  IN0014-INPUT-INFORCE        PIC XXX.
TB0104         05  IN0014-INPUT-START-DATE     PIC 9(8).
TB0104         05  FILLER                      PIC X(6).
TB0104         05  IN0014-SWEEP-TRANS          PIC X.
TB0104         05  IN0014-FROM-ACC-CURRENCY    PIC XXX.
TB0104         05  IN0014-INPUT-SEC-CODE       PIC 9(4).
TB0104         05  IN0014-INPUT-AMT-CCY       PIC XXX.
TB0104         05  IN0014-CHANGE-AMOUNT        PIC S9(14)V999
INITWS                                 TRAILING SEPARATE VALUE 0.
TB0104         05  IN0014-CHANGE-FREQ          PIC 99.
TB0104         05  IN0014-CHANGE-CODE          PIC X.
TB0104         05  IN0014-INPUT-PRIORITY       PIC 99.
TB0104         05  IN0014-SWEEP-TRANS-NO       PIC 9(6).
TB0104         05  FILLER                      PIC X(6).
TB0104         05  IN0014-TO-ACC-COMMENTS      PIC X(25).
TB0104         05  FILLER                      PIC X(19).
TB0104         05  IN0014-INSURANCE-TRANS      PIC X.
TB0104         05  IN0014-CHANGE-END-DATE      PIC 9(8).
TB0104         05  IN0014-TO-ACC-CURRENCY      PIC XXX.
TB0104         05  IN0014-INPUT-RECNME         PIC X(40).
TB0104         05  IN0014-INPUT-ADD-2          PIC X(40).
TB0104         05  IN0014-INPUT-ADD-3          PIC X(40).
TB0104         05  FILLER                      PIC X(106).
TB0104         05  IN0014-FROM-ACC-SYSTEM      PIC XXX.
TB0104         05  IN0014-INPUT-TDATE          PIC 9(8).
TB0104         05  IN0014-CHARGE-AMOUNT        PIC S9(14)V999
INITWS                             SIGN TRAILING SEPARATE VALUE 0.
TB0104         05  FILLER                      PIC X(227).
TB0104         05  IN0014-INPUT-POST-CODE      PIC X(8).
TB0104         05  FILLER                      PIC X(90).
TB0104         05  IN0014-INPUT-BSB            PIC X(12).
TB0104         05  FILLER                      PIC X.
TB0104         05  IN0014-COMMON-CHASE-DAYS    PIC 9(2).
TB0104         05  IN0014-INPUT-PYMNT-TYPE     PIC X.
TB0104         05  IN0014-INPUT-PURPOSE        PIC X(5).
TB0255         05  IN0014-PROMO-CODE           PIC XX.
TB0255         05  FILLER                      PIC X(54).
TB0104/
TB0104******************************************************************
TB0104*
TB0104*    OUTPUT AREA FOR TRANSACTIONS
TB0104*
TB0104******************************************************************
TB0104   02    IN0014-OUTPUT-MESSAGE.
TB0104*
TB0104     03  IN0014-OUTPUT-ERR-MSG-NO        PIC 9(4).
TB0104     03  IN0014-OUTPUT-MSG-LENGTH        PIC 9(4).
TB0104     03  IN0014-OUTPUT-MSG               PIC X(983).
TB0104*
TB0104     03  IN0014-OUTPUT-MSG-RR  REDEFINES IN0014-OUTPUT-MSG.
TB0104         05  IN0014-OUTPUT-DATA-SET      PIC X(4).
TB0104         05  IN0014-OUTPUT-STATUS        PIC X(4).
TB0104         05  FILLER                      PIC X(975).
TB0104   02    IN0014-BORM-RECORD-AREA.
S12772     03  BORM-DATA-REC-WA                PIC X(984).
TB0104   02  IN0014-STATUS-LEVEL-1             PIC 99.
TB0104   02  IN0014-STATUS-LEVEL-2             PIC 99.
TB0104   02  IN0014-STATUS-LEVEL-3             PIC 99.
TB0104   02  IN0014-STATUS-LEVEL-4             PIC 99.
TB0104   02  IN0014-CHQ-CLEAR-DAYS             PIC 9(4).
TB0104   02  IN0014-NPB-CALL-CODE              PIC XX.
LON619*S12772   02  FILLER                            PIC X(915).
LON619   02  FILLER                            PIC X(916).
028920***************************************************************
028930***  DBIO
028940***************************************************************
028950*
029030     COPY UT0151CA IN LIBRYMIS.
029040*
LON327*029050     COPY UT0152CA IN LIBRYMIS.
029060/
029070     COPY DBIOCALL IN LIBRYMIS.
029080/
LON327*029090 01  WA-CUSV-OLD-IMAGE.
LON327*029100     COPY INVMAREA IN LIBRYINV.
029110 01  WA-BORM-OLD-IMAGE.
029120     COPY INVMAREA IN LIBRYINV.
LON327*029130 01  WA-BLDV-OLD-IMAGE.
LON327*029140     COPY INVMAREA IN LIBRYINV.
LON327*029150 01  WA-BINS-OLD-IMAGE.
LON327*029160     COPY INVMAREA IN LIBRYINV.
LON327*TB0027 01  WA-BOIS-OLD-IMAGE.
LON327*TB0027     COPY INVMAREA IN LIBRYINV.
029190 01  WA-AUDT-AREA.
029200     03  WA-AUDT-FUNCTION                PIC 9(02) VALUE 01.
029210     03  WA-AUDT-STATUS                  PIC X(04).
029220     03  WA-AUDT-RRN                     PIC 9(06).
029230     COPY AUDTHEADER IN LIBRYMIS.
029240         03  WA-AUDT-OLD-IMAGE.
029250             COPY INVMAREA IN LIBRYINV.
029260         03  WA-AUDT-NEW-IMAGE.
029270             COPY INVMAREA IN LIBRYINV.
029280*GZA 730538 THE FILLER OF 135 BELOW IS NOW STRUCTURED AS SHOWN
029290*TO ACCOMODATE  THE ABEX ACCT. EXTRACT INDICATOR
029300         03  FILLER                      PIC X(16).
029310         03  ABEX-AUDT-OLD               PIC X(1).
029320         03  ABEX-AUDT-NEW               PIC X(1).
029330         03  FILLER                      PIC X(117).
029340/
LON236*029350 01  BRTH-RECORD.
LON236*029360     COPY BRTHMAST IN LIBRYBOR.
029370/
032980******************************************************************
032990*                                                                *
033000*    MULTI LANGUAGE TEXT AREAS                                   *
033010*                                                                *
033020******************************************************************
033030*
033040 01  TXT-LOAD-FLAG                       PIC 9 VALUE 0.
033050     88  TXT-NOT-LOADED                        VALUE 0.
033060*
033070 01  TXT-TABLE.
033080     03  TXT-INST-TABLE OCCURS 2 INDEXED BY TXT-1.
033090         05  TXT-INST                    PIC 999.
033100         05  TXT-LANG-TABLE OCCURS 2 INDEXED BY TXT-2.
033110             07  TXT-LANG                PIC 99.
033120             07  TXT-DATA-AREA.
S11711                 09  TXT-LINE OCCURS 47 INDEXED BY TXT-3
033200                                         PIC X(40).
033210             07  FILLER REDEFINES TXT-DATA-AREA.
033220                 09  TXT-BAD-DEBT-1      PIC X(40).
033230                 09  TXT-BAD-DEBT-2      PIC X(40).
033240                 09  TXT-BAD-DEBT-3      PIC X(40).
033250                 09  TXT-BAD-DEBT-4      PIC X(40).
033260                 09  TXT-REPAYMENT       PIC X(40).
033270                 09  TXT-PDCC-WARNING    PIC X(40).
033280                 09  TXT-REM-REPAY       PIC X(40).
033290                 09  TXT-BAD-DEBT-5      PIC X(40).
033300                 09  TXT-BAD-DEBT-6      PIC X(40).
033310                 09  TXT-BAD-DEBT-7      PIC X(40).
TB0092                 09  TXT-BAD-DEBT-8      PIC X(40).
TB0092                 09  TXT-BAD-DEBT-9      PIC X(40).
TB0092                 09  TXT-BAD-DEBT-10     PIC X(40).
TB0092                 09  TXT-BAD-DEBT-11     PIC X(40).
TB0092                 09  TXT-BAD-DEBT-12     PIC X(40).
TB0258                 09  TXT-BAD-DEBT-13     PIC X(40).
033380                 09  TXT-USER-DESC-1     PIC X(40).
033390                 09  TXT-USER-DESC-2     PIC X(40).
033400                 09  TXT-USER-DESC-3     PIC X(40).
033410                 09  TXT-USER-DESC-4     PIC X(40).
033420                 09  TXT-USER-DESC-5     PIC X(40).
033430                 09  TXT-USER-DESC-6     PIC X(40).
033440                 09  TXT-USER-DESC-7     PIC X(40).
033450                 09  TXT-USER-DESC-8     PIC X(40).
033460                 09  TXT-USER-DESC-9     PIC X(40).
033470                 09  TXT-USER-DESC-10    PIC X(40).
S11218                 09  TXT-REASON          PIC X(40).
S11218                 09  TXT-TELLER          PIC X(40).
S11218                 09  TXT-LIM-FM-DESC     PIC X(40).
S11218                 09  TXT-LIM-TO-DESC     PIC X(40).
S11218                 09  TXT-OVERDUE-1       PIC X(40).
S11218                 09  TXT-OVERDUE-2       PIC X(40).
S11218                 09  TXT-OVERDUE-3       PIC X(40).
S11218                 09  TXT-OVERDUE-4       PIC X(40).
S11218                 09  TXT-OVERDUE-5       PIC X(40).
S11218                 09  TXT-OVERDUE-6       PIC X(40).
S11218                 09  TXT-OVERDUE-7       PIC X(40).
S11218                 09  TXT-OVERDUE-8       PIC X(40).
S11218                 09  TXT-OVERDUE-9       PIC X(40).
S11218                 09  TXT-YOUBE-STOP      PIC X(40).
S11323                 09  TXT-INST-PAY-ON     PIC X(40).
S11323                 09  TXT-INST-PAY-OFF    PIC X(40).
S11413                 09  TXT-CORRECT-ON      PIC X(40).
S11413                 09  TXT-CORRECT-OFF     PIC X(40).
S11711                 09  FILLER              PIC X(40).
S11711                 09  TXT-CARD-STOP       PIC X(40).
S11218                 09  FILLER              PIC X(40).
033700/
033720     COPY UT0099CA IN LIBRYMIS.
033730*
033740     COPY UT8500CA IN LIBRYMIS.
033750     COPY UT8500FU IN LIBRYMIS.

LON327*033780 01  UT0520CA-CALL-AREA.
LON327*033790     COPY UT0520CA IN LIBRYMIS.

LON327*033810 01  UTACHNCA-CALL-AREA.
LON327*033820     COPY UTACHNCA IN LIBRYMIS.
033830/
033840 01  UT1996CA-CALL-AREA.
033850     COPY UT1996CA IN LIBRYMIS.
033860     COPY UT1999CA IN LIBRYMIS.
033870*
033880 01  WA-TODAY-DATE    PIC S9(9) BINARY.
S11215*01  WA-NEXT-WORKDATE PIC S9(9) BINARY.
033900*01  WA-MAT-DT        PIC S9(9) BINARY VALUE 0.
033910*01  CUSV-BB-IND      PIC 9.
033920*
033930* 01  WA-POINTER.
033940*  03 WA-DBIO-SAVE     PIC X(4).
033950*  03 WA-DBIO-SAVE-R REDEFINES
033960*        WA-DBIO-SAVE  PIC S9(9) BINARY.

LON327*033980 01  WA-ACCEPT-TIME      PIC 9(8).
LON327*033990 01  WA-EFFECTIVE-TIME   PIC 9(8).
LON327*034000 01  WA-EFFE-DATE     PIC S9(9) BINARY.
034020/
LON327*034030 01  WA-STORE-SORT-NAME.
LON327*034080   03  WA-STORE-CHARACTER                PIC X.
LON327*034090   03  WA-SUB1                           PIC 99.
LON327*034100   03  WA-SUB2                           PIC 99.
LON327*034110   03  WA-BEGIN-FLG                      PIC X.
LON327*034240 01   J                                  PIC 99.
LON327*034250 01   I                                  PIC 99.
034260 01  WA-BORM-KEY.
034270     03 WA-BORM-SOC-NO                  PIC 999  VALUE ZEROES.
034280     03 WA-BORM-MEMB-CUST-AC            PIC 9(16) VALUE ZEROES.
LON327*034390 01  WA-LOAN-REPAY-AMT                  PIC S9(14)V999.
LON327*S11218 01  WA-STORE-RSN                       PIC X(2).
LON327*S11323 01  WA-TXTM-INST-TEXT                  PIC X(16).
S12756 01  WA-B4-BAD-DEBT-IND                 PIC 99.
S11711*
LON327*S11711 01  WA-CARD-STOP-TXTM.
LON327*S11711     03  WA-TXTM-TEXT2        PIC X(08) VALUE SPACES.
LON327*S11711     03  FILLER                          PIC X.
LON327*S11711     03  WA-CARD-STOP-FLAG               PIC X.
LON327*S11711     03  FILLER                          PIC X.
LON327*S11711     03  WA-CARD-STOP-REASON             PIC X(3).
LON327*S11711     03  FILLER                          PIC X.
LON327*S11711     03  WA-CARD-REASON-DESC             PIC X(20).
LON327*S11711 01  WA-STORE-CARD-STOP                  PIC X(1).
LON327*S11711 01  WA-STORE-CARD-RSN                   PIC X(3).
LON327*S11711 01  WA-UT9000-KEY.
LON327*S11711     03 WA-HEAD-2                    PIC X(2) VALUE 'FZ'.
LON327*S11711     03 WA-FZCODE                        PIC X(03).
034690*
LON327*034700 01  WA-CUSV-BB-AREA    PIC X(336).
034710*
034720 01  BORV-RECORD-AREA.
034730     COPY BORVVARV IN LIBRYBOR.
034740/
LON327*TB0072 01  SRCT-RECORD.
LON327*TB0072     COPY SRCTMAST IN LIBRYMIS.
TB0072*
LON327*TB0072 01  UTCRINCA-CALL-PARAM.
LON327*TB0072     COPY UTCRINCA IN LIBRYMIS.
TB0072*
LON327*TB0072 01  UTAPPOCA-CALL-PARAM.
LON327*TB0072     COPY UTAPPOCA IN LIBRYMIS.
TB0072*
TB0072 01  UT0312CA-CALL-AREA.
TB0072     COPY UT0312CA IN LIBRYBOR.
TB0072/
LON327*034900 01  TEMP-ABEX-IND       PIC X.
034910*
LON327*034920 01  BAID-RECORD.
LON327*034930     COPY BAIDMAST IN LIBRYBOR.
TB0007/
LON327*TB0007 01  AORM-RECORD.
LON327*TB0007     COPY AORMMAST IN LIBRYMIS.
LON327*S11323 01  YBAM-RECORD-AREA.
S11323* COPY YBAMMAST IN LIBRYBOR REPLACING ==(YBAM)== BY ==YBAM==.
TB0069/
LON327*TB0069 01  WA-BUCV-AREA.
LON327*TB0069     COPY BUCVVARV   IN LIBRYBOR.
LON327*TB0069     COPY BUCVALT    IN LIBRYBOR.
LON327*TB0069     COPY BUCVALT2   IN LIBRYBOR.
TB0069/
LON327*035050 COPY UT0025CA IN LIBRYMIS.
LON327*S10093 01  GLDM-RECORD-AREA.
LON327*S10093     COPY GLDMMAST IN LIBRYGEN.
TB0211/
TB0211 01 BAD-DEBT-VAR.
TB0211     03 WA-BAD-DBT-IND              PIC 99.
SC1456        88 VALID-IND-BEFORE-NPL     VALUE 00 01
SC1456                                          50 51.
SC1456        88 VALID-IND-BEFORE-PAR-BAD VALUE 02 03
SC1456                                          52 53.
SC1456        88 VALID-IND-BEFORE-BAD     VALUE 02 03
SC1456                                          52 53.
SC1456     03 WA-ALL-DEBT-AMOUNT          PIC S9(14)V999 COMP.
SC1456     03 WA-PRIN-TOTAL               PIC S9(14)V999 COMP.
SC1456     03 WA-INT-TOTAL                PIC S9(14)V999 COMP.
TB0211     03 WA-BAD-DBT-IDX              PIC 99.
TB0211     03 WA-BAD-DBT-CORR-IND         PIC X.
TB0211     03 WA-INT-PRIN-TOTAL           PIC S9(14)V999 COMP.
SC3009     03 WA-MANUAL-BD-IND            PIC 99.
SC3009        88 VALID-IND-MAN            VALUE 50 51 00
SC3009                                          52 53
SC3009                                          54 59
S14340                                          60
ELJAI                                           02 03.
SC8804     03 WA-BAD-DEBT-INDICATOR       PIC 99.
SC8804     03 WA-BAD-DEBT-ACCT-NO         PIC 9(16).
S11908     03 WS-INT-BUCKET-FLAG          PIC X.
S11908        88  WS-INT-BUCKET-NOT-FOUND      VALUE "N".
S11908        88  WS-INT-BUCKET-FOUND          VALUE "Y".
S11908     03 WS-PRIN-BUCKET-FLAG         PIC X.
S11908        88  WS-PRIN-BUCKET-NOT-FOUND     VALUE "N".
S11908        88  WS-PRIN-BUCKET-FOUND         VALUE "Y".
S11908     03 WS-BD-PRIN-BUCKET-FLAG      PIC X.
S11908        88  WS-BD-PRIN-BUCKET-NOT-FOUND  VALUE "N".
S11908        88  WS-BD-PRIN-BUCKET-FOUND      VALUE "Y".
S11908     03 WS-BD-INT-BUCKET-FLAG      PIC X.
S11908        88  WS-BD-INT-BUCKET-NOT-FOUND   VALUE "N".
S11908        88  WS-BD-INT-BUCKET-FOUND       VALUE "Y".
S11908     03 WS-INT-PORTION              PIC S9(14)V999 COMP.
S11908     03 WS-PRIN-PORTION             PIC S9(14)V999 COMP.
TB0159/
S12749 01  WS-S12749-VARS.
S12749     03  WS-MTHS-IN-ARREARS         PIC 9(4).
S12749     03  WS-POSTING-DATE.
S12749         05  WS-POSTING-DATE-DD     PIC 9(02).
S12749         05  WS-POSTING-DATE-MM     PIC 9(02).
S12749         05  WS-POSTING-DATE-CY     PIC 9(04).
S12749     03  WS-LAST-UNPAID-DUE-DATE.
S12749         05  WS-UNPAID-DUE-DATE-DD  PIC 9(02).
S12749         05  WS-UNPAID-DUE-DATE-MM  PIC 9(02).
S12749         05  WS-UNPAID-DUE-DATE-CY  PIC 9(04).
S12749     03  WS-DATE-DIFF               PIC 9(7).
S12749     03  WS-YEAR-DIFF               PIC 9(6).
S12749     03  WS-MTHS-DIFF               PIC 9(6).
S12749     03  WS-DAYS-DIFF               PIC 9(2).
S12749     03  WS-DATE-DEC                PIC 9(4).
LON327*S12787 01  WA-BUCKETS                     PIC 9(2) VALUE 1.
S12787 01  WA-TXN                         PIC 9(6).
S12787 01  WA-GL-TXN-NBR                  PIC 9(6).
S12787 01  W900-TXN-AMOUNT                PIC S9(14)V999  COMP.
S12787 01  UT0107CA-CALL-PARAM.
S12787     COPY UT0107CA IN LIBRYBOR.
S12787 01  RPYV-RECORD-AREA.
S12787     COPY RPYVVARV IN LIBRYBOR.

035750/===========================================================
INITWS
INITWS 01 WS-INIT-CPBOOK-VAR            PIC X VALUE "Y".
INITWS 01 WA-INITWS-LOOP-IDX            PIC 9(5) VALUE 0.
INITWS
035760 LINKAGE SECTION.
035770*--------------

035790 01  STARTAREA.
035800     COPY STRTAREA IN LIBRYMIS.
035810     03  BR0001-INPUT-MESSAGE.
035820         05  BR0001-INPUT-KEY            PIC X(16).
035830         05  BR0001-INPUT-KEY-R REDEFINES
035840             BR0001-INPUT-KEY            PIC 9(16).
035860         05  BR0001-CUSTOMER-NO          PIC 9(16).
S13299         05  BR0001-12000-DETS           PIC X(938).
036110         05  BR0001-LAST-12              PIC X(12).
LON236*---------------------------------------------------------------
LON236* USE WA-INPUT-AMOUNT
LON236*---------------------------------------------------------------
S10093     03 WA-INPUT-MSG REDEFINES BR0001-INPUT-MESSAGE.
S10093         05  WA-INPUT-KEY.
S10093             07  WA-INPUT-ACCT-NO        PIC 9(16).
S10093         05  WA-INPUT-KEY-R REDEFINES WA-INPUT-KEY    PIC 9(16).
S10093         05  FILLER  REDEFINES  WA-INPUT-KEY.
S10093           07  FILLER                    PIC XXX.
S10093           07  WA-INPUT-KEY-BRANCH       PIC 9(5).
S10093           07  FILLER                    PIC X(8).
S10093         05  WA-INPUT-AMOUNT            PIC S9(14)V999
S10093                                     SIGN IS TRAILING SEPARATE.
S10093         05  FILLER                      PIC X(267).
S10093         05  WA-ATM-JRNL-NO              PIC 9(9).
S10093         05  WA-ATM-UPDT-FLAG            PIC X.
S10093         05  WA-OUTPUT-TXN-DATE          PIC XX.
S10093         05  WA-OUTPUT-CLEAR-CODE        PIC XX.
S10093         05  FILLER                      PIC X(658).
S10093         05  WA-INPUT-MSG-CODE           PIC 99.
S10093         05  FILLER                      PIC X(7).
LON236*---------------------------------------------------------------
LON236* FOR 17053 BAD DEBT
LON236*---------------------------------------------------------------
S10093     03  BR0001-17053-BAD-DEBT REDEFINES BR0001-INPUT-MESSAGE.
S10093         05 BR0001-17053-ACCT-NO         PIC X(16).
S10093         05 BR0001-17053-TYPE            PIC X.
S10093            88 BR0001-17053-CONTRACT  VALUE "C".
S10093            88 BR0001-17053-ACCOUNT   VALUE "A".
S10093         05 FILLER                       PIC X(965).
LON236*---------------------------------------------------------------
LON236* 17050 OPTIONAL 1 2 3
LON236*---------------------------------------------------------------
037810     03  BR0001-SH-INPUT REDEFINES BR0001-INPUT-MESSAGE.
037820         05  BR0001-SH-INPUT-KEY         PIC 9(16).
037830         05  BR0001-SH-ENQ-TYPE          PIC X.
LON236             88 BR0001-SH-ENQ-TYPE-VALUE VALUE
LON236                                        "1" "2" "3".
S10108         05  BR0001-SH-ACCT-INFO-TYPE    PIC 9.
S10108         05  FILLER                      PIC X(964).
LON236*----------------------------------------------------------
LON236* 17003 17053 FOR WRITE-OFF
LON236*----------------------------------------------------------
TB0211     03  BR0001-17003-TRAN REDEFINES BR0001-INPUT-MESSAGE.
TB0211         05  WA-17003-CORR-TRAN-CD       PIC 9(6).
TB0211         05  FILLER                      PIC X(976).
038280     03  BR0001-BAD-DEBT REDEFINES BR0001-INPUT-MESSAGE.
TB0092         05  BR0001-BAD-DEBT-ACCT-NO     PIC 9(16).
S10093         05  BR0001-BAD-DEBT-TYPE        PIC X(1).
S10093            88 BR0001-BAD-DEBT-CONTRACT  VALUE "C".
S10093            88 BR0001-BAD-DEBT-ACCOUNT   VALUE "A".
TB0092         05  BR0001-BAD-DEBT-NAME        PIC X(60).
038350         05  BR0001-BAD-DEBT-IND         PIC 99.
S14015*        05  BR0001-BAD-DEBT-DESC        PIC X(30).
S14015         05  BR0001-BAD-DEBT-DESC        PIC X(40).
S14015         05  FILLER                      PIC X(438).
TB0092         05  BR0001-BAD-DEBT-REASON-CODE PIC 9(04).
TB0105         05  BR0001-BAD-DEBT-REASON-2 REDEFINES
TB0105                             BR0001-BAD-DEBT-REASON-CODE.
TB0105             10 BR0001-BAD-DEBT-R1       PIC 9(02).
TB0105             10 BR0001-BAD-DEBT-R2       PIC 9(02).
TB0092         05  BR0001-BAD-DEBT-NARRATIVE   PIC X(30).
TB0092         05  BR0001-BAD-DEBT-DATE.
TB0092             10 BR0001-BAD-DEBT-DATE-DD  PIC 9(02).
TB0092             10 BR0001-BAD-DEBT-DATE-MM  PIC 9(02).
TB0092             10 BR0001-BAD-DEBT-DATE-CC  PIC 9(02).
TB0092             10 BR0001-BAD-DEBT-DATE-YY  PIC 9(02).
S12727         05  BR0001-BAD-DEBT-JRNL-NO     PIC 9(09).
FG1580         05  BR0001-BAD-DEBT-AMOUNT      PIC S9(14)V999
FG1580                                         TRAILING SEPARATE.
S12749         05  BR0001-ARRS-REASON-CD       PIC X(02).
S12749         05  BR0001-ASSET-RATING         PIC 9.
S12749         05  BR0001-WRITE-OFF-APRV-DT    PIC 9(08).
S12749         05  BR0001-WRITE-OFF-APRV-LEVEL-1 PIC 9(03).
S12749         05  BR0001-WRITE-OFF-APRV-LEVEL-2 PIC 9(02).
S12749         05  BR0001-WRITE-OFF-APRV-LEVEL-3 PIC 9(02).
S12727         05  FILLER                      PIC X(338).
LON236*--------------------------------------------------------
LON236* 17465 SCREEN
LON236*--------------------------------------------------------
038820     03  BR0001-NAME-AREA REDEFINES BR0001-INPUT-MESSAGE.
038830         05  BR0001-N-INPUT-KEY       PIC 9(16).
038840         05  BR0001-SHORT-NAME        PIC X(60).
038850         05  BR0001-S-DEL-FLAG        PIC X.
038860         05  BR0001-LONG-NAME.
038870             10  BR0001-LONG-NAME1    PIC X(60).
038880             10  BR0001-LONG-NAME2    PIC X(60).
038890             10  BR0001-LONG-NAME3    PIC X(60).
038900             10  BR0001-LONG-NAME4    PIC X(60).
038910             10  BR0001-LONG-NAME5    PIC X(60).
038920             10  BR0001-LONG-NAME6    PIC X(60).
038930             10  BR0001-LONG-NAME7    PIC X(60).
038940             10  BR0001-LONG-NAME8    PIC X(60).
038950         05  BR0001-L-DEL-FLAG        PIC X.
038960         05  FILLER                    PIC X(424).
LON236*----------------------------------------------------------------
LON236* FOR 17046 CHANGE PRODUCT
LON236*----------------------------------------------------------------
039210     03  BR0001-IN-17046 REDEFINES BR0001-INPUT-MESSAGE.
039220         05  FILLER                          PIC X(16).
039230         05  BR0001-IN-17046-NAME            PIC X(60).
COR003         05  BR0001-IN-17046-OLD-BRANCH      PIC 99999.
039260         05  BR0001-IN-17046-CURRENCY        PIC XXX.
039270         05  BR0001-IN-17046-OLD-ACCT-TYPE   PIC 9999.
039280         05  BR0001-IN-17046-DESC            PIC X(30).
039290         05  BR0001-IN-17046-OLD-ACCT-CAT    PIC 9999.
COR003         05  BR0001-IN-17046-NEW-BRANCH      PIC 99999.
039320         05  BR0001-IN-17046-NEW-ACCT-TYPE   PIC 9999.
039330         05  BR0001-IN-17046-NEW-ACCT-CAT    PIC 9999.
039340         05  BR0001-IN-17046-TOM-ID          PIC X(4).
SC2451         05  BR0001-IN-17046-IND             PIC 9.
SC4108         05  BR0001-IN-17046-SYS             PIC X(3).
S10842         05  BR0001-IN-17046-RATE            PIC S999V9999
S10842                                         TRAILING SEPARATE.
S10842         05  BR0001-IN-17046-STRETCH-TERM    PIC 9999.
COR003         05  FILLER                          PIC X(827).
LON236*--------------------------------------------------------------
LON236* 17099 17475 17476 17477 FOR LOAN RENEWAL
LON236*--------------------------------------------------------------
S13708     03  BR0064-IN-17475 REDEFINES BR0001-INPUT-MESSAGE.
S13708         05 INP-17475-ACCOUNT-NO           PIC X(16).
S13708         05 INP-17475-CUST-NO              PIC X(17).
S13708         05 INP-17475-INP-ID-NO            PIC X(32).
S13708         05 INP-17475-INP-TYPE             PIC X(02).
S13708         05 INP-17475-NAME-1               PIC X(60).
S13708         05 INP-17475-CURR-MAT-DT          PIC 9(8).
S13708         05 INP-17475-NEW-MAT-DT           PIC 9(8).
S13708         05 INP-17475-FAC-ACCT-LIM         PIC S9(14)V999
S13708                                            TRAILING SEPARATE.
S13708         05 INP-17475-ORIG-IN-CAT          PIC 9(4).
S13708         05 INP-17475-NEW-IN-CAT           PIC 9(4).
S13708         05 INP-17475-EXT-AMOUNT           PIC S9(14)V999
S13708                                            TRAILING SEPARATE.
S13708         05 INP-17475-EXT-REQ-STAT         PIC X.
S13708         05 INP-17475-OPTION               PIC X.
S13708            88 WA-OPTION-AMEND             VALUE "A".
S13708            88 WA-OPTION-CREATE            VALUE "C".
S13708            88 WA-OPTION-DELETE            VALUE "D".
S13708            88 WA-OPTION-ENQUIRE           VALUE "E".
S13708         05 INP-17475-CURR-LOAN-TERM       PIC S9(5)
S13708                                            TRAILING SEPARATE.
S13708         05 INP-17475-ACCT-TYPE            PIC 9(4).
S13708         05 INP-17475-FIX-RATE             PIC S999V9(4)
S13708                                            TRAILING SEPARATE.
S13708         05 INP-17475-RATE-INCR            PIC S99V9(5)
S13708                                            TRAILING SEPARATE.
S13708         05 INP-17475-RATE-OPTION          PIC X.
S13708            88  WA-RATE-VALID2              VALUE "1" "2".
S13708            88  WA-RATE-FLAT                VALUE "1".
S13708            88  WA-RATE-PERCENT             VALUE "2".
S13708         05 INP-17475-CURR-LON-BAL         PIC S9(14)V999
S13708                                            TRAILING SEPARATE.
S13708         05 INP-17475-REPAY-DAY            PIC 99.
S13708         05 INP-17475-NEW-ACCT-TYPE        PIC 9(4).
LON032         05 INP-17475-NEW-LOAN-TERM        PIC S9(5)
LON032                                            TRAILING SEPARATE.
S13708         05 FILLER                         PIC X(736).
S13708     03  BR0064-IN-17474 REDEFINES BR0001-INPUT-MESSAGE.
S13708         05 INP-17474-ACCOUNT-NO           PIC X(16).
S13708         05 INP-17474-ACTION               PIC X(1).
S13708         05 FILLER                         PIC X(965).
039440     03  BR0001-IN-ACHN-DET REDEFINES BR0001-INPUT-MESSAGE.
039450         05 BR0001-IN-ACHN-ACCT-NO           PIC 9(16).
039460         05 BR0001-IN-ACHN-SYSTEM            PIC X(3).
039470         05 BR0001-IN-ACHN-CHANNEL           PIC X(5).
039480         05 FILLER                           PIC X(958).
LON236*-------------------------------------------------------
LON236* 17047 LOAN RENEWAL SCREEN USE IT TO PASS VALUE
LON236*-------------------------------------------------------
LON236*-------------------------------------------------------
LON236* 17035 LOAN RENEWAL MAIN SCREEN
LON236*-------------------------------------------------------
TB0105     03  BR0001-IN-17035 REDEFINES BR0001-INPUT-MESSAGE.
040730         05 BR0001-17035-INPUT-KEY           PIC X(16).
TB0105         05 BR0001-17035-CUST-NO             PIC X(16).
S10258         05 BR0001-17035-INP-ID              PIC X(32).
TB0105         05 BR0001-17035-INP-ID-TYPE         PIC X(02).
TB0105         05 BR0001-17035-NAME-1              PIC X(60).
TB0105         05 BR0001-17035-CURR-MAT-DT         PIC 9(8).
TB0105         05 FILLER                           PIC 9(8).
040820         05 BR0001-17035-NEW-MAT-DT          PIC 9(8).
040830         05 BR0001-17035-SUP-OVRD            PIC X(10).
S10258         05 FILLER                           PIC X(822).
LON236*-------------------------------------------------------
LON236* 10915 10916 FOR AUTO LOANS ADVANCE
LON236*-------------------------------------------------------
TB0255     03  BR0001-IN-10915 REDEFINES BR0001-INPUT-MESSAGE.
TB0255         05  BR0001-10915-ACCT-NO        PIC 9(16).
TB0255         05  BR0001-10915-AMOUNT         PIC S9(14)V999
TB0255                                         TRAILING SEPARATE.
TB0255         05  BR0001-10915-FREQ           PIC 99.
TB0255         05  BR0001-10915-FREQ-CODE      PIC X(1).
TB0255         05  BR0001-10915-START-DATE     PIC 9(8).
TB0255         05  BR0001-10915-END-DATE       PIC 9(8).
TB0255         05  BR0001-10915-TRF-ACCT-NO    PIC 9(17).
TB0255         05  BR0001-10915-ACCT-SYSTEM    PIC X(3).
TB0255         05  BR0001-10915-COMMENTS       PIC X(25).
TB0255         05  BR0001-10915-SEC-CODE       PIC 9(4).
TB0255         05  BR0001-10915-CURR-CODE      PIC X(3).
TB0255         05  BR0001-10915-PRTY-CODE      PIC 9(2).
TB0255         05  BR0001-10915-SNAME          PIC X(15).
TB0255         05  BR0001-10915-FNAME          PIC X(25).
TB0255         05  BR0001-10915-BSB-NO         PIC 9(6).
TB0255         05  BR0001-10915-PROMO-CD       PIC XX.
TB0255         05  FILLER                      PIC X(827).
LON236*-------------------------------------------------------
TB0255     03  BR0001-IN-10916 REDEFINES BR0001-INPUT-MESSAGE.
TB0255         05  BR0001-1016-ACCT-NO         PIC 9(16).
TB0255         05  BR0001-10916-AMOUNT         PIC S9(14)V999
TB0255                                         TRAILING SEPARATE.
TB0255         05  BR0001-10916-FREQ           PIC 99.
TB0255         05  BR0001-10916-CODE           PIC X(1).
TB0255         05  BR0001-10916-START-DATE     PIC 9(8).
TB0255         05  BR0001-10916-END-DATE       PIC 9(8).
TB0255         05  FILLER                      PIC X.
TB0255         05  BR0001-10916-TRF-NO         PIC 9(17).
TB0255         05  BR0001-10916-REC-SYS        PIC X(3).
TB0255         05  BR0001-10916-COMMENTS       PIC X(25).
TB0255         05  BR0001-10916-SEC-CODE       PIC 9(4).
TB0255         05  BR0001-10916-CURR-CODE      PIC X(3).
TB0255         05  BR0001-10916-BSB-NO         PIC 9(6).
TB0255         05  BR0001-10916-PROMO-CD       PIC XX.
TB0255         05  BR0001-10916-PRTY-CODE      PIC 9(2).
TB0255         05  FILLER                      PIC X(866).
LON236*-------------------------------------------------------
041780  02 BR0001-OUTPUT-AREA.
041790     03  BR0001-OUTPUT-ERR-MSG-NO         PIC 9999.
041800     03  BR0001-OUTPUT-MSG-LENGTH         PIC 9999.
041810     03  BR0001-OUTPUT-MSG.
041820         05  BR0001-OUTPUT-DATA-SET       PIC X(4).
041830         05  BR0001-OUTPUT-STATUS         PIC X(4).
041840         05  FILLER                       PIC X(975).
S10093     03  WA-DB-ERR-OUTPUT REDEFINES BR0001-OUTPUT-MSG.
S10093         05  WA-DBERR-DATA-SET           PIC X(4).
S10093         05  WA-DB-ERROR                 PIC X(4).
S10093         05  FILLER                      PIC X(975).
TB0165*************************************************************
044190     03  BR0001-OUTPUT-BAD-DEBT REDEFINES BR0001-OUTPUT-MSG.
044200         05  BR0001-OUT-BAD-DEBT-ACCT-NO PIC 9(17).
S10093         05  BR0001-OUT-BAD-DEBT-TYPE    PIC X(1).
044220         05  BR0001-OUT-BAD-DEBT-NAME    PIC X(60).
044230         05  BR0001-OUT-BAD-DEBT-IND     PIC 99.
S14015*        05  BR0001-OUT-BAD-DEBT-DESC    PIC X(30).
S14015         05  BR0001-OUT-BAD-DEBT-DESC    PIC X(40).
S14015         05  FILLER                      PIC X(438).
TB0092         05  BR0001-OUT-BAD-REASON-CODE  PIC 9(04).
TB0092         05  BR0001-OUT-BAD-NARRATIVE    PIC X(30).
TB0092         05  BR0001-OUT-BAD-DATE.
TB0092             10 BR0001-OUT-BAD-DATE-DD   PIC 9(02).
TB0092             10 BR0001-OUT-BAD-DATE-MM   PIC 9(02).
TB0092             10 BR0001-OUT-BAD-DATE-CC   PIC 9(02).
TB0092             10 BR0001-OUT-BAD-DATE-YY   PIC 9(02).
S12727*TB0092         05  BR0001-OUT-BAD-JRNL-NO      PIC 9(06).
S12727         05  BR0001-OUT-BAD-JRNL-NO      PIC 9(09).
FG1580         05  BR0001-OUT-BAD-AMOUNT       PIC S9(14)V999
FG1580                                         TRAILING SEPARATE.
S12749         05  BR0001-OUT-ARRS-REASON-CD   PIC X(02).
S12749         05  BR0001-OUT-ASSET-RATING     PIC X.
S12749         05  BR0001-OUT-WRIT-OFF-APRV-DT PIC 9(08).
S12693         05  BR0001-OUT-WRIT-OFF-LVL-1   PIC X(03).
S12749         05  BR0001-OUT-WRIT-OFF-LVL-2   PIC 9(02).
S12749         05  BR0001-OUT-WRIT-OFF-LVL-3   PIC 9(02).
S12727         05  FILLER                      PIC X(338).
045020     03  BR0001-OUTPUT-NAMES     REDEFINES BR0001-OUTPUT-MSG.
045030         05  BR0001-OUT-LSN-ACCT-NO      PIC 9(17).
045040         05  BR0001-OUT-SHORT-NAME       PIC X(60).
045050         05  BR0001-OUT-DEL-1            PIC X.
045060         05  BR0001-OUT-LONG-NAME.
045070             10  BR0001-OUT-LONG-NAME1   PIC X(60).
045080             10  BR0001-OUT-LONG-NAME2   PIC X(60).
045090             10  BR0001-OUT-LONG-NAME3   PIC X(60).
045100             10  BR0001-OUT-LONG-NAME4   PIC X(60).
045110             10  BR0001-OUT-LONG-NAME5   PIC X(60).
045120             10  BR0001-OUT-LONG-NAME6   PIC X(60).
045130             10  BR0001-OUT-LONG-NAME7   PIC X(60).
045140             10  BR0001-OUT-LONG-NAME8   PIC X(60).
045150         05  BR0001-OUT-DEL-2            PIC X.
045160         05  FILLER                      PIC X(424).
045520     03  BR0001-OUT-17046 REDEFINES BR0001-OUTPUT-MSG.
045530         05  BR0001-OUT-17046-ACCT           PIC 9(17).
045540         05  BR0001-OUT-17046-NAME           PIC X(60).
COR003         05  BR0001-OUT-17046-OLD-BRANCH     PIC 99999.
045570         05  BR0001-OUT-17046-CURRENCY       PIC XXX.
045580         05  BR0001-OUT-17046-OLD-ACCT-TYPE  PIC 9999.
045590         05  BR0001-OUT-17046-DESC           PIC X(30).
045600         05  BR0001-OUT-17046-OLD-ACCT-CAT   PIC 9999.
COR003         05  BR0001-OUT-17046-NEW-BRANCH     PIC 99999.
045630         05  BR0001-OUT-17046-NEW-ACCT-TYPE  PIC 9999.
045640         05  BR0001-OUT-17046-NEW-ACCT-CAT   PIC 9999.
045650         05  BR0001-OUT-17046-TOM-ID         PIC X(4).
SC2451         05  BR0001-OUT-17046-IND            PIC 9.
S10842         05  BR0001-OUT-17046-SYS            PIC X(3).
S10842         05  BR0001-OUT-17046-RATE           PIC S999V9999
S10842                                         TRAILING SEPARATE.
S10842         05  BR0001-OUT-17046-STRETCH-TERM   PIC 9999.
COR003         05  FILLER                          PIC X(827).

046450     03  BR0001-DISCH-OUTPUT-MSG REDEFINES BR0001-OUTPUT-MSG.
046460         05  BR0001-OUTPUT-TRAN-ACCT-NO      PIC 9(16).
NR0001         05  BR0001-OUTPUT-TRANS-AREA        PIC X(102).
046490         05  BR0001-OUTPUT-OK-MSG REDEFINES
046500             BR0001-OUTPUT-TRANS-AREA.
046510             07  BR0001-OUTPUT-OK-NAME-1     PIC X(35).
NR0001             07  FILLER                      PIC X(67).
046540         05  FILLER                          PIC X(78).
046550         05  BR0001-OUTPUT-TRAN-CURR         PIC X(3).
NR0001         05  FILLER                          PIC X(784).
LON236*--------------------------------------------------------------
LON236* 17035 FOR LOAN RENEWAL
LON236*--------------------------------------------------------------
TB0105     03  BR0001-OUT-17035 REDEFINES BR0001-OUTPUT-MSG.
LON236*046920         05 BR0001-OUT-17035-INPUT-KEY     PIC X(16).
LON236*TB0105         05 BR0001-OUT-17035-CUST-NO       PIC X(16).
LON236         05 BR0001-OUT-17035-INPUT-KEY     PIC X(17).
LON236         05 BR0001-OUT-17035-CUST-NO       PIC X(17).
S10258         05 BR0001-OUT-17035-INP-ID        PIC X(32).
TB0105         05 BR0001-OUT-17035-INP-ID-TYPE   PIC X(02).
TB0105         05 BR0001-OUT-17035-NAME-1        PIC X(60).
TB0105         05 BR0001-OUT-17035-CURR-MAT-DT   PIC 9(8).
TB0105         05 FILLER                         PIC 9(8).
047010         05 BR0001-OUT-17035-NEW-MAT-DT    PIC 9(8).
047020         05 BR0001-OUT-17035-SUP-OVRD      PIC X(10).
S10258         05 FILLER                         PIC X(821).
LON236* FOR LOAN RENEWAL
047150     03  BR0001-OUT-JRNL REDEFINES BR0001-OUTPUT-MSG.
047160            05 FILLER                    PIC X(172).
047170            05 O17047-JRNL-STAT          PIC X.
047180            05 FILLER                    PIC X(810).
047700*
047710     02  BORM-RECORD-AREA.
047720     COPY BORMMAST IN LIBRYBOR.
047730/
047740     COPY STRTREST IN LIBRYMIS.
S10093/
047810 01  LS01-BORR-PARAMS.
047820     COPY BORPARM IN LIBRYBOR.
047830/
047840 01  LS01-UT0290CA-CALL-AREA.
047850     COPY UT0290CA IN LIBRYMIS.
047860/
047870 01  BR0001-CALL-AREA.
047880     COPY BR0001CA IN LIBRYBOR.
047890/
047900
047910 01  MULTI-OVERRIDE-AREA.
047920     COPY MULTIOVR IN LIBRYMIS.
TB0027/
TB0027 01  BOIS-RECORD-AREA.
TB0027     COPY BOISMAST IN LIBRYBOR.
047960/
047970*
SC1959 01  UT0073CA-CALL-AREA.
SC1959     COPY UT0073CA IN LIBRYBOR.

S11778 01  BR0032CA-CALL-PARAM.
S11778     COPY BR0032CA IN LIBRYBOR.
S10093 01  CONB-RECORD-AREA.
S10093     COPY CONBMAST IN LIBRYMIS.

048060 PROCEDURE DIVISION  USING STARTAREA
048070                           LS01-BORR-PARAMS
048080                           LS01-UT0290CA-CALL-AREA
048090                           BR0001-CALL-AREA
TB0027                           MULTI-OVERRIDE-AREA
TB0027                           BOIS-RECORD-AREA
SC1959                           UT0073CA-CALL-AREA
S17778                           BR0032CA-CALL-PARAM
S10093                           CONB-RECORD-AREA.
INITWS
INITWS A000-INIT-CPBOOK-VAR.
INITWS     IF WS-INIT-CPBOOK-VAR = "Y"
INITWS        PERFORM Z999-INIT-CPBOOK-VAR THRU Z999-INIT-CPBOOK-EXIT
INITWS     END-IF.
INITWS
048160 A000-MAIN-LOGIC.

TB0143     PERFORM A400-INIT-MISC THRU A409-EXIT.
048190     IF  WA-FIRST-TIME-FLAG = 0
048200         MOVE 1 TO WA-FIRST-TIME-FLAG
048210         PERFORM A200-TODAY-DATE THRU A209-EXIT
048230         IF  TXT-NOT-LOADED
048240             MOVE UT8500CA-FUNC-NORMAL-DISPLAY TO
048250                  UT8500CA-CALL-CODE
048260             MOVE SPACES TO UT8500CA-INPUT-MSG
048270             STRING "BR0001:(A000) LOADING TXTM NOW "
048280                    DELIMITED BY SIZE INTO UT8500CA-INPUT-MSG
048290             PERFORM UT8500-CALL THRU UT8500-EXIT
048300             PERFORM A100-LOAD-TXT THRU A109-EXIT
048310             MOVE ZERO           TO UT0220CA-CALL-STATUS
048320                                    UT0220CA-CALL-SOC-NO
048330                                    UT0220CA-CALL-GRP-NO
048340             MOVE 1 TO WA-UT0280-CALL-CODE
048350             CALL "UT0280" USING

048370                           WA-UT0280-CALL-AREA
048380             MOVE 1 TO TXT-LOAD-FLAG
048400             GO TO A099-EXIT
048410         ELSE
048420             MOVE ZERO           TO UT0220CA-CALL-STATUS
048430                                    UT0220CA-CALL-SOC-NO
048440                                    UT0220CA-CALL-GRP-NO
048450             MOVE 1 TO WA-UT0280-CALL-CODE
048460             CALL "UT0280" USING

048480                           WA-UT0280-CALL-AREA
048510             GO TO A099-EXIT
LON236         END-IF
LON236     END-IF.
048520*
048530     MOVE STRT-AREA-PROCESS-BITS  TO WA-UBITES-BITS.
048540     CALL "UT0022"                USING WA-UBITES-AREA.
048550*
048560     IF  STRT-AREA-SOC-NO = UT0220CA-CALL-SOC-NO
048570     AND UT0220CA-CALL-GRP-NO = 1
S13569         GO  TO A030
LON236     END-IF.
048610*
048620     MOVE 02 TO UT0220CA-CALL-CODE.
048630     MOVE ZERO TO UT0220CA-CALL-STATUS.
048640     MOVE STRT-AREA-SOC-NO TO UT0220CA-CALL-SOC-NO.
048650     MOVE 1 TO UT0220CA-CALL-GRP-NO.
048660     CALL "UT0220" USING

048680                   WA-UT0220-CALL-AREA.
048690     IF  UT0220CA-CALL-STATUS NOT = 0
048700         MOVE 411 TO BR0001-OUTPUT-ERR-MSG-NO
048710         MOVE 1   TO WA-MSG-ERR-FLAG
048720         MOVE UT8500CA-FUNC-NORMAL-DISPLAY TO UT8500CA-CALL-CODE
048730         MOVE SPACES TO UT8500CA-INPUT-MSG
048740         STRING "BR0001:(A000) INST REC NOT FOUND "
048750                 UT0220CA-CALL-SOC-NO " " UT0220CA-CALL-GRP-NO
048760                 DELIMITED BY SIZE INTO UT8500CA-INPUT-MSG
048770         PERFORM UT8500-CALL THRU UT8500-EXIT
048780         MOVE 0 TO UT0220CA-CALL-SOC-NO
048790         GO TO A010-CHECK-ERRORS.
048800     SET TXT-1 TO 1.
048810     SEARCH TXT-INST-TABLE AT END
S13569*           MOVE 411 TO BR0001-OUTPUT-ERR-MSG-NO
S13569*           MOVE 1 TO WA-MSG-ERR-FLAG
048840            MOVE UT8500CA-FUNC-NORMAL-DISPLAY TO
048850                 UT8500CA-CALL-CODE
048860            MOVE SPACES TO UT8500CA-INPUT-MSG
048870            STRING "BR0001:(A000) NO TEXT FOR "
048880                   "INSTITUTUION " STRT-AREA-SOC-NO
048890                    DELIMITED BY SIZE INTO UT8500CA-INPUT-MSG
048900            PERFORM UT8500-CALL THRU UT8500-EXIT
S13569*                           GO TO A010-CHECK-ERRORS
S13569                            GO TO A030
048930            WHEN STRT-AREA-SOC-NO = TXT-INST(TXT-1)
048940            NEXT SENTENCE.
048950     GO TO A020.
048960*
048970 A005.
048980     IF  STRT-AREA-TELLER-LANG = TXT-LANG (TXT-1, TXT-2)
048990         GO TO A030.
049000 A020.
049010     SET TXT-2 TO 1
049020     SEARCH TXT-LANG-TABLE AT END
049030                           SET TXT-2 TO 1
049040                           GO TO A030
049050            WHEN STRT-AREA-TELLER-LANG = TXT-LANG (TXT-1, TXT-2)
049060            NEXT SENTENCE.
049070*
049080     MOVE TXT-BAD-DEBT-1(TXT-1, TXT-2) TO WA-BAD-DEBT-ITEMS(1).
049090     MOVE TXT-BAD-DEBT-2(TXT-1, TXT-2) TO WA-BAD-DEBT-ITEMS(2).
049100     MOVE TXT-BAD-DEBT-3(TXT-1, TXT-2) TO WA-BAD-DEBT-ITEMS(3).
049110     MOVE TXT-BAD-DEBT-4(TXT-1, TXT-2) TO WA-BAD-DEBT-ITEMS(4).
049120     MOVE TXT-BAD-DEBT-5(TXT-1, TXT-2) TO WA-BAD-DEBT-ITEMS(5).
049130     MOVE TXT-BAD-DEBT-6(TXT-1, TXT-2) TO WA-BAD-DEBT-ITEMS(6).
049140     MOVE TXT-BAD-DEBT-7(TXT-1, TXT-2) TO WA-BAD-DEBT-ITEMS(7).
TB0092     MOVE TXT-BAD-DEBT-8 (TXT-1, TXT-2) TO WA-BAD-DEBT-ITEMS(8).
TB0092     MOVE TXT-BAD-DEBT-9 (TXT-1, TXT-2) TO WA-BAD-DEBT-ITEMS(9).
TB0092     MOVE TXT-BAD-DEBT-10(TXT-1, TXT-2) TO WA-BAD-DEBT-ITEMS(10).
TB0092     MOVE TXT-BAD-DEBT-11(TXT-1, TXT-2) TO WA-BAD-DEBT-ITEMS(11).
TB0092     MOVE TXT-BAD-DEBT-12(TXT-1, TXT-2) TO WA-BAD-DEBT-ITEMS(12).
TB0258     MOVE TXT-BAD-DEBT-13(TXT-1, TXT-2) TO WA-BAD-DEBT-ITEMS(13).
049210*
049220 A030.
LON236* THIS IS NO NECESSAGE,BECAUSE UT0001 WILL DO IT.
LON236*S10108  IF  STRT-AREA-TRN-NO = 12000 OR 17000 OR 17116 OR 17117
LON236*TB0069      MOVE BR0001-12000-DETS TO BR0001-12000-WORK
LON236*TB0069     END-IF.
TB0069*
LON236*---------------------------------------------------------------
LON236* CALL  UT0001 HERE
LON236*---------------------------------------------------------------
S12223     IF ((STRT-AREA-TRN-NO = 12000 OR 17000
TB0169                          OR 12006 OR 17036
TB0169                          OR 12007 OR 17037
S10108                          OR 17116 OR 17117)
TB0169          OR (STRT-AREA-TRN-NO = 17050
LON236*TB0169              AND BR0001-SH-ENQ-TYPE = SPACES OR "1"))
LON236          AND BR0001-SH-ENQ-TYPE = "1"))
TB0169         PERFORM B100-CALL-UT0001 THRU B199-EXIT
TB0169         GO   TO A099-EXIT
TB0169     END-IF.
TB0169*
LON236*----------------------------------------------------------------
LON236* 17046  17035/17039 17050/17099 17474/17475/17476 TRAN-CODE
LON236*----------------------------------------------------------------
049590     IF  STRT-AREA-TRN-NO = 17046
049600         PERFORM F830-UPDATE-17046 THRU F839-EXIT
049610         GO TO A010-CHECK-ERRORS.
LON236*TB0105     IF  STRT-AREA-TRN-NO = 17035 OR 17047 OR 17039
LON236     IF  STRT-AREA-TRN-NO = 17035 OR 17039
TB0105         PERFORM F920-CALL-RENWL-MOD THRU F920-EXIT
TB0105         GO TO A010-CHECK-ERRORS.
S13708     IF  STRT-AREA-TRN-NO = 17474
S13708         PERFORM F940-SETUP-17475  THRU F940-EXIT
S13708         GO TO A010-CHECK-ERRORS.
S13708     IF  STRT-AREA-TRN-NO = 17475
S13708         PERFORM F930-LOAN-RENEWAL THRU F930-EXIT
S13708         GO TO A010-CHECK-ERRORS.
049650     IF  STRT-AREA-TRN-NO = 17050 OR 17099
049660         PERFORM F000-SEND-17000 THRU F099-EXIT
049670         GO TO A010-CHECK-ERRORS.
LON236*-----------------------------------------------------------
LON236* 10915 10916 CALL IN0014
LON236*-----------------------------------------------------------
TB0255     IF  STRT-AREA-TRN-NO = 10915
TB0255         MOVE 10915     TO IN0014-TRN-NO
TB0255         PERFORM Z400-CREATE-SORD THRU Z499-EXIT
TB0255         GO TO A010-CHECK-ERRORS.
TB0255     IF  STRT-AREA-TRN-NO = 10916
TB0255         MOVE 10916     TO IN0014-TRN-NO
TB0255         PERFORM Z500-AMEND-SORD THRU Z599-EXIT
TB0255         GO TO A010-CHECK-ERRORS.
LON236*-----------------------------------------------------------
LON236* 17003 17053 TRAN-CODE
LON236*-----------------------------------------------------------
050410     IF  STRT-AREA-TRN-NO = 17053
ST0094         IF BORM-STAT = 20
ST0094            MOVE  1         TO WA-MSG-ERR-FLAG
ST0094            MOVE 165        TO BR0001-OUTPUT-ERR-MSG-NO
ST0094            GO TO A010-CHECK-ERRORS
ST0094         END-IF
050470         PERFORM L000-BAD-DEBT-ENQ THRU L099-EXIT
050480         GO TO A010-CHECK-ERRORS.
050490     IF  STRT-AREA-TRN-NO = 17003
SC8397         MOVE "N"     TO UT0073CA-BATCH-FLAG
SC6343         MOVE "N"     TO  WA-53-52-FLAG
SC5100         MOVE "N"     TO  WA-53-54-FLAG
S11641         MOVE ZEROS   TO  WA-RATE-NPL
LON120         MOVE SPACES  TO  WA-LATEST-TXN
S10093         IF BR0001-BAD-DEBT-ACCOUNT
S12756            MOVE BORM-BAD-DEBT-IND   TO WA-B4-BAD-DEBT-IND
S10093         ELSE
S10093            MOVE CONB-BAD-DEBT-IND  TO WA-B4-BAD-DEBT-IND
S10093         END-IF
S13300         IF BORP-PL-NPL-NOT-APPL
S13300            MOVE 1       TO WA-MSG-ERR-FLAG
S13300            MOVE 7623    TO BR0001-OUTPUT-ERR-MSG-NO
S13300            PERFORM A010-CHECK-ERRORS THRU A099-EXIT
S13300         END-IF
S13300         IF BR0001-BAD-DEBT-IND = 52 OR 50 OR 54
S13300            CONTINUE
S13300         ELSE
S13300            MOVE 1       TO WA-MSG-ERR-FLAG
S13300            MOVE 7845    TO BR0001-OUTPUT-ERR-MSG-NO
S13300            PERFORM A010-CHECK-ERRORS THRU A099-EXIT
S13300         END-IF
S14107**S10093         IF SOC-NPL-VALIDATION-REQD-Y
LON120         IF BR0001-BAD-DEBT-JRNL-NO NOT = 0
LON120            CONTINUE
LON120         ELSE
S10093            IF (WA-B4-BAD-DEBT-IND = 0 OR 50)
S10093            AND BR0001-BAD-DEBT-IND = 52
S10093                IF BORP-METHOD(2) NOT EQUAL ('M' AND 'A')
S10093                   MOVE 1       TO WA-MSG-ERR-FLAG
S10093                   MOVE 6772   TO BR0001-OUTPUT-ERR-MSG-NO
S10093                   PERFORM A010-CHECK-ERRORS THRU A099-EXIT
S13203                END-IF
S10093            END-IF
S10093            IF (WA-B4-BAD-DEBT-IND = 52 OR 02)
S10093            AND BR0001-BAD-DEBT-IND = 50
S10093               IF BORP-BAD-D-NPL-PI-FROM-METH NOT
S10093                             EQUAL ('M' AND 'A')
S10093                  MOVE 1       TO WA-MSG-ERR-FLAG
S10093                  MOVE 6774   TO BR0001-OUTPUT-ERR-MSG-NO
S10093                  PERFORM A010-CHECK-ERRORS THRU A099-EXIT
S13645               END-IF
S10093            END-IF
LON531            IF BR0001-BAD-DEBT-ACCOUNT
LON531               IF  BR0001-BAD-DEBT-IND = 54
LON531               AND (WA-B4-BAD-DEBT-IND = 02 OR 52)
LON531               AND BOIS-STOP-ACCRUAL NOT = "Y"
LON531                   MOVE 1  TO WA-MSG-ERR-FLAG
LON531                   MOVE 8019  TO BR0001-OUTPUT-ERR-MSG-NO
LON531                   PERFORM A010-CHECK-ERRORS THRU A099-EXIT
LON531               END-IF
LON531            END-IF
LON120         END-IF
S10093
S12760         INITIALIZE UT0073CA-RETURN-AREA
S10093**** IF TYPE = CONTRACT ALL ACCOUNTS UNDER THE CONTRACT HAVE TO BE
S10093****  PROCESSED ONE BY ONE.RELM TABLE CONTAINS THE RELATION
S10093****  BETWEEN CONTRACT AND LOAN ACCOUNTS AND IS ACCESSED TO
S10093****  GET THE ACCOUNTS UNDER A CONTRACT ADD THE FOLLOWING CODE
S10093****  TO THE A030 PARAGRAPH IN BR0001.COB.
S10093****  1.) CALL UT0400 WITH CALL CODE 02 TO GET THE FIRST ACCOUNT
S10093**** UNDER THE CONTRACT.
S10093****  2.) CALL UT0400 WITH CALL CODE 03 TO GET THE NEXT ACCOUNT
S10093**** UNDER THE CONTRACT.
S10093****  3.) PERFORM THE FOLLOWING PARAGRAPHS FOR ALL THE ACCOUNTS
S10093**** UNDER THE CONTRACT.
S10093****     a) B110-READ-BORM
S10093****     b) B5100-ACCESS-BOIS
S10093****     c) M000-BAD-DEBT-AMEND
S10093****     d) C110-CONT
S10093****     e) E000-COMPLETION-PARA
S10093         IF BR0001-BAD-DEBT-CONTRACT
S10093            MOVE BR0001-BAD-DEBT-IND TO CONB-BAD-DEBT-IND
S10093            MOVE BR0001-BAD-DEBT-REASON-CODE
S10093                                     TO CONB-BAD-DEBT-REASON-CODE
S10093************Modify CONB table********************
S10093*           MOVE 74                TO DBIO-CALL-FUNC
LON531**S10093            PERFORM B6100-UPDATE-CONB THRU B6199-EXIT
LON531**LON189            IF WA-MSG-ERR-FLAG NOT =  0
LON531**LON189               GO TO A010-CHECK-ERRORS
LON531**LON189            END-IF
S10093************End of Modify CONB table
S10093            INITIALIZE                 UT0400-CALL-AREA
S10093            MOVE 02                 TO UT0400CA-CALL-CODE
S10093            MOVE STRT-AREA-SOC-NO   TO UT0400CA-RELM-INST-1
S10093                                       UT0400CA-RELM-INST-1-REL
S10093            MOVE 1                  TO UT0400CA-KEY-NO
S10093            MOVE  BR0001-BAD-DEBT-ACCT-NO  TO UT0400CA-RELM-ID-1
S10093            MOVE "CON"              TO UT0400CA-RELM-ENTITY-1
S10093            MOVE 0                  TO UT0400CA-RELM-ID-1-REL
S10093            MOVE "LON"              TO UT0400CA-RELM-ENTITY-1-REL
S10093            MOVE 2001 TO RELATIONSHIP-VALUE
S10093            MOVE SPACES   TO UT0400CA-CALL-STATUS
S10093            CALL "UT0400" USING
S10093                      UT0400-CALL-AREA
S10093            IF UT0400CA-CALL-STATUS NOT = "****"
S10093                MOVE "N" TO WS-FOUND
S10093                MOVE  1            TO WA-MSG-ERR-FLAG
S10093                MOVE 155           TO BR0001-OUTPUT-ERR-MSG-NO
S10093                PERFORM A010-CHECK-ERRORS THRU A099-EXIT
S10093            ELSE
S10093                MOVE "Y" TO WS-FOUND
S10093            END-IF
S10093            MOVE RELM-ID-1-REL TO BORM-MEMB-CUST-AC
S10093*           PERFORM B110-READ-BORM THRU B110-EXIT
S10093*           MOVE "READ" TO DBIOMOD-FUNCTION
S10093*           PERFORM B5100-ACCESS-BOIS THRU B5199-EXIT
S10093            PERFORM B200-OLD-RCD-DBIO-PARA THRU B299-EXIT
LON531            IF WA-MSG-ERR-FLAG = 1
LON531               GO TO A010-CHECK-ERRORS
LON531            END-IF
S10093            PERFORM M000-BAD-DEBT-AMEND THRU M099-EXIT
LON531            IF WA-MSG-ERR-FLAG = 1
LON531               GO TO A010-CHECK-ERRORS
LON531            END-IF
S10093            PERFORM C110-CONT THRU C110-EXIT
LON531            IF WA-MSG-ERR-FLAG = 1
LON531               GO TO A010-CHECK-ERRORS
LON531            END-IF
LON460            IF ST-STATUS-LEVEL-1 = 01
LON460               GO TO A099-EXIT
LON460            END-IF
LON286*LON120            IF BR0001-BAD-DEBT-JRNL-NO NOT = ZEROES
LON286*LON120               CONTINUE
LON286*LON120            ELSE
LON286            IF (((WA-B4-BAD-DEBT-IND NOT = 54 OR 04 ) AND
LON286               (BORM-BAD-DEBT-IND = 54 OR 04 ) AND
LON286               (BR0001-BAD-DEBT-JRNL-NO = ZEROES)) OR
LON286               (WA-PREV-BAD-DEBT-IND = (02 OR 52) AND
LON286               WA-NEXT-BAD-DEBT-IND = 54  AND
LON286               BR0001-BAD-DEBT-JRNL-NO NOT = ZEROES))
S12787                   INITIALIZE UT1996CA-RETURN-CODE
S10099                   PERFORM Z830-WRITE-PROM THRU Z830-EXIT
S10099                   IF  UT1996CA-RETURN-CODE NOT = 0
S10099                       GO TO A010-CHECK-ERRORS
S10099                   END-IF
S10099            END-IF
LON286*LON120            END-IF
S10093            MOVE  CONB-CURRENCY TO WS-SAVE-TXN-CURRENCY
S10093            INITIALIZE BORV-01-DETAIL
S10093            MOVE STRT-AREA-POST-DT TO BORV-01-TRN-DATE
S10093            PERFORM E000-COMPLETION-PARA THRU E010-EXIT
S12787            IF WA-MSG-ERR-FLAG NOT =  0
S12787               GO TO A010-CHECK-ERRORS
S12787            END-IF
S10093            PERFORM UNTIL UT0400CA-CALL-STATUS NOT EQUAL "****"
S10093                    OR WS-FOUND = "N"
S10093            MOVE RELM-INST-1         TO UT0400CA-RELM-INST-1
S10093            MOVE RELM-ID-1           TO UT0400CA-RELM-ID-1
S10093            MOVE RELM-ENTITY-1       TO UT0400CA-RELM-ENTITY-1
S10093            MOVE RELM-RELATIONSHIP-1
S10093                                  TO UT0400CA-RELM-RELATIONSHIP-1
S10093            MOVE RELM-INST-1-REL     TO UT0400CA-RELM-INST-1-REL
S10093            MOVE RELM-ID-1-REL       TO UT0400CA-RELM-ID-1-REL
S10093            MOVE RELM-ENTITY-1-REL   TO UT0400CA-RELM-ENTITY-1-REL
S10093            MOVE 1                   TO UT0400CA-KEY-NO
S11261            MOVE 05                  TO UT0400CA-CALL-CODE
S10093            MOVE SPACES   TO UT0400CA-CALL-STATUS
S10093            CALL "UT0400" USING
S10093                       UT0400-CALL-AREA
S10093            IF UT0400CA-CALL-STATUS NOT = "****"
S10093                MOVE "N" TO WS-FOUND
S10093            ELSE
S10093                MOVE "Y" TO WS-FOUND
S10093            END-IF
S10093            IF RELM-ID-1 = BR0001-BAD-DEBT-ACCT-NO AND
S10093                           RELM-ENTITY-1 = "CON"
S14073                           AND RELM-ENTITY-1-REL = "LON"
S10093               INITIALIZE BORM-RECORD-AREA
S10093               INITIALIZE BOIS-RECORD-AREA
S10093               MOVE RELM-ID-1-REL TO BORM-MEMB-CUST-AC
S10093*           PERFORM B110-READ-BORM THRU B110-EXIT
S10093*           MOVE "READ" TO DBIOMOD-FUNCTION
S10093*           PERFORM B5100-ACCESS-BOIS THRU B5199-EXIT
S10093               PERFORM B200-OLD-RCD-DBIO-PARA THRU B299-EXIT
LON531               IF WA-MSG-ERR-FLAG = 1
LON531                  GO TO A010-CHECK-ERRORS
LON531               END-IF
S10093               PERFORM M000-BAD-DEBT-AMEND THRU M099-EXIT
LON531               IF WA-MSG-ERR-FLAG = 1
LON531                  GO TO A010-CHECK-ERRORS
LON531               END-IF
S10093               PERFORM C110-CONT THRU C110-EXIT
LON531               IF WA-MSG-ERR-FLAG = 1
LON531                  GO TO A010-CHECK-ERRORS
LON531               END-IF
LON460               IF ST-STATUS-LEVEL-1 = 01
LON460                  GO TO A099-EXIT
LON460               END-IF
LON286               IF (((WA-B4-BAD-DEBT-IND NOT = 54 OR 04 ) AND
LON286                  (BORM-BAD-DEBT-IND = 54 OR 04 ) AND
LON286                  (BR0001-BAD-DEBT-JRNL-NO = ZEROES)) OR
LON286                  (WA-PREV-BAD-DEBT-IND = (02 OR 52) AND
LON286                   WA-NEXT-BAD-DEBT-IND = 54  AND
LON286                   BR0001-BAD-DEBT-JRNL-NO NOT = ZEROES))
S10099                   PERFORM Z830-WRITE-PROM THRU Z830-EXIT
S10099                   IF  UT1996CA-RETURN-CODE NOT = 0
S10099                       GO TO A010-CHECK-ERRORS
S10099                   END-IF
S10099               END-IF
S10093               MOVE  CONB-CURRENCY TO WS-SAVE-TXN-CURRENCY
S10093               INITIALIZE BORV-01-DETAIL
S10093               MOVE STRT-AREA-POST-DT TO BORV-01-TRN-DATE
S10093               PERFORM E000-COMPLETION-PARA THRU E010-EXIT
LON531               IF WA-MSG-ERR-FLAG = 1
LON531                  GO TO A010-CHECK-ERRORS
LON531               END-IF
S10093            ELSE
S10093               MOVE "N" TO WS-FOUND
S10093            END-IF
S10093            END-PERFORM
LON531            IF WA-MSG-ERR-FLAG = 0
LON531               PERFORM B6100-UPDATE-CONB THRU B6199-EXIT
LON531            END-IF
S10093            GO TO A010-CHECK-ERRORS
S10093      ELSE
052550         PERFORM M000-BAD-DEBT-AMEND THRU M099-EXIT
ST0094         MOVE WA-BAD-DEBT-IND-AFT TO
ST0094                           UT0073CA-BAD-DEBT-IND-AFTER
ST0094         MOVE WA-BAD-DEBT-IND-BEF TO
ST0094                           UT0073CA-BAD-DEBT-IND-BEFORE
LON120         IF BR0001-BAD-DEBT-JRNL-NO NOT = 0
LON327*LON120            DISPLAY "CHANGE JRNL"
LON120            MOVE BR0001-BAD-DEBT-JRNL-NO TO UT0073CA-CORR-JRNL
LON120         END-IF
LON327*LON120         DISPLAY "JRNL NO = " UT0073CA-CORR-JRNL
LON327*LON120      DISPLAY "BR0001 BEF = " UT0073CA-BAD-DEBT-IND-BEFORE
SC6343         MOVE WA-53-52-FLAG TO UT0073CA-FLAG-53-52
SC5100         IF (WA-53-54-FLAG = "Y")
SC5014            MOVE 0 TO WA-INDX
SC5014            PERFORM
SC5014               VARYING WA-INDX FROM 1 BY 1
SC5014                     UNTIL WA-INDX > 5
SC5014               MOVE  GLIF-GL-BUCKET-CDE(WA-INDX) TO
SC5014                       UT0073CA-GL-BUCKET-CDE(WA-INDX)
SC5014               MOVE  GLIF-GL-BUCKET-DUE(WA-INDX) TO
SC5014                       UT0073CA-GL-BUCKET-DUE(WA-INDX)
SC5014            END-PERFORM
SC5014         END-IF
S10093       END-IF
S10099********ADD FOR DCF&MM PROVISION***********************
S10099       IF (((WA-B4-BAD-DEBT-IND NOT = 54 OR 04 ) AND
S10099          (BORM-BAD-DEBT-IND = 54 OR 04 ) AND
LON286          (BR0001-BAD-DEBT-JRNL-NO = ZEROES)) OR
LON286          (WA-PREV-BAD-DEBT-IND = (02 OR 52) AND
LON286           WA-NEXT-BAD-DEBT-IND = 54  AND
LON286           BR0001-BAD-DEBT-JRNL-NO NOT = ZEROES))
S10099          PERFORM Z830-WRITE-PROM THRU Z830-EXIT
S10099          IF  UT1996CA-RETURN-CODE NOT = 0
S10099              GO TO A010-CHECK-ERRORS
S10099          END-IF
S10099       END-IF
052960       GO TO A010-CHECK-ERRORS.
053040     IF  STRT-AREA-TRN-NO = 17465
053050         PERFORM B400-SNA-CHK    THRU B499-EXIT
053060         IF WA-MSG-ERR-FLAG = 0
053070            PERFORM B600-LNA-CHK THRU B699-EXIT.
053290*
053300 A010-CHECK-ERRORS.
053310     IF  WA-MSG-ERR-FLAG = 1
053320         PERFORM Z000-FORMT-SEND-PARA THRU Z099-EXIT
053330         MOVE 01                 TO ST-STATUS-LEVEL-1.
LON236*
053340 A099-EXIT.
053350     EXIT PROGRAM.
053360/
053370 A100-LOAD-TXT.
053380*
053400     MOVE 0 TO UT9000-CALL-CODE.
053410     PERFORM U9100-RETRIEVE-TEXT THRU U9199-EXIT.
053420     PERFORM A120-INIT-TXTM-AREA THRU A129-EXIT
053430             VARYING TXT-1 FROM 1 BY 1
053440             UNTIL TXT-1 > 2
053450             AFTER  TXT-2 FROM 1 BY 1
053460             UNTIL TXT-2 > 2.
053470     SET  TXT-1  TO 1.
053480     SET  TXT-1 DOWN BY 1.
053490     MOVE 75     TO DBIO-CALL-FUNC.
053500     MOVE "LNGM" TO DBIO-CALL-N-SYS.
053510     MOVE "BEGN" TO DBIO-CALL-SAVE-REFER.
053520     MOVE SPACES TO DBIO-CALL-STATUS.
053530     PERFORM A110-LOAD-INST-LANG-CODE THRU A119-EXIT
053540             UNTIL DBIO-CALL-SAVE-REFER = "END.".
053550     SET TXT-1, TXT-2 TO 1.
053570*
053580 A109-EXIT.
053590     EXIT.
053600/
053610 A110-LOAD-INST-LANG-CODE.
053620*
053630     PERFORM U500-LANG-ACCESS THRU U599-EXIT.
053640     IF  DBIO-CALL-STATUS NOT = "****"
053650         MOVE UT8500CA-FUNC-NORMAL-DISPLAY TO UT8500CA-CALL-CODE
053660         MOVE SPACES TO UT8500CA-INPUT-MSG
053670         STRING "BR0001:(A110) ERROR READING LNGM TABLE/"
053680                "CALL STATUS = " DBIO-CALL-STATUS
053690                DELIMITED BY SIZE INTO UT8500CA-INPUT-MSG
053700         PERFORM UT8500-CALL THRU UT8500-EXIT
053710         MOVE "END." TO DBIO-CALL-SAVE-REFER.
053720     IF  DBIO-CALL-SAVE-REFER NOT = "END."
053730     AND LNGM-STAT NOT = 9
S11218         MOVE STRT-AREA-SOC-NO   TO UT9000-INST-NO
053760         MOVE "BR0001"           TO UT9000-CALLING-ID
S13569         MOVE LNGM-LANGUAGE-CODE TO UT9000-LANGUAGE-CODE
053800         MOVE 1                  TO UT9000-REPORT-NO
053810         MOVE 0                  TO UT9000-SEQUENCE-NO
053820         MOVE " "                TO UT9000-CALL-STATUS
053830         PERFORM U9000-RETRIEVE-TXTM-RECORDS THRU U9009-EXIT
053840                 UNTIL UT9000-CALL-STATUS = "E".
053850*
053860 A119-EXIT.
053870     EXIT.
053880/
053890 A120-INIT-TXTM-AREA.
053900     IF  TXT-2 = 1
053910         MOVE 0 TO TXT-INST (TXT-1).
053920     MOVE 0 TO TXT-LANG (TXT-1, TXT-2).
053930     MOVE SPACES TO TXT-DATA-AREA (TXT-1, TXT-2).
053940 A129-EXIT.
053950     EXIT.
TB0169/
TB0169 B100-CALL-UT0001.
TB0169*
TB0169     CALL "UT0001"  USING
054000                             STARTAREA
054010                             BORR-PARAMS
054020                             LS01-UT0290CA-CALL-AREA
054030                             BR0001-CALL-AREA
054040                             MULTI-OVERRIDE-AREA
054050                             BOIS-RECORD-AREA.
LON527     IF ST-STATUS-LEVEL-1 NOT = 0
LON527        MOVE 1 TO WA-MSG-ERR-FLAG
LON527        GO     TO B199-EXIT
LON527     END-IF.
LON236*
TB0169 B199-EXIT.
TB0169     EXIT.
LON236/
S10093 B200-OLD-RCD-DBIO-PARA.
LON236*
S10093     MOVE 01                     TO DBIO-CALL-FUNC.
S10093     MOVE "BOR"                  TO DBIO-CALL-SYS-AREA.
S10093     MOVE STRT-AREA-SOC-NO       TO WA-BORM-SOC-NO.
S10093     MOVE BORM-MEMB-CUST-AC      TO WA-BORM-MEMB-CUST-AC.
S10093     MOVE WA-BORM-KEY            TO DBIO-CALL-KEY-AREA.
S10093     MOVE "1"                    TO DBIO-CALL-LOCK-FLAG.
S10093     MOVE SPACES                 TO DBIO-CALL-STATUS.
S10093     CALL "DBIO" USING
S10093                                 DBIO-CALL-PARAM
S10093                                 DBIO-CALL-SYS-AREA
S10093                                 DBIO-CALL-KEY-AREA
S10093                                 DBIO-CALL-STATUS
S10093                                 BORM-RECORD-AREA.
S10093     IF  DBIO-CALL-STATUS = "MRNF"
STDMSG         DISPLAY "BR0000: (B200) ERROR IN READING BORM"
STDMSG                 ", KEY = " DBIO-CALL-KEY-AREA
STDMSG                 ", STATUS = " DBIO-CALL-STATUS
S10093         MOVE 108        TO BR0001-OUTPUT-ERR-MSG-NO
S10093         MOVE 1 TO WA-MSG-ERR-FLAG
S10093         GO TO B299-EXIT.
S10093     IF  DBIO-CALL-STATUS = "HELD"
S10093         MOVE 112 TO BR0001-OUTPUT-ERR-MSG-NO
S10093         MOVE 1 TO WA-MSG-ERR-FLAG
S10093         GO TO B299-EXIT.
S10093     IF DBIO-CALL-STATUS NOT = "****"
S10093        MOVE UT8500CA-FUNC-NORMAL-DISPLAY TO UT8500CA-CALL-CODE
S10093        MOVE SPACES TO UT8500CA-INPUT-MSG
S10093        STRING "BR0001:(B200) DATA BASE CORRUPT FOR KEY = "
S10093                                    DBIO-CALL-KEY-AREA
S10093            DELIMITED BY SIZE INTO UT8500CA-INPUT-MSG
S10093        CALL "UT8500" USING
S10093
S10093                         UT8500CA-AREA
S10093        MOVE 155                    TO BR0001-OUTPUT-ERR-MSG-NO
S10093        MOVE 1                      TO WA-MSG-ERR-FLAG
S10093        GO TO B299-EXIT.
S10093
S10093     IF  DBIO-CALL-STATUS = "****"
S10093         MOVE "READ" TO DBIOMOD-FUNCTION
S10093         PERFORM B250-CHECK-HOLIDAY-TXN-REST THRU B259-EXIT
S10093         PERFORM W3100-ACCESS-BOIS THRU W3199-EXIT
S10093         IF DBIOMOD-STAT NOT = "****"
S10093            GO TO B299-EXIT
S10093         END-IF
S10093     END-IF.
S10093     IF  STRT-AREA-TRN-NO = 17003
LON531         IF BR0001-BAD-DEBT-CONTRACT AND
LON531            BR0001-BAD-DEBT-IND = 54 AND
LON531            BR0001-BAD-DEBT-JRNL-NO  = 0 AND
LON531            WA-B4-BAD-DEBT-IND = (02 OR 52) AND
LON531            BOIS-STOP-ACCRUAL NOT = "Y"
LON531            MOVE 1    TO WA-MSG-ERR-FLAG
LON531            MOVE 8019 TO BR0001-OUTPUT-ERR-MSG-NO
LON531            GO TO B299-EXIT
LON531         END-IF
LON531     END-IF.
S10093     PERFORM V510-CALL-UT0500 THRU V519-EXIT.
S10093     IF BORM-SOC-NO   NOT = BORP-SOC-NO
S10093     OR BORM-ACT-TYPE NOT = BORP-ACCT-TYPE
S10093     OR BORM-CAT      NOT = BORP-INT-CAT
S10093        MOVE 02               TO UT2000CA-CALL-CODE
S10093        MOVE 0                TO UT2000CA-CALL-INDEX
S10093        MOVE BORM-SOC-NO      TO UT2000CA-CALL-SOC-NO
S10093        MOVE BORM-ACT-TYPE    TO UT2000CA-CALL-ACCT-TYPE
S10093        MOVE BORM-CAT         TO UT2000CA-CALL-INT-CAT
S10093        PERFORM D110-CALL-UT2000 THRU D119-EXIT
S10093        IF  WA-MSG-ERR-FLAG = 1
S10093            MOVE UT8500CA-FUNC-NORMAL-DISPLAY
S10093                             TO UT8500CA-CALL-CODE
S10093            MOVE SPACES TO UT8500CA-INPUT-MSG
S10093            STRING "BR0001(B200) ERROR IN RETURN FROM D100 "
S10093            BORM-ACT-TYPE " " BORM-CAT
S10093            DELIMITED BY SIZE INTO UT8500CA-INPUT-MSG
S10093            CALL "UT8500" USING
S10093                       UT8500CA-AREA.
S10093 B299-EXIT.
S10093     EXIT.
S10093/
S10093 B250-CHECK-HOLIDAY-TXN-REST.
S10093
S10093     MOVE STRT-AREA-POST-DT           TO UT0020-BINARY-DATE.
S10093     MOVE 2                           TO UT0020-FUNCTION.
S10093     CALL "UT0020" USING
S10093                   UT0020-FUNCTION
S10093                   UT0020-DATE-AREA
S10093                   UT0020-BINARY-DATE.
S10093     MOVE UT0020-DATE-DDMMCCYY        TO WA-DDMMCCYY.
S10093     PERFORM B951-DDMMYY-YYMMDD       THRU B951-EXIT.
S10093     MOVE WA-CCYYMMDD             TO UT0310CA-CALENDAR-IN-CCYYMMDD
S10093     MOVE BORP-CALENDAR-CODE          TO UT0310CA-CALENDAR-CODE.
S10093     MOVE 0                           TO UT0310CA-CALENDAR-METHOD.
S10093     MOVE 02                          TO UT0310CA-CALL-FUNC.
S10093     MOVE STRT-AREA-SOC-NO            TO UT0310CA-SOC-NO.
S10093     CALL "UT0310" USING
S10093                   UT0310CA-AREA.
S10093
S10093     IF (UT0310CA-CALL-STATUS = 0)
S10093         MOVE UT0310CA-CALENDAR-OUT-CODE TO DATE-CODE-VALUE
S10093         IF DATE-HOLIDAY
S10093            EVALUATE BORP-REST-TXN-ON-HOL-IND
S10093                   WHEN  0
S10093                         CONTINUE
S10093                   WHEN  1
S10093                         IF  WA-EDIT-MASK-BIT(4)  = 1
S10093                         OR  WA-EDIT-MASK-BIT(5)  = 1
S10093                         OR  WA-EDIT-MASK-BIT(29) = 1
S10093                         MOVE 5024  TO BR0001-OUTPUT-ERR-MSG-NO
S10093                             MOVE   1   TO WA-MSG-ERR-FLAG
S10093                         END-IF
S10093                   WHEN  2
S10093                         IF  WA-EDIT-MASK-BIT(23) = 1
S10093                         MOVE 5023  TO BR0001-OUTPUT-ERR-MSG-NO
S10093                             MOVE   1   TO WA-MSG-ERR-FLAG
S10093                         END-IF
S10093                   WHEN  3
S10093                      MOVE 5022      TO BR0001-OUTPUT-ERR-MSG-NO
S10093                         MOVE   1       TO WA-MSG-ERR-FLAG
S10093            END-EVALUATE
S10093         END-IF
S10093     ELSE
S10093         MOVE  126          TO BR0001-OUTPUT-ERR-MSG-NO
S10093         MOVE   1           TO WA-MSG-ERR-FLAG
S10093     END-IF.
S10093*
S10093 B259-EXIT.
S10093     EXIT.
S10093/
S10093 B951-DDMMYY-YYMMDD.
S10093     MOVE WA-CAL1-DD       TO  WA-CAL2-DD.
S10093     MOVE WA-CAL1-MM       TO  WA-CAL2-MM.
S10093     MOVE WA-CAL1-CC       TO  WA-CAL2-CC.
S10093     MOVE WA-CAL1-YY       TO  WA-CAL2-YY.
S10093
S10093 B951-EXIT.
S10093     EXIT.
S10093/
S10093 D110-CALL-UT2000.
S10093     CALL "UT2000" USING
S10093
S10093                         UT2000CA-CALL-AREA.
S10093     IF UT2000CA-CALL-STATUS NOT = 0
S10093         MOVE 201 TO UT0060CA-CALL-ERROR-CODE
S10093         PERFORM Z600-OVERRIDABLE-ERROR-CHECK THRU Z699-EXIT
S10093         IF WA-MSG-ERR-FLAG = 0
S10093             MOVE 201 TO WA-ERR-MSG-NO
S10093         ELSE
S10093             MOVE 201 TO BR0001-OUTPUT-ERR-MSG-NO
S10093             MOVE 1 TO WA-MSG-ERR-FLAG
S10093           MOVE UT8500CA-FUNC-NORMAL-DISPLAY TO UT8500CA-CALL-CODE
S10093             MOVE SPACES TO UT8500CA-INPUT-MSG
S10093             STRING "BR0001:(D100) INVALID A/C TYPE OR INT CAT "
S10093             UT2000CA-CALL-ACCT-TYPE " " UT2000CA-CALL-INT-CAT
S10093                 DELIMITED BY SIZE INTO UT8500CA-INPUT-MSG
S10093             CALL "UT8500" USING
S10093
S10093                                 UT8500CA-AREA.
S10093 D119-EXIT.
S10093     EXIT.
S10093/
S10093 V510-CALL-UT0500.
S10093
S10538     IF STRT-AREA-TRN-NO = 17003 AND BR0001-17053-CONTRACT
S10538        MOVE CONB-CUSTOMER-NO       TO UT0500CA-CUSM-NO
S10538        MOVE CONB-BRANCH-CODE       TO UT0500CA-BRHM-NO
S10538     ELSE
S10538        MOVE BR0001-INPUT-KEY-R     TO UT0500CA-CUSM-NO
S10538        MOVE BORM-BR-NO             TO UT0500CA-BRHM-NO
S10538     END-IF
S10093     MOVE 1                TO UT0500CA-CALL-CODE.
S10093     MOVE STRT-AREA-SOC-NO TO UT0500CA-INST-NO.
S10093     MOVE STRT-AREA-BRANCH TO UT0500CA-BRHM-NO.
S10093     MOVE STRT-AREA-TELLER-LANG TO UT0500RA-LANGUAGE-CODE.
S10093     MOVE 0                TO UT0500CA-INST-LANGUAGE-CODE.
S10093     CALL "UT0500"      USING
S10093
S10093                              UT0500-CALL-PARAM.
S10093     IF UT0500RA-CALL-STATUS NOT = 0
S10093        MOVE UT0500RA-ERR-MSG-NO TO BR0001-OUTPUT-ERR-MSG-NO
S10093        MOVE 1 TO WA-MSG-ERR-FLAG
LON237*S10093        GO TO V599-EXIT.
LON237        GO TO V519-EXIT.
S10093 V519-EXIT.
S10093     EXIT.
055840/
TB0143 A400-INIT-MISC.
LON236*
LON236     INITIALIZE WA-ALL-VARIABLE.
S10093     MOVE 0 TO WA-SAVE-BRANCH.
TB0143     MOVE 0                TO WA-MSG-ERR-FLAG.
TB0143     MOVE "N"              TO WA-ERROR-FLAG
TB0143                              BR0001CA-CHG-FAC-IND.
S12692     MOVE 0                TO WA-MARG-INT-ACCR-BEFORE
S12692                              WA-MARG-INT-ACCR-AFTER
S12692                              WA-BASE-INT-ACCR-BEFORE
S12692                              WA-BASE-INT-ACCR-AFTER.
S13026     MOVE 0                TO WA-RATE-NPL
S13026                              WA-RT-INCR
S13026                              WA-INT-RATE-A.
S12692
TB0143 A409-EXIT.
TB0143     EXIT.
055930/
055940 B400-SNA-CHK.
055950     MOVE  0               TO WA-MSG-ERR-FLAG.
055960     INITIALIZE WA-ACSN-KEY.
055970     MOVE 71               TO DBIO-CALL-FUNC.
055980     MOVE "ACSN"           TO DBIO-CALL-SYS-AREA.
055990     MOVE STRT-AREA-SOC-NO TO WA-ACSN-SOC-NO.
056000     MOVE "BOR"            TO WA-ACSN-SYS-ID.
056010     MOVE BR0001-INPUT-KEY TO WA-ACSN-ACCT-NO.
056020     MOVE WA-ACSN-KEY      TO DBIO-CALL-KEY-AREA.
056030     PERFORM B500-DBIO-ACSN THRU B509-EXIT.

056050     IF DBIO-CALL-STATUS  = "****"
056060         MOVE ACSN-RECORD-AREA TO SAVE-ACSN-RECORD-AREA
056070         PERFORM B530-CHECK-ACSN THRU B539-EXIT
056080     ELSE
056090     IF  DBIO-CALL-STATUS = "MRNF"
056100         INITIALIZE            ACSN-DATA-REC-RD
056110         MOVE WA-ACSN-KEY      TO ACSN-KEY
056120         MOVE BR0001-SHORT-NAME TO
056130              ACSN-SHORT-ACCOUNT-NAME
056140         PERFORM B520-CREATE-ACSN THRU B529-EXIT
056150     ELSE
056160         MOVE  1            TO WA-MSG-ERR-FLAG
056170         MOVE 155           TO BR0001-OUTPUT-ERR-MSG-NO
056180         MOVE "ACSN"        TO BR0001-OUTPUT-DATA-SET
056190         MOVE DBIO-CALL-STATUS TO BR0001-OUTPUT-STATUS
056200         MOVE UT8500CA-FUNC-NORMAL-DISPLAY TO
056210              UT8500CA-CALL-CODE
056220         MOVE SPACES TO UT8500CA-INPUT-MSG
056230         STRING "BR0001:(B400) 'ACSN' READ ERROR, STAT- "
056240                 DBIO-CALL-STATUS ", KEY- " DBIO-CALL-KEY
056250             DELIMITED BY SIZE INTO UT8500CA-INPUT-MSG
056260         PERFORM UT8500-CALL THRU UT8500-EXIT.

056280     IF  WA-MSG-ERR-FLAG = 1
056290         GO TO B499-EXIT
056300     ELSE
056310         PERFORM B800-ACSN-AUDIT THRU B809-EXIT.
056320*
056340 B499-EXIT.
056350     EXIT.
056360/
056370 B500-DBIO-ACSN.
056380     MOVE SPACES TO DBIO-CALL-STATUS.
056390     CALL "DBIO" USING

056410                       DBIO-CALL-PARAM
056420                       DBIO-CALL-SYS-AREA
056430                       DBIO-CALL-KEY-AREA
056440                       DBIO-CALL-STATUS
056450                       ACSN-RECORD-AREA.
056460 B509-EXIT.
056470     EXIT.
056480/
056490 B510-DBIO-ACLN.
056500     MOVE SPACES TO DBIO-CALL-STATUS.
056510     CALL "DBIO" USING

056530                       DBIO-CALL-PARAM
056540                       DBIO-CALL-SYS-AREA
056550                       DBIO-CALL-KEY-AREA
056560                       DBIO-CALL-STATUS
056570                       ACLN-RECORD-AREA.
056580 B519-EXIT.
056590     EXIT.
056600/
056610 B520-CREATE-ACSN.
056620     MOVE 72    TO DBIO-CALL-FUNC.
056630     MOVE "ACSN"           TO DBIO-CALL-SYS-AREA.
056640     MOVE STRT-AREA-SOC-NO TO WA-ACSN-SOC-NO.
056650     MOVE "BOR"            TO WA-ACSN-SYS-ID.
056660     MOVE BR0001-INPUT-KEY TO WA-ACSN-ACCT-NO.
056670     MOVE WA-ACSN-KEY      TO DBIO-CALL-KEY-AREA.
056680     PERFORM B500-DBIO-ACSN THRU B509-EXIT.

056700     IF DBIO-CALL-STATUS NOT = "****"
056710           MOVE 155 TO BR0001-OUTPUT-ERR-MSG-NO
056720           MOVE  1  TO WA-MSG-ERR-FLAG
056730           MOVE "ACSN" TO BR0001-OUTPUT-DATA-SET
056740           MOVE DBIO-CALL-STATUS TO BR0001-OUTPUT-STATUS
056750           MOVE UT8500CA-FUNC-NORMAL-DISPLAY TO UT8500CA-CALL-CODE
056760           MOVE SPACES                TO UT8500CA-INPUT-MSG
056770           STRING "BR0001:(B400) 'ACSN' WRITE ERROR, STAT- "
056780                   DBIO-CALL-STATUS ", KEY- "
056790                   WA-ACSN-KEY
056800               DELIMITED BY SIZE INTO UT8500CA-INPUT-MSG
056810           PERFORM UT8500-CALL THRU UT8500-EXIT
056820           GO TO B529-EXIT.

056840     MOVE ACSN-RECORD-AREA    TO WA-AUDT-OLD-IMAGE
056850                                 WA-AUDT-NEW-IMAGE.
056860 B529-EXIT.
056870     EXIT.
056880/
056890 B530-CHECK-ACSN.
056900     IF BR0001-S-DEL-FLAG = 'Y'
056910         MOVE WA-ACSN-KEY      TO ACSN-KEY
056920         MOVE 73               TO DBIO-CALL-FUNC
056930     ELSE
056940         MOVE WA-ACSN-KEY      TO ACSN-KEY
056950         MOVE BR0001-SHORT-NAME TO
056960              ACSN-SHORT-ACCOUNT-NAME
056970         MOVE 74    TO DBIO-CALL-FUNC.

056990     MOVE "ACSN"           TO DBIO-CALL-SYS-AREA.
057000     MOVE STRT-AREA-SOC-NO TO WA-ACSN-SOC-NO.
057010     MOVE "BOR"            TO WA-ACSN-SYS-ID.
057020     MOVE BR0001-INPUT-KEY TO WA-ACSN-ACCT-NO.
057030     MOVE WA-ACSN-KEY      TO DBIO-CALL-KEY-AREA.
057040     PERFORM B500-DBIO-ACSN THRU B509-EXIT.

057060     IF DBIO-CALL-STATUS NOT = "****"
057070           MOVE 155 TO BR0001-OUTPUT-ERR-MSG-NO
057080           MOVE  1  TO WA-MSG-ERR-FLAG
057090           MOVE "ACSN" TO BR0001-OUTPUT-DATA-SET
057100           MOVE DBIO-CALL-STATUS TO BR0001-OUTPUT-STATUS
057110           MOVE UT8500CA-FUNC-NORMAL-DISPLAY TO UT8500CA-CALL-CODE
057120           MOVE SPACES                TO UT8500CA-INPUT-MSG
057130           STRING "BR0001:(B400) 'ACSN' UPD-DEL ERROR, STAT- "
057140                   DBIO-CALL-STATUS ", KEY- "
057150                   WA-ACSN-KEY
057160               DELIMITED BY SIZE INTO UT8500CA-INPUT-MSG
057170           PERFORM UT8500-CALL THRU UT8500-EXIT
057180           GO TO B539-EXIT.

057200     MOVE SAVE-ACSN-RECORD-AREA TO WA-AUDT-OLD-IMAGE.

057220     IF BR0001-S-DEL-FLAG = 'Y'
057230        MOVE SPACES             TO WA-AUDT-NEW-IMAGE
057240     ELSE
057250        MOVE ACSN-RECORD-AREA   TO WA-AUDT-NEW-IMAGE.
057260 B539-EXIT.
057270     EXIT.
057280/
057290 B600-LNA-CHK.
057300     INITIALIZE    WA-ACSN-KEY.
057310     MOVE 71               TO DBIO-CALL-FUNC.
057320     MOVE "ACLN"           TO DBIO-CALL-SYS-AREA.
057330     MOVE STRT-AREA-SOC-NO TO WA-ACSN-SOC-NO.
057340     MOVE "BOR"            TO WA-ACSN-SYS-ID.
057350     MOVE BR0001-INPUT-KEY TO WA-ACSN-ACCT-NO.
057360     MOVE WA-ACSN-KEY      TO DBIO-CALL-KEY-AREA.
057370     PERFORM B510-DBIO-ACLN THRU B519-EXIT.

057390     IF DBIO-CALL-STATUS  = "****"
057400         MOVE ACLN-RECORD-AREA TO SAVE-ACLN-RECORD-AREA
057410         PERFORM B730-CHECK-ACLN THRU B739-EXIT
057420     ELSE
057430     IF  DBIO-CALL-STATUS = "MRNF"
057440         INITIALIZE            ACLN-DATA-REC-RD
057450         MOVE WA-ACSN-KEY      TO ACLN-KEY
057460         MOVE BR0001-LONG-NAME TO ACLN-LONG-NAME
057470         PERFORM B720-CREATE-ACLN THRU B729-EXIT
057480     ELSE
057490         MOVE  1            TO WA-MSG-ERR-FLAG
057500         MOVE 155           TO BR0001-OUTPUT-ERR-MSG-NO
057510         MOVE "ACLN"        TO BR0001-OUTPUT-DATA-SET
057520         MOVE DBIO-CALL-STATUS TO BR0001-OUTPUT-STATUS
057530         MOVE UT8500CA-FUNC-NORMAL-DISPLAY TO
057540              UT8500CA-CALL-CODE
057550         MOVE SPACES TO UT8500CA-INPUT-MSG
057560         STRING "BR0001:(B600) 'ACLN' READ ERROR, STAT- "
057570                 DBIO-CALL-STATUS ", KEY- " DBIO-CALL-KEY
057580             DELIMITED BY SIZE INTO UT8500CA-INPUT-MSG
057590         PERFORM UT8500-CALL THRU UT8500-EXIT.

057610     IF  WA-MSG-ERR-FLAG = 0
057620         PERFORM B850-ACLN-AUDIT THRU B859-EXIT
057630         MOVE 0 TO BR0001-OUTPUT-ERR-MSG-NO
057640         PERFORM Z000-FORMT-SEND-PARA THRU Z099-EXIT.

057660 B699-EXIT.
057670     EXIT.
057680/
057690 B720-CREATE-ACLN.
057700     MOVE 72    TO DBIO-CALL-FUNC.
057710     MOVE "ACLN"           TO DBIO-CALL-SYS-AREA.
057720     MOVE STRT-AREA-SOC-NO TO WA-ACSN-SOC-NO.
057730     MOVE "BOR"            TO WA-ACSN-SYS-ID.
057740     MOVE BR0001-INPUT-KEY TO WA-ACSN-ACCT-NO.
057750     MOVE WA-ACSN-KEY      TO DBIO-CALL-KEY-AREA.
057760     PERFORM B510-DBIO-ACLN THRU B519-EXIT.

057780     IF DBIO-CALL-STATUS NOT = "****"
057790           MOVE 155 TO BR0001-OUTPUT-ERR-MSG-NO
057800           MOVE  1  TO WA-MSG-ERR-FLAG
057810           MOVE "ACLN" TO BR0001-OUTPUT-DATA-SET
057820           MOVE DBIO-CALL-STATUS TO BR0001-OUTPUT-STATUS
057830           MOVE UT8500CA-FUNC-NORMAL-DISPLAY TO UT8500CA-CALL-CODE
057840           MOVE SPACES                TO UT8500CA-INPUT-MSG
057850           STRING "BR0001:(B600) 'ACLN' WRITE ERROR, STAT- "
057860                   DBIO-CALL-STATUS ", KEY- "
057870                   WA-ACSN-KEY
057880               DELIMITED BY SIZE INTO UT8500CA-INPUT-MSG
057890           PERFORM UT8500-CALL THRU UT8500-EXIT
057900           GO TO B729-EXIT.

057920     MOVE ACLN-RECORD-AREA    TO WA-AUDT-OLD-IMAGE
057930                                 WA-AUDT-NEW-IMAGE.
057940 B729-EXIT.
057950     EXIT.
057960/
057970 B730-CHECK-ACLN.
057980     IF BR0001-L-DEL-FLAG = 'Y'
057990         MOVE WA-ACSN-KEY      TO ACLN-KEY
058000         MOVE 73               TO DBIO-CALL-FUNC
058010     ELSE
058020         MOVE WA-ACSN-KEY      TO ACLN-KEY
058030         MOVE BR0001-LONG-NAME TO
058040              ACLN-LONG-NAME
058050         MOVE 74    TO DBIO-CALL-FUNC.

058070     MOVE "ACLN"           TO DBIO-CALL-SYS-AREA.
058080     MOVE STRT-AREA-SOC-NO TO WA-ACSN-SOC-NO.
058090     MOVE "BOR"            TO WA-ACSN-SYS-ID.
058100     MOVE BR0001-INPUT-KEY TO WA-ACSN-ACCT-NO.
058110     MOVE WA-ACSN-KEY      TO DBIO-CALL-KEY-AREA.
058120     PERFORM B510-DBIO-ACLN THRU B519-EXIT.

058140     IF DBIO-CALL-STATUS NOT = "****"
058150           MOVE 155 TO BR0001-OUTPUT-ERR-MSG-NO
058160           MOVE  1  TO WA-MSG-ERR-FLAG
058170           MOVE "ACLN" TO BR0001-OUTPUT-DATA-SET
058180           MOVE DBIO-CALL-STATUS TO BR0001-OUTPUT-STATUS
058190           MOVE UT8500CA-FUNC-NORMAL-DISPLAY TO UT8500CA-CALL-CODE
058200           MOVE SPACES                TO UT8500CA-INPUT-MSG
058210           STRING "BR0001:(B400) 'ACSN' UPD-DEL ERROR, STAT- "
058220                   DBIO-CALL-STATUS ", KEY- "
058230                   WA-ACSN-KEY
058240               DELIMITED BY SIZE INTO UT8500CA-INPUT-MSG
058250           PERFORM UT8500-CALL THRU UT8500-EXIT
058260           GO TO B739-EXIT.

058280     MOVE SAVE-ACLN-RECORD-AREA TO WA-AUDT-OLD-IMAGE.

058300     IF BR0001-S-DEL-FLAG = 'Y'
058310        MOVE SPACES             TO WA-AUDT-NEW-IMAGE
058320     ELSE
058330        MOVE ACLN-RECORD-AREA   TO WA-AUDT-NEW-IMAGE
LON236     END-IF.

058350 B739-EXIT.
058360     EXIT.
058370/
058380 B800-ACSN-AUDIT.
058390     MOVE STRT-AREA-CNTL-INFO TO WA-AUDT-CNTL-INFO.
058400     MOVE "B"                 TO WA-AUDT-SYS.
058410     MOVE STRT-AREA-BRANCH    TO WA-AUDT-HOME-BR.
058420     MOVE "ACSN"              TO WA-AUDT-DATASET.
058430     CALL "SY70SEND" USING

058450                           STARTAREA
058460                           WA-AUDT-AREA.
058470 B809-EXIT.
058480     EXIT.
058490/
058500 B850-ACLN-AUDIT.
058510     MOVE STRT-AREA-CNTL-INFO TO WA-AUDT-CNTL-INFO.
058520     MOVE "B"                 TO WA-AUDT-SYS.
058530     MOVE STRT-AREA-BRANCH    TO WA-AUDT-HOME-BR.
058540     MOVE "ACLN"              TO WA-AUDT-DATASET.
058550     CALL "SY70SEND" USING

058570                           STARTAREA
058580                           WA-AUDT-AREA.
058590 B859-EXIT.
058600     EXIT.
073920/
073930 C200-CALC-MTHLY.
073940     MOVE 0                      TO   UT0079CA-RET-STATUS.
073950     MOVE 03                     TO   UT0079CA-CALL-CODE.
073960     MOVE 0                      TO   UT0079CA-BLDV-FLAG.
073970     PERFORM C300-CALC-INT-DUE-DT THRU C309-EXIT
073980     MOVE WA-INT-DUE-DT          TO   UT0079CA-CAPN-DUE-DT.
073990     MOVE WA-TODAY-DATE          TO   UT0079CA-TDAY-DT.
074000     MOVE ZEROES                 TO   UT0079CA-NXT-WK-DT.
074010     MOVE BORM-ADV-DATE          TO   UT0079CA-ADV-DT.
074020     MOVE 0                      TO   UT0079CA-ADD-LN-CD.
074030     MOVE BORM-SOC-NO            TO   UT0079CA-BORM-SOC.
074040     MOVE BORM-MEMB-CUST-AC      TO   UT0079CA-BORM-ACC.
074050     MOVE BORM-BORR-TYPE         TO   UT0079CA-PROG-PAYMT-IND
074060     MOVE BORM-BACK-DT-IND       TO   UT0079CA-BKD-IND.
074070     IF BORM-CAPN-METHOD = " "
074080        MOVE BORP-CAPN-METHOD    TO   UT0079CA-CAPN-METHOD
074090     ELSE
074100        MOVE BORM-CAPN-METHOD    TO   UT0079CA-CAPN-METHOD.
074110     MOVE BORP-ACCRUE-FREQ       TO   UT0079CA-ACCRUE-FREQ.
074120     MOVE 0                      TO   UT0079CA-NO-DAYS-PER.
074130     MOVE BORM-APP-AMT           TO   UT0079CA-APPR-AMT.
074140     MOVE BORM-ADV-VAL           TO   UT0079CA-ADV-AMT.

074160     IF BORP-ACCRUE-METHOD = "4"
074170        MOVE BORM-LOAN-BAL
074180                                 TO        UT0079CA-INT-BASE-AMT
074190     ELSE
074200     IF (BORM-ADD-LOAN-CDE  > 0) AND
S12884        (BORM-ADD-LOAN-CDE NOT > 9) AND
S12884*       (BORM-ADD-LOAN-CDE  < 10) AND
074230        (BORP-ACCRUE-METHOD = "3" OR "2" OR "1")
074240        IF BORP-ACCRUE-METHOD = "3"
074250            SUBTRACT BORM-ADD-LOAN-ADV
074260                                 FROM      BORM-EOY-BASIC-BAL
074270                                 GIVING    UT0079CA-INT-BASE-AMT
074280            SUBTRACT BORM-PRINC-REDUCT-EFF
074290                                 FROM UT0079CA-INT-BASE-AMT
074300        ELSE
074310        IF BORP-ACCRUE-METHOD = "2"
074320            SUBTRACT BORM-ADD-LOAN-ADV
074330                                 FROM      BORM-EOM-BASIC-BAL
074340                                 GIVING    UT0079CA-INT-BASE-AMT
074350            SUBTRACT BORM-PRINC-REDUCT-EFF
074360                                FROM UT0079CA-INT-BASE-AMT
074370        ELSE
074380        IF BORP-ACCRUE-METHOD = "1"
074390            MOVE BORM-ADV-VAL
074400                                 TO UT0079CA-INT-BASE-AMT
074410            SUBTRACT BORM-PRINC-REDUCT-EFF
074420                                FROM UT0079CA-INT-BASE-AMT
074430        ELSE
074440            NEXT SENTENCE

074460     ELSE
074470     IF BORP-ACCRUE-METHOD = "3"
074480         MOVE BORM-EOY-BASIC-BAL
074490                                 TO        UT0079CA-INT-BASE-AMT
074500         SUBTRACT BORM-PRINC-REDUCT-EFF
074510                             FROM UT0079CA-INT-BASE-AMT
074520     ELSE
074530     IF BORP-ACCRUE-METHOD = "2"
074540         MOVE BORM-EOM-BASIC-BAL
074550                                 TO        UT0079CA-INT-BASE-AMT
074560         SUBTRACT BORM-PRINC-REDUCT-EFF
074570                             FROM UT0079CA-INT-BASE-AMT
074580     ELSE
074590     IF BORP-ACCRUE-METHOD = "1"
074600         MOVE BORM-ADV-VAL
074610                              TO        UT0079CA-INT-BASE-AMT
074620         SUBTRACT BORM-PRINC-REDUCT-EFF
S11909                             FROM UT0079CA-INT-BASE-AMT
S11909     ELSE
S11909     IF BORP-ACCRUE-METHOD = "7"
S11909         MOVE BORM-THEO-LOAN-BAL
S11909                              TO        UT0079CA-INT-BASE-AMT.
074690     IF UT0079CA-CALL-CODE NOT = 02
074700        IF UT0079CA-CAPN-METHOD = "6" OR "7"
074710           MOVE 04                 TO      UT0079CA-CALL-CODE
074720           MOVE BORM-INT-START-DATE  TO
074730                                           UT0079CA-INT-START-DATE
074740           MOVE BORM-INT             TO
074750                                           UT0079CA-INT-ACCM-AMT
074760           MOVE BORM-PDCC-VAL        TO
074770                                           UT0079CA-INT-BASE-AMT
074780        ELSE
074790        IF UT0079CA-CAPN-METHOD NOT NUMERIC
074800           MOVE 04 TO UT0079CA-CALL-CODE
074810           MOVE BORM-ADV-VAL
074820                            TO UT0079CA-INT-BASE-AMT
074830           SUBTRACT BORM-PRINC-REDUCT-EFF
074840                             FROM UT0079CA-INT-BASE-AMT
074850        ELSE
074860           MOVE 03                 TO      UT0079CA-CALL-CODE.

074880     MOVE BORM-INT-RATE          TO        UT0079CA-INT-RATE.
074890     MOVE BORP-COMM-INT          TO        UT0079CA-COMM-RATE.
074900     MOVE 0                      TO        UT0079CA-CI
074910                                           UT0079CA-BPI
074920                                           UT0079CA-LI.
074930*
074940     MOVE BORP-INT-DAYS-YR       TO    UT0079CA-BORM-INT-DAYS-YR.
074950     MOVE BORP-CALENDAR-CODE     TO    UT0079CA-CALENDAR-CODE.
074960     MOVE BORM-LOAN-REPAY        TO    UT0079CA-REPAY-AMT.
074970*
074980     MOVE BORP-CAPN-FREQ         TO    UT0079CA-CAPN-FREQ.
074990     IF BORM-REPAY-FREQ NOT = 0
075000         MOVE BORM-REPAY-FREQ TO UT0079CA-REPAY-FREQ
075010     ELSE
075020         MOVE BORP-REPAY-FREQ TO UT0079CA-REPAY-FREQ.
075030     MOVE BORP-ACCRUE-METHOD  TO UT0079CA-ACCRUE-METHOD.
075040*
075050     MOVE BORP-COFM-METH      TO UT0079CA-COFM-METH-FLAG.
075060     MOVE BORM-INT-ACCR       TO UT0079CA-LI-ACCR.
075070     MOVE BORM-BPI-ACCR       TO UT0079CA-BPI-ACCR.
075080     MOVE BORM-CI-ACCR        TO UT0079CA-CI-ACCR.
075090     MOVE BORM-ARR-ACCRUAL    TO UT0079CA-ARR-ACCR.
075100     MOVE BORM-TOM-ACRU-AMT   TO UT0079CA-TOM-ACRU.
075110     IF   BORM-TOM-RATE-TBL-ID = SPACE OR "0"
075120          MOVE BORP-TOM-RATE-TBL-ID
075130                              TO UT0079CA-TOM-RATE-TBL-ID
075140     ELSE
075150          MOVE BORM-TOM-RATE-TBL-ID
075160                              TO UT0079CA-TOM-RATE-TBL-ID
075170     END-IF.
075180     MOVE BORM-FCC-CLUS       TO UT0079CA-FCC-CLUS.
075190     MOVE BORM-INT-ADJUSTMENT
075200                              TO UT0079CA-INT-ADJ-AMT.
075210     CALL "UT0079" USING

075230            UT0079-LINKAGE.
075240     IF UT0079CA-RET-STATUS NOT = 0
075250         MOVE UT8500CA-FUNC-NORMAL-DISPLAY
075260           TO UT8500CA-CALL-CODE
075270         MOVE SPACES TO UT8500CA-INPUT-MSG
075280         STRING "BR0001:(C200)"
075290                ": UT0079 STATUS ERROR "
075300                 BORM-KEY  " "
075310                 UT0079CA-RET-STATUS
075320                DELIMITED BY SIZE INTO UT8500CA-INPUT-MSG
075330         CALL "UT8500" USING

075350             UT8500CA-AREA
075360         GO TO C209-EXIT.
075370 C209-EXIT.
075380     EXIT.
075390/
075400 C300-CALC-INT-DUE-DT.
075410*
075420     IF BORM-INT-START-DATE = WA-ZERO
075430        MOVE ZEROES TO WA-INT-ST-DATE
075440                       WA-INT-DUE-DT
075450         GO TO C309-EXIT
075460     ELSE
075470         MOVE BORM-INT-START-DATE TO       WA-PROC-DT-BINARY.
075480     MOVE WA-PROC-DT-BINARY      TO        WA-INT-ST-DATE
075490                                           UT0020-BINARY-DATE.
075500     MOVE 1                      TO        UT0020-FUNCTION.
075510     PERFORM Y010-CALL-UT0020 THRU Y019-EXIT.
075520     MOVE UT0020-DATE-DDMMCCYY   TO        WA-PROC-DDMMCCYY.
075530     MOVE WA-TODAY-DATE          TO        UT0020-BINARY-DATE.
075540     MOVE 1                      TO        UT0020-FUNCTION.
075550     PERFORM Y010-CALL-UT0020 THRU Y019-EXIT
075560     MOVE UT0020-DATE-DDMMCCYY   TO        WA-WORK-DDMMCCYY.
075570     SUBTRACT 1 FROM WA-WORK-CCYY-R.
075580 C301.
075590     IF WA-PROC-CCYY-R < WA-WORK-CCYY-R
075600        ADD 1                    TO        WA-PROC-CCYY-R
075610        GO TO C301.
075620     IF BORM-PAY-DAY         > 0
075630         MOVE BORM-PAY-DAY
075640                                 TO        WA-WORK-DD
075650     ELSE
075660     IF BORP-CAPN-DAY         > 0
075670         MOVE BORP-CAPN-DAY      TO        WA-WORK-DD.

075690     IF BORM-INT-REST         > 0
075700         MOVE BORM-INT-REST
075710                                 TO        WA-INT-FREQ
075720     ELSE
075730         MOVE BORP-CAPN-FREQ     TO        WA-INT-FREQ.
075740     IF WA-INT-FREQ = 98 AND BORM-TERM-BASIS = "M"
075750         MOVE BORM-LOAN-TRM      TO        WA-INT-FREQ.

075770     IF WA-INT-FREQ = 98 AND BORM-TERM-BASIS = "D"
075780         ADD BORM-LOAN-TRM TO WA-PROC-DT-BINARY
075790         GO TO C305.
075800     IF WA-WORK-DD = WA-PROC-DD
075810         ADD WA-INT-FREQ         TO        WA-PROC-MM
075820     ELSE
075830     IF WA-WORK-DD > WA-PROC-DD
075840         MOVE WA-WORK-DD         TO        WA-PROC-DD
075850     ELSE
075860         ADD 1                   TO        WA-PROC-MM
075870         MOVE WA-WORK-DD         TO        WA-PROC-DD.
075880 C302.
075890     IF WA-PROC-MM  > 12
075900         SUBTRACT 12             FROM      WA-PROC-MM
075910         ADD 1                   TO        WA-PROC-CCYY-R.
075920     MOVE WA-PROC-DD             TO        WA-WORK-DD.
075930     IF WA-PROC-MM = 2
075940         IF WA-PROC-DD > 28
075950*SPR:760098:Y2K:START------------------------------------------
075960            IF  WA-PROC-YY  = 0
075970                DIVIDE WA-PROC-CC  BY 4 GIVING WA-LEAP-REM1
075980                    REMAINDER WA-LEAP-REM
075990            ELSE
076000                DIVIDE WA-PROC-YY  BY 4 GIVING WA-LEAP-REM1
076010                    REMAINDER WA-LEAP-REM
076020            END-IF
076030            IF WA-LEAP-YEAR
076040               MOVE 29              TO        WA-PROC-DD
076050            ELSE
076060*SPR:760098:Y2K:END--------------------------------------------
076070               MOVE 28              TO        WA-PROC-DD.
076080     IF WA-PROC-MM = 4 OR 6 OR 9 OR 11
076090         IF WA-PROC-DD > 30
076100           MOVE 30               TO        WA-PROC-DD.
076110     IF WA-PROC-DD > 31
076120         MOVE 31                 TO        WA-PROC-DD.
076130     MOVE WA-PROC-DDMMCCYY       TO        UT0030-DATE-DDMMCCYY.
076140     PERFORM Y020-CALL-UT0030 THRU Y029-EXIT
076150     MOVE UT0030-BINARY-DATE     TO        WA-PROC-DT-BINARY.
076160     IF WA-PROC-DT-BINARY < WA-TODAY-DATE
076170         MOVE WA-PROC-DT-BINARY  TO        WA-INT-ST-DATE
076180         MOVE WA-WORK-DD         TO        WA-PROC-DD
076190         ADD WA-INT-FREQ         TO        WA-PROC-MM
076200         GO TO C302.
076210     MOVE WA-PROC-DT-BINARY      TO        WA-INT-DUE-DT.
076220 C305.
076230     ADD 1                       TO        WA-INT-ST-DATE.
076240     IF WA-INT-ST-DATE < BORM-INT-START-DATE
076250        MOVE BORM-INT-START-DATE  TO        WA-INT-ST-DATE.
076260 C309-EXIT.
076270     EXIT.
076280/
076290 D200-VALIDATE-BRANCH.
076300*
076310       IF WA-SAVE-BRANCH = 0
076320        MOVE 526 TO BR0001-OUTPUT-ERR-MSG-NO
076330        MOVE 1   TO WA-MSG-ERR-FLAG
076340        GO TO D209-EXIT.
076350*
TB0055*    MOVE SPACES           TO DBIO-CALL-KEY.
TB0055*    MOVE 71               TO DBIO-CALL-FUNC.
TB0055*    MOVE "BRHM"           TO DBIO-CALL-N-SYS
TB0055*    MOVE STRT-AREA-SOC-NO TO BRHM-SOC-NO
TB0055*    MOVE WA-SAVE-BRANCH   TO BRHM-BR-NO
TB0055     MOVE STRT-AREA-SOC-NO TO UTBRHMCA-SOC-NO.
TB0055     MOVE WA-SAVE-BRANCH   TO UTBRHMCA-BRCH-NO.
TB0055*    MOVE BRHM-KEY         TO DBIO-CALL-KEY.
TB0055*    CALL "DBIO"   USING

TB0055*                        DBIO-CALL-PARAM
TB0055*                        DBIO-CALL-SYS-AREA
TB0055*                        DBIO-CALL-KEY-AREA
TB0055*                        DBIO-CALL-STATUS
TB0055*                        BRHM-RECORD
TB0055     MOVE 2           TO UTBRHMCA-CALL-CODE.
TB0055     CALL "UTBRHM" USING UTBRHMCA-CALL-AREA
TB0055                         UTBRHMRA-CALL-AREA.
TB0055*    IF DBIO-CALL-STATUS NOT = "****"
TB0055*       IF DBIO-CALL-STATUS = "MRNF"
TB0055     IF UTBRHMCA-STATUS  NOT = 0
TB0055        IF UTBRHMCA-STATUS  = 2
076580           MOVE 1   TO WA-MSG-ERR-FLAG
076590           MOVE 526 TO BR0001-OUTPUT-ERR-MSG-NO
076600        ELSE
076610        MOVE UT8500CA-FUNC-NORMAL-DISPLAY TO UT8500CA-CALL-CODE
076620        MOVE SPACES TO UT8500CA-INPUT-MSG
TB0055*       STRING "BR0001:(D200) BRHM ERROR " DBIO-CALL-STATUS
TB0055        STRING "BR0001:(D200) BRHM ERROR " UTBRHMCA-STATUS
076650                " " STRT-AREA-SOC-NO "/" WA-SAVE-BRANCH
076660            DELIMITED BY SIZE INTO UT8500CA-INPUT-MSG
076670        PERFORM UT8500-CALL THRU UT8500-EXIT
076680        MOVE 1   TO WA-MSG-ERR-FLAG
076690        MOVE 155 TO BR0001-OUTPUT-ERR-MSG-NO
076700        MOVE "BRHM" TO BR0001-OUTPUT-DATA-SET
076710        MOVE DBIO-CALL-STATUS TO BR0001-OUTPUT-STATUS
LON619     END-IF.
076720*
076730 D209-EXIT.
076740     EXIT.
077120/
078610 F000-SEND-17000.

LON236     IF STRT-AREA-TRN-NO = 17050
LON236     AND NOT BR0001-SH-ENQ-TYPE-VALUE
LON236        MOVE 5549        TO BR0001-OUTPUT-ERR-MSG-NO
LON236        MOVE 1           TO WA-MSG-ERR-FLAG
LON236        GO TO F099-EXIT
LON236     END-IF.
LON236*
LON236*----------------------------------------------------------------
LON236* OPTION 2 CHANGE SHORT LONG NAME
LON236*----------------------------------------------------------------
078730     IF BR0001-SH-ENQ-TYPE = 2
078740        PERFORM F900-SHORT-LONG-NAME  THRU F909-EXIT
078750        GO TO F099-EXIT.
078760
LON236*----------------------------------------------------------------
LON236* OPTION 3 CHANGE PRODUCT TYPE
LON236*----------------------------------------------------------------
LON236     IF BR0001-SH-ENQ-TYPE = 3
078840        PERFORM F740-SETUP-17046      THRU F749-EXIT
078850        GO TO F099-EXIT.
LON236
LON236*----------------------------------------------------------------
LON236*  17099 FOR LOAN RENEWAL
LON236*----------------------------------------------------------------
LON236*  IF BR0001-SH-ENQ-TYPE = 4 OR STRT-AREA-TRN-NO = 17099
LON236     IF STRT-AREA-TRN-NO = 17099
TB0105        PERFORM F950-SETUP-PENDING-RENWL THRU F950-EXIT
TB0105        GO TO F099-EXIT.
TB0105
LON236*----------------------------------------------------------------
LON236* OPTION 4 LOAN EXTENSION NO NEED HERE
LON236*----------------------------------------------------------------
LON236*SC1223   IF BR0001-SH-ENQ-TYPE = 9
LON236*    IF BR0001-SH-ENQ-TYPE = 4
LON236*TB0105        PERFORM F960-LOAN-EXTENSION      THRU F960-EXIT
LON236*TB0105        GO TO F099-EXIT.

084570 F099-EXIT.
084580     EXIT.
084590/
084600 F100-RETRIEVE-CUSM.
084610*
084620     MOVE STRT-AREA-SOC-NO       TO CUSM-SOC-NO.
084630     MOVE 71                     TO DBIO-CALL-FUNC.
084640     MOVE CUSM-KEY               TO DBIO-CALL-KEY-AREA.
084650     MOVE "CUSM"                 TO DBIO-CALL-SYS-AREA.
084660*
084670     PERFORM U100-CUSM-ACCESS THRU U199-EXIT.
084680*
084690     IF  DBIO-CALL-STATUS NOT = "****"
084700         MOVE 1                  TO WA-MSG-ERR-FLAG
084710         MOVE 155                TO BR0001-OUTPUT-ERR-MSG-NO
084720         MOVE "CUSM"             TO BR0001-OUTPUT-DATA-SET
084730         MOVE DBIO-CALL-STATUS   TO BR0001-OUTPUT-STATUS
084740         MOVE UT8500CA-FUNC-NORMAL-DISPLAY
084750                          TO UT8500CA-CALL-CODE
084760         MOVE SPACES TO UT8500CA-INPUT-MSG
084770         STRING "BR0001:(F100) READ CUSM ERROR, STAT "
084780                 DBIO-CALL-STATUS "KEY = " DBIO-CALL-KEY-AREA
084790                 DELIMITED BY SIZE INTO UT8500CA-INPUT-MSG
084800         CALL "UT8500" USING
084810                               UT8500CA-AREA
084820         GO TO F199-EXIT.
084830*
084840 F199-EXIT.
084850     EXIT.
084860/
084870 F200-RETRIEVE-NAME-ADDR.
084880      CONTINUE.
084890 F210.
084900     MOVE 2                      TO UT0500CA-CALL-CODE.
084910     MOVE STRT-AREA-SOC-NO       TO UT0500CA-INST-NO.
S10093     IF STRT-AREA-TRN-NO = 17053 AND BR0001-17053-CONTRACT
S10093        MOVE CONB-CUSTOMER-NO       TO UT0500CA-CUSM-NO
S10093        MOVE CONB-BRANCH-CODE       TO UT0500CA-BRHM-NO
S10093     ELSE
084960        MOVE BR0001-INPUT-KEY-R     TO UT0500CA-CUSM-NO
084970        MOVE BORM-BR-NO             TO UT0500CA-BRHM-NO
S10093     END-IF
084990     MOVE STRT-AREA-TELLER-LANG  TO UT0500RA-LANGUAGE-CODE.
085000     MOVE 0                      TO UT0500CA-INST-LANGUAGE-CODE.
085010*
085020     CALL "UT0500" USING

085040                   UT0500-CALL-PARAM.
085050*
S14015     IF STRT-AREA-TRN-NO = 17053
S14015         GO TO F299-EXIT
S14015     END-IF.
LON236*
085120 F299-EXIT.
085130     EXIT.
085140/
086350*-----------------------------------------------------------
086380* GET THE LASTEST POSTING RESTRICTION RECORD FROM BORV
086390* FOR THIS ACCOUNT
086400*-----------------------------------------------------------
088020 F740-SETUP-17046.
088030*
088040* SET UP 17046 : ACCT TYPE / HOME BRANCH CHANGE
088050*
088080     MOVE BR0001-INPUT-KEY-R TO UT0151CA-ACCT-IN.
088090     MOVE SOC-ACCT-NO-FORMAT TO UT0151CA-FORMAT.
088100     PERFORM S100-REFORMAT-ACCOUNT-NO THRU S199-EXIT.
088110     MOVE UT0151CA-ACCT-EX  TO BR0001-OUT-17046-ACCT.
088120     MOVE BORM-BR-NO        TO BR0001-OUT-17046-OLD-BRANCH
088130                               BR0001-OUT-17046-NEW-BRANCH
088140     MOVE BORM-CURRENCY-IND TO BR0001-OUT-17046-CURRENCY
088150     MOVE BORM-ACT-TYPE     TO BR0001-OUT-17046-OLD-ACCT-TYPE
088160                               BR0001-OUT-17046-NEW-ACCT-TYPE
088170     MOVE BORM-CAT          TO BR0001-OUT-17046-OLD-ACCT-CAT
088180                               BR0001-OUT-17046-NEW-ACCT-CAT
088190     MOVE BORM-TOM-RATE-TBL-ID TO BR0001-OUT-17046-TOM-ID.
S10842     MOVE BORM-STORE-RATE   TO BR0001-OUT-17046-RATE.
S10842     MOVE BOIS-STRETCH-TERM TO BR0001-OUT-17046-STRETCH-TERM.
SC2451     MOVE 0 TO BR0001-OUT-17046-IND
SC3766     MOVE 01                     TO UT0500CA-SUBCODE
SC3766     MOVE 2                      TO UT0500CA-CALL-CODE.
SC3766     MOVE STRT-AREA-SOC-NO       TO UT0500CA-INST-NO.
SC3766     MOVE BR0001-INPUT-KEY-R     TO UT0500CA-CUSM-NO.
SC3766     MOVE BORM-BR-NO             TO UT0500CA-BRHM-NO.
SC3766     MOVE STRT-AREA-TELLER-LANG  TO UT0500RA-LANGUAGE-CODE.
SC3766     MOVE 0                      TO UT0500CA-INST-LANGUAGE-CODE.
SC3766     CALL "UT0500" USING
SC3766                   UT0500-CALL-PARAM.
SC3766     IF UT0500RA-CALL-STATUS NOT = 0
088510        MOVE 1            TO WA-MSG-ERR-FLAG
088520        MOVE 155          TO BR0001-OUTPUT-ERR-MSG-NO
SC3766        MOVE "UT0500"     TO BR0001-OUTPUT-DATA-SET
088550        MOVE DBIO-CALL-STATUS TO BR0001-OUTPUT-STATUS
088560        MOVE UT8500CA-FUNC-NORMAL-DISPLAY TO
088570              UT8500CA-CALL-CODE
088580        MOVE SPACES TO UT8500CA-INPUT-MSG
SC3766        STRING "BR0001:(F740) 'UT0500' READ ERROR, STAT- "
088610                 DBIO-CALL-STATUS ", KEY- " DBIO-CALL-KEY
088620             DELIMITED BY SIZE INTO UT8500CA-INPUT-MSG
088630        PERFORM UT8500-CALL THRU UT8500-EXIT
088640        GO TO F749-EXIT.
SC3766     MOVE UT0500RA-FULL-NAME     TO BR0001-OUT-17046-NAME.

088670     MOVE BORP-CAT-TYPE-NAME TO BR0001-OUT-17046-DESC.
088680     MOVE 998    TO BR0001-OUTPUT-ERR-MSG-NO.
S10842     MOVE 158    TO BR0001-OUTPUT-MSG-LENGTH.
088730     ADD  6      TO STRT-AREA-IEXEC-CNTL.
088740     MOVE 17046  TO STRT-AREA-TRN-NO.
088750     MOVE 0      TO WA-MSG-ERR-FLAG.
S10614*
S10614     MOVE "N"    TO WS-CHANGE-OVERRIDE.
S10614     IF ( STRT-AREA-TRN-NO = 17046
S10614     AND  STRT-AREA-OTHER-OVERRIDE = "1"
S11144     AND (STRT-FEP-ATM-TXN OR STRT-MORTGAGE-TXN))
S10614          MOVE STRT-AREA-OTHER-OVERRIDE TO WS-OTHER-OVERRIDE
S10614          MOVE "Y"  TO WS-CHANGE-OVERRIDE
S10614          MOVE " "  TO STRT-AREA-OTHER-OVERRIDE
S10614     END-IF.
S10614*
088870     PERFORM Z000-FORMT-SEND-PARA THRU Z099-EXIT.
S10614*
S10614     IF ( WS-CHANGE-OVERRIDE = "Y" )
S10614          MOVE WS-OTHER-OVERRIDE TO STRT-AREA-OTHER-OVERRIDE
S10614     END-IF.
S10614*
088930 F749-EXIT.
088940     EXIT.
088950/
094450 F830-UPDATE-17046.
094460*
094470* VALIDATE THE INPUT
094480*
094490     MOVE 0                      TO BR0001CA-DR-INT
094500                                    BR0001CA-CR-INT
094510                                    BR0001CA-TOM-CR-ACRU
094520                                    BR0001CA-TOM-DR-ACRU
094530                                    BR0001CA-FCC-DR-ACRU
094540                                    BR0001CA-FCC-ARR-ACRU
094550                                    BR0001CA-FCC-PRNC-ADJ.

LON435**S13836     PERFORM Z200-VALIDATE-CHANGE THRU Z200-EXIT.
LON435     IF BORM-LAST-ACCT-TYPE-CHANGE = STRT-AREA-POST-DT
LON435        IF BR0001-IN-17046-OLD-ACCT-CAT NOT =
LON435           BR0001-IN-17046-NEW-ACCT-CAT
LON435           MOVE 7688 TO BR0001-OUTPUT-ERR-MSG-NO
LON435           MOVE 1    TO WA-MSG-ERR-FLAG
LON435           GO TO F839-EXIT
LON435        END-IF
LON435        IF BR0001-IN-17046-OLD-BRANCH   NOT =
LON435           BR0001-IN-17046-NEW-BRANCH
LON435           MOVE 7689 TO BR0001-OUTPUT-ERR-MSG-NO
LON435           MOVE 1    TO WA-MSG-ERR-FLAG
LON435           GO TO F839-EXIT
LON435        END-IF
LON435     END-IF.
LON435**S13836     IF WA-MSG-ERR-FLAG = 1
LON435**S13836        GO TO F839-EXIT
LON435**S13836     END-IF.
S13836     IF ( BOIS-OFF-BAL-FUND-PERC IS NUMERIC ) AND
S13836        ( BOIS-OFF-BAL-FUND-PERC NOT = ZEROS ) AND
S13836        (BOIS-FUND-AGREEMENT-NO NOT = SPACES AND ZERO ) AND
S13836        (BR0001-IN-17046-OLD-ACCT-CAT NOT =
S13836        BR0001-IN-17046-NEW-ACCT-CAT)
S13836        MOVE 7676 TO BR0001-OUTPUT-ERR-MSG-NO
S13836        MOVE 1   TO WA-MSG-ERR-FLAG
S13836        GO TO F839-EXIT
S13836     END-IF.
S13888     IF ((BR0001-IN-17046-OLD-ACCT-CAT NOT =
S13888         BR0001-IN-17046-NEW-ACCT-CAT) AND
S13888        (BR0001-IN-17046-NEW-ACCT-CAT = "0000"))
S13888         MOVE 7788 TO BR0001-OUTPUT-ERR-MSG-NO
S13888         MOVE 1   TO WA-MSG-ERR-FLAG
S13888         GO TO F839-EXIT
S13888     END-IF.
SC2451     IF NOT BORP-COMML
094580      IF BR0001-IN-17046-OLD-ACCT-TYPE
094590                     = BR0001-IN-17046-NEW-ACCT-TYPE
094600        AND BR0001-IN-17046-OLD-ACCT-CAT
094610                     = BR0001-IN-17046-NEW-ACCT-CAT
094620        AND BR0001-IN-17046-OLD-BRANCH
094630                     = BR0001-IN-17046-NEW-BRANCH
094640           MOVE 2111 TO BR0001-OUTPUT-ERR-MSG-NO
094650           MOVE 1    TO WA-MSG-ERR-FLAG
094660           GO TO F839-EXIT
094670      END-IF
SC2451     END-IF.
094690     IF BR0001-IN-17046-OLD-ACCT-TYPE NOT = BORM-ACT-TYPE
094700       OR BR0001-IN-17046-OLD-ACCT-CAT NOT = BORM-CAT
094710       OR BR0001-IN-17046-OLD-BRANCH  NOT = BORM-BR-NO
094720           MOVE 2118 TO BR0001-OUTPUT-ERR-MSG-NO
094730           MOVE 1    TO WA-MSG-ERR-FLAG
094740           GO TO F839-EXIT
094750     END-IF.

S13420     IF SOC-PBOOK-ACCT-CHANGE-YES
S13420     AND STRT-BANCSLINK
S13420         PERFORM F833-VALIDATE-PREBOOK    THRU F833-EXIT
S13420     END-IF.

094820     MOVE BORM-RECORD-AREA TO WA-AUDT-OLD-IMAGE
094830     IF BR0001-IN-17046-NEW-BRANCH = BR0001-IN-17046-OLD-BRANCH
094840        GO TO F831
094850     END-IF.
094860     IF BR0001-IN-17046-OLD-BRANCH NOT = STRT-AREA-BRANCH
094870        MOVE 2112 TO BR0001-OUTPUT-ERR-MSG-NO
094880        MOVE 1   TO WA-MSG-ERR-FLAG
094890        GO TO F839-EXIT
094900     END-IF.
094910*
094920     IF BR0001-IN-17046-OLD-ACCT-TYPE NOT =
094930              BR0001-IN-17046-NEW-ACCT-TYPE  OR
094940        BR0001-IN-17046-OLD-ACCT-CAT NOT =
094950              BR0001-IN-17046-NEW-ACCT-CAT
094960        MOVE 2113 TO BR0001-OUTPUT-ERR-MSG-NO
094970        MOVE 1   TO WA-MSG-ERR-FLAG
094980        GO TO F839-EXIT
094990     END-IF.
095000     MOVE BR0001-IN-17046-NEW-BRANCH TO WA-SAVE-BRANCH
095010     PERFORM D200-VALIDATE-BRANCH THRU D209-EXIT
095020     IF WA-MSG-ERR-FLAG = 1
095030        GO TO F839-EXIT
095040     END-IF.
095050     MOVE BR0001-IN-17046-NEW-BRANCH TO BORM-BR-NO.
S13420     IF SOC-PBOOK-ACCT-CHANGE-YES
S13420     AND STRT-BANCSLINK
S13420         PERFORM F833-VALIDATE-PREBOOK    THRU F833-EXIT
S13420     END-IF.
SC4108     MOVE "LON" TO BR0001-IN-17046-SYS
S13681     IF ST-STATUS-LEVEL-1 NOT = 0
SC4108        MOVE 1   TO WA-MSG-ERR-FLAG
SC4108        GO TO F839-EXIT.
095350 F831.
095360*
095370* VALIDATE THE NEW ACCT TYPE AND INTEREST CATEGORY
095380*
095410*    WA-SAVE-BORP-FLAG IS NOT USED ANYWHERE BUT HERE WITHIN THE
095420*    SCOPE OF THE 17046 TXN. IT IS SET TO "N" WHEN THE 17046 IS
095430*    SCHEDULED BUT WE CAN'T RELY ON THAT SETTING REMAINING IN
095440*    FORCE FROM THAT TRANSACTION TO THIS. ANOTHER, UNRELATED
095450*    17046 TXN, COULD BE PROCESSED IN THE MEANTIME, LEAVING IT
095460*    SET TO "Y" AND WE WOULD THEN BE USING THE SAVED VALUES FROM
095470*    THE UNRELATED TXN, NOT THE CORRECT ONES FOR THE CURRENT TXN
095480*
S10454     MOVE BORP-LOAN-CLASS         TO WA-SAVE-LOAN-CLASS
095510     MOVE BORP-CAPN-METHOD        TO WA-SAVE-CAPN-METHOD
095520     MOVE BORP-CAPN-FREQ-EOFY     TO WA-SAVE-CAPN-FREQ-EOFY
095530     MOVE BORP-CAPN-FREQ          TO WA-SAVE-CAPN-FREQ
095540     MOVE BORP-RESIDUAL-IND       TO WA-SAVE-RESIDUAL-IND
095550     MOVE BORP-ACCRUE-FREQ        TO WA-SAVE-ACCRUE-FREQ
095560     MOVE BORP-REPAY-FREQ         TO WA-SAVE-REPAY-FREQ
095570     MOVE BORP-INT-ADV            TO WA-SAVE-INT-ADV
095580     MOVE BORP-CURRENCY-IND       TO WA-SAVE-CURRENCY-IND
095590     MOVE BORP-TOM-RATE-TBL-ID TO WA-SAVE-TOM-RATE-TBL-ID
S10237     MOVE BORP-REPAY-DAY          TO WA-OLD-PRO-REPAY-DAY
S11215     MOVE BORP-FINE-METH          TO WA-SAVE-FINE-METH
S11215     MOVE BORP-FINE-CONDT         TO WA-SAVE-FINE-CONDT
095630     MOVE BORP-REPAY-METHOD       TO WA-SAVE-REPAY-METHOD.
S13203     MOVE BORP-CONTRACT-REQUIRED-IND TO WA-CON-REQD-IND.
S13203     MOVE BORP-NEG-RATE-IND       TO WA-NEGO-RATE-IND.
S13827     MOVE BORP-EFR-IND            TO WA-EFR-IND.
S13827     MOVE BORP-RATE-OPT-APPLIC TO WA-RATE-OPT-APPLIC.
S13203     MOVE BORP-PROVISIONING       TO WA-PROVISION.
S13203     MOVE BORP-LOAN-COMMITMENT TO WA-COMMITMENT.
S13203     MOVE BORP-PL-NPL-APPL        TO WA-PL-NPL-APPL.
S13203     MOVE BORP-AUTO-RISK-IND      TO WA-AUTO-RISK-IND.
S13203     MOVE BORP-MANUAL-RISK-IND TO WA-MANUAL-RISK-IND.
S13203     MOVE BORP-INT-CAT-REQUIRED-IND TO WA-INT-CAT-REQD.
095660     MOVE 02                  TO UT2000CA-CALL-CODE
095670     MOVE ZERO                TO UT2000CA-CALL-INDEX
095680     MOVE STRT-AREA-SOC-NO    TO UT2000CA-CALL-SOC-NO
095690     MOVE BR0001-IN-17046-NEW-ACCT-TYPE
095700                              TO UT2000CA-CALL-ACCT-TYPE
095710     MOVE BR0001-IN-17046-NEW-ACCT-CAT
095720                              TO UT2000CA-CALL-INT-CAT
095730     CALL "UT2000" USING

095750                       UT2000CA-CALL-AREA
095760     IF UT2000CA-CALL-STATUS NOT = ZERO
S13624         MOVE 201             TO BR0001-OUTPUT-ERR-MSG-NO
LB0113         MOVE 1               TO WA-MSG-ERR-FLAG
LB0113         MOVE UT8500CA-FUNC-NORMAL-DISPLAY
LB0113                              TO UT8500CA-CALL-CODE
LB0113         MOVE SPACES           TO UT8500CA-INPUT-MSG
LB0113         STRING "BR0001:(F831) "
LB0113         ", ACCT = " BR0001-IN-17046-NEW-ACCT-TYPE
LB0113         ", CAT = " BR0001-IN-17046-NEW-ACCT-CAT
LB0113           DELIMITED BY SIZE INTO UT8500CA-INPUT-MSG
LB0113         CALL "UT8500" USING UT8500CA-AREA
095880         GO TO F839-EXIT
095890     ELSE
LON327*CHKING         DISPLAY "RST:PARAMETER CALL OK"
095900         MOVE LS01-BORR-PARAMS   TO WA-SAVE-BORPARM-AREA
095910         MOVE WA-UT2000-CALL-REC TO LS01-BORR-PARAMS.
S13624*S13203    IF  BORP-CONTRACT-REQUIRED-IND NOT = WA-CON-REQD-IND
S13624*S13203    OR  (BR0001-IN-17046-NEW-ACCT-TYPE NOT =
S13624     IF  (BR0001-IN-17046-NEW-ACCT-TYPE NOT =
S13203         BR0001-IN-17046-OLD-ACCT-TYPE)
S13624         MOVE 7680 TO BR0001-OUTPUT-ERR-MSG-NO
S13624         MOVE 1    TO WA-MSG-ERR-FLAG
S13624         GO TO F839-EXIT
S13624     END-IF
S13624     IF  BORP-CONTRACT-REQUIRED-IND NOT = WA-CON-REQD-IND
S13624         MOVE 7679 TO BR0001-OUTPUT-ERR-MSG-NO
S13624         MOVE 1    TO WA-MSG-ERR-FLAG
S13624         GO TO F839-EXIT
S13624     END-IF
S13624*S13203    OR  (BORP-CURRENCY-IND NOT = WA-SAVE-CURRENCY-IND)
S13624     IF BORP-CURRENCY-IND NOT = WA-SAVE-CURRENCY-IND
S13624         MOVE 7681 TO BR0001-OUTPUT-ERR-MSG-NO
S13624         MOVE 1    TO WA-MSG-ERR-FLAG
S13624         GO TO F839-EXIT
S13624     END-IF
LON327*S13836     DISPLAY "NEW PROD NEG RATE = " BORP-NEG-RATE-IND
LON327*S13836     DISPLAY "NEW PROD EFR IND = "  BORP-EFR-IND
LON327*S13836     DISPLAY "NEW PROD OPT APPLIC = " BORP-RATE-OPT-APPLIC
LON327*S13836     DISPLAY "OLD PROD NEG RATE = " WA-NEGO-RATE-IND
LON327*S13836     DISPLAY "OLD PROD EFR IND  = " WA-EFR-IND
LON327*S13836     DISPLAY "OLD PROD OPT APPLIC = " WA-RATE-OPT-APPLIC
S13624*S13203    OR  (BORP-NEG-RATE-IND NOT = WA-NEGO-RATE-IND)
S13624     IF ((BORP-NEG-RATE-IND = 0 AND BORP-EFR-IND = " ") AND
S13827      (WA-NEGO-RATE-IND = "0"  AND WA-EFR-IND = " "))  OR
S13827      ((BORP-NEG-RATE-IND = 0 AND BORP-EFR-IND NOT = " ") AND
S13827      (WA-NEGO-RATE-IND = "0"  AND WA-EFR-IND NOT = " ")) OR
S13827      (BORP-NEG-RATE-IND = 3 AND WA-NEGO-RATE-IND = "3") OR
S13827      ((BORP-NEG-RATE-IND = 2 AND BORP-RATE-OPT-APPLIC = "Y") AND
S13827      (WA-NEGO-RATE-IND = "2" AND WA-RATE-OPT-APPLIC = "Y")) OR
S13827      ((BORP-NEG-RATE-IND = 2 AND BORP-RATE-OPT-APPLIC = "N") AND
S13827      (WA-NEGO-RATE-IND = "2" AND WA-RATE-OPT-APPLIC = "N"))
S13827            CONTINUE
S13827     ELSE
S13624         MOVE 7682 TO BR0001-OUTPUT-ERR-MSG-NO
S13624         MOVE 1    TO WA-MSG-ERR-FLAG
S13624         GO TO F839-EXIT
S13624     END-IF
S13624*S13203    OR  (BORP-PROVISIONING NOT = WA-PROVISION)
S13624     IF  BORP-PROVISIONING NOT = WA-PROVISION
S13624         MOVE 7683 TO BR0001-OUTPUT-ERR-MSG-NO
S13624         MOVE 1    TO WA-MSG-ERR-FLAG
S13624         GO TO F839-EXIT
S13624     END-IF
S13624*S13203    OR  (BORP-LOAN-COMMITMENT NOT = WA-COMMITMENT)
S13624     IF  BORP-LOAN-COMMITMENT NOT = WA-COMMITMENT
S13624         MOVE 7684 TO BR0001-OUTPUT-ERR-MSG-NO
S13624         MOVE 1    TO WA-MSG-ERR-FLAG
S13624         GO TO F839-EXIT
S13624     END-IF
S13624*S13203    OR  (BORP-PL-NPL-APPL  NOT = WA-PL-NPL-APPL)
S13624     IF  BORP-PL-NPL-APPL  NOT = WA-PL-NPL-APPL
S13624         MOVE 7685 TO BR0001-OUTPUT-ERR-MSG-NO
S13624         MOVE 1    TO WA-MSG-ERR-FLAG
S13624         GO TO F839-EXIT
S13624     END-IF
S13624*S13203    OR  (BORP-AUTO-RISK-IND NOT = WA-AUTO-RISK-IND)
S13624     IF  (BORP-AUTO-RISK-IND NOT = WA-AUTO-RISK-IND)
S13203     OR  (BORP-MANUAL-RISK-IND NOT = WA-MANUAL-RISK-IND)
S13624*S13203    OR  (BORP-INT-CAT-REQUIRED-IND NOT = WA-INT-CAT-REQD)
S13203         MOVE 7556 TO BR0001-OUTPUT-ERR-MSG-NO
S13203         MOVE 1               TO WA-MSG-ERR-FLAG
S13203         MOVE UT8500CA-FUNC-NORMAL-DISPLAY
S13203              TO UT8500CA-CALL-CODE
S13203         MOVE SPACES           TO UT8500CA-INPUT-MSG
LB0113         STRING "BR0001:(F831) "
LB0113         ", ACCT = " BR0001-IN-17046-NEW-ACCT-TYPE
LB0113         ", CAT = " BR0001-IN-17046-NEW-ACCT-CAT
LB0113           DELIMITED BY SIZE INTO UT8500CA-INPUT-MSG
LB0113         CALL "UT8500" USING UT8500CA-AREA
S13203         GO TO F839-EXIT
S13203     END-IF.
SC7357     IF BOIS-NO-OF-RENEWAL = 0
096090        IF BORM-LOAN-TRM > BORP-MAX-TERM OR
096100           BORM-LOAN-TRM < BORP-MIN-TERM
S14011           IF WA-SAVE-ST-AREA-TRN-NO NOT = 17475
LON155              AND (NOT STRT-TRICKLE-TXN)
096110              MOVE 771  TO BR0001-OUTPUT-ERR-MSG-NO
096120              MOVE 1    TO WA-MSG-ERR-FLAG
096130              GO TO F839-EXIT
S14011           END-IF
SC7357        END-IF
SC7357     ELSE
SC7357        COMPUTE WA-MAX-TERM = ( 1 + BOIS-NO-OF-RENEWAL )
SC7357                            * BORP-MAX-TERM
SC7357        IF BORM-LOAN-TRM > WA-MAX-TERM
SC7357        OR BORM-LOAN-TRM < BORP-MIN-TERM
S14011           IF WA-SAVE-ST-AREA-TRN-NO NOT = 17475
LON155              AND (NOT STRT-TRICKLE-TXN)
SC7357              MOVE 771  TO BR0001-OUTPUT-ERR-MSG-NO
SC7357              MOVE 1    TO WA-MSG-ERR-FLAG
SC7357              GO TO F839-EXIT
S14011           END-IF
SC7357        END-IF
SC7357     END-IF.
S13064*SC7125    IF NOT BORP-PRIORITY-TERM-BY-TERM
S13064    IF NOT BOIS-PRIORITY-TERM-BY-TERM
096270     IF BORP-CAPN-METHOD    NOT = WA-SAVE-CAPN-METHOD OR
096280        BORP-CAPN-FREQ-EOFY NOT = WA-SAVE-CAPN-FREQ-EOFY OR
096290        BORP-CAPN-FREQ      NOT = WA-SAVE-CAPN-FREQ OR
096300        BORP-RESIDUAL-IND   NOT = WA-SAVE-RESIDUAL-IND OR
096310        BORP-ACCRUE-FREQ    NOT = WA-SAVE-ACCRUE-FREQ OR
096320        BORP-REPAY-FREQ     NOT = WA-SAVE-REPAY-FREQ OR
096330        BORP-INT-ADV        NOT = WA-SAVE-INT-ADV OR
096340        BORP-CURRENCY-IND   NOT = WA-SAVE-CURRENCY-IND
096350     OR BORP-SERIAL-REPAY-METHOD AND
096360        NOT WA-SAVE-SERIAL-REPAY-METHOD
096370     OR NOT BORP-SERIAL-REPAY-METHOD AND
096380        WA-SAVE-SERIAL-REPAY-METHOD
LB0113         MOVE BORP-ACCRUE-FREQ TO WA-XX
LB0113         MOVE BORP-REPAY-FREQ  TO WA-YY
LB0113         MOVE 4253            TO BR0001-OUTPUT-ERR-MSG-NO
LB0113         MOVE 1               TO WA-MSG-ERR-FLAG
LB0113         MOVE UT8500CA-FUNC-NORMAL-DISPLAY
LB0113                              TO UT8500CA-CALL-CODE
LB0113         MOVE SPACES          TO UT8500CA-INPUT-MSG
LB0113             STRING "BR0001:(F831)"
LB0113             ", CAPN " BORP-CAPN-METHOD
LB0113             ", " WA-SAVE-CAPN-METHOD
LB0113             ", FEOF " BORP-CAPN-FREQ-EOFY
LB0113             ", " WA-SAVE-CAPN-FREQ-EOFY
LB0113             ", FCAPN " BORP-CAPN-FREQ
LB0113             ", " WA-SAVE-CAPN-FREQ
LB0113             ", RESID " BORP-RESIDUAL-IND
LB0113             ", " WA-SAVE-RESIDUAL-IND
LB0113             ", ACCF " WA-XX
LB0113             ", " WA-SAVE-ACCRUE-FREQ
LB0113             ", RFRQ " WA-YY
LB0113             ", " WA-SAVE-REPAY-FREQ
LB0113             ", INTA " BORP-INT-ADV
LB0113             ", " WA-SAVE-INT-ADV
LB0113             ", CURI " BORP-CURRENCY-IND
LB0113             ", " WA-SAVE-CURRENCY-IND
LB0113                 DELIMITED BY SIZE INTO UT8500CA-INPUT-MSG
LB0113             CALL "UT8500" USING UT8500CA-AREA
096660         GO TO F839-EXIT
SC7125      END-IF
096680     END-IF.

S10517     IF BR0001-IN-17046-OLD-ACCT-TYPE NOT =
S10517              BR0001-IN-17046-NEW-ACCT-TYPE
S10517        IF BOIS-SORD-AUTO-DBT-ACCT-NO NOT = 0
S10517           IF BORM-REPAY-SCHED = "N" OR "J" OR "K"
S10517              GO TO F839-EXIT
S10517           ELSE
S10517              PERFORM Y040-SORD-CREATE-MAINT THRU Y049-EXIT
S10517              IF WA-MSG-ERR-FLAG = 1
S10517                 GO TO F839-EXIT
S10517              END-IF
S10517           END-IF
S10517        END-IF
S10517     END-IF.

LON582     IF BR0032CA-TXN-NO NOT = 17475
097350        MOVE STRT-AREA-POST-DT     TO BORM-LAST-ACCT-TYPE-CHANGE
LON582     END-IF.
097370     MOVE BR0001-IN-17046-NEW-ACCT-TYPE TO BORM-ACT-TYPE.
097380     MOVE BR0001-IN-17046-NEW-ACCT-CAT  TO BORM-CAT.
097390     IF WA-SAVE-TOM-RATE-TBL-ID = BR0001-IN-17046-TOM-ID
097400         MOVE BORP-TOM-RATE-TBL-ID TO BORM-TOM-RATE-TBL-ID
097410     END-IF.
097420 F832.
097640*
097650* WRITE A BORV
097660*
097670     INITIALIZE BORV-47-DETAIL
097680     MOVE 47                        TO BORV-47-TRAN-TYPE
097690     MOVE 02                        TO BORV-47-TRAN-STATUS
097700     MOVE STRT-AREA-BRANCH          TO BORV-47-BRANCH
097710     MOVE STRT-AREA-TERM-NO         TO BORV-47-BRANCH-TERM
097720     MOVE STRT-AREA-TELL-NO         TO BORV-47-TELLER
097730     MOVE STRT-AREA-POST-DT         TO BORV-47-TRAN-DATE
097740     MOVE STRT-AREA-TRN-NO          TO BORV-47-TRAN-CODE
097750     MOVE STRT-AREA-JRNL-NO         TO BORV-47-JRNL-NO
097760     MOVE STRT-AREA-POST-DT         TO BORV-47-POST-DATE
097770     MOVE BR0001-IN-17046-OLD-ACCT-TYPE
097780                                    TO BORV-47-OLD-ACCT-TYPE
097790     MOVE BR0001-IN-17046-NEW-ACCT-TYPE
097800                                    TO BORV-47-NEW-ACCT-TYPE
097810     MOVE BR0001-IN-17046-OLD-ACCT-CAT
097820                                    TO BORV-47-OLD-ACCT-CAT
097830     MOVE BR0001-IN-17046-NEW-ACCT-CAT
097840                                    TO BORV-47-NEW-ACCT-CAT
097850     MOVE BR0001-IN-17046-NEW-BRANCH
097860                                    TO BORV-47-NEW-BRANCH
097870     MOVE BR0001-IN-17046-OLD-BRANCH
097880                                    TO BORV-47-OLD-BRANCH
S11476     MOVE ZEROES                    TO BORV-47-LAST-FIN-DATE.
097940     PERFORM X200-ADD-NEW-TXN THRU X299-EXIT
097950     IF WA-MSG-ERR-FLAG = 1
097960        GO TO F839-EXIT.
097970*
097980* CALCULATING RATES NEEDS THE RIGHT BORPARM IF AN ACCT TYPE
097990* CHANGE HAS OCCURED
098000*
098010     IF BR0001-IN-17046-NEW-BRANCH = BR0001-IN-17046-OLD-BRANCH
098020        MOVE WA-SAVE-BORPARM-AREA TO LS01-BORR-PARAMS
LON327*CHKING        DISPLAY "RST123: " BORP-BASM-RATE-ID
098030     END-IF.
098040     PERFORM N700-ACCR-INT THRU N709-EXIT.
S11476
S13203     IF BR0001-IN-17046-NEW-BRANCH NOT =
S13203                                   BR0001-IN-17046-OLD-BRANCH
S13203        CONTINUE
S13203     ELSE
S11476        MOVE WA-UT2000-CALL-REC      TO LS01-BORR-PARAMS
LON236     END-IF.
LON236
099360     IF BR0001-IN-17046-NEW-BRANCH = BR0001-IN-17046-OLD-BRANCH
099370        MOVE WA-UT2000-CALL-REC TO LS01-BORR-PARAMS
LON327*CHKING        DISPLAY "RST234: " BORP-BASM-RATE-ID
S13888        PERFORM N710-GET-INT-RATE THRU N719-EXIT
SC9679        IF BORM-STORE-RATE < BORP-LOW-INT
SC9679           MOVE 1 TO WA-MSG-ERR-FLAG
SC9679           MOVE 333 TO BR0001-OUTPUT-ERR-MSG-NO
SC9679           GO TO F839-EXIT
SC9679        ELSE
SC9679           IF BORM-STORE-RATE > BORP-HIGH-INT
SC9679              MOVE 1 TO WA-MSG-ERR-FLAG
SC9679              MOVE 328 TO BR0001-OUTPUT-ERR-MSG-NO
SC9679              GO TO F839-EXIT
SC9679           END-IF
SC9679        END-IF
099500     END-IF.
099510*
099520* WRITE AUDIT RECORD(S)
099530*
099540     MOVE BORM-RECORD-AREA TO WA-AUDT-NEW-IMAGE
099550     MOVE STRT-AREA-CNTL-INFO TO WA-AUDT-CNTL-INFO
099560     MOVE "B"                 TO WA-AUDT-SYS
099570     MOVE STRT-AREA-BRANCH    TO WA-AUDT-HOME-BR
099580     MOVE "BORM"              TO WA-AUDT-DATASET
099590     CALL "SY70SEND" USING

099610                           STARTAREA
099620                           WA-AUDT-AREA

099630     IF BR0001-IN-17046-NEW-BRANCH NOT =
099640                           BR0001-IN-17046-OLD-BRANCH
099650        MOVE BR0001-IN-17046-NEW-BRANCH TO WA-AUDT-HOME-BR
099660        CALL "SY70SEND" USING

099680                           STARTAREA
099690                           WA-AUDT-AREA

099700     END-IF

099720*
099730     MOVE SPACES TO BR0001-OUTPUT-MSG.
099740     MOVE ZERO   TO BR0001-OUTPUT-ERR-MSG-NO.
099750     PERFORM Z000-FORMT-SEND-PARA THRU Z099-EXIT.
099760*
099770 F839-EXIT.
099780     EXIT.
111550/
111560 F900-SHORT-LONG-NAME.
LON236*
111570     INITIALIZE WA-ACSN-KEY.
111580     MOVE 71               TO DBIO-CALL-FUNC.
111590     MOVE "ACSN"           TO DBIO-CALL-SYS-AREA.
111600     MOVE STRT-AREA-SOC-NO TO WA-ACSN-SOC-NO.
111610     MOVE "BOR"            TO WA-ACSN-SYS-ID.
111620     MOVE BR0001-INPUT-KEY TO WA-ACSN-ACCT-NO.
111630     MOVE WA-ACSN-KEY      TO DBIO-CALL-KEY-AREA.
111640     PERFORM B500-DBIO-ACSN THRU B509-EXIT.
111650     IF DBIO-CALL-STATUS  = "****"
111660        MOVE ACSN-SHORT-ACCOUNT-NAME TO BR0001-OUT-SHORT-NAME
111670     ELSE
111680     IF  DBIO-CALL-STATUS = "MRNF"
111690         MOVE SPACES       TO  BR0001-OUT-SHORT-NAME
111700     ELSE
111710         MOVE 1             TO WA-MSG-ERR-FLAG
111720         MOVE 155           TO BR0001-OUTPUT-ERR-MSG-NO
111730         MOVE "ACSN"        TO BR0001-OUTPUT-DATA-SET
111740         MOVE DBIO-CALL-STATUS TO BR0001-OUTPUT-STATUS
111750         MOVE UT8500CA-FUNC-NORMAL-DISPLAY TO
111760              UT8500CA-CALL-CODE
111770         MOVE SPACES TO UT8500CA-INPUT-MSG
111780         STRING "BR0001:(F900) 'ACSN' READ ERROR, STAT- "
111790                 DBIO-CALL-STATUS ", KEY- " DBIO-CALL-KEY
111800             DELIMITED BY SIZE INTO UT8500CA-INPUT-MSG
111810         PERFORM UT8500-CALL THRU UT8500-EXIT.

111830     MOVE BR0001-INPUT-KEY TO UT0151CA-ACCT-IN.
111840     MOVE SOC-ACCT-NO-FORMAT TO UT0151CA-FORMAT.
111850     PERFORM S100-REFORMAT-ACCOUNT-NO THRU S199-EXIT.
111860     MOVE UT0151CA-ACCT-EX  TO BR0001-OUT-LSN-ACCT-NO.

111880     IF  WA-MSG-ERR-FLAG = 1
111890         PERFORM Z000-FORMT-SEND-PARA THRU Z099-EXIT
111900         MOVE 01                 TO ST-STATUS-LEVEL-1
111910         GO TO F909-EXIT.

111930     INITIALIZE WA-ACSN-KEY.
111940     MOVE 71               TO DBIO-CALL-FUNC.
111950     MOVE "ACLN"           TO DBIO-CALL-SYS-AREA.
111960     MOVE STRT-AREA-SOC-NO TO WA-ACSN-SOC-NO.
111970     MOVE "BOR"            TO WA-ACSN-SYS-ID.
111980     MOVE BR0001-INPUT-KEY TO WA-ACSN-ACCT-NO.
111990     MOVE WA-ACSN-KEY      TO DBIO-CALL-KEY-AREA.
112000     PERFORM B510-DBIO-ACLN THRU B519-EXIT.
112010     IF  DBIO-CALL-STATUS  = "****"
112020        MOVE ACLN-LONG-NAME TO BR0001-OUT-LONG-NAME
112030     ELSE
112040     IF  DBIO-CALL-STATUS = "MRNF"
112050         MOVE SPACES        TO BR0001-OUT-LONG-NAME
112060     ELSE
112070         MOVE 1             TO WA-MSG-ERR-FLAG
112080         MOVE 155           TO BR0001-OUTPUT-ERR-MSG-NO
112090         MOVE "ACLN"        TO BR0001-OUTPUT-DATA-SET
112100         MOVE DBIO-CALL-STATUS TO BR0001-OUTPUT-STATUS
112110         MOVE UT8500CA-FUNC-NORMAL-DISPLAY TO
112120              UT8500CA-CALL-CODE
112130         MOVE SPACES TO UT8500CA-INPUT-MSG
112140         STRING "BR0001:(F900) 'ACLN' READ ERROR, STAT- "
112150                 DBIO-CALL-STATUS ", KEY- " DBIO-CALL-KEY
112160             DELIMITED BY SIZE INTO UT8500CA-INPUT-MSG
112170         PERFORM UT8500-CALL THRU UT8500-EXIT.

112190     IF  WA-MSG-ERR-FLAG = 1
112200         PERFORM Z000-FORMT-SEND-PARA THRU Z099-EXIT
112210         MOVE 01                 TO ST-STATUS-LEVEL-1
112220         GO TO F909-EXIT.

112240     MOVE 17465                       TO STRT-AREA-TRN-NO.
112250     ADD  6                           TO STRT-AREA-IEXEC-CNTL.
112260     MOVE 998                         TO BR0001-OUTPUT-ERR-MSG-NO.
112270     MOVE 559                         TO BR0001-OUTPUT-MSG-LENGTH.
112280     PERFORM Z000-FORMT-SEND-PARA THRU Z099-EXIT.
LON236*
112290 F909-EXIT.
112300     EXIT.
TB0105/
S13708 F940-SETUP-17475.
LON236*
S13708     PERFORM F930-LOAN-RENEWAL THRU F930-EXIT.
LON327*LONTST     DISPLAY "BR0001:WA-MSG-ERR-FLAG:" WA-MSG-ERR-FLAG
S13708     IF WA-MSG-ERR-FLAG = 0
LON327*LONTST        DISPLAY "BR0001:INP-17474-ACTION:" INP-17474-ACTION
S13708        IF INP-17474-ACTION = "E"
S13708           MOVE 17476  TO STRT-AREA-TRN-NO
S13708        ELSE
S13708           MOVE 17475  TO STRT-AREA-TRN-NO
S13708        END-IF
S13708        MOVE 6      TO STRT-AREA-IEXEC-CNTL
S13708        MOVE 270    TO BR0001-OUTPUT-MSG-LENGTH
S13708        MOVE 998    TO BR0001-OUTPUT-ERR-MSG-NO
S13708        PERFORM Z000-FORMT-SEND-PARA THRU Z099-EXIT
S13708     END-IF.
LON236*
S13708 F940-EXIT.
S13708     EXIT.
TB0105
TB0105 F920-CALL-RENWL-MOD.
LON236*
TB0105     CALL "BR0032" USING
TB0105                          BORM-RECORD-AREA
TB0105                          BOIS-RECORD-AREA
TB0105                          LS01-BORR-PARAMS
TB0105*                         CUSM-RECORD
S11778                          BR0032CA-CALL-PARAM.
TB0105     IF ST-STATUS-LEVEL-1 NOT = 0
TB0105        MOVE 1 TO WA-MSG-ERR-FLAG
112710        MOVE SPACE TO O17047-JRNL-STAT
TB0105     END-IF.
TB0105
TB0105 F920-EXIT.
TB0105     EXIT.
LON236/
S13708 F930-LOAN-RENEWAL.
LON327*LONTST     DISPLAY "BR0001:DISPLAY F930".
S13708     INITIALIZE BR0001-OUTPUT-MSG.
S13708     IF  BORM-STAT EQUAL 10 OR
S13708         BORM-STAT EQUAL 20 OR
S13708         BORM-STAT EQUAL 40
S13708         MOVE 1513          TO BR0001-OUTPUT-ERR-MSG-NO
S13708         MOVE 1             TO WA-MSG-ERR-FLAG
S13708                               ST-STATUS-LEVEL-1
S13708         GO TO F930-EXIT
S13708     END-IF.
S13708     IF ((INP-17475-OPTION = "A" AND
S13708          INP-17475-EXT-REQ-STAT = "2") OR
S13708          INP-17475-OPTION = "D") AND
S13708        ((INP-17475-NEW-ACCT-TYPE NOT = INP-17475-ACCT-TYPE) OR
S13708         (INP-17475-NEW-IN-CAT NOT = INP-17475-ORIG-IN-CAT)) AND
S13708        (INP-17475-NEW-ACCT-TYPE NOT = ZEROES AND
S13708         INP-17475-NEW-IN-CAT    NOT = ZEROES)
LON327*LONTST        DISPLAY "BR0001:CHANGE PRODUCT HERE"
S13708        MOVE STRT-AREA-TRN-NO        TO WA-SAVE-ST-AREA-TRN-NO
S13708        MOVE STRT-AREA-DATA          TO WA-SAVE-ST-AREA-DATA
S13708        MOVE BR0064-IN-17475         TO WA-SAVE-INPUT-MSG
S13708        IF INP-17475-OPTION = "A"
LON327*LONTST           DISPLAY "BR0001: A OPTION FOR APPROVAL"
S13708           MOVE INP-17475-ACCT-TYPE     TO WA-OLD-ACCT-TYPE
S13708           MOVE INP-17475-ORIG-IN-CAT   TO WA-OLD-INT-CAT
S13708           MOVE INP-17475-NEW-ACCT-TYPE TO WA-NEW-ACCT-TYPE
S13708           MOVE INP-17475-NEW-IN-CAT    TO WA-NEW-INT-CAT
S13708        ELSE
LON327*LONTST           DISPLAY "BR0001: D OPTION FOR DELETE"
S13708           MOVE INP-17475-NEW-ACCT-TYPE TO WA-OLD-ACCT-TYPE
S13708           MOVE INP-17475-NEW-IN-CAT    TO WA-OLD-INT-CAT
S13708           MOVE INP-17475-ACCT-TYPE     TO WA-NEW-ACCT-TYPE
S13708           MOVE INP-17475-ORIG-IN-CAT   TO WA-NEW-INT-CAT
S13708        END-IF
S13708        INITIALIZE BR0001-IN-17046
S13708        MOVE 17046               TO STRT-AREA-TRN-NO
S13708        MOVE STRT-AREA-TRN-NO      TO LINK-IN-TXCODE
S13708        MOVE STRT-AREA-SOC-NO      TO LINK-IN-SOC-NO
S13708        MOVE 02                    TO LINK-IN-CALL-CODE
S13708        CALL "UT0250" USING UT0250-CALL-AREA
S13708        IF  LINK-OUT-STAT NOT = "****"
S13708            MOVE 100 TO BR0001-OUTPUT-ERR-MSG-NO
S13708            MOVE 1   TO WA-MSG-ERR-FLAG
S13708                        ST-STATUS-LEVEL-1
S13708            MOVE 0   TO LINK-IN-TXCODE
S13708            GO       TO F930-EXIT
S13708        ELSE
S13708           MOVE LINK-POSTING-LEVEL TO STRT-AREA-POSTING-LEVEL
S13708           MOVE LINK-EDIT-MASK     TO STRT-AREA-PROCESS-BITS
S13708           MOVE LINK-SIGN          TO STRT-AREA-SIGN
S13708        END-IF
S13708        MOVE BORM-BR-NO           TO BR0001-IN-17046-OLD-BRANCH
S13708                                     BR0001-IN-17046-NEW-BRANCH
S13708        MOVE BORM-CURRENCY-IND    TO BR0001-IN-17046-CURRENCY
S13708        MOVE WA-OLD-ACCT-TYPE     TO BR0001-IN-17046-OLD-ACCT-TYPE
S13708        MOVE SPACES               TO BR0001-IN-17046-DESC
S13708        MOVE WA-OLD-INT-CAT       TO BR0001-IN-17046-OLD-ACCT-CAT
S13708        MOVE WA-NEW-ACCT-TYPE     TO BR0001-IN-17046-NEW-ACCT-TYPE
S13708        MOVE WA-NEW-INT-CAT       TO BR0001-IN-17046-NEW-ACCT-CAT
S13708        MOVE BOIS-STRETCH-TERM    TO BR0001-IN-17046-STRETCH-TERM
S13708        MOVE BORM-STORE-RATE      TO BR0001-IN-17046-RATE
S13708        MOVE BORM-TOM-RATE-TBL-ID TO BR0001-IN-17046-TOM-ID
S13708        MOVE 0                    TO BR0001-IN-17046-IND
LON327*LONTST        DISPLAY "PERFORM CHANGE PRODUCT IN BR0001"
S13708        PERFORM F830-UPDATE-17046 THRU F839-EXIT
LON327*LONTST        DISPLAY "BR0001:AFTER UPDATE:" WA-MSG-ERR-FLAG
S13708        IF WA-MSG-ERR-FLAG NOT = 0
S13708           GO TO F930-EXIT
S13708        END-IF
S13708        MOVE WA-SAVE-ST-AREA-TRN-NO    TO STRT-AREA-TRN-NO
S13708        MOVE WA-SAVE-ST-AREA-DATA      TO STRT-AREA-DATA
S13708        MOVE WA-SAVE-INPUT-MSG         TO BR0064-IN-17475
S13708     END-IF.
STDMSG     DISPLAY "CALL BR0064".
LON327*LONTST   DISPLAY "BR0064: BORM-MEMB-CUST-AC:" BORM-MEMB-CUST-AC.
S13708     CALL "BR0064" USING  STARTAREA
S13708                          BOIS-RECORD-AREA
S13708                          LS01-BORR-PARAMS
S13708                          CUSM-RECORD.
LON327*LONTST     DISPLAY "BR0001:ST-STATUS-LEVEL-1:" ST-STATUS-LEVEL-1.
S13708     IF ST-STATUS-LEVEL-1 NOT = 0
LON327*LONTST        DISPLAY "BR0001: GET INTO ERROR"
S13708        MOVE 1 TO WA-MSG-ERR-FLAG
S13708        MOVE SPACE TO O17047-JRNL-STAT
S13708     END-IF.
S13708 F930-EXIT.
S13708      EXIT.
TB0105/
TB0105 F950-SETUP-PENDING-RENWL.
LON236*
TESTPO     IF STRT-AREA-TRN-NO = 17050
TB0105        MOVE 17099  TO STRT-AREA-TRN-NO
TESTPO     END-IF.
TB0105     PERFORM F920-CALL-RENWL-MOD THRU F920-EXIT.
TB0105     IF WA-MSG-ERR-FLAG = 0
TB0105        MOVE 6      TO STRT-AREA-IEXEC-CNTL
S11480        MOVE 281    TO BR0001-OUTPUT-MSG-LENGTH
TB0105        MOVE 998    TO BR0001-OUTPUT-ERR-MSG-NO
TB0105        PERFORM Z000-FORMT-SEND-PARA THRU Z099-EXIT
TB0105     END-IF.
LON236*
TB0105 F950-EXIT.
TB0105     EXIT.
TB0023/
LON236*-----------------------------------------------------------
LON236* BR0032 WILL CALL 17035 TO DO IT.NO NEED HERE
LON236*-----------------------------------------------------------
LON236*TB0105 F960-LOAN-EXTENSION.
LON236*-----------------------------------------------------------
LON236* POPULATE THE 17035 EXPAND THE LENGTH 16 TO 17
LON236*-----------------------------------------------------------
LON236*    MOVE BR0001-INPUT-KEY-R TO UT0151CA-ACCT-IN.
LON236*    MOVE SOC-ACCT-NO-FORMAT TO UT0151CA-FORMAT.
LON236*    PERFORM S100-REFORMAT-ACCOUNT-NO THRU S199-EXIT.
LON236*    MOVE UT0151CA-ACCT-EX   TO BR0001-OUT-17035-INPUT-KEY.
LON236*    MOVE BORM-CUSTOMER-NO   TO UT0151CA-ACCT-IN.
LON236*    MOVE SOC-ACCT-NO-FORMAT TO UT0151CA-FORMAT.
LON236*    PERFORM S100-REFORMAT-ACCOUNT-NO THRU S199-EXIT.
LON236*    MOVE UT0151CA-ACCT-EX   TO BR0001-OUT-17035-CUST-NO.
LON236*FOR ID AND ID TYPE
LON236*    MOVE UT0500RA-ID-NO     TO  BR0001-OUT-17035-INP-ID.
LON236*    MOVE UT0500RA-FULL-NAME TO  BR0001-OUT-17035-NAME-1.
LON236*
113020*    PERFORM F920-CALL-RENWL-MOD THRU F920-EXIT.
TB0105*    IF WA-MSG-ERR-FLAG = 0
113040*       MOVE 17035 TO STRT-AREA-TRN-NO
TB0105*       MOVE 6      TO STRT-AREA-IEXEC-CNTL
TB0105*       MOVE 173    TO BR0001-OUTPUT-MSG-LENGTH
TB0105*       MOVE 998    TO BR0001-OUTPUT-ERR-MSG-NO
TB0105*       PERFORM Z000-FORMT-SEND-PARA THRU Z099-EXIT
TB0105*    END-IF.
113100*F960-EXIT.
TB0105*    EXIT.
S11475/
SC4083 G500-GET-PRIORITY-BUCKETS.
SC4083     MOVE "BOR"               TO UTHISTCA-SYSTEM
SC4083     MOVE BORM-SOC-NO         TO UTHISTCA-SOC-NO
SC4083     MOVE BORM-ACT-TYPE       TO UTHISTCA-ACCT-TYPE
SC4083     MOVE BORM-CAT            TO UTHISTCA-INT-CAT
SC4083     MOVE WA-BUCKET-CODE      TO UTHISTCA-CALL-PRIO-CD
SC4083     MOVE WA-BUCKET-NO        TO UTHISTCA-CALL-PRIO-NO
SC4083     MOVE STRT-AREA-POST-DT   TO UTHISTCA-EFFECTIVE-DATE
SC4083     MOVE 02                  TO UTHISTCA-CALL-CODE.
SC4083     CALL "UTHIST"
SC4083           USING UTHISTCA-CALL-PARAM.
SC4083     IF UTHISTRA-RET-STATUS = WA-ZERO
SC4083         MOVE UTHISTRA-RET-PRIO-CD TO WA-BUCKET-CODE
SC4083         MOVE UTHISTRA-RET-PRIO-NO TO WA-BUCKET-NO
SC4083     ELSE
SC4083         MOVE 1 TO WA-MSG-ERR-FLAG
SC4083         MOVE UTHISTRA-ERR-MSG-NO TO BR0001-OUTPUT-ERR-MSG-NO
SC4083         SET UT8500CA-NORMAL-DISPLAY TO TRUE
SC4083         MOVE SPACES TO UT8500CA-INPUT-MSG
SC4083         STRING "BR0001:(G500) ERROR CALLING BR0001"
SC4083                ", STATUS=" UT0073CA-RET-STATUS
SC4083             DELIMITED BY SIZE INTO UT8500CA-INPUT-MSG
SC4083         CALL "UT8500" USING
SC4083                       UT8500CA-AREA.
SC4083 G599-EXIT.
SC4083     EXIT.
144780/
157650 L000-BAD-DEBT-ENQ.
TB0092*
TB0092        MOVE 0 TO WA-MSG-ERR-FLAG.
TB0092        PERFORM MANUAL-BAD-DEBT-ENQ
TB0092           THRU MANUAL-BAD-DEBT-ENQ-EXIT.
TB0161        IF BR0001-BAD-DEBT-IND = 52
TB0156           INITIALIZE UT0073CA-CALL-PARAMETER
TB0156           MOVE 01 TO UT0073CA-CALL-CODE
TB0156           MOVE WA-TODAY-DATE   TO UT0073CA-EFF-DATE
TB0156           CALL "UT0073" USING UT0073CA-CALL-AREA
TB0161                         BORM-RECORD-AREA
TB0156                         BOIS-RECORD-AREA
TB0156                         BORR-PARAMS
TB0156                         SOC-PARAMS
S12311                         BORV-RECORD-AREA
S12311                         CUSV-DDRECORD-AREA
S12311                         STARTAREA
LON527           IF UT0073CA-RET-STATUS NOT = 0
LON527              MOVE 1 TO WA-MSG-ERR-FLAG
LON527              MOVE UT0073CA-RET-ERROR-MSG TO
LON527                                  BR0001-OUTPUT-ERR-MSG-NO
LON527              GO TO L099-EXIT
LON527           END-IF
TB0161        END-IF.
158150 L099-EXIT.
158160     EXIT.
LON236*
158170 M000-BAD-DEBT-AMEND.
ST0102     MOVE BORP-CURRENCY-IND       TO UT0290CA-CALL-CURRENCY.
ST0102     MOVE 0                       TO UT0290CA-CALL-STATUS
ST0102                                     UT0290CA-CALL-MSG-NO.
ST0102     MOVE 2                       TO UT0290CA-CALL-CODE.
ST0102     CALL "UT0290"             USING LS01-UT0290CA-CALL-AREA.
ST0102     IF UT0290CA-CALL-STATUS NOT = 0
ST0102        MOVE HIGH-VALUES          TO UT0290CA-CALL-CURRENCY
ST0102        MOVE 1                    TO WA-MSG-ERR-FLAG
ST0102        MOVE UT0290CA-CALL-MSG-NO TO BR0001-OUTPUT-ERR-MSG-NO
ST0102        GO TO M099-EXIT
ST0102     END-IF.
LON120     IF BR0001-BAD-DEBT-JRNL-NO NOT = 0
LON120        CONTINUE
LON120     ELSE
LON120        PERFORM VARYING WA-BAD-DBT-IDX FROM 1 BY 1
LON120        UNTIL WA-BAD-DBT-IDX > 5
LON120           MOVE SPACES TO BOIS-GL-BUCKET-CDE(WA-BAD-DBT-IDX)
LON120           MOVE ZEROES TO BOIS-GL-BUCKET-DUE(WA-BAD-DBT-IDX)
LON120        END-PERFORM
LON120        PERFORM VARYING WA-CTR FROM 1 BY 1
LON120        UNTIL WA-CTR  > 15
LON120            MOVE SPACES TO BOIS-BUCKET-CDE(WA-CTR)
LON120            MOVE ZEROES TO BOIS-BUCKET-DUE(WA-CTR)
LON120        END-PERFORM
LON120     END-IF
ST0102
SC6871     MOVE BR0001-BAD-DEBT-AMOUNT TO WA-VA-AMT.
SC6871     PERFORM V550-INPUT-AMT-DECIMAL-CHECK THRU V550-EXIT.
SC6871     IF WA-MSG-ERR-FLAG = 1
SC6871        GO TO M099-EXIT.
LON120     IF BR0001-BAD-DEBT-JRNL-NO NOT = 0
LON120        IF BR0001-BAD-DEBT-DATE NOT = ZEROES
LON120           MOVE BR0001-BAD-DEBT-DATE-DD TO WA-BD-DD
LON120           MOVE BR0001-BAD-DEBT-DATE-MM TO WA-BD-MM
LON120           MOVE BR0001-BAD-DEBT-DATE-CC TO WA-BD-CC
LON120           MOVE BR0001-BAD-DEBT-DATE-YY TO WA-BD-YY
LON120        ELSE
LON120           MOVE 1     TO UT1999CA-CALL-CODE
LON120           CALL "UT1999" USING
LON120                         UT1999CA-CALL-AREA
LON120           MOVE UT1999CA-CCYYMMDD  TO WA-TODAYS-DATE
LON120           MOVE UT1999CA-DD        TO BR0001-BAD-DEBT-DATE-DD
LON120           MOVE UT1999CA-MM        TO BR0001-BAD-DEBT-DATE-MM
LON120           MOVE UT1999CA-CC        TO BR0001-BAD-DEBT-DATE-CC
LON120           MOVE UT1999CA-YY        TO BR0001-BAD-DEBT-DATE-YY
LON120**LON120           MOVE WA-TODAYS-DATE     TO BR0001-BAD-DEBT-DATE
LON327*LON120           DISPLAY "BAD DEBT DATE = " BR0001-BAD-DEBT-DATE
LON120           IF BR0001-BAD-DEBT-DATE NOT = ZEROES
LON120              MOVE BR0001-BAD-DEBT-DATE-DD TO WA-BD-DD
LON120              MOVE BR0001-BAD-DEBT-DATE-MM TO WA-BD-MM
LON120              MOVE BR0001-BAD-DEBT-DATE-CC TO WA-BD-CC
LON120              MOVE BR0001-BAD-DEBT-DATE-YY TO WA-BD-YY
LON120           END-IF
LON120        END-IF
LON327*LON120        DISPLAY "BR0001-BAD DATE = " BR0001-BAD-DEBT-DATE
LON120        MOVE 1                   TO UT0030-FUNCTION
LON120        MOVE BR0001-BAD-DEBT-DATE
LON120                             TO UT0030-DATE-DDMMCCYY
LON120        PERFORM Y020-CALL-UT0030 THRU Y029-EXIT
LON120        MOVE UT0030-BINARY-DATE  TO WA-BAD-DEBT-DT
LON120        MOVE WA-BAD-DEBT-DT    TO  WA-TEMP-DT
LON327*LON120        DISPLAY "BAD DEBT DATE1 = " WA-TEMP-DT
LON120        INITIALIZE XXCT-ALT-KEY2
LON120        INITIALIZE XXCT-DATAREC
LON120        INITIALIZE WA-SAVE-BOCT-REC
LON120        MOVE ZEROES TO WA-REC-NO
LON120        IF BR0001-BAD-DEBT-IND NOT = BORM-BAD-DEBT-IND AND
LON120           BR0001-BAD-DEBT-JRNL-NO NOT = WA-ZERO
LON120           MOVE 1    TO WA-MSG-ERR-FLAG
LON120           MOVE 3886 TO BR0001-OUTPUT-ERR-MSG-NO
LON120           GO TO M099-EXIT
LON120        END-IF
LON120        PERFORM FIND-REC-NO THRU FIND-REC-EXIT
LON120        IF WA-MSG-ERR-FLAG = 1
LON120           GO TO M099-EXIT
LON120        END-IF
LON120        IF XXCT-STAT = 20
LON120           MOVE 1    TO WA-MSG-ERR-FLAG
LON120           MOVE 598  TO BR0001-OUTPUT-ERR-MSG-NO
LON120           GO TO M099-EXIT
LON120        END-IF
LON120        IF WA-REC-NO = 0
LON120           MOVE 1    TO WA-MSG-ERR-FLAG
LON120           MOVE 132  TO BR0001-OUTPUT-ERR-MSG-NO
LON120           GO TO M099-EXIT
LON120        END-IF
LON120        IF XXCT-TRN-CODE = 17103
LON120           MOVE 1    TO WA-MSG-ERR-FLAG
LON120           MOVE 132  TO BR0001-OUTPUT-ERR-MSG-NO
LON120           GO TO M099-EXIT
LON120        END-IF
LON327*LON120        DISPLAY "CORR RECORD NO = " WA-REC-NO
LON120        MOVE XXCT-DATAREC TO WA-SAVE-BOCT-REC
LON120        PERFORM CHECK-CORRECTION-TXN  THRU
LON120                CHECK-CORRECTION-EXIT
LON120        IF WA-MSG-ERR-FLAG = 1
LON120           GO TO M099-EXIT
LON120        END-IF
LON120        IF WA-LATEST-TXN = "N"
LON120           MOVE 4764   TO BR0001-OUTPUT-ERR-MSG-NO
LON120           MOVE 1      TO WA-MSG-ERR-FLAG
LON120           GO TO M099-EXIT
LON120        ELSE
LON327*LON120           DISPLAY "NOT N"
LON120           MOVE "Y" TO WA-LATEST-TXN
LON120           MOVE "Y"    TO WA-FLAG-CORR
LON120        END-IF
LON120        MOVE WA-SAVE-BOCT-REC TO XXCT-DATAREC
LON120        ADD 20 TO XXCT-STAT
LON120        PERFORM REWRITE-BOCT THRU REWRITE-BOCT-EXIT
LON120        IF WA-MSG-ERR-FLAG = 1
LON120           GO TO M099-EXIT
LON120        END-IF
LON120     END-IF.
S12749     PERFORM M020-WRITE-OFF-PROCESS THRU M029-EXIT.
S12749     IF WA-MSG-ERR-FLAG = 1
S12749        GO TO M099-EXIT.
S12749
S12223     IF SOC-SITE-GCR
SC3007        MOVE STRT-AREA-POST-DT           TO WA-TODAY-DATE
SC2386        MOVE BOIS-DEF-BUCKET-AMT         TO WA-DEF-BUCKET-AMT
SC1121        MOVE BORM-INT-ACCR               TO WA-BORM-INT-ACCR
SC2275        ADD  BORM-INT-ADJUSTMENT         TO WA-BORM-INT-ACCR
SC1121        MOVE BR0001-BAD-DEBT-REASON-CODE TO WA-REASON-CODE
TB0092        MOVE BR0001-BAD-DEBT-ACCT-NO TO WA-ACCT-NO-16
TB0092        MOVE 0 TO WA-MSG-ERR-FLAG
TB0092        PERFORM MANUAL-BAD-DEBT-AMEND
TB0092           THRU MANUAL-BAD-DEBT-AMEND-EXIT
SC3713        IF WA-MSG-ERR-FLAG = 1
SC3713           GO TO M099-EXIT
SC3713        END-IF
LON120        IF BR0001-BAD-DEBT-JRNL-NO > 0
LON120           CONTINUE
LON120        ELSE
LON120           MOVE "P" TO BOIS-GL-BUCKET-CDE(1)
LON120           IF BORM-LOAN-BAL > 0
LON120              MOVE BORM-LOAN-BAL TO BOIS-GL-BUCKET-DUE(1)
LON120           END-IF
LON120           MOVE "I" TO BOIS-GL-BUCKET-CDE(2)
LON120           IF BORM-CAP-UNPAID-INT > 0
LON120              MOVE BORM-CAP-UNPAID-INT TO BOIS-GL-BUCKET-DUE(2)
LON120           END-IF
LON120           MOVE "W" TO BOIS-GL-BUCKET-CDE(3)
LON120           IF BOIS-UNPAID-ARRS-PRIN-BAL > 0
LON120           MOVE BOIS-UNPAID-ARRS-PRIN-BAL TO BOIS-GL-BUCKET-DUE(3)
LON120           END-IF
LON120           MOVE "A" TO BOIS-GL-BUCKET-CDE(4)
LON120           IF BORM-UNPAID-ARRS-INT-BAL > 0
LON120           MOVE BORM-UNPAID-ARRS-INT-BAL  TO BOIS-GL-BUCKET-DUE(4)
LON120           END-IF
LON120           MOVE "E" TO BOIS-GL-BUCKET-CDE(5)
LON120           IF BOIS-UNPAID-INT-ARRS-PRIN-BAL > 0
LON120              MOVE BOIS-UNPAID-INT-ARRS-PRIN-BAL TO
LON120                              BOIS-GL-BUCKET-DUE(5)
LON120           END-IF
LON120        END-IF
TB0092        GO TO M099-EXIT
TB0092     ELSE
TB0258        IF BR0001-BAD-DEBT-IND > 9
158690            MOVE 1 TO WA-MSG-ERR-FLAG
158700            MOVE 759 TO BR0001-OUTPUT-ERR-MSG-NO
158710            GO TO M099-EXIT
TB0092        END-IF

158740        MOVE BORM-RECORD-AREA   TO WA-BORM-OLD-IMAGE
158750        MOVE WA-BORM-OLD-IMAGE  TO WA-AUDT-OLD-IMAGE
158760        IF BR0001-BAD-DEBT-IND  = BORM-BAD-DEBT-IND
158770           GO TO M090-SEND-OK-MSG
158780        ELSE
158800           IF BR0001-BAD-DEBT-IND = 03
158810              MOVE BORM-LOAN-BAL TO BORM-NON-PERF-BAL
158820           END-IF
158830           IF BORM-BAD-DEBT-IND = 03
158840              MOVE ZEROS         TO BORM-NON-PERF-BAL
158850           END-IF
158860*
TB0156           MOVE BR0001-BAD-DEBT-AMOUNT TO BOIS-BAD-DEBT-AMT
158880           MOVE BR0001-BAD-DEBT-IND TO BORM-BAD-DEBT-IND
158890           PERFORM W500-AMEND-BORM THRU W599-EXIT
158900           IF  WA-MSG-ERR-FLAG = 1
158910               GO TO M099-EXIT
158920           ELSE
158930               MOVE BORM-RECORD-AREA TO WA-AUDT-NEW-IMAGE
158950               PERFORM S000-WRITE-BORM-AUDIT-REC THRU S099-EXIT
TB0092           END-IF
TB0092        END-IF.
TB0092*
158990 M090-SEND-OK-MSG.
159000     MOVE 0 TO BR0001-OUTPUT-ERR-MSG-NO.
159010     PERFORM Z000-FORMT-SEND-PARA THRU Z099-EXIT.
159020 M099-EXIT.
159030     EXIT.
159040/
S12749 M020-WRITE-OFF-PROCESS.
S12749     IF BR0001-ARRS-REASON-CD > 0
S13681        OR BR0001-ARRS-REASON-CD NOT = SPACES
S12479          MOVE STRT-AREA-SOC-NO       TO  RSNC-INST-NO
S12479          MOVE BR0001-ARRS-REASON-CD  TO  RSNC-REASON-CODE
S12749/
S12749        PERFORM GET-REASON-CODE
S12749           THRU GET-REASON-CODE-EXIT
S12749/
S13681        IF WA-MSG-ERR-FLAG NOT = 0
S12749           MOVE 4274 TO BR0001-OUTPUT-ERR-MSG-NO
S12749           GO TO  M029-EXIT
S12749        END-IF
S12749        MOVE BR0001-ARRS-REASON-CD     TO  BOIS-ARRS-REASON-CD
S12749        MOVE "2"  TO  BOIS-ASSET-RATING
S12749     END-IF.
S12749/
S12749     IF STRT-AREA-POST-DT > BOIS-LAST-UNPAID-DUE-DATE
S12749        MOVE BOIS-LAST-UNPAID-DUE-DATE  TO  UT0020-BINARY-DATE
S12749        MOVE 1                          TO  UT0020-FUNCTION
S12749        CALL "UT0020"  USING  UT0020-FUNCTION
S12749                              UT0020-DATE-AREA
S12749                              UT0020-BINARY-DATE
S12749        IF UT0020-BINARY-DATE > 0
S12749           MOVE UT0020-DATE-DDMMCCYY TO WS-LAST-UNPAID-DUE-DATE
S12749        END-IF
S12749/
S12749        MOVE STRT-AREA-POST-DT  TO  UT0020-BINARY-DATE
S12749        MOVE 1                          TO  UT0020-FUNCTION
S12749        CALL "UT0020"  USING  UT0020-FUNCTION
S12749                              UT0020-DATE-AREA
S12749                              UT0020-BINARY-DATE
S12749        IF UT0020-BINARY-DATE > 0
S12749           MOVE UT0020-DATE-DDMMCCYY TO  WS-POSTING-DATE
S12749        END-IF
S12749/
S12749        COMPUTE WS-YEAR-DIFF =
S12749                    WS-POSTING-DATE-CY - WS-UNPAID-DUE-DATE-CY
S12749        IF WS-YEAR-DIFF > 0
S12749           COMPUTE WS-MTHS-IN-ARREARS = (WS-YEAR-DIFF * 12)
S12749        END-IF
S12749/
S12749        COMPUTE WS-MTHS-DIFF =
S12749                    WS-POSTING-DATE-MM - WS-UNPAID-DUE-DATE-MM
S12749        IF WS-POSTING-DATE-MM > WS-UNPAID-DUE-DATE-MM
S12749           ADD WS-MTHS-DIFF TO WS-MTHS-IN-ARREARS
S12749        END-IF
S12749/
S12749        IF WS-POSTING-DATE-MM < WS-UNPAID-DUE-DATE-MM
S12749           ADD WS-POSTING-DATE-MM         TO   WS-MTHS-IN-ARREARS
S12749           SUBTRACT WS-UNPAID-DUE-DATE-MM FROM WS-MTHS-IN-ARREARS
S12749        END-IF
S12749/
S12749        IF WS-POSTING-DATE-DD > WS-UNPAID-DUE-DATE-DD
S12749           ADD 1  TO  WS-MTHS-IN-ARREARS
S12749        END-IF
S12749/
S12749       IF BORP-SEC-FLG > 0
S12749          IF WS-MTHS-IN-ARREARS < 3
S12749             MOVE "2"  TO  BOIS-ASSET-RATING
S12749          ELSE
S12749            IF WS-MTHS-IN-ARREARS < 6
S12749               MOVE "3"  TO  BOIS-ASSET-RATING
S12749            ELSE
S12749              IF WS-MTHS-IN-ARREARS < 12
S12749                 MOVE "4"  TO BOIS-ASSET-RATING
S12749              ELSE
S12749                 IF WS-MTHS-IN-ARREARS > 11
S12749                    MOVE "5"  TO BOIS-ASSET-RATING
S12749                 END-IF
S12749              END-IF
S12749            END-IF
S12749          END-IF
S12749       ELSE
S12749          IF WS-MTHS-IN-ARREARS < 12
S12749             MOVE "2"  TO  BOIS-ASSET-RATING
S12749          ELSE
S12749             IF WS-MTHS-IN-ARREARS > 11
S12749                MOVE "3"  TO  BOIS-ASSET-RATING
S12749             END-IF
S12749          END-IF
S12749       END-IF
S12749     END-IF.
S12749/
S14340     IF BR0001-WRITE-OFF-APRV-DT > ZEROES
S12749     MOVE BR0001-WRITE-OFF-APRV-DT  TO  UT0030-DATE-DDMMCCYY
S12749     MOVE 1                         TO  UT0030-FUNCTION
S12749     CALL "UT0030"  USING  UT0030-FUNCTION
S12749                           UT0030-DATE-AREA
S12749                           UT0030-BINARY-DATE
S12749     IF UT0030-BINARY-DATE = ZEROES
S12749        MOVE 1          TO  WA-MSG-ERR-FLAG
S12749        MOVE 126        TO  BR0001-OUTPUT-ERR-MSG-NO
S12749        GO TO M029-EXIT
S12749     ELSE
S12749        MOVE UT0030-BINARY-DATE TO  BOIS-WRITE-OFF-APRV-DT
S14340     END-IF
S12749     END-IF.
S12749/
S12749     MOVE BR0001-WRITE-OFF-APRV-LEVEL-1
S12749                             TO BOIS-WRITE-OFF-APRV-LEVEL-1.
S12749     MOVE BR0001-WRITE-OFF-APRV-LEVEL-2
S12749                             TO BOIS-WRITE-OFF-APRV-LEVEL-2
S12749     MOVE BR0001-WRITE-OFF-APRV-LEVEL-3
S12749                             TO BOIS-WRITE-OFF-APRV-LEVEL-3.
S12749 M029-EXIT.
S12749     EXIT.
SC4757/
172770 N700-ACCR-INT.
172780*
172790     PERFORM N710-GET-INT-RATE THRU N719-EXIT

S11470     IF BOIS-RT-INCR-OPTION = "0" OR "1"
S11470        COMPUTE WA-INT-RATE-A = WA-INT-RATE-A +
S11470                                BORM-RT-INCR
S11470     ELSE
S11470        IF BOIS-RT-INCR-OPTION = "2"
S11470           COMPUTE WA-INT-RATE-A = WA-INT-RATE-A +
S11470            (WA-INT-RATE-A * BORM-RT-INCR) / 100
S11470        END-IF
S11470     END-IF
172920*
172930* COMPUTE ACCRUED INTEREST
172940*
172950     IF BORP-ACCRUE-FREQ = 99
172960         COMPUTE WA-ACCRUED-INT ROUNDED = BORM-INT-ACCR
172970                                        + BORM-BPI-ACCR
172980                                        + BORM-CI-ACCR
172990         MOVE BORM-TOM-ACRU-AMT  TO WA-TOM-ACRU-AMT
173000     ELSE
173010         PERFORM C200-CALC-MTHLY THRU      C209-EXIT
173020         IF UT0079CA-RET-STATUS NOT = 0
173030             MOVE 0              TO        WA-ACCRUED-INT
173040                                           WA-TOM-ACRU-AMT
173050         ELSE
173060             COMPUTE WA-ACCRUED-INT ROUNDED = UT0079CA-BPI
173070                                                  + UT0079CA-LI
173080             MOVE UT0079CA-TOM-ACRU TO WA-TOM-ACRU-AMT
173090         END-IF
173100     END-IF.
173210     IF  UT0290CA-CURR-DECS NOT = 3 AND WA-ACCRUED-INT > 0
173220         MOVE WA-ACCRUED-INT   TO   WA-WORK-AMOUNT-3
173230         PERFORM N900-ROUND-CURRENCY THRU N999-EXIT
173240         MOVE WA-WORK-AMOUNT-3 TO   WA-ACCRUED-INT
173250     END-IF.
173260     IF  UT0290CA-CURR-DECS NOT = 3 AND WA-TOM-ACRU-AMT > 0
173270         MOVE ZEROS            TO   WA-WORK-AMOUNT-3
173280         MOVE WA-TOM-ACRU-AMT  TO   WA-WORK-AMOUNT-3
173290         PERFORM N900-ROUND-CURRENCY THRU N999-EXIT
173300         MOVE WA-WORK-AMOUNT-3 TO   WA-TOM-ACRU-AMT
173310     END-IF.
173350     IF WA-TOM-ACRU-AMT     NOT = 0
173360        PERFORM N810-WRITE-TOM-TXNS THRU N819-EXIT
173370     END-IF.
173380     IF BR0001CA-FCC-PRNC-ADJ NOT = ZERO
173390        PERFORM N720-WRITE-PRNC-ADJ-TXN THRU N729-EXIT
173400     END-IF.
173410*
173420 N709-EXIT.
173430     EXIT.
173440/
173450 N710-GET-INT-RATE.
173460*
173470* FIND THE INTEREST RATE
173480*
S12223     IF SOC-SITE-GCR
173510        IF  BORM-INT-RATE = 0
173520        AND BOIS-NEGO-RATE-IND = "0"
173530            OR (BOIS-NEGO-RATE-IND = "2"
173540                AND BOIS-NI-FLOAT)
173550          MOVE 0                   TO WA-TIER-LEVEL
173560          MOVE BORP-TIER-METH      TO WA-TIER-METHOD
173570          MOVE STRT-AREA-POST-DT   TO WA-SEARCH-RATE-DATE
173580          IF BORP-BASM-BASE-ID = SPACES OR
173590             BORP-BASM-RATE-ID = SPACES
173600             MOVE 0  TO WA-INT-RATE-A
173610          ELSE
LON327*CHKING             DISPLAY "E290 HERE 1"
173620             PERFORM E290-SEARCH-RATE-TABLE THRU E299-EXIT
173630          END-IF
SC8314          MOVE BORM-STORE-RATE     TO WA-OLD-RATE
173650          MOVE WA-INT-RATE-A       TO BORM-STORE-RATE
S11470          IF BOIS-RT-INCR-OPTION = "0" OR "1"
S11470             COMPUTE BORM-STORE-RATE = BORM-STORE-RATE +
S11470                                       BORM-RT-INCR
S11470          ELSE
S11470             IF BOIS-RT-INCR-OPTION = "2"
S11470                COMPUTE BORM-STORE-RATE = BORM-STORE-RATE +
S11470                 (BORM-STORE-RATE * BORM-RT-INCR) / 100
S11470             END-IF
S11470          END-IF
SC8314** We should recaculate the new int incr based on the new int-rate
SC8314          IF BORM-LOAN-BAL > 0
SC8314          AND BORM-STORE-RATE > 0
SC8314          AND WA-OLD-RATE NOT = BORM-STORE-RATE
SC8314             INITIALIZE UT0078CA-CALL-PARAM
SC8314             MOVE 9                TO UT0078CA-CALL-CODE
SC8314             MOVE BORM-LOAN-BAL    TO UT0078CA-AMOUNT
SC8314             MOVE BORM-STORE-RATE  TO UT0078CA-INT-RATE
SC8314             MOVE BORP-INT-DAYS-YR TO UT0078CA-INT-DAYS-YR
SC8314             CALL "UT0078" USING
SC8314                           UT0078CA-CALL-PARAM
SC8314
S13681             IF UT0078CA-RET-STATUS NOT = WA-ZERO
SC8314                MOVE UT8500CA-FUNC-NORMAL-DISPLAY
SC8314                                   TO UT8500CA-CALL-CODE
SC8314                MOVE SPACES TO UT8500CA-INPUT-MSG
SC8314                STRING "BR0001(N710): DUES RECALC ERROR ..."
SC8314                       BORM-KEY " " UT0078CA-RET-STATUS
SC8314                       DELIMITED BY SIZE INTO UT8500CA-INPUT-MSG
SC8314                CALL "UT8500" USING
SC8314                              UT8500CA-AREA
SC8314                MOVE 0             TO BORM-INT-INCR
SC8314                MOVE 1             TO WA-MSG-ERR-FLAG
SC8314                MOVE 767           TO BR0001-OUTPUT-ERR-MSG-NO
SC8314                GO TO N719-EXIT
SC8314             ELSE
SC8314                MOVE UT0078CA-LI-INCR TO BORM-INT-INCR
SC8314             END-IF
SC8314          ELSE
SC8314             IF BORM-LOAN-BAL = 0
SC8314              OR BORM-STORE-RATE = 0
SC8314                MOVE 0 TO BORM-INT-INCR
SC8314             END-IF
SC8314          END-IF
SC8314
SC8314          IF BORM-STORE-RATE NOT = WA-OLD-RATE
SC8314             MOVE "Y"             TO WA-STORE-RATE-CH-IND
SC8314             MOVE BORM-RT-INCR    TO WA-RT-INCR
SC8314             MOVE STRT-AREA-POST-DT TO WA-12000-PROCESS-DATE
S11470             IF BOIS-RT-INCR-OPTION = "0" OR "1"
S11470                COMPUTE WA-INT-RATE-A = WA-INT-RATE-A +
S11470                                        BORM-RT-INCR
S11470             ELSE
S11470                IF BOIS-RT-INCR-OPTION = "2"
S11470                   COMPUTE WA-INT-RATE-A = WA-INT-RATE-A +
S11470                    (WA-INT-RATE-A * BORM-RT-INCR) / 100
S11470                END-IF
S11470             END-IF
SC8314             MOVE WA-OLD-RATE     TO BORM-STORE-RATE
SC8314             MOVE BORM-INT-ACCR   TO WA-INT-ACCR
LON236*
LON236* HOW TO CHANGE THE RATE HISTORY
LON236*SC8314             PERFORM ZC1000-PROCESS-RT-CH
LON236*SC8314                THRU ZC1999-EXIT
SC8314             MOVE WA-INT-RATE-A   TO BORM-STORE-RATE
S11470             IF BOIS-RT-INCR-OPTION = "0" OR "1"
S11470                COMPUTE WA-INT-RATE-A = WA-INT-RATE-A -
S11470                                        BORM-RT-INCR
S11470             ELSE
S11470                IF BOIS-RT-INCR-OPTION = "2"
S11470                   COMPUTE WA-INT-RATE-A = WA-INT-RATE-A -
S11470                    (WA-INT-RATE-A * BORM-RT-INCR) / 100
S11470                END-IF
S11470             END-IF
SC8314          END-IF
174450        ELSE
174460          MOVE STRT-AREA-POST-DT   TO WA-INT-DATE-A
174470          MOVE BORM-INT-RATE       TO WA-INT-RATE-A
174480        END-IF
TB0069     ELSE
174510     IF BORM-INT-RATE = 0
174520        MOVE 0                   TO WA-TIER-LEVEL
174530        MOVE BORP-TIER-METH      TO WA-TIER-METHOD
174540        MOVE STRT-AREA-POST-DT   TO WA-SEARCH-RATE-DATE
174550        IF BORP-BASM-BASE-ID = SPACES OR
174560           BORP-BASM-RATE-ID = SPACES
174570           MOVE 0  TO WA-INT-RATE-A
174590        ELSE
LON327*CHKING             DISPLAY "E290 HERE 2"
174600            PERFORM E290-SEARCH-RATE-TABLE THRU E299-EXIT
174610        END-IF
174620        PERFORM X750-SEARCH-BLDVWW THRU X759-EXIT
174630        MOVE WA-INT-RATE-A       TO BORM-STORE-RATE
S11470        IF BOIS-RT-INCR-OPTION = "0" OR "1"
S11470           COMPUTE BORM-STORE-RATE = BORM-STORE-RATE +
S11470                                     BORM-RT-INCR
S11470        ELSE
S11470           IF BOIS-RT-INCR-OPTION = "2"
S11470              COMPUTE BORM-STORE-RATE = BORM-STORE-RATE +
S11470               (BORM-STORE-RATE * BORM-RT-INCR) / 100
S11470           END-IF
S11470        END-IF
174740     ELSE
174750        MOVE STRT-AREA-POST-DT   TO WA-INT-DATE-A
174760        MOVE BORM-INT-RATE       TO WA-INT-RATE-A.
174770*
174780 N719-EXIT.
174790     EXIT.
174800/
174810 N720-WRITE-PRNC-ADJ-TXN.
174820*------------------------
174830     INITIALIZE BORV-01-DETAIL.
174840     MOVE ZEROS                  TO WA-TXN-AMOUNT.
174850     IF BR0001CA-FCC-PRNC-ADJ < ZERO
174860         MOVE STRT-AREA-SOC-NO   TO LINK-IN-SOC-NO
174870         MOVE 2                  TO LINK-IN-CALL-CODE
174880         MOVE 13513              TO BORV-01-TRN-CODE
174890                                    LINK-IN-TXCODE
174900         MOVE 7                  TO BORV-01-SUB-LEDG-CODE
174910         COMPUTE WA-TXN-AMOUNT ROUNDED = BR0001CA-FCC-PRNC-ADJ
174920         SUBTRACT WA-TXN-AMOUNT FROM BORM-LOAN-BAL
174930         CALL "UT0250" USING UT0250-CALL-AREA
174940         IF LINK-OUT-STAT NOT = "****"
LON615             MOVE 100 TO BR0001-OUTPUT-ERR-MSG-NO
LON615             MOVE 1   TO WA-MSG-ERR-FLAG
LON615             MOVE 0   TO LINK-IN-TXCODE
174950             MOVE UT8500CA-FUNC-NORMAL-DISPLAY
174960                                 TO UT8500CA-CALL-CODE
174970             MOVE SPACES         TO UT8500CA-INPUT-MSG
174980             STRING "BR0001:(N720) TRAN 13513 NOT FOUND"
174990                    ", INST: " LINK-IN-SOC-NO
175000                    ", ACCOUNT NO: " BORM-MEMB-CUST-AC
175010                 DELIMITED BY SIZE INTO UT8500CA-INPUT-MSG
175020             CALL "UT8500" USING UT8500CA-AREA
LON615             GO TO N729-EXIT
175030         ELSE
175040             IF LINK-PRIORITY-CODE = "I"
175050                 SUBTRACT WA-TXN-AMOUNT FROM BORM-CAP-UNPAID-INT
175060             END-IF
175070             IF LINK-PRIORITY-CODE = "A"
175080                 SUBTRACT WA-TXN-AMOUNT FROM
175090                                       BORM-UNPAID-ARRS-INT-BAL
175100             END-IF
175110             IF LINK-PRIORITY-CODE = "C"
175120                 SUBTRACT WA-TXN-AMOUNT FROM
175130                                       BORM-UNPAID-CHRG-BAL
175140             END-IF
175150             IF LINK-PRIORITY-CODE = "P"
175160                 SUBTRACT WA-TXN-AMOUNT FROM
175170                                       BORM-UNPAID-PRINCIPAL-BAL
175180             END-IF
175190             MOVE LINK-BOR-CODE  TO WA-LEDGER-CODE
175200             MOVE LINK-PRIORITY-CODE
175210                                 TO WA-PRIORITY-CODE
175220         END-IF
175230         MOVE STRT-AREA-POST-DT  TO BORV-01-TRN-DATE
175240                                    BORV-01-POST-DATE
175250         MOVE 1                  TO BORV-01-CODE
175260         MOVE WA-TXN-AMOUNT      TO BORV-01-AMOUNT
175270         MOVE BORM-LOAN-BAL      TO BORV-01-BALANCE
175280         SUBTRACT BORM-THEO-LOAN-BAL FROM
175290                                    BORM-LOAN-BAL
175300                             GIVING BORV-01-ARREARS
175310         MOVE STRT-AREA-BRANCH   TO UPTELLCA-BRCH
175320         MOVE STRT-AREA-TELL-NO  TO UPTELLCA-TELR
175330         CALL "UT0031" USING UPTELLCA-PARAM-1
175340                             UPTELLCA-PARAM-2
175350         MOVE UPTELLCA-TELLER-BRANCH
175360                                 TO BORV-01-TELL-AND-BR
175370         MOVE STRT-AREA-TERM-NO  TO BORV-01-BRTERM
175380         MOVE ZERO               TO BORV-01-STATUS
175390                                    BORV-01-CHQ-DAYS
175400         MOVE STRT-AREA-JRNL-NO  TO BORV-01-JRNL-NO
175410         MOVE STRT-AREA-MSG-TIME TO BORV-01-POST-TIME
175420         PERFORM X200-ADD-NEW-TXN THRU X299-EXIT
175430*
175440*** FCCD - FOREIGN CURRENCY DEBIT
175450*** BUT THIS FIELD SHOULD ALREADY BE SET UP, SO IT IS COMMENTED
175460*
175470         MOVE BORV-01-DETAIL     TO BR0001-OUTPUT-TRANS-AREA
175490         MOVE 001                TO BR0001-OUTPUT-ERR-MSG-NO
175500         MOVE 84                 TO BR0001-OUTPUT-MSG-LENGTH
175510         PERFORM Z000-FORMT-SEND-PARA THRU Z099-EXIT
175520
175530         MOVE 13513              TO BORV-01-TRN-CODE
175540                                    LINK-IN-TXCODE
175550         COMPUTE WA-TXN-AMOUNT ROUNDED = -1 *
175560                                         BR0001CA-FCC-PRNC-ADJ
175570         ADD WA-TXN-AMOUNT       TO BORM-LOAN-BAL
175580         CALL "UT0250" USING UT0250-CALL-AREA
175590         IF  LINK-OUT-STAT NOT = "****"
LON615             MOVE 100 TO BR0001-OUTPUT-ERR-MSG-NO
LON615             MOVE 1   TO WA-MSG-ERR-FLAG
LON615             MOVE 0   TO LINK-IN-TXCODE
175600             MOVE UT8500CA-FUNC-NORMAL-DISPLAY
175610                                 TO UT8500CA-CALL-CODE
175620             MOVE SPACES         TO UT8500CA-INPUT-MSG
175630             STRING "BR0001:(N720) TRAN 13513 NOT FOUND"
175640                    ", INST: " LINK-IN-SOC-NO
175650                    ", ACCOUNT NO: " BORM-MEMB-CUST-AC
175660                 DELIMITED BY SIZE INTO UT8500CA-INPUT-MSG
175670             CALL "UT8500" USING UT8500CA-AREA
LON615             GO TO N729-EXIT
175680         ELSE
175690             MOVE LINK-BOR-CODE  TO WA-LEDGER-CODE
175700             MOVE LINK-PRIORITY-CODE
175710                                 TO WA-PRIORITY-CODE
175720             IF LINK-PRIORITY-CODE = "I"
175730                 ADD WA-TXN-AMOUNT
175740                                 TO BORM-CAP-UNPAID-INT
175750             END-IF
175760             IF LINK-PRIORITY-CODE = "A"
175770                 ADD WA-TXN-AMOUNT
175780                                 TO BORM-UNPAID-ARRS-INT-BAL
175790             END-IF
175800             IF LINK-PRIORITY-CODE = "C"
175810                 ADD WA-TXN-AMOUNT
175820                                 TO BORM-UNPAID-CHRG-BAL
175830             END-IF
175840             IF LINK-PRIORITY-CODE = "P"
175850                 ADD WA-TXN-AMOUNT
175860                                 TO BORM-UNPAID-PRINCIPAL-BAL
175870             END-IF
175880         END-IF
175890         MULTIPLY WA-TXN-AMOUNT BY -1 GIVING BORV-01-AMOUNT
175900         MOVE BORM-LOAN-BAL      TO BORV-01-BALANCE
175910         SUBTRACT BORM-THEO-LOAN-BAL
175920                               FROM BORM-LOAN-BAL
175930                             GIVING BORV-01-ARREARS
175940         PERFORM X200-ADD-NEW-TXN THRU X299-EXIT
175950*
175960*** FCCC - FOREIGN CURRENCY CREDIT
175970*** BUT THIS FIELD SHOULD ALREADY BE SET UP, SO IT IS COMMENTED
175980*
175990         MOVE BORV-01-DETAIL     TO BR0001-OUTPUT-TRANS-AREA
176010         MOVE 001                TO BR0001-OUTPUT-ERR-MSG-NO
176020         MOVE 84                 TO BR0001-OUTPUT-MSG-LENGTH
176030         PERFORM Z000-FORMT-SEND-PARA THRU Z099-EXIT
176040         MOVE ZERO               TO BR0001CA-TOM-CR-ACRU
176050         MOVE WA-TXN-AMOUNT      TO BR0001CA-TOM-DR-ACRU
176060     ELSE
176070         MOVE STRT-AREA-SOC-NO   TO LINK-IN-SOC-NO
176080         MOVE 2                  TO LINK-IN-CALL-CODE
176090         MOVE 13513              TO BORV-01-TRN-CODE
176100                                    LINK-IN-TXCODE
176110         MOVE 7                  TO BORV-01-SUB-LEDG-CODE
176120         COMPUTE WA-TXN-AMOUNT ROUNDED = BR0001CA-FCC-PRNC-ADJ
176130         SUBTRACT WA-TXN-AMOUNT FROM BORM-LOAN-BAL
176140         CALL "UT0250" USING UT0250-CALL-AREA
176150         IF LINK-OUT-STAT NOT = "****"
LON615             MOVE 100 TO BR0001-OUTPUT-ERR-MSG-NO
LON615             MOVE 1   TO WA-MSG-ERR-FLAG
LON615             MOVE 0   TO LINK-IN-TXCODE
176160             MOVE UT8500CA-FUNC-NORMAL-DISPLAY
176170                                 TO UT8500CA-CALL-CODE
176180             MOVE SPACES         TO UT8500CA-INPUT-MSG
176190             STRING "BR0001:(N720) TRAN 13513 NOT FOUND"
176200                    ", INST: " LINK-IN-SOC-NO
176210                    ", ACCOUNT NO: " BORM-MEMB-CUST-AC
176220                 DELIMITED BY SIZE INTO UT8500CA-INPUT-MSG
176230             CALL "UT8500" USING UT8500CA-AREA
LON615             GO TO N729-EXIT
176240         ELSE
176250             IF LINK-PRIORITY-CODE = "I"
176260                 SUBTRACT WA-TXN-AMOUNT FROM BORM-CAP-UNPAID-INT
176270             END-IF
176280             IF LINK-PRIORITY-CODE = "A"
176290                SUBTRACT WA-TXN-AMOUNT FROM
176300                                       BORM-UNPAID-ARRS-INT-BAL
176310             END-IF
176320             IF LINK-PRIORITY-CODE = "C"
176330                SUBTRACT WA-TXN-AMOUNT FROM
176340                                       BORM-UNPAID-CHRG-BAL
176350             END-IF
176360             IF LINK-PRIORITY-CODE = "P"
176370                SUBTRACT WA-TXN-AMOUNT FROM
176380                                       BORM-UNPAID-PRINCIPAL-BAL
176390             END-IF
176400             MOVE LINK-BOR-CODE  TO WA-LEDGER-CODE
176410             MOVE LINK-PRIORITY-CODE
176420                                 TO WA-PRIORITY-CODE
176430         END-IF
176440         MOVE STRT-AREA-POST-DT  TO BORV-01-TRN-DATE
176450                                    BORV-01-POST-DATE
176460         MOVE 1                  TO BORV-01-CODE
176470         MOVE WA-TXN-AMOUNT      TO BORV-01-AMOUNT
176480         MOVE BORM-LOAN-BAL      TO BORV-01-BALANCE
176490         SUBTRACT BORM-THEO-LOAN-BAL
176500                               FROM BORM-LOAN-BAL
176510                             GIVING BORV-01-ARREARS
176520         MOVE STRT-AREA-BRANCH   TO UPTELLCA-BRCH
176530         MOVE STRT-AREA-TELL-NO  TO UPTELLCA-TELR
176540         CALL "UT0031" USING UPTELLCA-PARAM-1
176550                             UPTELLCA-PARAM-2
176560         MOVE UPTELLCA-TELLER-BRANCH
176570                                 TO BORV-01-TELL-AND-BR
176580         MOVE STRT-AREA-TERM-NO  TO BORV-01-BRTERM
176590         MOVE ZERO               TO BORV-01-STATUS
176600                                    BORV-01-CHQ-DAYS
176610         MOVE STRT-AREA-JRNL-NO  TO BORV-01-JRNL-NO
176620         MOVE STRT-AREA-MSG-TIME TO BORV-01-POST-TIME
176630         PERFORM X200-ADD-NEW-TXN THRU X299-EXIT
176640
176650         MOVE BORV-01-DETAIL     TO BR0001-OUTPUT-TRANS-AREA
176670         MOVE 001                TO BR0001-OUTPUT-ERR-MSG-NO
176680         MOVE 84                 TO BR0001-OUTPUT-MSG-LENGTH
176690         PERFORM Z000-FORMT-SEND-PARA THRU Z099-EXIT
176700
176710         MOVE 13513              TO BORV-01-TRN-CODE
176720                                    LINK-IN-TXCODE
176730         COMPUTE WA-TXN-AMOUNT ROUNDED = BR0001CA-FCC-PRNC-ADJ
176740         ADD WA-TXN-AMOUNT       TO BORM-LOAN-BAL
176750         CALL "UT0250" USING UT0250-CALL-AREA
176760         IF LINK-OUT-STAT NOT = "****"
LON615             MOVE 100 TO BR0001-OUTPUT-ERR-MSG-NO
LON615             MOVE 1   TO WA-MSG-ERR-FLAG
LON615             MOVE 0   TO LINK-IN-TXCODE
176770             MOVE UT8500CA-FUNC-NORMAL-DISPLAY
176780                                 TO UT8500CA-CALL-CODE
176790             MOVE SPACES         TO UT8500CA-INPUT-MSG
176800             STRING "BR0001:(N720) TRAN 13513 NOT FOUND"
176810                    ", INST: " LINK-IN-SOC-NO
176820                    ", ACCOUNT NO: " BORM-MEMB-CUST-AC
176830                 DELIMITED BY SIZE INTO UT8500CA-INPUT-MSG
176840             CALL "UT8500" USING UT8500CA-AREA
LON615             GO TO N729-EXIT
176850         ELSE
176860             MOVE LINK-BOR-CODE  TO WA-LEDGER-CODE
176870             MOVE LINK-PRIORITY-CODE
176880                                 TO WA-PRIORITY-CODE
176890             IF LINK-PRIORITY-CODE = "I"
176900                 ADD WA-TXN-AMOUNT TO BORM-CAP-UNPAID-INT
176910             END-IF
176920             IF LINK-PRIORITY-CODE = "A"
176930                 ADD WA-TXN-AMOUNT
176940                                 TO BORM-UNPAID-ARRS-INT-BAL
176950             END-IF
176960             IF LINK-PRIORITY-CODE = "C"
176970                 ADD WA-TXN-AMOUNT
176980                                 TO BORM-UNPAID-CHRG-BAL
176990             END-IF
177000             IF LINK-PRIORITY-CODE = "P"
177010                 ADD WA-TXN-AMOUNT
177020                                 TO BORM-UNPAID-PRINCIPAL-BAL
177030             END-IF
177040         END-IF
177050         MULTIPLY WA-TXN-AMOUNT BY -1 GIVING BORV-01-AMOUNT
177060         MOVE BORM-LOAN-BAL      TO BORV-01-BALANCE
177070         SUBTRACT BORM-THEO-LOAN-BAL
177080                               FROM BORM-LOAN-BAL
177090                             GIVING BORV-01-ARREARS
177100         PERFORM X200-ADD-NEW-TXN THRU X299-EXIT
177110         MOVE BORV-01-DETAIL     TO BR0001-OUTPUT-TRANS-AREA
177130         MOVE 001                TO BR0001-OUTPUT-ERR-MSG-NO
177140         MOVE 84                 TO BR0001-OUTPUT-MSG-LENGTH
177150         PERFORM Z000-FORMT-SEND-PARA THRU Z099-EXIT
177160     END-IF.
177170 N729-EXIT.
177180     EXIT.
177190/
179660 N810-WRITE-TOM-TXNS.
179670*
179680* WRITE THE TOM INFLATION AMT CREDIT TRANSACTIONS
179690*
179700     INITIALIZE BORV-01-DETAIL.
179710     MOVE ZEROS                TO WA-TXN-AMOUNT.
179720     IF WA-TOM-ACRU-AMT < 0
179730         MOVE STRT-AREA-SOC-NO TO LINK-IN-SOC-NO
179740         MOVE 2                TO LINK-IN-CALL-CODE
179750         MOVE 13512            TO BORV-01-TRN-CODE
179760                                  LINK-IN-TXCODE
179770         MOVE 7                TO BORV-01-SUB-LEDG-CODE
179780         MULTIPLY WA-TOM-ACRU-AMT BY 1
179790                               GIVING  WA-TXN-AMOUNT ROUNDED
179800         SUBTRACT WA-TXN-AMOUNT FROM BORM-LOAN-BAL
179810         CALL "UT0250" USING

179830                         UT0250-CALL-AREA
179840         IF  LINK-OUT-STAT NOT = "****"
LON615             MOVE 100 TO BR0001-OUTPUT-ERR-MSG-NO
LON615             MOVE 1   TO WA-MSG-ERR-FLAG
LON615             MOVE 0   TO LINK-IN-TXCODE
179850             MOVE UT8500CA-FUNC-NORMAL-DISPLAY
179860                         TO UT8500CA-CALL-CODE
179870             MOVE SPACES TO UT8500CA-INPUT-MSG
179880             STRING "BR0001:(N810) TRAN 13512 NOT "
179890                    "FOUND, INST: " LINK-IN-SOC-NO
179900                    "ACCOUNT NO: " BORM-MEMB-CUST-AC
179910                 DELIMITED BY SIZE INTO UT8500CA-INPUT-MSG
179920             CALL "UT8500" USING

179940                         UT8500CA-AREA
LON615             GO TO N819-EXIT
179950         ELSE
179960             IF LINK-PRIORITY-CODE = "I"
179970                SUBTRACT WA-TXN-AMOUNT FROM BORM-CAP-UNPAID-INT
179980             END-IF
179990             IF LINK-PRIORITY-CODE = "A"
180000                SUBTRACT WA-TXN-AMOUNT FROM
180010                                       BORM-UNPAID-ARRS-INT-BAL
180020             END-IF
180030             IF LINK-PRIORITY-CODE = "C"
180040                SUBTRACT WA-TXN-AMOUNT FROM
180050                                       BORM-UNPAID-CHRG-BAL
180060             END-IF
180070             IF LINK-PRIORITY-CODE = "P"
180080                SUBTRACT WA-TXN-AMOUNT FROM
180090                                       BORM-UNPAID-PRINCIPAL-BAL
180100             END-IF
180110             MOVE LINK-BOR-CODE TO WA-LEDGER-CODE
180120             MOVE LINK-PRIORITY-CODE TO WA-PRIORITY-CODE
180130         END-IF
180140         MOVE STRT-AREA-POST-DT      TO  BORV-01-TRN-DATE
180150                                         BORV-01-POST-DATE
180160         MOVE 1                      TO  BORV-01-CODE
180170         MOVE WA-TXN-AMOUNT          TO  BORV-01-AMOUNT
180180         MOVE BORM-LOAN-BAL          TO  BORV-01-BALANCE
180190         SUBTRACT BORM-THEO-LOAN-BAL
180200                                 FROM    BORM-LOAN-BAL
180210                                 GIVING  BORV-01-ARREARS
180220         MOVE STRT-AREA-BRANCH       TO UPTELLCA-BRCH
180230         MOVE STRT-AREA-TELL-NO      TO UPTELLCA-TELR
180240         CALL "UT0031" USING

180260                         UPTELLCA-PARAM-1
180270                         UPTELLCA-PARAM-2
180280         MOVE UPTELLCA-TELLER-BRANCH TO  BORV-01-TELL-AND-BR
180290         MOVE STRT-AREA-TERM-NO      TO  BORV-01-BRTERM
180300         MOVE 0                      TO  BORV-01-STATUS
180310                                         BORV-01-CHQ-DAYS
180320         MOVE STRT-AREA-JRNL-NO      TO  BORV-01-JRNL-NO
180330         MOVE STRT-AREA-MSG-TIME     TO  BORV-01-POST-TIME
180340         PERFORM X200-ADD-NEW-TXN THRU X299-EXIT

180360         MOVE BORV-01-DETAIL     TO BR0001-OUTPUT-TRANS-AREA
180370         MOVE "DTOM"             TO STRT-AREA-MNEMONIC
180380         MOVE 001                TO BR0001-OUTPUT-ERR-MSG-NO
180390         MOVE 84                 TO BR0001-OUTPUT-MSG-LENGTH
180400         PERFORM Z000-FORMT-SEND-PARA THRU Z099-EXIT

180420         MOVE 10731            TO BORV-01-TRN-CODE
180430                                  LINK-IN-TXCODE
180440         MULTIPLY WA-TOM-ACRU-AMT BY 1
180450                                  GIVING  WA-TXN-AMOUNT ROUNDED
180460         ADD       WA-TXN-AMOUNT  TO   BORM-LOAN-BAL
180470         CALL "UT0250" USING

180490                         UT0250-CALL-AREA
180500         IF  LINK-OUT-STAT NOT = "****"
LON615             MOVE 100 TO BR0001-OUTPUT-ERR-MSG-NO
LON615             MOVE 1   TO WA-MSG-ERR-FLAG
LON615             MOVE 0   TO LINK-IN-TXCODE
180510             MOVE UT8500CA-FUNC-NORMAL-DISPLAY
180520                         TO UT8500CA-CALL-CODE
180530             MOVE SPACES TO UT8500CA-INPUT-MSG
180540             STRING "BR0001:(N810) TRAN 10731 NOT "
180550                    "FOUND, INST: " LINK-IN-SOC-NO
180560                    "ACCOUNT NO: " BORM-MEMB-CUST-AC
180570                 DELIMITED BY SIZE INTO UT8500CA-INPUT-MSG
180580             CALL "UT8500" USING

180600                         UT8500CA-AREA
LON615             GO TO N819-EXIT
180610         ELSE
180620             MOVE LINK-BOR-CODE TO WA-LEDGER-CODE
180630             MOVE LINK-PRIORITY-CODE TO WA-PRIORITY-CODE
180640             IF LINK-PRIORITY-CODE = "I"
180650                ADD WA-TXN-AMOUNT TO BORM-CAP-UNPAID-INT
180660             END-IF
180670             IF LINK-PRIORITY-CODE = "A"
180680                ADD WA-TXN-AMOUNT TO BORM-UNPAID-ARRS-INT-BAL
180690             END-IF
180700             IF LINK-PRIORITY-CODE = "C"
180710                ADD WA-TXN-AMOUNT TO BORM-UNPAID-CHRG-BAL
180720             END-IF
180730             IF LINK-PRIORITY-CODE = "P"
180740                ADD WA-TXN-AMOUNT TO BORM-UNPAID-PRINCIPAL-BAL
180750             END-IF
180760         END-IF
180770         MULTIPLY WA-TXN-AMOUNT BY -1 GIVING BORV-01-AMOUNT
180780         MOVE BORM-LOAN-BAL          TO  BORV-01-BALANCE
180790         SUBTRACT BORM-THEO-LOAN-BAL
180800                                 FROM    BORM-LOAN-BAL
180810                                 GIVING  BORV-01-ARREARS
180820         PERFORM X200-ADD-NEW-TXN THRU X299-EXIT
180830         MOVE BORV-01-DETAIL     TO BR0001-OUTPUT-TRANS-AREA
180840         MOVE "CTOM"             TO STRT-AREA-MNEMONIC
180850         MOVE 001                TO BR0001-OUTPUT-ERR-MSG-NO
180860         MOVE 84                 TO BR0001-OUTPUT-MSG-LENGTH
180870         PERFORM Z000-FORMT-SEND-PARA THRU Z099-EXIT
180880         MOVE ZERO               TO BR0001CA-TOM-CR-ACRU
180890         MOVE WA-TXN-AMOUNT      TO BR0001CA-TOM-DR-ACRU
180900     ELSE
180910         MOVE STRT-AREA-SOC-NO TO LINK-IN-SOC-NO
180920         MOVE 2                TO LINK-IN-CALL-CODE
180930         MOVE 13512            TO BORV-01-TRN-CODE
180940                                  LINK-IN-TXCODE
180950         MOVE 7                TO BORV-01-SUB-LEDG-CODE
180960         MULTIPLY WA-TOM-ACRU-AMT BY 1
180970                                  GIVING  WA-TXN-AMOUNT ROUNDED
180980         SUBTRACT WA-TXN-AMOUNT FROM BORM-LOAN-BAL
180990         CALL "UT0250" USING

181010                         UT0250-CALL-AREA
181020         IF  LINK-OUT-STAT NOT = "****"
LON615             MOVE 100 TO BR0001-OUTPUT-ERR-MSG-NO
LON615             MOVE 1   TO WA-MSG-ERR-FLAG
LON615             MOVE 0   TO LINK-IN-TXCODE
181030             MOVE UT8500CA-FUNC-NORMAL-DISPLAY
181040                         TO UT8500CA-CALL-CODE
181050             MOVE SPACES TO UT8500CA-INPUT-MSG
181060             STRING "BR0001:(N810) TRAN 13512 NOT "
181070                    "FOUND, INST: " LINK-IN-SOC-NO
181080                    "ACCOUNT NO: " BORM-MEMB-CUST-AC
181090                 DELIMITED BY SIZE INTO UT8500CA-INPUT-MSG
181100             CALL "UT8500" USING

181120                         UT8500CA-AREA
LON615             GO TO N819-EXIT
181130         ELSE
181140             IF LINK-PRIORITY-CODE = "I"
181150                SUBTRACT WA-TXN-AMOUNT FROM BORM-CAP-UNPAID-INT
181160             END-IF
181170             IF LINK-PRIORITY-CODE = "A"
181180                SUBTRACT WA-TXN-AMOUNT FROM
181190                                       BORM-UNPAID-ARRS-INT-BAL
181200             END-IF
181210             IF LINK-PRIORITY-CODE = "C"
181220                SUBTRACT WA-TXN-AMOUNT FROM
181230                                       BORM-UNPAID-CHRG-BAL
181240             END-IF
181250             IF LINK-PRIORITY-CODE = "P"
181260                SUBTRACT WA-TXN-AMOUNT FROM
181270                                       BORM-UNPAID-PRINCIPAL-BAL
181280             END-IF
181290             MOVE LINK-BOR-CODE TO WA-LEDGER-CODE
181300             MOVE LINK-PRIORITY-CODE TO WA-PRIORITY-CODE
181310         END-IF
181320         MOVE STRT-AREA-POST-DT      TO  BORV-01-TRN-DATE
181330                                         BORV-01-POST-DATE
181340         MOVE 1                      TO  BORV-01-CODE
181350         MOVE WA-TXN-AMOUNT          TO  BORV-01-AMOUNT
181360         MOVE BORM-LOAN-BAL          TO  BORV-01-BALANCE
181370         SUBTRACT BORM-THEO-LOAN-BAL
181380                                 FROM    BORM-LOAN-BAL
181390                                 GIVING  BORV-01-ARREARS
181400         MOVE STRT-AREA-BRANCH       TO UPTELLCA-BRCH
181410         MOVE STRT-AREA-TELL-NO      TO UPTELLCA-TELR
181420         CALL "UT0031" USING

181440                         UPTELLCA-PARAM-1
181450                         UPTELLCA-PARAM-2
181460         MOVE UPTELLCA-TELLER-BRANCH TO  BORV-01-TELL-AND-BR
181470         MOVE STRT-AREA-TERM-NO      TO  BORV-01-BRTERM
181480         MOVE 0                      TO  BORV-01-STATUS
181490                                         BORV-01-CHQ-DAYS
181500         MOVE STRT-AREA-JRNL-NO      TO  BORV-01-JRNL-NO
181510         MOVE STRT-AREA-MSG-TIME     TO  BORV-01-POST-TIME
181520         PERFORM X200-ADD-NEW-TXN THRU X299-EXIT

181540         MOVE BORV-01-DETAIL     TO BR0001-OUTPUT-TRANS-AREA
181550         MOVE "DTOM"             TO STRT-AREA-MNEMONIC
181560         MOVE 001                TO BR0001-OUTPUT-ERR-MSG-NO
181570         MOVE 84                 TO BR0001-OUTPUT-MSG-LENGTH
181580         PERFORM Z000-FORMT-SEND-PARA THRU Z099-EXIT

181600         MOVE 10731            TO BORV-01-TRN-CODE
181610                                  LINK-IN-TXCODE
181620         MULTIPLY WA-TOM-ACRU-AMT BY 1
181630                                  GIVING  WA-TXN-AMOUNT ROUNDED
181640         ADD       WA-TXN-AMOUNT  TO   BORM-LOAN-BAL
181650         CALL "UT0250" USING

181670                         UT0250-CALL-AREA
181680         IF  LINK-OUT-STAT NOT = "****"
LON615             MOVE 100 TO BR0001-OUTPUT-ERR-MSG-NO
LON615             MOVE 1   TO WA-MSG-ERR-FLAG
LON615             MOVE 0   TO LINK-IN-TXCODE
181690             MOVE UT8500CA-FUNC-NORMAL-DISPLAY
181700                         TO UT8500CA-CALL-CODE
181710             MOVE SPACES TO UT8500CA-INPUT-MSG
181720             STRING "BR0001:(N810) TRAN 10731 NOT "
181730                    "FOUND, INST: " LINK-IN-SOC-NO
181740                    "ACCOUNT NO: " BORM-MEMB-CUST-AC
181750                 DELIMITED BY SIZE INTO UT8500CA-INPUT-MSG
181760             CALL "UT8500" USING

181780                         UT8500CA-AREA
LON615             GO TO N819-EXIT
181790         ELSE
181800             MOVE LINK-BOR-CODE TO WA-LEDGER-CODE
181810             MOVE LINK-PRIORITY-CODE TO WA-PRIORITY-CODE
181820             IF LINK-PRIORITY-CODE = "I"
181830                ADD WA-TXN-AMOUNT TO BORM-CAP-UNPAID-INT
181840             END-IF
181850             IF LINK-PRIORITY-CODE = "A"
181860                ADD WA-TXN-AMOUNT TO BORM-UNPAID-ARRS-INT-BAL
181870             END-IF
181880             IF LINK-PRIORITY-CODE = "C"
181890                ADD WA-TXN-AMOUNT TO BORM-UNPAID-CHRG-BAL
181900             END-IF
181910             IF LINK-PRIORITY-CODE = "P"
181920                ADD WA-TXN-AMOUNT TO BORM-UNPAID-PRINCIPAL-BAL
181930             END-IF
181940         END-IF
181950         MULTIPLY WA-TXN-AMOUNT BY -1 GIVING BORV-01-AMOUNT
181960         MOVE BORM-LOAN-BAL          TO  BORV-01-BALANCE
181970         SUBTRACT BORM-THEO-LOAN-BAL
181980                                 FROM    BORM-LOAN-BAL
181990                                 GIVING  BORV-01-ARREARS
182000         PERFORM X200-ADD-NEW-TXN THRU X299-EXIT
182010         MOVE BORV-01-DETAIL     TO BR0001-OUTPUT-TRANS-AREA
182020         MOVE "CTOM"             TO STRT-AREA-MNEMONIC
182030         MOVE 001                TO BR0001-OUTPUT-ERR-MSG-NO
182040         MOVE 84                 TO BR0001-OUTPUT-MSG-LENGTH
182050         PERFORM Z000-FORMT-SEND-PARA THRU Z099-EXIT
182060         MOVE ZERO               TO BR0001CA-TOM-CR-ACRU
182070         MOVE WA-TXN-AMOUNT      TO BR0001CA-TOM-DR-ACRU
182080     END-IF.
182090*
182100 N819-EXIT.
182110     EXIT.
182120/
182130 N900-ROUND-CURRENCY.
182140*
182150     IF UT0290CA-CURR-DECS = 0
182160         COMPUTE WA-WORK-AMOUNT-0 ROUNDED = WA-WORK-AMOUNT-3
182170         MOVE WA-WORK-AMOUNT-0 TO WA-WORK-AMOUNT-3
182180     ELSE IF UT0290CA-CURR-DECS = 1
182190         COMPUTE WA-WORK-AMOUNT-1 ROUNDED = WA-WORK-AMOUNT-3
182200         MOVE WA-WORK-AMOUNT-1 TO WA-WORK-AMOUNT-3
182210     ELSE IF UT0290CA-CURR-DECS = 2
182220         COMPUTE WA-WORK-AMOUNT-2 ROUNDED = WA-WORK-AMOUNT-3
182230         MOVE WA-WORK-AMOUNT-2 TO WA-WORK-AMOUNT-3.
182270*
182280 N999-EXIT.
182290     EXIT.
182660/
182670 E290-SEARCH-RATE-TABLE.
182680*    USE CALL CODE 2 AND 3 IF SHARED MEMORY
182690     MOVE BORP-SOC-NO    TO UT0298CA-CALL-SOC-NO
182700     MOVE BORP-ACCT-TYPE TO UT0298CA-CALL-ACCT-TYPE
182710     MOVE BORP-INT-CAT   TO UT0298CA-CALL-INT-CAT
182720     MOVE BORP-BASM-BASE-ID TO UT0298CA-CALL-BASE-ID
182730     MOVE BORP-BASM-RATE-ID TO UT0298CA-CALL-RATE-ID
LON327*CHKING        DISPLAY "RST567: " BORP-BASM-RATE-ID
182740     MOVE "BOR"             TO UT0298CA-CALL-SYS
182750     IF BORP-TIER-GROUP-ID = SPACES
182760         MOVE 2 TO UT0298CA-CALL-CODE
182770     ELSE
182780         MOVE 3 TO UT0298CA-CALL-CODE
182790         MOVE BORP-TIER-GROUP-ID TO UT0298CA-CALL-TIER-ID
182800         MOVE SPACES             TO UT0298CA-CALL-TIER-RATE-ID
182810         MOVE SPACES             TO UT0298CA-ACCT-TIER-ID
182820         MOVE BORM-LOAN-BAL TO UT0298CA-CALL-TIER-MAX-VAL
182830     END-IF.
182840     MOVE STRT-AREA-POST-DT   TO UT0298CA-CALL-TXN-DATE.
182850     CALL "UT0298" USING

182870                         UT0298CA-CALL-AREA.
182880     IF UT0298CA-RET-STATUS = 0
182890         MOVE UT0298CA-RET-EFF-DATE TO WA-INT-DATE-A
182900         MOVE UT0298CA-RET-EFF-RATE TO WA-INT-RATE-A
182910     ELSE
182920         MOVE 1920 TO BR0001-OUTPUT-ERR-MSG-NO
182930         MOVE 1    TO WA-MSG-ERR-FLAG
182940         MOVE UT8500CA-FUNC-NORMAL-DISPLAY TO UT8500CA-CALL-CODE
182950         MOVE SPACES TO UT8500CA-INPUT-MSG
182960         STRING "BR0000:(E290) - INVALID CALL TO UT0298, "
182970                "STATUS:"    UT0298CA-RET-STATUS
182980                " FOR BORP:" UT0298CA-CALL-KEY
182990                ", ACCT:"    BORM-KEY
183000             DELIMITED BY SIZE INTO UT8500CA-INPUT-MSG
183010         CALL "UT8500" USING

183030                         UT8500CA-AREA.
183040 E299-EXIT.
183050     EXIT.
185090/
187410 S000-WRITE-BORM-AUDIT-REC.
LON236*
187420     MOVE        STRT-AREA-CNTL-INFO TO
187430                                         WA-AUDT-CNTL-INFO.
187440     MOVE        "B" TO                  WA-AUDT-SYS.
187450     MOVE        BORM-BR-NO TO           WA-AUDT-HOME-BR.
187460     MOVE        "BORM" TO               WA-AUDT-DATASET.
187480* THE OLD AND NEW AUDIT IMAGES ARE POPULATED BFORE CALLING S000.
187490     CALL "SY70SEND" USING

187510                     STARTAREA
187520                     WA-AUDT-AREA.
187530*
187540 S099-EXIT.
187550     EXIT.
TB0069/
TB0069 S000A-WRITE-BOIS-AUDIT-REC.
TB0069* the old and new audit images shld be populated b4 calling this r
TB0069
TB0069     MOVE    STRT-AREA-CNTL-INFO TO  WA-AUDT-CNTL-INFO.
TB0069     MOVE    "B"                 TO  WA-AUDT-SYS.
TB0069     MOVE    BORM-BR-NO          TO  WA-AUDT-HOME-BR.
TB0069     MOVE    "BOIS"              TO  WA-AUDT-DATASET.
TB0069     CALL "SY70SEND" USING
TB0069
TB0069                     STARTAREA
TB0069                     WA-AUDT-AREA.
TB0069*
TB0069 S099A-EXIT.
TB0069     EXIT.
187710/
187720 S100-REFORMAT-ACCOUNT-NO.
187730*
187740     MOVE 2 TO  UT0151CA-CALL-FUNC.
187750     MOVE SOC-CHK-DGT            TO
187760                                 UT0151CA-CHK-DGT-TYPE.
187770     CALL "UT0151" USING UT0151CA-CALL-AREA.
187780     IF  UT0151CA-STATUS NOT = ZERO
187790         MOVE UT8500CA-FUNC-NORMAL-DISPLAY TO UT8500CA-CALL-CODE
187800         MOVE SPACES TO UT8500CA-INPUT-MSG
187810         STRING "BR0001:(S100) CHECK DIGIT CALC ERROR "
187820                DELIMITED BY SIZE INTO UT8500CA-INPUT-MSG
187830         PERFORM UT8500-CALL THRU UT8500-EXIT
187840         MOVE 1 TO WA-MSG-ERR-FLAG
187850         GO TO S199-EXIT.
187860*
187870 S199-EXIT.
187880     EXIT.
187890/
LON236* CANCEL THIS BORV-34
TB0069*T100-STORE-BORV-34.
TB0069******************************************************************
TB0069*    to be adjusted for rate type change on ZERO rate
TB0069******************************************************************
S12223*    IF STRT-AREA-TRN-NO = 17003
S11444*       GO TO   T199-EXIT.
S11444*
188980*    IF BORP-INT-TRANS NOT = "Y"
188990*       GO TO   T199-EXIT.
189000*    INITIALIZE BORV-34-DETAIL.
189010*    MOVE ZERO  TO BORV-34-TO-RATE.
S12223*    IF STRT-AREA-TRN-NO = 17000
NR0017*       MOVE WA-BKDATE-EXP-FROM     TO BORV-34-TRN-DATE
189060*    ELSE
NR0017*       MOVE WA-12000-PROCESS-DATE  TO BORV-34-TRN-DATE.
NR0017*    MOVE STRT-AREA-POST-DT      TO BORV-34-POST-DATE.
189100*    MOVE 34 TO BORV-34-CODE.
189110*    MOVE UPTELLCA-TELLER-BRANCH TO BORV-34-TELL-AND-BR.
189120*    MOVE 13507 TO BORV-34-TRN-CODE.
189130*    MOVE 10    TO BORV-34-STATUS.
189140*    IF BORM-STORE-RATE = ZERO
S12223*    AND NOT SOC-SITE-GCR
189160*       MOVE "EFFECTIVE RATE"  TO BORV-34-MESSAGE1
189170*       MOVE WA-INT-RATE-A     TO BORV-34-FROM-RATE
189180*       MOVE "%    "           TO BORV-34-MESSAGE2
189190*    ELSE
189200*       MOVE "RATE CHANGED FM" TO BORV-34-MESSAGE1
189210*       MOVE BORM-STORE-RATE   TO BORV-34-FROM-RATE
189220*       MOVE "% TO "           TO BORV-34-MESSAGE2
189230*       MOVE WA-INT-RATE-A     TO BORV-34-TO-RATE
189240*       MOVE "%"               TO BORV-34-PERCENT.
189250*    MOVE 1  TO BORV-34-BRTERM.
      *
189270*    IF WA-BOR-FIELD-CHANGE NOT = 1
189280*       MOVE "Y" TO WA-NO-NPB-ADD
189290*       MOVE 1   TO WA-BOR-FIELD-CHANGE
189300*    END-IF.
189310*    ADD  1 TO BORM-NO-NPB.
      *
189330*    MOVE 0 TO DBIO-CALL-REFER-AREA.
189340*    PERFORM X200-ADD-NEW-TXN THRU X299-EXIT.
189350*    IF WA-MSG-ERR-FLAG = 1
189360*       SUBTRACT 1 FROM BORM-NO-NPB
189370*       IF WA-NO-NPB-ADD = "Y"
189380*          MOVE "N" TO WA-NO-NPB-ADD
189390*          MOVE 0   TO WA-BOR-FIELD-CHANGE
189400*       END-IF
189410*       GO   TO  T199-EXIT
189420*    END-IF.
      *
JNATMP*    IF NOT BORP-YOUBE
TB0069*       PERFORM ZA2300-CALC-BUCV-ACCR
      *
189490*       IF WA-MSG-ERR-FLAG = 1
189500*          GO   TO  T199-EXIT
189510*       END-IF
JNATMP*    END-IF.
      *
189830*T199-EXIT.
189840*    EXIT.
LON236* END THIS COMMENT
190460/
ST0125 U000-CALL-MI0095.
ST0125     MOVE STRT-AREA-RECOVERY TO MI0095CA-RECOVERY-FLAG.
ST0125     CALL "MI0095" USING
ST0125                          LS-MI0095CA
190510                          WA-EDIT-MASK-TABLE
190520                          BORR-PARAMS
190530                          BORM-RECORD-AREA
190540                          BORV-RECORD-AREA
S10211                          BOIS-RECORD-AREA
190550                          LS-RFSH-AREA
190560                          WA-TODAY-DATE
190570                          MULTI-OVERRIDE-AREA.
190580 U099-EXIT.
190590     EXIT.
190600/
190610 U100-CUSM-ACCESS.
190620*
190630     MOVE SPACES TO DBIO-CALL-STATUS.
190640     CALL "DBIO" USING

190660                 DBIO-CALL-PARAM
190670                 DBIO-CALL-SYS-AREA
190680                 DBIO-CALL-KEY-AREA
190690                 DBIO-CALL-STATUS
190700                 CUSM-RECORD.
190710 U199-EXIT.
190720     EXIT.
190730/
191280 U500-LANG-ACCESS.
191290      CALL "DBIO"  USING

191310                   DBIO-CALL-PARAM
191320                   DBIO-CALL-SYS-AREA
191330                   LNGM-KEY-AREA
191340                   DBIO-CALL-STATUS
191350                   LNGM-AREA.
191360 U599-EXIT.
191370     EXIT.
191380/
191390 U9000-RETRIEVE-TXTM-RECORDS.
191400     MOVE 1  TO UT9000-CALL-CODE.
191410     PERFORM U9100-RETRIEVE-TEXT THRU U9199-EXIT.
191420     IF  UT9000-CALL-STATUS = "0"
S11711         IF  UT9000-SEQUENCE-NO > 47
191480             MOVE "E" TO UT9000-CALL-STATUS
191490             MOVE UT8500CA-FUNC-NORMAL-DISPLAY TO
191500                  UT8500CA-CALL-CODE
191510             MOVE SPACES TO UT8500CA-INPUT-MSG
191520             STRING "BR0001:(U900) BYPASSING TXTM, SEQ NO > 20 "
191530                     "/CHECK TXTM RECORDS FOR THIS PROGRAM "
191540                    DELIMITED BY SIZE INTO UT8500CA-INPUT-MSG
191550             PERFORM UT8500-CALL THRU UT8500-EXIT
191560         ELSE
191570             PERFORM U9050-CHECK-TXT-INDEX THRU U9059-EXIT
191580             IF  TXT-1 > 2 OR TXT-2 > 2
191590                 MOVE "E" TO UT9000-CALL-STATUS
191600             ELSE
191610                 SET TXT-3 TO UT9000-SEQUENCE-NO
191620                 MOVE UT9000RA-LINE1 TO TXT-LINE
191630                      (TXT-1 TXT-2 TXT-3)
191640     ELSE
191650         IF  UT9000-CALL-STATUS NOT = "E"
191660             MOVE UT8500CA-FUNC-NORMAL-DISPLAY TO
191670                  UT8500CA-CALL-CODE
191680             MOVE SPACES TO UT8500CA-INPUT-MSG
191690             STRING "BR0001:(U900) INVALID CALL TO UT9000, "
191700                    "KEY:"      UT9000-RECORD-KEY
191710                    ", STATUS:" UT9000-CALL-STATUS
191720                    DELIMITED BY SIZE INTO UT8500CA-INPUT-MSG
191730             PERFORM UT8500-CALL THRU UT8500-EXIT
191740             MOVE "E" TO UT9000-CALL-STATUS.
191750*
191760 U9009-EXIT.
191770     EXIT.
191780/
191790 U9050-CHECK-TXT-INDEX.
191800     IF  TXT-1 = 0
191810         SET TXT-1, TXT-2 TO 1
191820         MOVE UT9000-INST-NO       TO TXT-INST (TXT-1)
191830         MOVE UT9000-LANGUAGE-CODE TO TXT-LANG (TXT-1, TXT-2).
191840     IF  UT9000-INST-NO = TXT-INST (TXT-1)
191850         IF  UT9000-LANGUAGE-CODE = TXT-LANG (TXT-1, TXT-2)
191860             NEXT SENTENCE
191870         ELSE
191880             SET TXT-2 UP BY 1
191890             IF  TXT-2 > 2
191900                 MOVE UT8500CA-FUNC-NORMAL-DISPLAY TO
191910                      UT8500CA-CALL-CODE
191920                 MOVE SPACES TO UT8500CA-INPUT-MSG
191930                 STRING "BR0001:(U905) MAXIMUM LANGUAGE IS 2"
191940                        ", INST-NO:" UT9000-INST-NO
191950                        ", LANG:"    UT9000-LANGUAGE-CODE
191960                        ", BYPASS LOADING FOR THIS LANG CODE"
191970                        DELIMITED BY SIZE INTO UT8500CA-INPUT-MSG
191980                 PERFORM UT8500-CALL THRU UT8500-EXIT
191990             ELSE
192000                 MOVE UT9000-LANGUAGE-CODE TO TXT-LANG
192010                                              (TXT-1 TXT-2)
192020     ELSE
192030         SET TXT-1 UP BY 1
192040         IF  TXT-1 > 2
192050             MOVE UT8500CA-FUNC-NORMAL-DISPLAY TO
192060                  UT8500CA-CALL-CODE
192070             MOVE SPACES TO UT8500CA-INPUT-MSG
192080             STRING "BR0001:(U905) MAXIMUM INSTITUTION IS 2"
192090                     ", INST-NO:" UT9000-INST-NO
192100                     ", BYPASS LOADING OF REMAINING TXTM"
192110                    DELIMITED BY SIZE INTO UT8500CA-INPUT-MSG
192120             PERFORM UT8500-CALL THRU UT8500-EXIT
192130             MOVE "END." TO DBIO-CALL-SAVE-REFER
192140             MOVE "E" TO UT9000-CALL-STATUS
192150         ELSE
192160             MOVE UT9000-INST-NO TO TXT-INST (TXT-1)
192170             SET TXT-2 TO 1
192180             MOVE UT9000-LANGUAGE-CODE TO TXT-LANG (TXT-1, TXT-2).
192190 U9059-EXIT.
192200     EXIT.
192210/
192220 U9100-RETRIEVE-TEXT.
192230*
192240     CALL "UT9000" USING

192260                          UT9000-AREA.
LON527     IF UT9000-CALL-STATUS NOT = 0
LON527        MOVE UT8500CA-FUNC-NORMAL-DISPLAY TO
LON527             UT8500CA-CALL-CODE
LON527        MOVE SPACES TO UT8500CA-INPUT-MSG
LON527        STRING "BR0001:(U9100) UT9000 CALL ERROR, STAT- "
LON527        UT9000-CALL-STATUS ", KEY- " UT9000-RECORD-KEY
LON527        DELIMITED BY SIZE INTO UT8500CA-INPUT-MSG
LON527        CALL "UT8500" USING
LON527                           UT8500CA-AREA
LON527        GO  TO  U9199-EXIT
LON527     END-IF.
192270*
192280 U9199-EXIT.
192290     EXIT.
192300/
SC6871 V550-INPUT-AMT-DECIMAL-CHECK.
SC6871     IF WA-VA-AMT = 0
SC6871        GO TO V550-EXIT.
SC6871     IF (UT0290CA-CURR-DECS = 0  AND  WA-DECS-0 > 0 )   OR
SC6871        (UT0290CA-CURR-DECS = 1  AND  WA-DECS-1 > 0 )   OR
SC6871        (UT0290CA-CURR-DECS = 2  AND  WA-DECS-2 > 0 )
SC6871         MOVE 1                  TO WA-MSG-ERR-FLAG
SC6871         MOVE 850                TO BR0001-OUTPUT-ERR-MSG-NO.
SC6871  V550-EXIT.
SC6871     EXIT.
218520/
221680******************************************************************
221690*        DBIO OPERATIONS
221700******************************************************************
222040/
222050 W500-AMEND-BORM.
222060     MOVE 32           TO DBIO-CALL-FUNC.
222070     MOVE "BOR"        TO DBIO-CALL-SYS-AREA.
222080     MOVE SPACES       TO DBIO-CALL-STATUS.
222090     MOVE BORM-KEY     TO DBIO-CALL-KEY-AREA.
222100     CALL "DBIO" USING

222120                 DBIO-CALL-PARAM
222130                 DBIO-CALL-SYS-AREA
222140                 DBIO-CALL-KEY-AREA
222150                 DBIO-CALL-STATUS
222160                 BORM-RECORD-AREA.
TB0069*
TB0069*    IF   DBIO-CALL-STATUS = "****"
TB0069*         MOVE "REWRITE" TO DBIOMOD-FUNCTION
TB0069*         PERFORM W3100-ACCESS-BOIS THRU W3199-EXIT
TB0069*    END-IF.
TB0069*
222230     IF  DBIO-CALL-STATUS NOT = "****"
222240         MOVE UT8500CA-FUNC-NORMAL-DISPLAY TO UT8500CA-CALL-CODE
222250         MOVE SPACES TO UT8500CA-INPUT-MSG
222260         STRING "BR0001:(W500) REWRITE ERROR ON LOANS KEY = "
222270                   DBIO-CALL-KEY-AREA
222280             DELIMITED BY SIZE INTO UT8500CA-INPUT-MSG
222290         PERFORM UT8500-CALL THRU UT8500-EXIT
222300         MOVE 155                TO BR0001-OUTPUT-ERR-MSG-NO
222310         MOVE "BORM" TO BR0001-OUTPUT-DATA-SET
222320         MOVE DBIO-CALL-STATUS TO BR0001-OUTPUT-STATUS
222330         MOVE 1                  TO WA-MSG-ERR-FLAG.
222340 W599-EXIT.
222350     EXIT.
223950/
TB0027 W3100-ACCESS-BOIS.
TB0027     MOVE "BOIS"   TO DBIOMOD-FILE
TB0027                      DBIOMOD-SUFFIX.
TB0027     MOVE "DBIO"   TO DBIOMOD-PREFIX.
TB0027     MOVE BORM-KEY OF BORM-RECORD-AREA
TB0027                   TO DBIOMOD-KEY
TB0027                      BOIS-KEY.
TB0027     MOVE ZEROES   TO DBIOMOD-KEY-NO.
TB0027     MOVE SPACES   TO DBIOMOD-STATUS
TB0027                      DBIOMOD-RELEASE-LOCK
TB0027                      DBIOMOD-ACCESS-COMMAND
TB0027                      DBIOMOD-ACCESS-COND.
TB0027     CALL DBIOMODNAME USING
TB0027                              DBIOMOD-FILE
TB0027                              DBIOMOD-KEY
TB0027                              BOIS-RECORD-AREA
TB0027                              DBIOMOD-FUNCTION
TB0027                              DBIOMOD-KEY-NO
TB0027                              DBIOMOD-RELEASE-LOCK
TB0027                              DBIOMOD-ACCESS-COMMAND
TB0027                              DBIOMOD-ACCESS-COND
TB0027                              DBIOMOD-STATUS.
TB0027
TB0027     IF DBIOMOD-STAT NOT = "****"
TB0027         MOVE 1            TO WA-MSG-ERR-FLAG
TB0027         MOVE "BOIS"       TO BR0001-OUTPUT-DATA-SET
TB0027         MOVE DBIOMOD-STAT TO BR0001-OUTPUT-STATUS
TB0027         MOVE 155          TO BR0001-OUTPUT-ERR-MSG-NO
TB0027         MOVE UT8500CA-FUNC-NORMAL-DISPLAY TO UT8500CA-CALL-CODE
TB0027         MOVE SPACES TO UT8500CA-INPUT-MSG
TB0027         STRING "BR0001:(W3100) WRITE BOIS ERROR"
TB0027                ", STAT="  DBIOMOD-STAT
TB0027                ", KEY="   DBIOMOD-KEY
TB0027                ", FUNC="  DBIOMOD-FUNCTION
TB0027                DELIMITED BY SIZE INTO UT8500CA-INPUT-MSG
TB0027         CALL "UT8500" USING UT8500CA-AREA.
TB0027 W3199-EXIT.
TB0027     EXIT.
229080/
229090 X200-ADD-NEW-TXN.
229100     MOVE 12                     TO DBIO-CALL-FUNC.
229110     MOVE "BOR"                  TO DBIO-CALL-SYS.
229120     MOVE BORM-KEY               TO DBIO-CALL-KEY.
229130     MOVE SPACES                 TO DBIO-CALL-STATUS.
229140     CALL "DBIO" USING

229160                       DBIO-CALL-PARAM
229170                       DBIO-CALL-SYS-AREA
229180                       DBIO-CALL-KEY-AREA
229190                       DBIO-CALL-STATUS
229200                       BORV-RECORD-AREA.
229210     IF  DBIO-CALL-STATUS NOT = "****"
229220         MOVE 1             TO WA-MSG-ERR-FLAG
229230         MOVE 155           TO BR0001-OUTPUT-ERR-MSG-NO
229240         MOVE "BORV"        TO BR0001-OUTPUT-DATA-SET
229250         MOVE DBIO-CALL-STATUS TO BR0001-OUTPUT-STATUS
229260         MOVE UT8500CA-FUNC-NORMAL-DISPLAY TO
229270              UT8500CA-CALL-CODE
229280         MOVE SPACES TO UT8500CA-INPUT-MSG
229290         STRING "BR0001:(X200) PROBLEM WHILE ADDING TO BORV "
229300                 BORM-KEY
229310                DELIMITED BY SIZE INTO UT8500CA-INPUT-MSG
229320           CALL "UT8500" USING

229340                       UT8500CA-AREA
229350         GO TO X299-EXIT.
229360*
ST0125*** IF A FINANCIAL TXN CHANGED ACCOUNT BALANCE, THEN CALL MI0095 T
229380*** POPULATE REFRESH TXN TABLE.
229390*
229400     IF WA-EDIT-MASK-BIT (23) = 1
229410     OR NOT BORV-FINANCIAL-TRANSACTION
229420        GO TO X299-EXIT.
ST0125     MOVE 1   TO MI0095CA-CALL-CODE.
ST0125     MOVE 0   TO MI0095CA-FUNC.
ST0125     MOVE "L" TO MI0095CA-SYSTEM.
LON546*ST0125     PERFORM U000-CALL-MI0095.
LON546     PERFORM U000-CALL-MI0095 THRU U099-EXIT.
229470*
229480 X299-EXIT.
229490     EXIT.
ST0170/
231340 X700-READ-CUSVDD.
231350*
231360     MOVE CUSM-PRIM-ACCT     TO WA-CUST-NO.
231370     MOVE STRT-AREA-SOC-NO   TO WA-SOC-NO.
231380     MOVE 61                 TO DBIO-CALL-FUNC.
231390     MOVE WA-DUMMY-KEY       TO DBIO-CALL-KEY-AREA.
231400     MOVE ZERO               TO DBIO-CALL-REFER-AREA.
231410     MOVE SPACES TO DBIO-CALL-STATUS.
231420     MOVE "CUSM" TO DBIO-CALL-N-SYS.
231430     MOVE "CUSV" TO DBIO-CALL-SYST-VAR.
231440     MOVE "DD"               TO DBIO-CALL-CODE-B.
231450     CALL "DBIO" USING
231460
231470                 DBIO-CALL-PARAM
231480                 DBIO-CALL-SYS-AREA
231490                 DBIO-CALL-KEY-AREA
231500                 DBIO-CALL-STATUS
231510                 CUSV-DDRECORD-AREA
231520                 DBIO-CALL-REFER-AREA
231530                 DBIO-CALL-TXN-TYPE-AREA.
231540*
231550     IF DBIO-CALL-SAVE-REFER = "END."
231560        MOVE 1    TO ST-STATUS-LEVEL-1
231570        MOVE 155  TO BR0001-OUTPUT-ERR-MSG-NO
231580        PERFORM Z000-FORMT-SEND-PARA THRU Z099-EXIT
231590     ELSE
231600     IF DBIO-CALL-STATUS NOT = "****"
231610          MOVE 1    TO ST-STATUS-LEVEL-1
231620          MOVE 155  TO BR0001-OUTPUT-ERR-MSG-NO
231630          MOVE UT8500CA-FUNC-NORMAL-DISPLAY TO UT8500CA-CALL-CODE
231640          MOVE SPACES TO UT8500CA-INPUT-MSG
231650          STRING "BR0001:(X700) CUSVDD READ ERROR "
231660                 ", STAT= " DBIO-CALL-STATUS
231670                 DELIMITED BY SIZE INTO UT8500CA-INPUT-MSG
231680          CALL "UT8500" USING UT8500CA-AREA
231690          PERFORM Z000-FORMT-SEND-PARA THRU Z099-EXIT
231700     END-IF
231710     END-IF.
231720*
231730 X799-EXIT.
231740      EXIT.
231750/
235140 Z000-FORMT-SEND-PARA.
235150*
235160***  CALL FORMATTER TO SEND MESSAGES AND/OR DATA TO FRONTEND
235170*
235180*    GET EDIT MASK FOR TRACKING INTERFACE
235190*
235200     MOVE STRT-AREA-PROCESS-BITS  TO WA-UBITES-BITS.
235210     CALL "UT0022"                USING WA-UBITES-AREA.
235220*
235230******************************************************************
235240*
235270*    ULTIMATELY SHOULD BE INCLUDED IN ALL MODULES IMMEDIATELY
235280*    BEFORE CALLS TO TFT0000
235290*
235300*    NOTE THAT THIS CODE PRESUPPOSES THE AVAILABILITY OF THE EDIT
235310*    MASK IN THE CALLING MODULE. IF A MODULE DOES NOT ALREADY USE
235320*    THE EDIT MASK, CODE MUST BE INCLUDED TO MAKE IT AVAILABLE
235330*
235340     IF SOC-LOANS-TRACKING-OPTION = "Y" AND
235350        WA-EDIT-MASK-BIT (22) = 1
235360         CALL "BR0013" USING

235380                       STARTAREA
LON527        IF ST-STATUS-LEVEL-1 NOT = 0
LON527           MOVE 1 TO WA-MSG-ERR-FLAG
LON527           GO     TO Z099-EXIT
LON527        END-IF
235390     ELSE
235400        IF STRT-AREA-BATCH-TANDEM = "T"
235410           GO TO Z099-EXIT
235420     ELSE
235430     CALL "FT0000" USING

235450                   STARTAREA.
LON236*
235460 Z099-EXIT.
235470     EXIT.
235480/
235490 A200-TODAY-DATE.
235500*
235510         CALL "UT1999" USING

235530                             UT1999CA-CALL-AREA
235540         MOVE UT1999CA-DD  TO UT0030-DATE-DD
235550         MOVE UT1999CA-MM  TO UT0030-DATE-MM
235560         MOVE UT1999CA-CC  TO UT0030-DATE-CC
235570         MOVE UT1999CA-YY  TO UT0030-DATE-YY
235580         MOVE 1            TO UT0030-FUNCTION
235590         PERFORM Y020-CALL-UT0030 THRU Y029-EXIT
235600         MOVE UT0030-BINARY-DATE TO WA-TODAY-DATE.
235610 A209-EXIT.
235620      EXIT.
240370*
240380 UT8500-CALL.
240390*
240400             CALL "UT8500" USING

240420                           UT8500CA-AREA.
240430 UT8500-EXIT.
240440        EXIT.
LON236/
246360 Z2300-CALL-BLDVWW.
246370     CALL "DBIO" USING

246390                       DBIO-CALL-PARAM
246400                       DBIO-CALL-SYS-AREA
246410                       DBIO-CALL-KEY-AREA
246420                       DBIO-CALL-STATUS
246430                       BLDV-RECORD
246440                       DBIO-CALL-REFER-AREA
246450                       DBIO-CALL-TXN-TYPE-AREA.
246460 Z2399-EXIT.
246470     EXIT.
247500/
247510 X750-SEARCH-BLDVWW.
LON236*
247520     IF BORP-FLEXI-CAFM-IND = 0
247530        MOVE BORP-CAFM-TYPE TO WA-CAFM-TYPE
247540     ELSE
247550     IF BORM-CAFM-TYPE > 0
247560        MOVE BORM-CAFM-TYPE TO WA-CAFM-TYPE
247570     ELSE
247580        MOVE BORP-CAFM-TYPE TO WA-CAFM-TYPE.
247590     MOVE WA-IN-PROGRESS  TO  WA-FOUND-IND.
247600     IF NO-CAP-OR-COLLARS
247610        NEXT SENTENCE
247620     ELSE
247630        MOVE ZERO              TO DBIO-CALL-REFER-AREA
247640        MOVE BORM-KEY          TO DBIO-CALL-KEY-AREA
247650        MOVE 61                TO DBIO-CALL-FUNC
247660        MOVE "BORM"            TO DBIO-CALL-N-SYS
247670        MOVE "BLDV"            TO DBIO-CALL-SYST-VAR
247680        MOVE "WW"              TO DBIO-CALL-TXN-TYPE-AREA
247690        MOVE SPACES            TO DBIO-CALL-STATUS
247700        PERFORM Z1000-RETRIEVE-BLDVWW THRU Z1099-EXIT
247710        UNTIL NOT NEW-RATE-IN-PROGRESS
247720        IF NEW-RATE-NOT-FOUND
247730           NEXT SENTENCE
247740        ELSE
247750           PERFORM Z1100-SET-EFF-RATE THRU Z1199-EXIT
LON236        END-IF.
LON236*
247760 X759-EXIT.
247770     EXIT.
247780/
247790 Z1000-RETRIEVE-BLDVWW.
LON236*
247800     PERFORM Z2300-CALL-BLDVWW  THRU  Z2399-EXIT.
247810     IF DBIO-CALL-SAVE-REFER = "END."
247820           MOVE WA-NEW-RATE-NOT-FOUND  TO  WA-FOUND-IND
247830           GO TO Z1099-EXIT.
247840     IF DBIO-CALL-STATUS = "****"
247850        IF BORM-SOC-NO NOT = BLDV-WW-SOCIETY OR
247860           BORM-MEMB-CUST-AC NOT = BLDV-WW-MEMB-NO
247870           MOVE WA-NEW-RATE-NOT-FOUND  TO  WA-FOUND-IND
247880        ELSE
247890         IF WA-SEARCH-RATE-DATE NOT <
247900                                BLDV-WW-CAFM-START-RTE-DTE
247910            AND WA-SEARCH-RATE-DATE <
247920                                BLDV-WW-CAFM-RESET-RTE-DTE
247930            MOVE WA-NEW-RATE-FOUND  TO  WA-FOUND-IND
247940         ELSE
247950            NEXT SENTENCE
247960     ELSE
247970     IF DBIO-CALL-STATUS NOT = "END."
247980         STRING  "BR0000:(Z100) ERROR IN BLDV-WW READ "
247990                 "ACCOUNT NUMBER  "
248000                 DBIO-CALL-KEY
248010                 "RETURN STATUS  "
248020                 DBIO-CALL-STATUS
248030                 " ; DEFAULTED TO VARIABLE RATE."
248040           DELIMITED BY SIZE INTO UT8500CA-INPUT-MSG
248050        CALL "UT8500" USING

248070                       UT8500CA-AREA
248080         MOVE WA-NEW-RATE-NOT-FOUND  TO  WA-FOUND-IND
248090     ELSE
248100         MOVE WA-NEW-RATE-NOT-FOUND  TO  WA-FOUND-IND.
248110*
248120 Z1099-EXIT.
248130     EXIT.
248140/
248150 Z1100-SET-EFF-RATE.
248160     MOVE BLDV-WW-CAFM-TYPE TO WA-CAFM-TYPE.
248170     IF CAP-IN-FORCE
248180        IF WA-INT-RATE-A > BLDV-WW-CAFM-HIGH-RTE
248190           MOVE BLDV-WW-CAFM-HIGH-RTE  TO   WA-INT-RATE-A.
248200     IF FLOOR-IN-FORCE
248210        IF WA-INT-RATE-A < BLDV-WW-CAFM-LOW-RTE
248220           MOVE BLDV-WW-CAFM-LOW-RTE  TO    WA-INT-RATE-A.
248230     IF FIXED-IN-FORCE
248240        MOVE BLDV-WW-CAFM-HIGH-RTE    TO    WA-INT-RATE-A.
248250*
248260 Z1199-EXIT.
248270     EXIT.
249360/
254980 MANUAL-BAD-DEBT-AMEND.
254990*
255100     MOVE 0 TO WA-BOR-FIELD-CHANGE.
LON120     IF BR0001-BAD-DEBT-JRNL-NO NOT = 0
LON120        MOVE WA-NEXT-BAD-DEBT-IND TO WA-BAD-DBT-IND
LON120        MOVE WA-PREV-BAD-DEBT-IND TO WA-BAD-DEBT-INDICATOR
LON120     ELSE
SC3112        MOVE BORM-BAD-DEBT-IND TO WA-BAD-DBT-IND
SC8804        MOVE BR0001-BAD-DEBT-IND  TO WA-BAD-DEBT-INDICATOR
LON120     END-IF.
S10093     IF STRT-AREA-TRN-NO = 17053 OR 17003
S10093        IF BR0001-17053-CONTRACT
S10093           MOVE BORM-MEMB-CUST-AC    TO WA-BAD-DEBT-ACCT-NO
S10093        END-IF
S10093     ELSE
SC8804        MOVE BR0001-BAD-DEBT-ACCT-NO TO WA-BAD-DEBT-ACCT-NO
S10093     END-IF.
LON327*S10093     DISPLAY 'BOIS-PRIORITY-CODE:' BOIS-PRIORITY-CODE.
LON327*S10093     DISPLAY 'BORP-ACCT-TYPE:' BORP-ACCT-TYPE.
S13064*255220     IF BORP-PRIORITY-TERM-BY-TERM
S13064*S13252     IF BOIS-PRIORITY-TERM-BY-TERM
255230     PERFORM M100-EDIT-INDICATOR
255240        THRU M199-EXIT.
255250     IF WA-MSG-ERR-FLAG = 1
255260        GO TO MANUAL-BAD-DEBT-AMEND-EXIT
255270     END-IF.
LON135     INITIALIZE WA-WORK-AMOUNT-1.
LON135     INITIALIZE WA-WORK-AMOUNT-2.
LON135     INITIALIZE WA-WORK-AMOUNT-3.
LON120     MOVE "I" TO  BOIS-BUCKET-CDE(1).
LON120     IF BORM-INT-ACCR > 0
LON135        MOVE BORM-INT-ACCR TO WA-WORK-AMOUNT-3
LON135        PERFORM N900-ROUND-CURRENCY THRU N999-EXIT
LON135        MOVE WA-WORK-AMOUNT-3 TO BOIS-BUCKET-DUE(1)
LON120     END-IF
LON120     MOVE "W" TO BOIS-BUCKET-CDE(2)
LON120     IF BORM-ARR-INT-ACCR > 0
LON135        MOVE BORM-ARR-INT-ACCR TO WA-WORK-AMOUNT-3
LON135        PERFORM N900-ROUND-CURRENCY THRU N999-EXIT
LON120        MOVE WA-WORK-AMOUNT-3 TO BOIS-BUCKET-DUE(2)
LON120     END-IF
LON120     MOVE "A" TO BOIS-BUCKET-CDE(3)
LON120     IF BOIS-FINE2-INT-ACCR > 0
LON135        MOVE BOIS-FINE2-INT-ACCR TO WA-WORK-AMOUNT-3
LON135        PERFORM N900-ROUND-CURRENCY THRU N999-EXIT
LON120        MOVE WA-WORK-AMOUNT-3    TO BOIS-BUCKET-DUE(3)
LON120     END-IF
LON120     MOVE "E" TO BOIS-BUCKET-CDE(4)
LON120     IF BOIS-FINE4-INT-ACCR > 0
LON135        MOVE BOIS-FINE4-INT-ACCR TO WA-WORK-AMOUNT-3
LON135        PERFORM N900-ROUND-CURRENCY THRU N999-EXIT
LON120        MOVE WA-WORK-AMOUNT-3    TO BOIS-BUCKET-DUE(4)
LON120     END-IF
SC1456     IF ( BR0001-BAD-DEBT-IND = 54 AND
SC1456          VALID-IND-BEFORE-PAR-BAD AND
LON120          BR0001-BAD-DEBT-JRNL-NO = 0)
LON120        OR (BR0001-BAD-DEBT-JRNL-NO NOT = 0  AND
LON120           WA-PREV-BAD-DEBT-IND = (52 OR 02) AND
LON120           WA-NEXT-BAD-DEBT-IND = 54 )
SC1456          IF ((WA-BAD-DBT-IND = (02 OR 52 OR 53) AND
LON120             BR0001-BAD-DEBT-JRNL-NO = 0) OR
LON120             (BR0001-BAD-DEBT-JRNL-NO NOT = 0 AND
LON120             WA-NEXT-BAD-DEBT-IND = 54 AND
LON120             WA-PREV-BAD-DEBT-IND = (52 OR 02)))
S12787             PERFORM C6450-BILL-BUCKET THRU C6450-EXIT
LON387             IF WA-MSG-ERR-FLAG = 1
LON387                GO TO MANUAL-BAD-DEBT-AMEND-EXIT
LON387             END-IF
SC6108             MOVE BORM-BR-NO       TO UT0099CA-BRANCH
LON120             IF BR0001-BAD-DEBT-JRNL-NO NOT = 0
LON120                MOVE BORP-NON-CGL-COMPONENT-1 TO
LON120                     UT0099CA-COMPONENT-1
LON120             ELSE
SC5018                MOVE BORP-BAD-D-CGL-1
SC5018                              TO UT0099CA-COMPONENT-1
LON120             END-IF
LON120             IF BR0001-BAD-DEBT-JRNL-NO NOT = 0
LON120                MOVE BORP-NON-CGL-COMPONENT-2 TO
LON120                     UT0099CA-COMPONENT-2
LON120             ELSE
SC5018                MOVE BORP-BAD-D-CGL-2
SC5018                              TO UT0099CA-COMPONENT-2
LON120             END-IF
SC5018             MOVE CUSM-SEGMENT-CODE TO UT0099CA-SEGMENT
SC5018             MOVE CUSM-GROUP-CODE   TO UT0099CA-GROUP-CODE
SC5018             MOVE BORP-CURRENCY-IND
SC5018                              TO UT0099CA-CURRENCY
SC5018             MOVE SOC-GL-CLASS-CODE-STRUCTURE TO
SC5018                              UT0099CA-GLCC-STRUCTURE
SC5018             CALL "UT0099" USING UT0099CA-AREA
SC5018             IF UT0099CA-RETURN-STATUS NOT = 0
SC5018                MOVE 2083  TO BR0001-OUTPUT-ERR-MSG-NO
SC5018                MOVE 1 TO WA-MSG-ERR-FLAG
SC5018                GO TO MANUAL-BAD-DEBT-AMEND-EXIT
SC5018             END-IF
SC5018             MOVE UT0099CA-GLCC-OUTPUT-KEY TO
SC5018                  BORM-GL-CLASSIFICATION-CODE
SC1456             IF BR0001-BAD-DEBT-AMOUNT = 0
LON120                IF BR0001-BAD-DEBT-JRNL-NO = 0
SC1456                   INITIALIZE UT0073CA-CALL-PARAMETER
SC3318* transfer customer segment in BAD DEBT
SC1456                   MOVE 0 TO WA-ALL-DEBT-AMOUNT
SC3318                   MOVE 0 TO WA-BAD-DBT-IDX
SC5100                   MOVE "Y" TO WA-53-54-FLAG
S11908                   ADD  WA-INT-TOTAL
S11908                        TO BOIS-TOT-BAD-DEBT-AMOUNT
S11908                           WA-SAV-PREV-BAD-DEBT-AMT
SC5232                   MOVE BOIS-BAD-DEBT-TRF-DATE TO
SC5232                           WA-SAV-PREV-BAD-DEBT-TRF-DT
SC1456                   MOVE WA-TODAY-DATE TO BOIS-BAD-DEBT-TRF-DATE
LON120                END-IF
SC1456             ELSE
SC1456                MOVE 1       TO WA-MSG-ERR-FLAG
SC1456                MOVE 2897    TO BR0001-OUTPUT-ERR-MSG-NO
SC1456                GO           TO MANUAL-BAD-DEBT-AMEND-EXIT
SC1456             END-IF
SC1456          END-IF.
S12787     IF WA-MSG-ERR-FLAG = 1
S12787        GO  TO MANUAL-BAD-DEBT-AMEND-EXIT
S12787     END-IF.
TB0105     MOVE BORM-CUSTOMER-NO    TO WA-CUST-NO.
TB0105     MOVE STRT-AREA-SOC-NO    TO WA-SOC-NO.
TB0105     MOVE 71                  TO DBIO-CALL-FUNC.
TB0105     PERFORM M500-ACCESS-CUSM-RECORD THRU M500-EXIT.
TB0105     IF ST-STATUS-LEVEL-1 NOT = 0
TB0105         GO      TO MANUAL-BAD-DEBT-AMEND-EXIT
TB0105     END-IF.
TB0105     PERFORM X700-READ-CUSVDD THRU X799-EXIT.
TB0105     IF ST-STATUS-LEVEL-1 NOT = 0
TB0105         GO      TO MANUAL-BAD-DEBT-AMEND-EXIT
TB0105     END-IF.

258540     IF BR0001-BAD-DEBT-REASON-CODE = WA-ZERO
S12173        CONTINUE
258580     ELSE
258590        INITIALIZE RSNC-DATA-KEY
258600        MOVE STRT-AREA-SOC-NO            TO RSNC-INST-NO
258610        MOVE BR0001-BAD-DEBT-REASON-CODE TO RSNC-REASON-CODE

258630        PERFORM GET-REASON-CODE
258640           THRU GET-REASON-CODE-EXIT

S13681        IF WA-MSG-ERR-FLAG NOT = 0
258680           MOVE 1    TO WA-MSG-ERR-FLAG
258690           MOVE 4274 TO BR0001-OUTPUT-ERR-MSG-NO
258700           GO        TO MANUAL-BAD-DEBT-AMEND-EXIT
TB0211        ELSE
TB0211           MOVE RSNC-REASON-DESC TO BR0001-OUT-BAD-NARRATIVE
258730        END-IF
258740     END-IF.
TB0151************************************************************
TB0105* IN BAD DEBT 51
TB0105
LON120   IF BR0001-BAD-DEBT-JRNL-NO NOT = 0
LON120      CONTINUE
LON120   ELSE
TB0105      IF BR0001-BAD-DEBT-IND NOT =  WA-BAD-DBT-IND
S12173         IF BR0001-BAD-DEBT-REASON-CODE NOT EQUAL WA-ZERO
TB0105            INITIALIZE UT0073CA-CALL-PARAMETER
TB0105            MOVE BORM-CUSTOMER-NO    TO WA-CUST-NO
TB0105            MOVE BR0001-BAD-DEBT-IND TO
TB0105                 UT0073CA-BAD-DEBT-CHANGE
TB0105            MOVE STRT-AREA-SOC-NO    TO WA-SOC-NO
TB0105                                     UT0073CA-SOC-NO
TB0105            MOVE 08 TO UT0073CA-CALL-CODE
TB0105            MOVE WA-TODAY-DATE   TO UT0073CA-EFF-DATE
TB0105            MOVE BR0001-BAD-DEBT-R2 TO
TB0105                                   UT0073CA-REASON-CODE
TB0105            CALL "UT0073" USING UT0073CA-CALL-AREA
TB0105                          BORM-RECORD-AREA
TB0105                          BOIS-RECORD-AREA
TB0105                          BORR-PARAMS
TB0105                          SOC-PARAMS
SC1413                          BORV-RECORD-AREA
TB0105                          CUSV-DDRECORD-AREA
SC1413                          STARTAREA
TB0105         IF UT0073CA-RET-STATUS > 0
TB0105            MOVE 1 TO WA-MSG-ERR-FLAG
TB0105            MOVE 1    TO ST-STATUS-LEVEL-1
TB0105            MOVE UT0073CA-RET-ERROR-MSG TO
TB0105                         BR0001-OUTPUT-ERR-MSG-NO
TB0105            GO           TO MANUAL-BAD-DEBT-AMEND-EXIT
TB0105         END-IF
SC1413         MOVE BORM-CUSTOMER-NO    TO WA-CUST-NO
SC1413         MOVE STRT-AREA-SOC-NO    TO WA-SOC-NO
SC1413         MOVE 71                  TO DBIO-CALL-FUNC
SC1413         PERFORM M500-ACCESS-CUSM-RECORD THRU M500-EXIT
SC1413         IF ST-STATUS-LEVEL-1 NOT = 0
SC1413            GO      TO MANUAL-BAD-DEBT-AMEND-EXIT
SC1413         END-IF
SC1413         IF CUSM-REJECTED-STATUS NOT = 1
SC1413            SET REJECTED-CUSTOMER TO TRUE
S10673            MOVE STRT-AREA-BRANCH    TO CUSM-LAST-MAINT-BRNO
S10673            MOVE STRT-AREA-TELL-NO   TO CUSM-LAST-MAINT-TELL
S10673            MOVE STRT-AREA-POST-DT   TO CUSM-LAST-MAINT-DATE
SC1413            MOVE 74                  TO DBIO-CALL-FUNC
SC1413            PERFORM M500-ACCESS-CUSM-RECORD THRU M500-EXIT
SC1413         END-IF
LON120       END-IF
S12173     END-IF
TB0105    END-IF.
TB0151************************************************************
259540    MOVE STRT-AREA-TRN-NO TO WA-NEW-TRAN.
TB0161    MOVE "N"   TO  WA-FLAG-CORR
TB0211*--- CORRECTION FROM BAD DEBT TO PREVIOUS INDICATOR

263090     INITIALIZE BORV-48-DETAIL
LON120     IF BR0001-BAD-DEBT-JRNL-NO NOT = ZEROES
LON120        MOVE WA-NEXT-BAD-DEBT-IND TO BORV-48-PREV-STATUS
LON120                                     WA-BAD-DEBT-IND-BEF
LON120     ELSE
263100        MOVE BORM-BAD-DEBT-IND   TO BORV-48-PREV-STATUS
ST0094                                 WA-BAD-DEBT-IND-BEF
LON120     END-IF
LON120     IF BR0001-BAD-DEBT-JRNL-NO NOT = ZEROES
LON120        MOVE WA-PREV-BAD-DEBT-IND TO BORV-48-NEW-STATUS
LON120                                     WA-BAD-DEBT-IND-AFT
LON120     ELSE
263120        MOVE BR0001-BAD-DEBT-IND  TO BORV-48-NEW-STATUS
ST0094                                     WA-BAD-DEBT-IND-AFT
LON120     END-IF
263140     MOVE BORM-RECORD-AREA   TO WA-BORM-OLD-IMAGE.
263150     MOVE WA-BORM-OLD-IMAGE  TO WA-AUDT-OLD-IMAGE.
265630*** SAVE ORIGINAL VALUE TO OUTPUT BUFFER, FOR JOURNALIZING THE
265640*** BALANCE TRANSFER IF NEEDED
265650     MOVE BORM-BAD-DEBT-IND   TO BR0001-OUT-BAD-DEBT-IND
LON077     IF BR0001-BAD-DEBT-IND NOT = BORM-BAD-DEBT-IND OR
LON120        (BR0001-BAD-DEBT-JRNL-NO NOT = ZEROES AND
LON120         WA-PREV-BAD-DEBT-IND NOT = WA-NEXT-BAD-DEBT-IND)
LON077         MOVE 1 TO WA-BOR-FIELD-CHANGE
LON077     END-IF
LON120     IF BR0001-BAD-DEBT-JRNL-NO > 0
LON120        MOVE WA-PREV-BAD-DEBT-IND TO BORM-BAD-DEBT-IND
LON189        IF BR0001-BAD-DEBT-CONTRACT
LON189           MOVE WA-PREV-BAD-DEBT-IND TO CONB-BAD-DEBT-IND
LON189           PERFORM B6100-UPDATE-CONB THRU B6199-EXIT
LON189           IF WA-MSG-ERR-FLAG = 1
LON189              GO TO MANUAL-BAD-DEBT-AMEND-EXIT
LON189           END-IF
LON189        END-IF
LON120     ELSE
265660        MOVE BR0001-BAD-DEBT-IND TO BORM-BAD-DEBT-IND
LON120     END-IF
SC9713*** Need to zero out deffered schedule when transferring to normal
LON189**SC9713     IF BORV-48-PREV-STATUS = 52
LON189**SC9713        AND BORM-BAD-DEBT-IND = 50
LON189**SC9713        AND WA-FLAG-CORR NOT = "Y"
LON189**SC9713          MOVE 0                TO BOIS-DEF-INT-STRT-DTE
LON189**SC9713                                   BOIS-DEF-INT-END-DTE
LON189**SC9713                                   BOIS-DEF-PRIN-END-DTE
LON189**SC9713                                   BOIS-DEF-TOT-AMT
LON189**SC9713                                   BOIS-DEF-BUCKET-AMT
LON189**SC9713                                   BOIS-DEF-AMORT-TERM
LON189**SC9713     END-IF

S13681     IF BR0001-BAD-DEBT-REASON-CODE
S13681        NOT = BOIS-BAD-DEBT-REASON-CODE
265980           MOVE BOIS-BAD-DEBT-REASON-CODE TO
265990                BR0001-OUT-BAD-REASON-CODE

266010           MOVE BR0001-BAD-DEBT-REASON-CODE TO
266020                BOIS-BAD-DEBT-REASON-CODE

266040           MOVE 1 TO WA-BOR-FIELD-CHANGE
266060     END-IF.

SC1448     MOVE 0                       TO WA-BR0001-BAD-DEBT-DATE.
SC1448     IF  BR0001-BAD-DEBT-DATE NOT = ZEROS
SC1448         MOVE BR0001-BAD-DEBT-DATE   TO UT0030-DATE-DDMMCCYY
SC1448         MOVE 1                   TO UT0030-FUNCTION
SC1448         CALL "UT0030" USING  UT0030-FUNCTION
SC1448                              UT0030-DATE-AREA
SC1448                              UT0030-BINARY-DATE
SC1448         IF (UT0030-BINARY-DATE     = ZEROES)
SC1448            MOVE 1          TO WA-MSG-ERR-FLAG
SC1448            MOVE 126        TO BR0001-OUTPUT-ERR-MSG-NO
SC1441            GO TO MANUAL-BAD-DEBT-AMEND-EXIT
SC1448         END-IF
SC1448         MOVE UT0030-BINARY-DATE  TO WA-BR0001-BAD-DEBT-DATE
SC1448     END-IF.
LON120     IF  BR0001-BAD-DEBT-JRNL-NO NOT = ZEROES
LON120         AND (WA-PREV-BAD-DEBT-IND = (52 OR 02) AND
LON120              WA-NEXT-BAD-DEBT-IND = (50 OR 00))
LON120              MOVE BORP-NON-CGL-COMPONENT-1
LON120                   TO WA-NPL-COMPONENT-1
LON120              MOVE BORP-NON-CGL-COMPONENT-2
LON120                   TO WA-NPL-COMPONENT-2
LON120              PERFORM GET-CGL-CODE-NPL THRU
LON120              GET-CGL-CODE-NPL-EXIT
LON120              MOVE UT0099CA-GLCC-OUTPUT-KEY TO
LON120              BORM-GL-CLASSIFICATION-CODE
LON120     END-IF.
SC1456     IF  ((WA-BR0001-BAD-DEBT-DATE = ZEROES
SC1456     OR  WA-BR0001-BAD-DEBT-DATE = WA-TODAY-DATE)
LON120     AND BR0001-BAD-DEBT-JRNL-NO = ZEROES)
SC1456         MOVE ZEROES TO BORM-POOL-DEPOSIT-DATE
S11761         IF (BR0001-BAD-DEBT-IND  = 52 OR 02)
FG1580              AND ( BORV-48-PREV-STATUS NOT = 53 AND 03 )
SC1456            IF WA-FLAG-CORR NOT = "Y"
SC1456               INITIALIZE UT0073CA-CALL-PARAMETER
SC8397               MOVE "N"     TO UT0073CA-BATCH-FLAG
SC1456               MOVE 01 TO UT0073CA-CALL-CODE
SC1456               MOVE BR0001-BAD-DEBT-REASON-CODE
SC1456                               TO UT0073CA-REASON-CODE
SC1456               MOVE WA-TODAY-DATE   TO UT0073CA-EFF-DATE
SC1456               CALL "UT0073" USING UT0073CA-CALL-AREA
SC1456                      BORM-RECORD-AREA
SC1456                      BOIS-RECORD-AREA
SC1456                      BORR-PARAMS
SC1456                      SOC-PARAMS
SC1456                      BORV-RECORD-AREA
SC1456                      CUSV-DDRECORD-AREA
SC1456                      STARTAREA
LON527               IF UT0073CA-RET-STATUS NOT = 0
LON527                  MOVE 1 TO WA-MSG-ERR-FLAG
LON527                  MOVE UT0073CA-RET-ERROR-MSG TO
LON527                            BR0001-OUTPUT-ERR-MSG-NO
LON527                  GO     TO MANUAL-BAD-DEBT-AMEND-EXIT
LON527               END-IF
SC5843               IF BOIS-921-FLAG > 0
SC5843                  MOVE BORP-921-L-CGL-1 TO
SC5843                     WA-NPL-COMPONENT-1
SC5843                  MOVE BORP-921-L-CGL-2 TO
SC5843                     WA-NPL-COMPONENT-2
SC5843               ELSE
SC2263                  MOVE BORP-NON-CGL-COMPONENT-1
SC2263                                     TO WA-NPL-COMPONENT-1
SC2263                  MOVE BORP-NON-CGL-COMPONENT-2
SC2263                                     TO WA-NPL-COMPONENT-2
SC5843               END-IF
SC5843               PERFORM GET-CGL-CODE-NPL THRU
SC5843                          GET-CGL-CODE-NPL-EXIT
SC2263               MOVE UT0099CA-GLCC-OUTPUT-KEY TO
SC2263                         BORM-GL-CLASSIFICATION-CODE

SC1456               MOVE UT0073CA-REGULAR-INT-DUE TO
SC1456                         BOIS-GL-TRNSF-ORIG-AMT-I
SC1456               MOVE UT0073CA-PRINCIPAL-DUE   TO
SC1456                         BOIS-GL-TRNSF-ORIG-AMT-P
SC1456            END-IF
ST0094            MOVE BORM-INT-ADJUSTMENT TO WA-NPL-INT-ADJ
ST0094            MOVE BORM-STORE-RATE     TO WA-NPL-STOR-RT
266880                                        WA-RATE-NPL
SC8308            MOVE BORM-INT-RATE       TO WA-NPL-INT-RATE
ST0094            MOVE BORM-INT-RATE       TO WA-NPL-INT-RT
SC7560            MOVE BOIS-NEGO-RATE-IND  TO WA-NEGO-RATE-IND
SC7560            MOVE BOIS-NEGO-RATE-FLAG TO WA-NEGO-RATE-FLAG
SC7560            MOVE BOIS-EFR-CHNG-IND   TO WA-EFR-CHNG-IND
SC7560            MOVE BORM-DUES-RECALC-IND TO WA-DUES-RECALC-IND
ST0094            MOVE BORM-RT-INCR        TO WA-RATE-INCR
SC7850            MOVE STRT-AREA-JRNL-NO      TO BOIS-NPL-JRNL-NO
SC1456            MOVE 1 TO WA-BOR-FIELD-CHANGE
SC1456            GO TO MANUAL-CONTINUE
SC1456     END-IF.
SC1456
S14066     IF BR0001-BAD-DEBT-IND  = 54
LON327*S14066        DISPLAY "SUCCESS 1"
LON120        IF BR0001-BAD-DEBT-JRNL-NO NOT = ZEROES
LON120           MOVE ZEROES TO BOIS-GL-TRNSF-ORIG-AMT-I
LON120                          BOIS-GL-TRNSF-ORIG-AMT-P
LON120        ELSE
S14066           COMPUTE BOIS-GL-TRNSF-ORIG-AMT-I ROUNDED =
S14066                 (BORM-CAP-UNPAID-INT +
S14066                 BOIS-UNPAID-ARRS-PRIN-BAL +
S14066                 BOIS-UNPAID-INT-ARRS-PRIN-BAL +
S14066                 BORM-UNPAID-ARRS-INT-BAL)
S14066                 MOVE BOIS-GL-TRNSF-ORIG-AMT-I TO WA-TMP-AMT-DI
LON327*S14066                 DISPLAY "AMT I 1 = " WA-TMP-AMT-DI
S14066                 COMPUTE BOIS-GL-TRNSF-ORIG-AMT-P ROUNDED =
S14066                 BORM-THEO-LOAN-BAL + BORM-UNPAID-PRINCIPAL-BAL
S14066                 MOVE BOIS-GL-TRNSF-ORIG-AMT-P TO WA-TMP-AMT-DI
LON327*S14066                 DISPLAY "AMT P 1 = " WA-TMP-AMT-DI
LON120        END-IF
S14066        MOVE 1 TO WA-BOR-FIELD-CHANGE
S14066     END-IF.
268740     MOVE 1                    TO UT0030-FUNCTION.
268750     MOVE BR0001-BAD-DEBT-DATE TO UT0030-DATE-DDMMCCYY.
268760     PERFORM Y020-CALL-UT0030 THRU Y029-EXIT
268840     IF  UT0030-BINARY-DATE NOT = BORM-POOL-DEPOSIT-DATE
268850         MOVE UT0030-BINARY-DATE TO BORM-POOL-DEPOSIT-DATE
268860         MOVE 1 TO WA-BOR-FIELD-CHANGE
268870     END-IF.

268890 MANUAL-CONTINUE.
268900*
LON327*S14066     DISPLAY "FIELD CHANGE IND = " WA-BOR-FIELD-CHANGE.
269000     IF WA-BOR-FIELD-CHANGE = 1
269010        PERFORM W500-AMEND-BORM
269020           THRU W599-EXIT

269040        MOVE "REWRITE" TO DBIOMOD-FUNCTION
269050        PERFORM W3100-ACCESS-BOIS
269060           THRU W3199-EXIT

269080        IF WA-MSG-ERR-FLAG = 1
269090           GO TO MANUAL-BAD-DEBT-AMEND-EXIT
269100        END-IF
269110        MOVE BORM-RECORD-AREA TO WA-AUDT-NEW-IMAGE
269120        INITIALIZE WA-AUDT-OLD-IMAGE
269130        PERFORM S000-WRITE-BORM-AUDIT-REC
269140           THRU S099-EXIT

269160        MOVE BOIS-RECORD-AREA TO WA-AUDT-NEW-IMAGE
269170        INITIALIZE WA-AUDT-OLD-IMAGE
269180        PERFORM S000A-WRITE-BOIS-AUDIT-REC
269190           THRU S099A-EXIT

S13064*269210        IF BORP-PRIORITY-TERM-BY-TERM
S13801*S13064        IF BOIS-PRIORITY-TERM-BY-TERM
269220           PERFORM M200-WRITE-BORV-TXN48
269230              THRU M299-EXIT
269240           IF WA-MSG-ERR-FLAG = 1
269250              GO TO MANUAL-BAD-DEBT-AMEND-EXIT
269260           END-IF
LON120           IF BR0001-BAD-DEBT-JRNL-NO NOT = ZEROES
LON120              CONTINUE
LON120           ELSE
FG1580              IF (BORV-48-NEW-STATUS  = 03 OR 53
S11659                                        OR 54 OR 04
SC4235                                        OR 50
SC8815                                        OR 59) OR
SC8133                 (WA-BAD-DEBT-IND-AFT = 50)
FG1580                 PERFORM M700-WRITE-BORV-TXN51
FG1580                    THRU M799-EXIT
FG1580                 IF WA-MSG-ERR-FLAG = 1
FG1580                    GO TO MANUAL-BAD-DEBT-AMEND-EXIT
FG1580                 END-IF
269380              END-IF
LON120           END-IF
269390     END-IF.

269410 MANUAL-SEND-OK-MSG.
269420*===================
S10093     IF UT0400CA-CALL-STATUS EQUAL "****"
S10093        GO TO MANUAL-BAD-DEBT-AMEND-EXIT
S10093     ELSE
269460        MOVE 0 TO BR0001-OUTPUT-ERR-MSG-NO
269470        PERFORM Z000-FORMT-SEND-PARA
269480            THRU Z099-EXIT
S10093     END-IF.

269510 MANUAL-BAD-DEBT-AMEND-EXIT.
269520*---------------------------
269530     EXIT.

269550 MANUAL-BAD-DEBT-ENQ.
269560*====================

269580     MOVE BR0001-INPUT-KEY-R TO UT0151CA-ACCT-IN.
269590     MOVE SOC-ACCT-NO-FORMAT TO UT0151CA-FORMAT.
269600     PERFORM S100-REFORMAT-ACCOUNT-NO
269610       THRU S199-EXIT.

269630     MOVE UT0151CA-ACCT-EX   TO BR0001-OUT-BAD-DEBT-ACCT-NO.
S10093     MOVE BR0001-17053-TYPE  TO BR0001-OUT-BAD-DEBT-TYPE.
S10093     IF  STRT-AREA-TRN-NO = 17053 AND BR0001-17053-CONTRACT
S10093         MOVE CONB-CUSTOMER-NO   TO CUSM-CUST-ACCT-NO
S10093     ELSE
269680         MOVE BR0001-INPUT-KEY-R TO CUSM-CUST-ACCT-NO
S10093     END-IF
269700     PERFORM F100-RETRIEVE-CUSM
269710         THRU F199-EXIT.

269730     IF WA-MSG-ERR-FLAG = 1
269740        GO TO MANUAL-BAD-DEBT-ENQ-EXIT
269750     END-IF.

269770     PERFORM F200-RETRIEVE-NAME-ADDR
269780        THRU F299-EXIT.

269800     IF WA-MSG-ERR-FLAG = 1
269810        GO TO MANUAL-BAD-DEBT-ENQ-EXIT
269820     END-IF.

269840     MOVE UT0500RA-FULL-NAME TO BR0001-OUT-BAD-DEBT-NAME.
S10093     IF BR0001-17053-ACCOUNT
269860        IF  BORM-BAD-DEBT-IND  IS NOT NUMERIC
269870            MOVE 0 TO BORM-BAD-DEBT-IND
269880        END-IF
S10093     ELSE
S10093*       IF  CONB-BAD-DEBT-IND  IS NOT NUMERIC
MVSWLW        MOVE CONB-BAD-DEBT-IND  TO WA-CONB-BAD-DEBT-IND
MVSWLW        IF  WA-CONB-BAD-DEBT-IND  IS NOT NUMERIC
S10093            MOVE 0 TO CONB-BAD-DEBT-IND
S10093        END-IF
S10093     END-IF.
S10093     IF BR0001-17053-ACCOUNT
TB0156        MOVE BOIS-BAD-DEBT-AMT TO BR0001-OUT-BAD-AMOUNT
TB0211        MOVE BORM-BAD-DEBT-IND TO WA-BAD-DBT-IND
TB0211        IF WA-BAD-DBT-IND = 53
TB0211           MOVE 0              TO BR0001-OUT-BAD-AMOUNT
TB0211        END-IF
270020        MOVE BORM-BAD-DEBT-IND TO BR0001-OUT-BAD-DEBT-IND
270030                               WA-BAD-DEBT-IND
S10093     ELSE
S10093        MOVE CONB-BAD-DEBT-IND TO WA-BAD-DBT-IND
S10093        IF WA-BAD-DBT-IND = 53
S10093           MOVE 0              TO BR0001-OUT-BAD-AMOUNT
S10093        END-IF
S10093        MOVE CONB-BAD-DEBT-IND TO BR0001-OUT-BAD-DEBT-IND
S10093                                  WA-BAD-DEBT-IND
S10093     END-IF.
270120     IF WA-BAD-DEBT-IND-MANUAL
270130        MOVE 7 TO WA-PTR
270140     ELSE
270150        MOVE 1 TO WA-PTR
270160     END-IF.
S10093     IF BR0001-17053-ACCOUNT
270180        IF BORM-BAD-DEBT-IND = 00
270190           MOVE SPACES TO BR0001-OUT-BAD-DEBT-DESC
270200        ELSE
270210           ADD WA-BAD-DEBT-IND-2 TO WA-PTR
270220           IF WA-BAD-DEBT-IND-2 = 9
270230              SUBTRACT 3 FROM WA-PTR
270240           END-IF
270250        MOVE WA-BAD-DEBT-ITEMS(WA-PTR) TO BR0001-OUT-BAD-DEBT-DESC
270260        END-IF
S10093     ELSE
S10093        IF CONB-BAD-DEBT-IND = 00
S10093           MOVE SPACES TO BR0001-OUT-BAD-DEBT-DESC
S10093        ELSE
S10093           ADD WA-BAD-DEBT-IND-2 TO WA-PTR
S10093           IF WA-BAD-DEBT-IND-2 = 9
S10093              SUBTRACT 3 FROM WA-PTR
S10093           END-IF
S10093           MOVE WA-BAD-DEBT-ITEMS(WA-PTR)
270360                                      TO BR0001-OUT-BAD-DEBT-DESC
S10093        END-IF
S10093     END-IF.
S10093     IF BR0001-17053-ACCOUNT
270400        MOVE BOIS-BAD-DEBT-REASON-CODE TO
270410             BR0001-OUT-BAD-REASON-CODE
S10093     ELSE
S10093        MOVE CONB-BAD-DEBT-REASON-CODE TO
S10093             BR0001-OUT-BAD-REASON-CODE
S10093     END-IF.
S10093     IF BR0001-17053-ACCOUNT
270470        IF BOIS-BAD-DEBT-REASON-CODE = WA-ZERO
270480           MOVE SPACES TO BR0001-OUT-BAD-NARRATIVE
270490        ELSE
270500           INITIALIZE RSNC-DATA-KEY
S12209           MOVE SPACES TO RSNC-REASON-DESC
270510           MOVE STRT-AREA-SOC-NO           TO RSNC-INST-NO
270520           MOVE BOIS-BAD-DEBT-REASON-CODE  TO RSNC-REASON-CODE

270540           PERFORM GET-REASON-CODE
270550              THRU GET-REASON-CODE-EXIT

S13681           IF WA-MSG-ERR-FLAG NOT = 0
270590              MOVE 1    TO WA-MSG-ERR-FLAG
270600              MOVE 4274 TO BR0001-OUTPUT-ERR-MSG-NO
270610              GO        TO MANUAL-BAD-DEBT-ENQ-EXIT
270620           ELSE
270630              MOVE RSNC-REASON-DESC TO BR0001-OUT-BAD-NARRATIVE
270640           END-IF
270650        END-IF
S10093     ELSE
S10093        IF CONB-BAD-DEBT-REASON-CODE = WA-ZERO
S10093           MOVE SPACES TO BR0001-OUT-BAD-NARRATIVE
S10093        ELSE
S10093           INITIALIZE RSNC-DATA-KEY
S12209           MOVE SPACES TO RSNC-REASON-DESC
S10093           MOVE STRT-AREA-SOC-NO           TO RSNC-INST-NO
S10093           MOVE CONB-BAD-DEBT-REASON-CODE  TO RSNC-REASON-CODE
S10093           PERFORM GET-REASON-CODE
S10093             THRU GET-REASON-CODE-EXIT
S10093           IF WA-MSG-ERR-FLAG NOT = 0
S10093              MOVE 1    TO WA-MSG-ERR-FLAG
S10093              MOVE 4274 TO BR0001-OUTPUT-ERR-MSG-NO
S10093              GO        TO MANUAL-BAD-DEBT-ENQ-EXIT
S10093           ELSE
S10093              MOVE RSNC-REASON-DESC TO BR0001-OUT-BAD-NARRATIVE
S10093           END-IF
S10093        END-IF
S10093     END-IF.
S10093     IF BR0001-17053-ACCOUNT
S12749        MOVE BOIS-ARRS-REASON-CD    TO  BR0001-OUT-ARRS-REASON-CD
S12749        MOVE BOIS-ASSET-RATING      TO  BR0001-OUT-ASSET-RATING
LON236     END-IF.
S10093     IF BR0001-17053-ACCOUNT
S12576        IF BOIS-WRITE-OFF-APRV-DT NOT = ZEROS
S12749        MOVE BOIS-WRITE-OFF-APRV-DT TO  UT0020-BINARY-DATE
S12749        MOVE 1                      TO  UT0020-FUNCTION
S12749        CALL "UT0020" USING  UT0020-FUNCTION
S12749                              UT0020-DATE-AREA
S12749                              UT0020-BINARY-DATE
S12749        IF UT0020-DATE-DDMMCCYY > 0
S12749         MOVE UT0020-DATE-DDMMCCYY TO  BR0001-OUT-WRIT-OFF-APRV-DT
S12749        END-IF
S12576     END-IF
S12749/
S12749       MOVE BOIS-WRITE-OFF-APRV-LEVEL-1
S12749                           TO  BR0001-OUT-WRIT-OFF-LVL-1
S12749       MOVE BOIS-WRITE-OFF-APRV-LEVEL-2
S12749                           TO  BR0001-OUT-WRIT-OFF-LVL-2
S12749       MOVE BOIS-WRITE-OFF-APRV-LEVEL-3
S12749                           TO  BR0001-OUT-WRIT-OFF-LVL-3
S10093    END-IF.
271140 MANUAL-BAD-DEBT-ENQ-EXIT.
271150*-------------------------
271160      EXIT.
LON236/
271570 M100-EDIT-INDICATOR.
271580*====================
LON152     INITIALIZE WA-AMOUNT-ROUND.
LON152     INITIALIZE WA-RESULT-INTEGER.
LON152     INITIALIZE WA-RES.
LON152     INITIALIZE WA-ROUNDING.
LON152     INITIALIZE ROUNDING-FACT.
LON152     INITIALIZE WA-REMD.
LON120     IF BR0001-BAD-DEBT-JRNL-NO NOT = ZEROES
LON120        MOVE WA-PREV-BAD-DEBT-IND TO WA-BAD-DEBT-IND
LON120                                     WA-MANUAL-BD-IND
LON120     ELSE
271610        MOVE BR0001-BAD-DEBT-IND TO WA-BAD-DEBT-IND
SC3009                                    WA-MANUAL-BD-IND
LON120     END-IF.
LON120     IF BR0001-BAD-DEBT-JRNL-NO NOT = ZEROES
LON120        CONTINUE
LON120     ELSE
271630        IF (WA-BAD-DEBT-IND-2 > 4)
271640           IF ((WA-BAD-DEBT-IND-2 > 4) AND
271650              (WA-BAD-DEBT-IND-2 < 9))
S13681           OR (WA-BAD-DEBT-IND-1 NOT = WA-ZERO AND 5)
271680              MOVE 1 TO WA-MSG-ERR-FLAG
271690              MOVE 759 TO BR0001-OUTPUT-ERR-MSG-NO
271700           ELSE
271710              MOVE BORM-BAD-DEBT-IND TO WA-BAD-DEBT-IND
271720              IF NOT WA-BAD-DEBT-IND-MANUAL
271730                 IF BR0001-BAD-DEBT-IND <= 03
S11761                 AND BR0001-BAD-DEBT-IND NOT = 02
271750*** BAD DEBT IND 00, 01, 02, 03 : AUTOMATICALLY SET BY THE SYSTEM
271760                     MOVE 1 TO WA-MSG-ERR-FLAG
271770                     MOVE 4876 TO BR0001-OUTPUT-ERR-MSG-NO
271780                 ELSE
271790                 IF BR0001-BAD-DEBT-IND = 04 AND
S13681                    BORM-BAD-DEBT-IND NOT = 03
271820                    MOVE 1    TO WA-MSG-ERR-FLAG
271830                    MOVE 4877 TO BR0001-OUTPUT-ERR-MSG-NO
271840                 ELSE
271850                    IF BR0001-BAD-DEBT-IND = 05 AND
S13681                       BORM-BAD-DEBT-IND NOT = 04
271880                       MOVE 1    TO WA-MSG-ERR-FLAG
271890                       MOVE 4878 TO BR0001-OUTPUT-ERR-MSG-NO
271900                    END-IF
271910                 END-IF
271920              END-IF
271930           END-IF
271940        END-IF
LON120     END-IF.
LON120     IF BR0001-BAD-DEBT-JRNL-NO NOT = ZEROES
LON120        CONTINUE
LON120     ELSE
SC5848        IF BORM-CAPN-METHOD = "X" OR
SC5848           BORP-CAPN-METHOD = "X"
SC5848           IF STRT-AREA-POST-DT < BOIS-MATURITY-DATE
SC5848              MOVE 1    TO WA-MSG-ERR-FLAG
SC5848              MOVE 4192 TO BR0001-OUTPUT-ERR-MSG-NO
SC5848           END-IF
LON120        END-IF
SC5848     END-IF.

SC3009     IF NOT VALID-IND-MAN
SC3009        MOVE 1    TO WA-MSG-ERR-FLAG
SC3009        MOVE 4145 TO BR0001-OUTPUT-ERR-MSG-NO
SC3009     END-IF
           IF BR0001-BAD-DEBT-JRNL-NO NOT = ZEROES
LON120        MOVE WA-PREV-BAD-DEBT-IND TO WA-BAD-DEBT-IND
LON120     ELSE
272080        MOVE BR0001-BAD-DEBT-IND TO WA-BAD-DEBT-IND
LON120     END-IF

S11761     IF BR0001-BAD-DEBT-IND = 02 AND
S11761        BR0001-BAD-DEBT-REASON-CODE = 9999 AND
S11761        BR0001-BAD-DEBT-DESC = "AUTO*TRANSFER*NPL"
S11761        MOVE 14 TO BR0001-BAD-DEBT-REASON-CODE
S11761     ELSE
S10591        IF BR0001-BAD-DEBT-IND < 50 AND
S10591          BR0001-BAD-DEBT-JRNL-NO = 0
S10591          MOVE 1    TO WA-MSG-ERR-FLAG
S10591          MOVE 4664 TO BR0001-OUTPUT-ERR-MSG-NO
S10591        END-IF
S11761     END-IF

S13681     IF BR0001-BAD-DEBT-JRNL-NO NOT = ZEROES
SC3306        CONTINUE
SC3306     ELSE
S11761        IF (WA-BAD-DEBT-IND = 52 OR 02) AND
SC3601           (BORM-BAD-DEBT-IND = 52 OR 54 OR 02 OR 04
SC8788                                               OR 59)
SC3009           MOVE 1    TO WA-MSG-ERR-FLAG
SC3009           MOVE 4144 TO BR0001-OUTPUT-ERR-MSG-NO
SC3009        END-IF

SC3009        IF (WA-BAD-DEBT-IND = 54) AND
SC3009          ((BORM-BAD-DEBT-IND NOT EQUAL 52 AND 02
SC3312                           AND 03 AND 53)
SC3009          OR BORM-BAD-DEBT-IND = 54)
SC3009          MOVE 1    TO WA-MSG-ERR-FLAG
SC3009          MOVE 4146 TO BR0001-OUTPUT-ERR-MSG-NO
SC3009        END-IF

SC3009        IF (WA-BAD-DEBT-IND = 53) AND
SC3009           (BORM-BAD-DEBT-IND NOT EQUAL 52 AND 02 AND 53)
SC3009           MOVE 1    TO WA-MSG-ERR-FLAG
SC3009           MOVE 4147 TO BR0001-OUTPUT-ERR-MSG-NO
SC3009        END-IF
SC3306     END-IF

272500     IF ((WA-BAD-DEBT-IND = 50 AND BR0001-BAD-DEBT-JRNL-NO = 0)
LON120        OR (BR0001-BAD-DEBT-JRNL-NO NOT = ZEROES  AND
LON120           WA-PREV-BAD-DEBT-IND = (00 OR 50)))
SC6903        IF BOIS-921-FLAG > 0
SC6903           CONTINUE
SC6903        ELSE
SC4921           MOVE BORP-CGL-COMPONENT-1-DR TO
SC4921              WA-NPL-COMPONENT-1
SC4921           MOVE BORP-CGL-COMPONENT-2-DR TO
SC4921              WA-NPL-COMPONENT-2
SC4921           PERFORM GET-CGL-CODE-NPL THRU
SC4921                GET-CGL-CODE-NPL-EXIT
SC4921           MOVE UT0099CA-GLCC-OUTPUT-KEY TO
SC4921                   BORM-GL-CLASSIFICATION-CODE
SC6903        END-IF
SC5190        MOVE "Y"     TO   WA-53-54-FLAG
S13681        IF BR0001-BAD-DEBT-JRNL-NO NOT = ZEROES
SC4235           GO TO M199-EXIT
SC4235        END-IF
SC3591        IF BORM-BAD-DEBT-IND NOT = 52 AND 02
SC4434                               AND 51 AND 01
S14340                               AND 60 AND 10
SC3591           MOVE 1    TO WA-MSG-ERR-FLAG
SC3591           MOVE 4156 TO BR0001-OUTPUT-ERR-MSG-NO
SC3591           GO        TO M199-EXIT
SC3591        END-IF
SC6374        IF BORP-AUTO-ACCT-GROUP-NO = 70
SC6374           MOVE 1    TO WA-MSG-ERR-FLAG
SC6374           MOVE 4370 TO BR0001-OUTPUT-ERR-MSG-NO
SC6374           GO        TO M199-EXIT
SC6374        END-IF
SC3980        IF BORM-BAD-DEBT-IND = 51 OR 01
SC3980           GO        TO M199-EXIT
SC3980        END-IF
SC4083        INITIALIZE UTHISTCA-CALL-AREA
SC4083        MOVE SPACE TO WA-BUCKET-CODE
SC4083        MOVE 0     TO WA-BUCKET-NO
SC4083        PERFORM G500-GET-PRIORITY-BUCKETS
SC4083                     THRU G599-EXIT
SC4083        MOVE 0 TO WA-INDX
SC4083        PERFORM
SC4083           VARYING WA-INDX FROM 1 BY 1
SC4083                 UNTIL WA-INDX > 15
SC4083           MOVE UTHISTRA-PRIO-NUM(WA-INDX) TO WA-PRIO-INDX
SC4083           IF UTHISTRA-PRIO-NUM(WA-INDX) = 00 OR " "
SC4083              CONTINUE
SC4083           ELSE
SC4083              MOVE UTHISTRA-PRIO-CDE(WA-INDX) TO
SC4083                   WA-PRIOCDE(WA-PRIO-INDX)
SC4083              MOVE UTHISTRA-PRIO-NUM(WA-INDX) TO
SC4083                   WA-PRIONUM(WA-PRIO-INDX)
SC4083              MOVE UTHISTRA-BUCKET-NO(WA-INDX) TO
SC4083                   WA-BUCKET-NO
SC4083           END-IF
SC4083        END-PERFORM
S10093     IF STRT-AREA-TRN-NO = 17053 OR 17003
S10093        IF BR0001-17053-CONTRACT
S10093           CONTINUE
S10093        END-IF
S10093     ELSE
273090        PERFORM GET-BOIS
273100           THRU GET-BOIS-EXIT
S10093     END-IF
SC4083        MOVE 0 TO WA-INDX
SC4083        MOVE 0 TO WA-TOTAL-BUCKET-INT
S12787        COMPUTE WA-TOTAL-BUCKET-INT = BORM-CAP-UNPAID-INT +
S12787                              BOIS-UNPAID-ARRS-PRIN-BAL +
S12787                              BOIS-UNPAID-INT-ARRS-PRIN-BAL +
S12787                              BORM-UNPAID-ARRS-INT-BAL
SC9326        MOVE WA-TOTAL-BUCKET-INT TO WA-WORK-AMOUNT-3
SC9326        PERFORM N900-ROUND-CURRENCY THRU N999-EXIT
SC9326        MOVE WA-WORK-AMOUNT-3  TO WA-TOTAL-BUCKET-INT
LON152        MOVE BORP-ROUNDING     TO WA-ROUNDING
LON152        IF WA-ROUNDING = 0
LON152           MOVE BOIS-FINE2-INT-ACCR TO WA-WORK-AMOUNT-3
LON152           PERFORM N900-ROUND-CURRENCY THRU N999-EXIT
LON152           IF WA-WORK-AMOUNT-3 > 0
LON152              MOVE 1    TO WA-MSG-ERR-FLAG
LON152              MOVE 3398 TO BR0001-OUTPUT-ERR-MSG-NO
LON152              GO TO M199-EXIT
LON152           END-IF
LON152           MOVE BOIS-FINE4-INT-ACCR TO WA-WORK-AMOUNT-3
LON152           PERFORM N900-ROUND-CURRENCY THRU N999-EXIT
LON152           IF WA-WORK-AMOUNT-3 > 0
LON152              MOVE 1    TO WA-MSG-ERR-FLAG
LON152              MOVE 3398 TO BR0001-OUTPUT-ERR-MSG-NO
LON152              GO TO M199-EXIT
LON152           END-IF
LON152           MOVE BORM-ARR-INT-ACCR TO WA-WORK-AMOUNT-3
LON152           PERFORM N900-ROUND-CURRENCY THRU N999-EXIT
LON152           IF WA-WORK-AMOUNT-3 > 0
LON152              MOVE 1    TO WA-MSG-ERR-FLAG
LON152              MOVE 3398 TO BR0001-OUTPUT-ERR-MSG-NO
LON152              GO TO M199-EXIT
LON152           END-IF
LON152        ELSE
LON152           MOVE BOIS-FINE2-INT-ACCR TO WA-AMOUNT-ROUND
LON152           PERFORM X150-ROUND-AMT THRU X159-EXIT
LON152           IF WA-RESULT >= WA-ROUNDING
LON152              MOVE 1 TO WA-MSG-ERR-FLAG
LON152              MOVE 3398 TO BR0001-OUTPUT-ERR-MSG-NO
LON152              GO TO M199-EXIT
LON152           END-IF
LON152           MOVE BOIS-FINE4-INT-ACCR TO WA-AMOUNT-ROUND
LON152           PERFORM X150-ROUND-AMT THRU X159-EXIT
LON152           IF WA-RESULT >= WA-ROUNDING
LON152              MOVE 1 TO WA-MSG-ERR-FLAG
LON152              MOVE 3398 TO BR0001-OUTPUT-ERR-MSG-NO
LON152              GO TO M199-EXIT
LON152           END-IF
LON152           MOVE BORM-ARR-INT-ACCR TO WA-AMOUNT-ROUND
LON327*LON152           DISPLAY "BEFORE ROUND"
LON152           PERFORM X150-ROUND-AMT THRU X159-EXIT
LON152           IF WA-RESULT >= WA-ROUNDING
LON152              MOVE 1 TO WA-MSG-ERR-FLAG
LON152              MOVE 3398 TO BR0001-OUTPUT-ERR-MSG-NO
LON152              GO TO M199-EXIT
LON152           END-IF
LON152        END-IF
SC3663        IF WA-TOTAL-BUCKET-INT > 0
LON152**LON051           BORM-ARR-INT-ACCR > 0   OR
LON152**LON051           BOIS-FINE2-INT-ACCR > 0 OR
LON152**LON051           BOIS-FINE4-INT-ACCR > 0)
SC1795           MOVE 1    TO WA-MSG-ERR-FLAG
SC1795           MOVE 3398 TO BR0001-OUTPUT-ERR-MSG-NO
SC1795        ELSE
SC3867           MOVE BOIS-NPL-STRT-PEN-CALC-DATE TO
SC4235                           WA-SAV-PREV-BAD-DEBT-TRF-DT
LON189**SC3867                           BOIS-LAST-UNPAID-DUE-DATE
SC3867           MOVE 0   TO BOIS-NPL-STRT-PEN-CALC-DATE
SC3867           MOVE 0 TO WA-BAD-DBT-IDX
ST0094           MOVE BORM-LOAN-BAL TO WA-SAV-PREV-BAD-DEBT-AMT
SC7779           MOVE BOIS-PREV-DUE-DATE
SC7779                              TO WA-NPL-PREV-DUE-DATE
SC8133           MOVE BORM-STORE-RATE      TO WA-NPL-STOR-RT
SC8133           MOVE BORM-INT-RATE        TO WA-NPL-INT-RT
SC8133           MOVE BORM-INT-ADJUSTMENT  TO WA-NPL-INT-ADJ
SC8133           MOVE BOIS-NEGO-RATE-IND   TO WA-NEGO-RATE-IND
SC8133           MOVE BOIS-NEGO-RATE-FLAG  TO WA-NEGO-RATE-FLAG
SC8133           MOVE BOIS-EFR-CHNG-IND    TO WA-EFR-CHNG-IND
SC8133           MOVE BORM-DUES-RECALC-IND TO WA-DUES-RECALC-IND
SC8133           MOVE BORM-RT-INCR         TO WA-RATE-INCR
SC8133           MOVE BOIS-NPL-JRNL-NO     TO WA-NPL-JRNL-NO
SC1795        END-IF.

275280 M199-EXIT.
275290*----------
275300     EXIT.

275320 M200-WRITE-BORV-TXN48.
275330*======================

SC1456     IF BORV-48-NEW-STATUS  = 03 OR 53
SC1456        CONTINUE
SC1456     ELSE
SC1456        IF BORV-48-PREV-STATUS = BORV-48-NEW-STATUS
SC1456           GO TO M299-EXIT
SC1456        END-IF
SC1456     END-IF.
275450     MOVE 48                TO BORV-48-TRAN-TYPE
275460     MOVE 00                TO BORV-48-TRAN-STATUS
275470     MOVE STRT-AREA-POST-DT TO BORV-48-TRAN-DATE
275480     MOVE STRT-AREA-BRANCH  TO BORV-48-BRANCH
275490     MOVE STRT-AREA-TERM-NO TO BORV-48-BRANCH-TERM
275500     MOVE STRT-AREA-TELL-NO TO BORV-48-TELLER
LON120     IF BR0001-BAD-DEBT-JRNL-NO NOT = 0
LON120        ADD 100 TO WA-NEW-TRAN
LON120        MOVE WA-NEW-TRAN TO BORV-48-TRAN-CODE
LON120     ELSE
275510        MOVE WA-NEW-TRAN TO BORV-48-TRAN-CODE
LON120     END-IF
275520     MOVE STRT-AREA-POST-DT TO BORV-48-POST-DATE
275530     MOVE STRT-AREA-JRNL-NO TO BORV-48-JRNL-NO
275540     MOVE "CHNG STAT "      TO BORV-48-DESC
SC1121     MOVE WA-BORM-INT-ACCR  TO BORV-48-INT-ACCR
SC2386     MOVE WA-DEF-BUCKET-AMT TO BORV-48-DEF-AMT
SC1580     MOVE WA-SAV-PREV-BAD-DEBT-TRF-DT TO BORV-48-PREV-BAD-DEBT-DT
275590     PERFORM X200-ADD-NEW-TXN
275600        THRU X299-EXIT.
S11716     IF (BR0001-BAD-DEBT-IND = 52 OR 50 OR 00 OR 02)
SC8277        AND NOT BORP-YOUBE
ST0094        MOVE 53                TO BORV-53-TRAN-TYPE
ST0094        MOVE 00                TO BORV-53-TRAN-STATUS
ST0094        MOVE STRT-AREA-POST-DT TO BORV-53-TRAN-DATE
ST0094        MOVE STRT-AREA-BRANCH  TO BORV-53-BRANCH
ST0094        MOVE STRT-AREA-TERM-NO TO BORV-53-BRANCH-TERM
ST0094        MOVE STRT-AREA-TELL-NO TO BORV-53-TELLER
ST0094        MOVE WA-NEW-TRAN       TO BORV-53-TRAN-CODE
ST0094        MOVE STRT-AREA-POST-DT TO BORV-53-POST-DATE
ST0094        MOVE STRT-AREA-JRNL-NO TO BORV-53-JRNL-NO
ST0094        MOVE WA-NPL-STOR-RT    TO BORV-53-STORE-RATE
ST0094        MOVE WA-NPL-INT-RT     TO BORV-53-INT-RATE
ST0094        MOVE WA-NPL-INT-ADJ    TO BORV-53-INT-ADJUSTMENT
ST0094        MOVE WA-NPL-INT-DUE    TO BORV-53-INT-DUE
ST0094        MOVE WA-RATE-INCR        TO BORV-53-NPL-RT-INCR
ST0094        MOVE WA-NEGO-RATE-IND  TO BORV-53-NEGO-RATE-IND
SC7560        MOVE WA-NEGO-RATE-FLAG TO BORV-53-NEGO-RATE-FLAG
SC7560        MOVE WA-EFR-CHNG-IND   TO BORV-53-EFR-CHNG-IND
SC7560        MOVE WA-DUES-RECALC-IND TO BORV-53-DUES-RECALC-IND
ST0094        PERFORM X200-ADD-NEW-TXN
ST0094           THRU X299-EXIT
ST0094     END-IF.

276330 M299-EXIT.
276340*----------
276350     EXIT.
TB0258/
SC1413 M320-CORRECT-REJM.
SC1413*
SC1413       MOVE BORM-MEMB-CUST-AC    TO REJM-ACCT-NO.
SC1413       MOVE ZEROES               TO DBIOMOD-KEY-NO.
SC1413       MOVE  REJM-KEY            TO DBIOMOD-KEY.
SC1413       MOVE "READNEXT"           TO DBIOMOD-FUNCTION.
SC1413       PERFORM M600-ACCESS-REJM-DBIOMOD THRU M600-EXIT.
SC5861       IF   BORM-MEMB-CUST-AC  NOT =  REJM-ACCT-NO
SC5861            MOVE 132        TO BR0001-OUTPUT-ERR-MSG-NO
SC5861            MOVE 1          TO WA-MSG-ERR-FLAG
SC5861            GO TO M320-EXIT
SC5861       END-IF
SC1413       IF   DBIOMOD-STAT =  "****"
SC5861            IF REJM-REASON-CODE NOT = WA-REJM-REASON-CODE
SC5861            OR REJM-BUILD-DATE  NOT = WA-BR0001-BAD-DEBT-DATE
SC1413            OR REJM-STATUS NOT = "J"
SC1413               GO                TO  M320-CORRECT-REJM
SC1413            END-IF
SC1413            MOVE "REWRITE"    TO DBIOMOD-FUNCTION
SC1413       ELSE
SC1413            GO TO M320-EXIT
SC1413       END-IF.
SC1413       MOVE  UT0073CA-EFF-DATE   TO  REJM-RELEASE-DATE.
SC1413       MOVE  "C"                 TO  REJM-STATUS.
SC1413       MOVE STRT-AREA-TELL-NO    TO  REJM-RELEASE-TELLER.
SC1413       MOVE  REJM-KEY            TO  DBIOMOD-KEY.
SC1413       PERFORM M600-ACCESS-REJM-DBIOMOD THRU M600-EXIT.
SC1413       IF DBIOMOD-STAT NOT =  "****"
SC1413          GO TO M320-EXIT
SC1413       END-IF.
SC1413*
SC1413 M320-EXIT.
SC1413     EXIT.
SC4455/
280060 M500-ACCESS-CUSM-RECORD.
280070     MOVE WA-DUMMY-KEY        TO CUSM-KEY
280080     MOVE CUSM-KEY            TO DBIO-CALL-KEY-AREA
280090     MOVE "CUSM"              TO DBIO-CALL-SYS-AREA
280100     PERFORM U100-CUSM-ACCESS THRU U199-EXIT
280110     IF DBIO-CALL-STATUS NOT = "****"
280120        MOVE 1               TO WA-MSG-ERR-FLAG
280130        MOVE 1   TO ST-STATUS-LEVEL-1
280140        MOVE 155             TO BR0001-OUTPUT-ERR-MSG-NO
280150        MOVE "CUSM"          TO BR0001-OUTPUT-DATA-SET
280160        MOVE DBIO-CALL-STATUS TO BR0001-OUTPUT-STATUS
280170        MOVE UT8500CA-FUNC-NORMAL-DISPLAY
280180                                   TO UT8500CA-CALL-CODE
280190        MOVE SPACES TO UT8500CA-INPUT-MSG
280200        STRING  "BR0001:(M500) BAD " DBIO-CALL-FUNC
280210                    " ON CUSM, STAT = " DBIO-CALL-STATUS
280220                    "KEY = " DBIO-CALL-KEY-AREA
280230                 DELIMITED BY SIZE INTO UT8500CA-INPUT-MSG
280240        PERFORM UT8500-CALL THRU UT8500-EXIT.
280250 M500-EXIT.
280260     EXIT.

FG1580 M700-WRITE-BORV-TXN51.
FG1580
FG1580     IF (BORV-48-NEW-STATUS  = 03 OR 53
S11659                                 OR 54 OR 04
SC4235                                 OR 50) OR
SC8133        (WA-BAD-DEBT-IND-AFT = 50)
FG1580        CONTINUE
FG1580     ELSE
FG1580        IF BORV-48-PREV-STATUS = BORV-48-NEW-STATUS
LON237           GO TO M799-EXIT
FG1580        END-IF
FG1580     END-IF.
FG1580     MOVE 51                TO BORV-51-TRAN-TYPE
FG1580     MOVE 00                TO BORV-51-TRAN-STATUS
FG1580     MOVE STRT-AREA-POST-DT TO BORV-51-TRAN-DATE
FG1580     MOVE STRT-AREA-BRANCH  TO BORV-51-BRANCH
FG1580     MOVE STRT-AREA-TELL-NO TO BORV-51-TELLER
FG1580     MOVE WA-NEW-TRAN       TO BORV-51-TRAN-CODE
FG1580     MOVE STRT-AREA-POST-DT TO BORV-51-POST-DATE
FG1580     MOVE STRT-AREA-JRNL-NO TO BORV-51-JRNL-NO
FG1580     MOVE WA-SAV-PREV-BAD-DEBT-TRF-DT TO
FG1580                                      BORV-51-PREV-BAD-DEBT-DT
FG1580     MOVE WA-SAV-PREV-BAD-DEBT-AMT    TO
FG1580                                      BORV-51-PREV-BAD-DEBT-AMT
FG1580     MOVE WA-SAV-PREV-GL-INT-AMT      TO
FG1580                                      BORV-51-PREV-GL-INT-AMT
FG1580     MOVE WA-SAV-PREV-GL-PRIN-AMT     TO
FG1580                                      BORV-51-PREV-GL-PRIN-AMT
FG1580     MOVE WA-SAV-PREV-TOT-BAD-AMT     TO
FG1580                                      BORV-51-PREV-TOT-BAD-AMT
SC8133     MOVE WA-PREV-CYCLE-NO            TO
SC8133                                      BORV-51-PREV-CYCLE-NO
FG1580
FG1580     PERFORM X200-ADD-NEW-TXN
FG1580        THRU X299-EXIT.
FG1580
FG1580 M799-EXIT.
FG1580     EXIT.
LON236/
284130 M600-ACCESS-REJM-DBIOMOD.
284140     MOVE "REJM"          TO DBIOMOD-FILE
284150                             DBIOMOD-SUFFIX.
284160     MOVE "DBIO"          TO DBIOMOD-PREFIX.
284170     MOVE SPACES          TO DBIOMOD-RELEASE-LOCK
284180                             DBIOMOD-ACCESS-COMMAND
284190                             DBIOMOD-ACCESS-COND
284200                             DBIOMOD-STATUS.

284220     CALL DBIOMODNAME USING

284240                         DBIOMOD-FILE
284250                         DBIOMOD-KEY
284260                         REJM-RECORD-AREA
284270                         DBIOMOD-FUNCTION
284280                         DBIOMOD-KEY-NO
284290                         DBIOMOD-RELEASE-LOCK
284300                         DBIOMOD-ACCESS-COMMAND
284310                         DBIOMOD-ACCESS-COND
284320                         DBIOMOD-STATUS.

284340 M600-EXIT.
TB0258     EXIT.

SC5100 GET-CGL-CODE-NPL.
SC5100*
SC6108     MOVE BORM-BR-NO       TO UT0099CA-BRANCH
SC5100     MOVE WA-NPL-COMPONENT-1
SC5100                           TO UT0099CA-COMPONENT-1
SC5100     MOVE WA-NPL-COMPONENT-2
SC5100                           TO UT0099CA-COMPONENT-2
SC5100     MOVE CUSM-SEGMENT-CODE TO UT0099CA-SEGMENT
SC5100     MOVE CUSM-GROUP-CODE   TO UT0099CA-GROUP-CODE
SC5100     MOVE BORP-CURRENCY-IND
SC5100                         TO UT0099CA-CURRENCY
SC5100     MOVE SOC-GL-CLASS-CODE-STRUCTURE TO
SC5100                         UT0099CA-GLCC-STRUCTURE
SC5100     CALL "UT0099" USING UT0099CA-AREA
SC5100     IF UT0099CA-RETURN-STATUS NOT = 0
SC5100        MOVE 2083  TO BR0001-OUTPUT-ERR-MSG-NO
SC5100        MOVE 1 TO WA-MSG-ERR-FLAG
SC5100        GO TO GET-CGL-CODE-NPL-EXIT
SC5100     END-IF.
SC5100 GET-CGL-CODE-NPL-EXIT.
SC5100    EXIT.

284590 REWRITE-BORV.
284600*
284620     IF BORV-48-PREV-STATUS = BORV-48-NEW-STATUS
284630        GO TO REWRITE-BORV-EXIT.
284640     MOVE WA-TRAN-STATUS    TO BORV-48-TRAN-STATUS
284650     PERFORM REWRITE-TXN
284660        THRU REWRITE-TXN-EXIT.

284680 REWRITE-BORV-EXIT.
284700     EXIT.
LON236/
FG1580 REWRITE-BORV-51.
FG1580     MOVE WA-TRAN-STATUS    TO BORV-48-TRAN-STATUS
FG1580     PERFORM REWRITE-TXN
FG1580        THRU REWRITE-TXN-EXIT.
FG1580 REWRITE-BORV-EXIT-51.
FG1580      EXIT.

284780 REWRITE-TXN.
284790*============

284810     MOVE 13        TO DBIO-CALL-FUNC.
284820     MOVE SPACES    TO DBIO-CALL-STATUS.
284830     CALL "DBIO" USING
284840                 DBIO-CALL-PARAM
284850                 DBIO-CALL-SYS-AREA
284860                 DBIO-CALL-KEY-AREA
284870                 DBIO-CALL-STATUS
284880                 BORV-RECORD-AREA
284890                 DBIO-CALL-REFER-AREA.

284910     IF  DBIO-CALL-STATUS NOT = "****"
284920         MOVE 1             TO WA-MSG-ERR-FLAG
284930         MOVE 155           TO BR0001-OUTPUT-ERR-MSG-NO
284940         MOVE "BORV"        TO BR0001-OUTPUT-DATA-SET
284950         MOVE DBIO-CALL-STATUS TO BR0001-OUTPUT-STATUS
284960         MOVE UT8500CA-FUNC-NORMAL-DISPLAY TO
284970              UT8500CA-CALL-CODE
284980         MOVE SPACES TO UT8500CA-INPUT-MSG
284990         STRING "BR0001:(X200) PROBLEM WHILE REWRITE TO BORV"
285000                BORM-KEY
285010                DELIMITED BY SIZE INTO UT8500CA-INPUT-MSG
285020            CALL "UT8500" USING
285030                          UT8500CA-AREA
285040         GO TO REWRITE-TXN-EXIT
285050     END-IF.

285070 REWRITE-TXN-EXIT.
285080*================
285090     EXIT.

285110**
285120** THIS ROUTINE READS THE TRANSACTIONS FROM MOST RECENT TO EARLIES
285130**
285140 G100-READ-BORV.
285150*===============

285170     MOVE 01               TO DBIO-CALL-TXN-TYPE.
285180     MOVE "BOR"            TO DBIO-CALL-SYS-AREA.
285190     MOVE STRT-AREA-SOC-NO TO WA-BORV-SOC-NO.
285200     MOVE WA-ACCT-NO-16    TO WA-BORV-ACT-NO.
285210     MOVE WA-BORV-KEY      TO DBIO-CALL-KEY-AREA.
285220     MOVE 06               TO DBIO-CALL-FUNC.

285240     PERFORM ACCESS-BORV
285250        THRU ACCESS-BORV-EXIT.

285270 G100-READ-BORV-EXIT.
285280*--------------------
285290     EXIT.

285310 ACCESS-BORV.
285320*============

285340     MOVE SPACES TO DBIO-CALL-STATUS.
285350     CALL "DBIO" USING
285360                 DBIO-CALL-PARAM
285370                 DBIO-CALL-SYS-AREA
285380                 DBIO-CALL-KEY-AREA
285390                 DBIO-CALL-STATUS
285400                 BORV-RECORD-AREA
285410                 DBIO-CALL-REFER-AREA
285420                 DBIO-CALL-TXN-TYPE-AREA.

285440     IF DBIO-CALL-STATUS = "XCOT"
285450        IF WA-READ > 0
285460           MOVE "Y"  TO WA-END-OF-FILE
285470           GO TO ACCESS-BORV-EXIT
285480        ELSE
285490           MOVE 1    TO WA-MSG-ERR-FLAG
285500           MOVE 3886 TO BR0001-OUTPUT-ERR-MSG-NO
285510           GO TO ACCESS-BORV-EXIT
285520     ELSE
285530        IF DBIO-CALL-STATUS = "****"
285540           ADD 1 TO WA-READ
285550        END-IF
285560     END-IF .

285580 ACCESS-BORV-EXIT.
285590*-----------------
285600     EXIT.

285620 GET-BOIS.
285630*=========

285650     INITIALIZE BOIS-KEY.
285660     MOVE STRT-AREA-SOC-NO TO BOIS-INST-NO.
285670     MOVE WA-ACCT-NO-16    TO BOIS-MEMB-CUST-AC.
285680     MOVE "READ"           TO DBIOMOD-FUNCTION.
285690     MOVE ZEROES           TO DBIOMOD-KEY-NO.
285700     MOVE BOIS-KEY         TO DBIOMOD-KEY.

285720     PERFORM ACCESS-BOIS
285730        THRU ACCESS-BOIS-EXIT.


285760     IF DBIO-CALL-STATUS = "MRNF"
285770        MOVE 1    TO WA-MSG-ERR-FLAG
285780        MOVE 8987 TO BR0001-OUTPUT-ERR-MSG-NO
285790        GO TO GET-BOIS-EXIT
285800     END-IF.

285820     IF DBIO-CALL-STATUS NOT = "****"
285830        MOVE 1                TO WA-MSG-ERR-FLAG
285840        MOVE 155              TO BR0001-OUTPUT-ERR-MSG-NO
285850        MOVE "BOIS"           TO BR0001-OUTPUT-DATA-SET
285860        MOVE DBIO-CALL-STATUS TO BR0001-OUTPUT-STATUS
285870        MOVE UT8500CA-FUNC-NORMAL-DISPLAY
285880                              TO UT8500CA-CALL-CODE
285890        MOVE SPACES           TO UT8500CA-INPUT-MSG
285900        STRING "BR0001:(A0000) 'BOIS' READ ERROR, STAT "
285910                DBIO-CALL-STATUS "KEY = "
285920                DBIO-CALL-KEY-AREA
285930                DELIMITED BY SIZE INTO
285940                UT8500CA-INPUT-MSG
285950        CALL "UT8500" USING
285960                      UT8500CA-AREA

285980        PERFORM Z000-FORMT-SEND-PARA
285990            THRU Z099-EXIT
286000     END-IF.

286020 GET-BOIS-EXIT.
286030*==============
286040     EXIT.

286060 ACCESS-BOIS.
286070*============

286090     MOVE "BOIS"   TO DBIOMOD-FILE
286100                      DBIOMOD-SUFFIX.
286110     MOVE "DBIO"   TO DBIOMOD-PREFIX.
286120     MOVE SPACES   TO DBIOMOD-RELEASE-LOCK
286130                      DBIOMOD-ACCESS-COMMAND
286140                      DBIOMOD-ACCESS-COND
286150                      DBIOMOD-STATUS.

286170     CALL DBIOMODNAME USING
286180                      DBIOMOD-FILE
286190                      DBIOMOD-KEY
286200                      BOIS-RECORD-AREA
286210                      DBIOMOD-FUNCTION
286220                      DBIOMOD-KEY-NO
286230                      DBIOMOD-RELEASE-LOCK
286240                      DBIOMOD-ACCESS-COMMAND
286250                      DBIOMOD-ACCESS-COND
286260                      DBIOMOD-STATUS.

286280 ACCESS-BOIS-EXIT.
286290*-----------------
286300     EXIT.

286320 VERIFY-BORV.
286330*============

286350     MOVE STRT-AREA-TRN-NO    TO WA-SAVE-TRANS-NO
S13681     IF BORV-01-CODE      NOT = 48
FG1580                          AND 51
ST0094                          AND 53
SC5572        IF BORV-01-CODE  = 20
SC5572           MOVE BORV-20-CHQ-PAYEE TO WA-STORED-TEXT-LN
SC5572           MOVE WA-ORIG-TXN-CODE  TO LINK-IN-TXCODE
SC5572        ELSE
SC5595          IF BORV-01-CODE = 52
SC5595            MOVE BORV-52-TRAN-CODE   TO LINK-IN-TXCODE
SC5595          ELSE
NR0001            IF BORV-01-CODE = 21
NR0001               MOVE BORV-20-TRAN-NO  TO LINK-IN-TXCODE
NR0001            ELSE
SC5572               MOVE BORV-01-TRN-CODE    TO LINK-IN-TXCODE
NR0001            END-IF
SC5595          END-IF
SC5572        END-IF
286540     ELSE
286550        MOVE BORV-48-TRAN-CODE   TO LINK-IN-TXCODE
286560     END-IF.
SC6273     IF BORV-48-JRNL-NO = BR0001-BAD-DEBT-JRNL-NO
286580        MOVE STRT-AREA-EDIT-MASK TO WA-SAVE-TRAN-EDIT-MASK
286590        PERFORM A700-SET-UP-EDIT-MASK
286600           THRU A709-EXIT

286620        MOVE LINK-EDIT-TABLE     TO STRT-AREA-EDIT-MASK
286630        MOVE LINK-POSTING-LEVEL  TO STRT-AREA-POSTING-LEVEL
286640        MOVE STRT-AREA-PROCESS-BITS TO FIELD-B
286650        CALL "UT0022" USING PUAREA
286660        MOVE FIELD-A                TO WA-EDIT-32

286680        MOVE WA-SAVE-TRANS-NO       TO STRT-AREA-TRN-NO
286690        MOVE WA-SAVE-TRAN-EDIT-MASK TO STRT-AREA-EDIT-MASK
SC6273     END-IF

286720     IF (BORV-48-TRAN-CODE   =  17003 OR 17103) AND
286730        BORV-48-TRAN-TYPE    =  48
FG1580                                OR 51
ST0094                                OR 53
286760        MOVE BORV-48-TRAN-DATE TO UT0020-BINARY-DATE
286770        MOVE 2 TO UT0020-FUNCTION
286780        PERFORM Y010-CALL-UT0020 THRU Y019-EXIT
286790        MOVE UT0020-DATE-CCYYMMDD TO WA-BORV-TRAN-DATE
286800        IF BORV-48-JRNL-NO      = BR0001-BAD-DEBT-JRNL-NO
286810           IF WA-BORV-TRAN-DATE = WA-BD-DATE
FG1580              IF BORV-01-CODE = 51
ST0094                              OR 53
FG1580                 CONTINUE
FG1580              ELSE
286860                 MOVE "Y" TO WA-FOUND-MANUAL-TAG
FG1580              END-IF
286880              IF ((BORV-01-STATUS > 19) AND (BORV-01-STATUS < 40))
286890              OR ((BORV-01-STATUS > 69) AND (BORV-01-STATUS < 90))
286900              OR   WA-EDIT-32-BIT(7) = 1
286910                   MOVE "R" TO WA-FOUND-MANUAL-TAG
286920              ELSE
FG1580                   IF BORV-01-CODE = 48 OR BORV-01-CODE = 53
ST0094                    IF BORV-01-CODE = 53
ST0094                      MOVE BORV-53-TRAN-STATUS TO WA-TRAN-STATUS
ST0094                      ADD 020                  TO WA-TRAN-STATUS
ST0094                      MOVE BORV-53-STORE-RATE  TO WA-NPL-STOR-RT
ST0094                      MOVE BORV-53-INT-RATE    TO WA-NPL-INT-RT
ST0094                      MOVE BORV-53-INT-ADJUSTMENT
ST0094                                               TO WA-NPL-INT-ADJ
ST0094                      MOVE BORV-53-INT-DUE     TO WA-NPL-INT-DUE
ST0094                      MOVE BORV-53-NPL-RT-INCR TO WA-RATE-INCR
ST0094                      MOVE BORV-53-NEGO-RATE-IND TO
ST0094                                               WA-NEGO-RATE-IND
SC7560                      MOVE BORV-53-NEGO-RATE-FLAG TO
SC7560                                               WA-NEGO-RATE-FLAG
SC7560                      MOVE BORV-53-EFR-CHNG-IND   TO
SC7560                                               WA-EFR-CHNG-IND
SC7560                      MOVE BORV-53-DUES-RECALC-IND TO
SC7560                                               WA-DUES-RECALC-IND
SC7850                      MOVE BORV-53-JRNL-NO     TO WA-NPL-JRNL-NO
ST0094                    ELSE
287140                      MOVE BORV-48-TRAN-STATUS TO WA-TRAN-STATUS
287150                      ADD  020                 TO WA-TRAN-STATUS
SC1121                      MOVE BORV-48-INT-ACCR    TO WA-BORM-INT-ACCR
SC2386                      MOVE BORV-48-DEF-AMT    TO WA-DEF-BUCKET-AMT
287180                      PERFORM REWRITE-BORV
287190                         THRU REWRITE-BORV-EXIT

287210                      MOVE BORV-48-PREV-STATUS
287220                        TO BR0001-BAD-DEBT-IND
287230                      MOVE 17103  TO WA-NEW-TRAN
TB0211                                     WA-17003-CORR-TRAN-CD
287250                      MOVE ZEROES
287260                        TO BR0001-BAD-DEBT-REASON-CODE
ST0094                    END-IF
FG1580                   ELSE
FG1580                    MOVE BORV-51-TRAN-STATUS TO WA-TRAN-STATUS
FG1580                    ADD  020                 TO WA-TRAN-STATUS
FG1580                    PERFORM REWRITE-BORV-51
FG1580                       THRU REWRITE-BORV-EXIT-51
FG1580
FG1580                    MOVE 17103  TO WA-NEW-TRAN
FG1580                                   WA-17003-CORR-TRAN-CD
FG1580                    MOVE BORV-51-PREV-BAD-DEBT-DT TO
FG1580                         WA-SAV-PREV-BAD-DEBT-TRF-DT
FG1580                    MOVE BORV-51-PREV-TOT-BAD-AMT TO
FG1580                         WA-SAV-PREV-TOT-BAD-AMT
FG1580                    MOVE BORV-51-PREV-GL-PRIN-AMT TO
FG1580                         WA-SAV-PREV-GL-PRIN-AMT
FG1580                    MOVE BORV-51-PREV-GL-INT-AMT  TO
FG1580                         WA-SAV-PREV-GL-INT-AMT
FG1580                    MOVE BORV-51-PREV-BAD-DEBT-AMT TO
FG1580                         WA-SAV-PREV-BAD-DEBT-AMT
FG1580                    MOVE ZEROES
FG1580                      TO BR0001-BAD-DEBT-REASON-CODE
FG1580                   END-IF
SC1725                   IF (BORM-BAD-DEBT-IND = 52
ST0094                       AND BORV-01-CODE NOT = 53)
SC1725                      OR BORV-01-CODE = 48
SC1082                      GO TO  VERIFY-BORV-EXIT
FG1580                   END-IF
287540              END-IF
287550           ELSE
287560              MOVE 1    TO WA-MSG-ERR-FLAG
287570              MOVE 0126 TO BR0001-OUTPUT-ERR-MSG-NO
287580              GO TO VERIFY-BORV-EXIT
287590           END-IF
287600        ELSE
287610           IF ((BORV-01-STATUS > 19) AND (BORV-01-STATUS < 40)) OR
287620              ((BORV-01-STATUS > 69) AND (BORV-01-STATUS < 90)) OR
287630                WA-EDIT-32-BIT(7) = 1
287640                NEXT SENTENCE
287650           ELSE
287660              ADD 1 TO WA-PRECEEDING-TRANS
287670           END-IF
287680        END-IF
287690     ELSE
287700        IF ((BORV-01-STATUS > 19) AND (BORV-01-STATUS < 40)) OR
287710           ((BORV-01-STATUS > 69) AND (BORV-01-STATUS < 90)) OR
287720             WA-EDIT-32-BIT(7) = 1
287730             NEXT SENTENCE
287740        ELSE
S13681           IF BORV-01-CODE NOT = 36
287770              ADD 1 TO WA-PRECEEDING-TRANS
287780           END-IF
287790        END-IF
287800     END-IF.

287820     PERFORM G100-READ-BORV
287830        THRU G100-READ-BORV-EXIT.

287850 VERIFY-BORV-EXIT.
287860*-----------------
287870     EXIT.

SC9829 CHECK-CORRECTION-TXN.
SC9829     SET WA-CONTINUE             TO TRUE.
SC9829
LON120     INITIALIZE XXCT-ALT-KEY2.
LON120     INITIALIZE XXCT-DATAREC.
SC9829     PERFORM CHECK-BORV THRU CHECK-BORV-EXIT.
SC9829 CHECK-CORRECTION-EXIT.
SC9829     EXIT.

SC9829 CHECK-BORV.
SC9829     IF STRT-AREA-OTHER-OVERRIDE = "7"
SC9829        SET WA-STOP TO TRUE
SC9829        GO TO CHECK-BORV-EXIT.
LON327*LON120     DISPLAY "CHECK TXN".
LON120     MOVE STRT-AREA-SOC-NO  TO  XXCT-ALT2-INST-NO
LON120     MOVE WA-BAD-DEBT-DT    TO  XXCT-ALT2-TRN-DATE
LON120     MOVE BORM-MEMB-CUST-AC TO  XXCT-ALT2-ACCT-NO
LON120     MOVE WA-REC-NO         TO  XXCT-ALT2-REC-NO
LON120     MOVE XXCT-ALT-KEY2     TO  DBIOMOD-KEY
LON120                                DBIOMOD-ACCESS-COND
LON120     MOVE 2                 TO  DBIOMOD-KEY-NO
LON120     MOVE "BOCT"            TO  DBIOMOD-FILE
LON120                                DBIOMOD-SUFFIX
LON120     MOVE "DBIO"            TO  DBIOMOD-PREFIX
LON120     PERFORM UNTIL STRT-AREA-SOC-NO NOT = XXCT-ALT2-INST-NO
LON120     OR BORM-MEMB-CUST-AC NOT = XXCT-ALT2-ACCT-NO
LON120        MOVE "READNEXT"  TO DBIOMOD-FUNCTION
LON120        CALL  DBIOMODNAME
LON120              USING
LON120              DBIOMOD-FILE
LON120              DBIOMOD-KEY
LON120              XXCT-RECORD-AREA
LON120              DBIOMOD-FUNCTION
LON120              DBIOMOD-KEY-NO
LON120              DBIOMOD-RELEASE-LOCK
LON120              DBIOMOD-ACCESS-COMMAND
LON120              DBIOMOD-ACCESS-COND
LON120              DBIOMOD-STATUS
LON120        IF DBIOMOD-STAT NOT = "****" AND "MRNF"
LON120           MOVE UT8500CA-FUNC-NORMAL-DISPLAY TO
LON120                UT8500CA-CALL-CODE
LON120           MOVE SPACES  TO UT8500CA-INPUT-MSG
LON120           STRING "BR0001:(FIND-REC-NO) ERROR ACCESSING BOCT"
LON120           "DBIO-STAT = "        DBIOMOD-STAT
LON120           "DBIOMOD-KEY = "      DBIOMOD-KEY
LON120           "DBIOMOD-FUNCTION = " DBIOMOD-FUNCTION
LON120           DELIMITED BY SIZE    INTO UT8500CA-INPUT-MSG
LON120           CALL "UT8500"        USING UT8500CA-AREA
LON120           MOVE 1               TO WA-MSG-ERR-FLAG
LON120           MOVE 155             TO BR0001-OUTPUT-ERR-MSG-NO
LON120           GO TO CHECK-BORV-EXIT
LON120        END-IF
LON120        IF DBIOMOD-STAT = "MRNF"
LON120           MOVE "Y" TO WA-LATEST-TXN
LON120           GO TO CHECK-BORV-EXIT
LON120        END-IF
LON344        IF DBIOMOD-STAT = "****"
LON344           MOVE STRT-AREA-TRN-NO    TO WA-SAVE-TRANS-NO
LON344           MOVE XXCT-TRN-CODE       TO LINK-IN-TXCODE
LON344           MOVE STRT-AREA-EDIT-MASK TO WA-SAVE-TRAN-EDIT-MASK
LON344           PERFORM A700-SET-UP-EDIT-MASK
LON344                  THRU A709-EXIT
LON344
LON344           MOVE LINK-EDIT-TABLE        TO STRT-AREA-EDIT-MASK
LON344           MOVE LINK-POSTING-LEVEL     TO STRT-AREA-POSTING-LEVEL
LON344           MOVE STRT-AREA-PROCESS-BITS TO FIELD-B
LON344           CALL "UT0022" USING PUAREA
LON344           MOVE FIELD-A     TO WA-EDIT-32
LON344
LON344           MOVE WA-SAVE-TRANS-NO       TO STRT-AREA-TRN-NO
LON344           MOVE WA-SAVE-TRAN-EDIT-MASK TO STRT-AREA-EDIT-MASK
LON344
LON344           IF WA-EDIT-32-BIT(7) = 1
LON344              CONTINUE
LON344           ELSE
LON120              IF DBIOMOD-STAT = "****"
LON120                 AND XXCT-INST-NO = STRT-AREA-SOC-NO
LON120                 AND XXCT-ACCT-NO = BORM-MEMB-CUST-AC
LON120                 AND (XXCT-TRAN-TYPE = 01 OR 47 OR 48)
LON120                 AND XXCT-JRNL-NO NOT = BR0001-BAD-DEBT-JRNL-NO
LON120                 AND XXCT-REC-NO < WA-REC-NO
LON120                 AND XXCT-TRN-DATE >= WA-BAD-DEBT-DT
LON344*LON120                 AND XXCT-STAT NOT = 20
LON344                 AND XXCT-STAT NOT = 20 AND 30
LON120                     MOVE "N" TO WA-LATEST-TXN
LON120                     SET WA-STOP TO TRUE
LON120                     GO TO CHECK-BORV-EXIT
LON344*LON120              ELSE
LON344              END-IF
LON344           END-IF
LON120           IF DBIOMOD-STAT = "****"
LON327*LON120              DISPLAY "XXCT-REC-NO = " XXCT-REC-NO
LON120              MOVE XXCT-INST-NO TO XXCT-ALT2-INST-NO
LON120              MOVE XXCT-ACCT-NO TO XXCT-ALT2-ACCT-NO
LON120              MOVE XXCT-TRN-DATE TO XXCT-ALT2-TRN-DATE
LON120              MOVE XXCT-REC-NO TO XXCT-ALT2-REC-NO
LON120              MOVE XXCT-ALT-KEY2 TO DBIOMOD-KEY
LON120           END-IF
LON120          END-IF
LON120     END-PERFORM.
SC9829 CHECK-BORV-EXIT.
SC9829     EXIT.

288800 GET-REASON-CODE.
288810*================

288830     MOVE "READ"          TO DBIOMOD-FUNCTION.
288840     MOVE SPACES          TO DBIOMOD-STAT.
288850     MOVE "RSNC"          TO DBIOMOD-FILE
288860                             DBIOMOD-SUFFIX.
288870     MOVE "DBIO"          TO DBIOMOD-PREFIX.
288880     MOVE RSNC-DATA-KEY   TO DBIOMOD-KEY.
288890     MOVE ZEROES          TO DBIOMOD-KEY-NO.
288900     MOVE SPACES          TO DBIOMOD-RELEASE-LOCK
288910                             DBIOMOD-ACCESS-COMMAND
288920                             DBIOMOD-ACCESS-COND
288930                             DBIOMOD-STATUS.

288950     CALL DBIOMODNAME USING
288960                      DBIOMOD-FILE
288970                      DBIOMOD-KEY
288980                      WA-REASON-CODE-REC
288990                      DBIOMOD-FUNCTION
289000                      DBIOMOD-KEY-NO
289010                      DBIOMOD-RELEASE-LOCK
289020                      DBIOMOD-ACCESS-COMMAND
289030                      DBIOMOD-ACCESS-COND
289040                      DBIOMOD-STATUS.

289060     IF DBIOMOD-STAT = "MRNF"
S12209        IF STRT-AREA-TRN-NO = 17053
S12209           CONTINUE
S12209        ELSE
289070           MOVE 1    TO WA-MSG-ERR-FLAG
289080           MOVE "Y"  TO WA-NO-MORE-FLAG
289090           MOVE 1702 TO BR0001-OUTPUT-ERR-MSG-NO
289100           GO TO GET-REASON-CODE-EXIT
S12209        END-IF
S12209     ELSE
289130        IF DBIOMOD-STAT NOT = "****"
289140           MOVE 1                TO WA-MSG-ERR-FLAG
289150           MOVE 155              TO BR0001-OUTPUT-ERR-MSG-NO
289160           MOVE "RSNC"           TO BR0001-OUTPUT-DATA-SET
289170           MOVE DBIO-CALL-STATUS TO BR0001-OUTPUT-STATUS
289180           MOVE UT8500CA-FUNC-NORMAL-DISPLAY
289190                                 TO UT8500CA-CALL-CODE
289200           MOVE SPACES           TO UT8500CA-INPUT-MSG
289210           STRING "BR0001:(A0000) 'RSNC' READ ERROR, STAT "
289220                   DBIO-CALL-STATUS "KEY = "
289230                   DBIO-CALL-KEY-AREA
289240                   DELIMITED BY SIZE INTO
289250                   UT8500CA-INPUT-MSG
289260           CALL "UT8500" USING
289270                         UT8500CA-AREA
289280           PERFORM Z000-FORMT-SEND-PARA
289290              THRU Z099-EXIT
289300           GO TO GET-REASON-CODE-EXIT
S12209        END-IF
289310     END-IF.

289330 GET-REASON-CODE-EXIT.
289340*---------------------
289350     EXIT.

289370 A700-SET-UP-EDIT-MASK.
289380*======================

289400     MOVE 02               TO LINK-IN-CALL-CODE.
289410     MOVE STRT-AREA-SOC-NO TO LINK-IN-SOC-NO.
289420     CALL "UT0250" USING
289430                   UT0250-CALL-AREA.
289440     IF LINK-OUT-STAT NOT = "****"
289450        MOVE 100 TO BR0001-OUTPUT-ERR-MSG-NO
289460        MOVE 1   TO WA-MSG-ERR-FLAG
289470        MOVE LINK-IN-SOC-NO TO WA-DISP-SOC-NO
289480        MOVE LINK-IN-TXCODE TO WA-DISP-TXCODE
289490        MOVE UT8500CA-FUNC-NORMAL-DISPLAY TO UT8500CA-CALL-CODE
289500        MOVE SPACES TO UT8500CA-INPUT-MSG
289510        STRING "BR0001:(A700) - ERROR IN CALL UT0250"
289520               ", KEY:"    WA-DISP-KEY
289530               ", STATUS:" LINK-OUT-STAT
289540               DELIMITED BY SIZE INTO UT8500CA-INPUT-MSG
289550        CALL "UT8500" USING
289560                      UT8500CA-AREA
289570        PERFORM Z000-FORMT-SEND-PARA
289580           THRU Z099-EXIT
289590        GO TO A709-EXIT
289600     END-IF.

289620 A709-EXIT.
289630*----------
289640     EXIT.
TB0092/
294380 Y010-CALL-UT0020.
294390     CALL "UT0020" USING UT0020-FUNCTION
294400                         UT0020-DATE-AREA
294410                         UT0020-BINARY-DATE.
294420 Y019-EXIT.
294430     EXIT.
294440/
294450 Y020-CALL-UT0030.
294460     CALL "UT0030" USING UT0030-FUNCTION
294470                         UT0030-DATE-AREA
294480                         UT0030-BINARY-DATE.
294490 Y029-EXIT.
294500     EXIT.
TB0104*
TB0104 Y040-SORD-CREATE-MAINT.
TB0104*-----------------------*
TB0104*** MOVE FIELDS TO UT0014 PASSALONG AREA
TB0104*
TB0104    INITIALIZE IN0014-INPUT-MESSAGE.
TB0104*
TB0104    MOVE STRT-AREA-DATA     TO IN0014-DATA.
TB0104    IF STRT-AREA-TRN-NO = 12000
TB0104       MOVE 901             TO IN0014-TRN-NO
TB0104    ELSE
TB0104    IF STRT-AREA-TRN-NO = 17000
S10517       OR 17046
TB0104       IF WA-BOIS-AUTO-DBT-ACCT-UPDTD
S10517          OR STRT-AREA-TRN-NO = 17046
tb0104          MOVE 951          TO IN0014-TRN-NO
TB0104       ELSE
TB0104          GO TO Y049-EXIT
TB0104       END-IF
TB0104    END-IF
TB0104    END-IF.
TB0104    MOVE BOIS-SORD-AUTO-DBT-ACCT-NO
TB0104                            TO IN0014-INPUT-ACCT-NO.

TB0104    MOVE 0                  TO IN0014-INPUT-AMOUNT.
TB0104    MOVE BORM-REPAY-FREQ    TO IN0014-INPUT-FREQ
TB0104                               WA-UT0014-INPUT-FREQ.
TB0104    IF DAILY-INPUT-FREQ
TB0104       MOVE "D"             TO IN0014-INPUT-CODE
TB0104    ELSE
TB0104       IF MONTHLY-INPUT-FREQ
TB0104          MOVE "M"          TO IN0014-INPUT-CODE.
TB0104    MOVE BORM-MEMB-CUST-AC  TO IN0014-INPUT-RECACC-NO.
TB0104    MOVE "LON"              TO IN0014-INPUT-REC-SYS.
TB0104    MOVE SPACES             TO IN0014-INPUT-COMMENTS.
TB0104***  prepare to call ut0312 to get maturity date
TB0104     MOVE 04                TO UT0312CA-CALL-CODE.
TB0104     IF BORM-INT-START-DATE = 0
TB0104         MOVE STRT-AREA-POST-DT   TO UT0312CA-DUE-STRT-DT
TB0104     ELSE
TB0104         MOVE BORM-INT-START-DATE TO UT0312CA-DUE-STRT-DT.
S10373     MOVE UT0312CA-DUE-STRT-DT    TO UT0312CA-EFF-DATE.

TB0104     IF BORM-REPAY-FREQ = 88 OR 77
TB0104        MOVE BORM-REPAY-FREQ TO UT0312CA-REPAY-FREQ
TB0104     ELSE
TB0104        MOVE BORM-INT-REST     TO UT0312CA-REPAY-FREQ
TB0104        MOVE SPACE             TO UT0312CA-REPAY-SCHED.

TB0104* convert UT0312CA-MATURITY-DT To gregorian
TB0104    MOVE UT0312CA-MATURITY-DT  TO UT0020-BINARY-DATE.
TB0104    MOVE 1 TO UT0020-FUNCTION
TB0104    CALL "UT0020"        USING UT0020-FUNCTION
TB0104                               UT0020-DATE-AREA
TB0104                               UT0020-BINARY-DATE.
TB0104    MOVE UT0020-DATE-DDMMCCYY  TO IN0014-INPUT-TDATE.

S10373    IF BORM-STAT > 06
S10373       MOVE BOIS-NEXT-DUE-DATE TO UT0020-BINARY-DATE
S10373       MOVE 1 TO UT0020-FUNCTION
S10373       CALL "UT0020"        USING UT0020-FUNCTION
S10373                            UT0020-DATE-AREA
S10373                            UT0020-BINARY-DATE
S10373       IF UT0020-DATE-MM = 4 OR 6 OR 9 OR 11
S10373          IF UT0020-DATE-DD > 30
S10373             MOVE 30 TO UT0020-DATE-DD
S10373          END-IF
S10373       END-IF
S10373       MOVE UT0020-DATE-DDMMCCYY
S10373            TO IN0014-INPUT-START-DATE
S10373       MOVE STRT-AREA-TRN-NO(2:5) TO IN0014-INPUT-PURPOSE
S10373    ELSE
TB0104    MOVE ZEROES                TO IN0014-INPUT-START-DATE.
TB0104    MOVE BORP-CURRENCY-IND     TO IN0014-TO-ACC-CURRENCY.
TB0104    MOVE BORP-AUTO-CHSE-DAYS   TO IN0014-COMMON-CHASE-DAYS.
TB0104    MOVE "Y"                   TO IN0014-INSURANCE-TRANS.
TB0104    MOVE " "                   TO IN0014-INPUT-PYMNT-TYPE.
TB0104    MOVE 0                     TO WA-UT0014-CALL-CODE.
TB0104    CALL "UT0014"            USING   WA-UT0014-PASSALONG
TB0104                                     WA-UT0014-CALL-CODE.
TB0104*
TB0104 Y049-EXIT.
TB0104     EXIT.
298070/
LON236* CANCEL
298760*ZA2300-CALC-BUCV-ACCR.

TB0069*    COMPUTE WA-BUCV-INT-ACCR
TB0069*         = WA-INT-ACCR - BOIS-LAST-BPI-INT-ACCR.
299130/
LON236* CANCEL BRTH TABLE
299930*ZC1000-PROCESS-RT-CH.
299940***--saving of new rate to BRTH
      *
300010*    IF  WA-STORE-RATE-CH
300020*        MOVE 71   TO DBIO-CALL-FUNC
300030*        MOVE "Y"  TO WA-CHECK-AMEND-BRTH
300080*        IF DBIO-CALL-STATUS = "****"
300150*           MOVE SPACES TO DBIO-CALL-STATUS
300160*           PERFORM T100-STORE-BORV-34 THRU T199-EXIT
300170*           IF WA-MSG-ERR-FLAG = 1
300180*              GO TO ZC1999-EXIT
300190*           END-IF
300200*        ELSE
300210*           IF DBIO-CALL-STATUS = "MRNF" OR "RCNF"
300220*              MOVE SPACES TO DBIO-CALL-STATUS
300230*              MOVE 72 TO DBIO-CALL-FUNC
300290*              PERFORM T100-STORE-BORV-34 THRU T199-EXIT
300300*              IF WA-MSG-ERR-FLAG = 1
300310*                 GO TO ZC1999-EXIT
300320*              END-IF
300330*           END-IF
300340*        END-IF
300350*    END-IF.
300360***
300370*ZC1999-EXIT.
300380*    EXIT.
LON236* END
300390/
TB0255 Z400-CREATE-SORD.
LON236*
TB0255    INITIALIZE IN0014-INPUT-MESSAGE.
TB0255    MOVE STRT-AREA-DATA  TO IN0014-DATA.
SC3055    PERFORM Z350-GET-NEXT-WORKING-DATE THRU Z350-EXIT.
TB0255    MOVE BORM-MEMB-CUST-AC        TO IN0014-INPUT-ACCT-NO.
TB0255    MOVE BR0001-10915-AMOUNT      TO IN0014-INPUT-AMOUNT.
TB0255    MOVE BR0001-10915-FREQ        TO IN0014-INPUT-FREQ.
S10113    IF BR0001-10915-FREQ-CODE = SPACES
S10113       MOVE "D"   TO IN0014-INPUT-CODE
S10113    ELSE
S10113    MOVE BR0001-10915-FREQ-CODE   TO IN0014-INPUT-CODE
S10113    END-IF.
TB0255    MOVE BR0001-10915-ACCT-SYSTEM TO IN0014-INPUT-REC-SYS.
TB0255    MOVE "LON"                    TO IN0014-FROM-ACC-SYSTEM.
SC3055    MOVE UT0030-DATE-DDMMCCYY     TO IN0014-INPUT-START-DATE
SC3055                                     IN0014-INPUT-TDATE.
TB0255    MOVE BR0001-10915-COMMENTS    TO IN0014-INPUT-COMMENTS.
TB0255    MOVE BR0001-10915-SEC-CODE    TO IN0014-INPUT-SEC-CODE.
TB0255    MOVE BR0001-10915-CURR-CODE   TO IN0014-FROM-ACC-CURRENCY
302590                                     IN0014-INPUT-AMT-CCY.
TB0255    MOVE BORP-CURRENCY-IND        TO IN0014-TO-ACC-CURRENCY.
TB0255    MOVE BR0001-10915-TRF-ACCT-NO TO IN0014-INPUT-RECACC-NO.
TB0255    MOVE BR0001-10915-PRTY-CODE   TO IN0014-INPUT-PRIORITY.
TB0255    MOVE BR0001-10915-PROMO-CD    TO IN0014-PROMO-CODE.
TB0255    MOVE 0                        TO BR0001-OUTPUT-ERR-MSG-NO.
TB0255    PERFORM Z000-FORMT-SEND-PARA THRU Z099-EXIT.
TB0255    MOVE "Y"                      TO IN0014-INSURANCE-TRANS.
TB0255    MOVE " "                      TO IN0014-INPUT-PYMNT-TYPE.
TB0255    MOVE 0                        TO WA-UT0014-CALL-CODE.
SC3219    IF IN0014-INPUT-TDATE = SPACES
SC3219       MOVE 0                     TO IN0014-INPUT-TDATE
SC3219    END-IF.
TB0255    CALL "UT0014"            USING   WA-UT0014-PASSALONG
TB0255                                     WA-UT0014-CALL-CODE.
TB0255    IF  IN0014-STATUS-LEVEL-1 NOT = 0
TB0255        MOVE 0 TO IN0014-STATUS-LEVEL-1
TB0255        MOVE 1 TO WA-MSG-ERR-FLAG.
TB0255    MOVE 0    TO BR0001-OUTPUT-ERR-MSG-NO.
TB0255    MOVE 0    TO WA-MSG-ERR-FLAG.
TB0255    PERFORM Z000-FORMT-SEND-PARA THRU Z099-EXIT.
LON236*
TB0255 Z499-EXIT.
TB0255     EXIT.
LON236/
TB0255 Z500-AMEND-SORD.
LON236*
TB0255    INITIALIZE IN0014-INPUT-MESSAGE.
TB0255    MOVE STRT-AREA-DATA          TO IN0014-DATA.
TB0255    MOVE BORM-MEMB-CUST-AC       TO IN0014-INPUT-ACCT-NO.
TB0255    MOVE BR0001-10916-AMOUNT     TO IN0014-INPUT-AMOUNT.
TB0255    MOVE BR0001-10916-FREQ       TO IN0014-INPUT-FREQ.
TB0255    MOVE BR0001-10916-CODE       TO IN0014-INPUT-CODE.
SC3965    MOVE BR0001-10916-START-DATE TO IN0014-INPUT-START-DATE.
SC3965    MOVE BR0001-10916-END-DATE   TO IN0014-INPUT-TDATE.
TB0255    MOVE BR0001-10916-COMMENTS   TO IN0014-INPUT-COMMENTS.
TB0255    MOVE BR0001-10916-SEC-CODE   TO IN0014-INPUT-SEC-CODE.
TB0255    MOVE BR0001-10916-CURR-CODE  TO IN0014-FROM-ACC-CURRENCY
TB0255                                    IN0014-INPUT-AMT-CCY.
TB0255    MOVE BR0001-10916-TRF-NO     TO IN0014-INPUT-RECACC-NO.
TB0255    MOVE BORP-CURRENCY-IND       TO IN0014-TO-ACC-CURRENCY.
TB0255    MOVE "Y"                     TO IN0014-INSURANCE-TRANS.
TB0255    MOVE " "                     TO IN0014-INPUT-PYMNT-TYPE.
TB0255    MOVE 0                       TO WA-UT0014-CALL-CODE.
TB0255    CALL "UT0014"           USING   WA-UT0014-PASSALONG
TB0255                                    WA-UT0014-CALL-CODE.
TB0255    IF  IN0014-STATUS-LEVEL-1 NOT = 0
TB0255        MOVE 0 TO IN0014-STATUS-LEVEL-1
TB0255        MOVE 1 TO WA-MSG-ERR-FLAG.
TB0255    MOVE 0    TO BR0001-OUTPUT-ERR-MSG-NO.
TB0255    MOVE 0    TO WA-MSG-ERR-FLAG.
TB0255    PERFORM Z000-FORMT-SEND-PARA THRU Z099-EXIT.
LON236*
TB0255 Z599-EXIT.
TB0255     EXIT.
LON236/
SC3055 Z350-GET-NEXT-WORKING-DATE.
LON236*
303220      MOVE 1     TO UT1999CA-CALL-CODE
303230      CALL "UT1999" USING
303240           UT1999CA-CALL-AREA
303250      MOVE UT1999CA-CCYYMMDD  TO W100-TODAY.
303260      MOVE W100-TODAY-DD TO UT0310CA-CALENDAR-IN-DD.
303270      MOVE W100-TODAY-MM TO UT0310CA-CALENDAR-IN-MM.
303280      MOVE W100-TODAY-CC TO UT0310CA-CALENDAR-IN-CC.
303290      MOVE W100-TODAY-YY TO UT0310CA-CALENDAR-IN-YY.
303300      MOVE 5                  TO UT0310CA-CALL-FUNC.
303310      PERFORM Z400-FIND-WK-DAY THRU Z400-EXIT.
LON236*
303320 Z350-EXIT.
303330      EXIT.
LON236/
303340 Z400-FIND-WK-DAY.
S11469*
S11469     MOVE BORP-CALENDAR-CODE TO UT0310CA-CALENDAR-CODE.
303370     MOVE ZEROES             TO UT0310CA-CALL-STATUS
303380                                UT0310CA-CALENDAR-METHOD.
303390     MOVE STRT-AREA-SOC-NO   TO UT0310CA-SOC-NO.
303400     CALL "UT0310"        USING UT0310CA-AREA.
LON527     IF UT0310CA-CALL-STATUS NOT = 0
LON527        MOVE UT8500CA-FUNC-NORMAL-DISPLAY TO UT8500CA-CALL-CODE
LON527        MOVE SPACES TO UT8500CA-INPUT-MSG
LON527        STRING "BR0000:(Z400) UT0310 INIT - BAD STATUS "
LON527               UT0310CA-CALL-STATUS
LON527        DELIMITED BY SIZE INTO UT8500CA-INPUT-MSG
LON527        CALL "UT8500" USING UT8500CA-AREA
LON527        MOVE 126 TO BR0001-OUTPUT-ERR-MSG-NO
LON527        MOVE 1   TO WA-MSG-ERR-FLAG
LON527        GO       TO Z400-EXIT
LON527     END-IF.
303430     MOVE UT0310CA-CALENDAR-OUT-CCYYMMDD
303440                             TO UT0030-DATE-CCYYMMDD.
303450     MOVE 2                  TO UT0030-FUNCTION.
303460     CALL "UT0030"        USING UT0030-FUNCTION
303470                                UT0030-DATE-AREA
303480                                UT0030-BINARY-DATE.
303490 Z400-EXIT.
303500      EXIT.
LON236/
S10139 Z600-OVERRIDABLE-ERROR-CHECK.
S10139     MOVE ZERO                 TO WA-MSG-ERR-FLAG.
S10139
S10139* ** READ AND TEST THE TELLER RECORD
S10139
S10139     MOVE "11"                 TO UT0060CA-CALL-CODE.
S10139     MOVE STRT-AREA-SOC-NO     TO UT0060CA-CALL-SOCIETY-NO.
S10139     MOVE STRT-AREA-ABS-TERM   TO UT0060CA-CALL-TERM-NO.
S10139     MOVE STRT-AREA-TELL-NO    TO UT0060CA-CALL-TELR-NO.
S10139
S10139     IF WA-SUPERVISOR-OVERRIDE-IN-USE = SPACE OR "0"
S10139        MOVE WA-OVER-RIDE      TO UT0060CA-CALL-OVERRIDE-SIGNAL
S10139     ELSE
S10139        MOVE SPACES            TO UT0060CA-CALL-OVERRIDE-SIGNAL
S10139        MOVE WA-SUPERVISOR-OVERRIDE-IN-USE
S10139                               TO UT0060CA-CALL-OVERRIDE-SIGN.
S10139
S10139        MOVE BORM-MEMB-CUST-AC      TO UT0060CA-CALL-ACC-NO
S10139        MOVE STRT-AREA-TRN-NO       TO UT0060CA-CALL-TRAN-CODE
S10139        MOVE 1                      TO UT0060CA-CALL-ACC-INVOLVED
S10139                                       UT0060CA-CALL-PROGRESS-FLAG
S10139*
S10139*    IF THE TRAN IS A BATCH TRAN REPRESENTING A FEE CHARGING
S10139*       TO CUSTOMER A/C, THEN THE OVERRIDE CODE "61" WILL ENABLE
S10139*       THE TRAN TO OVERRIDE ALL OVERRIDABLE ERRORS, IF ANY.
S10139*       (I.E. ONE "61" MAY OVERRIDE MORE THAN ONE OVERRIDABLE
S10139*       ERROR, IF NECESSARY).
S10139*    IF (STRT-BATCH-TRICKLE-TXN) AND
S10139*       (WA-OVER-RIDE = "XV" OR "TT")
S10139*
S10139***  SET UP OVERRIDE FIELDS FOR ALL BATCH/TRICKLE FEED TXNS -
S10139***  UT0060 WILL DETERMINE WHETHER TXN SHOULD BE OVERRIDDEN OR NOT
S10139*
S10139     IF STRT-BATCH-TRICKLE-TXN
S10139        MOVE "61"              TO UT0060CA-CALL-OVERRIDE-SIGNAL
S10139        MOVE WA-OVER-RIDE      TO UT0060CA-CALL-OVERRIDE-SGNL-OL.
S10139
S10139     MOVE 00                   TO UT0060CA-CALL-STATUS.
S10139     MOVE STRT-AREA-BATCH-TANDEM TO WK-BATCH-TANDEM.
S10139
S10139     CALL "UT0060" USING
S10139                         UT0060CA-AREA
S10139                         MULTI-OVERRIDE-AREA
S10139                         WK-UT0060-WORK-AREA.
S10139
S10139     MOVE "Y"                  TO WA-CALL-UT0060-FLAG.
S10139     IF UT0060CA-CALL-STATUS = 00
S10139        GO TO Z699-EXIT.
S10139
S10139     IF UT0060CA-CALL-STATUS = 99
S10139*  **   THE CURRENT OVERRIDE SIGNAL HAS BEEN USED, SAVE/CLEAR IT
S10139        MOVE WA-OVER-RIDE TO WA-OVER-RIDE-USED
S10139        MOVE WA-SUPERVISOR-OVERRIDE-IN-USE TO
S10139             WA-SUPERVISOR-OVERRIDE-USED
S10139        MOVE SPACE TO WA-OVER-RIDE, WA-SUPERVISOR-OVERRIDE-IN-USE
S10139        GO TO Z699-EXIT.
S10139
S10139     IF UT0060CA-CALL-STATUS = 98
S10139*  **   THE TRAN WAS OVERRIDEN BEFORE; RESTORE OLD OVERRIDE VALUE
S10139        MOVE "69" TO WA-OVER-RIDE-USED
S10139        IF UT0060CA-CALL-OVERRIDE-SGNL-OL = "69"
S10139           MOVE SPACE TO WA-SUPERVISOR-OVERRIDE-USED
S10139           GO TO Z699-EXIT
S10139        ELSE
S10139           MOVE UT0060CA-CALL-OVERRIDE-SIGN-OL TO
S10139                WA-SUPERVISOR-OVERRIDE-USED
S10139           GO TO Z699-EXIT.
S10139
S10139     IF UT0060CA-CALL-STATUS = 97
S10139*  **   BATCH TRAN WITH "61"; DON'T CLEAR OVERRIDE SIGNAL
S10139        MOVE "61" TO WA-OVER-RIDE-USED
S10139        MOVE WA-SUPERVISOR-OVERRIDE-IN-USE TO
S10139             WA-SUPERVISOR-OVERRIDE-USED
S10139        GO TO Z699-EXIT.
S10139*
S10139* ** ERROR HANDLING
S10139
S10139     MOVE 1 TO WA-MSG-ERR-FLAG.
S10139
S10139     IF UT0060CA-CALL-STATUS = 01
S10139        MOVE 155 TO BR0001-OUTPUT-ERR-MSG-NO
S10139        MOVE UT8500CA-FUNC-NORMAL-DISPLAY TO UT8500CA-CALL-CODE
S10139        MOVE SPACES TO UT8500CA-INPUT-MSG
S10139        STRING "BR0001(Z600) INVALID CALL CODE "
S10139                UT0060CA-CALL-CODE
S10139            DELIMITED BY SIZE INTO UT8500CA-INPUT-MSG
S10139        CALL "UT8500" USING
S10139
S10139                            UT8500CA-AREA
S10139        GO TO Z699-EXIT.
S10139
S10139     IF UT0060CA-CALL-STATUS = 02
S10139        MOVE 155 TO BR0001-OUTPUT-ERR-MSG-NO
S10139        MOVE UT0060CA-CALL-TELR-NO TO WA-DISPLAY-NUMBER
S10139        MOVE UT8500CA-FUNC-NORMAL-DISPLAY TO UT8500CA-CALL-CODE
S10139        MOVE SPACES TO UT8500CA-INPUT-MSG
S10139        STRING "BR0001(Z600) INVALID TELLER NO "
S10139                WA-DISPLAY-NUMBER
S10139            DELIMITED BY SIZE INTO UT8500CA-INPUT-MSG
S10139        CALL "UT8500" USING
S10139                            UT8500CA-AREA
S10139        GO TO Z699-EXIT.
S10139
S10139     IF UT0060CA-CALL-STATUS = 03 OR 09
S10139        MOVE 155 TO BR0001-OUTPUT-ERR-MSG-NO
S10139        MOVE UT0060CA-CALL-TERM-NO TO WA-DISPLAY-NUMBER
S10139        MOVE UT8500CA-FUNC-NORMAL-DISPLAY TO UT8500CA-CALL-CODE
S10139        MOVE SPACES TO UT8500CA-INPUT-MSG
S10139        STRING "BR0001(Z600) INVALID TERMINAL NO "
S10139                WA-DISPLAY-NUMBER
S10139            DELIMITED BY SIZE INTO UT8500CA-INPUT-MSG
S10139        CALL "UT8500" USING
S10139                            UT8500CA-AREA
S10139        GO TO Z699-EXIT.
S10139
S10139     IF UT0060CA-CALL-STATUS = 04
S10139        MOVE 465 TO  BR0001-OUTPUT-ERR-MSG-NO
S10139        GO TO Z699-EXIT.
S10139
S10139
S10139     IF UT0060CA-CALL-STATUS = 05 OR 06 OR 07 OR 08
S10139        MOVE UT0060CA-CALL-ERROR-CODE TO  BR0001-OUTPUT-ERR-MSG-NO
S10139        GO TO Z699-EXIT.
S10139
S10139**** CALL STATUS ERROR
S10139
S10139     MOVE 155 TO BR0001-OUTPUT-ERR-MSG-NO.
S10139
S10139     MOVE UT8500CA-FUNC-NORMAL-DISPLAY TO UT8500CA-CALL-CODE
S10139     MOVE SPACES TO UT8500CA-INPUT-MSG
S10139     STRING "BR0001(Z600) WA-UT0060 CALL ERROR "
S10139             UT0060CA-CALL-STATUS
S10139         DELIMITED BY SIZE INTO UT8500CA-INPUT-MSG
S10139     CALL "UT8500" USING
S10139                         UT8500CA-AREA.
S10139*
S10139 Z699-EXIT.
S10139     EXIT.

S13420 F833-VALIDATE-PREBOOK.
S13420*----------------------
S13420     MOVE STRT-AREA-SOC-NO   TO SYPR-SOC-NO
S13420     MOVE "BOR"              TO SYPR-SYSTEM
S13420     MOVE BR0001-INPUT-KEY-R TO SYPR-ACCT-NO
S13420     MOVE ZEROES             TO SYPR-OD-NO
S13420                                SYPR-SEQNO
S13420                                SYPR-CURR-EXP-DATE.
S13420     MOVE "READNEXT"         TO DBIOMOD-FUNCTION.
S13420     PERFORM N0200-ACCESS-SYPR THRU N0200-EXIT.
S13420*    READ SYPR where SYPR-CURR-EXP-DATE > = WA-TODAY-DATE
S13420     IF DBIOMOD-STAT = '****'
S13420      IF SYPR-SOC-NO = STRT-AREA-SOC-NO AND
S13420         SYPR-SYSTEM = "BOR" AND
S13420         SYPR-ACCT-NO = BR0001-INPUT-KEY-R AND
S13420         SYPR-CURR-EXP-DATE >= WA-TODAY-DATE
S13681         IF SYPR-NEW-INT-CAT NOT = SPACES AND
S13681            SYPR-NEW-ACCT-TYPE NOT = SPACES
S13420          MOVE 5471 TO BR0001-OUTPUT-ERR-MSG-NO
S13420          MOVE 1    TO WA-MSG-ERR-FLAG
LON237          GO TO F833-EXIT
S13420        END-IF
S13420      END-IF
S13420     END-IF.
S13420
S13420 F833-EXIT.
S13420*----------
S13420    EXIT.

S13420 N0200-ACCESS-SYPR.
S13420*-----------------
S13420
S13420     MOVE SYPR-KEY              TO DBIOMOD-KEY.
S13420     MOVE ZEROES                TO DBIOMOD-KEY-NO.
S13420     MOVE SPACES                TO DBIOMOD-STAT.
S13420     MOVE "SYPR"                TO DBIOMOD-FILE
S13420                                   DBIOMOD-SUFFIX.
S13420     MOVE "DBIO"                TO DBIOMOD-PREFIX.
S13420     MOVE SPACES                TO DBIOMOD-RELEASE-LOCK
S13420                                   DBIOMOD-ACCESS-COMMAND
S13420                                   DBIOMOD-ACCESS-COND.
S13420
S13420     CALL DBIOMODNAME        USING DBIOMOD-FILE
S13420                                   DBIOMOD-KEY
S13420                                   SYPR-RECORD
S13420                                   DBIOMOD-FUNCTION
S13420                                   DBIOMOD-KEY-NO
S13420                                   DBIOMOD-RELEASE-LOCK
S13420                                   DBIOMOD-ACCESS-COMMAND
S13420                                   DBIOMOD-ACCESS-COND
S13420                                   DBIOMOD-STATUS.
S13420 N0200-EXIT.
S13420*----------
S13420     EXIT.
S10099******************************************************
S10099* NAME OF SECTION: Z830-WRITE-PROM
S10099* FUNCTION: WRITE PROM
S10099******************************************************
S10099 Z830-WRITE-PROM.
LON362      MOVE 0 TO WA-TMP-AMT.
S10099      IF ((BOIS-PROV-AMOUNT NOT = BORM-LOAN-BAL AND
LON286         BR0001-BAD-DEBT-JRNL-NO = ZEROES)    OR
LON286         (BR0001-BAD-DEBT-JRNL-NO NOT = ZEROES))
S10099         INITIALIZE PROM-RECORD-AREA
LON286         IF BR0001-BAD-DEBT-JRNL-NO NOT = ZEROES
LON286            MOVE 0  TO PROM-PREV-AMT
LON286**                     PROM-BAD-DEBT-IND
LON286            MOVE WA-TODAY-DATE TO PROM-CURT-DATE
LON286            MOVE BOIS-PROM-PREV-AMT TO
LON286                      PROM-PROVISION-AMOUNT
LON286         ELSE
S10099            MOVE BOIS-PROV-AMOUNT TO PROM-PREV-AMT
S10099            MOVE BORM-LOAN-BAL TO PROM-PROVISION-AMOUNT
S10099            MOVE WA-TODAY-DATE TO PROM-CURT-DATE
S10099**          SET PROM-PROV-IND-MM TO TRUE
LON362            MOVE BOIS-PROV-IND TO PROM-PROV-IND
S10099            IF BORM-BAD-DEBT-IND NOT = ZERO
S10099               MOVE "1" TO PROM-BAD-DEBT-IND
S10099            ELSE
S10099               MOVE "0" TO PROM-BAD-DEBT-IND
S10099            END-IF
LON286         END-IF
S10099         MOVE BOIS-CONTRACT-NO TO PROM-CTRT-NO
S10099         PERFORM E1000-GET-TIME THRU E1099-EXIT
S10099         IF  UT1996CA-RETURN-CODE NOT = 0
S10099             GO TO Z830-EXIT
S10099         END-IF
S10099         MOVE WA-SOC-NO          TO PROM-INST-NO
S10099         MOVE BORM-MEMB-CUST-AC  TO PROM-ACCOUNT-NO
S10099         MOVE "BOR"              TO PROM-SYSTEM
S10099         MOVE 0                  TO PROM-RESERVED-INTEREST
S10099         MOVE STRT-AREA-TELL-NO  TO PROM-TELLER-NUMBER
S10099         MOVE STRT-AREA-BRANCH   TO PROM-BRANCH-NUMBER
S10099         MOVE WA-ENTRY-DATE-TIME TO PROM-ENTRY-DATE-TIME
S10099         MOVE SPACE              TO PROM-PROV-IND
S10099         MOVE ZERO               TO PROM-MM-RATE
S10099****WRITE PROM
S10099         SET WRITE-CMD           TO TRUE
S10099         MOVE PROM-KEY           TO DBIOMOD-KEY
S10099         MOVE "PROM"             TO DBIOMOD-FILE
S10099                                    DBIOMOD-SUFFIX
S10099         MOVE ZEROS              TO DBIOMOD-KEY-NO
S10099         MOVE SPACES             TO DBIOMOD-STATUS
S10099                                    DBIOMOD-RELEASE-LOCK
S10099                                    DBIOMOD-ACCESS-COMMAND
S10099                                    DBIOMOD-ACCESS-COND
S10099         CALL DBIOMODNAME USING DBIOMOD-FILE
S10099                                DBIOMOD-KEY
S10099                                PROM-RECORD-AREA
S10099                                DBIOMOD-FUNCTION
S10099                                DBIOMOD-KEY-NO
S10099                                DBIOMOD-RELEASE-LOCK
S10099                                DBIOMOD-ACCESS-COMMAND
S10099                                DBIOMOD-ACCESS-COND
S10099                                DBIOMOD-STATUS
S10099         IF DBIOMOD-STAT NOT = "****"
S10099            MOVE 1               TO WA-MSG-ERR-FLAG
S10099            MOVE UT8500CA-FUNC-NORMAL-DISPLAY
S10099                                     TO UT8500CA-CALL-CODE
S10099            MOVE SPACES              TO UT8500CA-INPUT-MSG
S10099            STRING "BR0001:(Z830) PROM WRITE ERR"
S10099                   ", KEY="    DBIOMOD-KEY
S10099            DELIMITED BY SIZE      INTO UT8500CA-INPUT-MSG
S10099            CALL "UT8500" USING    UT8500CA-AREA
S10099            GO TO Z830-EXIT
S10099         END-IF
S10099****END OF WRITE
LON286         IF BR0001-BAD-DEBT-JRNL-NO > 0
LON362            IF BR0001-BAD-DEBT-CONTRACT
LON362               SUBTRACT BOIS-PROM-PREV-AMT FROM
LON362                  BOIS-PROV-AMOUNT GIVING WA-TMP-AMT
LON362               IF BOIS-PROV-IND-MM
LON362                  SUBTRACT WA-TMP-AMT FROM
LON362                      CONB-MM-CURRENT-PROVISION
LON362               END-IF
LON362               IF BOIS-PROV-IND-DCF
LON362                  SUBTRACT WA-TMP-AMT FROM
LON362                     CONB-DCF-PROVISION-AMOUNT
LON362               END-IF
LON362               PERFORM B6100-UPDATE-CONB THRU B6199-EXIT
LON362            END-IF
LON286            MOVE BOIS-PROM-PREV-AMT TO BOIS-PROV-AMOUNT
LON286            MOVE ZEROES  TO  BOIS-PROM-PREV-AMT
LON286         ELSE
S10099            MOVE BOIS-PROV-AMOUNT TO BOIS-PROM-PREV-AMT
S10099            MOVE BORM-LOAN-BAL TO BOIS-PROV-AMOUNT
LON362            IF BR0001-BAD-DEBT-CONTRACT
LON362               SUBTRACT BOIS-PROM-PREV-AMT FROM
LON362                        BOIS-PROV-AMOUNT GIVING WA-TMP-AMT
LON362               IF BOIS-PROV-IND-MM
LON362                  ADD WA-TMP-AMT TO CONB-MM-CURRENT-PROVISION
LON362               END-IF
LON362               IF BOIS-PROV-IND-DCF
LON362                  ADD WA-TMP-AMT TO CONB-DCF-PROVISION-AMOUNT
LON362               END-IF
LON362               PERFORM B6100-UPDATE-CONB THRU B6199-EXIT
LON362            END-IF
LON286         END-IF
S10099         MOVE WA-TODAY-DATE TO BOIS-PROV-CURT-DATE
S10099     END-IF.
S10099 Z830-EXIT.
S10099     EXIT.
S10099/
S10099************************************************************
S10099* NAME OF SECTION: E1000-GET-TIME
S10099* FUNCTION: CONVERT THE DATE AND TIME
S10099************************************************************
S10099 E1000-GET-TIME.
S10099    MOVE UT1999CA-CC  TO      WA-ENTRY-DATE-CC
S10099    MOVE UT1999CA-YY  TO      WA-ENTRY-DATE-YY
S10099    MOVE UT1999CA-MM  TO      WA-ENTRY-DATE-MM
S10099    MOVE UT1999CA-DD  TO      WA-ENTRY-DATE-DD
S10099    MOVE 0 TO UT1996CA-CALL-CODE
S10099    CALL "UT1996" USING UT1996CA-CALL-AREA
S10099    IF  UT1996CA-RETURN-CODE NOT = 0
S10099        MOVE UT8500CA-FUNC-NORMAL-DISPLAY TO UT8500CA-CALL-CODE
S10099        MOVE SPACES TO UT8500CA-INPUT-MSG
S10099        STRING "BR0001:(E1000) BAD RETURN FROM UT1996, STATUS = "
S10099                                      UT1996CA-RETURN-CODE
S10099        DELIMITED BY SIZE INTO UT8500CA-INPUT-MSG
S10099        MOVE "N" TO UT8500CA-RESPONSE-REQD-FLAG
S10099        CALL "UT8500" USING UT8500CA-AREA
S10099        GO TO E1099-EXIT
S10099    END-IF
S10099    MOVE UT1996CA-HH  TO WA-ENTRY-TIME-HH
S10099    MOVE UT1996CA-MM  TO WA-ENTRY-TIME-MM
S10099    MOVE UT1996CA-SS  TO WA-ENTRY-TIME-SS.
S10099 E1099-EXIT.
S10099     EXIT.
S10093***This paragraph is called whenever a contract number is entered.
S10093***All the accounts under the contract no are processed one by one
S10093***This paragraph is similar to the E000-COMPLETION-PARA in BR0000
S10093***COB which will not be called if a contract no is entered.
S10093 E000-COMPLETION-PARA.
S10093     IF ST-STATUS-LEVEL-1 = 00
S10093        MOVE 0 TO BR0001-OUTPUT-ERR-MSG-NO
S10093                  WA-MSG-ERR-FLAG.
S10093 E009.
S10093     INITIALIZE JNRST-DB-DATA.
S10093*
S10093     IF  WA-EDIT-MASK-BIT(8) = 1
S10093         MOVE "Y" TO JNRST-BOR-BACKDATED-FLAG
S10093     ELSE
S10093         MOVE "N" TO JNRST-BOR-BACKDATED-FLAG.
S10093*
S10093     MOVE WA-MARG-INT-ACCR-BEFORE TO JNRST-MARG-INT-ACCR-BEFORE.
S10093     MOVE WA-MARG-INT-ACCR-AFTER  TO JNRST-MARG-INT-ACCR-AFTER.
S10093     MOVE WA-BASE-INT-ACCR-BEFORE TO JNRST-BASE-INT-ACCR-BEFORE.
S10093     MOVE WA-BASE-INT-ACCR-AFTER  TO JNRST-BASE-INT-ACCR-AFTER.
S10093*
S10093     IF  WA-CALL-UT0060-FLAG = "Y"
S10093       IF  WA-ERR-MSG-NO NOT = 0
S10093         MOVE SOC-OVERRIDE-ENQ-HDR TO JNRST-OVERRIDE-HDR-NO.

S10093 E010-CONTINUE.
S10093**BACKDATE CORRECTION TXN BYPASS E300 ALREADY INITIATED BEFORE CAL
S10093     PERFORM E200-JRNL-RESTART-PARA THRU E201-EXIT.
S10093     IF  WA-MSG-ERR-FLAG = 1
S10093         GO TO E010-EXIT.
S10093     IF  STRT-AREA-TRN-NO = 17003
S10093         PERFORM W600-REWRITE-MASTER THRU W601-EXIT
S10093         IF  WA-MSG-ERR-FLAG = 1
S10093             GO TO E010-EXIT.
S10093     IF  STRT-AREA-SOC-NO  = SOC-TRAINING-SOCIETY OR
S10093         WA-EDIT-MASK-BIT(14) = 0
S10093*        GO TO E099-EXIT.
S10093         GO TO E010-EXIT
S10093     END-IF
S10093     MOVE "L"                TO WA-WORK-SYSTEM-ID.
S10093     MOVE "BOR"              TO WA-WORK-SECTION-ID.
S10093     MOVE WA-WORK-SYSTEM     TO UT0017CA-CALL-SYSTEM.
S10093     MOVE WA-EDIT-MASK       TO UT0017CA-CALL-PROCESS-BITS.
S10093     MOVE WA-SAVE-TXN-AMT    TO UT0017CA-CALL-AMOUNT.
S10093     IF WS-SAVE-TXN-CURRENCY = SOC-NATIVE-CURRENCY
S10093        MOVE SPACES          TO UT0017CA-CALL-CURRENCY
S10093     ELSE
S10093        MOVE WS-SAVE-TXN-CURRENCY TO UT0017CA-CALL-CURRENCY.
S10093     CALL "UT0017" USING
S10093                         UT0017-CALL-AREA
S10093                         STARTAREA.
S10093     IF (BR0001-OUTPUT-ERR-MSG-NO > 99 AND < 900)
S10093     OR BR0001-OUTPUT-ERR-MSG-NO > 999
S10093        MOVE 1 TO WA-MSG-ERR-FLAG
S10093     END-IF.
S10093 E010-EXIT.
S10093     EXIT.
S10093*** Add the following procedure to access the BORM table if a
S10093***contract no has been entered in BR0001.COB.All the accounts
S10093***under that contract are processed one after the other.

S10093 B6100-UPDATE-CONB.
S10093     MOVE "CONB"           TO DBIOMOD-FILE
S10093                              DBIOMOD-SUFFIX
S10093     MOVE "REWRITE"           TO DBIOMOD-FUNCTION
S10093     MOVE "DBIO"           TO DBIOMOD-PREFIX
S10093     MOVE CONB-KEY         TO DBIOMOD-KEY
S10093     MOVE ZEROES           TO DBIOMOD-KEY-NO
S10093     MOVE SPACES           TO DBIOMOD-STATUS
S10093                              DBIOMOD-RELEASE-LOCK
S10093                              DBIOMOD-ACCESS-COMMAND
S10093                              DBIOMOD-ACCESS-COND
S10093     CALL  DBIOMODNAME USING
S10093                       DBIOMOD-FILE
S10093                       DBIOMOD-KEY
S10093                       CONB-RECORD-AREA
S10093                       DBIOMOD-FUNCTION
S10093                       DBIOMOD-KEY-NO
S10093                       DBIOMOD-RELEASE-LOCK
S10093                       DBIOMOD-ACCESS-COMMAND
S10093                       DBIOMOD-ACCESS-COND
S10093                       DBIOMOD-STATUS.
S10093     IF DBIOMOD-STAT NOT = "****"
S10093        IF DBIOMOD-STAT = "MRNF"
S10093           GO TO B6199-EXIT
S10093        END-IF
S10093        MOVE UT8500CA-FUNC-NORMAL-DISPLAY TO UT8500CA-CALL-CODE
S10093        MOVE SPACES TO UT8500CA-INPUT-MSG
S10093        STRING "BR0001:(DP110) ERROR ACCESS TABLE-" DBIOMOD-FILE
S10093               ", KEY-" DBIOMOD-KEY ", STAT-" DBIOMOD-STAT
S10093               DELIMITED BY SIZE INTO UT8500CA-INPUT-MSG
S10093        CALL "UT8500" USING UT8500CA-AREA
S10093        MOVE 188 TO BR0001-OUTPUT-ERR-MSG-NO
S10093        MOVE 1   TO WA-MSG-ERR-FLAG
S10093     END-IF.
S10093 B6199-EXIT.
S10093       EXIT.

S10093***This paragraph to be called to process remaining information if
S10093***the account is under a contract.
MVSWLW C110-CONT.
LON120     IF BR0001-BAD-DEBT-JRNL-NO NOT = ZEROES
LON120        MOVE WA-BAD-DEBT-IND-AFT TO
LON120             UT0073CA-BAD-DEBT-IND-AFTER
LON120        MOVE WA-BAD-DEBT-IND-BEF TO
LON120             UT0073CA-BAD-DEBT-IND-BEFORE
LON120        MOVE BR0001-BAD-DEBT-JRNL-NO TO
LON120             UT0073CA-CORR-JRNL
LON120     ELSE
S10093        MOVE WA-BAD-DEBT-IND-AFT TO
S10093                      UT0073CA-BAD-DEBT-IND-AFTER
S10093        MOVE WA-BAD-DEBT-IND-BEF TO
S10093                      UT0073CA-BAD-DEBT-IND-BEFORE
S10093        MOVE WA-53-52-FLAG TO UT0073CA-FLAG-53-52
S10093        IF (WA-53-54-FLAG = "Y")
S10093           MOVE 0 TO WA-INDX
S10093           PERFORM
S10093           VARYING WA-INDX FROM 1 BY 1
S10093           UNTIL WA-INDX > 5
S10093           MOVE  GLIF-GL-BUCKET-CDE(WA-INDX) TO
S10093                   UT0073CA-GL-BUCKET-CDE(WA-INDX)
S10093           MOVE  GLIF-GL-BUCKET-DUE(WA-INDX) TO
S10093                   UT0073CA-GL-BUCKET-DUE(WA-INDX)
S10093           END-PERFORM
S10093        END-IF
LON120     END-IF
S10093     IF  BR0001-17053-ACCOUNT OR  WA-MSG-ERR-FLAG = 1
LON460         PERFORM Z000-FORMT-SEND-PARA THRU Z099-EXIT
LON460         MOVE 01                 TO ST-STATUS-LEVEL-1
LON460         GO TO C110-EXIT
LON460     END-IF.
LON460*S10093  GO TO A010-CHECK-ERRORS.
MVSWLW C110-EXIT.
S10093         EXIT.
S10093***This paragraph is accessed to rewrite borm and bois records in
S10093***case a contract no has been entered and NPL processing is
S10093***completed
S10093 W600-REWRITE-MASTER.
S10093     MOVE 32                     TO DBIO-CALL-FUNC.
S10093     MOVE "BOR"                  TO DBIO-CALL-SYS-AREA.
S10093     MOVE STRT-AREA-SOC-NO       TO WA-BORM-SOC-NO.
S10093     MOVE BORM-MEMB-CUST-AC      TO WA-BORM-MEMB-CUST-AC.
S10093     MOVE WA-BORM-KEY            TO DBIO-CALL-KEY-AREA.
S10093     MOVE SPACES                 TO DBIO-CALL-STATUS.
S10093     CALL "DBIO" USING
S10093                                 DBIO-CALL-PARAM
S10093                                 DBIO-CALL-SYS-AREA
S10093                                 DBIO-CALL-KEY-AREA
S10093                                 DBIO-CALL-STATUS
S10093                                 BORM-RECORD-AREA.
S10093     IF  DBIO-CALL-STATUS NOT = "****"
S10093         MOVE UT8500CA-FUNC-NORMAL-DISPLAY TO UT8500CA-CALL-CODE
S10093         MOVE SPACES TO UT8500CA-INPUT-MSG
S10093         STRING "BR0000:(W600) PROBLEM WHILE REWRITING BORM "
S10093                  DBIO-CALL-KEY-AREA " " DBIO-CALL-STATUS
S10093             DELIMITED BY SIZE INTO UT8500CA-INPUT-MSG
S10093         CALL "UT8500" USING
S10093                             UT8500CA-AREA
S10093         MOVE DBIO-CALL-STATUS TO WA-DB-ERROR
S10093         MOVE "BORM" TO WA-DBERR-DATA-SET
S10093         MOVE 155                 TO BR0001-OUTPUT-ERR-MSG-NO
S10093         MOVE 1 TO WA-MSG-ERR-FLAG
S10093         GO TO W601-EXIT.
S10093     IF (WA-EDIT-MASK-BIT(5) = 1
S10093     AND WA-EDIT-MASK-BIT(7) = 0)
S10093         IF BOIS-FIRST-ADV-DATE = 0
S10093            MOVE BORM-ADV-DATE    TO BOIS-FIRST-ADV-DATE
S10093         END-IF
S10093     END-IF.
S10093     MOVE "REWRITE" TO DBIOMOD-FUNCTION.
S10093     PERFORM W3100-ACCESS-BOIS THRU W3199-EXIT.
S10093     IF  DBIOMOD-STAT NOT = "****"
S10093         GO TO W601-EXIT.
S10093 W601-EXIT.
S10093     EXIT.
S10093 E200-JRNL-RESTART-PARA.
S10093     IF  STRT-AREA-SOC-NO = SOC-TRAINING-SOCIETY
S10093         GO TO E201-EXIT.
S10093     IF ((STRT-AREA-TRN-NO = 17050 OR 17000 OR 17036)
S10093        AND BORM-STAT = 40)
S10093         GO TO E201-EXIT.
S10093     IF WA-DUMMY-FST-ONE = 2
S10093        MOVE "GEN"  TO JNRST-SYST
S10093     ELSE
S10093        MOVE "BOR"  TO JNRST-SYST.
S10093* INITIALISE CROSS CURRENCY INDICATOR.
S10093     MOVE STRT-AREA-XCURR-IND     TO JNRST-XCURR-IND.
S10093* BUILD GLCC BASED ON CUSTOMER SEGMENT CODE.
S10093      EVALUATE UT28-GLCC-OVERRIDE-IND
S10093      WHEN 1
S10093           MOVE UT28-GL-CLASS-CODE1   TO JNRST-GL-CLASS-CODE
S10093      WHEN 2
S10093           MOVE UT28-GL-CLASS-CODE2   TO JNRST-GL-CLASS-CODE
S10093      WHEN OTHER
S10093*       IF BORP-YOUBE
S10093*          MOVE WA-SOUR-GL-CLASS-CODE to JNRST-GL-CLASS-CODE
S10093*       ELSE
S10093        MOVE BORM-GL-CLASSIFICATION-CODE TO JNRST-GL-CLASS-CODE
S10093*       END-IF
S10093*       IF STRT-ASYNC-TPS
S10093*          MOVE BORM-GL-CLASSIFICATION-CODE TO JNRST-GL-CLASS-CODE
S10093*       END-IF
S10093      END-EVALUATE
S10093        MOVE BORP-CGL-POSTING-IND        TO JNRST-CGL-POSTING-IND.
S10093     IF STRT-AREA-TRN-NO = 17003
S10093        MOVE STRT-AREA-JRNL-NO           TO JNRST-JRNL-NO
S10093        MOVE BORM-MEMB-CUST-AC            TO JNRST-CUSM-NO
S10093        IF WA-17003-CORR-TRAN-CD NOT EQUAL 17103
S10093           IF (BORM-BAD-DEBT-IND = 50 AND
LON120               BR0001-BAD-DEBT-JRNL-NO = 0) OR
LON120              (BR0001-BAD-DEBT-JRNL-NO NOT = ZEROES AND
LON120              (WA-PREV-BAD-DEBT-IND = (52 OR 02) AND
LON120               WA-NEXT-BAD-DEBT-IND = 50))
S10093              PERFORM NPL-TO-NORMAL-PROCESS
S10093                 THRU NPL-TO-NORMAL-PROCESS-EXIT
S10093           END-IF
S10093           IF (BORM-BAD-DEBT-IND = (52 OR 02) AND
LON120               BR0001-BAD-DEBT-JRNL-NO = 0) OR
LON120              (WA-PREV-BAD-DEBT-IND = (00 OR 50) AND
LON120               WA-NEXT-BAD-DEBT-IND = (02 OR 52) AND
LON120               BR0001-BAD-DEBT-JRNL-NO NOT = ZEROES)
S10093               IF UT0073CA-FLAG-53-52 = "Y"
S10093                  CONTINUE
S10093               ELSE
S10093*                IF BOIS-921-FLAG > 0
S10093*                   PERFORM 921-TO-NPL-PROCESS
S10093*                   THRU 921-TO-NPL-PROCESS-EXIT
S10093*                ELSE
S10093                    PERFORM NORMAL-TO-NPL-PROCESS
S10093                    THRU NORMAL-TO-NPL-PROCESS-EXIT
S10093*                END-IF
S10093               END-IF
S10093           END-IF
S10093           IF (BORM-BAD-DEBT-IND = 54 AND
LON120              BR0001-BAD-DEBT-JRNL-NO = 0) OR
LON120              (WA-PREV-BAD-DEBT-IND = (52 OR 02) AND
LON120              WA-NEXT-BAD-DEBT-IND = 54 AND
LON120              BR0001-BAD-DEBT-JRNL-NO NOT = ZEROES)
S10093              PERFORM FULL-BAD-DEBT-GLIF
S10093                 THRU FULL-BAD-DEBT-GLIF-EXIT
S10093           END-IF
S10093        ELSE
S10093           IF UT0073CA-CORR-IND = 50
S10093              PERFORM NPL-TO-NORMAL-PROCESS
S10093                 THRU NPL-TO-NORMAL-PROCESS-EXIT
S10093           END-IF
S10093           IF BORM-BAD-DEBT-IND = 00 OR 50 OR 51 OR 01
S10093                               OR 61 OR 11
S10093*             IF BOIS-921-FLAG > 0
S10093*                PERFORM 921-TO-NPL-PROCESS
S10093*                   THRU 921-TO-NPL-PROCESS-EXIT
S10093*             ELSE
S10093                IF (BORM-BAD-DEBT-IND = 00
S10093                    AND UT0073CA-BAD-DEBT-IND-BEFORE = 51)
S10093                   CONTINUE
S10093                ELSE
S10093                 PERFORM NORMAL-TO-NPL-PROCESS
S10093                    THRU NORMAL-TO-NPL-PROCESS-EXIT
S10093                END-IF
S10093*             END-IF
S10093           END-IF
S10093           IF UT0073CA-CORR-IND = 53
S10093            IF UT0073CA-FLAG-53-52 = "Y"
S10093              CONTINUE
S10093            ELSE
S10093              PERFORM NPL-TO-BAD-DEBT-PROCESS
S10093                 THRU NPL-TO-BAD-DEBT-PROCESS-EXIT
S10093            END-IF
S10093           END-IF
S10093           IF UT0073CA-CORR-IND = 54
S10093              PERFORM FULL-BAD-DEBT-GLIF
S10093                 THRU FULL-BAD-DEBT-GLIF-EXIT
S10093           END-IF
S10093           IF UT0073CA-CORR-IND = 59
S10093              PERFORM COUNTERFEIT-GLIF
S10093                 THRU COUNTERFEIT-GLIF-EXIT
S10093           END-IF
S10093        END-IF
S10093        GO TO E201-EXIT
S10093     END-IF.
S10093 E201-EXIT.
S10093     EXIT.
S10093*---------------------------------------------------------
S10093 NPL-TO-NORMAL-PROCESS.
S10093     IF WA-17003-CORR-TRAN-CD NOT EQUAL 17103
LON120        IF BR0001-BAD-DEBT-JRNL-NO NOT = ZEROES
LON120           MOVE BORP-CGL-COMPONENT-1-DR
LON120                     TO UT0099CA-COMPONENT-1
LON120           MOVE BORP-CGL-COMPONENT-2-DR
LON120                     TO UT0099CA-COMPONENT-2
LON120           PERFORM NPL-BUILD-CGL THRU
LON120                   NPL-BUILD-CGL-EXIT
LON120           MOVE UT0099CA-GLCC-OUTPUT-KEY
LON120                TO WA-DEST-GL-CLASS-CODE
LON120        ELSE
S10093           MOVE BORM-GL-CLASSIFICATION-CODE
S12787                              TO WA-DEST-GL-CLASS-CODE
LON120        END-IF
S10093        IF BOIS-921-FLAG > 0
S10093           MOVE BORM-GL-CLASSIFICATION-CODE
S10093                       TO WA-DEST-GL-CLASS-CODE
S10093        ELSE
S10093           MOVE BORP-NON-CGL-COMPONENT-1
S10093                       TO UT0099CA-COMPONENT-1
S10093           MOVE BORP-NON-CGL-COMPONENT-2
S10093                       TO UT0099CA-COMPONENT-2
S10093           PERFORM NPL-BUILD-CGL THRU
S10093                NPL-BUILD-CGL-EXIT
S10093           MOVE UT0099CA-GLCC-OUTPUT-KEY
S12787                              TO WA-SOUR-GL-CLASS-CODE
S10093        END-IF
S10093     ELSE
S10093        IF BOIS-921-FLAG > 0
S10093           MOVE  BORM-GL-CLASSIFICATION-CODE
S10093                       TO WA-SOUR-GL-CLASS-CODE
S10093        ELSE
S10093           MOVE BORP-CGL-COMPONENT-1-DR
S10093                       TO UT0099CA-COMPONENT-1
S10093           MOVE BORP-CGL-COMPONENT-1-DR
S10093                       TO UT0099CA-COMPONENT-2
S10093
S10093           PERFORM NPL-BUILD-CGL THRU
S10093                NPL-BUILD-CGL-EXIT
S10093           MOVE UT0099CA-GLCC-OUTPUT-KEY
S10093                       TO WA-SOUR-GL-CLASS-CODE
S10093        END-IF
S10093        MOVE BORM-GL-CLASSIFICATION-CODE
S10093                       TO WA-DEST-GL-CLASS-CODE
S10093     END-IF
S10093     MOVE 0                   TO WA-GL-INDX
S10093                                 WA-TMP-GL-P.
S10093            MOVE WA-SOUR-GL-CLASS-CODE TO
S10093                                JNRST-GL-CLASS-CODE
S10093            MOVE WA-DEST-GL-CLASS-CODE TO
S10093                                JNRST-XFER-GL-CLASS-CODE
LON120            IF BR0001-BAD-DEBT-JRNL-NO NOT = ZEROES
LON120               MOVE BOIS-GL-BUCKET-DUE(1) TO WA-GLCC-XFER-AMOUNT
LON120            ELSE
S10093               MOVE BORM-LOAN-BAL
S10093                              TO WA-GLCC-XFER-AMOUNT
LON120            END-IF
S10093            MOVE 98924           TO JNRST-TRAN-CODE
S10093                                  LINK-IN-TXCODE
LON120            IF BR0001-BAD-DEBT-JRNL-NO NOT = ZEROES
LON120               MULTIPLY -1 BY WA-GLCC-XFER-AMOUNT
LON120            END-IF
S10093            PERFORM WRITE-TO-JOURNAL-PROCESS
S10093               THRU WRITE-TO-JOURNAL-PROCESS-EXIT.
S10093 NPL-TO-NORMAL-PROCESS-EXIT.
S10093     EXIT.
S10093 NORMAL-TO-NPL-PROCESS.
S10093     INITIALIZE JNRST-DB-DATA
S10093     IF WA-17003-CORR-TRAN-CD NOT EQUAL 17103
S12787          MOVE  13655 TO JNRST-TRAN-CODE
S12787                         LINK-IN-TXCODE
S12787                         STRT-AREA-TRN-NO
LON120          IF BR0001-BAD-DEBT-JRNL-NO NOT = ZEROES
LON120             MOVE BOIS-GL-BUCKET-DUE(2) TO WA-GLCC-XFER-AMOUNT
LON120             MULTIPLY -1 BY WA-GLCC-XFER-AMOUNT
LON120          ELSE
S12787             MOVE BORM-CAP-UNPAID-INT TO WA-GLCC-XFER-AMOUNT
LON120          END-IF
LON157*S12787   IF WA-GLCC-XFER-AMOUNT > 0
LON157          IF WA-GLCC-XFER-AMOUNT NOT = ZEROES
S12787             PERFORM Z150-BUILD-JRNL THRU Z150-EXIT
S12787             PERFORM WRITE-TO-JOURNAL-PROCESS THRU
S12787                WRITE-TO-JOURNAL-PROCESS-EXIT
S12787          END-IF
LON120          IF BR0001-BAD-DEBT-JRNL-NO NOT = ZEROES
LON120             MOVE BOIS-GL-BUCKET-DUE(3) TO WA-GLCC-XFER-AMOUNT
LON120             MULTIPLY -1 BY WA-GLCC-XFER-AMOUNT
LON120          ELSE
S12787            MOVE BOIS-UNPAID-ARRS-PRIN-BAL TO WA-GLCC-XFER-AMOUNT
LON120          END-IF
LON157*S12787   IF WA-GLCC-XFER-AMOUNT > 0
LON157          IF WA-GLCC-XFER-AMOUNT NOT = ZEROES
S12787             PERFORM Z150-BUILD-JRNL THRU Z150-EXIT
S12787             PERFORM WRITE-TO-JOURNAL-PROCESS THRU
S12787                WRITE-TO-JOURNAL-PROCESS-EXIT
S12787          END-IF
S12787          MOVE 13656    TO    JNRST-TRAN-CODE
S12787                              LINK-IN-TXCODE
S12787                              STRT-AREA-TRN-NO
LON120          IF BR0001-BAD-DEBT-JRNL-NO NOT = ZEROES
LON120             MOVE BOIS-GL-BUCKET-DUE(2) TO WA-GLCC-XFER-AMOUNT
LON120             MULTIPLY -1 BY WA-GLCC-XFER-AMOUNT
LON120          ELSE
S14071             MOVE BORM-CAP-UNPAID-INT TO WA-GLCC-XFER-AMOUNT
LON120          END-IF
LON157*S12787   IF WA-GLCC-XFER-AMOUNT > 0
LON157          IF WA-GLCC-XFER-AMOUNT NOT = ZEROES
S14071             PERFORM Z150-BUILD-JRNL THRU Z150-EXIT
S14071             PERFORM WRITE-TO-JOURNAL-PROCESS THRU
S14071                WRITE-TO-JOURNAL-PROCESS-EXIT
S14071          END-IF
LON120          IF BR0001-BAD-DEBT-JRNL-NO NOT = ZEROES
LON120             MOVE BOIS-GL-BUCKET-DUE(3) TO WA-GLCC-XFER-AMOUNT
LON120             MULTIPLY -1 BY WA-GLCC-XFER-AMOUNT
LON120          ELSE
S14071             MOVE BOIS-UNPAID-ARRS-PRIN-BAL TO WA-GLCC-XFER-AMOUNT
LON120          END-IF
LON157*S12787   IF WA-GLCC-XFER-AMOUNT > 0
LON157          IF WA-GLCC-XFER-AMOUNT NOT = ZEROES
S14071             PERFORM Z150-BUILD-JRNL THRU Z150-EXIT
S14071             PERFORM WRITE-TO-JOURNAL-PROCESS THRU
S14071                WRITE-TO-JOURNAL-PROCESS-EXIT
S14071          END-IF
S10093     ELSE
S10093        MOVE BORP-CGL-COMPONENT-1-DR
S10093                    TO UT0099CA-COMPONENT-1
S10093        MOVE BORP-CGL-COMPONENT-2-DR
S10093                    TO UT0099CA-COMPONENT-2
S10093
S10093        PERFORM NPL-BUILD-CGL THRU
S10093                NPL-BUILD-CGL-EXIT
S10093
S10093        MOVE UT0099CA-GLCC-OUTPUT-KEY TO
S10093                           JNRST-GL-CLASS-CODE
S10093                           WA-SOUR-GL-CLASS-CODE

S10093        MOVE 13609         TO JNRST-TRAN-CODE
S10093                             LINK-IN-TXCODE
S10093                             STRT-AREA-TRN-NO
S10093        PERFORM X720-CALL-UT0251
S10093        THRU X729-EXIT
S10093
S10093        IF WA-MSG-ERR-FLAG NOT =  0
S10093           GO TO NORMAL-TO-NPL-PROCESS-EXIT
S10093        END-IF
S10093        MOVE UT0251RA-CGL-COMPONENT-1-SB TO
S10093                       UT0099CA-COMPONENT-1
S10093
S10093        PERFORM NPL-BUILD-CGL THRU
S10093                NPL-BUILD-CGL-EXIT
S10093
S10093        MOVE UT0099CA-GLCC-OUTPUT-KEY  TO
S10093                       JNRST-XFER-GL-CLASS-CODE
S10093                           WA-DEST-GL-CLASS-CODE
S10093
S10093        PERFORM VARYING WA-GL-INDX FROM 1 BY 1
S10093          UNTIL WA-GL-INDX > 5
S10093         IF UT0073CA-GL-BUCKET-CDE(WA-GL-INDX) = "I"
S10093            COMPUTE WA-WORK-INT-0 ROUNDED =
S10093               (UT0073CA-GL-BUCKET-DUE(WA-GL-INDX) -
S10093               BOIS-INTEREST-DUE) * -1
S10093         END-IF
S10093        END-PERFORM
S10093        MOVE WA-WORK-INT-0 TO WA-GLCC-XFER-AMOUNT
S10093     END-IF.
S10093
S10093     IF WA-17003-CORR-TRAN-CD NOT EQUAL 17103
LON120        IF BR0001-BAD-DEBT-JRNL-NO NOT = ZEROES
LON120           MOVE BOIS-GL-BUCKET-DUE(1) TO WA-GLCC-XFER-AMOUNT
LON120           MULTIPLY -1 BY WA-GLCC-XFER-AMOUNT
LON120        ELSE
S12787           MOVE BORM-LOAN-BAL TO WA-GLCC-XFER-AMOUNT
LON120        END-IF
S10093        MOVE BORP-CGL-COMPONENT-1-DR
S10093                       TO UT0099CA-COMPONENT-1
S10093        MOVE BORP-CGL-COMPONENT-2-DR
S10093                       TO UT0099CA-COMPONENT-2
S10093
S10093        PERFORM NPL-BUILD-CGL THRU
S10093                NPL-BUILD-CGL-EXIT
S10093
S10093        MOVE UT0099CA-GLCC-OUTPUT-KEY
S10093                       TO WA-SOUR-GL-CLASS-CODE
LON120        IF BR0001-BAD-DEBT-JRNL-NO NOT = ZEROES
LON120           MOVE BORP-NON-CGL-COMPONENT-1
LON120                          TO UT0099CA-COMPONENT-1
LON120           MOVE BORP-NON-CGL-COMPONENT-2
LON120                          TO UT0099CA-COMPONENT-2
LON120           PERFORM NPL-BUILD-CGL THRU NPL-BUILD-CGL-EXIT
LON120           MOVE UT0099CA-GLCC-OUTPUT-KEY
LON120                          TO WA-DEST-GL-CLASS-CODE
LON120        ELSE
S10093           MOVE BORM-GL-CLASSIFICATION-CODE
S10093                       TO WA-DEST-GL-CLASS-CODE
LON120        END-IF
S10093     ELSE
S10093        MOVE BORP-NON-CGL-COMPONENT-1
S10093                       TO UT0099CA-COMPONENT-1
S10093        MOVE BORP-NON-CGL-COMPONENT-2
S10093                       TO UT0099CA-COMPONENT-2
S10093
S10093        PERFORM NPL-BUILD-CGL THRU
S10093                NPL-BUILD-CGL-EXIT

S10093        MOVE UT0099CA-GLCC-OUTPUT-KEY
S10093                       TO WA-SOUR-GL-CLASS-CODE
S10093        MOVE BORM-GL-CLASSIFICATION-CODE
S10093                       TO WA-DEST-GL-CLASS-CODE
S10093     END-IF
S10093     MOVE 0                   TO WA-GL-INDX.
S10093     MOVE WA-SOUR-GL-CLASS-CODE TO
S10093                          JNRST-GL-CLASS-CODE.
S10093     MOVE WA-DEST-GL-CLASS-CODE TO
S10093                                JNRST-XFER-GL-CLASS-CODE.
S10093     MOVE 98924           TO JNRST-TRAN-CODE
S10093                             LINK-IN-TXCODE.
S10093     PERFORM WRITE-TO-JOURNAL-PROCESS
S10093             THRU WRITE-TO-JOURNAL-PROCESS-EXIT.
S12787     MOVE 17003         TO STRT-AREA-TRN-NO.
S10093 NORMAL-TO-NPL-PROCESS-EXIT.
S10093     EXIT.
S10093 NPL-TO-BAD-DEBT-PROCESS.

S10093     IF WA-17003-CORR-TRAN-CD NOT EQUAL 17103
S10093        MOVE BORM-GL-CLASSIFICATION-CODE
S10093                       TO WA-SOUR-GL-CLASS-CODE
S10093        MOVE BORP-BAD-D-CGL-1
S10093                       TO UT0099CA-COMPONENT-1
S10093        MOVE BORP-BAD-D-CGL-2
S10093                       TO UT0099CA-COMPONENT-2
S10093
S10093        PERFORM NPL-BUILD-CGL THRU
S10093                NPL-BUILD-CGL-EXIT
S10093        MOVE UT0099CA-GLCC-OUTPUT-KEY
S10093                       TO WA-DEST-GL-CLASS-CODE
S10093     ELSE
S10093        MOVE BORP-BAD-D-CGL-1
S10093                       TO UT0099CA-COMPONENT-1
S10093        MOVE BORP-BAD-D-CGL-2
S10093                       TO UT0099CA-COMPONENT-2
S10093
S10093        PERFORM NPL-BUILD-CGL THRU
S10093                NPL-BUILD-CGL-EXIT
S10093
S10093        MOVE UT0099CA-GLCC-OUTPUT-KEY
S10093                       TO WA-SOUR-GL-CLASS-CODE
S10093        MOVE BORM-GL-CLASSIFICATION-CODE
S10093                       TO WA-DEST-GL-CLASS-CODE
S10093     END-IF
S10093     MOVE 0                   TO WA-GL-INDX.
S10093     PERFORM
S10093       VARYING WA-GL-INDX FROM 1 BY 1
S10093       UNTIL WA-GL-INDX > 5
S10093         IF (UT0073CA-GL-BUCKET-CDE(WA-GL-INDX) = "P" OR "I")
S10093            AND (UT0073CA-GL-BUCKET-DUE(WA-GL-INDX) NOT EQUAL 0)
S10093            MOVE WA-SOUR-GL-CLASS-CODE TO
S10093                                JNRST-GL-CLASS-CODE
S10093            MOVE WA-DEST-GL-CLASS-CODE TO
S10093                                JNRST-XFER-GL-CLASS-CODE
S10093            MOVE UT0073CA-GL-BUCKET-DUE(WA-GL-INDX)
S10093                              TO WA-GLCC-XFER-AMOUNT
S10093            MOVE 98924           TO JNRST-TRAN-CODE
S10093                                  LINK-IN-TXCODE
S10093            PERFORM WRITE-TO-JOURNAL-PROCESS
S10093               THRU WRITE-TO-JOURNAL-PROCESS-EXIT
S10093         END-IF
S10093     END-PERFORM.
S11555*
S10093 NPL-TO-BAD-DEBT-PROCESS-EXIT.
S10093     EXIT.
LON236/
S10093 FULL-BAD-DEBT-GLIF.
LON236* 921-FLAG IS NO NEED IN BOC REQUIREMENT
043076*        IF BOIS-921-FLAG > 0
043077*           MOVE BORP-921-L-CGL-1 TO UT0099CA-COMPONENT-1
043078*           MOVE BORP-921-L-CGL-2 TO UT0099CA-COMPONENT-2
043079*
043080*           MOVE UT0099CA-GLCC-OUTPUT-KEY
043081*                       TO WA-SOUR-GL-CLASS-CODE
043082*           MOVE BORM-GL-CLASSIFICATION-CODE
043083*                       TO WA-DEST-GL-CLASS-CODE
043084*           MOVE WA-SOUR-GL-CLASS-CODE TO
043085*                            JNRST-GL-CLASS-CODE
043086*           MOVE WA-DEST-GL-CLASS-CODE TO
043087*                            JNRST-XFER-GL-CLASS-CODE
043088*           MOVE BORM-LOAN-BAL   TO WA-GLCC-XFER-AMOUNT
043089*           MOVE 98924           TO JNRST-TRAN-CODE
043090*                                   LINK-IN-TXCODE
043091*           PERFORM WRITE-TO-JOURNAL-PROCESS
043092*              THRU WRITE-TO-JOURNAL-PROCESS-EXIT
S14107*        END-IF.
043094         MOVE 98922        TO JNRST-TRAN-CODE
043095                              LINK-IN-TXCODE
043096                              STRT-AREA-TRN-NO.
LON120         IF BR0001-BAD-DEBT-JRNL-NO NOT = ZEROES
LON120            MOVE BOIS-GL-BUCKET-DUE(1) TO WA-GLCC-XFER-AMOUNT
LON120            MULTIPLY -1 BY WA-GLCC-XFER-AMOUNT
LON120         ELSE
043097            MOVE BORM-LOAN-BAL TO WA-GLCC-XFER-AMOUNT
LON120         END-IF.
043098         IF WA-GLCC-XFER-AMOUNT NOT = 0
043099            PERFORM Z150-BUILD-JRNL THRU Z150-EXIT
043100            IF WA-MSG-ERR-FLAG NOT =  0
043101               GO TO FULL-BAD-DEBT-GLIF-EXIT
043102            END-IF
043103            PERFORM WRITE-TO-JOURNAL-PROCESS
043104               THRU WRITE-TO-JOURNAL-PROCESS-EXIT
043105         END-IF.
043106         MOVE 98923        TO JNRST-TRAN-CODE
043107                              LINK-IN-TXCODE
043108                              STRT-AREA-TRN-NO.
LON120         IF BR0001-BAD-DEBT-JRNL-NO NOT = ZEROES
LON120            MOVE BOIS-GL-BUCKET-DUE(1) TO WA-GLCC-XFER-AMOUNT
LON120            MULTIPLY -1 BY WA-GLCC-XFER-AMOUNT
LON120         ELSE
043109            MOVE BORM-LOAN-BAL   TO WA-GLCC-XFER-AMOUNT
LON120         END-IF.
043110         IF WA-GLCC-XFER-AMOUNT NOT = 0
043111            PERFORM Z150-BUILD-JRNL THRU Z150-EXIT
043112            IF WA-MSG-ERR-FLAG NOT =  0
043113               GO TO FULL-BAD-DEBT-GLIF-EXIT
043114            END-IF
043115            PERFORM WRITE-TO-JOURNAL-PROCESS
043116                THRU WRITE-TO-JOURNAL-PROCESS-EXIT
043117         END-IF.
043118         MOVE 98925        TO JNRST-TRAN-CODE
043119                              LINK-IN-TXCODE
043120                              STRT-AREA-TRN-NO.
LON120         IF BR0001-BAD-DEBT-JRNL-NO NOT = ZEROES
LON120            MOVE BOIS-GL-BUCKET-DUE(2) TO WA-GLCC-XFER-AMOUNT
LON120            MULTIPLY -1 BY WA-GLCC-XFER-AMOUNT
LON120         ELSE
043121            MOVE BORM-CAP-UNPAID-INT TO WA-GLCC-XFER-AMOUNT
LON120         END-IF.
043122         IF WA-GLCC-XFER-AMOUNT NOT = 0
043123            PERFORM Z150-BUILD-JRNL THRU Z150-EXIT
043124            IF WA-MSG-ERR-FLAG NOT =  0
043125               GO TO FULL-BAD-DEBT-GLIF-EXIT
043126            END-IF
043127            PERFORM WRITE-TO-JOURNAL-PROCESS
043128              THRU WRITE-TO-JOURNAL-PROCESS-EXIT
043129         END-IF.
043130         MOVE 98926        TO JNRST-TRAN-CODE
043131                              LINK-IN-TXCODE
043132                              STRT-AREA-TRN-NO.
LON120         IF BR0001-BAD-DEBT-JRNL-NO NOT = ZEROES
LON120            MOVE BOIS-GL-BUCKET-DUE(2) TO WA-GLCC-XFER-AMOUNT
LON120            MULTIPLY -1 BY WA-GLCC-XFER-AMOUNT
LON120         ELSE
043133            MOVE BORM-CAP-UNPAID-INT TO WA-GLCC-XFER-AMOUNT
LON120         END-IF.
043134         IF WA-GLCC-XFER-AMOUNT NOT = 0
043135            PERFORM Z150-BUILD-JRNL THRU Z150-EXIT
043136            IF WA-MSG-ERR-FLAG NOT =  0
043137               GO TO FULL-BAD-DEBT-GLIF-EXIT
043138            END-IF
043139            PERFORM WRITE-TO-JOURNAL-PROCESS
043140            THRU WRITE-TO-JOURNAL-PROCESS-EXIT
043141         END-IF.
043142         MOVE 98925        TO JNRST-TRAN-CODE
043143                              LINK-IN-TXCODE
043144                              STRT-AREA-TRN-NO.
LON120         IF BR0001-BAD-DEBT-JRNL-NO NOT = ZEROES
LON120            MOVE BOIS-GL-BUCKET-DUE(4) TO WA-GLCC-XFER-AMOUNT
LON120            MULTIPLY -1 BY WA-GLCC-XFER-AMOUNT
LON120         ELSE
043145            MOVE BORM-UNPAID-ARRS-INT-BAL TO WA-GLCC-XFER-AMOUNT
LON120         END-IF.
043146         IF WA-GLCC-XFER-AMOUNT NOT = 0
043147            PERFORM Z150-BUILD-JRNL THRU Z150-EXIT
043148            IF WA-MSG-ERR-FLAG NOT =  0
043149               GO TO FULL-BAD-DEBT-GLIF-EXIT
043150            END-IF
043151            PERFORM WRITE-TO-JOURNAL-PROCESS
043152              THRU WRITE-TO-JOURNAL-PROCESS-EXIT
043153         END-IF.
043154         MOVE 98926        TO JNRST-TRAN-CODE
043155                              LINK-IN-TXCODE
043156                              STRT-AREA-TRN-NO.
LON120         IF BR0001-BAD-DEBT-JRNL-NO NOT = ZEROES
LON120            MOVE BOIS-GL-BUCKET-DUE(4) TO WA-GLCC-XFER-AMOUNT
LON120            MULTIPLY -1 BY WA-GLCC-XFER-AMOUNT
LON120         ELSE
043157            MOVE BORM-UNPAID-ARRS-INT-BAL TO WA-GLCC-XFER-AMOUNT
LON120         END-IF.
043158         IF WA-GLCC-XFER-AMOUNT NOT = 0
043159            PERFORM Z150-BUILD-JRNL THRU Z150-EXIT
043160            IF WA-MSG-ERR-FLAG NOT =  0
043161               GO TO FULL-BAD-DEBT-GLIF-EXIT
043162            END-IF
043163            PERFORM WRITE-TO-JOURNAL-PROCESS
043164            THRU WRITE-TO-JOURNAL-PROCESS-EXIT
043165         END-IF.
043166         MOVE 98925        TO JNRST-TRAN-CODE
043167                              LINK-IN-TXCODE
043168                              STRT-AREA-TRN-NO.
LON120         IF BR0001-BAD-DEBT-JRNL-NO NOT = ZEROES
LON120            MOVE BOIS-GL-BUCKET-DUE(3) TO WA-GLCC-XFER-AMOUNT
LON120            MULTIPLY -1 BY WA-GLCC-XFER-AMOUNT
LON120         ELSE
043169           MOVE BOIS-UNPAID-ARRS-PRIN-BAL TO WA-GLCC-XFER-AMOUNT
LON120         END-IF.
043170         IF WA-GLCC-XFER-AMOUNT NOT = 0
043171            PERFORM Z150-BUILD-JRNL THRU Z150-EXIT
043172            IF WA-MSG-ERR-FLAG NOT =  0
043173               GO TO FULL-BAD-DEBT-GLIF-EXIT
043174            END-IF
043175            PERFORM WRITE-TO-JOURNAL-PROCESS
043176              THRU WRITE-TO-JOURNAL-PROCESS-EXIT
043177         END-IF.
043178         MOVE 98926        TO JNRST-TRAN-CODE
043179                              LINK-IN-TXCODE
043180                              STRT-AREA-TRN-NO.
LON120         IF BR0001-BAD-DEBT-JRNL-NO NOT = ZEROES
LON120            MOVE BOIS-GL-BUCKET-DUE(3) TO WA-GLCC-XFER-AMOUNT
LON120            MULTIPLY -1 BY WA-GLCC-XFER-AMOUNT
LON120         ELSE
043181            MOVE BOIS-UNPAID-ARRS-PRIN-BAL TO WA-GLCC-XFER-AMOUNT
LON120         END-IF
043182         IF WA-GLCC-XFER-AMOUNT NOT = 0
043183            PERFORM Z150-BUILD-JRNL THRU Z150-EXIT
043184            IF WA-MSG-ERR-FLAG NOT =  0
043185               GO TO FULL-BAD-DEBT-GLIF-EXIT
043186            END-IF
043187            PERFORM WRITE-TO-JOURNAL-PROCESS
043188              THRU WRITE-TO-JOURNAL-PROCESS-EXIT
043189         END-IF.
043190         MOVE 98925        TO JNRST-TRAN-CODE
043191                              LINK-IN-TXCODE
043192                              STRT-AREA-TRN-NO.
LON120         IF BR0001-BAD-DEBT-JRNL-NO NOT = ZEROES
LON120            MOVE BOIS-GL-BUCKET-DUE(5) TO WA-GLCC-XFER-AMOUNT
LON120            MULTIPLY -1 BY WA-GLCC-XFER-AMOUNT
LON120         ELSE
043193         MOVE BOIS-UNPAID-INT-ARRS-PRIN-BAL TO WA-GLCC-XFER-AMOUNT.
LON120         END-IF.
043194         IF WA-GLCC-XFER-AMOUNT NOT = 0
043195            PERFORM Z150-BUILD-JRNL THRU Z150-EXIT
043196            IF WA-MSG-ERR-FLAG NOT =  0
043197               GO TO FULL-BAD-DEBT-GLIF-EXIT
043198            END-IF
043199            PERFORM WRITE-TO-JOURNAL-PROCESS
043200              THRU WRITE-TO-JOURNAL-PROCESS-EXIT
043201         END-IF.
043202         MOVE 98926        TO JNRST-TRAN-CODE
043203                              LINK-IN-TXCODE
043204                              STRT-AREA-TRN-NO.
LON120         IF BR0001-BAD-DEBT-JRNL-NO NOT = ZEROES
LON120            MOVE BOIS-GL-BUCKET-DUE(5) TO WA-GLCC-XFER-AMOUNT
LON120            MULTIPLY -1 BY WA-GLCC-XFER-AMOUNT
LON120         ELSE
043205         MOVE BOIS-UNPAID-INT-ARRS-PRIN-BAL TO WA-GLCC-XFER-AMOUNT.
LON120         END-IF.
043206         IF WA-GLCC-XFER-AMOUNT NOT = 0
043207            PERFORM Z150-BUILD-JRNL THRU Z150-EXIT
043208            IF WA-MSG-ERR-FLAG NOT =  0
043209               GO TO FULL-BAD-DEBT-GLIF-EXIT
043210            END-IF
043211            PERFORM WRITE-TO-JOURNAL-PROCESS
043212              THRU WRITE-TO-JOURNAL-PROCESS-EXIT
043213         END-IF.
043214         MOVE 17003 TO STRT-AREA-TRN-NO.
S10093 FULL-BAD-DEBT-GLIF-EXIT.
S10093     EXIT.
LON236/
S10093 COUNTERFEIT-GLIF.
S10093     IF WA-17003-CORR-TRAN-CD NOT EQUAL 17103
S10093*       IF BOIS-921-FLAG > 0
S10093*          MOVE BORP-921-L-CGL-1 TO UT0099CA-COMPONENT-1
S10093*          MOVE BORP-921-L-CGL-2 TO UT0099CA-COMPONENT-2
S10093*       ELSE
S10093           MOVE BORP-BAD-D-CGL-1 TO UT0099CA-COMPONENT-1
S10093           MOVE BORP-BAD-D-CGL-2 TO UT0099CA-COMPONENT-2
S10093*       END-IF
S10093        PERFORM NPL-BUILD-CGL THRU
S10093                NPL-BUILD-CGL-EXIT
S10093        MOVE UT0099CA-GLCC-OUTPUT-KEY
S10093                       TO WA-SOUR-GL-CLASS-CODE
S10093        MOVE BORM-GL-CLASSIFICATION-CODE
S10093                       TO WA-DEST-GL-CLASS-CODE
S10093     ELSE
S10093        MOVE BORP-COUNTERFEIT-CGL-1
S10093                       TO UT0099CA-COMPONENT-1
S10093        MOVE BORP-COUNTERFEIT-CGL-2
S10093                       TO UT0099CA-COMPONENT-2
S10093        PERFORM NPL-BUILD-CGL THRU
S10093                NPL-BUILD-CGL-EXIT
S10093        MOVE BORM-GL-CLASSIFICATION-CODE
S10093                       TO WA-SOUR-GL-CLASS-CODE
S10093        MOVE UT0099CA-GLCC-OUTPUT-KEY
S10093                       TO WA-DEST-GL-CLASS-CODE
S10093     END-IF
S10093     MOVE 0                   TO WA-GL-INDX.
S10093     PERFORM
S10093       VARYING WA-GL-INDX FROM 1 BY 1
S10093       UNTIL WA-GL-INDX > 5
S10093         IF (BOIS-GL-BUCKET-CDE(WA-GL-INDX) = "I" OR "P" OR "B")
S10093         AND (BOIS-GL-BUCKET-DUE(WA-GL-INDX) NOT EQUAL 0)
S10093            MOVE WA-SOUR-GL-CLASS-CODE TO
S10093                                JNRST-GL-CLASS-CODE
S10093            MOVE WA-DEST-GL-CLASS-CODE TO
S10093                                JNRST-XFER-GL-CLASS-CODE
S10093            IF WA-17003-CORR-TRAN-CD = 17103
S10093               COMPUTE WA-GLCC-XFER-AMOUNT =
S10093                      BOIS-GL-BUCKET-DUE(WA-GL-INDX) * -1
S10093            ELSE
S10093               MOVE BOIS-GL-BUCKET-DUE(WA-GL-INDX)
S10093                              TO WA-GLCC-XFER-AMOUNT
S10093            END-IF
S10093            MOVE 98924           TO JNRST-TRAN-CODE
S10093                                  LINK-IN-TXCODE
S10093            PERFORM WRITE-TO-JOURNAL-PROCESS
S10093               THRU WRITE-TO-JOURNAL-PROCESS-EXIT
S10093         END-IF
S10093     END-PERFORM.
S10093 COUNTERFEIT-GLIF-EXIT.
S10093     EXIT.
LON236* 921 FLAG IS NO NEED IN BOC
S10093*921-TO-NPL-PROCESS.
S10093*    INITIALIZE JNRST-DB-DATA
S10093*    IF WA-17003-CORR-TRAN-CD NOT EQUAL 17103
S10093*       MOVE BORP-CGL-COMPONENT-1-DR
S10093*                   TO UT0099CA-COMPONENT-1
S10093*       MOVE BORP-CGL-COMPONENT-2-DR
S10093*                   TO UT0099CA-COMPONENT-2
S10093*
S10093*       PERFORM NPL-BUILD-CGL THRU
S10093*               NPL-BUILD-CGL-EXIT
S10093*
S10093*       MOVE UT0099CA-GLCC-OUTPUT-KEY TO
S10093*                          JNRST-GL-CLASS-CODE
S10093*                          WA-SOUR-GL-CLASS-CODE
S10093*
S10093*       MOVE 13509         TO JNRST-TRAN-CODE
S10093*                            LINK-IN-TXCODE
S10093*                            STRT-AREA-TRN-NO
S10093*
S10093*       PERFORM X700-CALL-UT0250 THRU X701-EXIT
S10093*
S10093*       IF WA-MSG-ERR-FLAG = 1
S10093*          GO TO 921-TO-NPL-PROCESS-EXIT
S10093*       ELSE
S10093*          MOVE LINK-EDIT-TABLE TO JNRST-EDIT-MASK
S10093*          MOVE LINK-SIGN       TO STRT-AREA-SIGN
S10093*       END-IF
S10093*
S10093*       PERFORM X720-CALL-UT0251
S10093*       THRU X729-EXIT
S10093*
S10093*       IF WA-MSG-ERR-FLAG NOT =  0
S10093*          GO TO 921-TO-NPL-PROCESS-EXIT
S10093*       END-IF
S10093*       MOVE UT0251RA-CGL-COMPONENT-1-SB TO
S10093*                      UT0099CA-COMPONENT-1
S10093*
S10093*       PERFORM NPL-BUILD-CGL THRU
S10093*               NPL-BUILD-CGL-EXIT
S10093*       MOVE UT0099CA-GLCC-OUTPUT-KEY  TO
S10093*                      JNRST-XFER-GL-CLASS-CODE
S10093*                      WA-DEST-GL-CLASS-CODE
S10093*       COMPUTE WA-WORK-INT-0 ROUNDED = BORM-INT-ACCR
S10093*                               + BORM-INT-ADJUSTMENT
S10093*       MOVE WA-WORK-INT-0 TO WA-GLCC-XFER-AMOUNT
S10093*    ELSE
S10093*       MOVE BORP-CGL-COMPONENT-1-DR
S10093*                   TO UT0099CA-COMPONENT-1
S10093*       MOVE BORP-CGL-COMPONENT-2-DR
S10093*                   TO UT0099CA-COMPONENT-2
S10093*
S10093*       PERFORM NPL-BUILD-CGL THRU
S10093*               NPL-BUILD-CGL-EXIT
S10093*
S10093*       MOVE UT0099CA-GLCC-OUTPUT-KEY TO
S10093*                          JNRST-GL-CLASS-CODE
S10093*                          WA-SOUR-GL-CLASS-CODE
S10093*
S10093*       MOVE 13609         TO JNRST-TRAN-CODE
S10093*                            LINK-IN-TXCODE
S10093*                            STRT-AREA-TRN-NO
S10093*       PERFORM X720-CALL-UT0251
S10093*       THRU X729-EXIT
S10093*
S10093*       IF WA-MSG-ERR-FLAG NOT =  0
S10093*          GO TO 921-TO-NPL-PROCESS-EXIT
S10093*       END-IF
S10093*       MOVE UT0251RA-CGL-COMPONENT-1-SB TO
S10093*                      UT0099CA-COMPONENT-1
S10093*
S10093*       PERFORM NPL-BUILD-CGL THRU
S10093*               NPL-BUILD-CGL-EXIT
S10093*
S10093*       MOVE UT0099CA-GLCC-OUTPUT-KEY  TO
S10093*                      JNRST-XFER-GL-CLASS-CODE
S10093*                      WA-DEST-GL-CLASS-CODE
S10093*       COMPUTE WA-WORK-INT-0 ROUNDED =
S10093*           (BORM-INT-ACCR + BORM-INT-ADJUSTMENT) * -1
S10093*
S10093*       MOVE WA-WORK-INT-0 TO WA-GLCC-XFER-AMOUNT
S10093*    END-IF.
S10093*
S10093*
S10093*    PERFORM WRITE-TO-JOURNAL-PROCESS
S10093*              THRU WRITE-TO-JOURNAL-PROCESS-EXIT
S10093*    MOVE 17003         TO STRT-AREA-TRN-NO
S10093*S10093     IF WA-17003-CORR-TRAN-CD <> 17103
S10093*    IF WA-17003-CORR-TRAN-CD NOT EQUAL 17103
S10093*       IF NOT BORP-YOUBE
S10093*          MOVE 0   TO BORM-INT-ACCR
S10093*          MOVE 0   TO BORM-INT-ADJUSTMENT
S10093*       END-IF
S10093*       MOVE BORP-CGL-COMPONENT-1-DR
S10093*                      TO UT0099CA-COMPONENT-1
S10093*       MOVE BORP-CGL-COMPONENT-2-DR
S10093*                      TO UT0099CA-COMPONENT-2
S10093*
S10093*       PERFORM NPL-BUILD-CGL THRU
S10093*               NPL-BUILD-CGL-EXIT
S10093*
S10093*       MOVE UT0099CA-GLCC-OUTPUT-KEY
S10093*                      TO WA-SOUR-GL-CLASS-CODE
S10093*       MOVE BORM-GL-CLASSIFICATION-CODE
S10093*                      TO WA-DEST-GL-CLASS-CODE
S10093*    ELSE
S10093*       MOVE BORP-CGL-COMPONENT-1-DR
S10093*                      TO UT0099CA-COMPONENT-1
S10093*       MOVE BORP-CGL-COMPONENT-2-DR
S10093*                      TO UT0099CA-COMPONENT-2
S10093*
S10093*       PERFORM NPL-BUILD-CGL THRU
S10093*               NPL-BUILD-CGL-EXIT
S10093*
S10093*       MOVE UT0099CA-GLCC-OUTPUT-KEY
S10093*                      TO WA-SOUR-GL-CLASS-CODE
S10093*       MOVE BORM-GL-CLASSIFICATION-CODE
S10093*                      TO WA-DEST-GL-CLASS-CODE
S10093*    END-IF
S10093*    MOVE 0                   TO WA-GL-INDX.
S10093*    PERFORM
S10093*      VARYING WA-GL-INDX FROM 1 BY 1
S10093*      UNTIL WA-GL-INDX > 5
S10093*        IF UT0073CA-GL-BUCKET-CDE(WA-GL-INDX) = "I"
S10093*S10093     AND UT0073CA-GL-BUCKET-DUE(WA-GL-INDX) <> 0
S10093*           AND UT0073CA-GL-BUCKET-DUE(WA-GL-INDX) NOT EQUAL 0
S10093*S10093     IF WA-17003-CORR-TRAN-CD <> 17103
S10093*           IF WA-17003-CORR-TRAN-CD NOT EQUAL 17103
S10093*               MOVE WA-SOUR-GL-CLASS-CODE TO
S10093*                               JNRST-GL-CLASS-CODE
S10093*               MOVE WA-DEST-GL-CLASS-CODE TO
S10093*                               JNRST-XFER-GL-CLASS-CODE
S10093*               MOVE UT0073CA-GL-BUCKET-DUE(WA-GL-INDX)
S10093*                             TO WA-GLCC-XFER-AMOUNT
S10093*           ELSE
S10093*               MOVE BORP-921-L-CGL-1
S10093*                      TO UT0099CA-COMPONENT-1
S10093*               MOVE BORP-921-L-CGL-2
S10093*                      TO UT0099CA-COMPONENT-2
S10093*               PERFORM NPL-BUILD-CGL THRU
S10093*               NPL-BUILD-CGL-EXIT
S10093*               MOVE UT0099CA-GLCC-OUTPUT-KEY
S10093*                      TO JNRST-XFER-GL-CLASS-CODE
S10093*               MOVE WA-DEST-GL-CLASS-CODE
S10093*                      TO JNRST-GL-CLASS-CODE
S10093*               COMPUTE WA-GLCC-XFER-AMOUNT =
S10093*                       UT0073CA-GL-BUCKET-DUE(WA-GL-INDX)
S10093*                           * -1
S10093*           END-IF
S10093*           MOVE 98921        TO JNRST-TRAN-CODE
S10093*                                  LINK-IN-TXCODE
S10093*           PERFORM WRITE-TO-JOURNAL-PROCESS
S10093*              THRU WRITE-TO-JOURNAL-PROCESS-EXIT
S10093*        END-IF
S10093*    END-PERFORM.
S10093*
S10093*921-TO-NPL-PROCESS-EXIT.
S10093*    EXIT.
S10093  NPL-BUILD-CGL.
S10093* GL CLASSIFICATION CODE IS BUILT HERE. (ANZ023).
S10093*
S10093*    MOVE STRT-AREA-BRANCH        TO UT0099CA-BRANCH.
S10093     MOVE BORM-BR-NO              TO UT0099CA-BRANCH.
S10093*    MOVE BORP-CGL-COMPONENT-1-DR TO UT0099CA-COMPONENT-1.
S10093*    MOVE BORP-CGL-COMPONENT-2-DR TO UT0099CA-COMPONENT-2.
S10093     MOVE CUSM-LANGUAGE-CODE TO UT0099CA-SEGMENT.
S10093     MOVE CUSM-SEGMENT-CODE  TO UT0099CA-GROUP-CODE.
S10093     MOVE CUSM-GROUP-CODE    TO UT0099CA-CURRENCY
S10093     MOVE BORP-CURRENCY-IND
S10093                                  TO UT0099CA-CURRENCY.
S10093     MOVE SOC-GL-CLASS-CODE-STRUCTURE TO
S10093          UT0099CA-GLCC-STRUCTURE.
S10093     CALL "UT0099" USING
S10093                 UT0099CA-AREA.
LON527     IF UT0099CA-RETURN-STATUS NOT = 0
LON527        MOVE 2083  TO BR0001-OUTPUT-ERR-MSG-NO
LON527        MOVE 1 TO WA-MSG-ERR-FLAG
LON527        GO TO NPL-BUILD-CGL-EXIT
LON527     END-IF.
LON527*
S10093 NPL-BUILD-CGL-EXIT.
S10093     EXIT.
S10093 X700-CALL-UT0250.
S10093     MOVE 02               TO LINK-IN-CALL-CODE.
S10093     MOVE STRT-AREA-SOC-NO TO LINK-IN-SOC-NO.
S10093     CALL "UT0250" USING
S10093                         UT0250-CALL-AREA.
S10093     IF LINK-OUT-STAT NOT = "****"
S10093        MOVE 100 TO BR0001-OUTPUT-ERR-MSG-NO
S10093        MOVE 1   TO WA-MSG-ERR-FLAG
S10093        MOVE UT8500CA-FUNC-NORMAL-DISPLAY TO UT8500CA-CALL-CODE
S10093        MOVE SPACES TO UT8500CA-INPUT-MSG
S10093        MOVE LINK-IN-TXCODE TO WA-STORE-TRN-NO
S10093        STRING "BR0001:(X700) INVALID TRAN CODE " WA-STORE-TRN-NO
S10093           DELIMITED BY SIZE INTO UT8500CA-INPUT-MSG
S10093        CALL "UT8500" USING
S10093
S10093                            UT8500CA-AREA
LON615        GO  TO X701-EXIT
LON615     END-IF.
S10093*
S10093 X701-EXIT.
S10093     EXIT.
LON236/
S10093 X720-CALL-UT0251.
S10093/
S10093     MOVE 2                  TO UT0251CA-CALL-CODE.
S10093     MOVE STRT-AREA-SOC-NO   TO UT0251CA-SOC-NO.
S10093     MOVE STRT-AREA-TRN-NO   TO UT0251CA-TRAN-CODE.
S10093     CALL "UT0251" USING UT0251CA-CALL-AREA.
S10093     IF UT0251CA-CALL-STATUS  NOT = "****"
LON615        MOVE 100 TO BR0001-OUTPUT-ERR-MSG-NO
LON615        MOVE 1   TO WA-MSG-ERR-FLAG
LON615        MOVE 0   TO LINK-IN-TXCODE
S10093        STRING PROGNAME ":(X720) UT0251 ERROR"
S10093             ",FUNC=" UT0251CA-CALL-CODE
S10093             ",STAT=" UT0251CA-CALL-STATUS
S10093             ",KEY="  UT0251CA-CALL-KEY
S10093        DELIMITED BY SIZE INTO UT8500CA-INPUT-MSG
S10093        CALL "UT8500" USING UT8500CA-AREA
S10093        GO TO X729-EXIT
S10093     END-IF.
S10093
S10093 X729-EXIT.
S10093     EXIT.
S10093/
S10093 WRITE-TO-JOURNAL-PROCESS.
S10093     IF WA-SOUR-GL-CLASS-CODE = WA-DEST-GL-CLASS-CODE
S10093        GO TO WRITE-TO-JOURNAL-PROCESS-EXIT
S10093     END-IF.
S10093
S10093     MOVE BORP-CGL-POSTING-IND  TO JNRST-CGL-POSTING-IND.
S10093     MOVE "BOR"                 TO JNRST-SYST.
S10093     MOVE STRT-AREA-MSG-TIME    TO JNRST-TIME.
S10093     MOVE STRT-AREA-SOC-NO      TO JNRST-SOC.
S10093     MOVE BOIS-MEMB-CUST-AC     TO JNRST-CUSM-NO.
S10093     MOVE STRT-AREA-BRANCH      TO JNRST-BRCH.
S10093     MOVE 00                    TO JNRST-TRN-TYP.
S10093     MOVE STRT-AREA-TERM-NO     TO JNRST-TERMNL.
S10093     MOVE STRT-AREA-TELL-NO     TO JNRST-TELLER.
S10093     PERFORM X700-CALL-UT0250
S10093        THRU X701-EXIT.
S10093
S10093     IF WA-MSG-ERR-FLAG NOT =  0
S10093        GO TO WRITE-TO-JOURNAL-PROCESS-EXIT
S10093     END-IF.
S10093
S10093     MOVE LINK-EDIT-TABLE       TO JNRST-EDIT-MASK.
S10093     MOVE BORM-CURRENCY-IND     TO JNRST-CURRENCY-CODE.
S10093     MOVE STRT-AREA-POST-DT     TO JNRST-POST-DATE.
S10093     MOVE STRT-AREA-POST-DT     TO JNRST-TRAN-DATE.
S10093     MOVE STRT-AREA-JRNL-NO     TO JNRST-JRNL-NO.
S10093     MOVE BORM-BR-NO            TO JNRST-HOME-BRNCH.
S10093     MOVE BORM-ACT-TYPE         TO JNRST-ACC-TYPE.
S10093     MOVE BORM-CAT              TO JNRST-INT-CAT.
S10093     MOVE WA-GLCC-XFER-AMOUNT   TO JNRST-AMOUNT.
S10093     MOVE BORM-CURRENCY-IND     TO JNRST-FROM-CURRENCY-CODE
S10093                                   JNRST-TO-CURRENCY-CODE.
S10093     MOVE 1                     TO JNRST-FLAG.
S10093     MOVE ZEROES                TO JNRST-NEW-ACC-FLG
S10093                                   JNRST-LIM-TOL-FLAG
S10093                                   JNRST-BOR-GROUP.
S10093     MOVE SPACES                TO JNRST-ATM-SAF-FLAG.
S12787        PERFORM X720-CALL-UT0251
S12787        THRU X729-EXIT.
S12787
S12787        IF WA-MSG-ERR-FLAG NOT =  0
S12787           GO TO WRITE-TO-JOURNAL-PROCESS-EXIT
S12787        END-IF
S12787     IF UT0251RA-CGL-QUALIFIER = "Y"
S14107        IF JNRST-TRAN-CODE = 98922 OR 98923 OR 98925 OR 98926
S14107           MOVE "02" TO JNRST-CALL-BAD-DEBT-STAT
S14107        ELSE
S12787          IF JNRST-TRAN-CODE = 13655 OR 13656
S13844                        OR (  JNRST-TRAN-CODE > 98970 AND
S13844                        JNRST-TRAN-CODE < 98987 )
S12787             MOVE "00" TO JNRST-CALL-BAD-DEBT-STAT
S12787          ELSE
S12787            MOVE BORM-BAD-DEBT-IND TO JNRST-CALL-BAD-DEBT-STAT
S12787          END-IF
S14107        END-IF
S12787     END-IF.
S10093
S10093*--- GET BASE CURRENCY EQUIVALENT FOR ACCOUNT BALANCE
S10093     IF BORM-CURRENCY-IND NOT = SOC-NATIVE-CURRENCY
S10093        MOVE UT0296-CALL-AREA         TO WA-SAVE-UT0296CA-AREA
S10093        MOVE 3                        TO UT0296CA-CALL-CODE
S10093        MOVE "LON"                    TO UT0296CA-SYSTEM
S10093        MOVE BORM-CURRENCY-IND        TO UT0296CA-FROM-CURRENCY
S10093                                         UT0296CA-TO-CURRENCY
S10093        MOVE WA-GLCC-XFER-AMOUNT      TO UT0296CA-FROM-AMOUNT
S10093                                         UT0296CA-TO-AMOUNT
S10093        MOVE "ZZZZ"    TO UT0296CA-RATE-VERSION
S10093        MOVE 01        TO UT0296CA-RATE-TYPE
S10093        MOVE SPACES    TO UT0296CA-USER-CODES-LON
S10093        MOVE ZEROES    TO UT0296CA-COMMISSION
S10093                          UT0296CA-CHANGE
S10093                          UT0296CA-BACKDATED-DATE
S10093                          UT0296CA-FROM-BASE-EQUIVALENT
S10093        INITIALIZE        UT0296CA-MULTI-FROM-AMOUNTS
S10093        CALL "UT0296" USING
S10093                          UT0296-CALL-AREA
LON619*S10093                          STRT-AREA-DATA
LON619                          STARTAREA
S10093                          SOC-PARAMS
S10093
S10093        IF UT0296CA-CALL-STATUS NOT = 0
S10093           MOVE UT0296CA-CALL-STATUS  TO BR0001-OUTPUT-ERR-MSG-NO
S10093           MOVE 1                     TO WA-MSG-ERR-FLAG
S10093           GO TO WRITE-TO-JOURNAL-PROCESS-EXIT
S10093        END-IF
S10093
S10093        MOVE UT0296RA-FROM-RATE       TO JNRST-FROM-CURRENCY-RATE
S10093        MOVE UT0296RA-EQUIV-BASE-AMOUNT TO
S10093                                       JNRST-FROM-BASE-EQUIVALENT
S10093                                       JNRST-TO-BASE-EQUIVALENT
S10093        MOVE UT0296CA-RATE-TYPE       TO JNRST-RATE-TYPE
S10093        MOVE BORM-CURRENCY-IND        TO JNRST-FROM-CURRENCY-CODE
S10093                                         JNRST-TO-CURRENCY-CODE
S10093     END-IF.
S10093*--- MOVE SAVED UT0296 AREA BACK TO ORIGINAL BUFFER.
S10093     MOVE WA-SAVE-UT0296CA-AREA       TO UT0296-CALL-AREA.
S10093
S10093     PERFORM Z000-WRITE-JRNL THRU Z009-EXIT.
S10093
S10093 WRITE-TO-JOURNAL-PROCESS-EXIT.
S10093     EXIT.
S10093/
S10093 Z000-WRITE-JRNL.
S10093*----------------------------------------------------------------
S10093*       IF CLOSED ACCOUNT AND STATEMENT REQUIRED, WRITE THE
S10093*       ENQUIRY HEADER TO THE JOURNAL, TO BE USED BY SECONDARY
S10093*       UPDATE FOR STATEMENT PROCESSING.
S10093*----------------------------------------------------------------
S10093     IF STRT-AREA-TRN-NO = 16039 OR 16059
S10093         MOVE WA-INPUT-AMOUNT TO JNRST-AMOUNT
S10093                                 JNRST-TO-CURR-AMOUNT
S10093         MOVE WA-MULTI-BASE-CURR-AMT TO
S10093                JNRST-FROM-BASE-EQUIVALENT
S10093                JNRST-TO-BASE-EQUIVALENT
S10093         MOVE BORM-CURRENCY-IND TO JNRST-CURRENCY-CODE
S10093                                   JNRST-FROM-CURRENCY-CODE
S10093                                   JNRST-TO-CURRENCY-CODE.
S10093
S10093        MOVE 0 TO JNRST-DEP-LON-STMT-HDR-NO
S10093        MOVE STRT-AREA-XCURR-IND      TO JNRST-XCURR-IND.
S10093        IF WA-EDIT-MASK-BIT(25) = 1
S10093           IF BORM-STATEMENT-FREQUENCY = SPACE
S10093              IF BORP-STATEMENT-FREQUENCY NOT = "N"
S10093                 MOVE 051 TO JNRST-DEP-LON-STMT-HDR-NO
S10093              END-IF
S10093           ELSE
S10093              IF BORM-STATEMENT-FREQUENCY NOT = "N"
S10093                 MOVE 051 TO JNRST-DEP-LON-STMT-HDR-NO
S10093              END-IF
S10093           END-IF
S10093        END-IF.
S10093
S10093        IF STRT-ASYNC-TPS
S10093           IF JNRST-AMOUNT > 0 AND
S10093                  STRT-AREA-SIGN = "-"
S10093           AND STRT-AREA-SIGN = LINK-SIGN
S10093           AND JNRST-SYST     = "BOR"
S10093               COMPUTE JNRST-AMOUNT = JNRST-AMOUNT * -1
S10093               MOVE JNRST-AMOUNT TO JNRST-TO-CURR-AMOUNT
S10093            ELSE
S10093               MOVE JNRST-AMOUNT TO JNRST-TO-CURR-AMOUNT
S10093            END-IF
S10093        END-IF
S10093
S10093        IF JNRST-TRAN-CODE = 13528 OR 13628
S10093           MULTIPLY -1 BY JNRST-AMOUNT
S10093        END-IF.
S10093
S10093        IF  BORP-LOAN-FR-OBANK = "Y"  AND
S10093           (BORP-CAPN-METHOD = "X"  OR
S10093             BORM-CAPN-METHOD = "X" )
S10093             IF JNRST-AMOUNT > 0
S10093                  MULTIPLY -1  BY  JNRST-AMOUNT
S10093             END-IF
S10093        END-IF.
S10093
S10093* Internal txn code not to have 0 as JNRST-TRANSFER-TXN-BRANCH for
S10093        IF ST-LOAN-BACKDATED-CORR-YES
S10093           MOVE STRT-AREA-BRANCH
S10093                TO JNRST-TRANSFER-TXN-BRANCH
S10093        END-IF.
S10093
S10093*
S10093        IF JNRST-SYST = "BOR"
S10093           MOVE BOIS-FUND-AGREEMENT-NO TO
S10093                                        JNRST-FUND-AGREEMENT-CODE
S10093           MOVE BOIS-OFF-BAL-FUND-PERC TO JNRST-OFF-BAL-FUND-PERC
S10093        END-IF.
S10093*
S10093
S10093        MOVE 2    TO SY0010CA-FUNC
S10093        CALL "SY0010"   USING
S10093                               SY0010CA-COMMON-AREA
S10093                               WA-JRNL-CALL-AREA.
S10093        IF  SY0010CA-STATUS NOT = 0
S10093            MOVE 155 TO BR0001-OUTPUT-ERR-MSG-NO
S10093            MOVE 1 TO WA-MSG-ERR-FLAG
S10093            MOVE "JRNL" TO WA-DBERR-DATA-SET
S10093            MOVE "ERR" TO WA-DB-ERROR
LON619        END-IF.
S10093*
S10093 Z009-EXIT.
S10093     EXIT.
S10093
S10093     COPY GLCCPROC IN LIBRYMIS.
LON236*
S12787 C6450-BILL-BUCKET.
S12787     INITIALIZE   BORV-01-DETAIL.
LON120     INITIALIZE   JNRST-DB-DATA.
LON135     MOVE  0  TO  WA-WORK-AMOUNT-1
LON135                  WA-WORK-AMOUNT-2
LON135                  WA-WORK-AMOUNT-3.
LON387     MOVE  0      TO    WA-RPYV-AMT(1)
LON387                        WA-RPYV-AMT(2)
LON387                        WA-RPYV-AMT(3)
LON387                        WA-RPYV-AMT(4).
LON387     MOVE  SPACES   TO  WA-RPYV-CODE(1)
LON387                        WA-RPYV-CODE(2)
LON387                        WA-RPYV-CODE(3)
LON387                        WA-RPYV-CODE(4).
LON387     MOVE ZEROES    TO  WA-RPYV-BUK(1)
LON387                        WA-RPYV-BUK(2)
LON387                        WA-RPYV-BUK(3)
LON387                        WA-RPYV-BUK(4).
LON387     PERFORM C700-RPYV-ACCESS THRU C799-EXIT.
LON387     IF WA-MSG-ERR-FLAG = 1
LON387        GO TO C6450-EXIT
LON387     END-IF.
LON120     IF BR0001-BAD-DEBT-JRNL-NO > 0 AND
LON120        BOIS-BUCKET-DUE(1) > 0
LON120        MOVE  13700      TO WA-TXN
LON120                            JNRST-TRAN-CODE
LON120                            LINK-IN-TXCODE
LON120                            STRT-AREA-TRN-NO
LON120        MOVE  BOIS-BUCKET-DUE(1) TO WA-WORK-AMOUNT-3
LON120        PERFORM C6460-SETUP-PENALTY-TXN THRU C6469-EXIT
LON120        IF WA-MSG-ERR-FLAG = 1
LON120           GO TO C6450-EXIT
LON120        END-IF
LON120        SUBTRACT W900-TXN-AMOUNT FROM BORM-CAP-UNPAID-INT
LON120        ADD  W900-TXN-AMOUNT TO  BORM-INT-ACCR
LON120     ELSE
S12787        IF BORM-INT-ACCR > 0
LON512*LON120           DISPLAY "BEFORE BILLING"
S12787           MOVE  13600      TO WA-TXN
S12787                               JNRST-TRAN-CODE
S12787                               LINK-IN-TXCODE
S12787                               STRT-AREA-TRN-NO
S12787           MOVE  BORM-INT-ACCR TO WA-WORK-AMOUNT-3
S12787           PERFORM C6460-SETUP-PENALTY-TXN THRU C6469-EXIT
S12787           IF WA-MSG-ERR-FLAG = 1
S12787              GO TO C6450-EXIT
S12787           ELSE
S12787              ADD W900-TXN-AMOUNT TO BORM-CAP-UNPAID-INT
S12787              MOVE 0 TO BORM-INT-ACCR
S12787           END-IF
LON120        END-IF
S12787     END-IF.
LON120     IF BR0001-BAD-DEBT-JRNL-NO > 0 AND
LON120        BOIS-BUCKET-DUE(2) > 0
LON120        INITIALIZE JNRST-DB-DATA
LON120        MOVE 13702       TO  WA-TXN
LON120                             JNRST-TRAN-CODE
LON120                             LINK-IN-TXCODE
LON120                             STRT-AREA-TRN-NO
LON120        MOVE BOIS-BUCKET-DUE(2) TO WA-WORK-AMOUNT-3
LON120        PERFORM C6460-SETUP-PENALTY-TXN THRU C6469-EXIT
LON120        IF WA-MSG-ERR-FLAG = 1
LON120           GO TO C6450-EXIT
LON120        END-IF
LON120        SUBTRACT BOIS-BUCKET-DUE(2) FROM
LON120                 BOIS-UNPAID-ARRS-PRIN-BAL
LON120        ADD BOIS-BUCKET-DUE(2) TO BORM-ARR-INT-ACCR
LON120     ELSE
S12787        IF BORM-ARR-INT-ACCR > 0
S12787           INITIALIZE JNRST-DB-DATA
S12787           MOVE  13602      TO  WA-TXN
S12787                                JNRST-TRAN-CODE
S12787                                LINK-IN-TXCODE
S12787                                STRT-AREA-TRN-NO
S12787           MOVE  BORM-ARR-INT-ACCR TO WA-WORK-AMOUNT-3
S12787           PERFORM C6460-SETUP-PENALTY-TXN THRU C6469-EXIT
S12787           IF WA-MSG-ERR-FLAG = 1
S12787              GO TO C6450-EXIT
S12787           ELSE
S12787              ADD W900-TXN-AMOUNT TO BOIS-UNPAID-ARRS-PRIN-BAL
S12787              MOVE 0 TO BORM-ARR-INT-ACCR
S12787           END-IF
LON120        END-IF
S12787     END-IF.
LON120     IF BR0001-BAD-DEBT-JRNL-NO > 0 AND
LON120        BOIS-BUCKET-DUE(3) > 0
LON120        INITIALIZE JNRST-DB-DATA
LON120        MOVE  13703      TO  WA-TXN
LON120                             JNRST-TRAN-CODE
LON120                             LINK-IN-TXCODE
LON120                             STRT-AREA-TRN-NO
LON120        MOVE BOIS-BUCKET-DUE(3) TO WA-WORK-AMOUNT-3
LON120        PERFORM C6460-SETUP-PENALTY-TXN THRU C6469-EXIT
LON120        IF WA-MSG-ERR-FLAG = 1
LON120           GO TO C6450-EXIT
LON120        ELSE
LON120           SUBTRACT BOIS-BUCKET-DUE(3) FROM
LON120                    BORM-UNPAID-ARRS-INT-BAL
LON120           ADD BOIS-BUCKET-DUE(3) TO BOIS-FINE2-INT-ACCR
LON120        END-IF
LON120     ELSE
S12787        IF BOIS-FINE2-INT-ACCR > 0
S12787           INITIALIZE JNRST-DB-DATA
S12787           MOVE  13603      TO  WA-TXN
S12787                                JNRST-TRAN-CODE
S12787                                LINK-IN-TXCODE
S12787                                STRT-AREA-TRN-NO
S12787           MOVE  BOIS-FINE2-INT-ACCR TO WA-WORK-AMOUNT-3
S12787           PERFORM C6460-SETUP-PENALTY-TXN THRU C6469-EXIT
S12787           IF WA-MSG-ERR-FLAG = 1
S12787              GO TO C6450-EXIT
S12787           ELSE
S12787              ADD W900-TXN-AMOUNT TO BORM-UNPAID-ARRS-INT-BAL
S12787              MOVE 0 TO BOIS-FINE2-INT-ACCR
S12787           END-IF
S12787        END-IF
LON120     END-IF.
LON120     IF BR0001-BAD-DEBT-JRNL-NO > 0 AND
LON120        BOIS-BUCKET-DUE(4) > 0
LON120        INITIALIZE JNRST-DB-DATA
LON120        MOVE  13704      TO  WA-TXN
LON120                             JNRST-TRAN-CODE
LON120                             LINK-IN-TXCODE
LON120                             STRT-AREA-TRN-NO
LON120        MOVE  BOIS-BUCKET-DUE(4) TO WA-WORK-AMOUNT-3
LON120        PERFORM C6460-SETUP-PENALTY-TXN THRU C6469-EXIT
LON120        IF WA-MSG-ERR-FLAG = 1
LON120           GO TO C6450-EXIT
LON120        ELSE
LON120           SUBTRACT W900-TXN-AMOUNT FROM
LON120                    BOIS-UNPAID-INT-ARRS-PRIN-BAL
LON120           ADD W900-TXN-AMOUNT TO BOIS-FINE4-INT-ACCR
LON120        END-IF
LON120     ELSE
S12787        IF BOIS-FINE4-INT-ACCR > 0
S12787           INITIALIZE JNRST-DB-DATA
S12787           MOVE  13604      TO  WA-TXN
S12787                                JNRST-TRAN-CODE
S12787                                LINK-IN-TXCODE
S12787                                STRT-AREA-TRN-NO
S12787           MOVE  BOIS-FINE4-INT-ACCR TO WA-WORK-AMOUNT-3
S12787           PERFORM C6460-SETUP-PENALTY-TXN THRU C6469-EXIT
S12787           IF WA-MSG-ERR-FLAG = 1
S12787              GO TO C6450-EXIT
S12787           ELSE
S12787              ADD W900-TXN-AMOUNT TO
LON135                  BOIS-UNPAID-INT-ARRS-PRIN-BAL
S12787              MOVE 0 TO BOIS-FINE4-INT-ACCR
S12787           END-IF
LON120        END-IF
S12787     END-IF.
S12787     MOVE 17003 TO STRT-AREA-TRN-NO.
S12787 C6450-EXIT.
S12787     EXIT.
S12787/
S12787 C6460-SETUP-PENALTY-TXN.
S12787     MOVE WA-TXN TO WA-GL-TXN-NBR.
LON140     SET WA-RPYV-NO TO TRUE
LON135     IF BR0001-BAD-DEBT-JRNL-NO > 0
LON135        CONTINUE
LON135     ELSE
LON135        PERFORM N900-ROUND-CURRENCY THRU N999-EXIT
LON135     END-IF.
LON135     MOVE WA-WORK-AMOUNT-3  TO W900-TXN-AMOUNT.
S12787     MOVE STRT-AREA-POST-DT TO WA-TXN-DATE
S12787                                 WA-POST-DATE.
S12787     PERFORM C3620-UPD-BUCKET-BUCV THRU C3629-EXIT.
S12787     IF WA-MSG-ERR-FLAG = 1
LON387        GO TO C6469-EXIT.
S12787     MOVE W900-TXN-AMOUNT    TO WA-SIGNED-TXN-AMOUNT.
S12787     MOVE W900-TXN-AMOUNT    TO WA-GLCC-XFER-AMOUNT.
S12787     PERFORM Z150-BUILD-JRNL THRU Z150-EXIT.
S12787     IF WA-MSG-ERR-FLAG = 1
S12787           GO TO C6469-EXIT.
S12787     PERFORM WRITE-TO-JOURNAL-PROCESS THRU
S12787         WRITE-TO-JOURNAL-PROCESS-EXIT.
S12787
S12787 C6469-EXIT.
S12787     EXIT.
S12787/
S12787 C3620-UPD-BUCKET-BUCV.
LON182     INITIALIZE  BORV-01-DETAIL.
LON182     MOVE 1  TO  BORV-01-CODE.
LON182     MOVE 0  TO  BORV-01-CHQ-DAYS.
LON182     MOVE 0  TO  BORV-01-STATUS.
S12787     MOVE WA-TXN      TO BORV-01-TRN-CODE.
LON182     MOVE W900-TXN-AMOUNT   TO BORV-01-AMOUNT.
S12787     PERFORM X700-CALL-UT0250 THRU X701-EXIT.
S12787     IF WA-MSG-ERR-FLAG = 1
S12787        GO TO C3629-EXIT.
S12787     PERFORM X720-CALL-UT0251 THRU X729-EXIT.
S12787     IF WA-MSG-ERR-FLAG NOT =  0
S12787        GO TO C3629-EXIT.
S12787     MOVE LINK-BOR-CODE              TO  WA-LEDGER-CODE.
S12787     MOVE LINK-PRIORITY-CODE         TO  WA-PRIORITY-CODE.
S12787     MOVE WA-TXN-DATE  TO BORV-01-TRN-DATE
S12787                          BORV-01-POST-DATE.
S12787     MOVE BORM-LOAN-BAL TO BORV-01-BALANCE.
LON182     MOVE STRT-AREA-JRNL-NO TO BORV-01-JRNL-NO.
LON182     MOVE STRT-AREA-BRANCH  TO UPTELLCA-BRCH.
LON182     MOVE STRT-AREA-TELL-NO TO UPTELLCA-TELR
LON182     CALL "UT0031" USING
LON182                   UPTELLCA-PARAM-1
LON182                   UPTELLCA-PARAM-2
LON182     MOVE UPTELLCA-TELLER-BRANCH TO BORV-01-TELL-AND-BR.
LON182     SUBTRACT BORM-THEO-LOAN-BAL FROM BORM-LOAN-BAL
LON182              GIVING BORV-01-ARREARS.
LON182     MOVE 1   TO   BORV-01-BRTERM.
LON182     PERFORM X200-ADD-NEW-TXN THRU X299-EXIT.
S12787 C3629-EXIT.
S12787     EXIT.
S12787/
S12787 C3630-POST-BILLING-DTLS.
S12787     INITIALIZE            UT0107CA-CALL-PARAM
S12787                           RPYV-RECORD-AREA
LON387                           RPYV-TERM-DETAILS.
LON387     SET WA-RPYV-NO TO TRUE.
LON387     MOVE ZEROES           TO WA-RPYV-BUK(1)
LON387                              WA-RPYV-BUK(2)
LON387                              WA-RPYV-BUK(3)
LON387                              WA-RPYV-BUK(4).
S12787     MOVE 04               TO UT0107CA-CALL-CODE.
S12787     MOVE "BOR"            TO UT0107CA-SYSTEM.
S12787     MOVE 0                TO UT0107CA-JRNL-NO.
S12787     MOVE 9996             TO UT0107CA-BASED-TERM-NO.
S12787     MOVE 1                TO UT0107CA-SEQUENCE-NO.
S12787     CALL "UT0107" USING UT0107CA-CALL-PARAM
S12787                         UT0094CA-CALL-PARAM
S12787                         BORM-RECORD-AREA
S12787                         BORV-RECORD-AREA
S12787                         BOIS-RECORD-AREA
S12787                         LS01-BORR-PARAMS.
S12787     IF UT0107CA-RET-STATUS NOT = 0
LON387        MOVE UT0107CA-ERROR-MSG-NO TO BR0001-OUTPUT-ERR-MSG-NO
LON387        MOVE  1    TO WA-MSG-ERR-FLAG
S12787        GO TO C3639-EXIT
S12787     END-IF.
S12787
S12787     MOVE UT0107CA-RPYV-RECORD TO RPYV-RECORD-AREA.
LON120     IF RPYV-TERM-NO = BOIS-LAST-TERM-PAID AND
LON387        BR0001-BAD-DEBT-JRNL-NO = 0
LON387        SUBTRACT 1 FROM BOIS-LAST-TERM-PAID
LON387        ADD 1 TO BOIS-ARREARS-CTR
LON387        MOVE "B" TO RPYV-TXN-IND
LON387     END-IF.
S12787     PERFORM VARYING WA-SUB FROM 1 BY 1
S12787     UNTIL WA-SUB > 15
S12787        IF RPYV-BUCKET-CODE (WA-SUB) = 'I' AND
LON387        WA-RPYV-AMT(1) NOT = 0
LON387           MOVE 1 TO WA-RPYV-BUK(1)
S12787           ADD WA-RPYV-AMT(1) TO
LON120               RPYV-TXN-AMT (WA-SUB)
LON387           MOVE STRT-AREA-POST-DT TO
LON387                RPYV-BASED-TO-DATE(1)
S12787        END-IF
S12787        IF RPYV-BUCKET-CODE (WA-SUB) = 'A' AND
LON387        WA-RPYV-AMT(2) NOT = 0
LON387           MOVE 1 TO WA-RPYV-BUK(2)
S12787           ADD WA-RPYV-AMT(2) TO
LON120               RPYV-TXN-AMT (WA-SUB)
LON387           MOVE STRT-AREA-POST-DT TO
LON387                RPYV-BASED-TO-DATE(2)
S12787        END-IF
S12787        IF RPYV-BUCKET-CODE (WA-SUB) = 'W' AND
LON387        WA-RPYV-AMT(3) NOT = 0
LON387           MOVE 1 TO WA-RPYV-BUK(3)
S12787           ADD WA-RPYV-AMT(3) TO
LON120               RPYV-TXN-AMT (WA-SUB)
LON387           MOVE STRT-AREA-POST-DT TO
LON387                RPYV-BASED-TO-DATE(3)
S12787        END-IF
S12787        IF RPYV-BUCKET-CODE (WA-SUB) = 'E' AND
LON387        WA-RPYV-AMT(4) NOT = 0
LON387           MOVE 1 TO WA-RPYV-BUK(4)
S12787           ADD WA-RPYV-AMT(4) TO
LON120               RPYV-TXN-AMT (WA-SUB)
LON387           MOVE STRT-AREA-POST-DT TO
LON387                RPYV-BASED-TO-DATE(4)
S12787        END-IF
S12787     END-PERFORM.
LON387     PERFORM VARYING WA-SUB FROM 1 BY 1
LON387     UNTIL WA-SUB > 4
LON387         SET WA-RPYV-NO TO TRUE
LON387         IF (WA-RPYV-BUK(WA-SUB) = 0 AND
LON387            WA-RPYV-AMT(WA-SUB) NOT = 0)
LON387            PERFORM VARYING WA-SUB1 FROM 1 BY 1
LON387            UNTIL WA-SUB1 > 15 OR WA-RPYV-YES
LON387               IF RPYV-BUCKET-CODE(WA-SUB1) = SPACES
LON387                  ADD  WA-RPYV-AMT(WA-SUB)  TO
LON387                       RPYV-TXN-AMT(WA-SUB)
LON387                  MOVE WA-RPYV-CODE(WA-SUB) TO
LON387                       RPYV-BUCKET-CODE(WA-SUB1)
LON387                  SET  WA-RPYV-YES TO TRUE
LON387               END-IF
LON387            END-PERFORM
LON387         END-IF
LON387     END-PERFORM.
S12787     MOVE RPYV-RECORD-AREA   TO UT0107CA-RPYV-RECORD.
S12787     MOVE 05                 TO UT0107CA-CALL-CODE.
S12787     MOVE "LON"              TO UT0107CA-SYSTEM.
S12787     MOVE 1                TO UT0107CA-SEQUENCE-NO.
S12787     MOVE RPYV-TERM-NO     TO UT0107CA-BASED-TERM-NO.
S12787     CALL "UT0107"
S12787           USING UT0107CA-CALL-PARAM
S12787                 UT0094CA-CALL-PARAM
S12787                 BORM-RECORD-AREA
S12787                 BORV-RECORD-AREA
S12787                 BOIS-RECORD-AREA
S12787                 BORR-PARAMS.
S12787     IF UT0107CA-RET-STATUS NOT = 0
LON387        MOVE UT0107CA-ERROR-MSG-NO TO BR0001-OUTPUT-ERR-MSG-NO
LON387        MOVE 1  TO  WA-MSG-ERR-FLAG
S12787     END-IF.
S12787 C3639-EXIT.
S12787     EXIT.
S12787 C4630-POST-CORR-BILLING-DTLS.
S12787     INITIALIZE            UT0107CA-CALL-PARAM
S12787                           RPYV-RECORD-AREA
LON387                           RPYV-TERM-DETAILS.
LON387     MOVE ZEROES           TO WA-TMP-AMT-T(1)
LON387                              WA-TMP-AMT-T(2)
LON387                              WA-TMP-AMT-T(3)
LON387                              WA-TMP-AMT-T(4)
LON387                              WA-TMP-AMT-T(5).
LON387     MOVE ZEROES           TO WA-ACCR-TMP.
S12787     MOVE 04               TO UT0107CA-CALL-CODE.
S12787     MOVE "BOR"            TO UT0107CA-SYSTEM.
S12787     MOVE 0                TO UT0107CA-JRNL-NO.
S12787     MOVE 9996             TO UT0107CA-BASED-TERM-NO.
S12787     MOVE 1                TO UT0107CA-SEQUENCE-NO.
S12787     CALL "UT0107" USING UT0107CA-CALL-PARAM
S12787                         UT0094CA-CALL-PARAM
S12787                         BORM-RECORD-AREA
S12787                         BORV-RECORD-AREA
S12787                         BOIS-RECORD-AREA
S12787                         LS01-BORR-PARAMS.
S12787     IF UT0107CA-RET-STATUS NOT = 0
LON387        MOVE UT0107CA-ERROR-MSG-NO TO BR0001-OUTPUT-ERR-MSG-NO
LON387        MOVE  1    TO WA-MSG-ERR-FLAG
S12787        GO TO C4639-EXIT
S12787     END-IF.
S12787     MOVE UT0107CA-RPYV-RECORD TO RPYV-RECORD-AREA.
S12787     PERFORM VARYING WA-SUB FROM 1 BY 1
S12787     UNTIL WA-SUB > 15
LON387        IF RPYV-BUCKET-CODE (WA-SUB) = 'P'
LON387           ADD RPYV-TXN-AMT(WA-SUB) TO WA-ACCR-TMP
LON387           COMPUTE WA-TMP-AMT-T(1) = WA-TMP-AMT-T(1)
LON387           + RPYV-TXN-AMT(WA-SUB)
LON387           + RPYV-ADJUSTED-AMT(WA-SUB)
LON387           - RPYV-COLLECTED-AMT(WA-SUB)
LON387        END-IF
S12787        IF RPYV-BUCKET-CODE (WA-SUB) = 'I'
LON387           IF WA-RPYV-AMT(1) NOT = 0
LON387              IF RPYV-TXN-AMT (WA-SUB) > 0
LON120                 SUBTRACT WA-RPYV-AMT(1) FROM
LON120                          RPYV-TXN-AMT (WA-SUB)
LON387                 MOVE STRT-AREA-POST-DT TO
LON387                          RPYV-BASED-TO-DATE(1)
LON387              END-IF
LON387           END-IF
LON387           ADD RPYV-TXN-AMT(WA-SUB) TO WA-ACCR-TMP
LON387           COMPUTE WA-TMP-AMT-T(2) = WA-TMP-AMT-T(2)
LON387           + RPYV-TXN-AMT(WA-SUB)
LON387           + RPYV-ADJUSTED-AMT(WA-SUB)
LON387           - RPYV-COLLECTED-AMT(WA-SUB)
S12787        END-IF
S12787        IF RPYV-BUCKET-CODE (WA-SUB) = 'A'
LON387           IF WA-RPYV-AMT(2) NOT = 0
LON387              IF RPYV-TXN-AMT (WA-SUB) > 0
LON120                 SUBTRACT WA-RPYV-AMT(2) FROM
LON120                          RPYV-TXN-AMT (WA-SUB)
LON387                 MOVE STRT-AREA-POST-DT TO
LON387                          RPYV-BASED-TO-DATE(2)
LON387              END-IF
LON387           END-IF
LON387           ADD RPYV-TXN-AMT(WA-SUB) TO WA-ACCR-TMP
LON387           COMPUTE WA-TMP-AMT-T(3) = WA-TMP-AMT-T(3)
LON387           + RPYV-TXN-AMT(WA-SUB)
LON387           + RPYV-ADJUSTED-AMT(WA-SUB)
LON387           - RPYV-COLLECTED-AMT(WA-SUB)
S12787        END-IF
S12787        IF RPYV-BUCKET-CODE (WA-SUB) = 'W'
LON387           IF WA-RPYV-AMT(3) NOT = 0
LON387              IF RPYV-TXN-AMT (WA-SUB) > 0
LON120                 SUBTRACT WA-RPYV-AMT(3) FROM
LON120                          RPYV-TXN-AMT (WA-SUB)
LON387                 MOVE STRT-AREA-POST-DT TO
LON387                      RPYV-BASED-TO-DATE(3)
LON387              END-IF
LON387           END-IF
LON387           ADD RPYV-TXN-AMT(WA-SUB) TO WA-ACCR-TMP
LON387           COMPUTE WA-TMP-AMT-T(4) = WA-TMP-AMT-T(4)
LON387           + RPYV-TXN-AMT(WA-SUB)
LON387           + RPYV-ADJUSTED-AMT(WA-SUB)
LON387           - RPYV-COLLECTED-AMT(WA-SUB)
S12787        END-IF
S12787        IF RPYV-BUCKET-CODE (WA-SUB) = 'E'
LON387           IF WA-RPYV-AMT(4) NOT = 0
LON387              IF RPYV-TXN-AMT (WA-SUB) > 0
LON120                 SUBTRACT WA-RPYV-AMT(4) FROM
LON120                          RPYV-TXN-AMT (WA-SUB)
LON387                 MOVE STRT-AREA-POST-DT TO
LON387                          RPYV-BASED-TO-DATE(4)
LON387              END-IF
LON387              ADD RPYV-TXN-AMT(WA-SUB) TO WA-ACCR-TMP
LON387              COMPUTE WA-TMP-AMT-T(5) = WA-TMP-AMT-T(5)
LON387              + RPYV-TXN-AMT(WA-SUB)
LON387              + RPYV-ADJUSTED-AMT(WA-SUB)
LON387              - RPYV-COLLECTED-AMT(WA-SUB)
S12787           END-IF
LON387        END-IF
S12787     END-PERFORM.
S12787     MOVE RPYV-RECORD-AREA   TO UT0107CA-RPYV-RECORD.
S12787     MOVE 05                 TO UT0107CA-CALL-CODE.
LON387     IF RPYV-ADJ-REASON-CODE = "9999"
LON387        IF WA-TMP-AMT-T(1) = 0 AND WA-TMP-AMT-T(2) = 0
LON387           AND WA-TMP-AMT-T(3) = 0 AND WA-TMP-AMT-T(4) = 0
LON387           AND WA-TMP-AMT-T(5) = 0 AND WA-ACCR-TMP NOT = 0
LON387           ADD 1 TO BOIS-LAST-TERM-PAID
LON387           SUBTRACT 1 FROM BOIS-ARREARS-CTR
LON387           MOVE "C" TO RPYV-TXN-IND
LON387        END-IF
LON387        IF WA-ACCR-TMP = 0
LON387           MOVE 07 TO UT0107CA-CALL-CODE
LON387           SUBTRACT 1 FROM BOIS-ARREARS-CTR
LON387        END-IF
LON387     END-IF.
S12787     MOVE "LON"            TO UT0107CA-SYSTEM.
S12787     MOVE 1                TO UT0107CA-SEQUENCE-NO.
S12787     MOVE RPYV-TERM-NO     TO UT0107CA-BASED-TERM-NO.
S12787     CALL "UT0107"
S12787           USING UT0107CA-CALL-PARAM
S12787                 UT0094CA-CALL-PARAM
S12787                 BORM-RECORD-AREA
S12787                 BORV-RECORD-AREA
S12787                 BOIS-RECORD-AREA
S12787                 BORR-PARAMS.
S12787     IF UT0107CA-RET-STATUS NOT = 0
LON387        MOVE UT0107CA-ERROR-MSG-NO TO BR0001-OUTPUT-ERR-MSG-NO
LON387        MOVE 1  TO  WA-MSG-ERR-FLAG
S12787     END-IF.
S12787 C4639-EXIT.
S12787     EXIT.
S10093*-----------------------------------------------------------------
S12787 Z150-BUILD-JRNL.
S12787     PERFORM X700-CALL-UT0250 THRU X701-EXIT.
S12787     IF WA-MSG-ERR-FLAG = 1
S12787        GO TO Z150-EXIT
S12787     ELSE
S12787        MOVE LINK-EDIT-TABLE TO JNRST-EDIT-MASK
S12787        MOVE LINK-SIGN       TO STRT-AREA-SIGN
S12787     END-IF.
S12787*LON645     PERFORM X720-CALL-UT0251
S12787*LON645     THRU X729-EXIT.
S12787*LON645     IF WA-MSG-ERR-FLAG NOT = 0
S12787*LON645        GO TO Z150-EXIT
S12787*LON645     END-IF.
S12787*LON645     IF UT0251RA-CGL-QUALIFIER = "Y"
S12787*LON645        MOVE 3 TO UT0145CA-CALL-CODE
S12787*LON645        MOVE BORM-ACT-TYPE TO UT0145CA-CALL-PRODUCT-TYPE
S12787*LON645        MOVE BORM-CAT TO UT0145CA-CALL-PRODUCT-CAT
S12787*LON645        MOVE STRT-AREA-TRN-NO TO UT0145CA-CALL-TRAN
S12787*LON645        MOVE STRT-AREA-SOC-NO TO UT0145CA-CALL-INST
S12787*LON645        IF STRT-AREA-TRN-NO = 13655 OR 13656
S12787*LON645           MOVE "00" TO UT0145CA-CALL-BOR-BAD-STAT
S14066*LON645        ELSE
S14066*LON645          IF STRT-AREA-TRN-NO = 98922 OR 98923 OR 98925 OR 98926
S14066*LON645             MOVE "02" TO UT0145CA-CALL-BOR-BAD-STAT
S12787*LON645   ELSE
S12787*LON645      MOVE BORM-BAD-DEBT-IND TO UT0145CA-CALL-BOR-BAD-STAT
S12787*LON645      IF BORM-BAD-DEBT-IND = "52"
S12787*LON645         MOVE "02" TO UT0145CA-CALL-BOR-BAD-STAT
S12787*LON645      END-IF
S12787*LON645      IF BORM-BAD-DEBT-IND = "50"
S12787*LON645         MOVE "00" TO UT0145CA-CALL-BOR-BAD-STAT
S12787*LON645      END-IF
S14066*LON645   END-IF
S12787*LON645 END-IF
S12787*LON645 CALL "UT0145" USING UT0145CA-CALL-AREA
S12787*LON645 IF UT0145CA-CALL-STATUS NOT = 0
S12787*LON645    IF STRT-AREA-TRN-NO = 13655 OR 13656
S12787*LON645       MOVE 7562 TO BR0001-OUTPUT-ERR-MSG-NO
S12787*LON645       MOVE 1 TO WA-MSG-ERR-FLAG
S12787*LON645       GO TO Z150-EXIT
S12787*LON645    END-IF
S12787*LON645 ELSE
LON168*LON645    IF UT0145RA-COMPONENT-1 > 0
S12787*LON645       MOVE UT0145RA-COMPONENT-1 TO UT0099CA-COMPONENT-1
LON168*LON645    ELSE
S12787              MOVE BORP-NON-CGL-COMPONENT-1
LON168                                        TO UT0099CA-COMPONENT-1
LON168*LON645    END-IF
LON168*LON645    IF UT0145RA-COMPONENT-2 > 0
S12787*LON645       MOVE UT0145RA-COMPONENT-2 TO UT0099CA-COMPONENT-2
LON168*LON645    ELSE
S12787              MOVE BORP-NON-CGL-COMPONENT-2
LON168                                        TO UT0099CA-COMPONENT-2
LON168*LON645    END-IF
S12787*LON645    MOVE UT0145RA-POSTING-IND TO WA-SAVE-POSTING-IND
LON168*LON645    IF STRT-AREA-SIGN = '-'
S12787*LON645       MULTIPLY -1 BY WA-GLCC-XFER-AMOUNT
S12787*LON645    END-IF
S12787           PERFORM NPL-BUILD-CGL THRU
S12787                   NPL-BUILD-CGL-EXIT
S12787           MOVE UT0099CA-GLCC-OUTPUT-KEY  TO
S12787                JNRST-GL-CLASS-CODE
S12787                WA-SOUR-GL-CLASS-CODE
S12787           MOVE SOC-GL-CLASS-CODE-STRUCTURE  TO
S12787                UT0099CA-GLCC-STRUCTURE.
LON168*LON645    IF UT0145RA-COMPONENT-1-CON > 0
S12787*LON645       MOVE UT0145RA-COMPONENT-1-CON
S12787*LON645                                  TO UT0099CA-COMPONENT-1
LON168*LON645    ELSE
S12787*LON645       MOVE UT0251RA-CGL-COMPONENT-1-SB
C12787*LON645                                  TO UT0099CA-COMPONENT-1
LON168*LON645    END-IF
LON168*LON645    IF UT0145RA-COMPONENT-2-CON > 0
S12787*LON645       MOVE UT0145RA-COMPONENT-2-CON
S12787*LON645                                  TO UT0099CA-COMPONENT-2
LON168*LON645    ELSE
S12787*LON645       MOVE UT0251RA-CGL-COMPONENT-2-SB
S12787*LON645                                  TO UT0099CA-COMPONENT-2
LON168*LON645    END-IF
S12787*LON645    PERFORM NPL-BUILD-CGL THRU
S12787*LON645            NPL-BUILD-CGL-EXIT
S12787*LON645    MOVE UT0099CA-GLCC-OUTPUT-KEY  TO
S12787*LON645         JNRST-XFER-GL-CLASS-CODE
S12787*LON645         WA-DEST-GL-CLASS-CODE
S12787*LON645 END-IF
S12787*LON645     ELSE
S12787*LON645 MOVE 7560 TO BR0001-OUTPUT-ERR-MSG-NO
S12787*LON645 MOVE 1 TO WA-MSG-ERR-FLAG
S12787*LON645 GO TO Z150-EXIT
S12787*LON645     END-IF.
S12787 Z150-EXIT.
S12787     EXIT.
S12787/
S13668 Z200-VALIDATE-CHANGE.
LON327*S13794     DISPLAY "IN VALIDATE PARA 1".
S13668     MOVE STRT-AREA-SOC-NO  TO XXCT-ALT3-INST-NO.
S13668     MOVE BORM-MEMB-CUST-AC TO XXCT-ALT3-ACCT-NO.
S13668     MOVE STRT-AREA-POST-DT TO XXCT-ALT3-POST-DATE.
S13668     MOVE HIGH-VALUES        TO XXCT-ALT3-REC-NO.
S13668     MOVE XXCT-ALT-KEY3     TO DBIOMOD-KEY.
S13668     MOVE SPACE             TO DBIOMOD-STATUS
S13668                               DBIOMOD-ACCESS-COND.
S13668     MOVE 2                 TO DBIOMOD-KEY-NO.
S13668     MOVE "BOCT"            TO DBIOMOD-FILE
S13668                               DBIOMOD-SUFFIX.
S13668     MOVE "DBIO"            TO DBIOMOD-PREFIX.
S13668     PERFORM UNTIL XXCT-ALT3-INST-NO NOT = STRT-AREA-SOC-NO
S13668     OR XXCT-ALT3-ACCT-NO  NOT = BORM-MEMB-CUST-AC
S13668     OR XXCT-ALT3-POST-DATE NOT = STRT-AREA-POST-DT
S13668     MOVE "READNEXT"        TO DBIOMOD-FUNCTION
S13668
LON327*S13794     DISPLAY "READNEXT PARA"
S13668         CALL    DBIOMODNAME
S13668                 USING
S13668                 DBIOMOD-FILE
S13668                 DBIOMOD-KEY
S13668                 XXCT-RECORD-AREA
S13668                 DBIOMOD-FUNCTION
S13668                 DBIOMOD-KEY-NO
S13668                 DBIOMOD-RELEASE-LOCK
S13668                 DBIOMOD-ACCESS-COMMAND
S13668                 DBIOMOD-ACCESS-COND
S13668                 DBIOMOD-STATUS
S13668
LON327*S13794         DISPLAY "Z2 DBIOMOD-STAT = " DBIOMOD-STAT
S13668         IF DBIOMOD-STAT NOT = "****" AND "MRNF"
S13668            MOVE UT8500CA-FUNC-NORMAL-DISPLAY TO
S13668                              UT8500CA-CALL-CODE
S13668            MOVE SPACES           TO UT8500CA-INPUT-MSG
S13668            STRING "BR0001:(Z200) ERROR ACCESSING BOCT"
S13668            "DBIO-STAT = "        DBIOMOD-STAT
S13668            "DBIOMOD-KEY = "      DBIOMOD-KEY
S13668            "DBIOMOD-FUNCTION = " DBIOMOD-FUNCTION
S13668            DELIMITED BY SIZE    INTO UT8500CA-INPUT-MSG
S13668            CALL "UT8500"        USING UT8500CA-AREA
S13668            MOVE 1               TO WA-MSG-ERR-FLAG
S13668            MOVE 155             TO BR0001-OUTPUT-ERR-MSG-NO
S13668            GO TO Z200-EXIT
S13668         END-IF
S13668
S13668         IF DBIOMOD-STAT = "MRNF"
S13668            GO TO Z200-EXIT
S13668         END-IF
S13668         IF (DBIOMOD-STAT = "****" AND
S13668             XXCT-INST-NO = STRT-AREA-SOC-NO AND
S13668             XXCT-ACCT-NO = BORM-MEMB-CUST-AC AND
S13668             XXCT-POST-DATE = STRT-AREA-POST-DT AND
S13668             XXCT-TRN-CODE = 17046)
LON327*S13794            DISPLAY "ACCOUNT FOUND"
LON327*S13794            DISPLAY "BOCT ACCT = " XXCT-ACCT-NO
LON327*S13794            DISPLAY "OLD CAT SCREEN = "
LON327*S13794            DISPLAY BR0001-IN-17046-OLD-ACCT-CAT
LON327*S13794            DISPLAY "NEW CAT SCREEN = "
LON327*S13794            DISPLAY "OLD BOCT CAT = "
LON327*S13794            DISPLAY XXCT-47-OLD-ACCT-CAT
LON327*S13794            DISPLAY "NEW BOCT CAT = "
LON327*S13794            DISPLAY XXCT-47-NEW-ACCT-CAT
LON327*S13794            DISPLAY BR0001-IN-17046-NEW-ACCT-CAT
S13668            IF  (BR0001-IN-17046-OLD-ACCT-CAT NOT =
S13668                BR0001-IN-17046-NEW-ACCT-CAT) AND
S13668                ((XXCT-47-OLD-ACCT-CAT NOT =
S13668                XXCT-47-NEW-ACCT-CAT) OR
S13836                (XXCT-47-OLD-BRANCH NOT =
S13836                XXCT-47-NEW-BRANCH))
S13668                MOVE 7688 TO BR0001-OUTPUT-ERR-MSG-NO
S13668                MOVE 1    TO WA-MSG-ERR-FLAG
S13668                MOVE UT8500CA-FUNC-NORMAL-DISPLAY
S13668                     TO UT8500CA-CALL-CODE
S13668                MOVE SPACES      TO UT8500CA-INPUT-MSG
S13668                STRING "BR0001:(Z200) "
S13668                ", ACCT = " BR0001-IN-17046-NEW-ACCT-TYPE
S13668                ", CAT = " BR0001-IN-17046-NEW-ACCT-CAT
S13668                  DELIMITED BY SIZE INTO UT8500CA-INPUT-MSG
S13668                CALL "UT8500" USING UT8500CA-AREA
S13668                GO TO Z200-EXIT
S13668            END-IF
S13668            IF  (BR0001-IN-17046-OLD-BRANCH NOT =
S13668                 BR0001-IN-17046-NEW-BRANCH) AND
S13836                ((XXCT-47-OLD-ACCT-CAT NOT =
S13836                XXCT-47-NEW-ACCT-CAT) OR
S13668                (XXCT-47-OLD-BRANCH NOT =
S13668                XXCT-47-NEW-BRANCH))
S13668                MOVE 7689 TO BR0001-OUTPUT-ERR-MSG-NO
S13668                MOVE 1    TO WA-MSG-ERR-FLAG
S13668                MOVE UT8500CA-FUNC-NORMAL-DISPLAY
S13668                     TO UT8500CA-CALL-CODE
S13668                MOVE SPACES      TO UT8500CA-INPUT-MSG
S13668                STRING "BR0001:(Z200) "
S13668                ", BRANCH = " BR0001-IN-17046-NEW-BRANCH
S13668                DELIMITED BY SIZE INTO UT8500CA-INPUT-MSG
S13668                CALL "UT8500" USING UT8500CA-AREA
S13668                GO TO Z200-EXIT
S13668            END-IF
S13668            MOVE XXCT-REC-NO   TO XXCT-ALT3-REC-NO
S13668            MOVE XXCT-ACCT-NO  TO XXCT-ALT3-ACCT-NO
S13668            MOVE XXCT-INST-NO  TO XXCT-ALT3-INST-NO
S13668            MOVE XXCT-POST-DATE TO XXCT-ALT3-POST-DATE
S13668            MOVE XXCT-ALT-KEY3 TO DBIOMOD-KEY
S13668         ELSE
S13668            MOVE XXCT-REC-NO   TO XXCT-ALT3-REC-NO
S13668            MOVE XXCT-ACCT-NO  TO XXCT-ALT3-ACCT-NO
S13668            MOVE XXCT-INST-NO  TO XXCT-ALT3-INST-NO
S13668            MOVE XXCT-POST-DATE TO XXCT-ALT3-POST-DATE
S13668            MOVE XXCT-ALT-KEY3 TO DBIOMOD-KEY
S13668         END-IF
S13668     END-PERFORM.
S13668 Z200-EXIT.
S13668      EXIT.
LON120/
LON120 FIND-REC-NO.
LON327*LON120     DISPLAY "FIND REC NO".
LON120     MOVE STRT-AREA-SOC-NO  TO XXCT-ALT2-INST-NO.
S13668     MOVE BORM-MEMB-CUST-AC TO XXCT-ALT2-ACCT-NO
S13668     MOVE WA-BAD-DEBT-DT    TO XXCT-ALT2-TRN-DATE.
S13668     MOVE HIGH-VALUES       TO XXCT-ALT2-REC-NO.
S13668     MOVE XXCT-ALT-KEY2     TO DBIOMOD-KEY.
S13668     MOVE SPACE             TO DBIOMOD-STATUS
S13668                               DBIOMOD-ACCESS-COND.
S13668     MOVE 2                 TO DBIOMOD-KEY-NO.
S13668     MOVE "BOCT"            TO DBIOMOD-FILE
S13668                               DBIOMOD-SUFFIX.
S13668     MOVE "DBIO"            TO DBIOMOD-PREFIX.
LON120
LON327*LON120     DISPLAY "ACCT NO = " XXCT-ALT2-ACCT-NO.
LON120*    DISPLAY "TRN DATE = " XXCT-ALT2-TRN-DATE.
LON327*LON120     DISPLAY "INST NO = "  XXCT-ALT2-INST-NO.
LON120     MOVE WA-BAD-DEBT-DT    TO  WA-TEMP-DT.
LON327*LON120     DISPLAY "BAD DEBT DATE = " WA-TEMP-DT.
LON120     PERFORM UNTIL STRT-AREA-SOC-NO NOT = XXCT-ALT2-INST-NO
LON120     OR BORM-MEMB-CUST-AC NOT = XXCT-ALT2-ACCT-NO
LON120     OR WA-BAD-DEBT-DT    NOT = XXCT-ALT2-TRN-DATE
LON327*LON120        DISPLAY "ENTER 1"
LON120        MOVE "READNEXT"        TO DBIOMOD-FUNCTION
LON120        CALL    DBIOMODNAME
LON120                USING
LON120                DBIOMOD-FILE
LON120                DBIOMOD-KEY
LON120                XXCT-RECORD-AREA
LON120                DBIOMOD-FUNCTION
LON120                DBIOMOD-KEY-NO
LON120                DBIOMOD-RELEASE-LOCK
LON120                DBIOMOD-ACCESS-COMMAND
LON120                DBIOMOD-ACCESS-COND
LON120                DBIOMOD-STATUS
LON120
LON327*LON120        DISPLAY "DBIOMOD STAT = " DBIOMOD-STAT
LON120        IF DBIOMOD-STAT NOT = "****" AND "MRNF"
LON120           MOVE UT8500CA-FUNC-NORMAL-DISPLAY TO
LON120           UT8500CA-CALL-CODE
LON120           MOVE SPACES           TO UT8500CA-INPUT-MSG
LON120           STRING "BR0001:(FIND-REC-NO) ERROR ACCESSING BOCT"
LON120           "DBIO-STAT = "        DBIOMOD-STAT
LON120           "DBIOMOD-KEY = "      DBIOMOD-KEY
LON120           "DBIOMOD-FUNCTION = " DBIOMOD-FUNCTION
LON120           DELIMITED BY SIZE    INTO UT8500CA-INPUT-MSG
LON120           CALL "UT8500"        USING UT8500CA-AREA
LON120           MOVE 1               TO WA-MSG-ERR-FLAG
LON120           MOVE 155             TO BR0001-OUTPUT-ERR-MSG-NO
LON120           GO TO FIND-REC-EXIT
LON120        END-IF
LON120
LON120        IF DBIOMOD-STAT = "MRNF"
LON120           MOVE 132  TO BR0001-OUTPUT-ERR-MSG-NO
LON120           MOVE 1    TO WA-MSG-ERR-FLAG
LON120           GO TO FIND-REC-EXIT
LON120        END-IF
LON120        IF (DBIOMOD-STAT = "****" AND
LON120           XXCT-INST-NO = STRT-AREA-SOC-NO  AND
S13668           XXCT-ACCT-NO = BORM-MEMB-CUST-AC AND
S13668           XXCT-TRN-DATE = WA-BAD-DEBT-DT   AND
S13668           XXCT-JRNL-NO = BR0001-BAD-DEBT-JRNL-NO AND
LON120           XXCT-TRAN-TYPE = 48)
LON120               MOVE XXCT-REC-NO TO WA-REC-NO
LON120               MOVE XXCT-48-PREV-STATUS TO WA-PREV-BAD-DEBT-IND
LON120               MOVE XXCT-48-NEW-STATUS  TO WA-NEXT-BAD-DEBT-IND
LON120               GO TO FIND-REC-EXIT
LON120        ELSE
LON120           IF DBIOMOD-STAT = "****"
LON120              MOVE XXCT-INST-NO  TO XXCT-ALT2-INST-NO
LON120              MOVE XXCT-ACCT-NO  TO XXCT-ALT2-ACCT-NO
LON120              MOVE XXCT-TRN-DATE TO XXCT-ALT2-TRN-DATE
LON120              MOVE XXCT-REC-NO   TO XXCT-ALT2-REC-NO
LON120              MOVE XXCT-ALT-KEY2 TO DBIOMOD-KEY
LON120           END-IF
LON120        END-IF
LON120     END-PERFORM.
LON120 FIND-REC-EXIT.
LON120     EXIT.
LON120/
LON120 REWRITE-BOCT.
LON120     MOVE STRT-AREA-SOC-NO  TO XXCT-INST-NO.
S13668     MOVE BORM-MEMB-CUST-AC TO XXCT-ACCT-NO.
S13668     MOVE WA-REC-NO         TO XXCT-REC-NO.
S13668     MOVE XXCT-KEY          TO DBIOMOD-KEY.
LON327*LON120     DISPLAY  "KEY = "  XXCT-KEY
S13668     MOVE SPACE             TO DBIOMOD-STATUS
S13668                               DBIOMOD-ACCESS-COND.
S13668     MOVE 0                 TO DBIOMOD-KEY-NO.
S13668     MOVE "BOCT"            TO DBIOMOD-FILE
S13668                               DBIOMOD-SUFFIX.
S13668     MOVE "DBIO"            TO DBIOMOD-PREFIX.
LON120     SET  REWRITE-CMD       TO TRUE.
LON120     CALL    DBIOMODNAME
LON120             USING
LON120             DBIOMOD-FILE
LON120             DBIOMOD-KEY
LON120             XXCT-RECORD-AREA
LON120             DBIOMOD-FUNCTION
LON120             DBIOMOD-KEY-NO
LON120             DBIOMOD-RELEASE-LOCK
LON120             DBIOMOD-ACCESS-COMMAND
LON120             DBIOMOD-ACCESS-COND
LON120             DBIOMOD-STATUS
LON120     IF DBIOMOD-STAT NOT = "****"
LON120         MOVE 1            TO WA-MSG-ERR-FLAG
LON120         MOVE "BOCT"       TO BR0001-OUTPUT-DATA-SET
LON120         MOVE DBIOMOD-STAT TO BR0001-OUTPUT-STATUS
LON120         MOVE 155          TO BR0001-OUTPUT-ERR-MSG-NO
LON120         MOVE UT8500CA-FUNC-NORMAL-DISPLAY TO UT8500CA-CALL-CODE
LON120         MOVE SPACES TO UT8500CA-INPUT-MSG
LON120         STRING "BR0001:(REWRITE-BOCT) WRITE BOCT ERROR"
LON120                ", STAT="  DBIOMOD-STAT
LON120                ", KEY="   DBIOMOD-KEY
LON120                ", FUNC="  DBIOMOD-FUNCTION
LON120                DELIMITED BY SIZE INTO UT8500CA-INPUT-MSG
LON120         CALL "UT8500" USING UT8500CA-AREA.
LON120 REWRITE-BOCT-EXIT.
LON120     EXIT.
LON120/
LON152 X150-ROUND-AMT.
LON152     IF WA-ROUNDING = ZEROS
LON152        MOVE WA-AMOUNT-ROUND TO WA-RES
LON152     ELSE
LON152        IF WA-ROUNDING IS NEGATIVE
LON152           MULTIPLY -1 BY WA-ROUNDING GIVING
LON152           ROUNDING-FACT
LON152        ELSE
LON152           MOVE WA-ROUNDING TO ROUNDING-FACT
LON152        END-IF
LON152        DIVIDE WA-AMOUNT-ROUND BY ROUNDING-FACT
LON152        GIVING WA-RESULT-INTEGER REMAINDER WA-REMD
LON152        IF WA-REMD > ZERO AND WA-ROUNDING IS POSITIVE
LON152           AND WA-REMD >= .5
LON152           ADD 1 TO WA-RESULT-INTEGER
LON152        END-IF
LON152        MULTIPLY WA-RESULT-INTEGER BY ROUNDING-FACT
LON152        GIVING   WA-RES
LON152     END-IF.
LON152 X159-EXIT.
LON152     EXIT.
LON152/
LON387 C700-RPYV-ACCESS.
LON387     SET WA-RPYV-NO TO TRUE
LON512*LON387     DISPLAY "IN RPYV ACCESS"
LON387     INITIALIZE RPYV-RECORD-AREA
LON387                RPYV-TERM-DETAILS.
LON387     MOVE  0 TO WA-SUB
LON387                WA-SUB1.
LON387     MOVE  STRT-AREA-POST-DT TO RPYV-TXN-DATE
LON387                                RPYV-POSTING-DATE.
LON387     MOVE  STRT-AREA-JRNL-NO TO RPYV-JRNL-NO.
LON387     IF BR0001-BAD-DEBT-JRNL-NO > 0 AND
LON387        BOIS-BUCKET-DUE(1) > 0
LON387        MOVE  BOIS-BUCKET-DUE(1)  TO WA-RPYV-AMT(1)
LON387     ELSE
LON387        IF BORM-INT-ACCR > 0 AND BR0001-BAD-DEBT-JRNL-NO = 0
LON387           MOVE    BORM-INT-ACCR    TO WA-WORK-AMOUNT-3
LON387           PERFORM N900-ROUND-CURRENCY THRU N999-EXIT
LON387           MOVE    WA-WORK-AMOUNT-3 TO WA-RPYV-AMT(1)
LON512*LON387           DISPLAY "AFTER MOVE AMOUNT"
LON387        END-IF
LON387     END-IF.
LON387     IF BR0001-BAD-DEBT-JRNL-NO > 0 AND
LON387        BOIS-BUCKET-DUE(2) > 0
LON387        MOVE BOIS-BUCKET-DUE(2)   TO WA-RPYV-AMT(3)
LON387     ELSE
LON387        IF BORM-ARR-INT-ACCR > 0 AND BR0001-BAD-DEBT-JRNL-NO = 0
LON387           MOVE BORM-ARR-INT-ACCR TO WA-WORK-AMOUNT-3
LON387           PERFORM N900-ROUND-CURRENCY THRU N999-EXIT
LON387           MOVE WA-WORK-AMOUNT-3  TO WA-RPYV-AMT(3)
LON387        END-IF
LON387     END-IF.
LON387     IF BR0001-BAD-DEBT-JRNL-NO > 0 AND
LON387        BOIS-BUCKET-DUE(3) > 0
LON387        MOVE BOIS-BUCKET-DUE(3)   TO WA-RPYV-AMT(2)
LON387     ELSE
LON387        IF (BOIS-FINE2-INT-ACCR > 0 AND
LON387            BR0001-BAD-DEBT-JRNL-NO = 0)
LON387            MOVE BOIS-FINE2-INT-ACCR TO WA-WORK-AMOUNT-3
LON387            PERFORM N900-ROUND-CURRENCY THRU N999-EXIT
LON387            MOVE WA-WORK-AMOUNT-3 TO WA-RPYV-AMT(2)
LON387        END-IF
LON387     END-IF.
LON387     IF BR0001-BAD-DEBT-JRNL-NO > 0 AND
LON387        BOIS-BUCKET-DUE(4) > 0
LON387        MOVE BOIS-BUCKET-DUE(4)   TO WA-RPYV-AMT(4)
LON387     ELSE
LON387        IF BOIS-FINE4-INT-ACCR > 0 AND
LON387           BR0001-BAD-DEBT-JRNL-NO = 0
LON387           MOVE BOIS-FINE4-INT-ACCR TO WA-WORK-AMOUNT-3
LON387           PERFORM N900-ROUND-CURRENCY THRU N999-EXIT
LON387           MOVE WA-WORK-AMOUNT-3 TO WA-RPYV-AMT(4)
LON387        END-IF
LON387     END-IF.
LON467     IF WA-RPYV-AMT(1) = 0 AND WA-RPYV-AMT(2) = 0
LON467        AND WA-RPYV-AMT(3) = 0 AND WA-RPYV-AMT(4) = 0
STDMSG        DISPLAY "C700 NOTHING TO RPYV"
LON467        GO TO C799-EXIT
LON467     END-IF.
LON387     MOVE WA-RPYV-AMT(1) TO WA-DISP-AMT.
LON512*LON387     DISPLAY "AMT I = "  WA-DISP-AMT.
LON387     MOVE 'I'  TO  WA-RPYV-CODE(1).
LON512*LON387     DISPLAY "MOVED I".
LON387     MOVE 'A'  TO  WA-RPYV-CODE(2).
LON387     MOVE 'W'  TO  WA-RPYV-CODE(3).
LON387     MOVE 'E'  TO  WA-RPYV-CODE(4).
LON387     IF BR0001-BAD-DEBT-JRNL-NO > 0
LON387        PERFORM C4630-POST-CORR-BILLING-DTLS THRU
LON387                C4639-EXIT
LON387        IF WA-MSG-ERR-FLAG = 1
LON387           GO TO C799-EXIT
LON387        END-IF
LON387     ELSE
LON387        PERFORM VARYING WA-SUB FROM 1 BY 1 UNTIL
LON387                WA-SUB  > 4
LON512*LON387            DISPLAY "IN LOOP"
LON387            IF  WA-RPYV-AMT(WA-SUB) > 0
LON387                MOVE WA-RPYV-AMT(WA-SUB)  TO RPYV-TXN-AMT(WA-SUB)
LON387                MOVE STRT-AREA-POST-DT    TO
LON387                             RPYV-BASED-TO-DATE(WA-SUB)
LON512*LON387                DISPLAY "RPYV BUCK CODE = "
LON512*LON387                        WA-RPYV-CODE(WA-SUB)
LON387                MOVE WA-RPYV-CODE(WA-SUB) TO
LON387                     RPYV-BUCKET-CODE(WA-SUB)
LON387            END-IF
LON387        END-PERFORM
LON387        IF BORM-REM-REPAYS = 0
LON387           PERFORM C3630-POST-BILLING-DTLS
LON387                   THRU C3639-EXIT
LON387           GO TO C799-EXIT
LON387        END-IF
S14098        MOVE "9999" TO RPYV-ADJ-REASON-CODE
S13815        MOVE BORM-SOC-NO         TO RPYV-INST-NO
S13815        MOVE BORM-MEMB-CUST-AC   TO RPYV-ACCT-NO
S13815        MOVE BOIS-CYCLE-NO       TO RPYV-CYCLE-NO
S13815        COMPUTE RPYV-TERM-NO = BORM-LOAN-TRM
S13815                             - BORM-REM-REPAYS + 1
S13815        MOVE "B" TO RPYV-TXN-IND
S13815        MOVE 1                   TO RPYV-SEQUENCE-NO
S13815        MOVE 0                   TO DBIOMOD-KEY-NO
S13815        MOVE RPYV-KEY            TO DBIOMOD-KEY
S13815        SET RPYV-TXN-BILLING     TO TRUE
S13815        MOVE "WRITE"             TO DBIOMOD-FUNCTION
S13815        MOVE "RPYV"              TO DBIOMOD-FILE
S13815                                    DBIOMOD-SUFFIX
S13815        MOVE "DBIO"          TO DBIOMOD-PREFIX
S13815        MOVE ZEROES          TO DBIOMOD-KEY-NO
S13815        MOVE SPACES          TO DBIOMOD-RELEASE-LOCK
S13815                                DBIOMOD-ACCESS-COMMAND
S13815                                DBIOMOD-ACCESS-COND
S13815                                DBIOMOD-STATUS
S13815        CALL DBIOMODNAME USING
S13815                       DBIOMOD-FILE
S13815                       DBIOMOD-KEY
S13815                       RPYV-RECORD-AREA
S13815                       DBIOMOD-FUNCTION
S13815                       DBIOMOD-KEY-NO
S13815                       DBIOMOD-RELEASE-LOCK
S13815                       DBIOMOD-ACCESS-COMMAND
S13815                       DBIOMOD-ACCESS-COND
S13815                       DBIOMOD-STATUS
S14115        IF DBIOMOD-STAT = "****"
S14115           ADD 1 TO BOIS-ARREARS-CTR
S14115           GO TO C799-EXIT
S14115        END-IF
S13815        IF DBIOMOD-STAT = "DUPM"
S14098           MOVE "READ"  TO DBIOMOD-FUNCTION
S14098           CALL DBIOMODNAME USING
S14098                        DBIOMOD-FILE
S14098                        DBIOMOD-KEY
S14098                        RPYV-RECORD-AREA
S14098                        DBIOMOD-FUNCTION
S14098                        DBIOMOD-KEY-NO
S14098                        DBIOMOD-RELEASE-LOCK
S14098                        DBIOMOD-ACCESS-COMMAND
S14098                        DBIOMOD-ACCESS-COND
S14098                        DBIOMOD-STATUS
S14098           IF DBIOMOD-STAT NOT = "****"
S14098              MOVE 155         TO BR0001-OUTPUT-ERR-MSG-NO
S14098              MOVE  1          TO WA-MSG-ERR-FLAG
S14098              GO TO C799-EXIT
S14098           END-IF
S14115           IF RPYV-TERM-NO = BOIS-LAST-TERM-PAID
S14115              SUBTRACT 1 FROM BOIS-LAST-TERM-PAID
S14115              ADD 1 TO BOIS-ARREARS-CTR
S14115              MOVE "B" TO RPYV-TXN-IND
S14115           END-IF
S13815           MOVE "REWRITE"  TO DBIOMOD-FUNCTION
S13815           SET RPYV-TXN-BILLING     TO TRUE
S13815           PERFORM VARYING WA-SUB FROM 1 BY 1
S13815                           UNTIL WA-SUB > 15
LON387               EVALUATE RPYV-BUCKET-CODE(WA-SUB)
LON387                 WHEN 'I'
S13815                    IF WA-RPYV-AMT(1) NOT = 0
S13815                       MOVE 1 TO WA-RPYV-BUK(1)
S13815                       ADD WA-RPYV-AMT(1) TO RPYV-TXN-AMT(WA-SUB)
S14115                       MOVE STRT-AREA-POST-DT TO
S14115                           RPYV-BASED-TO-DATE(1)
S13815                    END-IF
S13815                 WHEN 'A'
S14115                    IF WA-RPYV-AMT(2) NOT = 0
S13815                       MOVE 1 TO WA-RPYV-BUK(2)
S13815                       ADD WA-RPYV-AMT(2) TO RPYV-TXN-AMT(WA-SUB)
S14115                       MOVE STRT-AREA-POST-DT TO
S14115                           RPYV-BASED-TO-DATE(2)
S13815                    END-IF
S13815                 WHEN 'W'
S14115                    IF WA-RPYV-AMT(3) NOT = 0
S13815                       MOVE 1 TO WA-RPYV-BUK(3)
S13815                       ADD WA-RPYV-AMT(3) TO RPYV-TXN-AMT(WA-SUB)
S14115                       MOVE STRT-AREA-POST-DT TO
S14115                            RPYV-BASED-TO-DATE(3)
S13815                    END-IF
S13815                 WHEN 'E'
S14115                    IF WA-RPYV-AMT(4) NOT = 0
S13815                       MOVE 1 TO WA-RPYV-BUK(4)
S13815                       ADD WA-RPYV-AMT(4) TO RPYV-TXN-AMT(WA-SUB)
S14115                       MOVE STRT-AREA-POST-DT TO
S14115                       RPYV-BASED-TO-DATE(4)
S13815                    END-IF
LON387               END-EVALUATE
S13815           END-PERFORM
LON387           PERFORM VARYING WA-SUB FROM 1 BY 1
LON387              UNTIL WA-SUB > 4
LON387              SET WA-RPYV-NO TO TRUE
LON387              IF (WA-RPYV-BUK(WA-SUB) = 0 AND
LON387                 WA-RPYV-AMT(WA-SUB) > 0)
LON512*LON387                 DISPLAY "FOUND AMOUNT"
LON387                 PERFORM VARYING WA-SUB1 FROM 1 BY 1
LON387                 UNTIL WA-SUB1 > 15 OR
LON387                 WA-RPYV-YES
LON512*LON387                    DISPLAY "BUCK CODE IN RPYV = "
LON512*LON387                            RPYV-BUCKET-CODE(WA-SUB1)
LON387                    IF RPYV-BUCKET-CODE(WA-SUB1) = SPACES
LON512*LON387                       DISPLAY "SUB1 = " WA-SUB1
LON387                       ADD  WA-RPYV-AMT(WA-SUB)  TO
LON387                            RPYV-TXN-AMT(WA-SUB1)
LON387                       MOVE WA-RPYV-CODE(WA-SUB) TO
LON387                            RPYV-BUCKET-CODE(WA-SUB1)
LON387                       SET WA-RPYV-YES TO TRUE
LON387                    END-IF
LON387                 END-PERFORM
LON387              END-IF
LON387           END-PERFORM
S13815           CALL DBIOMODNAME USING
S13815                       DBIOMOD-FILE
S13815                       DBIOMOD-KEY
S13815                       RPYV-RECORD-AREA
S13815                       DBIOMOD-FUNCTION
S13815                       DBIOMOD-KEY-NO
S13815                       DBIOMOD-RELEASE-LOCK
S13815                       DBIOMOD-ACCESS-COMMAND
S13815                       DBIOMOD-ACCESS-COND
S13815                       DBIOMOD-STATUS
S13815           IF DBIOMOD-STAT NOT = "****"
S13815              MOVE 155     TO BR0001-OUTPUT-ERR-MSG-NO
S13815              MOVE  1      TO WA-MSG-ERR-FLAG
S13815              GO TO C799-EXIT
S13815           END-IF
LON387        END-IF
S13815        IF DBIOMOD-STAT NOT = "DUPM" AND "****"
STDMSG           DISPLAY "RPYV WRITE 155"
S13815           MOVE 155           TO BR0001-OUTPUT-ERR-MSG-NO
S13815           MOVE  1            TO WA-MSG-ERR-FLAG
S13815        END-IF
LON387     END-IF.
LON387 C799-EXIT.
LON387     EXIT.

INITWS
INITWS Z999-INIT-CPBOOK-VAR SECTION.
INITWS       MOVE "N" TO WS-INIT-CPBOOK-VAR.
INITWS       MOVE 0 TO
INITWS                  WA-RPYV-AMT(1)
INITWS                  WA-RPYV-AMT(2)
INITWS                  WA-RPYV-AMT(3)
INITWS                  WA-RPYV-AMT(4)
INITWS                  WA-NOTICE-AMT
INITWS                  WA-TERM-INT-WDL
INITWS                  WA-SAVE-BASE-TO-AMOUNT
INITWS                  UT0310CA-TAX-SOY
INITWS                  UT0310CA-TAX-EOY
INITWS                  UT0079CA-LI-ACCR
INITWS                  UT0079CA-CI-ACCR
INITWS                  UT0079CA-BPI-ACCR
INITWS                  UT0079CA-ARR-ACCR
INITWS                  WA-AMOUNT-TO-ADD
INITWS                  UT0017CA-CALL-AMOUNT
INITWS                  UT0017CA-CALL-CLOSE-INT-AVAIL
INITWS                  UT0078CA-AMOUNT
INITWS                  UT0078CA-RESIDUAL
INITWS                  UT0078CA-ROUNDING
INITWS                  MI0095CA-MAX-OD
INITWS                  MI0095CA-INT-YTD
INITWS                  .
INITWS     PERFORM VARYING WA-INITWS-LOOP-IDX FROM 1 BY 1
INITWS     UNTIL WA-INITWS-LOOP-IDX > 5
INITWS         MOVE 0 TO WA-TMP-AMT-T(WA-INITWS-LOOP-IDX)
INITWS     END-PERFORM.
INITWS     PERFORM VARYING WA-INITWS-LOOP-IDX FROM 1 BY 1
INITWS     UNTIL WA-INITWS-LOOP-IDX > 30
INITWS         MOVE 0 TO WA-PA-CHARGE-AMOUNT(WA-INITWS-LOOP-IDX)
INITWS     END-PERFORM.
INITWS     PERFORM VARYING WA-INITWS-LOOP-IDX FROM 1 BY 1
INITWS     UNTIL WA-INITWS-LOOP-IDX > 30
INITWS         MOVE 0 TO WA-PA-REBATE-AMOUNT(WA-INITWS-LOOP-IDX)
INITWS     END-PERFORM.
INITWS     PERFORM VARYING RFSH-IDX1 FROM 1 BY 1
INITWS     UNTIL RFSH-IDX1 > 5
INITWS         MOVE 0 TO EFTR-CURR-BAL(RFSH-IDX1)
INITWS     END-PERFORM.
INITWS     PERFORM VARYING RFSH-IDX1 FROM 1 BY 1
INITWS     UNTIL RFSH-IDX1 > 5
INITWS         MOVE 0 TO EFTR-AVAIL-BAL(RFSH-IDX1)
INITWS     END-PERFORM.
INITWS     PERFORM VARYING RFSH-IDX1 FROM 1 BY 1
INITWS     UNTIL RFSH-IDX1 > 5
INITWS         MOVE 0 TO EFTR-INT-YTD(RFSH-IDX1)
INITWS     END-PERFORM.
INITWS     PERFORM VARYING RFSH-IDX1 FROM 1 BY 1
INITWS     UNTIL RFSH-IDX1 > 5
INITWS         MOVE 0 TO EFTR-CREDIT-LIMIT(RFSH-IDX1)
INITWS     END-PERFORM.
INITWS Z999-INIT-CPBOOK-EXIT.
INITWS      EXIT.
INITWS
