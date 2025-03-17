000460******************************************************************
000470*   (C) COPYRIGHT FINANCIAL NETWORK SERVICES PTY LTD. 1982       *
000480*       ALL RIGHTS RESERVED. NO PART OF THIS PROGRAM MAY         *
000490*       BE PHOTOCOPIED, REPRODUCED, TRANSLATED TO ANOTHER        *
000500*       PROGRAM LANGUAGE OR USED IN ANY WAY WITHOUT THE          *
000510*       PRIOR WRITTEN CONSENT OF FINANCIAL NETWORK SERVICES      *
000520*       OR THE AUTHORISED SELLING AGENT.                         *
000530*----------------------------------------------------------------*
000540*             FINANCIAL NETWORK SERVICES PTY LTD.                *
000550*           UNIT 6, 70 ROSEHILL ST., REDFERN, 2016               *
000560*                   SYDNEY, AUSTRALIA.                           *
000570*                     TEL. 318-1088                              *
000580******************************************************************
000590*
000600******************************************************************
000610****          P R O G R A M    H I S T O R Y                  ****
000620****  PROGRAMMERS INITIALS TOGETHER WITH THE "SPR" NUMBER     ****
000630****  MUST BE REFLECTED IN COLUMNS 73-80 OF EVERY LINES       ****
000640****  CHANGED OR ADDED.                                       ****
000650****                                                          ****
000660* PROGRAMMER : DATE : SPR NO: COMMENTS                          *
000670*-----------------------------------------------------------------
000680* R STUART   :130391: 5008  : RECOMPILE FOR KOREA BR 5
000690*-----------------------------------------------------------------
000700* J.NELSON   :221092: N0210 : FOR TELLER/BRANCH ONLY
000710*-----------------------------------------------------------------
000720* J.NELSON   :080793:580138 : RELEASE 6.1
000730*-----------------------------------------------------------------
COR003*-----------------------------------------------------------------
COR003* ZHU HUI :28122006:COR003: FIELDS WILL BE EXPANDED
COR003*-----------------------------------------------------------------
S10575*CHINTA SASTRY:13112007:S10175:THE LOGIC TO MERGE TELLER AND
S10575*                             :BRANCH IS CORRECTED
S10575*----------------------------------------------------------------
000770/
000780 IDENTIFICATION DIVISION.
000790*
000800 PROGRAM-ID.    UT0031.
000810*
000820 AUTHOR.        JOHN NELSON.
000830* DESC. CONVERTS TELLER/BRANCH TO BINARY
000840 INSTALLATION.  FINANCIAL NETWORK SERVICES.
000850 DATE-WRITTEN.  SEPTEMBER, 1989.
000860 DATE-COMPILED. XXXXXXXXXX.
000870*
000880 ENVIRONMENT DIVISION.
000890 CONFIGURATION SECTION.
000900 SOURCE-COMPUTER. VAX.
000910 OBJECT-COMPUTER. VAX.
000920*
000930 INPUT-OUTPUT SECTION.
000940 FILE-CONTROL.
000950*
000960 DATA DIVISION.
000970 FILE SECTION.
000980/
000990 WORKING-STORAGE SECTION.
001000*
001010/
001020 LINKAGE SECTION.
001030*
001040     COPY UPTELLCA IN LIBRYMIS.
001050/
001060 PROCEDURE DIVISION USING UPTELLCA-PARAM-1
001070                          UPTELLCA-PARAM-2.
001080 A000-MAIN-LINE.
001090*
COR003***  MULTIPLY UPTELLCA-TELR BY 10000 GIVING
COR003*    MULTIPLY UPTELLCA-TELR BY 1000000 GIVING
S10175     MULTIPLY UPTELLCA-TELR BY 100000 GIVING
001120              UPTELLCA-TELLER-BRANCH.
001130     ADD UPTELLCA-BRCH TO UPTELLCA-TELLER-BRANCH.
001140*
001150 A099-EXIT-PROGRAM.
001160     EXIT PROGRAM.

