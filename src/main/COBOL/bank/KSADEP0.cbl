      ****************************************************************
       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     "KSADEP0".
       AUTHOR.         CBOD.
       DATE-WRITTEN.   2017/07/01.
      ****************************************************************
      * PROGRAM NAME.......: KSADEP0                                 *
      * DESCRIPTION........: DEPOSIT FOR ENTERPRISE SA ACCT          *
      * KB/CFS TO BE CALLED: CSAINF0                                 *
      *                      CSAQRY1                                 *
      *                      CSAPRD2                                 *
      *                      CSACHK0                                 *
      *                      CSALMT0                                 *
      *                      CCMCUR2                                 *
      *                      GCCBATMP                                *
      * INPUT..............: PKSADEP0                                *
      * OUTPUT.............: PKSADEP0                                *
      * MESSAGE CODE.......: E1085,E1030,E1044,E1095,E1034           *
      *                      E7100,EN097,E1060,W0700,E1158           *
      * DATA ACCESS TABLE:                                           *
      * DB NAME | SEGMENT NAME | ACCESS TYPE(R/U/I/D)                *
      * ---------------------------------------------                *
      * BSAACND   SAACNACN       R/U                                 *
      * BSAACND   SAACNAMT       R/U                                 *
      * BSAACND   SAACNTXN       R/I/U                               *
      *                                                              *
      * CHANGE HISTORY:                                              *
      * FLAG    |REASON               |DATE   |MODIFIED BY |COMMENT  *
      * ------------------------------------------------------------ *
      *                                                              *
      ****************************************************************
       ENVIRONMENT DIVISION.
      ****************************************************************
       DATA DIVISION.
      ****************************************************************
       WORKING-STORAGE SECTION.
      ****************************************************************
      * CONSTANT VALUE DEFINITION AREA                               *
      ****************************************************************
       01  PSACONST.
           COPY                        PSACONST.
       01  MEMCONST.
           COPY                        MEMCONST.
       01  PCMCONST.
           COPY                        PCMCONST.
       01  PCICONST.
           COPY                        PCICONST.
       01  PECCONST.
           COPY                        PECCONST.  
       01  PXTCONST.
           COPY                        PXTCONST.  
       01  PPRCONST.
           COPY                        PPRCONST.       
       01  FILLER                      PIC X(24)
                                       VALUE '/// WK-AREA KSADEP0 ///'.
      ****************************************************************
      * EXTERNAL KB/CF INTERFACE AREA                                *
      ****************************************************************
       01  PCCBACNE.
           COPY                        PSA0ACNE.
       01  PCCBACND.
           COPY                        PSA0ACND.
       01  PCCBAAMT.
           COPY                        PSA0AAMT.
       01  PCCBIBG.
           COPY                        PCCBIBG.
       01  PCSACHK0.
           COPY                        PCSACHK0.
       01  PCSAQRY1.
           COPY                        PCSAQRY1.
       01  PCPDPSA0.
           COPY                        PCPDPSA0.
       01  PCSAPRD2.
           COPY                        PCSAPRD2.
       01  PCCMCUR2.
           COPY                        PCCMCUR2.
       01  PCSAPRD4.
           COPY                        PCSAPRD4.
      *01  PCCICRF8.
      *    COPY                        PCCICRF8.
       01  PCCIATN1.
           COPY                        PCCIATN1.
       01  PCCIATN3.
           COPY                        PCCIATN3.
       01  PCCICOA1.
           COPY                        PCCICOA1.
       01  PCCICOA2.
           COPY                        PCCICOA2.
       01  PKSATXN1.
           COPY                        PKSATXN1.
       01  PCCMACC6.
           COPY                        PCCMACC6.
       01  PCCMPAR2.
           COPY                        PCCMPAR2.
       01  PCSABKH4.
           COPY                        PCSABKH4.
       01  PCECCII7.
           COPY                        PCECCII7.
       01  PCECSPT0.
           COPY                        PCECSPT0.
       01  PCCMCSH0.
           COPY                        PCCMCSH0.
       01  PCSACAR0.
           COPY                        PCSACAR0.
       01  PCSACAA1.
           COPY                        PCSACAA1.  
       01  PCCBAPAT.
           COPY                        PCCBAPAT.  
       01  PCCMMSC2.
           COPY                        PCCMMSC2.
       01  PCCMROB1.
           COPY                        PCCMROB1.
       01  PCPRCAL1.
           COPY                        PCPRCAL1.
       01  PCCIATN4.
           COPY                        PCCIATN4.
      ****************************************************************
      * DBI PARAMETER AREA                                           *
      ****************************************************************
       01 PDBIMAIN.
          COPY                         PDBIMAIN.
       01 DBI-FTCALL.
          COPY                         FTCALL.
       01 DBI-RTRN.
          COPY                         FTRTRN.
      ****************************************************************
      * DB SEGMENT AND FILE DEFINITION AREA                          *
      ****************************************************************
       01 SAACNACN.
          COPY                         SAACNACN.
       01 SAACNAMT.
          COPY                         SAACNAMT.
       01 SAACNTXN.
          COPY                         SAACNTXN.
       01 SAACNAGR.
          COPY                         SAACNAGR.
       01 SAPRTPRT.
          COPY                         SAPRTPRT.
       01  OSAACNACN.
           COPY                        SAACNACN.
       01  O-SAACNAMT.
           COPY                        SAACNAMT.
       01  O-SAACNAGR.
           COPY                        SAACNAGR.
       01  SAACNCOL.
           COPY                        SAACNCOL.
       01  SAACNSPV.
           COPY                        SAACNSPV.
       01  SALSTLST.
           COPY                        SALSTLST.
       01  CMMSCSAW.
           COPY                        CMMSCSAW.
      ****************************************************************
      * APPLICATION SERVER INTERFACE AREA                            *
      ****************************************************************
      *    24 HR TEMP DB
       01  PCSATMP0.
           COPY                        PCSATMP0.
       01  PCSATMP1.
           COPY                        PCSATMP1.
      ****************************************************************
      * WORK VARIABLE DEFINITION AREA                                *
      ****************************************************************
       01 WK-AREA.
           05 WK-AVL-BAL               PIC S9(16)V9(2) COMP-3.
           05 WK-INTC-AMT              PIC S9(16)V9(2) COMP-3.
           05 WK-N                     PIC S9(2).
           05 WK-ACCT-BAL              PIC S9(16)V9(2) COMP-3.
      *    DEP+INTC-FRZ(CHANGED)
           05 WK-ACCT-BAL1             PIC S9(16)V9(2) COMP-3.
      *    DEP+INTC-FRZ(NO CHANGED)
           05 WK-ACCT-BAL2             PIC S9(16)V9(2) COMP-3.
      *    SUB SA-OD-INT
           05 WK-OD-INT-SUB            PIC S9(16)V9(2) COMP-3.
      *    SUB INTC-AMT
           05 WK-INTC-AMT-SUB          PIC S9(16)V9(2) COMP-3.
      *    ADD INTC-AMT
           05 WK-INTC-AMT-ADD          PIC S9(16)V9(2) COMP-3.
      *    SUB OVDLN-INT
           05 WK-OVDLN-OD-INT-SUB      PIC S9(16)V9(2) COMP-3.
      *    OD-AMT
           05 WK-OD-AMT                PIC S9(16)V9(2) COMP-3.
      *    SUB OD-INT-AMT
           05 WK-OD-INT-AMT-SUB        PIC S9(16)V9(2) COMP-3.
           05 WK-DET-ITEM              PIC 9(7).
           05 WK-DET-ITEM-A            PIC 9(7).
           05 WK-DET-ITEM-B            PIC 9(7).
           05 WK-OD-DAYS-N             PIC 9(4).
           05 WK-OD-INT-DAYS-N         PIC 9(4).
           05 WK-RMRK.
              10 FILLER                PIC X(2).
              10 ODODOD                PIC S9(16)V99 COMP-3.
              10 FILLER                PIC X(3).
              10 OVDOVD                PIC S9(16)V99 COMP-3.
              10 FILLER                PIC X(3).
              10 AMTAMT                PIC S9(16)V99 COMP-3.
              10 FILLER                PIC X(6).
           05 WK-DDP-PDT               PIC S9(16)V9(2) COMP-3.
           05 WK-CRNT-DT-DRW-LMT       PIC S9(16)V9(2) COMP-3.
           05 WK-NORMAL-PRDT           PIC S9(16)V9(2) COMP-3.
           05 WK-ADD-PRDT              PIC S9(16)V9(2) COMP-3.
           05 WK-8888-AMT              PIC -------------9.99.
           05 WK-SAACNAMT-KEY.
              10 WK-CURR-COD           PIC X(3).
              10 WK-CURR-IDEN          PIC X.
            05  WK-NGO-REC  OCCURS 5.
                10  WK-SA-NGO-AMT      PIC S9(16)V9(2) COMP-3.
                10  WK-SA-NGO-PRDT     PIC S9(16)V9(2) COMP-3.
            05  WK-NUM                 PIC 9(2) VALUE ZEROS.
            05  WK-FILE-READ-FLAG      PIC X(1) VALUE 'N'.
                88  WK-FILE-EOF                 VALUE 'Y'.
           05 WK-DET-ITEM-N            PIC 9(7).
           05 WK-DDP-REC  OCCURS 5.
              10  WK-O-NGO-PRDT        PIC S9(16)V9(2) COMP-3.
           05 WK-SAACNAGR-KEY-AREA.
              10 WK-SAACNAGR-KEY       PIC S9(2).
           05 WK-IDX                   PIC 9(02).
           05 WK-BRH-FLG               PIC X(01).
           05 WK-I-ACCT-NO             PIC X(32).
           05 WK-SA-LAST-TXN-DT        PIC X.
           05 WK-BUSN-QUARTER          PIC X.
           05 WK-LAST-TXN-QUARTER      PIC X.
           05 WK-24H-MODE              PIC X.
           05 WK-JPY-AMT               PIC S9(16)V9(2) COMP-3.
           05 WK-PRT-FLG               PIC X(01).
           05 WK-BKH-FLG               PIC X(1).
           05 WK-SYS-BRANCH-STD        PIC X(9).
           05 WK-SYS-TELLER-ID         PIC X(12).
           05 WK-SYS-TX-LOG-NO         PIC X(24).
           05 WK-DB-PARTITION-ID       PIC X(9).
           05 WK-LEGAL-PERSON-ID       PIC X(3).
           05 WK-TR-ACCT-NO            PIC X(32).
           05 WK-CR-AMT-TOT            PIC S9(16)V9(2) COMP-3.
           05 WK-CORP-TRANS-FLAG       PIC X(01) VALUE SPACES.
      * 05识当天首笔交易
           05 WK-TODAY-FIRST           PIC X(1).
           05  C-2201                  PIC X(04) VALUE '2201'.
           05  WK-BRANCH-COD           PIC X(9).
           05  WK-ACCT-NO              PIC X(32).
           05  WK-CUST-NO              PIC X(20).
           05  WK-PDP-CODE             PIC X(11).
           05  WK-PERSON-TYP           PIC X(2).
           05  WK-AMT-TYP              PIC X(2).
           05  WK-UPD-FLG              PIC X(2).
           05  WK-SLEEP-FLG            PIC X(2).
           05  WK-OPAC-BRANCH          PIC X(9).
           05  WK-DR-CR-COD            PIC X(1).
      *    24H变更前积数中间变量
           05 WK-BEF-TMP-PRD           PIC S9(16)V9(2) COMP-3.
      *    24H变更后积数中间变量     
           05 WK-AF-TMP-PRD            PIC S9(16)V9(2) COMP-3.
      *    24H变更前协定积数中间变量
           05 WK-BEF-NTMP-PRD          PIC S9(16)V9(2) COMP-3.
      *    24H变更后协定积数中间变量
           05 WK-AF-NTMP-PRD           PIC S9(16)V9(2) COMP-3.
      *    透支积数
           05 WK-OD-PRD                PIC S9(16)V9(2) COMP-3.
      *    一户通签约标志
           05 WK-FEA-ACCT-NO           PIC X(32).
      *    一户通标志
           05 WK-FEA-FLG               PIC X(1).
           05 WK-BRA-ACCT-NAME         PIC X(120).
           05 WK-BRA-OPAC-NO           PIC X(9).
      *    协议档24小时登记TMP档默认只查1档     
      
       01  WK-SAAGR-KEY                PIC X(2) VALUE '01'.
       
      ****************************************************************
       LINKAGE SECTION.
      ****************************************************************
      ****************************************************************
      * APPLICATION INTERFACE AREA                                   *
      ****************************************************************
           COPY                        APPAREA.
       01  KBA-AREA-R REDEFINES KBA-AREA.
           05  APP-NORMAL-AREA.
               COPY                    PSAKBN01.
      ****************************************************************
      * KB/CF PARAMETER AREA                                       ***
      ****************************************************************
       01  PKSADEP0.
           COPY                        PKSADEP0.
      
      ****************************************************************
      * COMMON TRANSACTION FIELD                                     *
      ****************************************************************
       01  CWK-APP-AREA.
           COPY                        CBCAAARE.
      
      ****************************************************************
      ****************************************************************
       PROCEDURE DIVISION USING APA-AREA,
                                PKSADEP0.
      ****************************************************************
       0000-MAIN-PROCESS-RTN.
           PERFORM 1000-INIT-RTN.
           PERFORM 2000-CHECK-INPUT-RTN.
           PERFORM 3000-NORMAL-CHECK-RTN.
           PERFORM 4000-PROCESS-DATA-RTN.
           PERFORM 5000-PREPARE-OUTPUT-RTN.
           PERFORM 6000-SETUP-ACCOUNTING-RTN.
           PERFORM 9000-END-TXN-RTN.
           GOBACK.
      
       1000-INIT-RTN.
           COPY                        SETAREA.
           SET ADDRESS OF CWK-APP-AREA TO APA-CAA-ADDR.
           CALL 'GSYSMOTR' USING AIF-AREA
                                CONTENT 'BKSADEP0'.
           INITIALIZE                  PCCBAAMT.
           INITIALIZE                  PCCBACND.
           INITIALIZE                  PCCBACNE.
           INITIALIZE                  PCCBIBG.
           INITIALIZE                  PCSATMP0.
           INITIALIZE                  PDBIMAIN.
           INITIALIZE                  SAACNACN.
           INITIALIZE                  SAACNTXN.
           INITIALIZE                  SAACNAMT.
           INITIALIZE                  PCSACHK0.
           INITIALIZE                  PCSAPRD4.
           INITIALIZE                  PCSAQRY1.
           INITIALIZE                  PCPDPSA0.
           INITIALIZE                  PCSAPRD2.
           INITIALIZE                  PCCMCUR2.
           INITIALIZE                  PCCICOA1.
           INITIALIZE                  PCCICOA2.
           INITIALIZE                  PKSATXN1.
           INITIALIZE                  PCSATMP1.
           INITIALIZE                  PCSACAR0.
           INITIALIZE                  PCSACAA1.
           INITIALIZE                  PCCBAPAT.
           INITIALIZE                  PCCMPAR2.
           INITIALIZE                  SAACNCOL.
           INITIALIZE                  SAACNSPV.
           INITIALIZE                  PCCMROB1.
           INITIALIZE                  PCPRCAL1.
           INITIALIZE                  PCCIATN4.
           INITIALIZE                  WK-AREA.
           INITIALIZE                  O-PZZZ1130.         
           MOVE SPACES                 TO AIF-MSG-CODE.
           MOVE CWK-CORP-TRANS-FLAG    TO WK-CORP-TRANS-FLAG.

           MOVE SYS-BRANCH-STD         TO WK-SYS-BRANCH-STD.
           MOVE SYS-TELLER-ID          TO WK-SYS-TELLER-ID.
           MOVE SYS-TX-LOG-NO          TO WK-SYS-TX-LOG-NO.
      *    MOVE SYS-PARTITION-ID       TO WK-DB-PARTITION-ID.
      *    MOVE SYS-LEGAL-PERSON-ID    TO WK-LEGAL-PERSON-ID.

           
           
       2000-CHECK-INPUT-RTN.
           IF I-CURR-COD OF PKSADEP0 = C-CM-JPY-COD
           THEN
             COMPUTE WK-JPY-AMT = ( I-AMT OF PKSADEP0 / 100 ) * 100
             IF WK-JPY-AMT NOT = I-AMT OF PKSADEP0
             THEN
               MOVE 'EN169'            TO AIF-MSG-CODE
               PERFORM 9999-MESSAGE-HANDLE-RTN
             END-IF
           END-IF.
      
           IF I-ACCT-NO-EC OF PKSADEP0 = SPACE
           THEN
             MOVE C-CM-FLAG-NO         TO I-ACCT-NO-EC OF PKSADEP0
           END-IF.
           IF I-ACCT-NO-EC OF PKSADEP0 NOT = C-CM-FLAG-YES AND
              I-ACCT-NO-EC OF PKSADEP0 NOT = C-CM-FLAG-NO
           THEN
             MOVE 'EN139'              TO AIF-MSG-CODE
             PERFORM 9999-MESSAGE-HANDLE-RTN
           END-IF.
     
           IF I-VAL-DT OF PKSADEP0 NOT NUMERIC OR
              I-VAL-DT OF PKSADEP0 > SYS-BUSN-DT
           THEN
             MOVE 'EN142'              TO AIF-MSG-CODE
             PERFORM 9999-MESSAGE-HANDLE-RTN
           END-IF.
      
           IF I-VAL-DT OF PKSADEP0 <  '20000101'
           THEN
             MOVE 'EN145'              TO AIF-MSG-CODE
             PERFORM 9999-MESSAGE-HANDLE-RTN
           END-IF.
           IF I-AMT OF PKSADEP0 < 0
           THEN
             MOVE 'EN033'              TO AIF-MSG-CODE
             PERFORM 9999-MESSAGE-HANDLE-RTN
           END-IF.
           PERFORM 9710-CALL-CCMCUR2-RTN.           
           IF O-FX-EUR-CUR OF PCCMCUR2 = C-CM-FLAG-YES
           THEN
              MOVE 'EN247'              TO AIF-MSG-CODE
TEST****      MOVE '欧元区货币'       TO AIF-MSG-TEXT
               PERFORM 9999-MESSAGE-HANDLE-RTN
           END-IF
      
           IF I-CURR-COD OF PKSADEP0 = C-CM-RMB-COD
           THEN
             MOVE C-SA-RMB-CURR-IDEN      TO I-CURR-IDEN OF PKSADEP0
           END-IF.
      
           IF I-CURR-IDEN OF PKSADEP0 NOT = C-CM-CASH-ID
             AND C-CM-EXC-ID
           THEN
             MOVE 'EN025'              TO AIF-MSG-CODE
             PERFORM 9999-MESSAGE-HANDLE-RTN
           END-IF.
      
           IF I-TX-DT OF PKSADEP0 NOT NUMERIC OR
              I-TX-DT OF PKSADEP0 > SYS-BUSN-DT
           THEN
             MOVE 'EN276'              TO AIF-MSG-CODE
             PERFORM 9999-MESSAGE-HANDLE-RTN
           END-IF.
      
           IF I-DSCRP-COD  OF PKSADEP0 = SPACES
           THEN
             PERFORM 9701-GEN-DSCRP-COD-RTN
           END-IF.
      *    检查账号是否为对公活期 ，若为保证金或一户通需要获取其子账号
           INITIALIZE                PCCMACC6
           MOVE I-ACCT-NO OF PKSADEP0
                                       TO I-ACCT-NO OF PCCMACC6
           PERFORM 9703-CALL-CCMACC6-RTN.
           IF O-ACCT-TYPE OF PCCMACC6  = C-CM-ENT-COMSA OR C-CM-CORP-COM
                                         OR C-CM-CORP-SA-COM 
           THEN
      *      保证金一户通
             IF O-ACCT-TYPE OF PCCMACC6  = C-CM-CORP-SA-COM AND
                I-SUB-ACCT-NO OF PKSADEP0  = SPACES  
             THEN
               MOVE 'EN996'            TO AIF-MSG-CODE
      *         MOVE '若为保证金，该子账号序号必输'
      *                                 TO AIF-MSG-TEXT
               PERFORM 9999-MESSAGE-HANDLE-RTN
             END-IF
             INITIALIZE                PCSACAR0
             MOVE I-ACCT-NO     OF PKSADEP0
                                       TO  I-ACCT-NO        OF PCSACAR0
             MOVE I-SUB-ACCT-NO OF PKSADEP0
                                       TO  I-SUB-ACCT-ID    OF PCSACAR0
             PERFORM 9722-CALL-CSACAR0-RTN
             MOVE I-ACCT-NO     OF PKSADEP0
                                       TO WK-FEA-ACCT-NO
             MOVE O-SUB-ACCT-NO OF PCSACAR0 
                                       TO I-ACCT-NO  OF PKSADEP0
            IF O-ACCT-TYPE OF PCCMACC6  = C-CM-CORP-SA-COM 
            THEN
               MOVE C-CM-FLAG-YES      TO WK-FEA-FLG   
            END-IF
           END-IF. 
           
           
           
           
           MOVE I-ACCT-NO OF PKSADEP0  TO WK-ACCT-NO.
           PERFORM 9711-CALL-CSACHK0-RTN.
           IF O-ACCT-FLG OF PCSACHK0 NOT = C-CM-FLAG-TRUE
              OR O-PUB-PRI-FLG  NOT = C-CM-PUB-FLG
           THEN
             MOVE 'EN297'              TO AIF-MSG-CODE
             PERFORM 9999-MESSAGE-HANDLE-RTN
           END-IF.
               
      
       3000-NORMAL-CHECK-RTN.
      
           PERFORM 9802-GU-SAACNACN-RTN.
      *    DISPLAY '3000-KSADEP0['DBI-DB-STATUS']'
           IF ( DBI-DB-STATUS = FT-RTRN-NOTFOUND )
           THEN
             MOVE 'EN048'              TO AIF-MSG-CODE
             PERFORM 9999-MESSAGE-HANDLE-RTN
           END-IF.
      *    DISPLAY '3000-KSADEP0'
           MOVE SA-BKH-FLG OF SAACNACN TO WK-BKH-FLG.
           
           MOVE SA-CUST-NO OF SAACNACN TO WK-CUST-NO.
           MOVE SA-ACCT-NAME OF SAACNACN 
                                       TO WK-BRA-ACCT-NAME.
           MOVE SA-OPAC-INSTN-NO OF SAACNACN 
                                       TO WK-BRA-OPAC-NO.
           
           IF SA-ACCT-CHAR OF SAACNACN = C-SA-TEMP-ACCT-RMB  OR
              SA-FX-ACCT-CHAR OF SAACNACN = C-SA-CAP-SAP-FEE OR
              SA-FX-ACCT-CHAR OF SAACNACN = C-SA-CAP-FX-FEE
           THEN
            IF SA-OVERDUE-CTL-STS OF SAACNACN  = C-SA-OVERDUE-LOCK
                                       OF C-SA-OVERDUE-CTL-STS   
            OR ( SA-AVL-DUE-DT OF SAACNACN < SYS-BUSN-DT AND
                 SA-AVL-DUE-DT OF SAACNACN NOT = SPACES  AND
                 SA-OVERDUE-CTL-STS OF SAACNACN = SPACES )                                 
            THEN
               MOVE 'END66'            TO AIF-MSG-CODE
               PERFORM 9999-MESSAGE-HANDLE-RTN
            END-IF
           END-IF.               
           IF SYS-TX-TYP = C-CLG-TXT-NOR AND
              SA-CUST-NO OF SAACNACN NOT = SPACE
           THEN
DEBUG        PERFORM 9713-CALL-CECCII7-RTN
           END-IF.
      *     开通多级账簿且账簿编号不为空，则限制其不允许在99997 99998中输入
           IF ( I-BKH-NO OF PKSADEP0  = C-SA-BKH-JDCFRZ OR 
                I-BKH-NO OF PKSADEP0  =  C-SA-BKH-OTFRZ ) AND
                WK-BKH-FLG  = C-SA-BKH-SIGN-YES    
           THEN
             MOVE 'ENE43'              TO AIF-MSG-CODE
             PERFORM 9999-MESSAGE-HANDLE-RTN
           END-IF
           IF SA-DRW-TYP OF SAACNACN(1:1) NOT = C-SA-DRW-BY-PSBK
           THEN
             MOVE C-CM-FLAG-YES        TO WK-PRT-FLG
           ELSE
             MOVE SPACES               TO WK-PRT-FLG
           END-IF. 
                
     ***   集团客户签约 子公司实时归集，调用KSATXN1增加明细信息及SAACNACN中明细数
      *    实际存入该集团客户子公司的母公司中
           IF SA-COA-FLG OF SAACNACN = C-CI-BRA-OFF
           THEN
             MOVE C-CM-FLAG-YES        TO WK-BRH-FLG
             PERFORM 3500-BRANCH-RTN
             MOVE I-ACCT-NO OF PKSADEP0 
                                       TO WK-I-ACCT-NO
      *      总公司账号
             MOVE CI-HEAD-OFF-ACCT-NO OF O-CICOACOA OF PCCICOA1
                                       TO I-ACCT-NO OF PKSADEP0
             PERFORM 9802-GU-SAACNACN-RTN
      
           END-IF.
      *    明细中的分区键与法人号与交易账号的开户机构一致
           MOVE  SA-DB-PARTITION-ID  OF SAACNACN
                                       TO WK-DB-PARTITION-ID
           MOVE  SA-LEGAL-PERSON-ID  OF SAACNACN 
                                       TO WK-LEGAL-PERSON-ID
           MOVE SA-PDP-CODE OF SAACNACN 
                                       TO WK-PDP-CODE.
           MOVE SA-DEP-CUST-TYPE OF SAACNACN
                                       TO WK-PERSON-TYP
           MOVE SA-AMT-TYPE OF SAACNACN 
                                       TO WK-AMT-TYP
           MOVE SA-GUR-TYP OF SAACNACN 
                                       TO WK-UPD-FLG 
           MOVE SA-OPAC-INSTN-NO OF SAACNACN 
                                       TO WK-OPAC-BRANCH     
           MOVE SAACNACN               TO OSAACNACN. 
           PERFORM 9803-GU-SAACNAMT-RTN.
           IF SA-DDP-ACCT-STS OF SAACNAMT = C-SA-CLOSED-COD
           THEN
             MOVE 'EN298'              TO AIF-MSG-CODE
             PERFORM 9999-MESSAGE-HANDLE-RTN
           END-IF
           MOVE SAACNAMT               TO O-SAACNAMT.
           MOVE SA-SLEEP-STS OF O-SAACNAMT 
                                       TO WK-SLEEP-FLG.
      *    CHECK DEPOSIT ACCT STATUS
           PERFORM 9709-CALL-CSAQRY1-RTN.
      *    24H日切点总分不平问题修正 2-日终
           MOVE  SYS-24H-MODE          TO WK-24H-MODE
           IF WK-24H-MODE = '2' AND
              SYS-TX-MODE = ( C-CM-ONLINE-MODE OR 
                              C-CM-TCCF-MODE   OR 
                              C-CM-TLAT-MODE   OR
                              C-CM-OLCC-MODE   OR 
                              C-CM-GLA-CENTER-MODE OR
                              C-CM-OPM-MODE OR 
                              '7' )
           THEN
            IF SA-NGO-CNCL-DT OF SAACNAMT NOT  = SPACE AND
               SA-NGO-CNCL-DT OF SAACNAMT >= SYS-BUSN-DT
            THEN
              PERFORM 9804-GU-SAACNAGR-RTN
              MOVE SAACNAGR            TO O-SAACNAGR
            END-IF
           END-IF.
      *    新系统不支持透支，注释掉关于透支的判断     
           IF SYS-TX-TYP = C-CLG-TXT-EC AND
              SA-AVL-BAL OF SAACNAMT < I-AMT OF PKSADEP0
            THEN  
      *        IF SA-COM-OD-FLG OF SAACNAMT NOT = C-SA-COM-OD-YES  OR
      *           ( SA-COM-OD-FLG OF SAACNAMT = C-SA-COM-OD-YES
      *        AND SA-DDP-OD-MAX OF SAACNAMT <
      *            ( I-AMT OF PKSADEP0 - SA-AVL-BAL OF SAACNAMT ))
      *      THEN
               MOVE 'EN299'              TO AIF-MSG-CODE
      *         MOVE ' 余额不足不能冲正 '
      *                                   TO AIF-MSG-TEXT
               PERFORM 9999-MESSAGE-HANDLE-RTN
      *      END-IF
           END-IF.
DEBUG      PERFORM 9712-CALL-CPDPSA0-RTN.
      *    该账号不允许现金存入
           IF PD-CASH-DEP-FLG  OF O-PDPRTSAC OF PCPDPSA0 NOT = 
                                       C-CM-FLAG-YES   AND 
              I-TX-TYP OF PKSADEP0 = C-CM-TXTYP-CASH
           THEN
             MOVE 'ENB84'              TO AIF-MSG-CODE
      *         MOVE ' 该账号不允许现金存入 '
      *                                   TO AIF-MSG-TEXT
             PERFORM 9999-MESSAGE-HANDLE-RTN
           END-IF
      *    检查起存金额
           IF SA-DET-ITEM-N OF SAACNACN = 0
           THEN
             IF PD-FDEP-AMT OF O-PDPRTSAA OF PCPDPSA0 > 0 AND 
               I-AMT OF PKSADEP0 < PD-FDEP-AMT OF O-PDPRTSAA OF PCPDPSA0
              THEN
                 MOVE 'EN300'          TO AIF-MSG-CODE
      *           MOVE '起存金额不足' TO AIF-MSG-TEXT
                  PERFORM 9999-MESSAGE-HANDLE-RTN
              END-IF
               IF PD-MAX-FDEP-AMT OF O-PDPRTSAA OF PCPDPSA0 > 0 AND
                 I-AMT OF PKSADEP0 > 
                             PD-MAX-FDEP-AMT OF O-PDPRTSAA OF PCPDPSA0 
              THEN
                 MOVE 'EN301'          TO AIF-MSG-CODE
      *           MOVE '开户金额超出最大开户金额' TO AIF-MSG-TEXT
                 PERFORM 9999-MESSAGE-HANDLE-RTN
               END-IF
           END-IF.
      *    PERFORM 9803-GU-SAACNAMT-RTN.
           IF SYS-TX-TYP = C-CLG-TXT-NOR
           THEN
             IF PD-MAX-AMT OF O-PDPRTSAC OF PCPDPSA0 >0 AND
                I-AMT OF PKSADEP0 + SA-ACCT-BAL OF SAACNAMT >
                PD-MAX-AMT OF O-PDPRTSAC OF PCPDPSA0                 
             THEN
               MOVE 'EN302'                    TO AIF-MSG-CODE
      *        MOVE '存款余额超出最大存款余额' TO AIF-MSG-TEXT
               PERFORM 9999-MESSAGE-HANDLE-RTN
             END-IF
           END-IF.
      
           IF SA-LTM-TX-DT OF SAACNAMT = SYS-BUSN-DT
           THEN
             MOVE C-CM-FLAG-NO         TO WK-TODAY-FIRST
           ELSE
             MOVE C-CM-FLAG-YES        TO WK-TODAY-FIRST
           END-IF.
         
      * 交易限额的授权检查   
      *    
      *     检查是否为可疑和涉案账号---是否只检测柜面    
DEBUG       PERFORM 3700-CHECK-SPTSPT-RTN. 
           IF SYS-TX-TYP = C-CLG-TXT-NOR  
           AND SYS-TX-ID(1:7) NOT = 'SA01116'
           THEN           
            PERFORM 9724-CALL-PCCBAPAT-RTN
           END-IF.
           IF SYS-LCH-CHANNEL-FLAG = C-CM-CHANNEL-TLR
           THEN
            PERFORM 3600-SPEC-CTL-RTN
           END-IF
      *     错账调整时不再累计限额
           IF I-ACCT-NO-EC OF PKSADEP0 NOT = C-CM-FLAG-YES AND
              SYS-TX-ID NOT = 'PR0071801'  AND
              SYS-TX-ID NOT = 'PR0071401'  AND
              SYS-TX-ID NOT = 'PR0070901'  AND 
              SYS-TX-ID NOT = 'PR0070802'  AND 
              SYS-TX-ID NOT = 'PR0072201'  AND 
              SYS-TX-ID NOT = 'CR0523505' AND
              SYS-TX-ID NOT = 'CR0523507' AND
              SYS-TX-ID NOT = 'CR0523508' 

           THEN
             PERFORM 9726-CALL-PCCMROB1-RTN
           END-IF.
      *    IF I-ACCT-NO-EC OF PKSADEP0  = C-CM-FLAG-YES 错账调整 限额是否回    
      *    集团客户子账户实时归集
       3500-BRANCH-RTN.
           MOVE C-CM-FLAG-YES          TO WK-BRH-FLG
           MOVE I-ACCT-NO OF PKSADEP0  TO WK-I-ACCT-NO
           PERFORM 9714-CALL-CCICOA2-RTN.    
           PERFORM 9715-CALL-KSATXN1-RTN.
           PERFORM 9716-CALL-CCICOA1-RTN.
           
DEBUG *    PERFORM 9802-GU-SAACNACN-RTN.
DEBUG *    PERFORM 9803-GU-SAACNAMT-RTN.
      *    CCIATN1若子账号的关注种类个数 发送信息 
      *    CCIATN3电信诈骗/有权机关消息推送 
           IF SYS-TX-TYP = C-CLG-TXT-NOR AND I-AMT OF PKSADEP0 > 0
           THEN
      *      IF SA-RECOG-TYP-NUM-N OF SAACNACN > ZEROS
      *      THEN
               PERFORM 5341-CALL-CCIATN1-SUB-RTN
      *      END-IF
             PERFORM 5343-CALL-CCIATN3-SUB-RTN
           END-IF
           IF SYS-TX-TYP = C-CLG-TXT-EC AND I-AMT OF PKSADEP0 > 0
           THEN          
      *      IF SA-RECOG-TYP-NUM-N OF SAACNACN > ZEROS
      *      THEN
             PERFORM 5342-CALL-CCIATN1-SUB-RTN
      *      END-IF
             PERFORM 5344-CALL-CCIATN3-SUB-RTN
           END-IF.
           
      ********************************
      * 特殊账户属性控制
      *******************************
       3600-SPEC-CTL-RTN.
      *     托管账户 SAACNCOL 现金监管--转账支取授权
           IF SA-SPC-ACCT-FLG OF OSAACNACN NOT = SPACES  AND
              SA-SPC-ACCT-FLG OF OSAACNACN = C-SA-SPC-CA AND
              I-TX-TYP OF PKSADEP0 = C-CM-TXTYP-TR AND
              SYS-LCH-CHANNEL-FLAG = C-CM-CHANNEL-TLR
           THEN 
            PERFORM 9805-GU-SAACNCOL-RTN
            IF SA-COL-CR-TRAUTH-AMT OF SAACNCOL NOT = ZEROS AND
               I-AMT OF PKSADEP0 > SA-COL-CR-TRAUTH-AMT OF SAACNCOL 
            THEN
              MOVE 'AN022'             TO AIF-MSG-CODE
              PERFORM 9999-MESSAGE-HANDLE-RTN
            END-IF
           END-IF.
      *    监管账户 SAACNSPV 现金监管 SA
      *    收款人 SALSTLST
           IF SA-SPC-ACCT-FLG OF OSAACNACN NOT = SPACES   AND
              SA-SPC-ACCT-FLG OF OSAACNACN = C-SA-SPC-SVA AND
               SYS-LCH-CHANNEL-FLAG = C-CM-CHANNEL-TLR
           THEN 
            PERFORM 9806-GU-SAACNSPV-RTN
            IF I-TX-TYP OF PKSADEP0 = C-CM-TXTYP-TR 
            THEN
              IF SA-SPV-CR-TR-AMT OF SAACNSPV NOT = ZEROS AND
                 I-AMT OF PKSADEP0 > SA-SPV-CR-TR-AMT OF SAACNSPV 
              THEN     
                MOVE 'AN018'           TO AIF-MSG-CODE
                PERFORM 9999-MESSAGE-HANDLE-RTN
              END-IF
            ELSE
              IF SA-SPV-CR-CASH-AMT OF SAACNSPV NOT = ZEROS AND
                 I-AMT OF PKSADEP0 > SA-SPV-CR-CASH-AMT OF SAACNSPV 
              THEN    
                MOVE 'AN018'           TO AIF-MSG-CODE
                PERFORM 9999-MESSAGE-HANDLE-RTN
              END-IF
            END-IF
           END-IF.
      
       3700-CHECK-SPTSPT-RTN.
           IF SYS-TX-TYP = C-CLG-TXT-NOR AND
              SYS-TX-ID(1:7) NOT = 'SA01116'
           THEN 
              INITIALIZE               PCECSPT0
              MOVE I-ACCT-NO OF PKSADEP0 
                                       TO I-ACCT-NO OF PCECSPT0
              MOVE C-EC-ACCT-NO-I      TO I-TRF-TYP OF PCECSPT0
              PERFORM 9705-CALL-CECSPT0-RTN
      
              IF CWK-DRAWEE-ACCT-NO NOT = SPACES
              THEN
                 INITIALIZE            PCECSPT0
                 MOVE CWK-DRAWEE-ACCT-NO
                                       TO I-ACCT-NO OF PCECSPT0
                 MOVE C-EC-ACCT-NO-O   TO I-TRF-TYP OF PCECSPT0
                 PERFORM 9705-CALL-CECSPT0-RTN
              END-IF
           END-IF.                 
      
       4000-PROCESS-DATA-RTN.
           MOVE C-CM-FLAG-NO           TO WK-SA-LAST-TXN-DT.
           IF SYS-BUSN-DT(3:6) > SA-LAST-TXN-DT OF SAACNACN
           THEN
             MOVE C-CM-FLAG-YES        TO WK-SA-LAST-TXN-DT
           END-IF.
           IF SYS-TX-TYP = C-CLG-TXT-NOR
           THEN
             PERFORM 4080-DSCRP-CHK-RTN
             PERFORM 4100-PROCESS-DATA-RTN
             PERFORM 4200-KBR-LOG-RTN
             PERFORM 4300-ARL-LOG-RTN
           ELSE
             PERFORM 4600-PROCESS-DATA-RTN-R
             PERFORM 4700-KBR-LOG-RTN-R
             PERFORM 4800-ARL-LOG-RTN-R
           END-IF.
           IF SA-FX-ACCT-CHAR OF OSAACNACN NOT = SPACES
           THEN
             PERFORM 9725-CALL-CCMMSC2-RTN
           END-IF.
           MOVE IO-APLY-DATA-DSCRP  OF PCCMMSC2
                                       TO CM-APLY-DATA-DSCRP-SAW
                                       OF CMMSCSAW
      *    是否资本项目 
           IF CM-SAW-M-FLG OF CM-APLY-DATA-DSCRP-SAW OF CMMSCSAW = 
                                       C-CM-FLAG-YES
           THEN
               MOVE 'IN036'            TO AIF-MSG-CODE
               PERFORM 9999-MESSAGE-HANDLE-RTN
      
      *        MOVE 'I6666'            TO AIF-MSG-CODE
      *        PERFORM 9999-MESSAGE-HANDLE-RTN
           END-IF.
      *    经常项目
           IF CM-OFFEN-CHAR-FLG  OF CM-APLY-DATA-DSCRP-SAW OF CMMSCSAW = 
                                       C-CM-FLAG-YES
           THEN
               MOVE 'IN037'            TO AIF-MSG-CODE
               PERFORM 9999-MESSAGE-HANDLE-RTN
      *        MOVE 'I6666'            TO AIF-MSG-CODE
      *        PERFORM 9999-MESSAGE-HANDLE-RTN
           END-IF.
      *    登记TMP档日终的第一笔联机
      *                                    0 - 标准联机交易
      *                                    1 - 快速联机
      *                                    2 - 组合联机
      *                                    3 - 联机批量
      *                                    4 - CENTER CUT
      *                                    5 - 核算中心
      *                                    6 - 超长输出(3M)
      *                                    7 - 延迟入账
      *                                    8 - 连线报表

           IF WK-24H-MODE = '2' AND
              SYS-TX-MODE = ( C-CM-ONLINE-MODE OR 
                              C-CM-TCCF-MODE   OR 
                              C-CM-TLAT-MODE   OR
                              C-CM-OLCC-MODE   OR 
                              C-CM-GLA-CENTER-MODE OR
                              C-CM-OPM-MODE OR 
                              '7' )
           THEN
      *     登记SAACNTMP
            INITIALIZE                 PCSATMP0
            MOVE C-SA-TBL-ACN          TO I-TBL-TYPE OF   PCSATMP0
            MOVE OSAACNACN             TO I-SAACNACN OF   PCSATMP0         
            PERFORM 9717-CALL-CSATMP0-RTN
      *     登记SAAMTTMP
            INITIALIZE                 PCSATMP0
            MOVE C-SA-TBL-AMT          TO I-TBL-TYPE OF   PCSATMP0
            MOVE O-SAACNAMT            TO I-SAACNAMT OF   PCSATMP0            
            PERFORM 9717-CALL-CSATMP0-RTN
      *     如果存在协定存款且未到期，将SAACNAGR登记TMP档
            IF SA-NGO-CNCL-DT OF SAACNAMT NOT  = SPACE AND
               SA-NGO-CNCL-DT OF SAACNAMT >= SYS-BUSN-DT
            THEN
              INITIALIZE               PCSATMP0
              MOVE C-SA-TBL-AGR        TO I-TBL-TYPE OF   PCSATMP0
              MOVE O-SAACNAGR          TO I-SAACNAGR OF   PCSATMP0            
              PERFORM 9717-CALL-CSATMP0-RTN 
            END-IF
           END-IF.
      *    联机批量的日期     
           IF WK-24H-MODE = '2' AND
              SYS-TX-MODE =   C-CM-CENTERCUT-MODE 
           THEN           
             PERFORM 9718-CALL-CSATMP1-RTN
           END-IF.
           
      
       4080-DSCRP-CHK-RTN.
      *    原224L账户性质编码改为2102   资本项目-外汇资本金账户
           IF SA-FX-ACCT-CHAR OF SAACNACN = '2102'
           THEN
             IF I-DSCRP-COD OF PKSADEP0 = C-MEM-0201
             THEN
               MOVE I-RMRK OF PKSADEP0(1:4)
                                       TO I-DSCRP-COD OF PKSADEP0
             END-IF
             IF I-DSCRP-COD OF PKSADEP0 NOT = C-MEM-SA-6000 AND 
                                              C-MEM-SA-6001 AND 
                                              C-MEM-SA-6002 AND 
                                              C-MEM-SA-6003 AND 
                                              C-MEM-SA-6004 AND 
                                              C-MEM-SA-6005 AND
                                              C-MEM-SA-6006 AND 
                                              C-MEM-SA-6007 AND
                                              C-MEM-SA-6008 AND
                                              C-MEM-SA-6009 AND
                                              C-MEM-SA-6010 AND
                                              C-MEM-SA-6011 AND
                                              C-MEM-SA-6012 AND
                                              C-MEM-SA-6013 AND
                                              C-MEM-SA-6014 AND
                                              C-MEM-SA-6015 AND
                                              C-MEM-1126
             THEN
               MOVE 'EN303'            TO AIF-MSG-CODE
      *         MOVE '资本金账户，摘要代码错'
      *                                 TO AIF-MSG-TEXT
               PERFORM 9999-MESSAGE-HANDLE-RTN
             END-IF
           END-IF.
           IF I-DSCRP-COD OF PKSADEP0  = ( C-MEM-SA-6008 OR
                                              C-MEM-SA-6009 OR
                                              C-MEM-SA-6010 OR
                                              C-MEM-SA-6012 OR
                                              C-MEM-SA-6013 OR
                                              C-MEM-SA-6014 OR
                                              C-MEM-SA-6015  )
             AND SYS-APP-TX-CODE = ( '1102' OR '1103' OR '1114' )
           THEN
             MOVE 'EN161'              TO AIF-MSG-CODE
      *       MOVE '本交易不可输入此摘要代码'
      *                                 TO AIF-MSG-TEXT
             PERFORM 9999-MESSAGE-HANDLE-RTN
           END-IF.
      
       4100-PROCESS-DATA-RTN.
      * 透支科目标志 呆账账户
           IF SA-OD-LG-FLG OF SAACNAMT = C-SA-BDDB-OD-LG OR
              SA-OD-LG-FLG OF SAACNAMT = C-SA-MISC-OD-LG
           THEN
             PERFORM 4111-PROCESS0-AMT-RTN
           ELSE
             PERFORM 4110-PROCESS-AMT-RTN
           END-IF.
           PERFORM 4130-CHECK-AMT-LMT-RTN.
           IF SA-INTC-FLG OF SAACNACN  = C-CM-FLAG-YES
           THEN
             PERFORM 4140-PROCESS-PRD-RTN
           END-IF. 
                  
           IF SA-OD-INT-AMT OF SAACNAMT > ZEROS OR
              SA-OD-INT     OF SAACNAMT > ZEROS OR
              SA-OVDLN-OD-INT OF SAACNAMT > ZEROS
           THEN
             PERFORM 4143-CAL-PRDT-OD-RTN
           END-IF.
           IF I-AMT OF PKSADEP0 > ZEROS
           THEN
             PERFORM 4160-INSERT-TXN-RTN
      *      新增一户通明细
            IF WK-FEA-FLG = C-CM-FLAG-YES
            THEN
              PERFORM 9723-CALL-CSACAA1-RTN
              MOVE O-DET OF PCSACAA1   TO KBN-ACC-UNPRINT-NO

            END-IF
           END-IF
           IF ( WK-OD-INT-SUB > ZEROS OR 
                WK-OVDLN-OD-INT-SUB > ZEROS OR
                WK-OD-INT-AMT-SUB > ZEROS ) AND 
                I-AMT OF PKSADEP0 = ZEROS
           THEN
             PERFORM 4161-INSERT-TXN-RTN
             PERFORM 4162-INSERT-TXN-RTN
           END-IF
           PERFORM 4150-UPDATE-ACN-AMT-RTN.
           IF WK-O-NGO-PRDT(1) > 0
           THEN
             PERFORM 4180-INSERT-SAACNAGR
           END-IF.
      *    透支需求未通过
      *    IF WK-OD-INT-AMT-SUB > 0
      *    THEN
      **   活期透支额度查询及异动-未通
      *      PERFORM 4188-CCICRF8-RTN
      *    END-IF.
      *    IF SA-RECOG-TYP-NUM-N OF SAACNACN > ZEROS
      *       AND I-AMT OF PKSADEP0 > 0
           IF I-AMT OF PKSADEP0 > 0
           THEN
      ***    关注信息设定  关注种类个数
             PERFORM 5341-CALL-CCIATN1-RTN
           END-IF.
           IF I-AMT OF PKSADEP0 > 0
           THEN
     ***      监控信息设定
              PERFORM 5343-CALL-CCIATN3-RTN
           END-IF.
      
       4110-PROCESS-AMT-RTN.
           COMPUTE WK-ACCT-BAL1 = I-AMT       OF PKSADEP0
                                + SA-INTC-AMT OF SAACNAMT 
                                - SA-FRZ-AMT  OF SAACNAMT.
           MOVE WK-ACCT-BAL1           TO  WK-ACCT-BAL2
           IF WK-ACCT-BAL1 > ZEROS
           THEN
      *      可用金额小于透支息金额
             IF SA-OD-INT OF SAACNAMT >= WK-ACCT-BAL1
             THEN
               MOVE WK-ACCT-BAL1       TO WK-OD-INT-SUB
               COMPUTE WK-INTC-AMT = SA-INTC-AMT OF SAACNAMT
                                   + I-AMT OF PKSADEP0
                                   - WK-ACCT-BAL1
               MOVE ZEROS              TO WK-ACCT-BAL1
               COMPUTE WK-INTC-AMT-SUB = SA-INTC-AMT OF SAACNAMT
                                       - WK-INTC-AMT
              IF WK-INTC-AMT-SUB < ZEROS
              THEN
                 COMPUTE WK-INTC-AMT-ADD = 0 - WK-INTC-AMT-SUB
                 MOVE ZEROS            TO WK-INTC-AMT-SUB
              END-IF
              COMPUTE WK-AVL-BAL = WK-INTC-AMT 
                                 - SA-FRZ-AMT OF SAACNAMT
                                 - SA-AUTH-AMT OF SAACNAMT
              IF WK-AVL-BAL < ZEROS
              THEN
                MOVE ZEROS             TO WK-AVL-BAL
              END-IF
              MOVE SA-OD-AMT OF SAACNAMT
                                       TO WK-OD-AMT
      *      可用金额大于 透支息金额
             ELSE
               COMPUTE WK-OD-INT-SUB = 0 + SA-OD-INT OF SAACNAMT
               COMPUTE WK-ACCT-BAL1 = WK-ACCT-BAL1
                                   - SA-OD-INT OF SAACNAMT
               IF WK-ACCT-BAL1 <= SA-OVDLN-OD-INT OF SAACNAMT
               THEN
                 MOVE WK-ACCT-BAL1     TO WK-OVDLN-OD-INT-SUB
                 COMPUTE WK-INTC-AMT = SA-INTC-AMT OF SAACNAMT
                                     + I-AMT OF PKSADEP0
                                     - WK-ACCT-BAL2
                 MOVE ZEROS            TO WK-ACCT-BAL1
                 COMPUTE WK-INTC-AMT-SUB = SA-INTC-AMT OF SAACNAMT
                                         - WK-INTC-AMT
                 IF WK-INTC-AMT-SUB < ZEROS
                 THEN
                   COMPUTE WK-INTC-AMT-ADD = 0 - WK-INTC-AMT-SUB
                   MOVE ZEROS          TO WK-INTC-AMT-SUB
                 END-IF
                 COMPUTE WK-AVL-BAL = WK-INTC-AMT 
                                  - SA-FRZ-AMT OF SAACNAMT
                                  - SA-AUTH-AMT OF SAACNAMT
                 IF WK-AVL-BAL < ZEROS
                 THEN
                   MOVE ZEROS          TO WK-AVL-BAL
                 END-IF
                 MOVE SA-OD-AMT OF SAACNAMT
                                       TO WK-OD-AMT
      *       可用余额 透支金额
              ELSE
                COMPUTE WK-OD-INT-SUB = 0 + SA-OD-INT OF SAACNAMT
                MOVE SA-OVDLN-OD-INT OF SAACNAMT
                                       TO WK-OVDLN-OD-INT-SUB
                COMPUTE WK-ACCT-BAL1 = WK-ACCT-BAL1
                                     - SA-OVDLN-OD-INT OF SAACNAMT
                IF WK-ACCT-BAL1 <= SA-OD-INT-AMT OF SAACNAMT
                THEN
                  MOVE WK-ACCT-BAL1    TO WK-OD-INT-AMT-SUB
                  COMPUTE WK-INTC-AMT = SA-INTC-AMT OF SAACNAMT
                                     + I-AMT OF PKSADEP0
                                     - WK-ACCT-BAL2
                 COMPUTE WK-OD-AMT = SA-OD-AMT OF SAACNAMT
                                   - WK-OD-INT-AMT-SUB
                 IF WK-OD-AMT < ZEROS
                 THEN
                   MOVE ZEROS           TO WK-OD-AMT
                 END-IF
                 MOVE ZEROS            TO WK-ACCT-BAL1
                 COMPUTE WK-INTC-AMT-SUB = SA-INTC-AMT OF SAACNAMT
                                        - WK-INTC-AMT
                IF WK-INTC-AMT-SUB < ZEROS
                THEN
                  COMPUTE WK-INTC-AMT-ADD = 0 - WK-INTC-AMT-SUB
                  MOVE ZEROS             TO WK-INTC-AMT-SUB
                END-IF
                COMPUTE WK-AVL-BAL = WK-INTC-AMT 
                                   - SA-FRZ-AMT  OF SAACNAMT
                                   - SA-AUTH-AMT OF SAACNAMT
                IF WK-AVL-BAL < ZEROS
                THEN
                  MOVE ZEROS            TO WK-AVL-BAL
                END-IF
               ELSE
                 COMPUTE WK-OD-INT-AMT-SUB = SA-OD-INT-AMT OF SAACNAMT
                 COMPUTE WK-ACCT-BAL1 = WK-ACCT-BAL1 - WK-OD-INT-AMT-SUB
                 COMPUTE WK-INTC-AMT = SA-INTC-AMT OF SAACNAMT
                                     + I-AMT OF PKSADEP0
                                     - WK-ACCT-BAL2 + WK-ACCT-BAL1
                 MOVE ZEROS            TO WK-OD-AMT
                 COMPUTE WK-INTC-AMT-SUB = SA-INTC-AMT OF SAACNAMT
                                       - WK-INTC-AMT
                 IF WK-INTC-AMT-SUB < ZEROS
                 THEN
                   COMPUTE WK-INTC-AMT-ADD = 0 - WK-INTC-AMT-SUB
                   MOVE ZEROS          TO WK-INTC-AMT-SUB
                 END-IF
                 COMPUTE WK-AVL-BAL = WK-INTC-AMT 
                                   - SA-FRZ-AMT  OF SAACNAMT
                                   - SA-AUTH-AMT OF SAACNAMT
                 IF WK-AVL-BAL < ZEROS
                 THEN
                   MOVE ZEROS          TO WK-AVL-BAL
                 END-IF
               END-IF
              END-IF
             END-IF
           ELSE
             COMPUTE WK-INTC-AMT = SA-INTC-AMT OF SAACNAMT
                                 + I-AMT OF PKSADEP0
             MOVE SA-OD-AMT OF SAACNAMT
                                       TO WK-OD-AMT
             COMPUTE WK-INTC-AMT-ADD = 0 + I-AMT OF PKSADEP0
             COMPUTE WK-AVL-BAL = WK-INTC-AMT - SA-FRZ-AMT OF SAACNAMT
                                - SA-AUTH-AMT OF SAACNAMT 
             IF WK-AVL-BAL < ZEROS
             THEN
               MOVE ZEROS              TO WK-AVL-BAL
             END-IF
           END-IF.
      
       4111-PROCESS0-AMT-RTN.
           COMPUTE WK-ACCT-BAL1
                            = I-AMT OF PKSADEP0
                            + SA-INTC-AMT OF SAACNAMT 
                            - SA-FRZ-AMT OF SAACNAMT.
           MOVE WK-ACCT-BAL1          TO  WK-ACCT-BAL2
           IF WK-ACCT-BAL1 > ZEROS
           THEN
             IF WK-ACCT-BAL1 <= SA-OD-INT-AMT OF SAACNAMT
             THEN
              MOVE WK-ACCT-BAL1        TO WK-OD-INT-AMT-SUB
              COMPUTE WK-INTC-AMT = SA-INTC-AMT OF SAACNAMT
                                  + I-AMT OF PKSADEP0
                                  - WK-ACCT-BAL1
              MOVE ZEROS               TO WK-ACCT-BAL1
              COMPUTE WK-OD-AMT = SA-OD-AMT OF SAACNAMT
                                - WK-OD-INT-AMT-SUB
              IF WK-OD-AMT < ZEROS
                MOVE ZEROS             TO WK-OD-AMT
              END-IF
              COMPUTE WK-INTC-AMT-SUB = SA-INTC-AMT OF SAACNAMT 
                                      - WK-INTC-AMT
              IF WK-INTC-AMT-SUB < ZEROS
              THEN
                COMPUTE WK-INTC-AMT-ADD = 0 - WK-INTC-AMT-SUB
                MOVE ZEROS              TO WK-INTC-AMT-SUB
              END-IF
              COMPUTE WK-AVL-BAL = WK-INTC-AMT - SA-FRZ-AMT OF SAACNAMT
                                 - SA-AUTH-AMT OF SAACNAMT
              IF WK-AVL-BAL < ZEROS
              THEN
                MOVE ZEROS             TO WK-AVL-BAL
              END-IF
             ELSE
              COMPUTE WK-OD-INT-AMT-SUB = 0 + SA-OD-INT-AMT OF SAACNAMT
              COMPUTE WK-ACCT-BAL1 = WK-ACCT-BAL1 
                                  - SA-OD-INT-AMT OF SAACNAMT
      
              IF WK-ACCT-BAL1 <= SA-OD-INT OF SAACNAMT
              THEN
               MOVE WK-ACCT-BAL1       TO WK-OD-INT-SUB
               COMPUTE WK-INTC-AMT = SA-INTC-AMT OF SAACNAMT
                                   + I-AMT OF PKSADEP0
                                   - WK-ACCT-BAL2
               COMPUTE WK-INTC-AMT-SUB = SA-INTC-AMT OF SAACNAMT
                                       - WK-INTC-AMT
               IF WK-INTC-AMT-SUB < ZEROS
               THEN
                 COMPUTE WK-INTC-AMT-ADD = 0 - WK-INTC-AMT-SUB
                 MOVE ZEROS             TO WK-INTC-AMT-SUB
               END-IF
               MOVE ZEROS               TO WK-ACCT-BAL1
               COMPUTE WK-AVL-BAL = WK-INTC-AMT - SA-FRZ-AMT OF SAACNAMT 
                                  - SA-AUTH-AMT OF SAACNAMT 
               IF WK-AVL-BAL < ZEROS
               THEN
                 MOVE ZEROS            TO WK-AVL-BAL
               END-IF
              ELSE
               COMPUTE WK-OD-INT-SUB = 0 + SA-OD-INT OF SAACNAMT
               COMPUTE WK-ACCT-BAL1 = WK-ACCT-BAL1
                                    - SA-OD-INT OF SAACNAMT
      
               IF WK-ACCT-BAL1 <= SA-OVDLN-OD-INT OF SAACNAMT
               THEN
                MOVE WK-ACCT-BAL1       TO WK-OVDLN-OD-INT-SUB
                COMPUTE WK-INTC-AMT = SA-INTC-AMT OF SAACNAMT
                                    + I-AMT OF PKSADEP0
                                    - WK-ACCT-BAL2
                COMPUTE WK-INTC-AMT-SUB = SA-INTC-AMT  OF SAACNAMT
                                        - WK-INTC-AMT
                IF WK-INTC-AMT-SUB < ZEROS
                THEN
                  COMPUTE WK-INTC-AMT-ADD = 0 - WK-INTC-AMT-SUB
                  MOVE ZEROS             TO WK-INTC-AMT-SUB
                END-IF
                MOVE ZEROS               TO WK-ACCT-BAL1
                COMPUTE WK-AVL-BAL = WK-INTC-AMT 
                                   - SA-FRZ-AMT OF SAACNAMT
                                   - SA-AUTH-AMT  OF SAACNAMT
                IF WK-AVL-BAL < ZEROS
                THEN
                  MOVE ZEROS            TO WK-AVL-BAL
                END-IF
               ELSE
                COMPUTE WK-OVDLN-OD-INT-SUB = 0 +
                               SA-OVDLN-OD-INT OF SAACNAMT
                COMPUTE WK-ACCT-BAL1 = WK-ACCT-BAL1 -
                                       SA-OVDLN-OD-INT OF SAACNAMT
                COMPUTE WK-INTC-AMT = SA-INTC-AMT OF SAACNAMT
                                    + I-AMT OF PKSADEP0
                                    - WK-ACCT-BAL2 + WK-ACCT-BAL1
                COMPUTE WK-INTC-AMT-SUB = SA-INTC-AMT OF SAACNAMT 
                                        - WK-INTC-AMT
                IF WK-INTC-AMT-SUB < ZEROS
                THEN
                  COMPUTE WK-INTC-AMT-ADD = 0 - WK-INTC-AMT-SUB
                  MOVE ZEROS             TO WK-INTC-AMT-SUB
                END-IF
                COMPUTE WK-AVL-BAL = WK-INTC-AMT 
                                   - SA-FRZ-AMT OF SAACNAMT 
                                   - SA-AUTH-AMT OF SAACNAMT
                IF WK-AVL-BAL < ZEROS
                THEN
                  MOVE ZEROS            TO WK-AVL-BAL
                END-IF
               END-IF
              END-IF
             END-IF
           ELSE
             COMPUTE WK-INTC-AMT = SA-INTC-AMT OF SAACNAMT
                                + I-AMT OF PKSADEP0
             MOVE SA-OD-AMT OF SAACNAMT
                                      TO WK-OD-AMT
             COMPUTE WK-INTC-AMT-ADD = 0 + I-AMT OF PKSADEP0
             COMPUTE WK-AVL-BAL = WK-INTC-AMT - SA-FRZ-AMT OF SAACNAMT 
                                - SA-AUTH-AMT OF SAACNAMT 
             IF WK-AVL-BAL < ZEROS
             THEN
               MOVE ZEROS            TO WK-AVL-BAL
             END-IF
           END-IF.
      
       
     **     帐户余额上限查询  
       4130-CHECK-AMT-LMT-RTN.
           IF I-CURR-COD OF PKSADEP0 NOT = C-CM-RMB-COD
             AND WK-ACCT-BAL > SA-ACCT-BAL-ULMT OF SAACNAMT
           THEN
             MOVE 'WN014'              TO AIF-MSG-CODE
             PERFORM 9999-MESSAGE-HANDLE-RTN
           END-IF.
      
       4140-PROCESS-PRD-RTN.

           IF PD-CLSD-INTC-TYP OF O-PDPRTSAI OF PCPDPSA0 NOT =
                                                        C-SA-N-INT-CAL
           THEN
             PERFORM 4141-CAL-NORMAL-PRDT-RTN
      *      PERFORM 4145-PRD-BEF-RTN
      *      只有联机的倒起息
             IF I-VAL-DT OF PKSADEP0 < SYS-BUSN-DT 
      *        AND  SYS-TX-MODE NOT = (  C-CM-OLCC-MODE AND
      *                              C-CM-CENTERCUT-MODE )
             THEN
               PERFORM 4142-CAL-PRDT-ADJ-RTN
             END-IF
      
             COMPUTE WK-DDP-PDT = SA-DDP-PDT OF SAACNAMT
                    + WK-NORMAL-PRDT + WK-ADD-PRDT
           END-IF.
      *    计算积数
       4141-CAL-NORMAL-PRDT-RTN.
           IF SA-NGO-AVAL-DT OF SAACNAMT NOT = SPACES
           THEN
             IF SA-NGO-AVAL-DT OF SAACNAMT NOT > SYS-BUSN-DT
             THEN
               MOVE 'N'                TO WK-FILE-READ-FLAG
               MOVE 1                  TO WK-NUM
               PERFORM 5610-GET-AGR-RTN UNTIL WK-FILE-EOF
               PERFORM 5611-CLOSE-SAACNAGR-RTN               
               MOVE WK-SA-NGO-AMT(1)   TO I-NGO-FDEP-AMT1 OF PCSAPRD2
               MOVE WK-SA-NGO-AMT(2)   TO I-NGO-FDEP-AMT2 OF PCSAPRD2
               MOVE WK-SA-NGO-AMT(3)   TO I-NGO-FDEP-AMT3 OF PCSAPRD2
               MOVE WK-SA-NGO-AMT(4)   TO I-NGO-FDEP-AMT4 OF PCSAPRD2
               MOVE WK-SA-NGO-AMT(5)   TO I-NGO-FDEP-AMT5 OF PCSAPRD2
               
             END-IF
           END-IF.
DEBUG      MOVE PD-INTC-TYPE OF O-PDPRTSAI OF PCPDPSA0
                                       TO I-INTC-TYPE       OF PCSAPRD2.
           MOVE SA-INTC-AMT OF SAACNAMT
                                       TO I-INTC-AMT        OF PCSAPRD2.
           IF WK-24H-MODE = '2' AND
                SYS-TX-MODE =  C-CM-CENTERCUT-MODE  AND
                SA-LTM-PDTC-DT OF SAACNAMT  > SYS-BUSN-DT  
           THEN                   
             MOVE SYS-BUSN-DT          TO I-LTM-PDTC-DT     OF PCSAPRD2
           ELSE
            MOVE SA-LTM-PDTC-DT OF SAACNAMT
                                       TO I-LTM-PDTC-DT     OF PCSAPRD2
           END-IF.
           MOVE SYS-BUSN-DT            TO I-THIS-PDTC-DT    OF PCSAPRD2.
           MOVE I-CURR-COD OF PKSADEP0 TO I-CURR-COD        OF PCSAPRD2.
           MOVE SA-NGO-AVAL-DT OF SAACNAMT
                                       TO I-AVAL-DT         OF PCSAPRD2.
           MOVE SA-NGO-CNCL-DT OF SAACNAMT
                                       TO I-CACL-DATE       OF PCSAPRD2.
           IF PD-NGO-FLG OF O-PDPRTSAI OF PCPDPSA0 =  C-SA-INT-MODE-1 OR 
              PD-NGO-FLG OF O-PDPRTSAI OF PCPDPSA0 =  C-SA-INT-MODE-2
           THEN 
              MOVE PD-NGO-FLG OF O-PDPRTSAI OF PCPDPSA0  
                                       TO  I-SA-INT-MODE OF PCSAPRD2
           ELSE
              MOVE SPACES              TO I-SA-INT-MODE OF PCSAPRD2
           END-IF
      *    7x24小时的日终CC程序，会出现上次计算积数日大于当前营业日
           IF I-LTM-PDTC-DT OF PCSAPRD2 <= I-THIS-PDTC-DT OF PCSAPRD2
           THEN
            CALL 'CSAPRD2'              USING AIF-AREA
                                              PCSAPRD2
                                              SYS-AREA
             PERFORM 9999-MESSAGE-HANDLE-RTN
           END-IF.
            
           IF O-MSG-TYPE OF PCSAPRD2 NOT = SPACE
           THEN
             MOVE O-RTRN-CODE OF PCSAPRD2
                                       TO AIF-MSG-CODE
             PERFORM 9999-MESSAGE-HANDLE-RTN
           END-IF.
           MOVE O-PRDT OF PCSAPRD2     TO WK-NORMAL-PRDT.
           MOVE O-NGO-PRDT(1)          TO WK-O-NGO-PRDT(1).
           MOVE O-NGO-PRDT(2)          TO WK-O-NGO-PRDT(2).
           MOVE O-NGO-PRDT(3)          TO WK-O-NGO-PRDT(3).
           MOVE O-NGO-PRDT(4)          TO WK-O-NGO-PRDT(4).
           MOVE O-NGO-PRDT(5)          TO WK-O-NGO-PRDT(5).
      
       4142-CAL-PRDT-ADJ-RTN.
           INITIALIZE                  PCSAPRD2.
           MOVE PD-INTC-TYPE OF O-PDPRTSAI OF PCPDPSA0
                                       TO I-INTC-TYPE       OF PCSAPRD2.
           MOVE I-AMT OF PKSADEP0      TO I-INTC-AMT        OF PCSAPRD2.
           MOVE I-VAL-DT OF PKSADEP0   TO I-LTM-PDTC-DT     OF PCSAPRD2.
           MOVE SYS-BUSN-DT            TO I-THIS-PDTC-DT    OF PCSAPRD2.
           MOVE SA-CURR-COD OF SAACNAMT
                                       TO I-CURR-COD        OF PCSAPRD2.
           IF PD-NGO-FLG OF O-PDPRTSAI OF PCPDPSA0 =  C-SA-INT-MODE-1 OR 
              PD-NGO-FLG OF O-PDPRTSAI OF PCPDPSA0 =  C-SA-INT-MODE-2
           THEN 
              MOVE PD-NGO-FLG OF O-PDPRTSAI OF PCPDPSA0  
                                       TO I-SA-INT-MODE     OF PCSAPRD2
           ELSE
              MOVE SPACES              TO I-SA-INT-MODE     OF PCSAPRD2
           END-IF
           CALL 'CSAPRD2'        USING AIF-AREA
                                       PCSAPRD2
                                       SYS-AREA.
           PERFORM 9999-MESSAGE-HANDLE-RTN.
           IF O-MSG-TYPE OF PCSAPRD2 NOT = SPACE
           THEN
             MOVE O-RTRN-CODE OF PCSAPRD2
                                       TO AIF-MSG-CODE
             PERFORM 9999-MESSAGE-HANDLE-RTN
           END-IF.
           MOVE O-PRDT OF PCSAPRD2     TO WK-ADD-PRDT.
      
       4143-CAL-PRDT-OD-RTN.
           INITIALIZE                  PCSAPRD4.
           MOVE SA-OVDLN-OD-INT OF SAACNAMT
                                       TO I-OVDLN-OD-INT-15 OF PCSAPRD4.
           MOVE SA-OD-INT OF SAACNAMT
                                       TO I-OD-INT-15       OF PCSAPRD4.
           MOVE SA-OD-INT-AMT OF SAACNAMT
                                       TO I-OD-AMT          OF PCSAPRD4.
           MOVE SA-OD-DAYS-N OF SAACNAMT
                                       TO IO-OD-DAYS-N      OF PCSAPRD4.
           MOVE SA-OD-INT-DAYS-N OF SAACNAMT
                                       TO IO-OD-INT-DAYS-N  OF PCSAPRD4.
           MOVE SA-LTM-PDTC-DT OF SAACNAMT
                                       TO I-LTM-PDTC-DT     OF PCSAPRD4.
           MOVE SYS-BUSN-DT            TO I-THIS-PDTC-DT    OF PCSAPRD4.
           MOVE SA-CURR-COD OF SAACNAMT
                                       TO I-CURR-COD        OF PCSAPRD4.
           CALL 'CSAPRD4'              USING AIF-AREA
                                             PCSAPRD4
                                             SYS-AREA.
           PERFORM 9999-MESSAGE-HANDLE-RTN.
           IF O-MSG-TYPE OF PCSAPRD4 NOT = SPACE
           THEN
             MOVE O-RTRN-CODE OF PCSAPRD4
                                       TO AIF-MSG-CODE
             PERFORM 9999-MESSAGE-HANDLE-RTN
           END-IF.
      *    透支积数 = 原透支积数 - 变更前当日计透支息金额 + 变更后当日计透支息金额
      *    24小时批处理时的透支积数 PCSAPRD4 返回的透支积数一定为0
           IF WK-24H-MODE = '2' AND
              SYS-TX-MODE =  C-CM-CENTERCUT-MODE  AND
              SA-LTM-TX-DT OF SAACNAMT  = SYS-BUSN-DT
           THEN 
              COMPUTE WK-OD-PRD =  SA-OD-PDT OF SAACNAMT 
                                -  WK-OD-INT-AMT-SUB
              MOVE  WK-OD-PRD          TO  O-OD-PDT OF PCSAPRD4
           END-IF.
       4145-PRD-BEF-RTN.
      *      批处理发生业务,由7x24小时导致的倒记积数时，不再通过倒起息的方式补记积数
           IF WK-24H-MODE = '2' AND
              SYS-TX-MODE =  C-CM-CENTERCUT-MODE  AND
              SA-LTM-PDTC-DT OF SAACNAMT  > SYS-BUSN-DT  AND
               I-VAL-DT OF PKSADEP0 = SYS-BUSN-DT              
           THEN
             IF SA-NGO-AVAL-DT OF SAACNAMT NOT = SPACES AND
                SA-NGO-AVAL-DT OF SAACNAMT NOT > SYS-BUSN-DT 
             THEN
      *      协议需求中，只存在1档  该逻辑段时，上次计算积数日一定为SYS-BUSN-DT
              IF I-CURR-COD OF PKSADEP0 = C-CM-RMB-COD
              THEN
      *           原多金额未到协议档金额
                 IF WK-SA-NGO-AMT(1) > SA-INTC-AMT OF SAACNAMT
                 THEN
                    MOVE SA-INTC-AMT  OF SAACNAMT
                                     TO  WK-BEF-TMP-PRD 
                    MOVE ZEROS       TO  WK-BEF-NTMP-PRD    
                 ELSE
                    MOVE WK-SA-NGO-AMT(1)
                                     TO  WK-BEF-TMP-PRD 
                    COMPUTE WK-BEF-NTMP-PRD = SA-INTC-AMT OF SAACNAMT
                                            - WK-SA-NGO-AMT(1)
                 END-IF
      *          WK-INTC-AMT变更后计息金额             
                 IF WK-SA-NGO-AMT(1) > WK-INTC-AMT
                 THEN
                    MOVE WK-INTC-AMT
                                     TO  WK-AF-TMP-PRD 
                    MOVE  ZEROS      TO  WK-AF-NTMP-PRD 
                 ELSE
                    MOVE WK-SA-NGO-AMT(1)
                                     TO  WK-AF-TMP-PRD 
                    COMPUTE WK-AF-NTMP-PRD = WK-INTC-AMT
                                            - WK-SA-NGO-AMT(1)
                 END-IF
                 COMPUTE WK-NORMAL-PRDT = SA-DDP-PDT OF SAACNAMT
                                        - WK-BEF-TMP-PRD 
                                        + WK-AF-TMP-PRD
                 COMPUTE WK-O-NGO-PRDT(1) = 0 
                                          - WK-BEF-NTMP-PRD 
                                          + WK-AF-NTMP-PRD
               ELSE
      *         有协定存款且为外币时 且协定只有1档 
      *         外币时，超过协议档，全部金额都计协议利息
      *         变更前计息金额和变更后计息金额与外币起存金额 的关系
                 IF SA-INTC-AMT OF SAACNAMT < WK-SA-NGO-AMT(1) AND
                    WK-INTC-AMT < WK-SA-NGO-AMT(1)
                 THEN
                   COMPUTE  WK-NORMAL-PRDT = SA-DDP-PDT OF SAACNAMT
                                        - SA-INTC-AMT OF SAACNAMT 
                                        + WK-INTC-AMT
                 END-IF
                 IF SA-INTC-AMT OF SAACNAMT > WK-SA-NGO-AMT(1) AND
                    WK-INTC-AMT > WK-SA-NGO-AMT(1)
                 THEN
                   COMPUTE WK-O-NGO-PRDT(1) = 0 
                                            - SA-INTC-AMT OF SAACNAMT 
                                            + WK-INTC-AMT
                   MOVE SA-DDP-PDT   OF SAACNAMT 
                                     TO WK-NORMAL-PRDT                          
                 END-IF
                 IF SA-INTC-AMT OF SAACNAMT < WK-SA-NGO-AMT(1) AND
                    WK-INTC-AMT > WK-SA-NGO-AMT(1)
                 THEN
                   COMPUTE  WK-NORMAL-PRDT = SA-DDP-PDT  OF SAACNAMT
                                           - SA-INTC-AMT OF SAACNAMT 
                   COMPUTE WK-O-NGO-PRDT(1) = 0 
                                            + WK-INTC-AMT
                 END-IF
                 IF SA-INTC-AMT OF SAACNAMT > WK-SA-NGO-AMT(1) AND
                    WK-INTC-AMT < WK-SA-NGO-AMT(1)
                 THEN
                   COMPUTE  WK-NORMAL-PRDT = SA-DDP-PDT  OF SAACNAMT
                                           + SA-INTC-AMT OF SAACNAMT 
                   COMPUTE WK-O-NGO-PRDT(1) = 0
                                            - WK-INTC-AMT
                 END-IF
               END-IF 
             ELSE
      *       不存在协定存款
               COMPUTE  WK-NORMAL-PRDT = SA-DDP-PDT OF SAACNAMT
                                       - SA-INTC-AMT OF SAACNAMT 
                                       + WK-INTC-AMT
             END-IF 
              
      *     重复计算积数,再些将原积数删除，该值可能为负值
             COMPUTE WK-NORMAL-PRDT = WK-NORMAL-PRDT 
                                    - SA-DDP-PDT OF SAACNAMT
           
           END-IF.
       4150-UPDATE-ACN-AMT-RTN.
      * UPDATE SAACNAMT
           INITIALIZE PDBIMAIN.
           MOVE FT-RTRN-NOTFOUND       TO DBI-NORMAL-STATUS(1).
           MOVE 'BSAACND'              TO DBI-DB-NAME.
           MOVE FT-GHU                 TO DBI-FT-NAME.
           MOVE I-ACCT-NO OF PKSADEP0  TO DBI-KEY-VALUE1(1).
           MOVE 'SAACNAMT'             TO DBI-SEGMENT-NAME(2).
           MOVE I-CURR-COD OF PKSADEP0 TO WK-CURR-COD.
           MOVE I-CURR-IDEN OF PKSADEP0
                                       TO WK-CURR-IDEN.
           MOVE WK-SAACNAMT-KEY        TO DBI-KEY-VALUE1(2).
           CALL 'GDBIMAIN'             USING PDBIMAIN
                                             SAACNAMT
                                             AIF-AREA.
           PERFORM 9999-MESSAGE-HANDLE-RTN.
           IF  DBI-DB-STATUS = FT-RTRN-NOTFOUND
           THEN 
              MOVE 'EN597'             TO AIF-MSG-CODE 
              PERFORM 9999-MESSAGE-HANDLE-RTN
           END-IF
           MOVE WK-AVL-BAL             TO SA-AVL-BAL        OF SAACNAMT.
           MOVE WK-INTC-AMT            TO SA-INTC-AMT       OF SAACNAMT.
           MOVE WK-DDP-PDT             TO SA-DDP-PDT        OF SAACNAMT.
           MOVE WK-OD-AMT              TO SA-OD-AMT         OF SAACNAMT.
           COMPUTE SA-OD-INT OF SAACNAMT = SA-OD-INT OF SAACNAMT -
                   WK-OD-INT-SUB.
           COMPUTE SA-OD-INT-AMT OF SAACNAMT = SA-OD-INT-AMT OF SAACNAMT
                   - WK-OD-INT-AMT-SUB.
           COMPUTE SA-OVDLN-OD-INT OF SAACNAMT =
                   SA-OVDLN-OD-INT OF SAACNAMT - WK-OVDLN-OD-INT-SUB.
           COMPUTE SA-OD-INT-DAYS-N OF SAACNAMT =
                          IO-OD-INT-DAYS-N OF PCSAPRD4 + 0
           MOVE IO-OD-INT-DAYS-N OF PCSAPRD4
                                       TO WK-OD-INT-DAYS-N
           COMPUTE SA-OD-DAYS-N OF SAACNAMT =
                  IO-OD-DAYS-N OF PCSAPRD4 + 0
           MOVE IO-OD-DAYS-N OF PCSAPRD4
                                       TO WK-OD-DAYS-N
           COMPUTE SA-OD-PDT OF SAACNAMT = SA-OD-PDT OF SAACNAMT +
                                         + O-OD-PDT OF PCSAPRD4.
           COMPUTE SA-OD-CSH-AMT OF SAACNAMT = SA-OD-CSH-AMT OF SAACNAMT
                                         + O-OD-INT-PDT OF PCSAPRD4.
           COMPUTE SA-ACTU-PDT OF SAACNAMT = SA-DDP-PDT OF SAACNAMT -
                                             SA-OD-PDT OF SAACNAMT.
           COMPUTE WK-ACCT-BAL = SA-INTC-AMT OF SAACNAMT
                               - SA-OD-INT-AMT OF SAACNAMT
                               - SA-OD-INT OF SAACNAMT
                               - SA-OVDLN-OD-INT OF SAACNAMT.
           MOVE WK-ACCT-BAL            TO SA-ACCT-BAL OF SAACNAMT.
           IF SA-LTM-TX-DT OF SAACNAMT NOT = SYS-BUSN-DT
           THEN
             MOVE ZEROS               TO SA-TODAY-OD-CSH-AMT OF SAACNAMT
           END-IF.
     
           MOVE SA-LTM-TX-DT OF SAACNAMT
                                       TO KBN-LTM-TX-DT.
      *    7x24小时的日终CC程序，会出现上次计算积数日大于当前营业日 ,此时不再更新时间                                 
           IF  SA-LTM-PDTC-DT OF SAACNAMT <= SYS-BUSN-DT
           THEN                              
             MOVE SYS-BUSN-DT          TO SA-LTM-TX-DT      OF SAACNAMT
             MOVE SYS-BUSN-DT          TO SA-LTM-PDTC-DT    OF SAACNAMT
           END-IF  
           IF SA-FX-ACCT-CHAR OF SAACNACN = '2102'
             AND I-DSCRP-COD OF PKSADEP0 = C-MEM-SA-6002
           THEN
      *     视钞为汇金额
             COMPUTE SA-AS-EXC-AMT OF SAACNAMT = SA-AS-EXC-AMT OF
                     SAACNAMT + I-AMT OF PKSADEP0
           END-IF.
      
           IF SA-FX-ACCT-CHAR OF SAACNACN = '2102'
             AND I-DSCRP-COD OF PKSADEP0 = C-MEM-SA-6005
           THEN
             COMPUTE SA-AS-EXC-AMT OF SAACNAMT = SA-AS-EXC-AMT OF
                     SAACNAMT + I-AMT OF PKSADEP0
           END-IF.
      
           IF SA-FX-ACCT-CHAR OF SAACNACN = '2102' AND
            (I-DSCRP-COD OF PKSADEP0 = C-MEM-SA-6003 OR C-MEM-SA-6004)
           THEN
             COMPUTE SA-CLN-AMT OF SAACNAMT = SA-CLN-AMT OF
                     SAACNAMT - I-AMT OF PKSADEP0
           END-IF.
           IF SA-CLN-AMT OF SAACNAMT < ZEROS
           THEN
             MOVE 'EN304'              TO AIF-MSG-CODE
      *      MOVE '资本金账户，可入账限额不足'
      *                                TO AIF-MSG-TEXT
             PERFORM 9999-MESSAGE-HANDLE-RTN
           END-IF.
      
     
           IF SA-INTC-AMT OF SAACNAMT < SA-LOWEST-BAL OF SAACNAMT
           THEN
               MOVE SA-INTC-AMT OF SAACNAMT
                                       TO SA-LOWEST-BAL OF SAACNAMT
           END-IF.
      

           MOVE 'BSAACND'              TO DBI-DB-NAME.
           MOVE FT-REPL                TO DBI-FT-NAME.
           CALL 'GDBIMAIN' USING PDBIMAIN
                                 SAACNAMT
                                 AIF-AREA.
           PERFORM 9999-MESSAGE-HANDLE-RTN.
      
      *    UPDATE SAACNACN
           INITIALIZE PDBIMAIN.
           MOVE FT-RTRN-NOTFOUND       TO DBI-NORMAL-STATUS(1).
           MOVE 'BSAACND'              TO DBI-DB-NAME.
           MOVE 'SAACNACN'             TO DBI-SEGMENT-NAME(1).
           MOVE FT-GHU                 TO DBI-FT-NAME.
           MOVE I-ACCT-NO OF PKSADEP0  TO DBI-KEY-VALUE1(1).
           CALL 'GDBIMAIN' USING PDBIMAIN
                                 SAACNACN
                                 AIF-AREA.
           PERFORM 9999-MESSAGE-HANDLE-RTN.
     
           COMPUTE SA-DET-ITEM-N OF SAACNACN = SA-DET-ITEM-N OF SAACNACN
                                       + WK-DET-ITEM.
           COMPUTE SA-ACC-UNPRINT-NO OF SAACNACN = SA-ACC-UNPRINT-NO 
                                                             OF SAACNACN
                                                + WK-DET-ITEM.
      
           IF WK-SA-LAST-TXN-DT = C-CM-FLAG-YES 
           THEN
             MOVE SYS-BUSN-DT(3:6)     TO SA-LAST-TXN-DT OF SAACNACN
           END-IF.
           INITIALIZE PDBIMAIN.
           MOVE 'BSAACND'              TO DBI-DB-NAME.
           MOVE FT-REPL                TO DBI-FT-NAME.
           CALL 'GDBIMAIN' USING PDBIMAIN
                                 SAACNACN
                                 AIF-AREA.
           PERFORM 9999-MESSAGE-HANDLE-RTN.
      
       4160-INSERT-TXN-RTN.
           MOVE 1                      TO WK-DET-ITEM.
           COMPUTE SA-DDP-ACCT-NO-DET-N OF SAACNTXN =
                   SA-DET-ITEM-N OF SAACNACN + WK-DET-ITEM
           MOVE I-CURR-COD OF PKSADEP0 TO SA-CURR-COD       OF SAACNTXN.
           MOVE WK-SYS-TELLER-ID       TO SA-OPR-NO         OF SAACNTXN.
           MOVE I-CURR-IDEN OF PKSADEP0
                                       TO SA-CURR-IDEN      OF SAACNTXN.
           MOVE C-CLG-TXT-NOR          TO SA-EC-FLG         OF SAACNTXN.
           IF I-ACCT-NO-EC OF PKSADEP0  = C-CM-FLAG-YES
           THEN
             COMPUTE SA-DR-AMT OF SAACNTXN = 0 - I-AMT OF PKSADEP0
             MOVE SA-DR-AMT OF SAACNTXN  
                                       TO SA-TX-AMT         OF SAACNTXN
             MOVE 0                    TO SA-CR-AMT         OF SAACNTXN
           ELSE
             COMPUTE SA-CR-AMT OF SAACNTXN = 0 + I-AMT OF PKSADEP0
             MOVE SA-CR-AMT OF SAACNTXN 
                                       TO SA-TX-AMT         OF SAACNTXN
             MOVE 0                    TO SA-DR-AMT         OF SAACNTXN
           END-IF.
           COMPUTE SA-DDP-ACCT-BAL OF SAACNTXN =
                   SA-ACCT-BAL OF SAACNAMT + I-AMT OF PKSADEP0.
           MOVE I-TX-TYP OF PKSADEP0   TO SA-TX-TYP         OF SAACNTXN.
           MOVE WK-SYS-TX-LOG-NO       TO SA-TX-LOG-NO      OF SAACNTXN.
           MOVE I-DOC-NO OF PKSADEP0   TO SA-DOC-NO         OF SAACNTXN.
           MOVE I-DOC-TYP OF PKSADEP0  TO SA-DOC-TYP        OF SAACNTXN.
           MOVE I-VAL-DT OF PKSADEP0   TO SA-VAL-DT         OF SAACNTXN.
           MOVE WK-SYS-BRANCH-STD      TO SA-OPUN-COD       OF SAACNTXN.
           MOVE I-DSCRP-COD OF PKSADEP0 
                                       TO SA-DSCRP-COD      OF SAACNTXN.
      *    备注1的长度不够
           IF WK-OD-INT-SUB > ZEROS OR
              WK-OVDLN-OD-INT-SUB > ZEROS OR
              WK-OD-INT-AMT-SUB > ZEROS
           THEN
             MOVE 'OD'                 TO WK-RMRK(1:2)
             MOVE WK-OD-INT-SUB        TO ODODOD
             MOVE 'OVD'                TO WK-RMRK(11:3)
             MOVE WK-OVDLN-OD-INT-SUB
                                       TO OVDOVD
             MOVE 'AMT'                TO WK-RMRK(22:3)
             MOVE WK-OD-INT-AMT-SUB
                                       TO AMTAMT
             MOVE SA-OD-LG-FLG OF SAACNAMT
                                       TO WK-RMRK(40:1)
             MOVE WK-RMRK              TO SA-RMRK-1          OF SAACNTXN
           ELSE                                             
             MOVE SPACE                TO SA-RMRK-1         OF SAACNTXN
           END-IF                                           
           IF WK-BRH-FLG = C-CM-FLAG-YES
           THEN
      *      MOVE O-ACCT-NAME OF PKSATXN1
      *                                 TO SA-RMRK          OF SAACNTXN
      *      集团客户子公司账号信息
             MOVE WK-I-ACCT-NO         TO SA-OP-ACCT-NO-32  OF SAACNTXN
             MOVE WK-BRA-OPAC-NO       TO SA-OP-BANK-NO     OF SAACNTXN
             MOVE WK-BRA-ACCT-NAME     TO SA-OP-CUST-NAME   OF SAACNTXN
             MOVE '2'                  TO SA-TX-TYP         OF SAACNTXN                            
      *    ELSE                                             
      *      MOVE I-RMRK OF PKSADEP0    TO SA-RMRK          OF SAACNTXN
           END-IF       
           MOVE I-RMRK OF PKSADEP0    TO SA-RMRK          OF SAACNTXN                                    
           MOVE I-TX-DT OF PKSADEP0    TO SA-TX-DT          OF SAACNTXN.
           MOVE SYS-BUSN-DT            TO SA-TX-DT          OF SAACNTXN.
           MOVE SYS-CPU-TM2            TO SA-TX-TM          OF SAACNTXN.
           MOVE SYS-CPU-DT             TO SA-SYS-DT         OF SAACNTXN.

             IF AIF-OPPO-ACCOUNT-NO NOT = SPACES
             THEN
               IF AIF-OPPO-ACCOUNT-NO = I-ACCT-NO OF PKSADEP0
               THEN
                 MOVE AIF-ACCOUNT-NO(1:19)
                                       TO SA-TX-CRD-NO       OF SAACNTXN
                 MOVE AIF-ACCOUNT-NO(20:13)
                                       TO SA-CUST-DOCAG-STNO OF SAACNTXN
               ELSE
                 MOVE AIF-OPPO-ACCOUNT-NO(1:19)
                                       TO SA-TX-CRD-NO      OF SAACNTXN
                 MOVE AIF-OPPO-ACCOUNT-NO(20:13)
                                       TO SA-CUST-DOCAG-STNO
                                          OF SAACNTXN(1:13)
               END-IF
             END-IF
      *      集团客户子公司归集，则登记子公司账号到SA-TX-CRD-NO、SA-CUST-DOCAG-STNO-两个栏位共同记录账号信息
             IF WK-BRH-FLG = C-CM-FLAG-YES
             THEN
               MOVE WK-I-ACCT-NO(1:19)
                                       TO SA-TX-CRD-NO      OF SAACNTXN
               MOVE WK-I-ACCT-NO(20:13)
                                       TO SA-CUST-DOCAG-STNO
                                          OF SAACNTXN(1:13)
             END-IF.
           MOVE WK-DDP-PDT             TO SA-DDP-PDT        OF SAACNTXN.
           MOVE SYS-APP-TX-CODE        TO SA-APP-TX-CODE    OF SAACNTXN.
           MOVE WK-PRT-FLG             TO SA-PRT-FLG        OF SAACNTXN.
           MOVE I-PSBK-PRT-NO OF PKSADEP0
                                       TO SA-MEM-NO          OF SAACNTXN
           MOVE WK-LEGAL-PERSON-ID     TO SA-LEGAL-PERSON-ID OF SAACNTXN
           MOVE WK-DB-PARTITION-ID     TO SA-DB-PARTITION-ID OF SAACNTXN
           MOVE I-AGT-CER-TYP   OF PKSADEP0      
                                       TO SA-AGT-CERT-TYP   OF SAACNTXN
           MOVE I-AGT-CER-ID    OF PKSADEP0      
                                       TO SA-AGT-CERT-ID    OF SAACNTXN
           MOVE I-AGT-CUST-NAME OF PKSADEP0      
                                       TO SA-AGT-CUST-NAME  OF SAACNTXN
           MOVE I-AGT-CUST-TEL  OF PKSADEP0
                                       TO SA-AGT-CUST-TEL   OF SAACNTXN                          
           
           MOVE SYS-LCH-CHANNEL-FLAG   TO SA-LCH-CHAN-NO    OF SAACNTXN.
           MOVE SYS-ACC-CHANNEL-FLAG   TO SA-ACC-CHAN-NO    OF SAACNTXN.
           INITIALIZE                  PDBIMAIN.
           MOVE FT-RTRN-NOTFOUND       TO DBI-NORMAL-STATUS(1).
           MOVE 'BSAACND'              TO DBI-DB-NAME.
           MOVE FT-ISRT                TO DBI-FT-NAME.
           MOVE 'SAACNACN'             TO DBI-SEGMENT-NAME(1).
           MOVE I-ACCT-NO OF PKSADEP0  TO DBI-KEY-VALUE1(1)
           MOVE 'SAACNTXN'             TO DBI-SEGMENT-NAME(2).
           IF CWK-OPPO-CUST-NAME NOT = SPACES AND LOW-VALUES
              AND CWK-DR-CR-FLAG = C-ACA-CR-COD-RMRK 
              AND  WK-BRH-FLG NOT = C-CM-FLAG-YES
           THEN
             MOVE CWK-OPPO-CUST-NAME   TO SA-OP-CUST-NAME OF SAACNTXN
           END-IF.
      *    临时F协议起存金额和变更后积数登记使用
           MOVE WK-SA-NGO-AMT(1)         TO SA-FILLER5 OF SAACNTXN
           COMPUTE SA-FILLER6 OF SAACNTXN = WK-SA-NGO-PRDT(1)
                                          + WK-O-NGO-PRDT(1).
                                           
      *    针对行内转账交易，需要在BSAETXD中记录对方信息
           IF I-TX-TYP OF PKSADEP0 = C-CM-TXTYP-TR AND
              ( CWK-DRAWEE-ACCT-NO NOT = SPACES OR
                CWK-DRAWEE-NAME NOT = SPACES ) AND 
                WK-BRH-FLG NOT = C-CM-FLAG-YES
           THEN
             MOVE CWK-DRAWEE-ACCT-NO   TO SA-OP-ACCT-NO-32  OF SAACNTXN
             MOVE CWK-DRAWEE-BK-NO     TO SA-OP-BANK-NO     OF SAACNTXN
             MOVE CWK-DRAWEE-NAME      TO SA-OP-CUST-NAME   OF SAACNTXN                           
           END-IF.
           CALL 'GDBIMAIN'       USING PDBIMAIN
                                       SAACNTXN
                                       AIF-AREA.
           PERFORM 9999-MESSAGE-HANDLE-RTN.
      
      
           IF WK-PRT-FLG NOT = C-CM-FLAG-YES
           THEN
             PERFORM 9801-INSERT-PRT-RTN
           END-IF.
      
       4161-INSERT-TXN-RTN.
           MOVE 1                      TO WK-DET-ITEM.
           COMPUTE SA-DDP-ACCT-NO-DET-N OF SAACNTXN =
                   SA-DET-ITEM-N OF SAACNACN + WK-DET-ITEM
           MOVE I-CURR-COD OF PKSADEP0 TO SA-CURR-COD       OF SAACNTXN.
           MOVE WK-SYS-TELLER-ID       TO SA-OPR-NO         OF SAACNTXN.
           MOVE I-CURR-IDEN OF PKSADEP0
                                       TO SA-CURR-IDEN      OF SAACNTXN.
           MOVE C-CLG-TXT-NOR          TO SA-EC-FLG         OF SAACNTXN.
           IF I-ACCT-NO-EC OF PKSADEP0  = C-CM-FLAG-YES
           THEN
             COMPUTE SA-CR-AMT OF SAACNTXN = 0 - WK-OD-INT-SUB
                               - WK-OVDLN-OD-INT-SUB - WK-OD-INT-AMT-SUB
             MOVE SA-CR-AMT OF SAACNTXN   TO SA-TX-AMT      OF SAACNTXN
             MOVE 0                    TO SA-DR-AMT         OF SAACNTXN
             COMPUTE SA-DDP-ACCT-BAL OF SAACNTXN =
                   SA-ACCT-BAL OF SAACNAMT + SA-CR-AMT OF SAACNTXN
           ELSE
             COMPUTE SA-DR-AMT OF SAACNTXN = 0 + WK-OD-INT-SUB
                               + WK-OVDLN-OD-INT-SUB + WK-OD-INT-AMT-SUB
             MOVE SA-DR-AMT OF SAACNTXN TO SA-TX-AMT        OF SAACNTXN
             MOVE 0                    TO SA-CR-AMT         OF SAACNTXN
             COMPUTE SA-DDP-ACCT-BAL OF SAACNTXN =
                   SA-ACCT-BAL OF SAACNAMT - SA-DR-AMT      OF SAACNTXN
           END-IF.
           MOVE I-TX-TYP OF PKSADEP0   TO SA-TX-TYP         OF SAACNTXN.
           MOVE 'ZERO'                 TO SA-AUTH-NO(1:4).
           MOVE WK-SYS-TX-LOG-NO       TO SA-TX-LOG-NO      OF SAACNTXN.
           MOVE I-DOC-NO OF PKSADEP0   TO SA-DOC-NO         OF SAACNTXN.
           MOVE I-DOC-TYP OF PKSADEP0  TO SA-DOC-TYP        OF SAACNTXN.
           MOVE I-VAL-DT OF PKSADEP0   TO SA-VAL-DT         OF SAACNTXN.
           MOVE WK-SYS-BRANCH-STD      TO SA-OPUN-COD       OF SAACNTXN.
           MOVE C-TRF-DRW              TO SA-DSCRP-COD      OF SAACNTXN.
           IF WK-BRH-FLG = C-CM-FLAG-YES
           THEN
             MOVE O-ACCT-NAME OF PKSATXN1
                                        TO SA-RMRK OF SAACNTXN
           ELSE
             MOVE I-RMRK OF PKSADEP0    TO SA-RMRK OF SAACNTXN
           END-IF
           MOVE I-TX-DT OF PKSADEP0    TO SA-TX-DT OF SAACNTXN.
           MOVE SYS-BUSN-DT            TO SA-TX-DT OF SAACNTXN.
           MOVE SYS-CPU-TM2            TO SA-TX-TM.
           MOVE SYS-CPU-DT             TO SA-SYS-DT.
           MOVE WK-DDP-PDT             TO SA-DDP-PDT OF SAACNTXN.
           MOVE SYS-APP-TX-CODE        TO SA-APP-TX-CODE OF SAACNTXN.
           MOVE WK-PRT-FLG             TO SA-PRT-FLG OF SAACNTXN.
           MOVE I-AGT-CER-TYP   OF PKSADEP0      
                                       TO SA-AGT-CERT-TYP   OF SAACNTXN
           MOVE I-AGT-CER-ID    OF PKSADEP0      
                                       TO SA-AGT-CERT-ID    OF SAACNTXN
           MOVE I-AGT-CUST-NAME OF PKSADEP0      
                                       TO SA-AGT-CUST-NAME  OF SAACNTXN
           MOVE I-AGT-CUST-TEL  OF PKSADEP0
                                       TO SA-AGT-CUST-TEL   OF SAACNTXN  
           MOVE WK-LEGAL-PERSON-ID     TO SA-LEGAL-PERSON-ID OF 
                                                             SAACNTXN.
           MOVE WK-DB-PARTITION-ID     TO SA-DB-PARTITION-ID  OF
                                                             SAACNTXN.
           
           MOVE SYS-LCH-CHANNEL-FLAG   TO SA-LCH-CHAN-NO    OF SAACNTXN.
           MOVE SYS-ACC-CHANNEL-FLAG   TO SA-ACC-CHAN-NO    OF SAACNTXN.
           INITIALIZE PDBIMAIN.
           MOVE 'BSAACND'              TO DBI-DB-NAME.
           MOVE FT-ISRT                TO DBI-FT-NAME.
           MOVE 'SAACNACN'             TO DBI-SEGMENT-NAME(1).
           MOVE I-ACCT-NO OF PKSADEP0  TO DBI-KEY-VALUE1(1)
           MOVE 'SAACNTXN'             TO DBI-SEGMENT-NAME(2).
           IF CWK-OPPO-CUST-NAME NOT = SPACES AND LOW-VALUES
              AND CWK-DR-CR-FLAG = C-ACA-CR-COD-RMRK
           THEN
             MOVE CWK-OPPO-CUST-NAME   TO SA-OP-CUST-NAME OF SAACNTXN
           END-IF.
           CALL 'GDBIMAIN' USING PDBIMAIN
                                 SAACNTXN
                                 AIF-AREA.
           PERFORM 9999-MESSAGE-HANDLE-RTN.
      
           IF WK-PRT-FLG NOT = C-CM-FLAG-YES
           THEN
             PERFORM 9801-INSERT-PRT-RTN
           END-IF.
      
       4162-INSERT-TXN-RTN.
           ADD 1                       TO WK-DET-ITEM.
           COMPUTE SA-DDP-ACCT-NO-DET-N OF SAACNTXN =
                   SA-DET-ITEM-N OF SAACNACN + WK-DET-ITEM
           MOVE I-CURR-COD OF PKSADEP0 TO SA-CURR-COD OF SAACNTXN.
           MOVE WK-SYS-TELLER-ID       TO SA-OPR-NO OF SAACNTXN.
           MOVE I-CURR-IDEN OF PKSADEP0
                                       TO SA-CURR-IDEN OF SAACNTXN.
           MOVE C-CLG-TXT-NOR          TO SA-EC-FLG OF SAACNTXN.
           IF I-ACCT-NO-EC OF PKSADEP0  = C-CM-FLAG-YES
           THEN
             COMPUTE SA-DR-AMT OF SAACNTXN = 0 - WK-OD-INT-SUB
                               - WK-OVDLN-OD-INT-SUB - WK-OD-INT-AMT-SUB
             MOVE SA-DR-AMT OF SAACNTXN TO SA-TX-AMT OF SAACNTXN
             MOVE 0                    TO SA-CR-AMT OF SAACNTXN
             COMPUTE SA-DDP-ACCT-BAL OF SAACNTXN =
                   SA-ACCT-BAL OF SAACNAMT + 0
           ELSE
             COMPUTE SA-CR-AMT OF SAACNTXN = 0 + WK-OD-INT-SUB
                               + WK-OVDLN-OD-INT-SUB + WK-OD-INT-AMT-SUB
             MOVE SA-CR-AMT OF SAACNTXN TO SA-TX-AMT OF SAACNTXN
             MOVE 0                    TO SA-DR-AMT OF SAACNTXN
             COMPUTE SA-DDP-ACCT-BAL OF SAACNTXN =
                   SA-ACCT-BAL OF SAACNAMT + 0
           END-IF.
           MOVE I-TX-TYP OF PKSADEP0   TO SA-TX-TYP.
           MOVE WK-SYS-TX-LOG-NO       TO SA-TX-LOG-NO.
           MOVE I-DOC-NO OF PKSADEP0   TO SA-DOC-NO.
           MOVE I-DOC-TYP OF PKSADEP0  TO SA-DOC-TYP OF SAACNTXN.
           MOVE I-VAL-DT OF PKSADEP0   TO SA-VAL-DT.
           MOVE WK-SYS-BRANCH-STD      TO SA-OPUN-COD OF SAACNTXN.
           MOVE C-RETURN-PRINT         TO SA-DSCRP-COD OF SAACNTXN.
           IF WK-OD-INT-SUB > ZEROS OR WK-OVDLN-OD-INT-SUB > ZEROS
             OR WK-OD-INT-AMT-SUB > ZEROS
             MOVE 'OD'                 TO WK-RMRK(1:2)
             MOVE WK-OD-INT-SUB        TO ODODOD
             MOVE 'OVD'                TO WK-RMRK(11:3)
             MOVE WK-OVDLN-OD-INT-SUB
                                       TO OVDOVD
             MOVE 'AMT'                TO WK-RMRK(22:3)
             MOVE WK-OD-INT-AMT-SUB
                                       TO AMTAMT
             MOVE SA-OD-LG-FLG OF SAACNAMT
                                       TO WK-RMRK(40:1)
             MOVE WK-RMRK              TO SA-RMRK-1
           ELSE
             MOVE SPACE                TO SA-RMRK-1
           END-IF
           IF WK-BRH-FLG = C-CM-FLAG-YES
           THEN
             MOVE O-ACCT-NAME OF PKSATXN1
                                        TO SA-RMRK          OF SAACNTXN
           ELSE                                             
             MOVE I-RMRK OF PKSADEP0    TO SA-RMRK          OF SAACNTXN
           END-IF                                           
           MOVE I-TX-DT OF PKSADEP0    TO SA-TX-DT          OF SAACNTXN.
           MOVE SYS-BUSN-DT            TO SA-TX-DT          OF SAACNTXN.
           MOVE SYS-CPU-TM2            TO SA-TX-TM.
           MOVE SYS-CPU-DT             TO SA-SYS-DT.
           MOVE WK-DDP-PDT             TO SA-DDP-PDT        OF SAACNTXN.
           MOVE SYS-APP-TX-CODE        TO SA-APP-TX-CODE    OF SAACNTXN.
           MOVE WK-PRT-FLG             TO SA-PRT-FLG        OF SAACNTXN.
           MOVE WK-LEGAL-PERSON-ID     TO SA-LEGAL-PERSON-ID OF 
                                                             SAACNTXN.
           MOVE WK-DB-PARTITION-ID     TO SA-DB-PARTITION-ID  OF
                                                              SAACNTXN.
           MOVE SYS-LCH-CHANNEL-FLAG   TO SA-LCH-CHAN-NO    OF SAACNTXN.
           MOVE SYS-ACC-CHANNEL-FLAG   TO SA-ACC-CHAN-NO    OF SAACNTXN.
           INITIALIZE                  PDBIMAIN.
           MOVE 'BSAACND'              TO DBI-DB-NAME.
           MOVE FT-ISRT                TO DBI-FT-NAME.
           MOVE I-ACCT-NO OF PKSADEP0  TO DBI-KEY-VALUE1(1).
           MOVE 'SAACNACN'             TO DBI-SEGMENT-NAME(1).
           MOVE 'SAACNTXN'             TO DBI-SEGMENT-NAME(2).
           IF CWK-OPPO-CUST-NAME NOT = SPACES AND LOW-VALUES
              AND CWK-DR-CR-FLAG = C-ACA-CR-COD-RMRK
           THEN
             MOVE CWK-OPPO-CUST-NAME   TO SA-OP-CUST-NAME OF SAACNTXN
           END-IF.
           CALL 'GDBIMAIN' USING PDBIMAIN
                                 SAACNTXN
                                 AIF-AREA.
           PERFORM 9999-MESSAGE-HANDLE-RTN.
      
           IF WK-PRT-FLG NOT = C-CM-FLAG-YES
           THEN
             PERFORM 9801-INSERT-PRT-RTN
           END-IF.
      
       4180-INSERT-SAACNAGR.
      
           MOVE 1                      TO WK-SAACNAGR-KEY.
           MOVE 1                      TO WK-N.
           INITIALIZE                  PDBIMAIN.
           INITIALIZE                  SAACNAGR.
           MOVE FT-RTRN-NOTFOUND       TO DBI-NORMAL-STATUS(1).
           MOVE 'BSAACND'              TO DBI-DB-NAME.
           MOVE FT-GHNP                 TO DBI-FT-NAME.
           MOVE I-ACCT-NO OF PKSADEP0  TO DBI-KEY-VALUE1(1).
           MOVE 'SAACNACN'             TO DBI-SEGMENT-NAME(1).
           MOVE I-CURR-COD OF PKSADEP0 TO WK-CURR-COD OF
                                          WK-SAACNAMT-KEY.
           MOVE I-CURR-IDEN OF PKSADEP0
                                       TO WK-CURR-IDEN OF
                                          WK-SAACNAMT-KEY.
           MOVE WK-SAACNAMT-KEY        TO DBI-KEY-VALUE1(2).
           MOVE 'SAACNAMT'             TO DBI-SEGMENT-NAME(2).
           MOVE 'SAACNAGR'             TO DBI-SEGMENT-NAME(3).
           MOVE '> '                   TO DBI-OP1(3).
           MOVE '1'                    TO DBI-CMD-CODE(3 , 1).           
           CALL 'GDBIMAIN' USING PDBIMAIN
                                 SAACNAGR
                                 AIF-AREA.
           PERFORM 9999-MESSAGE-HANDLE-RTN.
           IF  DBI-DB-STATUS = FT-RTRN-NOTFOUND
           THEN
               PERFORM 4181-INSERT-SAACNAGR-SEG
                 UNTIL WK-SAACNAGR-KEY > 5
           ELSE
               PERFORM 4188-REPLACE-AGR
               PERFORM 4182-UPDATE-SAACNAGR-SEG
                 UNTIL WK-SAACNAGR-KEY > 5
               PERFORM 5611-CLOSE-SAACNAGR-RTN  
           END-IF.
      
       4181-INSERT-SAACNAGR-SEG.
      
           INITIALIZE                  PDBIMAIN.
           INITIALIZE                  SAACNAGR.
           MOVE 'BSAACND'              TO DBI-DB-NAME.
           MOVE I-ACCT-NO OF PKSADEP0  TO DBI-KEY-VALUE1(1).
           MOVE 'SAACNACN'             TO DBI-SEGMENT-NAME(1).
           MOVE I-CURR-COD OF PKSADEP0 TO WK-CURR-COD OF
                                          WK-SAACNAMT-KEY.
           MOVE I-CURR-IDEN OF PKSADEP0
                                       TO WK-CURR-IDEN OF
                                          WK-SAACNAMT-KEY.
           MOVE WK-SAACNAMT-KEY        TO DBI-KEY-VALUE1(2).
           MOVE 'SAACNAMT'             TO DBI-SEGMENT-NAME(2).
           MOVE 'SAACNAGR'             TO DBI-SEGMENT-NAME(3).
           MOVE WK-SAACNAGR-KEY        TO SA-NGO-FL-NO-N OF SAACNAGR.
           MOVE WK-O-NGO-PRDT(WK-N)    TO SA-NGO-PRDT OF SAACNAGR.
           MOVE WK-LEGAL-PERSON-ID     TO SA-LEGAL-PERSON-ID OF 
                                                             SAACNAGR.
           MOVE WK-DB-PARTITION-ID     TO SA-DB-PARTITION-ID  OF
                                                             SAACNAGR.
           MOVE FT-ISRT                TO DBI-FT-NAME.
           CALL 'GDBIMAIN' USING PDBIMAIN
                                 SAACNAGR
                                 AIF-AREA.
           PERFORM 9999-MESSAGE-HANDLE-RTN.
           ADD 1                       TO WK-N.
           ADD 1                       TO WK-SAACNAGR-KEY.
      
       4182-UPDATE-SAACNAGR-SEG.
           MOVE FT-RTRN-NOTFOUND       TO DBI-NORMAL-STATUS(1).
           MOVE 'BSAACND'              TO DBI-DB-NAME.
           MOVE FT-GHNP                TO DBI-FT-NAME.
           MOVE I-ACCT-NO OF PKSADEP0  TO DBI-KEY-VALUE1(1).
           MOVE 'SAACNACN'             TO DBI-SEGMENT-NAME(1).
           MOVE I-CURR-COD OF PKSADEP0 TO WK-CURR-COD OF
                                          WK-SAACNAMT-KEY.
           MOVE I-CURR-IDEN OF PKSADEP0
                                       TO WK-CURR-IDEN OF
                                          WK-SAACNAMT-KEY.
           MOVE WK-SAACNAMT-KEY        TO DBI-KEY-VALUE1(2).
           MOVE 'SAACNAMT'             TO DBI-SEGMENT-NAME(2).
           MOVE 'SAACNAGR'             TO DBI-SEGMENT-NAME(3).
           CALL 'GDBIMAIN' USING PDBIMAIN
                                 SAACNAGR
                                 AIF-AREA.
           PERFORM 9999-MESSAGE-HANDLE-RTN.
           MOVE 'BSAACND'              TO DBI-DB-NAME.
           MOVE I-ACCT-NO OF PKSADEP0  TO DBI-KEY-VALUE1(1).
           MOVE 'SAACNACN'             TO DBI-SEGMENT-NAME(1).
           MOVE I-CURR-COD OF PKSADEP0 TO WK-CURR-COD OF
                                          WK-SAACNAMT-KEY.
           MOVE I-CURR-IDEN OF PKSADEP0
                                       TO WK-CURR-IDEN OF
                                          WK-SAACNAMT-KEY.
           MOVE WK-SAACNAMT-KEY        TO DBI-KEY-VALUE1(2).
           MOVE 'SAACNAMT'             TO DBI-SEGMENT-NAME(2).
           MOVE 'SAACNAGR'             TO DBI-SEGMENT-NAME(3).
           MOVE WK-SAACNAGR-KEY        TO DBI-KEY-VALUE1(3).
           MOVE FT-REPL                TO DBI-FT-NAME.
           ADD  WK-O-NGO-PRDT(WK-N)    TO SA-NGO-PRDT OF SAACNAGR.
           CALL 'GDBIMAIN' USING PDBIMAIN
                                 SAACNAGR
                                 AIF-AREA.
           PERFORM 9999-MESSAGE-HANDLE-RTN.
           ADD 1                       TO WK-N.
           ADD 1                       TO WK-SAACNAGR-KEY.
      
       4188-REPLACE-AGR.
      
           MOVE 'BSAACND'              TO DBI-DB-NAME.
           MOVE I-ACCT-NO OF PKSADEP0  TO DBI-KEY-VALUE1(1).
           MOVE 'SAACNACN'             TO DBI-SEGMENT-NAME(1).
           MOVE I-CURR-COD OF PKSADEP0 TO WK-CURR-COD OF
                                          WK-SAACNAMT-KEY.
           MOVE I-CURR-IDEN OF PKSADEP0
                                       TO WK-CURR-IDEN OF
                                          WK-SAACNAMT-KEY.
           MOVE WK-SAACNAMT-KEY        TO DBI-KEY-VALUE1(2).
           MOVE 'SAACNAMT'             TO DBI-SEGMENT-NAME(2).
           MOVE 'SAACNAGR'             TO DBI-SEGMENT-NAME(3).
           MOVE WK-SAACNAGR-KEY        TO DBI-KEY-VALUE1(3).
           MOVE FT-REPL                TO DBI-FT-NAME.
           ADD  WK-O-NGO-PRDT(WK-N)    TO SA-NGO-PRDT OF SAACNAGR.
           CALL 'GDBIMAIN' USING PDBIMAIN
                                 SAACNAGR
                                 AIF-AREA.
           PERFORM 9999-MESSAGE-HANDLE-RTN.
           ADD 1                       TO WK-N.
           ADD 1                       TO WK-SAACNAGR-KEY.
      
      * 4188-CCICRF8-RTN.
      *
      *    INITIALIZE PCCICRF8.
      **   额度冲回
      *    MOVE C-CI-CRLMT-RL          TO I-CRLMT-FUNC OF PCCICRF8.
      *    MOVE SA-CUST-NO OF SAACNACN TO I-CUST-NO OF PCCICRF8.
      *    MOVE C-CI-SA-CRLMT-NO       TO I-CRLMT-NO OF PCCICRF8.
      *    MOVE I-ACCT-NO OF PKSADEP0
      *                                TO I-ACCT-NO OF PCCICRF8.
      *    MOVE C-CM-FLAG-NO
      *                                TO I-EC-FLG OF PCCICRF8.
      *    COMPUTE IO-AMT OF PCCICRF8 = 0 + WK-OD-INT-AMT-SUB.
      *    CALL 'CCICRF8'       USING  AIF-AREA
      *                                PCCICRF8
      *                                SYS-AREA.
      *    PERFORM 9999-MESSAGE-HANDLE-RTN.
      *    IF O-MSG-TYPE  OF PCCICRF8 = SPACES
      *      CONTINUE
      *    ELSE
      *      MOVE O-RTRN-CODE OF PCCICRF8
      *                                TO AIF-MSG-CODE
      *      PERFORM 9999-MESSAGE-HANDLE-RTN
      *    END-IF.
      
       4200-KBR-LOG-RTN.
           COMPUTE KBN-INTC-AMT = WK-INTC-AMT-ADD - WK-INTC-AMT-SUB.
           MOVE WK-OD-INT-SUB          TO KBN-OD-INT.
           MOVE WK-OVDLN-OD-INT-SUB    TO KBN-INT.
           MOVE WK-OD-INT-AMT-SUB      TO KBN-OD-INT-AMT.
           MOVE WK-OD-DAYS-N           TO KBN-OD-DAYS-N.
      *    MOVE WK-OD-INT-DAYS-N       TO KBN-ACC-UNPRINT-NO.
           MOVE WK-DET-ITEM            TO KBN-DET-ITEM-N-NO.
           MOVE SA-DET-ITEM-N OF SAACNACN
                                       TO KBN-DET-ITEM-N.                 
           MOVE WK-ADD-PRDT            TO KBN-ADD-PRDT.

           MOVE I-ACCT-NO OF PKSADEP0  TO KBN-ACCT-NO.
           MOVE I-CURR-IDEN OF PKSADEP0 TO KBN-CURR-IDEN.
           MOVE I-CURR-COD OF PKSADEP0 TO KBN-CURR-COD.
     
       4300-ARL-LOG-RTN.
           CONTINUE.
      
       4600-PROCESS-DATA-RTN-R.
           PERFORM 4610-PROCESS-AMT-RTN-R
           PERFORM 4630-PROCESS-PRD-RTN-R.
           COMPUTE WK-DET-ITEM-A = KBN-DET-ITEM-N -
                   KBN-DET-ITEM-N-NO.
           IF I-AMT OF PKSADEP0 > ZEROS
           THEN
             PERFORM 4650-PROCESS-TXN-RTN-R
             IF WK-FEA-FLG = C-CM-FLAG-YES
             THEN
               PERFORM 9723-CALL-CSACAA1-RTN
             END-IF
           END-IF.
           IF (KBN-OD-INT > ZEROS OR KBN-INT > ZEROS
             OR KBN-OD-INT-AMT > ZEROS) AND I-AMT OF PKSADEP0 = ZEROS
           THEN
             PERFORM 4651-PROCESS-TXN-RTN-R
             PERFORM 4652-PROCESS-TXN-RTN-R
           END-IF.
           PERFORM 4640-UPDATE-ACN-AMT-RTN-R.
      *    透支需求未通过
      *    IF KBN-OD-INT-AMT > ZEROS
      *    THEN
      *      PERFORM 4688-CCICRF8-RTN
      *    END-IF.
      *    IF SA-RECOG-TYP-NUM-N OF SAACNACN > ZEROS
      *      AND I-AMT OF PKSADEP0 > 0
           IF I-AMT OF PKSADEP0 > 0
           THEN
             PERFORM 5342-CALL-CCIATN1-RTN
           END-IF.
           IF I-AMT OF PKSADEP0 > 0
           THEN
              PERFORM 5344-CALL-CCIATN3-RTN
              CONTINUE
           END-IF.
           CONTINUE.
      
       4610-PROCESS-AMT-RTN-R.
           COMPUTE WK-INTC-AMT = SA-INTC-AMT OF SAACNAMT
                              - KBN-INTC-AMT.
           IF WK-INTC-AMT < ZEROS
           THEN
             MOVE 'EN249'              TO AIF-MSG-CODE
      *       MOVE '计息金额小于零'   TO AIF-MSG-TEXT
             PERFORM 9999-MESSAGE-HANDLE-RTN
           END-IF.

           COMPUTE WK-AVL-BAL = WK-INTC-AMT - SA-FRZ-AMT OF SAACNAMT
                              - SA-AUTH-AMT OF SAACNAMT.
           IF WK-AVL-BAL < ZEROS
             MOVE ZEROS                TO WK-AVL-BAL
           END-IF.
      
       
      
       4630-PROCESS-PRD-RTN-R.
           IF SA-DDP-PDT OF SAACNAMT < KBN-ADD-PRDT
           THEN
             MOVE 'EN299'              TO AIF-MSG-CODE
             PERFORM 9999-MESSAGE-HANDLE-RTN
           END-IF.
           COMPUTE WK-DDP-PDT = SA-DDP-PDT OF SAACNAMT - KBN-ADD-PRDT.
      
       4640-UPDATE-ACN-AMT-RTN-R.
      * UPDATE SAACNACN
           INITIALIZE PDBIMAIN.
           MOVE FT-RTRN-NOTFOUND       TO DBI-NORMAL-STATUS(1).
           MOVE 'BSAACND'              TO DBI-DB-NAME.
           MOVE FT-GHU                 TO DBI-FT-NAME.
           MOVE I-ACCT-NO OF PKSADEP0  TO DBI-KEY-VALUE1(1).
           CALL 'GDBIMAIN' USING PDBIMAIN
                                 SAACNACN
                                 AIF-AREA.
           PERFORM 9999-MESSAGE-HANDLE-RTN.
      
           MOVE SA-DET-ITEM-N  OF SAACNACN
                                       TO WK-DET-ITEM-N.
           COMPUTE SA-DET-ITEM-N OF SAACNACN = SA-DET-ITEM-N OF SAACNACN 
                                             + WK-DET-ITEM-B.
           COMPUTE SA-ACC-UNPRINT-NO  OF SAACNACN = 
                                       SA-ACC-UNPRINT-NO  OF SAACNACN
                                                 + WK-DET-ITEM-B.
      
           INITIALIZE PDBIMAIN.
           MOVE 'BSAACND'              TO DBI-DB-NAME.
           MOVE FT-REPL                TO DBI-FT-NAME.
           CALL 'GDBIMAIN' USING PDBIMAIN
                                 SAACNACN
                                 AIF-AREA.
           PERFORM 9999-MESSAGE-HANDLE-RTN.
      
      * UPDATE SAACNAMT
           INITIALIZE PDBIMAIN.
           MOVE FT-RTRN-NOTFOUND       TO DBI-NORMAL-STATUS(1).
           MOVE 'BSAACND'              TO DBI-DB-NAME.
           MOVE FT-GHU                 TO DBI-FT-NAME.
           MOVE I-ACCT-NO OF PKSADEP0  TO DBI-KEY-VALUE1(1).
           MOVE 'SAACNAMT'             TO DBI-SEGMENT-NAME(2).
           MOVE I-CURR-COD OF PKSADEP0 TO WK-CURR-COD.
           MOVE I-CURR-IDEN OF PKSADEP0
                                       TO WK-CURR-IDEN.
           MOVE WK-SAACNAMT-KEY        TO DBI-KEY-VALUE1(2).
           CALL 'GDBIMAIN' USING PDBIMAIN
                                 SAACNAMT
                                 AIF-AREA.
           PERFORM 9999-MESSAGE-HANDLE-RTN.
      
           MOVE WK-AVL-BAL             TO SA-AVL-BAL  OF SAACNAMT.
           MOVE WK-INTC-AMT            TO SA-INTC-AMT OF SAACNAMT.
           MOVE WK-DDP-PDT             TO SA-DDP-PDT OF SAACNAMT.
           COMPUTE SA-ACTU-PDT OF SAACNAMT = SA-DDP-PDT OF SAACNAMT 
                                           - SA-OD-PDT OF SAACNAMT.
           COMPUTE SA-OD-INT OF SAACNAMT = SA-OD-INT OF SAACNAMT
                                         + KBN-OD-INT.
           COMPUTE SA-OVDLN-OD-INT OF SAACNAMT =
                   SA-OVDLN-OD-INT OF SAACNAMT + KBN-INT.
           COMPUTE SA-OD-INT-AMT OF SAACNAMT =
                   SA-OD-INT-AMT OF SAACNAMT + KBN-OD-INT-AMT.
           IF WK-INTC-AMT > ZEROS AND SA-OD-INT-AMT OF SAACNAMT > ZEROS
             AND WK-INTC-AMT > SA-FRZ-AMT OF SAACNAMT
           THEN
             MOVE 'EN177'              TO AIF-MSG-CODE
      *      MOVE '计息，透支金额均大于零'
      *                                TO AIF-MSG-TEXT
             PERFORM 9999-MESSAGE-HANDLE-RTN
           END-IF.
           COMPUTE WK-ACCT-BAL = SA-INTC-AMT     OF SAACNAMT
                               - SA-OD-INT-AMT   OF SAACNAMT
                               - SA-OD-INT       OF SAACNAMT
                               - SA-OVDLN-OD-INT OF SAACNAMT.
           MOVE WK-ACCT-BAL            TO SA-ACCT-BAL OF SAACNAMT.
           COMPUTE SA-OD-AMT OF SAACNAMT =
                   SA-OD-AMT OF SAACNAMT + KBN-OD-INT-AMT.

           IF KBN-DET-ITEM-N = WK-DET-ITEM-N
           THEN
             MOVE KBN-LTM-TX-DT        TO SA-LTM-TX-DT OF SAACNAMT
           ELSE
             MOVE SYS-BUSN-DT          TO SA-LTM-TX-DT OF SAACNAMT
           END-IF.
      
           IF WK-24H-MODE = '2'
           THEN
             MOVE SYS-BUSN-DT          TO SA-LTM-TX-DT OF SAACNAMT
           END-IF
      
      *    IF (SA-ACCT-CHAR OF SAACNACN(4:1) = 'L' OR X'93')
           IF SA-FX-ACCT-CHAR OF SAACNACN = '2102'
             AND I-DSCRP-COD OF PKSADEP0 = C-MEM-SA-6002
           THEN
             COMPUTE SA-AS-EXC-AMT OF SAACNAMT = SA-AS-EXC-AMT OF
                     SAACNAMT - I-AMT OF PKSADEP0
           END-IF.
      
      *    IF (SA-ACCT-CHAR OF SAACNACN(4:1) = 'L' OR X'93')
           IF SA-FX-ACCT-CHAR OF SAACNACN = '2102'
             AND I-DSCRP-COD OF PKSADEP0 = C-MEM-SA-6005 
           THEN
             COMPUTE SA-AS-EXC-AMT OF SAACNAMT = SA-AS-EXC-AMT OF
                     SAACNAMT - I-AMT OF PKSADEP0
           END-IF.
           IF SA-AS-EXC-AMT OF SAACNAMT < ZEROS
           THEN
             MOVE 'EN176'              TO AIF-MSG-CODE
      *       MOVE '资本金账户，限额不足'
      *                                 TO AIF-MSG-TEXT
             PERFORM 9999-MESSAGE-HANDLE-RTN
           END-IF.
      
      *    IF (SA-ACCT-CHAR OF SAACNACN(4:1) = 'L' OR X'93') AND
           IF SA-FX-ACCT-CHAR OF SAACNACN = '2102' AND
             (I-DSCRP-COD OF PKSADEP0 = C-MEM-SA-6003 OR C-MEM-SA-6004)
           THEN
             COMPUTE SA-CLN-AMT OF SAACNAMT = SA-CLN-AMT OF
                     SAACNAMT + I-AMT OF PKSADEP0
           END-IF.
TEST***    IF SA-LTM-TX-DT OF SAACNAMT NOT = SYS-BUSN-DT
TEST***    THEN
TEST***      MOVE ZEROS TO SA-TODAY-OD-CSH-AMT OF SAACNAMT
TEST***    END-IF.
TEST***
TEST***    IF SA-INTC-AMT OF SAACNAMT < SA-LOWEST-BAL OF SAACNAMT
TEST***    THEN
TEST***      MOVE SA-INTC-AMT OF SAACNAMT
TEST***                                TO SA-LOWEST-BAL OF SAACNAMT
TEST***    END-IF.

TEST****    IF I-TX-TYP OF PKSADEP0 = C-CM-TXTYP-CASH
TEST****    THEN
TEST****      SUBTRACT I-AMT OF PKSADEP0
TEST****        FROM SA-CASH-DEP-DTOT OF SAACNAMT
TEST****      IF SA-CASH-DEP-DTOT OF SAACNAMT < 0
TEST****      THEN
TEST****        MOVE 0                  TO SA-CASH-DEP-DTOT OF SAACNAMT
TEST****      END-IF
TEST****    ELSE
TEST****      SUBTRACT I-AMT OF PKSADEP0 FROM SA-TR-DEP-DTOT OF SAACNAMT
TEST****      IF SA-TR-DEP-DTOT OF SAACNAMT < 0
TEST****      THEN
TEST****        MOVE 0 TO SA-TR-DEP-DTOT OF SAACNAMT
TEST****      END-IF
TEST****    END-IF.
      
      *    INITIALIZE PDBIMAIN.
           MOVE 'BSAACND'              TO DBI-DB-NAME.
           MOVE FT-REPL                TO DBI-FT-NAME.
           CALL 'GDBIMAIN' USING PDBIMAIN
                                 SAACNAMT
                                 AIF-AREA.
           PERFORM 9999-MESSAGE-HANDLE-RTN.
      
       4650-PROCESS-TXN-RTN-R.
      *    UPDATE ORIGINAL TXN
           INITIALIZE PDBIMAIN.
           MOVE FT-RTRN-NOTFOUND       TO DBI-NORMAL-STATUS(1).
           MOVE 'BSAACND'              TO DBI-DB-NAME.
           MOVE FT-GHU                 TO DBI-FT-NAME.
           MOVE I-ACCT-NO OF PKSADEP0  TO DBI-KEY-VALUE1(1).
           MOVE 'SAACNACN'             TO DBI-SEGMENT-NAME(1).
           MOVE 'SAACNTXN'             TO DBI-SEGMENT-NAME(2).
           ADD 1                       TO WK-DET-ITEM-A.
           MOVE WK-DET-ITEM-A          TO DBI-KEY-VALUE1(2).
           CALL 'GDBIMAIN' USING PDBIMAIN
                                 SAACNTXN
                                 AIF-AREA.
           PERFORM 9999-MESSAGE-HANDLE-RTN.
      
           MOVE C-CLG-TXT-REVERSED     TO SA-EC-FLG OF SAACNTXN.
           MOVE 1                      TO WK-DET-ITEM-B.
           COMPUTE SA-EC-DET-NO-N OF SAACNTXN =
                   SA-DET-ITEM-N OF SAACNACN + WK-DET-ITEM-B.
           MOVE SYS-TIMESTAMP          TO SA-DB-TIMESTAMP  OF SAACNTXN.
           INITIALIZE PDBIMAIN.
           MOVE 'BSAACND'              TO DBI-DB-NAME.
           MOVE FT-REPL                TO DBI-FT-NAME.
           MOVE 'SAACNACN'             TO DBI-SEGMENT-NAME(1).
           MOVE I-ACCT-NO OF PKSADEP0  TO DBI-KEY-VALUE1(1)
           MOVE 'SAACNTXN'             TO DBI-SEGMENT-NAME(2).
           CALL 'GDBIMAIN' USING PDBIMAIN
                                 SAACNTXN
                                 AIF-AREA.
           PERFORM 9999-MESSAGE-HANDLE-RTN.
      
      *    INSERT TXN FOR EC

           COMPUTE SA-DDP-ACCT-NO-DET-N OF SAACNTXN =
                   SA-DET-ITEM-N OF SAACNACN + WK-DET-ITEM-B.

           MOVE I-CURR-COD OF PKSADEP0 TO SA-CURR-COD OF SAACNTXN.

           MOVE WK-SYS-TELLER-ID       TO SA-OPR-NO OF SAACNTXN.
           MOVE I-CURR-IDEN OF PKSADEP0
                                       TO SA-CURR-IDEN OF SAACNTXN.
           MOVE C-CLG-TXT-EC           TO SA-EC-FLG OF SAACNTXN.

           MOVE WK-DET-ITEM-A          TO SA-EC-DET-NO-N.

           IF I-ACCT-NO-EC OF PKSADEP0  = C-CM-FLAG-YES
           THEN
             COMPUTE SA-DR-AMT OF SAACNTXN = 0 + I-AMT OF PKSADEP0
             MOVE SA-DR-AMT OF SAACNTXN TO SA-TX-AMT OF SAACNTXN
             MOVE 0                    TO SA-CR-AMT OF SAACNTXN
           ELSE
             COMPUTE SA-CR-AMT OF SAACNTXN = 0 - I-AMT OF PKSADEP0
             MOVE SA-CR-AMT OF SAACNTXN TO SA-TX-AMT OF SAACNTXN
             MOVE 0                    TO SA-DR-AMT OF SAACNTXN
           END-IF.

           COMPUTE SA-DDP-ACCT-BAL OF SAACNTXN
                                     = SA-ACCT-BAL OF SAACNAMT
                                     - I-AMT OF PKSADEP0.

           MOVE I-TX-TYP OF PKSADEP0   TO SA-TX-TYP.
           
           MOVE WK-SYS-TX-LOG-NO       TO SA-TX-LOG-NO.

           MOVE I-DOC-NO OF PKSADEP0   TO SA-DOC-NO.
           MOVE I-DOC-TYP OF PKSADEP0  TO SA-DOC-TYP OF SAACNTXN.
           MOVE I-VAL-DT OF PKSADEP0   TO SA-VAL-DT.

           MOVE WK-SYS-BRANCH-STD      TO SA-OPUN-COD OF SAACNTXN.
           MOVE C-REVERSE OF MEMCONST
                                       TO SA-DSCRP-COD OF SAACNTXN.
           IF KBN-OD-INT > ZEROS OR
              KBN-INT > ZEROS OR
              KBN-OD-INT-AMT > ZEROS 
           THEN
             MOVE 'OD'                 TO WK-RMRK(1:2)
             COMPUTE WK-OD-INT-SUB = 0 - KBN-OD-INT
             MOVE WK-OD-INT-SUB        TO ODODOD
             MOVE 'OVD'                TO WK-RMRK(11:3)
             COMPUTE WK-OVDLN-OD-INT-SUB = 0 - KBN-INT
             MOVE WK-OVDLN-OD-INT-SUB  TO OVDOVD
             MOVE 'AMT'                TO WK-RMRK(22:3)
             COMPUTE WK-OD-INT-AMT-SUB = 0 - KBN-OD-INT-AMT
             MOVE WK-OD-INT-AMT-SUB    TO AMTAMT
             MOVE SA-OD-LG-FLG OF SAACNAMT
                                       TO WK-RMRK(40:1)
             MOVE WK-RMRK              TO SA-RMRK OF SAACNTXN
           END-IF
           IF KBN-OD-INT = ZEROS AND KBN-INT = ZEROS
             AND KBN-OD-INT-AMT = ZEROS
           THEN
             MOVE SPACES               TO SA-RMRK OF SAACNTXN
             MOVE I-DSCRP-COD OF PKSADEP0
                                       TO SA-RMRK OF SAACNTXN(1:4)
           END-IF
           MOVE I-TX-DT OF PKSADEP0    TO SA-TX-DT OF SAACNTXN.
           MOVE SYS-BUSN-DT            TO SA-TX-DT OF SAACNTXN.
           MOVE SYS-CPU-TM2            TO SA-TX-TM.
TEST****   IF AIF-FL-MSG-KEY NOT = SPACES
TEST****     MOVE AIF-FL-MSG-KEY       TO SA-TX-CRD-NO OF SAACNTXN(1:9)
TEST****   ELSE
             IF AIF-OPPO-ACCOUNT-NO NOT = SPACES
             THEN
               IF AIF-OPPO-ACCOUNT-NO = I-ACCT-NO OF PKSADEP0
               THEN
                 MOVE AIF-ACCOUNT-NO(1:19)
                                       TO SA-TX-CRD-NO OF SAACNTXN
                 MOVE AIF-ACCOUNT-NO(20:13)
                                       TO SA-CUST-DOCAG-STNO
                                          OF SAACNTXN(1:13)
               ELSE
                 MOVE AIF-OPPO-ACCOUNT-NO(1:19)
                                       TO SA-TX-CRD-NO OF SAACNTXN
                 MOVE AIF-OPPO-ACCOUNT-NO(20:13)
                                       TO SA-CUST-DOCAG-STNO
                                          OF SAACNTXN(1:13)
               END-IF
             END-IF
             IF WK-BRH-FLG = C-CM-FLAG-YES
             THEN
               MOVE WK-I-ACCT-NO(1:19)
                                       TO SA-TX-CRD-NO OF SAACNTXN
               MOVE WK-I-ACCT-NO(20:13)
                                       TO SA-CUST-DOCAG-STNO
                                          OF SAACNTXN(1:13)
             END-IF.
TEST****   END-IF.
           MOVE SYS-CPU-DT             TO SA-SYS-DT.
           MOVE SA-DDP-PDT OF SAACNAMT TO SA-DDP-PDT OF SAACNTXN.
           MOVE SYS-APP-TX-CODE        TO SA-APP-TX-CODE OF SAACNTXN.
           MOVE WK-PRT-FLG             TO SA-PRT-FLG OF SAACNTXN.
           MOVE WK-LEGAL-PERSON-ID     TO SA-LEGAL-PERSON-ID OF 
                                                             SAACNTXN.
           MOVE WK-DB-PARTITION-ID     TO SA-DB-PARTITION-ID  OF
                                                             SAACNTXN.
           INITIALIZE PDBIMAIN.
           MOVE 'BSAACND'              TO DBI-DB-NAME.
           MOVE FT-ISRT                TO DBI-FT-NAME.
           MOVE I-ACCT-NO OF PKSADEP0  TO DBI-KEY-VALUE1(1).
           MOVE 'SAACNACN'             TO DBI-SEGMENT-NAME(1).
           MOVE 'SAACNTXN'             TO DBI-SEGMENT-NAME(2).
           CALL 'GDBIMAIN' USING PDBIMAIN
                                 SAACNTXN
                                 AIF-AREA.
           PERFORM 9999-MESSAGE-HANDLE-RTN.
      
           IF WK-PRT-FLG NOT = C-CM-FLAG-YES
           THEN
             PERFORM 9801-INSERT-PRT-RTN
           END-IF.
      
       4651-PROCESS-TXN-RTN-R.
           INITIALIZE PDBIMAIN.
           MOVE FT-RTRN-NOTFOUND       TO DBI-NORMAL-STATUS(1).
           MOVE 'BSAACND'              TO DBI-DB-NAME.
           MOVE FT-GHU                 TO DBI-FT-NAME.
           MOVE I-ACCT-NO OF PKSADEP0  TO DBI-KEY-VALUE1(1).
           MOVE 'SAACNACN'             TO DBI-SEGMENT-NAME(1).
           MOVE 'SAACNTXN'             TO DBI-SEGMENT-NAME(2).
           ADD 1                       TO WK-DET-ITEM-A.
           MOVE WK-DET-ITEM-A          TO DBI-KEY-VALUE1(2).
           CALL 'GDBIMAIN' USING PDBIMAIN
                                 SAACNTXN
                                 AIF-AREA.
           PERFORM 9999-MESSAGE-HANDLE-RTN.
      
           MOVE C-CLG-TXT-REVERSED     TO SA-EC-FLG OF SAACNTXN.
           ADD  1                      TO WK-DET-ITEM-B.
           COMPUTE SA-EC-DET-NO-N OF SAACNTXN =
                   SA-DET-ITEM-N OF SAACNACN + WK-DET-ITEM-B.

           INITIALIZE PDBIMAIN.
           MOVE 'BSAACND'              TO DBI-DB-NAME.
           MOVE FT-REPL                TO DBI-FT-NAME.
           MOVE 'SAACNACN'             TO DBI-SEGMENT-NAME(1).
           MOVE I-ACCT-NO OF PKSADEP0  TO DBI-KEY-VALUE1(1)
           MOVE 'SAACNTXN'             TO DBI-SEGMENT-NAME(2).
           CALL 'GDBIMAIN' USING PDBIMAIN
                                 SAACNTXN
                                 AIF-AREA.
           PERFORM 9999-MESSAGE-HANDLE-RTN.
      

           COMPUTE SA-DDP-ACCT-NO-DET-N OF SAACNTXN =
                   SA-DET-ITEM-N OF SAACNACN + WK-DET-ITEM-B.
           MOVE I-CURR-COD OF PKSADEP0 TO SA-CURR-COD OF SAACNTXN.
    
           MOVE WK-SYS-TELLER-ID       TO SA-OPR-NO OF SAACNTXN.
           MOVE I-CURR-IDEN OF PKSADEP0
                                       TO SA-CURR-IDEN OF SAACNTXN.
           MOVE C-CLG-TXT-EC           TO SA-EC-FLG OF SAACNTXN.
           MOVE WK-DET-ITEM-A          TO SA-EC-DET-NO-N.
           IF I-ACCT-NO-EC OF PKSADEP0  = C-CM-FLAG-YES
           THEN
             COMPUTE SA-CR-AMT OF SAACNTXN = 0 + KBN-OD-INT + KBN-INT
                                   + KBN-OD-INT-AMT
             MOVE SA-CR-AMT OF SAACNTXN TO SA-TX-AMT OF SAACNTXN
             MOVE 0                    TO SA-DR-AMT OF SAACNTXN
             COMPUTE SA-DDP-ACCT-BAL OF SAACNTXN
                                = SA-ACCT-BAL OF SAACNAMT
                                 + SA-CR-AMT OF SAACNTXN
           ELSE
             COMPUTE SA-DR-AMT OF SAACNTXN = 0 - KBN-OD-INT - KBN-INT
                                   - KBN-OD-INT-AMT
             MOVE SA-DR-AMT OF SAACNTXN  TO SA-TX-AMT OF SAACNTXN
             MOVE 0                    TO SA-CR-AMT OF SAACNTXN
             COMPUTE SA-DDP-ACCT-BAL OF SAACNTXN
                                = SA-ACCT-BAL OF SAACNAMT
                                - SA-DR-AMT OF SAACNTXN
           END-IF.
           MOVE I-TX-TYP OF PKSADEP0   TO SA-TX-TYP.
      
           MOVE WK-SYS-TX-LOG-NO       TO SA-TX-LOG-NO.
           MOVE I-DOC-NO OF PKSADEP0   TO SA-DOC-NO.
           MOVE I-DOC-TYP OF PKSADEP0  TO SA-DOC-TYP OF SAACNTXN.
           MOVE I-VAL-DT OF PKSADEP0   TO SA-VAL-DT.
      
           MOVE WK-SYS-BRANCH-STD      TO SA-OPUN-COD OF SAACNTXN.
           MOVE C-REVERSE OF MEMCONST
                                       TO SA-DSCRP-COD OF SAACNTXN.
           MOVE SPACES                 TO SA-RMRK OF SAACNTXN.
           MOVE I-DSCRP-COD OF PKSADEP0
                                       TO SA-RMRK OF SAACNTXN(1:4).
           MOVE I-TX-DT OF PKSADEP0    TO SA-TX-DT OF SAACNTXN.
           MOVE SYS-BUSN-DT            TO SA-TX-DT OF SAACNTXN.
           MOVE SYS-CPU-TM2            TO SA-TX-TM.
           MOVE SYS-CPU-DT             TO SA-SYS-DT.
           MOVE SA-DDP-PDT OF SAACNAMT TO SA-DDP-PDT OF SAACNTXN.
           MOVE SYS-APP-TX-CODE        TO SA-APP-TX-CODE OF SAACNTXN.
           MOVE WK-PRT-FLG             TO SA-PRT-FLG OF SAACNTXN.
           MOVE WK-LEGAL-PERSON-ID     TO SA-LEGAL-PERSON-ID OF 
                                                             SAACNTXN.
           MOVE WK-DB-PARTITION-ID     TO SA-DB-PARTITION-ID  OF
                                                             SAACNTXN.
           INITIALIZE PDBIMAIN.
           MOVE 'BSAACND'              TO DBI-DB-NAME.
           MOVE FT-ISRT                TO DBI-FT-NAME.
           MOVE 'SAACNACN'             TO DBI-SEGMENT-NAME(1).
           MOVE I-ACCT-NO OF PKSADEP0  TO DBI-KEY-VALUE1(1).
           MOVE 'SAACNTXN'             TO DBI-SEGMENT-NAME(2).
           CALL 'GDBIMAIN' USING PDBIMAIN
                                 SAACNTXN
                                 AIF-AREA.
           PERFORM 9999-MESSAGE-HANDLE-RTN.
      
           IF WK-PRT-FLG NOT = C-CM-FLAG-YES
           THEN
             PERFORM 9801-INSERT-PRT-RTN
           END-IF.
      
       4652-PROCESS-TXN-RTN-R.
           INITIALIZE PDBIMAIN.
           MOVE FT-RTRN-NOTFOUND       TO DBI-NORMAL-STATUS(1).
           MOVE 'BSAACND'              TO DBI-DB-NAME.
           MOVE FT-GHU                 TO DBI-FT-NAME.
           MOVE 'SAACNACN'             TO DBI-SEGMENT-NAME(1).
           MOVE I-ACCT-NO OF PKSADEP0  TO DBI-KEY-VALUE1(1).
           MOVE 'SAACNTXN'             TO DBI-SEGMENT-NAME(2).
           ADD 1                       TO WK-DET-ITEM-A.
           MOVE WK-DET-ITEM-A          TO DBI-KEY-VALUE1(2).
           CALL 'GDBIMAIN' USING PDBIMAIN
                                 SAACNTXN
                                 AIF-AREA.
           PERFORM 9999-MESSAGE-HANDLE-RTN.
      
           MOVE C-CLG-TXT-REVERSED     TO SA-EC-FLG OF SAACNTXN.
           ADD  1                      TO WK-DET-ITEM-B.
           COMPUTE SA-EC-DET-NO-N OF SAACNTXN =
                   SA-DET-ITEM-N OF SAACNACN + WK-DET-ITEM-B.
           MOVE SYS-TIMESTAMP          TO SA-DB-TIMESTAMP  OF SAACNTXN .
           INITIALIZE PDBIMAIN.
           MOVE 'BSAACND'              TO DBI-DB-NAME.
           MOVE FT-REPL                TO DBI-FT-NAME.
           MOVE 'SAACNACN'             TO DBI-SEGMENT-NAME(1).
           MOVE I-ACCT-NO OF PKSADEP0  TO DBI-KEY-VALUE1(1)
           MOVE 'SAACNTXN'             TO DBI-SEGMENT-NAME(2).
           CALL 'GDBIMAIN' USING PDBIMAIN
                                 SAACNTXN
                                 AIF-AREA.
           PERFORM 9999-MESSAGE-HANDLE-RTN.
      

           COMPUTE SA-DDP-ACCT-NO-DET-N OF SAACNTXN =
                   SA-DET-ITEM-N OF SAACNACN + WK-DET-ITEM-B.
           MOVE I-CURR-COD OF PKSADEP0 TO SA-CURR-COD OF SAACNTXN.

           MOVE WK-SYS-TELLER-ID       TO SA-OPR-NO OF SAACNTXN.
           MOVE I-CURR-IDEN OF PKSADEP0
                                       TO SA-CURR-IDEN OF SAACNTXN.
           MOVE C-CLG-TXT-EC           TO SA-EC-FLG OF SAACNTXN.
           MOVE WK-DET-ITEM-A          TO SA-EC-DET-NO-N.
           IF I-ACCT-NO-EC OF PKSADEP0  = C-CM-FLAG-YES
           THEN
             COMPUTE SA-DR-AMT OF SAACNTXN = 0 + KBN-INT + KBN-OD-INT
                                   + KBN-OD-INT-AMT
             MOVE SA-DR-AMT OF SAACNTXN TO SA-TX-AMT OF SAACNTXN
             MOVE 0                    TO SA-CR-AMT OF SAACNTXN
             COMPUTE SA-DDP-ACCT-BAL OF SAACNTXN =
                                       SA-ACCT-BAL OF SAACNAMT + 0
           ELSE
             COMPUTE SA-CR-AMT OF SAACNTXN = 0 - KBN-INT - KBN-OD-INT
                                   - KBN-OD-INT-AMT
             MOVE SA-CR-AMT OF SAACNTXN TO SA-TX-AMT OF SAACNTXN
             MOVE 0                    TO SA-DR-AMT OF SAACNTXN
             COMPUTE SA-DDP-ACCT-BAL OF SAACNTXN = 
                                       SA-ACCT-BAL OF SAACNAMT + 0
           END-IF.
           MOVE I-TX-TYP OF PKSADEP0   TO SA-TX-TYP.
 
           MOVE WK-SYS-TX-LOG-NO       TO SA-TX-LOG-NO.
           MOVE I-DOC-NO OF PKSADEP0   TO SA-DOC-NO.
           MOVE I-DOC-TYP OF PKSADEP0  TO SA-DOC-TYP OF SAACNTXN.
           MOVE I-VAL-DT OF PKSADEP0   TO SA-VAL-DT.
 
           MOVE WK-SYS-BRANCH-STD      TO SA-OPUN-COD OF SAACNTXN.
           MOVE C-REVERSE OF MEMCONST
                                       TO SA-DSCRP-COD OF SAACNTXN.
           IF KBN-OD-INT > ZEROS OR KBN-INT > ZEROS
             OR KBN-OD-INT-AMT > ZEROS
             MOVE 'OD'                 TO WK-RMRK(1:2)
             COMPUTE WK-OD-INT-SUB = 0 - KBN-OD-INT
             MOVE WK-OD-INT-SUB        TO ODODOD
             MOVE 'OVD'                TO WK-RMRK(11:3)
             COMPUTE WK-OVDLN-OD-INT-SUB = 0 - KBN-INT
             MOVE WK-OVDLN-OD-INT-SUB  TO OVDOVD
             MOVE 'AMT'                TO WK-RMRK(22:3)
             COMPUTE WK-OD-INT-AMT-SUB = 0 - KBN-OD-INT-AMT
             MOVE WK-OD-INT-AMT-SUB    TO AMTAMT
             MOVE SA-OD-LG-FLG OF SAACNAMT
                                       TO WK-RMRK(40:1)
             MOVE WK-RMRK              TO SA-RMRK OF SAACNTXN
           END-IF
           IF KBN-OD-INT = ZEROS AND KBN-INT = ZEROS
             AND KBN-OD-INT-AMT = ZEROS
           THEN
             MOVE SPACES               TO SA-RMRK OF SAACNTXN
             MOVE I-DSCRP-COD OF PKSADEP0
                                       TO SA-RMRK OF SAACNTXN(1:4)
           END-IF
           MOVE I-TX-DT OF PKSADEP0    TO SA-TX-DT OF SAACNTXN.
           MOVE SYS-BUSN-DT            TO SA-TX-DT OF SAACNTXN.
           MOVE SYS-CPU-TM2            TO SA-TX-TM.
           MOVE SYS-CPU-DT             TO SA-SYS-DT.
           MOVE SA-DDP-PDT OF SAACNAMT TO SA-DDP-PDT OF SAACNTXN.
           MOVE SYS-APP-TX-CODE        TO SA-APP-TX-CODE OF SAACNTXN.
           MOVE WK-PRT-FLG             TO SA-PRT-FLG OF SAACNTXN.
           MOVE WK-LEGAL-PERSON-ID     TO SA-LEGAL-PERSON-ID OF 
                                                             SAACNTXN.
           MOVE WK-DB-PARTITION-ID     TO SA-DB-PARTITION-ID  OF
                                                             SAACNTXN.
           INITIALIZE PDBIMAIN.
           MOVE 'BSAACND'              TO DBI-DB-NAME.
           MOVE FT-ISRT                TO DBI-FT-NAME.
           MOVE I-ACCT-NO OF PKSADEP0  TO DBI-KEY-VALUE1(1).
           MOVE 'SAACNACN'             TO DBI-SEGMENT-NAME(1).
           MOVE 'SAACNTXN'             TO DBI-SEGMENT-NAME(2).
           CALL 'GDBIMAIN' USING PDBIMAIN
                                 SAACNTXN
                                 AIF-AREA.
           PERFORM 9999-MESSAGE-HANDLE-RTN.
      
           IF WK-PRT-FLG NOT = C-CM-FLAG-YES
           THEN
             PERFORM 9801-INSERT-PRT-RTN
           END-IF.
      
      *4688-CCICRF8-RTN.
      *
      *    INITIALIZE PCCICRF8.
      **   额度冲回
      *    MOVE C-CI-CRLMT-RL          TO I-CRLMT-FUNC OF PCCICRF8.
      *    MOVE SA-CUST-NO OF SAACNACN TO I-CUST-NO OF PCCICRF8.
      *    MOVE I-ACCT-NO OF PKSADEP0
      *                                TO I-ACCT-NO OF PCCICRF8.
      *    MOVE C-CI-SA-CRLMT-NO       TO I-CRLMT-NO OF PCCICRF8.
      *    MOVE C-CM-FLAG-YES
      *                                TO I-EC-FLG OF PCCICRF8.
      *    COMPUTE IO-AMT OF PCCICRF8 = 0 + KBN-OD-INT-AMT.
      *    CALL 'CCICRF8' USING  AIF-AREA
      *                          PCCICRF8
      *                          SYS-AREA.
      *    PERFORM 9999-MESSAGE-HANDLE-RTN.
      *    IF O-MSG-TYPE  OF PCCICRF8 = SPACES
      *      CONTINUE
      *    ELSE
      *      MOVE O-RTRN-CODE OF PCCICRF8
      *                                TO AIF-MSG-CODE
      *      PERFORM 9999-MESSAGE-HANDLE-RTN
      *    END-IF.
      **********************************
      * 读取协议金额 协议积数
      ********************************
       5610-GET-AGR-RTN.
      
           INITIALIZE                  PDBIMAIN.
           INITIALIZE                  SAACNAGR.
           MOVE FT-RTRN-NOTFOUND       TO DBI-NORMAL-STATUS(1).
           MOVE 'BSAACND'              TO DBI-DB-NAME.
           MOVE FT-GNP                 TO DBI-FT-NAME.
           MOVE 'SAACNACN'             TO DBI-SEGMENT-NAME(1).
           MOVE 'SAACNAMT'             TO DBI-SEGMENT-NAME(2).
           MOVE 'SAACNAGR'             TO DBI-SEGMENT-NAME(3).
           MOVE I-CURR-COD OF PKSADEP0 TO WK-CURR-COD
                                          OF WK-SAACNAMT-KEY.
           MOVE I-CURR-IDEN OF PKSADEP0
                                       TO WK-CURR-IDEN
                                          OF WK-SAACNAMT-KEY.
           MOVE I-ACCT-NO OF PKSADEP0  TO DBI-KEY-VALUE1(1).
           MOVE WK-SAACNAMT-KEY        TO DBI-KEY-VALUE1(2).
           MOVE '> '                   TO DBI-OP1(3).
           MOVE '&'                    TO DBI-BOOLEAN(3).
           MOVE '<='                   TO DBI-OP2(3).
           MOVE LOW-VALUE              TO DBI-KEY-VALUE1(3).
           MOVE HIGH-VALUE             TO DBI-KEY-VALUE2(3).
           IF WK-NUM = 1
           THEN
             MOVE '1'                  TO DBI-CMD-CODE(3 , 1)
           ELSE
             MOVE '0'                  TO DBI-CMD-CODE(3 , 1)
           END-IF.
           MOVE 99                     TO DBI-DB-SEQUENCE.
           CALL 'GDBIMAIN'       USING PDBIMAIN
                                       SAACNAGR
                                       AIF-AREA.
           PERFORM 9999-MESSAGE-HANDLE-RTN.
           IF DBI-DB-STATUS = FT-RTRN-NOTFOUND
           THEN
             SET WK-FILE-EOF           TO TRUE
           ELSE
             MOVE SA-NGO-AMT OF SAACNAGR
                                       TO WK-SA-NGO-AMT( WK-NUM )
             MOVE SA-NGO-PRDT OF SAACNAGR
                                       TO WK-SA-NGO-PRDT( WK-NUM )
                                       
             ADD 1                     TO WK-NUM
           END-IF.
           
       5611-CLOSE-SAACNAGR-RTN.
           MOVE FT-CLOSE-C             TO DBI-FT-NAME.
           CALL 'GDBIMAIN' USING       PDBIMAIN
                                       SAACNAGR
                                       AIF-AREA.
           PERFORM 9999-MESSAGE-HANDLE-RTN.           
      
       4700-KBR-LOG-RTN-R.
           CONTINUE.
      
       4800-ARL-LOG-RTN-R.
           CONTINUE.
      
       4900-24-HR-AFT-RTN.
            CONTINUE.
      
       5341-CALL-CCIATN1-RTN.
           INITIALIZE PCCIATN1.
           MOVE I-ACCT-NO OF PKSADEP0  TO I-ACCT-NO OF PCCIATN1.
           MOVE '1'                    TO I-TX-TYPE OF PCCIATN1.
           MOVE C-CM-FLAG-NO           TO I-EC-FLAG OF PCCIATN1.
           MOVE I-AMT OF PKSADEP0      TO I-TX-AMT OF PCCIATN1.
           MOVE SA-AVL-BAL OF SAACNAMT TO I-AVL-AMT OF PCCIATN1.
           MOVE I-DSCRP-COD OF PKSADEP0
                                       TO I-DSCRP-COD OF PCCIATN1.
           MOVE I-RMRK OF PKSADEP0     TO I-RMRK OF PCCIATN1.
           MOVE SA-OD-INT-AMT OF SAACNAMT
                                       TO I-OD-AMT OF PCCIATN1.
           IF AIF-OPPO-ACCOUNT-NO NOT = SPACES
           THEN
             IF AIF-OPPO-ACCOUNT-NO = I-ACCT-NO OF PKSADEP0
             THEN
               MOVE AIF-ACCOUNT-NO   TO I-OPP-ACCT-NO OF PCCIATN1
             ELSE
               MOVE AIF-OPPO-ACCOUNT-NO
                                     TO I-OPP-ACCT-NO OF PCCIATN1
             END-IF
           END-IF
           IF CWK-OPPO-CUST-NAME NOT = SPACES AND LOW-VALUES
              AND CWK-DR-CR-FLAG = C-ACA-CR-COD-RMRK
           THEN
             MOVE CWK-OPPO-CUST-NAME   TO I-OPP-ACCT-NAME OF PCCIATN1
           END-IF.
           MOVE I-CURR-COD OF PKSADEP0 TO I-CURR-COD OF PCCIATN1
           MOVE I-CURR-IDEN OF PKSADEP0 TO I-CURR-IDEN OF PCCIATN1
           MOVE WK-SYS-TX-LOG-NO       TO I-TX-LOG-NO OF PCCIATN1
           MOVE SA-CARD-NO OF SAACNACN TO I-CRD-NO OF PCCIATN1
      
           PERFORM 9707-CALL-CCIATN1-RTN.
      
      
       5342-CALL-CCIATN1-RTN.
           INITIALIZE PCCIATN1.
           MOVE I-ACCT-NO OF PKSADEP0  TO I-ACCT-NO OF PCCIATN1.
           MOVE '1'                    TO I-TX-TYPE OF PCCIATN1.
           MOVE C-CM-FLAG-YES          TO I-EC-FLAG OF PCCIATN1.
           MOVE I-AMT OF PKSADEP0      TO I-TX-AMT OF PCCIATN1.
           MOVE SA-AVL-BAL OF SAACNAMT TO I-AVL-AMT OF PCCIATN1.
           MOVE C-REVERSE OF MEMCONST  TO I-DSCRP-COD OF PCCIATN1.
           MOVE SPACES                 TO I-RMRK OF PCCIATN1.
           MOVE SA-OD-INT-AMT OF SAACNAMT
                                       TO I-OD-AMT OF PCCIATN1.
           IF AIF-OPPO-ACCOUNT-NO NOT = SPACES
           THEN
             IF AIF-OPPO-ACCOUNT-NO = I-ACCT-NO OF PKSADEP0
             THEN
               MOVE AIF-ACCOUNT-NO     TO I-OPP-ACCT-NO OF PCCIATN1
             ELSE
               MOVE AIF-OPPO-ACCOUNT-NO
                                     TO I-OPP-ACCT-NO OF PCCIATN1
             END-IF
           END-IF
           IF CWK-OPPO-CUST-NAME NOT = SPACES AND LOW-VALUES
              AND CWK-DR-CR-FLAG = C-ACA-CR-COD-RMRK
           THEN
             MOVE CWK-OPPO-CUST-NAME   TO I-OPP-ACCT-NAME OF PCCIATN1
           END-IF.
           MOVE I-CURR-COD OF PKSADEP0 TO I-CURR-COD OF PCCIATN1
           MOVE I-CURR-IDEN OF PKSADEP0 TO I-CURR-IDEN OF PCCIATN1
           MOVE WK-SYS-TX-LOG-NO       TO I-TX-LOG-NO OF PCCIATN1
           MOVE SA-CARD-NO OF SAACNACN TO I-CRD-NO OF PCCIATN1
      
           PERFORM 9707-CALL-CCIATN1-RTN.
           
      
       5343-CALL-CCIATN3-RTN.
           INITIALIZE                  PCCIATN3.
           MOVE I-ACCT-NO OF PKSADEP0  TO I-ACCT-NO         OF PCCIATN3.
           MOVE '1'                    TO I-TX-TYPE         OF PCCIATN3.
           MOVE I-TX-TYP OF PKSADEP0   TO I-TX-COD          OF PCCIATN3.
           MOVE C-EC-ACCT-NO-I         TO I-TRF-TYP         OF PCCIATN3.
           MOVE C-CM-FLAG-NO           TO I-EC-FLAG         OF PCCIATN3.
           MOVE I-AMT OF PKSADEP0      TO I-TX-AMT          OF PCCIATN3.
           MOVE SA-AVL-BAL OF SAACNAMT TO I-AVL-AMT         OF PCCIATN3.
           MOVE I-DSCRP-COD OF PKSADEP0
                                       TO I-DSCRP-COD       OF PCCIATN3.
           MOVE I-RMRK OF PKSADEP0     TO I-RMRK            OF PCCIATN3.
           MOVE SA-OD-INT-AMT OF SAACNAMT
                                       TO I-OD-AMT          OF PCCIATN3.
           IF AIF-OPPO-ACCOUNT-NO NOT = SPACES
             IF AIF-OPPO-ACCOUNT-NO = I-ACCT-NO OF PKSADEP0
               MOVE AIF-ACCOUNT-NO     TO I-OPP-ACCT-NO     OF PCCIATN3
             ELSE
               MOVE AIF-OPPO-ACCOUNT-NO
                                       TO I-OPP-ACCT-NO     OF PCCIATN3
             END-IF
           END-IF
           IF CWK-OPPO-CUST-NAME NOT = SPACES AND LOW-VALUES
              AND CWK-DR-CR-FLAG = C-ACA-CR-COD-RMRK
           THEN
             MOVE CWK-OPPO-CUST-NAME   TO I-OPP-ACCT-NAME   OF PCCIATN3
           END-IF.
           MOVE I-CURR-COD OF PKSADEP0 TO I-CURR-COD        OF PCCIATN3
           MOVE I-CURR-IDEN OF PKSADEP0 
                                       TO I-CURR-IDEN       OF PCCIATN3
           MOVE WK-SYS-TX-LOG-NO       TO I-TX-LOG-NO       OF PCCIATN3
           MOVE I-DOC-TYP OF PKSADEP0  TO I-DOC-TYP         OF PCCIATN3.
           MOVE I-DOC-NO OF PKSADEP0   TO I-DOC-NO          OF PCCIATN3. 
      
           PERFORM 9706-CALL-CCIATN3-RTN.
      
      
       5344-CALL-CCIATN3-RTN.
           INITIALIZE PCCIATN1.
           MOVE I-ACCT-NO OF PKSADEP0  TO I-ACCT-NO         OF PCCIATN3.
           MOVE '1'                    TO I-TX-TYPE         OF PCCIATN3.
           MOVE I-TX-TYP OF PKSADEP0   TO I-TX-COD          OF PCCIATN3.
           MOVE C-EC-ACCT-NO-I         TO I-TRF-TYP         OF PCCIATN3.
           MOVE C-CM-FLAG-YES          TO I-EC-FLAG         OF PCCIATN3.
           MOVE I-AMT OF PKSADEP0      TO I-TX-AMT          OF PCCIATN3.
           MOVE SA-AVL-BAL OF SAACNAMT TO I-AVL-AMT         OF PCCIATN3.
           MOVE C-REVERSE OF MEMCONST  TO I-DSCRP-COD       OF PCCIATN3.
           MOVE SPACES                 TO I-RMRK            OF PCCIATN3.
           MOVE SA-OD-INT-AMT OF SAACNAMT
                                       TO I-OD-AMT          OF PCCIATN3.
           IF AIF-OPPO-ACCOUNT-NO NOT = SPACES
             IF AIF-OPPO-ACCOUNT-NO = I-ACCT-NO OF PKSADEP0
               MOVE AIF-ACCOUNT-NO     TO I-OPP-ACCT-NO     OF PCCIATN3
             ELSE
               MOVE AIF-OPPO-ACCOUNT-NO
                                       TO I-OPP-ACCT-NO     OF PCCIATN3
             END-IF
           END-IF
           IF CWK-OPPO-CUST-NAME NOT = SPACES AND LOW-VALUES
              AND CWK-DR-CR-FLAG = C-ACA-CR-COD-RMRK
           THEN
             MOVE CWK-OPPO-CUST-NAME   TO I-OPP-ACCT-NAME   OF PCCIATN3
           END-IF.
           MOVE I-CURR-COD OF PKSADEP0 TO I-CURR-COD        OF PCCIATN3
           MOVE I-CURR-IDEN OF PKSADEP0 
                                       TO I-CURR-IDEN       OF PCCIATN3
           MOVE WK-SYS-TX-LOG-NO       TO I-TX-LOG-NO       OF PCCIATN3
           MOVE I-DOC-TYP OF PKSADEP0  TO I-DOC-TYP         OF PCCIATN3
           MOVE I-DOC-NO OF PKSADEP0   TO I-DOC-NO          OF PCCIATN3.
      
           PERFORM 9706-CALL-CCIATN3-RTN.
      
       5000-PREPARE-OUTPUT-RTN.
           IF WK-BRH-FLG = C-CM-FLAG-YES
           THEN
             MOVE SPACES               TO O-RTRN-CODE       OF PKSADEP0
             MOVE 0                    TO O-INTC-AMT        OF PKSADEP0
             MOVE 0                    TO O-AVL-BAL         OF PKSADEP0
             MOVE 0                    TO O-ACCT-BAL        OF PKSADEP0
             MOVE O-ACCT-NAME OF PKSATXN1 
                                       TO O-ACCT-NAME       OF PKSADEP0
             MOVE O-RCV-PAY-RANG OF PKSATXN1
                                       TO O-RCV-PAY-RANG    OF PKSADEP0
             MOVE O-ACCT-CHAR OF PKSATXN1 TO O-ACCT-CHAR    OF PKSADEP0
           ELSE
             MOVE SPACES               TO O-RTRN-CODE      OF PKSADEP0
             MOVE SA-INTC-AMT OF SAACNAMT
                                       TO O-INTC-AMT       OF PKSADEP0
             MOVE SA-AVL-BAL OF SAACNAMT
                                       TO O-AVL-BAL        OF PKSADEP0
             MOVE SA-ACCT-NAME  OF SAACNACN
                                       TO O-ACCT-NAME      OF PKSADEP0
             MOVE SA-RCV-PAY-RANG  OF SAACNAMT
                                       TO O-RCV-PAY-RANG   OF PKSADEP0
             IF SA-ACCT-CHAR OF SAACNACN NOT = SPACE
             THEN
               MOVE SA-ACCT-CHAR OF SAACNACN
                                       TO O-ACCT-CHAR      OF PKSADEP0
             ELSE
               MOVE SA-FX-ACCT-CHAR OF SAACNACN
                                       TO O-ACCT-CHAR      OF PKSADEP0
             END-IF
             MOVE SA-ACCT-BAL  OF SAACNAMT
                                        TO O-ACCT-BAL       OF PKSADEP0
           END-IF.
      
           MOVE LENGTH OF O-PZZZ1130   TO PZZZ1130-LL.
           MOVE 'PZZZ1130'             TO PZZZ1130-FORMID.
           MOVE WK-SYS-TELLER-ID       TO FM-OPR-NO       OF O-PZZZ1130.
           IF WK-BRH-FLG = C-CM-FLAG-YES
           THEN
             MOVE WK-I-ACCT-NO         TO FM-ACCT-NO      OF O-PZZZ1130
             MOVE O-ACCT-NAME OF PKSATXN1 TO FM-CUST-NAME OF O-PZZZ1130
             MOVE O-ACCT-CHAR OF PKSATXN1 TO FM-ACCT-CHAR OF O-PZZZ1130
             MOVE O-DRW-TYP OF PKSATXN1 TO FM-DRW-TYP     OF O-PZZZ1130
             MOVE O-PSBK-NO OF PKSATXN1 TO FM-VOL-NO-N    OF O-PZZZ1130
           ELSE
             MOVE I-ACCT-NO OF PKSADEP0 TO FM-ACCT-NO     OF O-PZZZ1130
      *      MOVE SA-CUST-NAME OF SAACNACN
             MOVE SA-ACCT-NAME OF SAACNACN 
                                       TO FM-CUST-NAME    OF O-PZZZ1130
             IF SA-ACCT-CHAR OF SAACNACN NOT = SPACE
             THEN
               MOVE SA-ACCT-CHAR OF SAACNACN
                                       TO FM-ACCT-CHAR    OF O-PZZZ1130
             ELSE
               MOVE SA-FX-ACCT-CHAR OF SAACNACN
                                       TO FM-ACCT-CHAR    OF O-PZZZ1130
             END-IF
             MOVE SA-DRW-TYP OF SAACNACN
                                       TO FM-DRW-TYP      OF O-PZZZ1130
             IF SA-PSBK-NO OF SAACNACN NOT = SPACES
             THEN                          
               MOVE SA-PSBK-NO OF SAACNACN 
                                       TO FM-VOL-NO       OF O-PZZZ1130
             END-IF                       
             MOVE SA-ACCT-BAL OF SAACNAMT
                                       TO FM-BAL OF O-PZZZ1130

           END-IF
           MOVE I-TX-DT OF PKSADEP0    TO FM-DEP-DT      OF O-PZZZ1130.
           MOVE I-VAL-DT OF PKSADEP0   TO FM-VAL-DT      OF O-PZZZ1130.
           MOVE WK-SYS-BRANCH-STD      TO FM-DEP-INSTN   OF O-PZZZ1130.
           MOVE WK-SYS-TX-LOG-NO       TO FM-TX-LOG-NO   OF O-PZZZ1130.
           MOVE I-AMT OF PKSADEP0      TO FM-DEP-AMT     OF O-PZZZ1130.
           MOVE I-CURR-COD OF PKSADEP0 TO FM-CURR-COD    OF O-PZZZ1130.
           MOVE SYS-CPU-TM1            TO FM-DEP-TM      OF O-PZZZ1130.
           MOVE I-CURR-IDEN OF PKSADEP0
                                       TO FM-CURR-IDEN   OF O-PZZZ1130
           IF SYS-SPV-B NOT = SPACES
           THEN
             MOVE SYS-SPV-B            TO FM-SPV-TLR     OF O-PZZZ1130
           END-IF
           IF SYS-SPV-A NOT = SPACES
           THEN
             MOVE SYS-SPV-A            TO FM-SPV-TLR     OF O-PZZZ1130
            END-IF
      
           IF SYS-TX-TYP = C-CLG-TXT-EC
           THEN
             STRING C-CM-EC-CHAR, SYS-EC-LOG-NO
                         DELIMITED BY SIZE INTO FM-RMRK OF O-PZZZ1130
           ELSE
             MOVE I-RMRK OF PKSADEP0   TO FM-RMRK       OF O-PZZZ1130
           END-IF.
      
      *    PREPARE PZZZ1181  法透需求未做，该凭证实际不输出
           MOVE LENGTH OF O-PZZZ1181   TO PZZZ1181-LL.
           IF KBN-OD-INT-AMT > ZEROS OR 
              KBN-OD-INT > ZEROS OR
              KBN-INT > ZEROS
           THEN
             MOVE 'PZZZ1181'           TO PZZZ1181-FORMID
             MOVE SYS-BUSN-DT          TO FM-DATE         OF O-PZZZ1181
             MOVE I-ACCT-NO OF PKSADEP0
                                       TO FM-ACCT-NO      OF O-PZZZ1181
             MOVE SA-CUST-NAME OF SAACNACN
                                       TO FM-CUST-NAM     OF O-PZZZ1181
             MOVE SA-CUST-NO OF SAACNACN
                                       TO FM-CUST-NO      OF O-PZZZ1181
             MOVE KBN-OD-INT-AMT       TO FM-OD-AMT       OF O-PZZZ1181
             MOVE KBN-OD-INT           TO FM-OD-INT-15    OF O-PZZZ1181
             MOVE KBN-INT              TO FM-OVDLN-OD-INT-15 
                                                          OF O-PZZZ1181
             IF KBN-INTC-AMT > ZEROS
             THEN
               MOVE KBN-INTC-AMT       TO  FM-DEP-AMT     OF O-PZZZ1181
             END-IF
             MOVE WK-SYS-TX-LOG-NO     TO FM-TX-LOG-NO    OF O-PZZZ1181
             MOVE I-CURR-COD OF PKSADEP0 TO FM-CURR-COD   OF O-PZZZ1181
             MOVE I-CURR-IDEN OF PKSADEP0
                                         TO FM-CURR-IDEN  OF O-PZZZ1181
             MOVE WK-SYS-TELLER-ID       TO FM-OPR-NO     OF O-PZZZ1181
             IF SYS-TX-TYP = C-CLG-TXT-EC
             THEN
               STRING C-CM-EC-CHAR, SYS-EC-LOG-NO
                 DELIMITED BY SIZE INTO FM-RMRK           OF O-PZZZ1181
             ELSE
               MOVE I-RMRK OF PKSADEP0   TO FM-RMRK       OF O-PZZZ1181
             END-IF
           END-IF.
           IF  SYS-TX-TYP NOT = C-CLG-TXT-EC AND
               PD-FDEP-AMT OF O-PDPRTSAA OF PCPDPSA0 >0
           THEN
             IF SA-ACCT-BAL OF SAACNAMT <  PD-FDEP-AMT OF
                                       O-PDPRTSAA OF PCPDPSA0
             THEN
               MOVE 'EN173'            TO AIF-MSG-CODE
      *        MOVE '起存金额不足'     TO AIF-MSG-TEXT
               PERFORM 9999-MESSAGE-HANDLE-RTN
             END-IF
           END-IF.
           IF I-BKH-NO OF PKSADEP0 NOT = SPACES
           THEN
             INITIALIZE                PCSABKH4
             MOVE 'C'                  TO I-FUN-CODE        OF PCSABKH4
             MOVE I-ACCT-NO OF PKSADEP0
                                       TO I-ACCT-NO         OF PCSABKH4
             MOVE I-BKH-NO OF PKSADEP0 TO I-BKH-NO          OF PCSABKH4
             MOVE I-AMT OF PKSADEP0    TO I-AMT             OF PCSABKH4
             MOVE I-DSCRP-COD OF PKSADEP0
                                       TO I-DSCRP-COD       OF PCSABKH4
             MOVE I-RMRK OF PKSADEP0   TO I-RMRK            OF PCSABKH4
             MOVE SA-DDP-ACCT-NO-DET-N OF SAACNTXN
                                       TO I-TX-DET-NO       OF PCSABKH4
             MOVE I-TX-TYP OF PKSADEP0 TO I-TX-TYP          OF PCSABKH4
             CALL 'CSABKH4'      USING AIF-AREA
                                       PCSABKH4
                                       SYS-AREA
             PERFORM 9999-MESSAGE-HANDLE-RTN
             IF O-RTRN-CODE OF PCSABKH4 NOT = SPACE
             THEN
               MOVE O-RTRN-CODE OF PCSABKH4
                                       TO AIF-MSG-CODE
               PERFORM 9999-MESSAGE-HANDLE-RTN
             END-IF
             PERFORM 9728-CALL-CCIATN4-RTN
           ELSE
             INITIALIZE                PCCMACC6
             MOVE I-ACCT-NO OF PKSADEP0
                                       TO I-ACCT-NO OF PCCMACC6
             PERFORM 9703-CALL-CCMACC6-RTN
             IF O-RTRN-CODE OF PCCMACC6 NOT = SPACE
             THEN
               MOVE O-RTRN-CODE OF PCCMACC6
                                       TO AIF-MSG-CODE
               PERFORM 9999-MESSAGE-HANDLE-RTN
             END-IF
      *      已开通多级账簿但未输入多级账簿编号，默认99999
             IF O-ACCT-NO-TYPE-1 OF PCCMACC6 = C-CM-RMB-ENT-SA-ACNT AND
                WK-BKH-FLG =  C-SA-BKH-SIGN-YES
             THEN
               INITIALIZE PCSABKH4
               MOVE 'C'                TO I-FUN-CODE        OF PCSABKH4
               MOVE I-ACCT-NO OF PKSADEP0
                                       TO I-ACCT-NO         OF PCSABKH4
               MOVE C-SA-BKH-WAT       TO I-BKH-NO          OF PCSABKH4
               MOVE I-AMT OF PKSADEP0  TO I-AMT             OF PCSABKH4
               MOVE I-DSCRP-COD OF PKSADEP0
                                       TO I-DSCRP-COD       OF PCSABKH4
               MOVE I-RMRK OF PKSADEP0 TO I-RMRK            OF PCSABKH4
               MOVE SA-DDP-ACCT-NO-DET-N OF SAACNTXN
                                       TO I-TX-DET-NO       OF PCSABKH4
               MOVE I-TX-TYP OF PKSADEP0
                                       TO I-TX-TYP          OF PCSABKH4
               CALL 'CSABKH4'    USING AIF-AREA
                                       PCSABKH4
                                       SYS-AREA
               PERFORM 9999-MESSAGE-HANDLE-RTN
      
               IF O-RTRN-CODE OF PCSABKH4 NOT = SPACE
               THEN
                 MOVE O-RTRN-CODE OF PCSABKH4
                                       TO AIF-MSG-CODE
                 PERFORM 9999-MESSAGE-HANDLE-RTN
               END-IF
               PERFORM 9728-CALL-CCIATN4-RTN
             END-IF
           END-IF.
       5341-CALL-CCIATN1-SUB-RTN.
           INITIALIZE                  PCCIATN1.
           MOVE I-ACCT-NO OF PKSADEP0  TO I-ACCT-NO         OF PCCIATN1.
           MOVE '1'                    TO I-TX-TYPE         OF PCCIATN1.
           MOVE C-CM-FLAG-NO           TO I-EC-FLAG         OF PCCIATN1.
           MOVE I-AMT OF PKSADEP0      TO I-TX-AMT          OF PCCIATN1.

           MOVE CI-DEPO-LMT OF O-CICOADTL TO I-AVL-AMT      OF PCCIATN1.
           MOVE I-DSCRP-COD OF PKSADEP0
                                       TO I-DSCRP-COD       OF PCCIATN1.
           MOVE I-RMRK OF PKSADEP0     TO I-RMRK            OF PCCIATN1.
 
           MOVE CI-OVER-DRAW-AMT OF O-CICOADTL
                                       TO I-OD-AMT          OF PCCIATN1.
           IF AIF-OPPO-ACCOUNT-NO NOT = SPACES
           THEN
             IF AIF-OPPO-ACCOUNT-NO = I-ACCT-NO OF PKSADEP0
             THEN
               MOVE AIF-ACCOUNT-NO     TO I-OPP-ACCT-NO     OF PCCIATN1
             ELSE
               MOVE AIF-OPPO-ACCOUNT-NO
                                       TO I-OPP-ACCT-NO     OF PCCIATN1
             END-IF
           END-IF
           IF CWK-OPPO-CUST-NAME NOT = SPACES AND LOW-VALUES AND
              CWK-DR-CR-FLAG = C-ACA-CR-COD-RMRK
           THEN
             MOVE CWK-OPPO-CUST-NAME   TO I-OPP-ACCT-NAME   OF PCCIATN1
           END-IF.
           MOVE I-CURR-COD OF PKSADEP0 TO I-CURR-COD        OF PCCIATN1
           MOVE I-CURR-IDEN OF PKSADEP0 TO I-CURR-IDEN      OF PCCIATN1
           MOVE WK-SYS-TX-LOG-NO       TO I-TX-LOG-NO       OF PCCIATN1
           MOVE SA-PDP-CODE OF OSAACNACN
                                       TO I-PRD-COD         OF PCCIATN1
           PERFORM 9707-CALL-CCIATN1-RTN.
      
      
     
       5342-CALL-CCIATN1-SUB-RTN.
           INITIALIZE                  PCCIATN1.
           MOVE I-ACCT-NO OF PKSADEP0  TO I-ACCT-NO         OF PCCIATN1.
           MOVE '1'                    TO I-TX-TYPE         OF PCCIATN1.
           MOVE C-CM-FLAG-YES          TO I-EC-FLAG         OF PCCIATN1.
           MOVE I-AMT OF PKSADEP0      TO I-TX-AMT          OF PCCIATN1.
     
           MOVE CI-DEPO-LMT OF O-CICOADTL 
                                       TO I-AVL-AMT         OF PCCIATN1.
           MOVE C-REVERSE OF MEMCONST  TO I-DSCRP-COD       OF PCCIATN1.
           MOVE SPACES                 TO I-RMRK            OF PCCIATN1.
           MOVE CI-OVER-DRAW-AMT OF O-CICOADTL
                                       TO I-OD-AMT          OF PCCIATN1.
           IF AIF-OPPO-ACCOUNT-NO NOT = SPACES
           THEN
             IF AIF-OPPO-ACCOUNT-NO = I-ACCT-NO OF PKSADEP0
             THEN
               MOVE AIF-ACCOUNT-NO     TO I-OPP-ACCT-NO     OF PCCIATN1
             ELSE
               MOVE AIF-OPPO-ACCOUNT-NO
                                       TO I-OPP-ACCT-NO     OF PCCIATN1
             END-IF
           END-IF
           IF CWK-OPPO-CUST-NAME NOT = SPACES AND LOW-VALUES
              AND CWK-DR-CR-FLAG = C-ACA-CR-COD-RMRK
           THEN
             MOVE CWK-OPPO-CUST-NAME   TO I-OPP-ACCT-NAME   OF PCCIATN1
           END-IF.
           MOVE I-CURR-COD OF PKSADEP0 TO I-CURR-COD        OF PCCIATN1
           MOVE I-CURR-IDEN OF PKSADEP0 
                                       TO I-CURR-IDEN       OF PCCIATN1
           MOVE WK-SYS-TX-LOG-NO       TO I-TX-LOG-NO       OF PCCIATN1
           MOVE SA-PDP-CODE OF OSAACNACN
                                       TO I-PRD-COD         OF PCCIATN1
           PERFORM 9707-CALL-CCIATN1-RTN.
      
       5343-CALL-CCIATN3-SUB-RTN.
           INITIALIZE                  PCCIATN3.
           MOVE I-ACCT-NO OF PKSADEP0  TO I-ACCT-NO         OF PCCIATN3.
           MOVE '1'                    TO I-TX-TYPE         OF PCCIATN3.
           MOVE I-TX-TYP OF PKSADEP0   TO I-TX-COD          OF PCCIATN3.
           MOVE C-CM-FLAG-NO           TO I-EC-FLAG         OF PCCIATN3.
           MOVE C-EC-ACCT-NO-I         TO I-TRF-TYP         OF PCCIATN3.
           MOVE I-AMT OF PKSADEP0      TO I-TX-AMT          OF PCCIATN3.
           MOVE CI-DEPO-LMT OF O-CICOADTL TO I-AVL-AMT      OF PCCIATN3.
           MOVE I-DSCRP-COD OF PKSADEP0
                                       TO I-DSCRP-COD       OF PCCIATN3.
           MOVE I-RMRK OF PKSADEP0     TO I-RMRK            OF PCCIATN3.
           MOVE CI-OVER-DRAW-AMT OF O-CICOADTL
                                       TO I-OD-AMT          OF PCCIATN3.
           IF AIF-OPPO-ACCOUNT-NO NOT = SPACES
           THEN
             IF AIF-OPPO-ACCOUNT-NO = I-ACCT-NO OF PKSADEP0
             THEN
               MOVE AIF-ACCOUNT-NO   TO I-OPP-ACCT-NO       OF PCCIATN3
             ELSE
               MOVE AIF-OPPO-ACCOUNT-NO
                                     TO I-OPP-ACCT-NO       OF PCCIATN3
             END-IF
           END-IF
           IF CWK-OPPO-CUST-NAME NOT = SPACES AND LOW-VALUES
              AND CWK-DR-CR-FLAG = C-ACA-CR-COD-RMRK
           THEN
             MOVE CWK-OPPO-CUST-NAME   TO I-OPP-ACCT-NAME   OF PCCIATN3
           END-IF.
           MOVE I-CURR-COD  OF PKSADEP0 
                                       TO I-CURR-COD        OF PCCIATN3
           MOVE I-CURR-IDEN OF PKSADEP0 
                                       TO I-CURR-IDEN       OF PCCIATN3
           MOVE WK-SYS-TX-LOG-NO       TO I-TX-LOG-NO       OF PCCIATN3
           MOVE I-DOC-TYP OF PKSADEP0  TO I-DOC-TYP         OF PCCIATN3.
           MOVE I-DOC-NO  OF PKSADEP0  TO I-DOC-NO          OF PCCIATN3. 
           
           PERFORM 9706-CALL-CCIATN3-RTN.
      
      
      *    电信诈骗，有权机关消息推送 
       5344-CALL-CCIATN3-SUB-RTN.
           INITIALIZE PCCIATN3.
           MOVE I-ACCT-NO OF PKSADEP0  TO I-ACCT-NO         OF PCCIATN3.
           MOVE '1'                    TO I-TX-TYPE         OF PCCIATN3.
           MOVE I-TX-TYP OF PKSADEP0   TO I-TX-COD          OF PCCIATN3.
           MOVE C-EC-ACCT-NO-I         TO I-TRF-TYP         OF PCCIATN3.
           MOVE C-CM-FLAG-YES          TO I-EC-FLAG         OF PCCIATN3.
           MOVE I-AMT OF PKSADEP0      TO I-TX-AMT          OF PCCIATN3.
           MOVE CI-DEPO-LMT OF O-CICOADTL 
                                       TO I-AVL-AMT         OF PCCIATN3.
           MOVE C-REVERSE OF MEMCONST  TO I-DSCRP-COD       OF PCCIATN3.
           MOVE SPACES                 TO I-RMRK            OF PCCIATN3.
           MOVE CI-OVER-DRAW-AMT OF O-CICOADTL
                                       TO I-OD-AMT          OF PCCIATN3.
           IF AIF-OPPO-ACCOUNT-NO NOT = SPACES
           THEN
             IF AIF-OPPO-ACCOUNT-NO = I-ACCT-NO OF PKSADEP0
             THEN
               MOVE AIF-ACCOUNT-NO     TO I-OPP-ACCT-NO     OF PCCIATN3
             ELSE
               MOVE AIF-OPPO-ACCOUNT-NO
                                       TO I-OPP-ACCT-NO     OF PCCIATN3
             END-IF
           END-IF
           IF CWK-OPPO-CUST-NAME NOT = SPACES AND LOW-VALUES
              AND CWK-DR-CR-FLAG = C-ACA-CR-COD-RMRK
           THEN
             MOVE CWK-OPPO-CUST-NAME   TO I-OPP-ACCT-NAME   OF PCCIATN3
           END-IF.
           MOVE I-CURR-COD OF PKSADEP0 TO I-CURR-COD        OF PCCIATN3
           MOVE I-CURR-IDEN OF PKSADEP0 
                                       TO I-CURR-IDEN       OF PCCIATN3
           MOVE WK-SYS-TX-LOG-NO       TO I-TX-LOG-NO       OF PCCIATN3
           MOVE I-DOC-TYP OF PKSADEP0  TO I-DOC-TYP         OF PCCIATN3.
           MOVE I-DOC-NO OF PKSADEP0   TO I-DOC-NO          OF PCCIATN3. 
           PERFORM 9706-CALL-CCIATN3-RTN.
      
      
       6000-SETUP-ACCOUNTING-RTN.
           IF SYS-TX-TYP =  C-CLG-TXT-NOR
           THEN
             PERFORM 6100-AMT-RTN
             PERFORM 6200-CONDITION-RTN
             PERFORM 6300-ACCOUNTING-RULE-RTN
           END-IF.
      
       6100-AMT-RTN.
           MOVE 'SA0'                  TO SA-BUSN-COD OF PCCBAAMT.
           MOVE I-AMT OF PKSADEP0      TO SA-AMT-TX-AMT OF PCCBAAMT.
           IF I-ACCT-NO-EC OF PKSADEP0  = C-CM-FLAG-YES
           THEN
              COMPUTE SA-AMT-TX-AMT OF PCCBAAMT =
                      0 - SA-AMT-TX-AMT OF PCCBAAMT
              COMPUTE SA-NET-DEP-AMT OF PCCBAAMT =
                      0 - WK-INTC-AMT-ADD
              COMPUTE SA-OD-AMT OF PCCBAAMT =
                      0 - WK-OD-INT-AMT-SUB
              COMPUTE SA-AMT-OD-PRIN-INT OF PCCBAAMT =
                      0 - WK-OD-INT-SUB
              COMPUTE SA-OVDLN-OD-INT OF PCCBAAMT =
                      0 - WK-OVDLN-OD-INT-SUB
              COMPUTE SA-AMT-INT OF PCCBAAMT =
                      0 - WK-INTC-AMT-SUB
           ELSE
              COMPUTE SA-NET-DEP-AMT OF PCCBAAMT =
                      0 + WK-INTC-AMT-ADD
              COMPUTE SA-OD-AMT OF PCCBAAMT =
                      0 + WK-OD-INT-AMT-SUB
              COMPUTE SA-AMT-OD-PRIN-INT OF PCCBAAMT =
                      0 + WK-OD-INT-SUB
              COMPUTE SA-OVDLN-OD-INT OF PCCBAAMT =
                      0 + WK-OVDLN-OD-INT-SUB
              COMPUTE SA-AMT-INT OF PCCBAAMT =
                      0 + WK-INTC-AMT-SUB
           END-IF.
           MOVE PCCBAAMT               TO IBG-AAMT-AREA OF PCCBIBG.
      
       6200-CONDITION-RTN.
           MOVE 'SA0'                  TO BUSN-COD OF PCCBACNE.
           MOVE WK-PDP-CODE            TO PRD-CODE OF PCCBACNE.
           MOVE WK-AMT-TYP             TO AMT-TYP  OF PCCBACNE.
           MOVE WK-PERSON-TYP          TO DEP-PERSON-TYP OF PCCBACNE.
           MOVE WK-UPD-FLG             TO UPD-FLG OF PCCBACNE
           MOVE WK-SLEEP-FLG           TO SLEEP-STS OF PCCBACNE
           MOVE PCCBACNE               TO IBG-ACNE-AREA OF PCCBIBG.
           
           IF I-ACCT-NO-EC OF PKSADEP0 = C-CM-FLAG-YES
           THEN 
             MOVE I-ACCT-NO OF PKSADEP0  
                                       TO AIF-TR-OUT-ACCOUNT 
             MOVE   WK-OPAC-BRANCH     TO AIF-TR-OUT-BRANCH        
           ELSE
             MOVE I-ACCT-NO OF PKSADEP0  
                                       TO AIF-TR-IN-ACCOUNT
             MOVE   WK-OPAC-BRANCH     TO AIF-TR-IN-BRANCH
           END-IF.
           
           IF I-CURR-COD OF PKSADEP0 = C-CM-RMB-COD
           THEN
             MOVE C-ACTBOOK-RMB        TO AIF-BOOK-TYPE
           ELSE
             MOVE C-ACTBOOK-FX         TO AIF-BOOK-TYPE
           END-IF.
           MOVE I-CURR-COD OF PKSADEP0 TO AIF-CURRENCY-CODE.
           MOVE 'SA0'                  TO BUSN-COD OF PCCBACND.
           MOVE C-TR-DIR-DEPOSITE      TO AIF-TR-DIRECTION.
           MOVE I-TX-TYP OF PKSADEP0   TO TX-TYP OF PCCBACND.
           MOVE SA-OD-LG-FLG OF SAACNAMT
                                       TO SA-OD-LG-FLG OF PCCBACND.
           MOVE SA-CURR-TYP OF SAACNACN
                                       TO SA-CURR-TYP OF PCCBACND.
           MOVE SA-DDP-OD-MAX OF SAACNAMT
                                       TO DDP-OD-MAX OF PCCBACND.
           MOVE I-ACCT-NO-EC OF PKSADEP0 
                                       TO SA-ACCT-NO-EC OF PCCBACND.
           MOVE PCCBACND               TO IBG-ACND-AREA OF PCCBIBG.
          
      
       6300-ACCOUNTING-RULE-RTN.
           IF I-DSCRP-COD OF PKSADEP0 NOT = SPACES
           THEN
             MOVE I-DSCRP-COD OF PKSADEP0 
                                       TO  IBG-NAR-MEMO-DES(1:4)
           END-IF
           IF I-RMRK OF PKSADEP0 NOT = SPACES
           THEN
             MOVE I-RMRK OF PKSADEP0   TO  IBG-NAR-MEMO-DES(5:58)
           END-IF.
           CALL 'GCCBIBG'              USING APA-AREA
                                             PCCBIBG.
           PERFORM 9999-MESSAGE-HANDLE-RTN. 
       9000-END-TXN-RTN.
           MOVE WK-CORP-TRANS-FLAG     TO CWK-CORP-TRANS-FLAG.
           CALL 'GSYSMOTR' USING AIF-AREA
                                 CONTENT 'EKSADEP0'.
      
       9701-GEN-DSCRP-COD-RTN.
           EVALUATE I-TX-TYP OF PKSADEP0
             WHEN C-CM-TXTYP-CASH
               MOVE C-CSH-DEP OF MEMCONST
                                       TO I-DSCRP-COD OF PKSADEP0
             WHEN C-CM-TXTYP-CLRIN
             WHEN C-CM-TXTYP-CLROOT
             WHEN C-CM-TXTYP-TRC
               MOVE C-CLN-DEP OF MEMCONST
                                       TO I-DSCRP-COD OF PKSADEP0
             WHEN OTHER
               MOVE C-TRF-DEP OF MEMCONST
                                       TO I-DSCRP-COD OF PKSADEP0
           END-EVALUATE.
      
   
       
       9703-CALL-CCMACC6-RTN.
      
           CALL 'CCMACC6' USING        AIF-AREA
                                       PCCMACC6
                                       SYS-AREA.
           PERFORM 9999-MESSAGE-HANDLE-RTN.
       
      
       9705-CALL-CECSPT0-RTN.
           CALL 'CECSPT0'        USING AIF-AREA
                                       PCECSPT0
                                       SYS-AREA.
           PERFORM 9999-MESSAGE-HANDLE-RTN.
           IF O-MSG-TYPE OF PCECSPT0 NOT = SPACE
           THEN
             MOVE O-RTRN-CODE OF PCECSPT0
                                       TO AIF-MSG-CODE
             PERFORM 9999-MESSAGE-HANDLE-RTN
           END-IF. 
       9706-CALL-CCIATN3-RTN.
           CALL 'CCIATN3'              USING AIF-AREA
                                             PCCIATN3
                                             SYS-AREA.
           PERFORM 9999-MESSAGE-HANDLE-RTN.
           IF O-RTRN-CODE OF PCCIATN3(1:1) NOT = SPACE
           THEN
             MOVE O-RTRN-CODE OF PCCIATN3 
                                       TO AIF-MSG-CODE
             PERFORM 9999-MESSAGE-HANDLE-RTN
           END-IF.
       9707-CALL-CCIATN1-RTN.
           CALL 'CCIATN1' USING AIF-AREA
                                PCCIATN1
                                SYS-AREA.
           PERFORM 9999-MESSAGE-HANDLE-RTN.
           IF O-RTRN-CODE OF PCCIATN1(1:1) NOT = SPACE
           THEN
             MOVE O-RTRN-CODE OF PCCIATN1 
                                       TO AIF-MSG-CODE
             PERFORM 9999-MESSAGE-HANDLE-RTN
           END-IF.
       
       9709-CALL-CSAQRY1-RTN.
           INITIALIZE                  PCSAQRY1.
           MOVE I-ACCT-NO OF PKSADEP0  TO I-ACCT-NO OF PCSAQRY1.
           MOVE I-CURR-COD OF PKSADEP0 TO I-CURR-COD OF PCSAQRY1.
           MOVE I-CURR-IDEN OF PKSADEP0
                                       TO I-CURR-IDEN OF PCSAQRY1.
           MOVE C-SA-DEP-CHK           TO I-CHECK-FUNC OF PCSAQRY1.
           CALL 'CSAQRY1'              USING AIF-AREA
                                             PCSAQRY1
                                             SYS-AREA.
           PERFORM 9999-MESSAGE-HANDLE-RTN.
           IF O-MSG-TYPE OF PCSAQRY1 NOT = SPACE
           THEN
             MOVE O-RTRN-CODE OF PCSAQRY1
                                       TO AIF-MSG-CODE
             PERFORM 9999-MESSAGE-HANDLE-RTN
           END-IF.
       9710-CALL-CCMCUR2-RTN.
           INITIALIZE                  PCCMCUR2.
           MOVE I-CURR-COD OF PKSADEP0 TO I-CURR-COD OF PCCMCUR2.
           CALL 'CCMCUR2' USING AIF-AREA
                                PCCMCUR2
                                SYS-AREA.
           PERFORM 9999-MESSAGE-HANDLE-RTN.
           IF O-MSG-TYPE OF PCCMCUR2 NOT = SPACE
           THEN
             MOVE O-RTRN-CODE OF PCCMCUR2
                                       TO AIF-MSG-CODE
             PERFORM 9999-MESSAGE-HANDLE-RTN
           END-IF.
       9711-CALL-CSACHK0-RTN.
           INITIALIZE                  PCSACHK0.
           MOVE WK-ACCT-NO             TO I-ACCT-NO OF PCSACHK0.
           CALL 'CSACHK0' USING AIF-AREA
                                PCSACHK0
                                SYS-AREA.
           PERFORM 9999-MESSAGE-HANDLE-RTN.
           IF O-MSG-TYPE OF PCSACHK0 NOT = SPACE
           THEN
             MOVE O-RTRN-CODE OF PCSACHK0
                                       TO AIF-MSG-CODE
             PERFORM 9999-MESSAGE-HANDLE-RTN
           END-IF.
       9712-CALL-CPDPSA0-RTN.
           INITIALIZE                  PCPDPSA0.
           MOVE I-CURR-COD OF PKSADEP0 TO I-CURR-COD OF PCPDPSA0
           MOVE WK-PDP-CODE            TO I-PDP-CODE OF PCPDPSA0
           MOVE  '2'                   TO I-CUST-TYP OF PCPDPSA0
           MOVE  '1'                   TO I-QRY-TYPE OF PCPDPSA0
           MOVE 'SAA'                  TO I-PD-PRT-COD 
                                       OF I-PDP-PRT-COD-GRP 
                                       OF PCPDPSA0(1)
           MOVE 'SAC'                  TO I-PD-PRT-COD 
                                       OF I-PDP-PRT-COD-GRP 
                                       OF PCPDPSA0(2)
           MOVE 'SAI'                  TO I-PD-PRT-COD 
                                       OF I-PDP-PRT-COD-GRP 
                                       OF PCPDPSA0(3)                                                        
           CALL 'CPDPSA0'              USING AIF-AREA
                                             PCPDPSA0
                                              SYS-AREA.
           PERFORM 9999-MESSAGE-HANDLE-RTN.
           IF O-MSG-TYPE OF PCPDPSA0 NOT = SPACE
           THEN
             MOVE O-RTRN-CODE OF PCPDPSA0
                                       TO AIF-MSG-CODE
             PERFORM 9999-MESSAGE-HANDLE-RTN
           END-IF.
       9713-CALL-CECCII7-RTN.
           INITIALIZE                  PCECCII7.
           IF SYS-TX-MODE = C-CM-ONLINE-MODE OR C-CM-TLAT-MODE
           THEN
             MOVE C-TX-TYP-ONL-DEP     TO I-TX-TYP           OF PCECCII7
           ELSE
             MOVE C-TX-TYP-BAT-CTL     TO I-TX-TYP           OF PCECCII7
           END-IF
           MOVE  WK-CUST-NO            TO I-CUST-NO          OF PCECCII7
      
           CALL 'CECCII7'              USING AIF-AREA
                                             PCECCII7
                                             SYS-AREA
           PERFORM 9999-MESSAGE-HANDLE-RTN
           IF O-MSG-TYPE OF PCECCII7 = SPACE AND
                O-CTL-MSGCOD OF PCECCII7 NOT = SPACE
           THEN
             MOVE O-CTL-MSGCOD OF PCECCII7
                                       TO AIF-MSG-CODE
             MOVE O-CTL-MSGTXT OF PCECCII7
                                       TO AIF-MSG-TEXT
             PERFORM 9999-MESSAGE-HANDLE-RTN
           END-IF.
      **********************************
      * 查询集团客户信息
      ******************************** 
       9714-CALL-CCICOA2-RTN.
           INITIALIZE                  PCCICOA2.
           MOVE C-CI-LMT-FUN-ADD       TO I-FUN-CODE        OF PCCICOA2.
           MOVE I-AMT OF PKSADEP0      TO I-AMT             OF PCCICOA2.
           MOVE I-ACCT-NO OF PKSADEP0
                                       TO I-ACCT-NO         OF PCCICOA2.
           MOVE C-CI-BRA-OFF           TO I-HEAD-BRA-ACCT-FLAG
                                       OF PCCICOA2.
      
           IF CWK-OPPO-ACCT-NO NOT = SPACES AND LOW-VALUE
           THEN
             MOVE C-CM-FLAG-YES        TO I-INNER-TRAN-FLG OF PCCICOA2
           END-IF
   
           IF SYS-TX-TYP = C-CLG-TXT-EC
           THEN
             MOVE C-CM-FLAG-YES        TO I-EC-FLAG OF PCCICOA2
           END-IF
           CALL 'CCICOA2'              USING AIF-AREA
                                            PCCICOA2
                                            SYS-AREA.
           PERFORM 9999-MESSAGE-HANDLE-RTN.
           IF O-RTRN-CODE OF PCCICOA2 NOT = SPACES
           THEN
             MOVE O-RTRN-CODE OF PCCICOA2
                                       TO AIF-MSG-CODE
             PERFORM 9999-MESSAGE-HANDLE-RTN
           END-IF.
      **********************************
      *    集团客户子账户存取
      ******************************** 
       9715-CALL-KSATXN1-RTN.
      
           INITIALIZE                  PKSATXN1
           MOVE I-ACCT-NO OF PKSADEP0
                                       TO I-ACCT-NO    OF PKSATXN1
           MOVE I-DSCRP-COD OF PKSADEP0                
                                       TO I-DSCRP-COD  OF PKSATXN1
           MOVE I-TX-TYP OF PKSADEP0   TO I-TX-TYP     OF PKSATXN1
           MOVE I-AMT OF PKSADEP0      TO I-AMT        OF PKSATXN1
           MOVE I-DOC-NO OF PKSADEP0   TO I-DOC-NO     OF PKSATXN1
           MOVE I-DOC-TYP OF PKSADEP0                  
                                       TO I-DOC-TYP    OF PKSATXN1
           MOVE I-RMRK OF PKSADEP0     TO I-RMRK       OF PKSATXN1
           MOVE I-TX-DT OF PKSADEP0    TO I-TX-DT      OF PKSATXN1
           MOVE C-SA-DEP-MOD           TO I-DRW-FLAG   OF PKSATXN1
           MOVE SPACE                  TO I-PSWD       OF PKSATXN1
           MOVE I-VAL-DT OF PKSADEP0   TO I-VAL-DT     OF PKSATXN1
           MOVE I-CURR-COD OF PKSADEP0                 
                                       TO I-CURR-COD   OF PKSATXN1
           MOVE I-CURR-IDEN OF PKSADEP0                
                                       TO I-CURR-IDEN  OF PKSATXN1
           MOVE I-ACCT-NO-EC OF PKSADEP0
                                       TO I-ACCT-NO-EC OF PKSATXN1
           CALL 'GSYSTRIG'             USING APA-AREA
                                             PKSATXN1
                                       CONTENT 'KSATXN1 '
           PERFORM 9999-MESSAGE-HANDLE-RTN
           IF O-RTRN-CODE OF PKSATXN1 NOT = SPACES
           THEN
             MOVE O-RTRN-CODE OF PKSATXN1
                                     TO AIF-MSG-CODE
             PERFORM 9999-MESSAGE-HANDLE-RTN
           END-IF.
       9716-CALL-CCICOA1-RTN.
           INITIALIZE                  PCCICOA1.
           MOVE I-ACCT-NO OF PKSADEP0
                                       TO I-ACCT-NO OF PCCICOA1.
           IF WK-BRH-FLG = C-CM-FLAG-YES
           THEN
             MOVE C-CM-FLAG-YES        TO CWK-CORP-TRANS-FLAG
           END-IF
           MOVE C-CI-BRA-OFF           TO I-HEAD-BRA-ACCT-FLAG
                                       OF PCCICOA1
           CALL 'CCICOA1'              USING AIF-AREA
                                           PCCICOA1
                                           SYS-AREA
           PERFORM 9999-MESSAGE-HANDLE-RTN.
           IF O-RTRN-CODE OF PCCICOA1 NOT = SPACES
           THEN
             MOVE O-RTRN-CODE OF PCCICOA1
                                     TO AIF-MSG-CODE
             PERFORM 9999-MESSAGE-HANDLE-RTN
           END-IF.
      *********************************
      *  24小时登记TMP档
      ********************************
       9717-CALL-CSATMP0-RTN.
           MOVE I-ACCT-NO OF PKSADEP0
                                       TO I-ACCT-NO OF PCSATMP0.
           CALL 'CSATMP0'         USING AIF-AREA
                                        PCSATMP0
                                        SYS-AREA
           PERFORM 9999-MESSAGE-HANDLE-RTN.
           IF O-RTRN-CODE OF PCSATMP0 NOT = SPACES
           THEN
             MOVE O-RTRN-CODE OF PCSATMP0
                                     TO AIF-MSG-CODE
             PERFORM 9999-MESSAGE-HANDLE-RTN
           END-IF.
      *********************************
      *  24小时更新TMP档
      ********************************
       9718-CALL-CSATMP1-RTN.
           INITIALIZE                  PCSATMP1
           MOVE C-ACA-CR-COD-RMRK      TO I-CR-DR-FLG OF PCSATMP1.
           MOVE I-CURR-COD  OF PKSADEP0 
                                       TO I-CURR-COD  OF PCSATMP1.
           MOVE I-CURR-IDEN OF PKSADEP0 
                                       TO I-CURR-IDEN OF PCSATMP1.
           MOVE I-ACCT-NO   OF PKSADEP0
                                       TO I-ACCT-NO   OF PCSATMP1.
           MOVE I-AMT     OF PKSADEP0  TO I-CR-DR-AMT OF PCSATMP1.
           CALL 'CSATMP1'         USING AIF-AREA
                                        PCSATMP1
                                        SYS-AREA
           PERFORM 9999-MESSAGE-HANDLE-RTN.
           IF O-RTRN-CODE OF PCSATMP1 NOT = SPACES
           THEN
             MOVE O-RTRN-CODE OF PCSATMP1
                                     TO AIF-MSG-CODE
             PERFORM 9999-MESSAGE-HANDLE-RTN
           END-IF.
       9722-CALL-CSACAR0-RTN.
            CALL 'CSACAR0'             USING AIF-AREA
                                             PCSACAR0
                                             SYS-AREA
           PERFORM 9999-MESSAGE-HANDLE-RTN
           IF O-MSG-TYPE OF PCSACAR0 NOT = SPACES
           THEN
             MOVE O-RTRN-CODE OF PCSACAR0
                                       TO AIF-MSG-CODE
             PERFORM 9999-MESSAGE-HANDLE-RTN
           END-IF. 
      *******************************
      * 根据一户通账号明细登记
      ******************************
       9723-CALL-CSACAA1-RTN.
           INITIALIZE                  PCSACAA1.
           MOVE WK-FEA-ACCT-NO         TO I-ACCT-NO        OF PCSACAA1.
           MOVE C-SA-DP-FUNC           TO I-FUNC-CODE      OF PCSACAA1.
           MOVE I-AMT         OF PKSADEP0         
                                       TO I-TX-AMT         OF PCSACAA1.
           MOVE SYS-TX-LOG-NO          TO I-TX-LOG-NO      OF PCSACAA1.
           MOVE I-CURR-COD    OF PKSADEP0         
                                       TO I-CURR-COD       OF PCSACAA1.
           MOVE I-CURR-IDEN   OF PKSADEP0         
                                       TO I-CURR-IDEN      OF PCSACAA1.
           MOVE I-TX-TYP      OF PKSADEP0        
                                       TO I-TX-TYP         OF PCSACAA1.
           MOVE I-DOC-NO      OF PKSADEP0       
                                       TO I-DOC-NO         OF PCSACAA1.
           MOVE I-DOC-TYP     OF PKSADEP0       
                                       TO I-DOC-TYP        OF PCSACAA1.
           MOVE I-VAL-DT      OF PKSADEP0       
                                       TO I-VAL-DT         OF PCSACAA1.
           MOVE I-DSCRP-COD   OF PKSADEP0       
                                       TO I-DSCRP-COD      OF PCSACAA1.
           MOVE I-RMRK        OF PKSADEP0   
                                       TO I-RMRK           OF PCSACAA1.
           IF  I-TX-TYP OF PKSADEP0 = C-CM-TXTYP-CASH
           THEN
             MOVE I-AGT-CER-TYP   OF PKSADEP0      
                                       TO I-AGT-CERT-TYP   OF PCSACAA1
             MOVE I-AGT-CER-ID    OF PKSADEP0      
                                       TO I-AGT-CERT-ID    OF PCSACAA1
             MOVE I-AGT-CUST-NAME OF PKSADEP0      
                                       TO I-AGT-CUST-NAME  OF PCSACAA1
           END-IF.
           IF  I-TX-TYP OF PKSADEP0 = C-CM-TXTYP-TR
           THEN
             MOVE CWK-DRAWEE-ACCT-NO   TO I-OP-ACCT-NO     OF PCSACAA1
             MOVE CWK-DRAWEE-NAME      TO I-OP-CUST-NAME   OF PCSACAA1
             MOVE CWK-DRAWEE-BK-NO     TO I-OP-BANK-NO     OF PCSACAA1
           END-IF
           IF SYS-TX-TYP = C-CLG-TXT-EC
           THEN
             MOVE KBN-ACC-UNPRINT-NO   TO I-EC-DET-NO      OF PCSACAA1
           END-IF
           MOVE I-SUB-ACCT-NO  OF PKSADEP0
                                       TO I-SUB-ACCT-NO    OF PCSACAA1.
           CALL 'CSACAA1'              USING AIF-AREA
                                             PCSACAA1
                                             SYS-AREA.
           PERFORM 9999-MESSAGE-HANDLE-RTN.
      
           IF O-RTRN-CODE OF PCSACAA1 NOT = SPACE
           THEN 
             MOVE O-RTRN-CODE OF PCSACAA1
                                       TO AIF-MSG-CODE
             PERFORM 9999-MESSAGE-HANDLE-RTN
           END-IF.
      ********************************
      * 关注客户控制专题
      ********************************* 
       9724-CALL-PCCBAPAT-RTN.
           INITIALIZE                  PCCBAPAT.
           MOVE I-ACCT-NO  OF PKSADEP0 TO I-ACCT-NO      OF PCCBAPAT
           MOVE I-CURR-COD OF PKSADEP0 TO I-CURR         OF PCCBAPAT
           MOVE C-ACA-CR-COD-RMRK      TO I-DR-CR-COD    OF PCCBAPAT
           MOVE SA-CUST-NO OF OSAACNACN TO I-CUST-NO     OF PCCBAPAT
           IF I-TX-TYP OF PKSADEP0 = C-CM-TXTYP-CASH
           THEN
             MOVE C-CM-TXN-TYP-CASH-DPST
                                       TO  I-TXN-TYP        OF PCCBAPAT
           ELSE                       
             MOVE C-CM-TXN-TYP-TRSF-DPST  
                                       TO  I-TXN-TYP        OF PCCBAPAT
           END-IF
           CALL 'GCCBAPAT'             USING APA-AREA
                                              PCCBAPAT
           PERFORM 9999-MESSAGE-HANDLE-RTN.
      
       9725-CALL-CCMMSC2-RTN.
           
            INITIALIZE                  PCCMMSC2.
            MOVE C-CM-FUN-INQUIRE      TO I-FUN-CODE OF PCCMMSC2.
            MOVE 'SAW'                 TO I-WORD-CLSFN OF PCCMMSC2.
            MOVE ZEROS                 TO I-LEGAL-PERSON-ID OF PCCMMSC2
            MOVE ZEROS                 TO I-WORD-COD-MSC(1:4).
            MOVE SA-FX-ACCT-CHAR OF OSAACNACN
                                       TO I-WORD-COD-MSC(5:4).
            CALL 'CCMMSC2'       USING AIF-AREA
                                       PCCMMSC2
                                       SYS-AREA.
            PERFORM 9999-MESSAGE-HANDLE-RTN.
            IF O-MSG-TYPE OF PCCMMSC2 NOT = SPACES
            THEN
              MOVE O-RTRN-CODE OF PCCMMSC2
                                       TO AIF-MSG-CODE
              PERFORM 9999-MESSAGE-HANDLE-RTN
            END-IF.
      ********************************
      * 控制对象登记 --限额控制
      *    04 系统账号+产品代码
      *    01 产品代码 
      *    A5 系统账号
      *     02 现金存入
      *     06 转账存入
      *     03 存入且检查
      ********************************* 
       9726-CALL-PCCMROB1-RTN.
           INITIALIZE                  PCCMROB1.
           MOVE C-CM-RGST-TYP-PROD-SYS TO I-RGST-TYP        OF PCCMROB1
           MOVE C-CM-OBJ-TYP-PROD-NO   TO I-OBJ-TYP         OF PCCMROB1
           MOVE SA-PDP-CODE OF OSAACNACN 
                                       TO I-OBJ-NO          OF PCCMROB1
           MOVE C-CM-SRN-TYP-SYS-ACCT  TO I-SRN-TYP         OF PCCMROB1
           MOVE I-ACCT-NO OF PKSADEP0  TO I-SRN-NO          OF PCCMROB1
           MOVE SA-LEGAL-PERSON-ID OF OSAACNACN    
                                       TO  I-LEGAL-PERSON-ID OF PCCMROB1
           IF I-CURR-COD OF PKSADEP0 = C-CM-RMB-COD OR  C-CM-USD-COD
           THEN
             MOVE I-CURR-COD OF PKSADEP0 
                                       TO  I-CURR           OF PCCMROB1
             MOVE  I-AMT OF PKSADEP0   TO  I-TXN-AMT        OF PCCMROB1                          
           ELSE
             MOVE  C-CM-USD-COD        TO  I-CURR           OF PCCMROB1
             IF I-AMT OF PKSADEP0 NOT = ZEROS
             THEN
      *        金额为零时，该CF报错       
               PERFORM 9727-CALL-CPRCAL1-RTN
               MOVE IO-SELL-AMT OF PCPRCAL1 
                                       TO I-TXN-AMT         OF PCCMROB1
             ELSE
               MOVE ZEROS              TO I-TXN-AMT         OF PCCMROB1
             END-IF
           END-IF
      *    冲正时，送负值
      *    IF SYS-TX-TYP = C-CLG-TXT-EC
      *    THEN
      *      COMPUTE I-TXN-AMT OF PCCMROB1 =
      *                                I-TXN-AMT OF PCCMROB1 * (-1)
      *    END-IF
      *    MOVE I-CURR-IDEN OF PKSADEP0   IF I-ACCT-NO-EC OF PKSADEP0  = C-CM-FLAG-YES 
      *                                TO  I-CURR-IDEN      OF PCCMROB1 
           MOVE  1                     TO  I-TXN-COUNT      OF PCCMROB1
           MOVE  SYS-LCH-CHANNEL-FLAG  TO  I-CNL-NO         OF PCCMROB1
           IF I-TX-TYP OF PKSADEP0 = C-CM-TXTYP-CASH
           THEN
             MOVE C-CM-TXN-TYP-CASH-DPST
                                       TO  I-TXN-TYP        OF PCCMROB1
           ELSE                       
             MOVE C-CM-TXN-TYP-TRSF-DPST  
                                       TO  I-TXN-TYP        OF PCCMROB1
           END-IF
      *    MOVE                        TO  I-AREA-CODE      OF PCCMROB1
           MOVE C-CM-NET-TYP-ALL       TO  I-NET-TYP        OF PCCMROB1
           MOVE C-CM-HANDLE-TYP-CHK-SM TO  I-HANDLE-TYP     OF PCCMROB1  
            CALL 'CCMROB1'       USING AIF-AREA
                                       PCCMROB1
                                       SYS-AREA.
            PERFORM 9999-MESSAGE-HANDLE-RTN.
            IF O-MSG-TYPE OF PCCMROB1 NOT = SPACES
            THEN
              MOVE O-RTRN-CODE OF PCCMROB1
                                       TO AIF-MSG-CODE
              PERFORM 9999-MESSAGE-HANDLE-RTN
            END-IF.
      ********************************
      *    折算美元
      *******************************
       9727-CALL-CPRCAL1-RTN.
           
           INITIALIZE                  PCPRCAL1.
           MOVE C-PR-USD-TRANS         TO I-BUSN-TYP       OF PCPRCAL1.
           MOVE I-CURR-COD OF PKSADEP0 TO I-BUY-CURR-COD   OF PCPRCAL1.  
           MOVE C-CM-USD-COD           TO I-SELL-CURR-COD  OF PCPRCAL1.
           MOVE C-CM-EXC-ID            TO I-BUY-CURR-IDEN  OF PCPRCAL1.
           MOVE C-CM-EXC-ID            TO I-SELL-CURR-IDEN OF PCPRCAL1.
           MOVE SYS-BUSN-DT            TO I-TX-DT          OF PCPRCAL1.
           MOVE I-AMT OF PKSADEP0      TO IO-BUY-AMT      OF PCPRCAL1.
            CALL 'CPRCAL1'       USING AIF-AREA
                                       PCPRCAL1
                                       SYS-AREA.
            PERFORM 9999-MESSAGE-HANDLE-RTN.
            IF O-MSG-TYPE OF PCPRCAL1  NOT = SPACES
            THEN
              MOVE O-RTRN-CODE OF PCPRCAL1
                                       TO AIF-MSG-CODE
              PERFORM 9999-MESSAGE-HANDLE-RTN
            END-IF.
      ********************************
      *   多级账簿发送短信
      *******************************      
       9728-CALL-CCIATN4-RTN.
           INITIALIZE                  PCCIATN4.
           MOVE I-ACCT-NO OF PKSADEP0  TO I-ACCT-NO        OF PCCIATN4.
           MOVE I-BKH-NO  OF PCSABKH4  TO I-BKH-NO         OF PCCIATN4.
           MOVE '1'                    TO I-TX-TYPE        OF PCCIATN4.
      *    冲正标志、摘要代码、备注:正常交易、冲正交易合并到一起
           IF SYS-TX-TYP = C-CLG-TXT-NOR
           THEN
             MOVE C-CM-FLAG-NO         TO I-EC-FLAG        OF PCCIATN4
             MOVE I-DSCRP-COD OF PKSADEP0                  
                                       TO I-DSCRP-COD      OF PCCIATN4
             MOVE I-RMRK OF PKSADEP0   TO I-RMRK           OF PCCIATN4
           ELSE                                            
             MOVE C-CM-FLAG-YES        TO I-EC-FLAG        OF PCCIATN4
             MOVE C-REVERSE OF MEMCONST                    
                                       TO I-DSCRP-COD      OF PCCIATN4
             MOVE SPACES               TO I-RMRK           OF PCCIATN4
           END-IF.
   
           MOVE I-AMT OF PKSADEP0      TO I-TX-AMT         OF PCCIATN4.
                                                           
      *    可用金额、透支金额、卡号                        
           MOVE SA-AVL-BAL OF SAACNAMT TO I-AVL-AMT        OF PCCIATN4.
           MOVE SA-OD-INT-AMT OF SAACNAMT TO I-OD-AMT      OF PCCIATN4.
           MOVE SA-CARD-NO OF SAACNACN TO I-CRD-NO         OF PCCIATN4.
                                                           
           MOVE O-SELF-BAL OF PCSABKH4 TO I-BKH-AMT        OF PCCIATN4.
   
           IF I-TX-TYP OF PKSADEP0 = C-CM-TXTYP-TR AND
              CWK-DRAWEE-ACCT-NO NOT = SPACES
           THEN
             MOVE CWK-DRAWEE-ACCT-NO   TO I-OPP-ACCT-NO    OF PCCIATN4
           END-IF.
   
           IF I-TX-TYP OF PKSADEP0 = C-CM-TXTYP-TR AND
              CWK-DRAWEE-NAME NOT = SPACES
           THEN
             MOVE CWK-DRAWEE-NAME      TO I-OPP-ACCT-NAME OF PCCIATN4
           END-IF.
   
           MOVE I-CURR-COD OF PKSADEP0 TO I-CURR-COD      OF PCCIATN4.
           MOVE I-CURR-IDEN OF PKSADEP0                   
                                       TO I-CURR-IDEN     OF PCCIATN4.
           MOVE WK-SYS-TX-LOG-NO       TO I-TX-LOG-NO     OF PCCIATN4.
           MOVE SYS-TX-ID              TO I-TX-COD        OF PCCIATN4.
           MOVE C-CI-CON-TYP-T02       TO I-CON-TYP       OF PCCIATN4.
           CALL 'CCIATN4' USING        AIF-AREA
                                       PCCIATN4
                                       SYS-AREA.
           PERFORM 9999-MESSAGE-HANDLE-RTN.
           IF O-RTRN-CODE OF PCCIATN4(1:1) NOT = SPACE
           THEN
             MOVE O-RTRN-CODE OF PCCIATN4 
                                       TO AIF-MSG-CODE
             PERFORM 9999-MESSAGE-HANDLE-RTN
           END-IF.                     
      ********************************* 
      *    写未登折档SAPRTPRT.
      **********************************
       9801-INSERT-PRT-RTN.
      
           INITIALIZE                  PDBIMAIN
           INITIALIZE                  SAPRTPRT
           MOVE FK-SAACN-KEY OF SAACNTXN
                                       TO SA-ACCT-NO        OF SAPRTPRT
           MOVE SA-DDP-ACCT-NO-DET-N OF SAACNTXN
                                       TO SA-DDP-ACCT-NO-DET-N
                                       OF SAPRTPRT
           MOVE SA-CURR-COD OF SAACNTXN TO SA-CURR-COD      OF SAPRTPRT
           MOVE SA-CURR-IDEN OF SAACNTXN
                                       TO SA-CURR-IDEN      OF SAPRTPRT
           MOVE SA-TX-DT OF SAACNTXN   TO SA-TX-DT          OF SAPRTPRT
           MOVE SA-CR-AMT OF SAACNTXN  TO SA-CR-AMT         OF SAPRTPRT
           MOVE SA-DR-AMT OF SAACNTXN  TO SA-DR-AMT         OF SAPRTPRT
           MOVE SA-DDP-ACCT-BAL OF SAACNTXN
                                       TO SA-DDP-ACCT-BAL   OF SAPRTPRT
           MOVE SA-DSCRP-COD OF SAACNTXN
                                       TO SA-DSCRP-COD      OF SAPRTPRT
           MOVE SA-RMRK OF SAACNTXN    TO SA-RMRK           OF SAPRTPRT
           MOVE SA-EC-FLG OF SAACNTXN  TO SA-EC-FLG         OF SAPRTPRT
           MOVE SA-OPR-NO OF SAACNTXN  TO SA-OPR-NO         OF SAPRTPRT
           MOVE SYS-LCH-CHANNEL-FLAG   TO SA-CHAN-NO        OF SAPRTPRT
           MOVE WK-LEGAL-PERSON-ID     TO SA-LEGAL-PERSON-ID OF 
                                                             SAPRTPRT.
           MOVE WK-DB-PARTITION-ID     TO SA-DB-PARTITION-ID  OF
                                                              SAPRTPRT.
           MOVE 'BSAPRTD'              TO DBI-DB-NAME
           MOVE 'SAPRTPRT'             TO DBI-SEGMENT-NAME(1)
           MOVE FT-ISRT                TO DBI-FT-NAME.
           CALL 'GDBIMAIN'       USING PDBIMAIN
                                       SAPRTPRT
                                       AIF-AREA.
           PERFORM 9999-MESSAGE-HANDLE-RTN.
           
       9802-GU-SAACNACN-RTN.
           INITIALIZE                  PDBIMAIN
           INITIALIZE                  SAACNACN
           MOVE FT-RTRN-NOTFOUND       TO DBI-NORMAL-STATUS(1).
           MOVE 'SAACNACN'             TO DBI-SEGMENT-NAME(1)
           MOVE 'BSAACND'              TO DBI-DB-NAME.
           MOVE FT-GHU                 TO DBI-FT-NAME.
      *    MOVE FT-GU                  TO DBI-FT-NAME.
           MOVE I-ACCT-NO OF PKSADEP0  TO DBI-KEY-VALUE1(1).
           CALL 'GDBIMAIN' USING PDBIMAIN
                                 SAACNACN
                                 AIF-AREA.
           PERFORM 9999-MESSAGE-HANDLE-RTN.
           
      
       9803-GU-SAACNAMT-RTN.
           INITIALIZE PDBIMAIN.
           MOVE FT-RTRN-NOTFOUND       TO DBI-NORMAL-STATUS(1).
           MOVE 'BSAACND'              TO DBI-DB-NAME.
           MOVE FT-GU                  TO DBI-FT-NAME.
           MOVE I-ACCT-NO OF PKSADEP0  TO DBI-KEY-VALUE1(1).
           MOVE 'SAACNACN'             TO DBI-SEGMENT-NAME(1).
           MOVE 'SAACNAMT'             TO DBI-SEGMENT-NAME(2).
           MOVE I-CURR-COD OF PKSADEP0 TO WK-CURR-COD.
           MOVE I-CURR-IDEN OF PKSADEP0
                                       TO WK-CURR-IDEN.
           MOVE WK-SAACNAMT-KEY        TO DBI-KEY-VALUE1(2).
           CALL 'GDBIMAIN' USING PDBIMAIN
                                 SAACNAMT
                                 AIF-AREA.
           PERFORM 9999-MESSAGE-HANDLE-RTN.
      *********************************
      * 查询SAACNAGR
      ********************************
       9804-GU-SAACNAGR-RTN.
           INITIALIZE                  PDBIMAIN.  
           INITIALIZE                  SAACNAGR. 
           MOVE FT-RTRN-NOTFOUND       TO DBI-NORMAL-STATUS(1).
           MOVE 'BSAACND'              TO DBI-DB-NAME.
           MOVE FT-GU                  TO DBI-FT-NAME.
           MOVE I-ACCT-NO OF PKSADEP0  TO DBI-KEY-VALUE1(1).
           MOVE 'SAACNACN'             TO DBI-SEGMENT-NAME(1).
           MOVE I-CURR-COD OF PKSADEP0 TO WK-CURR-COD OF
                                          WK-SAACNAMT-KEY.
           MOVE I-CURR-IDEN OF PKSADEP0
                                       TO WK-CURR-IDEN OF
                                          WK-SAACNAMT-KEY.
           MOVE WK-SAACNAMT-KEY        TO DBI-KEY-VALUE1(2).
           MOVE WK-SAAGR-KEY           TO DBI-KEY-VALUE1(3).
           MOVE 'SAACNAMT'             TO DBI-SEGMENT-NAME(2).
           MOVE 'SAACNAGR'             TO DBI-SEGMENT-NAME(3).
           CALL 'GDBIMAIN'       USING PDBIMAIN
                                       SAACNAGR
                                       AIF-AREA.
           PERFORM 9999-MESSAGE-HANDLE-RTN.
           IF ( DBI-DB-STATUS = FT-RTRN-NOTFOUND )
           THEN
             MOVE 'ENF41'              TO AIF-MSG-CODE
             PERFORM 9999-MESSAGE-HANDLE-RTN
           END-IF.
      *    PERFORM 5611-CLOSE-SAACNAGR-RTN.
      *********************************
      * 查询SAACNCOL
      ********************************
       9805-GU-SAACNCOL-RTN.
           INITIALIZE                  PDBIMAIN.       
           MOVE FT-RTRN-NOTFOUND       TO DBI-NORMAL-STATUS(1).
           MOVE 'BSACOLM'              TO DBI-DB-NAME.
           MOVE FT-GU                  TO DBI-FT-NAME.
           MOVE I-ACCT-NO OF PKSADEP0  TO DBI-KEY-VALUE1(1).
           MOVE 'SAACNCOL'             TO DBI-SEGMENT-NAME(1).
           CALL 'GDBIMAIN'       USING PDBIMAIN
                                       SAACNCOL
                                       AIF-AREA.
           PERFORM 9999-MESSAGE-HANDLE-RTN.
           IF ( DBI-DB-STATUS = FT-RTRN-NOTFOUND )
           THEN
             MOVE 'EN048'              TO AIF-MSG-CODE
             PERFORM 9999-MESSAGE-HANDLE-RTN
           END-IF.
      *********************************
      * 查询SAACNSPV
      ********************************
       9806-GU-SAACNSPV-RTN.
           INITIALIZE                  PDBIMAIN.       
           MOVE FT-RTRN-NOTFOUND       TO DBI-NORMAL-STATUS(1).
           MOVE 'BSASPVM'              TO DBI-DB-NAME.
           MOVE FT-GU                  TO DBI-FT-NAME.
           MOVE I-ACCT-NO OF PKSADEP0  TO DBI-KEY-VALUE1(1).
           MOVE 'SAACNSPV'             TO DBI-SEGMENT-NAME(1).
           CALL 'GDBIMAIN'       USING PDBIMAIN
                                       SAACNSPV
                                       AIF-AREA.
           PERFORM 9999-MESSAGE-HANDLE-RTN.
           IF ( DBI-DB-STATUS = FT-RTRN-NOTFOUND )
           THEN
             MOVE 'EN048'              TO AIF-MSG-CODE
             PERFORM 9999-MESSAGE-HANDLE-RTN
           END-IF.
       
    
       9999-MESSAGE-HANDLE-RTN.
           COPY                        GSYSEHRT.
