      ****************************************************************
       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     "GSA01060".
       AUTHOR.         CBOD.
       DATE-WRITTEN.   2017/07/01.
      ****************************************************************
       ENVIRONMENT DIVISION.
      ****************************************************************
      * PROGRAM NAME.......: GSA01060                                *
      * DESCRIPTION........: ACCT FEA OPEN                           *
      * KB/CF TO BE CALLED.:                                         *
      *                                                              *
      *                                                              *
      * INPUT..............:                                         *
      * OUTPUT.............:                                         *
      *                                                              *
      *                                                              *
      *                                                              *
      * ERROR/WARNING MSG..:                                         *
      *                                                              *
      *                                                              *
      * DATA ACCESS TABLE:                                           *
      * DB NAME | SEGMENT NAME | ACCESS TYPE(R/U/I/D)                *
      * ---------------------------------------------                *
      *                                                              *
      * CHANGE HISTORY:                                              *
      * FLAG    |REASON               |DATE   |MODIFIED BY |COMMENT  *
      * ------------------------------------------------------------ *
      *                                                              *
      *                                                              *
      ****************************************************************
       DATA DIVISION.
      ****************************************************************
       WORKING-STORAGE SECTION.
      ****************************************************************
      ****************************************************************
      * CONSTANT VALUE DEFINATION AREA                               *
      ****************************************************************
       01  FILLER                      PIC X(24)
                                       VALUE '/// WK-AREA GSA01060///'.
      ****************************************************************
      * EXTERNAL KB/CF INTERFACE AREA                                *
      ****************************************************************
       01  PKSAACN1.
           COPY                        PKSAACN1.           
      ****************************************************************
      * WORK VARIABLE DEFINITION AREA                                *
      ****************************************************************
       01  WK-VAR.
           05  WK-TALLY                PIC 9(1).
           05  WK-ACCT-USE             PIC X(45).
           05  WK-ACCT-NO              PIC X(32).
           05  WK-FOUND                PIC X(1). 
           05  WK-EOF-FLAG             PIC X(1).  
           05  WK-CMDATE-END-DATE      PIC X(8). 
           05  WK-AGT-BIRDT            PIC X(8).                                                              
      * 1030外汇开户用子交易码为01
       01  WK-SUB-TX-CODE.
           05  WK-SUB-TX-CODE-1060     PIC X(02) VALUE '00'.
           05  WK-SUB-TX-CODE-1030     PIC X(02) VALUE '01'.  
       01  WK-PDP-CODE                 PIC X(11) VALUE '999SA000002'.  
       01  WK-PERIOD                   PIC 9(5). 
       01  WK-WORD-CLSFN               PIC X(3).
       01  WK-WORD-COD-MSC             PIC X(8). 
          
      * 客户账号档开户时，账户的状态 0 为正常 1 为销户
       01  WK-NORMAL                   PIC X(1) VALUE '0'. 
      * 对私对公标志，1 为对私 2 位对公
       01  WK-PRI-FLG                  PIC X(1) VALUE '1'.  
       
       01  WK-JM-FLAG.
           05 WK-JM-YES                PIC X(1) VALUE '1'.
           05 WK-JM-NO                 PIC X(1) VALUE '2'.
       01  WK-FUN-CODE. 
           05 WK-FUN-ADD               PIC X(1) VALUE '1'. 
      * 代理开户标志 1为代理开户 0位代理销户
       01  WK-OPAC-CANL-FLAG.
           05 WK-OPAC-FLAG             PIC X(1) VALUE '1'. 
       01  WK-FC-LMT-TYPE.
           05 WK-FC-LMT-TYPE-1         PIC X(2) VALUE '11'. 
      * 无限额
           05 WK-FC-LMT-TYPE-2         PIC X(2) VALUE '12'.
      * 余额限额
           05 WK-FC-LMT-TYPE-3         PIC X(2) VALUE '13'.  
      * 贷方流入限额           
      ****************************************************************
       LINKAGE SECTION.
     ****************************************************************
       PROCEDURE DIVISION USING APA-AREA.
      ****************************************************************
       0000-MAIN-PROCESS-RTN.
           
           PERFORM 1000-TXN-INIT-RTN.
           
           PERFORM 2000-INPUT-CHECK-RTN.
           
           PERFORM 3000-OTHER-CHECK-RTN.
           
           PERFORM 5000-FUNCTION-PROCESS-RTN.
           
           PERFORM 7000-OUTPUT-PROCESS-RTN.
           
           PERFORM 9000-TXN-END-RTN.
           GOBACK.
      
       1000-TXN-INIT-RTN.
           SET ADDRESS OF CBIBAARE     TO APA-IBA-ADDR.
           SET ADDRESS OF INM-AREA     TO APA-INM-ADDR.
           CALL 'GSYSMOTR' USING AIF-AREA
                                 CONTENT 'BGSA01060'.
           INITIALIZE                  PKSAACN1.
           INITIALIZE                  PCECCSA8.
           INITIALIZE                  PCSAPME0.
           INITIALIZE                  PCSARMA0.
           INITIALIZE                  PCCMENC3.
           INITIALIZE                  PCCMLST1.
           INITIALIZE                  PCCMMSC2.
           INITIALIZE                  PCCICLTA.
           INITIALIZE                  PCCIRAA0.
           INITIALIZE                  PCECAGT0.
           INITIALIZE                  PCCMAUH1.
           INITIALIZE                  PCECCII1.
           INITIALIZE                  PCECACNC.
           INITIALIZE                  PCECACND.
           INITIALIZE                  PCCMDAT1.
           INITIALIZE                  PCCMCHK1.
           INITIALIZE                  PCCBAPAT.
    
           INITIALIZE                  WK-PCECCII1.
           INITIALIZE                  WK-VAR.
           INITIALIZE                  WK-PERIOD.
           INITIALIZE                  WK-WORD-CLSFN.
           INITIALIZE                  WK-WORD-COD-MSC.
           
           INITIALIZE                  CMMSCSAW.  
         
           PERFORM 1100-INIT-FORM-RTN.           
           
       1100-INIT-FORM-RTN.
           INITIALIZE                  SSA10600.
           INITIALIZE                  PZZZ1120.
           INITIALIZE                  PSA10300.
           INITIALIZE                  PZZC0211.
           INITIALIZE                  MZZC0212.   

                           
       2000-INPUT-CHECK-RTN. 

      * 该交易不允许冲正 
           IF SYS-TX-TYP = C-CLG-TXT-EC
           THEN
             MOVE 'END43'              TO AIF-MSG-CODE
             PERFORM 9999-MESSAGE-HANDLE-RTN
           END-IF.
       
      * 产品代码不能为空
           IF TFT-PD-CODE = SPACES
           THEN
             MOVE 'EN005'              TO AIF-MSG-CODE
             PERFORM 9999-MESSAGE-HANDLE-RTN
           END-IF. 
           
      * 存折印刷号
           IF SYS-LCH-CHANNEL-FLAG = C-CM-CHANNEL-TLR AND
              INM-CC-R-FLG NOT = C-CM-FLAG-YES AND
              TFT-PSBK-PRT-NO1 = SPACES
           THEN
             MOVE 'EN502'              TO AIF-MSG-CODE
             PERFORM 9999-MESSAGE-HANDLE-RTN
           END-IF.
           
      * 账户种类只能为I 和II 类
           IF SYS-SUB-TX-CODE = WK-SUB-TX-CODE-1060
           THEN
             IF TFT-ACCT-KIND = C-SA-ACC-I OR C-SA-ACC-II
             THEN
               CONTINUE
             ELSE
               MOVE 'END55'            TO AIF-MSG-CODE
               PERFORM 9999-MESSAGE-HANDLE-RTN
             END-IF
           END-IF.
     
      * 非外汇活期存款开户，账户性质不能为空
           IF SYS-SUB-TX-CODE = WK-SUB-TX-CODE-1060 AND
              TFT-ACCT-CHAR = SPACES 
           THEN
             MOVE 'EN023'              TO AIF-MSG-CODE
             PERFORM 9999-MESSAGE-HANDLE-RTN
           END-IF.
           
      * 账户性质与外管账户性质二者必须存其一      
           IF (TFT-ACCT-CHAR = SPACES AND
              TFT-FX-ACCT-CHAR = SPACES) OR
              (TFT-ACCT-CHAR NOT = SPACES AND
              TFT-FX-ACCT-CHAR NOT = SPACES)
           THEN
             MOVE 'EN119'              TO AIF-MSG-CODE
             PERFORM 9999-MESSAGE-HANDLE-RTN
           END-IF.
           
      * 代理开户时，前端不再送密码,核心默认置为32个1
           IF TFT-AGT-FLG = C-CM-FLAG-YES
           THEN
             MOVE '11111111111111111111111111111111'   
                                       TO TFT-PSWD 
           END-IF 
      * 联机交易时，支取方式为凭密时，密码不能为空    
           IF TFT-PSWD = SPACE AND 
              (
                TFT-DRW-TYP(4:1) = C-SA-DRW-BY-PWD OR     
                TFT-DRW-TYP(4:1) = C-SA-DRW-BY-FGR-OR-PWD
                                                         ) AND 
              SYS-TX-MODE = C-CM-ONLINE-MODE AND
              INM-CC-R-FLG NOT = C-CM-FLAG-YES 
           THEN
             MOVE 'EN011'              TO AIF-MSG-CODE
             PERFORM 9999-MESSAGE-HANDLE-RTN
           END-IF.
      
      * 支取方式不能为不凭折
           IF TFT-DRW-TYP(1:1) = C-SA-DRW-BY-PSBK-N
           THEN
             MOVE 'EN012'              TO AIF-MSG-CODE
             PERFORM 9999-MESSAGE-HANDLE-RTN
           END-IF.
      
      * 验证支取方式是否合法
           INSPECT TFT-DRW-TYP TALLYING WK-TALLY
                                       FOR ALL '1' '0' 'A' 'C'.
           IF WK-TALLY NOT = 4
           THEN
             MOVE 'EN013'              TO AIF-MSG-CODE
             PERFORM 9999-MESSAGE-HANDLE-RTN
           END-IF.
   
      * 账号若通兑，支取方式必须凭密或指纹
           IF (TFT-DRW-TYP(4:1)  NOT   = C-SA-DRW-BY-PWD AND
               TFT-DRW-TYP(4:1)  NOT   = C-SA-DRW-BY-FINGER AND
               TFT-DRW-TYP(4:1)  NOT   = C-SA-DRW-BY-FGR-OR-PWD) AND
              (TFT-PRDS-INSTN-DPDW-FLG = C-SA-FULL-DP-DW OR
               TFT-PRDS-INSTN-DPDW-FLG = C-SA-COULD-DRW-DW)
           THEN
             MOVE 'EN014'              TO AIF-MSG-CODE
             PERFORM 9999-MESSAGE-HANDLE-RTN
           END-IF.
           
      * 外汇开户
           IF SYS-SUB-TX-CODE = WK-SUB-TX-CODE-1030
           THEN
      * 检查机构是否有外汇权限
             IF CM-FX-BUSN-HQBK OF BCT-AREA = SPACES
             THEN
               MOVE 'EN363'            TO AIF-MSG-CODE
               PERFORM 9999-MESSAGE-HANDLE-RTN
             END-IF             
          
      * 币种，钞汇鉴别不能为空
             IF TFT-CURR-COD = SPACES OR
                TFT-CURR-IDEN = SPACES
             THEN
               MOVE 'EN442'            TO AIF-MSG-CODE
               PERFORM 9999-MESSAGE-HANDLE-RTN
             END-IF
                        
      * 外管账户性质不能为空
             IF TFT-FX-ACCT-CHAR = SPACES
             THEN
               MOVE 'EN580'            TO AIF-MSG-CODE
               PERFORM 9999-MESSAGE-HANDLE-RTN
             ELSE
               MOVE 'SAW'              TO WK-WORD-CLSFN
               MOVE ZEROS              TO WK-WORD-COD-MSC(1:4)
               MOVE TFT-FX-ACCT-CHAR   TO WK-WORD-COD-MSC(5:4)
               PERFORM 9708-CALL-CCMMSC2-RTN
               MOVE IO-APLY-DATA-DSCRP 
                     OF PCCMMSC2       TO CM-APLY-DATA-DSCRP-SAW OF
                                          CMMSCSAW
      * 币别必须为人民币的外管账户性质                                    
               IF CM-PER-RMB = C-CM-FLAG-YES AND
                   TFT-CURR-COD NOT = C-CM-RMB-COD
               THEN
                 MOVE 'EN581'          TO AIF-MSG-CODE
                 PERFORM 9999-MESSAGE-HANDLE-RTN
               END-IF
      * 币别不支持人民币的外管账户性质         
               IF CM-PER-RMB    = C-CM-FLAG-NO AND
                  TFT-CURR-COD  = C-CM-RMB-COD
               THEN
                 MOVE 'EN582'          TO AIF-MSG-CODE
                 PERFORM 9999-MESSAGE-HANDLE-RTN
               END-IF
               
      * ****************************************************************
      * 外管账户性质为资本项目类时：外管核准件编号必输， 账户限额、限额
      * 类型必输
      * ****************************************************************    
               IF CM-SAW-M-FLG = C-CM-FLAG-YES
               THEN     
                 IF TFT-SFEA-APPR-NO = SPACES
                 THEN
                   MOVE 'EN117'        TO AIF-MSG-CODE
                   PERFORM 9999-MESSAGE-HANDLE-RTN
                 END-IF
                 
                 IF TFT-FC-LMT-TYPE = SPACES OR  
                   (TFT-FC-LMT-AMT  = ZEROS AND
                    TFT-FC-LMT-TYPE NOT =  WK-FC-LMT-TYPE-1)
                 THEN
                   MOVE 'EN583'        TO AIF-MSG-CODE
                   PERFORM 9999-MESSAGE-HANDLE-RTN
                 END-IF
               END-IF
               
      * 外管账户性质为2101 2108 时，账户有效期栏位不能为空
               IF CM-TEMP-FLG = C-CM-FLAG-YES AND
                  TFT-ACCT-VALIDITY = SPACES
               THEN
                 MOVE 'EN584'          TO AIF-MSG-CODE
                 PERFORM 9999-MESSAGE-HANDLE-RTN
               END-IF            
               
      * 外管账户性质为1000时工商登记执照不能为空
               IF CM-INSRT-FLG = C-CM-FLAG-YES AND
                  TFT-BUSN-LICE = SPACES
               THEN
                 MOVE 'EN814'          TO AIF-MSG-CODE
                 PERFORM 9999-MESSAGE-HANDLE-RTN
               END-IF 
                
      * 外管账户性质为2103时开户主体为居民
      * 外管账户性质为2108时开户主体为非居民
               IF CM-HABIT-FLG NOT = SPACES
               THEN
                 IF CM-HABIT-FLG = WK-JM-YES 
                 THEN
                   IF TFT-CERT-TYP = ( C-EC-CER-TYPE1001 OR 
                                     C-EC-CER-TYPE1002 OR 
                                     C-EC-CER-TYPE1004 OR 
                                     C-EC-CER-TYPE1005 OR 
                                     C-EC-CER-TYPE1006 )
                   THEN
                     CONTINUE
                   ELSE
                     MOVE 'EN815'      TO AIF-MSG-CODE
                     PERFORM 9999-MESSAGE-HANDLE-RTN
                   END-IF 
                 END-IF
                 
                 IF CM-HABIT-FLG = WK-JM-NO 
                 THEN
                    IF TFT-CERT-TYP = ( C-EC-CER-TYPE1003 OR  
                                     C-EC-CER-TYPE1007 OR  
                                     C-EC-CER-TYPE1008 OR  
                                     C-EC-CER-TYPE1009 OR  
                                     C-EC-CER-TYPE1010 OR  
                                     C-EC-CER-TYPE1011 OR  
                                     C-EC-CER-TYPE1012 ) 
                   THEN  
                     CONTINUE
                   ELSE
                     MOVE 'EN816'      TO AIF-MSG-CODE
                     PERFORM 9999-MESSAGE-HANDLE-RTN
                   END-IF  
                 END-IF
               END-IF     
             END-IF  
      * 外汇开户：除2101 2108外 非临时身份证开户，有效期必须为空

             IF CM-TEMP-FLG = C-CM-FLAG-NO AND
                TFT-CERT-TYP  NOT = C-EC-CER-TYPE1002 AND
                TFT-ACCT-VALIDITY NOT = SPACES 
             THEN
               MOVE 'EN585'            TO AIF-MSG-CODE
               PERFORM 9999-MESSAGE-HANDLE-RTN
             END-IF 
       
      * 外汇开户，有效期不为空时，不能超过12个月
             IF TFT-ACCT-VALIDITY NOT = SPACES 
             THEN
               INITIALIZE              WK-PERIOD
               MOVE 180                TO WK-PERIOD
               PERFORM 9710-CALL-CCMDAT1-RTN
               
               IF TFT-ACCT-VALIDITY < IO-CMDATE-END-DATE 
               THEN
                 MOVE 'ENE14'          TO AIF-MSG-CODE
                 PERFORM 9999-MESSAGE-HANDLE-RTN
               END-IF
               
               INITIALIZE              WK-PERIOD
               MOVE 360                TO WK-PERIOD
               PERFORM 9710-CALL-CCMDAT1-RTN
               IF TFT-ACCT-VALIDITY > IO-CMDATE-END-DATE
               THEN
                 MOVE 'EN586'          TO AIF-MSG-CODE
                 PERFORM 9999-MESSAGE-HANDLE-RTN
               END-IF
             END-IF                     
           END-IF.
      
      * 代理人标志的合法性检查    
           IF TFT-AGT-FLG = C-CM-FLAG-YES OR C-CM-FLAG-NO
           THEN
             CONTINUE
           ELSE
             MOVE 'END21'              TO AIF-MSG-CODE
             PERFORM 9999-MESSAGE-HANDLE-RTN
           END-IF.
                 
      * 代理业务时，代理人信息检查
           IF TFT-AGT-FLG = C-CM-FLAG-YES 
           THEN
             IF TFT-AGT-CERT-TYP = SPACES OR
                TFT-AGT-CERT-ID  = SPACES OR
                TFT-AGT-NAME     = SPACES OR
                TFT-NATY-COD     = SPACES OR
                TFT-AGT-TEL      = SPACES 
             THEN
 
               MOVE 'EN559'            TO AIF-MSG-CODE
               PERFORM 9999-MESSAGE-HANDLE-RTN
             END-IF
           END-IF.

      * 临时身份证开户有效期不能为空       
           IF TFT-CERT-TYP = C-EC-CER-TYPE1002 AND 
              TFT-ACCT-VALIDITY = SPACES
           THEN 
             MOVE 'EN015'              TO AIF-MSG-CODE
             PERFORM 9999-MESSAGE-HANDLE-RTN
           END-IF.
           
      * 临时身份证开户有效期不能超过3个月
           IF TFT-CERT-TYP = C-EC-CER-TYPE1002 AND 
              TFT-ACCT-VALIDITY NOT = SPACES
           THEN
             INITIALIZE                WK-PERIOD
             MOVE 90                   TO WK-PERIOD
             PERFORM 9710-CALL-CCMDAT1-RTN

             MOVE IO-CMDATE-END-DATE OF PCCMDAT1
                                       TO WK-CMDATE-END-DATE        

             IF TFT-ACCT-VALIDITY > WK-CMDATE-END-DATE 
             THEN
               MOVE 'ENE07'            TO AIF-MSG-CODE
               PERFORM 9999-MESSAGE-HANDLE-RTN
             END-IF
           END-IF.
    
      * 非外汇开户：非临时身份证开户，有效期必须为空
           IF TFT-CERT-TYP  NOT = C-EC-CER-TYPE1002 AND 
              TFT-ACCT-VALIDITY NOT = SPACES  AND
              TFT-PD-CODE NOT = WK-PDP-CODE
           THEN 
             MOVE 'EN585'              TO AIF-MSG-CODE
             PERFORM 9999-MESSAGE-HANDLE-RTN
           END-IF.  
    
           IF TFT-CERT-TYP = C-EC-CER-TYPE1004  AND
              TFT-AGT-FLG  = C-CM-FLAG-NO
           THEN 
             MOVE 'EN016'              TO AIF-MSG-CODE
             PERFORM 9999-MESSAGE-HANDLE-RTN
           END-IF.
    
           IF  TFT-AGT-FLG      = C-CM-FLAG-YES AND 
              (TFT-AGT-CERT-TYP = C-EC-CER-TYPE1004  OR  
                                  C-EC-CER-TYPE1002 )
           THEN
             MOVE 'EN017'              TO AIF-MSG-CODE
             PERFORM 9999-MESSAGE-HANDLE-RTN
           END-IF.
    
       3000-OTHER-CHECK-RTN.
           INITIALIZE                  PCECCII1.
           MOVE TFT-CERT-TYP           TO I-CER-TYP  OF PCECCII1.
           MOVE TFT-CERT-ID            TO I-CER-NO   OF PCECCII1.
           MOVE TFT-CUST-NAME          TO I-FULL-NAM OF PCECCII1.

           CALL 'CECCII1'    USING     AIF-AREA
                                       PCECCII1
                                       SYS-AREA.
           PERFORM 9999-MESSAGE-HANDLE-RTN.
           
           IF O-IDE-RESULT OF PCECCII1 NOT = '1'
           THEN
              MOVE 'ENF86'             TO AIF-MSG-CODE
              PERFORM 9999-MESSAGE-HANDLE-RTN
           END-IF.
           
      * 对公客户不允许开户
           IF O-CUST-TYP OF PCECCII1  =  C-CM-INDIVIDUAL
           THEN
             CONTINUE
           ELSE
             MOVE 'ENF28'              TO AIF-MSG-CODE
             PERFORM 9999-MESSAGE-HANDLE-RTN
           END-IF.
           
      * 非活动客户不允许开户
           IF O-CUST-STS OF PCECCII1 = C-EC-CUST-NOACTIVE
           THEN
             MOVE 'ENE75'              TO AIF-MSG-CODE
             PERFORM 9999-MESSAGE-HANDLE-RTN
           END-IF.
           
      * 潜在客户不允许开户
           IF O-POT-CUST OF PCECCII1 = C-CM-FLAG-YES
           THEN
             MOVE 'ENE83'              TO AIF-MSG-CODE
             PERFORM 9999-MESSAGE-HANDLE-RTN
           END-IF.
           
      * 非主证件不能开户
           IF O-PRI-CER-FLG OF PCECCII1 = C-CM-FLAG-NO
           THEN
             MOVE 'ENE95'              TO AIF-MSG-CODE
             PERFORM 9999-MESSAGE-HANDLE-RTN
           END-IF           
           
      * 由于增加代理人信息-需要将开户人信息暂存
           MOVE PCECCII1               TO WK-PCECCII1.
           
      * 校验开户人未成年提示信息     
           IF SYS-TX-MODE NOT = C-CM-CENTERCUT-MODE AND 
              INM-CC-R-FLG NOT = C-CM-FLAG-YES
           THEN
              PERFORM 3700-CHK-CERT-RTN              
           END-IF.           

           
      * 证件超期不允许开户
           INITIALIZE                  PCECCII7.
           MOVE C-TX-TYP-ONLY-OPEN     TO I-TX-TYP    OF PCECCII7.
           MOVE TFT-CERT-TYP           TO I-CER-TYP   OF PCECCII7.
           MOVE TFT-CERT-ID            TO I-CER-NO    OF PCECCII7.
           MOVE TFT-CUST-NAME          TO I-FULL-NAM  OF PCECCII7.
           CALL 'CECCII7'              USING   AIF-AREA
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
           END-IF      
                                       
                                           
           
      * 法人内账户数量\介质数量控制
           IF SYS-SUB-TX-CODE = WK-SUB-TX-CODE-1060
           THEN
             PERFORM 9709-CALL-CECACNC-RTN
             PERFORM 9720-CALL-CECACND-RTN
           END-IF.
.      
           
      * 一个客户全省只能有一个财政补贴账户用途的账号
      * 非个人结算账户，不允许设定财政补贴用途
           IF TFT-USG-RE = C-CM-PRE-COD-0003
           THEN
              PERFORM 9700-CALL-CECCSA8-INQ-RTN
              IF O-SIGN-NUM OF PCECCSA8 > 0
              THEN
                 MOVE 'EN018'          TO AIF-MSG-CODE 
                 PERFORM 9999-MESSAGE-HANDLE-RTN
              END-IF
              IF TFT-ACCT-CHAR NOT = C-SA-STL-ACCT
              THEN
                 MOVE 'EN019'          TO AIF-MSG-CODE 
                 PERFORM 9999-MESSAGE-HANDLE-RTN
              END-IF
           END-IF.
           
           IF TFT-USG-RE NOT = SPACES  AND
              TFT-ACCT-FREE  = C-CM-FLAG-NO
           THEN 
             MOVE 'EN020'              TO AIF-MSG-CODE
             PERFORM 9999-MESSAGE-HANDLE-RTN
           END-IF.
           
       3700-CHK-CERT-RTN.   
           IF O-CUST-NO OF WK-PCECCII1 NOT = SPACE
           THEN
             INITIALIZE                PCCICIF1                                
             MOVE O-CUST-NO OF WK-PCECCII1
                                       TO I-CUST-NO OF PCCICIF1
                                       
             PERFORM 5431-CALL-PCCICIF1-RTN
             MOVE O-REG-BIRDY OF PCCICIF1
                                       TO WK-AGT-BIRDT
           ELSE
             MOVE TFT-AGT-CERT-ID(7:8) TO WK-AGT-BIRDT
             
           END-IF
           
           PERFORM 5432-CALL-CCMDAT1-RTN.           
           
      *小于注册出生日期
           IF IO-CMDATE-STA-DATE OF PCCMDAT1 <                                  
              WK-AGT-BIRDT                                         
           THEN                                                                 
             MOVE 'FN030'              TO AIF-MSG-CODE 
             PERFORM 9999-MESSAGE-HANDLE-RTN            
           END-IF. 
     
       5000-FUNCTION-PROCESS-RTN.
       
           PERFORM 5100-CALL-KSAACN1-RTN.
           PERFORM 5200-PROCESS-DB-RTN.           
           PERFORM 5300-PROCESS-USG-RTN.         
           PERFORM 5400-PROCESS-AGT-RTN.
           PERFORM 5500-PROCESS-AUH-RTN.
           PERFORM 5600-PROCESS-CHK-RTN.
    
           
       5100-CALL-KSAACN1-RTN.
           INITIALIZE                  PKSAACN1.
           IF TFT-PD-CODE  = WK-PDP-CODE
           THEN
             MOVE TFT-CURR-COD         TO I-CURR-COD        OF PKSAACN1
             MOVE TFT-CURR-IDEN        TO I-CURR-IDEN       OF PKSAACN1
             MOVE TFT-BUSN-LICE        TO I-BUSN-LICE       OF PKSAACN1
             MOVE TFT-SFEA-APPR-NO     TO I-SFEA-APPR-NO    OF PKSAACN1
             MOVE TFT-FC-LMT-TYPE      TO I-FC-LMT-TYPE     OF PKSAACN1
             MOVE TFT-FC-LMT-AMT       TO I-FC-LMT-AMT      OF PKSAACN1
             MOVE TFT-FX-ACCT-CHAR     TO I-FX-ACCT-CHAR    OF PKSAACN1
           END-IF.
           MOVE TFT-CUST-NAME          TO I-CUST-NAME       OF PKSAACN1.
           MOVE TFT-INTC-FLG           TO I-INTC-FLG        OF PKSAACN1.
           MOVE TFT-DRW-TYP            TO I-DRW-TYP         OF PKSAACN1.
        
           MOVE TFT-PRDS-INSTN-DPDW-FLG
                                       TO I-INTND-STN       OF PKSAACN1.
           MOVE TFT-ACCT-TYP           TO I-ACCT-TYP        OF PKSAACN1.
           MOVE TFT-PSBK-PRT-NO1       TO I-PSBK-PRT-NO1    OF PKSAACN1.
           IF SYS-TX-MODE = C-CM-ONLINE-MODE AND
              INM-CC-R-FLG NOT = C-CM-FLAG-YES
           THEN
             MOVE C-SA-HANDOUT-PSBK    TO I-PSBK-STS        OF PKSAACN1
           END-IF.
    
           IF SYS-TX-MODE = C-CM-CENTERCUT-MODE OR
              INM-CC-R-FLG = C-CM-FLAG-YES
           THEN
             MOVE C-SA-NOT-HANDOUT-PSBK
                                       TO I-PSBK-STS        OF PKSAACN1
           END-IF.
           
           MOVE TFT-PSWD               TO I-PSWD            OF PKSAACN1.
           MOVE TFT-CERT-TYP           TO I-CERT-TYP        OF PKSAACN1.
           MOVE TFT-CERT-ID            TO I-CERT-ID         OF PKSAACN1.
           MOVE C-SA-PSBK-ACCT         TO I-CRD-FLG         OF PKSAACN1.
           MOVE 0                      TO I-MAX-MHD         OF PKSAACN1.
           MOVE O-CUST-NO OF WK-PCECCII1
                                       TO I-CUST-NO         OF PKSAACN1.                   
           MOVE C-SA-LESS-INTR-FLG-Y   TO I-LESS-INTR-FLG   OF PKSAACN1.
           MOVE TFT-ACCT-NO            TO IO-ACCT-NO        OF PKSAACN1.
           MOVE TFT-PD-CODE            TO I-PD-CODE         OF PKSAACN1.
           MOVE TFT-DPDW-RANG          TO I-DPDW-RANG       OF PKSAACN1.
           MOVE TFT-ACCT-CHAR          TO I-ACCT-CHAR       OF PKSAACN1.
           MOVE SYS-LEGAL-PERSON-ID    TO I-LEGAL-PERSON-ID OF PKSAACN1.                    
           MOVE TFT-MEDIUM-TYPE        TO I-MEDIUM-TYPE     OF PKSAACN1. 
           MOVE TFT-ACCT-KIND          TO I-ACCT-KIND       OF PKSAACN1.
           MOVE TFT-ACCT-FREE          TO I-MAFE-FLG        OF PKSAACN1.
           MOVE TFT-ACCT-VALIDITY      TO I-ACCT-VALIDITY   OF PKSAACN1.
    
           IF TFT-ACCT-INFO NOT = SPACES 
      *        AND WK-ACCT-NO    NOT = SPACES
           THEN 
             MOVE TFT-ACCT-INFO        TO I-ACCT-INFO       OF PKSAACN1
           END-IF.
    
           MOVE SYS-LCH-CHANNEL-FLAG   TO I-OPAC-CHAL       OF PKSAACN1.
    
           IF SYS-LCH-CHANNEL-FLAG = C-CM-CHANNEL-TLR
           THEN
             MOVE C-SA-ACCT-CHK-Y      TO I-ACCT-CHK        OF PKSAACN1
           END-IF.

      * 单笔代理开户业务紧急优化项目,将是否代理栏位传入PKSAACN1,
      * 代理开户时不作简单密码校验，密码状态置为"密码补设待生效":
           MOVE TFT-AGT-FLG            TO I-AGT-FLG         OF PKSAACN1.
           CALL 'GSYSTRIG'             USING APA-AREA
                                             PKSAACN1
                                             CONTENT 'KSAACN1 '.
           PERFORM 9999-MESSAGE-HANDLE-RTN.
           MOVE IO-ACCT-NO OF PKSAACN1 TO WK-ACCT-NO.
    
      * 维护客户账号档、客户账号与系统账号关系档、介质档、介质与账号关系档
       5200-PROCESS-DB-RTN.
          
           PERFORM 9702-CALL-CCICLTA-RTN.
           
           PERFORM 9703-CALL-CCIRAA0-RTN.

           PERFORM 9704-CALL-CSAPME0-RTN.
          
           PERFORM 9705-CALL-CSARMA0-RTN. 
       
       5300-PROCESS-USG-RTN.
      * 增加账户用途签约逻辑
           IF TFT-USG-RE NOT = SPACE
           THEN
             PERFORM 9701-CALL-CECCSA8-ADD-RTN
           END-IF.
              
       5400-PROCESS-AGT-RTN.    
           IF TFT-AGT-FLG = C-CM-FLAG-YES AND
              WK-ACCT-NO NOT = SPACES
           THEN
             PERFORM 5420-CALL-CECCII1-RTN
      *代理标志选择是校验代理开户跟被代理开户                  
             IF SYS-TX-MODE NOT = C-CM-CENTERCUT-MODE AND 
                INM-CC-R-FLG NOT = C-CM-FLAG-YES
             THEN
               PERFORM 9810-CALL-CECACNC-RTN
               PERFORM 5430-COMP-AGE-RTN
             END-IF
             PERFORM 5410-CALL-CECAGT0-RTN                        
             PERFORM 5440-CALL-GCCBAPAT-RTN
           END-IF.               
             
      * 插入代理人信息
       5410-CALL-CECAGT0-RTN.
       
           INITIALIZE                  PCECAGT0.
           MOVE WK-FUN-ADD             TO I-FUN-CODE      OF PCECAGT0.
           MOVE WK-ACCT-NO             TO I-ACCT-NO       OF PCECAGT0.
           MOVE WK-OPAC-FLAG           TO I-OPAC-CANL-FLG OF PCECAGT0.        
           MOVE TFT-AGT-NAME           TO IO-AGT-NAME     OF PCECAGT0.
           MOVE C-CI-ACCN-TYP-SA       TO IO-ACCN-TYP     OF PCECAGT0.
           MOVE TFT-AGT-CERT-TYP       TO IO-AGT-CER-TYP  OF PCECAGT0.
           MOVE TFT-AGT-CERT-ID        TO IO-AGT-CERT-ID  OF PCECAGT0.
           MOVE TFT-NATY-COD           TO IO-NATY-COD     OF PCECAGT0.
           MOVE TFT-AGT-TEL            TO IO-AGT-TEL      OF PCECAGT0.
           CALL 'CECAGT0'              USING AIF-AREA
                                             PCECAGT0
                                             SYS-AREA.
           PERFORM 9999-MESSAGE-HANDLE-RTN.
      
           IF O-MSG-TYPE OF PCECAGT0 NOT = SPACE
           THEN
             MOVE O-RTRN-CODE OF PCECAGT0
                                       TO AIF-MSG-CODE
             PERFORM 9999-MESSAGE-HANDLE-RTN
           END-IF. 
    
           
      * 取得代理人的客户编号,由于存在未登记核心的代理人信息故该CF不能报错
       5420-CALL-CECCII1-RTN.
           INITIALIZE                  PCECCII1.
           MOVE TFT-AGT-CERT-TYP       TO I-CER-TYP  OF PCECCII1.
           MOVE TFT-AGT-CERT-ID        TO I-CER-NO   OF PCECCII1.
           MOVE TFT-AGT-NAME           TO I-FULL-NAM OF PCECCII1.
           CALL 'CECCII1' USING        AIF-AREA
                                       PCECCII1
                                       SYS-AREA.
           PERFORM 9999-MESSAGE-HANDLE-RTN.
      
           IF O-RTRN-CODE OF PCECCII1 NOT = SPACES 
           THEN
             MOVE SPACES               TO O-CUST-NO OF PCECCII1
           END-IF.

      *
       5430-COMP-AGE-RTN.
           IF O-CUST-NO OF PCECCII1 NOT = SPACE
           THEN
             INITIALIZE                PCCICIF1                                
             MOVE O-CUST-NO OF PCECCII1
                                       TO I-CUST-NO OF PCCICIF1 
             PERFORM 5431-CALL-PCCICIF1-RTN
             MOVE O-REG-BIRDY OF PCCICIF1
                                       TO WK-AGT-BIRDT
           ELSE
             MOVE TFT-AGT-CERT-ID(7:8) TO WK-AGT-BIRDT
           END-IF
           
           PERFORM 5432-CALL-CCMDAT1-RTN.           
           
      *小于注册出生日期
           IF IO-CMDATE-STA-DATE OF PCCMDAT1 <                                  
              WK-AGT-BIRDT                                         
           THEN                                                                 
             MOVE 'FN028'              TO AIF-MSG-CODE 
             PERFORM 9999-MESSAGE-HANDLE-RTN            
           END-IF. 
       5431-CALL-PCCICIF1-RTN.    
               
           CALL 'CCICIF1' USING AIF-AREA                                        
                                PCCICIF1                                        
                                SYS-AREA.                                       
           PERFORM 9999-MESSAGE-HANDLE-RTN. 
            
       5432-CALL-CCMDAT1-RTN.
                                    
           INITIALIZE                  PCCMDAT1.                                
           MOVE C-CM-DT-EDTOSD-MO      TO I-CMDATE-FUNC OF PCCMDAT1.            
           MOVE SYS-BUSN-DT            TO                                       
                                       IO-CMDATE-END-DATE OF PCCMDAT1.          
           MOVE '192'                  TO                                       
                                       IO-CMDATE-PERIOD-DDD OF PCCMDAT1.        
           CALL 'CCMDAT1' USING AIF-AREA                                        
                                PCCMDAT1                                        
                                SYS-AREA.                                       
           PERFORM 9999-MESSAGE-HANDLE-RTN.                     

       5440-CALL-GCCBAPAT-RTN.
           INITIALIZE                  PCCBAPAT.
           MOVE TFT-AGT-CERT-TYP       TO I-CER-TYP  OF PCCBAPAT.           
           MOVE TFT-AGT-NAME           TO I-FULL-NAM OF PCCBAPAT.
           MOVE TFT-AGT-CERT-ID        TO I-CER-NO   OF PCCBAPAT.
           MOVE TFT-NATY-COD           TO I-RESCNTY  OF PCCBAPAT.
           
           
           CALL 'GCCBAPAT'             USING  APA-AREA
                                              PCCBAPAT
           PERFORM 9999-MESSAGE-HANDLE-RTN.            
                 
    
      * 开户数超限/代理人代理开户数超限/被代理开户数超限登记      
       5500-PROCESS-AUH-RTN.
           IF TFT-REASON-AUTHORY(1:1)  = C-CM-VALID OR
              TFT-REASON-AUTHORY(2:1)  = C-CM-VALID OR
              TFT-REASON-AUTHORY(3:1)  = C-CM-VALID OR
              TFT-REASON-AUTHORY(4:1)  = C-CM-VALID OR
              TFT-REASON-AUTHORY(5:1)  = C-CM-VALID OR
              TFT-REASON-AUTHORY(6:1)  = C-CM-VALID OR
              TFT-REASON-AUTHORY(7:1)  = C-CM-VALID OR
              TFT-REASON-AUTHORY(8:1)  = C-CM-VALID OR
              TFT-REASON-AUTHORY(9:1)  = C-CM-VALID OR
              TFT-REASON-AUTHORY(10:1) = C-CM-VALID OR
              TFT-REASON-AUTHORY(11:1) = C-CM-VALID OR
              TFT-REASON-AUTHORY(12:1) = C-CM-VALID OR
              TFT-REASON-AUTHORY(13:1) = C-CM-VALID OR
              TFT-REASON-AUTHORY(14:1) = C-CM-VALID OR
              TFT-REASON-AUTHORY(15:1) = C-CM-VALID               
           THEN
             PERFORM 5510-ADD-CMAUHAUH-RTN
           END-IF.       
     
      * 开户数超限/代理人代理开户数超限/被代理开户数超限登记
       5510-ADD-CMAUHAUH-RTN.
       
           INITIALIZE                  PCCMAUH1.
           MOVE WK-FUN-ADD             TO I-FUN-CODE        OF PCCMAUH1.
           MOVE WK-ACCT-NO             TO I-ACCT-NO         OF PCCMAUH1.
           MOVE SYS-LEGAL-PERSON-ID    TO IO-LEGAL-PERSON-ID OF PCCMAUH1.
           MOVE SYS-PARTITION-ID       TO IO-DB-PARTITION-ID OF PCCMAUH1.
           MOVE O-CUST-NO OF WK-PCECCII1
                                       TO IO-CUST-NO        OF PCCMAUH1.
           MOVE TFT-CUST-NAME          TO IO-CUST-NAME      OF PCCMAUH1.
           IF TFT-REASON-AUTHORY(2:1) =  C-CM-VALID
           THEN
             MOVE O-CUST-NO OF PCECCII1
                                       TO IO-AGT-CUST-NO    OF PCCMAUH1
             MOVE TFT-AGT-NAME         TO IO-AGT-CUST-NAME  OF PCCMAUH1
           END-IF.
           MOVE TFT-REASON-AUTHORY(1:1)    
                                       TO IO-AUH-REASON OF PCCMAUH1(1).
           MOVE TFT-REASON-AUTHORY(2:1)   
                                       TO IO-AUH-REASON OF PCCMAUH1(2).
           MOVE TFT-REASON-AUTHORY(3:1)  
                                       TO IO-AUH-REASON OF PCCMAUH1(3).
           MOVE TFT-REASON-AUTHORY(4:1)   
                                       TO IO-AUH-REASON OF PCCMAUH1(4).
           MOVE TFT-REASON-AUTHORY(5:1)   
                                       TO IO-AUH-REASON OF PCCMAUH1(5).
           MOVE TFT-REASON-AUTHORY(6:1)   
                                       TO IO-AUH-REASON OF PCCMAUH1(6).
           MOVE TFT-REASON-AUTHORY(7:1)   
                                       TO IO-AUH-REASON OF PCCMAUH1(7).
           MOVE TFT-REASON-AUTHORY(8:1)   
                                       TO IO-AUH-REASON OF PCCMAUH1(8).
           MOVE TFT-REASON-AUTHORY(9:1)   
                                       TO IO-AUH-REASON OF PCCMAUH1(9).
           MOVE TFT-REASON-AUTHORY(10:1) 
                                       TO IO-AUH-REASON OF PCCMAUH1(10).
           MOVE TFT-REASON-AUTHORY(11:1)  
                                       TO IO-AUH-REASON OF PCCMAUH1(11).
           MOVE TFT-REASON-AUTHORY(12:1)  
                                       TO IO-AUH-REASON OF PCCMAUH1(12).
           MOVE TFT-REASON-AUTHORY(13:1) 
                                       TO IO-AUH-REASON OF PCCMAUH1(13).
           MOVE TFT-REASON-AUTHORY(14:1) 
                                       TO IO-AUH-REASON OF PCCMAUH1(14).
           MOVE TFT-REASON-AUTHORY(15:1) 
                                       TO IO-AUH-REASON OF PCCMAUH1(15).

           MOVE SYS-BRANCH-STD         TO IO-CRT-ORG        OF PCCMAUH1.
           MOVE SYS-TELLER-ID          TO IO-CRT-TLR        OF PCCMAUH1.
           MOVE SYS-BUSN-DT            TO IO-CRT-DT         OF PCCMAUH1.
           MOVE SYS-BRANCH-STD         TO IO-LST-UPD-ORG    OF PCCMAUH1.
           MOVE SYS-TELLER-ID          TO IO-LST-UPD-TLR    OF PCCMAUH1.
           MOVE SYS-BUSN-DT            TO IO-LST-UPD-DT     OF PCCMAUH1.
           CALL 'CCMAUH1'              USING AIF-AREA
                                             PCCMAUH1
                                             SYS-AREA.
           PERFORM 9999-MESSAGE-HANDLE-RTN.
      
           IF O-MSG-TYPE OF PCCMAUH1 NOT = SPACE
           THEN
             MOVE O-RTRN-CODE OF PCCMAUH1
                                       TO AIF-MSG-CODE
             PERFORM 9999-MESSAGE-HANDLE-RTN
           END-IF.     
           
       5600-PROCESS-CHK-RTN.
           INITIALIZE                  PCCMCHK1.
           MOVE C-CM-FUN-ADD           TO I-FUN-CODE OF PCCMCHK1.
           MOVE WK-ACCT-NO             TO CM-ACCT-NO  OF IO-CMCERCHK 
                                                      OF PCCMCHK1.
           MOVE SYS-LEGAL-PERSON-ID    TO CM-LEGAL-PERSON-ID 
                                                      OF IO-CMCERCHK
                                                      OF PCCMCHK1. 
           MOVE SYS-PARTITION-ID       TO CM-DB-PARTITION-ID   
                                                      OF IO-CMCERCHK
                                                      OF PCCMCHK1.  
           MOVE '11'                   TO CM-ACCT-TYP OF IO-CMCERCHK
                                                      OF PCCMCHK1.
           MOVE O-CUST-NO OF WK-PCECCII1 
                                       TO CM-CUST-ID  OF IO-CMCERCHK
                                                      OF PCCMCHK1.
           MOVE '02'                   TO CM-CHK-RSLT OF IO-CMCERCHK 
                                                      OF PCCMCHK1.  
           MOVE SPACES                 TO CM-RSLT-RSN OF IO-CMCERCHK 
                                                      OF PCCMCHK1.   
           MOVE '10000000000000000000' TO CM-DISP-TYP OF IO-CMCERCHK
                                                      OF PCCMCHK1. 
           MOVE SYS-BRANCH-STD         TO CM-OPUN-COD OF IO-CMCERCHK                                                      
                                                      OF PCCMCHK1. 
           CALL 'CCMCHK1'              USING AIF-AREA                                                                 
                                             PCCMCHK1                                                                 
                                             SYS-AREA.                                                                
           PERFORM 9999-MESSAGE-HANDLE-RTN.                                                                           
                                                                                                                      
           IF O-MSG-TYPE OF PCCMCHK1 NOT = SPACE                                                                      
           THEN                                                                                                       
             MOVE O-RTRN-CODE OF PCCMCHK1                                                                             
                                       TO AIF-MSG-CODE                                                                
             PERFORM 9999-MESSAGE-HANDLE-RTN                                                                          
           END-IF.                                                                                                    
                                                      
                                                        
       7000-OUTPUT-PROCESS-RTN.    
                     
           PERFORM 7100-GEN-FORM-SSA10600-RTN.
           IF SYS-SUB-TX-CODE = WK-SUB-TX-CODE-1060
           THEN
             PERFORM 7200-GEN-FORM-PZZZ1120-RTN
           END-IF.
           IF SYS-SUB-TX-CODE = WK-SUB-TX-CODE-1030
           THEN
             PERFORM 7200-GEN-FORM-PSA10300-RTN
           END-IF.
           PERFORM 7300-GEN-FORM-PZZC0211-RTN. 
           PERFORM 7400-GEN-FORM-MZZC0212-RTN.  
      
       7100-GEN-FORM-SSA10600-RTN.
           MOVE 'SSA10600'             TO SSA10600-FORMID.
           MOVE O-CUST-NO OF WK-PCECCII1
                                       TO SA-CUST-NO   OF SSA10600.
           MOVE IO-ACCT-NO OF PKSAACN1 TO SA-ACCT-NO   OF SSA10600.
           MOVE TFT-CUST-NAME          TO SA-CUST-NAME OF SSA10600.
           MOVE SYS-BUSN-DT            TO SA-OPAC-DT   OF SSA10600.
     
       7200-GEN-FORM-PZZZ1120-RTN.  
           MOVE O-PZZZ1120 OF PKSAACN1 TO PZZZ1120.      
           MOVE TFT-USG-RE             TO FM-ACCT-USE      OF PZZZ1120.
           MOVE SPACES                 TO FM-CURR-COD      OF PZZZ1120.
           MOVE TFT-AGT-CERT-TYP       TO FM-AGT-CERT-TYPE OF PZZZ1120.
           MOVE TFT-AGT-CERT-ID        TO FM-AGT-CERT-NO   OF PZZZ1120.
           MOVE TFT-AGT-NAME           TO FM-AGT-NAME      OF PZZZ1120.
           MOVE TFT-AGT-TEL            TO FM-AGT-TEL       OF PZZZ1120.
           MOVE TFT-ACCT-VALIDITY      TO FM-ACCT-VALIDITY OF PZZZ1120. 
           MOVE TFT-ACCT-FREE          TO FM-ACCT-FREE     OF PZZZ1120.           
             
       7200-GEN-FORM-PSA10300-RTN.
           MOVE 'PSA10300'             TO PSA10300-FORMID  OF PSA10300.
           MOVE IO-ACCT-NO OF PKSAACN1 
                                       TO FM-ACCT-NO       OF PSA10300.
           MOVE O-CUST-NO OF WK-PCECCII1
                                       TO FM-CUST-NO       OF PSA10300.
           MOVE SYS-BRANCH-STD         TO FM-OPAC-INSTN-NO OF PSA10300.
           MOVE SYS-TELLER-ID          TO FM-OPR-NO        OF PSA10300.                            
           MOVE TFT-DRW-TYP            TO FM-DRW-TYP       OF PSA10300.
           IF SYS-TX-TYP = C-CLG-TXT-EC
           THEN
               STRING C-CM-EC-CHAR, SYS-EC-LOG-NO
                   DELIMITED BY SIZE INTO FM-RMRK          OF PSA10300
           END-IF.
           MOVE SYS-TX-LOG-NO          TO FM-TX-LOG-NO     OF PSA10300.
           MOVE TFT-CUST-NAME          TO FM-CUST-NAME     OF PSA10300.
           MOVE SYS-BUSN-DT            TO FM-OPAC-DT       OF PSA10300.
           MOVE TFT-PRDS-INSTN-DPDW-FLG  
                                       TO FM-FBDP-FBDW     OF PSA10300.
           MOVE TFT-PSBK-PRT-NO1       TO FM-DOC-NO        OF PSA10300.
           IF SYS-SPV-B NOT = SPACES
           THEN
             MOVE SYS-SPV-B            TO FM-SPV-TLR       OF PSA10300 
           END-IF.
           IF SYS-SPV-A NOT = SPACES
           THEN
             MOVE SYS-SPV-A            TO FM-SPV-TLR       OF PSA10300
           END-IF.
           MOVE TFT-CURR-COD           TO FM-CURR-COD      OF PSA10300.
           MOVE TFT-CURR-IDEN          TO FM-CURR-IDEN     OF PSA10300.
           STRING SYS-BUSN-DT, SYS-CPU-TM1
              DELIMITED BY SIZE INTO  FM-OPAC-TM           OF PSA10300.
           MOVE FM-CERT-TYP OF O-PZZZ1120 
                                       TO FM-CERT-TYP      OF PSA10300.
           MOVE FM-CERT-NO OF O-PZZZ1120
                                       TO FM-CERT-NO       OF PSA10300.
           MOVE FM-TEL OF O-PZZZ1120   TO FM-TEL           OF PSA10300.
           MOVE FM-AGT-CERT-TYPE OF O-PZZZ1120
                                       TO FM-AGT-CERT-TYPE OF PSA10300.
           MOVE FM-AGT-CERT-NO OF O-PZZZ1120
                                       TO FM-AGT-CERT-NO   OF PSA10300.
           MOVE FM-AGT-NAME OF O-PZZZ1120
                                       TO FM-AGT-NAME      OF PSA10300.
           MOVE FM-AGT-TEL OF O-PZZZ1120
                                       TO FM-AGT-TEL       OF PSA10300.
           MOVE TFT-PD-CODE            TO FM-PD-CODE       OF PSA10300.
           MOVE TFT-FX-ACCT-CHAR       TO FM-FX-ACCT-CHAR  OF PSA10300.
           MOVE TFT-MEDIUM-TYPE        TO FM-MEDIUM-TYPE   OF PSA10300.
           MOVE TFT-BUSN-LICE          TO FM-BUSN-LICE     OF PSA10300.
           MOVE TFT-SFEA-APPR-NO       TO FM-SFEA-APPR-NO  OF PSA10300.
           MOVE TFT-ACCT-FREE          TO FM-ACCT-FREE     OF PSA10300.
           MOVE TFT-FC-LMT-TYPE        TO FM-FC-LMT-TYPE   OF PSA10300.
           MOVE TFT-FC-LMT-AMT         TO FM-FC-LMT-AMT    OF PSA10300.
           MOVE TFT-ACCT-VALIDITY      TO FM-ACCT-VALIDITY OF PSA10300.

       7300-GEN-FORM-PZZC0211-RTN.     
           MOVE O-PZZC0211 OF PKSAACN1 TO PZZC0211.  
           IF TFT-USG-RE = SPACES
           THEN 
             CONTINUE
           ELSE    
             EVALUATE TFT-USG-RE
                WHEN C-CM-PRE-COD-0001
                   MOVE C-CM-PRE-COD-NAME-0001
                                       TO WK-ACCT-USE(1:10)
                WHEN C-CM-PRE-COD-0002
                   MOVE C-CM-PRE-COD-NAME-0002
                                       TO WK-ACCT-USE(13:6)
                WHEN C-CM-PRE-COD-0003
                   MOVE C-CM-PRE-COD-NAME-0003
                                       TO WK-ACCT-USE(21:8)
                WHEN OTHER 
                   MOVE C-CM-PRE-COD-NAME-OTHER
                                       TO WK-ACCT-USE(31:8)
             END-EVALUATE
           END-IF.          
           MOVE WK-ACCT-USE            TO FM-ACCT-USE OF PZZC0211.
           
       7400-GEN-FORM-MZZC0212-RTN.
           IF SYS-TX-TYP = C-CLG-TXT-NOR
           THEN
             MOVE O-MZZC0212 OF PKSAACN1 
                                       TO MZZC0212
           END-IF.
           
       9000-TXN-END-RTN.
      
           MOVE IO-ACCT-NO OF PKSAACN1 TO AIF-ACCOUNT-NO.
           MOVE LENGTH OF SSA10600     TO SSA10600-LL OF SSA10600.
           MOVE LENGTH OF PSA10300     TO PSA10300-LL OF PSA10300.
           MOVE LENGTH OF PZZZ1120     TO PZZZ1120-LL OF PZZZ1120.
           MOVE LENGTH OF PZZC0211     TO PZZC0211-LL OF PZZC0211.
           MOVE LENGTH OF MZZC0212     TO MZZC0212-LL OF MZZC0212. 
           MOVE LENGTH OF FLAST        TO FFFFFFFF-LL.
           MOVE 'FFFFFFFF'             TO FFFFFFFF-FORMID.
           MOVE SPACE                  TO FFFFFFFF-FILLER.
           CALL 'GSYSMOTR' USING AIF-AREA
                                 CONTENT 'EGSA01060'.
                                                                
      
      * ****************************************************************                           
      *    调用CF"账户用途签约信息维护CECCSA8"，进行账户用途签约查询.
      * ****************************************************************
       9700-CALL-CECCSA8-INQ-RTN.
           INITIALIZE                  PCECCSA8.
           MOVE C-CM-FUN-INQUIRE       TO I-FUN-CODE OF PCECCSA8.
           MOVE TFT-USG-RE             TO I-CSA-TYP  OF PCECCSA8.
           MOVE O-CUST-NO OF PCECCII1  TO IO-CUST-NO OF PCECCSA8
           CALL 'CECCSA8' USING AIF-AREA
                                PCECCSA8
                                SYS-AREA.
           PERFORM 9999-MESSAGE-HANDLE-RTN.
      
           IF O-MSG-TYPE OF PCECCSA8 NOT = SPACE
           THEN
             MOVE O-RTRN-CODE OF PCECCSA8
                                       TO AIF-MSG-CODE
             PERFORM 9999-MESSAGE-HANDLE-RTN
           END-IF.           
       
      * ****************************************************************    
      *    调用CF"账户用途签约信息维护CECCSA8"，进行账户用途签约.
      * ****************************************************************
       9701-CALL-CECCSA8-ADD-RTN.
           INITIALIZE                  PCECCSA8.
           MOVE C-CM-FUN-ADD           TO I-FUN-CODE OF PCECCSA8.
           MOVE IO-ACCT-NO OF PKSAACN1
                                       TO I-ACCT-NO  OF PCECCSA8.
           MOVE TFT-USG-RE             TO I-CSA-TYP  OF PCECCSA8.
           MOVE O-CUST-NO OF PCECCII1
                                       TO IO-CUST-NO OF PCECCSA8.
           CALL 'CECCSA8' USING AIF-AREA
                                PCECCSA8
                                SYS-AREA.
           PERFORM 9999-MESSAGE-HANDLE-RTN.
      
           IF O-MSG-TYPE OF PCECCSA8 NOT = SPACE
           THEN
             MOVE O-RTRN-CODE OF PCECCSA8
                                       TO AIF-MSG-CODE
             PERFORM 9999-MESSAGE-HANDLE-RTN
           END-IF. 
           
      * ****************************************************************
      * 登记客户账号档
      * ****************************************************************
       9702-CALL-CCICLTA-RTN.
           INITIALIZE                  PCCICLTA.
           MOVE C-SA-ADD               TO I-FUN-CODE         OF PCCICLTA
           MOVE IO-ACCT-NO   OF PKSAACN1
                                       TO I-CI-ACCT-NO       OF PCCICLTA
           MOVE SYS-BRANCH-STD         TO I-CI-OPAC-INSTN-NO OF PCCICLTA
           MOVE SYS-BUSN-DT            TO I-CI-OPAC-DT       OF PCCICLTA
           MOVE I-CERT-TYP OF PKSAACN1 TO I-CI-CERT-TYP      OF PCCICLTA
           MOVE I-CERT-ID OF PKSAACN1  TO I-CI-CERT-NO       OF PCCICLTA
           MOVE I-CUST-NO OF PKSAACN1  TO I-CI-CUST-NO       OF PCCICLTA
           MOVE I-CUST-NAME OF PKSAACN1 
                                       TO I-CI-CUST-NAME     OF PCCICLTA
           MOVE I-CUST-NAME OF PKSAACN1
                                       TO I-CI-ACCT-NAME     OF PCCICLTA
           MOVE I-OPAC-CHAL OF PKSAACN1
                                       TO I-CI-OPAC-CHAL     OF PCCICLTA
           MOVE WK-PRI-FLG             TO I-CI-CUST-TYP      OF PCCICLTA
           MOVE I-ACCT-CHK OF PKSAACN1 TO I-CI-ACCT-CHK      OF PCCICLTA
           MOVE I-ACCT-KIND OF PKSAACN1         
                                       TO I-CI-ACCT-TYP      OF PCCICLTA
           MOVE WK-NORMAL              TO I-CI-ACCT-STS      OF PCCICLTA  
           
           CALL 'CCICLTA'              USING AIF-AREA
                                             PCCICLTA
                                             SYS-AREA.
           PERFORM 9999-MESSAGE-HANDLE-RTN.
           IF O-MSG-TYPE OF PCCICLTA NOT = SPACES
           THEN
             MOVE O-RTRN-CODE OF PCCICLTA
                                       TO AIF-MSG-CODE
             PERFORM 9999-MESSAGE-HANDLE-RTN
           END-IF. 
    
      * ****************************************************************
      * 登记客户账号与系统账号关系档
      * ****************************************************************
       9703-CALL-CCIRAA0-RTN.
           INITIALIZE                  PCCIRAA0.
           MOVE C-SA-ADD               TO I-FUN-CODE        OF PCCIRAA0.
           MOVE IO-ACCT-NO OF PKSAACN1 TO I-ACCT-NO         OF PCCIRAA0
                                          I-SACCT-NO        OF PCCIRAA0.       
           CALL 'CCIRAA0'              USING AIF-AREA
                                             PCCIRAA0
                                             SYS-AREA.
           PERFORM 9999-MESSAGE-HANDLE-RTN.
           IF O-MSG-TYPE OF PCCIRAA0 NOT = SPACES
           THEN
             MOVE O-RTRN-CODE OF PCCIRAA0
                                       TO AIF-MSG-CODE
             PERFORM 9999-MESSAGE-HANDLE-RTN
           END-IF.      
                  
           
      * ****************************************************************
      * 插入记录到介质档 
      * ****************************************************************
       9704-CALL-CSAPME0-RTN.
           INITIALIZE                  PCSAPME0.
           MOVE  C-SA-ADD              TO I-FUN-CODE        OF PCSAPME0.
           MOVE  SYS-LEGAL-PERSON-ID   TO I-LEGAL-PERSON-ID OF PCSAPME0.                
           MOVE  TFT-MEDIUM-TYPE       TO I-DOC-TYP         OF PCSAPME0.                           	               
           MOVE  '001'                 TO I-PSBK-NO         OF PCSAPME0.
           MOVE 1                      TO I-PAGESUM-N       OF PCSAPME0. 
           MOVE 0                      TO I-PGLN-TOTL-N     OF PCSAPME0.
           IF SYS-TX-MODE = C-CM-ONLINE-MODE AND
              INM-CC-R-FLG NOT = C-CM-FLAG-YES
           THEN
             MOVE  TFT-PSBK-PRT-NO1    TO I-MEDM-NO         OF PCSAPME0       	                             
             MOVE  C-SA-HANDOUT-PSBK   TO I-PSBK-STS        OF PCSAPME0
           END-IF.
           IF SYS-TX-MODE = C-CM-CENTERCUT-MODE OR 
              INM-CC-R-FLG = C-CM-FLAG-YES
           THEN
             MOVE IO-ACCT-NO OF PKSAACN1
                                       TO I-MEDM-NO         OF PCSAPME0
             MOVE C-SA-NOT-HANDOUT-PSBK
                                       TO I-PSBK-STS        OF PCSAPME0
           END-IF.

           IF TFT-PSWD NOT = SPACES AND
              ( TFT-DRW-TYP(4:1) = C-SA-DRW-BY-PWD OR   
                TFT-DRW-TYP(4:1) = C-SA-DRW-BY-FGR-AND-PWD OR    
                TFT-DRW-TYP(4:1) = C-SA-DRW-BY-FGR-OR-PWD )
           THEN 
 
             IF SYS-TX-MODE = C-CM-CENTERCUT-MODE
             THEN
               MOVE C-SA-PWD-PRE-APPROVE
                                       TO I-PWD-STS OF PCSAPME0
             ELSE            
               IF I-AGT-FLG OF PKSAACN1 = C-CM-FLAG-NO
               THEN
                 
                 PERFORM 9704-CALL-CCMLST1-RTN
                 PERFORM 9705-CALL-CCMENC3-6-RTN 
                 PERFORM 9706-CALL-CCMENC3-4-RTN
                 MOVE C-SA-PWD         TO I-PWD-STS    OF PCSAPME0
               ELSE
                 PERFORM 9707-CALL-CCMENC3-3-RTN
                 MOVE C-SA-PWD-PRE-APPROVE
                                       TO I-PWD-STS    OF PCSAPME0             
               END-IF               
               
               IF O-F21-OFFSET OF PCCMENC3 = SPACES
               THEN
                 MOVE 'EN051'          TO AIF-MSG-CODE
                 PERFORM 9999-MESSAGE-HANDLE-RTN
               END-IF
               IF O-F21-PVK OF PCCMENC3 = SPACES
               THEN
                 MOVE 'EN052'          TO AIF-MSG-CODE
                 PERFORM 9999-MESSAGE-HANDLE-RTN
               END-IF
               MOVE O-F21-OFFSET OF PCCMENC3
                                       TO I-CRPT-PIN   OF PCSAPME0
               MOVE O-F21-PVK OF PCCMENC3
                                       TO I-ENCKEY-VER OF PCSAPME0
               MOVE '3'                TO I-ENC-TYP    OF PCSAPME0
           ELSE
             IF SYS-TX-TYP NOT = C-CLG-TXT-EC AND
                TFT-PSWD NOT = SPACES AND
              ( TFT-DRW-TYP(4:1) = C-SA-DRW-BY-PWD OR   
                TFT-DRW-TYP(4:1) = C-SA-DRW-BY-FGR-AND-PWD OR    
                TFT-DRW-TYP(4:1) = C-SA-DRW-BY-FGR-OR-PWD )  
             THEN
               IF SYS-TX-MODE = C-CM-CENTERCUT-MODE
               THEN
                 MOVE C-SA-PWD-PRE-APPROVE
                                       TO I-PWD-STS   OF PCSAPME0
               ELSE
                 MOVE C-SA-PWD         TO I-PWD-STS   OF PCSAPME0
               END-IF             
             
               PERFORM 9707-CALL-CCMENC3-3-RTN
               IF O-F21-OFFSET OF PCCMENC3 = SPACES
               THEN
                 MOVE 'EN051'            TO AIF-MSG-CODE
                 PERFORM 9999-MESSAGE-HANDLE-RTN
               END-IF
               IF O-F21-PVK OF PCCMENC3 = SPACES
               THEN
                 MOVE 'EN052'            TO AIF-MSG-CODE
                 PERFORM 9999-MESSAGE-HANDLE-RTN
               END-IF
      
               MOVE O-F21-OFFSET OF PCCMENC3
                                       TO I-CRPT-PIN    OF PCSAPME0
               MOVE O-F21-PVK OF PCCMENC3
                                       TO I-ENCKEY-VER  OF PCSAPME0
               MOVE '3'                TO I-ENC-TYP     OF PCSAPME0  
               MOVE C-SA-PWD-PRE-APPROVE  
                                       TO I-PWD-STS     OF PCSAPME0  
             ELSE

               MOVE C-SA-NON-PWD       TO I-PWD-STS     OF PCSAPME0                                                     
             END-IF
           END-IF.
           CALL 'CSAPME0'              USING  AIF-AREA
                                              PCSAPME0
                                              SYS-AREA.
           PERFORM 9999-MESSAGE-HANDLE-RTN.
           IF O-MSG-TYPE OF PCSAPME0 NOT = SPACES
           THEN
             MOVE O-RTRN-CODE OF PCSAPME0
                                       TO AIF-MSG-CODE
             PERFORM 9999-MESSAGE-HANDLE-RTN
           END-IF.  
    
      * ****************************************************************
      * 维护介质与账号关系档
      * ****************************************************************                                  
       9705-CALL-CSARMA0-RTN.
           INITIALIZE                  PCSARMA0.
           MOVE C-SA-ADD               TO I-FUN-CODE        OF PCSARMA0.
           MOVE SYS-LEGAL-PERSON-ID    TO I-LEGAL-PERSON-ID OF PCSARMA0.
           MOVE SYS-PARTITION-ID       TO I-DB-PARTITION-ID OF PCSARMA0.
           IF SYS-TX-MODE = C-CM-ONLINE-MODE AND
              INM-CC-R-FLG NOT = C-CM-FLAG-YES
           THEN
             MOVE  TFT-PSBK-PRT-NO1    TO I-PSBK-PRT-NO     OF PCSARMA0
           END-IF.
           IF SYS-TX-MODE = C-CM-CENTERCUT-MODE OR 
              INM-CC-R-FLG = C-CM-FLAG-YES
           THEN
             MOVE IO-ACCT-NO OF PKSAACN1             
                                       TO I-PSBK-PRT-NO     OF PCSARMA0
           END-IF. 
           MOVE TFT-MEDIUM-TYPE        TO I-DOC-TYP         OF PCSARMA0.          	     	
           MOVE IO-ACCT-NO OF PKSAACN1 TO I-ACCT-NO         OF PCSARMA0. 
           CALL 'CSARMA0'              USING AIF-AREA
                                             PCSARMA0
                                             SYS-AREA.
           PERFORM 9999-MESSAGE-HANDLE-RTN.
           IF O-MSG-TYPE OF PCSARMA0 NOT = SPACES
           THEN
             MOVE O-RTRN-CODE OF PCSARMA0
                                       TO AIF-MSG-CODE
             PERFORM 9999-MESSAGE-HANDLE-RTN
           END-IF.   
    
       9704-CALL-CCMLST1-RTN.
           INITIALIZE                  PCCMLST1.
           IF TFT-CERT-TYP = C-EC-CER-TYPE1001
           THEN
             MOVE '1000000000'         TO I-CHK-LST  OF PCCMLST1
           ELSE
             MOVE '0000000000'         TO I-CHK-LST  OF PCCMLST1
           END-IF.
           MOVE TFT-CERT-TYP           TO I-CERT-TYP OF PCCMLST1.
           MOVE TFT-CERT-ID            TO I-CERT-ID  OF PCCMLST1.
           CALL 'CCMLST1'              USING AIF-AREA
                                             PCCMLST1
                                             SYS-AREA.
           PERFORM 9999-MESSAGE-HANDLE-RTN.
      
           IF O-MSG-TYPE OF PCCMLST1 NOT = SPACE
           THEN
             MOVE O-RTRN-CODE OF PCCMLST1
                                       TO AIF-MSG-CODE
             PERFORM 9999-MESSAGE-HANDLE-RTN
           END-IF.      

       9705-CALL-CCMENC3-6-RTN.
           
           INITIALIZE                  PCCMENC3.
           MOVE '29'                   TO I-FUNC-CODE OF PCCMENC3.
           MOVE '000000000000000000000' 
                                       TO I-F29-ACTNO OF PCCMENC3.
           MOVE TFT-PSWD               TO I-F29-PIN   OF PCCMENC3.
           IF O-CTL-LST OF PCCMLST1 = SPACES
           THEN
             MOVE ZEROS                TO I-CHK-LST   OF PCCMENC3
           ELSE 
             MOVE O-CTL-LST OF PCCMLST1  
                                       TO I-CHK-LST   OF PCCMENC3
           END-IF.
           CALL 'CCMENC3'              USING AIF-AREA
                                             PCCMENC3
                                             SYS-AREA.
           PERFORM 9999-MESSAGE-HANDLE-RTN.
      
           IF O-MSG-TYPE OF PCCMENC3 NOT = SPACE
           THEN
             MOVE O-RTRN-CODE OF PCCMENC3
                                       TO AIF-MSG-CODE
             PERFORM 9999-MESSAGE-HANDLE-RTN
           END-IF.             
            
    
       9706-CALL-CCMENC3-4-RTN.
           INITIALIZE                  PCCMENC3.
           MOVE '20'                   TO I-FUNC-CODE       OF PCCMENC3.
           MOVE IO-ACCT-NO OF PKSAACN1 TO I-F20-ACTNO       OF PCCMENC3.
           MOVE TFT-PSWD               TO I-F20-NULLPAN-PIN OF PCCMENC3.
           CALL 'CCMENC3'              USING AIF-AREA
                                             PCCMENC3
                                             SYS-AREA.
           PERFORM 9999-MESSAGE-HANDLE-RTN.
           IF O-MSG-TYPE OF PCCMENC3 NOT = SPACE
           THEN
             MOVE O-RTRN-CODE OF PCCMENC3
                                       TO AIF-MSG-CODE
             PERFORM 9999-MESSAGE-HANDLE-RTN
           END-IF.

       9707-CALL-CCMENC3-3-RTN.
           INITIALIZE                  PCCMENC3.
           MOVE '21'                   TO I-FUNC-CODE OF PCCMENC3.
           MOVE IO-ACCT-NO OF PKSAACN1 TO I-F21-ACTNO OF PCCMENC3.
           MOVE '06111111FFFFFFFFFFFFFFFFFFFFFFFF'   
                                       TO I-F21-PIN   OF PCCMENC3.
           CALL 'CCMENC3'              USING AIF-AREA
                                             PCCMENC3
                                             SYS-AREA.
           PERFORM 9999-MESSAGE-HANDLE-RTN.
      
           IF O-MSG-TYPE OF PCCMENC3 NOT = SPACE
           THEN
             MOVE O-RTRN-CODE OF PCCMENC3
                                       TO AIF-MSG-CODE
             PERFORM 9999-MESSAGE-HANDLE-RTN
           END-IF.

       9708-CALL-CCMMSC2-RTN.
           INITIALIZE                  PCCMMSC2.                            
           MOVE C-CM-FUN-INQUIRE       TO I-FUN-CODE OF PCCMMSC2.       
           MOVE WK-WORD-CLSFN          TO I-WORD-CLSFN.                 
           MOVE '999'                  TO I-LEGAL-PERSON-ID OF PCCMMSC2
           MOVE WK-WORD-COD-MSC        TO I-WORD-COD-MSC.                   
           CALL 'CCMMSC2'              USING AIF-AREA                                
                                             PCCMMSC2                                
                                             SYS-AREA.                               
           PERFORM 9999-MESSAGE-HANDLE-RTN.                             
           IF O-MSG-TYPE OF PCCMMSC2 NOT = SPACE                        
           THEN                                                         
             MOVE O-RTRN-CODE OF PCCMMSC2 TO AIF-MSG-CODE               
             PERFORM 9999-MESSAGE-HANDLE-RTN                            
           END-IF.
           
       9709-CALL-CECACNC-RTN.
           INITIALIZE                  PCECACNC.
           MOVE C-SA-FUN-1             TO I-FUN-CODE     OF PCECACNC.

           MOVE O-CUST-NO OF WK-PCECCII1                 
                                       TO I-CUST-NO      OF PCECACNC.
           MOVE SYS-BRANCH-STD         TO I-OPUN-COD     OF PCECACNC.
           MOVE TFT-PD-CODE            TO I-PDT-COD      OF PCECACNC.
           MOVE SYS-LEGAL-PERSON-ID    TO I-LEGAL-ID     OF PCECACNC.
           MOVE '1'                    TO IO-CTRL-RANG   OF PCECACNC.
           MOVE TFT-ACCT-KIND          TO IO-ACCT-NO-TYP OF PCECACNC.
           CALL 'CECACNC'              USING AIF-AREA                                
                                             PCECACNC                                
                                             SYS-AREA.                               
           PERFORM 9999-MESSAGE-HANDLE-RTN.                             
           IF O-MSG-TYPE OF PCECACNC NOT = SPACE                        
           THEN                                                         
             MOVE O-RTRN-CODE OF PCECACNC 
                                       TO AIF-MSG-CODE               
             PERFORM 9999-MESSAGE-HANDLE-RTN                            
           END-IF.
           
      *     IF TFT-ACCT-KIND = C-SA-ACC-I AND
      *       O-ACC-LEGAL-FLAG OF PCECACNC = C-CM-FLAG-YES
      *    THEN
      *      MOVE 'ENE05'              TO AIF-MSG-CODE
      *      PERFORM 9999-MESSAGE-HANDLE-RTN
      *    END-IF.
      *    
      *    IF TFT-ACCT-KIND = C-SA-ACC-II AND
      *       O-ACC-LEGAL-FLAG OF PCECACNC = C-CM-FLAG-YES
      *    THEN
      *      MOVE 'ENE06'              TO AIF-MSG-CODE
      *      PERFORM 9999-MESSAGE-HANDLE-RTN
      *    END-IF
      * 法人内开户超限标志              
      *     IF O-ACC-LEGAL-FLAG OF PCECACNC = C-CM-FLAG-YES
      *     THEN
      *       MOVE 'END32'              TO AIF-MSG-CODE
      *       PERFORM 9999-MESSAGE-HANDLE-RTN
      *     END-IF.
           IF TFT-ACCT-KIND = C-SA-ACC-I AND
              (O-ACC-LEGAL-OPEN OF PCECACNC >= 
                                       O-ACC-LEGAL-NUM OF PCECACNC)
           THEN
             MOVE 'ENE05'              TO AIF-MSG-CODE
             STRING '已开I户数量:', O-ACC-LEGAL-OPEN OF PCECACNC 
               DELIMITED BY SIZE INTO  AIF-MSG-TEXT

             PERFORM 9999-MESSAGE-HANDLE-RTN
           END-IF
      
           IF TFT-ACCT-KIND = C-SA-ACC-II AND
              (O-ACC-LEGAL-OPEN OF PCECACNC >= 
                                       O-ACC-LEGAL-NUM OF PCECACNC)
           THEN
             MOVE 'ENE06'              TO AIF-MSG-CODE
             STRING '已开II户数量:', O-ACC-LEGAL-OPEN OF PCECACNC 
               DELIMITED BY SIZE INTO  AIF-MSG-TEXT
             PERFORM 9999-MESSAGE-HANDLE-RTN
           END-IF
      * 全省内开户超限标志     
           IF O-ACC-PRO-FLAG OF PCECACNC = C-CM-FLAG-YES
           THEN
             MOVE 'FN027'            TO AIF-MSG-CODE
             PERFORM 9999-MESSAGE-HANDLE-RTN
           END-IF.

         
       9720-CALL-CECACND-RTN.
      *  开户介质数量控制只控法人 
           INITIALIZE                 PCECACND.
           MOVE C-SA-FUN-1             TO I-FUN-CODE OF PCECACND.
           MOVE O-CUST-NO OF WK-PCECCII1
                                       TO I-CUST-NO OF PCECACND.
           MOVE SYS-LEGAL-PERSON-ID    TO I-LEGAL-ID OF PCECACND.

      * 介质类型:1：折介质 2：卡介质 3：卡+折介质
           MOVE '1'                    TO I-ACCT-NO-TYP OF PCECACND
           
           CALL 'CECACND'              USING AIF-AREA
                                             PCECACND
                                             SYS-AREA.
           IF O-MSG-TYPE OF PCECACNC NOT = SPACE
           THEN
             MOVE O-RTRN-CODE OF PCECACND 
                                       TO AIF-MSG-CODE
             PERFORM 9999-MESSAGE-HANDLE-RTN
           END-IF.  
           
           IF O-ACC-LEGAL-NUM OF PCECACND NOT = ZERO 
           THEN
             IF O-ACC-LEGAL-NUM OF PCECACND <= 
                O-ACC-LEGAL-OPEN OF PCECACND
             THEN
               MOVE 'END34'             TO AIF-MSG-CODE

               STRING '已开介质数量:', O-ACC-LEGAL-OPEN OF PCECACND 
                  DELIMITED BY SIZE INTO  AIF-MSG-TEXT
               PERFORM 9999-MESSAGE-HANDLE-RTN
             END-IF
           END-IF.
              
           
      *     IF O-ACC-LEGAL-FLAG OF PCECACND = C-CM-FLAG-YES
      *     THEN
      *        MOVE 'END34'             TO AIF-MSG-CODE
      *        PERFORM 9999-MESSAGE-HANDLE-RTN
      *     END-IF.
           
           
           INITIALIZE                 PCECACND.
           MOVE C-SA-FUN-1             TO I-FUN-CODE OF PCECACND.
           MOVE O-CUST-NO OF WK-PCECCII1
                                       TO I-CUST-NO OF PCECACND.
           MOVE SYS-LEGAL-PERSON-ID    TO I-LEGAL-ID OF PCECACND.

      * 介质类型:1：折介质 2：卡介质 3：卡+折介质
           MOVE '3'                    TO I-ACCT-NO-TYP OF PCECACND
           
           CALL 'CECACND'              USING AIF-AREA
                                             PCECACND
                                             SYS-AREA.
           IF O-MSG-TYPE OF PCECACNC NOT = SPACE
           THEN
             MOVE O-RTRN-CODE OF PCECACND 
                                       TO AIF-MSG-CODE
             PERFORM 9999-MESSAGE-HANDLE-RTN
           END-IF.  
           
           IF O-ACC-LEGAL-NUM OF PCECACND NOT = ZERO 
           THEN
             IF O-ACC-LEGAL-NUM OF PCECACND <= 
                O-ACC-LEGAL-OPEN OF PCECACND
             THEN
               MOVE 'END34'             TO AIF-MSG-CODE

               STRING '卡加折介质数量:', O-ACC-LEGAL-OPEN OF PCECACND 
                 DELIMITED BY SIZE INTO  AIF-MSG-TEXT
               PERFORM 9999-MESSAGE-HANDLE-RTN
             END-IF
           END-IF.
           
       9710-CALL-CCMDAT1-RTN.
           INITIALIZE                  PCCMDAT1.
           MOVE C-CM-DT-SDTOED         TO I-CMDATE-FUNC      OF PCCMDAT1.
           MOVE C-CM-DT-DEP-DAY        TO I-CMDATE-TYPE      OF PCCMDAT1.
           MOVE SYS-BUSN-DT            TO IO-CMDATE-STA-DATE OF PCCMDAT1.     
           MOVE WK-PERIOD              TO IO-CMDATE-PERIOD-DDD OF 
                                          PCCMDAT1.
           CALL 'CCMDAT1'              USING AIF-AREA                                
                                             PCCMDAT1                                
                                             SYS-AREA.                               
           PERFORM 9999-MESSAGE-HANDLE-RTN.                             
           IF O-CMDATE-RESULT OF PCCMDAT1 NOT = SPACE
           THEN
             MOVE O-CMDATE-RESULT OF PCCMDAT1
                                       TO AIF-MSG-CODE
             PERFORM 9999-MESSAGE-HANDLE-RTN
           END-IF.
      * 检验代理开户跟被代理开户     
       9810-CALL-CECACNC-RTN.
           INITIALIZE                  PCECACNC.
      *  代理开户
           MOVE C-SA-FUN-2             TO I-FUN-CODE     OF PCECACNC.

           MOVE O-CUST-NO OF WK-PCECCII1                 
                                       TO I-AG-CUST-NO OF PCECACNC.
           MOVE TFT-AGT-CERT-TYP       TO I-AG-CER-TYP OF PCECACNC.
           MOVE TFT-AGT-CERT-ID        TO I-AG-CER-NO OF PCECACNC.
           MOVE TFT-AGT-NAME           TO I-AG-FULL-NAME OF PCECACNC
                                       
           MOVE SYS-BRANCH-STD         TO I-OPUN-COD     OF PCECACNC.
           MOVE TFT-PD-CODE            TO I-PDT-COD      OF PCECACNC.
           MOVE SYS-LEGAL-PERSON-ID    TO I-LEGAL-ID     OF PCECACNC.
           MOVE '1'                    TO IO-CTRL-RANG   OF PCECACNC.
           MOVE '1'                    TO IO-ACCT-NO-TYP OF PCECACNC.
           CALL 'CECACNC'              USING AIF-AREA                                
                                             PCECACNC                                
                                             SYS-AREA.                               
           PERFORM 9999-MESSAGE-HANDLE-RTN.                             
           IF O-MSG-TYPE OF PCECACNC NOT = SPACE                        
           THEN                                                         
             MOVE O-RTRN-CODE OF PCECACNC 
                                       TO AIF-MSG-CODE               
             PERFORM 9999-MESSAGE-HANDLE-RTN                            
           END-IF.
           
      * 法人内开户超限标志              
           IF O-ACC-LEGAL-FLAG OF PCECACNC = C-CM-FLAG-YES
           THEN
             MOVE 'FN025'              TO AIF-MSG-CODE
             PERFORM 9999-MESSAGE-HANDLE-RTN
           END-IF

           
           
           INITIALIZE                  PCECACNC.
      *  被代理开户
           MOVE C-SA-FUN-3             TO I-FUN-CODE     OF PCECACNC.

           MOVE O-CUST-NO OF WK-PCECCII1                 
                                       TO I-CUST-NO OF PCECACNC.
           MOVE SYS-BRANCH-STD         TO I-OPUN-COD     OF PCECACNC.
           MOVE TFT-PD-CODE            TO I-PDT-COD      OF PCECACNC.
           MOVE SYS-LEGAL-PERSON-ID    TO I-LEGAL-ID     OF PCECACNC.
           MOVE '1'                    TO IO-CTRL-RANG   OF PCECACNC.
           MOVE TFT-ACCT-KIND          TO IO-ACCT-NO-TYP OF PCECACNC.
           CALL 'CECACNC'              USING AIF-AREA                                
                                             PCECACNC                                
                                             SYS-AREA.                               
           PERFORM 9999-MESSAGE-HANDLE-RTN.                             
           IF O-MSG-TYPE OF PCECACNC NOT = SPACE                        
           THEN                                                         
             MOVE O-RTRN-CODE OF PCECACNC 
                                       TO AIF-MSG-CODE               
             PERFORM 9999-MESSAGE-HANDLE-RTN                            
           END-IF.
           
      * 法人内开户超限标志              
           IF O-ACC-LEGAL-FLAG OF PCECACNC = C-CM-FLAG-YES
           THEN
             MOVE 'FN026'              TO AIF-MSG-CODE
             PERFORM 9999-MESSAGE-HANDLE-RTN
           END-IF.

