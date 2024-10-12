        IDENTIFICATION DIVISION.
        PROGRAM-ID. TEST-COPY.
        DATA DIVISION.
           WORKING-STORAGE SECTION.
           01 PCSAACN0.
              05  I-TRAC-ALWAYS                     PIC 9(1).                      
                88  TRAC-C-ALWAYS                   VALUE 1.                       
                88  TRAC-C-NOT-ALWAYS               VALUE 0.                       
              05  I-TRAC-FLAG.                                                     
                10  I-TRAC-CONSOLE-TRACE            PIC 9.                         
                  88  TRAC-C-CONSOLE-NO             VALUE  0.                      
                  88  TRAC-C-CONSOLE-YES            VALUE  1.                      
                10  I-TRAC-FILE-TRACE               PIC 9.                         
                  88  TRAC-C-FILE-NO                VALUE  0.                      
                  88  TRAC-C-FILE-YES               VALUE  1.                      
                10  I-TRAC-QUEUE-TRACE              PIC 9.                         
                  88  TRAC-C-QUEUE-NO               VALUE  0.                      
                  88  TRAC-C-QUEUE-YES              VALUE  1.                      
              05  I-TRAC-DATA.                                                     
                10  I-TRAC-PGM-ID          PIC   X(8).                             
                10  I-TRAC-MESSAGE         PIC   X(52). 
        PROCEDURE DIVISION.
              IF TRAC-C-ALWAYS
                 DISPLAY "ABC".