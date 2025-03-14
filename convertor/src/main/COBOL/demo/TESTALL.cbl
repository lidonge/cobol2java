       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTALL.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

* 定义不同的数据类型
       01 WS-NUMBER-1        PIC 9(4) VALUE 1000.
       01 WS-NUMBER-2        PIC 9(4) VALUE 250.
       01 WS-DECIMAL-NUMBER  PIC 9(4)V9(2) VALUE 123.45.
       01 WS-CHARACTER       PIC X(10) VALUE 'HELLO'.
       01 WS-RESULT          PIC 9(6).
       01 WS-RETURN-CODE     PIC 9(4).

* 定义一维数组
       01 WS-ARRAY-1         PIC 9(4) OCCURS 5 TIMES.

* 定义二维数组
       01 WS-ARRAY-2.
          05 WS-ROW OCCURS 3 TIMES.
             10 WS-COLUMN OCCURS 3 TIMES PIC 9(4).

* 定义一个字符串
       01 WS-STRING          PIC X(20) VALUE 'COBOL EXAMPLE'.

       PROCEDURE DIVISION.
* 初始化
       INITIALIZE WS-RESULT, WS-STRING.
       INITIALIZE WS-NUMBER-1 REPLACING NUMERIC DATA BY 12345.
       INITIALIZE WS-CHARACTER REPLACING ALPHABETIC DATA BY 'USA'.

* 执行加法
       ADD WS-NUMBER-1 TO WS-NUMBER-2 GIVING WS-RESULT.
       DISPLAY 'ADD RESULT: ' WS-RESULT.

* 执行减法
       SUBTRACT WS-NUMBER-2 FROM WS-NUMBER-1 GIVING WS-RESULT.
       DISPLAY 'SUBTRACT RESULT: ' WS-RESULT.

* 执行乘法
       MULTIPLY WS-NUMBER-1 BY WS-NUMBER-2 GIVING WS-RESULT.
       DISPLAY 'MULTIPLY RESULT: ' WS-RESULT.

* 执行除法
       DIVIDE WS-NUMBER-1 BY WS-NUMBER-2 GIVING WS-RESULT.
       DISPLAY 'DIVIDE RESULT: ' WS-RESULT.

* 计算表达式
       COMPUTE WS-RESULT = (WS-NUMBER-1 + WS-NUMBER-2) * 2.
       DISPLAY 'COMPUTE RESULT: ' WS-RESULT.

* 将字符移动到另一个变量
       MOVE WS-CHARACTER TO WS-STRING.
       DISPLAY 'MOVED STRING: ' WS-STRING.

* 数组操作
       MOVE 10 TO WS-ARRAY-1(1).
       MOVE 20 TO WS-ARRAY-1(2).
       MOVE 30 TO WS-ARRAY-1(3).
       DISPLAY 'ARRAY-1 ELEMENTS:' WS-ARRAY-1(1) WS-ARRAY-1(2) WS-ARRAY-1(3).

* 二维数组操作
       MOVE 1 TO WS-ARRAY-2(1, 1).
*FIXME        (1 ,2) |(1, 2) because NUMERICLITERAL
       MOVE 2 TO WS-ARRAY-2(1,2).
       MOVE 3 TO WS-ARRAY-2(1,3).
       MOVE 4 TO WS-ARRAY-2(2,1).
       MOVE 5 TO WS-ARRAY-2(2,2).
       MOVE 6 TO WS-ARRAY-2(2,3).
       DISPLAY 'ARRAY-2 ELEMENTS ROW 1:' WS-ARRAY-2(1,1) WS-ARRAY-2(1,2) WS-ARRAY-2(1,3).
       DISPLAY 'ARRAY-2 ELEMENTS ROW 2:' WS-ARRAY-2(2,1) WS-ARRAY-2(2,2) WS-ARRAY-2(2,3).

* 条件语句
       IF WS-NUMBER-1 > WS-NUMBER-2
           DISPLAY 'WS-NUMBER-1 IS GREATER THAN WS-NUMBER-2'
       ELSE
           DISPLAY 'WS-NUMBER-1 IS NOT GREATER THAN WS-NUMBER-2'
       END-IF.

* 循环语句
       PERFORM VARYING WS-RESULT FROM 1 BY 1 UNTIL WS-RESULT > 3
           DISPLAY 'LOOP ITERATION: ' WS-RESULT
       END-PERFORM.

* 字符串处理
       STRING 'WELCOME ' DELIMITED BY SPACE
              WS-STRING DELIMITED BY SIZE
              INTO WS-STRING
       END-STRING.
       DISPLAY 'FINAL STRING: ' WS-STRING.
* 调用子程序
       CALL 'SUBPROG' USING BY VALUE WS-NUMBER-1, BY CONTENT WS-NUMBER-2, BY REFERENCE WS-RESULT GIVING WS-RETURN-CODE
           ON EXCEPTION
               DISPLAY "Subprogram not found or error occurred during execution"
               MOVE 1 TO WS-RETURN-CODE
           NOT ON EXCEPTION
               DISPLAY "Subprogram executed successfully"
               MOVE 0 TO WS-RETURN-CODE
       END-CALL.
       STOP RUN.