grammar CobolWithSql;
cobolSql
    : dataVisionSql | sqlStatement | declareCursor
    ;

dataVisionSql
    : sqlInclude
    | beginDeclare
    | endDeclare
    ;

endDeclare : SQL_EXEC SQL 'END' 'DECLARE' 'SECTION' SQL_END ;

beginDeclare : SQL_EXEC SQL 'BEGIN' 'DECLARE' 'SECTION' SQL_END ;

sqlInclude : SQL_EXEC SQL 'INCLUDE' (identifier | FILE_LITERAL) SQL_END ;

sqlStatement
    : SQL_EXEC SQL (sqlQuery | sqlCursorOperation | errorHandle) SQL_END
    ;

errorHandle: 'WHENEVER' errorType errorAction;

errorAction: goToAction | 'CONTINUE';

goToAction: 'GOTO' cobolVariable;

errorType: 'SQLERROR' | 'SQLWARNING' |'NOT' 'FOUND';

declareCursor
    : SQL_EXEC SQL 'DECLARE' cursorName 'CURSOR' withClause? 'FOR' selectQuery forUpdateClause? isolationClause? SQL_END
    ;

isolationClause: 'WITH' ('UR'|'RS');
withClause
    : 'WITH' ('HOLD' | 'ROWSET' 'POSITIONING' | 'INSENSITIVE' 'SCROLL'|'SENSITIVE' 'SCROLL')
    ;
sqlQuery
    : selectQuery
    | insertQuery
    | updateQuery
    | deleteQuery
    ;

selectQuery
    : 'SELECT' (allColumn | columnList) ('INTO' cobolVariableList)? 'FROM' tableName whereClause? orderByClause?
    ;

insertQuery
    : 'INSERT' 'INTO' tableName ('(' columnList ')')? 'VALUES' '(' valueList ')'
    ;

updateQuery
    : 'UPDATE' tableName 'SET' assignmentList whereClause
    ;

deleteQuery
    : 'DELETE' 'FROM' tableName whereClause
    ;

sqlCursorOperation
    : openCursor
    | closeCursor
    | fetchCursor
    ;

openCursor
    : 'OPEN' cursorName
    ;

closeCursor
    : 'CLOSE' cursorName
    ;

fetchCursor
    : 'FETCH' cursorName 'INTO' cobolVariableList
    ;

columnList
    : columnName (',' columnName)*
    ;

valueList
    : value (',' value)*
    ;

assignmentList
    : columnName '=' value (',' columnName '=' value)*
    ;

whereClause
    : 'WHERE' (conditionExpression | cursorCondition)
    ;

cursorCondition: 'CURRENT' 'OF' cursorName;

conditionExpression
    : condition (logicalOperator condition)*
    | '(' conditionExpression ')' (logicalOperator conditionExpression)*
    ;

condition
    : columnName operator value
    | columnName 'IS' 'NULL'
    | columnName 'IS NOT' 'NULL'
    | '(' conditionExpression ')'
    ;

logicalOperator
    : 'AND'
    | 'OR'
    ;

orderByClause
    : 'ORDER' 'BY' orderByColumn (',' orderByColumn)*
    ;

orderByColumn
    : columnName orderDirection?
    ;

orderDirection
    : 'ASC' | 'DESC'
    ;

forUpdateClause
    : 'FOR' ('UPDATE' | 'READ' 'ONLY'| 'FETCH' 'ONLY' |'SYSTEM' 'NAME')
    ;


columnName
    : IDENTIFIER
    ;

operator
    : '='
    | '<>'
    | '>'
    | '>='
    | '<'
    | '<='
    | 'LIKE'
    | 'IN' '(' valueList ')'
    ;

value
    : ':' IDENTIFIER
    | STRING_LITERAL
    | NUMBER_LITERAL
    ;

cobolVariableList
    : ':' cobolVariable (',' ':' cobolVariable)*
    ;

identifier
    : IDENTIFIER
    ;

cursorName
    : IDENTIFIER
    ;

tableName
    : identifier
    ;


cobolVariable
    : identifier
    ;

allColumn
    : '*'
    ;
FILE_LITERAL : FILELITERAL;
STRING_LITERAL : STRINGLITERAL;
NUMBER_LITERAL : NUMBERLITERAL;
fragment FILELITERAL
    : '\'' (~['\r\n'] | '\'\'' | '\\\'')* '\''
    ;
fragment STRINGLITERAL :
	'"' (~["\n\r] | '""' | '\'')* '"'
	| '\'' (~['\n\r] | '\'\'' | '"')* '\''
;
fragment NUMBERLITERAL : [0-9]+;
SQL_EXEC            : 'EXEC';
SQL                 : 'SQL';
SQL_END                 : 'END-EXEC' '.'?;
IDENTIFIER : [a-zA-Z0-9]+ ([-_]+ [a-zA-Z0-9]+)*;
WS : [ \t\n\r]+ -> skip ;