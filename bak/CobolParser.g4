parser grammar CobolParser;

compilationUnit : (programUnit nestedProgramUnit* endProgramStatement?)* EOF  ;

programUnit : identificationDivision environmentDivision? dataDivision? procedureDivision?  ;

nestedProgramUnit : nestedIdentificationDivision environmentDivision? dataDivision? procedureDivision? nestedProgramUnit* endProgramStatement  ;

endProgramStatement : END PROGRAM programName DOT  ;

identificationDivision : IDENTIFICATION DIVISION DOT programIdParagraph identificationDivisionParagraph*  ;

nestedIdentificationDivision : (IDENTIFICATION | ID) DIVISION DOT nestedProgramIdParagraph identificationDivisionParagraph*  ;

identificationDivisionParagraph : (authorParagraph | installationParagraph | dateWrittenParagraph | dateCompiledParagraph | securityParagraph)  ;

programIdParagraph : PROGRAM_ID DOT programName (IS? INITIAL PROGRAM?)? DOT  ;

nestedProgramIdParagraph : PROGRAM_ID DOT programName (IS? initialOrCommon PROGRAM?)? DOT  ;

initialOrCommon : (INITIAL COMMON? | COMMON INITIAL?)  ;

authorParagraph : (AUTHOR | AUTHOR2) (DOT2 | DOT) commentLine?  ;

installationParagraph : (INSTALLATION | INSTALLATION2) (DOT2 | DOT) commentLine?  ;

dateWrittenParagraph : (DATE_WRITTEN | DATE_WRITTEN2) (DOT2 | DOT) commentLine?  ;

dateCompiledParagraph : (DATE_COMPILED | DATE_COMPILED2) (DOT2 | DOT) commentLine?  ;

securityParagraph : (SECURITY | SECURITY2) (DOT2 | DOT) commentLine?  ;

environmentDivision : (ENVIRONMENT DIVISION DOT | ENVIRONMENT_DIVISION) environmentSection*  ;

environmentSection : (configurationSection | inputOutputSection)  ;

configurationSection : CONFIGURATION SECTION DOT configurationSectionParagraph*  ;

configurationSectionParagraph : (sourceComputerParagraph | objectComputerParagraph | specialNamesParagraph)  ;

sourceComputerParagraph : SOURCE_COMPUTER DOT computerName (WITH? DEBUGGING MODE)? DOT  ;

objectComputerParagraph : OBJECT_COMPUTER DOT computerName objectComputerClause* DOT  ;

objectComputerClause : (memorySizeClause | collatingSequenceClause | segmentLimitClause | characterSetClause)  ;

memorySizeClause : MEMORY SIZE? integerConstant (WORDS | CHARACTERS | MODULES)?  ;

collatingSequenceClause : PROGRAM? COLLATING? SEQUENCE IS? alphabetName  ;

segmentLimitClause : SEGMENT_LIMIT IS? integerConstant  ;

characterSetClause : CHARACTER SET  ;

specialNamesParagraph : SPECIAL_NAMES DOT (specialNameClause (COMMACHAR? specialNameClause)* DOT)?  ;

specialNameClause : (alphabetClause | classClause | currencySignClause | decimalPointClause | symbolicCharactersClause | environmentNameIsMnemonicNameClause)  ;

alphabetClause : ALPHABET alphabetName IS? (STANDARD_1 | STANDARD_2 | NATIVE | cobolWord | (literal ((THROUGH | THRU) literal | (ALSO literal COMMACHAR?)+)? COMMACHAR?)+)  ;

classClause : CLASS className IS? (literal ((THROUGH | THRU) literal)?)+  ;

currencySignClause : CURRENCY SIGN? IS? literal  ;

decimalPointClause : DECIMAL_POINT IS? COMMA  ;

symbolicCharactersClause : SYMBOLIC CHARACTERS? (symbolicCharacter+ (ARE | IS)? integerConstant+)+ (IN alphabetName)?  ;

environmentNameIsMnemonicNameClause : (environmentName IS? mnemonicName specialNamesParagraphStatusPhrase? | specialNamesParagraphStatusPhrase)  ;

specialNamesParagraphStatusPhrase : (ON STATUS? IS? condition (OFF STATUS? IS? condition)? | OFF STATUS? IS? condition (ON STATUS? IS? condition)?)  ;

inputOutputSection : (INPUT_OUTPUT SECTION DOT)? inputOutputSectionParagraph+  ;

inputOutputSectionParagraph : (fileControlParagraph | iOControlParagraph)  ;

fileControlParagraph : (FILE_CONTROL DOT | fileControlEntry DOT) (fileControlEntry DOT)*  ;

fileControlEntry : selectClause fileControlClause*  ;

fileControlClause : (assignClause | reserveClause | keyClause | organizationClause | paddingCharacterClause | recordDelimiterClause | accessModeClause | alternateRecordKeyClause | fileStatusClause | passwordClause)  ;

selectClause : SELECT OPTIONAL? fileName  ;

assignClause : ASSIGN TO? DISK? (assignmentName | literal)  ;

reserveClause : RESERVE integerConstant (AREA | AREAS)?  ;

organizationClause : ORGANIZATION? IS? (sequentialOrganizationClause | indexedOrganizationClause | relativeOrganizationClause | lineSequentialOrganizationClause)  ;

sequentialOrganizationClause : SEQUENTIAL  ;

lineSequentialOrganizationClause : LINE SEQUENTIAL  ;

relativeOrganizationClause : RELATIVE  ;

indexedOrganizationClause : INDEXED  ;

paddingCharacterClause : PADDING CHARACTER? IS? (qualifiedDataName | literal)  ;

recordDelimiterClause : RECORD DELIMITER IS? (STANDARD_1 | IMPLICIT | assignmentName)  ;

accessModeClause : ACCESS MODE? IS? (sequentialAccessMode | randomAccessMode | dynamicAccessMode)  ;

sequentialAccessMode : SEQUENTIAL  ;

randomAccessMode : RANDOM  ;

dynamicAccessMode : DYNAMIC  ;

keyClause : (RELATIVE | RECORD) KEY? IS? qualifiedDataName  ;

alternateRecordKeyClause : ALTERNATE RECORD KEY? IS? qualifiedDataName passwordClause? (WITH? DUPLICATES)?  ;

passwordClause : PASSWORD IS? dataName  ;

fileStatusClause : FILE? STATUS IS? qualifiedDataName qualifiedDataName?  ;

iOControlParagraph : I_O_CONTROL DOT (iOControlClause (DOT? iOControlClause)* DOT)?  ;

iOControlClause : COMMACHAR? (rerunClause | sameAreaClause | multipleFileClause) COMMACHAR?  ;

rerunClause : RERUN (ON (assignmentName | fileName))? EVERY (rerun2 | integerConstant CLOCK_UNITS?)  ;

rerun2 : (integerConstant RECORDS | END? OF? (REEL | UNIT) OF fileName)  ;

sameAreaClause : SAME (RECORD | SORT | SORT_MERGE)? AREA? FOR? (fileName COMMACHAR?)+  ;

multipleFileClause : MULTIPLE FILE TAPE? CONTAINS? (fileName POSITION? integerConstant? COMMACHAR?)+  ;

dataDivision : (DATA DIVISION DOT | DATA_DIVISION) dataDivisionSection*  ;

dataDivisionSection : (fileSection | workingStorageSection | linkageSection | communicationSection)  ;

communicationSection : COMMUNICATION SECTION DOT (communicationDescriptionEntry dataDescriptionEntry*)*  ;

communicationDescriptionEntry : (communicationInputEntry | communicationOutputEntry | communicationIOEntry) DOT  ;

communicationInputEntry : CD cdName FOR? INITIAL? INPUT communicationInputClause* (dataName | FILLER)*  ;

communicationOutputEntry : CD cdName FOR? OUTPUT communicationOutputClause*  ;

communicationIOEntry : CD cdName FOR? INITIAL? I_O communicationIOClause* (dataName | FILLER)*  ;

communicationInputClause : (MESSAGE (DATE | TIME | COUNT) IS? dataName | TEXT LENGTH IS? dataName | END KEY IS? dataName | STATUS KEY IS? dataName | COUNT IS? dataName | SYMBOLIC? (QUEUE | SUB_QUEUE_1 | SUB_QUEUE_2 | SUB_QUEUE_3 | SOURCE) IS? dataName)  ;

communicationOutputClause : (DESTINATION COUNT IS? dataName | TEXT LENGTH IS? dataName | STATUS KEY IS? dataName | SYMBOLIC? DESTINATION IS? dataName | DESTINATION TABLE OCCURS integerConstant TIMES? (INDEXED BY? (indexName COMMACHAR?)+)? | ERROR KEY IS? dataName)  ;

communicationIOClause : (MESSAGE (DATE | TIME) IS? dataName | TEXT LENGTH IS? dataName | END KEY IS? dataName | STATUS KEY IS? dataName | SYMBOLIC? TERMINAL IS? dataName)  ;

fileSection : (FILE SECTION DOT | fileAndSortDescriptionEntry dataDescriptionEntry+) (fileAndSortDescriptionEntry dataDescriptionEntry+)*  ;

fileAndSortDescriptionEntry : (FD | SD) fileName fileAndSortDescriptionEntryClause* DOT  ;

fileAndSortDescriptionEntryClause : (externalClause | globalClause | blockContainsClause | recordContainsClause | labelRecordsClause | valueOfClause | dataRecordClause | linageClause | codeSetClause | reportClause | recordingModeClause)  ;

externalClause : IS? EXTERNAL  ;

globalClause : IS? GLOBAL  ;

blockContainsClause : BLOCK CONTAINS? (integerConstant TO)? integerConstant (RECORDS | CHARACTERS)?  ;

recordContainsClause : RECORD CONTAINS? ((integerConstant TO)? integerConstant CHARACTERS? | IS? VARYING IN? SIZE? (FROM? integerConstant (TO integerConstant)? CHARACTERS?)? (DEPENDING ON? qualifiedDataName)?)  ;

labelRecordsClause : LABEL (RECORD IS? | RECORDS ARE?) (OMITTED | STANDARD | dataName+)  ;

valueOfClause : VALUE OF (systemName IS (qualifiedDataName | literal))+  ;

dataRecordClause : DATA (RECORD IS? | RECORDS ARE?) (dataName COMMACHAR?)+  ;

linageClause : LINAGE IS? (dataName | integerConstant) LINES? (WITH? FOOTING AT? (dataName | integerConstant) | LINES? AT? TOP (dataName | integerConstant) | LINES? AT? BOTTOM (dataName | integerConstant))*  ;

recordingModeClause : RECORDING MODE? IS? modeOf  ;

codeSetClause : CODE_SET IS? alphabetName  ;

reportClause : (REPORT IS? | REPORTS ARE?) qualifiedDataName+  ;

dataDescriptionEntry : (levelNumber (dataName | FILLER)? dataDescriptionEntryClause* DOT | LEVEL_66 dataName renamesClause DOT | LEVEL_77 dataName dataDescriptionEntryClause* DOT | LEVEL_78 conditionName conditionValueClause DOT | LEVEL_88 conditionName conditionValueClause DOT | (EXEC | EXECUTE) K_SQL (K_INCLUDE (S_IDENTIFIER | S_QUOTED_IDENTIFIER) DOT | K_BEGIN K_DECLARE K_SECTION END_EXEC DOT | K_END K_DECLARE K_SECTION END_EXEC DOT | declareCursorStatement END_EXEC DOT))  ;

dataDescriptionEntryClause : COMMACHAR? (dataPictureClause | dataValueClause | dataUsageClause | dataRedefinesClause | dataExternalClause | dataGlobalClause | dataSignClause | dataOccursClause | dataSynchronizedClause | dataJustifiedClause | dataBlankWhenZeroClause) COMMACHAR?  ;

dataRedefinesClause : REDEFINES dataName  ;

dataBlankWhenZeroClause : BLANK WHEN? (ZERO | ZEROS | ZEROES)  ;

dataJustifiedClause : (JUSTIFIED | JUST) RIGHT?  ;

dataOccursClause : OCCURS ((integerConstant | dataName) TO)? (integerConstant | dataName) TIMES? (DEPENDING ON? qualifiedDataName)? ((ASCENDING | DESCENDING) KEY? IS? qualifiedDataName+)* (INDEXED BY? (indexName COMMACHAR?)+)?  ;

dataPictureClause : (PICTURE | PIC) IS? pictureString VARYING?  ;

pictureString : pictureOccurence (DOTCHAR pictureOccurence | pictureOccurence)* DOTCHAR*  ;

pictureOccurence : (nonDotChars+ (LPARENCHAR (integerConstant | dataName) RPARENCHAR)? | DOTCHAR (LPARENCHAR (integerConstant | dataName) RPARENCHAR | nonDotChars))  ;

picturePunctuation : (SLASHCHAR | COMMACHAR | COLONCHAR | ASTERISKCHAR | MINUSCHAR | PLUSCHAR | POW | LESSTHANOREQUAL | LESSTHANCHAR | MORETHANOREQUAL | MORETHANCHAR | EQUALCHAR | NOTEQUALCHAR)  ;

pictureCurrency : DOLLARCHAR  ;

nonDotChars : (integerConstant | cobolWord | picturePunctuation | pictureCurrency)  ;

dataExternalClause : IS? EXTERNAL  ;

dataGlobalClause : IS? GLOBAL  ;

dataUsageClause : (USAGE IS?)? (BINARY | COMP | COMP_1 | COMP_2 | COMP_3 | COMP_4 | COMP_5 | COMPUTATIONAL | COMPUTATIONAL_1 | COMPUTATIONAL_2 | COMPUTATIONAL_3 | COMPUTATIONAL_4 | COMPUTATIONAL_5 | DISPLAY | DISPLAY_1 | INDEX | PACKED_DECIMAL | POINTER | FUNCTION_POINTER | PROCEDURE_POINTER | OBJECT REFERENCE dataName)  ;

dataSignClause : (SIGN IS?)? (LEADING | TRAILING) (SEPARATE CHARACTER?)?  ;

dataSynchronizedClause : (SYNCHRONIZED | SYNC) (LEFT | RIGHT)?  ;

dataValueClause : (VALUE IS? | VALUES ARE?) ((identifier | literal) COMMACHAR? ((THROUGH | THRU) literal COMMACHAR?)?)+  ;

conditionValueClause : dataValueClause  ;

renamesClause : RENAMES qualifiedDataName ((THROUGH | THRU) qualifiedDataName)?  ;

workingStorageSection : WORKING_STORAGE SECTION DOT dataDescriptionEntry*  ;

linkageSection : LINKAGE SECTION DOT dataDescriptionEntry*  ;

procedureDivision : (PROCEDURE DIVISION | PROCEDURE_DIVISION) (DOT? usingArgs)? DOT declaratives? procedureBody  ;

usingArgs : USING ((BY? (REFERENCE | VALUE))? qualifiedDataName COMMACHAR?)+  ;

declaratives : DECLARATIVES DOT (sectionHeader DOT useStatement DOT paragraphs)+ END DECLARATIVES DOT  ;

procedureBody : paragraphs procedureSection*  ;

procedureSection : sectionHeader DOT paragraphs  ;

sectionHeader : sectionName SECTION integerConstant?  ;

paragraphs : sentence* paragraph*  ;

paragraph : (paragraphName | entryStatement) DOT (exitProgramStatement DOT | exitStatement DOT | alteredGoto | sentence*)  ;

sentence : statement+ DOT  ;

statementList : statement+  ;

statement : (acceptStatement | addStatement | alterStatement | callStatement | cancelStatement | closeStatement | computeStatement | continueStatement | deleteStatement | displayStatement | divideStatement | evaluateStatement | exitProgramStatement | exitStatement | gobackStatement | gotoStatement | ifStatement | initializeStatement | inspectStatement | mergeStatement | moveStatement | multiplyStatement | openStatement | performStatement | readStatement | releaseStatement | returnStatement | rewriteStatement | searchStatement | setStatement | sortStatement | startStatement | stopStatement | stringStatement | subtractStatement | unstringStatement | writeStatement | execSqlStatement | enableStatement | disableStatement | receiveStatement | sendStatement) COMMACHAR?  ;

enableStatement : ENABLE (INPUT TERMINAL? | OUTPUT) (identifier | literal) WITH? KEY (identifier | literal)  ;

disableStatement : DISABLE (INPUT TERMINAL? | OUTPUT) (identifier | literal) WITH? KEY (identifier | literal)  ;

receiveStatement : RECEIVE (identifier | literal) (MESSAGE | SEGMENT) INTO identifier (';' | NO DATA statement)?  ;

sendStatement : SEND (identifier | literal) (FROM identifier)? (WITH (identifier | ESI | EMI | EGI))? ((BEFORE | AFTER) ADVANCING? ((identifier | literal) (LINE | LINES)? | (mnemonicName | PAGE)))?  ;

execSqlStatement : (EXEC | EXECUTE) K_SQL (K_WHENEVER (K_NOT K_FOUND | K_SQLERROR | K_SQLWARNING) statement | (sQLStatement | declareCursorStatement | K_PREPARE S_IDENTIFIER K_FROM S_BIND | K_ALTER K_SESSION sQLSetStatement | K_EXECUTE | K_CONNECT S_BIND)? END_EXEC)  ;

declareCursorStatement : K_DECLARE S_IDENTIFIER K_CURSOR K_FOR (S_IDENTIFIER | queryStatement)  ;

acceptStatement : ACCEPT identifier (FROM (mnemonicName | environmentName | DATE COBOL_WORD? | DAY COBOL_WORD? | DAY_OF_WEEK | TIME) | MESSAGE? COUNT)?  ;

addStatement : ADD addBody (ON? SIZE ERROR statementList)? (NOT ON? SIZE ERROR statementList)? END_ADD?  ;

addBody : (idOrLiteralList (TO idOrLiteral)? GIVING arithIdentifierList | idOrLiteralList TO arithIdentifierList | (CORRESPONDING | CORR) identifier TO identifier ROUNDED?)  ;

arithIdentifier : identifier ROUNDED?  ;

arithIdentifierList : (arithIdentifier COMMACHAR?)+  ;

idOrLiteral : (identifier | literal)  ;

idOrLiteralList : (idOrLiteral COMMACHAR?)+  ;

alteredGoto : GO TO? DOT  ;

alterStatement : ALTER (procedureName TO (PROCEED TO)? procedureName COMMACHAR?)+  ;

callStatement : CALL (identifier | literal) (USING ((BY? REFERENCE)? (callByReferenceArgs COMMACHAR?)+ | BY? (CONTENT | VALUE) (callByContentArgs COMMACHAR?)+)+)? (ON? OVERFLOW statementList)? (ON? EXCEPTION statementList)? (NOT ON? EXCEPTION statementList)? END_CALL?  ;

callByReferenceArgs : (identifier | ADDRESS OF identifier | fileName)  ;

callByContentArgs : ((LENGTH OF)? identifier | ADDRESS OF identifier | literal)  ;

cancelStatement : CANCEL ((identifier | literal) COMMACHAR?)+  ;

closeStatement : CLOSE (fileName ((REEL | UNIT) (FOR? REMOVAL | WITH? NO REWIND)? | WITH? (NO REWIND | LOCK))? COMMACHAR?)+  ;

computeStatement : COMPUTE (identifier ROUNDED?)+ (EQUALCHAR | EQUAL) arithmeticExpression (ON? SIZE ERROR statementList)? (NOT ON? SIZE ERROR statementList)? END_COMPUTE?  ;

continueStatement : CONTINUE  ;

deleteStatement : DELETE fileName RECORD? (INVALID KEY? statementList)? (NOT INVALID KEY? statementList)? END_DELETE?  ;

displayStatement : DISPLAY ((identifier | literal) COMMACHAR?)+ (UPON (mnemonicName | environmentName))? (WITH? NO ADVANCING)?  ;

divideStatement : DIVIDE divideBody (ON? SIZE ERROR statementList)? (NOT ON? SIZE ERROR statementList)? END_DIVIDE?  ;

divideBody : (idOrLiteral INTO (idOrLiteral | arithIdentifierList) (GIVING arithIdentifierList (REMAINDER arithIdentifier)?)? | idOrLiteral BY idOrLiteral GIVING arithIdentifierList (REMAINDER arithIdentifier)?)  ;

entryStatement : ENTRY literal usingArgs?  ;

evaluateStatement : EVALUATE evaluateValue (ALSO evaluateValue)* ((WHEN EQUALCHAR? evaluatePhrase (ALSO evaluatePhrase)*)+ statementList)+ (WHEN OTHER statementList)? END_EVALUATE?  ;

evaluateValue : (identifier | condition | arithmeticExpression | literal | TRUE | FALSE)  ;

evaluatePhrase : (ANY | NOT? (identifier | literal | arithmeticExpression) ((THROUGH | THRU) (identifier | literal | arithmeticExpression))? | condition | TRUE | FALSE)  ;

exitStatement : EXIT  ;

exitProgramStatement : EXIT PROGRAM  ;

gobackStatement : GOBACK  ;

gotoStatement : GO TO? (procedureName (procedureName* DEPENDING ON? identifier)? | MORE_LABELS)  ;

ifStatement : IF condition THEN? (statementList (NEXT SENTENCE)? | NEXT SENTENCE) (ELSE (statementList (NEXT SENTENCE)? | NEXT SENTENCE))? END_IF?  ;

initializeStatement : INITIALIZE (identifier COMMACHAR?)+ (REPLACING ((ALPHABETIC | ALPHANUMERIC | NUMERIC | ALPHANUMERIC_EDITED | NUMERIC_EDITED | DBCS | EGCS) DATA? BY (identifier | literal COMMACHAR?))+)?  ;

inspectStatement : INSPECT identifier (tallyingPhrase | convertingPhrase | replacingPhrase)  ;

tallyingPhrase : TALLYING (identifier FOR (CHARACTERS beforeAfterPhrase* | (ALL | LEADING) ((identifier | literal) beforeAfterPhrase*)+)+)+ replacingPhrase?  ;

convertingPhrase : CONVERTING (identifier | literal) TO (identifier | literal) beforeAfterPhrase*  ;

replacingPhrase : REPLACING (CHARACTERS BY (identifier | literal) beforeAfterPhrase* | (ALL | LEADING | FIRST) ((identifier | literal) BY (identifier | literal) beforeAfterPhrase*)+)+  ;

beforeAfterPhrase : (BEFORE | AFTER) INITIAL? (identifier | literal)  ;

mergeStatement : MERGE fileName (ON? (ASCENDING | DESCENDING) KEY? (qualifiedDataName COMMACHAR?)+)+ (COLLATING? SEQUENCE IS? alphabetName)? USING fileName (COMMACHAR? fileName)+ (OUTPUT PROCEDURE IS? procedureName ((THROUGH | THRU) procedureName)? | GIVING fileName+)  ;

moveStatement : MOVE ((identifier | literal) TO (identifier COMMACHAR?)+ | (CORRESPONDING | CORR) identifier TO (identifier COMMACHAR?)+)  ;

multiplyStatement : MULTIPLY multiplyBody (ON? SIZE ERROR statementList)? (NOT ON? SIZE ERROR statementList)? END_MULTIPLY?  ;

multiplyBody : idOrLiteral BY (idOrLiteral GIVING arithIdentifierList | arithIdentifierList)  ;

openStatement : OPEN (INPUT (fileName (REVERSED | WITH? NO REWIND)? COMMACHAR?)+ | OUTPUT (fileName (WITH? NO REWIND)? COMMACHAR?)+ | I_O (fileName COMMACHAR?)+ | EXTEND (fileName COMMACHAR?)+)+  ;

performStatement : PERFORM performBody  ;

performBody : (performOption? statementList? END_PERFORM | performProcedure performOption?)  ;

performProcedure : procedureName ((THRU | THROUGH) procedureName)?  ;

beforeOrAfter : (BEFORE | AFTER)  ;

performOption : ((identifier | literal) TIMES | performTest? UNTIL condition | performTest? VARYING performVaryingList)  ;

performTest : WITH? TEST beforeOrAfter  ;

performVaryingList : performVarying (AFTER performVarying COMMACHAR?)*  ;

performVarying : identifier FROM idOrLiteral BY idOrLiteral UNTIL condition  ;

readStatement : READ fileName NEXT? RECORD? (INTO identifier)? (KEY IS? qualifiedDataName)? (INVALID KEY? statementList)? (NOT INVALID KEY? statementList)? (AT? END statementList)? (NOT AT? END statementList)? END_READ?  ;

releaseStatement : RELEASE recordName (FROM qualifiedDataName)?  ;

returnStatement : RETURN fileName RECORD? (INTO qualifiedDataName)? AT? END statementList (NOT AT? END statementList)? END_RETURN?  ;

rewriteStatement : REWRITE recordName (FROM identifier)? (INVALID KEY? statementList)? (NOT INVALID KEY? statementList)? END_REWRITE?  ;

searchStatement : SEARCH ALL? qualifiedDataName (VARYING qualifiedDataName)? (AT? END statementList)? (WHEN condition (statementList | NEXT SENTENCE))+ END_SEARCH?  ;

setStatement : SET ((identifier COMMACHAR?)+ (TO (identifier | TRUE | FALSE | ON | OFF | literal) | (UP | DOWN) BY? (identifier | literal)))+  ;

sortStatement : SORT fileName (ON? (ASCENDING | DESCENDING) KEY? (qualifiedDataName COMMACHAR?)+)+ (WITH? DUPLICATES IN? ORDER?)? (COLLATING? SEQUENCE IS? alphabetName)? (USING fileName+ | INPUT PROCEDURE IS? procedureName ((THROUGH | THRU) procedureName)?) (GIVING fileName+ | OUTPUT PROCEDURE IS? procedureName ((THROUGH | THRU) procedureName)?)  ;

startStatement : START fileName (KEY IS? (EQUAL TO? | EQUALCHAR | GREATER THAN? OR EQUAL TO? | GREATER THAN? | MORETHANCHAR | NOT LESS THAN? | NOT LESSTHANCHAR | MORETHANOREQUAL) qualifiedDataName)? (INVALID KEY? statementList)? (NOT INVALID KEY? statementList)? END_START?  ;

stopStatement : STOP (RUN | literal)  ;

stringStatement : STRING ((identifier | literal)+ (DELIMITED BY? (identifier | literal | SIZE))?)+ INTO identifier (WITH? POINTER qualifiedDataName)? (ON? OVERFLOW statementList)? (NOT ON? OVERFLOW statementList)? END_STRING?  ;

subtractStatement : SUBTRACT (idOrLiteralList FROM (idOrLiteral GIVING arithIdentifierList | arithIdentifierList) | (CORRESPONDING | CORR) qualifiedDataName FROM qualifiedDataName) (ON? SIZE ERROR statementList)? (NOT ON? SIZE ERROR statementList)? END_SUBTRACT?  ;

unstringStatement : UNSTRING identifier (DELIMITED BY? ALL? (identifier | literal) (OR ALL? (identifier | literal))*)? INTO (identifier (DELIMITER IN? identifier)? (COUNT IN? identifier)? COMMACHAR?)+ (WITH? POINTER qualifiedDataName)? (TALLYING IN? qualifiedDataName)? (ON? OVERFLOW statementList)? (NOT ON? OVERFLOW statementList)? END_UNSTRING?  ;

useStatement : USE (FOR? DEBUGGING ON? ((identifier | ALL REFERENCES? OF? identifier | fileName | procedureName)+ | ALL PROCEDURES) | GLOBAL? AFTER STANDARD? ((EXCEPTION | ERROR) | (BEGINNING | ENDING)? (FILE | REEL | UNIT)? LABEL) PROCEDURE ON? ((fileName COMMACHAR?)+ | INPUT | OUTPUT | I_O | EXTEND))  ;

writeStatement : WRITE recordName (FROM (identifier | literal))? advancingPhrase? (AT? (END_OF_PAGE | EOP) statementList)? (NOT AT? (END_OF_PAGE | EOP) statementList)? (INVALID KEY? statementList)? (NOT INVALID KEY? statementList)? END_WRITE?  ;

advancingPhrase : (BEFORE | AFTER) ADVANCING? (PAGE | (identifier | integerConstant | figurativeConstant) (LINE | LINES)? | mnemonicName)  ;

s_Identifier : S_IDENTIFIER  ;

s_Quoted_Identifier : S_QUOTED_IDENTIFIER  ;

s_Char_Literal : S_CHAR_LITERAL  ;

sQLStatement : (sQLCloseStatement | commitStatement | sQLUsingDMLReturn? (sQLDeleteStatement | insertStatement | updateStatement) | fetchStatement | lockTableStatement | sQLOpenStatement | rollbackStatement | savepointStatement | queryStatement | sQLSetStatement)  ;

sQLCloseStatement : K_CLOSE relObjectName  ;

commitStatement : K_COMMIT K_WORK? (K_COMMENT s_Char_Literal)?  ;

fetchStatement : (K_FOR (relObjectName | S_BIND))? K_FETCH relObjectName K_INTO (relObjectName (K_INDICATOR? S_BIND)? | indicatorBind) (COMMACHAR (relObjectName (K_INDICATOR? S_BIND)? | indicatorBind))*  ;

indicatorBind : S_BIND (K_INDICATOR? S_BIND)?  ;

lockTableStatement : K_LOCK K_TABLE tableReference (COMMACHAR tableReference)* K_IN lockMode K_MODE K_NOWAIT?  ;

sQLOpenStatement : K_OPEN relObjectName (K_USING arguments)?  ;

rollbackStatement : K_ROLLBACK K_WORK? (K_TO K_SAVEPOINT? relObjectName)? (K_COMMENT s_Char_Literal)?  ;

setTransactionStatement : K_SET K_TRANSACTION (K_READ (K_ONLY | K_WRITE) | K_USE K_ROLLBACK K_SEGMENT relObjectName)  ;

setVariableStatement : K_SET relObjectName (K_TO | EQUALCHAR) arguments  ;

sQLSetStatement : (setTransactionStatement | setVariableStatement)  ;

lockMode : (K_ROW (K_SHARE | K_EXCLUSIVE) | K_SHARE (K_UPDATE | K_ROW K_EXCLUSIVE)? | K_EXCLUSIVE)  ;

savepointStatement : K_SAVEPOINT relObjectName  ;

updateStatement : K_UPDATE tableReference relObjectName? K_SET columnValues (K_WHERE (sQLExpression | K_CURRENT K_OF relObjectName))?  ;

columnValues : tableColumn EQUALCHAR updatedValue (COMMACHAR tableColumn EQUALCHAR updatedValue)*  ;

updatedValue : (LPARENCHAR selectStatement RPARENCHAR | plSqlExpression)  ;

insertStatement : K_INSERT K_INTO tableReference (LPARENCHAR tableColumn (COMMACHAR tableColumn)* RPARENCHAR)? (K_VALUES LPARENCHAR plSqlExpressionList RPARENCHAR | selectStatement)  ;

sQLUsingDMLReturn : K_USING (S_IDENTIFIER | S_BIND)  ;

sQLDeleteStatement : K_DELETE K_FROM? tableReference relObjectName? (K_WHERE (sQLExpression | K_CURRENT K_OF relObjectName))?  ;

queryStatement : selectStatement  ;

plSqlExpression : plSqlExpressions  ;

plSqlExpressions : (plSqlOrExpression | plSqlAndExpressions)  ;

plSqlOrExpression : plSqlAndExpressions (K_OR plSqlAndExpressions)+  ;

plSqlAndExpressions : (plSqlAndExpression | plSqlUnaryLogicalExpressions)  ;

plSqlAndExpression : plSqlUnaryLogicalExpressions (K_AND plSqlUnaryLogicalExpressions)+  ;

plSqlUnaryLogicalExpressions : (plSqlUnaryLogicalExpression | plSqlRelationalExpressions)  ;

plSqlUnaryLogicalExpression : K_NOT plSqlRelationalExpressions  ;

plSqlRelationalExpressions : (plSqlRelationalExpression | plSqlSimpleExpressions)  ;

plSqlRelationalExpression : plSqlSimpleExpressions (relop plSqlSimpleExpressions | plSqlInClause | plSqlBetweenClause | plSqlLikeClause | isNullClause)  ;

plSqlExpressionList : plSqlExpression (COMMACHAR plSqlExpression)*  ;

plSqlInClause : K_NOT? K_IN LPARENCHAR plSqlExpressionList RPARENCHAR  ;

plSqlBetweenClause : K_NOT? K_BETWEEN plSqlSimpleExpressions K_AND plSqlSimpleExpressions  ;

plSqlLikeClause : K_NOT? K_LIKE plSqlSimpleExpressions  ;

isNullClause : K_IS K_NOT? K_NULL  ;

plSqlSimpleExpression : plSqlSimpleExpressions  ;

plSqlSimpleExpressions : (plSqlAdditiveExpression | plSqlMultiplicativeExpressions)  ;

plSqlAdditiveExpression : plSqlMultiplicativeExpressions (((PLUSCHAR | PLUSCHAR_SUBS) | (MINUSCHAR | MINUSCHAR_SUBS) | CONCAT) plSqlMultiplicativeExpressions)+  ;

plSqlMultiplicativeExpressions : (plSqlMultiplicativeExpression | plSqlExpotentExpressions)  ;

plSqlMultiplicativeExpression : plSqlExpotentExpressions ((ASTERISKCHAR | SLASHCHAR) plSqlExpotentExpressions)+  ;

plSqlExpotentExpressions : (plSqlExpotentExpression | plSqlUnaryExpressions)  ;

plSqlExpotentExpression : plSqlUnaryExpressions (POW plSqlUnaryExpressions)+  ;

plSqlUnaryExpressions : (plSqlUnaryExpression | plSqlPrimaryExpression)  ;

plSqlUnaryExpression : ((PLUSCHAR | PLUSCHAR_SUBS) | (MINUSCHAR | MINUSCHAR_SUBS)) plSqlPrimaryExpression  ;

plSqlPrimaryExpression : (K_NULL | relObjectName ('%FOUND' | '%NOTFOUND' | '%ISOPEN' | '%ROWCOUNT') | relObjectName LPARENCHAR arguments RPARENCHAR | relObjectName (DOTCHAR dotObjectName)? | K_SQL ('%FOUND' | '%NOTFOUND' | '%ISOPEN' | '%ROWCOUNT') | S_NUMBER | indicatorBind | LPARENCHAR plSqlExpression RPARENCHAR)  ;

tableColumn : relObjectName (DOTCHAR dotObjectName (DOTCHAR dotObjectName)?)?  ;

relObjectName : (S_IDENTIFIER | S_QUOTED_IDENTIFIER | S_CHAR_LITERAL)  ;

dotObjectName : (S_IDENTIFIER | S_QUOTED_IDENTIFIER | S_CHAR_LITERAL)  ;

oracleObjectName : (S_IDENTIFIER | S_QUOTED_IDENTIFIER)  ;

relop : (EQUALCHAR | JAVA_NE | '#' | NOTEQUALCHAR | MORETHANCHAR | MORETHANOREQUAL | LESSTHANCHAR | LESSTHANOREQUAL)  ;

tableReference : relObjectName (SLASHCHAR dotObjectName)?  ;

numOrID : (S_IDENTIFIER | ((PLUSCHAR | PLUSCHAR_SUBS) | (MINUSCHAR | MINUSCHAR_SUBS))? S_NUMBER)  ;

arguments : plSqlExpressionList  ;

selectStatement : selectWithoutOrder orderByClause? forUpdateClause?  ;

selectWithoutOrder : K_SELECT (K_ALL | K_DISTINCT)? selectList intoClause? fromClause whereClause? connectClause? groupByClause? setClause?  ;

selectList : (ASTERISKCHAR | selectItem (COMMACHAR selectItem)*)  ;

selectItem : (selectAllItems | sQLSimpleExpression S_IDENTIFIER? | functionCall asObjectName? | selectAllItems | tableColumn asObjectName?)  ;

selectAllItems : (relObjectName DOTCHAR ASTERISKCHAR | relObjectName DOTCHAR dotObjectName DOTCHAR ASTERISKCHAR)  ;

asObjectName : (relObjectName | K_AS dotObjectName)  ;

intoClause : K_INTO intoItem (COMMACHAR intoItem)*  ;

intoItem : (relObjectName (DOTCHAR dotObjectName)? | indicatorBind)  ;

fromClause : K_FROM fromItem (COMMACHAR fromItem)*  ;

fromItem : (tableReference | LPARENCHAR fromItemExpression RPARENCHAR) (joinerExpression (K_AS asObjectName)? | asObjectName?)  ;

fromItemExpression : ((tableReference | LPARENCHAR fromItemExpression RPARENCHAR) joinerExpression* | selectStatement)  ;

joinerExpression : (K_JOIN tableReference joinWhereClause? | relObjectName K_JOIN tableReference joinWhereClause?)  ;

joinWhereClause : K_ON sQLExpression  ;

whereClause : K_WHERE sQLExpression  ;

connectClause : (K_START K_WITH sQLExpression)? K_CONNECT K_BY sQLExpression (K_START K_WITH sQLExpression)?  ;

groupByClause : K_GROUP K_BY sQLExpressionList (K_HAVING sQLExpression)?  ;

setClause : (K_UNION K_ALL? | K_INTERSECT | K_MINUS) (LPARENCHAR selectStatement RPARENCHAR | selectStatement)  ;

orderByClause : K_ORDER K_BY sQLSimpleExpression (K_ASC | K_DESC)? (COMMACHAR sQLSimpleExpression (K_ASC | K_DESC)?)*  ;

forUpdateClause : K_FOR K_UPDATE (K_OF tableColumn (COMMACHAR tableColumn)*)?  ;

sQLExpression : sQLOrExpressions  ;

sQLOrExpressions : (sQLOrExpression | sQLAndExpressions)  ;

sQLOrExpression : sQLAndExpressions (K_OR sQLAndExpressions)+  ;

sQLAndExpressions : (sQLAndExpression | sQLUnaryLogicalExpressions)  ;

sQLAndExpression : sQLUnaryLogicalExpressions (K_AND sQLUnaryLogicalExpressions)+  ;

sQLUnaryLogicalExpressions : (existsClause | sQLRelationalExpressions)  ;

existsClause : K_NOT? K_EXISTS LPARENCHAR subQuery RPARENCHAR  ;

sQLRelationalExpressions : (sQLRelationalExpression | (sQLRelopExpression | LPARENCHAR sQLExpressionList RPARENCHAR | (sQLPriorExpression | sQLSimpleExpressions)))  ;

sQLRelationalExpression : (LPARENCHAR sQLExpressionList RPARENCHAR | (sQLPriorExpression | sQLSimpleExpressions)) (sQLInClause | sQLBetweenClause | sQLLikeClause | isNullClause)  ;

sQLPriorExpression : K_NOT? K_PRIOR sQLSimpleExpressions  ;

sQLExpressionList : sQLSimpleExpression (COMMACHAR sQLSimpleExpression)*  ;

sQLRelopExpression : (LPARENCHAR sQLExpressionList RPARENCHAR | (sQLPriorExpression | sQLSimpleExpressions)) relop ((K_ALL | K_ANY)? LPARENCHAR subQuery RPARENCHAR | sQLPriorExpression | sQLSimpleExpressions)  ;

sQLRelationalOperatorExpression : relop ((K_ALL | K_ANY)? LPARENCHAR subQuery RPARENCHAR | sQLPriorExpression | sQLSimpleExpression)  ;

sQLInClause : K_NOT? K_IN LPARENCHAR (sQLExpressionList | subQuery) RPARENCHAR  ;

sQLBetweenClause : K_NOT? K_BETWEEN sQLSimpleExpression K_AND sQLSimpleExpression  ;

sQLLikeClause : K_NOT? K_LIKE sQLSimpleExpression  ;

sQLSimpleExpression : sQLSimpleExpressions  ;

sQLSimpleExpressions : sQLAdditiveExpressions  ;

sQLAdditiveExpressions : (sQLAdditiveExpression | sQLMultiplicativeExpressions)  ;

sQLAdditiveExpression : sQLMultiplicativeExpressions (((PLUSCHAR_SUBS | PLUSCHAR) | (MINUSCHAR_SUBS | MINUSCHAR) | CONCAT) sQLMultiplicativeExpressions)+  ;

sQLMultiplicativeExpressions : (sQLMultiplicativeExpression | sQLExpotentExpressions)  ;

sQLMultiplicativeExpression : sQLExpotentExpressions ((ASTERISKCHAR | SLASHCHAR) sQLExpotentExpressions)+  ;

sQLExpotentExpressions : (sQLExpotentExpression | sQLUnaryExpressions)  ;

sQLExpotentExpression : sQLUnaryExpressions (POW sQLUnaryExpressions)+  ;

sQLUnaryExpressions : (sQLUnaryExpression | sQLPrimaryExpression)  ;

sQLUnaryExpression : ((PLUSCHAR | PLUSCHAR_SUBS) | (MINUSCHAR | MINUSCHAR_SUBS)) sQLPrimaryExpression  ;

sQLPrimaryExpression : (K_NULL | functionCall | outerJoinExpression | tableColumn | S_NUMBER | indicatorBind | LPARENCHAR sQLExpression RPARENCHAR)  ;

functionCall : relObjectName (DOTCHAR dotObjectName (DOTCHAR dotObjectName)?)? LPARENCHAR ((K_DISTINCT | K_ALL)? (sQLArguments | ASTERISKCHAR))? RPARENCHAR  ;

sQLArguments : sQLExpressionList  ;

outerJoinExpression : relObjectName (DOTCHAR dotObjectName (DOTCHAR dotObjectName)?)? LPARENCHAR (PLUSCHAR | PLUSCHAR_SUBS) RPARENCHAR  ;

subQuery : selectWithoutOrder  ;

cobolWord : COBOL_WORD  ;

integerConstant : (LEVEL_66 | LEVEL_77 | LEVEL_78 | LEVEL_88 | LEVEL_NUMBER | INTEGER | COMMA_INTEGER)  ;

numericConstant : (PLUSCHAR | MINUSCHAR)? (integerConstant DOTCHAR integerConstant? | DOTCHAR integerConstant | integerConstant)  ;

levelNumber : LEVEL_NUMBER  ;

figurativeConstant : (ZERO | ZEROS | ZEROES | SPACE | SPACES | HIGH_VALUE | HIGH_VALUES | LOW_VALUE | LOW_VALUES | QUOTE | QUOTES | NULL | NULLS)  ;

nonNumericConstant : (QUOTEDSTRING | HEXNUMBER)  ;

literal : ALL? (nonNumericConstant | numericConstant | figurativeConstant | intrinsicFunction | specialRegister | LINAGE_COUNTER ((IN | OF) fileName)?)  ;

condition : combinableCondition ((AND | OR) (combinableCondition | abbreviationRest))*  ;

combinableCondition : NOT? simpleCondition  ;

simpleCondition : (classCondition | relationCondition | conditionNameCondition | LPARENCHAR condition RPARENCHAR)  ;

classCondition : identifier IS? NOT? (NUMERIC | ALPHABETIC | ALPHABETIC_LOWER | ALPHABETIC_UPPER | className | DBCS | KANJI)  ;

conditionNameCondition : conditionNameReference  ;

relationCondition : arithmeticExpression (relationalOperator arithmeticExpression | signCondition)  ;

signCondition : IS? NOT? (POSITIVE | NEGATIVE | (ZERO | ZEROS | ZEROES))  ;

relationalOperator : IS? NOT? (GREATER THAN? OR EQUAL TO? | MORETHANOREQUAL | LESS THAN? OR EQUAL TO? | LESSTHANOREQUAL | GREATER THAN? | MORETHANCHAR | LESS THAN? | LESSTHANCHAR | (EQUAL | EQUALS) TO? | EQUALCHAR TO? | NOTEQUALCHAR)  ;

abbreviationRest : (NOT? relationalOperator? abbreviationLeaf)+  ;

abbreviationLeaf : (arithmeticExpression | LPARENCHAR arithmeticExpression abbreviationRest RPARENCHAR)  ;

procedureName : (paragraphName ((IN | OF) sectionName)? | sectionName)  ;

identifier : (qualifiedDataName (LPARENCHAR subscript (COMMACHAR? subscript)* RPARENCHAR)* (LPARENCHAR leftmostCharacterPosition COLONCHAR length? RPARENCHAR)? | RETURN_CODE)  ;

qualifiedDataName : dataName ((IN | OF) dataName)* ((IN | OF) fileName)?  ;

intrinsicFunction : FUNCTION (F_ACOS | F_ANNUITY | F_ASIN | F_ATAN | F_CHAR | F_COS | F_CURRENT_DATE | F_DATE_OF_INTEGER | F_DATE_TO_YYYYMMDD | F_DATEVAL | F_DAY_OF_INTEGER | F_DAY_TO_YYYYDDD | F_DISPLAY_OF | F_FACTORIAL | F_INTEGER | F_INTEGER_OF_DATE | F_INTEGER_OF_DAY | F_INTEGER_PART | F_LENGTH | F_LOG | F_LOG10 | F_LOWER_CASE | F_MAX | F_MEAN | F_MEDIAN | F_MIDRANGE | F_MIN | F_MOD | F_NATIONAL_OF | F_NUMVAL | F_NUMVAL_C | F_ORD | F_ORD_MAX | F_ORD_MIN | F_PRESENT_VALUE | F_RANDOM | F_RANGE | F_REM | F_REVERSE | F_SIN | F_SQRT | F_STANDARD_DEVIATION | F_SUM | F_TAN | F_UNDATE | F_UPPER_CASE | F_VARIANCE | F_WHEN_COMPILED | F_YEAR_TO_YYYY | F_YEARWINDOW) (LPARENCHAR (qualifiedDataName LPARENCHAR (ALL COMMACHAR?)+ RPARENCHAR | functionArgument (COMMACHAR? functionArgument)*)? RPARENCHAR)?  ;

functionArgument : (identifier | literal | arithmeticExpression)  ;

length : arithmeticExpression  ;

leftmostCharacterPosition : arithmeticExpression  ;

conditionNameReference : conditionName (((IN | OF) dataName)* ((IN | OF) fileName)? (LPARENCHAR subscript (COMMACHAR? subscript)* RPARENCHAR)* | ((IN | OF) mnemonicName)*)  ;

subscript : (((PLUSCHAR_SUBS | PLUSCHAR) | (MINUSCHAR_SUBS | MINUSCHAR))? integerConstant | qualifiedDataName ((PLUSCHAR_SUBS | MINUSCHAR_SUBS) integerConstant)? | indexName ((PLUSCHAR_SUBS | MINUSCHAR_SUBS) integerConstant)?)  ;

modeOf : cobolWord  ;

alphabetName : cobolWord  ;

className : cobolWord  ;

conditionName : cobolWord  ;

dataName : cobolWord  ;

fileName : cobolWord  ;

indexName : cobolWord  ;

mnemonicName : cobolWord  ;

recordName : qualifiedDataName  ;

routineName : cobolWord  ;

symbolicCharacter : cobolWord  ;

libraryName : cobolWord  ;

programName : cobolWord  ;

cdName : cobolWord  ;

sectionName : (LEVEL_66 | LEVEL_77 | LEVEL_78 | LEVEL_88 | LEVEL_NUMBER | INTEGER | cobolWord)  ;

paragraphName : (LEVEL_66 | LEVEL_77 | LEVEL_78 | LEVEL_88 | LEVEL_NUMBER | INTEGER | cobolWord)  ;

systemName : cobolWord  ;

computerName : systemName  ;

languageName : systemName  ;

environmentName : systemName  ;

assignmentName : systemName  ;

basisName : programName  ;

specialRegister : (ADDRESS OF dataName | LENGTH OF identifier | DEBUG_LINE | DEBUG_NAME | DEBUG_CONTENTS | DEBUG_ITEM | DEBUG_SUB_1 | DEBUG_SUB_2 | DEBUG_SUB_3 | RETURN_CODE | SHIFT_OUT | SHIFT_IN | SORT_CONTROL | SORT_CORE_SIZE | SORT_FILE_SIZE | SORT_MESSAGE | SORT_MODE_SIZE | SORT_RETURN | TALLY | WHEN_COMPILED)  ;

arithmeticExpression : timesDiv (((PLUSCHAR_SUBS | PLUSCHAR) | (MINUSCHAR_SUBS | MINUSCHAR)) timesDiv)*  ;

timesDiv : power ((ASTERISKCHAR | SLASHCHAR) power)*  ;

power : ((PLUSCHAR_SUBS | PLUSCHAR) | (MINUSCHAR_SUBS | MINUSCHAR))? basis (POW basis)*  ;

basis : (identifier | literal | LPARENCHAR arithmeticExpression RPARENCHAR)  ;

commentLine : (COMMENT2 DOT2?)+  ;
