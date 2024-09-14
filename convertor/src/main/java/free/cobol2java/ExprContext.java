package free.cobol2java;

import free.cobol2java.antlr.CobolWithSqlLexer;
import free.cobol2java.antlr.CobolWithSqlParser;
import free.cobol2java.config.CobolConfig;
import free.cobol2java.sql.SqlStatement;
import free.cobol2java.sql.handler.CobolSqlVisitor;
import free.cobol2java.util.CobolConstant;
import free.cobol2java.util.ExprUtil;
import free.cobol2java.util.RelationalOperator;
import free.servpp.multiexpr.IEvaluatorEnvironment;
import io.proleap.cobol.CobolLexer;
import io.proleap.cobol.CobolParser;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.TerminalNode;

import java.io.File;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * @author lidong@date 2024-08-12@version 1.0
 */
public class ExprContext {
    private IEvaluatorEnvironment environment;
    private ExprContext copybookContext;
    private ExprContext orMappingContext;
    private Map<String, String> javaFieldToType = new HashMap<>();
    private Map<String, String> fieldToClassType = new HashMap<>();
    private Map<String, String> javaFieldToQualifiedName = new HashMap<>();
    private Map<String, String> javaDimFieldToQualifiedName = new HashMap<>();
    /**
     * ======One dimension array=====
     * 01 WS-ARRAYS.
     *    05 WS-NUMBER-ARRAY.
     *        10 WS-NUMBER-ITEM OCCURS 10 TIMES PIC 9(3) VALUE 0.
     * wsArrays.wsNumberArray.wsNumberItem -> 0,10
     * =======Multiple dimension array========
     * 01 WS-TABLE1.
     *    05 WS-A1 OCCURS 10 TIMES.
     *        10 WS-B1 PIC A(10).
     *        10 WS-C1 OCCURS 5 TIMES.
     *            15 WS-D1 PIC X(6).
     * wsTable1.wsA1.wsC1.wsD1 -> 10,5,0 (wsTable1.wsA1[0~9].wsC1[0~5].wsD1)
     * wsTable1.wsA1.wsC1 -> 10,5,0
     * wsTable1.wsA1.wsB1 -> 10,0 (wsTable1.wsA1[0~9].wsB1)
     */
    private Map<String, String> qualifiedNameToDimLevel = new HashMap<>();
    private Map<String,String> copybookFirstNameToFileName = new HashMap<>();
    private Stack<String> clsLevel = new Stack<>();
    private Stack<Integer> curDims = new Stack<>();
    private Stack<Object> variables = new Stack<>();

    public ExprContext getCopybookContext() {
        return copybookContext;
    }

    public void setCopybookContext(ExprContext copybookExprContext) {
        this.copybookContext = copybookExprContext;
    }

    public ExprContext getOrMappingContext() {
        return orMappingContext;
    }

    public void setOrMappingContext(ExprContext orMappingContext) {
        this.orMappingContext = orMappingContext;
    }

    public void setEnvironment(IEvaluatorEnvironment environment) {
        this.environment = environment;
    }

    public Object var_push(Object var){
        return variables.push(var);
    }

    public Object var_pop(){
        return variables.pop();
    }

    public Object var_peek(){
        return variables.peek();
    }

    public int dim_push(Number dim) {
        return curDims.push(dim.intValue());
    }

    public int dim_pop() {
        return curDims.pop();
    }

    public int dim_peek() {
        return curDims.peek();
    }

    public int dim_size() {
        return curDims.size();
    }

    public String dim_putQlfLevel(String fieldName, String dimLevelStr) {
        String qlfName = javaFieldToQualifiedName.get(fieldName);
        if (qlfName == null || "null".equals(qlfName)) {
            String[] dims = dimLevelStr.split(",");
            if (dims[dims.length - 1].equals("0"))
                return null;
        }
        qualifiedNameToDimLevel.put(qlfName, dimLevelStr);
        String realQlfName = dimLevelStr.lastIndexOf(",0") != -1 ?
                qlfName.substring(0, qlfName.lastIndexOf('.')) : (qlfName != null ? qlfName : fieldName);
        return qualifiedNameToDimLevel.put(realQlfName, dimLevelStr);
    }

    private void makeQlfNameAllLevel(String qlfName) {
        String[] names = qlfName.split("\\.");
        for (String name : names) {
            javaDimFieldToQualifiedName.put(name, qlfName);
        }
    }

    public String dim_getQlfLevel(String qlfName) {
        return qualifiedNameToDimLevel.get(qlfName);
    }

    public String dim_value() {
        String ret = "";
        for (int i = 0; i < curDims.size(); i++) {
            if (i != 0)
                ret += ",";
            ret += curDims.get(i);
        }
        return ret;
    }

    public String name_toClass(String cblName) {
        cblName = cblName.replace("\"","");
        String[] parts = cblName.split("-");
        String ret = "";
        for (int i = 0; i < parts.length; i++) {
            String sp = parts[i].toLowerCase();
            ret += capitalizeFirstLetter(sp);
        }
        return ret;
    }

    public String name_toField(String cblName) {
        String[] parts = cblName.split("-");
        String ret = parts[0].toLowerCase();
        for (int i = 1; i < parts.length; i++) {
            String sp = parts[i].toLowerCase();
            ret += capitalizeFirstLetter(sp);
        }

        return digitalStart(ret);
    }

    private String digitalStart(String name) {
        if (name != null && name.length() > 0 && name.charAt(0) >= '0' && name.charAt(0) <= '9')
            return "m_" + name;
        return name;
    }

    public String name_getFieldType(String fieldName) {
        String ret = javaFieldToType.get(fieldName);
        return ret == null ? name_getFieldClsType(fieldName) : ret;
    }

    public String name_getFieldClsType(String fieldName) {
        return fieldToClassType.get(fieldName);
    }

    public String name_setFieldType(String fieldName, String type) {
        return javaFieldToType.put(fieldName, type);
    }

    public String name_setFieldClsType(String fieldName, String type) {
        return fieldToClassType.put(fieldName, type);
    }

    public String type_getType(String cblType) {
        boolean bString = cblType.indexOf("X(") != -1 || cblType.indexOf("A(") != -1;
        if (bString)
            return "String";
        boolean bFloat = cblType.indexOf("V") != -1;
        if (bFloat)
            return "Double";
        return "Integer";
    }

    public String name_enterClass(String fieldName) {
        return clsLevel.push(fieldName);
    }

    public String name_exitClass() {
        return clsLevel.pop();
    }

    public String name_putInnerField(String fieldName) {
        String qualifiedName = clsLevel.size() == 0 ? null : createQualifedName(fieldName);
        if (qualifiedName != null) {
            javaFieldToQualifiedName.put(fieldName, qualifiedName);
            makeQlfNameAllLevel(qualifiedName);

        }else{
            javaFieldToQualifiedName.put(fieldName, fieldName);
            qualifiedName = fieldName;
        }
        return qualifiedName;
    }

    public String name_delegateName(String fieldName) {
        String ret = javaFieldToQualifiedName.get(fieldName);
        if (ret == null && copybookContext != null)
            ret = copybookContext.name_delegateName(fieldName);
        else if (ret == null)
            ret = fieldName;
        return name_delegateName1(ret, null);
    }

    public String name_delegateName1(String fieldName, String dimStr) {
        String ret = fieldName;
        if (dimStr != null) {
            String qlfName = javaDimFieldToQualifiedName.get(ret);
            ret = qlfName != null ? qlfName : ret;
            String dimLevelStr = dim_getQlfLevel(ret);
            String[] dims = dimStr.split(",");
            String[] names = ret.split("\\.");
            String[] level = dimLevelStr.split(",");
            if (level.length == names.length) {
                String[] lv = new String[names.length - 1];
                System.arraycopy(level, 0, lv, 0, lv.length);
                level = lv;
            }
            ret = "";
            int usedDim = 0;
            for (int i = 0; i < level.length; i++) {
                String node;
                if (level[level.length - 1 - i].equals("0")) {
                    node = names[names.length - 1 - i];
                } else {
                    node = names[names.length - 1 - i] + "[" + dims[dims.length - 1 - usedDim] + "]";
                    usedDim++;
                }
                if (i != 0) {
                    node += ".";
                }
                ret = node + ret;
            }
            if(level.length == 0){
                ret = names[0] + "[" + dims[0] + "]";
            }else {
                for (int i = level.length; i < names.length; i++) {
                    if (ret.length() == 0)
                        ret = names[names.length - i - 1];
                    else
                        ret = names[names.length - i - 1] + "." + ret;
                }
            }
        }
        return nestedQualifiedName(ret);
    }

    private String nestedQualifiedName(String qname){
        if(javaFieldToQualifiedName.get(qname) != null)
            return qname;
        String firstName = qname.split("\\.")[0];
        String fieldName = copybookFirstNameToFileName.get(firstName);
        String firstQName = fieldName == null ? javaFieldToQualifiedName.get(firstName) : javaFieldToQualifiedName.get(fieldName);
        String ret = qname;
        if(firstQName != null && !qname.equals(firstQName)){
            ret = firstQName + (qname.length() == firstName.length() ? "": qname.substring(firstName.length()));
        }
        return ret;
    }

    public String setCopyFirstNameToFieldName(String copyName, String fieldName){
        return copybookFirstNameToFileName.put(copyName,fieldName);
    }

    public String cobol_compile(String fileName){
        fileName = fileName.replace("\"","");
        ICobolConvertor cobolConvertor = CobolConfig.getCobolConvertor();
        List<File> files = new ArrayList<>();
        cobolConvertor.findFiles(new File(cobolConvertor.getSourcePath()),files,cobolConvertor.getSuffixes(),fileName+"*");
        if(files.size() > 0)
            return cobolConvertor.convertAFile(files.get(0)) ? "Success":"Error";
        else
            return "File " + fileName + " not found";
    }
    private String createQualifedName(String fieldName) {
        String ret = "";
        for (String superField : clsLevel) {
            ret += superField + ".";
        }
        return ret + fieldName;
    }

    public String getQualifedName(String fieldName) {
        return javaFieldToQualifiedName.get(fieldName);
    }

    public String rel_getOper(String cobolOper, String left, String right) {
        String ret = null;
        RelationalOperator oper = RelationalOperator.valueOf(cobolOper);
        switch (oper) {
            case GREATER:
                ret = left + ">" + right;
                break;
            case GREATER_OR_EQUAL:
                ret = left + ">=" + right;
                break;
            case LESS:
                ret = left + "<" + right;
                break;
            case LESS_OR_EQUAL:
                ret = left + "<=" + right;
                break;
            case EQUAL:
                ret = left + "==" + right;
                break;
            case NOT_EQUAL:
                ret = left + "!=" + right;
                break;
            case EQUALCHAR:
                ret = "Util.compare(" + left + "," + right + ") == 0";
                break;
            case LESSTHANCHAR:
                ret = "Util.compare(" + left + "," + right + ") < 0";
                break;
            case MORETHANCHAR:
                ret = "Util.compare(" + left + "," + right + ") > 0";
                break;
            case NOTEQUALCHAR:
                ret = "Util.compare(" + left + "," + right + ") != 0";
                break;
            case LESSTHANOREQUAL:
                ret = left + "<=" + right;
                break;
            case MORETHANOREQUAL:
                ret = left + ">=" + right;
                break;
        }
        return ret;
    }

    private record PropOfField(String id, String ofId) {
    }

    private String getCtxText(CobolParser.ArithmeticExpressionContext ctx,List< Object> ofIds){
        List<TerminalNode > list = new ArrayList<>();
        getAllTerm(ctx,list);
        boolean isOf = false;
        TerminalNode prevNode = null;
        for(TerminalNode node : list){
            String text = node.getText();
            if ("OF".equals(text)) {
                isOf = true;
            } else if(node.getSymbol().getType() == CobolLexer.IDENTIFIER) {
                if(isOf){
                    isOf = false;
                    String id = prevNode.getText();
                    String ofField = node.getText();
                    PropOfField propOfField = new PropOfField(id, ofField);
                    ofIds.add(propOfField);
                    prevNode = null;
                    continue;
                }else if(prevNode != null){
                    ofIds.add(prevNode.getText());
                }
                prevNode = node;
            }
        }
        if(prevNode != null)
            ofIds.add(prevNode.getText());
        String ret = "";
        String tmp = ctx.getText();
        for(int i = 0;i<ctx.getChildCount();i++){
            ret += " " + ctx.getChild(i).getText();
        }
        return ret;
    }

    private static void getAllTerm(ParseTree root,List<TerminalNode > list) {
        int count = root.getChildCount();
        for(int i = 0;i<count;i++){
            ParseTree node = root.getChild(i);
            if(node instanceof TerminalNode){
                list.add((TerminalNode) node);
            }else{
                getAllTerm(node,list);
            }
        }
    }

    public String expr_convertExpr(CobolParser.ArithmeticExpressionContext ctx) {
        List<Object> ofIds = new ArrayList<>();
        String cobolExpr = getCtxText(ctx, ofIds);
        String ret = cobolExpr.replace("**", "^");
        for (Object value: ofIds) {
            ret = calcID(value, cobolExpr, ret);
        }
        return ret.indexOf('^') != -1 ? ExprUtil.convertExpression(ret) : ret;
    }

    private String calcID(Object value, String cobolExpr, String ret) {
        String id = null;
        String fieldName = null;
        if(value instanceof String) {
            id = (String) value;
            fieldName = name_toField(id);
        }
        else {
            PropOfField propOfField = ((PropOfField)value);
            fieldName = name_toField(propOfField.id);
            id = propOfField.id + "OF" + propOfField.ofId;
        }
        if(CobolConstant.isConstant(id))
            return "CobolConstant." + id;
        String[] dims = getDimStringOfVar(cobolExpr, id);
        String dimStr = dims[1];
        if (dimStr == null) {
            ret = ret.replace(id, name_delegateName(fieldName));
        } else {
            int index = cobolExpr.indexOf(":");
            if (index != -1) {
                int right = cobolExpr.indexOf(')', index);
                int left = cobolExpr.lastIndexOf('(', index);
                String range = cobolExpr.substring(left + 1, right);
                ret = "Util.subvalue(" + fieldName + ",\"" + range + "\")";
            } else {
                String qlfNameWithDims = name_delegateName1(name_delegateName(fieldName), dimStr);
                ret = dims[0] + qlfNameWithDims + dims[2];
            }
        }
        return ret;
    }

    private String[] getDimStringOfVar(String cobolExpr, String var) {
        String[] ret = new String[3];
        int index = cobolExpr.indexOf(var);
        String sub = cobolExpr.substring(index + var.length()).trim();
        ret[0] = cobolExpr.substring(0, index);
        ret[2] = sub;
        if (sub.length() != 0 && sub.charAt(0) == '(') {
            int leftIndex = sub.indexOf(')');
            String dimString = sub.substring(1, leftIndex);
            ret[1] = dimString;
            ret[2] = sub.substring(leftIndex + 1);
        }
        return ret;
    }

    public String array_initString(String dims, String val) {
        String[] vals = dims.split(",");
        String str = val;
        for (int i = vals.length - 1; i >= 0; i--) {
            String arr = _initString(vals[i], str);
            str = arr;
        }
        return str;
    }

    public String cbl_getComment(int start, String ctxLine, List<String> lines) {
        String ret = null;
        for (int i = start - 2; i >= 0; i--) {
            String line = lines.get(i);
            String trim = line.trim();
            if (trim.startsWith("*>") && !trim.startsWith("*>EXECSQL")) {
                if (ret == null)
                    ret = "//" + trim.substring(2);
                else
                    ret += "\n//" + trim.substring(2);
            } else if (trim.length() != 0)
                break;
        }
        return ret;
    }

    public SqlStatement sql_exec(String sql){
        CobolWithSqlLexer lexer = new CobolWithSqlLexer(CharStreams.fromString(sql));
        CommonTokenStream tokens = new CommonTokenStream(lexer);
        CobolWithSqlParser parser = new CobolWithSqlParser(tokens);

        ParseTree tree = parser.cobolSql();

        CobolSqlVisitor visitor = new CobolSqlVisitor();
        SqlStatement statement = visitor.visit(tree);
        return statement;
    }
    private String _initString(String num, String str) {
        int n = Integer.parseInt(num);
        // Check for invalid input
        if (n <= 0 || str == null) {
            return "{}";
        }

        // Use a StringBuilder to construct the result efficiently
        StringBuilder result = new StringBuilder("{");

        // Append the string `str` `n` times, separated by commas
        for (int i = 0; i < n; i++) {
            result.append(str);
            if (i < n - 1) {
                result.append(",");
            }
        }

        // Close the curly braces
        result.append("}");

        // Convert StringBuilder to String and return
        return result.toString();
    }

    private String capitalizeFirstLetter(String str) {
        return str.substring(0, 1).toUpperCase() + str.substring(1);
    }

    private void debugPoint() {
        if(environment != null) {
            String debug = (String) environment.getVar("DEBUG");
            if ("program1".equals(debug))
                debug = debug;
        }
    }

}
