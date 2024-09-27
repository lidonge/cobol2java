package free.cobol2java;

import free.cobol2java.antlr.CobolWithSqlLexer;
import free.cobol2java.antlr.CobolWithSqlParser;
import free.cobol2java.config.CobolConfig;
import free.cobol2java.copybook.CopyBookManager;
import free.cobol2java.sql.SqlStatement;
import free.cobol2java.sql.handler.CobolSqlVisitor;
import free.cobol2java.util.CobolConstant;
import free.cobol2java.util.ExprUtil;
import free.cobol2java.util.RelationalOperator;
import free.servpp.multiexpr.IEvaluatorEnvironment;
import free.servpp.mustache.ILogable;
import io.proleap.cobol.CobolLexer;
import io.proleap.cobol.CobolParser;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.TerminalNode;

import java.io.File;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * @author lidong@date 2024-08-12@version 1.0
 */
public class ExprContext implements ILogable {
    public static final String LENGTHOF = "LENGTHOF";
    private IEvaluatorEnvironment environment;
    //    private ExprContext copybookContext;
    private Map<String, ExprContext> copybookContexts = CopyBookManager.getDefaultManager().getGlobalFunc();
    private ExprContext orMappingContext;
    private Map<String, String> javaQlfFieldToType = new HashMap<>();
    private Map<String, String> fieldToClassType = new HashMap<>();
    private Map<String, String> javaFieldToQualifiedName = new HashMap<>();
    private Map<String, String> javaFieldToQlfNameWithLeaf = new HashMap<>();
    private Map<String, Number> javaFieldNameToDim = new HashMap<>();
    private Map<String, String> copybookFirstNameToFileName = new HashMap<>();
    private Map<String, String> innerClsNameToCopybookName = new HashMap<>();
    private static Map<String, String> compiledCobol = new HashMap<>();
    /**
     * The field of nested class.
     * class A{
     * class B{
     * class C{
     * class D{
     * <p>
     * }
     * D d;
     * }
     * C c;
     * }
     * B b;
     * }
     * The stack should be [c,b]
     */
    private Stack<String> clsLevel = new Stack<>();
    private Stack<Integer> curDims = new Stack<>();
    private Stack<Object> variables = new Stack<>();

    public ExprContext getOrMappingContext() {
        return orMappingContext;
    }

    public void setOrMappingContext(ExprContext orMappingContext) {
        this.orMappingContext = orMappingContext;
    }

    public void setEnvironment(IEvaluatorEnvironment environment) {
        this.environment = environment;
    }

    public Object var_push(Object var) {
        return variables.push(var);
    }

    public Object var_pop() {
        return variables.pop();
    }

    public Object var_peek() {
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


    public Number dim_putFieldDim(String fieldName, Number dim) {
        return javaFieldNameToDim.put(fieldName, dim);
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
        return toClassName(cblName);
    }

    public static String toClassName(String cblName) {
        cblName = cblName.replace("\"", "");
        String[] parts = cblName.split("-");
        String ret = "";
        for (int i = 0; i < parts.length; i++) {
            String sp = parts[i].toLowerCase();
            ret += capitalizeFirstLetter(sp);
        }
        return ret;
    }

    public String name_toField(String cblName) {
        return toFieldName(cblName);
    }

    public static String toFieldName(String cblName) {
        String[] parts = cblName.split("-");
        String ret = parts[0].toLowerCase();
        for (int i = 1; i < parts.length; i++) {
            String sp = parts[i].toLowerCase();
            ret += capitalizeFirstLetter(sp);
        }

        return digitalStart(ret);
    }

    private static String digitalStart(String name) {
        if (name != null && name.length() > 0 && name.charAt(0) >= '0' && name.charAt(0) <= '9')
            return "m_" + name;
        return name;
    }

    public String name_getFieldType(String fieldName) {
        if (fieldName.equals("pceccii4")) {
            debugPoint();
        }
        String ret = javaQlfFieldToType.get(fieldName);
        if (ret != null && innerClsNameToCopybookName.get(ret) != null)
            ret = innerClsNameToCopybookName.get(ret);
        return ret == null ? name_getFieldClsType(fieldName) : ret;
    }

    public String name_getFieldClsType(String fieldName) {
        return fieldToClassType.get(fieldName);
    }

    public String name_setFieldType(String fieldName, String type) {
        return javaQlfFieldToType.put(fieldName, type);
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
        return name_putInnerField1(fieldName, null);
    }

    public String name_putInnerField1(String fieldName, String isSubCopybook) {
        if(fieldName.equals("pdbimain")){
            debugPoint();
        }
        String qualifiedName = null;
        if (isSubCopybook != null && !isSubCopybook.equals("null")) {
            qualifiedName = null;
        } else {
            qualifiedName = clsLevel.size() == 0 ? fieldName : createQualifedName(fieldName);
            javaFieldToQualifiedName.put(fieldName, qualifiedName);
            makeQlfNameAllLevel(qualifiedName);
        }
        return qualifiedName;
    }

    private void makeQlfNameAllLevel(String qlfName) {
        String[] names = qlfName.split("\\.");
        for (String name : names) {
            String lastName = javaFieldToQlfNameWithLeaf.get(name);
            if (lastName == null || lastName.length() < qlfName.length())
                javaFieldToQlfNameWithLeaf.put(name, qlfName);
        }
    }

    public String dim_udfCall(String ctxText) {
        String ret = null;
        int idx = ctxText.indexOf('(');
        if (idx != -1) {
            ret = ctxText.substring(idx + 1, ctxText.indexOf(')'));
        }
        return ret;
    }

    /**
     * Conditions:
     *  1. ofCopy == null
     *      1.1 fieldName is local variable, return the qlf_name of the fieldName in javaFieldToQualifiedName
     *      1.2 fieldName is in a copybook(of more but that is cobol error), get the context
     *          use the context, and the fieldName call 1.1
     *  2. ofCopy == a field, e.g. "account", ofCopyCls = find the classType in javaFieldToType
     *      2.0 ofCopyCls == null, ofCopy is a field of a copyfile
     *          2.0.1 take ofCopy as fieldName, ofCopy=null,
     *              then 1 get return of qlf_name of the ofCopy and the copyfile context
     *          2.0.2 use returned context, take fieldName and ofCopy=null call 1 get returned qlf_name
     *          2.0.3 merge two qlf_name
     *      2.1 ofCopyCls != null
     *          2.1.1 ofCopyCls is a real copyfile, it can be found in contexts by ofCopy value
     *              then 1.1
     *          2.1.2 ofCopyCls is an alias of copyfile, it can be found in innerClsNameToCopybookName by ofCopy Class name
     *              then become 2.1 and replace copyfile name by alias name of the return
     *          2.1.3 ofCopyCls is a local inner class, it can be found in javaFieldToQualifiedName by fieldName
     *              then become 1.1
     *          2.1.4 both 2.1.1 and 2.1.3, is cobol error
     *  3. ofCopy == a qualified name, e.g. "a.b.c"
     *      3.1 take "b" as fieldName and "c" is ofCopy, then 2 get returned qlf_name and context of "b"
     *      3.2 use the context of "b" take "a" as fieldName and ofCopy=null, call 1
     *      3.3 merge two name and call 3.2 recursive
     * @param fieldName
     * @param ofCopy
     * @return
     */
    public String name_qlfName(String fieldName, String ofCopy) {
        String ret = null;
        if(fieldName.equals("tblIdx")){
            debugPoint();
        }
        boolean isInCopy = ofCopy != null && ofCopy.length() != 0 && !"null".equals(ofCopy);
        if (isInCopy) {
            String[] ofIds = ofCopy.split("\\.");
            if (ofIds.length == 1) {
                String ofCopyField = name_toField(ofCopy);
                String qlfNameInCopybook = _getQlfName(fieldName, ofCopyField);
                if(qlfNameInCopybook == null){
                    debugPoint();
                }
                ret = name_qlfName(ofCopyField,null)+
                        qlfNameInCopybook.substring(qlfNameInCopybook.indexOf(ofCopyField) + ofCopyField.length());
            } else {
                for (int i = ofIds.length-1; i > 0; i--) {
                    String theFieldName = ofIds[i-1];
                    String theOfCopy = ofIds[i];
                    String qlfName = _getQlfName(theFieldName,theOfCopy);
                    if(ret == null){
                        ret = qlfName;
                    }else{
                        ret = ret + "." + qlfName;
                    }
                }
            }
        }else{
            if(isFieldLocal(fieldName))
                ret = javaFieldToQualifiedName.get(fieldName);
            else{
                if(javaQlfFieldToType.get(fieldName) != null) {
                    String firstName = copybookFirstNameToFileName.get(fieldName);
                    ret = firstName != null ? firstName:fieldName;
                }
                else {
                    ExprContext exprContext = getExprContext(fieldName);
                    if(exprContext == null){
                        debugPoint();
                        getLogger().error("Error Undefined field:" + fieldName);
                        return "UNDEFINED_FIELD_"+fieldName;
                    }
                    ret = exprContext.javaFieldToQualifiedName.get(fieldName);
                    int index = ret.indexOf(".");
                    String firstName = ret.substring(0,index);
                    String firstToField = copybookFirstNameToFileName.get(firstName);
                    if(firstToField == null){
                        firstToField = javaFieldToQualifiedName.get(firstName);
                    }
                    if(firstToField != null) {//constant
                        ret = firstToField + ret.substring(index);
                    }
                }
            }
        }
        return ret;
    }

    private String _getQlfName(String fieldName, String ofCopyField) {
        String ret = null;
        ExprContext exprContext = getOfCopyContext(ofCopyField);
        if(exprContext == null)
            ret = javaFieldToQualifiedName.get(fieldName);
        else {
            ret = exprContext.name_qlfName(fieldName,null);
        }
        return ret;
    }

    private boolean isFieldLocal(String fieldName){
        return javaFieldToQualifiedName.get(fieldName) != null;
    }

    private ExprContext getOfCopyContext(String ofCopyField) {
        String qlfName = javaFieldToQualifiedName.get(ofCopyField);
        if(qlfName == null)
            qlfName = ofCopyField;
        String ofCopyCls = javaQlfFieldToType.get(qlfName);
        String realCopyCls = null;
        ExprContext exprContext = null;
        if(ofCopyCls == null){
            //of copy is not defined in main cobol
            exprContext = getExprContext(qlfName);
        }else{
            realCopyCls = innerClsNameToCopybookName.get(ofCopyCls);
            exprContext = copybookContexts.get(realCopyCls != null ? realCopyCls : ofCopyCls);
        }
        return exprContext;
    }

    private ExprContext getExprContext(String fieldName) {
        ExprContext value = null;
        for (Map.Entry<String, ExprContext> entry : copybookContexts.entrySet()) {
            if (entry.getValue().javaFieldToQualifiedName.get(fieldName) != null) {
                value = entry.getValue();
                break;
            }
        }
        return value;
    }

    public String name_qlfUdfNameWithDim(String javaQlfName, String dimStr) {
        String ret = null;
        String delegate = javaQlfName.substring(javaQlfName.lastIndexOf('.') + 1);
        ExprContext exprContext = getExprContext(delegate);
        ret = exprContext.name_qlfNameWithDim(javaQlfName, dimStr);
        ret = nestedQualifiedName(ret);

        return ret;
    }

    public String name_qlfNameWithDim(String theJavaQlfName, String dimStr) {
        String ret = null;

        if (dimStr == null) {
            ret = theJavaQlfName;
        } else {
            String[] dims = dimStr.split(",");
            //use max name access array, there is an ambiguity here, but can not avoid
            String javaQlfName = getJavaQlfNameWithLeaf(theJavaQlfName);
            String[] names = javaQlfName.split("\\.");
            int dimIndex = 0;
            for (String name : names) {
                Number dim = javaFieldNameToDim.get(name);
                if (dim != null && dim.intValue() != 0) {
                    //access part dimensions of the multiple dimension array
                    if (dimIndex == dims.length) {
                        break;
                    }
                    name = name + "[" + dims[dimIndex++] + "]";
                }
                if (ret == null)
                    ret = name;
                else
                    ret += "." + name;
            }
        }
        ret = nestedQualifiedName(ret);
        if(ret.indexOf("[a]") != -1){
            debugPoint();
        }
        return ret;
    }

    private String getJavaQlfNameWithLeaf(String javaQlfName) {
        return javaFieldToQlfNameWithLeaf.get(javaQlfName.substring(javaQlfName.lastIndexOf(".") + 1));
    }

    /**
     * If main cbl define
     * 01 DBI-FTCALL.
     * COPY FTCALL.
     * <p>
     * And COPYBOOL FTCALL define
     * 05  FT-GU                   PIC X(4) VALUE 'GU  '.
     * 05  FT-GHU                  PIC X(4) VALUE 'GHU '.
     * <p>
     * Then main cbl access use 'FT-GU'
     * the qname = "ftcall.ftGu"
     * String fieldName = copybookFirstNameToFileName.get(firstName ="ftcall");
     * fieldName = "dbiFtcall"
     * ret = "dbiFtcall.ftGu"
     *
     * @param qname
     * @return
     */
    private String nestedQualifiedName(String qname) {
        String qlfName = javaFieldToQualifiedName.get(qname);
        if (qlfName != null && !qlfName.equals(qname))
            return qname;
        if (qname == null)
            debugPoint();
        String firstName = qname.split("\\.")[0];
        String fieldName = copybookFirstNameToFileName.get(firstName);
        String firstQName = fieldName == null ? javaFieldToQualifiedName.get(firstName) : fieldName;
        String ret = qname;
        if (firstQName != null && !qname.equals(firstQName)) {
            ret = firstQName + (qname.length() == firstName.length() ? "" : qname.substring(firstName.length()));
        }
        return ret;
    }

    public String setCopyFirstNameToFieldName(String copyName, String fieldName) {
        return copybookFirstNameToFileName.put(copyName, fieldName);
    }

    public String setInnerClsNameToCopyName(String innerClsName, String copyName) {
        return innerClsNameToCopybookName.put(innerClsName, copyName);
    }

    public String cobol_compile(String fileName) {
        fileName = fileName.replace("\"", "");
        String fullClsName = compiledCobol.get(fileName);
        if (fullClsName != null) {
            return fullClsName;
        }
        ICobolConvertor cobolConvertor = CobolConfig.getCobolConvertor();
        List<File> files = new ArrayList<>();
        cobolConvertor.findFiles(new File(cobolConvertor.getSourcePath()), files, cobolConvertor.getSuffixes(), fileName + "*");
        if (files.size() > 0) {
            fullClsName = cobolConvertor.convertAFile(files.get(0));
        } else {
            getLogger().error("Can not find given file: {}", fileName);
            fullClsName = "UNDEFINED";
        }
        compiledCobol.put(fileName, fullClsName);
        return fullClsName;
    }

    private String createQualifedName(String fieldName) {
        String ret = "";
        for (String superField : clsLevel) {
            ret += superField + ".";
        }
        return ret + fieldName;
    }

    public String rel_getOper(String cobolOper, String left, String right) {
        String ret = null;
        RelationalOperator oper = RelationalOperator.valueOf(cobolOper);
        List<String> lefts = extractQualifiedNames(left);
        List<String> rights = extractQualifiedNames(right);
        switch (oper) {
            case EQUAL:
            case EQUALCHAR:
                ret = "Util.compare(" + left + "," + right + ") == 0";
                break;
            case LESS:
            case LESSTHANCHAR:
                ret = "Util.compare(" + left + "," + right + ") < 0";
                break;
            case GREATER:
            case MORETHANCHAR:
                ret = "Util.compare(" + left + "," + right + ") > 0";
                break;
            case NOT_EQUAL:
            case NOTEQUALCHAR:
                ret = "Util.compare(" + left + "," + right + ") != 0";
                break;
            case LESS_OR_EQUAL:
            case LESSTHANOREQUAL:
                ret = "Util.compare(" + left + "," + right + ") <= 0";
                break;
            case GREATER_OR_EQUAL:
            case MORETHANOREQUAL:
                ret = "Util.compare(" + left + "," + right + ") >= 0";
                break;
        }
        return ret;
    }

    private List<String> extractQualifiedNames(String expression) {
        // 使用 replaceAll 方法替换匹配的数组部分为空字符串
        expression = expression.replaceAll("\\[[0-9]*\\]", "");
        // 定义匹配 qualifiedName 的正则表达式
        String regex = "\\b[a-zA-Z_][a-zA-Z0-9_]*(\\.[a-zA-Z_][a-zA-Z0-9_]*)*\\b";
        Pattern pattern = Pattern.compile(regex);
        Matcher matcher = pattern.matcher(expression);

        // 存储所有匹配到的 qualifiedName 变量
        List<String> qualifiedNames = new ArrayList<>();
        while (matcher.find()) {
            qualifiedNames.add(matcher.group());
        }
        return qualifiedNames;
    }

    private record PropOfField(String id, List<String> ofId) {
    }

    private String getCtxText(ParserRuleContext ctx, List<Object> ofIds) {
        getPropOfIds(ctx, ofIds);
        String ret = "";
        int begTable = -1;
        for (int i = 0; i < ctx.getChildCount(); i++) {
            if (i != 0)
                ret += " ";
            String text = ctx.getChild(i).getText();
            if(text.equals("("))
                begTable = 0;
            else if(text.equals(")"))
                begTable = -1;
            if(begTable != -1){
                if(begTable > 1)
                    ret +=",";
                begTable++;
            }

            if (text.startsWith("'") && text.endsWith("'"))
                text = "\"" + text.substring(1, text.length() - 1) + "\"";
            ret += text;
        }
        return ret;
    }

    public String name_ofCopy(Object o) {
        if (!(o instanceof CobolParser.QualifiedDataNameContext))
            return null;
        CobolParser.QualifiedDataNameContext ctx = (CobolParser.QualifiedDataNameContext) o;
        List<TerminalNode> list = new ArrayList<>();
        getAllTerm(ctx, list);
        boolean isOf = false;
        String ret = null;
        for (TerminalNode node : list) {
            String text = node.getText();
            if ("OF".equals(text)) {
                isOf = true;
            } else if (node.getSymbol().getType() == CobolLexer.IDENTIFIER) {
                if (isOf) {
                    if (ret == null)
                        ret = node.getText();
                    else
                        ret += "." + node.getText();
                }
            }
        }
        return ret;
    }

    public String value_fix(String left, Object right) {
        String ret = right + "";
        String leftType = getTypeByQlfName(null, left);
        String rightType = getTypeByQlfName(null, ret);
        if (leftType == null) {
            debugPoint();
            leftType = "";
        }
        if (rightType == null) {
            debugPoint();
            rightType = "";
        }
        if (isBaseType(leftType) && isBaseType(rightType)) {
            if (!leftType.equals("String") && ret.indexOf("\"") != -1) {
                ret = ret.replace("\"", "");
            } else if (leftType.equals("Double") && ret.indexOf(".") == -1) {
                ret = right + "d";
            }
        } else {
            ret = "Util.copyObject(" + right + "," + left + ")";
        }
        return ret;
    }

    private boolean isBaseType(String type) {
        boolean ret = false;
        switch (type) {
            case "String":
            case "Integer":
            case "Double":
                ret = true;
                break;
        }
        return ret;
    }

    private String getTypeByQlfName(String parent, String left) {
        String ret = javaQlfFieldToType.get(left);
        int index = left.indexOf(".");
        if (ret == null && index != -1) {
            String clsFieldName = left.substring(0, index);
            String clsType = javaQlfFieldToType.get(parent == null ? clsFieldName : parent + "." + clsFieldName);

            String innerClsName = innerClsNameToCopybookName.get(clsType);
            if (innerClsName == null) {
                //type and field same name
                innerClsName = clsType;
            }
            ExprContext exprContext = copybookContexts.get(innerClsName);
            if (exprContext != null) {
                String fieldName = innerClsName.substring(0, 1).toLowerCase() + innerClsName.substring(1);
                String qlfNameInContext = left.replace(clsFieldName, fieldName);
                ret = exprContext.javaQlfFieldToType.get(qlfNameInContext);
            } else {
                ret = getTypeByQlfName(clsFieldName, left.substring(index + 1));
            }
        }
        return ret;
    }

    private void getPropOfIds(ParserRuleContext ctx, List<Object> ofIds) {
        List<TerminalNode> list = new ArrayList<>();
        getAllTerm(ctx, list);
        if (ctx.getText().indexOf("ARL-DATA-LL+ASR-DATA-LL+LENGTHOFWK-ASR-AREA") != -1)
            debugPoint();
        boolean isOf = false;
        String prevNode = null;
        boolean isLengthOf = false;
        for (TerminalNode node : list) {
            String text = node.getText();
            int symbolType = node.getSymbol().getType();
            switch (symbolType) {
                case CobolLexer.OF: {
                    if (!isLengthOf)
                        isOf = true;
                    break;
                }
                case CobolLexer.LENGTH:
                    isLengthOf = true;
                    break;
                case CobolLexer.IDENTIFIER: {
                    if (isOf) {
                        isOf = false;
                        List<String> ofs = null;
                        if (prevNode != null) {
                            String id = prevNode;
                            String ofField = node.getText();
                            ofs = new ArrayList<>();
                            ofs.add(ofField);
                            PropOfField propOfField = new PropOfField(id, ofs);
                            ofIds.add(propOfField);
                        } else {
                            PropOfField propOfField = (PropOfField) ofIds.get(ofIds.size() - 1);
                            propOfField.ofId.add(node.getText());
                        }
                        prevNode = null;
                        continue;
                    } else if (prevNode != null) {
                        if (isLengthOf) {
                            isLengthOf = false;
                            text = LENGTHOF + node.getText();
                        }
                        ofIds.add(prevNode);
                    }
                    prevNode = text;
                    break;
                }
            }
        }
        if (prevNode != null)
            ofIds.add(prevNode);
    }

    private static void getAllTerm(ParseTree root, List<TerminalNode> list) {
        int count = root.getChildCount();
        for (int i = 0; i < count; i++) {
            ParseTree node = root.getChild(i);
            if (node instanceof TerminalNode) {
                list.add((TerminalNode) node);
            } else {
                getAllTerm(node, list);
            }
        }
    }

    public String expr_convertExpr(ParserRuleContext ctx) {
        if (ctx.getText().indexOf("DBI-CMD-CODE") != -1) {
            debugPoint();
        }
        List<Object> ofIds = new ArrayList<>();
        String cobolExpr = getCtxText(ctx, ofIds);

        String ret = cobolExpr.replace("**", "^");
        if (ofIds.size() != 0) {
            for (Object value : ofIds) {
                ret = calcID(value, cobolExpr, ret);
            }
        } else if (CobolConstant.isConstant(ret)) {
            ret = "CobolConstant." + ret;
        }
        return ret.indexOf('^') != -1 ? ExprUtil.convertExpression(ret) : ret;

    }

    private String calcID(Object value, String cobolExpr, String ret) {
        String id = null;
        String fieldName = null;
        String ofId = null;
        boolean isLengthOf = false;
        if (value instanceof String) {
            id = (String) value;
            String realId = id;
            if (id.startsWith(LENGTHOF)) {
                realId = id.substring(LENGTHOF.length());
                isLengthOf = true;
            }
            fieldName = name_toField(realId);
        } else {
            PropOfField propOfField = ((PropOfField) value);
            fieldName = name_toField(propOfField.id);
            if (propOfField.ofId.size() > 1) {
                debugPoint();
                ofId = ofId;
            }
            ofId = propOfField.ofId.get(propOfField.ofId.size() - 1);
            id = propOfField.id;
            for (String of : propOfField.ofId) {
                id = id + "OF" + of;
            }
        }
        if (CobolConstant.isConstant(id))
            return "CobolConstant." + id;
        String[] dims = getDimStringOfVar(cobolExpr, id);
        String dimStr = dims[1];
        String qlfName = name_qlfName(fieldName, ofId);
        if (dimStr == null) {
            String sExpr = qlfName;
            if (isLengthOf) {
                sExpr = "Util.sizeof(" + qlfName + ")";
            }
            ret = ret.replace(id, sExpr);
        } else {
            if(dimStr.indexOf(":") == -1) {
                String qlfNameWithDims = null;
                if (getJavaQlfNameWithLeaf(qlfName) != null)
                    qlfNameWithDims = name_qlfNameWithDim(qlfName, dimStr);
                else
                    qlfNameWithDims = name_qlfUdfNameWithDim(qlfName, dimStr);
                ret = dims[0] + qlfNameWithDims + dims[2];
            }else{
                ret = dims[0] + qlfName+"("+dimStr+")" + dims[2];
            }
        }
        int index = ret.indexOf(":");
        if (index != -1) {
            int right = ret.indexOf(')', index);
            int left = ret.lastIndexOf('(', index);
            String range = ret.substring(left + 1, right);
            ret = "Util.subvalue(" + ret.substring(0,left) + ",\"" + range.replace(':',',') + "\")";
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

    public SqlStatement sql_exec(String sql) {
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

    private static String capitalizeFirstLetter(String str) {
        return str.substring(0, 1).toUpperCase() + str.substring(1);
    }

    private void debugPoint() {
        if (environment != null) {
            String debug = (String) environment.getVar("DEBUG");
            if ("program1".equals(debug))
                debug = debug;
        }
    }

}
