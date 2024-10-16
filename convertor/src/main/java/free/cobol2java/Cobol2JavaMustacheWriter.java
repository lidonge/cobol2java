package free.cobol2java;

import free.cobol2java.context.ExprContext;
import free.servpp.mustache.MustacheCompiler;
import free.servpp.mustache.handler.IPartialFileHandler;
import free.servpp.mustache.handler.MustacheListenerImpl;
import free.servpp.mustache.handler.MustacheWriter;
import free.servpp.mustache.model.Template;
import org.antlr.v4.runtime.ParserRuleContext;

import java.io.IOException;
import java.net.URI;
import java.net.URL;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static free.cobol2java.copybook.ICobol2JavaBase.LOCAL_CONTEXT;

/**
 * @author lidong@date 2024-08-30@version 1.0
 */
public class Cobol2JavaMustacheWriter extends MustacheWriter {
    private String packageName;

    public Cobol2JavaMustacheWriter(URI mustacheFile, Object root, String packageName, boolean lookupParent) {
        super(mustacheFile,root, lookupParent);
        this.packageName = packageName;
        createCobol2JavaEnvironment();
        createPartialHandler();
    }

    private void createPartialHandler() {
        getExprEvaluator().setVar("model_package", packageName);
        setPartialFileHandler(new IPartialFileHandler() {
            Map<String, Template> templateMap = new HashMap<>();
            String lastName = "";

            @Override
            public Template compilePartialTemplate(String partialName) {
                Template tmpl = templateMap.get(partialName);

                if (tmpl == null) {
                    URL url = Cobol2JavaMustacheWriter.class.getResource("/mustache/" + partialName.replace(".", "/") + ".mustache");
                    try {
                        MustacheCompiler mustacheCompiler = new MustacheCompiler(url);
                        try {
//                            System.out.println("Reading mustache template :" + partialName);

                            lastName = partialName;
                            mustacheCompiler.compileAntlr4();
                            MustacheListenerImpl listener = new MustacheListenerImpl(url.toURI());
                            mustacheCompiler.workListener(listener);
                            tmpl = listener.getTemplate();
                            templateMap.put(partialName, tmpl);
                        } catch (IOException e) {
                            throw new RuntimeException(e);
                        }
                    } catch (Throwable e) {
                        throw new RuntimeException(e);
                    }
                }
                return tmpl;
            }
        });
    }

    private void createCobol2JavaEnvironment() {
        this.getExprEvaluator().setEnvironment(new MustacheEnvironment() {
            @Override
            public void addDefault() {
                super.addDefault();
                ExprContext exprContext = (ExprContext) getVar(LOCAL_CONTEXT);
                if (exprContext == null) {
                    exprContext = new ExprContext();
                    setVar(LOCAL_CONTEXT, exprContext);
                }
                exprContext.setEnvironment(this);
                addFunction("sql_exec", args -> ((ExprContext) getVar(LOCAL_CONTEXT)).sql_exec((String) args[0]));
                addFunction("var_push", args -> ((ExprContext) getVar(LOCAL_CONTEXT)).var_push(args[0]));
                addFunction("var_pop", args -> ((ExprContext) getVar(LOCAL_CONTEXT)).var_pop());
                addFunction("var_peek", args -> ((ExprContext) getVar(LOCAL_CONTEXT)).var_peek());
                addFunction("dim_putFieldDim", args -> ((ExprContext) getVar(LOCAL_CONTEXT)).dim_putFieldDim((String) args[0],(Number) args[1]));
                addFunction("dim_push", args -> ((ExprContext) getVar(LOCAL_CONTEXT)).dim_push((Number) args[0]));
                addFunction("dim_pop", args -> ((ExprContext) getVar(LOCAL_CONTEXT)).dim_pop());
                addFunction("dim_peek", args -> ((ExprContext) getVar(LOCAL_CONTEXT)).dim_peek());
                addFunction("dim_size", args -> ((ExprContext) getVar(LOCAL_CONTEXT)).dim_size());
                addFunction("dim_value", args -> ((ExprContext) getVar(LOCAL_CONTEXT)).dim_value());
                addFunction("dim_udfCall", args -> ((ExprContext) getVar(LOCAL_CONTEXT)).dim_udfCall((String) args[0]));
                addFunction("str_replace", args -> ((String) args[0]).replace((String) args[1], (String) args[2]));
                addFunction("value_fix", args -> ((ExprContext) getVar(LOCAL_CONTEXT)).value_fix((String) args[0], args[1]));
                addFunction("value_fixFullArraySet", args -> ((ExprContext) getVar(LOCAL_CONTEXT)).value_fixFullArraySet(args[0]+"",args[1]+""));
                addFunction("name_toField", args -> ((ExprContext) getVar(LOCAL_CONTEXT)).name_toField((String) args[0]));
                addFunction("name_toClass", args -> ((ExprContext) getVar(LOCAL_CONTEXT)).name_toClass((String) args[0]));
                addFunction("name_enterClass", args -> ((ExprContext) getVar(LOCAL_CONTEXT)).name_enterClass((String) args[0]));
                addFunction("name_exitClass", args -> ((ExprContext) getVar(LOCAL_CONTEXT)).name_exitClass());
                addFunction("name_putInnerField", args -> ((ExprContext) getVar(LOCAL_CONTEXT)).name_putInnerField((String) args[0]));
                addFunction("name_putInnerField1", args -> ((ExprContext) getVar(LOCAL_CONTEXT)).name_putInnerField1((String) args[0],(String) args[1]));
                addFunction("name_ofCopy", args -> ((ExprContext) getVar(LOCAL_CONTEXT)).name_ofCopy((Object) args[0]));
                addFunction("name_qlfName", args -> ((ExprContext) getVar(LOCAL_CONTEXT)).name_qlfName((String) args[0], (String) args[1]));
                addFunction("name_isConst", args -> ((ExprContext) getVar(LOCAL_CONTEXT)).name_isConst((String) args[0]));
                addFunction("name_qlfNameWithDim", args -> ((ExprContext) getVar(LOCAL_CONTEXT)).name_qlfNameWithDim((String) args[0], (String) args[1]));
                addFunction("name_qlfUdfNameWithDim", args -> ((ExprContext) getVar(LOCAL_CONTEXT)).name_qlfUdfNameWithDim((String) args[0], (String) args[1]));
//                addFunction("copybook", args -> ((ExprContext) getVar(LOCAL_CONTEXT)).copybook((String) args[0], (String) args[1]));
                addFunction("name_getFullFieldType", args -> ((ExprContext) getVar(LOCAL_CONTEXT)).name_getFullFieldType((String) args[0]));
                addFunction("name_getFieldType", args -> ((ExprContext) getVar(LOCAL_CONTEXT)).name_getFieldType((String) args[0]));
                addFunction("name_setFieldType", args -> ((ExprContext) getVar(LOCAL_CONTEXT)).name_setFieldType((String) args[0], (String) args[1]));
                addFunction("name_getFieldClsType", args -> ((ExprContext) getVar(LOCAL_CONTEXT)).name_getFieldClsType((String) args[0]));
//                addFunction("name_setFieldClsType", args -> ((ExprContext) getVar(LOCAL_CONTEXT)).name_setFieldClsType((String) args[0], (String) args[1]));
                addFunction("expr_convertExpr", args -> ((ExprContext) getVar(LOCAL_CONTEXT)).expr_convertExpr((ParserRuleContext) args[0]));
                addFunction("expr_convertFuncParam", args -> ((ExprContext) getVar(LOCAL_CONTEXT)).expr_convertFuncParam((ParserRuleContext) args[0]));
                addFunction("expr_conditionReference", args -> ((ExprContext) getVar(LOCAL_CONTEXT)).expr_conditionReference((String) args[0]));
                addFunction("expr_changeAddressType", args -> ((ExprContext) getVar(LOCAL_CONTEXT)).expr_changeAddressType(args[0] + "", args[1]+""));
                addFunction("rel_getOper", args -> ((ExprContext) getVar(LOCAL_CONTEXT)).rel_getOper(args[0] + "", args[1]+"", ""+args[2]));
                addFunction("array_initString", args -> ((ExprContext) getVar(LOCAL_CONTEXT)).array_initString(args[0].toString(), args[1].toString()));
                addFunction("type_getType", args -> ((ExprContext) getVar(LOCAL_CONTEXT)).type_getType((String) args[0]));
                addFunction("setInnerClsNameToCopyName", args -> ((ExprContext) getVar(LOCAL_CONTEXT)).setInnerClsNameToCopyName((String) args[0],(String) args[1]));
                addFunction("setFieldNameToCopyFieldName", args -> ((ExprContext) getVar(LOCAL_CONTEXT)).setFieldNameToCopyFieldName((String) args[0],(String) args[1]));
                addFunction("cobol_precompile", args -> ((ExprContext) getVar(LOCAL_CONTEXT)).cobol_precompile((String) args[0]));
                addFunction("cobol_compile", args -> ((ExprContext) getVar(LOCAL_CONTEXT)).cobol_compile((String) args[0]));
                addFunction("cbl_getComment", args -> ((ExprContext) getVar(LOCAL_CONTEXT)).cbl_getComment((Integer) args[0], (String) args[1], (List<String>) args[2]));
                addFunction("model_getPackage", args -> ((ExprContext) getVar(LOCAL_CONTEXT)).model_getPackage( (String) args[0],(String) args[1]));
                addFunction("model_replaceImports", args -> ((ExprContext) getVar(LOCAL_CONTEXT)).model_replaceImports( (List) args[0],args[1]+""));

            }
        });
    }
}
