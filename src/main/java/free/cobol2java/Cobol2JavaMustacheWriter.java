package free.cobol2java;

import free.cobol2java.util.Func;
import free.servpp.mustache.MustacheCompiler;
import free.servpp.mustache.handler.IPartialFileHandler;
import free.servpp.mustache.handler.MustacheWriter;
import free.servpp.mustache.model.Template;
import io.proleap.cobol.CobolParser;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author lidong@date 2024-08-30@version 1.0
 */
public class Cobol2JavaMustacheWriter extends MustacheWriter {
    private String packageName;
    public Cobol2JavaMustacheWriter(Object root, String packageName, boolean lookupParent) {
        super(root,lookupParent);
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

                if(tmpl == null) {
                    URL url = Cobol2JavaMustacheWriter.class.getResource("/mustache/" + partialName.replace(".", "/") + ".mustache");
                    try {
                        MustacheCompiler mustacheCompiler = new MustacheCompiler(url);
                        try {
//                            System.out.println("Reading mustache template :" + partialName);

                            lastName = partialName;
                            tmpl = mustacheCompiler.compile().getTemplate();
                            templateMap.put(partialName,tmpl);
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
                Func func = (Func) getVar("__System_Function");
                if(func == null){
                    func = new Func();
                    setVar("__System_Function", func);
                }
                addFunction("var_push", args -> ((Func) getVar("__System_Function")).var_push( args[0]));
                addFunction("var_pop", args -> ((Func) getVar("__System_Function")).var_pop());
                addFunction("var_peek", args -> ((Func) getVar("__System_Function")).var_peek());
                addFunction("dim_push", args -> ((Func) getVar("__System_Function")).dim_push((Number) args[0]));
                addFunction("dim_pop", args -> ((Func) getVar("__System_Function")).dim_pop());
                addFunction("dim_peek", args -> ((Func) getVar("__System_Function")).dim_peek());
                addFunction("dim_size", args -> ((Func) getVar("__System_Function")).dim_size());
                addFunction("dim_value", args -> ((Func) getVar("__System_Function")).dim_value());
                addFunction("dim_putQlfLevel", args -> ((Func) getVar("__System_Function")).dim_putQlfLevel((String) args[0], (String) args[1]));
                addFunction("dim_getQlfLevel", args -> ((Func) getVar("__System_Function")).dim_getQlfLevel((String) args[0]));
                addFunction("str_replace", args -> ((String) args[0]).replace((String) args[1], (String) args[2]));
                addFunction("name_toField", args -> ((Func) getVar("__System_Function")).name_toField((String) args[0]));
                addFunction("name_toClass", args -> ((Func) getVar("__System_Function")).name_toClass((String) args[0]));
                addFunction("name_enterClass", args -> ((Func) getVar("__System_Function")).name_enterClass((String) args[0]));
                addFunction("name_exitClass", args -> ((Func) getVar("__System_Function")).name_exitClass());
                addFunction("name_putInnerField", args -> ((Func) getVar("__System_Function")).name_putInnerField((String) args[0]));
                addFunction("name_delegateName", args -> ((Func) getVar("__System_Function")).name_delegateName((String) args[0]));
                addFunction("name_delegateName1", args -> ((Func) getVar("__System_Function")).name_delegateName1((String) args[0], (String) args[1]));
                addFunction("name_getFieldType", args -> ((Func) getVar("__System_Function")).name_getFieldType((String) args[0]));
                addFunction("name_setFieldType", args -> ((Func) getVar("__System_Function")).name_setFieldType((String) args[0], (String) args[1]));
                addFunction("name_getFieldClsType", args -> ((Func) getVar("__System_Function")).name_getFieldClsType((String) args[0]));
                addFunction("name_setFieldClsType", args -> ((Func) getVar("__System_Function")).name_setFieldClsType((String) args[0], (String) args[1]));
                addFunction("expr_convertExpr", args -> ((Func) getVar("__System_Function")).expr_convertExpr((CobolParser.ArithmeticExpressionContext) args[0]));
                addFunction("rel_getOper", args -> ((Func) getVar("__System_Function")).rel_getOper(args[0]+"", (String) args[1], (String) args[2]));
                addFunction("array_initString", args -> ((Func) getVar("__System_Function")).array_initString(args[0].toString(), args[1].toString()));
                addFunction("type_getType", args -> ((Func) getVar("__System_Function")).type_getType((String) args[0]));
                addFunction("cbl_getComment", args -> ((Func) getVar("__System_Function")).cbl_getComment((Integer) args[0], (String) args[1], (List<String>) args[2]));
            }
        });
    }
}
