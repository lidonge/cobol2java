package free.cobol2java.copybook;

import free.cobol2java.Cobol2JavaMustacheWriter;
import free.cobol2java.ICobolConvertor;
import free.cobol2java.context.ExprContext;
import free.cobol2java.context.ICopybookContext;
import free.cobol2java.context.IExprBaseContext;
import free.cobol2java.parser.TopCompiler;
import free.servpp.logger.ILogable;
import free.servpp.multiexpr.IEvaluatorEnvironment;
import free.servpp.mustache.CodeFormator;
import free.servpp.mustache.handler.MustacheListenerImpl;
import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.asg.params.CobolParserParams;

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * @author lidong@date 2024-09-10@version 1.0
 */
public class CopyBookManager implements ICobol2JavaBase, ILogable {
    private static final String modelTemplate = "/ModelCopyBook.cbl";
    private static final String dclModelTemplate = "/DclModelCopyBook.cbl";
    private static CopyBookManager defaultManager = new CopyBookManager();

    public static void initDefaultManager(ICobolConvertor cobolConvertor) {
        defaultManager.init(cobolConvertor);
    }

    public static CopyBookManager getDefaultManager() {
        return defaultManager;
    }

    public static void setDefaultManager(CopyBookManager defaultManager) {
        CopyBookManager.defaultManager = defaultManager;
    }

    private ICobolConvertor cobolConvertor;
    Map<String, String> copyBookMap = new HashMap<>();
    Map<String, Boolean> copyBookWritten = new HashMap<>();
    Map<String, List<String>> dupCopyBook = new HashMap<>();
    Map<String, ExprContext> exprContextMap = new HashMap<>();
    //    Map<String,String> subNameToCopybook = new HashMap<>();
    Map<String, String> classNameToPackageName = new HashMap<>();
//    ExprContext globalExprContext = new ExprContext();

    public Map<String, ExprContext> getGlobalFunc() {
        return exprContextMap;
    }

    public void init(ICobolConvertor cobolConvertor) {
        this.cobolConvertor = cobolConvertor;
    }

    /**
     * @param copyBook
     * @param params
     * @param copyText
     * @return If is dcl copybook, return 01 COPY_BOOK_NAME.
     * @throws URISyntaxException
     * @throws IOException
     * @throws CopybookException
     */
    public String loadCopyBook(File copyBook,
                               CobolParserParams params,
                               String copyText) throws URISyntaxException, IOException, CopybookException {
        String dclRet = null;
        String name = copyBook.getName();
        boolean isDcl = name.endsWith(".dcl");
        if (isDcl) {
            name = name.substring(0, name.length() - ".dcl".length());
        }
        name = IExprBaseContext.toClassName(name);

        if (exprContextMap.get(name) == null) {
            getLogger().info("Loading copybook:{}", copyBook.getName());
            URL url = CopyBookManager.class.getResource(isDcl ? dclModelTemplate : modelTemplate);
            String sText = getString(url.toURI(), cobolConvertor.getEncoding());
            sText = sText.replace("COPY-BOOK-NAME", name.toUpperCase());
            sText += "\n" + copyText;
            CompilationUnit compilationUnit;
            try {
                compilationUnit = getCompilationUnit(name, sText, params);
            } catch (Throwable t) {
                throw new CopybookException(t);
            }
            Map<String, Object> variables = new HashMap<>();
//            variables.put(COPYBOOK_CONTEXT, exprContextMap);
            if (name.endsWith("const"))
                variables.put("IsConstantCopybook", "IsConstantCopybook");

            Cobol2JavaMustacheWriter writer = createMustacheWriter(url.toURI(), cobolConvertor.getRootPackageName(),
                    compilationUnit.getProgramUnit());

            ExprContext exprContext = (ExprContext) writer.getExprEvaluator().getEnvironment().getVar(LOCAL_CONTEXT);
            exprContext.setCopyBookName(name);
            exprContext.setCopyBookPath(copyBook.getAbsolutePath());
            if(isDcl)
                exprContext.getEnvironment().setVar("dclCopybookName",name);
            exprContextMap.put(name, exprContext);
            String packageName = cobolConvertor.getRootPackageName() + ".models" +
                    getRelativePath(name).replace(File.separator, ".");
            packageName = ICobolConvertor.checkPackageName(packageName);
            classNameToPackageName.put(name, packageName);

            MustacheListenerImpl listener = createMustacheListener("/mustache/copybook.mustache");
            convert(variables, writer, listener);

            String prog = CodeFormator.formatCode(writer.getOutText().toString());
            if (prog.trim().length() != 0) {
                copyBookMap.put(name, prog);
            }
            if (isDcl) {
                DataDescriptionEntry entry = compilationUnit.getProgramUnit().getDataDivision().
                        getWorkingStorageSection().getRootDataDescriptionEntries().get(0);
                String cobolName = entry.getName();
                String clsName = name+IExprBaseContext.toClassName(cobolName);
                copyBookMap.remove(name);
                copyBookMap.put(clsName, prog);
                classNameToPackageName.put(clsName, packageName);
                exprContextMap.put(clsName, exprContext);
                dclRet = "01 " + cobolName + ".\n             75 "+name.toUpperCase()+"-"+cobolName+".";
                name = clsName;
            }
            Map<String, Object> copyBookCls = (Map<String, Object>) writer.getExprEvaluator().getEnvironment().getVar("innerMap");
            removeDupCls(exprContext, copyBook, copyBookCls, prog, name);
            for (String key : copyBookCls.keySet()) {
                String clsText = copyBookCls.get(key).toString();
                copyBookMap.put(key, clsText);
                classNameToPackageName.put(key, packageName);
            }

//            getLogger().info("Copybook:{} Loaded. ",copyBook.getName());
        }
        return dclRet;
    }

    private void removeDupCls(ExprContext exprContext, File copyBook, Map<String, Object> copyBookCls, String mainContent, String mainClsName) {
        List<String> keys = new ArrayList<>(copyBookCls.keySet());
        for (String key : keys) {
            String clsText = CodeFormator.formatCode(copyBookCls.get(key).toString());
            if (clsText.trim().length() != 0) {
                //String[] splitKey = key.split("\\.");
                //key = splitKey[splitKey.length - 1];
                copyBookCls.put(key, clsText);
                String oldClsText = copyBookMap.get(key);
                if (oldClsText != null) {
                    String oldKey = key;
                    if (compareClassContent(oldClsText, clsText)) {
                        copyBookCls.remove(key);
                        importClass(key, copyBookCls);
                    } else {
                        List<String> dupKeys = dupCopyBook.get(key);
                        if (dupKeys == null) {
                            dupKeys = new ArrayList<>();
                            dupCopyBook.put(key, dupKeys);
                        }
                        boolean isDupCls = false;
                        for (String dupKey : dupKeys) {
                            oldClsText = copyBookMap.get(dupKey);
                            if (compareClassContent(oldClsText, clsText)) {
                                copyBookCls.remove(key);
                                importClass(dupKey, copyBookCls);
                                isDupCls = true;
                                break;
                            }
                        }
                        if (!isDupCls) {
                            key = getUniqueClassName(key);
                            dupKeys.add(key);
                            String target = "\\b" + oldKey + "\\b";
                            clsText = clsText.replaceAll(target, key);
                            copyBookCls.remove(oldKey);
                            copyBookCls.put(key, clsText);
                            changeClassTo(exprContext, oldKey, key);
                            mainContent = mainContent.replaceAll(target, key);
                            Map<String, IEvaluatorEnvironment.MyObject> imports = (Map<String, IEvaluatorEnvironment.MyObject>) exprContext.getEnvironment().getVar("importsMap");
                            for (String clsName : imports.keySet()) {
                                List<IEvaluatorEnvironment.MyObject> importCls = (List<IEvaluatorEnvironment.MyObject>) imports.get(clsName).getValue();
                                for (int i = 0; i < importCls.size(); i++) {
                                    String imp = (String) importCls.get(i).getValue();
                                    importCls.set(i, new IEvaluatorEnvironment.MyObject(imp.replaceAll(target, key)));
                                }
                            }
                            copyBookMap.put(mainClsName, mainContent);
                            replaceAllCopy(copyBookCls, oldKey, key);
                            getLogger().warn("Warning: Duplicate inner class {} of copybook:{}, change to new class {}.", oldKey, copyBook, key);
                        }
                    }
                }
            } else {
                copyBookCls.remove(key);
            }
        }
    }

    private void changeClassTo(ExprContext exprContext, String oldKey, String key) {
        Map<String, String> javaQlfFieldToSimpleType = exprContext.getJavaQlfFieldToSimpleType();
        replaceCls(oldKey, key, javaQlfFieldToSimpleType, false);
        Map<String, String> javaQlfFieldToFullType = exprContext.getJavaQlfFieldToFullType();
        replaceCls(oldKey, key, javaQlfFieldToFullType, true);
    }

    private static void replaceCls(String oldKey, String key, Map<String, String> javaQlfFieldToType, boolean isFullType) {
        String fullOldKey = "." + oldKey;
        String fullKey = "." + key;
        for (String name : javaQlfFieldToType.keySet()) {
            String value = javaQlfFieldToType.get(name);
            if (isFullType) {
                if (value.endsWith(fullOldKey)) {
                    javaQlfFieldToType.put(name, value.replace(fullOldKey, fullKey));
                }
            } else {
                if (value.equals(oldKey)) {
                    javaQlfFieldToType.put(name, key);
                }
            }
        }
    }

    private static boolean compareClassContent(String oldContent, String contentClsText) {
        contentClsText = contentClsText.substring(contentClsText.indexOf("{"));
        oldContent = oldContent.substring(oldContent.indexOf("{"));
        contentClsText = removeLinesStartingWith(contentClsText, new String[]{"@FieldInfo", "//"});
        oldContent = removeLinesStartingWith(oldContent, new String[]{"@FieldInfo", "//"});
        contentClsText = removeInit(contentClsText);
        oldContent = removeInit(oldContent);
        return oldContent.equals(contentClsText);
    }

    private static String removeInit(String input) {
        String[] lines = input.split("\n");

        StringBuilder result = new StringBuilder();
        boolean isInit = false;
        for (String line : lines) {
            int idx = line.indexOf(" = ");
            boolean isOneLine = line.endsWith(";");
            if (idx != -1) {
                line = line.substring(0, idx);
                if (isOneLine)
                    line += ";";
            }
            if (isInit && isOneLine) {
                isInit = false;
            }
            if (!isInit) {
                result.append(line).append("\n");
            }
            if (idx != -1 && !isOneLine) {
                isInit = true;
            }
        }

        return result.toString().trim();
    }

    private static String removeLinesStartingWith(String input, String[] prefixes) {
        String[] lines = input.split("\n");

        StringBuilder result = new StringBuilder();

        for (String line : lines) {
            boolean shouldRemove = false;

            for (String prefix : prefixes) {
                if (line.trim().startsWith(prefix)) {
                    shouldRemove = true;
                    break;
                }
            }

            if (!shouldRemove) {
                result.append(line).append("\n");
            }
        }

        return result.toString().trim();
    }

    private void importClass(String oldKey, Map<String, Object> copyBookCls) {
        String oldPackage = classNameToPackageName.get(oldKey);
        List<String> imports = new ArrayList<>();
        imports.add(oldPackage + "." + oldKey);
        String regex = "\\b" + oldKey + "\\b";
        Pattern pattern = Pattern.compile(regex);
        for (String key : copyBookCls.keySet()) {
            String clsText = copyBookCls.get(key).toString();
            Matcher matcher = pattern.matcher(clsText);
            if (matcher.find() && !oldPackage.equals(classNameToPackageName.get(key))) {
                clsText = ICopybookContext.replaceImports(new ArrayList(imports), clsText, true);
                copyBookCls.put(key, clsText);
            }
        }
    }

    //FIXME should replace with whole word
    private void replaceAllCopy(Map<String, Object> copyBookCls, String oldCls, String newCls) {
        String target = "\\b" + oldCls + "\\b";
        for (String key : copyBookCls.keySet()) {
            if (!newCls.equals(key)) {
                String clsText = copyBookCls.get(key).toString();
                if (clsText.trim().length() != 0) {
                    clsText = clsText.replaceAll(target, newCls);
                    copyBookCls.put(key, clsText);
                }
            }

        }
    }

    private String getUniqueClassName(String key) {
        int index = 0;
        String ret = key;
        while (copyBookMap.get(ret) != null) {
            ret = key + "_" + index++;
        }
        return ret;
    }


    public String getPackageNameByModelName(String modelName, boolean useCurrentCopybook) {
        String ret = classNameToPackageName.get(modelName);
        if (ret == null && useCurrentCopybook) {
            String curCopybook = TopCompiler.currentCompiler().currentCopybook();
            boolean isDcl = curCopybook.endsWith(".dcl");
            if (isDcl) {
                curCopybook = curCopybook.substring(0, curCopybook.length() - ".dcl".length());
            }
            ret = classNameToPackageName.get(curCopybook);
        }
        return ret;
    }

    public void writeCopyBook() {
        for (String key : copyBookMap.keySet()) {
            String text = copyBookMap.get(key);
            if (copyBookWritten.get(key) == null) {
                String packageName = classNameToPackageName.get(key);
                String relativePath = packageName.replace(".", File.separator);
                File outFile = new File(cobolConvertor.getTargetPath(), relativePath);
                outFile = new File(outFile, key + ".java");
                String outputFilePath = outFile.getAbsolutePath();
                ExprContext exprContext = exprContextMap.get(key);
                if (exprContext != null) {
                    Map map = (Map) exprContext.getEnvironment().getVar("importsMap");
                    IEvaluatorEnvironment.MyObject myObject = (IEvaluatorEnvironment.MyObject) map.get(key);
                    List imports = null;
                    if (myObject != null) {
                        imports = (List) myObject.getValue();
                        text = ICopybookContext.replaceImports(imports, text, true);
                        copyBookMap.put(key, text);
                    }

                }
                String content = CodeFormator.formatCode(text + "");
                cobolConvertor.writeToFile(outputFilePath, content);
                if (content.indexOf("public Object ") == -1 /*|| content.indexOf("public Object[] ") == -1*/)
                    copyBookWritten.put(key, true);
            }
        }
    }

    private String getRelativePath(String copyName) {
        ExprContext exprContext = exprContextMap.get(copyName);
        String fullPath = exprContext.getCopyBookPath();
        String srcRoot = cobolConvertor.getSourcePath();
        fullPath = new File(fullPath).getParent();
        srcRoot = new File(srcRoot).getAbsolutePath();
        String relativePath = "";
        if (fullPath.indexOf(srcRoot) != -1) {
            relativePath = fullPath.substring(srcRoot.length());
        }
        return relativePath;
    }

    public boolean isCopybookManage() {
        return cobolConvertor.isCopybookManage();
    }

    public Map<String, String> getCopyBookMap() {
        return copyBookMap;
    }
}
