package free.cobol2java;

import com.typesafe.config.Config;
import free.cobol2java.copybook.CopyBookManager;
import free.servpp.config.IConfig;
import free.servpp.config.hocon.HoconConfigTypeManager;
import free.servpp.mustache.ILogable;
import io.proleap.cobol.preprocessor.CobolPreprocessor;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.*;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

import static free.cobol2java.copybook.ICobol2JavaBase.COPYBOOK_CONTEXT;

/**
 * @author lidong@date 2024-09-13@version 1.0
 */
public interface ICobolConvertor extends ILogable {
    String getSourcePath();

    void setSourcePath(String sourcePath);

    String getTargetPath();

    void setTargetPath(String targetPath);

    List<File> getCopyDirs();

    void addCopyDirs(File copyDir);

    String getRootPackageName();

    void setRootPackageName(String rootPackageName);

    String getFormat();

    void setFormat(String format);

    String getEncoding();

    void setEncoding(String encoding);

    String[] getSuffixes();

    void setSuffixes(String[] suffixes);

    boolean isCopybookManage();

    void setCopybookManage(boolean copybookManage);

    void setCompileFiles(String[] cpFiles);

    String[] getCompileFiles();
    default void initConfig(String[] args) {
        setSourcePath(args[0]);
        setTargetPath(args[1]);
        findMatchingDirectories(getSourcePath(), args[2]);
        setRootPackageName(args[3]);
        setFormat(args[4]);
        setEncoding(args.length == 6 ? args[5] : "utf-8");

    }


    default void initConfig(HoconConfigTypeManager manager) {
        IConfig config = manager.getHoconConfigManager("application").getConfigById("cobol2java");
        Config con = (Config) config.getConfigObject();
        setSourcePath(con.getString("application.dirs.sourcePath"));
        setTargetPath(con.getString("application.dirs.targetPath"));

        String sCopyDir = con.getString("application.dirs.copyDir");
        findMatchingDirectories(getSourcePath(), sCopyDir);
        setRootPackageName(con.getString("application.rootPackageName"));
        setFormat(con.getString("application.format"));
        setEncoding(con.getString("application.encoding"));
        setCopybookManage(con.getBoolean("application.copybookManage"));
        String theSuffixes = con.getString("application.suffixes");
        if (theSuffixes != null && !theSuffixes.isEmpty()) {
            setSuffixes(theSuffixes.split(";"));
        }
        String compileFiles = con.getString("application.compileFiles");
        compileFiles = compileFiles == null ? "*" : compileFiles;
        String[] cpFiles = compileFiles.split(";");
        setCompileFiles(cpFiles);
    }

    private void findMatchingDirectories(String rootDir, String pattern) {
        Path startPath = Paths.get(rootDir);

        String regexPattern = pattern.replace("*", ".*");
        Pattern compiledPattern = Pattern.compile(regexPattern);

        try {
            Files.walkFileTree(startPath, new SimpleFileVisitor<Path>() {
                @Override
                public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) {
                    return FileVisitResult.CONTINUE;
                }

                @Override
                public FileVisitResult preVisitDirectory(Path dir, BasicFileAttributes attrs) {
                    String dirName = dir.getFileName().toString();
                    if (compiledPattern.matcher(dirName).matches()) {
                        addCopyDirs(dir.toFile());
                    }
                    return FileVisitResult.CONTINUE;
                }
            });
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    default void findFiles(File directory, List<File> files, String[] suffixes) {
        findFiles(directory, files, suffixes, null);
    }

    default void findFiles(File directory, List<File> files, String[] suffixes, String regexPattern) {
        if (directory.isDirectory()) {
            for (File file : directory.listFiles()) {
                if (file.isDirectory()) {
                    findFiles(file, files, suffixes, regexPattern);
                } else {
                    if (suffixes == null || suffixes.length == 0)
                        files.add(file);
                    else {
                        for (String suffix : suffixes) {
                            if (file.getName().endsWith(suffix) && isMatch(file.getName(), regexPattern)) {
                                files.add(file);
                                break;
                            }
                        }
                    }
                }
            }
        }
    }

    default boolean isMatchCompileFiles(String file) {
        boolean ret = false;
        for (String regexPattern : getCompileFiles()) {
            if (isMatch(file, regexPattern)) {
                ret = true;
                break;
            }
        }
        return ret;
    }

    default boolean convertAFile(File file) {
        String relativePath = file.getAbsolutePath().substring(getSourcePath().length() + 1);
        String fileName = file.getName();
        String className = fileName.substring(0, fileName.lastIndexOf("."));
        className = className.substring(0, 1).toUpperCase() + className.substring(1).toLowerCase();
        String outputFilePath = getTargetPath() + File.separator +
                getRootPackageName().replace(".", File.separator) + File.separator +
                relativePath.substring(0,relativePath.length() - fileName.length()).replace(".",File.separator)+
                className + ".java";
        String relativeParent = new File(relativePath).getParent();
        String packageName = getRootPackageName() +
                (relativeParent != null ? "." + relativeParent.replace(File.separator, ".") : "");

        // Call the convert function and get the result as a string
        try {
            String convertedContent = convert(file, packageName);

            // Write the result to the target file
            writeToFile(outputFilePath, convertedContent);
            return true;
        } catch (Throwable t) {
            t.printStackTrace();
            return false;
        }

    }
    // Write the converted content to the target file
    default void writeToFile(String outputFilePath, String content) {
        File targetFile = new File(outputFilePath);
        targetFile.getParentFile().mkdirs();  // Create directories if they don't exist

        try (FileWriter writer = new FileWriter(targetFile)) {
            writer.write(content);
            writer.close();
            getLogger().info("Written to file: {}", targetFile.getAbsolutePath());
        } catch (IOException e) {
            getLogger().error("Error writing to file: {}" , targetFile.getAbsolutePath());
            e.printStackTrace();
        }
    }
    // Dummy convert function (you can replace it with your actual logic)
    private String convert(File sourceFile, String packageName) {
        // Implement your conversion logic here
        System.out.println("Converting file: " + sourceFile.getAbsolutePath());
        System.out.println("Package name: " + packageName);
        String fileName = sourceFile.getName();
        String prog = null;

        Cobol2Java cobol2Java = new Cobol2Java(sourceFile.getAbsolutePath(),
                fileName.substring(0, fileName.lastIndexOf(".")),getCopyDirs(), packageName,
                CobolPreprocessor.CobolSourceFormatEnum.valueOf(getFormat()), getEncoding());
        CopyBookManager defaultManager = CopyBookManager.getDefaultManager();
        Map<String,Object> varables = new HashMap<>();
        if(defaultManager.isCopybookManage()) {
            varables.put(COPYBOOK_CONTEXT,defaultManager.getGlobalFunc());
        }
        prog = cobol2Java.convertAll(varables);

        return prog;
    }
    private boolean isMatch(String file, String regexPattern) {
        if (regexPattern == null || regexPattern.length() == 0)
            return true;
        Pattern compiledPattern = Pattern.compile(regexPattern.
                replace(".", "\\.").
                replace("*", ".*"));

        return compiledPattern.matcher(file).matches();
    }
}
