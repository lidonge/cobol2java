package free.cobol2java;

import com.typesafe.config.Config;
import free.servpp.config.IConfig;
import free.servpp.config.hocon.HoconConfigTypeManager;

import java.io.File;
import java.io.IOException;
import java.nio.file.*;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

/**
 * @author lidong@date 2024-09-11@version 1.0
 */
public class BaseConvertor implements ICobolConvertor {
    private String sourcePath;
    private String targetPath;
    private List<File> copyDirs = new ArrayList<>();
    private String rootPackageName;
    private String format;
    private String encoding;
    private String[] suffixes = new String[]{".cbl",".sqb"};
    private boolean copybookManage = false;
    private String[] compileFiles;

    @Override
    public String[] getCompileFiles() {
        return compileFiles;
    }

    @Override
    public void setCompileFiles(String[] compileFiles) {
        this.compileFiles = compileFiles;
    }

    @Override
    public String getSourcePath() {
        return sourcePath;
    }

    @Override
    public void setSourcePath(String sourcePath) {
        this.sourcePath = sourcePath;
    }

    @Override
    public String getTargetPath() {
        return targetPath;
    }

    @Override
    public void setTargetPath(String targetPath) {
        this.targetPath = targetPath;
    }

    @Override
    public List<File> getCopyDirs() {
        return copyDirs;
    }

    @Override
    public void addCopyDirs(File copyDir) {
        this.copyDirs.add( copyDir);
    }

    @Override
    public String getRootPackageName() {
        return rootPackageName;
    }

    @Override
    public void setRootPackageName(String rootPackageName) {
        this.rootPackageName = rootPackageName;
    }

    @Override
    public String getFormat() {
        return format;
    }

    @Override
    public void setFormat(String format) {
        this.format = format;
    }

    @Override
    public String getEncoding() {
        return encoding;
    }

    @Override
    public void setEncoding(String encoding) {
        this.encoding = encoding;
    }

    @Override
    public String[] getSuffixes() {
        return suffixes;
    }

    @Override
    public void setSuffixes(String[] suffixes) {
        this.suffixes = suffixes;
    }

    @Override
    public boolean isCopybookManage() {
        return copybookManage;
    }

    @Override
    public void setCopybookManage(boolean copybookManage) {
        this.copybookManage = copybookManage;
    }
}
