package free.cobol2java;

import free.cobol2java.copybook.ICobol2Java;
import io.proleap.cobol.preprocessor.CobolPreprocessor;

import java.io.File;
import java.util.List;

/**
 * @author lidong@date 2024-08-30@version 1.0
 */
public class Cobol2Java implements ICobol2Java {
    private String cblFile;
    private String progName;
    private String encoding;
    private String rootPackageName;
    private CobolPreprocessor.CobolSourceFormatEnum format;
    private List<File> copyDirs;

    public Cobol2Java(String cblFile, String progName, List<File> copyDirs, String rootPackageName) {
        this(cblFile,progName, copyDirs, rootPackageName,CobolPreprocessor.CobolSourceFormatEnum.TANDEM,null);
    }

    public Cobol2Java(String cblFile, String progName, List<File> copyDirs, String rootPackageName,
                      CobolPreprocessor.CobolSourceFormatEnum format,
                      String encoding) {
        this.cblFile = cblFile;
        this.progName = progName;
        this.encoding = encoding;
        this.rootPackageName = rootPackageName;
        this.format = format;
        this.copyDirs = copyDirs;
    }

    @Override
    public String getCblFile() {
        return cblFile;
    }

    @Override
    public String getProgName() {
        return progName;
    }

    @Override
    public String getEncoding() {
        return encoding;
    }

    @Override
    public String getRootPackageName() {
        return rootPackageName;
    }

    @Override
    public CobolPreprocessor.CobolSourceFormatEnum getFormat() {
        return format;
    }

    @Override
    public List<File> getCopyDirs() {
        return copyDirs;
    }
}
