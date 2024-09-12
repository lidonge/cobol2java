package free.cobol2java.parser;


import io.proleap.cobol.asg.params.CobolParserParams;
import io.proleap.cobol.asg.util.FilenameUtils;

import java.io.File;

/**
 * @author lidong@date 2024-09-12@version 1.0
 */

public class SqlCobolWordCopyBookFinderImpl {
    public File findCopyBook(final CobolParserParams params, final String copyBookIdentifier) {
        if (params.getCopyBookFiles() != null) {
            for (final File copyBookFile : params.getCopyBookFiles()) {
                if (isMatchingCopyBook(copyBookFile, params, copyBookIdentifier)) {
                    return copyBookFile;
                }
            }
        }

        if (params.getCopyBookDirectories() != null) {
            for (final File copyBookDirectory : params.getCopyBookDirectories()) {
                final File validCopyBook = findCopyBookInDirectory(copyBookDirectory, params, copyBookIdentifier);

                if (validCopyBook != null) {
                    return validCopyBook;
                }
            }
        }

        return null;
    }

    protected File findCopyBookInDirectory(final File copyBooksDirectory, final CobolParserParams params,
                                           final String copyBookIdentifier) {
        for (final File copyBookCandidate : copyBooksDirectory.listFiles()) {
            if (isMatchingCopyBook(copyBookCandidate, params, copyBookIdentifier)) {
                return copyBookCandidate;
            }
        }

        return null;
    }

    protected boolean isMatchingCopyBook(final File copyBookCandidate, final CobolParserParams params,
                                         final String copyBookIdentifier) {
        if (params.getCopyBookExtensions() != null) {
            for (final String copyBookExtension : params.getCopyBookExtensions()) {
                if (isMatchingCopyBookWithExtension(copyBookCandidate, copyBookIdentifier, copyBookExtension)) {
                    return true;
                }
            }

            return false;
        } else {
            return isMatchingCopyBookWithoutExtension(copyBookCandidate, copyBookIdentifier);
        }
    }

    protected boolean isMatchingCopyBookWithExtension(final File copyBookCandidate, final String copyBookIdentifier,
                                                      final String copyBookExtension) {
        final String copyBookFilename = copyBookExtension == null || copyBookExtension.isEmpty() ? copyBookIdentifier
                : copyBookIdentifier + "." + copyBookExtension;
        final String copyBookCandidateName = copyBookCandidate.getName();
        final boolean result = copyBookFilename.equalsIgnoreCase(copyBookCandidateName);
        return result;
    }

    protected boolean isMatchingCopyBookWithoutExtension(final File copyBookCandidate,
                                                         final String copyBookIdentifier) {
        final String copyBookCandidateBaseName = FilenameUtils.getBaseName(copyBookCandidate.getName());
        final boolean result = copyBookCandidateBaseName.equalsIgnoreCase(copyBookIdentifier);
        return result;
    }
}

