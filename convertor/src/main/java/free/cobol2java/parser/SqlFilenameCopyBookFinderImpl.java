package free.cobol2java.parser;


import io.proleap.cobol.asg.params.CobolParserParams;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.stream.Collectors;

/**
 * @author lidong@date 2024-09-12@version 1.0
 */

public class SqlFilenameCopyBookFinderImpl {

    public File findCopyBook(final CobolParserParams params, final String copyBookIdentifier) {
        if (params.getCopyBookFiles() != null) {
            for (final File copyBookFile : params.getCopyBookFiles()) {
                if (isMatchingCopyBook(copyBookFile, null, copyBookIdentifier)) {
                    return copyBookFile;
                }
            }
        }

        if (params.getCopyBookDirectories() != null) {
            for (final File copyBookDirectory : params.getCopyBookDirectories()) {
                final File validCopyBook = findCopyBookInDirectory(copyBookDirectory, copyBookIdentifier);

                if (validCopyBook != null) {
                    return validCopyBook;
                }
            }
        }

        return null;
    }

    protected File findCopyBookInDirectory(final File copyBooksDirectory, final String copyBookIdentifier) {
        try {
            for (final File copyBookCandidate : Files.walk(copyBooksDirectory.toPath()).map(Path::toFile)
                    .collect(Collectors.toList())) {
                if (isMatchingCopyBook(copyBookCandidate, copyBooksDirectory, copyBookIdentifier)) {
                    return copyBookCandidate;
                }
            }
        } catch (final IOException e) {
        }

        return null;
    }

    protected boolean isMatchingCopyBook(final File copyBookCandidate, final File cobolCopyDir,
                                         final String copyBookIdentifier) {
        final boolean result;

        if (cobolCopyDir == null) {
            result = isMatchingCopyBookRelative(copyBookCandidate, copyBookIdentifier);
        } else {
            result = isMatchingCopyBookAbsolute(copyBookCandidate, cobolCopyDir, copyBookIdentifier);
        }

        return result;
    }

    protected boolean isMatchingCopyBookAbsolute(final File copyBookCandidate, final File cobolCopyDir,
                                                 final String copyBookIdentifier) {
        final Path copyBookCandidateAbsolutePath = Paths.get(copyBookCandidate.getAbsolutePath()).normalize();
        final Path copyBookIdentifierAbsolutePath = Paths.get(cobolCopyDir.getAbsolutePath(), copyBookIdentifier)
                .normalize();
        final boolean result = copyBookIdentifierAbsolutePath.toString()
                .equalsIgnoreCase(copyBookCandidateAbsolutePath.toString());
        return result;
    }

    protected boolean isMatchingCopyBookRelative(final File copyBookCandidate, final String copyBookIdentifier) {
        final Path copyBookCandidateAbsolutePath = Paths.get(copyBookCandidate.getAbsolutePath()).normalize();
        final Path copyBookIdentifierRelativePath = Paths.get("/" + copyBookIdentifier).normalize();

        final boolean result = copyBookCandidateAbsolutePath.toString().toLowerCase()
                .endsWith(copyBookIdentifierRelativePath.toString().toLowerCase());
        return result;
    }
}
