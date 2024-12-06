package free.optimize;

/**
 * @author lidong@date 2024-12-06@version 1.0
 */

import java.io.*;
import java.nio.file.*;
import java.util.ArrayList;
import java.util.List;

public class JavaFileFilter {

    private String matchedLineStarts;

    public JavaFileFilter(String matchedLineStarts) {
        this.matchedLineStarts = matchedLineStarts;
    }

    public List<File> find(String rootDirectory) {
        // Root directory path
        List<File> matchingFiles = new ArrayList<>();

        // Recursively traverse the directory
        findMatchingJavaFiles(new File(rootDirectory), matchingFiles);

        return matchingFiles;
    }

    /**
     * Recursively search for Java files that meet the conditions
     *
     * @param dir           Current directory
     * @param matchingFiles List to store files that match the criteria
     */
    private void findMatchingJavaFiles(File dir, List<File> matchingFiles) {
        if (dir == null || !dir.exists()) return;

        // Traverse all files and subdirectories in the current directory
        File[] files = dir.listFiles();
        if (files == null) return;

        for (File file : files) {
            if (file.isDirectory()) {
                // Recursively process subdirectories
                findMatchingJavaFiles(file, matchingFiles);
            } else if (file.isFile() && file.getName().endsWith(".java")) {
                // Check if the file meets the criteria
                if (containsTargetLine(file)) {
                    matchingFiles.add(file);
                }
            }
        }
    }

    /**
     * Check if the file contains the target content
     *
     * @param file The file to check
     * @return true if it contains the target content, false otherwise
     */
    private boolean containsTargetLine(File file) {
        try (BufferedReader reader = new BufferedReader(new FileReader(file))) {
            String line;
            while ((line = reader.readLine()) != null) {
                // Check if the line starts with the specified string
                if (line.trim().startsWith(matchedLineStarts)) {
                    return true;
                }
            }
        } catch (IOException e) {
            System.err.println("Failed to read file: " + file.getAbsolutePath());
            e.printStackTrace();
        }
        return false;
    }
}