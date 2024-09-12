package free.cobol2java;

import java.io.*;

/**
 * @author lidong@date 2023-11-23@version 1.0
 */
public class CodeFormator {

    public static String formatCode(String code) {
        StringBuilder formattedCode = new StringBuilder();
        String[] lines = code.split("\n");
        int indentLevel = 0;
        boolean inMultilineComment = false;

        for (String line : lines) {
            line = line.trim();

            // Skip empty lines
            if (line.isEmpty()) {
                continue;
            }

            // Handle multiline comments
            if (line.startsWith("/*")) {
                inMultilineComment = true;
            }

            if (inMultilineComment) {
                formattedCode.append(line).append("\n");
                if (line.endsWith("*/")) {
                    inMultilineComment = false;
                }
                continue;
            }

            // Adjust indent level for closing braces
            if (line.endsWith("}") || line.startsWith("}")) {
                indentLevel--;
            }

            // Add the line with the current indent level
            addIndentedLine(formattedCode, line, indentLevel);

            // Adjust indent level for opening braces
            if (line.endsWith("{")) {
                indentLevel++;
            }
        }

        return formattedCode.toString();
    }

    private static void addIndentedLine(StringBuilder formattedCode, String line, int indentLevel) {
        for (int i = 0; i < indentLevel; i++) {
            formattedCode.append("    "); // Each indent level equals 4 spaces
        }
        formattedCode.append(line).append("\n");
    }

    public static void main(String[] args) throws IOException {
        FileInputStream fin = new FileInputStream("/Users/lidong/gitspace/cobol2java/src/main/java/abc.j");
        String s = formatCode(new String(fin.readAllBytes()));
        System.out.println(s);
    }
}
