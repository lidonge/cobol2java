package free.cobol2java.util;

/**
 * @author lidong@date 2025-02-27@version 1.0
 */
import java.io.*;
import java.nio.file.*;
import java.util.*;

public class CobolFilePreprocess {

    public static void process(String inputFile) {
        String outputFile = inputFile;  // 输出文件

        try {
            List<String> lines = Files.readAllLines(Paths.get(inputFile));
            List<String> processedLines = new ArrayList<>();

            for (String line : lines) {
                // 处理每一行
                String processedLine = processLine(line);
                processedLines.add(processedLine);
            }

            // 将处理后的内容写回到文件
            Files.write(Paths.get(outputFile), processedLines);

            System.out.println("文件处理完成，结果已保存至：" + outputFile);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    // 处理每一行，删除前面的字符并填充空格，改正拼写错误
    private static String processLine(String line) {
        // 将拼写错误ENVRONMENT改成ENVIRONMENT
        line = line.replace("ENVRONMENT", "ENVIRONMENT");

        // 检查第7列是否为*，并且第6列之前不是空白
        if (line.length() >= 7 && line.charAt(6) == '*' && !isWhitespaceBefore(line, 6)) {
            // 删除前面的字符，空格填充
            String modifiedLine = "      " + line.substring(6);  // 保留6个空格
            return modifiedLine;
        }

        return line;
    }

    // 检查第6列之前是否为空白字符
    private static boolean isWhitespaceBefore(String line, int index) {
        for (int i = 0; i < index; i++) {
            if (!Character.isWhitespace(line.charAt(i))) {
                return false;
            }
        }
        return true;
    }
}
