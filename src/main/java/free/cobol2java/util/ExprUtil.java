package free.cobol2java.util;

import java.util.HashMap;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * @author lidong@date 2024-08-15@version 1.0
 */
public class ExprUtil {

    public static String convertExpression(String expr) {
        expr = replaceExponentiation(expr);
        String[] parts = expr.split("\\^");
        String ret = "";
        Map<String,String> codeToExpr = new HashMap<>();
        for(int i =0;i<parts.length;i+=2){
            {
                String left = parts[i].trim();
                if (left.charAt(left.length() - 1) == ')') {
                    left = getSubstringBetweenLastParentheses(left);
                    ret += parts[i].substring(0, parts[i].indexOf(left));
                    String code = "left" + i + "_" + left.hashCode();
                    codeToExpr.put(code, left);
                    ret += code;
                } else
                    ret += parts[i];
            }

            {
                String right = parts[i + 1].trim();
                if (right.charAt(0) == '(') {
                    right = getSubstringBetweenFirstParentheses(right);
                    String code = "right" + i + "_" + right.hashCode();
                    codeToExpr.put(code, right);
                    ret += code;
                    ret += parts[i].substring(right.length(), parts[i].length());
                } else
                    ret += '^' + parts[i];
            }
        }
        ret = replaceExponentiation(ret);
        for(Map.Entry<String,String> entry:codeToExpr.entrySet()){
            ret = ret.replace(entry.getKey(),entry.getValue());
        }
        return ret;
    }
    /**
     * Finds the substring between the last ')' and its matching '('.
     *
     * @param str The input string.
     * @return The substring between the last ')' and its matching '('.
     */
    public static String getSubstringBetweenLastParentheses(String str) {
        int counter = 0;
        int lastOpenIndex = -1;
        int lastCloseIndex = -1;

        // Traverse the string backwards
        for (int i = str.length() - 1; i >= 0; i--) {
            char ch = str.charAt(i);

            if (ch == ')') {
                // Increment counter for every ')'
                counter++;
                if (lastCloseIndex == -1) {
                    lastCloseIndex = i; // Track the position of the last ')'
                }
            } else if (ch == '(') {
                // Decrement counter for every '('
                counter--;
                if (counter == 0) {
                    lastOpenIndex = i; // Track the position of the matching '('
                    break; // Stop when we find the matching '('
                }
            }
        }

        // Check if we found matching parentheses
        if (lastOpenIndex != -1 && lastCloseIndex != -1 && lastOpenIndex < lastCloseIndex) {
            return str.substring(lastOpenIndex , lastCloseIndex + 1);
        }

        // Return an empty string if no matching parentheses are found
        return "";
    }

    /**
     * Finds the substring between the first '(' and its matching ')'.
     *
     * @param str The input string.
     * @return The substring between the first '(' and its matching ')'.
     */
    public static String getSubstringBetweenFirstParentheses(String str) {
        int counter = 0;
        int firstOpenIndex = -1;
        int matchingCloseIndex = -1;

        // Traverse the string from left to right
        for (int i = 0; i < str.length(); i++) {
            char ch = str.charAt(i);

            if (ch == '(') {
                if (counter == 0) {
                    // Record the index of the first '('
                    firstOpenIndex = i;
                }
                // Increment counter for every '('
                counter++;
            } else if (ch == ')') {
                // Decrement counter for every ')'
                counter--;
                if (counter == 0 && firstOpenIndex != -1) {
                    // Record the index of the matching ')'
                    matchingCloseIndex = i;
                    break; // Stop when the first matching ')' is found
                }
            }
        }

        // Check if we found matching parentheses
        if (firstOpenIndex != -1 && matchingCloseIndex != -1 && firstOpenIndex < matchingCloseIndex) {
            return str.substring(firstOpenIndex, matchingCloseIndex + 1);
        }

        // Return an empty string if no matching parentheses are found
        return "";
    }
    /**
     * Converts expressions of the form expr1 ^ expr2 to Math.pow(expr1, expr2).
     *
     * @param expr The input expression containing ^ operators.
     * @return The converted expression with Math.pow.
     */
    private static String replaceExponentiation(String expr) {
        // Regex to match expr1 ^ expr2
        String regex = "(\\b\\w+\\b)\\s*\\^\\s*(\\b\\w+\\b)";
        Pattern pattern = Pattern.compile(regex);
        Matcher matcher = pattern.matcher(expr);

        StringBuffer result = new StringBuffer();
        while (matcher.find()) {
            // Replace ^ with Math.pow(expr1, expr2)
            String replacement = "Math.pow(" + matcher.group(1) + ", " + matcher.group(2) + ")";
            matcher.appendReplacement(result, replacement);
        }
        matcher.appendTail(result);

        return result.toString();
    }

}
