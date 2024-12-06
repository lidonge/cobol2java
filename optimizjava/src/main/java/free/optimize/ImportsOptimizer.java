package free.optimize;

/**
 * @author lidong@date 2024-12-06@version 1.0
 */

import com.github.javaparser.ast.CompilationUnit;
import com.github.javaparser.ast.ImportDeclaration;
import com.github.javaparser.ast.expr.Name;
import com.github.javaparser.ast.expr.SimpleName;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class ImportsOptimizer {
    public void optimize(CompilationUnit compilationUnit) {
        // Get all the import statements
        List<ImportDeclaration> imports = compilationUnit.getImports();
        // Find the maximum line number of import statements
        int maxImportLine = getMaxImportLine(imports);
        // Retrieve all the symbols used in the current code
        List<Name> names = compilationUnit.findAll(Name.class);
        // Collect symbols actually used in the code (ignoring import section)
        Set<String> usedSymbols = collectUsedSymbols(compilationUnit, maxImportLine);

        // Keep only the import statements that are used
        List<ImportDeclaration> usedImports = new ArrayList<>();
        for (ImportDeclaration importDecl : imports) {
            String importName = importDecl.getNameAsString();
            if(importDecl.toString().indexOf("*") != -1){
                usedImports.add(importDecl);
                continue;
            }
            String simpleImportName = getSimpleName(importName);
            for (String symbol : usedSymbols) {
                if (symbol.equals(simpleImportName) || symbol.equals(importName)) {
                    usedImports.add(importDecl);
                    break;
                }
            }
        }

        // Clear unused import statements
        compilationUnit.getImports().clear();
        for (ImportDeclaration usedImport : usedImports) {
            compilationUnit.addImport(usedImport);
        }
    }

    /**
     * Get the maximum line number of import statements
     *
     * @param imports All import declarations
     * @return The maximum line number
     */
    private static int getMaxImportLine(List<ImportDeclaration> imports) {
        int maxLine = 0;
        for (ImportDeclaration importDecl : imports) {
            if (importDecl.getRange().isPresent()) {
                int importLine = importDecl.getRange().get().end.line;
                if (importLine > maxLine) {
                    maxLine = importLine;
                }
            }
        }
        return maxLine;
    }

    /**
     * Collect symbols actually used in the code
     *
     * @param compilationUnit The compilation unit
     * @param maxImportLine   The maximum line number of import statements
     * @return A set of symbols that are used
     */
    private static Set<String> collectUsedSymbols(CompilationUnit compilationUnit, int maxImportLine) {
        Set<String> usedSymbols = new HashSet<>();
        // Get all simple names in the code (e.g., method names, variable names, class names)
        List<SimpleName> names = compilationUnit.findAll(SimpleName.class);

        for (SimpleName name : names) {
            if (name.getRange().isPresent() && name.getRange().get().begin.line > maxImportLine) {
                usedSymbols.add(name.asString());
            }
        }

        return usedSymbols;
    }

    /**
     * Extract the simple name of a class
     *
     * @param fullName The fully qualified name of the class
     * @return The simple name of the class
     */
    private static String getSimpleName(String fullName) {
        int lastDotIndex = fullName.lastIndexOf('.');
        if (lastDotIndex == -1) {
            return fullName; // If there is no dot, it is already a simple name
        }
        return fullName.substring(lastDotIndex + 1);
    }
}