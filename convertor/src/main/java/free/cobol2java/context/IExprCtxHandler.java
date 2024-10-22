package free.cobol2java.context;

import io.proleap.cobol.CobolLexer;
import io.proleap.cobol.CobolParser;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.tree.TerminalNode;

import java.util.ArrayList;
import java.util.List;

import static io.proleap.cobol.CobolLexer.*;

/**
 * @author lidong@date 2024-09-29@version 1.0
 */
public interface IExprCtxHandler extends IExprEnvContext{
    String LENGTHOF = "LENGTHOF";
    String FUNCTION = "FUNCTION";
    record PropOfField(String id, List<String> ofId) {
    }

    default String getCtxText(ParserRuleContext ctx, List<Object> ofIds) {
        getPropOfIds(ctx, ofIds);
        String ret = "";
        int begTable = -1;
        for (int i = 0; i < ctx.getChildCount(); i++) {
            if (i != 0)
                ret += " ";
            String text = ctx.getChild(i).getText();
            if(text.equals("("))
                begTable = 0;
            else if(text.equals(")"))
                begTable = -1;
            if(begTable != -1){
                if(text.equals(","))
                    continue;
                if(begTable > 1)
                    ret +=",";
                begTable++;
            }

            if (text.startsWith("'") && text.endsWith("'"))
                text = "\"" + text.substring(1, text.length() - 1) + "\"";
            ret += text;
        }
        return ret;
    }


    private void getPropOfIds(ParserRuleContext ctx, List<Object> ofIds) {
        List<TerminalNode> list = new ArrayList<>();
        IExprBaseContext.getAllTerm(ctx, list);
        boolean isOf = false;
        String prevNode = null;
        boolean isLengthOf = false;
        boolean isFunction = false;
        for (TerminalNode node : list) {
            String text = node.getText();
            int symbolType = node.getSymbol().getType();
            switch (symbolType) {
                case CobolLexer.FUNCTION:
                    isFunction = true;
                    break;
                case CobolLexer.OF: {
                    if (!isLengthOf)
                        isOf = true;
                    break;
                }
                case CobolLexer.LENGTH:
                    isLengthOf = true;
                    break;
                case CobolLexer.IDENTIFIER: {
                    if (isOf) {
                        isOf = false;
                        List<String> ofs = null;
                        if (prevNode != null) {
                            String id = prevNode;
                            String ofField = node.getText();
                            ofs = new ArrayList<>();
                            ofs.add(ofField);
                            PropOfField propOfField = new PropOfField(id, ofs);
                            ofIds.add(propOfField);
                        } else {
                            PropOfField propOfField = (PropOfField) ofIds.get(ofIds.size() - 1);
                            propOfField.ofId.add(node.getText());
                        }
                        prevNode = null;
                        continue;
                    } else if (prevNode != null) {
                        if (isFunction) {
                            isFunction = false;
                            text = FUNCTION + text;
                        }
                        if (isLengthOf) {
                            isLengthOf = false;
                            text = LENGTHOF + text;
                        }
                        ofIds.add(prevNode);
                    }else{
                        if (isFunction) {
                            isFunction = false;
                            text = FUNCTION + text;
                        }
                        if (isLengthOf) {
                            isLengthOf = false;
                            text = LENGTHOF + text;
                        }
                    }
                    prevNode = text;
                    break;
                }
                case NONNUMERICLITERAL:
                case INTEGERLITERAL:
                case NUMERICLITERAL:
                    ofIds.add(prevNode);
                    prevNode = null;
                    break;
            }
        }
        if (prevNode != null)
            ofIds.add(prevNode);
    }

}
