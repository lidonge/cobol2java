package free.optimize;

import free.servpp.mustache.handler.MustacheWriter;

import java.net.URI;
import java.util.List;

/**
 * @author lidong@date 2025-01-14@version 1.0
 */
public class MappingMustacheWriter extends MustacheWriter {
    public MappingMustacheWriter(URI mustacheFile, Object currentObj, boolean lookUpParents) {
        super(mustacheFile, currentObj, lookUpParents);
        createCobol2JavaEnvironment();
    }
    private void createCobol2JavaEnvironment() {
        this.getExprEvaluator().setEnvironment(new MustacheEnvironment() {
            @Override
            public void addDefault() {
                super.addDefault();
            }
        });
    }

}
