package free.cobol2java.java;

/**
 * @author lidong@date 2024-10-12@version 1.0
 */
public class TallyingTarget {
    private String target;
    private BeforeAfter[] beforeAfters;

    public TallyingTarget(String target,BeforeAfter... beforeAfters) {
        this.target = target;
        this.beforeAfters = beforeAfters;
    }

    public String getTarget() {
        return target;
    }

    public BeforeAfter[] getBeforeAfters() {
        return beforeAfters;
    }
}
