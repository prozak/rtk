package test;

public enum Documented {
    /** First constant. */
    ALPHA,

    /** Second constant, annotated, with arguments. */
    @Deprecated
    BETA(2),

    /** Third, with a body. */
    GAMMA {
        void hook() { }
    };

    /** Field doc. */
    private int value;

    Documented() { }

    Documented(int v) {
        value = v;
    }
}
