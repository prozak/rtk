package test;

class VolatileFields {
    volatile int counter;
    private volatile boolean initialized;
    static volatile Object ref;

    volatile int get() { return counter; }
}

strictfp class StrictClass {
    double d = 0.1;

    strictfp double sum(double a, double b) {
        return a + b;
    }
}

interface WithStrictfp {
    // modifiers combine freely on one list
    static strictfp double half(double x) { return x / 2.0; }
}
