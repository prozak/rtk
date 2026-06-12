package test;

interface I {
    default int m() { return 1; }

    default String greet(String name) {
        return name;
    }

    int abstractOne();
}

class WithSwitch {
    int pick(int x) {
        switch (x) {
            case 1:
                return 10;
            default:
                return 0;
        }
    }
}

@interface WithDefault {
    int value() default 5;
}
