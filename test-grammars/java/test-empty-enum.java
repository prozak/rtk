package test;

enum EmptyEnum { }

// declarations-only body (JLS 8.9: both the constant list and the
// declarations tail are independently optional)
enum NoConstants {
    ;
    static int helper() { return 1; }
}

// regression guard: the existing forms keep parsing
enum WithConstants {
    ONE, TWO,
}

enum ConstantsAndMembers {
    A, B;
    int m() { return 0; }
}
