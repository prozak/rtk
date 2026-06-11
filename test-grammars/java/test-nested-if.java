// Dangling else: a brace-less nested if, with else binding to the NEAREST if
// (JLS 14.5). The then-branch of IfStatement must be a full Statement.
class NestedIf {
    int direct(int a, int b) {
        if (a > 0)
            if (b > 0)
                return 1;
            else
                return 2;
        return 3;
    }

    void throughLoop(int a, int b) {
        // the inner if sits under a while which sits under the outer if;
        // the else still binds to the innermost if
        if (a > 0) while (b > 0) if (a > b) g(); else h();
        if (a > 0) if (b > 0) g(); else h(); else g();
    }

    void g() { }
    void h() { }
}
