class Operators {
    void m() {
        int a = 1 + 2 - 3 * 4 / 5 % 6;
        a += 1;
        a -= 1;
        a *= 2;
        a /= 2;
        a %= 2;
        a <<= 1;
        a >>= 1;
        a >>>= 1;
        a &= 1;
        a |= 1;
        a ^= 1;
        int b = a << 1 | a >> 2 & a >>> 3 ^ ~a;
        boolean c = a < b || a > b && a <= b | a >= b & a == b ^ a != b;
        boolean d = !c;
        a++;
        a--;
        ++a;
        --a;
        int e = c ? a : b;
        Runnable r = () -> m();
        IntUnaryOperator f = x -> x - 1;
        Supplier<Operators> s = Operators::new;
        IntPredicate p = Operators::odd;
        boolean g = a-- > b;
    }
}
