package test;

class A {
    <T> void m() { }

    <K, V> int count(K k, V v) { return 0; }

    <T> T id(T t) { return t; }

    <T> int[] arr() { return null; }

    static <E> void each(E[] items) { }

    <T> boolean test(T t) { return true; }
}
