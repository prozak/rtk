package test;

public class ArrayInit {
    private static final char[] LETTERS = new char[] {'A', 'B'};
    private static final int[][] GRID = new int[][] {{1}, {2}};

    public int[] make() {
        new char[] {'A'};
        int[] empty = new int[] {};
        long[] trailing = new long[] {1, 2, 3,};
        String[] strings = new String[] {"a", "b"};
        use(new Object[] {null, "x", 1});
        return new int[] {1, 2};
    }

    private void use(Object[] os) {
    }
}
