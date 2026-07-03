package test;

class ForUpdateList {
    void twoUpdates(int n) {
        for (int i = 0; i < n; i++, n--) {
        }
    }

    void mixedUpdates(int[] a) {
        int pos = 0;
        long time = 0;
        for (int i = 0; i < a.length; i++, pos++, time += 1000) {
            a[pos] = a[i];
        }
    }

    // regression guards: single update and empty update keep parsing
    void singleUpdate(int n) {
        for (int i = 0; i < n; i++) {
        }
    }

    void emptyUpdate(int n) {
        for (int i = 0; i < n;) {
            i++;
        }
    }
}
