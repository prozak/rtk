package test;

public class AnonClass {
    private static final ToStringStyle STYLE = new ToStringStyle() { };

    public void run() {
        new ToStringStyle() { };
        Runnable r = new Runnable() {
            private int count = 0;

            public void run() {
                count = count + 1;
            }
        };
        r.run();
        helper(new java.util.ArrayList<String>() { });
        new Thread(new Runnable() {
            public void run() {
                helper(null);
            }
        }).start();
    }

    private void helper(Object o) {
    }
}
