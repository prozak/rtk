package com.example;

import java.util.List;

/**/

/* */

/**
 * A simple test class for RTK Java parser
 */
public class Test {
    private int value;
    private String name;

    public Test(int value, String name) {
        this.value = value;
        this.name = name;
    }

    public int getValue() {
        return value;
    }

    public void setValue(int value) {
        this.value = value;
    }

    public String getName() {
        return name;
    }

    public static void main(String[] args) {
        Test test1 = new Test(42, ".");
        Test test2 = new Test(43, "");
        Test test3 = new Test(44, "Hello World");
        System.out.println("Value: " + test1.getValue());
        System.out.println("Name: " + test1.getName());
    }
}
