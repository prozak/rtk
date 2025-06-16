package com.example;

import java.util.List;

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
        Test test = new Test(42, "Hello World");
        System.out.println("Value: " + test.getValue());
        System.out.println("Name: " + test.getName());
    }
}