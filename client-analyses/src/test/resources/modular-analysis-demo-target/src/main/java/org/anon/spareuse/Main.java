package org.anon.spareuse;

import com.google.gson.Gson;

public class Main {
    public static void main(String[] args) {
        Gson g = new Gson();

        User u = new User(0, "dummy");

        String json = g.toJson(u);

        User u2 = g.fromJson(json, User.class);

        System.out.println(u2.userId);
    }
}