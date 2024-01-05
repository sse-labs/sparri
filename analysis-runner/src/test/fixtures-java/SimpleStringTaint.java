public class SimpleStringTaint {

    private static String field = "save";

    public static void main(String[] args){
        String tainted = source();

        String save = "some-hardcoded-string";

        field = StringConcatHelper.concatStrings(tainted, save);
        save = tainted;

        sink(field);
    }

    public static String source(){
        return "tainted";
    }

    public static void sink(String foo){
        return;
    }

}