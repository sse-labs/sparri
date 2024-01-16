public class BranchingTaint {

    public static void main(String[] args){

        String t1 = source();
        String t2 = "untainted";

        if(Math.random() > 0.5){
            t1 = "untainted";
        } else {
            t2 = source();
        }

        int c = 0;
        while (c < 3){
            t2 = "";
            c++;
        }

        sink(t1);
    }

    private static String source() {
        return "tainted";
    }

    private static void sink(String s){
        System.out.println(s);
    }


}