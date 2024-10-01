public class StringConcatenation {

    public String add(String a, String b){
        String temp = b;
        temp = "NO_TAINT";

        String ret = a+temp;

        return ret;
    }


}