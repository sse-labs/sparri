public class StringConcatenation {

    public String add(String a, String b){
        String temp = b;
        temp = "NO_TAINT";

        String ret = a+temp;

        return ret;
    }

    public String initTaint(String a){

        StringBuilder sb = new StringBuilder(a);

        return sb.toString();
    }

    public String replace(String a){
        StringBuilder sb = new StringBuilder();

        sb.append("NO_TAINT");
        sb.replace(0,5,a);
        return sb.toString();
    }

    public String insert(String a){
        StringBuilder sb = new StringBuilder();

        sb.append("NO_TAINT");
        sb.insert(0,a);
        return sb.toString();
    }


}