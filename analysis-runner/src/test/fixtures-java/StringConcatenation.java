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

    public String alias(String a){
        StringBuilder sb1 = new StringBuilder();
        StringBuilder sb2 = new StringBuilder();


        if(Math.random() > 0.5){
            sb2 = sb1;
        }

        sb2.append(a);

        return sb1.toString();
    }

    public String transitiveAlias(String a){
        StringBuilder sb1 = new StringBuilder();
        StringBuilder sb2 = new StringBuilder();
        StringBuilder sb3 = new StringBuilder();

        if(Math.random() > 0.5){
            sb3 = sb1;
        }


        if(Math.random() > 0.5){
            sb2 = sb1;
        }

        sb2.append(a);

        return sb3.toString();
    }


}