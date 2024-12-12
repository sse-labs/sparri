public class NoTaints {

    public void doNothing(){
        int a = 0;
        int b = a;
        boolean c = (a+b) == 0;
        System.out.println(b);
        if(a > 0){
            System.out.println(a);
        } else {
            System.out.println(b);
        }
    }

}