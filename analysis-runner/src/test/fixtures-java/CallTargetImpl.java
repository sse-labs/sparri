public class CallTargetImpl extends Calls implements CallTarget {

    public CallTargetImpl(){
        super();
    }

    public void beCalled(String param){
        System.out.println(param);
    }

    @Override
    public void doStaticCalls(){
        return;
    }

}