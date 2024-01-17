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

    @Override
    public String toString(){
        return "foobar";
    }

    @Override
    public void recurse(Calls c){
        return;
    }

    @Override
    public void doRecursiveCalls(){
        return;
    }

}