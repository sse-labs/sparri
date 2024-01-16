public class Calls {

    private Object someObj;
    private BranchingTaint bt = new BranchingTaint();

    public void doVirtualCalls() {
        someObj = new BranchingTaint(); // special invoke of constructor
        String s1 = someObj.toString(); // Calls to all instantiated subtypes of object

        Calls c1 = new Calls(); // Invoke Special of constructor for this class
        c1.doStaticCalls(); // Only target is this method, not the one of CallTargetImpl!

        Calls t1 = new CallTargetImpl();
        t1.doStaticCalls(); // Now also the implementation of CallTargetImpl is possible

        CallTargetImpl t2 = new CallTargetImpl();
        t2.doStaticCalls(); // Now only the implementation of CallTargetImpl is possible
    }

    public void doStaticCalls() {
        Calls.doFoo();
        this.doFoo();
    }

    public void doInterfaceCalls() {
        CallTarget t = new CallTargetImpl();
        t.beCalled("");
    }

    public static void doFoo(){
        System.out.println("Foo");
    }




}