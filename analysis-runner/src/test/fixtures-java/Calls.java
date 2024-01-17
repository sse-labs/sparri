public class Calls {

    private Object someObj;
    private BranchingTaint bt = new BranchingTaint();

    public void doVirtualCalls() {
        someObj = new BranchingTaint(); // special invoke of constructor [PC 5]
        String s1 = someObj.toString(); // Calls to all instantiated subtypes of object [PC 15]

        Calls c1 = new Calls(); // Invoke Special of constructor for this class [PC 23]
        c1.doStaticCalls(); // Only target is this method, not the one of CallTargetImpl! [PC 28]

        Calls t1 = new CallTargetImpl(); // [PC 35]
        t1.doStaticCalls(); // Now also the implementation of CallTargetImpl is possible [PC 40]

        CallTargetImpl t2 = new CallTargetImpl(); // [PC 47]
        t2.doStaticCalls(); // Now only the implementation of CallTargetImpl is possible [PC 54]
    }

    public void doStaticCalls() {
        Calls.doFoo();
        this.doFoo();
    }

    public void doInterfaceCalls() {
        CallTarget t = new CallTargetImpl();
        t.beCalled("");
    }

    public void recurse(Calls c){
        c.doRecursiveCalls(); // [PC 1]
        c = new CallTargetImpl(); // [PC 8]
        c.doRecursiveCalls(); // [PC 13]
    }

    public void doRecursiveCalls(){
        Calls c = new Calls(); // [PC 4]
        recurse(c); // [PC 10]
    }

    public static void doFoo(){
        System.out.println("Foo");
    }




}