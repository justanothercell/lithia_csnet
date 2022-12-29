class Module {
    public static int my_cool_fn(int arg1, int arg2){
        return arg2 - arg1;
    }

    public static void Main() {
        System.Console.WriteLine("Hello, world!");
        int a = 12;
        int b = 4;
        int c = 255;
        c -= b * 4;
        int r = Module.my_cool_fn(a, c);
        System.Console.WriteLine("answer: " + r);
        a_module.Module.a_foo();
        a_module.Module.a_bar();
        b_module.Module.b_foo();
        b_module.Module.b_bar();
        c_module.Module.c_foo();
        c_module.Module.c_foo();
    }
}