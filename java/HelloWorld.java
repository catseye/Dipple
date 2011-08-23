import java.io.*;

class HelloWorld
{
    public static void main(String args[])
    {
        Foo f = new Foo<Integer>(new Integer(4));
        f.hello();
    }
}

class Foo<Y>
{
    private Y state;
    Foo(Y s)
    {
        state = s;
    }
    
    void hello()
    {
        System.out.println("Hello World!" + state.toString());
    }
}
