using System;

class Pair<t1, t2>
{
    private t1 a;
    private t2 b;

    public Pair(t1 a, t2 b)
    {
        this.a = a;
        this.b = b;
    }

    public t1 First
    {
        get { return this.a; }
    }

    public t2 Second
    {
        get { return this.b; }
    }
}

class Program
{
    static void Main()
    {
        //Pair<int, string> p = new Pair<int, string>(77, "hi");
        var p = new Pair<int, string>(77, "hi");
        Console.WriteLine(p.First * 2);
        Console.WriteLine(p.Second);
    }
}