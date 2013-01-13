using System;

class Thing
{
    public int whatever;

    override public String ToString()
    {
        return "Thing(" + whatever + ")";
    }
}

class Program
{
    static void Main()
    {
        Thing t = new Thing();
        t.whatever = 20;
        Console.WriteLine(t.ToString());
        Nougat(t);
        Console.WriteLine(t.ToString());
        Splunge(ref t);
        Console.WriteLine(t.ToString());
        Plucky(out t);
        Console.WriteLine(t.ToString());
    }

    static void Nougat(Thing t)
    {
        t.whatever = 10;
    }

    static void Splunge(ref Thing t)
    {
        Thing r = new Thing();
        r.whatever = 17;
        Console.WriteLine("t was " + t);
        t = r;
    }

    static void Plucky(out Thing t)
    {
        Thing r = new Thing();
        r.whatever = 8;
        // Can't do this: Console.Write("t was " + t);
        t = r;
    }
}
