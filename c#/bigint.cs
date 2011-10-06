using System;
using System.Numeric; // Apparently this isn't supported yet in Mono...

namespace Big {
    class Big {
        static void Main(string[] args) {
            String s = Console.ReadLine();
            BigInteger x = BigInteger.Parse(s);
            Console.WriteLine(x.ToString());
        }
    }
}