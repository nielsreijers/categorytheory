using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace CategoryTheory {
    public static class Chapter1 {
        public static T Identity<T>(T x) => x;

        public static Func<A, C> Compose<A, B, C>(Func<A, B> f, Func<B, C> g) => x => g(f(x));
    }
}
