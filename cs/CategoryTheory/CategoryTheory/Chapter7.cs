using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace CategoryTheory {
    public static class Chapter7 {
        public static class Reader {
            public static Func<Func<R, A>, Func<R, B>> fmap<R, A, B>(Func<A, B> f) =>
                reader => (x => f(reader(x)));

            public static Func<R, B>fmap<R, A, B>(Func<A, B> f, Func<R, A> reader) =>
                x => f(reader(x));
        }
    }
}
