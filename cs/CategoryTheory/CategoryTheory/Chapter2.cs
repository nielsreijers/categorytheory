using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace CategoryTheory {
    public class Chapter2 {
        public static Func<A, B> Memoize<A, B>(Func<A, B> f) {
            Dictionary<A, B> results = new Dictionary<A, B>();
            return x => {
                if (!results.ContainsKey(x)) {
                    results[x] = f(x);
                }
                return results[x];
            };
        }
    }
}
