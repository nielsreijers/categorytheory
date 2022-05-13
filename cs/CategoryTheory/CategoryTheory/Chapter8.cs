using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace CategoryTheory {
    public static class Chapter8 {

        public interface IBifunctor<A, B> {
            IBifunctor<C, D> Bimap<C, D>(Func<A, C> f, Func<B, D> g);
        }

        // Does not work until covariant return types for interfaces is added to the language:
        //public class Ideal_Pair<A, B> : IBifunctor<A, B> {
        //    public A item1;
        //    public B item2;

        //    public Ideal_Pair<C, D> Bimap<C, D>(Func<A, C> f, Func<B, D> g)
        //        => new Ideal_Pair<C, D> {
        //            item1 = f(item1),
        //            item2 = g(item2)
        //        };
        //}


        public class InterfacePair<A, B> : IBifunctor<A, B> {
            public A item1;
            public B item2;

            IBifunctor<C, D> IBifunctor<A, B>.Bimap<C, D>(Func<A, C> f, Func<B, D> g)
                => this.Bimap(f, g);

            public InterfacePair<C, D> Bimap<C, D>(Func<A, C> f, Func<B, D> g)
                => new InterfacePair<C, D> {
                    item1 = f(item1),
                    item2 = g(item2)
                };
        }


        public abstract class Bifunctor<A, B> {
            public abstract Bifunctor<C, D> Bimap<C, D>(Func<A, C> f, Func<B, D> g);
        }

        public class SubclassPair<A, B> : Bifunctor<A, B> {
            public A item1;
            public B item2;

            public override SubclassPair<C, D> Bimap<C, D>(Func<A, C> f, Func<B, D> g)
                => new SubclassPair<C, D> {
                    item1 = f(item1),
                    item2 = g(item2)
                };
        }
    }
}
