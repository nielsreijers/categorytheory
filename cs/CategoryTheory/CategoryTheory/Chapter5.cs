using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace CategoryTheory {
    public class Chapter5 {
        public class Either<A, B> {
            private A left;
            private B right;
            private bool isLeft;
            public Either(A Left) {
                this.left = Left;
                this.isLeft = true;
            }
            public Either(B Right) {
                this.right = Right;
                this.isLeft = false;
            }
            public C Factorizer<C>(Func<A, C> f, Func<B, C> g)
                => this.isLeft
                    ? f(this.left)
                    : g(this.right);
        }
    }
}
