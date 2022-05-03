using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using CategoryTheory;

namespace TestProject {
    [TestClass]
    public class Chapter1Test {
        [TestMethod]
        public void Test1_1() {
            Assert.AreEqual(42, Chapter1.Identity(42));
            Chapter1Test dummy = new Chapter1Test();
            Assert.AreEqual(dummy, Chapter1.Identity(dummy));
        }

        [TestMethod]
        public void Test1_2() {
            Func<string, int> f = x => x.Length;
            Func<int, double> g = x => x / 2.0;
            Assert.AreEqual(1.5, Chapter1.Compose(f, g)("123"));
        }

        public void TestCompositionRespectsIdentity<A, B>(Func<A, B> f, A x) {
            var fid = Chapter1.Compose(f, Chapter1.Identity);
            var idf = Chapter1.Compose<A, A, B>(Chapter1.Identity, f);
            var fx = f(x);
            Assert.AreEqual(fx, fid(x));
            Assert.AreEqual(fx, idf(x));
        }

        [TestMethod]
        public void Test1_3() {
            TestCompositionRespectsIdentity((string x) => x.Length, "hello world");
            TestCompositionRespectsIdentity((int x) => x / 2, 41);
            TestCompositionRespectsIdentity((double x) => x / 2, 2.0/3.0);
        }
    }
}