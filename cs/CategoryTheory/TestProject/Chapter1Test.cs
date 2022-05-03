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
        public void TestIdentity() {
            Assert.AreEqual(42, Chapter1.Identity(42));
            Chapter1Test dummy = new Chapter1Test();
            Assert.AreEqual(dummy, Chapter1.Identity(dummy));
        }

        [TestMethod]
        public void TestComposition() {
            Func<string, int> f = x => x.Length;
            Func<int, double> g = x => x / 2.0;
            Assert.AreEqual(1.5, Chapter1.Compose(f, g)("123"));
        }
    }
}