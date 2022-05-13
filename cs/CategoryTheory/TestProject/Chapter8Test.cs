using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using CategoryTheory;

namespace TestProject {
    [TestClass]
    public class Chapter8Test {
        Func<int, string> f = x => (x + 1).ToString();
        Func<string, int> g = x => Int32.Parse(x);

        [TestMethod]
        public void TestInterfacePair() {
            Chapter8.InterfacePair<int, string> p1 = new Chapter8.InterfacePair<int, string> { item1 = 42, item2 = "42" };
            Chapter8.IBifunctor<int, string> p2 = p1;

            Assert.AreEqual(p1.Bimap(f, g).item1, "43");
            Assert.AreEqual(p1.Bimap(f, g).item2, 42);
            Assert.IsInstanceOfType(p2.Bimap(f, g), typeof(Chapter8.IBifunctor<string, int>));
        }

        [TestMethod]
        public void TestSubclassPair() {
            //Chapter8.SubclassPair<int, string> p1 = new Chapter8.SubclassPair<int, string> { item1 = 42, item2 = "42" };
            //Chapter8.Bifunctor<int, string> p2 = p1;

            //Assert.AreEqual(p1.Bimap(f, g).item1, "43");
            //Assert.AreEqual(p1.Bimap(f, g).item2, 42);
            //Assert.IsInstanceOfType(p2.Bimap(f, g), typeof(Chapter8.Bifunctor<string, int>));
        }
    }
}
