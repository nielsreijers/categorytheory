using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using CategoryTheory;

namespace TestProject {
    [TestClass]
    public class Chapter6Test {

        [TestMethod]
        public void TestShapes() {
            Chapter6.Shape c = new Chapter6.Circle() { r = 10 };
            Chapter6.Shape r = new Chapter6.Rect() { h = 10, d = 10 };

            Assert.AreEqual(Math.PI * 100, c.area());
            Assert.AreEqual(100, r.area());

            Assert.AreEqual(2 * Math.PI * 10, c.circ());
            Assert.AreEqual(40, r.circ());
        }
    }
}
