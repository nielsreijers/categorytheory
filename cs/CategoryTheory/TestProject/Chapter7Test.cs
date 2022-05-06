using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using CategoryTheory;

namespace TestProject {
    [TestClass]
    public class Chapter7Test {

        [TestMethod]
        public void TestReader() {
            Func<int, double> reader = x => x / 2.0;
            Func<double, string> f = x => $"value: {x}";

            // Curried version
            Assert.AreEqual("value: 1.5", Chapter7.Reader.fmap<int, double, string>(f)(reader)(3));
            // Uncurried version
            Assert.AreEqual("value: 1.5", Chapter7.Reader.fmap<int, double, string>(f, reader)(3));
        }
    }
}
