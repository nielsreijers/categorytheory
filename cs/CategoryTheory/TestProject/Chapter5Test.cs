using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using CategoryTheory;

namespace TestProject {
    [TestClass]
    public class Chapter5Test {

        [TestMethod]
        public void TestEither() {
            Chapter5.Either<double, int> d = new Chapter5.Either<double, int>(1.1);
            Chapter5.Either<double, int> i = new Chapter5.Either<double, int>(42);

            Func<double, string> f = x => x.ToString();
            Func<int, string> g = x => x.ToString();

            Assert.AreEqual("1.1", d.Factorizer(f, g));
            Assert.AreEqual("42", i.Factorizer(f, g));
        }
    }
}
