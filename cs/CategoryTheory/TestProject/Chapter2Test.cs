using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using CategoryTheory;

namespace TestProject {
    [TestClass]
    public class Chapter2Test {
        [TestMethod]
        public void TestMemoize() {
            int callCount = 0;
            Func<int, int> f = x => {
                callCount++;
                return x + 1;
            };
            Func<int, int> memoizedF = Chapter2.Memoize(f);

            Assert.AreEqual(1, memoizedF(0));
            Assert.AreEqual(1, memoizedF(0));
            Assert.AreEqual(1, memoizedF(0));
            Assert.AreEqual(1, callCount);
            Assert.AreEqual(101, memoizedF(100));
            Assert.AreEqual(2, callCount);
            Assert.AreEqual(101, memoizedF(100));
            Assert.AreEqual(1001, memoizedF(1000));
            Assert.AreEqual(3, callCount);
        }

        [TestMethod]
        public void MemoizeRandom() {
            Func<int, int> f = max => new Random().Next(0, max);
            var memoized = Chapter2.Memoize<int, int>(f);

            int firstValue = memoized(1000000);
            Assert.AreEqual(firstValue, memoized(1000000));
            Assert.AreNotEqual(firstValue, f(1000000)); // Unless we're really unlucky
            Assert.AreEqual(firstValue, memoized(1000000));
            Assert.AreNotEqual(firstValue, f(1000000)); // Unless we're really unlucky
            Assert.AreEqual(firstValue, memoized(1000000));
            Assert.AreNotEqual(firstValue, f(1000000)); // Unless we're really unlucky
            Assert.AreEqual(firstValue, memoized(1000000));
            Assert.AreNotEqual(firstValue, f(1000000)); // Unless we're really unlucky
            Assert.AreEqual(firstValue, memoized(1000000));
            Assert.AreNotEqual(firstValue, f(1000000)); // Unless we're really unlucky
        }

        [TestMethod]
        public void MemoizeSeededRandom() {
            Func<int, int> f = seed => new Random(seed).Next(0, Int32.MaxValue);
            var memoized = Chapter2.Memoize<int, int>(f);

            int firstValue = memoized(1000000);
            Assert.AreEqual(firstValue, memoized(1000000));
            Assert.AreEqual(firstValue, f(1000000));
            Assert.AreEqual(firstValue, memoized(1000000));
            Assert.AreEqual(firstValue, f(1000000));
            Assert.AreEqual(firstValue, memoized(1000000));
            Assert.AreEqual(firstValue, f(1000000));
            Assert.AreEqual(firstValue, memoized(1000000));
            Assert.AreEqual(firstValue, f(1000000));
            Assert.AreEqual(firstValue, memoized(1000000));
            Assert.AreEqual(firstValue, f(1000000));
        }

        [TestMethod]
        public void BoolToBool() {
            Func<bool, bool> booleanTrue = _ => true;
            Func<bool, bool> booleanFalse = _ => false;
            Func<bool, bool> booleanIdentity = x => x;
            Func<bool, bool> booleanNot = x => !x;

            Assert.AreEqual(true, booleanTrue(true));
            Assert.AreEqual(true, booleanTrue(false));
            Assert.AreEqual(false, booleanFalse(true));
            Assert.AreEqual(false, booleanFalse(false));
            Assert.AreEqual(true, booleanIdentity(true));
            Assert.AreEqual(false, booleanIdentity(false));
            Assert.AreEqual(false, booleanNot(true));
            Assert.AreEqual(true, booleanNot(false));
        }
    }
}
