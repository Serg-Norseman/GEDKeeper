using System;
using System.Collections.Generic;
using System.Linq;
using NUnit.Framework;

namespace GKCore.NetDiff
{
    [TestFixture]
    public class NetDiffTests
    {
        [Test]
        public void StdCase_General()
        {
            var str1 = "string";
            var str2 = "strength";

            var results = DiffUtil.Diff(str1, str2).ToList();

            Assert.AreEqual(9, results.Count());

            Assert.AreEqual("= s", results[0].ToString());
            Assert.AreEqual("= t", results[1].ToString());
            Assert.AreEqual("= r", results[2].ToString());
            Assert.AreEqual("- i", results[3].ToString());
            Assert.AreEqual("+ e", results[4].ToString());
            Assert.AreEqual("= n", results[5].ToString());
            Assert.AreEqual("= g", results[6].ToString());
            Assert.AreEqual("+ t", results[7].ToString());
            Assert.AreEqual("+ h", results[8].ToString());
        }

        [Test]
        public void StdCase_Equal()
        {
            var str1 = "abcde";
            var str2 = "abcde";

            var results = DiffUtil.Diff(str1, str2);

            Assert.AreEqual(str1.Count(), results.Count());
            Assert.IsTrue(results.All(r => r.Status == DiffStatus.Equal));
        }

        /*
             a b a b a b
             - + - + - +
        */
        [Test]
        public void StdCase_DifferentAll()
        {
            var str1 = "aaa";
            var str2 = "bbb";

            var results = DiffUtil.Diff(str1, str2).ToList();

            Assert.AreEqual("+ b", results.ElementAt(0).ToString());
            Assert.AreEqual("- a", results.ElementAt(1).ToString());
            Assert.AreEqual("- a", results.ElementAt(2).ToString());
            Assert.AreEqual("- a", results.ElementAt(3).ToString());
            Assert.AreEqual("+ b", results.ElementAt(4).ToString());
            Assert.AreEqual("+ b", results.ElementAt(5).ToString());
        }

        /*
             a b c d
             = = = +
        */
        [Test]
        public void StdCase_Appended()
        {
            var str1 = "abc";
            var str2 = "abcd";

            var results = DiffUtil.Diff(str1, str2);

            Assert.AreEqual(DiffStatus.Equal, results.ElementAt(0).Status);
            Assert.AreEqual(DiffStatus.Equal, results.ElementAt(1).Status);
            Assert.AreEqual(DiffStatus.Equal, results.ElementAt(2).Status);
            Assert.AreEqual(DiffStatus.Inserted, results.ElementAt(3).Status);
        }

        /*
             a b c d
             + = = = 
        */
        [Test]
        public void StdCase_Prepended()
        {
            var str1 = "bcd";
            var str2 = "abcd";

            var results = DiffUtil.Diff(str1, str2);

            Assert.AreEqual(DiffStatus.Inserted, results.ElementAt(0).Status);
            Assert.AreEqual(DiffStatus.Equal, results.ElementAt(1).Status);
            Assert.AreEqual(DiffStatus.Equal, results.ElementAt(2).Status);
            Assert.AreEqual(DiffStatus.Equal, results.ElementAt(3).Status);
        }

        [Test]
        public void StdCase_CaseMultiSameScore()
        {
            var str1 = "cdhijkz";
            var str2 = "ldxhnokz";

            var results = DiffUtil.Diff(str1, str2);

            Assert.AreEqual(DiffStatus.Inserted, results.ElementAt(0).Status);
            Assert.AreEqual(DiffStatus.Deleted, results.ElementAt(1).Status);
            Assert.AreEqual(DiffStatus.Equal, results.ElementAt(2).Status);
            Assert.AreEqual(DiffStatus.Inserted, results.ElementAt(3).Status);
            Assert.AreEqual(DiffStatus.Equal, results.ElementAt(4).Status);
            Assert.AreEqual(DiffStatus.Deleted, results.ElementAt(5).Status);
            Assert.AreEqual(DiffStatus.Deleted, results.ElementAt(6).Status);
            Assert.AreEqual(DiffStatus.Inserted, results.ElementAt(7).Status);
            Assert.AreEqual(DiffStatus.Inserted, results.ElementAt(8).Status);
            Assert.AreEqual(DiffStatus.Equal, results.ElementAt(9).Status);
            Assert.AreEqual(DiffStatus.Equal, results.ElementAt(10).Status);
        }

        [Test]
        public void StdCase_CaseRepeat()
        {
            string str1 = "abbbc";
            string str2 = "adbbc";

            var results = DiffUtil.Diff(str1, str2);

            Assert.AreEqual(DiffStatus.Equal, results.ElementAt(0).Status);
            Assert.AreEqual(DiffStatus.Inserted, results.ElementAt(1).Status);
            Assert.AreEqual(DiffStatus.Equal, results.ElementAt(2).Status);
            Assert.AreEqual(DiffStatus.Equal, results.ElementAt(3).Status);
            Assert.AreEqual(DiffStatus.Deleted, results.ElementAt(4).Status);
            Assert.AreEqual(DiffStatus.Equal, results.ElementAt(5).Status);
        }


        [Test]
        public void StdCase_SpecifiedComparer()
        {
            var str1 = "abc";
            var str2 = "dBf";

            var option = new DiffOption<char>();
            option.EqualityComparer = new CaseInsensitiveComparer();

            var results = DiffUtil.Diff(str1, str2, option).ToList();

            Assert.AreEqual("+ d", results[0].ToString());
            Assert.AreEqual("- a", results[1].ToString());
            Assert.AreEqual("= b", results[2].ToString());
            Assert.AreEqual("- c", results[3].ToString());
            Assert.AreEqual("+ f", results[4].ToString());

            Assert.AreEqual("abc", new String(DiffUtil.CreateSrc(results).ToArray()));
            Assert.AreEqual("dBf", new String(DiffUtil.CreateDst(results).ToArray()));
        }

        [Test]
        public void StdCase_CaseReplace()
        {
            string str1 = "abbbc";
            string str2 = "adbbc";

            var results = DiffUtil.Diff(str1, str2);

            Assert.AreEqual(DiffStatus.Equal, results.ElementAt(0).Status);
            Assert.AreEqual(DiffStatus.Inserted, results.ElementAt(1).Status);
            Assert.AreEqual(DiffStatus.Equal, results.ElementAt(2).Status);
            Assert.AreEqual(DiffStatus.Equal, results.ElementAt(3).Status);
            Assert.AreEqual(DiffStatus.Deleted, results.ElementAt(4).Status);
            Assert.AreEqual(DiffStatus.Equal, results.ElementAt(5).Status);
        }

        [Test]
        public void BadCase_Seq1Empty()
        {
            string str1 = "";
            string str2 = "abcde";

            var results = DiffUtil.Diff(str1, str2);

            Assert.AreEqual(DiffStatus.Inserted, results.ElementAt(0).Status);
            Assert.AreEqual(DiffStatus.Inserted, results.ElementAt(1).Status);
            Assert.AreEqual(DiffStatus.Inserted, results.ElementAt(2).Status);
            Assert.AreEqual(DiffStatus.Inserted, results.ElementAt(3).Status);
            Assert.AreEqual(DiffStatus.Inserted, results.ElementAt(4).Status);
        }

        [Test]
        public void BadCase_Seq2Empty()
        {
            string str1 = "abced";
            string str2 = "";

            var results = DiffUtil.Diff(str1, str2);

            Assert.AreEqual(DiffStatus.Deleted, results.ElementAt(0).Status);
            Assert.AreEqual(DiffStatus.Deleted, results.ElementAt(1).Status);
            Assert.AreEqual(DiffStatus.Deleted, results.ElementAt(2).Status);
            Assert.AreEqual(DiffStatus.Deleted, results.ElementAt(3).Status);
            Assert.AreEqual(DiffStatus.Deleted, results.ElementAt(4).Status);
        }

        [Test]
        public void BadCase_Empty()
        {
            var str1 = string.Empty;
            var str2 = string.Empty;

            var results = DiffUtil.Diff(str1, str2);

            Assert.IsTrue(!results.Any());
        }

        [Test]
        public void BadCase_Null()
        {
            string str1 = null;
            var str2 = string.Empty;

            var results = DiffUtil.Diff(str1, str2);

            Assert.IsTrue(!results.Any());
        }

        internal class CaseInsensitiveComparer : IEqualityComparer<char>
        {
            public bool Equals(char x, char y)
            {
                return x.ToString().ToLower().Equals(y.ToString().ToLower());
            }

            public int GetHashCode(char obj)
            {
                return obj.ToString().ToLower().GetHashCode();
            }
        }
    }
}
