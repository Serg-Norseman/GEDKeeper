/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using NUnit.Framework;

namespace GKCore.Utilities
{
    [TestFixture]
    public class ValuesCollectionTests
    {
        [Test]
        public void Test_Common()
        {
            var valsCol = new ValuesCollection();

            valsCol.Add("red", "rojo");
            valsCol.Add("green", "verde");
            valsCol.Add("blue", "azul");
            valsCol.Add("red", "rouge");
            valsCol.Add("red", null);
            valsCol.Add("red", "rouge");

            Assert.AreEqual(3, valsCol.Count);

            valsCol.Remove("green");
            Assert.AreEqual(2, valsCol.Count);

            string[] values = valsCol.GetValues("xxxxx");
            Assert.AreEqual(0, values.Length);

            values = valsCol.GetValues("red");
            Assert.AreEqual("rojo", values[0]);
            Assert.AreEqual("rouge", values[1]);

            valsCol.Clear();
            Assert.AreEqual(0, valsCol.Count);
        }
    }
}
