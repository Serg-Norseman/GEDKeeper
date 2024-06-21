/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
 *
 *  This file is part of "GEDKeeper".
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

using NUnit.Framework;

namespace GKCore
{
    [TestFixture]
    public class ValuesCollectionTests
    {
        [Test]
        public void Test_Common()
        {
            ValuesCollection valsCol = new ValuesCollection();

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
