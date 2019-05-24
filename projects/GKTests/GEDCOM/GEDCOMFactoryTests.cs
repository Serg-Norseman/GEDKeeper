/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2019 by Sergey V. Zhdanovskih.
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

using GDModel;
using GDModel.Providers.GEDCOM;
using NUnit.Framework;

namespace GDModel
{
    [TestFixture]
    public class GEDCOMFactoryTests
    {

        private GDMTag TagConstructorTest(GDMObject owner, string tagName, string tagValue)
        {
            return null;
        }

        [Test]
        public void Test_GEDCOMFactory()
        {
            TagConstructor tagConst = TagConstructorTest;
            Assert.AreEqual(null, tagConst.Invoke(null, "x", "x"));

            //

            GEDCOMFactory f = GEDCOMFactory.GetInstance();
            Assert.IsNotNull(f, "f != null");

            f.RegisterTag(GEDCOMTagType.DATE, GDMDateValue.Create);

            GDMTag tag = f.CreateTag(null, GEDCOMTagType.DATE, "");
            Assert.IsNotNull(tag, "tag != null");

            tag = f.CreateTag(null, "TEST", "");
            Assert.IsNull(tag, "tag == null");
        }

        [Test]
        public void Test_CtorDyn()
        {
            var uref = GEDCOMFactory.CreateTagEx<GDMUserReference>(null, "", "test 12345");
            Assert.IsNotNull(uref);
            Assert.AreEqual("test 12345", uref.StringValue);
        }
    }
}
