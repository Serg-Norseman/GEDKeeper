/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2018 by Sergey V. Zhdanovskih.
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

using System;
using System.IO;
using System.Reflection;
using GKCore;
using NUnit.Framework;

namespace GKCommon.GEDCOM
{
    [TestFixture]
    public class FileFormatTests
    {
        [Test]
        public void Test_Agelong_PseudoAnsel_Win1251()
        {
            using (BaseContext ctx = new BaseContext(null)) {
                Assembly assembly = typeof(CoreTests).Assembly;
                using (Stream stmGed1 = assembly.GetManifestResourceStream("GKTests.Resources.test_agelong_ansel(win1251).ged")) {
                    var gedcomProvider = new GEDCOMProvider(ctx.Tree);
                    gedcomProvider.LoadFromStreamExt(stmGed1, stmGed1);

                    GEDCOMIndividualRecord iRec1 = ctx.Tree.XRefIndex_Find("I1") as GEDCOMIndividualRecord;
                    Assert.IsNotNull(iRec1);

                    Assert.AreEqual("Иван ОВЕЧКИН", iRec1.GetPrimaryFullName());
                }
            }
        }

    }
}
