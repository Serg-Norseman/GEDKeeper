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

using System;
using GKCommon.GEDCOM;
using GKCore;
using GKUI;
using GKUI.Components;
using NUnit.Framework;

namespace GKTests.UITests
{
    [TestFixture]
    public class CommonTests
    {
        [TestFixtureSetUp]
        public void SetUp()
        {
            WinFormsAppHost.ConfigureBootstrap(false);
        }

        [Test]
        public void Test_UIHelper()
        {
            Assert.Throws(typeof(ArgumentNullException), () => { UIHelper.CreateListView(null); });
            Assert.Throws(typeof(ArgumentNullException), () => { UIHelper.CreateRecordsView(null, null, GEDCOMRecordType.rtIndividual); });
        }

        [Test]
        public void Test_ColorLD()
        {
            var color = AppHost.Utilities.CreateColor(100, 100, 100);
            var chk_res = AppHost.Utilities.CreateColor(50, 50, 50);
            Assert.AreEqual(((ColorHandler)chk_res).Handle, ((ColorHandler)color.Darker(0.5f)).Handle);

            color = AppHost.Utilities.CreateColor(50, 50, 50);
            chk_res = AppHost.Utilities.CreateColor(75, 75, 75);
            Assert.AreEqual(((ColorHandler)chk_res).Handle, ((ColorHandler)color.Lighter(0.5f)).Handle);
        }
    }
}
