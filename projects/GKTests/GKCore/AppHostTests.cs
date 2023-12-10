/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih.
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
using GKCore.Types;
using GKTests;
using NUnit.Framework;

namespace GKCore
{
    [TestFixture]
    public class AppHostTests
    {
        public AppHostTests()
        {
            TestUtils.InitUITest();
        }

        [Test]
        public void Test_AppHost()
        {
            Assert.IsNotEmpty(AppHost.GetAppPath());
            Assert.IsNotEmpty(AppHost.GetLogFilename());

            Assert.IsNotEmpty(AppHost.Instance.GetAppDataPath());

            Assert.ThrowsAsync(typeof(ArgumentNullException), async () => { await AppHost.Instance.LoadBase(null, null); });

            AppHost.Instance.SetArgs(new string[] { "" });

            AppHost.CheckPortable(new string[] { "" });

            Assert.IsNotNull(AppHost.NamesTable);
            Assert.IsNotNull(AppHost.Plugins);
        }

        [Test]
        public void Test_WidgetInfo()
        {
            var instance = new WidgetInfo(null, null);
            Assert.IsNotNull(instance);
        }

        [Test]
        public void Test_Target()
        {
            var instance = new Target();
            Assert.IsNotNull(instance);
        }
    }
}
