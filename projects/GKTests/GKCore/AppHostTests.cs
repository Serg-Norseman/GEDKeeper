// Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
// This file is part of GEDKeeper, licensed under the GNU GPL v3.
// See LICENSE file in the project root for full license information.

using System;
using GKCore.Design.Views;
using GKCore.Plugins;
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
