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
using GKCore.Interfaces;
using GKCore.Plugins;
using GKCore.Types;
using NUnit.Framework;

namespace GKTests.GKCore
{
    [TestFixture]
    public class PluginTests
    {
        [TestFixtureSetUp]
        public void SetUp()
        {
        }

        [Test]
        public void Test_GetPluginInfo()
        {
            Assert.Throws(typeof(ArgumentNullException), () => { PluginInfo.GetPluginInfo(null); });

            var plugin = new PluginTest();
            Assert.IsNotNull(plugin);

            var pluginInfo = PluginInfo.GetPluginInfo(plugin);
            Assert.AreEqual("GKTests", pluginInfo.Title);
            Assert.AreEqual("", pluginInfo.Description);
            Assert.AreEqual("Copyright © 2013-2017 by Sergey V. Zhdanovskih", pluginInfo.Copyright);
            Assert.AreEqual("1.0.0.0", pluginInfo.Version);
        }

        private class PluginTest : IPlugin
        {
            public string DisplayName { get { return "PluginTest"; } }
            public IHost Host { get { return null; } }
            public ILangMan LangMan { get { return null; } }

            public void Execute() { }
            public void OnHostClosing(ref bool cancelClosing) {}
            public void OnHostActivate() {}
            public void OnHostDeactivate() {}
            public void OnLanguageChange() {}
            public bool Startup(IHost host) { return true; }
            public bool Shutdown() { return true; }
        }

        [Test]
        public void Test_PluginsMan()
        {
            var pluginsMan = new PluginsMan();
            Assert.IsNotNull(pluginsMan);

            Assert.AreEqual(0, pluginsMan.Count);

            pluginsMan.Load(null, null);
            pluginsMan.Unload();
            pluginsMan.OnLanguageChange();
            pluginsMan.NotifyRecord(null, null, RecordAction.raAdd);

            Assert.AreEqual(null, pluginsMan.CreateLangMan(null));
        }
    }
}
