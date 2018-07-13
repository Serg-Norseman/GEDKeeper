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
using System.Reflection;
using GKCore.Interfaces;
using GKCore.Plugins;
using GKCore.Types;
using GKUI;
using NUnit.Framework;

namespace GKCore
{
    [TestFixture]
    public class PluginTests
    {
        private class TestPlugin : OrdinaryPlugin, IPlugin
        {
            public override string DisplayName { get { return "TestPlugin"; } }
            public override ILangMan LangMan { get { return null; } }
            public override IImage Icon { get { return null; } }
            public override PluginCategory Category { get { return PluginCategory.Common; } }

            public override void Execute()
            {
            }
        }

        [TestFixtureSetUp]
        public void SetUp()
        {
            WinFormsAppHost.ConfigureBootstrap(false);
        }

        [Test]
        public void Test_OrdinaryPlugin()
        {
            var plugin = new TestPlugin();
            Assert.AreEqual(PluginCategory.Common, plugin.Category);
            Assert.AreEqual(null, plugin.Icon);
            Assert.AreEqual(null, plugin.LangMan);
            Assert.AreEqual("TestPlugin", plugin.DisplayName);

            plugin.Startup(null);
            Assert.AreEqual(null, plugin.Host);

            plugin.OnHostClosing(null);
            plugin.OnHostActivate();
            plugin.OnHostDeactivate();
            plugin.OnLanguageChange();

            plugin.Shutdown();
        }

        [Test]
        public void Test_HostClosingEventArgs()
        {
            var args = new HostClosingEventArgs();
            Assert.AreEqual(false, args.Cancel);
        }

        [Test]
        public void Test_GetPluginInfo()
        {
            Assert.Throws(typeof(ArgumentNullException), () => {
                PluginInfo.GetPluginInfo(null);
            });

            var plugin = new TestPlugin();
            Assert.IsNotNull(plugin);

            var pluginInfo = PluginInfo.GetPluginInfo(plugin);
            Assert.AreEqual("GKTests", pluginInfo.Title);
            Assert.AreEqual("", pluginInfo.Description);
            Assert.AreEqual("Copyright © 2013-2018 by Sergey V. Zhdanovskih", pluginInfo.Copyright);
            Assert.AreEqual("1.0.0.0", pluginInfo.Version);
        }

        [Test]
        public void Test_PluginsMan()
        {
            var pluginsMan = new PluginsMan();
            Assert.IsNotNull(pluginsMan);

            string path = null;
            pluginsMan.Load(null, path);

            Assembly asm = null;
            pluginsMan.Load(null, asm);

            Assert.AreEqual(0, pluginsMan.Count);
            pluginsMan.Load(AppHost.Instance, GetType().Assembly);
            Assert.AreEqual(1, pluginsMan.Count);
            Assert.IsNotNull(pluginsMan[0]);

            pluginsMan.Unload();
            pluginsMan.OnLanguageChange();
            pluginsMan.NotifyRecord(null, null, RecordAction.raAdd);

            Assert.AreEqual(null, pluginsMan.CreateLangMan(null));
        }
    }
}
