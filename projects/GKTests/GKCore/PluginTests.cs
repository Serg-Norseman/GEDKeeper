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
using System.Reflection;
using GKCore.Lists;
using GKCore.Plugins;
using GKTests;
using GKTests.Stubs;
using NSubstitute;
using NUnit.Framework;

namespace GKCore
{
    [TestFixture]
    public class PluginTests
    {
        public PluginTests()
        {
            TestUtils.InitUITest();
        }

        [Test]
        public void Test_OrdinaryPlugin()
        {
            var plugin = Substitute.For<OrdinaryPlugin>();

            plugin.Category.Returns(PluginCategory.Common);
            Assert.AreEqual(PluginCategory.Common, plugin.Category);

            Assert.IsNotNull(plugin.Icon);
            Assert.IsNotNull(plugin.LangMan);

            plugin.DisplayName.Returns("TestPlugin");
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
        public void Test_WidgetPlugin()
        {
            var plugin = Substitute.For<WidgetPlugin>();

            plugin.Category.Returns(PluginCategory.Tool);
            Assert.AreEqual(PluginCategory.Tool, plugin.Category);

            Assert.IsNotNull(plugin.Icon);
            Assert.IsNotNull(plugin.LangMan);

            plugin.DisplayName.Returns("WidgetPlugin");
            Assert.AreEqual("WidgetPlugin", plugin.DisplayName);

            plugin.Startup(null);
            Assert.AreEqual(null, plugin.Host);

            plugin.OnHostClosing(null);
            plugin.OnHostActivate();
            plugin.OnHostDeactivate();
            plugin.OnLanguageChange();

            plugin.Shutdown();

            plugin.WidgetInit(null);
            plugin.BaseChanged(null);
            plugin.BaseClosed(null);
            plugin.BaseRenamed(null, string.Empty, string.Empty);
            plugin.SelectedIndexChanged(null);
            plugin.TabChanged(null);
            plugin.WidgetEnable();
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
            Assert.AreEqual(GKData.APP_COPYRIGHT, pluginInfo.Copyright);
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

            object sender = null;
            Assert.AreEqual(null, pluginsMan.CreateLangMan(sender));
        }
    }
}
