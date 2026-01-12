/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Reflection;
using GKCore.Lists;
using GKTests;
using GKTests.Stubs;
using NSubstitute;
using NUnit.Framework;

namespace GKCore.Plugins
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
            Assert.AreEqual("GKCore tests", pluginInfo.Description);
            Assert.AreEqual(GKData.APP_COPYRIGHT, pluginInfo.Copyright);
            Assert.AreEqual(GKData.APP_VERSION_3X, pluginInfo.Version);
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
