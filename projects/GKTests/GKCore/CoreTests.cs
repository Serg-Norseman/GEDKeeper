/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections.Generic;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Lists;
using GKCore.Locales;
using GKCore.Search;
using GKTests;
using NSubstitute;
using NUnit.Framework;

namespace GKCore
{

    [TestFixture]
    public class CoreTests
    {
        private readonly BaseContext fContext;

        public CoreTests()
        {
            TestUtils.InitUITest();

            fContext = TestUtils.CreateContext();
            TestUtils.FillContext(fContext);
        }

        [Test]
        public void Test_Search()
        {
            SearchResult searchResult = new SearchResult(null);
            Assert.IsNotNull(searchResult);

            Assert.Throws(typeof(ArgumentNullException), () => { new WorkSearchStrategy(null, null); });

            var workWindow = Substitute.For<IWorkWindow>();
            WorkSearchStrategy strat = new WorkSearchStrategy(workWindow, "");
            Assert.IsNotNull(strat);

            IList<ISearchResult> res = strat.FindAll();
            Assert.IsNotNull(res);

            Assert.IsFalse(strat.HasResults());
            Assert.IsNull(strat.FindNext());
            Assert.IsNull(strat.FindPrev());
        }

        [Test]
        public void Test_Other()
        {
            ComboItem<object> comboItem = new ComboItem<object>("Test", null);
            Assert.IsNotNull(comboItem);
            Assert.AreEqual("Test", comboItem.ToString());

            ModifyEventArgs args = new ModifyEventArgs(RecordAction.raAdd, null);
            Assert.IsNotNull(args);
        }

        [Test]
        public void Test_LuaScripts()
        {
            var script = new ScriptEngine(null);
            script.lua_run("gk_print(\"Hello\")", null);
        }

        [Test]
        public void Test_LangMan()
        {
            LangManager langMan = new LangManager();
            Assert.IsNotNull(langMan);

            Assert.AreEqual("?", langMan.LS(LSID.First));
        }
    }
}
