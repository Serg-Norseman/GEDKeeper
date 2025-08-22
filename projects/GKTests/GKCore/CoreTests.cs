/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
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
