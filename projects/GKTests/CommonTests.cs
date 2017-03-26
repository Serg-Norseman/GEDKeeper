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

using GKCommon;
using GKCommon.Controls;
using GKCommon.GEDCOM;
using GKCore.Interfaces;
using GKCore.Types;
using NUnit.Framework;

namespace GKTests.GKCommon
{
    [TestFixture]
    public class CommonTests
    {
        [Test]
        public void PG_Tests()
        {
            PatriarchObj pObj = new PatriarchObj();
            Assert.IsNotNull(pObj);
            Assert.IsNotNull(pObj.Links);

            PGNode pgNode = new PGNode("label", PGNodeType.Default);
            Assert.IsNotNull(pgNode);

            pgNode = new PGNode("label", PGNodeType.Default, 5);
            Assert.IsNotNull(pgNode);
        }

        private bool TestExternalFilterHandler(GEDCOMRecord record)
        {
            return false;
        }

        [Test]
        public void FiltersIntf_Tests()
        {
            FilterCondition cond = new FilterCondition(0, ConditionKind.ck_Contains, null);
            Assert.IsNotNull(cond);

            ExternalFilterHandler handler = TestExternalFilterHandler;
            Assert.IsFalse(handler.Invoke(null));
        }

        private void TweenHandler(int newX, int newY)
        {
        }

        [Test]
        public void Tween_Tests()
        {
            #if !__MonoCS__
            TweenLibrary tween = new TweenLibrary();
            tween.StartTween(TweenHandler, 0, 0, 10, 10, TweenAnimation.EaseInOutQuad, 20);
            #endif
        }

        [Test]
        public void ListItems_Tests()
        {
            var item1 = new GKListItem(10, null);
            var item2 = new GKListItem(20, null);
            Assert.AreEqual(-1, item1.CompareTo(item2));

            item1 = new GKListItem(10, null);
            item2 = new GKListItem(null, null);
            Assert.AreEqual(-1, item1.CompareTo(item2));

            item1 = new GKListItem(null, null);
            item2 = new GKListItem(20, null);
            Assert.AreEqual(1, item1.CompareTo(item2));

            Assert.AreEqual(0, item1.CompareTo(item1));
            Assert.AreEqual(-1, item1.CompareTo(null));


            var subitem1 = new GKListSubItem(10);
            var subitem2 = new GKListSubItem(20);
            Assert.AreEqual(-1, subitem1.CompareTo(subitem2));

            subitem1 = new GKListSubItem(10);
            subitem2 = new GKListSubItem(null);
            Assert.AreEqual(-1, subitem1.CompareTo(subitem2));

            subitem1 = new GKListSubItem(null);
            subitem2 = new GKListSubItem(20);
            Assert.AreEqual(1, subitem1.CompareTo(subitem2));

            Assert.AreEqual(0, subitem1.CompareTo(subitem1));
            Assert.AreEqual(-1, subitem1.CompareTo(null));
        }
    }
}
