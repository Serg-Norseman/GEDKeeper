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
using System.Drawing;

using GKCommon;
using GKCommon.GEDCOM;
using GKCore.Charts;
using GKCore.Interfaces;
using GKCore.Types;
using GKTests.Mocks;
using NUnit.Framework;

namespace GKTests.GKCore
{
    [TestFixture]
    public class TreeChartTests
    {
        private IBaseWindow fBase;

        [TestFixtureSetUp]
        public void SetUp()
        {
            fBase = new BaseWindowMock();
        }

        [Test]
        public void Test_ChartFilter()
        {
            using (ChartFilter cf = new ChartFilter()) {
                cf.Backup();
                cf.Restore();
            }
        }

        [Test]
        public void Test_PersonModifyEventArgs()
        {
            PersonModifyEventArgs args = new PersonModifyEventArgs(null);
            Assert.IsNotNull(args);
        }

        [Test]
        public void Test_PersonList()
        {
            PersonList personList = new PersonList(true);
            Assert.IsNotNull(personList);
        }

        [Test]
        public void Test_TreeChartPerson()
        {
            using (TreeChartPerson tcPerson = new TreeChartPerson(null)) {
                Assert.IsNotNull(tcPerson);

                bool hasFail = false;
                tcPerson.BuildBy(null, ref hasFail);

                Assert.AreEqual(null, tcPerson.Rec);

                Assert.AreEqual(null, tcPerson.Portrait);
                Assert.AreEqual(0, tcPerson.PortraitWidth);

                tcPerson.Divorced = false;
                Assert.AreEqual(false, tcPerson.Divorced);
                tcPerson.Divorced = true;
                Assert.AreEqual(true, tcPerson.Divorced);

                tcPerson.IsDup = false;
                Assert.AreEqual(false, tcPerson.IsDup);
                tcPerson.IsDup = true;
                Assert.AreEqual(true, tcPerson.IsDup);

                Assert.AreEqual(0, tcPerson.Height);
                Assert.AreEqual(0, tcPerson.Width);

                tcPerson.IsDead = false;
                Assert.AreEqual(false, tcPerson.IsDead);
                tcPerson.IsDead = true;
                Assert.AreEqual(true, tcPerson.IsDead);

                Assert.AreEqual(0, tcPerson.PtX);
                tcPerson.PtX = 11;
                Assert.AreEqual(11, tcPerson.PtX);

                Assert.AreEqual(0, tcPerson.PtY);
                tcPerson.PtY = 22;
                Assert.AreEqual(22, tcPerson.PtY);

                tcPerson.Selected = false;
                Assert.AreEqual(false, tcPerson.Selected);
                tcPerson.Selected = true;
                Assert.AreEqual(true, tcPerson.Selected);

                Assert.AreEqual(GEDCOMSex.svNone, tcPerson.Sex);
                tcPerson.Sex = GEDCOMSex.svMale;
                Assert.AreEqual(GEDCOMSex.svMale, tcPerson.Sex);

                EnumSet<SpecialUserRef> enums = tcPerson.Signs;
                Assert.IsTrue(enums.IsEmpty());

                Assert.AreEqual(0, tcPerson.GetChildsCount());
                Assert.AreEqual(0, tcPerson.GetSpousesCount());

                TreeChartPerson child = new TreeChartPerson(null);
                tcPerson.AddChild(null);
                tcPerson.AddChild(child);
                Assert.AreEqual(1, tcPerson.GetChildsCount());
                Assert.AreEqual(child, tcPerson.GetChild(0));

                TreeChartPerson spouse = new TreeChartPerson(null);
                tcPerson.AddSpouse(null);
                tcPerson.AddSpouse(spouse);
                Assert.AreEqual(1, tcPerson.GetSpousesCount());
                Assert.AreEqual(spouse, tcPerson.GetSpouse(0));

                Assert.IsFalse(tcPerson.HasFlag(PersonFlag.pfDescWalk));
                tcPerson.SetFlag(PersonFlag.pfDescWalk);
                Assert.IsTrue(tcPerson.HasFlag(PersonFlag.pfDescWalk));

                bool hasMediaFail = false;
                tcPerson.BuildBy(null, ref hasMediaFail);

                ExtRect psnRt = tcPerson.Rect;
                Assert.IsTrue(psnRt.IsEmpty());

                tcPerson.Sex = GEDCOMSex.svMale;
                Assert.AreEqual(Color.Blue, tcPerson.GetSelectedColor());
                tcPerson.Sex = GEDCOMSex.svFemale;
                Assert.AreEqual(Color.Red, tcPerson.GetSelectedColor());
                tcPerson.Sex = GEDCOMSex.svUndetermined;
                Assert.AreEqual(Color.Black, tcPerson.GetSelectedColor());
            }
        }

        [Test]
        public void Test_TreeChartModel()
        {
            using (var model = new TreeChartModel())
            {
                Assert.IsNotNull(model);

                model.Base = fBase;
                Assert.AreEqual(fBase, model.Base);

                model.BranchDistance = TreeChartModel.DEF_BRANCH_DISTANCE;
                Assert.AreEqual(TreeChartModel.DEF_BRANCH_DISTANCE, model.BranchDistance);

                model.CertaintyIndex = true;
                Assert.AreEqual(true, model.CertaintyIndex);

                model.DepthLimit = 8;
                Assert.AreEqual(8, model.DepthLimit);

                model.DepthLimit = 8;
                Assert.AreEqual(8, model.DepthLimit);

                Assert.IsNotNull(model.Filter);

                model.HighlightedPerson = null;
                Assert.AreEqual(null, model.HighlightedPerson);

                Assert.AreEqual(0, model.ImageHeight);
                Assert.AreEqual(0, model.ImageWidth);

                Size sz = model.ImageSize;
                Assert.AreEqual(0, sz.Height);
                Assert.AreEqual(0, sz.Width);

                model.KinRoot = null;
                Assert.AreEqual(null, model.KinRoot);

                model.Kind = TreeChartKind.ckAncestors;
                Assert.AreEqual(TreeChartKind.ckAncestors, model.Kind);

                model.Margins = 15;
                Assert.AreEqual(15, model.Margins);

                Assert.IsNull(model.Options);

                model.PathDebug = true;
                Assert.AreEqual(true, model.PathDebug);

                Assert.IsNotNull(model.Persons);

                Assert.IsNotNull(model.PreparedIndividuals);

                Assert.IsNull(model.Root);

                model.Scale = 1.3f;
                Assert.AreEqual(1.3f, model.Scale);

                model.Scale = 0.1f;
                Assert.AreEqual(0.5f, model.Scale);

                model.Scale = 1.7f;
                Assert.AreEqual(1.5f, model.Scale);

                Assert.Throws(typeof(ArgumentNullException), () => { model.DoFilter(null); });

                Assert.IsNull(model.FindPersonByCoords(0, 0));

                Assert.AreEqual(ExtRect.Create(0, -18, 15, -2), TreeChartModel.GetExpanderRect(ExtRect.Create(0, 0, 0, 0)));
            }
        }
    }
}
