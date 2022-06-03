/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2021 by Sergey V. Zhdanovskih.
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
using System.IO;
using BSLib;
using BSLib.Design.Handlers;
using GDModel;
using GKCore.Charts;
using GKCore.Interfaces;
using GKCore.Options;
using GKCore.Types;
using GKTests;
using GKTests.Stubs;
using GKUI.Platform;
using GKUI.Components;
using NUnit.Framework;

namespace GKCore
{
    [TestFixture]
    public class TreeChartTests
    {
        private IBaseWindow fBase;

        [TestFixtureSetUp]
        public void SetUp()
        {
            TestUtils.InitGEDCOMProviderTest();
            WFAppHost.ConfigureBootstrap(false);

            fBase = new BaseWindowStub();
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
        public void Test_TreeChartPerson()
        {
            var tcPerson = new TreeChartPerson(null);
            Assert.IsNotNull(tcPerson);

            tcPerson.BuildBy(null);

            Assert.AreEqual(null, tcPerson.Rec);

            Assert.AreEqual(null, tcPerson.Portrait);
            Assert.AreEqual(0, tcPerson.PortraitWidth);

            tcPerson.SetFlag(PersonFlag.pfDivorced, false);
            Assert.AreEqual(false, tcPerson.HasFlag(PersonFlag.pfDivorced));
            tcPerson.SetFlag(PersonFlag.pfDivorced, true);
            Assert.AreEqual(true, tcPerson.HasFlag(PersonFlag.pfDivorced));

            tcPerson.IsDup = false;
            Assert.AreEqual(false, tcPerson.IsDup);
            tcPerson.IsDup = true;
            Assert.AreEqual(true, tcPerson.IsDup);

            Assert.AreEqual(0, tcPerson.Height);
            Assert.AreEqual(0, tcPerson.Width);

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

            Assert.AreEqual(GDMSex.svUnknown, tcPerson.Sex);
            tcPerson.Sex = GDMSex.svMale;
            Assert.AreEqual(GDMSex.svMale, tcPerson.Sex);

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

            tcPerson.BuildBy(null);

            ExtRect psnRt = tcPerson.Rect;
            Assert.IsTrue(psnRt.IsEmpty());

            tcPerson.Sex = GDMSex.svMale;
            var color = ((ColorHandler) tcPerson.GetSelectedColor()).Handle;
            Assert.AreEqual(Color.FromArgb(255, Color.Blue), color);

            tcPerson.Sex = GDMSex.svFemale;
            color = ((ColorHandler) tcPerson.GetSelectedColor()).Handle;
            Assert.AreEqual(Color.FromArgb(255, Color.Red), color);

            tcPerson.Sex = GDMSex.svUnknown;
            color = ((ColorHandler) tcPerson.GetSelectedColor()).Handle;
            Assert.AreEqual(Color.FromArgb(255, Color.Black), color);
        }

        [Test]
        public void Test_CircleChartModel()
        {
            using (var model = new CircleChartModel())
            {
                Assert.IsNotNull(model);

                Assert.IsNotNull(model.Segments);

                model.Options = GlobalOptions.Instance.CircleChartOptions;
                Assert.AreEqual(GlobalOptions.Instance.CircleChartOptions, model.Options);

                model.Base = fBase;
                Assert.AreEqual(fBase, model.Base);

                model.Selected = null;
                Assert.AreEqual(null, model.Selected);

                Assert.AreEqual(null, model.FindSegmentByRec(null));

                Assert.AreEqual(0, model.IndividualsCount);

                model.GenWidth = 40; // valid
                Assert.AreEqual(40, model.GenWidth);
                model.GenWidth = 10; // invalid
                Assert.AreEqual(40, model.GenWidth);
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

                model.DepthLimitAncestors = 8;
                Assert.AreEqual(8, model.DepthLimitAncestors);

                model.DepthLimitDescendants = 8;
                Assert.AreEqual(8, model.DepthLimitDescendants);

                Assert.IsNotNull(model.Filter);

                model.HighlightedPerson = null;
                Assert.AreEqual(null, model.HighlightedPerson);

                Assert.AreEqual(0, model.ImageHeight);
                Assert.AreEqual(0, model.ImageWidth);

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
                Assert.AreEqual(ExtRect.Create(-8, -18, 7, -2), TreeChartModel.GetPersonExpandRect(ExtRect.Create(0, 0, 0, 0)));

                ExtRect rt = model.VisibleArea;
                Assert.IsTrue(rt.IsEmpty());
            }
        }

        [Test]
        public void Test_SvgGraphics()
        {
            using (MemoryStream stm = new MemoryStream()) {
                var svg = new SvgGraphics(stm, ExtRectF.CreateBounds(0, 0, 100, 100));

                svg.BeginDrawing();
                svg.Clear(UIHelper.ConvertColor(Color.Yellow));

                svg.BeginEntity(null);
                svg.DrawLine(10, 10, 50, 10, 1);
                svg.DrawArc(60, 60, 20, 15, 25, 1);
                svg.DrawEllipse(10, 10, 30, 30, null, null);
                svg.DrawRect(50, 50, 20, 20, 2);
                svg.DrawRoundedRect(80, 80, 10, 10, 3, 1);
                svg.EndEntity();

                svg.FillArc(60, 60, 20, 15, 25);
                svg.DrawEllipse(10, 10, 30, 30, null, null);
                svg.FillRect(50, 50, 20, 20);

                svg.SetColor(UIHelper.ConvertColor(Color.Red));
                svg.FillRoundedRect(80, 80, 10, 10, 3);

                svg.DrawCircleSegment(0, 0, 10, 20, 0, 17, null, null);
                svg.DrawImage(null, 0, 0, 100, 100);
                svg.DrawPolygon(new ExtPointF[] {}, null, null);
                svg.DrawString("x", 10, 10);

                svg.ResetState();
                svg.SaveState();
                svg.RestoreState();

                svg.Rotate(75);
                svg.Translate(10, 10);
                svg.Scale(0.5f, 0.5f);
                svg.SetClippingRect(0.0f, 0.0f, 10f, 10f);

                svg.SetFont(null);
                Assert.Throws(typeof(ArgumentNullException), () => { svg.SetColor(null); });

                svg.BeginLines(false);
                svg.EndLines();

                svg.EndDrawing();
            }
        }
    }
}
