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

#if !__MonoCS__

using System.Windows.Forms;
using GKCore.Charts;
using GKCore.Interfaces;
using GKCore.Options;
using GKTests.Stubs;
using NUnit.Framework;
using GKTests;

namespace GKUI.Components
{
    [TestFixture]
    public class TreeChartBoxTests : CustomWindowTest
    {
        private IBaseWindow fBase;
        private Form fForm;
        private TreeChartBox fTreeChartBox;

        public override void Setup()
        {
            TestUtils.InitGEDCOMProviderTest();
            WFAppHost.ConfigureBootstrap(false);

            fBase = new BaseWindowStub();

            fForm = new Form();
            fForm.ClientSize = new System.Drawing.Size(383, 221);
            fForm.Text = "ImageViewTests";

            fTreeChartBox = new TreeChartBox();
            fTreeChartBox.Dock = DockStyle.Fill;

            fForm.SuspendLayout();
            fForm.Controls.Add(fTreeChartBox);
            fForm.ResumeLayout(false);
            fForm.PerformLayout();
        }

        public override void TearDown()
        {
            fForm.Dispose();
        }

        [Test]
        public void TestMethod()
        {
            Assert.IsNull(fTreeChartBox.Base);
            fTreeChartBox.Base = fBase;
            Assert.AreEqual(fBase, fTreeChartBox.Base);

            Assert.IsNotNull(fTreeChartBox.Model);

            fTreeChartBox.Options = new TreeChartOptions();
            Assert.IsNotNull(fTreeChartBox.Options);

            Assert.AreEqual(0, fTreeChartBox.IndividualsCount);

            Assert.AreEqual(TreeChartKind.ckAncestors, fTreeChartBox.Kind);
            fTreeChartBox.Kind = TreeChartKind.ckBoth;
            Assert.AreEqual(TreeChartKind.ckBoth, fTreeChartBox.Kind);

            fTreeChartBox.CertaintyIndex = true;
            Assert.AreEqual(true, fTreeChartBox.CertaintyIndex);

            fTreeChartBox.TraceSelected = true;
            Assert.AreEqual(true, fTreeChartBox.TraceSelected);

            fTreeChartBox.TraceKinships = true;
            Assert.AreEqual(true, fTreeChartBox.TraceKinships);

            fTreeChartBox.Selected = null;
            Assert.AreEqual(null, fTreeChartBox.Selected);

            fTreeChartBox.DepthLimitAncestors = 8;
            Assert.AreEqual(8, fTreeChartBox.DepthLimitAncestors);

            fTreeChartBox.DepthLimitDescendants = 8;
            Assert.AreEqual(8, fTreeChartBox.DepthLimitDescendants);

            fTreeChartBox.SetScale(1.2f);
            Assert.AreEqual(1.2f, fTreeChartBox.Scale);

            fTreeChartBox.CenterPerson(null); // nothing

            fTreeChartBox.RefreshTree(); // nothing

            fTreeChartBox.RebuildKinships(); // nothing

            fTreeChartBox.RecalcChart(); // nothing

            fTreeChartBox.ToggleCollapse(); // nothing

            Assert.IsFalse(fTreeChartBox.IsLandscape());

            //Assert.IsNotNull(fTreeChartBox.GetPrintableImage());

            fForm.Show();

            fForm.Close();
        }
    }
}

#endif
