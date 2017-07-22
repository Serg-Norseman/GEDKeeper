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

#if !__MonoCS__

using System.Windows.Forms;

using GKCore.Charts;
using GKCore.Interfaces;
using GKTests.Mocks;
using GKUI;
using GKUI.Charts;
using NUnit.Framework;

namespace GKTests.GKComponents
{
    [TestFixture]
    public class CircleChartTests
    {
        private IBaseWindow fBase;
        private Form fForm;
        private CircleChart fCircleChart;

        [TestFixtureSetUp]
        public void Init()
        {
            WinFormsAppHost.ConfigureBootstrap(false);

            fBase = new BaseWindowMock();

            fForm = new Form();
            fForm.ClientSize = new System.Drawing.Size(383, 221);
            fForm.Text = "ImageViewTests";

            fCircleChart = new CircleChart();
            fCircleChart.Dock = DockStyle.Fill;

            fForm.SuspendLayout();
            fForm.Controls.Add(fCircleChart);
            fForm.ResumeLayout(false);
            fForm.PerformLayout();
        }

        [TestFixtureTearDown]
        public void Dispose()
        {
            fForm.Dispose();
        }

        [Test]
        public void TestMethod()
        {
            Assert.IsNull(fCircleChart.Base);
            fCircleChart.Base = fBase;
            Assert.AreEqual(fBase, fCircleChart.Base);

            Assert.IsNotNull(fCircleChart.Model);

            Assert.IsNotNull(fCircleChart.Options);

            Assert.IsNull(fCircleChart.RootPerson);

            Assert.AreEqual(CircleChartType.Ancestors, fCircleChart.ChartType);
            fCircleChart.ChartType = CircleChartType.Descendants;
            Assert.AreEqual(CircleChartType.Descendants, fCircleChart.ChartType);

            Assert.AreEqual(1.0f, fCircleChart.Zoom);

            fForm.Show();

            fForm.Close();
        }
    }
}

#endif
