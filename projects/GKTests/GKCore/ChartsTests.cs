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

using BSLib;
using GKUI;
using NSubstitute;
using NUnit.Framework;

namespace GKCore.Charts
{
    [TestFixture]
    public class ChartsTests
    {
        [TestFixtureSetUp]
        public void SetUp()
        {
            WFAppHost.ConfigureBootstrap(false);
        }

        [Test]
        public void Test_BorderPainter()
        {
            var chartRenderer = Substitute.For<ChartRenderer>();

            var rect = ExtRect.Create(0, 0, 99, 99);
            BorderPainter.DrawBorder(chartRenderer, rect, GfxBorderStyle.Single);
            BorderPainter.DrawBorder(chartRenderer, rect, GfxBorderStyle.Double);
            BorderPainter.DrawBorder(chartRenderer, rect, GfxBorderStyle.Triple);
            BorderPainter.DrawBorder(chartRenderer, rect, GfxBorderStyle.Sunken3D);
            BorderPainter.DrawBorder(chartRenderer, rect, GfxBorderStyle.Raised3D);
            BorderPainter.DrawBorder(chartRenderer, rect, GfxBorderStyle.SingleSquareCuts);
            BorderPainter.DrawBorder(chartRenderer, rect, GfxBorderStyle.DoubleSquareCuts);
            BorderPainter.DrawBorder(chartRenderer, rect, GfxBorderStyle.SingleRoundCuts);
            BorderPainter.DrawBorder(chartRenderer, rect, GfxBorderStyle.DoubleRoundCuts);
            BorderPainter.DrawBorder(chartRenderer, rect, GfxBorderStyle.SingleBevels);
            BorderPainter.DrawBorder(chartRenderer, rect, GfxBorderStyle.DoubleBevels);
            BorderPainter.DrawBorder(chartRenderer, rect, GfxBorderStyle.CrossCorners);
        }

        [Test]
        public void Test_TCGenerationsControl()
        {
            var treeChart = Substitute.For<ITreeChart>();
            var chartRenderer = Substitute.For<ChartRenderer>();

            var instance = new TCGenerationsControl(treeChart);
            Assert.IsNotNull(instance);

            //instance.Draw(chartRenderer);

            var tip = instance.Tip;
            var height = instance.Height;
            var width = instance.Width;

            instance.MouseDown(10, 10);
            instance.MouseMove(10, 20);
            instance.MouseUp(10, 20);
        }

        [Test]
        public void Test_TCScaleControl()
        {
            var treeChart = Substitute.For<ITreeChart>();
            var chartRenderer = Substitute.For<ChartRenderer>();

            var instance = new TCScaleControl(treeChart);
            Assert.IsNotNull(instance);

            //instance.Draw(chartRenderer);

            var tip = instance.Tip;
            var height = instance.Height;
            var width = instance.Width;

            instance.MouseDown(10, 10);
            instance.MouseMove(10, 20);
            instance.MouseUp(10, 20);
        }

        [Test]
        public void Test_TCPersonControl()
        {
            var treeChart = Substitute.For<ITreeChart>();
            var chartRenderer = Substitute.For<ChartRenderer>();

            var instance = new TCPersonControl(treeChart);
            Assert.IsNotNull(instance);

            //instance.Draw(chartRenderer);

            var tip = instance.Tip;
            var height = instance.Height;
            var width = instance.Width;

            instance.MouseDown(10, 10);
            instance.MouseMove(10, 20);
            instance.MouseUp(10, 20);
        }
    }
}
