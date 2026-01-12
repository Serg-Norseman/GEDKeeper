/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using BSLib;
using GDModel;
using GKTests;
using NSubstitute;
using NUnit.Framework;

namespace GKCore.Charts
{
    [TestFixture]
    public class ChartsTests
    {
        public ChartsTests()
        {
            TestUtils.InitUITest();
        }

        private void TweenHandler(int newX, int newY)
        {
        }

        [Test]
        public void Test_Tween()
        {
            var tween = new TweenLibrary();
            tween.StartTween(TweenHandler, 0, 0, 10, 10, TweenAnimation.EaseInOutQuad, 20);
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

            var instance = new TCGenerationsControl(treeChart, TreeChartKind.ckAncestors);
            Assert.IsNotNull(instance);

            //var chartRenderer = Substitute.For<ChartRenderer>();
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

            var instance = new TCScaleControl(treeChart);
            Assert.IsNotNull(instance);

            //var chartRenderer = Substitute.For<ChartRenderer>();
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

            var instance = new TCPersonControl(treeChart);
            Assert.IsNotNull(instance);

            //var chartRenderer = Substitute.For<ChartRenderer>();
            //instance.Draw(chartRenderer);

            var tip = instance.Tip;
            var height = instance.Height;
            var width = instance.Width;

            instance.MouseDown(10, 10);
            instance.MouseMove(10, 20);
            instance.MouseUp(10, 20);
        }

        [Test]
        public void Test_TreeChartPerson_GetSelectedColor()
        {
            var tcPerson = new TreeChartPerson(null);
            Assert.IsNotNull(tcPerson);

            tcPerson.BuildBy(null);

            tcPerson.Sex = GDMSex.svMale;
            var color = tcPerson.GetSelectedColor();
            //Assert.AreEqual(Color.FromArgb(255, Color.Blue), color);

            tcPerson.Sex = GDMSex.svFemale;
            color = tcPerson.GetSelectedColor();
            //Assert.AreEqual(Color.FromArgb(255, Color.Red), color);

            tcPerson.Sex = GDMSex.svUnknown;
            color = tcPerson.GetSelectedColor();
            //Assert.AreEqual(Color.FromArgb(255, Color.Black), color);
        }
    }
}
