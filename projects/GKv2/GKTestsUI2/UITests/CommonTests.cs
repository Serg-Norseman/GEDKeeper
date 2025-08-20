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

using System.Drawing;
using System.Windows.Forms;
using BSLib;
using GKCore;
using GKCore.Design;
using GKCore.Design.Graphics;
using GKCore.Kinships;
using GKCore.Locales;
using GKCore.Media;
using GKTests;
using GKTests.Stubs;
using GKUI.Components;
using GKUI.Platform;
using GKUI.Platform.Handlers;
using NUnit.Extensions.Forms;
using NUnit.Framework;

namespace GKUI.Forms
{
    [TestFixture]
    public class CommonTests
    {
        public static void InitUITest()
        {
            TestUtils.InitGEDCOMProviderTest();
            AppHost.TEST_MODE = true;

            // GlobalOptions -> IGraphicsProviderEx
            WFAppHost.ConfigureBootstrap();

            LangMan.DefInit();
            AppHost.EventDefinitions.InitPredefined();

            KinshipsGraph.InitDefaults();

            AppHost.Container.Register<IProgressDialog, ProgressStub>(GKCore.Utilities.LifeCycle.Singleton, true);
        }

        public CommonTests()
        {
            CommonTests.InitUITest();
        }

        [Test]
        public void Test_ColorLD()
        {
            var color = AppHost.GfxProvider.CreateColor(0x646464);
            var chk_res = AppHost.GfxProvider.CreateColor(0x323232);
            Assert.AreEqual(((ColorHandler)chk_res).Handle, ((ColorHandler)color.Darker(0.5f)).Handle);

            color = AppHost.GfxProvider.CreateColor(GKColors.Silver);
            Assert.AreEqual("ffc0c0c0", color.GetName());

            color = AppHost.GfxProvider.CreateColor(0x323232);
            chk_res = AppHost.GfxProvider.CreateColor(0x4B4B4B);
            Assert.AreEqual(((ColorHandler)chk_res).Handle, ((ColorHandler)color.Lighter(0.5f)).Handle);
            Assert.AreEqual(75, chk_res.GetR());
            Assert.AreEqual(75, chk_res.GetG());
            Assert.AreEqual(75, chk_res.GetB());
            Assert.AreEqual(255, chk_res.GetA());
            Assert.IsFalse(chk_res.IsTransparent());
            Assert.AreEqual("ff4b4b4b", chk_res.GetName());
        }

        [Test]
        public void TestNamedRegion()
        {
            var region = new NamedRegion("test", ExtRect.Empty);
            Assert.AreEqual("test", region.Name);
        }

        [Test]
        public void Test_ImageBox()
        {
            var fForm = new Form();
            fForm.ClientSize = new Size(383, 221);
            fForm.Text = "ImageViewTests";

            var fImageBox = new ImageBox();
            fImageBox.Dock = DockStyle.Fill;

            fForm.SuspendLayout();
            fForm.Controls.Add(fImageBox);
            fForm.ResumeLayout(false);
            fForm.PerformLayout();

            Bitmap img = new Bitmap(TestUtils.LoadResourceStream("shaytan_plant.jpg"));

            fImageBox.BeginUpdate();
            fImageBox.Image = null;
            Assert.AreEqual(null, fImageBox.Image);
            fImageBox.Image = img;
            Assert.AreEqual(img, fImageBox.Image);
            fImageBox.EndUpdate();

            fImageBox.SelectionMode = ImageBoxSelectionMode.Rectangle;
            Assert.AreEqual(ImageBoxSelectionMode.Rectangle, fImageBox.SelectionMode);

            fImageBox.ZoomToFit();
            fImageBox.ZoomIn();
            fImageBox.ZoomOut();

            fImageBox.Zoom = 200;
            Assert.AreEqual(200, fImageBox.Zoom);

            Assert.IsNotNull(fImageBox.ZoomLevels);

            fImageBox.SizeToFit = true;
            Assert.AreEqual(true, fImageBox.SizeToFit);

            fImageBox.AllowZoom = true;
            Assert.AreEqual(true, fImageBox.AllowZoom);

            fImageBox.AutoPan = true;
            Assert.AreEqual(true, fImageBox.AutoPan);

            fImageBox.ShowNamedRegionTips = false;
            Assert.AreEqual(false, fImageBox.ShowNamedRegionTips);
            fImageBox.ShowNamedRegionTips = true;
            Assert.AreEqual(true, fImageBox.ShowNamedRegionTips);

            fImageBox.ActualSize();

            fForm.Show();

            fForm.Close();
            fForm.Dispose();
        }

        [Test]
        public void Test_ImageView()
        {
            var fForm = new Form();
            fForm.ClientSize = new Size(383, 221);
            fForm.Text = "ImageViewTests";

            var fImageView = new ImageView();
            fImageView.Dock = DockStyle.Fill;

            fForm.SuspendLayout();
            fForm.Controls.Add(fImageView);
            fForm.ResumeLayout(false);
            fForm.PerformLayout();

            IImage image1 = null;
            fImageView.OpenImage(null, image1); // return without exceptions

            image1 = new ImageHandler(new Bitmap(TestUtils.LoadResourceStream("shaytan_plant.jpg")));
            fImageView.OpenImage(null, image1);

            fForm.Show();

            fImageView.ShowToolbar = false;
            Assert.IsFalse(fImageView.ShowToolbar);

            fImageView.ShowToolbar = true;
            Assert.IsTrue(fImageView.ShowToolbar);

            fImageView.SelectionMode = ImageBoxSelectionMode.Zoom;
            Assert.AreEqual(ImageBoxSelectionMode.Zoom, fImageView.SelectionMode);

            fImageView.SelectionRegion = ExtRect.Empty;
            Assert.AreEqual(ExtRect.Empty, fImageView.SelectionRegion);

            CustomWindowTest.ClickToolStripButton("btnZoomIn", fForm);
            CustomWindowTest.ClickToolStripButton("btnZoomOut", fForm);
            CustomWindowTest.ClickToolStripButton("btnSizeToFit", fForm);

            var tscbZoomLevels = new ToolStripComboBoxTester("zoomLevelsToolStripComboBox", fForm);
            tscbZoomLevels.Enter("500");

            fForm.Close();
            fForm.Dispose();
        }
    }
}
