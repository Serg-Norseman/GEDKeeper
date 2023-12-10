/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih.
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

#if !MONO

using System;
using System.Drawing;
using System.Windows.Forms;
using BSLib;
using GKCore.Types;
using GKTests;
using NUnit.Framework;

namespace GKUI.Components
{
    [TestFixture]
    public class ImageBoxTests : IDisposable
    {
        private Form fForm;
        private ImageBox fImageBox;

        public ImageBoxTests()
        {
            fForm = new Form();
            fForm.ClientSize = new Size(383, 221);
            fForm.Text = "ImageViewTests";

            fImageBox = new ImageBox();
            fImageBox.Dock = DockStyle.Fill;

            fForm.SuspendLayout();
            fForm.Controls.Add(fImageBox);
            fForm.ResumeLayout(false);
            fForm.PerformLayout();
        }

        public void Dispose()
        {
            fForm.Dispose();
        }

        [Test]
        public void TestNamedRegion()
        {
            var region = new NamedRegion("test", ExtRect.Empty);
            Assert.AreEqual("test", region.Name);
        }

        [Test]
        public void TestMethod()
        {
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
        }
    }
}

#endif
