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

#if !MONO && !DIS_NUF

using System;
using System.Drawing;
using System.Windows.Forms;
using BSLib;
using GKCore.Design.Graphics;
using GKTests;
using GKUI.Platform.Handlers;
using NUnit.Extensions.Forms;
using NUnit.Framework;

namespace GKUI.Components
{
    [TestFixture]
    public class ImageViewTests : CustomWindowTest, IDisposable
    {
        private Form fForm;
        private ImageView fImageView;

        public ImageViewTests()
        {
            fForm = new Form();
            fForm.ClientSize = new Size(383, 221);
            fForm.Text = "ImageViewTests";

            fImageView = new ImageView();
            fImageView.Dock = DockStyle.Fill;

            fForm.SuspendLayout();
            fForm.Controls.Add(fImageView);
            fForm.ResumeLayout(false);
            fForm.PerformLayout();
        }

        public void Dispose()
        {
            fForm.Dispose();
        }

        [Test]
        public void TestMethod()
        {
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

            ClickToolStripButton("btnZoomIn", fForm);
            ClickToolStripButton("btnZoomOut", fForm);
            ClickToolStripButton("btnSizeToFit", fForm);

            var tscbZoomLevels = new ToolStripComboBoxTester("zoomLevelsToolStripComboBox", fForm);
            tscbZoomLevels.Enter("500");

            fForm.Close();
        }
    }
}

#endif
