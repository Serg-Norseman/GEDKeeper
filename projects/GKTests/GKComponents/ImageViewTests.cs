﻿/*
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

using System.Drawing;
using System.Reflection;
using System.Windows.Forms;
using BSLib;
using BSLib.Design.Graphics;
using GKCore;
using GKCore.Interfaces;
using GKTests;
using GKUI.Components;
using NUnit.Extensions.Forms;
using NUnit.Framework;

namespace GKUI.Components
{
    [TestFixture]
    public class ImageViewTests : CustomWindowTest
    {
        private Form fForm;
        private ImageView fImageView;

        [TestFixtureSetUp]
        public void Init()
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

        [TestFixtureTearDown]
        public void Done()
        {
            fForm.Dispose();
        }

        [Test]
        public void TestMethod()
        {
            IImage image1 = null;
            fImageView.OpenImage(image1); // return without exceptions

            Image image2 = null;
            fImageView.OpenImage(image2); // return without exceptions

            Assembly assembly = typeof(CoreTests).Assembly;
            Bitmap img = new Bitmap(assembly.GetManifestResourceStream("GKTests.Resources.shaytan_plant.jpg"));

            fImageView.OpenImage(img);

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
