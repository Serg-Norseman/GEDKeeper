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

using System;
using System.Drawing;
using System.Windows.Forms;
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Design.Graphics;
using GKCore.Design.Views;
using GKUI.Components;
using GKUI.Platform.Handlers;

namespace GKUI.Forms
{
    public partial class SlideshowWin : StatusForm, ISlideshowWin
    {
        #region Design components

        private readonly ImageBox fImageCtl;

        #endregion

        private readonly SlideshowController fController;

        public IWindow OwnerWindow
        {
            get { return fController.Base; }
        }


        public SlideshowWin(IBaseWindow baseWin)
        {
            InitializeComponent();

            SuspendLayout();
            fImageCtl = new ImageBox();
            fImageCtl.Dock = DockStyle.Fill;
            fImageCtl.Location = new Point(0, 0);
            fImageCtl.Size = new Size(100, 100);
            fImageCtl.Margin = new Padding(4);
            fImageCtl.SelectionMode = ImageBoxSelectionMode.Zoom;
            Controls.Add(fImageCtl);
            Controls.SetChildIndex(fImageCtl, 0);
            ResumeLayout(false);

            fController = new SlideshowController(this);
            fController.Init(baseWin);
            fController.LoadList();
        }

        private void SlideshowWin_Load(object sender, EventArgs e)
        {
            WindowState = FormWindowState.Maximized;

            fController.Next();
        }

        private void SlideshowWin_Closed(object sender, FormClosedEventArgs e)
        {
            fController.Dispose();
        }

        private void SlideshowWin_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.KeyCode == Keys.Escape) Close();
        }

        public override void SetLocale()
        {
            fController.SetLocale();
        }

        public override void ApplyTheme()
        {
            base.ApplyTheme();
            fController.ApplyTheme();
        }

        public void SetImage(IImage image)
        {
            Image img = (image == null) ? null : ((ImageHandler)image).Handle;
            fImageCtl.Image = img;
            fImageCtl.ZoomToFit();
        }

        private void tsbStart_Click(object sender, EventArgs e)
        {
            fController.SwitchActive();
        }

        private void tsbPrev_Click(object sender, EventArgs e)
        {
            fController.Prev();
        }

        private void tsbNext_Click(object sender, EventArgs e)
        {
            fController.Next();
        }
    }
}
