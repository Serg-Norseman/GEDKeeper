/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2022 by Sergey V. Zhdanovskih.
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
using BSLib.Design.Graphics;
using Eto.Drawing;
using Eto.Forms;
using Eto.Serialization.Xaml;
using GKCore.Controllers;
using GKCore.Interfaces;
using GKCore.MVP.Views;
using GKUI.Components;
using GKUI.Platform;

namespace GKUI.Forms
{
    public partial class SlideshowWin : StatusForm, ISlideshowWin
    {
        #region Design components

        private ButtonToolItem tbNext;
        private ButtonToolItem tbPrev;
        private ButtonToolItem tbStart;

        #endregion

        private readonly SlideshowController fController;

        private readonly ImageBox fImageCtl;

        public SlideshowWin(IBaseWindow baseWin)
        {
            XamlReader.Load(this);

            /*SuspendLayout();
            fImageCtl = new ImageBox();
            //fImageCtl.BackgroundColor = SystemColors.ControlBackground;
            //fImageCtl.ImageBorderStyle = ImageBoxBorderStyle.FixedSingleGlowShadow;
            //fImageCtl.ImageBorderColor = Colors.AliceBlue;
            //fImageCtl.SelectionMode = ImageBoxSelectionMode.Zoom;
            Content = fImageCtl;
            ResumeLayout();

            WindowState = WindowState.Maximized;*/

            fController = new SlideshowController(this);
            fController.Init(baseWin);
            fController.LoadList();
        }

        private void SlideshowWin_Load(object sender, EventArgs e)
        {
            fController.Next();
        }

        private void SlideshowWin_Closed(object sender, EventArgs e)
        {
            fController.Dispose();
        }

        private void SlideshowWin_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.Key == Keys.Escape) Close();
        }

        public override void SetLocale()
        {
            fController.SetLocale();
        }

        public void SetImage(IImage image)
        {
            Image img = (image == null) ? null : ((ImageHandler)image).Handle;
            fImageCtl.Image = img;
            fImageCtl.ZoomToFit();
        }

        private void tsbStart_Click(object sender, EventArgs e)
        {
            bool active = fController.SwitchActive();

            if (active) {
                tbStart.Image = UIHelper.LoadResourceImage("Resources.btn_stop.gif");
            } else {
                tbStart.Image = UIHelper.LoadResourceImage("Resources.btn_start.gif");
            }
        }

        private void tsbPrev_Click(object sender, EventArgs e)
        {
            fController.Prev();
        }

        private void tsbNext_Click(object sender, EventArgs e)
        {
            fController.Next();
        }

        public void UpdateControls()
        {
            tbStart.Enabled = (fController.FileRefs.Count > 0);
            tbPrev.Enabled = (fController.CurrentIndex > 0);
            tbNext.Enabled = (fController.CurrentIndex < fController.FileRefs.Count - 1);
        }
    }
}
