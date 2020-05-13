/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2018 by Sergey V. Zhdanovskih.
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
using GKCore;
using GKCore.Controllers;
using GKCore.Interfaces;
using GKCore.MVP.Views;
using GKUI.Components;

namespace GKUI.Forms
{
    public partial class SlideshowWin : StatusForm, ISlideshowWin
    {
        private readonly SlideshowController fController;

        private readonly ImageBox fImageCtl;
        private ITimer fTimer;

        public SlideshowWin(IBaseWindow baseWin)
        {
            InitializeComponent();

            tbStart.Image = UIHelper.LoadResourceImage("Resources.btn_start.gif");
            tbPrev.Image = UIHelper.LoadResourceImage("Resources.btn_left.gif");
            tbNext.Image = UIHelper.LoadResourceImage("Resources.btn_right.gif");

            //SuspendLayout();
            fImageCtl = new GKUI.Components.ImageBox();
            fImageCtl.BackgroundColor = SystemColors.ControlBackground; //ControlDark;
            fImageCtl.ImageBorderStyle = ImageBoxBorderStyle.FixedSingleGlowShadow;
            fImageCtl.ImageBorderColor = Colors.AliceBlue;
            fImageCtl.SelectionMode = ImageBoxSelectionMode.Zoom;
            Content = fImageCtl;
            //ResumeLayout();

            fTimer = AppHost.Instance.CreateTimer(1000, Timer1Tick);

            SetLang();

            fController = new SlideshowController(this);
            fController.Init(baseWin);
            fController.LoadList();
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                if (fTimer != null) fTimer.Dispose();
            }
            base.Dispose(disposing);
        }

        protected override void OnLoad(EventArgs e)
        {
            base.OnLoad(e);
            fImageCtl.Focus();

            fController.Next();
        }

        private void SlideshowWin_Load(object sender, EventArgs e)
        {
        }

        private void SlideshowWin_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.Key == Keys.Escape) Close();
        }

        public override void SetLang()
        {
            Title = LangMan.LS(LSID.LSID_Slideshow);
            tbStart.Text = LangMan.LS(LSID.LSID_Start);
            SetToolTip(tbPrev, LangMan.LS(LSID.LSID_PrevRec));
            SetToolTip(tbNext, LangMan.LS(LSID.LSID_NextRec));
        }

        public void SetImage(IImage image)
        {
            Image img = (image == null) ? null : ((ImageHandler)image).Handle;
            fImageCtl.Image = img;
            fImageCtl.ZoomToFit();
        }

        private void tsbStart_Click(object sender, EventArgs e)
        {
            if (tbStart.Text == LangMan.LS(LSID.LSID_Start)) {
                tbStart.Text = LangMan.LS(LSID.LSID_Stop);
                tbStart.Image = UIHelper.LoadResourceImage("Resources.btn_stop.gif");
                fTimer.Start();
            } else {
                tbStart.Text = LangMan.LS(LSID.LSID_Start);
                tbStart.Image = UIHelper.LoadResourceImage("Resources.btn_start.gif");
                fTimer.Stop();
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

        private void Timer1Tick(object sender, EventArgs e)
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
