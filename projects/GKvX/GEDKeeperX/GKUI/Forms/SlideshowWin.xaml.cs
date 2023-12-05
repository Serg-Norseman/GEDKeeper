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

using System;
using GKCore.Controllers;
using GKCore.Design.Graphics;
using GKCore.Design.Views;
using GKCore.Interfaces;
using GKUI.Components;

namespace GKUI.Forms
{
    public partial class SlideshowWin : StatusForm, ISlideshowWin
    {
        private readonly SlideshowController fController;

        public IWindow OwnerWindow
        {
            get { return fController.Base; }
        }


        public SlideshowWin(IBaseWindow baseWin)
        {
            InitializeComponent();

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

        public override void SetLocale()
        {
            fController.SetLocale();
        }

        public void SetImage(IImage image)
        {
            var img = (image == null) ? null : ((SKImageHandler)image).Handle;
            fImageCtl.Image = img;
            fImageCtl.ZoomToFit();
        }

        private void tsbStart_Click(object sender, EventArgs e)
        {
            bool active = fController.SwitchActive();

            if (active) {
                tbStart.IconImageSource = UIHelper.LoadResourceImage("Resources.btn_stop.gif");
            } else {
                tbStart.IconImageSource = UIHelper.LoadResourceImage("Resources.btn_start.gif");
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
    }
}
