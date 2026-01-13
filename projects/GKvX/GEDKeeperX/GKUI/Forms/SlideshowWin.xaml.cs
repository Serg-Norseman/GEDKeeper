/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Design.Graphics;
using GKCore.Design.Views;
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
