/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using Eto.Drawing;
using Eto.Forms;
using Eto.Serialization.Xaml;
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Design.Graphics;
using GKCore.Design.Views;
using GKUI.Components;
using GKUI.Platform;
using GKUI.Platform.Handlers;

namespace GKUI.Forms
{
    public partial class SlideshowWin : StatusForm, ISlideshowWin
    {
        #region Design components
#pragma warning disable CS0169, CS0649, IDE0044, IDE0051

        private ButtonToolItem tbNext;
        private ButtonToolItem tbPrev;
        private GKButtonToolItem tbStart;
        private ImageBox fImageCtl;

#pragma warning restore CS0169, CS0649, IDE0044, IDE0051
        #endregion

        private readonly SlideshowController fController;

        public IWindow OwnerWindow
        {
            get { return fController.Base; }
        }


        public SlideshowWin(IBaseWindow baseWin)
        {
            XamlReader.Load(this);

            fImageCtl.SelectionMode = ImageBoxSelectionMode.Zoom;

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
