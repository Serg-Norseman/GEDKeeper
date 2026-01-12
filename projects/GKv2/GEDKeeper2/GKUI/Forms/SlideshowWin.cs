/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
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
