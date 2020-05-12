﻿/*
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
using Eto.Forms;
using GDModel;
using GKCore;
using GKCore.Controllers;
using GKCore.Interfaces;
using GKCore.MVP.Controls;
using GKCore.MVP.Views;
using GKUI.Components;

namespace GKUI.Forms
{
    public sealed partial class PortraitSelectDlg : EditorDialog, IPortraitSelectDlg
    {
        private readonly PortraitSelectDlgController fController;

        private ITimer fTimer;

        public GDMMultimediaLink MultimediaLink
        {
            get { return fController.MultimediaLink; }
            set { fController.MultimediaLink = value; }
        }

        #region View Interface

        IImageView IPortraitSelectDlg.ImageCtl
        {
            get { return imageView1; }
        }

        #endregion

        private void btnAccept_Click(object sender, EventArgs e)
        {
            DialogResult = fController.Accept() ? DialogResult.Ok : DialogResult.None;
        }

        public PortraitSelectDlg(IBaseWindow baseWin)
        {
            InitializeComponent();

            btnAccept.Image = UIHelper.LoadResourceImage("Resources.btn_accept.gif");
            btnCancel.Image = UIHelper.LoadResourceImage("Resources.btn_cancel.gif");

            // SetLang()
            btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
            btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
            Title = LangMan.LS(LSID.LSID_PortraitSelect);

            fTimer = AppHost.Instance.CreateTimer(100.0f, InitViewer_Tick);
            fTimer.Start();

            fController = new PortraitSelectDlgController(this);
            fController.Init(baseWin);
        }

        protected override void OnLoad(EventArgs e)
        {
            base.OnLoad(e);

            if (imageView1 != null) {
                imageView1.Focus();
                imageView1.Invalidate();
                imageView1.ZoomToFit();
            }
        }

        // dirty temporary hack
        private void InitViewer_Tick(object sender, EventArgs e)
        {
            if (imageView1 != null && !imageView1.Viewport.Size.IsEmpty) {
                imageView1.ZoomToFit();
                fTimer.Stop();
            }
        }
    }
}
