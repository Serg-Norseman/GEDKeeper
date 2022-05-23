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
using Eto.Forms;
using Eto.Serialization.Xaml;
using GDModel;
using GKCore.Controllers;
using GKCore.Interfaces;
using GKCore.MVP.Controls;
using GKCore.MVP.Views;

namespace GKUI.Forms
{
    public sealed partial class PortraitSelectDlg : EditorDialog, IPortraitSelectDlg
    {
        #region Design components
#pragma warning disable CS0169, CS0649, IDE0044, IDE0051

        private Button btnAccept;
        private Button btnCancel;
        private GKUI.Components.ImageView imageView1;

#pragma warning restore CS0169, CS0649, IDE0044, IDE0051
        #endregion

        private readonly PortraitSelectDlgController fController;

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
            XamlReader.Load(this);

            fController = new PortraitSelectDlgController(this);
            fController.Init(baseWin);
        }
    }
}
