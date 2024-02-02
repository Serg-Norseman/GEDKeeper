/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2024 by Sergey V. Zhdanovskih.
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
using GDModel;
using GKCore.Controllers;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Interfaces;
using GKUI.Components;

namespace GKUI.Forms
{
    public sealed partial class LocationLinkEditDlg : CommonDialog<ILocationLinkEditDlg, LocationLinkEditDlgController>, ILocationLinkEditDlg
    {
        public GDMLocationLink LocationLink
        {
            get { return fController.LocationLink; }
            set { fController.LocationLink = value; }
        }

        #region View Interface

        ITextBox ILocationLinkEditDlg.TopLevelText
        {
            get { return GetControlHandler<ITextBox>(txtTopLevel); }
        }

        IDateControl ILocationLinkEditDlg.DateCtl
        {
            get { return GetControlHandler<IDateControl>(dateCtl); }
        }

        #endregion

        public LocationLinkEditDlg(IBaseWindow baseWin)
        {
            InitializeComponent();

            btnLocationAdd.Image = UIHelper.LoadResourceImage("Resources.btn_rec_new.gif");
            btnAccept.Image = UIHelper.LoadResourceImage("Resources.btn_accept.gif");
            btnCancel.Image = UIHelper.LoadResourceImage("Resources.btn_cancel.gif");

            fController = new LocationLinkEditDlgController(this);
            fController.Init(baseWin);
        }

        private void btnLocationAdd_Click(object sender, EventArgs e)
        {
            fController.SetLocation();
        }
    }
}
