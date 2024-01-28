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

using GDModel;
using GKCore.Controllers;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Interfaces;
using GKUI.Components;

namespace GKUI.Forms
{
    public sealed partial class LocationNameEditDlg : CommonDialog<ILocationNameEditDlg, LocationNameEditDlgController>, ILocationNameEditDlg
    {
        public GDMLocationName LocationName
        {
            get { return fController.LocationName; }
            set { fController.LocationName = value; }
        }

        #region View Interface

        ITextBox ILocationNameEditDlg.NameText
        {
            get { return GetControlHandler<ITextBox>(txtTitle); }
        }

        ITextBox ILocationNameEditDlg.AbbrText
        {
            get { return GetControlHandler<ITextBox>(txtShortTitle); }
        }

        IDateControl ILocationNameEditDlg.DateCtl
        {
            get { return GetControlHandler<IDateControl>(dateCtl); }
        }

        #endregion

        public LocationNameEditDlg(IBaseWindow baseWin)
        {
            InitializeComponent();

            btnAccept.Image = UIHelper.LoadResourceImage("Resources.btn_accept.gif");
            btnCancel.Image = UIHelper.LoadResourceImage("Resources.btn_cancel.gif");

            fController = new LocationNameEditDlgController(this);
            fController.Init(baseWin);
        }
    }
}
