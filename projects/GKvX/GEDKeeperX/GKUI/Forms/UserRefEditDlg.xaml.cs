﻿/*
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

using BSLib.Design.MVP.Controls;
using GDModel;
using GKCore.Controllers;
using GKCore.Interfaces;
using GKCore.MVP.Views;
using Xamarin.Forms.Xaml;

namespace GKUI.Forms
{
    /// <summary>
    /// 
    /// </summary>
    [XamlCompilation(XamlCompilationOptions.Compile)]
    public partial class UserRefEditDlg : CommonDialog<IUserRefEditDlg, UserRefEditDlgController>, IUserRefEditDlg
    {
        public GDMUserReference UserReference
        {
            get { return fController.UserReference; }
            set { fController.UserReference = value; }
        }

        #region View Interface

        IComboBox IUserRefEditDlg.Ref
        {
            get { return fControlsManager.GetControl<IComboBox>(cmbRef); }
        }

        IComboBox IUserRefEditDlg.RefType
        {
            get { return fControlsManager.GetControl<IComboBox>(cmbRefType); }
        }

        #endregion

        public UserRefEditDlg() : this(null)
        {
        }

        public UserRefEditDlg(IBaseWindow baseWin)
        {
            InitializeComponent();

            fController = new UserRefEditDlgController(this);
            fController.Init(baseWin);
        }
    }
}
