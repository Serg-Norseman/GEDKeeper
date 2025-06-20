﻿/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
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

using System.Windows.Forms;
using GDModel;
using GDModel.Providers.GEDCOM;
using GKCore.Controllers;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Interfaces;
using GKCore.Lists;
using GKUI.Components;

namespace GKUI.Forms
{
    public sealed partial class AddressEditDlg : CommonDialog<IAddressEditDlg, AddressEditDlgController>, IAddressEditDlg
    {
        private readonly GKSheetList fPhonesList;
        private readonly GKSheetList fMailsList;
        private readonly GKSheetList fWebsList;

        public GDMAddress Address
        {
            get { return fController.Address; }
            set { fController.Address = value; }
        }

        #region View Interface

        ISheetList IAddressEditDlg.PhonesList
        {
            get { return fPhonesList; }
        }

        ISheetList IAddressEditDlg.MailsList
        {
            get { return fMailsList; }
        }

        ISheetList IAddressEditDlg.WebsList
        {
            get { return fWebsList; }
        }


        ITextBox IAddressEditDlg.Country
        {
            get { return GetControlHandler<ITextBox>(txtCountry); }
        }

        ITextBox IAddressEditDlg.State
        {
            get { return GetControlHandler<ITextBox>(txtState); }
        }

        ITextBox IAddressEditDlg.City
        {
            get { return GetControlHandler<ITextBox>(txtCity); }
        }

        ITextBox IAddressEditDlg.PostalCode
        {
            get { return GetControlHandler<ITextBox>(txtPostalCode); }
        }

        ITextBox IAddressEditDlg.AddressLine
        {
            get { return GetControlHandler<ITextBox>(txtAddress); }
        }

        #endregion

        public AddressEditDlg(IBaseWindow baseWin)
        {
            InitializeComponent();

            txtAddress.KeyDown += txtAddress_KeyDown;

            fPhonesList = new GKSheetList(pagePhones);
            fMailsList = new GKSheetList(pageEmails);
            fWebsList = new GKSheetList(pageWebPages);

            fController = new AddressEditDlgController(this);
            fController.Init(baseWin);
        }

        private void txtAddress_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.KeyCode == Keys.Return && txtAddress.Lines.Length == GEDCOMConsts.Address_Lines_MaxCount) {
                e.SuppressKeyPress = true;
                e.Handled = true;
            }
        }
    }
}
