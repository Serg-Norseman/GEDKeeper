/*
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

using Eto.Forms;
using Eto.Serialization.Xaml;
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
        #region Design components
#pragma warning disable CS0169, CS0649, IDE0044, IDE0051

        private Button btnAccept;
        private Button btnCancel;
        private TabPage pagePhones;
        private TabPage pageEmails;
        private TabPage pageCommon;
        private TabPage pageWebPages;
        private Label lblCountry;
        private Label lblState;
        private Label lblCity;
        private Label lblPostalCode;
        private Label lblAddress;
        private TextBox txtCountry;
        private TextBox txtState;
        private TextBox txtCity;
        private TextBox txtPostalCode;
        private TextArea txtAddress;
        private GKSheetList fPhonesList;
        private GKSheetList fMailsList;
        private GKSheetList fWebsList;

#pragma warning restore CS0169, CS0649, IDE0044, IDE0051
        #endregion

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
            XamlReader.Load(this);

            txtAddress.KeyDown += txtAddress_KeyDown;

            fController = new AddressEditDlgController(this);
            fController.Init(baseWin);
        }

        private void txtAddress_KeyDown(object sender, KeyEventArgs e)
        {
            var lines = UIHelper.Convert(txtAddress.Text);
            if (e.Key == Keys.Enter && lines.Length == GEDCOMConsts.Address_Lines_MaxCount) {
                e.Handled = true;
            }
        }
    }
}
