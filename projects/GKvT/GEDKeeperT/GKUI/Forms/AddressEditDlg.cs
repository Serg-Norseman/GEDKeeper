/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GDModel;
using GDModel.Providers.GEDCOM;
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Lists;
using GKUI.Components;
using Terminal.Gui;

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

            fPhonesList = new GKSheetList(pagePhones);
            fMailsList = new GKSheetList(pageEmails);
            fWebsList = new GKSheetList(pageWebPages);

            fController = new AddressEditDlgController(this);
            fController.Init(baseWin);
        }

        private void txtAddress_KeyDown(object sender, KeyEventEventArgs e)
        {
            if (e.KeyEvent.Key == Key.Enter && txtAddress.Lines == GEDCOMConsts.Address_Lines_MaxCount) {
                e.Handled = true;
            }
        }
    }
}
