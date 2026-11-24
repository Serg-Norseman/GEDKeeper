/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using Eto.Forms;
using Eto.Serialization.Xaml;
using GDModel;
using GDModel.Providers.GEDCOM;
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Design.Views;
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

        public AddressEditDlg(IBaseWindow baseWin)
        {
            XamlReader.Load(this);

            txtAddress.KeyDown += txtAddress_KeyDown;

            fController = new AddressEditDlgController(this);
            fController.InitView(baseWin);
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
