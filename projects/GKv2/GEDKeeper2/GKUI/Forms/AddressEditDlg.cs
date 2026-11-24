/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Windows.Forms;
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
        private readonly GKSheetList fPhonesList;
        private readonly GKSheetList fMailsList;
        private readonly GKSheetList fWebsList;

        public GDMAddress Address
        {
            get { return fController.Address; }
            set { fController.Address = value; }
        }

        public AddressEditDlg(IBaseWindow baseWin)
        {
            InitializeComponent();

            txtAddress.KeyDown += txtAddress_KeyDown;

            fPhonesList = new GKSheetList(pagePhones);
            fMailsList = new GKSheetList(pageEmails);
            fWebsList = new GKSheetList(pageWebPages);

            fController = new AddressEditDlgController(this);
            fController.InitView(baseWin);
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
