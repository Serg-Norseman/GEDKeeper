/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Windows.Forms;
using GKCore.Controllers;
using GKCore.Design.Views;
using GKCore.Names;

namespace GKUI.Forms
{
    public sealed partial class NameEditDlg : CommonDialog<INameEditDlg, NameEditDlgController>, INameEditDlg
    {
        public NameEntry IName
        {
            get { return fController.NameEntry; }
            set { fController.NameEntry = value; }
        }

        public NameEditDlg()
        {
            InitializeComponent();

            fController = new NameEditDlgController(this);
            fController.InitView(null);
        }

        private void edName_KeyPress(object sender, KeyPressEventArgs e)
        {
            if (e.KeyChar == '/') {
                e.Handled = true;
            }
        }
    }
}
