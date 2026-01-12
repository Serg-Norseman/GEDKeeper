/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GDModel;
using GKCore.Controllers;
using GKCore.Design.Views;

namespace GKUI.Forms
{
    public sealed partial class SexCheckDlg : CommonDialog<ISexCheckDlg, SexCheckDlgController>, ISexCheckDlg
    {
        public string IndividualName
        {
            get { return txtName.Text; }
            set { txtName.Text = value; }
        }

        public GDMSex Sex
        {
            get { return fController.Sex; }
            set { fController.Sex = value; }
        }

        public SexCheckDlg()
        {
            InitializeComponent();

            fController = new SexCheckDlgController(this);
        }
    }
}
