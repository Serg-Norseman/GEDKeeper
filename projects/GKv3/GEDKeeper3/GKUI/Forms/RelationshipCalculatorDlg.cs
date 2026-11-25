/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using Eto.Forms;
using Eto.Serialization.Xaml;
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Design.Views;

namespace GKUI.Forms
{
    public sealed partial class RelationshipCalculatorDlg : CommonDialog<IRelationshipCalculatorDlg, RelationshipCalculatorDlgController>, IRelationshipCalculatorDlg
    {
        #region Design components
#pragma warning disable CS0169, CS0649, IDE0044, IDE0051

        private Label lblKinship;
        private TextArea txtResult;
        private Button btnRec2Select;
        private Button btnRec1Select;
        private TextBox Edit2;
        private TextBox Edit1;
        private Label Lab2;
        private Label Lab1;
        private Button btnClose;
        private Button btnSwap;

#pragma warning restore CS0169, CS0649, IDE0044, IDE0051
        #endregion

        public IBaseWindow Base
        {
            get { return fController.Base; }
        }

        public RelationshipCalculatorDlg(IBaseWindow baseWin)
        {
            XamlReader.Load(this);

            fController = new RelationshipCalculatorDlgController(this);
            fController.InitView(baseWin);

            fController.SetRec1(null);
            fController.SetRec2(null);
        }

        private void btnRec1Select_Click(object sender, EventArgs e)
        {
            fController.SelectRec1();
        }

        private void btnRec2Select_Click(object sender, EventArgs e)
        {
            fController.SelectRec2();
        }

        private void btnSwap_Click(object sender, EventArgs e)
        {
            fController.Swap();
        }

        public override void ApplyTheme()
        {
            base.ApplyTheme();
            fController.ApplyTheme();
        }
    }
}
