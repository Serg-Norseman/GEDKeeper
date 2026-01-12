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
using GKCore.Design.Controls;
using GKCore.Design.Views;

namespace GKUI.Forms
{
    public sealed partial class RelationshipCalculatorDlg : CommonDialog, IRelationshipCalculatorDlg
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

#pragma warning restore CS0169, CS0649, IDE0044, IDE0051
        #endregion

        private readonly RelationshipCalculatorDlgController fController;

        public IBaseWindow Base
        {
            get { return fController.Base; }
        }

        #region View Interface

        ILabel IRelationshipCalculatorDlg.Label1
        {
            get { return GetControlHandler<ILabel>(Lab1); }
        }

        ILabel IRelationshipCalculatorDlg.Label2
        {
            get { return GetControlHandler<ILabel>(Lab2); }
        }

        ITextBox IRelationshipCalculatorDlg.Person1
        {
            get { return GetControlHandler<ITextBox>(Edit1); }
        }

        ITextBox IRelationshipCalculatorDlg.Person2
        {
            get { return GetControlHandler<ITextBox>(Edit2); }
        }

        ITextBox IRelationshipCalculatorDlg.Result
        {
            get { return GetControlHandler<ITextBox>(txtResult); }
        }

        #endregion

        public RelationshipCalculatorDlg(IBaseWindow baseWin)
        {
            XamlReader.Load(this);

            fController = new RelationshipCalculatorDlgController(this);
            fController.Init(baseWin);

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
