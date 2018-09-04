/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2018 by Sergey V. Zhdanovskih.
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

using System;

using GKCore;
using GKCore.Controllers;
using GKCore.Interfaces;
using GKCore.UIContracts;
using GKUI.Components;

namespace GKUI.Forms
{
    /// <summary>
    /// 
    /// </summary>
    public sealed partial class RelationshipCalculatorDlg : EditorDialog, IRelationshipCalculatorDlg
    {
        private readonly RelationshipCalculatorDlgController fController;

        #region View Interface

        ILabelHandler IRelationshipCalculatorDlg.Label1
        {
            get { return fControlsManager.GetControlHandler<ILabelHandler>(Lab1); }
        }

        ILabelHandler IRelationshipCalculatorDlg.Label2
        {
            get { return fControlsManager.GetControlHandler<ILabelHandler>(Lab2); }
        }

        ITextBoxHandler IRelationshipCalculatorDlg.Person1
        {
            get { return fControlsManager.GetControlHandler<ITextBoxHandler>(Edit1); }
        }

        ITextBoxHandler IRelationshipCalculatorDlg.Person2
        {
            get { return fControlsManager.GetControlHandler<ITextBoxHandler>(Edit2); }
        }

        ITextBoxHandler IRelationshipCalculatorDlg.Result
        {
            get { return fControlsManager.GetControlHandler<ITextBoxHandler>(txtResult); }
        }

        #endregion

        public RelationshipCalculatorDlg(IBaseWindow baseWin)
        {
            InitializeComponent();

            btnClose.Image = UIHelper.LoadResourceImage("Resources.btn_cancel.gif");

            // SetLang()
            Text = LangMan.LS(LSID.LSID_RelationshipCalculator);
            btnClose.Text = LangMan.LS(LSID.LSID_DlgClose);
            btnRec1Select.Text = LangMan.LS(LSID.LSID_DlgSelect) + @"...";
            btnRec2Select.Text = LangMan.LS(LSID.LSID_DlgSelect) + @"...";
            lblKinship.Text = LangMan.LS(LSID.LSID_Kinship);

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
    }
}
