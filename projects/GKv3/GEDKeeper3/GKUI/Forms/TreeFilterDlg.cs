/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2022 by Sergey V. Zhdanovskih.
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
using BSLib;
using BSLib.Design.MVP.Controls;
using Eto.Forms;
using Eto.Serialization.Xaml;
using GKCore;
using GKCore.Charts;
using GKCore.Controllers;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.MVP.Views;
using GKUI.Components;

namespace GKUI.Forms
{
    public sealed partial class TreeFilterDlg : EditorDialog, ITreeFilterDlg
    {
        #region Design components

        private Button btnAccept;
        private Button btnCancel;
        private Label lblRPSources;
        private ComboBox cmbSource;
        private GroupBox rgBranchCut;
        private RadioButton rbCutNone;
        private RadioButton rbCutYears;
        private RadioButton rbCutPersons;
        private Label lblYear;
        private NumericUpDown edYear;
        private Panel Panel1;
        private Panel Panel2;

        #endregion

        private readonly TreeFilterDlgController fController;

        private readonly GKSheetList fPersonsList;

        public ChartFilter Filter
        {
            get { return fController.Filter; }
            set { fController.Filter = value; }
        }

        #region View Interface

        ISheetList ITreeFilterDlg.PersonsList
        {
            get { return fPersonsList; }
        }

        INumericBox ITreeFilterDlg.YearNum
        {
            get { return GetControlHandler<INumericBox>(edYear); }
        }

        IComboBox ITreeFilterDlg.SourceCombo
        {
            get { return GetControlHandler<IComboBox>(cmbSource); }
        }

        void ITreeFilterDlg.SetCutModeRadio(int cutMode)
        {
            switch (cutMode) {
                case 0:
                    rbCutNone.Checked = true;
                    break;
                case 1:
                    rbCutYears.Checked = true;
                    break;
                case 2:
                    rbCutPersons.Checked = true;
                    break;
            }
        }

        int ITreeFilterDlg.GetCutModeRadio()
        {
            int cutMode = 0;
            if (rbCutNone.Checked) cutMode = 0;
            if (rbCutYears.Checked) cutMode = 1;
            if (rbCutPersons.Checked) cutMode = 2;
            return cutMode;
        }

        #endregion

        private void ListModify(object sender, ModifyEventArgs eArgs)
        {
            if (sender == fPersonsList) {
                fController.ModifyPersons(eArgs.Action, eArgs.ItemData);
            }
        }

        private void rbCutX_CheckedChanged(object sender, EventArgs e)
        {
            fController.Filter.BranchCut = (ChartFilter.BranchCutType)((ITreeFilterDlg)this).GetCutModeRadio();
            fController.UpdateControls();
        }

        private void btnAccept_Click(object sender, EventArgs e)
        {
            DialogResult = fController.Accept() ? DialogResult.Ok : DialogResult.None;
        }

        private void btnCancel_Click(object sender, EventArgs e)
        {
            fController.Filter.Reset();
            Close(DialogResult.Cancel);
        }

        private void TreeFilterDlg_Load(object sender, EventArgs e)
        {
            fController.UpdateView();
        }

        public TreeFilterDlg(IBaseWindow baseWin)
        {
            XamlReader.Load(this);

            btnAccept.Image = UIHelper.LoadResourceImage("Resources.btn_accept.gif");
            btnCancel.Image = UIHelper.LoadResourceImage("Resources.btn_cancel.gif");

            fPersonsList = new GKSheetList(Panel2);
            fPersonsList.Buttons = EnumSet<SheetButton>.Create(SheetButton.lbAdd, SheetButton.lbDelete);
            fPersonsList.OnModify += ListModify;

            fController = new TreeFilterDlgController(this);
            fController.Init(baseWin);
        }
    }
}
