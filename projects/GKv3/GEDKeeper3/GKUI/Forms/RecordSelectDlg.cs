/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2024 by Sergey V. Zhdanovskih.
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
using Eto.Forms;
using Eto.Serialization.Xaml;
using GDModel;
using GKCore;
using GKCore.Controllers;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Interfaces;
using GKCore.Types;
using GKUI.Components;

namespace GKUI.Forms
{
    public sealed partial class RecordSelectDlg : CommonDialog, IRecordSelectDialog
    {
        #region Design components
#pragma warning disable CS0169, CS0649, IDE0044, IDE0051

        private Button btnSelect;
        private Button btnCreate;
        private Button btnCancel;
        private Panel panList;
        public ComboBox txtFastFilter;
        private GKFilterControl fltCtl;

#pragma warning restore CS0169, CS0649, IDE0044, IDE0051
        #endregion

        private readonly RecordSelectDlgController fController;

        private GKListView fListRecords;
        private ContextMenu contextMenu;


        public GDMRecord ResultRecord { get; set; }

        #region View Interface

        IComboBox IRecordSelectDialog.FilterCombo
        {
            get { return GetControlHandler<IComboBox>(txtFastFilter); }
        }

        ITextBox IRecordSelectDialog.FilterText
        {
            get { return null; }
        }

        IFilterControl IRecordSelectDialog.FilterCtl
        {
            get { return fltCtl; }
        }

        IListView IRecordSelectDialog.RecordsList
        {
            get { return fListRecords; }
        }

        #endregion


        public RecordSelectDlg(IBaseWindow baseWin, GDMRecordType recType)
        {
            XamlReader.Load(this);

            txtFastFilter.KeyDown += Ctrl_KeyDown;

            fController = new RecordSelectDlgController(this);
            fController.Init(baseWin);
            fController.RecType = recType;

            fltCtl.ParamsChanged += txtFastFilter_TextChanged;

            var miDetails = new ButtonMenuItem();
            miDetails.Text = LangMan.LS(LSID.Details);
            miDetails.Click += miDetails_Click;

            contextMenu = new ContextMenu();
            contextMenu.Items.AddRange(new MenuItem[] { miDetails });

            UpdateRecordsView();
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                // dummy
            }
            base.Dispose(disposing);
        }

        private void Ctrl_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.Key == Keys.Tab) {
                if (sender == fListRecords) {
                    btnSelect.Focus();
                    e.Handled = true;
                }
                if (sender == txtFastFilter) {
                    fListRecords.Focus();
                    fListRecords.SelectItem(0);
                    e.Handled = true;
                }
            }
        }

        private void UpdateRecordsView()
        {
            if (fListRecords != null) {
                fListRecords.KeyDown -= Ctrl_KeyDown;
                fListRecords.ListMan = null;
                fListRecords.Dispose();
                fListRecords = null;
            }
            fListRecords = UIHelper.CreateRecordsView(panList, fController.Base.Context, fController.RecType, true);
            fListRecords.ContextMenu = contextMenu;
            fListRecords.KeyDown += Ctrl_KeyDown;
        }

        private void miDetails_Click(object sender, EventArgs e)
        {
            fController.ShowDetails();
        }

        private void btnSelect_Click(object sender, EventArgs e)
        {
            try {
                ResultRecord = fListRecords.GetSelectedData() as GDMRecord;
                Close(DialogResult.Ok);
            } catch (Exception ex) {
                Logger.WriteError("RecordSelectDlg.btnSelect_Click()", ex);
                ResultRecord = null;
                DialogResult = DialogResult.None;
            }
        }

        private async void btnCreate_Click(object sender, EventArgs e)
        {
            try {
                GDMRecord rec = await BaseController.AddRecord(this, fController.Base, fController.RecType, fController.Target);
                if (rec != null) {
                    ResultRecord = rec;
                    Close(DialogResult.Ok);
                }
            } catch (Exception ex) {
                Logger.WriteError("RecordSelectDlg.btnCreate_Click()", ex);
                ResultRecord = null;
                DialogResult = DialogResult.None;
            }
        }

        private void txtFastFilter_TextChanged(object sender, EventArgs e)
        {
            fController.UpdateView();
        }

        private void txtFastFilter_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.Key == Keys.Enter) {
                fController.ChangeFilter();
                e.Handled = true;
            }
        }

        public void SetTarget(TargetMode mode, GDMIndividualRecord target, GDMSex needSex, string defFilter = "*")
        {
            fController.SetTarget(mode, target, needSex, defFilter);
        }
    }
}
