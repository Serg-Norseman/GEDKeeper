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
using System.Windows.Forms;
using GDModel;
using GKCore;
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Interfaces;
using GKCore.Types;
using GKUI.Components;

namespace GKUI.Forms
{
    public sealed partial class RecordSelectDlg : CommonDialog, IRecordSelectDialog
    {
        public delegate void UpdateDelegate();

        private readonly RecordSelectDlgController fController;

        private GKListView fListRecords;
        private ContextMenuStrip contextMenu;


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
            InitializeComponent();

            TabIndexChanged += Form_TabIndexChanged;

            btnSelect.Image = UIHelper.LoadResourceImage("Resources.btn_accept.gif");
            btnCancel.Image = UIHelper.LoadResourceImage("Resources.btn_cancel.gif");

            fController = new RecordSelectDlgController(this);
            fController.Init(baseWin);
            fController.RecType = recType;

            fltCtl.ParamsChanged += txtFastFilter_TextChanged;

            var miDetails = new ToolStripMenuItem();
            miDetails.Text = LangMan.LS(LSID.Details);
            miDetails.Click += miDetails_Click;

            contextMenu = new ContextMenuStrip();
            contextMenu.Items.AddRange(new ToolStripItem[] { miDetails });

            UpdateRecordsView();
        }

        private void Form_TabIndexChanged(object sender, EventArgs e)
        {
            if (fltCtl.Focused) {
                fListRecords.Focus();
                fListRecords.SelectItem(0);
            }
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                if (fChangeTimer != null) {
                    fChangeTimer.Stop();
                    fChangeTimer.Dispose();
                    fChangeTimer = null;
                }
            }
            base.Dispose(disposing);
        }

        private void UpdateRecordsView()
        {
            if (fListRecords != null) {
                fListRecords.ListMan = null;
                fListRecords.Dispose();
                fListRecords = null;
            }
            fListRecords = UIHelper.CreateRecordsView(panList, fController.Base.Context, fController.RecType, true);
            fListRecords.Name = "fListRecords";
            fListRecords.ContextMenuStrip = contextMenu;
        }

        private void miDetails_Click(object sender, EventArgs e)
        {
            fController.ShowDetails();
        }

        private void btnSelect_Click(object sender, EventArgs e)
        {
            try {
                ResultRecord = fListRecords.GetSelectedData() as GDMRecord;
                Close(DialogResult.OK);
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
                    Close(DialogResult.OK);
                }
            } catch (Exception ex) {
                Logger.WriteError("RecordSelectDlg.btnCreate_Click()", ex);
                ResultRecord = null;
                DialogResult = DialogResult.None;
            }
        }

        private System.Timers.Timer fChangeTimer;

        private void txtFastFilter_TextChanged(object sender, EventArgs e)
        {
            if (!AppHost.TEST_MODE) {
                if (fChangeTimer == null) {
                    fChangeTimer = new System.Timers.Timer(500);
                    fChangeTimer.AutoReset = false;
                    fChangeTimer.Elapsed += (sdr, args) => {
                        if (IsHandleCreated)
                            BeginInvoke(new UpdateDelegate(fController.UpdateView));
                    };
                } else {
                    fChangeTimer.Stop();
                }
                fChangeTimer.Start();
            } else {
                fController.UpdateView();
            }
        }

        private void txtFastFilter_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.KeyCode == Keys.Enter) {
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
