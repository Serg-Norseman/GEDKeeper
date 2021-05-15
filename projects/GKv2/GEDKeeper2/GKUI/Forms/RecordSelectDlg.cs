/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2020 by Sergey V. Zhdanovskih.
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
using BSLib.Design.MVP.Controls;
using GDModel;
using GKCore;
using GKCore.Controllers;
using GKCore.Interfaces;
using GKCore.MVP.Views;
using GKCore.Types;
using GKUI.Components;
using GKUI.Platform;

namespace GKUI.Forms
{
    public sealed partial class RecordSelectDlg : EditorDialog, IRecordSelectDialog
    {
        public delegate void UpdateDelegate();

        private readonly RecordSelectDlgController fController;

        private GKListView fListRecords;


        public string FastFilter
        {
            get { return txtFastFilter.Text; }
            set { txtFastFilter.Text = value; }
        }

        public GDMRecord ResultRecord { get; set; }

        #region View Interface

        ITextBox IRecordSelectDialog.FilterBox
        {
            get { return GetControlHandler<ITextBox>(txtFastFilter); }
        }

        IListViewEx IRecordSelectDialog.RecordsList
        {
            get { return fListRecords; }
        }

        #endregion


        public RecordSelectDlg(IBaseWindow baseWin, GDMRecordType recType)
        {
            InitializeComponent();

            btnSelect.Image = UIHelper.LoadResourceImage("Resources.btn_accept.gif");
            btnCancel.Image = UIHelper.LoadResourceImage("Resources.btn_cancel.gif");

            // SetLang()
            Title = LangMan.LS(LSID.LSID_WinRecordSelect);
            btnCreate.Text = LangMan.LS(LSID.LSID_DlgAppend);
            btnSelect.Text = LangMan.LS(LSID.LSID_DlgSelect);
            btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);

            fController = new RecordSelectDlgController(this);
            fController.Init(baseWin);
            fController.RecType = recType;

            UpdateRecordsView();
            FastFilter = "*";
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
            fListRecords = UIHelper.CreateRecordsView(panList, fController.Base.Context, fController.RecType);
            fListRecords.Name = "fListRecords";
        }

        private void btnSelect_Click(object sender, EventArgs e)
        {
            try {
                ResultRecord = fListRecords.GetSelectedData() as GDMRecord;
                DialogResult = DialogResult.OK;
            } catch (Exception ex) {
                Logger.WriteError("RecordSelectDlg.btnSelect_Click()", ex);
                ResultRecord = null;
                DialogResult = DialogResult.None;
            }
        }

        private void btnCreate_Click(object sender, EventArgs e)
        {
            try {
                GDMRecord rec = BaseController.AddRecord(fController.Base, fController.RecType, fController.Target);
                if (rec != null) {
                    ResultRecord = rec;
                    DialogResult = DialogResult.OK;
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
            if (!WFAppHost.TEST_MODE) {
                if (fChangeTimer == null) {
                    fChangeTimer = new System.Timers.Timer(500);
                    fChangeTimer.AutoReset = false;
                    fChangeTimer.Elapsed += (sdr, args) => {
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

        public void SetTarget(TargetMode mode, GDMIndividualRecord target, GDMSex needSex)
        {
            fController.SetTarget(mode, target, needSex);
        }
    }
}
