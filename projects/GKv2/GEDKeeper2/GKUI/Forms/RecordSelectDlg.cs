/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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

using GKCommon;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.Types;
using GKCore.UIContracts;
using GKUI.Components;

namespace GKUI.Forms
{
    /// <summary>
    /// 
    /// </summary>
    public sealed partial class RecordSelectDlg : EditorDialog, IRecordSelectDialog
    {
        private string fFilter;
        private GKListView fListRecords;
        private GEDCOMRecordType fRecType;
        private Target fTarget;


        public string FastFilter
        {
            get { return txtFastFilter.Text; }
            set { txtFastFilter.Text = value; }
        }

        public string Filter
        {
            get { return fFilter; }
            set {
                string flt = value;
                if (flt == "") {
                    flt = "*";
                } else if (flt != "*") {
                    flt = "*" + flt + "*";
                }
                fFilter = flt;
                DataRefresh();
            }
        }

        public GEDCOMRecordType RecType
        {
            get { return fRecType; }
            set {
                fRecType = value;
                DataRefresh();
            }
        }

        public GEDCOMSex NeedSex
        {
            get { return fTarget.NeedSex; }
            set { fTarget.NeedSex = value; }
        }

        public GEDCOMRecord ResultRecord { get; set; }

        public GEDCOMIndividualRecord Target
        {
            get { return fTarget.TargetIndividual; }
            set { fTarget.TargetIndividual = value; }
        }

        public TargetMode TargetMode
        {
            get { return fTarget.TargetMode; }
            set { fTarget.TargetMode = value; }
        }


        public RecordSelectDlg()
        {
            InitializeComponent();

            btnSelect.Image = GKResources.iBtnAccept;
            btnCancel.Image = GKResources.iBtnCancel;

            // SetLang()
            Text = LangMan.LS(LSID.LSID_WinRecordSelect);
            btnCreate.Text = LangMan.LS(LSID.LSID_DlgAppend);
            btnSelect.Text = LangMan.LS(LSID.LSID_DlgSelect);
            btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);

            fTarget = new Target();
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
            }
            base.Dispose(disposing);
        }

        public override void InitDialog(IBaseWindow baseWin)
        {
            base.InitDialog(baseWin);
            fFilter = "*";
        }

        private void DataRefresh()
        {
            if (fListRecords != null)
            {
                fListRecords.Dispose();
                fListRecords = null;
            }

            fListRecords = UIHelper.CreateRecordsView(panList, fBase.Context, fRecType);
            fListRecords.Name = "fListRecords";
            fListRecords.ListMan.Filter.Clear();
            fListRecords.ListMan.QuickFilter = fFilter;

            if (fRecType == GEDCOMRecordType.rtIndividual) {
                IndividualListFilter iFilter = (IndividualListFilter)fListRecords.ListMan.Filter;
                iFilter.Sex = NeedSex;

                if (fTarget.TargetMode == TargetMode.tmParent) {
                    fListRecords.ListMan.ExternalFilter = ChildSelectorHandler;
                }
            }

            fListRecords.UpdateContents();
        }

        private static bool ChildSelectorHandler(GEDCOMRecord record)
        {
            GEDCOMIndividualRecord iRec = record as GEDCOMIndividualRecord;
            return (iRec == null) ? false : (iRec.ChildToFamilyLinks.Count == 0);
        }

        private void btnSelect_Click(object sender, EventArgs e)
        {
            try
            {
                ResultRecord = fListRecords.GetSelectedData() as GEDCOMRecord;
                DialogResult = DialogResult.OK;
            }
            catch (Exception ex)
            {
                Logger.LogWrite("RecordSelectDlg.btnSelect_Click(): " + ex.Message);
                ResultRecord = null;
                DialogResult = DialogResult.None;
            }
        }

        private void btnCreate_Click(object sender, EventArgs e)
        {
            try
            {
                GEDCOMRecord rec = BaseController.AddRecord(fBase, fRecType, fTarget);
                if (rec != null) {
                    ResultRecord = rec;
                    DialogResult = DialogResult.OK;
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("RecordSelectDlg.btnCreate_Click(): " + ex.Message);
                ResultRecord = null;
                DialogResult = DialogResult.None;
            }
        }

        private void txtFastFilter_TextChanged(object sender, EventArgs e)
        {
            Filter = txtFastFilter.Text;
        }
    }
}
