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

using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.Types;
using GKUI.Controls;
using GKUI.Engine;

namespace GKUI.Dialogs
{
    /// <summary>
    /// 
    /// </summary>
    public sealed partial class RecordSelectDlg : Form
    {
        private readonly IBaseWindow fBase;

        private GEDCOMRecordType fMode;
        private string fFilter;
        private TargetMode fTargetMode;
        private GKRecordsView fListRecords;

        public GEDCOMIndividualRecord Target { get; set; }
        public GEDCOMSex NeedSex { get; set; }
        public GEDCOMRecord ResultRecord { get; set; }


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

        public GEDCOMRecordType Mode
        {
            get { return fMode; }
            set {
                fMode = value;
                DataRefresh();
            }
        }

        public TargetMode TargetMode
        {
            get { return fTargetMode; }
            set { fTargetMode = value; }
        }


        public RecordSelectDlg(IBaseWindow baseWin)
        {
            InitializeComponent();

            btnSelect.Image = GKResources.iBtnAccept;
            btnCancel.Image = GKResources.iBtnCancel;

            fBase = baseWin;
            fFilter = "*";

            // SetLang()
            Text = LangMan.LS(LSID.LSID_WinRecordSelect);
            btnCreate.Text = LangMan.LS(LSID.LSID_DlgAppend);
            btnSelect.Text = LangMan.LS(LSID.LSID_DlgSelect);
            btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
            }
            base.Dispose(disposing);
        }

        private void DataRefresh()
        {
            if (fListRecords != null)
            {
                fListRecords.Dispose();
                fListRecords = null;
            }

            fListRecords = (GKRecordsView)UIEngine.UIHelper.CreateRecordsView(panList, fBase.Tree, fMode);
            fListRecords.Name = "fListRecords";
            fListRecords.ListMan.Filter.Clear();
            fListRecords.ListMan.QuickFilter = fFilter;

            if (fMode == GEDCOMRecordType.rtIndividual) {
                IndividualListFilter iFilter = (IndividualListFilter)fListRecords.ListMan.Filter;
                iFilter.Sex = NeedSex;
                
                if (fTargetMode == TargetMode.tmParent) {
                    fListRecords.ListMan.ExternalFilter = ChildSelectorHandler;
                }
            }

            fListRecords.UpdateContents(fBase.ShieldState, true, 1);
        }

        private static bool ChildSelectorHandler(GEDCOMRecord record)
        {
            GEDCOMIndividualRecord iRec = record as GEDCOMIndividualRecord;
            if (iRec == null) return false;

            return (iRec.ChildToFamilyLinks.Count == 0);
        }

        private void btnSelect_Click(object sender, EventArgs e)
        {
            try
            {
                ResultRecord = fListRecords.GetSelectedRecord();
                DialogResult = DialogResult.OK;
            }
            catch (Exception ex)
            {
                fBase.Host.LogWrite("RecordSelectDlg.btnSelect_Click(): " + ex.Message);
                ResultRecord = null;
                DialogResult = DialogResult.None;
            }
        }

        private void btnCreate_Click(object sender, EventArgs e)
        {
            try
            {
                switch (fMode) {
                    case GEDCOMRecordType.rtIndividual:
                        {
                            GEDCOMIndividualRecord iRec = null;
                            if (fBase.ModifyPerson(ref iRec, Target, fTargetMode, NeedSex)) {
                                ResultRecord = iRec;
                                DialogResult = DialogResult.OK;
                            }
                            break;
                        }

                    case GEDCOMRecordType.rtFamily:
                        {
                            GEDCOMFamilyRecord famRec = null;

                            FamilyTarget famTarget = (fTargetMode == TargetMode.tmChildToFamily) ? FamilyTarget.Child : FamilyTarget.None;

                            if (fBase.ModifyFamily(ref famRec, famTarget, Target))
                            {
                                ResultRecord = famRec;
                                DialogResult = DialogResult.OK;
                            }
                            break;
                        }

                    case GEDCOMRecordType.rtNote:
                        {
                            GEDCOMNoteRecord noteRec = null;
                            if (fBase.ModifyNote(ref noteRec))
                            {
                                ResultRecord = noteRec;
                                DialogResult = DialogResult.OK;
                            }
                            break;
                        }

                    case GEDCOMRecordType.rtMultimedia:
                        {
                            GEDCOMMultimediaRecord mmRec = null;
                            if (fBase.ModifyMedia(ref mmRec))
                            {
                                ResultRecord = mmRec;
                                DialogResult = DialogResult.OK;
                            }
                            break;
                        }

                    case GEDCOMRecordType.rtSource:
                        {
                            GEDCOMSourceRecord sourceRec = null;
                            if (fBase.ModifySource(ref sourceRec))
                            {
                                ResultRecord = sourceRec;
                                DialogResult = DialogResult.OK;
                            }
                            break;
                        }

                    case GEDCOMRecordType.rtRepository:
                        {
                            GEDCOMRepositoryRecord repRec = null;
                            if (fBase.ModifyRepository(ref repRec))
                            {
                                ResultRecord = repRec;
                                DialogResult = DialogResult.OK;
                            }
                            break;
                        }

                    case GEDCOMRecordType.rtGroup:
                        {
                            GEDCOMGroupRecord groupRec = null;
                            if (fBase.ModifyGroup(ref groupRec))
                            {
                                ResultRecord = groupRec;
                                DialogResult = DialogResult.OK;
                            }
                            break;
                        }

                    case GEDCOMRecordType.rtTask:
                        {
                            GEDCOMTaskRecord taskRec = null;
                            if (fBase.ModifyTask(ref taskRec))
                            {
                                ResultRecord = taskRec;
                                DialogResult = DialogResult.OK;
                            }
                            break;
                        }

                    case GEDCOMRecordType.rtCommunication:
                        {
                            GEDCOMCommunicationRecord corrRec = null;
                            if (fBase.ModifyCommunication(ref corrRec))
                            {
                                ResultRecord = corrRec;
                                DialogResult = DialogResult.OK;
                            }
                            break;
                        }

                    case GEDCOMRecordType.rtLocation:
                        {
                            GEDCOMLocationRecord locRec = null;
                            if (fBase.ModifyLocation(ref locRec))
                            {
                                ResultRecord = locRec;
                                DialogResult = DialogResult.OK;
                            }
                            break;
                        }
                }
            }
            catch (Exception ex)
            {
                fBase.Host.LogWrite("RecordSelectDlg.btnCreate_Click(): " + ex.Message);
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
