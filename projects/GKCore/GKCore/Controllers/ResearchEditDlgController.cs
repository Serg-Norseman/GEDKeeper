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
using BSLib.Design.MVP.Controls;
using GDModel;
using GKCore.MVP;
using GKCore.MVP.Views;
using GKCore.Types;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class ResearchEditDlgController : DialogController<IResearchEditDlg>
    {
        private GDMResearchRecord fResearchRecord;

        public GDMResearchRecord ResearchRecord
        {
            get { return fResearchRecord; }
            set {
                if (fResearchRecord != value) {
                    fResearchRecord = value;
                    UpdateView();
                }
            }
        }


        public ResearchEditDlgController(IResearchEditDlg view) : base(view)
        {
            for (GDMResearchPriority rp = GDMResearchPriority.rpNone; rp <= GDMResearchPriority.rpTop; rp++) {
                fView.Priority.Add(LangMan.LS(GKData.PriorityNames[(int)rp]));
            }

            for (GDMResearchStatus rs = GDMResearchStatus.rsDefined; rs <= GDMResearchStatus.rsWithdrawn; rs++) {
                fView.Status.Add(LangMan.LS(GKData.StatusNames[(int)rs]));
            }
        }

        public override bool Accept()
        {
            try {
                fResearchRecord.ResearchName = fView.Name.Text;
                fResearchRecord.Priority = (GDMResearchPriority)fView.Priority.SelectedIndex;
                fResearchRecord.Status = (GDMResearchStatus)fView.Status.SelectedIndex;
                fResearchRecord.StartDate.Assign(GDMDate.CreateByFormattedStr(fView.StartDate.NormalizeDate, true));
                fResearchRecord.StopDate.Assign(GDMDate.CreateByFormattedStr(fView.StopDate.NormalizeDate, true));
                fResearchRecord.Percent = int.Parse(fView.Percent.Text);

                fLocalUndoman.Commit();

                fBase.NotifyRecord(fResearchRecord, RecordAction.raEdit);

                return true;
            } catch (Exception ex) {
                Logger.WriteError("ResearchEditDlgController.Accept()", ex);
                return false;
            }
        }

        public override void UpdateView()
        {
            if (fResearchRecord == null) {
                fView.Name.Text = "";
                fView.Priority.SelectedIndex = -1;
                fView.Status.SelectedIndex = -1;
                fView.StartDate.Text = "";
                fView.StopDate.Text = "";
                fView.Percent.Value = 0;
            } else {
                fView.Name.Text = fResearchRecord.ResearchName;
                fView.Priority.SelectedIndex = (int)fResearchRecord.Priority;
                fView.Status.SelectedIndex = (int)fResearchRecord.Status;
                fView.StartDate.NormalizeDate = fResearchRecord.StartDate.GetDisplayString(DateFormat.dfDD_MM_YYYY);
                fView.StopDate.NormalizeDate = fResearchRecord.StopDate.GetDisplayString(DateFormat.dfDD_MM_YYYY);
                fView.Percent.Value = fResearchRecord.Percent;
            }

            fView.NotesList.ListModel.DataOwner = fResearchRecord;
            fView.TasksList.ListModel.DataOwner = fResearchRecord;
            fView.CommunicationsList.ListModel.DataOwner = fResearchRecord;
            fView.GroupsList.ListModel.DataOwner = fResearchRecord;
        }

        public void JumpToRecord(GDMRecord record)
        {
            if (record != null && Accept()) {
                fBase.SelectRecordByXRef(record.XRef);
                fView.Close();
            }
        }

        public void JumpToRecord(GDMPointer pointer)
        {
            if (pointer != null && Accept()) {
                fBase.SelectRecordByXRef(pointer.XRef);
                fView.Close();
            }
        }

        public override void SetLocale()
        {
            fView.Title = LangMan.LS(LSID.LSID_WinResearchEdit);

            GetControl<IButton>("btnAccept").Text = LangMan.LS(LSID.LSID_DlgAccept);
            GetControl<IButton>("btnCancel").Text = LangMan.LS(LSID.LSID_DlgCancel);
            GetControl<ITabPage>("pageTasks").Text = LangMan.LS(LSID.LSID_RPTasks);
            GetControl<ITabPage>("pageCommunications").Text = LangMan.LS(LSID.LSID_RPCommunications);
            GetControl<ITabPage>("pageGroups").Text = LangMan.LS(LSID.LSID_RPGroups);
            GetControl<ITabPage>("pageNotes").Text = LangMan.LS(LSID.LSID_RPNotes);
            GetControl<ILabel>("lblName").Text = LangMan.LS(LSID.LSID_Title);
            GetControl<ILabel>("lblPriority").Text = LangMan.LS(LSID.LSID_Priority);
            GetControl<ILabel>("lblStatus").Text = LangMan.LS(LSID.LSID_Status);
            GetControl<ILabel>("lblPercent").Text = LangMan.LS(LSID.LSID_Percent);
            GetControl<ILabel>("lblStartDate").Text = LangMan.LS(LSID.LSID_StartDate);
            GetControl<ILabel>("lblStopDate").Text = LangMan.LS(LSID.LSID_StopDate);
        }
    }
}
