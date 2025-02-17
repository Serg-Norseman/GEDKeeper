/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
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
using GDModel;
using GKCore.Design.Controls;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.Design;
using GKCore.Design.Views;
using GKCore.Types;
using GKUI.Themes;

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

        public override void Init(IBaseWindow baseWin)
        {
            base.Init(baseWin);

            fView.TasksList.ListModel = new ResTasksListModel(fView, baseWin, fLocalUndoman);
            fView.CommunicationsList.ListModel = new ResCommunicationsListModel(fView, baseWin, fLocalUndoman);
            fView.GroupsList.ListModel = new ResGroupsListModel(fView, baseWin, fLocalUndoman);
            fView.NotesList.ListModel = new NoteLinksListModel(fView, baseWin, fLocalUndoman);
        }

        public override void Done()
        {
            fView.TasksList.ListModel.SaveSettings();
            fView.CommunicationsList.ListModel.SaveSettings();
            fView.GroupsList.ListModel.SaveSettings();
            fView.NotesList.ListModel.SaveSettings();
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

        public override void SetLocale()
        {
            fView.Title = LangMan.LS(LSID.WinResearchEdit);

            GetControl<IButton>("btnAccept").Text = LangMan.LS(LSID.DlgAccept);
            GetControl<IButton>("btnCancel").Text = LangMan.LS(LSID.DlgCancel);
            GetControl<ITabPage>("pageTasks").Text = LangMan.LS(LSID.RPTasks);
            GetControl<ITabPage>("pageCommunications").Text = LangMan.LS(LSID.RPCommunications);
            GetControl<ITabPage>("pageGroups").Text = LangMan.LS(LSID.RPGroups);
            GetControl<ITabPage>("pageNotes").Text = LangMan.LS(LSID.RPNotes);
            GetControl<ILabel>("lblName").Text = LangMan.LS(LSID.Title);
            GetControl<ILabel>("lblPriority").Text = LangMan.LS(LSID.Priority);
            GetControl<ILabel>("lblStatus").Text = LangMan.LS(LSID.Status);
            GetControl<ILabel>("lblPercent").Text = LangMan.LS(LSID.Percent);
            GetControl<ILabel>("lblStartDate").Text = LangMan.LS(LSID.StartDate);
            GetControl<ILabel>("lblStopDate").Text = LangMan.LS(LSID.StopDate);
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            GetControl<IButton>("btnAccept").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Accept);
            GetControl<IButton>("btnCancel").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Cancel);

            fView.TasksList.ApplyTheme();
            fView.CommunicationsList.ApplyTheme();
            fView.GroupsList.ApplyTheme();
            fView.NotesList.ApplyTheme();
        }
    }
}
