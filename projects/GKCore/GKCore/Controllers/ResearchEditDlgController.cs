/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using GDModel;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Lists;
using GKCore.Locales;
using GKUI.Themes;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class ResearchEditDlgController : DialogController<IResearchEditDlg>
    {
        #region Design components
#pragma warning disable CS0649, IDE0044
        private ITabPage _pageTasks;
        private ITabPage _pageCommunications;
        private ITabPage _pageGroups;
        private ITabPage _pageNotes;
        private IButton _btnAccept;
        private IButton _btnCancel;
        private ITextBox _txtName;
        private ILabel _lblName;
        private ILabel _lblPriority;
        private IComboBox _cmbPriority;
        private ILabel _lblStatus;
        private IComboBox _cmbStatus;
        private ILabel _lblStartDate;
        private IDateBox _txtStartDate;
        private ILabel _lblStopDate;
        private IDateBox _txtStopDate;
        private ILabel _lblPercent;
        private INumericBox _nudPercent;
        private ISheetList _fTasksList;
        private ISheetList _fCommunicationsList;
        private ISheetList _fGroupsList;
        private ISheetList _fNotesList;
#pragma warning restore CS0649, IDE0044
        #endregion

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
        }

        private void ListJumpHandler(object sender, ModifyEventArgs eArgs)
        {
            if (eArgs.Action == RecordAction.raJump) {
                JumpToRecord(eArgs.ItemData as GDMPointer);
            }
        }

        public override void InitView(IBaseWindow baseWin)
        {
            base.InitView(baseWin);

            _fTasksList.OnModify += ListJumpHandler;
            _fCommunicationsList.OnModify += ListJumpHandler;
            _fGroupsList.OnModify += ListJumpHandler;

            _cmbPriority.ReadOnly = true;
            for (GDMResearchPriority rp = GDMResearchPriority.rpNone; rp <= GDMResearchPriority.rpTop; rp++) {
                _cmbPriority.Add(LangMan.LS(GKData.PriorityNames[(int)rp]));
            }

            _cmbStatus.ReadOnly = true;
            for (GDMResearchStatus rs = GDMResearchStatus.rsDefined; rs <= GDMResearchStatus.rsWithdrawn; rs++) {
                _cmbStatus.Add(LangMan.LS(GKData.StatusNames[(int)rs]));
            }

            _fTasksList.ListModel = new ResTasksListModel(fView, baseWin, fLocalUndoman);
            _fCommunicationsList.ListModel = new ResCommunicationsListModel(fView, baseWin, fLocalUndoman);
            _fGroupsList.ListModel = new ResGroupsListModel(fView, baseWin, fLocalUndoman);
            _fNotesList.ListModel = new NoteLinksListModel(fView, baseWin, fLocalUndoman);
        }

        public override void DoneView()
        {
            _fTasksList.ListModel.SaveSettings();
            _fCommunicationsList.ListModel.SaveSettings();
            _fGroupsList.ListModel.SaveSettings();
            _fNotesList.ListModel.SaveSettings();
        }

        public override bool Accept()
        {
            try {
                fResearchRecord.ResearchName = _txtName.Text;
                fResearchRecord.Priority = (GDMResearchPriority)_cmbPriority.SelectedIndex;
                fResearchRecord.Status = (GDMResearchStatus)_cmbStatus.SelectedIndex;
                fResearchRecord.StartDate.Assign(GDMDate.CreateByFormattedStr(_txtStartDate.NormalizeDate, true));
                fResearchRecord.StopDate.Assign(GDMDate.CreateByFormattedStr(_txtStopDate.NormalizeDate, true));
                fResearchRecord.Percent = (int)_nudPercent.Value;

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
                _txtName.Text = "";
                _cmbPriority.SelectedIndex = -1;
                _cmbStatus.SelectedIndex = -1;
                _txtStartDate.Text = "";
                _txtStopDate.Text = "";
                _nudPercent.Value = 0;
            } else {
                _txtName.Text = fResearchRecord.ResearchName;
                _cmbPriority.SelectedIndex = (int)fResearchRecord.Priority;
                _cmbStatus.SelectedIndex = (int)fResearchRecord.Status;
                _txtStartDate.NormalizeDate = fResearchRecord.StartDate.GetDisplayString(DateFormat.dfDD_MM_YYYY);
                _txtStopDate.NormalizeDate = fResearchRecord.StopDate.GetDisplayString(DateFormat.dfDD_MM_YYYY);
                _nudPercent.Value = fResearchRecord.Percent;
            }

            _fNotesList.ListModel.DataOwner = fResearchRecord;
            _fTasksList.ListModel.DataOwner = fResearchRecord;
            _fCommunicationsList.ListModel.DataOwner = fResearchRecord;
            _fGroupsList.ListModel.DataOwner = fResearchRecord;
        }

        public override void SetLocale()
        {
            fView.SetTitle(LangMan.LS(LSID.WinResearchEdit));

            _btnAccept.Text = LangMan.LS(LSID.DlgAccept);
            _btnCancel.Text = LangMan.LS(LSID.DlgCancel);
            _pageTasks.Text = LangMan.LS(LSID.RPTasks);
            _pageCommunications.Text = LangMan.LS(LSID.RPCommunications);
            _pageGroups.Text = LangMan.LS(LSID.RPGroups);
            _pageNotes.Text = LangMan.LS(LSID.RPNotes);
            _lblName.Text = LangMan.LS(LSID.Title);
            _lblPriority.Text = LangMan.LS(LSID.Priority);
            _lblStatus.Text = LangMan.LS(LSID.Status);
            _lblPercent.Text = LangMan.LS(LSID.Percent);
            _lblStartDate.Text = LangMan.LS(LSID.StartDate);
            _lblStopDate.Text = LangMan.LS(LSID.StopDate);
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            _btnAccept.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Accept);
            _btnCancel.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Cancel);
        }
    }
}
