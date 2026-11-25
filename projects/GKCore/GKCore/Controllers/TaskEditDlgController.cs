/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using GDModel;
using GDModel.Providers.GEDCOM;
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
    public sealed class TaskEditDlgController : DialogController<ITaskEditDlg>
    {
        #region Design components
#pragma warning disable CS0649, IDE0044
        private ITabPage _pageNotes;
        private IButton _btnAccept;
        private IButton _btnCancel;
        private ILabel _lblGoal;
        private IComboBox _cmbGoalType;
        private ITextBox _txtGoal;
        private IButton _btnGoalSelect;
        private ILabel _lblPriority;
        private IComboBox _txtPriority;
        private ILabel _lblStartDate;
        private IDateBox _txtStartDate;
        private IDateBox _txtStopDate;
        private ILabel _lblStopDate;
        private ISheetList _fNotesList;
#pragma warning restore CS0649, IDE0044
        #endregion

        private GDMTaskRecord fTaskRecord;
        private GDMRecord fTempRec;

        public GDMTaskRecord TaskRecord
        {
            get { return fTaskRecord; }
            set {
                if (fTaskRecord != value) {
                    fTaskRecord = value;
                    UpdateView();
                }
            }
        }


        public TaskEditDlgController(ITaskEditDlg view) : base(view)
        {
            fTempRec = null;
        }

        public override void InitView(IBaseWindow baseWin)
        {
            base.InitView(baseWin);

            _txtPriority.ReadOnly = true;
            for (GDMResearchPriority rp = GDMResearchPriority.rpNone; rp <= GDMResearchPriority.rpTop; rp++) {
                _txtPriority.Add(LangMan.LS(GKData.PriorityNames[(int)rp]));
            }

            _cmbGoalType.ReadOnly = true;
            for (GDMGoalType gt = GDMGoalType.gtIndividual; gt <= GDMGoalType.gtOther; gt++) {
                _cmbGoalType.Add(LangMan.LS(GKData.GoalNames[(int)gt]));
            }

            _fNotesList.ListModel = new NoteLinksListModel(fView, baseWin, fLocalUndoman);
        }

        public override void DoneView()
        {
            _fNotesList.ListModel.SaveSettings();
        }

        public override bool Accept()
        {
            try {
                GDMGoalType gt = (GDMGoalType)_cmbGoalType.SelectedIndex;
                switch (gt) {
                    case GDMGoalType.gtIndividual:
                    case GDMGoalType.gtFamily:
                    case GDMGoalType.gtSource:
                        if (fTempRec == null) {
                            AppHost.StdDialogs.ShowError(LangMan.LS(LSID.NoGoalRecordSpecified));
                            return false;
                        } else {
                            fTaskRecord.Goal = GEDCOMUtils.EncloseXRef(fTempRec.XRef);
                        }
                        break;

                    case GDMGoalType.gtOther:
                        fTaskRecord.Goal = _txtGoal.Text;
                        break;
                }

                fTaskRecord.Priority = (GDMResearchPriority)_txtPriority.SelectedIndex;
                fTaskRecord.StartDate.Assign(GDMDate.CreateByFormattedStr(_txtStartDate.NormalizeDate, true));
                fTaskRecord.StopDate.Assign(GDMDate.CreateByFormattedStr(_txtStopDate.NormalizeDate, true));

                fLocalUndoman.Commit();

                fBase.NotifyRecord(fTaskRecord, RecordAction.raEdit);

                return true;
            } catch (Exception ex) {
                Logger.WriteError("TaskEditDlgController.Accept()", ex);
                return false;
            }
        }

        public override void UpdateView()
        {
            if (fTaskRecord == null) {
                _txtPriority.SelectedIndex = -1;
                _txtStartDate.Text = "";
                _txtStopDate.Text = "";
                _cmbGoalType.SelectedIndex = 0;
                _txtGoal.Text = "";
            } else {
                _txtPriority.SelectedIndex = (sbyte)fTaskRecord.Priority;
                _txtStartDate.NormalizeDate = fTaskRecord.StartDate.GetDisplayString(DateFormat.dfDD_MM_YYYY);
                _txtStopDate.NormalizeDate = fTaskRecord.StopDate.GetDisplayString(DateFormat.dfDD_MM_YYYY);

                var goal = GKUtils.GetTaskGoal(fBase.Context.Tree, fTaskRecord);
                fTempRec = goal.GoalRec;
                _cmbGoalType.SelectedIndex = (sbyte)goal.GoalType;

                switch (goal.GoalType) {
                    case GDMGoalType.gtIndividual:
                    case GDMGoalType.gtFamily:
                    case GDMGoalType.gtSource:
                        _txtGoal.Text = GKUtils.GetGoalStr(fBase.Context.Tree, goal.GoalType, fTempRec);
                        break;

                    case GDMGoalType.gtOther:
                        _txtGoal.Text = fTaskRecord.Goal;
                        break;
                }
            }

            _fNotesList.ListModel.DataOwner = fTaskRecord;

            ChangeGoalType();
        }

        public async void SelectGoal()
        {
            GDMGoalType gt = (GDMGoalType)_cmbGoalType.SelectedIndex;
            switch (gt) {
                case GDMGoalType.gtIndividual:
                    fTempRec = await BaseController.SelectPerson(fView, fBase, null, TargetMode.tmNone, GDMSex.svUnknown);
                    _txtGoal.Text = GKUtils.GetGoalStr(fBase.Context.Tree, gt, fTempRec);
                    break;

                case GDMGoalType.gtFamily:
                    fTempRec = await BaseController.SelectRecord(fView, fBase, GDMRecordType.rtFamily, Array.Empty<object>());
                    _txtGoal.Text = GKUtils.GetGoalStr(fBase.Context.Tree, gt, fTempRec);
                    break;

                case GDMGoalType.gtSource:
                    fTempRec = await BaseController.SelectRecord(fView, fBase, GDMRecordType.rtSource, Array.Empty<object>());
                    _txtGoal.Text = GKUtils.GetGoalStr(fBase.Context.Tree, gt, fTempRec);
                    break;

                case GDMGoalType.gtOther:
                    break;
            }
        }

        public void ChangeGoalType()
        {
            GDMGoalType gt = (GDMGoalType)_cmbGoalType.SelectedIndex;
            switch (gt) {
                case GDMGoalType.gtIndividual:
                case GDMGoalType.gtFamily:
                case GDMGoalType.gtSource:
                    _btnGoalSelect.Enabled = true;
                    _txtGoal.ReadOnly = true;
                    break;

                case GDMGoalType.gtOther:
                    _btnGoalSelect.Enabled = false;
                    _txtGoal.ReadOnly = false;
                    break;
            }
        }

        public override void SetLocale()
        {
            fView.SetTitle(LangMan.LS(LSID.WinTaskEdit));

            _btnAccept.Text = LangMan.LS(LSID.DlgAccept);
            _btnCancel.Text = LangMan.LS(LSID.DlgCancel);
            _pageNotes.Text = LangMan.LS(LSID.RPNotes);
            _lblGoal.Text = LangMan.LS(LSID.Goal);
            _lblPriority.Text = LangMan.LS(LSID.Priority);
            _lblStartDate.Text = LangMan.LS(LSID.StartDate);
            _lblStopDate.Text = LangMan.LS(LSID.StopDate);

            SetToolTip("btnGoalSelect", LangMan.LS(LSID.GoalSelectTip));
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            _btnAccept.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Accept);
            _btnCancel.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Cancel);

            _btnGoalSelect.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Attach, true);
        }
    }
}
