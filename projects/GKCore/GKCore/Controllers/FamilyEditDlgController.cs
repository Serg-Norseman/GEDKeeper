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
using GKCore.Operations;
using GKCore.Options;
using GKUI.Themes;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class FamilyEditDlgController : DialogController<IFamilyEditDlg>
    {
        #region Design components
#pragma warning disable CS0649, IDE0044
        private ITabControl _tabsData;
        private ITabPage _pageEvents;
        private ITabPage _pageNotes;
        private ITabPage _pageMultimedia;
        private ITabPage _pageSources;
        private ITabPage _pageChilds;
        private ITabPage _pageUserRefs;
        private IButton _btnAccept;
        private IButton _btnCancel;
        private IGroupBox _GroupBox1;
        private ILabel _lblHusband;
        private ITextBox _txtHusband;
        private IButton _btnHusbandAdd;
        private IButton _btnHusbandDelete;
        private IButton _btnHusbandSel;
        private IButton _btnWifeSel;
        private IButton _btnWifeDelete;
        private IButton _btnWifeAdd;
        private ITextBox _txtWife;
        private ILabel _lblWife;
        private ILabel _lblStatus;
        private IComboBox _cmbMarriageStatus;
        private ILabel _lblRestriction;
        private IComboBox _cmbRestriction;
        private ISheetList _fChildrenList;
        private ISheetList _fEventsList;
        private ISheetList _fNotesList;
        private ISheetList _fMediaList;
        private ISheetList _fSourcesList;
        private ISheetList _fUserRefList;
#pragma warning restore CS0649, IDE0044
        #endregion

        private GDMFamilyRecord fFamilyRecord;

        public GDMFamilyRecord FamilyRecord
        {
            get { return fFamilyRecord; }
            set {
                if (fFamilyRecord != value) {
                    fFamilyRecord = value;
                    UpdateView();
                }
            }
        }


        public FamilyEditDlgController(IFamilyEditDlg view) : base(view)
        {
        }

        public override void InitView(IBaseWindow baseWin)
        {
            base.InitView(baseWin);

            _cmbRestriction.ReadOnly = true;
            for (GDMRestriction res = GDMRestriction.rnNone; res <= GDMRestriction.rnLast; res++) {
                _cmbRestriction.Add(LangMan.LS(GKData.Restrictions[(int)res]));
            }

            _cmbMarriageStatus.ReadOnly = true;
            for (int i = 0; i < GKData.MarriageStatus.Length; i++) {
                _cmbMarriageStatus.Add(LangMan.LS(GKData.MarriageStatus[i].Name));
            }

            _fChildrenList.OnModify += ModifyChildrenSheet;
            _fChildrenList.OnItemValidating += FamilyEditDlg_ItemValidating;

            _fChildrenList.ListModel = new FamilyChildrenListModel(fView, baseWin, fLocalUndoman);
            _fEventsList.ListModel = new EventsListModel(fView, baseWin, fLocalUndoman);
            _fNotesList.ListModel = new NoteLinksListModel(fView, baseWin, fLocalUndoman);
            _fMediaList.ListModel = new MediaLinksListModel(fView, baseWin, fLocalUndoman);
            _fSourcesList.ListModel = new SourceCitationsListModel(fView, baseWin, fLocalUndoman);
            _fUserRefList.ListModel = new URefsListModel(fView, baseWin, fLocalUndoman);
        }

        public override void DoneView()
        {
            _fChildrenList.ListModel.SaveSettings();
            _fEventsList.ListModel.SaveSettings();
            _fNotesList.ListModel.SaveSettings();
            _fMediaList.ListModel.SaveSettings();
            _fSourcesList.ListModel.SaveSettings();
            _fUserRefList.ListModel.SaveSettings();
        }

        private void ModifyChildrenSheet(object sender, ModifyEventArgs eArgs)
        {
            if (eArgs.Action == RecordAction.raJump) {
                JumpToRecord(eArgs.ItemData as GDMChildLink);
            }
        }

        private void FamilyEditDlg_ItemValidating(object sender, ItemValidatingEventArgs e)
        {
            var record = e.Item as GDMRecord;
            e.IsAvailable = record == null || fBase.Context.IsAvailableRecord(record);
        }

        public void SetTarget(TargetMode targetType, GDMIndividualRecord target)
        {
            if (targetType == TargetMode.tmNone || target == null) return;

            bool result = false;
            switch (targetType) {
                case TargetMode.tmSpouse:
                    result = fLocalUndoman.DoOrdinaryOperation(OperationType.otFamilySpouseAttach, fFamilyRecord, target);
                    break;
                case TargetMode.tmFamilyChild:
                    result = fLocalUndoman.DoOrdinaryOperation(OperationType.otIndividualParentsAttach, target, fFamilyRecord);
                    break;
            }

            if (result) UpdateControls();
        }

        public override bool Accept()
        {
            try {
                fFamilyRecord.Status = (GDMMarriageStatus)_cmbMarriageStatus.SelectedIndex;
                fFamilyRecord.Restriction = (GDMRestriction)_cmbRestriction.SelectedIndex;

                fBase.Context.ProcessFamily(fFamilyRecord);

                fLocalUndoman.Commit();

                fBase.NotifyRecord(fFamilyRecord, RecordAction.raEdit);

                return true;
            } catch (Exception ex) {
                Logger.WriteError("FamilyEditDlgController.Accept()", ex);
                return false;
            }
        }

        public override void UpdateView()
        {
            try {
                bool disNoStd = GlobalOptions.Instance.DisableNonStdFeatures;

                _fChildrenList.ListModel.DataOwner = fFamilyRecord;
                _fEventsList.ListModel.DataOwner = fFamilyRecord;
                _fNotesList.ListModel.DataOwner = fFamilyRecord;
                _fMediaList.ListModel.DataOwner = fFamilyRecord;
                _fSourcesList.ListModel.DataOwner = fFamilyRecord;
                _fUserRefList.ListModel.DataOwner = fFamilyRecord;

                if (fFamilyRecord == null) {
                    _cmbMarriageStatus.Enabled = false;
                    _cmbMarriageStatus.SelectedIndex = 0;
                    _cmbRestriction.SelectedIndex = 0;
                } else {
                    _cmbMarriageStatus.Enabled = true && !disNoStd;
                    _cmbMarriageStatus.SelectedIndex = (int)fFamilyRecord.Status;
                    _cmbRestriction.SelectedIndex = (sbyte)fFamilyRecord.Restriction;
                }

                UpdateControls();
            } catch (Exception ex) {
                Logger.WriteError("FamilyEditDlgController.SetFamily()", ex);
            }
        }

        private void UpdateControls()
        {
            GDMIndividualRecord husband, wife;

            if (fFamilyRecord == null) {
                husband = null;
                wife = null;

                LockEditor(true);
            } else {
                fBase.Context.Tree.GetSpouses(fFamilyRecord, out husband, out wife);

                LockEditor(fFamilyRecord.Restriction == GDMRestriction.rnLocked);
            }

            SetHusband((husband != null) ? GKUtils.GetNameString(husband, false) : null);
            SetWife((wife != null) ? GKUtils.GetNameString(wife, false) : null);
        }

        public void LockEditor(bool locked)
        {
            _btnHusbandAdd.Enabled = !locked;
            _btnHusbandDelete.Enabled = !locked;
            _btnWifeAdd.Enabled = !locked;
            _btnWifeDelete.Enabled = !locked;

            _cmbMarriageStatus.Enabled = !locked;

            _fChildrenList.ReadOnly = locked;
            _fEventsList.ReadOnly = locked;
            _fNotesList.ReadOnly = locked;
            _fMediaList.ReadOnly = locked;
            _fSourcesList.ReadOnly = locked;
            _fUserRefList.ReadOnly = locked;
        }

        public void SetHusband(string value)
        {
            bool res = !string.IsNullOrEmpty(value);
            _txtHusband.Text = (res) ? value : LangMan.LS(LSID.UnkMale);

            _btnHusbandAdd.Enabled = !res;
            _btnHusbandDelete.Enabled = res;
            _btnHusbandSel.Enabled = res;
        }

        public void SetWife(string value)
        {
            bool res = !string.IsNullOrEmpty(value);
            _txtWife.Text = (res) ? value : LangMan.LS(LSID.UnkFemale);

            _btnWifeAdd.Enabled = !res;
            _btnWifeDelete.Enabled = res;
            _btnWifeSel.Enabled = res;
        }

        public async void AddHusband()
        {
            if (await BaseController.AddFamilyHusband(fView, fBase, fLocalUndoman, fFamilyRecord)) {
                UpdateControls();
            }
        }

        public async void DeleteHusband()
        {
            if (await BaseController.DeleteFamilyHusband(fBase, fLocalUndoman, fFamilyRecord)) {
                UpdateControls();
            }
        }

        public async void AddWife()
        {
            if (await BaseController.AddFamilyWife(fView, fBase, fLocalUndoman, fFamilyRecord)) {
                UpdateControls();
            }
        }

        public async void DeleteWife()
        {
            if (await BaseController.DeleteFamilyWife(fBase, fLocalUndoman, fFamilyRecord)) {
                UpdateControls();
            }
        }

        public void JumpToHusband()
        {
            JumpToRecord(fFamilyRecord.Husband);
        }

        public void JumpToWife()
        {
            JumpToRecord(fFamilyRecord.Wife);
        }

        public override void SetLocale()
        {
            _btnAccept.Text = LangMan.LS(LSID.DlgAccept);
            _btnCancel.Text = LangMan.LS(LSID.DlgCancel);
            _lblHusband.Text = LangMan.LS(LSID.Husband);
            _lblWife.Text = LangMan.LS(LSID.Wife);
            _lblStatus.Text = LangMan.LS(LSID.Status);
            _pageChilds.Text = LangMan.LS(LSID.Childs);
            _pageEvents.Text = LangMan.LS(LSID.Events);
            _pageNotes.Text = LangMan.LS(LSID.RPNotes);
            _pageMultimedia.Text = LangMan.LS(LSID.RPMultimedia);
            _pageSources.Text = LangMan.LS(LSID.RPSources);
            _pageUserRefs.Text = LangMan.LS(LSID.UserRefs);
            _lblRestriction.Text = LangMan.LS(LSID.Restriction);

            SetToolTip("btnHusbandAdd", LangMan.LS(LSID.HusbandAddTip));
            SetToolTip("btnHusbandDelete", LangMan.LS(LSID.HusbandDeleteTip));
            SetToolTip("btnHusbandSel", LangMan.LS(LSID.HusbandSelTip));
            SetToolTip("btnWifeAdd", LangMan.LS(LSID.WifeAddTip));
            SetToolTip("btnWifeDelete", LangMan.LS(LSID.WifeDeleteTip));
            SetToolTip("btnWifeSel", LangMan.LS(LSID.WifeSelTip));
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            _btnAccept.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Accept);
            _btnCancel.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Cancel);

            _btnHusbandAdd.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Attach, true);
            _btnHusbandDelete.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Detach, true);
            _btnHusbandSel.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_LinkJump, true);

            _btnWifeAdd.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Attach, true);
            _btnWifeDelete.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Detach, true);
            _btnWifeSel.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_LinkJump, true);
        }
    }
}
