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
            for (GDMRestriction res = GDMRestriction.rnNone; res <= GDMRestriction.rnLast; res++) {
                fView.Restriction.Add(LangMan.LS(GKData.Restrictions[(int)res]));
            }

            for (int i = 0; i < GKData.MarriageStatus.Length; i++) {
                fView.MarriageStatus.Add(LangMan.LS(GKData.MarriageStatus[i].Name));
            }

            fView.ChildrenList.OnModify += ModifyChildrenSheet;
            fView.ChildrenList.OnItemValidating += FamilyEditDlg_ItemValidating;
        }

        public override void Init(IBaseWindow baseWin)
        {
            base.Init(baseWin);

            fView.ChildrenList.ListModel = new FamilyChildrenListModel(fView, baseWin, fLocalUndoman);
            fView.EventsList.ListModel = new EventsListModel(fView, baseWin, fLocalUndoman);
            fView.NotesList.ListModel = new NoteLinksListModel(fView, baseWin, fLocalUndoman);
            fView.MediaList.ListModel = new MediaLinksListModel(fView, baseWin, fLocalUndoman);
            fView.SourcesList.ListModel = new SourceCitationsListModel(fView, baseWin, fLocalUndoman);
            fView.UserRefList.ListModel = new URefsListModel(fView, baseWin, fLocalUndoman);
        }

        public override void Done()
        {
            fView.ChildrenList.ListModel.SaveSettings();
            fView.EventsList.ListModel.SaveSettings();
            fView.NotesList.ListModel.SaveSettings();
            fView.MediaList.ListModel.SaveSettings();
            fView.SourcesList.ListModel.SaveSettings();
            fView.UserRefList.ListModel.SaveSettings();
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
                fFamilyRecord.Status = (GDMMarriageStatus)fView.MarriageStatus.SelectedIndex;
                fFamilyRecord.Restriction = (GDMRestriction)fView.Restriction.SelectedIndex;

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

                fView.ChildrenList.ListModel.DataOwner = fFamilyRecord;
                fView.EventsList.ListModel.DataOwner = fFamilyRecord;
                fView.NotesList.ListModel.DataOwner = fFamilyRecord;
                fView.MediaList.ListModel.DataOwner = fFamilyRecord;
                fView.SourcesList.ListModel.DataOwner = fFamilyRecord;
                fView.UserRefList.ListModel.DataOwner = fFamilyRecord;

                if (fFamilyRecord == null) {
                    fView.MarriageStatus.Enabled = false;
                    fView.MarriageStatus.SelectedIndex = 0;
                    fView.Restriction.SelectedIndex = 0;
                } else {
                    fView.MarriageStatus.Enabled = true && !disNoStd;
                    fView.MarriageStatus.SelectedIndex = (int)fFamilyRecord.Status;
                    fView.Restriction.SelectedIndex = (sbyte)fFamilyRecord.Restriction;
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
            GetControl<IButton>("btnHusbandAdd").Enabled = !locked;
            GetControl<IButton>("btnHusbandDelete").Enabled = !locked;
            GetControl<IButton>("btnWifeAdd").Enabled = !locked;
            GetControl<IButton>("btnWifeDelete").Enabled = !locked;

            fView.MarriageStatus.Enabled = !locked;

            fView.ChildrenList.ReadOnly = locked;
            fView.EventsList.ReadOnly = locked;
            fView.NotesList.ReadOnly = locked;
            fView.MediaList.ReadOnly = locked;
            fView.SourcesList.ReadOnly = locked;
            fView.UserRefList.ReadOnly = locked;
        }

        public void SetHusband(string value)
        {
            bool res = !string.IsNullOrEmpty(value);
            fView.Husband.Text = (res) ? value : LangMan.LS(LSID.UnkMale);

            GetControl<IButton>("btnHusbandAdd").Enabled = !res;
            GetControl<IButton>("btnHusbandDelete").Enabled = res;
            GetControl<IButton>("btnHusbandSel").Enabled = res;
        }

        public void SetWife(string value)
        {
            bool res = !string.IsNullOrEmpty(value);
            fView.Wife.Text = (res) ? value : LangMan.LS(LSID.UnkFemale);

            GetControl<IButton>("btnWifeAdd").Enabled = !res;
            GetControl<IButton>("btnWifeDelete").Enabled = res;
            GetControl<IButton>("btnWifeSel").Enabled = res;
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
            GetControl<IButton>("btnAccept").Text = LangMan.LS(LSID.DlgAccept);
            GetControl<IButton>("btnCancel").Text = LangMan.LS(LSID.DlgCancel);
            GetControl<ILabel>("lblHusband").Text = LangMan.LS(LSID.Husband);
            GetControl<ILabel>("lblWife").Text = LangMan.LS(LSID.Wife);
            GetControl<ILabel>("lblStatus").Text = LangMan.LS(LSID.Status);
            GetControl<ITabPage>("pageChilds").Text = LangMan.LS(LSID.Childs);
            GetControl<ITabPage>("pageEvents").Text = LangMan.LS(LSID.Events);
            GetControl<ITabPage>("pageNotes").Text = LangMan.LS(LSID.RPNotes);
            GetControl<ITabPage>("pageMultimedia").Text = LangMan.LS(LSID.RPMultimedia);
            GetControl<ITabPage>("pageSources").Text = LangMan.LS(LSID.RPSources);
            GetControl<ITabPage>("pageUserRefs").Text = LangMan.LS(LSID.UserRefs);
            GetControl<ILabel>("lblRestriction").Text = LangMan.LS(LSID.Restriction);

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

            GetControl<IButton>("btnAccept").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Accept);
            GetControl<IButton>("btnCancel").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Cancel);

            GetControl<IButton>("btnHusbandAdd").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Attach, true);
            GetControl<IButton>("btnHusbandDelete").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Detach, true);
            GetControl<IButton>("btnHusbandSel").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_LinkJump, true);

            GetControl<IButton>("btnWifeAdd").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Attach, true);
            GetControl<IButton>("btnWifeDelete").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Detach, true);
            GetControl<IButton>("btnWifeSel").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_LinkJump, true);
        }
    }
}
