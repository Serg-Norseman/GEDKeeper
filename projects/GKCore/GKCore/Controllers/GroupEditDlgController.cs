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
    public sealed class GroupEditDlgController : DialogController<IGroupEditDlg>
    {
        #region Design components
#pragma warning disable CS0649, IDE0044
        private ITextBox _edName;
        private ILabel _lblName;
        private ITabPage _pageNotes;
        private ITabPage _pageMultimedia;
        private ITabPage _pageMembers;
        private IButton _btnAccept;
        private IButton _btnCancel;
        private ISheetList _fMembersList;
        private ISheetList _fNotesList;
        private ISheetList _fMediaList;
#pragma warning restore CS0649, IDE0044
        #endregion

        private GDMGroupRecord fGroupRecord;

        public GDMGroupRecord GroupRecord
        {
            get { return fGroupRecord; }
            set {
                if (fGroupRecord != value) {
                    fGroupRecord = value;
                    UpdateView();
                }
            }
        }


        public GroupEditDlgController(IGroupEditDlg view) : base(view)
        {
        }

        public override void InitView(IBaseWindow baseWin)
        {
            base.InitView(baseWin);

            _fMembersList.OnModify += ModifyMembersSheet;

            _edName.Activate();

            _fMembersList.ListModel = new GroupMembersListModel(fView, baseWin, fLocalUndoman);
            _fNotesList.ListModel = new NoteLinksListModel(fView, baseWin, fLocalUndoman);
            _fMediaList.ListModel = new MediaLinksListModel(fView, baseWin, fLocalUndoman);
        }

        public override void DoneView()
        {
            _fMembersList.ListModel.SaveSettings();
            _fNotesList.ListModel.SaveSettings();
            _fMediaList.ListModel.SaveSettings();
        }

        private void ModifyMembersSheet(object sender, ModifyEventArgs eArgs)
        {
            if (eArgs.Action == RecordAction.raJump) {
                JumpToRecord(eArgs.ItemData as GDMIndividualLink);
            }
        }

        public override bool Accept()
        {
            try {
                fGroupRecord.GroupName = _edName.Text;

                fBase.NotifyRecord(fGroupRecord, RecordAction.raEdit);

                CommitChanges();

                return true;
            } catch (Exception ex) {
                Logger.WriteError("GroupEditDlgController.Accept()", ex);
                return false;
            }
        }

        public override void UpdateView()
        {
            _edName.Text = (fGroupRecord == null) ? "" : fGroupRecord.GroupName;

            _fMembersList.ListModel.DataOwner = fGroupRecord;
            _fNotesList.ListModel.DataOwner = fGroupRecord;
            _fMediaList.ListModel.DataOwner = fGroupRecord;
        }

        public override void SetLocale()
        {
            fView.SetTitle(LangMan.LS(LSID.WinGroupEdit));
            _btnAccept.Text = LangMan.LS(LSID.DlgAccept);
            _btnCancel.Text = LangMan.LS(LSID.DlgCancel);
            _lblName.Text = LangMan.LS(LSID.Title);
            _pageMembers.Text = LangMan.LS(LSID.Members);
            _pageNotes.Text = LangMan.LS(LSID.RPNotes);
            _pageMultimedia.Text = LangMan.LS(LSID.RPMultimedia);
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            _btnAccept.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Accept);
            _btnCancel.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Cancel);
        }
    }
}
