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
            fView.MembersList.OnModify += ModifyMembersSheet;

            fView.Name.Activate();
        }

        public override void Init(IBaseWindow baseWin)
        {
            base.Init(baseWin);

            fView.MembersList.ListModel = new GroupMembersListModel(fView, baseWin, fLocalUndoman);
            fView.NotesList.ListModel = new NoteLinksListModel(fView, baseWin, fLocalUndoman);
            fView.MediaList.ListModel = new MediaLinksListModel(fView, baseWin, fLocalUndoman);
        }

        public override void Done()
        {
            fView.MembersList.ListModel.SaveSettings();
            fView.NotesList.ListModel.SaveSettings();
            fView.MediaList.ListModel.SaveSettings();
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
                fGroupRecord.GroupName = fView.Name.Text;

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
            fView.Name.Text = (fGroupRecord == null) ? "" : fGroupRecord.GroupName;

            fView.MembersList.ListModel.DataOwner = fGroupRecord;
            fView.NotesList.ListModel.DataOwner = fGroupRecord;
            fView.MediaList.ListModel.DataOwner = fGroupRecord;
        }

        public override void SetLocale()
        {
            fView.SetTitle(LangMan.LS(LSID.WinGroupEdit));
            GetControl<IButton>("btnAccept").Text = LangMan.LS(LSID.DlgAccept);
            GetControl<IButton>("btnCancel").Text = LangMan.LS(LSID.DlgCancel);
            GetControl<ILabel>("lblName").Text = LangMan.LS(LSID.Title);
            GetControl<ITabPage>("pageMembers").Text = LangMan.LS(LSID.Members);
            GetControl<ITabPage>("pageNotes").Text = LangMan.LS(LSID.RPNotes);
            GetControl<ITabPage>("pageMultimedia").Text = LangMan.LS(LSID.RPMultimedia);
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            GetControl<IButton>("btnAccept").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Accept);
            GetControl<IButton>("btnCancel").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Cancel);
        }
    }
}
