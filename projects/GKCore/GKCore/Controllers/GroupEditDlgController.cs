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
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.MVP;
using GKCore.MVP.Views;
using GKCore.Types;

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
            fView.Name.Activate();
        }

        public override void Init(IBaseWindow baseWin)
        {
            base.Init(baseWin);

            fView.MembersList.ListModel = new GroupMembersListModel(baseWin, fLocalUndoman);
            fView.NotesList.ListModel = new NoteLinksListModel(baseWin, fLocalUndoman);
            fView.MediaList.ListModel = new MediaLinksListModel(baseWin, fLocalUndoman);
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
            fView.Title = LangMan.LS(LSID.LSID_WinGroupEdit);
            GetControl<IButton>("btnAccept").Text = LangMan.LS(LSID.LSID_DlgAccept);
            GetControl<IButton>("btnCancel").Text = LangMan.LS(LSID.LSID_DlgCancel);
            GetControl<ILabel>("lblName").Text = LangMan.LS(LSID.LSID_Title);
            GetControl<ITabPage>("pageMembers").Text = LangMan.LS(LSID.LSID_Members);
            GetControl<ITabPage>("pageNotes").Text = LangMan.LS(LSID.LSID_RPNotes);
            GetControl<ITabPage>("pageMultimedia").Text = LangMan.LS(LSID.LSID_RPMultimedia);
        }
    }
}
