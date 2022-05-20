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
using System.ComponentModel;
using BSLib.Design.MVP.Controls;
using Eto.Forms;
using Eto.Serialization.Xaml;
using GDModel;
using GKCore.Controllers;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.MVP.Views;
using GKCore.Types;
using GKUI.Components;

namespace GKUI.Forms
{
    public sealed partial class GroupEditDlg : EditorDialog, IGroupEditDlg
    {
        #region Design components
#pragma warning disable CS0169

        private TextBox edName;
        private Label lblName;
        private TabPage pageNotes;
        private TabPage pageMultimedia;
        private TabPage pageMembers;
        private Button btnAccept;
        private Button btnCancel;

#pragma warning disable CS0169
        #endregion

        private readonly GroupEditDlgController fController;

        private readonly GKSheetList fMembersList;
        private readonly GKSheetList fNotesList;
        private readonly GKSheetList fMediaList;

        public GDMGroupRecord Group
        {
            get { return fController.Group; }
            set { fController.Group = value; }
        }

        #region View Interface

        ISheetList IGroupEditDlg.NotesList
        {
            get { return fNotesList; }
        }

        ISheetList IGroupEditDlg.MediaList
        {
            get { return fMediaList; }
        }

        ISheetList IGroupEditDlg.MembersList
        {
            get { return fMembersList; }
        }

        ITextBox IGroupEditDlg.Name
        {
            get { return GetControlHandler<ITextBox>(edName); }
        }

        #endregion

        public GroupEditDlg(IBaseWindow baseWin)
        {
            XamlReader.Load(this);

            fController = new GroupEditDlgController(this);
            fController.Init(baseWin);

            fMembersList.ListModel = new GroupMembersSublistModel(baseWin, fController.LocalUndoman);
            fNotesList.ListModel = new NoteLinksListModel(baseWin, fController.LocalUndoman);
            fMediaList.ListModel = new MediaLinksListModel(baseWin, fController.LocalUndoman);
        }

        private void ModifyMembersSheet(object sender, ModifyEventArgs eArgs)
        {
            if (eArgs.Action == RecordAction.raJump) {
                fController.JumpToRecord(eArgs.ItemData as GDMIndividualRecord);
            }
        }

        private void btnAccept_Click(object sender, EventArgs e)
        {
            DialogResult = fController.Accept() ? DialogResult.Ok : DialogResult.None;
        }

        private void btnCancel_Click(object sender, EventArgs e)
        {
            DialogResult = fController.Cancel() ? DialogResult.Cancel : DialogResult.None;
        }

        protected override void OnClosing(CancelEventArgs e)
        {
            base.OnClosing(e);
            e.Cancel = fController.CheckChangesPersistence();
        }
    }
}
