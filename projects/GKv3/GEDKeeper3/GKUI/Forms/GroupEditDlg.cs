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

using Eto.Forms;
using Eto.Serialization.Xaml;
using GDModel;
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Lists;
using GKUI.Components;

namespace GKUI.Forms
{
    public sealed partial class GroupEditDlg : CommonDialog<IGroupEditDlg, GroupEditDlgController>, IGroupEditDlg
    {
        #region Design components
#pragma warning disable CS0169, CS0649, IDE0044, IDE0051

        private TextBox edName;
        private Label lblName;
        private TabPage pageNotes;
        private TabPage pageMultimedia;
        private TabPage pageMembers;
        private Button btnAccept;
        private Button btnCancel;
        private GKSheetList fMembersList;
        private GKSheetList fNotesList;
        private GKSheetList fMediaList;

#pragma warning disable CS0169, CS0649, IDE0044, IDE0051
        #endregion

        public GDMGroupRecord GroupRecord
        {
            get { return fController.GroupRecord; }
            set { fController.GroupRecord = value; }
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
        }
    }
}
