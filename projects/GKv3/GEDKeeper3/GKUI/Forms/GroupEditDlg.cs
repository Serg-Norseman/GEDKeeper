/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
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
