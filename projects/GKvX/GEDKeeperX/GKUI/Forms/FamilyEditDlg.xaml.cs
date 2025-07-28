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
using GKCore;
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Lists;
using GKCore.Types;

namespace GKUI.Forms
{
    public partial class FamilyEditDlg : CommonDialog<IFamilyEditDlg, FamilyEditDlgController>, IFamilyEditDlg
    {
        public GDMFamilyRecord FamilyRecord
        {
            get { return fController.FamilyRecord; }
            set { fController.FamilyRecord = value; }
        }

        #region View Interface

        ISheetList IFamilyEditDlg.NotesList
        {
            get { return fNotesList; }
        }

        ISheetList IFamilyEditDlg.MediaList
        {
            get { return fMediaList; }
        }

        ISheetList IFamilyEditDlg.SourcesList
        {
            get { return fSourcesList; }
        }

        ISheetList IFamilyEditDlg.ChildrenList
        {
            get { return fChildrenList; }
        }

        ISheetList IFamilyEditDlg.EventsList
        {
            get { return fEventsList; }
        }

        ISheetList IFamilyEditDlg.UserRefList
        {
            get { return fUserRefList; }
        }

        IComboBox IFamilyEditDlg.MarriageStatus
        {
            get { return GetControlHandler<IComboBox>(cmbMarriageStatus); }
        }

        IComboBox IFamilyEditDlg.Restriction
        {
            get { return GetControlHandler<IComboBox>(cmbRestriction); }
        }

        ITextBox IFamilyEditDlg.Husband
        {
            get { return GetControlHandler<ITextBox>(txtHusband); }
        }

        ITextBox IFamilyEditDlg.Wife
        {
            get { return GetControlHandler<ITextBox>(txtWife); }
        }

        #endregion

        public FamilyEditDlg(IBaseWindow baseWin)
        {
            InitializeComponent();

            fController = new FamilyEditDlgController(this);
            fController.Init(baseWin);
        }

        private void cbRestriction_SelectedIndexChanged(object sender, EventArgs e)
        {
            fController.LockEditor(cmbRestriction.SelectedIndex == (int)GDMRestriction.rnLocked);
        }

        public void SetTarget(TargetMode targetType, GDMIndividualRecord target)
        {
            fController.SetTarget(targetType, target);
        }

        private void btnHusbandAddClick(object sender, EventArgs e)
        {
            fController.AddHusband();
        }

        private void btnHusbandDeleteClick(object sender, EventArgs e)
        {
            fController.DeleteHusband();
        }

        private void btnHusbandSelClick(object sender, EventArgs e)
        {
            fController.JumpToHusband();
        }

        private void btnWifeAddClick(object sender, EventArgs e)
        {
            fController.AddWife();
        }

        private void btnWifeDeleteClick(object sender, EventArgs e)
        {
            fController.DeleteWife();
        }

        private void btnWifeSelClick(object sender, EventArgs e)
        {
            fController.JumpToWife();
        }

        private void EditSpouse_TextChanged(object sender, EventArgs e)
        {
            Title = string.Format("{0} \"{1} - {2}\"", LangMan.LS(LSID.Family), txtHusband.Text, txtWife.Text);
        }
    }
}
