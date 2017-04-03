/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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
using System.Drawing;
using System.Windows.Forms;

using GKCommon;
using GKCommon.Controls;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.Operations;
using GKCore.Options;
using GKCore.Types;
using GKUI.Engine;
using GKUI.Sheets;

namespace GKUI.Dialogs
{
    /// <summary>
    /// 
    /// </summary>
    public partial class PersonEditDlg : EditorDialog
    {
        private readonly GKSheetList fEventsList;
        private readonly GKSheetList fSpousesList;
        private readonly GKSheetList fAssociationsList;
        private readonly GKSheetList fGroupsList;
        private readonly GKSheetList fNotesList;
        private readonly GKSheetList fMediaList;
        private readonly GKSheetList fSourcesList;
        private readonly GKSheetList fUserRefList;
        private readonly GKSheetList fNamesList;

        private GEDCOMIndividualRecord fPerson;
        private GEDCOMIndividualRecord fTarget;
        private TargetMode fTargetMode;
        private Image fPortraitImg;

        public GEDCOMIndividualRecord Person
        {
            get { return fPerson; }
            set { SetPerson(value); }
        }

        public GEDCOMIndividualRecord Target
        {
            get { return fTarget; }
            set { SetTarget(value); }
        }

        public TargetMode TargetMode
        {
            get { return fTargetMode; }
            set { fTargetMode = value; }
        }


        private void SetPerson(GEDCOMIndividualRecord value)
        {
            fPerson = value;

            try
            {
                string fam, nam, pat;
                GKUtils.GetNameParts(fPerson, out fam, out nam, out pat, false);
                txtSurname.Text = fam;
                txtName.Text = nam;
                cmbPatronymic.Text = pat;

                cmbSex.SelectedIndex = (sbyte)fPerson.Sex;
                chkPatriarch.Checked = fPerson.Patriarch;
                chkBookmark.Checked = fPerson.Bookmark;

                cmbRestriction.SelectedIndexChanged -= cbRestriction_SelectedIndexChanged;
                cmbRestriction.SelectedIndex = (sbyte)fPerson.Restriction;
                cmbRestriction.SelectedIndexChanged += cbRestriction_SelectedIndexChanged;

                if (fPerson.PersonalNames.Count > 0)
                {
                    GEDCOMPersonalName np = fPerson.PersonalNames[0];
                    txtNamePrefix.Text = np.Pieces.Prefix;
                    txtNickname.Text = np.Pieces.Nickname;
                    txtSurnamePrefix.Text = np.Pieces.SurnamePrefix;
                    txtNameSuffix.Text = np.Pieces.Suffix;

                    txtMarriedSurname.Text = np.Pieces.MarriedName;
                }

                fEventsList.ListModel.DataOwner = fPerson;
                fNotesList.ListModel.DataOwner = fPerson;
                fMediaList.ListModel.DataOwner = fPerson;
                fSourcesList.ListModel.DataOwner = fPerson;

                UpdateControls(true);
            }
            catch (Exception ex)
            {
                fBase.Host.LogWrite("PersonEditDlg.SetPerson(): " + ex.Message);
            }
        }

        private void SetTarget(GEDCOMIndividualRecord value)
        {
            try
            {
                fTarget = value;

                if (fTarget != null)
                {
                    ICulture culture = fBase.Context.Culture;
                    INamesTable namesTable = MainWin.Instance.NamesTable;

                    string surname, name, patronymic;
                    GKUtils.GetNameParts(fTarget, out surname, out name, out patronymic);
                    txtSurname.Text = surname;
                    GEDCOMSex sx = (GEDCOMSex)cmbSex.SelectedIndex;

                    switch (fTargetMode) {
                        case TargetMode.tmParent:
                            if (sx == GEDCOMSex.svFemale) {
                                SetMarriedSurname(surname);
                            }
                            if (culture.HasPatronymic()) {
                                cmbPatronymic.Items.Add(namesTable.GetPatronymicByName(name, GEDCOMSex.svMale));
                                cmbPatronymic.Items.Add(namesTable.GetPatronymicByName(name, GEDCOMSex.svFemale));
                                cmbPatronymic.Text = namesTable.GetPatronymicByName(name, sx);
                            }
                            break;

                        case TargetMode.tmChild:
                            switch (sx) {
                                case GEDCOMSex.svMale:
                                    if (culture.HasPatronymic()) {
                                        txtName.Text = namesTable.GetNameByPatronymic(patronymic);
                                    }
                                    break;

                                case GEDCOMSex.svFemale:
                                    SetMarriedSurname(surname);
                                    break;
                            }
                            break;

                        case TargetMode.tmWife:
                            SetMarriedSurname(surname);
                            break;
                    }
                }
            }
            catch (Exception ex)
            {
                fBase.Host.LogWrite("PersonEditDlg.SetTarget("+fTargetMode.ToString()+"): " + ex.Message);
            }
        }

        private void SetMarriedSurname(string husbSurname)
        {
            string surname = fBase.Context.Culture.GetMarriedSurname(husbSurname);
            if (IsExtendedWomanSurname()) {
                txtMarriedSurname.Text = surname;
            } else {
                txtSurname.Text = '(' + surname + ')';
            }
        }

        private bool IsExtendedWomanSurname()
        {
            bool result = (GlobalOptions.Instance.WomanSurnameFormat != WomanSurnameFormat.wsfNotExtend) &&
                (cmbSex.SelectedIndex == (sbyte)GEDCOMSex.svFemale);
            return result;
        }

        private void cbSex_SelectedIndexChanged(object sender, EventArgs e)
        {
            if (!IsExtendedWomanSurname()) {
                lblSurname.Text = LangMan.LS(LSID.LSID_Surname);
                txtMarriedSurname.Enabled = false;
            } else {
                lblSurname.Text = LangMan.LS(LSID.LSID_MaidenSurname);
                txtMarriedSurname.Enabled = true;
            }

            UpdatePortrait(true);
        }

        private void UpdateControls(bool totalUpdate = false)
        {
            bool locked = (cmbRestriction.SelectedIndex == (int)GEDCOMRestriction.rnLocked);

            if (fPerson.ChildToFamilyLinks.Count != 0)
            {
                GEDCOMFamilyRecord family = fPerson.ChildToFamilyLinks[0].Family;
                btnParentsAdd.Enabled = false;
                btnParentsEdit.Enabled = true && !locked;
                btnParentsDelete.Enabled = true && !locked;

                GEDCOMIndividualRecord relPerson = family.GetHusband();
                if (relPerson != null)
                {
                    btnFatherAdd.Enabled = false;
                    btnFatherDelete.Enabled = true && !locked;
                    btnFatherSel.Enabled = true && !locked;
                    txtFather.Text = GKUtils.GetNameString(relPerson, true, false);
                }
                else
                {
                    btnFatherAdd.Enabled = true && !locked;
                    btnFatherDelete.Enabled = false;
                    btnFatherSel.Enabled = false;
                    txtFather.Text = "";
                }

                relPerson = family.GetWife();
                if (relPerson != null)
                {
                    btnMotherAdd.Enabled = false;
                    btnMotherDelete.Enabled = true && !locked;
                    btnMotherSel.Enabled = true && !locked;
                    txtMother.Text = GKUtils.GetNameString(relPerson, true, false);
                }
                else
                {
                    btnMotherAdd.Enabled = true && !locked;
                    btnMotherDelete.Enabled = false;
                    btnMotherSel.Enabled = false;
                    txtMother.Text = "";
                }
            }
            else
            {
                btnParentsAdd.Enabled = true && !locked;
                btnParentsEdit.Enabled = false;
                btnParentsDelete.Enabled = false;

                btnFatherAdd.Enabled = true && !locked;
                btnFatherDelete.Enabled = false;
                btnFatherSel.Enabled = false;

                btnMotherAdd.Enabled = true && !locked;
                btnMotherDelete.Enabled = false;
                btnMotherSel.Enabled = false;

                txtFather.Text = "";
                txtMother.Text = "";
            }

            if (totalUpdate) {
                fEventsList.UpdateSheet();
                fNotesList.UpdateSheet();
                fMediaList.UpdateSheet();
                fSourcesList.UpdateSheet();

                UpdateSpousesSheet();
                UpdateAssociationsSheet();
                UpdateGroupsSheet();
                UpdateURefsSheet();
                UpdateNamesSheet();
            }

            UpdatePortrait(totalUpdate);

            // controls lock
            txtName.Enabled = !locked;
            cmbPatronymic.Enabled = !locked;
            txtSurname.Enabled = !locked;

            cmbSex.Enabled = !locked;
            chkPatriarch.Enabled = !locked;
            chkBookmark.Enabled = !locked;

            txtNamePrefix.Enabled = !locked;
            txtNickname.Enabled = !locked;
            txtSurnamePrefix.Enabled = !locked;
            txtNameSuffix.Enabled = !locked;

            fEventsList.ReadOnly = locked;
            fNotesList.ReadOnly = locked;
            fMediaList.ReadOnly = locked;
            fSourcesList.ReadOnly = locked;
            fSpousesList.ReadOnly = locked;
            fAssociationsList.ReadOnly = locked;
            fGroupsList.ReadOnly = locked;
            fUserRefList.ReadOnly = locked;

            ICulture culture = fBase.Context.Culture;
            txtSurname.Enabled = txtSurname.Enabled && culture.HasSurname();
            cmbPatronymic.Enabled = cmbPatronymic.Enabled && culture.HasPatronymic();
        }

        private void UpdatePortrait(bool totalUpdate)
        {
            if (fPortraitImg == null || totalUpdate) {
                fPortraitImg = fBase.Context.GetPrimaryBitmap(fPerson, imgPortrait.Width, imgPortrait.Height, false);
            }

            Image img = fPortraitImg;
            if (img == null) {
                // using avatar's image
                GEDCOMSex curSex = (GEDCOMSex)cmbSex.SelectedIndex;

                switch (curSex) {
                    case GEDCOMSex.svMale:
                        img = GKResources.piMale140;
                        break;

                    case GEDCOMSex.svFemale:
                        img = GKResources.piFemale140;
                        break;

                    default:
                        break;
                }
            }
            imgPortrait.Image = img;

            bool locked = (cmbRestriction.SelectedIndex == (int)GEDCOMRestriction.rnLocked);
            btnPortraitAdd.Enabled = !locked;
            btnPortraitDelete.Enabled = fPortraitImg != null && !locked;
        }

        private void cbRestriction_SelectedIndexChanged(object sender, EventArgs e)
        {
            UpdateControls();
        }

        private void AcceptChanges()
        {
            GEDCOMPersonalName np = fPerson.PersonalNames[0];
            GKUtils.SetRusNameParts(np, txtSurname.Text, txtName.Text, cmbPatronymic.Text);

            GEDCOMPersonalNamePieces pieces = np.Pieces;
            pieces.Nickname = txtNickname.Text;
            pieces.Prefix = txtNamePrefix.Text;
            pieces.SurnamePrefix = txtSurnamePrefix.Text;
            pieces.Suffix = txtNameSuffix.Text;
            if (IsExtendedWomanSurname()) {
                pieces.MarriedName = txtMarriedSurname.Text;
            }

            fPerson.Sex = (GEDCOMSex)cmbSex.SelectedIndex;
            fPerson.Patriarch = chkPatriarch.Checked;
            fPerson.Bookmark = chkBookmark.Checked;
            fPerson.Restriction = (GEDCOMRestriction)cmbRestriction.SelectedIndex;

            if (fPerson.ChildToFamilyLinks.Count > 0)
            {
                fPerson.ChildToFamilyLinks[0].Family.SortChilds();
            }

            fLocalUndoman.Commit();

            fBase.ChangeRecord(fPerson);
        }

        private void btnAccept_Click(object sender, EventArgs e)
        {
            try
            {
                AcceptChanges();
                DialogResult = DialogResult.OK;
            }
            catch (Exception ex)
            {
                fBase.Host.LogWrite("PersonEditDlg.btnAccept_Click(): " + ex.Message);
                DialogResult = DialogResult.None;
            }
        }

        private void btnCancel_Click(object sender, EventArgs e)
        {
            try
            {
                fLocalUndoman.Rollback();
            }
            catch (Exception ex)
            {
                fBase.Host.LogWrite("PersonEditDlg.btnCancel_Click(): " + ex.Message);
            }
        }

        private void AcceptTempData()
        {
            // It is very important for some methods
            // For the sample: we need to have gender's value on time of call AddSpouse (for define husband/wife)
            // And we need to have actual name's value for visible it in FamilyEditDlg

            fLocalUndoman.DoOrdinaryOperation(OperationType.otIndividualSexChange, fPerson, (GEDCOMSex)cmbSex.SelectedIndex);
            fLocalUndoman.DoIndividualNameChange(fPerson, txtSurname.Text, txtName.Text, cmbPatronymic.Text);
        }

        private GKSheetList CreateAssociationsSheet(Control owner)
        {
            GKSheetList sheet = new GKSheetList(owner);

            sheet.AddColumn(LangMan.LS(LSID.LSID_Relation), 300, false);
            sheet.AddColumn(LangMan.LS(LSID.LSID_Person), 200, false);

            sheet.Buttons = EnumSet<SheetButton>.Create(SheetButton.lbAdd, SheetButton.lbEdit, SheetButton.lbDelete, SheetButton.lbJump);
            sheet.OnModify += ModifyAssociationsSheet;

            return sheet;
        }

        private void UpdateAssociationsSheet()
        {
            try
            {
                fAssociationsList.ClearItems();

                foreach (GEDCOMAssociation ast in fPerson.Associations) {
                    string nm = ((ast.Individual == null) ? "" : GKUtils.GetNameString(ast.Individual, true, false));

                    GKListItem item = fAssociationsList.AddItem(ast.Relation, ast);
                    item.AddSubItem(nm);
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("PersonEditDlg.UpdateAssociationsSheet(): " + ex.Message);
            }
        }

        private void ModifyAssociationsSheet(object sender, ModifyEventArgs eArgs)
        {
            bool result = false;

            GEDCOMAssociation ast = eArgs.ItemData as GEDCOMAssociation;

            switch (eArgs.Action)
            {
                case RecordAction.raAdd:
                case RecordAction.raEdit:
                    using (AssociationEditDlg fmAstEdit = new AssociationEditDlg(fBase))
                    {
                        bool exists = (ast != null);
                        if (!exists) {
                            ast = new GEDCOMAssociation(fBase.Tree, fPerson, "", "");
                        }

                        fmAstEdit.Association = ast;
                        result = (MainWin.Instance.ShowModalEx(fmAstEdit, false) == DialogResult.OK);

                        if (!exists) {
                            if (result) {
                                //this.fPerson.Associations.Add(ast);
                                result = fLocalUndoman.DoOrdinaryOperation(OperationType.otIndividualAssociationAdd, fPerson, ast);
                            } else {
                                ast.Dispose();
                            }
                        }
                    }
                    break;

                case RecordAction.raDelete:
                    if (AppHub.StdDialogs.ShowQuestionYN(LangMan.LS(LSID.LSID_RemoveAssociationQuery)) != false)
                    {
                        //this.fPerson.Associations.Delete(ast);
                        //result = true;
                        result = fLocalUndoman.DoOrdinaryOperation(OperationType.otIndividualAssociationRemove, fPerson, ast);
                        fBase.Modified = true;
                    }
                    break;

                case RecordAction.raJump:
                    if (ast != null) {
                        AcceptChanges();
                        fBase.SelectRecordByXRef(ast.Individual.XRef);
                        Close();
                    }
                    break;
            }

            if (result) UpdateAssociationsSheet();
        }

        private GKSheetList CreateURefsSheet(Control owner)
        {
            GKSheetList sheet = new GKSheetList(owner);

            sheet.AddColumn(LangMan.LS(LSID.LSID_Reference), 300, false);
            sheet.AddColumn(LangMan.LS(LSID.LSID_Type), 200, false);

            sheet.OnModify += ModifyURefsSheet;

            return sheet;
        }

        private void UpdateURefsSheet()
        {
            try
            {
                fUserRefList.ClearItems();

                foreach (GEDCOMUserReference uref in fPerson.UserReferences) {
                    GKListItem item = fUserRefList.AddItem(uref.StringValue, uref);
                    item.AddSubItem(uref.ReferenceType);
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("PersonEditDlg.UpdateURefsSheet(): " + ex.Message);
            }
        }

        private void ModifyURefsSheet(object sender, ModifyEventArgs eArgs)
        {
            bool result = false;

            GEDCOMUserReference userRef = eArgs.ItemData as GEDCOMUserReference;

            switch (eArgs.Action) {
                case RecordAction.raAdd:
                case RecordAction.raEdit:
                    using (UserRefEditDlg dlg = new UserRefEditDlg(fBase))
                    {
                        bool exists = (userRef != null);
                        if (!exists) {
                            userRef = new GEDCOMUserReference(fBase.Tree, fPerson, "", "");
                        }

                        dlg.UserRef = userRef;
                        result = (MainWin.Instance.ShowModalEx(dlg, false) == DialogResult.OK);

                        if (!exists) {
                            if (result) {
                                //this.fPerson.UserReferences.Add(userRef);
                                result = fLocalUndoman.DoOrdinaryOperation(OperationType.otIndividualURefAdd, fPerson, userRef);
                            } else {
                                userRef.Dispose();
                            }
                        }
                    }
                    break;

                case RecordAction.raDelete:
                    {
                        string confirmation =
                            !string.IsNullOrEmpty(userRef.StringValue) ?
                            userRef.StringValue : userRef.ReferenceType;
                        confirmation = string.Format(
                            LangMan.LS(LSID.LSID_RemoveUserRefQuery), confirmation);
                        if (AppHub.StdDialogs.ShowQuestionYN(confirmation) != false)
                        {
                            //this.fPerson.UserReferences.Delete(userRef);
                            //result = true;
                            result = fLocalUndoman.DoOrdinaryOperation(OperationType.otIndividualURefRemove, fPerson, userRef);
                            fBase.Modified = true;
                        }
                        break;
                    }
            }

            if (result) UpdateURefsSheet();
        }

        private GKSheetList CreateSpousesSheet(Control owner)
        {
            GKSheetList sheet = new GKSheetList(owner);

            sheet.AddColumn("№", 25, false);
            sheet.AddColumn(LangMan.LS(LSID.LSID_Spouse), 300, false);
            sheet.AddColumn(LangMan.LS(LSID.LSID_MarriageDate), 100, false);

            sheet.Buttons = EnumSet<SheetButton>.Create(SheetButton.lbAdd, SheetButton.lbEdit, SheetButton.lbDelete,
                                                        SheetButton.lbJump, SheetButton.lbMoveUp, SheetButton.lbMoveDown);
            sheet.OnModify += ModifySpousesSheet;

            return sheet;
        }

        private void UpdateSpousesSheet()
        {
            try
            {
                fSpousesList.ClearItems();

                int idx = 0;
                foreach (GEDCOMSpouseToFamilyLink spLink in fPerson.SpouseToFamilyLinks) {
                    idx += 1;

                    GEDCOMFamilyRecord family = spLink.Family;
                    if (family == null) continue;

                    GEDCOMIndividualRecord relPerson;
                    string relName;

                    if (fPerson.Sex == GEDCOMSex.svMale) {
                        relPerson = family.GetWife();
                        relName = LangMan.LS(LSID.LSID_UnkFemale);
                    } else {
                        relPerson = family.GetHusband();
                        relName = LangMan.LS(LSID.LSID_UnkMale);
                    }

                    if (relPerson != null) {
                        relName = GKUtils.GetNameString(relPerson, true, false);
                    }

                    GKListItem item = fSpousesList.AddItem(idx, family);
                    item.AddSubItem(relName);
                    item.AddSubItem(new GEDCOMDateItem(GKUtils.GetMarriageDate(family)));
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("PersonEditDlg.UpdateSpousesSheet(): " + ex.Message);
            }
        }

        private void ModifySpousesSheet(object sender, ModifyEventArgs eArgs)
        {
            bool result = false;

            GEDCOMFamilyRecord family = eArgs.ItemData as GEDCOMFamilyRecord;

            switch (eArgs.Action)
            {
                case RecordAction.raAdd:
                    AcceptTempData();
                    result = (fBase.ModifyFamily(ref family, FamilyTarget.Spouse, fPerson));
                    if (result) {
                        eArgs.ItemData = family;
                    }
                    break;

                case RecordAction.raEdit:
                    AcceptTempData();
                    result = (fBase.ModifyFamily(ref family, FamilyTarget.None, null));
                    break;

                case RecordAction.raDelete:
                    if (family != null && AppHub.StdDialogs.ShowQuestionYN(LangMan.LS(LSID.LSID_DetachSpouseQuery)) != false)
                    {
                        //family.RemoveSpouse(this.fPerson);
                        //result = true;
                        result = fLocalUndoman.DoOrdinaryOperation(OperationType.otFamilySpouseDetach, family, fPerson);
                    }
                    break;

                case RecordAction.raMoveUp:
                case RecordAction.raMoveDown:
                    {
                        int idx = fPerson.IndexOfSpouse(family);

                        switch (eArgs.Action)
                        {
                            case RecordAction.raMoveUp:
                                fPerson.ExchangeSpouses(idx - 1, idx);
                                break;

                            case RecordAction.raMoveDown:
                                fPerson.ExchangeSpouses(idx, idx + 1);
                                break;
                        }

                        result = true;
                        break;
                    }

                case RecordAction.raJump:
                    if (family != null && (fPerson.Sex == GEDCOMSex.svMale || fPerson.Sex == GEDCOMSex.svFemale))
                    {
                        GEDCOMPointer sp = null;
                        switch (fPerson.Sex) {
                            case GEDCOMSex.svMale:
                                sp = family.Wife;
                                break;

                            case GEDCOMSex.svFemale:
                                sp = family.Husband;
                                break;
                        }

                        if (sp != null)
                        {
                            GEDCOMIndividualRecord spouse = (GEDCOMIndividualRecord)sp.Value;
                            AcceptChanges();
                            fBase.SelectRecordByXRef(spouse.XRef);
                            Close();
                        }
                    }
                    break;
            }

            if (result) UpdateSpousesSheet();
        }

        private GKSheetList CreateGroupsSheet(Control owner)
        {
            GKSheetList sheet = new GKSheetList(owner);

            sheet.AddColumn(LangMan.LS(LSID.LSID_Group), 350, false);

            sheet.Buttons = EnumSet<SheetButton>.Create(SheetButton.lbAdd, SheetButton.lbDelete, SheetButton.lbJump);
            sheet.OnModify += ModifyGroupsSheet;
            
            return sheet;
        }

        private void UpdateGroupsSheet()
        {
            try
            {
                fGroupsList.ClearItems();

                foreach (GEDCOMPointer ptr in fPerson.Groups) {
                    GEDCOMGroupRecord grp = ptr.Value as GEDCOMGroupRecord;

                    if (grp != null) {
                        fGroupsList.AddItem(grp.GroupName, grp);
                    }
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("PersonEditDlg.UpdateGroupsSheet(): " + ex.Message);
            }
        }

        private void ModifyGroupsSheet(object sender, ModifyEventArgs eArgs)
        {
            bool result = false;

            GEDCOMGroupRecord groupRec = eArgs.ItemData as GEDCOMGroupRecord;

            switch (eArgs.Action)
            {
                case RecordAction.raAdd:
                    groupRec = fBase.SelectRecord(GEDCOMRecordType.rtGroup, null) as GEDCOMGroupRecord;
                    result = (groupRec != null);
                    if (result) {
                        //result = groupRec.AddMember(this.fPerson);
                        result = fLocalUndoman.DoOrdinaryOperation(OperationType.otGroupMemberAttach, groupRec, fPerson);
                    }
                    break;

                case RecordAction.raDelete:
                    result = (AppHub.StdDialogs.ShowQuestionYN(LangMan.LS(LSID.LSID_DetachGroupQuery)) != false);
                    if (result) {
                        //result = groupRec.RemoveMember(this.fPerson);
                        result = fLocalUndoman.DoOrdinaryOperation(OperationType.otGroupMemberDetach, groupRec, fPerson);
                    }
                    break;
                    
                case RecordAction.raJump:
                    if (groupRec != null) {
                        AcceptChanges();
                        fBase.SelectRecordByXRef(groupRec.XRef);
                        Close();
                    }
                    break;
            }

            if (result) UpdateGroupsSheet();
        }

        private GKSheetList CreateNamesSheet(Control owner)
        {
            GKSheetList sheet = new GKSheetList(owner);

            sheet.AddColumn(LangMan.LS(LSID.LSID_Name), 350, false);
            sheet.AddColumn(LangMan.LS(LSID.LSID_Type), 100, false);

            sheet.Buttons = EnumSet<SheetButton>.Create(SheetButton.lbAdd, SheetButton.lbEdit, SheetButton.lbDelete,
                                                        SheetButton.lbMoveDown, SheetButton.lbMoveUp);
            sheet.OnModify += ModifyNamesSheet;
            
            return sheet;
        }

        private void UpdateNamesSheet()
        {
            try
            {
                fNamesList.ClearItems();

                foreach (GEDCOMPersonalName pn in fPerson.PersonalNames)
                {
                    GKListItem item = fNamesList.AddItem(pn.FullName, pn);
                    item.AddSubItem(LangMan.LS(GKData.NameTypes[(int)pn.NameType]));
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("PersonEditDlg.UpdateNamesSheet(): " + ex.Message);
            }
        }

        private void ModifyNamesSheet(object sender, ModifyEventArgs eArgs)
        {
            bool result = false;

            GEDCOMPersonalName persName = eArgs.ItemData as GEDCOMPersonalName;

            switch (eArgs.Action)
            {
                case RecordAction.raAdd:
                case RecordAction.raEdit:
                    using (PersonalNameEditDlg dlg = new PersonalNameEditDlg(fBase))
                    {
                        bool exists = (persName != null);
                        if (!exists) {
                            persName = new GEDCOMPersonalName(fBase.Tree, fPerson, "", "");
                        }

                        dlg.PersonalName = persName;
                        result = (MainWin.Instance.ShowModalEx(dlg, false) == DialogResult.OK);

                        if (!exists) {
                            if (result) {
                                //this.fPerson.PersonalNames.Add(persName);
                                result = fLocalUndoman.DoOrdinaryOperation(OperationType.otIndividualNameAdd, fPerson, persName);
                            } else {
                                persName.Dispose();
                            }
                        }
                    }
                    break;

                case RecordAction.raDelete:
                    if (fPerson.PersonalNames.Count > 1) {
                        result = (AppHub.StdDialogs.ShowQuestionYN(LangMan.LS(LSID.LSID_RemoveNameQuery)) != false);
                        if (result) {
                            //this.fPerson.PersonalNames.Delete(persName);
                            result = fLocalUndoman.DoOrdinaryOperation(OperationType.otIndividualNameRemove, fPerson, persName);
                        }
                    } else {
                        AppHub.StdDialogs.ShowError(LangMan.LS(LSID.LSID_RemoveNameFailed));
                    }
                    break;

                case RecordAction.raMoveUp:
                case RecordAction.raMoveDown:
                    int idx = fPerson.PersonalNames.IndexOf(persName);
                    switch (eArgs.Action)
                    {
                        case RecordAction.raMoveUp:
                            fPerson.PersonalNames.Exchange(idx - 1, idx);
                            break;

                        case RecordAction.raMoveDown:
                            fPerson.PersonalNames.Exchange(idx, idx + 1);
                            break;
                    }
                    result = true;
                    break;
            }

            if (result) UpdateNamesSheet();
        }

        private void SetTitle()
        {
            Text = string.Format("{0} \"{1} {2} {3}\" [{4}]", LangMan.LS(LSID.LSID_Person), txtSurname.Text, txtName.Text,
                                 cmbPatronymic.Text, fPerson.GetXRefNum());
        }

        private void edSurname_TextChanged(object sender, EventArgs e)
        {
            SetTitle();
        }

        private void btnFatherAdd_Click(object sender, EventArgs e)
        {
            if (AppHub.BaseController.AddFather(fBase, fLocalUndoman, fPerson)) {
                UpdateControls();
            }
        }

        private void btnFatherDelete_Click(object sender, EventArgs e)
        {
            if (AppHub.BaseController.DeleteFather(fBase, fLocalUndoman, fPerson)) {
                UpdateControls();
            }
        }

        private void btnFatherSel_Click(object sender, EventArgs e)
        {
            GEDCOMFamilyRecord family = fBase.GetChildFamily(fPerson, false, null);
            if (family == null) return;

            AcceptChanges();
            GEDCOMIndividualRecord father = family.GetHusband();
            fBase.SelectRecordByXRef(father.XRef);
            Close();
        }

        private void btnMotherAdd_Click(object sender, EventArgs e)
        {
            GEDCOMIndividualRecord mother = fBase.SelectPerson(fPerson, TargetMode.tmChild, GEDCOMSex.svFemale);
            if (mother == null) return;

            GEDCOMFamilyRecord family = fBase.GetChildFamily(fPerson, true, mother);
            if (family != null && family.Wife.Value == null) {
                fLocalUndoman.DoOrdinaryOperation(OperationType.otFamilySpouseAttach, family, mother);
                UpdateControls();
            }
        }

        private void btnMotherDelete_Click(object sender, EventArgs e)
        {
            if (AppHub.StdDialogs.ShowQuestionYN(LangMan.LS(LSID.LSID_DetachMotherQuery)) == false) return;

            GEDCOMFamilyRecord family = fBase.GetChildFamily(fPerson, false, null);
            if (family == null) return;

            GEDCOMIndividualRecord mother = family.GetWife();
            fLocalUndoman.DoOrdinaryOperation(OperationType.otFamilySpouseDetach, family, mother);
            UpdateControls();
        }

        private void btnMotherSel_Click(object sender, EventArgs e)
        {
            GEDCOMFamilyRecord family = fBase.GetChildFamily(fPerson, false, null);
            if (family == null) return;

            AcceptChanges();
            GEDCOMIndividualRecord mother = family.GetWife();
            fBase.SelectRecordByXRef(mother.XRef);
            Close();
        }

        private void btnParentsAdd_Click(object sender, EventArgs e)
        {
            AcceptTempData();

            GEDCOMFamilyRecord family = fBase.SelectFamily(fPerson);
            if (family == null) return;

            if (family.IndexOfChild(fPerson) < 0)
            {
                //family.AddChild(this.fPerson);
                fLocalUndoman.DoOrdinaryOperation(OperationType.otIndividualParentsAttach, fPerson, family);
            }
            UpdateControls();
        }

        private void btnParentsEdit_Click(object sender, EventArgs e)
        {
            AcceptTempData();

            GEDCOMFamilyRecord family = fBase.GetChildFamily(fPerson, false, null);
            if (family != null && fBase.ModifyFamily(ref family, FamilyTarget.None, null))
            {
                UpdateControls();
            }
        }

        private void btnParentsDelete_Click(object sender, EventArgs e)
        {
            if (AppHub.StdDialogs.ShowQuestionYN(LangMan.LS(LSID.LSID_DetachParentsQuery)) == false) return;

            GEDCOMFamilyRecord family = fBase.GetChildFamily(fPerson, false, null);
            if (family == null) return;

            //family.RemoveChild(this.fPerson);
            fLocalUndoman.DoOrdinaryOperation(OperationType.otIndividualParentsDetach, fPerson, family);
            UpdateControls();
        }

        private void btnNameCopy_Click(object sender, EventArgs e)
        {
            Clipboard.SetDataObject(GKUtils.GetNameString(fPerson, true, false));
        }

        private void btnPortraitAdd_Click(object sender, EventArgs e)
        {
            GEDCOMMultimediaRecord mmRec = fBase.SelectRecord(GEDCOMRecordType.rtMultimedia, null) as GEDCOMMultimediaRecord;
            if (mmRec == null) return;

            // remove previous portrait link
            GEDCOMMultimediaLink mmLink = fPerson.GetPrimaryMultimediaLink();
            if (mmLink != null) {
                mmLink.IsPrimary = false;
            }

            // set new portrait link
            mmLink = fPerson.SetPrimaryMultimediaLink(mmRec);

            // select portrait area
            using (PortraitSelectDlg selectDlg = new PortraitSelectDlg(fBase)) {
                selectDlg.MultimediaLink = mmLink;
                selectDlg.ShowDialog();
            }

            fMediaList.UpdateSheet();
            UpdatePortrait(true);
        }

        private void btnPortraitDelete_Click(object sender, EventArgs e)
        {
            GEDCOMMultimediaLink mmLink = fPerson.GetPrimaryMultimediaLink();
            if (mmLink == null) return;

            mmLink.IsPrimary = false;
            UpdatePortrait(true);
        }

        private void edSurname_KeyDown(object sender, KeyEventArgs e)
        {
            TextBox tb = (sender as TextBox);
            if (tb != null && e.KeyCode == Keys.Down && e.Control) {
                tb.Text = SysUtils.NormalizeName(tb.Text);
            }
        }

        private void edSurname_KeyPress(object sender, KeyPressEventArgs e)
        {
            if (e.KeyChar == '/')
            {
                e.Handled = true;
            }
        }

        public PersonEditDlg(IBaseWindow baseWin) : base(baseWin)
        {
            InitializeComponent();

            btnAccept.Image = GKResources.iBtnAccept;
            btnCancel.Image = GKResources.iBtnCancel;
            btnPortraitAdd.Image = GKResources.iRecNew;
            btnPortraitDelete.Image = GKResources.iRecDelete;
            btnParentsAdd.Image = GKResources.iRecNew;
            btnParentsEdit.Image = GKResources.iRecEdit;
            btnParentsDelete.Image = GKResources.iRecDelete;
            btnFatherAdd.Image = GKResources.iRecNew;
            btnFatherDelete.Image = GKResources.iRecEdit;
            btnFatherSel.Image = GKResources.iRecDelete;
            btnMotherAdd.Image = GKResources.iRecNew;
            btnMotherDelete.Image = GKResources.iRecEdit;
            btnMotherSel.Image = GKResources.iRecDelete;
            btnNameCopy.Image = GKResources.iCopy;

            imgPortrait.AddButton(btnPortraitAdd);
            imgPortrait.AddButton(btnPortraitDelete);
            for (GEDCOMRestriction res = GEDCOMRestriction.rnNone; res <= GEDCOMRestriction.rnPrivacy; res++)
            {
                cmbRestriction.Items.Add(LangMan.LS(GKData.Restrictions[(int)res]));
            }

            for (GEDCOMSex sx = GEDCOMSex.svNone; sx <= GEDCOMSex.svUndetermined; sx++)
            {
                cmbSex.Items.Add(GKUtils.SexStr(sx));
            }

            fEventsList = new GKSheetList(pageEvents, new GKEventsListModel(fBase, fLocalUndoman, true));
            fEventsList.SetControlName("fEventsList"); // for purpose of tests

            fSpousesList = CreateSpousesSheet(pageSpouses);
            fSpousesList.SetControlName("fSpousesList"); // for purpose of tests

            fNamesList = CreateNamesSheet(pageNames);
            fNamesList.SetControlName("fNamesList"); // for purpose of tests

            fAssociationsList = CreateAssociationsSheet(pageAssociations);
            fAssociationsList.SetControlName("fAssociationsList"); // for purpose of tests

            fGroupsList = CreateGroupsSheet(pageGroups);
            fGroupsList.SetControlName("fGroupsList"); // for purpose of tests

            fNotesList = new GKSheetList(pageNotes, new GKNotesListModel(fBase, fLocalUndoman));
            fNotesList.SetControlName("fNotesList"); // for purpose of tests

            fMediaList = new GKSheetList(pageMultimedia, new GKMediaListModel(fBase, fLocalUndoman));
            fMediaList.SetControlName("fMediaList"); // for purpose of tests

            fSourcesList = new GKSheetList(pageSources, new GKSourcesListModel(fBase, fLocalUndoman));
            fSourcesList.SetControlName("fSourcesList"); // for purpose of tests

            fUserRefList = CreateURefsSheet(pageUserRefs);
            fUserRefList.SetControlName("fUserRefList"); // for purpose of tests

            btnPortraitAdd.Image = GKResources.iRecNew;
            btnPortraitDelete.Image = GKResources.iRecDelete;
            btnFatherAdd.Image = GKResources.iRecNew;
            btnFatherDelete.Image = GKResources.iRecDelete;
            btnFatherSel.Image = GKResources.iToMan;
            btnMotherAdd.Image = GKResources.iRecNew;
            btnMotherDelete.Image = GKResources.iRecDelete;
            btnMotherSel.Image = GKResources.iToMan;
            btnParentsAdd.Image = GKResources.iRecNew;
            btnParentsEdit.Image = GKResources.iRecEdit;
            btnParentsDelete.Image = GKResources.iRecDelete;

            imgPortrait.SizeMode = PictureBoxSizeMode.CenterImage;

            SetLang();
        }

        public void SetLang()
        {
            btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
            btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
            Text = LangMan.LS(LSID.LSID_WinPersonEdit);
            lblSurname.Text = LangMan.LS(LSID.LSID_Surname);
            lblMarriedSurname.Text = LangMan.LS(LSID.LSID_MarriedSurname);
            lblName.Text = LangMan.LS(LSID.LSID_Name);
            lblPatronymic.Text = LangMan.LS(LSID.LSID_Patronymic);
            lblSex.Text = LangMan.LS(LSID.LSID_Sex);
            lblNickname.Text = LangMan.LS(LSID.LSID_Nickname);
            lblSurnamePrefix.Text = LangMan.LS(LSID.LSID_SurnamePrefix);
            lblNamePrefix.Text = LangMan.LS(LSID.LSID_NamePrefix);
            lblNameSuffix.Text = LangMan.LS(LSID.LSID_NameSuffix);
            chkPatriarch.Text = LangMan.LS(LSID.LSID_Patriarch);
            chkBookmark.Text = LangMan.LS(LSID.LSID_Bookmark);
            lblParents.Text = LangMan.LS(LSID.LSID_Parents);
            pageEvents.Text = LangMan.LS(LSID.LSID_Events);
            pageSpouses.Text = LangMan.LS(LSID.LSID_Spouses);
            pageAssociations.Text = LangMan.LS(LSID.LSID_Associations);
            pageGroups.Text = LangMan.LS(LSID.LSID_RPGroups);
            pageNotes.Text = LangMan.LS(LSID.LSID_RPNotes);
            pageMultimedia.Text = LangMan.LS(LSID.LSID_RPMultimedia);
            pageSources.Text = LangMan.LS(LSID.LSID_RPSources);
            pageUserRefs.Text = LangMan.LS(LSID.LSID_UserRefs);
            lblRestriction.Text = LangMan.LS(LSID.LSID_Restriction);
            pageNames.Text = LangMan.LS(LSID.LSID_Names);

            toolTip1.SetToolTip(btnPortraitAdd, LangMan.LS(LSID.LSID_PortraitAddTip));
            toolTip1.SetToolTip(btnPortraitDelete, LangMan.LS(LSID.LSID_PortraitDeleteTip));
            toolTip1.SetToolTip(btnParentsAdd, LangMan.LS(LSID.LSID_ParentsAddTip));
            toolTip1.SetToolTip(btnParentsEdit, LangMan.LS(LSID.LSID_ParentsEditTip));
            toolTip1.SetToolTip(btnParentsDelete, LangMan.LS(LSID.LSID_ParentsDeleteTip));
            toolTip1.SetToolTip(btnFatherAdd, LangMan.LS(LSID.LSID_FatherAddTip));
            toolTip1.SetToolTip(btnFatherDelete, LangMan.LS(LSID.LSID_FatherDeleteTip));
            toolTip1.SetToolTip(btnFatherSel, LangMan.LS(LSID.LSID_FatherSelTip));
            toolTip1.SetToolTip(btnMotherAdd, LangMan.LS(LSID.LSID_MotherAddTip));
            toolTip1.SetToolTip(btnMotherDelete, LangMan.LS(LSID.LSID_MotherDeleteTip));
            toolTip1.SetToolTip(btnMotherSel, LangMan.LS(LSID.LSID_MotherSelTip));
            toolTip1.SetToolTip(btnNameCopy, LangMan.LS(LSID.LSID_NameCopyTip));
        }
    }
}
