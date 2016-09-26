/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
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
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;
using GKCore.Types;
using GKUI.Controls;
using GKUI.Sheets;

namespace GKUI.Dialogs
{
    /// <summary>
    /// 
    /// </summary>
    public partial class PersonEditDlg : Form, IBaseEditor
    {
        private readonly IBaseWindow fBase;

        private readonly GKEventsSheet fEventsList;
        private readonly GKSheetList fSpousesList;
        private readonly GKSheetList fAssociationsList;
        private readonly GKSheetList fGroupsList;
        private readonly GKNotesSheet fNotesList;
        private readonly GKMediaSheet fMediaList;
        private readonly GKSourcesSheet fSourcesList;
        private readonly GKSheetList fUserRefList;
        private readonly GKSheetList fNamesList;

        private GEDCOMIndividualRecord fPerson;

        public GEDCOMIndividualRecord Person
        {
            get { return this.fPerson; }
            set { this.SetPerson(value); }
        }

        public IBaseWindow Base
        {
            get { return this.fBase; }
        }


        private void SetPerson(GEDCOMIndividualRecord value)
        {
            this.fPerson = value;
            try
            {
                string fam, nam, pat;
                GKUtils.GetNameParts(this.fPerson, out fam, out nam, out pat);
                this.txtSurname.Text = fam;
                this.txtName.Text = nam;
                this.txtPatronymic.Text = pat;
                this.cmbSex.SelectedIndex = (sbyte)this.fPerson.Sex;
                this.chkPatriarch.Checked = this.fPerson.Patriarch;
                this.chkBookmark.Checked = this.fPerson.Bookmark;

                this.cmbRestriction.SelectedIndexChanged -= cbRestriction_SelectedIndexChanged;
                this.cmbRestriction.SelectedIndex = (sbyte)this.fPerson.Restriction;
                this.cmbRestriction.SelectedIndexChanged += cbRestriction_SelectedIndexChanged;

                if (this.fPerson.PersonalNames.Count > 0)
                {
                    GEDCOMPersonalName np = this.fPerson.PersonalNames[0];
                    this.txtNamePrefix.Text = np.Pieces.Prefix;
                    this.txtNickname.Text = np.Pieces.Nickname;
                    this.txtSurnamePrefix.Text = np.Pieces.SurnamePrefix;
                    this.txtNameSuffix.Text = np.Pieces.Suffix;
                }

                this.UpdateControls(true);
            }
            catch (Exception ex)
            {
                this.fBase.Host.LogWrite("PersonEditDlg.SetPerson(): " + ex.Message);
            }
        }

        private void UpdateControls(bool totalUpdate = false)
        {
            bool locked = (this.cmbRestriction.SelectedIndex == (int)GEDCOMRestriction.rnLocked);

            if (this.fPerson.ChildToFamilyLinks.Count != 0)
            {
                GEDCOMFamilyRecord family = this.fPerson.ChildToFamilyLinks[0].Family;
                this.btnParentsAdd.Enabled = false;
                this.btnParentsEdit.Enabled = true && !locked;
                this.btnParentsDelete.Enabled = true && !locked;

                GEDCOMIndividualRecord relPerson = family.GetHusband();
                if (relPerson != null)
                {
                    this.btnFatherAdd.Enabled = false;
                    this.btnFatherDelete.Enabled = true && !locked;
                    this.btnFatherSel.Enabled = true && !locked;
                    this.txtFather.Text = relPerson.GetNameString(true, false);
                }
                else
                {
                    this.btnFatherAdd.Enabled = true && !locked;
                    this.btnFatherDelete.Enabled = false;
                    this.btnFatherSel.Enabled = false;
                    this.txtFather.Text = "";
                }

                relPerson = family.GetWife();
                if (relPerson != null)
                {
                    this.btnMotherAdd.Enabled = false;
                    this.btnMotherDelete.Enabled = true && !locked;
                    this.btnMotherSel.Enabled = true && !locked;
                    this.txtMother.Text = relPerson.GetNameString(true, false);
                }
                else
                {
                    this.btnMotherAdd.Enabled = true && !locked;
                    this.btnMotherDelete.Enabled = false;
                    this.btnMotherSel.Enabled = false;
                    this.txtMother.Text = "";
                }
            }
            else
            {
                this.btnParentsAdd.Enabled = true && !locked;
                this.btnParentsEdit.Enabled = false;
                this.btnParentsDelete.Enabled = false;

                this.btnFatherAdd.Enabled = true && !locked;
                this.btnFatherDelete.Enabled = false;
                this.btnFatherSel.Enabled = false;

                this.btnMotherAdd.Enabled = true && !locked;
                this.btnMotherDelete.Enabled = false;
                this.btnMotherSel.Enabled = false;

                this.txtFather.Text = "";
                this.txtMother.Text = "";
            }

            if (totalUpdate) {
                this.fEventsList.DataList = this.fPerson.Events.GetEnumerator();
                this.fNotesList.DataList = this.fPerson.Notes.GetEnumerator();
                this.fMediaList.DataList = this.fPerson.MultimediaLinks.GetEnumerator();
                this.fSourcesList.DataList = this.fPerson.SourceCitations.GetEnumerator();
                this.UpdateSpousesSheet();
                this.UpdateAssociationsSheet();
                this.UpdateGroupsSheet();
                this.UpdateURefsSheet();
                this.UpdateNamesSheet();
            }

            this.UpdatePortrait(totalUpdate);

            // controls lock
            this.txtName.Enabled = !locked;
            this.txtPatronymic.Enabled = !locked;
            this.txtSurname.Enabled = !locked;

            this.cmbSex.Enabled = !locked;
            this.chkPatriarch.Enabled = !locked;
            this.chkBookmark.Enabled = !locked;

            this.txtNamePrefix.Enabled = !locked;
            this.txtNickname.Enabled = !locked;
            this.txtSurnamePrefix.Enabled = !locked;
            this.txtNameSuffix.Enabled = !locked;

            this.fEventsList.ReadOnly = locked;
            this.fNotesList.ReadOnly = locked;
            this.fMediaList.ReadOnly = locked;
            this.fSourcesList.ReadOnly = locked;
            this.fSpousesList.ReadOnly = locked;
            this.fAssociationsList.ReadOnly = locked;
            this.fGroupsList.ReadOnly = locked;
            this.fUserRefList.ReadOnly = locked;
        }

        private void UpdatePortrait(bool totalUpdate)
        {
            if (totalUpdate) {
                Image img = this.fBase.Context.GetPrimaryBitmap(this.fPerson, this.imgPortrait.Width, this.imgPortrait.Height, false);
                this.imgPortrait.Image = img;
            }

            bool locked = (this.cmbRestriction.SelectedIndex == (int)GEDCOMRestriction.rnLocked);

            this.btnPortraitAdd.Enabled = !locked;
            this.btnPortraitDelete.Enabled = this.imgPortrait.Image != null && !locked;
        }

        private void cbRestriction_SelectedIndexChanged(object sender, EventArgs e)
        {
            this.UpdateControls();
        }

        private void AcceptChanges()
        {
            GEDCOMPersonalName np = this.fPerson.PersonalNames[0];
            np.SetNameParts(this.txtName.Text.Trim() + " " + this.txtPatronymic.Text.Trim(), this.txtSurname.Text.Trim(), np.LastPart);

            GEDCOMPersonalNamePieces pieces = np.Pieces;
            pieces.Nickname = this.txtNickname.Text;
            pieces.Prefix = this.txtNamePrefix.Text;
            pieces.SurnamePrefix = this.txtSurnamePrefix.Text;
            pieces.Suffix = this.txtNameSuffix.Text;

            this.fPerson.Sex = (GEDCOMSex)this.cmbSex.SelectedIndex;
            this.fPerson.Patriarch = this.chkPatriarch.Checked;
            this.fPerson.Bookmark = this.chkBookmark.Checked;
            this.fPerson.Restriction = (GEDCOMRestriction)this.cmbRestriction.SelectedIndex;

            if (this.fPerson.ChildToFamilyLinks.Count > 0)
            {
                this.fPerson.ChildToFamilyLinks[0].Family.SortChilds();
            }

            this.fBase.ChangeRecord(this.fPerson);
        }

        private void btnAccept_Click(object sender, EventArgs e)
        {
            try
            {
                this.AcceptChanges();
                base.DialogResult = DialogResult.OK;
            }
            catch (Exception ex)
            {
                this.fBase.Host.LogWrite("PersonEditDlg.btnAccept_Click(): " + ex.Message);
                base.DialogResult = DialogResult.None;
            }
        }

        private GKSheetList CreateAssociationsSheet(Control owner)
        {
            GKSheetList sheet = new GKSheetList(owner);

            sheet.Columns_BeginUpdate();
            sheet.AddColumn(LangMan.LS(LSID.LSID_Relation), 300, false);
            sheet.AddColumn(LangMan.LS(LSID.LSID_Person), 200, false);
            sheet.Columns_EndUpdate();

            sheet.Buttons = EnumSet<SheetButton>.Create(SheetButton.lbAdd, SheetButton.lbEdit, SheetButton.lbDelete, SheetButton.lbJump);
            sheet.OnModify += this.ModifyAssociationsSheet;

            return sheet;
        }

        private void UpdateAssociationsSheet()
        {
            try
            {
                fAssociationsList.ClearItems();

                foreach (GEDCOMAssociation ast in this.fPerson.Associations) {
                    string nm = ((ast.Individual == null) ? "" : ast.Individual.GetNameString(true, false));

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
                    using (AssociationEditDlg fmAstEdit = new AssociationEditDlg(this.fBase))
                    {
                        bool exists = (ast != null);
                        if (!exists) {
                            ast = new GEDCOMAssociation(this.fBase.Tree, this.fPerson, "", "");
                        }

                        fmAstEdit.Association = ast;
                        result = (MainWin.Instance.ShowModalEx(fmAstEdit, false) == DialogResult.OK);

                        if (!exists) {
                            if (result) {
                                this.fPerson.Associations.Add(ast);
                            } else {
                                ast.Dispose();
                            }
                        }
                    }
                    break;

                case RecordAction.raDelete:
                    if (GKUtils.ShowQuestion(LangMan.LS(LSID.LSID_RemoveAssociationQuery)) != DialogResult.No)
                    {
                        this.fPerson.Associations.Delete(ast);
                        result = true;
                        this.fBase.Modified = true;
                    }
                    break;

                case RecordAction.raJump:
                    if (ast != null) {
                        this.AcceptChanges();
                        this.fBase.SelectRecordByXRef(ast.Individual.XRef);
                        base.Close();
                    }
                    break;
            }

            if (result) this.UpdateAssociationsSheet();
        }

        private GKSheetList CreateURefsSheet(Control owner)
        {
            GKSheetList sheet = new GKSheetList(owner);

            sheet.Columns_BeginUpdate();
            sheet.AddColumn(LangMan.LS(LSID.LSID_Reference), 300, false);
            sheet.AddColumn(LangMan.LS(LSID.LSID_Type), 200, false);
            sheet.Columns_EndUpdate();

            sheet.OnModify += this.ModifyURefsSheet;

            return sheet;
        }

        private void UpdateURefsSheet()
        {
            try
            {
                this.fUserRefList.ClearItems();

                foreach (GEDCOMUserReference uref in this.fPerson.UserReferences) {
                    GKListItem item = this.fUserRefList.AddItem(uref.StringValue, uref);
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
                    using (UserRefEditDlg dlg = new UserRefEditDlg(this.fBase))
                    {
                        bool exists = (userRef != null);
                        if (!exists) {
                            userRef = new GEDCOMUserReference(this.fBase.Tree, this.fPerson, "", "");
                        }

                        dlg.UserRef = userRef;
                        result = (MainWin.Instance.ShowModalEx(dlg, false) == DialogResult.OK);

                        if (!exists) {
                            if (result) {
                                this.fPerson.UserReferences.Add(userRef);
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
                    if (GKUtils.ShowQuestion(confirmation) != DialogResult.No)
                    {
                        this.fPerson.UserReferences.Delete(userRef);
                        result = true;
                        this.fBase.Modified = true;
                    }
                    break;
                }
            }

            if (result) this.UpdateURefsSheet();
        }

        private GKSheetList CreateSpousesSheet(Control owner)
        {
            GKSheetList sheet = new GKSheetList(owner);

            sheet.Columns_BeginUpdate();
            sheet.AddColumn("№", 25, false);
            sheet.AddColumn(LangMan.LS(LSID.LSID_Spouse), 300, false);
            sheet.AddColumn(LangMan.LS(LSID.LSID_MarriageDate), 100, false);
            sheet.Columns_EndUpdate();

            sheet.Buttons = EnumSet<SheetButton>.Create(SheetButton.lbAdd, SheetButton.lbEdit, SheetButton.lbDelete,
                                                        SheetButton.lbJump, SheetButton.lbMoveUp, SheetButton.lbMoveDown);
            sheet.OnModify += this.ModifySpousesSheet;

            return sheet;
        }

        private void UpdateSpousesSheet()
        {
            try
            {
                this.fSpousesList.ClearItems();

                int idx = 0;
                foreach (GEDCOMSpouseToFamilyLink spLink in this.fPerson.SpouseToFamilyLinks) {
                    idx += 1;

                    GEDCOMFamilyRecord family = spLink.Family;
                    if (family != null)
                    {
                        GEDCOMIndividualRecord relPerson;
                        string relName;

                        if (this.fPerson.Sex == GEDCOMSex.svMale) {
                            relPerson = family.GetWife();
                            relName = LangMan.LS(LSID.LSID_UnkFemale);
                        } else {
                            relPerson = family.GetHusband();
                            relName = LangMan.LS(LSID.LSID_UnkMale);
                        }

                        if (relPerson != null) {
                            relName = relPerson.GetNameString(true, false);
                        }

                        GKListItem item = this.fSpousesList.AddItem(idx, family);
                        item.AddSubItem(relName);
                        item.AddSubItem(GKUtils.GetMarriageDate(family));
                    }

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
                    result = (this.fBase.ModifyFamily(ref family, FamilyTarget.Spouse, this.fPerson));
                    if (result) eArgs.ItemData = family;
                    break;

                case RecordAction.raEdit:
                    result = (this.fBase.ModifyFamily(ref family, FamilyTarget.None, null));
                    break;

                case RecordAction.raDelete:
                    if (family != null && GKUtils.ShowQuestion(LangMan.LS(LSID.LSID_DetachSpouseQuery)) != DialogResult.No)
                    {
                        family.RemoveSpouse(this.fPerson);
                        result = true;
                    }
                    break;

                case RecordAction.raMoveUp:
                case RecordAction.raMoveDown:
                    {
                        int idx = this.fPerson.IndexOfSpouse(family);

                        switch (eArgs.Action)
                        {
                            case RecordAction.raMoveUp:
                                this.fPerson.ExchangeSpouses(idx - 1, idx);
                                break;

                            case RecordAction.raMoveDown:
                                this.fPerson.ExchangeSpouses(idx, idx + 1);
                                break;
                        }

                        result = true;
                        break;
                    }

                case RecordAction.raJump:
                    if (family != null && (this.fPerson.Sex == GEDCOMSex.svMale || this.fPerson.Sex == GEDCOMSex.svFemale))
                    {
                        GEDCOMPointer sp = null;
                        switch (this.fPerson.Sex) {
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
                            this.AcceptChanges();
                            this.fBase.SelectRecordByXRef(spouse.XRef);
                            base.Close();
                        }
                    }
                    break;
            }

            if (result) this.UpdateSpousesSheet();
        }

        private GKSheetList CreateGroupsSheet(Control owner)
        {
            GKSheetList sheet = new GKSheetList(owner);
            
            sheet.Columns_BeginUpdate();
            sheet.AddColumn(LangMan.LS(LSID.LSID_Group), 350, false);
            sheet.Columns_EndUpdate();

            sheet.Buttons = EnumSet<SheetButton>.Create(SheetButton.lbAdd, SheetButton.lbDelete, SheetButton.lbJump);
            sheet.OnModify += this.ModifyGroupsSheet;
            
            return sheet;
        }

        private void UpdateGroupsSheet()
        {
            try
            {
                this.fGroupsList.ClearItems();

                foreach (GEDCOMPointer ptr in this.fPerson.Groups) {
                    GEDCOMGroupRecord grp = ptr.Value as GEDCOMGroupRecord;

                    if (grp != null) {
                        this.fGroupsList.AddItem(grp.GroupName, grp);
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
                    groupRec = this.fBase.SelectRecord(GEDCOMRecordType.rtGroup, null) as GEDCOMGroupRecord;
                    result = (groupRec != null && groupRec.AddMember(this.fPerson));
                    break;

                case RecordAction.raDelete:
                    result = (GKUtils.ShowQuestion(LangMan.LS(LSID.LSID_DetachGroupQuery)) != DialogResult.No && groupRec.RemoveMember(this.fPerson));
                    break;
                    
                case RecordAction.raJump:
                    if (groupRec != null) {
                        this.AcceptChanges();
                        this.fBase.SelectRecordByXRef(groupRec.XRef);
                        base.Close();
                    }
                    break;
            }

            if (result) this.UpdateGroupsSheet();
        }

        private GKSheetList CreateNamesSheet(Control owner)
        {
            GKSheetList sheet = new GKSheetList(owner);
            
            sheet.Columns_BeginUpdate();
            sheet.AddColumn(LangMan.LS(LSID.LSID_Name), 350, false);
            sheet.AddColumn(LangMan.LS(LSID.LSID_Type), 100, false);
            sheet.Columns_EndUpdate();

            sheet.Buttons = EnumSet<SheetButton>.Create(SheetButton.lbAdd, SheetButton.lbEdit, SheetButton.lbDelete,
                                                        SheetButton.lbMoveDown, SheetButton.lbMoveUp);
            sheet.OnModify += this.ModifyNamesSheet;
            
            return sheet;
        }

        private void UpdateNamesSheet()
        {
            try
            {
                this.fNamesList.ClearItems();

                foreach (GEDCOMPersonalName pn in this.fPerson.PersonalNames)
                {
                    GKListItem item = this.fNamesList.AddItem(pn.FullName, pn);
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
                    using (PersonalNameEditDlg dlg = new PersonalNameEditDlg(this.fBase))
                    {
                        bool exists = (persName != null);
                        if (!exists) {
                            persName = new GEDCOMPersonalName(this.fBase.Tree, this.fPerson, "", "");
                        }

                        dlg.PersonalName = persName;
                        result = (MainWin.Instance.ShowModalEx(dlg, false) == DialogResult.OK);

                        if (!exists) {
                            if (result) {
                                this.fPerson.PersonalNames.Add(persName);
                            } else {
                                persName.Dispose();
                            }
                        }
                    }
                    break;

                case RecordAction.raDelete:
                    if (this.fPerson.PersonalNames.Count > 1) {
                        result = (GKUtils.ShowQuestion(LangMan.LS(LSID.LSID_RemoveNameQuery)) != DialogResult.No);
                        if (result) {
                            this.fPerson.PersonalNames.Delete(persName);
                        }
                    } else {
                        GKUtils.ShowError(LangMan.LS(LSID.LSID_RemoveNameFailed));
                    }
                    break;

                case RecordAction.raMoveUp:
                case RecordAction.raMoveDown:
                    int idx = this.fPerson.PersonalNames.IndexOf(persName);
                    switch (eArgs.Action)
                    {
                        case RecordAction.raMoveUp:
                            this.fPerson.PersonalNames.Exchange(idx - 1, idx);
                            break;

                        case RecordAction.raMoveDown:
                            this.fPerson.PersonalNames.Exchange(idx, idx + 1);
                            break;
                    }
                    result = true;
                    break;
            }

            if (result) this.UpdateNamesSheet();
        }

        private void SetTitle()
        {
            this.Text = string.Format("{0} \"{1} {2} {3}\" [{4}]", LangMan.LS(LSID.LSID_Person), this.txtSurname.Text, this.txtName.Text,
                                      this.txtPatronymic.Text, this.fPerson.GetXRefNum());
        }

        private void edSurname_TextChanged(object sender, EventArgs e)
        {
            this.SetTitle();
        }

        private void EditName_TextChanged(object sender, EventArgs e)
        {
            this.SetTitle();
        }

        private void EditPatronymic_TextChanged(object sender, EventArgs e)
        {
            this.SetTitle();
        }

        private void btnFatherAdd_Click(object sender, EventArgs e)
        {
            GEDCOMIndividualRecord father = this.fBase.SelectPerson(this.fPerson, TargetMode.tmChild, GEDCOMSex.svMale);
            if (father == null) return;

            GEDCOMFamilyRecord family = this.fBase.GetChildFamily(this.fPerson, true, father);
            if (family != null && family.Husband.Value == null) {
                family.AddSpouse(father);
                this.UpdateControls();
            } else {
                this.fBase.Host.LogWrite("PersonEditDlg.btnFatherAdd_Click(): fail");
            }
        }

        private void btnFatherDelete_Click(object sender, EventArgs e)
        {
            if (GKUtils.ShowQuestion(LangMan.LS(LSID.LSID_DetachFatherQuery)) != DialogResult.No)
            {
                GEDCOMFamilyRecord family = this.fBase.GetChildFamily(this.fPerson, false, null);
                if (family != null)
                {
                    family.RemoveSpouse(family.GetHusband());
                    this.UpdateControls();
                }
            }
        }

        private void btnFatherSel_Click(object sender, EventArgs e)
        {
            GEDCOMFamilyRecord family = this.fBase.GetChildFamily(this.fPerson, false, null);
            if (family == null) return;

            this.AcceptChanges();
            GEDCOMIndividualRecord father = family.GetHusband();
            this.fBase.SelectRecordByXRef(father.XRef);
            base.Close();
        }

        private void btnMotherAdd_Click(object sender, EventArgs e)
        {
            GEDCOMIndividualRecord mother = this.fBase.SelectPerson(this.fPerson, TargetMode.tmChild, GEDCOMSex.svFemale);
            if (mother == null) return;

            GEDCOMFamilyRecord family = this.fBase.GetChildFamily(this.fPerson, true, mother);
            if (family != null && family.Wife.Value == null) {
                family.AddSpouse(mother);
                this.UpdateControls();
            } else {
                this.fBase.Host.LogWrite("PersonEditDlg.btnMotherAdd_Click(): fail");
            }
        }

        private void btnMotherDelete_Click(object sender, EventArgs e)
        {
            if (GKUtils.ShowQuestion(LangMan.LS(LSID.LSID_DetachMotherQuery)) != DialogResult.No)
            {
                GEDCOMFamilyRecord family = this.fBase.GetChildFamily(this.fPerson, false, null);
                if (family != null)
                {
                    GEDCOMIndividualRecord mother = family.GetWife();
                    family.RemoveSpouse(mother);
                    this.UpdateControls();
                }
            }
        }

        private void btnMotherSel_Click(object sender, EventArgs e)
        {
            GEDCOMFamilyRecord family = this.fBase.GetChildFamily(this.fPerson, false, null);
            if (family == null) return;

            this.AcceptChanges();
            GEDCOMIndividualRecord mother = family.GetWife();
            this.fBase.SelectRecordByXRef(mother.XRef);
            base.Close();
        }

        private void btnParentsAdd_Click(object sender, EventArgs e)
        {
            GEDCOMFamilyRecord family = this.fBase.SelectFamily(this.fPerson);
            if (family == null) return;

            if (family.IndexOfChild(this.fPerson) < 0)
            {
                family.AddChild(this.fPerson);
            }
            this.UpdateControls();
        }

        private void btnParentsEdit_Click(object sender, EventArgs e)
        {
            GEDCOMFamilyRecord family = this.fBase.GetChildFamily(this.fPerson, false, null);
            if (family != null && this.fBase.ModifyFamily(ref family, FamilyTarget.None, null))
            {
                this.UpdateControls();
            }
        }

        private void btnParentsDelete_Click(object sender, EventArgs e)
        {
            if (GKUtils.ShowQuestion(LangMan.LS(LSID.LSID_DetachParentsQuery)) != DialogResult.No)
            {
                GEDCOMFamilyRecord family = this.fBase.GetChildFamily(this.fPerson, false, null);
                if (family != null)
                {
                    family.RemoveChild(this.fPerson);
                    this.UpdateControls();
                }
            }
        }

        private void btnNameCopy1_Click(object sender, EventArgs e)
        {
            Clipboard.SetDataObject(this.fPerson.GetNameString(true, false));
        }

        private void btnPortraitAdd_Click(object sender, EventArgs e)
        {
            GEDCOMMultimediaRecord mmRec = fBase.SelectRecord(GEDCOMRecordType.rtMultimedia, null) as GEDCOMMultimediaRecord;
            if (mmRec == null) return;

            // remove previous portrait link
            GEDCOMMultimediaLink mmLink = this.fPerson.GetPrimaryMultimediaLink();
            if (mmLink != null) {
                mmLink.IsPrimary = false;
            }

            // set new portrait link
            mmLink = this.fPerson.SetPrimaryMultimediaLink(mmRec);

            // select portrait area
            using (PortraitSelectDlg selectDlg = new PortraitSelectDlg(this.fBase)) {
                selectDlg.MultimediaLink = mmLink;
                selectDlg.ShowDialog();
            }

            this.fMediaList.UpdateSheet();
            this.UpdatePortrait(true);
        }

        private void btnPortraitDelete_Click(object sender, EventArgs e)
        {
            GEDCOMMultimediaLink mmLink = this.fPerson.GetPrimaryMultimediaLink();
            if (mmLink == null) return;

            mmLink.IsPrimary = false;
            this.UpdatePortrait(true);
        }

        private void edSurname_KeyDown(object sender, KeyEventArgs e)
        {
            TextBox tb = (sender as TextBox);

            if (tb != null && e.KeyCode == Keys.Down && e.Control)
            {
                tb.Text = GEDCOMUtils.NormalizeName(tb.Text);
            }
        }

        private void edSurname_KeyPress(object sender, KeyPressEventArgs e)
        {
            if (e.KeyChar == '/')
            {
                e.Handled = true;
            }
        }

        public PersonEditDlg(IBaseWindow aBase)
        {
            this.InitializeComponent();

            this.btnAccept.Image = global::GKResources.iBtnAccept;
            this.btnCancel.Image = global::GKResources.iBtnCancel;
            this.btnPortraitAdd.Image = global::GKResources.iRecNew;
            this.btnPortraitDelete.Image = global::GKResources.iRecDelete;
            this.btnParentsAdd.Image = global::GKResources.iRecNew;
            this.btnParentsEdit.Image = global::GKResources.iRecEdit;
            this.btnParentsDelete.Image = global::GKResources.iRecDelete;
            this.btnFatherAdd.Image = global::GKResources.iRecNew;
            this.btnFatherDelete.Image = global::GKResources.iRecEdit;
            this.btnFatherSel.Image = global::GKResources.iRecDelete;
            this.btnMotherAdd.Image = global::GKResources.iRecNew;
            this.btnMotherDelete.Image = global::GKResources.iRecEdit;
            this.btnMotherSel.Image = global::GKResources.iRecDelete;
            this.btnNameCopy.Image = global::GKResources.iCopy;

            this.fBase = aBase;

            for (GEDCOMRestriction res = GEDCOMRestriction.rnNone; res <= GEDCOMRestriction.rnPrivacy; res++)
            {
                this.cmbRestriction.Items.Add(LangMan.LS(GKData.Restrictions[(int)res]));
            }

            for (GEDCOMSex sx = GEDCOMSex.svNone; sx <= GEDCOMSex.svUndetermined; sx++)
            {
                this.cmbSex.Items.Add(GKUtils.SexStr(sx));
            }

            this.fEventsList = new GKEventsSheet(this, this.pageEvents, true);

            this.fSpousesList = this.CreateSpousesSheet(this.pageSpouses);
            this.fNamesList = this.CreateNamesSheet(this.pageNames);
            this.fAssociationsList = this.CreateAssociationsSheet(this.pageAssociations);
            this.fGroupsList = this.CreateGroupsSheet(this.pageGroups);

            this.fNotesList = new GKNotesSheet(this, this.pageNotes);
            this.fMediaList = new GKMediaSheet(this, this.pageMultimedia);
            this.fSourcesList = new GKSourcesSheet(this, this.pageSources);

            this.fUserRefList = this.CreateURefsSheet(this.pageUserRefs);

            this.btnPortraitAdd.Image = global::GKResources.iRecNew;
            this.btnPortraitDelete.Image = global::GKResources.iRecDelete;
            this.btnFatherAdd.Image = global::GKResources.iRecNew;
            this.btnFatherDelete.Image = global::GKResources.iRecDelete;
            this.btnFatherSel.Image = global::GKResources.iToMan;
            this.btnMotherAdd.Image = global::GKResources.iRecNew;
            this.btnMotherDelete.Image = global::GKResources.iRecDelete;
            this.btnMotherSel.Image = global::GKResources.iToMan;
            this.btnParentsAdd.Image = global::GKResources.iRecNew;
            this.btnParentsEdit.Image = global::GKResources.iRecEdit;
            this.btnParentsDelete.Image = global::GKResources.iRecDelete;

            this.imgPortrait.SizeMode = PictureBoxSizeMode.CenterImage;

            this.SetLang();
        }

        public void SetLang()
        {
            this.btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
            this.btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
            this.Text = LangMan.LS(LSID.LSID_WinPersonEdit);
            this.lblSurname.Text = LangMan.LS(LSID.LSID_Surname);
            this.lblName.Text = LangMan.LS(LSID.LSID_Name);
            this.lblPatronymic.Text = LangMan.LS(LSID.LSID_Patronymic);
            this.lblSex.Text = LangMan.LS(LSID.LSID_Sex);
            this.lblNickname.Text = LangMan.LS(LSID.LSID_Nickname);
            this.lblSurnamePrefix.Text = LangMan.LS(LSID.LSID_SurnamePrefix);
            this.lblNamePrefix.Text = LangMan.LS(LSID.LSID_NamePrefix);
            this.lblNameSuffix.Text = LangMan.LS(LSID.LSID_NameSuffix);
            this.chkPatriarch.Text = LangMan.LS(LSID.LSID_Patriarch);
            this.chkBookmark.Text = LangMan.LS(LSID.LSID_Bookmark);
            this.lblParents.Text = LangMan.LS(LSID.LSID_Parents);
            this.pageEvents.Text = LangMan.LS(LSID.LSID_Events);
            this.pageSpouses.Text = LangMan.LS(LSID.LSID_Spouses);
            this.pageAssociations.Text = LangMan.LS(LSID.LSID_Associations);
            this.pageGroups.Text = LangMan.LS(LSID.LSID_RPGroups);
            this.pageNotes.Text = LangMan.LS(LSID.LSID_RPNotes);
            this.pageMultimedia.Text = LangMan.LS(LSID.LSID_RPMultimedia);
            this.pageSources.Text = LangMan.LS(LSID.LSID_RPSources);
            this.pageUserRefs.Text = LangMan.LS(LSID.LSID_UserRefs);
            this.lblRestriction.Text = LangMan.LS(LSID.LSID_Restriction);
            this.pageNames.Text = LangMan.LS(LSID.LSID_Names);

            this.toolTip1.SetToolTip(this.btnPortraitAdd, LangMan.LS(LSID.LSID_PortraitAddTip));
            this.toolTip1.SetToolTip(this.btnPortraitDelete, LangMan.LS(LSID.LSID_PortraitDeleteTip));
            this.toolTip1.SetToolTip(this.btnParentsAdd, LangMan.LS(LSID.LSID_ParentsAddTip));
            this.toolTip1.SetToolTip(this.btnParentsEdit, LangMan.LS(LSID.LSID_ParentsEditTip));
            this.toolTip1.SetToolTip(this.btnParentsDelete, LangMan.LS(LSID.LSID_ParentsDeleteTip));
            this.toolTip1.SetToolTip(this.btnFatherAdd, LangMan.LS(LSID.LSID_FatherAddTip));
            this.toolTip1.SetToolTip(this.btnFatherDelete, LangMan.LS(LSID.LSID_FatherDeleteTip));
            this.toolTip1.SetToolTip(this.btnFatherSel, LangMan.LS(LSID.LSID_FatherSelTip));
            this.toolTip1.SetToolTip(this.btnMotherAdd, LangMan.LS(LSID.LSID_MotherAddTip));
            this.toolTip1.SetToolTip(this.btnMotherDelete, LangMan.LS(LSID.LSID_MotherDeleteTip));
            this.toolTip1.SetToolTip(this.btnMotherSel, LangMan.LS(LSID.LSID_MotherSelTip));
            this.toolTip1.SetToolTip(this.btnNameCopy, LangMan.LS(LSID.LSID_NameCopyTip));
        }
    }
}
