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
using GKCommon.Controls;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.Operations;
using GKCore.Options;
using GKCore.Types;
using GKUI.Controls;
using GKUI.Sheets;

namespace GKUI.Dialogs
{
    /// <summary>
    /// 
    /// </summary>
    public partial class PersonEditDlg : EditorDialog
    {
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
        private GEDCOMIndividualRecord fTarget;
        private TargetMode fTargetMode;
        private Image fPortraitImg;

        public GEDCOMIndividualRecord Person
        {
            get { return this.fPerson; }
            set { this.SetPerson(value); }
        }

        public GEDCOMIndividualRecord Target
        {
            get { return this.fTarget; }
            set { this.SetTarget(value); }
        }

        public TargetMode TargetMode
        {
            get { return this.fTargetMode; }
            set { this.fTargetMode = value; }
        }


        private void SetPerson(GEDCOMIndividualRecord value)
        {
            this.fPerson = value;

            try
            {
                string fam, nam, pat;
                GKUtils.GetNameParts(this.fPerson, out fam, out nam, out pat, false);
                this.txtSurname.Text = fam;
                this.txtName.Text = nam;
                this.cmbPatronymic.Text = pat;

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

                    this.txtMarriedSurname.Text = np.Pieces.MarriedName;
                }

                this.UpdateControls(true);
            }
            catch (Exception ex)
            {
                this.fBase.Host.LogWrite("PersonEditDlg.SetPerson(): " + ex.Message);
            }
        }

        private void SetTarget(GEDCOMIndividualRecord value)
        {
            try
            {
                this.fTarget = value;

                if (this.fTarget != null)
                {
                    ICulture culture = this.fBase.Context.Culture;
                    INamesTable namesTable = MainWin.Instance.NamesTable;

                    string surname, name, patronymic;
                    GKUtils.GetNameParts(this.fTarget, out surname, out name, out patronymic);
                    this.txtSurname.Text = surname;
                    GEDCOMSex sx = (GEDCOMSex)this.cmbSex.SelectedIndex;

                    switch (this.fTargetMode) {
                        case TargetMode.tmParent:
                            if (sx == GEDCOMSex.svFemale) {
                                this.SetMarriedSurname(surname);
                            }
                            if (culture.HasPatronymic()) {
                                this.cmbPatronymic.Items.Add(namesTable.GetPatronymicByName(name, GEDCOMSex.svMale));
                                this.cmbPatronymic.Items.Add(namesTable.GetPatronymicByName(name, GEDCOMSex.svFemale));
                                this.cmbPatronymic.Text = namesTable.GetPatronymicByName(name, sx);
                            }
                            break;

                        case TargetMode.tmChild:
                            switch (sx) {
                                case GEDCOMSex.svMale:
                                    if (culture.HasPatronymic()) {
                                        this.txtName.Text = namesTable.GetNameByPatronymic(patronymic);
                                    }
                                    break;

                                case GEDCOMSex.svFemale:
                                    this.SetMarriedSurname(surname);
                                    break;
                            }
                            break;

                        case TargetMode.tmWife:
                            this.SetMarriedSurname(surname);
                            break;
                    }
                }
            }
            catch (Exception ex)
            {
                this.fBase.Host.LogWrite("PersonEditDlg.SetTarget("+this.fTargetMode.ToString()+"): " + ex.Message);
            }
        }

        private void SetMarriedSurname(string husbSurname)
        {
            string surname = fBase.Context.Culture.GetMarriedSurname(husbSurname);
            if (this.IsExtendedWomanSurname()) {
                this.txtMarriedSurname.Text = surname;
            } else {
                this.txtSurname.Text = '(' + surname + ')';
            }
        }

        private bool IsExtendedWomanSurname()
        {
            bool result = (GlobalOptions.Instance.WomanSurnameFormat != WomanSurnameFormat.wsfNotExtend) &&
                (this.cmbSex.SelectedIndex == (sbyte)GEDCOMSex.svFemale);
            return result;
        }

        private void cbSex_SelectedIndexChanged(object sender, EventArgs e)
        {
            if (!this.IsExtendedWomanSurname()) {
                this.lblSurname.Text = LangMan.LS(LSID.LSID_Surname);
                this.txtMarriedSurname.Enabled = false;
            } else {
                this.lblSurname.Text = LangMan.LS(LSID.LSID_MaidenSurname);
                this.txtMarriedSurname.Enabled = true;
            }

            this.UpdatePortrait(true);
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
                    this.txtFather.Text = GKUtils.GetNameString(relPerson, true, false);
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
                    this.txtMother.Text = GKUtils.GetNameString(relPerson, true, false);
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
            this.cmbPatronymic.Enabled = !locked;
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

            ICulture culture = this.fBase.Context.Culture;
            this.txtSurname.Enabled = this.txtSurname.Enabled && culture.HasSurname();
            this.cmbPatronymic.Enabled = this.cmbPatronymic.Enabled && culture.HasPatronymic();
        }

        private void UpdatePortrait(bool totalUpdate)
        {
            if (fPortraitImg == null || totalUpdate) {
                fPortraitImg = this.fBase.Context.GetPrimaryBitmap(this.fPerson, this.imgPortrait.Width, this.imgPortrait.Height, false);
            }

            Image img = fPortraitImg;
            if (img == null) {
                // using avatar's image
                GEDCOMSex curSex = (GEDCOMSex)this.cmbSex.SelectedIndex;

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
            this.imgPortrait.Image = img;

            bool locked = (this.cmbRestriction.SelectedIndex == (int)GEDCOMRestriction.rnLocked);
            this.btnPortraitAdd.Enabled = !locked;
            this.btnPortraitDelete.Enabled = fPortraitImg != null && !locked;
        }

        private void cbRestriction_SelectedIndexChanged(object sender, EventArgs e)
        {
            this.UpdateControls();
        }

        private void AcceptChanges()
        {
            GEDCOMPersonalName np = this.fPerson.PersonalNames[0];
            GKUtils.SetRusNameParts(np, this.txtSurname.Text, this.txtName.Text, this.cmbPatronymic.Text);

            GEDCOMPersonalNamePieces pieces = np.Pieces;
            pieces.Nickname = this.txtNickname.Text;
            pieces.Prefix = this.txtNamePrefix.Text;
            pieces.SurnamePrefix = this.txtSurnamePrefix.Text;
            pieces.Suffix = this.txtNameSuffix.Text;
            if (this.IsExtendedWomanSurname()) {
                pieces.MarriedName = txtMarriedSurname.Text;
            }

            this.fPerson.Sex = (GEDCOMSex)this.cmbSex.SelectedIndex;
            this.fPerson.Patriarch = this.chkPatriarch.Checked;
            this.fPerson.Bookmark = this.chkBookmark.Checked;
            this.fPerson.Restriction = (GEDCOMRestriction)this.cmbRestriction.SelectedIndex;

            if (this.fPerson.ChildToFamilyLinks.Count > 0)
            {
                this.fPerson.ChildToFamilyLinks[0].Family.SortChilds();
            }

            this.fLocalUndoman.Commit();

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

        private void btnCancel_Click(object sender, EventArgs e)
        {
            try
            {
                this.fLocalUndoman.Rollback();
            }
            catch (Exception ex)
            {
                this.fBase.Host.LogWrite("PersonEditDlg.btnCancel_Click(): " + ex.Message);
            }
        }

        private void AcceptTempData()
        {
            // It is very important for some methods
            // For the sample: we need to have gender's value on time of call AddSpouse (for define husband/wife)
            // And we need to have actual name's value for visible it in FamilyEditDlg

            this.fLocalUndoman.DoOrdinaryOperation(OperationType.otIndividualSexChange, this.fPerson, (GEDCOMSex)this.cmbSex.SelectedIndex);
            this.fLocalUndoman.DoIndividualNameChange(this.fPerson, this.txtSurname.Text, this.txtName.Text, this.cmbPatronymic.Text);
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
                                //this.fPerson.Associations.Add(ast);
                                result = this.fLocalUndoman.DoOrdinaryOperation(OperationType.otIndividualAssociationAdd, this.fPerson, ast);
                            } else {
                                ast.Dispose();
                            }
                        }
                    }
                    break;

                case RecordAction.raDelete:
                    if (GKUtils.ShowQuestion(LangMan.LS(LSID.LSID_RemoveAssociationQuery)) != DialogResult.No)
                    {
                        //this.fPerson.Associations.Delete(ast);
                        //result = true;
                        result = this.fLocalUndoman.DoOrdinaryOperation(OperationType.otIndividualAssociationRemove, this.fPerson, ast);
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
                                //this.fPerson.UserReferences.Add(userRef);
                                result = this.fLocalUndoman.DoOrdinaryOperation(OperationType.otIndividualURefAdd, this.fPerson, userRef);
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
                            //this.fPerson.UserReferences.Delete(userRef);
                            //result = true;
                            result = this.fLocalUndoman.DoOrdinaryOperation(OperationType.otIndividualURefRemove, this.fPerson, userRef);
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
                            relName = GKUtils.GetNameString(relPerson, true, false);
                        }

                        GKListItem item = this.fSpousesList.AddItem(idx, family);
                        item.AddSubItem(relName);
                        item.AddSubItem(new GEDCOMDateItem(GKUtils.GetMarriageDate(family)));
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
                    this.AcceptTempData();
                    result = (this.fBase.ModifyFamily(ref family, FamilyTarget.Spouse, this.fPerson));
                    if (result) {
                        eArgs.ItemData = family;
                    }
                    break;

                case RecordAction.raEdit:
                    this.AcceptTempData();
                    result = (this.fBase.ModifyFamily(ref family, FamilyTarget.None, null));
                    break;

                case RecordAction.raDelete:
                    if (family != null && GKUtils.ShowQuestion(LangMan.LS(LSID.LSID_DetachSpouseQuery)) != DialogResult.No)
                    {
                        //family.RemoveSpouse(this.fPerson);
                        //result = true;
                        result = this.fLocalUndoman.DoOrdinaryOperation(OperationType.otFamilySpouseDetach, family, this.fPerson);
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
                    result = (groupRec != null);
                    if (result) {
                        //result = groupRec.AddMember(this.fPerson);
                        result = this.fLocalUndoman.DoOrdinaryOperation(OperationType.otGroupMemberAttach, groupRec, this.fPerson);
                    }
                    break;

                case RecordAction.raDelete:
                    result = (GKUtils.ShowQuestion(LangMan.LS(LSID.LSID_DetachGroupQuery)) != DialogResult.No);
                    if (result) {
                        //result = groupRec.RemoveMember(this.fPerson);
                        result = this.fLocalUndoman.DoOrdinaryOperation(OperationType.otGroupMemberDetach, groupRec, this.fPerson);
                    }
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
                                //this.fPerson.PersonalNames.Add(persName);
                                result = this.fLocalUndoman.DoOrdinaryOperation(OperationType.otIndividualNameAdd, this.fPerson, persName);
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
                            //this.fPerson.PersonalNames.Delete(persName);
                            result = this.fLocalUndoman.DoOrdinaryOperation(OperationType.otIndividualNameRemove, this.fPerson, persName);
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
                                      this.cmbPatronymic.Text, this.fPerson.GetXRefNum());
        }

        private void edSurname_TextChanged(object sender, EventArgs e)
        {
            this.SetTitle();
        }

        private void btnFatherAdd_Click(object sender, EventArgs e)
        {
            GEDCOMIndividualRecord father = this.fBase.SelectPerson(this.fPerson, TargetMode.tmChild, GEDCOMSex.svMale);
            if (father == null) return;

            GEDCOMFamilyRecord family = this.fBase.GetChildFamily(this.fPerson, true, father);
            if (family != null && family.Husband.Value == null) {
                //family.AddSpouse(father);
                this.fLocalUndoman.DoOrdinaryOperation(OperationType.otFamilySpouseAttach, family, father);
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
                    GEDCOMIndividualRecord father = family.GetHusband();
                    //family.RemoveSpouse(father);
                    this.fLocalUndoman.DoOrdinaryOperation(OperationType.otFamilySpouseDetach, family, father);
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
                //family.AddSpouse(mother);
                this.fLocalUndoman.DoOrdinaryOperation(OperationType.otFamilySpouseAttach, family, mother);
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
                    //family.RemoveSpouse(mother);
                    this.fLocalUndoman.DoOrdinaryOperation(OperationType.otFamilySpouseDetach, family, mother);
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
            this.AcceptTempData();

            GEDCOMFamilyRecord family = this.fBase.SelectFamily(this.fPerson);
            if (family == null) return;

            if (family.IndexOfChild(this.fPerson) < 0)
            {
                //family.AddChild(this.fPerson);
                this.fLocalUndoman.DoOrdinaryOperation(OperationType.otIndividualParentsAttach, this.fPerson, family);
            }
            this.UpdateControls();
        }

        private void btnParentsEdit_Click(object sender, EventArgs e)
        {
            this.AcceptTempData();

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
                    //family.RemoveChild(this.fPerson);
                    this.fLocalUndoman.DoOrdinaryOperation(OperationType.otIndividualParentsDetach, this.fPerson, family);
                    this.UpdateControls();
                }
            }
        }

        private void btnNameCopy_Click(object sender, EventArgs e)
        {
            Clipboard.SetDataObject(GKUtils.GetNameString(this.fPerson, true, false));
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
            this.InitializeComponent();

            this.btnAccept.Image = GKResources.iBtnAccept;
            this.btnCancel.Image = GKResources.iBtnCancel;
            this.btnPortraitAdd.Image = GKResources.iRecNew;
            this.btnPortraitDelete.Image = GKResources.iRecDelete;
            this.btnParentsAdd.Image = GKResources.iRecNew;
            this.btnParentsEdit.Image = GKResources.iRecEdit;
            this.btnParentsDelete.Image = GKResources.iRecDelete;
            this.btnFatherAdd.Image = GKResources.iRecNew;
            this.btnFatherDelete.Image = GKResources.iRecEdit;
            this.btnFatherSel.Image = GKResources.iRecDelete;
            this.btnMotherAdd.Image = GKResources.iRecNew;
            this.btnMotherDelete.Image = GKResources.iRecEdit;
            this.btnMotherSel.Image = GKResources.iRecDelete;
            this.btnNameCopy.Image = GKResources.iCopy;

            for (GEDCOMRestriction res = GEDCOMRestriction.rnNone; res <= GEDCOMRestriction.rnPrivacy; res++)
            {
                this.cmbRestriction.Items.Add(LangMan.LS(GKData.Restrictions[(int)res]));
            }

            for (GEDCOMSex sx = GEDCOMSex.svNone; sx <= GEDCOMSex.svUndetermined; sx++)
            {
                this.cmbSex.Items.Add(GKUtils.SexStr(sx));
            }

            this.fEventsList = new GKEventsSheet(this, this.pageEvents, true, this.fLocalUndoman);
            this.fEventsList.SetControlName("fEventsList"); // for purpose of tests

            this.fSpousesList = this.CreateSpousesSheet(this.pageSpouses);
            this.fSpousesList.SetControlName("fSpousesList"); // for purpose of tests

            this.fNamesList = this.CreateNamesSheet(this.pageNames);
            this.fNamesList.SetControlName("fNamesList"); // for purpose of tests

            this.fAssociationsList = this.CreateAssociationsSheet(this.pageAssociations);
            this.fAssociationsList.SetControlName("fAssociationsList"); // for purpose of tests

            this.fGroupsList = this.CreateGroupsSheet(this.pageGroups);
            this.fGroupsList.SetControlName("fGroupsList"); // for purpose of tests

            this.fNotesList = new GKNotesSheet(this, this.pageNotes, this.fLocalUndoman);
            this.fNotesList.SetControlName("fNotesList"); // for purpose of tests

            this.fMediaList = new GKMediaSheet(this, this.pageMultimedia, this.fLocalUndoman);
            this.fMediaList.SetControlName("fMediaList"); // for purpose of tests

            this.fSourcesList = new GKSourcesSheet(this, this.pageSources, this.fLocalUndoman);
            this.fSourcesList.SetControlName("fSourcesList"); // for purpose of tests

            this.fUserRefList = this.CreateURefsSheet(this.pageUserRefs);
            this.fUserRefList.SetControlName("fUserRefList"); // for purpose of tests

            this.btnPortraitAdd.Image = GKResources.iRecNew;
            this.btnPortraitDelete.Image = GKResources.iRecDelete;
            this.btnFatherAdd.Image = GKResources.iRecNew;
            this.btnFatherDelete.Image = GKResources.iRecDelete;
            this.btnFatherSel.Image = GKResources.iToMan;
            this.btnMotherAdd.Image = GKResources.iRecNew;
            this.btnMotherDelete.Image = GKResources.iRecDelete;
            this.btnMotherSel.Image = GKResources.iToMan;
            this.btnParentsAdd.Image = GKResources.iRecNew;
            this.btnParentsEdit.Image = GKResources.iRecEdit;
            this.btnParentsDelete.Image = GKResources.iRecDelete;

            this.imgPortrait.SizeMode = PictureBoxSizeMode.CenterImage;

            this.SetLang();
        }

        public void SetLang()
        {
            this.btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
            this.btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
            this.Text = LangMan.LS(LSID.LSID_WinPersonEdit);
            this.lblSurname.Text = LangMan.LS(LSID.LSID_Surname);
            this.lblMarriedSurname.Text = LangMan.LS(LSID.LSID_MarriedSurname);
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
