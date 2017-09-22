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
using Eto.Drawing;
using Eto.Forms;

using GKCommon;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.Operations;
using GKCore.Options;
using GKCore.Types;
using GKCore.UIContracts;
using GKUI.Components;

namespace GKUI.Forms
{
    /// <summary>
    /// 
    /// </summary>
    public partial class PersonEditDlg : EditorDialog, IPersonEditDlg
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
        private IImage fPortraitImg;

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
                var parts = GKUtils.GetNameParts(fPerson, false);
                txtSurname.Text = parts.Surname;
                txtName.Text = parts.Name;

                cmbPatronymic.AutoComplete = true; // FIXME: Wrapper for EtoBug in ComboBox.setText
                cmbPatronymic.Text = parts.Patronymic;
                cmbPatronymic.AutoComplete = false;

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
                fAssociationsList.ListModel.DataOwner = fPerson;

                fGroupsList.ListModel.DataOwner = fPerson;
                fNamesList.ListModel.DataOwner = fPerson;
                fSpousesList.ListModel.DataOwner = fPerson;
                fUserRefList.ListModel.DataOwner = fPerson;

                UpdateControls(true);
            }
            catch (Exception ex)
            {
                Logger.LogWrite("PersonEditDlg.SetPerson(): " + ex.Message);
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
                    INamesTable namesTable = AppHost.NamesTable;

                    var parts = GKUtils.GetNameParts(fTarget);
                    txtSurname.Text = parts.Surname;
                    GEDCOMSex sx = (GEDCOMSex)cmbSex.SelectedIndex;

                    switch (fTargetMode) {
                        case TargetMode.tmParent:
                            if (sx == GEDCOMSex.svFemale) {
                                SetMarriedSurname(parts.Surname);
                            }
                            if (culture.HasPatronymic()) {
                                cmbPatronymic.Items.Add(namesTable.GetPatronymicByName(parts.Name, GEDCOMSex.svMale));
                                cmbPatronymic.Items.Add(namesTable.GetPatronymicByName(parts.Name, GEDCOMSex.svFemale));
                                cmbPatronymic.Text = namesTable.GetPatronymicByName(parts.Name, sx);
                            }
                            break;

                        case TargetMode.tmChild:
                            switch (sx) {
                                case GEDCOMSex.svMale:
                                    if (culture.HasPatronymic()) {
                                        txtName.Text = namesTable.GetNameByPatronymic(parts.Patronymic);
                                    }
                                    break;

                                case GEDCOMSex.svFemale:
                                    SetMarriedSurname(parts.Surname);
                                    break;
                            }
                            break;

                        case TargetMode.tmWife:
                            SetMarriedSurname(parts.Surname);
                            break;
                    }
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("PersonEditDlg.SetTarget("+fTargetMode.ToString()+"): " + ex.Message);
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
                fAssociationsList.UpdateSheet();

                fGroupsList.UpdateSheet();
                fNamesList.UpdateSheet();
                fSpousesList.UpdateSheet();
                fUserRefList.UpdateSheet();
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

            Image img = (fPortraitImg == null) ? null : ((ImageHandler)fPortraitImg).Handle;
            if (img == null) {
                // using avatar's image
                GEDCOMSex curSex = (GEDCOMSex)cmbSex.SelectedIndex;

                switch (curSex) {
                    case GEDCOMSex.svMale:
                        img = Bitmap.FromResource("Resources.pi_male_140.png");
                        break;

                    case GEDCOMSex.svFemale:
                        img = Bitmap.FromResource("Resources.pi_female_140.png");
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
            GKUtils.SetNameParts(np, txtSurname.Text, txtName.Text, cmbPatronymic.Text);

            GEDCOMPersonalNamePieces pieces = np.Pieces;
            pieces.Nickname = txtNickname.Text;
            pieces.Prefix = txtNamePrefix.Text;
            pieces.SurnamePrefix = txtSurnamePrefix.Text;
            pieces.Suffix = txtNameSuffix.Text;
            if (IsExtendedWomanSurname()) {
                pieces.MarriedName = txtMarriedSurname.Text;
            }

            fPerson.Sex = (GEDCOMSex)cmbSex.SelectedIndex;
            fPerson.Patriarch = chkPatriarch.Checked.GetValueOrDefault();
            fPerson.Bookmark = chkBookmark.Checked.GetValueOrDefault();
            fPerson.Restriction = (GEDCOMRestriction)cmbRestriction.SelectedIndex;

            if (fPerson.ChildToFamilyLinks.Count > 0)
            {
                fPerson.ChildToFamilyLinks[0].Family.SortChilds();
            }

            fLocalUndoman.Commit();

            fBase.NotifyRecord(fPerson, RecordAction.raEdit);
        }

        private void btnAccept_Click(object sender, EventArgs e)
        {
            try
            {
                AcceptChanges();
                DialogResult = DialogResult.Ok;
            }
            catch (Exception ex)
            {
                Logger.LogWrite("PersonEditDlg.btnAccept_Click(): " + ex.Message);
                DialogResult = DialogResult.None;
            }
        }

        private void btnCancel_Click(object sender, EventArgs e)
        {
            try
            {
                fLocalUndoman.Rollback();
                CancelClickHandler(sender, e);
            }
            catch (Exception ex)
            {
                Logger.LogWrite("PersonEditDlg.btnCancel_Click(): " + ex.Message);
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

        private void ModifyAssociationsSheet(object sender, ModifyEventArgs eArgs)
        {
            GEDCOMAssociation ast = eArgs.ItemData as GEDCOMAssociation;
            if (eArgs.Action == RecordAction.raJump && ast != null) {
                AcceptChanges();
                fBase.SelectRecordByXRef(ast.Individual.XRef);
                Close();
            }
        }

        private void ModifySpousesSheet(object sender, ModifyEventArgs eArgs)
        {
            GEDCOMFamilyRecord family = eArgs.ItemData as GEDCOMFamilyRecord;
            if (eArgs.Action == RecordAction.raJump) {
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

                    if (sp != null) {
                        GEDCOMIndividualRecord spouse = (GEDCOMIndividualRecord)sp.Value;
                        AcceptChanges();
                        fBase.SelectRecordByXRef(spouse.XRef);
                        Close();
                    }
                }
            }
        }

        private void ModifyGroupsSheet(object sender, ModifyEventArgs eArgs)
        {
            GEDCOMGroupRecord groupRec = eArgs.ItemData as GEDCOMGroupRecord;
            if (eArgs.Action == RecordAction.raJump && groupRec != null) {
                AcceptChanges();
                fBase.SelectRecordByXRef(groupRec.XRef);
                Close();
            }
        }

        private void Names_TextChanged(object sender, EventArgs e)
        {
            Title = string.Format("{0} \"{1} {2} {3}\" [{4}]", LangMan.LS(LSID.LSID_Person), txtSurname.Text, txtName.Text,
                                 cmbPatronymic.Text, fPerson.GetXRefNum());
        }

        private void btnFatherAdd_Click(object sender, EventArgs e)
        {
            if (BaseController.AddIndividualFather(fBase, fLocalUndoman, fPerson)) {
                UpdateControls();
            }
        }

        private void btnFatherDelete_Click(object sender, EventArgs e)
        {
            if (BaseController.DeleteIndividualFather(fBase, fLocalUndoman, fPerson)) {
                UpdateControls();
            }
        }

        private void btnFatherSel_Click(object sender, EventArgs e)
        {
            GEDCOMFamilyRecord family = fBase.Context.GetChildFamily(fPerson, false, null);
            if (family == null) return;

            AcceptChanges();
            GEDCOMIndividualRecord father = family.GetHusband();
            fBase.SelectRecordByXRef(father.XRef);
            Close();
        }

        private void btnMotherAdd_Click(object sender, EventArgs e)
        {
            if (BaseController.AddIndividualMother(fBase, fLocalUndoman, fPerson)) {
                UpdateControls();
            }
        }

        private void btnMotherDelete_Click(object sender, EventArgs e)
        {
            if (BaseController.DeleteIndividualMother(fBase, fLocalUndoman, fPerson)) {
                UpdateControls();
            }
        }

        private void btnMotherSel_Click(object sender, EventArgs e)
        {
            GEDCOMFamilyRecord family = fBase.Context.GetChildFamily(fPerson, false, null);
            if (family == null) return;

            AcceptChanges();
            GEDCOMIndividualRecord mother = family.GetWife();
            fBase.SelectRecordByXRef(mother.XRef);
            Close();
        }

        private void btnParentsAdd_Click(object sender, EventArgs e)
        {
            AcceptTempData();

            GEDCOMFamilyRecord family = fBase.Context.SelectFamily(fPerson);
            if (family == null) return;

            if (family.IndexOfChild(fPerson) < 0)
            {
                fLocalUndoman.DoOrdinaryOperation(OperationType.otIndividualParentsAttach, fPerson, family);
            }
            UpdateControls();
        }

        private void btnParentsEdit_Click(object sender, EventArgs e)
        {
            AcceptTempData();

            GEDCOMFamilyRecord family = fBase.Context.GetChildFamily(fPerson, false, null);
            if (family != null && BaseController.ModifyFamily(fBase, ref family, TargetMode.tmNone, null))
            {
                UpdateControls();
            }
        }

        private void btnParentsDelete_Click(object sender, EventArgs e)
        {
            if (AppHost.StdDialogs.ShowQuestionYN(LangMan.LS(LSID.LSID_DetachParentsQuery)) == false) return;

            GEDCOMFamilyRecord family = fBase.Context.GetChildFamily(fPerson, false, null);
            if (family == null) return;

            fLocalUndoman.DoOrdinaryOperation(OperationType.otIndividualParentsDetach, fPerson, family);
            UpdateControls();
        }

        private void btnNameCopy_Click(object sender, EventArgs e)
        {
            using (var clipboard = new Clipboard()) {
                clipboard.Text = GKUtils.GetNameString(fPerson, true, false);
            }
        }

        private void btnPortraitAdd_Click(object sender, EventArgs e)
        {
            if (BaseController.AddIndividualPortrait(fBase, fPerson)) {
                fMediaList.UpdateSheet();
                UpdatePortrait(true);
            }
        }

        private void btnPortraitDelete_Click(object sender, EventArgs e)
        {
            if (BaseController.DeleteIndividualPortrait(fBase, fPerson)) {
                UpdatePortrait(true);
            }
        }

        private void edNameX_KeyDown(object sender, KeyEventArgs e)
        {
            TextBox tb = (sender as TextBox);
            if (tb != null && e.Key == Keys.Down && e.Control) {
                tb.Text = SysUtils.NormalizeName(tb.Text);
            } else if (e.KeyChar == '/') {
                e.Handled = true;
            }
        }

        public void SetNeedSex(GEDCOMSex needSex)
        {
            cmbSex.SelectedIndex = (int)needSex;
        }

        public PersonEditDlg()
        {
            InitializeComponent();

            txtMarriedSurname.TextChanged += Names_TextChanged;
            txtSurname.TextChanged += Names_TextChanged;
            txtName.TextChanged += Names_TextChanged;
            cmbPatronymic.TextChanged += Names_TextChanged;

            btnPortraitAdd.Image = Bitmap.FromResource("Resources.btn_rec_new.gif");
            btnPortraitDelete.Image = Bitmap.FromResource("Resources.btn_rec_delete.gif");
            btnParentsAdd.Image = Bitmap.FromResource("Resources.btn_rec_new.gif");
            btnParentsEdit.Image = Bitmap.FromResource("Resources.btn_rec_edit.gif");
            btnParentsDelete.Image = Bitmap.FromResource("Resources.btn_rec_delete.gif");
            btnFatherAdd.Image = Bitmap.FromResource("Resources.btn_rec_new.gif");
            btnFatherDelete.Image = Bitmap.FromResource("Resources.btn_rec_edit.gif");
            btnFatherSel.Image = Bitmap.FromResource("Resources.btn_rec_delete.gif");
            btnMotherAdd.Image = Bitmap.FromResource("Resources.btn_rec_new.gif");
            btnMotherDelete.Image = Bitmap.FromResource("Resources.btn_rec_edit.gif");
            btnMotherSel.Image = Bitmap.FromResource("Resources.btn_rec_delete.gif");
            btnNameCopy.Image = Bitmap.FromResource("Resources.btn_copy.gif");

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

            fEventsList = new GKSheetList(pageEvents);

            fSpousesList = new GKSheetList(pageSpouses);
            fSpousesList.OnModify += ModifySpousesSheet;

            fNamesList = new GKSheetList(pageNames);

            fAssociationsList = new GKSheetList(pageAssociations);
            fAssociationsList.OnModify += ModifyAssociationsSheet;

            fGroupsList = new GKSheetList(pageGroups);
            fGroupsList.OnModify += ModifyGroupsSheet;

            fNotesList = new GKSheetList(pageNotes);

            fMediaList = new GKSheetList(pageMultimedia);

            fSourcesList = new GKSheetList(pageSources);

            fUserRefList = new GKSheetList(pageUserRefs);

            btnPortraitAdd.Image = Bitmap.FromResource("Resources.btn_rec_new.gif");
            btnPortraitDelete.Image = Bitmap.FromResource("Resources.btn_rec_delete.gif");
            btnFatherAdd.Image = Bitmap.FromResource("Resources.btn_rec_new.gif");
            btnFatherDelete.Image = Bitmap.FromResource("Resources.btn_rec_delete.gif");
            btnFatherSel.Image = Bitmap.FromResource("Resources.btn_jump.gif");
            btnMotherAdd.Image = Bitmap.FromResource("Resources.btn_rec_new.gif");
            btnMotherDelete.Image = Bitmap.FromResource("Resources.btn_rec_delete.gif");
            btnMotherSel.Image = Bitmap.FromResource("Resources.btn_jump.gif");
            btnParentsAdd.Image = Bitmap.FromResource("Resources.btn_rec_new.gif");
            btnParentsEdit.Image = Bitmap.FromResource("Resources.btn_rec_edit.gif");
            btnParentsDelete.Image = Bitmap.FromResource("Resources.btn_rec_delete.gif");

            SetLang();
        }

        public void SetLang()
        {
            btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
            btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
            Title = LangMan.LS(LSID.LSID_WinPersonEdit);
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

            btnPortraitAdd.ToolTip = LangMan.LS(LSID.LSID_PortraitAddTip);
            btnPortraitDelete.ToolTip = LangMan.LS(LSID.LSID_PortraitDeleteTip);
            btnParentsAdd.ToolTip = LangMan.LS(LSID.LSID_ParentsAddTip);
            btnParentsEdit.ToolTip = LangMan.LS(LSID.LSID_ParentsEditTip);
            btnParentsDelete.ToolTip = LangMan.LS(LSID.LSID_ParentsDeleteTip);
            btnFatherAdd.ToolTip = LangMan.LS(LSID.LSID_FatherAddTip);
            btnFatherDelete.ToolTip = LangMan.LS(LSID.LSID_FatherDeleteTip);
            btnFatherSel.ToolTip = LangMan.LS(LSID.LSID_FatherSelTip);
            btnMotherAdd.ToolTip = LangMan.LS(LSID.LSID_MotherAddTip);
            btnMotherDelete.ToolTip = LangMan.LS(LSID.LSID_MotherDeleteTip);
            btnMotherSel.ToolTip = LangMan.LS(LSID.LSID_MotherSelTip);
            btnNameCopy.ToolTip = LangMan.LS(LSID.LSID_NameCopyTip);
        }

        public override void InitDialog(IBaseWindow baseWin)
        {
            base.InitDialog(baseWin);

            fEventsList.ListModel = new EventsListModel(fBase, fLocalUndoman, true);
            fNotesList.ListModel = new NoteLinksListModel(fBase, fLocalUndoman);
            fMediaList.ListModel = new MediaLinksListModel(fBase, fLocalUndoman);
            fSourcesList.ListModel = new SourceCitationsListModel(fBase, fLocalUndoman);
            fAssociationsList.ListModel = new AssociationsListModel(fBase, fLocalUndoman);

            fGroupsList.ListModel = new GroupsSublistModel(fBase, fLocalUndoman);
            fNamesList.ListModel = new NamesSublistModel(fBase, fLocalUndoman);
            fSpousesList.ListModel = new SpousesSublistModel(fBase, fLocalUndoman);
            fUserRefList.ListModel = new URefsSublistModel(fBase, fLocalUndoman);
        }
    }
}
