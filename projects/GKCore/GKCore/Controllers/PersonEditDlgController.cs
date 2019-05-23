/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2018 by Sergey V. Zhdanovskih.
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

#define SEX_SYMBOLS

using System;
using GDModel;
using GKCore.Interfaces;
using GKCore.MVP;
using GKCore.MVP.Views;
using GKCore.Operations;
using GKCore.Options;
using GKCore.Types;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class PersonEditDlgController : DialogController<IPersonEditDlg>
    {
        private GDMIndividualRecord fPerson;
        private IImage fPortraitImg;
        private GDMIndividualRecord fTarget;
        private TargetMode fTargetMode;

        public GDMIndividualRecord Person
        {
            get { return fPerson; }
            set {
                if (fPerson != value) {
                    fPerson = value;
                    UpdateView();
                }
            }
        }

        public GDMIndividualRecord Target
        {
            get { return fTarget; }
            set { SetTarget(value); }
        }

        public TargetMode TargetMode
        {
            get { return fTargetMode; }
            set { fTargetMode = value; }
        }


        public PersonEditDlgController(IPersonEditDlg view) : base(view)
        {
            for (GDMRestriction res = GDMRestriction.rnNone; res <= GDMRestriction.rnPrivacy; res++) {
                fView.RestrictionCombo.Add(LangMan.LS(GKData.Restrictions[(int)res]));
            }

            for (GDMSex sx = GDMSex.svNone; sx <= GDMSex.svUndetermined; sx++) {
                string name = GKUtils.SexStr(sx);
                IImage image = null;
                #if SEX_SYMBOLS
                switch (sx) {
                    case GDMSex.svMale:
                        image = AppHost.GfxProvider.LoadResourceImage("sym_male.png", true);
                        break;
                    case GDMSex.svFemale:
                        image = AppHost.GfxProvider.LoadResourceImage("sym_female.png", true);
                        break;
                }
                #endif
                fView.SexCombo.AddItem(name, null, image);
            }
        }

        private bool IsExtendedWomanSurname()
        {
            bool result = (GlobalOptions.Instance.WomanSurnameFormat != WomanSurnameFormat.wsfNotExtend) &&
                (fView.SexCombo.SelectedIndex == (sbyte)GDMSex.svFemale);
            return result;
        }

        public void ChangeSex()
        {
            if (!IsExtendedWomanSurname()) {
                fView.SurnameLabel.Text = LangMan.LS(LSID.LSID_Surname);
                fView.MarriedSurname.Enabled = false;
            } else {
                fView.SurnameLabel.Text = LangMan.LS(LSID.LSID_MaidenSurname);
                fView.MarriedSurname.Enabled = true;
            }

            UpdatePortrait(true);
        }

        public override bool Accept()
        {
            try {
                GDMPersonalName np = fPerson.PersonalNames[0];
                GKUtils.SetNameParts(np, fView.Surname.Text, fView.Name.Text, fView.Patronymic.Text);

                GDMPersonalNamePieces pieces = np.Pieces;
                pieces.Nickname = fView.Nickname.Text;
                pieces.Prefix = fView.NamePrefix.Text;
                pieces.SurnamePrefix = fView.SurnamePrefix.Text;
                pieces.Suffix = fView.NameSuffix.Text;
                if (IsExtendedWomanSurname()) {
                    pieces.MarriedName = fView.MarriedSurname.Text;
                }

                fPerson.Sex = (GDMSex)fView.SexCombo.SelectedIndex;
                fPerson.Patriarch = fView.Patriarch.Checked;
                fPerson.Bookmark = fView.Bookmark.Checked;
                fPerson.Restriction = (GDMRestriction)fView.RestrictionCombo.SelectedIndex;

                fBase.Context.ProcessIndividual(fPerson);

                fLocalUndoman.Commit();

                fBase.NotifyRecord(fPerson, RecordAction.raEdit);

                return true;
            } catch (Exception ex) {
                Logger.LogWrite("PersonEditDlgController.Accept(): " + ex.Message);
                return false;
            }
        }

        public override void UpdateView()
        {
            try {
                GDMPersonalName np = (fPerson.PersonalNames.Count > 0) ? fPerson.PersonalNames[0] : null;
                UpdateNameControls(np);

                fView.SexCombo.SelectedIndex = (sbyte)fPerson.Sex;
                fView.Patriarch.Checked = fPerson.Patriarch;
                fView.Bookmark.Checked = fPerson.Bookmark;

                //cmbRestriction.SelectedIndexChanged -= cbRestriction_SelectedIndexChanged;
                fView.RestrictionCombo.SelectedIndex = (sbyte)fPerson.Restriction;
                //cmbRestriction.SelectedIndexChanged += cbRestriction_SelectedIndexChanged;

                fView.EventsList.ListModel.DataOwner = fPerson;
                fView.NotesList.ListModel.DataOwner = fPerson;
                fView.MediaList.ListModel.DataOwner = fPerson;
                fView.SourcesList.ListModel.DataOwner = fPerson;
                fView.AssociationsList.ListModel.DataOwner = fPerson;

                fView.GroupsList.ListModel.DataOwner = fPerson;
                fView.NamesList.ListModel.DataOwner = fPerson;
                fView.SpousesList.ListModel.DataOwner = fPerson;
                fView.UserRefList.ListModel.DataOwner = fPerson;
                fView.ParentsList.ListModel.DataOwner = fPerson;

                UpdateControls(true);
            } catch (Exception ex) {
                Logger.LogWrite("PersonEditDlgController.UpdateView(): " + ex.Message);
            }
        }

        public void UpdateParents()
        {
            bool locked = (fView.RestrictionCombo.SelectedIndex == (int)GDMRestriction.rnLocked);

            if (fPerson.ChildToFamilyLinks.Count != 0) {
                GDMFamilyRecord family = fPerson.ChildToFamilyLinks[0].Family;
                fView.SetParentsAvl(true, locked);

                GDMIndividualRecord relPerson = family.GetHusband();
                if (relPerson != null) {
                    fView.SetFatherAvl(true, locked);
                    fView.Father.Text = GKUtils.GetNameString(relPerson, true, false);
                } else {
                    fView.SetFatherAvl(false, locked);
                    fView.Father.Text = "";
                }

                relPerson = family.GetWife();
                if (relPerson != null) {
                    fView.SetMotherAvl(true, locked);
                    fView.Mother.Text = GKUtils.GetNameString(relPerson, true, false);
                } else {
                    fView.SetMotherAvl(false, locked);
                    fView.Mother.Text = "";
                }
            } else {
                fView.SetParentsAvl(false, locked);
                fView.SetFatherAvl(false, locked);
                fView.SetMotherAvl(false, locked);

                fView.Father.Text = "";
                fView.Mother.Text = "";
            }
        }

        public void UpdateControls(bool totalUpdate = false)
        {
            UpdateParents();

            if (totalUpdate) {
                fView.EventsList.UpdateSheet();
                fView.NotesList.UpdateSheet();
                fView.MediaList.UpdateSheet();
                fView.SourcesList.UpdateSheet();
                fView.AssociationsList.UpdateSheet();

                fView.GroupsList.UpdateSheet();
                fView.NamesList.UpdateSheet();
                fView.SpousesList.UpdateSheet();
                fView.UserRefList.UpdateSheet();
                fView.ParentsList.UpdateSheet();
            }

            UpdatePortrait(totalUpdate);

            bool locked = (fView.RestrictionCombo.SelectedIndex == (int)GDMRestriction.rnLocked);
            ICulture culture = fBase.Context.Culture;

            // controls lock
            fView.Name.Enabled = !locked;
            fView.Patronymic.Enabled = !locked && culture.HasPatronymic();
            fView.Surname.Enabled = !locked && culture.HasSurname();

            fView.SexCombo.Enabled = !locked;
            fView.Patriarch.Enabled = !locked;
            fView.Bookmark.Enabled = !locked;

            fView.NamePrefix.Enabled = !locked;
            fView.Nickname.Enabled = !locked;
            fView.SurnamePrefix.Enabled = !locked;
            fView.NameSuffix.Enabled = !locked;

            fView.EventsList.ReadOnly = locked;
            fView.NotesList.ReadOnly = locked;
            fView.MediaList.ReadOnly = locked;
            fView.SourcesList.ReadOnly = locked;
            fView.SpousesList.ReadOnly = locked;
            fView.AssociationsList.ReadOnly = locked;
            fView.GroupsList.ReadOnly = locked;
            fView.UserRefList.ReadOnly = locked;
            fView.ParentsList.ReadOnly = locked;
        }

        public void UpdateNameControls(GDMPersonalName np)
        {
            if (np != null) {
                var parts = GKUtils.GetNameParts(fPerson, np, false);

                fView.Surname.Text = parts.Surname;
                fView.Name.Text = parts.Name;
                fView.Patronymic.Text = parts.Patronymic;

                fView.NamePrefix.Text = np.Pieces.Prefix;
                fView.Nickname.Text = np.Pieces.Nickname;
                fView.SurnamePrefix.Text = np.Pieces.SurnamePrefix;
                fView.NameSuffix.Text = np.Pieces.Suffix;

                fView.MarriedSurname.Text = np.Pieces.MarriedName;
            } else {
                fView.Surname.Text = "";
                fView.Name.Text = "";
                fView.Patronymic.Text = "";

                fView.NamePrefix.Text = "";
                fView.Nickname.Text = "";
                fView.SurnamePrefix.Text = "";
                fView.NameSuffix.Text = "";

                fView.MarriedSurname.Text = "";
            }
        }

        public void UpdatePortrait(bool totalUpdate)
        {
            if (fPortraitImg == null || totalUpdate) {
                fPortraitImg = fBase.Context.GetPrimaryBitmap(fPerson, fView.Portrait.Width, fView.Portrait.Height, false);
            }

            IImage img = fPortraitImg;
            if (img == null) {
                // using avatar's image
                GDMSex curSex = (GDMSex)fView.SexCombo.SelectedIndex;

                switch (curSex) {
                    case GDMSex.svMale:
                        img = AppHost.GfxProvider.LoadResourceImage("pi_male_140.png", false);
                        break;

                    case GDMSex.svFemale:
                        img = AppHost.GfxProvider.LoadResourceImage("pi_female_140.png", false);
                        break;

                    default:
                        break;
                }
            }
            fView.SetPortrait(img);

            bool locked = (fView.RestrictionCombo.SelectedIndex == (int)GDMRestriction.rnLocked);
            fView.SetPortraitAvl((fPortraitImg != null), locked);
        }

        private void SetTarget(GDMIndividualRecord value)
        {
            try {
                fTarget = value;

                if (fTarget != null) {
                    ICulture culture = fBase.Context.Culture;
                    INamesTable namesTable = AppHost.NamesTable;

                    var parts = GKUtils.GetNameParts(fTarget);
                    fView.Surname.Text = parts.Surname;
                    GDMSex sx = (GDMSex)fView.SexCombo.SelectedIndex;

                    switch (fTargetMode) {
                        case TargetMode.tmParent:
                            if (sx == GDMSex.svFemale) {
                                SetMarriedSurname(parts.Surname);
                            }
                            if (culture.HasPatronymic()) {
                                AddPatronymic(namesTable.GetPatronymicByName(parts.Name, GDMSex.svMale));
                                AddPatronymic(namesTable.GetPatronymicByName(parts.Name, GDMSex.svFemale));
                                fView.Patronymic.Text = namesTable.GetPatronymicByName(parts.Name, sx);
                            }
                            break;

                        case TargetMode.tmChild:
                            switch (sx) {
                                case GDMSex.svMale:
                                    if (culture.HasPatronymic()) {
                                        fView.Name.Text = namesTable.GetNameByPatronymic(parts.Patronymic);
                                    }
                                    break;

                                case GDMSex.svFemale:
                                    SetMarriedSurname(parts.Surname);
                                    break;
                            }
                            break;

                        case TargetMode.tmWife:
                            SetMarriedSurname(parts.Surname);
                            break;
                    }
                }
            } catch (Exception ex) {
                Logger.LogWrite("PersonEditDlg.SetTarget(" + fTargetMode.ToString() + "): " + ex.Message);
            }
        }

        private void AddPatronymic(string patronymic)
        {
            if (!string.IsNullOrEmpty(patronymic)) {
                fView.Patronymic.Add(patronymic);
            }
        }

        private void SetMarriedSurname(string husbSurname)
        {
            string surname = fBase.Context.Culture.GetMarriedSurname(husbSurname);
            if (IsExtendedWomanSurname()) {
                fView.MarriedSurname.Text = surname;
            } else {
                fView.Surname.Text = "(" + surname + ")";
            }
        }

        public void AcceptTempData()
        {
            // It is very important for some methods
            // For the sample: we need to have gender's value on time of call AddSpouse (for define husband/wife)
            // And we need to have actual name's value for visible it in FamilyEditDlg

            fLocalUndoman.DoOrdinaryOperation(OperationType.otIndividualSexChange, fPerson, (GDMSex)fView.SexCombo.SelectedIndex);
            fLocalUndoman.DoIndividualNameChange(fPerson, fView.Surname.Text, fView.Name.Text, fView.Patronymic.Text);
        }

        public void AddPortrait()
        {
            if (BaseController.AddIndividualPortrait(fBase, fLocalUndoman, fPerson)) {
                fView.MediaList.UpdateSheet();
                UpdatePortrait(true);
            }
        }

        public void DeletePortrait()
        {
            if (BaseController.DeleteIndividualPortrait(fBase, fLocalUndoman, fPerson)) {
                UpdatePortrait(true);
            }
        }

        public void AddParents()
        {
            AcceptTempData();

            GDMFamilyRecord family = fBase.Context.SelectFamily(fPerson);
            if (family != null && family.IndexOfChild(fPerson) < 0) {
                fLocalUndoman.DoOrdinaryOperation(OperationType.otIndividualParentsAttach, fPerson, family);
            }
            UpdateControls();
        }

        public void EditParents()
        {
            AcceptTempData();

            GDMFamilyRecord family = fBase.Context.GetChildFamily(fPerson, false, null);
            if (family != null && BaseController.ModifyFamily(fBase, ref family, TargetMode.tmNone, null)) {
                UpdateControls();
            }
        }

        public void DeleteParents()
        {
            if (!AppHost.StdDialogs.ShowQuestionYN(LangMan.LS(LSID.LSID_DetachParentsQuery))) return;

            GDMFamilyRecord family = fBase.Context.GetChildFamily(fPerson, false, null);
            if (family == null) return;

            fLocalUndoman.DoOrdinaryOperation(OperationType.otIndividualParentsDetach, fPerson, family);
            UpdateControls();
        }

        public void AddFather()
        {
            if (BaseController.AddIndividualFather(fBase, fLocalUndoman, fPerson)) {
                UpdateControls();
            }
        }

        public void DeleteFather()
        {
            if (BaseController.DeleteIndividualFather(fBase, fLocalUndoman, fPerson)) {
                UpdateControls();
            }
        }

        public void AddMother()
        {
            if (BaseController.AddIndividualMother(fBase, fLocalUndoman, fPerson)) {
                UpdateControls();
            }
        }

        public void DeleteMother()
        {
            if (BaseController.DeleteIndividualMother(fBase, fLocalUndoman, fPerson)) {
                UpdateControls();
            }
        }

        public void JumpToRecord(GDMRecord record)
        {
            if (record != null && Accept()) {
                fBase.SelectRecordByXRef(record.XRef);
                fView.Close();
            }
        }

        public void JumpToFather()
        {
            GDMFamilyRecord family = fBase.Context.GetChildFamily(fPerson, false, null);
            if (family == null) return;

            JumpToRecord(family.GetHusband());
        }

        public void JumpToMother()
        {
            GDMFamilyRecord family = fBase.Context.GetChildFamily(fPerson, false, null);
            if (family == null) return;

            JumpToRecord(family.GetWife());
        }
    }
}
