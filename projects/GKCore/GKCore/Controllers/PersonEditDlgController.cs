﻿/*
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

#define SEX_SYMBOLS

using System;
using BSLib.Design.Graphics;
using BSLib.Design.MVP.Controls;
using GDModel;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.MVP;
using GKCore.MVP.Views;
using GKCore.Names;
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
        private GDMIndividualRecord fIndividualRecord;
        private IImage fPortraitImg;
        private GDMIndividualRecord fTarget;
        private TargetMode fTargetMode;

        public GDMIndividualRecord IndividualRecord
        {
            get { return fIndividualRecord; }
            set {
                if (fIndividualRecord != value) {
                    fIndividualRecord = value;
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

            for (GDMSex sx = GDMSex.svUnknown; sx <= GDMSex.svLast; sx++) {
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
                fView.SexCombo.AddItem(name, sx, image);
            }
        }

        public override void Init(IBaseWindow baseWin)
        {
            base.Init(baseWin);

            fView.EventsList.ListModel = new EventsListModel(baseWin, fLocalUndoman, true);
            fView.NotesList.ListModel = new NoteLinksListModel(baseWin, fLocalUndoman);
            fView.MediaList.ListModel = new MediaLinksListModel(baseWin, fLocalUndoman);
            fView.SourcesList.ListModel = new SourceCitationsListModel(baseWin, fLocalUndoman);
            fView.AssociationsList.ListModel = new AssociationsListModel(baseWin, fLocalUndoman);
            fView.GroupsList.ListModel = new GroupsSublistModel(baseWin, fLocalUndoman);
            fView.NamesList.ListModel = new NamesSublistModel(baseWin, fLocalUndoman);
            fView.SpousesList.ListModel = new SpousesSublistModel(baseWin, fLocalUndoman);
            fView.UserRefList.ListModel = new URefsSublistModel(baseWin, fLocalUndoman);
            fView.ParentsList.ListModel = new ParentsSublistModel(baseWin, fLocalUndoman);
            fView.ChildrenList.ListModel = new IndividualChildrenListModel(baseWin, fLocalUndoman);
        }

        private bool IsExtendedWomanSurname()
        {
            var selectedSex = fView.SexCombo.GetSelectedTag<GDMSex>();
            bool result = (GlobalOptions.Instance.WomanSurnameFormat != WomanSurnameFormat.wsfNotExtend) &&
                (selectedSex == GDMSex.svFemale);
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
                GDMPersonalName persName;
                if (fIndividualRecord.PersonalNames.Count > 0) {
                    persName = fIndividualRecord.PersonalNames[0];
                } else {
                    persName = new GDMPersonalName();
                    fIndividualRecord.PersonalNames.Add(persName);
                }

                GKUtils.SetNameParts(persName, fView.Surname.Text, fView.Name.Text, fView.Patronymic.Text);

                persName.Nickname = fView.Nickname.Text;
                persName.NamePrefix = fView.NamePrefix.Text;
                persName.SurnamePrefix = fView.SurnamePrefix.Text;
                persName.NameSuffix = fView.NameSuffix.Text;
                if (IsExtendedWomanSurname()) {
                    persName.MarriedName = fView.MarriedSurname.Text;
                }

                fIndividualRecord.Sex = (GDMSex)fView.SexCombo.SelectedIndex;
                fIndividualRecord.Patriarch = fView.Patriarch.Checked;
                fIndividualRecord.Bookmark = fView.Bookmark.Checked;
                fIndividualRecord.Restriction = (GDMRestriction)fView.RestrictionCombo.SelectedIndex;

                fBase.Context.ProcessIndividual(fIndividualRecord);

                fLocalUndoman.Commit();

                fBase.NotifyRecord(fIndividualRecord, RecordAction.raEdit);

                return true;
            } catch (Exception ex) {
                Logger.WriteError("PersonEditDlgController.Accept()", ex);
                return false;
            }
        }

        public override void UpdateView()
        {
            try {
                fView.SexCombo.SelectedIndex = (sbyte)fIndividualRecord.Sex;
                fView.Patriarch.Checked = fIndividualRecord.Patriarch;
                fView.Bookmark.Checked = fIndividualRecord.Bookmark;
                fView.RestrictionCombo.SelectedIndex = (sbyte)fIndividualRecord.Restriction;

                fView.EventsList.ListModel.DataOwner = fIndividualRecord;
                fView.NotesList.ListModel.DataOwner = fIndividualRecord;
                fView.MediaList.ListModel.DataOwner = fIndividualRecord;
                fView.SourcesList.ListModel.DataOwner = fIndividualRecord;
                fView.AssociationsList.ListModel.DataOwner = fIndividualRecord;

                fView.GroupsList.ListModel.DataOwner = fIndividualRecord;
                fView.NamesList.ListModel.DataOwner = fIndividualRecord;
                fView.SpousesList.ListModel.DataOwner = fIndividualRecord;
                fView.UserRefList.ListModel.DataOwner = fIndividualRecord;
                fView.ParentsList.ListModel.DataOwner = fIndividualRecord;
                fView.ChildrenList.ListModel.DataOwner = fIndividualRecord;

                UpdateControls(true);
            } catch (Exception ex) {
                Logger.WriteError("PersonEditDlgController.UpdateView()", ex);
            }
        }

        public void UpdateParents()
        {
            bool locked = (fView.RestrictionCombo.SelectedIndex == (int)GDMRestriction.rnLocked);

            if (fIndividualRecord.ChildToFamilyLinks.Count != 0) {
                GDMFamilyRecord family = fBase.Context.Tree.GetPtrValue(fIndividualRecord.ChildToFamilyLinks[0]);
                fView.SetParentsAvl(true, locked);

                GDMIndividualRecord father, mother;
                fBase.Context.Tree.GetSpouses(family, out father, out mother);

                if (father != null) {
                    fView.SetFatherAvl(true, locked);
                    fView.Father.Text = GKUtils.GetNameString(father, true, false);
                } else {
                    fView.SetFatherAvl(false, locked);
                    fView.Father.Text = "";
                }

                if (mother != null) {
                    fView.SetMotherAvl(true, locked);
                    fView.Mother.Text = GKUtils.GetNameString(mother, true, false);
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
            var np = (fIndividualRecord.PersonalNames.Count > 0) ? fIndividualRecord.PersonalNames[0] : null;
            UpdateNameControls(np);
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
                fView.ChildrenList.UpdateSheet();
            }

            UpdatePortrait(totalUpdate);

            bool locked = (fView.RestrictionCombo.SelectedIndex == (int)GDMRestriction.rnLocked);

            // controls lock
            fView.Name.Enabled = !locked;

            fView.SexCombo.Enabled = !locked;
            fView.Patriarch.Enabled = !locked;
            fView.Bookmark.Enabled = !locked;

            fView.NamePrefix.Enabled = !locked;
            fView.Nickname.Enabled = !locked;
            fView.SurnamePrefix.Enabled = !locked;
            fView.NameSuffix.Enabled = !locked;

            fView.EventsList.ReadOnly = locked;
            fView.NamesList.ReadOnly = locked;
            fView.NotesList.ReadOnly = locked;
            fView.MediaList.ReadOnly = locked;
            fView.SourcesList.ReadOnly = locked;
            fView.SpousesList.ReadOnly = locked;
            fView.AssociationsList.ReadOnly = locked;
            fView.GroupsList.ReadOnly = locked;
            fView.UserRefList.ReadOnly = locked;
            fView.ParentsList.ReadOnly = locked;
            fView.ChildrenList.ReadOnly = locked;
        }

        public void UpdateNameControls(GDMPersonalName np)
        {
            ICulture culture;
            if (np != null) {
                var parts = GKUtils.GetNameParts(fBase.Context.Tree, fIndividualRecord, np, false);
                culture = parts.Culture;

                fView.Surname.Text = parts.Surname;
                fView.Name.Text = parts.Name;
                fView.Patronymic.Text = parts.Patronymic;

                fView.NamePrefix.Text = np.NamePrefix;
                fView.Nickname.Text = np.Nickname;
                fView.SurnamePrefix.Text = np.SurnamePrefix;
                fView.NameSuffix.Text = np.NameSuffix;

                fView.MarriedSurname.Text = np.MarriedName;
            } else {
                culture = fBase.Context.Culture;

                fView.Surname.Text = "";
                fView.Name.Text = "";
                fView.Patronymic.Text = "";

                fView.NamePrefix.Text = "";
                fView.Nickname.Text = "";
                fView.SurnamePrefix.Text = "";
                fView.NameSuffix.Text = "";

                fView.MarriedSurname.Text = "";
            }

            var locked = (fView.RestrictionCombo.SelectedIndex == (int) GDMRestriction.rnLocked);
            fView.Patronymic.Enabled = !locked && culture.HasPatronymic();
            fView.Surname.Enabled = !locked && culture.HasSurname();
        }

        public void UpdatePortrait(bool totalUpdate)
        {
            if (fPortraitImg == null || totalUpdate) {
                fPortraitImg = fBase.Context.GetPrimaryBitmap(fIndividualRecord, fView.Portrait.Width, fView.Portrait.Height, false);
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
                    INamesTable namesTable = AppHost.NamesTable;

                    var parts = GKUtils.GetNameParts(fBase.Context.Tree, fTarget, false);
                    ICulture culture = parts.Culture;
                    GDMSex sx = (GDMSex)fView.SexCombo.SelectedIndex;

                    switch (fTargetMode) {
                        case TargetMode.tmParent:
                            fView.Surname.Text = parts.Surname;
                            if (culture.HasPatronymic()) {
                                AddPatronymic(namesTable.GetPatronymicByName(parts.Name, GDMSex.svMale));
                                AddPatronymic(namesTable.GetPatronymicByName(parts.Name, GDMSex.svFemale));
                                fView.Patronymic.Text = namesTable.GetPatronymicByName(parts.Name, sx);
                            }
                            break;

                        case TargetMode.tmChild:
                            switch (sx) {
                                case GDMSex.svMale:
                                    fView.Surname.Text = parts.Surname;
                                    if (culture.HasPatronymic()) {
                                        fView.Name.Text = namesTable.GetNameByPatronymic(parts.Patronymic);
                                    }
                                    break;

                                case GDMSex.svFemale:
                                    SetMarriedSurname(culture, parts.Surname);
                                    break;
                            }
                            break;

                        case TargetMode.tmSpouse:
                            if (sx == GDMSex.svFemale) {
                                SetMarriedSurname(culture, parts.Surname);
                            }
                            break;
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("PersonEditDlg.SetTarget(" + fTargetMode.ToString() + ")", ex);
            }
        }

        private void AddPatronymic(string patronymic)
        {
            if (!string.IsNullOrEmpty(patronymic)) {
                fView.Patronymic.Add(patronymic);
            }
        }

        private void SetMarriedSurname(ICulture culture, string husbSurname)
        {
            string surname = culture.GetMarriedSurname(husbSurname);
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

            fLocalUndoman.DoOrdinaryOperation(OperationType.otIndividualSexChange, fIndividualRecord, (GDMSex)fView.SexCombo.SelectedIndex);
            fLocalUndoman.DoIndividualNameChange(fIndividualRecord, fView.Surname.Text, fView.Name.Text, fView.Patronymic.Text);
        }

        public void AddPortrait()
        {
            if (BaseController.AddIndividualPortrait(fBase, fLocalUndoman, fIndividualRecord)) {
                fView.MediaList.UpdateSheet();
                UpdatePortrait(true);
            }
        }

        public void DeletePortrait()
        {
            if (BaseController.DeleteIndividualPortrait(fBase, fLocalUndoman, fIndividualRecord)) {
                UpdatePortrait(true);
            }
        }

        public void AddParents()
        {
            AcceptTempData();

            GDMFamilyRecord family = fBase.Context.SelectFamily(fIndividualRecord);
            if (family != null && family.IndexOfChild(fIndividualRecord) < 0) {
                fLocalUndoman.DoOrdinaryOperation(OperationType.otIndividualParentsAttach, fIndividualRecord, family);
            }
            UpdateControls();
        }

        public void EditParents()
        {
            AcceptTempData();

            GDMFamilyRecord family = fBase.Context.GetChildFamily(fIndividualRecord, false, null);
            if (family != null && BaseController.ModifyFamily(fBase, ref family, TargetMode.tmNone, null)) {
                UpdateControls();
            }
        }

        public void DeleteParents()
        {
            if (!AppHost.StdDialogs.ShowQuestionYN(LangMan.LS(LSID.LSID_DetachParentsQuery))) return;

            GDMFamilyRecord family = fBase.Context.GetChildFamily(fIndividualRecord, false, null);
            if (family == null) return;

            AcceptTempData();

            fLocalUndoman.DoOrdinaryOperation(OperationType.otIndividualParentsDetach, fIndividualRecord, family);
            UpdateControls();
        }

        public void AddFather()
        {
            AcceptTempData();

            if (BaseController.AddIndividualFather(fBase, fLocalUndoman, fIndividualRecord)) {
                UpdateControls();
            }
        }

        public void DeleteFather()
        {
            AcceptTempData();

            if (BaseController.DeleteIndividualFather(fBase, fLocalUndoman, fIndividualRecord)) {
                UpdateControls();
            }
        }

        public void AddMother()
        {
            AcceptTempData();

            if (BaseController.AddIndividualMother(fBase, fLocalUndoman, fIndividualRecord)) {
                UpdateControls();
            }
        }

        public void DeleteMother()
        {
            AcceptTempData();

            if (BaseController.DeleteIndividualMother(fBase, fLocalUndoman, fIndividualRecord)) {
                UpdateControls();
            }
        }

        public void JumpToFather()
        {
            GDMFamilyRecord family = fBase.Context.GetChildFamily(fIndividualRecord, false, null);
            if (family == null) return;

            JumpToRecord(family.Husband);
        }

        public void JumpToMother()
        {
            GDMFamilyRecord family = fBase.Context.GetChildFamily(fIndividualRecord, false, null);
            if (family == null) return;

            JumpToRecord(family.Wife);
        }

        public void JumpToPersonSpouse(GDMFamilyRecord family)
        {
            GDMIndividualRecord spouse = null;
            switch (fIndividualRecord.Sex) {
                case GDMSex.svMale:
                    spouse = fBase.Context.Tree.GetPtrValue(family.Wife);
                    break;

                case GDMSex.svFemale:
                    spouse = fBase.Context.Tree.GetPtrValue(family.Husband);
                    break;
            }
            JumpToRecord(spouse);
        }

        public void CopyPersonName()
        {
            AppHost.Instance.SetClipboardText(GKUtils.GetNameString(fIndividualRecord, true, false));
        }

        public override void SetLocale()
        {
            fView.Title = LangMan.LS(LSID.LSID_WinPersonEdit);

            GetControl<IButton>("btnAccept").Text = LangMan.LS(LSID.LSID_DlgAccept);
            GetControl<IButton>("btnCancel").Text = LangMan.LS(LSID.LSID_DlgCancel);
            GetControl<ILabel>("lblSurname").Text = LangMan.LS(LSID.LSID_Surname);
            GetControl<ILabel>("lblMarriedSurname").Text = LangMan.LS(LSID.LSID_MarriedSurname);
            GetControl<ILabel>("lblName").Text = LangMan.LS(LSID.LSID_Name);
            GetControl<ILabel>("lblPatronymic").Text = LangMan.LS(LSID.LSID_Patronymic);
            GetControl<ILabel>("lblSex").Text = LangMan.LS(LSID.LSID_Sex);
            GetControl<ILabel>("lblNickname").Text = LangMan.LS(LSID.LSID_Nickname);
            GetControl<ILabel>("lblSurnamePrefix").Text = LangMan.LS(LSID.LSID_SurnamePrefix);
            GetControl<ILabel>("lblNamePrefix").Text = LangMan.LS(LSID.LSID_NamePrefix);
            GetControl<ILabel>("lblNameSuffix").Text = LangMan.LS(LSID.LSID_NameSuffix);
            GetControl<ICheckBox>("chkPatriarch").Text = LangMan.LS(LSID.LSID_Patriarch);
            GetControl<ICheckBox>("chkBookmark").Text = LangMan.LS(LSID.LSID_Bookmark);
            GetControl<ILabel>("lblParents").Text = LangMan.LS(LSID.LSID_Parents);
            GetControl<ITabPage>("pageEvents").Text = LangMan.LS(LSID.LSID_Events);
            GetControl<ITabPage>("pageSpouses").Text = LangMan.LS(LSID.LSID_Spouses);
            GetControl<ITabPage>("pageAssociations").Text = LangMan.LS(LSID.LSID_Associations);
            GetControl<ITabPage>("pageGroups").Text = LangMan.LS(LSID.LSID_RPGroups);
            GetControl<ITabPage>("pageNotes").Text = LangMan.LS(LSID.LSID_RPNotes);
            GetControl<ITabPage>("pageMultimedia").Text = LangMan.LS(LSID.LSID_RPMultimedia);
            GetControl<ITabPage>("pageSources").Text = LangMan.LS(LSID.LSID_RPSources);
            GetControl<ITabPage>("pageUserRefs").Text = LangMan.LS(LSID.LSID_UserRefs);
            GetControl<ILabel>("lblRestriction").Text = LangMan.LS(LSID.LSID_Restriction);
            GetControl<ITabPage>("pageNames").Text = LangMan.LS(LSID.LSID_Names);
            GetControl<ITabPage>("pageParents").Text = LangMan.LS(LSID.LSID_Parents);
            GetControl<ITabPage>("pageChilds").Text = LangMan.LS(LSID.LSID_Childs);

            SetToolTip("btnPortraitAdd", LangMan.LS(LSID.LSID_PortraitAddTip));
            SetToolTip("btnPortraitDelete", LangMan.LS(LSID.LSID_PortraitDeleteTip));
            SetToolTip("btnParentsAdd", LangMan.LS(LSID.LSID_ParentsAddTip));
            SetToolTip("btnParentsEdit", LangMan.LS(LSID.LSID_ParentsEditTip));
            SetToolTip("btnParentsDelete", LangMan.LS(LSID.LSID_ParentsDeleteTip));
            SetToolTip("btnFatherAdd", LangMan.LS(LSID.LSID_FatherAddTip));
            SetToolTip("btnFatherDelete", LangMan.LS(LSID.LSID_FatherDeleteTip));
            SetToolTip("btnFatherSel", LangMan.LS(LSID.LSID_FatherSelTip));
            SetToolTip("btnMotherAdd", LangMan.LS(LSID.LSID_MotherAddTip));
            SetToolTip("btnMotherDelete", LangMan.LS(LSID.LSID_MotherDeleteTip));
            SetToolTip("btnMotherSel", LangMan.LS(LSID.LSID_MotherSelTip));
            SetToolTip("btnNameCopy", LangMan.LS(LSID.LSID_NameCopyTip));
        }
    }
}
