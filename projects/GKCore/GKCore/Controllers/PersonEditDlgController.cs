/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using GDModel;
using GKCore.Cultures;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Graphics;
using GKCore.Design.Views;
using GKCore.Lists;
using GKCore.Locales;
using GKCore.Operations;
using GKCore.Options;
using GKUI.Themes;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public class PersonEditDlgController<T> : DialogController<T> where T : IPersonEditDlg
    {
        #region Design components
#pragma warning disable CS0649, IDE0044
        private ITabControl _tabsOther;
        private ITabPage _pageEvents;
        private ITabPage _pageSpouses;
        private ITabPage _pageAssociations;
        private ITabPage _pageGroups;
        private ITabPage _pageNotes;
        private ITabPage _pageMultimedia;
        private ITabPage _pageSources;
        private ITabPage _pageUserRefs;
        private ITabPage _pageNames;
        private ITabPage _pageParents;
        private ITabPage _pageOther;
        private ITabPage _pageFamily;
        private IButton _btnAccept;
        private IButton _btnCancel;
        private IButton _btnPortraitAdd;
        private IButton _btnPortraitDelete;
        private IButton _btnParentsAdd;
        private IButton _btnParentsEdit;
        private IButton _btnParentsDelete;
        private IButton _btnFatherAdd;
        private IButton _btnFatherDelete;
        private IButton _btnFatherSel;
        private IButton _btnMotherAdd;
        private IButton _btnMotherDelete;
        private IButton _btnMotherSel;
        private IButton _btnNameCopy;
        private ILabel _lblSurname;
        private ILabel _lblMarriedSurname;
        private ILabel _lblName;
        private ILabel _lblPatronymic;
        private ILabel _lblSex;
        private ILabel _lblNickname;
        private ILabel _lblParents;
        private ILabel _lblMother;
        private ILabel _lblFather;
        private ILabel _lblRestriction;
        private ITextBox _txtSurname;
        private ITextBox _txtName;
        private IComboBox _cmbPatronymic;
        private IComboBox _cmbSex;
        private ITextBox _txtNickname;
        private ITextBox _txtFather;
        private ITextBox _txtMother;
        private ITextBox _txtMarriedSurname;
        private IComboBox _cmbRestriction;
        private ICheckBox _chkPatriarch;
        private ICheckBox _chkBookmark;
        private ISheetList _fEventsList;
        private ISheetList _fSpousesList;
        private ISheetList _fAssociationsList;
        private ISheetList _fGroupsList;
        private ISheetList _fNotesList;
        private ISheetList _fMediaList;
        private ISheetList _fSourcesList;
        private ISheetList _fUserRefList;
        private ISheetList _fNamesList;
        private ISheetList _fParentsList;
#pragma warning restore CS0649, IDE0044
        #endregion

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
                    UpdateListModels(fIndividualRecord);
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


        public PersonEditDlgController(T view) : base(view)
        {
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                _fNamesList.OnModify -= ModifyNamesSheet;
                _fSpousesList.OnModify -= ModifySpousesSheet;
                _fSpousesList.OnBeforeChange -= BeforeChangeSpousesSheet;
                _fAssociationsList.OnModify -= ModifyAssociationsSheet;
                _fGroupsList.OnModify -= ModifyGroupsSheet;
                _fParentsList.OnModify -= ModifyParentsSheet;

                if (fPortraitImg != null) fPortraitImg.Dispose();
            }
            base.Dispose(disposing);
        }

        private void ModifyNamesSheet(object sender, ModifyEventArgs eArgs)
        {
            if (eArgs.Action == RecordAction.raMoveUp || eArgs.Action == RecordAction.raMoveDown || eArgs.Action == RecordAction.raEdit) {
                UpdateNameControls(fIndividualRecord.PersonalNames[0]);
            }
        }

        private void BeforeChangeSpousesSheet(object sender, ModifyEventArgs eArgs)
        {
            if (eArgs.Action == RecordAction.raAdd || eArgs.Action == RecordAction.raEdit) {
                AcceptTempData();
            }
        }

        private void ModifySpousesSheet(object sender, ModifyEventArgs eArgs)
        {
            var family = fBase.Context.Tree.GetPtrValue<GDMFamilyRecord>(eArgs.ItemData as GDMSpouseToFamilyLink);
            if (eArgs.Action == RecordAction.raJump && family != null) {
                JumpToPersonSpouse(family);
            }
        }

        private void ModifyAssociationsSheet(object sender, ModifyEventArgs eArgs)
        {
            if (eArgs.Action == RecordAction.raJump) {
                JumpToRecord(eArgs.ItemData as GDMAssociation);
            }
        }

        private void ModifyParentsSheet(object sender, ModifyEventArgs eArgs)
        {
            UpdateParents();
        }

        private void ModifyGroupsSheet(object sender, ModifyEventArgs eArgs)
        {
            if (eArgs.Action == RecordAction.raJump) {
                JumpToRecord(eArgs.ItemData as GDMPointer);
            }
        }

        private IImage GetSexImage(GDMSex sx)
        {
            IImage image;
            if (AppHost.Instance.HasFeatureSupport(Feature.Mobile)) {
                string resImage = GKData.SexData[(int)sx].SymImage;
                image = AppHost.GfxProvider.LoadResourceImage(resImage, ImageTarget.UI, true);
            } else {
                var themeSym = GKData.SexData[(int)sx].ThemeElement;
                var themeMan = AppHost.ThemeManager;
                image = (themeSym == ThemeElement.None || themeMan == null) ? null : themeMan.GetThemeImage(themeSym, true);
            }
            return image;
        }

        public override void InitView(IBaseWindow baseWin)
        {
            base.InitView(baseWin);

            _fNamesList.OnModify += ModifyNamesSheet;
            _fSpousesList.OnModify += ModifySpousesSheet;
            _fSpousesList.OnBeforeChange += BeforeChangeSpousesSheet;
            _fAssociationsList.OnModify += ModifyAssociationsSheet;
            _fGroupsList.OnModify += ModifyGroupsSheet;
            _fParentsList.OnModify += ModifyParentsSheet;

            _cmbRestriction.ReadOnly = true;
            for (GDMRestriction res = GDMRestriction.rnNone; res <= GDMRestriction.rnPrivacy; res++) {
                _cmbRestriction.Add(LangMan.LS(GKData.Restrictions[(int)res]));
            }

            _cmbSex.ReadOnly = true;
            for (GDMSex sx = GDMSex.svUnknown; sx <= GDMSex.svLast; sx++) {
                string name = GKUtils.SexStr(sx);
                IImage image = GetSexImage(sx);
                _cmbSex.AddItem(name, sx, image);
            }

            _fEventsList.ListModel = new EventsListModel(fView, baseWin, fLocalUndoman);
            _fNotesList.ListModel = new NoteLinksListModel(fView, baseWin, fLocalUndoman);
            _fMediaList.ListModel = new MediaLinksListModel(fView, baseWin, fLocalUndoman);
            _fSourcesList.ListModel = new SourceCitationsListModel(fView, baseWin, fLocalUndoman);
            _fAssociationsList.ListModel = new AssociationsListModel(fView, baseWin, fLocalUndoman);
            _fGroupsList.ListModel = new IndiGroupsListModel(fView, baseWin, fLocalUndoman);
            _fNamesList.ListModel = new IndiNamesListModel(fView, baseWin, fLocalUndoman);
            _fSpousesList.ListModel = new IndiSpousesListModel(fView, baseWin, fLocalUndoman);
            _fUserRefList.ListModel = new URefsListModel(fView, baseWin, fLocalUndoman);
            _fParentsList.ListModel = new IndiParentsListModel(fView, baseWin, fLocalUndoman);
        }

        public override void DoneView()
        {
            _fEventsList.ListModel.SaveSettings();
            _fNotesList.ListModel.SaveSettings();
            _fMediaList.ListModel.SaveSettings();
            _fSourcesList.ListModel.SaveSettings();
            _fAssociationsList.ListModel.SaveSettings();
            _fGroupsList.ListModel.SaveSettings();
            _fNamesList.ListModel.SaveSettings();
            _fSpousesList.ListModel.SaveSettings();
            _fUserRefList.ListModel.SaveSettings();
            _fParentsList.ListModel.SaveSettings();
        }

        private bool IsExtendedWomanSurname()
        {
            var selectedSex = _cmbSex.GetSelectedTag<GDMSex>();
            return GlobalOptions.Instance.CanExtendedSurname(selectedSex);
        }

        public void ChangeSex()
        {
            if (!IsExtendedWomanSurname()) {
                _lblSurname.Text = LangMan.LS(LSID.Surname);
                _txtMarriedSurname.Enabled = false;
            } else {
                _lblSurname.Text = LangMan.LS(LSID.MaidenSurname);
                _txtMarriedSurname.Enabled = true;
            }

            UpdatePortrait(true);
        }

        public void AcceptTempData()
        {
            // It is very important for some methods
            // For the sample: we need to have gender's value on time of call AddSpouse (for define husband/wife)
            // And we need to have actual name's value for visible it in FamilyEditDlg

            fLocalUndoman.DoOrdinaryOperation(OperationType.otIndividualSexChange, fIndividualRecord, (GDMSex)_cmbSex.SelectedIndex);
            fLocalUndoman.DoIndividualNameChange(fIndividualRecord, _txtSurname.Text, _txtName.Text, _cmbPatronymic.Text, _txtMarriedSurname.Text, _txtNickname.Text);
        }

        protected virtual void AcceptNameParts(GDMPersonalName persName)
        {
            persName.Nickname = _txtNickname.Text;
            if (IsExtendedWomanSurname()) {
                persName.MarriedName = _txtMarriedSurname.Text;
            }
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

                GKUtils.SetNameParts(persName, _txtSurname.Text, _txtName.Text, _cmbPatronymic.Text);
                AcceptNameParts(persName);

                fIndividualRecord.Sex = (GDMSex)_cmbSex.SelectedIndex;
                fIndividualRecord.Patriarch = _chkPatriarch.Checked;
                fIndividualRecord.Bookmark = _chkBookmark.Checked;
                fIndividualRecord.Restriction = (GDMRestriction)_cmbRestriction.SelectedIndex;

                fBase.Context.ProcessIndividual(fIndividualRecord);

                fLocalUndoman.Commit();

                fBase.NotifyRecord(fIndividualRecord, RecordAction.raEdit);

                return true;
            } catch (Exception ex) {
                Logger.WriteError("PersonEditDlgController.Accept()", ex);
                return false;
            }
        }

        protected virtual void UpdateListModels(GDMIndividualRecord indiRec)
        {
            _fEventsList.ListModel.DataOwner = indiRec;
            _fNotesList.ListModel.DataOwner = indiRec;
            _fMediaList.ListModel.DataOwner = indiRec;
            _fSourcesList.ListModel.DataOwner = indiRec;
            _fAssociationsList.ListModel.DataOwner = indiRec;

            _fGroupsList.ListModel.DataOwner = indiRec;
            _fNamesList.ListModel.DataOwner = indiRec;
            _fSpousesList.ListModel.DataOwner = indiRec;
            _fUserRefList.ListModel.DataOwner = indiRec;
            _fParentsList.ListModel.DataOwner = indiRec;
        }

        public override void UpdateView()
        {
            try {
                bool disNoStd = GlobalOptions.Instance.DisableNonStdFeatures;
                _tabsOther.SetTabVisible(_pageGroups, !disNoStd);

                _chkPatriarch.Visible = !disNoStd;
                _chkBookmark.Visible = !disNoStd;

                _cmbSex.SelectedIndex = (sbyte)fIndividualRecord.Sex;
                _chkPatriarch.Checked = fIndividualRecord.Patriarch;
                _chkBookmark.Checked = fIndividualRecord.Bookmark;
                _cmbRestriction.SelectedIndex = (sbyte)fIndividualRecord.Restriction;

                UpdateControls(true);
            } catch (Exception ex) {
                Logger.WriteError("PersonEditDlgController.UpdateView()", ex);
            }
        }

        public void UpdateParents()
        {
            bool locked = (_cmbRestriction.SelectedIndex == (int)GDMRestriction.rnLocked);

            if (fIndividualRecord.ChildToFamilyLinks.Count != 0) {
                GDMFamilyRecord family = fBase.Context.Tree.GetPtrValue(fIndividualRecord.ChildToFamilyLinks[0]);
                fView.SetParentsAvl(true, locked);

                GDMIndividualRecord father, mother;
                fBase.Context.Tree.GetSpouses(family, out father, out mother);

                if (father != null) {
                    fView.SetFatherAvl(true, locked);
                    _txtFather.Text = GKUtils.GetNameString(father, false);
                } else {
                    fView.SetFatherAvl(false, locked);
                    _txtFather.Text = "";
                }

                if (mother != null) {
                    fView.SetMotherAvl(true, locked);
                    _txtMother.Text = GKUtils.GetNameString(mother, false);
                } else {
                    fView.SetMotherAvl(false, locked);
                    _txtMother.Text = "";
                }
            } else {
                fView.SetParentsAvl(false, locked);
                fView.SetFatherAvl(false, locked);
                fView.SetMotherAvl(false, locked);

                _txtFather.Text = "";
                _txtMother.Text = "";
            }
        }

        public void UpdateControls(bool totalUpdate = false)
        {
            var np = (fIndividualRecord.PersonalNames.Count > 0) ? fIndividualRecord.PersonalNames[0] : null;
            UpdateNameControls(np);
            UpdateParents();

            UpdatePortrait(totalUpdate);

            bool locked = (_cmbRestriction.SelectedIndex == (int)GDMRestriction.rnLocked);
            UpdateLocked(locked);
        }

        protected virtual void UpdateLocked(bool locked)
        {
            // controls lock
            _txtName.Enabled = !locked;

            _cmbSex.Enabled = !locked;
            _chkPatriarch.Enabled = !locked;
            _chkBookmark.Enabled = !locked;

            _txtNickname.Enabled = !locked;

            _fEventsList.ReadOnly = locked;
            _fNamesList.ReadOnly = locked;
            _fNotesList.ReadOnly = locked;
            _fMediaList.ReadOnly = locked;
            _fSourcesList.ReadOnly = locked;
            _fSpousesList.ReadOnly = locked;
            _fAssociationsList.ReadOnly = locked;
            _fGroupsList.ReadOnly = locked;
            _fUserRefList.ReadOnly = locked;
            _fParentsList.ReadOnly = locked;
        }

        public virtual void UpdateNameControls(GDMPersonalName np)
        {
            ICulture culture;
            if (np != null) {
                var parts = GKUtils.GetNameParts(fBase.Context.Tree, fIndividualRecord, np, false);
                culture = parts.Culture;

                _txtSurname.Text = parts.Surname;
                _txtName.Text = parts.Name;
                _cmbPatronymic.Text = parts.Patronymic;
                _txtNickname.Text = np.Nickname;
                _txtMarriedSurname.Text = np.MarriedName;
            } else {
                culture = fBase.Context.Culture;

                _txtSurname.Text = "";
                _txtName.Text = "";
                _cmbPatronymic.Text = "";
                _txtNickname.Text = "";
                _txtMarriedSurname.Text = "";
            }

            var locked = (_cmbRestriction.SelectedIndex == (int)GDMRestriction.rnLocked);
            _cmbPatronymic.Enabled = !locked && culture.HasPatronymic;
            _txtSurname.Enabled = !locked && culture.HasSurname;
        }

        public void UpdatePortrait(bool totalUpdate)
        {
            if (fPortraitImg == null || totalUpdate) {
                fPortraitImg = fBase.Context.GetPrimaryBitmap(fIndividualRecord, -1, -1, false);
            }

            IImage img = fPortraitImg;
            if (img == null) {
                // using avatar's image
                GDMSex curSex = (GDMSex)_cmbSex.SelectedIndex;
                string resImage = GKData.SexData[(int)curSex].DefPortraitImage;

                // HACK: on GKvX portrait's control on base ImageBox (SkiaSharp)
                ImageTarget target;
                if (AppHost.Instance.HasFeatureSupport(Feature.Mobile)) {
                    target = ImageTarget.Chart;
                } else {
                    target = ImageTarget.UI;
                }

                img = AppHost.GfxProvider.LoadResourceImage(resImage, target, false);
            }
            fView.SetPortrait(img);

            bool locked = (_cmbRestriction.SelectedIndex == (int)GDMRestriction.rnLocked);
            bool avail = (fPortraitImg != null);

            bool disNoStd = GlobalOptions.Instance.DisableNonStdFeatures;
            _btnPortraitAdd.Enabled = !disNoStd && !avail && !locked;
            _btnPortraitDelete.Enabled = !disNoStd && avail && !locked;
        }

        private void SetTarget(GDMIndividualRecord value)
        {
            try {
                fTarget = value;

                if (fTarget != null) {
                    var namesTable = AppHost.NamesTable;

                    var parts = GKUtils.GetNameParts(fBase.Context.Tree, fTarget, false);
                    ICulture culture = parts.Culture;
                    GDMSex sx = (GDMSex)_cmbSex.SelectedIndex;

                    switch (fTargetMode) {
                        case TargetMode.tmParent:
                            if (sx == GDMSex.svFemale) {
                                _txtSurname.Text = culture.GetMarriedSurname(parts.Surname);
                            } else {
                                _txtSurname.Text = parts.Surname;
                            }
                            if (culture.HasPatronymic) {
                                AddPatronymic(namesTable.GetPatronymicByName(parts.Name, GDMSex.svMale));
                                AddPatronymic(namesTable.GetPatronymicByName(parts.Name, GDMSex.svFemale));
                                _cmbPatronymic.Text = namesTable.GetPatronymicByName(parts.Name, sx);
                            }
                            break;

                        case TargetMode.tmChild:
                            switch (sx) {
                                case GDMSex.svMale:
                                    _txtSurname.Text = parts.Surname;
                                    if (culture.HasPatronymic) {
                                        _txtName.Text = namesTable.GetNameByPatronymic(parts.Patronymic);
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
                _cmbPatronymic.Add(patronymic);
            }
        }

        private void SetMarriedSurname(ICulture culture, string husbSurname)
        {
            string surname = culture.GetMarriedSurname(husbSurname);
            if (IsExtendedWomanSurname()) {
                _txtMarriedSurname.Text = surname;
            } else {
                _txtSurname.Text = "(" + surname + ")";
            }
        }

        public async void AddPortrait()
        {
            if (await BaseController.AddIndividualPortrait(fView, fBase, fLocalUndoman, fIndividualRecord)) {
                _fMediaList.UpdateSheet();
                UpdatePortrait(true);
            }
        }

        public void DeletePortrait()
        {
            if (BaseController.DeleteIndividualPortrait(fBase, fLocalUndoman, fIndividualRecord)) {
                UpdatePortrait(true);
            }
        }

        public async void AddParents()
        {
            AcceptTempData();

            GDMFamilyRecord family = await BaseController.SelectFamily(fBase, fView, fIndividualRecord);
            if (family != null) {
                if (family.HasMember(fIndividualRecord)) {
                    await AppHost.StdDialogs.ShowAlert(LangMan.LS(LSID.InvalidLink));
                    return;
                }

                fLocalUndoman.DoOrdinaryOperation(OperationType.otIndividualParentsAttach, fIndividualRecord, family);
            }
            UpdateControls();
        }

        public async void EditParents()
        {
            AcceptTempData();

            GDMFamilyRecord family = await fBase.Context.GetChildFamily(fIndividualRecord, false, null);
            if (family != null) {
                var famRes = await BaseController.ModifyFamily(fView, fBase, family, TargetMode.tmNone, null);
                if (famRes.Result) {
                    UpdateControls();
                }
            }
        }

        public async void DeleteParents()
        {
            var res = await AppHost.StdDialogs.ShowQuestion(LangMan.LS(LSID.DetachParentsQuery));
            if (!res) return;

            GDMFamilyRecord family = await fBase.Context.GetChildFamily(fIndividualRecord, false, null);
            if (family == null) return;

            AcceptTempData();

            fLocalUndoman.DoOrdinaryOperation(OperationType.otIndividualParentsDetach, fIndividualRecord, family);
            UpdateControls();
        }

        public async void AddFather()
        {
            AcceptTempData();

            if (await BaseController.AddIndividualFather(fView, fBase, fLocalUndoman, fIndividualRecord)) {
                UpdateControls();
            }
        }

        public async void DeleteFather()
        {
            AcceptTempData();

            if (await BaseController.DeleteIndividualFather(fBase, fLocalUndoman, fIndividualRecord)) {
                UpdateControls();
            }
        }

        public async void AddMother()
        {
            AcceptTempData();

            if (await BaseController.AddIndividualMother(fView, fBase, fLocalUndoman, fIndividualRecord)) {
                UpdateControls();
            }
        }

        public async void DeleteMother()
        {
            AcceptTempData();

            if (await BaseController.DeleteIndividualMother(fBase, fLocalUndoman, fIndividualRecord)) {
                UpdateControls();
            }
        }

        public async void JumpToFather()
        {
            GDMFamilyRecord family = await fBase.Context.GetChildFamily(fIndividualRecord, false, null);
            if (family != null)
                JumpToRecord(family.Husband);
        }

        public async void JumpToMother()
        {
            GDMFamilyRecord family = await fBase.Context.GetChildFamily(fIndividualRecord, false, null);
            if (family != null)
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
            fView.SetTitle(LangMan.LS(LSID.WinPersonEdit));

            _btnAccept.Text = LangMan.LS(LSID.DlgAccept);
            _btnCancel.Text = LangMan.LS(LSID.DlgCancel);
            _lblSurname.Text = LangMan.LS(LSID.Surname);
            _lblMarriedSurname.Text = LangMan.LS(LSID.MarriedSurname);
            _lblName.Text = LangMan.LS(LSID.GivenName);
            _lblPatronymic.Text = LangMan.LS(LSID.Patronymic);
            _lblSex.Text = LangMan.LS(LSID.Sex);
            _lblNickname.Text = LangMan.LS(LSID.Nickname);
            _chkPatriarch.Text = LangMan.LS(LSID.Patriarch);
            _chkBookmark.Text = LangMan.LS(LSID.Bookmark);
            _lblParents.Text = LangMan.LS(LSID.Parents);
            _lblMother.Text = LangMan.LS(LSID.Mother);
            _lblFather.Text = LangMan.LS(LSID.Father);
            _pageEvents.Text = LangMan.LS(LSID.Events);
            _pageSpouses.Text = LangMan.LS(LSID.Spouses);
            _pageAssociations.Text = LangMan.LS(LSID.Associations);
            _pageGroups.Text = LangMan.LS(LSID.RPGroups);
            _pageNotes.Text = LangMan.LS(LSID.RPNotes);
            _pageMultimedia.Text = LangMan.LS(LSID.RPMultimedia);
            _pageSources.Text = LangMan.LS(LSID.RPSources);
            _pageUserRefs.Text = LangMan.LS(LSID.UserRefs);
            _lblRestriction.Text = LangMan.LS(LSID.Restriction);
            _pageNames.Text = LangMan.LS(LSID.Names);
            _pageParents.Text = LangMan.LS(LSID.Parents);
            _pageOther.Text = LangMan.LS(LSID.Other);

            if (!AppHost.Instance.HasFeatureSupport(Feature.Mobile)) {
                _pageFamily.Text = LangMan.LS(LSID.Family);
            }

            SetToolTip("btnPortraitAdd", LangMan.LS(LSID.PortraitAddTip));
            SetToolTip("btnPortraitDelete", LangMan.LS(LSID.PortraitDeleteTip));
            SetToolTip("btnParentsAdd", LangMan.LS(LSID.ParentsAddTip));
            SetToolTip("btnParentsEdit", LangMan.LS(LSID.ParentsEditTip));
            SetToolTip("btnParentsDelete", LangMan.LS(LSID.ParentsDeleteTip));
            SetToolTip("btnFatherAdd", LangMan.LS(LSID.FatherAddTip));
            SetToolTip("btnFatherDelete", LangMan.LS(LSID.FatherDeleteTip));
            SetToolTip("btnFatherSel", LangMan.LS(LSID.FatherSelTip));
            SetToolTip("btnMotherAdd", LangMan.LS(LSID.MotherAddTip));
            SetToolTip("btnMotherDelete", LangMan.LS(LSID.MotherDeleteTip));
            SetToolTip("btnMotherSel", LangMan.LS(LSID.MotherSelTip));
            SetToolTip("btnNameCopy", LangMan.LS(LSID.NameCopyTip));
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            _btnAccept.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Accept);
            _btnCancel.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Cancel);

            _btnPortraitAdd.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Attach, true);
            _btnPortraitDelete.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Detach, true);

            _btnParentsAdd.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Attach, true);
            _btnParentsEdit.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_ItemEdit, true);
            _btnParentsDelete.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Detach, true);

            _btnFatherAdd.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Attach, true);
            _btnFatherDelete.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Detach, true);
            _btnFatherSel.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_LinkJump, true);

            _btnMotherAdd.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Attach, true);
            _btnMotherDelete.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Detach, true);
            _btnMotherSel.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_LinkJump, true);

            _btnNameCopy.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_CopyName, true);
        }
    }
}
