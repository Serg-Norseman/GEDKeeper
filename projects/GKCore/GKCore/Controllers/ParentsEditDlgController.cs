/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using GDModel;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Lists;
using GKCore.Locales;
using GKUI.Themes;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class ParentsEditDlgController : DialogController<IParentsEditDlg>
    {
        #region Design components
#pragma warning disable CS0649, IDE0044
        private IButton _btnAccept;
        private IButton _btnCancel;
        private IGroupBox _GroupBox1;
        private ILabel _lblChildName;
        private ITextBox _txtChildName;
        private ILabel _lblLinkageType;
        private IComboBox _cmbLinkageType;
        private ILabel _lblParents;
        private ITextBox _txtFather;
        private ITextBox _txtMother;
        private IButton _btnParentsEdit;
        private IButton _btnFatherAdd;
        private IButton _btnFatherDelete;
        private IButton _btnMotherAdd;
        private IButton _btnMotherDelete;
#pragma warning restore CS0649, IDE0044
        #endregion

        private GDMChildToFamilyLink fChildLink;
        private GDMIndividualRecord fIndividualRecord;

        public GDMChildToFamilyLink ChildLink
        {
            get { return fChildLink; }
            set {
                if (fChildLink != value) {
                    fChildLink = value;
                    UpdateView();
                }
            }
        }

        public GDMIndividualRecord IndividualRecord
        {
            get { return fIndividualRecord; }
            set { fIndividualRecord = value; }
        }


        public ParentsEditDlgController(IParentsEditDlg view) : base(view)
        {
        }

        public override void InitView(IBaseWindow baseWin)
        {
            base.InitView(baseWin);

            _cmbLinkageType.ReadOnly = true;
            for (GDMPedigreeLinkageType plt = GDMPedigreeLinkageType.plNone; plt <= GDMPedigreeLinkageType.plFoster; plt++) {
                _cmbLinkageType.Add(LangMan.LS(GKData.ParentTypes[(int)plt]));
            }
        }

        public override bool Accept()
        {
            try {
                fChildLink.PedigreeLinkageType = (GDMPedigreeLinkageType)_cmbLinkageType.SelectedIndex;

                fLocalUndoman.Commit();

                fBase.NotifyRecord(fIndividualRecord, RecordAction.raEdit);

                return true;
            } catch (Exception ex) {
                Logger.WriteError("ParentsEditDlgController.Accept()", ex);
                return false;
            }
        }

        public override void UpdateView()
        {
            try {
                _txtChildName.Text = GKUtils.GetNameString(fIndividualRecord, false);
                _cmbLinkageType.SelectedIndex = (sbyte)fChildLink.PedigreeLinkageType;

                UpdateControls();
            } catch (Exception ex) {
                Logger.WriteError("ParentsEditDlgController.UpdateView()", ex);
            }
        }

        public void UpdateControls()
        {
            if (fChildLink != null) {
                GDMFamilyRecord family = fBase.Context.Tree.GetPtrValue(fChildLink);
                SetParentsAvl(true);

                GDMIndividualRecord father, mother;
                fBase.Context.Tree.GetSpouses(family, out father, out mother);

                if (father != null) {
                    SetFatherAvl(true);
                    _txtFather.Text = GKUtils.GetNameString(father, false);
                } else {
                    SetFatherAvl(false);
                    _txtFather.Text = "";
                }

                if (mother != null) {
                    SetMotherAvl(true);
                    _txtMother.Text = GKUtils.GetNameString(mother, false);
                } else {
                    SetMotherAvl(false);
                    _txtMother.Text = "";
                }
            } else {
                SetParentsAvl(false);
                SetFatherAvl(false);
                SetMotherAvl(false);

                _txtFather.Text = "";
                _txtMother.Text = "";
            }
        }

        public async void EditParents()
        {
            GDMFamilyRecord family = await fBase.Context.GetChildFamily(fIndividualRecord, false, null);
            if (family != null) {
                var famRes = await BaseController.ModifyFamily(fView, fBase, family, TargetMode.tmNone, null);
                if (famRes.Result) {
                    UpdateControls();
                }
            }
        }

        public async void AddFather()
        {
            if (await BaseController.AddIndividualFather(fView, fBase, fLocalUndoman, fIndividualRecord)) {
                UpdateControls();
            }
        }

        public async void DeleteFather()
        {
            if (await BaseController.DeleteIndividualFather(fBase, fLocalUndoman, fIndividualRecord)) {
                UpdateControls();
            }
        }

        public async void AddMother()
        {
            if (await BaseController.AddIndividualMother(fView, fBase, fLocalUndoman, fIndividualRecord)) {
                UpdateControls();
            }
        }

        public async void DeleteMother()
        {
            if (await BaseController.DeleteIndividualMother(fBase, fLocalUndoman, fIndividualRecord)) {
                UpdateControls();
            }
        }

        private void SetParentsAvl(bool avail)
        {
            _btnParentsEdit.Enabled = avail;
        }

        private void SetFatherAvl(bool avail)
        {
            _btnFatherAdd.Enabled = !avail;
            _btnFatherDelete.Enabled = avail;
        }

        private void SetMotherAvl(bool avail)
        {
            _btnMotherAdd.Enabled = !avail;
            _btnMotherDelete.Enabled = avail;
        }

        public override void SetLocale()
        {
            fView.SetTitle(LangMan.LS(LSID.WinPersonEdit));

            _btnAccept.Text = LangMan.LS(LSID.DlgAccept);
            _btnCancel.Text = LangMan.LS(LSID.DlgCancel);
            _lblChildName.Text = LangMan.LS(LSID.GeneralName);
            _lblParents.Text = LangMan.LS(LSID.Parents);
            _lblLinkageType.Text = LangMan.LS(LSID.LinkageType);

            SetToolTip("btnParentsEdit", LangMan.LS(LSID.ParentsEditTip));
            SetToolTip("btnFatherAdd", LangMan.LS(LSID.FatherAddTip));
            SetToolTip("btnFatherDelete", LangMan.LS(LSID.FatherDeleteTip));
            SetToolTip("btnMotherAdd", LangMan.LS(LSID.MotherAddTip));
            SetToolTip("btnMotherDelete", LangMan.LS(LSID.MotherDeleteTip));
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            _btnAccept.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Accept);
            _btnCancel.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Cancel);

            _btnParentsEdit.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_ItemEdit, true);

            _btnFatherAdd.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Attach, true);
            _btnFatherDelete.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Detach, true);

            _btnMotherAdd.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Attach, true);
            _btnMotherDelete.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Detach, true);
        }
    }
}
