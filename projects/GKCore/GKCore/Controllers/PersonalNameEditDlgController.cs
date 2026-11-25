/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections.Generic;
using GDModel;
using GDModel.Providers.GEDCOM;
using GKCore.Cultures;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Locales;
using GKCore.Options;
using GKCore.Utilities;
using GKUI.Themes;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class PersonalNameEditDlgController : DialogController<IPersonalNameEditDlg>
    {
        #region Design components
#pragma warning disable CS0649, IDE0044
        private IButton _btnAccept;
        private IButton _btnCancel;
        private ITextBox _txtMarriedSurname;
        private ILabel _lblMarriedSurname;
        private IComboBox _cmbNameType;
        private ILabel _lblType;
        private ITextBox _txtNickname;
        private ITextBox _txtNameSuffix;
        private ITextBox _txtNamePrefix;
        private ITextBox _txtSurnamePrefix;
        private ITextBox _txtPatronymic;
        private ITextBox _txtName;
        private ITextBox _txtSurname;
        private ILabel _lblNickname;
        private ILabel _lblNameSuffix;
        private ILabel _lblNamePrefix;
        private ILabel _lblSurnamePrefix;
        private ILabel _lblPatronymic;
        private ILabel _lblName;
        private ILabel _lblSurname;
        private IComboBox _cmbLanguage;
        private ILabel _lblLanguage;
#pragma warning restore CS0649, IDE0044
        #endregion

        private GDMIndividualRecord fIndividualRecord;
        private GDMPersonalName fPersonalName;

        public GDMIndividualRecord IndividualRecord
        {
            get { return fIndividualRecord; }
            set { fIndividualRecord = value; }
        }

        public GDMPersonalName PersonalName
        {
            get { return fPersonalName; }
            set {
                if (fPersonalName != value) {
                    fPersonalName = value;
                    UpdateView();
                }
            }
        }

        public PersonalNameEditDlgController(IPersonalNameEditDlg view) : base(view)
        {
        }

        public override void InitView(IBaseWindow baseWin)
        {
            base.InitView(baseWin);

            _cmbNameType.ReadOnly = true;
            _cmbLanguage.ReadOnly = true;
            for (GDMNameType nt = GDMNameType.ntNone; nt <= GDMNameType.ntMarried; nt++) {
                _cmbNameType.Add(LangMan.LS(GKData.NameTypes[(int)nt]));
            }
        }

        public override bool Accept()
        {
            try {
                GKUtils.SetNameParts(fPersonalName, _txtSurname.Text, _txtName.Text, _txtPatronymic.Text);

                fPersonalName.Nickname = _txtNickname.Text;
                fPersonalName.NamePrefix = _txtNamePrefix.Text;
                fPersonalName.SurnamePrefix = _txtSurnamePrefix.Text;
                fPersonalName.NameSuffix = _txtNameSuffix.Text;

                fPersonalName.NameType = (GDMNameType)_cmbNameType.SelectedIndex;
                fPersonalName.Language = _cmbLanguage.GetSelectedTag<GDMLanguageID>();

                if (IsExtendedWomanSurname()) {
                    fPersonalName.MarriedName = _txtMarriedSurname.Text;
                }

                fBase.Context.CollectNameLangs(fPersonalName);

                return true;
            } catch (Exception ex) {
                Logger.WriteError("PersonalNameEditDlgController.Accept()", ex);
                return false;
            }
        }

        private bool IsExtendedWomanSurname()
        {
            return GlobalOptions.Instance.CanExtendedSurname(fIndividualRecord.Sex);
        }

        public override void UpdateView()
        {
            FillLanguages();

            var parts = GKUtils.GetNameParts(fBase.Context.Tree, fIndividualRecord, fPersonalName, false);

            _txtSurname.Text = parts.Surname;
            _txtName.Text = parts.Name;
            _txtPatronymic.Text = parts.Patronymic;
            _cmbNameType.SelectedIndex = (sbyte)fPersonalName.NameType;

            _txtNamePrefix.Text = fPersonalName.NamePrefix;
            _txtNickname.Text = fPersonalName.Nickname;
            _txtSurnamePrefix.Text = fPersonalName.SurnamePrefix;
            _txtNameSuffix.Text = fPersonalName.NameSuffix;

            _txtMarriedSurname.Text = fPersonalName.MarriedName;

            if (!IsExtendedWomanSurname()) {
                _lblSurname.Text = LangMan.LS(LSID.Surname);
                _txtMarriedSurname.Enabled = false;
            } else {
                _lblSurname.Text = LangMan.LS(LSID.MaidenSurname);
                _txtMarriedSurname.Enabled = true;
            }

            ICulture culture = parts.Culture;
            _txtSurname.Enabled = _txtSurname.Enabled && culture.HasSurname;
            _txtPatronymic.Enabled = _txtPatronymic.Enabled && culture.HasPatronymic;

            GDMLanguageID langID = fPersonalName.Language;
            _cmbLanguage.Text = GEDCOMUtils.GetLanguageStr(langID);
        }

        public void UpdateLanguage()
        {
            var selectedLanguageId = GetSelectedLanguageID();
            var culture = CulturesPool.DefineCulture(selectedLanguageId);
            _txtSurname.Enabled = culture.HasSurname;
            _txtPatronymic.Enabled = culture.HasPatronymic;
        }

        private void FillLanguages()
        {
            var freqList = new List<FreqItem<GDMLanguageID>>();

            var langStats = fBase.Context.LangStats;
            for (var lid = GDMLanguageID.Unknown; lid < GDMLanguageID.Yiddish; lid++) {
                int stat = (lid == GDMLanguageID.Unknown) ? 1000 : langStats.GetValue(lid);
                freqList.Add(new FreqItem<GDMLanguageID>(lid, GEDCOMUtils.GetLanguageStr(lid), stat));
                //_cmbLanguage.AddItem(GEDCOMUtils.GetLanguageStr(lid), lid);
            }

            FreqCollection<string>.PopulateCombo(_cmbLanguage, freqList, GDMLanguageID.Unknown);
        }

        private GDMLanguageID GetSelectedLanguageID()
        {
            var result = _cmbLanguage.GetSelectedTag<GDMLanguageID>();
            if (result == GDMLanguageID.Unknown) {
                var tree = fBase.Context.Tree;
                if (tree != null) {
                    result = tree.Header.Language;
                }
            }

            return result;
        }

        public override void SetLocale()
        {
            fView.SetTitle(LangMan.LS(LSID.GeneralName));

            _btnAccept.Text = LangMan.LS(LSID.DlgAccept);
            _btnCancel.Text = LangMan.LS(LSID.DlgCancel);
            _lblSurname.Text = LangMan.LS(LSID.Surname);
            _lblMarriedSurname.Text = LangMan.LS(LSID.MarriedSurname);
            _lblName.Text = LangMan.LS(LSID.GivenName);
            _lblPatronymic.Text = LangMan.LS(LSID.Patronymic);
            _lblNickname.Text = LangMan.LS(LSID.Nickname);
            _lblSurnamePrefix.Text = LangMan.LS(LSID.SurnamePrefix);
            _lblNamePrefix.Text = LangMan.LS(LSID.NamePrefix);
            _lblNameSuffix.Text = LangMan.LS(LSID.NameSuffix);
            _lblType.Text = LangMan.LS(LSID.Type);
            _lblLanguage.Text = LangMan.LS(LSID.Language);
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            _btnAccept.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Accept);
            _btnCancel.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Cancel);
        }
    }
}
