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
using System.Collections.Generic;
using GDModel;
using GDModel.Providers.GEDCOM;
using GKCore.Cultures;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Interfaces;
using GKCore.Options;
using GKCore.Types;
using GKUI.Themes;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class PersonalNameEditDlgController : DialogController<IPersonalNameEditDlg>
    {
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
            for (GDMNameType nt = GDMNameType.ntNone; nt <= GDMNameType.ntMarried; nt++) {
                fView.NameType.Add(LangMan.LS(GKData.NameTypes[(int)nt]));
            }
        }

        public override bool Accept()
        {
            try {
                GKUtils.SetNameParts(fPersonalName, fView.Surname.Text, fView.Name.Text, fView.Patronymic.Text);

                fPersonalName.Nickname = fView.Nickname.Text;
                fPersonalName.NamePrefix = fView.NamePrefix.Text;
                fPersonalName.SurnamePrefix = fView.SurnamePrefix.Text;
                fPersonalName.NameSuffix = fView.NameSuffix.Text;

                fPersonalName.NameType = (GDMNameType)fView.NameType.SelectedIndex;
                fPersonalName.Language = fView.Language.GetSelectedTag<GDMLanguageID>();

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

            fView.Surname.Text = parts.Surname;
            fView.Name.Text = parts.Name;
            fView.Patronymic.Text = parts.Patronymic;
            fView.NameType.SelectedIndex = (sbyte)fPersonalName.NameType;

            fView.NamePrefix.Text = fPersonalName.NamePrefix;
            fView.Nickname.Text = fPersonalName.Nickname;
            fView.SurnamePrefix.Text = fPersonalName.SurnamePrefix;
            fView.NameSuffix.Text = fPersonalName.NameSuffix;

            fView.MarriedSurname.Text = fPersonalName.MarriedName;

            if (!IsExtendedWomanSurname()) {
                fView.SurnameLabel.Text = LangMan.LS(LSID.Surname);
                fView.MarriedSurname.Enabled = false;
            } else {
                fView.SurnameLabel.Text = LangMan.LS(LSID.MaidenSurname);
                fView.MarriedSurname.Enabled = true;
            }

            ICulture culture = parts.Culture;
            fView.Surname.Enabled = fView.Surname.Enabled && culture.HasSurname;
            fView.Patronymic.Enabled = fView.Patronymic.Enabled && culture.HasPatronymic;

            GDMLanguageID langID = fPersonalName.Language;
            fView.Language.Text = GEDCOMUtils.GetLanguageStr(langID);
        }

        public void UpdateLanguage()
        {
            var selectedLanguageId = GetSelectedLanguageID();
            var culture = CulturesPool.DefineCulture(selectedLanguageId);
            fView.Surname.Enabled = culture.HasSurname;
            fView.Patronymic.Enabled = culture.HasPatronymic;
        }

        private void FillLanguages()
        {
            var freqList = new List<FreqItem<GDMLanguageID>>();

            var langStats = fBase.Context.LangStats;
            for (var lid = GDMLanguageID.Unknown; lid < GDMLanguageID.Yiddish; lid++) {
                int stat = (lid == GDMLanguageID.Unknown) ? 1000 : langStats.GetValue(lid);
                freqList.Add(new FreqItem<GDMLanguageID>(lid, GEDCOMUtils.GetLanguageStr(lid), stat));
                //fView.Language.AddItem(GEDCOMUtils.GetLanguageStr(lid), lid);
            }

            FreqCollection<string>.PopulateCombo(fView.Language, freqList, GDMLanguageID.Unknown);
        }

        private GDMLanguageID GetSelectedLanguageID()
        {
            var result = fView.Language.GetSelectedTag<GDMLanguageID>();
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
            fView.Title = LangMan.LS(LSID.GeneralName);

            GetControl<IButton>("btnAccept").Text = LangMan.LS(LSID.DlgAccept);
            GetControl<IButton>("btnCancel").Text = LangMan.LS(LSID.DlgCancel);
            GetControl<ILabel>("lblSurname").Text = LangMan.LS(LSID.Surname);
            GetControl<ILabel>("lblMarriedSurname").Text = LangMan.LS(LSID.MarriedSurname);
            GetControl<ILabel>("lblName").Text = LangMan.LS(LSID.GivenName);
            GetControl<ILabel>("lblPatronymic").Text = LangMan.LS(LSID.Patronymic);
            GetControl<ILabel>("lblNickname").Text = LangMan.LS(LSID.Nickname);
            GetControl<ILabel>("lblSurnamePrefix").Text = LangMan.LS(LSID.SurnamePrefix);
            GetControl<ILabel>("lblNamePrefix").Text = LangMan.LS(LSID.NamePrefix);
            GetControl<ILabel>("lblNameSuffix").Text = LangMan.LS(LSID.NameSuffix);
            GetControl<ILabel>("lblType").Text = LangMan.LS(LSID.Type);
            GetControl<ILabel>("lblLanguage").Text = LangMan.LS(LSID.Language);
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            GetControl<IButton>("btnAccept").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Accept);
            GetControl<IButton>("btnCancel").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Cancel);
        }
    }
}
