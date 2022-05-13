/*
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

using System;
using BSLib.Design.MVP.Controls;
using GDModel;
using GDModel.Providers.GEDCOM;
using GKCore.Cultures;
using GKCore.Interfaces;
using GKCore.MVP;
using GKCore.MVP.Views;
using GKCore.Options;
using GKCore.Types;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class PersonalNameEditDlgController : DialogController<IPersonalNameEditDlg>
    {
        private GDMIndividualRecord fIndividual;
        private GDMPersonalName fPersonalName;

        public GDMIndividualRecord Individual
        {
            get { return fIndividual; }
            set { fIndividual = value; }
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

            for (var lid = GDMLanguageID.Unknown; lid < GDMLanguageID.Yiddish; lid++) {
                fView.Language.AddItem(GEDCOMUtils.GetLanguageStr(lid), lid);
            }
            fView.Language.Sort();
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
            bool result = (GlobalOptions.Instance.WomanSurnameFormat != WomanSurnameFormat.wsfNotExtend) &&
                (fIndividual.Sex == GDMSex.svFemale);
            return result;
        }

        public override void UpdateView()
        {
            var parts = GKUtils.GetNameParts(fBase.Context.Tree, fIndividual, fPersonalName, false);

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
                fView.SurnameLabel.Text = LangMan.LS(LSID.LSID_Surname);
                fView.MarriedSurname.Enabled = false;
            } else {
                fView.SurnameLabel.Text = LangMan.LS(LSID.LSID_MaidenSurname);
                fView.MarriedSurname.Enabled = true;
            }

            ICulture culture = parts.Culture;
            fView.Surname.Enabled = fView.Surname.Enabled && culture.HasSurname();
            fView.Patronymic.Enabled = fView.Patronymic.Enabled && culture.HasPatronymic();

            GDMLanguageID langID = fPersonalName.Language;
            fView.Language.Text = GEDCOMUtils.GetLanguageStr(langID);
        }

        public void UpdateLanguage()
        {
            var selectedLanguageId = GetSelectedLanguageID();
            var culture = CulturesPool.DefineCulture(selectedLanguageId);
            fView.Surname.Enabled = culture.HasSurname();
            fView.Patronymic.Enabled = culture.HasPatronymic();
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
            fView.Title = LangMan.LS(LSID.LSID_Name);

            GetControl<IButton>("btnAccept").Text = LangMan.LS(LSID.LSID_DlgAccept);
            GetControl<IButton>("btnCancel").Text = LangMan.LS(LSID.LSID_DlgCancel);
            GetControl<ILabel>("lblSurname").Text = LangMan.LS(LSID.LSID_Surname);
            GetControl<ILabel>("lblMarriedSurname").Text = LangMan.LS(LSID.LSID_MarriedSurname);
            GetControl<ILabel>("lblName").Text = LangMan.LS(LSID.LSID_Name);
            GetControl<ILabel>("lblPatronymic").Text = LangMan.LS(LSID.LSID_Patronymic);
            GetControl<ILabel>("lblNickname").Text = LangMan.LS(LSID.LSID_Nickname);
            GetControl<ILabel>("lblSurnamePrefix").Text = LangMan.LS(LSID.LSID_SurnamePrefix);
            GetControl<ILabel>("lblNamePrefix").Text = LangMan.LS(LSID.LSID_NamePrefix);
            GetControl<ILabel>("lblNameSuffix").Text = LangMan.LS(LSID.LSID_NameSuffix);
            GetControl<ILabel>("lblType").Text = LangMan.LS(LSID.LSID_Type);
            GetControl<ILabel>("lblLanguage").Text = LangMan.LS(LSID.LSID_Language);
        }
    }
}
