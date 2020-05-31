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

using System;
using GDModel;
using GDModel.Providers.GEDCOM;
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
        private GDMPersonalName fPersonalName;

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
            fView.Language.SortItems();
        }

        public override bool Accept()
        {
            try {
                GKUtils.SetNameParts(fPersonalName, fView.Surname.Text, fView.Name.Text, fView.Patronymic.Text);

                GDMPersonalNamePieces pieces = fPersonalName.Pieces;
                pieces.Nickname = fView.Nickname.Text;
                pieces.Prefix = fView.NamePrefix.Text;
                pieces.SurnamePrefix = fView.SurnamePrefix.Text;
                pieces.Suffix = fView.NameSuffix.Text;

                fPersonalName.NameType = (GDMNameType)fView.NameType.SelectedIndex;
                fPersonalName.Language = fView.Language.GetSelectedTag<GDMLanguageID>();

                fBase.Context.CollectNameLangs(fPersonalName);

                return true;
            } catch (Exception ex) {
                Logger.LogWrite("PersonalNameEditDlgController.Accept(): " + ex.Message);
                return false;
            }
        }

        private bool IsExtendedWomanSurname()
        {
            GDMIndividualRecord iRec = fPersonalName.Owner as GDMIndividualRecord;

            bool result = (GlobalOptions.Instance.WomanSurnameFormat != WomanSurnameFormat.wsfNotExtend) &&
                (iRec.Sex == GDMSex.svFemale);
            return result;
        }

        public override void UpdateView()
        {
            GDMIndividualRecord iRec = fPersonalName.Owner as GDMIndividualRecord;

            var parts = GKUtils.GetNameParts(iRec, fPersonalName, false);

            fView.Surname.Text = parts.Surname;
            fView.Name.Text = parts.Name;
            fView.Patronymic.Text = parts.Patronymic;
            fView.NameType.SelectedIndex = (sbyte)fPersonalName.NameType;

            fView.NamePrefix.Text = fPersonalName.Pieces.Prefix;
            fView.Nickname.Text = fPersonalName.Pieces.Nickname;
            fView.SurnamePrefix.Text = fPersonalName.Pieces.SurnamePrefix;
            fView.NameSuffix.Text = fPersonalName.Pieces.Suffix;

            fView.MarriedSurname.Text = fPersonalName.Pieces.MarriedName;

            if (!IsExtendedWomanSurname()) {
                fView.SurnameLabel.Text = LangMan.LS(LSID.LSID_Surname);
                fView.MarriedSurname.Enabled = false;
            } else {
                fView.SurnameLabel.Text = LangMan.LS(LSID.LSID_MaidenSurname);
                fView.MarriedSurname.Enabled = true;
            }

            ICulture culture = fBase.Context.Culture;
            fView.Surname.Enabled = fView.Surname.Enabled && culture.HasSurname();
            fView.Patronymic.Enabled = fView.Patronymic.Enabled && culture.HasPatronymic();

            GDMLanguageID langID = fPersonalName.Language;
            fView.Language.Text = GEDCOMUtils.GetLanguageStr(langID);
        }
    }
}
