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
using GKCommon.GEDCOM;
using GKCore.Interfaces;
using GKCore.Options;
using GKCore.Types;
using GKCore.UIContracts;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class PersonalNameEditDlgController : DialogController<IPersonalNameEditDlg>
    {
        private GEDCOMPersonalName fPersonalName;

        public GEDCOMPersonalName PersonalName
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
            for (GEDCOMNameType nt = GEDCOMNameType.ntNone; nt <= GEDCOMNameType.ntMarried; nt++) {
                fView.NameType.Add(LangMan.LS(GKData.NameTypes[(int)nt]));
            }

            for (var lid = GEDCOMLanguageID.Unknown; lid < GEDCOMLanguageEnum.LastVal; lid++) {
                fView.Language.AddItem(GEDCOMLanguageEnum.Instance.GetStrValue(lid), lid);
            }
            fView.Language.SortItems();
        }

        public override bool Accept()
        {
            try {
                GKUtils.SetNameParts(fPersonalName, fView.Surname.Text, fView.Name.Text, fView.Patronymic.Text);

                GEDCOMPersonalNamePieces pieces = fPersonalName.Pieces;
                pieces.Nickname = fView.Nickname.Text;
                pieces.Prefix = fView.NamePrefix.Text;
                pieces.SurnamePrefix = fView.SurnamePrefix.Text;
                pieces.Suffix = fView.NameSuffix.Text;

                fPersonalName.NameType = (GEDCOMNameType)fView.NameType.SelectedIndex;
                fPersonalName.Language.Value = (GEDCOMLanguageID)fView.Language.SelectedTag;

                fBase.Context.CollectNameLangs(fPersonalName);

                return true;
            } catch (Exception ex) {
                Logger.LogWrite("PersonalNameEditDlgController.Accept(): " + ex.Message);
                return false;
            }
        }

        private bool IsExtendedWomanSurname()
        {
            GEDCOMIndividualRecord iRec = fPersonalName.Parent as GEDCOMIndividualRecord;

            bool result = (GlobalOptions.Instance.WomanSurnameFormat != WomanSurnameFormat.wsfNotExtend) &&
                (iRec.Sex == GEDCOMSex.svFemale);
            return result;
        }

        public override void UpdateView()
        {
            GEDCOMIndividualRecord iRec = fPersonalName.Parent as GEDCOMIndividualRecord;

            var parts = GKUtils.GetNameParts(iRec, fPersonalName);

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

            GEDCOMLanguageID langID = fPersonalName.Language.Value;
            fView.Language.Text = GEDCOMLanguageEnum.Instance.GetStrValue(langID);
        }
    }
}
