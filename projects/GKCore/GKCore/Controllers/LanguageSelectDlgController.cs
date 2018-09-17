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
using GKCore.Options;
using GKCore.UIContracts;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public class LanguageSelectDlgController : DialogController<ILanguageSelectDlg>
    {
        private int fSelectedLanguage;

        public int SelectedLanguage
        {
            get { return fSelectedLanguage; }
            set {
                if (fSelectedLanguage != value) {
                    fSelectedLanguage = value;
                    UpdateView();
                }
            }
        }


        public LanguageSelectDlgController(ILanguageSelectDlg view) : base(view)
        {
            fView.LanguagesList.ClearItems();
            foreach (LangRecord lngRec in GlobalOptions.Instance.Languages) {
                fView.LanguagesList.AddItem(lngRec, lngRec.Name);
            }

            // unit-testing and some other cases
            if (fView.LanguagesList.Items.Count == 0) {
                fView.LanguagesList.AddItem(new LangRecord(LangMan.LS_DEF_CODE, LangMan.LS_DEF_SIGN, LangMan.LS_DEF_NAME, ""), LangMan.LS_DEF_NAME);
            }

            fView.LanguagesList.SelectItem(GlobalOptions.Instance.GetLangByCode(LangMan.LS_DEF_CODE));
            fView.LanguagesList.Select();
        }

        public override bool Accept()
        {
            try {
                LangRecord lngRec = fView.LanguagesList.GetSelectedData() as LangRecord;
                fSelectedLanguage = lngRec.Code;

                return true;
            } catch (Exception ex) {
                Logger.LogWrite("LanguageSelectDlgController.Accept(): " + ex.Message);
                return false;
            }
        }

        public override void UpdateView()
        {
        }
    }
}
