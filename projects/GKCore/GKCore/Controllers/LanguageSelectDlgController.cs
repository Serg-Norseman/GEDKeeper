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
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.Options;
using GKCore.Types;
using GKUI.Themes;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public class LanguageSelectDlgController : DialogController<ILanguageSelectDlg>
    {
        private LangsListModel fLangsModel;
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
            fLangsModel = new LangsListModel();
            fView.LanguagesList.ListMan = fLangsModel;

            LangRecord defLang;
            if (GlobalOptions.Instance.Languages.Count > 0) {
                fLangsModel.DataSource = GlobalOptions.Instance.Languages;
                defLang = GlobalOptions.Instance.GetLangByCode(LangMan.LS_DEF_CODE);
            } else {
                var langs = new List<LangRecord>();
                // unit-testing and some other cases
                defLang = new LangRecord(LangMan.LS_DEF_CODE, LangMan.LS_DEF_SIGN, LangMan.LS_DEF_NAME, "English.lng", null);
                langs.Add(defLang);
                fLangsModel.DataSource = langs;
            }

            fView.LanguagesList.UpdateContents();
            fView.LanguagesList.Activate();
            fView.LanguagesList.SelectItem(defLang);
        }

        public override bool Accept()
        {
            try {
                LangRecord lngRec = fView.LanguagesList.GetSelectedData() as LangRecord;
                fSelectedLanguage = lngRec.Code;

                return true;
            } catch (Exception ex) {
                Logger.WriteError("LanguageSelectDlgController.Accept()", ex);
                return false;
            }
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            GetControl<IButton>("btnAccept").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Accept);
            GetControl<IButton>("btnCancel").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Cancel);
        }


        private sealed class LangsListModel : SimpleListModel<LangRecord>
        {
            public LangsListModel() : base()
            {
                ListColumns.AddColumn("Language", DataType.dtString, 260, false);
                ListColumns.ResetDefaults();
            }

            protected override object GetColumnValueEx(int colType, int colSubtype, bool isVisible)
            {
                return fFetchedRec.Name;
            }
        }
    }
}
