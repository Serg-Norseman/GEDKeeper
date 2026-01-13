/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections.Generic;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Lists;
using GKCore.Locales;
using GKCore.Options;
using GKUI.Themes;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public class LanguageSelectDlgController : DialogController<ILanguageSelectDlg>
    {
        private readonly LangRecord fDefLang;
        private readonly LangsListModel fLangsModel;
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

            if (GlobalOptions.Instance.Languages.Count > 0) {
                fLangsModel.DataSource = GlobalOptions.Instance.Languages;
                fDefLang = GlobalOptions.Instance.GetLangByCode(LangMan.LS_DEF_CODE);
            } else {
                var langs = new List<LangRecord>();
                // unit-testing and some other cases
                fDefLang = new LangRecord(LangMan.LS_DEF_CODE, LangMan.LS_DEF_SIGN, LangMan.LS_DEF_NAME, "English.lng", null);
                langs.Add(fDefLang);
                fLangsModel.DataSource = langs;
            }

            fView.LanguagesList.UpdateContents();
            fView.LanguagesList.Activate();
            fView.LanguagesList.SelectItem(fDefLang);
        }

        public override bool Accept()
        {
            try {
                var lngRec = fView.LanguagesList.GetSelectedData() as LangRecord;
                fSelectedLanguage = (lngRec != null) ? lngRec.Code : fDefLang.Code;

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
