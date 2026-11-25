/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using BSLib;
using GDModel;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Locales;
using GKCore.Options;
using GKCore.Search;
using GKUI.Themes;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public class FARDlgController : DialogController<IFARDlg>
    {
        #region Design components
#pragma warning disable CS0649, IDE0044
        private ILabel _lblPattern;
        private IComboBox _cmbPattern;
        private ILabel _lblReplacement;
        private IComboBox _cmbReplacement;
        private ICheckBox _chkMatchCase;
        private ICheckBox _chkMatchWildcards;
        private ICheckBox _chkWholeWord;
        private IGroupBox _gbFilters;
        private ILabel _lblRecord;
        private IComboBox _cmbRecord;
        private ILabel _lblProperty;
        private IComboBox _cmbProperty;
        private IButton _btnPrev;
        private IButton _btnNext;
        private IButton _btnReplace;
        private IButton _btnReplaceAll;
#pragma warning restore CS0649, IDE0044
        #endregion

        private FARParameters fParameters;
        private FARStrategy fStrategy;


        public FARDlgController(IFARDlg view) : base(view)
        {
        }

        public override void InitView(IBaseWindow baseWin)
        {
            base.InitView(baseWin);

            _cmbProperty.ReadOnly = true;
            _cmbRecord.ReadOnly = true;
        }

        public override void SetLocale()
        {
            fView.SetTitle(LangMan.LS(LSID.FindAndReplace));

            _lblPattern.Text = LangMan.LS(LSID.Find);
            _lblReplacement.Text = LangMan.LS(LSID.ReplaceWith);

            _chkMatchCase.Text = LangMan.LS(LSID.MatchCase);
            _chkMatchWildcards.Text = LangMan.LS(LSID.MatchWildcards);
            _chkWholeWord.Text = LangMan.LS(LSID.WholeWord);
            _gbFilters.Text = LangMan.LS(LSID.MIFilter);
            _lblRecord.Text = LangMan.LS(LSID.Record);
            _lblProperty.Text = LangMan.LS(LSID.Property);

            _btnPrev.Text = LangMan.LS(LSID.Prev);
            _btnNext.Text = LangMan.LS(LSID.Next);
            _btnReplace.Text = LangMan.LS(LSID.Replace);
            _btnReplaceAll.Text = LangMan.LS(LSID.ReplaceAll);

            for (var rt = GDMRecordType.rtIndividual; rt <= GDMRecordType.rtLocation; rt++) {
                _cmbRecord.AddItem(LangMan.LS(GKData.RecordTypes[(int)rt].Name), rt);
            }
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            _btnPrev.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Prev);
            _btnNext.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Next);
        }

        private void CheckListValue(StringList list, string value)
        {
            string val = value.Trim();
            if (!string.IsNullOrEmpty(val) && list.IndexOf(val) < 0) {
                list.Add(val);
            }
        }

        public override void UpdateView()
        {
            _cmbPattern.AddStrings(GlobalOptions.Instance.FARPatterns);
            _cmbPattern.ReadOnly = false;

            _cmbReplacement.AddStrings(GlobalOptions.Instance.FARReplacements);
            _cmbReplacement.ReadOnly = false;

            _chkWholeWord.Enabled = false; // TODO: next version

            var selectedRecType = fBase.GetSelectedRecordType();
            _cmbRecord.SetSelectedTag(selectedRecType);
            _cmbRecord.Enabled = false;

            FARPropertyType firstPropType = FARPropertyType.ptNone;

            for (var pt = FARPropertyType.ptName; pt <= FARPropertyType.ptLast; pt++) {
                var propStruct = GKData.FARPropertyTypes[(int)pt];
                if (propStruct.Enabled && propStruct.RecTypes.Contains(selectedRecType)) {
                    _cmbProperty.AddItem(LangMan.LS(propStruct.Name), pt);

                    if (firstPropType == FARPropertyType.ptNone) firstPropType = pt;
                }
            }

            if (firstPropType != FARPropertyType.ptNone)
                _cmbProperty.SetSelectedTag(firstPropType);
        }

        private void GetParameters()
        {
            bool paramsChanged = false;

            var pattern = _cmbPattern.Text;
            if (fParameters.Pattern != pattern) paramsChanged = true;

            var replacement = _cmbReplacement.Text;
            if (fParameters.Replacement != replacement) paramsChanged = true;

            var matchCase = _chkMatchCase.Checked;
            if (fParameters.MatchCase != matchCase) paramsChanged = true;

            var matchWildcards = _chkMatchWildcards.Checked;
            if (fParameters.MatchWildcards != matchWildcards) paramsChanged = true;

            var wholeWord = _chkWholeWord.Checked;
            if (fParameters.WholeWord != wholeWord) paramsChanged = true;

            var recordType = _cmbRecord.GetSelectedTag<GDMRecordType>();
            if (fParameters.RecordType != recordType) paramsChanged = true;

            var propertyType = _cmbProperty.GetSelectedTag<FARPropertyType>();
            if (fParameters.PropertyType != propertyType) paramsChanged = true;

            if (paramsChanged) {
                fParameters.Pattern = pattern;
                fParameters.Replacement = replacement;
                fParameters.MatchCase = matchCase;
                fParameters.MatchWildcards = matchWildcards;
                fParameters.WholeWord = wholeWord;
                fParameters.RecordType = recordType;
                fParameters.PropertyType = propertyType;
                fStrategy = new FARStrategy(fBase, fParameters);
            }

            // save values
            CheckListValue(GlobalOptions.Instance.FARPatterns, pattern);
            CheckListValue(GlobalOptions.Instance.FARReplacements, replacement);
        }

        private void SelectResult(SearchResult result)
        {
            if (result != null && result.Record != null)
                fBase.SelectByRec(result.Record);
        }

        private bool RequireResults()
        {
            GetParameters();

            if (fStrategy == null || !fStrategy.HasResults()) {
                AppHost.StdDialogs.ShowError(LangMan.LS(LSID.NoMatchesFound));
                return false;
            }

            return true;
        }

        public void Prev()
        {
            try {
                if (RequireResults())
                    SelectResult(fStrategy.FindPrev() as SearchResult);
            } catch (Exception ex) {
                Logger.WriteError("FARDlgController.Prev()", ex);
            }
        }

        public void Next()
        {
            try {
                if (RequireResults())
                    SelectResult(fStrategy.FindNext() as SearchResult);
            } catch (Exception ex) {
                Logger.WriteError("FARDlgController.Next()", ex);
            }
        }

        public void Replace()
        {
            try {
                if (RequireResults())
                    fStrategy.ReplaceCurrent();
            } catch (Exception ex) {
                Logger.WriteError("FARDlgController.Replace()", ex);
            }
        }

        public void ReplaceAll()
        {
            try {
                if (RequireResults())
                    fStrategy.ReplaceAll();
            } catch (Exception ex) {
                Logger.WriteError("FARDlgController.ReplaceAll()", ex);
            }
        }
    }
}
