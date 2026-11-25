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
using GKUI.Themes;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class SourceCitEditDlgController : DialogController<ISourceCitEditDlg>
    {
        #region Design components
#pragma warning disable CS0649, IDE0044
        private IButton _btnAccept;
        private IButton _btnCancel;
        private ITabPage _pageCommon;
        private ITabPage _pageOther;
        private ILabel _lblSource;
        private IComboBox _cmbSource;
        private IButton _btnSourceAdd;
        private ILabel _lblPage;
        private ITextBox _txtPage;
        private ILabel _lblCertainty;
        private IComboBox _txtCertainty;
        private IDateControl _dateCtl;
        private ITextBox _txtText;
#pragma warning restore CS0649, IDE0044
        #endregion

        private GDMSourceCitation fSourceCitation;
        private readonly StringList fSourcesList;

        public GDMSourceCitation SourceCitation
        {
            get { return fSourceCitation; }
            set {
                if (fSourceCitation != value) {
                    fSourceCitation = value;
                    UpdateView();
                }
            }
        }


        public SourceCitEditDlgController(ISourceCitEditDlg view) : base(view)
        {
            fSourcesList = new StringList();
        }

        public override bool Accept()
        {
            try {
                int idx = fSourcesList.IndexOf(_cmbSource.Text);
                GDMSourceRecord sourceRec = ((idx < 0) ? null : (fSourcesList.GetObject(idx) as GDMSourceRecord));

                if (sourceRec == null) {
                    AppHost.StdDialogs.ShowError(LangMan.LS(LSID.DoNotSetSource));

                    return false;
                } else {
                    fSourceCitation.XRef = sourceRec.XRef;
                    fSourceCitation.Page = _txtPage.Text;
                    fSourceCitation.CertaintyAssessment = _txtCertainty.SelectedIndex;

                    fSourceCitation.Data.Date.ParseString(_dateCtl.Date.StringValue);
                    fSourceCitation.Data.Text.Clear();
                    fSourceCitation.Data.Text.Lines.Text = _txtText.Text;

                    return true;
                }
            } catch (Exception ex) {
                Logger.WriteError("SourceCitEditDlgController.Accept()", ex);
                return false;
            }
        }

        public override void UpdateView()
        {
            var src = fBase.Context.Tree.GetPtrValue<GDMSourceRecord>(fSourceCitation);
            if (src != null) _cmbSource.Text = src.ShortTitle;

            _txtPage.Text = fSourceCitation.Page;
            _txtCertainty.SelectedIndex = fSourceCitation.CertaintyAssessment;

            _dateCtl.Date = fSourceCitation.Data.Date.Value;
            _txtText.Text = fSourceCitation.Data.Text.Lines.Text.Trim();
        }

        public async void AddSource()
        {
            object[] anArgs = Array.Empty<object>();
            GDMSourceRecord src = await BaseController.SelectRecord(fView, fBase, GDMRecordType.rtSource, anArgs) as GDMSourceRecord;
            if (src == null) return;

            fBase.Context.GetSourcesList(fSourcesList);
            RefreshSourcesList("");
            _cmbSource.Text = src.ShortTitle;
        }

        public override void InitView(IBaseWindow baseWin)
        {
            base.InitView(baseWin);

            _txtCertainty.ReadOnly = true;
            for (int i = 0; i < GKData.CertaintyAssessments.Length; i++) {
                _txtCertainty.Add(LangMan.LS(GKData.CertaintyAssessments[i]));
            }

            fBase.Context.GetSourcesList(fSourcesList);
            RefreshSourcesList("");

            _cmbSource.Activate();
        }

        public void RefreshSourcesList(string filter)
        {
            _cmbSource.BeginUpdate();
            try {
                _cmbSource.Clear();

                string flt = "*" + filter + "*";

                int num = fSourcesList.Count;
                for (int i = 0; i < num; i++) {
                    string st = fSourcesList[i];

                    if (filter == "" || GKUtils.MatchesMask(st, flt)) {
                        _cmbSource.AddItem(st, fSourcesList.GetObject(i));
                    }
                }
            } finally {
                _cmbSource.EndUpdate();
            }
        }

        public override void SetLocale()
        {
            fView.SetTitle(LangMan.LS(LSID.SourceCit));

            _btnAccept.Text = LangMan.LS(LSID.DlgAccept);
            _btnCancel.Text = LangMan.LS(LSID.DlgCancel);
            _pageCommon.Text = LangMan.LS(LSID.Common);
            _pageOther.Text = LangMan.LS(LSID.Other);
            _lblSource.Text = LangMan.LS(LSID.Source);
            _lblPage.Text = LangMan.LS(LSID.Page);
            _lblCertainty.Text = LangMan.LS(LSID.Certainty);

            SetToolTip("btnSourceAdd", LangMan.LS(LSID.SourceAddTip));
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            _btnAccept.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Accept);
            _btnCancel.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Cancel);

            _btnSourceAdd.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Attach, true);
        }
    }
}
