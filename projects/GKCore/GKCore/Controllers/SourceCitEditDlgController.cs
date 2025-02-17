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
using BSLib;
using GDModel;
using GKCore.Design.Controls;
using GKCore.Interfaces;
using GKCore.Design;
using GKCore.Design.Views;
using GKCore.Types;
using GKUI.Themes;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class SourceCitEditDlgController : DialogController<ISourceCitEditDlg>
    {
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

            for (int i = 0; i < GKData.CertaintyAssessments.Length; i++) {
                fView.Certainty.Add(LangMan.LS(GKData.CertaintyAssessments[i]));
            }
        }

        public override bool Accept()
        {
            try {
                int idx = fSourcesList.IndexOf(fView.Source.Text);
                GDMSourceRecord sourceRec = ((idx < 0) ? null : (fSourcesList.GetObject(idx) as GDMSourceRecord));

                if (sourceRec == null) {
                    AppHost.StdDialogs.ShowError(LangMan.LS(LSID.DoNotSetSource));

                    return false;
                } else {
                    fSourceCitation.XRef = sourceRec.XRef;
                    fSourceCitation.Page = fView.Page.Text;
                    fSourceCitation.CertaintyAssessment = fView.Certainty.SelectedIndex;

                    fSourceCitation.Data.Date.ParseString(fView.DataDate.Date.StringValue);
                    fSourceCitation.Data.Text.Clear();
                    fSourceCitation.Data.Text.Lines.Text = fView.DataText.Text;

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
            if (src != null) fView.Source.Text = src.ShortTitle;

            fView.Page.Text = fSourceCitation.Page;
            fView.Certainty.SelectedIndex = fSourceCitation.CertaintyAssessment;

            fView.DataDate.Date = fSourceCitation.Data.Date.Value;
            fView.DataText.Text = fSourceCitation.Data.Text.Lines.Text.Trim();
        }

        public async void AddSource()
        {
            object[] anArgs = new object[0];
            GDMSourceRecord src = await fBase.Context.SelectRecord(fView, GDMRecordType.rtSource, anArgs) as GDMSourceRecord;
            if (src == null) return;

            fBase.Context.GetSourcesList(fSourcesList);
            RefreshSourcesList("");
            fView.Source.Text = src.ShortTitle;
        }

        public override void Init(IBaseWindow baseWin)
        {
            base.Init(baseWin);

            fBase.Context.GetSourcesList(fSourcesList);
            RefreshSourcesList("");

            fView.Source.Activate();
        }

        public void RefreshSourcesList(string filter)
        {
            fView.Source.BeginUpdate();
            try {
                fView.Source.Clear();

                string flt = "*" + filter + "*";

                int num = fSourcesList.Count;
                for (int i = 0; i < num; i++) {
                    string st = fSourcesList[i];

                    if (filter == "" || GKUtils.MatchesMask(st, flt)) {
                        fView.Source.AddItem(st, fSourcesList.GetObject(i));
                    }
                }
            } finally {
                fView.Source.EndUpdate();
            }
        }

        public override void SetLocale()
        {
            fView.Title = LangMan.LS(LSID.WinSourceCitEdit);

            GetControl<IButton>("btnAccept").Text = LangMan.LS(LSID.DlgAccept);
            GetControl<IButton>("btnCancel").Text = LangMan.LS(LSID.DlgCancel);
            GetControl<ITabPage>("pageCommon").Text = LangMan.LS(LSID.Common);
            GetControl<ITabPage>("pageOther").Text = LangMan.LS(LSID.Other);
            GetControl<ILabel>("lblSource").Text = LangMan.LS(LSID.Source);
            GetControl<ILabel>("lblPage").Text = LangMan.LS(LSID.Page);
            GetControl<ILabel>("lblCertainty").Text = LangMan.LS(LSID.Certainty);

            SetToolTip("btnSourceAdd", LangMan.LS(LSID.SourceAddTip));
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            GetControl<IButton>("btnAccept").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Accept);
            GetControl<IButton>("btnCancel").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Cancel);

            GetControl<IButton>("btnSourceAdd").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Attach, true);
        }
    }
}
