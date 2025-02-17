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
using GDModel;
using GDModel.Providers.GEDCOM;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Types;
using GKUI.Themes;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class FilePropertiesDlgController : DialogController<IFilePropertiesDlg>
    {

        public FilePropertiesDlgController(IFilePropertiesDlg view) : base(view)
        {
            if (AppHost.Instance.HasFeatureSupport(Feature.Mobile)) {
                var mobileView = view as IMobileFilePropertiesDlg;
                for (var lid = GDMLanguageID.Unknown; lid < GDMLanguageID.Yiddish; lid++) {
                    mobileView.LanguageCombo.AddItem(GEDCOMUtils.GetLanguageStr(lid), lid);
                }
            }
        }

        public override bool Accept()
        {
            try {
                if (!AppHost.Instance.HasFeatureSupport(Feature.Mobile)) {
                    fBase.Context.Tree.Header.Language = GEDCOMUtils.GetLanguageVal(fView.Language.Text);
                } else {
                    var mobileView = fView as IMobileFilePropertiesDlg;
                    fBase.Context.Tree.Header.Language = mobileView.LanguageCombo.GetSelectedTag<GDMLanguageID>();
                }

                GDMSubmitterRecord submitter = fBase.Context.Tree.GetSubmitter();
                submitter.Name = fView.Name.Text;
                submitter.Address.SetAddressArray(fView.Address.Lines);

                if (submitter.Address.PhoneNumbers.Count > 0) {
                    submitter.Address.PhoneNumbers[0].StringValue = fView.Tel.Text;
                } else {
                    submitter.Address.AddPhoneNumber(fView.Tel.Text);
                }

                fBase.NotifyRecord(submitter, RecordAction.raEdit);

                return true;
            } catch (Exception ex) {
                Logger.WriteError("FilePropertiesDlgController.Accept()", ex);
                return false;
            }
        }

        public override void UpdateView()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Mobile)) {
                fView.Language.Text = GEDCOMUtils.GetLanguageStr(fBase.Context.Tree.Header.Language);
            } else {
                var mobileView = fView as IMobileFilePropertiesDlg;
                mobileView.LanguageCombo.Text = GEDCOMUtils.GetLanguageStr(fBase.Context.Tree.Header.Language);
            }

            GDMSubmitterRecord submitter = fBase.Context.Tree.GetSubmitter();
            fView.Name.Text = submitter.Name;
            fView.Address.Text = submitter.Address.Lines.Text;

            if (submitter.Address.PhoneNumbers.Count > 0) {
                fView.Tel.Text = submitter.Address.PhoneNumbers[0].StringValue;
            }

            // update stats
            int[] stats = fBase.Context.Tree.GetRecordStats();
            fView.RecordStats.ClearItems();
            for (int i = 1; i < stats.Length; i++) {
                fView.RecordStats.AddItem(null, LangMan.LS(GKData.RecordTypes[i].Name), stats[i].ToString());
            }
        }

        public async void ChangeLanguage()
        {
            if (AppHost.Instance.HasFeatureSupport(Feature.Mobile))
                return;

            using (var dlg = AppHost.ResolveDialog<ILanguageEditDlg>()) {
                dlg.LanguageID = fBase.Context.Tree.Header.Language;

                if (await AppHost.Instance.ShowModalAsync(dlg, fView)) {
                    // Assignment in control, instead of the header's property to work Cancel.
                    fView.Language.Text = GEDCOMUtils.GetLanguageStr(dlg.LanguageID);
                }
            }
        }

        public override void SetLocale()
        {
            fView.Title = LangMan.LS(LSID.MIFileProperties);

            GetControl<IButton>("btnAccept").Text = LangMan.LS(LSID.DlgAccept);
            GetControl<IButton>("btnCancel").Text = LangMan.LS(LSID.DlgCancel);
            GetControl<ITabPage>("pageAuthor").Text = LangMan.LS(LSID.Author);
            GetControl<ILabel>("lblName").Text = LangMan.LS(LSID.GeneralName);
            GetControl<ILabel>("lblAddress").Text = LangMan.LS(LSID.Address);
            GetControl<ILabel>("lblTelephone").Text = LangMan.LS(LSID.Telephone);
            GetControl<ITabPage>("pageOther").Text = LangMan.LS(LSID.Other);
            GetControl<ILabel>("lblLanguage").Text = LangMan.LS(LSID.Language);

            fView.RecordStats.ClearColumns();
            fView.RecordStats.AddColumn(LangMan.LS(LSID.RM_Records), 300, false);
            fView.RecordStats.AddColumn(LangMan.LS(LSID.Count), 100, false, BSDTypes.HorizontalAlignment.Right);
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            GetControl<IButton>("btnAccept").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Accept);
            GetControl<IButton>("btnCancel").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Cancel);
        }
    }
}
