/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using GDModel;
using GDModel.Providers.GEDCOM;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Lists;
using GKCore.Locales;
using GKUI.Themes;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class FilePropertiesDlgController : DialogController<IFilePropertiesDlg>
    {
        private readonly FlatListModel fListModel;

        public FilePropertiesDlgController(IFilePropertiesDlg view) : base(view)
        {
            fListModel = new FlatListModel();
            fView.RecordStats.ListMan = fListModel;

            for (var lid = GDMLanguageID.Unknown; lid < GDMLanguageID.Yiddish; lid++) {
                fView.Language.AddItem(GEDCOMUtils.GetLanguageStr(lid), lid);
            }
        }

        public override void Init(IBaseWindow baseWin)
        {
            base.Init(baseWin);

            fListModel.ListColumns.Clear();
            fListModel.ListColumns.AddColumn(LangMan.LS(LSID.RM_Records), DataType.dtString, 300, false);
            fListModel.ListColumns.AddColumn(LangMan.LS(LSID.Count), DataType.dtInteger, 100, false);
            fListModel.ListColumns.ResetDefaults();
        }

        public override bool Accept()
        {
            try {
                fBase.Context.Tree.Header.Language = fView.Language.GetSelectedTag<GDMLanguageID>();

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
            fView.Language.Text = GEDCOMUtils.GetLanguageStr(fBase.Context.Tree.Header.Language);

            GDMSubmitterRecord submitter = fBase.Context.Tree.GetSubmitter();
            fView.Name.Text = submitter.Name;
            fView.Address.Text = submitter.Address.Lines.Text;

            if (submitter.Address.PhoneNumbers.Count > 0) {
                fView.Tel.Text = submitter.Address.PhoneNumbers[0].StringValue;
            }

            // update stats
            int[] stats = fBase.Context.Tree.GetRecordStats();
            for (int i = 1; i < stats.Length; i++) {
                fListModel.AddItem(null, LangMan.LS(GKData.RecordTypes[i].Name), stats[i].ToString());
            }
            fView.RecordStats.UpdateContents();
        }

        public override void SetLocale()
        {
            fView.SetTitle(LangMan.LS(LSID.MIFileProperties));

            GetControl<IButton>("btnAccept").Text = LangMan.LS(LSID.DlgAccept);
            GetControl<IButton>("btnCancel").Text = LangMan.LS(LSID.DlgCancel);
            GetControl<ITabPage>("pageAuthor").Text = LangMan.LS(LSID.Author);
            GetControl<ILabel>("lblName").Text = LangMan.LS(LSID.GeneralName);
            GetControl<ILabel>("lblAddress").Text = LangMan.LS(LSID.Address);
            GetControl<ILabel>("lblTelephone").Text = LangMan.LS(LSID.Telephone);
            GetControl<ITabPage>("pageOther").Text = LangMan.LS(LSID.Other);
            GetControl<ILabel>("lblLanguage").Text = LangMan.LS(LSID.Language);
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            GetControl<IButton>("btnAccept").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Accept);
            GetControl<IButton>("btnCancel").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Cancel);
        }
    }
}
