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
    public sealed class FilePropertiesDlgController : DialogController<IFilePropertiesDlg>
    {
        #region Design components
        private IButton _btnAccept;
        private IButton _btnCancel;
        private ITabPage _pageAuthor;
        private ILabel _lblName;
        private ILabel _lblAddress;
        private ILabel _lblTelephone;
        private ITextBox _txtName;
        private ITextBox _txtTel;
        private ITextBox _txtAddress;
        private ITabPage _pageOther;
        private IListView _lvRecordStats;
        //private IButton _btnLangEdit;
        private IComboBox _txtLanguage;
        private ILabel _lblLanguage;
        #endregion

        private readonly FlatListModel fListModel;

        public FilePropertiesDlgController(IFilePropertiesDlg view) : base(view)
        {
            fListModel = new FlatListModel();
            fListModel.ListColumns.Clear();
            fListModel.ListColumns.AddColumn(LangMan.LS(LSID.RM_Records), DataType.dtString, 300, false);
            fListModel.ListColumns.AddColumn(LangMan.LS(LSID.Count), DataType.dtInteger, 100, false);
            fListModel.ListColumns.ResetDefaults();
        }

        public override void InitView(IBaseWindow baseWin)
        {
            base.InitView(baseWin);

            _lvRecordStats.ListMan = fListModel;

            _txtLanguage.ReadOnly = true;
            for (var lid = GDMLanguageID.Unknown; lid < GDMLanguageID.Yiddish; lid++) {
                _txtLanguage.AddItem(GEDCOMUtils.GetLanguageStr(lid), lid);
            }
        }

        public override bool Accept()
        {
            try {
                fBase.Context.Tree.Header.Language = _txtLanguage.GetSelectedTag<GDMLanguageID>();

                GDMSubmitterRecord submitter = fBase.Context.Tree.GetSubmitter();
                submitter.Name = _txtName.Text;
                submitter.Address.SetAddressArray(_txtAddress.Lines);

                if (submitter.Address.PhoneNumbers.Count > 0) {
                    submitter.Address.PhoneNumbers[0].StringValue = _txtTel.Text;
                } else {
                    submitter.Address.AddPhoneNumber(_txtTel.Text);
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
            _txtLanguage.SetSelectedTag(fBase.Context.Tree.Header.Language);

            GDMSubmitterRecord submitter = fBase.Context.Tree.GetSubmitter();
            _txtName.Text = submitter.Name;
            _txtAddress.Text = submitter.Address.Lines.Text;

            if (submitter.Address.PhoneNumbers.Count > 0) {
                _txtTel.Text = submitter.Address.PhoneNumbers[0].StringValue;
            }

            // update stats
            int[] stats = fBase.Context.Tree.GetRecordStats();
            for (int i = 1; i < stats.Length; i++) {
                fListModel.AddItem(null, LangMan.LS(GKData.RecordTypes[i].Name), stats[i].ToString());
            }
            _lvRecordStats.UpdateContents();
        }

        public override void SetLocale()
        {
            fView.SetTitle(LangMan.LS(LSID.MIFileProperties));

            _btnAccept.Text = LangMan.LS(LSID.DlgAccept);
            _btnCancel.Text = LangMan.LS(LSID.DlgCancel);
            _pageAuthor.Text = LangMan.LS(LSID.Author);
            _lblName.Text = LangMan.LS(LSID.GeneralName);
            _lblAddress.Text = LangMan.LS(LSID.Address);
            _lblTelephone.Text = LangMan.LS(LSID.Telephone);
            _pageOther.Text = LangMan.LS(LSID.Other);
            _lblLanguage.Text = LangMan.LS(LSID.Language);
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            _btnAccept.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Accept);
            _btnCancel.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Cancel);
        }
    }
}
