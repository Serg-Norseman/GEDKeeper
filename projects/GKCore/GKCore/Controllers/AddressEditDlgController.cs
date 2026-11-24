/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Threading.Tasks;
using GDModel;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Lists;
using GKCore.Locales;
using GKUI.Themes;

namespace GKCore.Controllers
{
    public sealed class AddressEditDlgController : DialogController<IAddressEditDlg>
    {
        #region Design components
#pragma warning disable CS0649, IDE0044
        private IButton _btnAccept;
        private IButton _btnCancel;
        private ITabPage _pageCommon;
        private ITabPage _pagePhones;
        private ITabPage _pageEmails;
        private ITabPage _pageWebPages;
        private ILabel _lblCountry;
        private ILabel _lblState;
        private ILabel _lblCity;
        private ILabel _lblPostalCode;
        private ILabel _lblAddress;
        private ITextBox _txtCountry;
        private ITextBox _txtState;
        private ITextBox _txtCity;
        private ITextBox _txtPostalCode;
        private ITextBox _txtAddress;
        private ISheetList _fPhonesList;
        private ISheetList _fMailsList;
        private ISheetList _fWebsList;
#pragma warning restore CS0649, IDE0044
        #endregion

        private GDMAddress fAddress;

        public GDMAddress Address
        {
            get { return fAddress; }
            set {
                if (fAddress != value) {
                    fAddress = value;
                    UpdateView();
                }
            }
        }


        public AddressEditDlgController(IAddressEditDlg view) : base(view)
        {
        }

        public override void InitView(IBaseWindow baseWin)
        {
            base.InitView(baseWin);

            _fPhonesList.OnModify += ListModify;
            _fMailsList.OnModify += ListModify;
            _fWebsList.OnModify += ListModify;

            _fPhonesList.ListView.ListMan = new TagsListModel(fBase.Context, LangMan.LS(LSID.Telephone));
            _fPhonesList.ListView.UpdateContents();

            _fMailsList.ListView.ListMan = new TagsListModel(fBase.Context, LangMan.LS(LSID.Mail));
            _fMailsList.ListView.UpdateContents();

            _fWebsList.ListView.ListMan = new TagsListModel(fBase.Context, LangMan.LS(LSID.WebSite));
            _fWebsList.ListView.UpdateContents();
        }

        public override bool Accept()
        {
            try {
                fAddress.AddressCountry = _txtCountry.Text;
                fAddress.AddressState = _txtState.Text;
                fAddress.AddressCity = _txtCity.Text;
                fAddress.AddressPostalCode = _txtPostalCode.Text;
                fAddress.SetAddressText(_txtAddress.Text);

                if (!Validate(fAddress)) return false;

                return true;
            } catch (Exception ex) {
                Logger.WriteError("AddressEditController.Accept()", ex);
                return false;
            }
        }

        public override void UpdateView()
        {
            _txtCountry.Text = fAddress.AddressCountry;
            _txtState.Text = fAddress.AddressState;
            _txtCity.Text = fAddress.AddressCity;
            _txtPostalCode.Text = fAddress.AddressPostalCode;
            _txtAddress.Text = fAddress.Lines.Text.Trim();

            UpdateLists();
        }

        public void UpdateLists()
        {
            ((TagsListModel)_fPhonesList.ListView.ListMan).DataSource = fAddress.PhoneNumbers.GetList();
            _fPhonesList.ListView.UpdateContents();

            ((TagsListModel)_fMailsList.ListView.ListMan).DataSource = fAddress.EmailAddresses.GetList();
            _fMailsList.ListView.UpdateContents();

            ((TagsListModel)_fWebsList.ListView.ListMan).DataSource = fAddress.WebPages.GetList();
            _fWebsList.ListView.UpdateContents();
        }

        private async void ListModify(object sender, ModifyEventArgs eArgs)
        {
            GDMTag itemTag = eArgs.ItemData as GDMTag;
            if (sender == _fPhonesList) {
                await DoPhonesAction(eArgs.Action, itemTag);
            } else if (sender == _fMailsList) {
                await DoMailsAction(eArgs.Action, itemTag);
            } else if (sender == _fWebsList) {
                await DoWebsAction(eArgs.Action, itemTag);
            }
        }

        private async Task<string> GetInput(string title, string val)
        {
            string strResult = await AppHost.StdDialogs.GetInput(fView, title, val);
            return strResult;
        }

        public async Task DoPhonesAction(RecordAction action, GDMTag itemTag)
        {
            if ((action == RecordAction.raEdit || action == RecordAction.raDelete) && (itemTag == null)) return;

            string val;
            switch (action) {
                case RecordAction.raAdd:
                    val = "";
                    if (!string.IsNullOrEmpty(val = await GetInput(LangMan.LS(LSID.Telephone), val))) {
                        fAddress.AddPhoneNumber(val);
                    }
                    break;

                case RecordAction.raEdit:
                    val = itemTag.StringValue;
                    if (!string.IsNullOrEmpty(val = await GetInput(LangMan.LS(LSID.Telephone), val))) {
                        itemTag.StringValue = val;
                    }
                    break;

                case RecordAction.raDelete:
                    fAddress.PhoneNumbers.Remove(itemTag);
                    break;
            }
            UpdateLists();
        }

        public async Task DoMailsAction(RecordAction action, GDMTag itemTag)
        {
            if ((action == RecordAction.raEdit || action == RecordAction.raDelete) && (itemTag == null)) return;

            string val;
            switch (action) {
                case RecordAction.raAdd:
                    val = "";
                    if (!string.IsNullOrEmpty(val = await GetInput(LangMan.LS(LSID.Mail), val))) {
                        fAddress.AddEmailAddress(val);
                    }
                    break;

                case RecordAction.raEdit:
                    val = itemTag.StringValue;
                    if (!string.IsNullOrEmpty(val = await GetInput(LangMan.LS(LSID.Mail), val))) {
                        itemTag.StringValue = val;
                    }
                    break;

                case RecordAction.raDelete:
                    fAddress.EmailAddresses.Remove(itemTag);
                    break;
            }
            UpdateLists();
        }

        public async Task DoWebsAction(RecordAction action, GDMTag itemTag)
        {
            if ((action == RecordAction.raEdit || action == RecordAction.raDelete) && (itemTag == null)) return;

            string val;
            switch (action) {
                case RecordAction.raAdd:
                    val = "";
                    if (!string.IsNullOrEmpty(val = await GetInput(LangMan.LS(LSID.WebSite), val))) {
                        fAddress.AddWebPage(val);
                    }
                    break;

                case RecordAction.raEdit:
                    val = itemTag.StringValue;
                    if (!string.IsNullOrEmpty(val = await GetInput(LangMan.LS(LSID.WebSite), val))) {
                        itemTag.StringValue = val;
                    }
                    break;

                case RecordAction.raDelete:
                    fAddress.WebPages.Remove(itemTag);
                    break;
            }
            UpdateLists();
        }

        public override void SetLocale()
        {
            fView.SetTitle(LangMan.LS(LSID.Address));

            _btnAccept.Text = LangMan.LS(LSID.DlgAccept);
            _btnCancel.Text = LangMan.LS(LSID.DlgCancel);
            _pageCommon.Text = LangMan.LS(LSID.Address);
            _lblCountry.Text = LangMan.LS(LSID.AdCountry);
            _lblState.Text = LangMan.LS(LSID.AdState);
            _lblCity.Text = LangMan.LS(LSID.AdCity);
            _lblPostalCode.Text = LangMan.LS(LSID.AdPostalCode);
            _lblAddress.Text = LangMan.LS(LSID.AddressLine);
            _pagePhones.Text = LangMan.LS(LSID.Telephones);
            _pageEmails.Text = LangMan.LS(LSID.EMails);
            _pageWebPages.Text = LangMan.LS(LSID.WebSites);
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            _btnAccept.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Accept);
            _btnCancel.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Cancel);
        }
    }
}
