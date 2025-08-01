﻿/*
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
using System.Threading.Tasks;
using GDModel;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.Types;
using GKUI.Themes;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class AddressEditDlgController : DialogController<IAddressEditDlg>
    {
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
            fView.PhonesList.OnModify += ListModify;
            fView.MailsList.OnModify += ListModify;
            fView.WebsList.OnModify += ListModify;
        }

        public override void Init(IBaseWindow baseWin)
        {
            base.Init(baseWin);

            fView.PhonesList.ListView.ListMan = new TagsListModel(fBase.Context, LangMan.LS(LSID.Telephone));
            fView.PhonesList.ListView.UpdateContents();

            fView.MailsList.ListView.ListMan = new TagsListModel(fBase.Context, LangMan.LS(LSID.Mail));
            fView.MailsList.ListView.UpdateContents();

            fView.WebsList.ListView.ListMan = new TagsListModel(fBase.Context, LangMan.LS(LSID.WebSite));
            fView.WebsList.ListView.UpdateContents();
        }

        public override bool Accept()
        {
            try {
                fAddress.AddressCountry = fView.Country.Text;
                fAddress.AddressState = fView.State.Text;
                fAddress.AddressCity = fView.City.Text;
                fAddress.AddressPostalCode = fView.PostalCode.Text;
                fAddress.SetAddressText(fView.AddressLine.Text);

                if (!Validate(fAddress)) return false;

                return true;
            } catch (Exception ex) {
                Logger.WriteError("AddressEditController.Accept()", ex);
                return false;
            }
        }

        public override void UpdateView()
        {
            fView.Country.Text = fAddress.AddressCountry;
            fView.State.Text = fAddress.AddressState;
            fView.City.Text = fAddress.AddressCity;
            fView.PostalCode.Text = fAddress.AddressPostalCode;
            fView.AddressLine.Text = fAddress.Lines.Text.Trim();

            UpdateLists();
        }

        public void UpdateLists()
        {
            ((TagsListModel)fView.PhonesList.ListView.ListMan).DataSource = fAddress.PhoneNumbers.GetList();
            fView.PhonesList.ListView.UpdateContents();

            ((TagsListModel)fView.MailsList.ListView.ListMan).DataSource = fAddress.EmailAddresses.GetList();
            fView.MailsList.ListView.UpdateContents();

            ((TagsListModel)fView.WebsList.ListView.ListMan).DataSource = fAddress.WebPages.GetList();
            fView.WebsList.ListView.UpdateContents();
        }

        private async void ListModify(object sender, ModifyEventArgs eArgs)
        {
            GDMTag itemTag = eArgs.ItemData as GDMTag;
            if (sender == fView.PhonesList) {
                await DoPhonesAction(eArgs.Action, itemTag);
            } else if (sender == fView.MailsList) {
                await DoMailsAction(eArgs.Action, itemTag);
            } else if (sender == fView.WebsList) {
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

            GetControl<IButton>("btnAccept").Text = LangMan.LS(LSID.DlgAccept);
            GetControl<IButton>("btnCancel").Text = LangMan.LS(LSID.DlgCancel);
            GetControl<ITabPage>("pageCommon").Text = LangMan.LS(LSID.Address);
            GetControl<ILabel>("lblCountry").Text = LangMan.LS(LSID.AdCountry);
            GetControl<ILabel>("lblState").Text = LangMan.LS(LSID.AdState);
            GetControl<ILabel>("lblCity").Text = LangMan.LS(LSID.AdCity);
            GetControl<ILabel>("lblPostalCode").Text = LangMan.LS(LSID.AdPostalCode);
            GetControl<ILabel>("lblAddress").Text = LangMan.LS(LSID.AddressLine);
            GetControl<ITabPage>("pagePhones").Text = LangMan.LS(LSID.Telephones);
            GetControl<ITabPage>("pageEmails").Text = LangMan.LS(LSID.EMails);
            GetControl<ITabPage>("pageWebPages").Text = LangMan.LS(LSID.WebSites);
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            GetControl<IButton>("btnAccept").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Accept);
            GetControl<IButton>("btnCancel").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Cancel);
        }
    }
}
