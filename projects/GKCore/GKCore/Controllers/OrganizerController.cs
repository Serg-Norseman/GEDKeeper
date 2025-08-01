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

using System.Collections.Generic;
using BSLib;
using GDModel;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Lists;
using GKCore.Locales;
using GKCore.Options;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public class OrganizerController : DialogController<IOrganizerWin>
    {
        private readonly List<OrgItem> fAdrList;
        private readonly List<OrgItem> fPhonesList;
        private readonly List<OrgItem> fMailsList;
        private readonly List<OrgItem> fWebsList;

        public OrganizerController(IOrganizerWin view) : base(view)
        {
            fAdrList = new List<OrgItem>();
            fPhonesList = new List<OrgItem>();
            fMailsList = new List<OrgItem>();
            fWebsList = new List<OrgItem>();

            fView.AdrList.Buttons = EnumSet<SheetButton>.Create();
            fView.PhonesList.Buttons = EnumSet<SheetButton>.Create();
            fView.MailsList.Buttons = EnumSet<SheetButton>.Create();
            fView.WebsList.Buttons = EnumSet<SheetButton>.Create();
        }

        public override void Init(IBaseWindow baseWin)
        {
            base.Init(baseWin);

            fView.AdrList.ListView.ListMan = new ItemsListModel(LangMan.LS(LSID.Address));
            fView.AdrList.ListView.UpdateContents();

            fView.PhonesList.ListView.ListMan = new ItemsListModel(LangMan.LS(LSID.Telephone));
            fView.PhonesList.ListView.UpdateContents();

            fView.MailsList.ListView.ListMan = new ItemsListModel(LangMan.LS(LSID.Mail));
            fView.MailsList.ListView.UpdateContents();

            fView.WebsList.ListView.ListMan = new ItemsListModel(LangMan.LS(LSID.WebSite));
            fView.WebsList.ListView.UpdateContents();
        }

        public override void UpdateView()
        {
            int num = fBase.Context.Tree.RecordsCount;
            for (int i = 0; i < num; i++) {
                GDMRecord rec = fBase.Context.Tree[i];
                if (rec.RecordType != GDMRecordType.rtIndividual) continue;

                GDMIndividualRecord iRec = (GDMIndividualRecord)rec;
                string nm = GKUtils.GetNameString(iRec, true, false);

                foreach (GDMCustomEvent evt in iRec.Events) {
                    PrepareEvent(nm, evt);
                }
            }

            ((ItemsListModel)fView.AdrList.ListView.ListMan).DataSource = fAdrList;
            fView.AdrList.ListView.UpdateContents();

            ((ItemsListModel)fView.PhonesList.ListView.ListMan).DataSource = fPhonesList;
            fView.PhonesList.ListView.UpdateContents();

            ((ItemsListModel)fView.MailsList.ListView.ListMan).DataSource = fMailsList;
            fView.MailsList.ListView.UpdateContents();

            ((ItemsListModel)fView.WebsList.ListView.ListMan).DataSource = fWebsList;
            fView.WebsList.ListView.UpdateContents();
        }

        private void PrepareEvent(string iName, IGDMStructWithAddress ev)
        {
            if (!ev.HasAddress) return;
            GDMAddress addr = ev.Address;

            string addrStr = addr.Lines.Text.Trim();
            if (addrStr != "") {
                string city = addr.AddressCity;
                if (city != "") {
                    addrStr = city + ", " + addrStr;
                }
                fAdrList.Add(new OrgItem(iName, addrStr));
            }

            foreach (GDMTag tag in addr.PhoneNumbers) {
                fPhonesList.Add(new OrgItem(iName, tag.StringValue));
            }

            foreach (GDMTag tag in addr.EmailAddresses) {
                fMailsList.Add(new OrgItem(iName, tag.StringValue));
            }

            foreach (GDMTag tag in addr.WebPages) {
                fWebsList.Add(new OrgItem(iName, tag.StringValue));
            }
        }

        public override void SetLocale()
        {
            fView.SetTitle(LangMan.LS(LSID.MIOrganizer));

            GetControl<ITabPage>("pageAddresses").Text = LangMan.LS(LSID.Addresses);
            GetControl<ITabPage>("pageTelephones").Text = LangMan.LS(LSID.Telephones);
            GetControl<ITabPage>("pageMails").Text = LangMan.LS(LSID.Mails);
            GetControl<ITabPage>("pageWebs").Text = LangMan.LS(LSID.Webs);
        }

        public override void ApplyTheme()
        {
            // dummy
        }


        private sealed class OrgItem
        {
            public string Individual;
            public string Value;

            public OrgItem(string individual, string value)
            {
                Individual = individual;
                Value = value;
            }
        }


        private sealed class ItemsListModel : SimpleListModel<OrgItem>
        {
            public ItemsListModel(string title) :
                base(null, CreateListColumns(title))
            {
            }

            public static ListColumns CreateListColumns(string title)
            {
                var result = new ListColumns(GKListType.ltNone);
                result.AddColumn(LangMan.LS(LSID.Person), DataType.dtString, 350, true);
                result.AddColumn(title, DataType.dtString, 100, true);
                return result;
            }

            protected override object GetColumnValueEx(int colType, int colSubtype, bool isVisible)
            {
                object result = null;
                switch (colType) {
                    case 0:
                        result = fFetchedRec.Individual;
                        break;
                    case 1:
                        result = fFetchedRec.Value;
                        break;
                }
                return result;
            }
        }
    }
}
