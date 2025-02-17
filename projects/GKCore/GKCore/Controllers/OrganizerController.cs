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

using BSLib;
using GDModel;
using GKCore.Design.Controls;
using GKCore.Lists;
using GKCore.Design;
using GKCore.Design.Views;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public class OrganizerController : DialogController<IOrganizerWin>
    {
        public OrganizerController(IOrganizerWin view) : base(view)
        {
            fView.AdrList.Buttons = EnumSet<SheetButton>.Create();
            fView.PhonesList.Buttons = EnumSet<SheetButton>.Create();
            fView.MailsList.Buttons = EnumSet<SheetButton>.Create();
            fView.WebsList.Buttons = EnumSet<SheetButton>.Create();
        }

        public override void UpdateView()
        {
            fView.AdrList.ListView.ClearItems();
            fView.PhonesList.ListView.ClearItems();
            fView.MailsList.ListView.ClearItems();
            fView.WebsList.ListView.ClearItems();

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

            fView.AdrList.ListView.ResizeColumn(0);
            fView.AdrList.ListView.ResizeColumn(1);
            fView.PhonesList.ListView.ResizeColumn(0);
            fView.PhonesList.ListView.ResizeColumn(1);
            fView.MailsList.ListView.ResizeColumn(0);
            fView.MailsList.ListView.ResizeColumn(1);
            fView.WebsList.ListView.ResizeColumn(0);
            fView.WebsList.ListView.ResizeColumn(1);
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
                fView.AdrList.ListView.AddItem(null, iName, addrStr);
            }

            foreach (GDMTag tag in addr.PhoneNumbers) {
                fView.PhonesList.ListView.AddItem(null, iName, tag.StringValue);
            }

            foreach (GDMTag tag in addr.EmailAddresses) {
                fView.MailsList.ListView.AddItem(null, iName, tag.StringValue);
            }

            foreach (GDMTag tag in addr.WebPages) {
                fView.WebsList.ListView.AddItem(null, iName, tag.StringValue);
            }
        }

        public override void SetLocale()
        {
            fView.Title = LangMan.LS(LSID.MIOrganizer);

            GetControl<ITabPage>("pageAddresses").Text = LangMan.LS(LSID.Addresses);
            GetControl<ITabPage>("pageTelephones").Text = LangMan.LS(LSID.Telephones);
            GetControl<ITabPage>("pageMails").Text = LangMan.LS(LSID.Mails);
            GetControl<ITabPage>("pageWebs").Text = LangMan.LS(LSID.Webs);

            fView.AdrList.ListView.AddColumn(LangMan.LS(LSID.Person), 350, false);
            fView.AdrList.ListView.AddColumn(LangMan.LS(LSID.Address), 100, false);

            fView.PhonesList.ListView.AddColumn(LangMan.LS(LSID.Person), 350, false);
            fView.PhonesList.ListView.AddColumn(LangMan.LS(LSID.Telephone), 100, false);

            fView.MailsList.ListView.AddColumn(LangMan.LS(LSID.Person), 350, false);
            fView.MailsList.ListView.AddColumn(LangMan.LS(LSID.Mail), 100, false);

            fView.WebsList.ListView.AddColumn(LangMan.LS(LSID.Person), 350, false);
            fView.WebsList.ListView.AddColumn(LangMan.LS(LSID.WebSite), 100, false);
        }

        public override void ApplyTheme()
        {
            // dummy
        }
    }
}
