/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2018 by Sergey V. Zhdanovskih.
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

using GDModel;
using GKCore.MVP;
using GKCore.MVP.Views;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public class OrganizerController : DialogController<IOrganizerWin>
    {
        public OrganizerController(IOrganizerWin view) : base(view)
        {
        }

        public override void UpdateView()
        {
            fView.AdrList.ClearItems();
            fView.PhonesList.ClearItems();
            fView.MailsList.ClearItems();
            fView.WebsList.ClearItems();

            int num = fBase.Context.Tree.RecordsCount;
            for (int i = 0; i < num; i++) {
                GDMRecord rec = fBase.Context.Tree[i];
                if (rec.RecordType != GEDCOMRecordType.rtIndividual) continue;

                GDMIndividualRecord iRec = (GDMIndividualRecord)rec;
                string nm = GKUtils.GetNameString(iRec, true, false);

                foreach (GDMCustomEvent evt in iRec.Events) {
                    PrepareEvent(nm, evt);
                }
            }

            fView.AdrList.ResizeColumn(0);
            fView.AdrList.ResizeColumn(1);
            fView.PhonesList.ResizeColumn(0);
            fView.PhonesList.ResizeColumn(1);
            fView.MailsList.ResizeColumn(0);
            fView.MailsList.ResizeColumn(1);
            fView.WebsList.ResizeColumn(0);
            fView.WebsList.ResizeColumn(1);
        }

        private void PrepareEvent(string iName, GDMCustomEvent ev)
        {
            GDMAddress addr = ev.Address;
            if (addr == null) return;

            string addrStr = addr.Address.Text.Trim();
            if (addrStr != "") {
                string city = addr.AddressCity;
                if (city != "") {
                    addrStr = city + ", " + addrStr;
                }
                fView.AdrList.AddItem(null, iName, addrStr);
            }

            foreach (GDMTag tag in addr.PhoneNumbers) {
                fView.PhonesList.AddItem(null, iName, tag.StringValue);
            }

            foreach (GDMTag tag in addr.EmailAddresses) {
                fView.MailsList.AddItem(null, iName, tag.StringValue);
            }

            foreach (GDMTag tag in addr.WebPages) {
                fView.WebsList.AddItem(null, iName, tag.StringValue);
            }
        }
    }
}
