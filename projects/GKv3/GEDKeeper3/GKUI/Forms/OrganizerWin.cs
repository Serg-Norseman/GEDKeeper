/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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
using Eto.Forms;
using GKCommon;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;
using GKCore.Lists;
using GKUI.Components;

namespace GKUI.Forms
{
    /// <summary>
    /// 
    /// </summary>
    public sealed partial class OrganizerWin : Dialog
    {
        private readonly IBaseWindow fBase;
        private readonly GKSheetList fAdrList;
        private readonly GKSheetList fPhonesList;
        private readonly GKSheetList fMailsList;
        private readonly GKSheetList fWebsList;

        public OrganizerWin(IBaseWindow baseWin)
        {
            InitializeComponent();

            fBase = baseWin;

            fAdrList = new GKSheetList(pageAddresses);
            fAdrList.Buttons = EnumSet<SheetButton>.Create();
            fAdrList.AddColumn(LangMan.LS(LSID.LSID_Person), 350, false);
            fAdrList.AddColumn(LangMan.LS(LSID.LSID_Address), 100, false);

            fPhonesList = new GKSheetList(pageTelephones);
            fPhonesList.Buttons = EnumSet<SheetButton>.Create();
            fPhonesList.AddColumn(LangMan.LS(LSID.LSID_Person), 350, false);
            fPhonesList.AddColumn(LangMan.LS(LSID.LSID_Telephone), 100, false);

            fMailsList = new GKSheetList(pageMails);
            fMailsList.Buttons = EnumSet<SheetButton>.Create();
            fMailsList.AddColumn(LangMan.LS(LSID.LSID_Person), 350, false);
            fMailsList.AddColumn(LangMan.LS(LSID.LSID_Mail), 100, false);

            fWebsList = new GKSheetList(pageWebs);
            fWebsList.Buttons = EnumSet<SheetButton>.Create();
            fWebsList.AddColumn(LangMan.LS(LSID.LSID_Person), 350, false);
            fWebsList.AddColumn(LangMan.LS(LSID.LSID_WebSite), 100, false);

            Title = LangMan.LS(LSID.LSID_MIOrganizer);
            pageAddresses.Text = LangMan.LS(LSID.LSID_Addresses);
            pageTelephones.Text = LangMan.LS(LSID.LSID_Telephones);
            pageMails.Text = LangMan.LS(LSID.LSID_Mails);
            pageWebs.Text = LangMan.LS(LSID.LSID_Webs);
        }

        private void OrganizerWin_Load(object sender, EventArgs e)
        {
            CollectData();
        }

        private void CollectData()
        {
            fAdrList.ClearItems();
            fPhonesList.ClearItems();
            fMailsList.ClearItems();
            fWebsList.ClearItems();

            int num = fBase.Context.Tree.RecordsCount;
            for (int i = 0; i < num; i++)
            {
                GEDCOMRecord rec = fBase.Context.Tree[i];
                if (rec.RecordType != GEDCOMRecordType.rtIndividual) continue;

                GEDCOMIndividualRecord iRec = (GEDCOMIndividualRecord)rec;
                string nm = GKUtils.GetNameString(iRec, true, false);

                foreach (GEDCOMCustomEvent evt in iRec.Events) {
                    PrepareEvent(nm, evt);
                }
            }

            fAdrList.ResizeColumn(0);
            fAdrList.ResizeColumn(1);
            fPhonesList.ResizeColumn(0);
            fPhonesList.ResizeColumn(1);
            fMailsList.ResizeColumn(0);
            fMailsList.ResizeColumn(1);
            fWebsList.ResizeColumn(0);
            fWebsList.ResizeColumn(1);
        }

        private static void AddItem(GKSheetList list, string name, string value)
        {
            list.AddItem(null, new object[] { name, value });
        }

        private void PrepareEvent(string iName, GEDCOMCustomEvent ev)
        {
            GEDCOMAddress addr = ev.Address;
            if (addr == null) return;

            string addrStr = addr.Address.Text.Trim();
            if (addrStr != "") {
                AddItem(fAdrList, iName, addrStr);
            }

            foreach (GEDCOMTag tag in addr.PhoneNumbers) {
                AddItem(fPhonesList, iName, tag.StringValue);
            }

            foreach (GEDCOMTag tag in addr.EmailAddresses) {
                AddItem(fMailsList, iName, tag.StringValue);
            }

            foreach (GEDCOMTag tag in addr.WebPages) {
                AddItem(fWebsList, iName, tag.StringValue);
            }
        }

        private void OrganizerWin_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.Key == Keys.Escape) Close();
        }
    }
}
