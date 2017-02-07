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
using System.Windows.Forms;

using GKCommon;
using GKCommon.Controls;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;
using GKUI.Controls;

namespace GKUI
{
    /// <summary>
    /// 
    /// </summary>
    public partial class OrganizerWin : Form
    {
        private readonly IBaseWindow fBase;
        private readonly GKSheetList fAdrList;
        private readonly GKSheetList fPhonesList;
        private readonly GKSheetList fMailsList;
        private readonly GKSheetList fWebsList;

        public OrganizerWin(IBaseWindow baseWin)
        {
            this.InitializeComponent();

            this.fBase = baseWin;
            
            this.fAdrList = new GKSheetList(this.pageAddresses);
            this.fAdrList.Buttons = EnumSet<SheetButton>.Create();
            this.fAdrList.AddColumn(LangMan.LS(LSID.LSID_Person), 350, false);
            this.fAdrList.AddColumn(LangMan.LS(LSID.LSID_Address), 100, false);
            
            this.fPhonesList = new GKSheetList(this.pageTelephones);
            this.fPhonesList.Buttons = EnumSet<SheetButton>.Create();
            this.fPhonesList.AddColumn(LangMan.LS(LSID.LSID_Person), 350, false);
            this.fPhonesList.AddColumn(LangMan.LS(LSID.LSID_Telephone), 100, false);
            
            this.fMailsList = new GKSheetList(this.pageMails);
            this.fMailsList.Buttons = EnumSet<SheetButton>.Create();
            this.fMailsList.AddColumn(LangMan.LS(LSID.LSID_Person), 350, false);
            this.fMailsList.AddColumn(LangMan.LS(LSID.LSID_Mail), 100, false);
            
            this.fWebsList = new GKSheetList(this.pageWebs);
            this.fWebsList.Buttons = EnumSet<SheetButton>.Create();
            this.fWebsList.AddColumn(LangMan.LS(LSID.LSID_Person), 350, false);
            this.fWebsList.AddColumn(LangMan.LS(LSID.LSID_WebSite), 100, false);
            
            this.Text = LangMan.LS(LSID.LSID_MIOrganizer);
            this.pageAddresses.Text = LangMan.LS(LSID.LSID_Addresses);
            this.pageTelephones.Text = LangMan.LS(LSID.LSID_Telephones);
            this.pageMails.Text = LangMan.LS(LSID.LSID_Mails);
            this.pageWebs.Text = LangMan.LS(LSID.LSID_Webs);
        }

        private void OrganizerWin_Load(object sender, EventArgs e)
        {
            this.CollectData();
        }

        private void CollectData()
        {
            this.fAdrList.ClearItems();
            this.fPhonesList.ClearItems();
            this.fMailsList.ClearItems();
            this.fWebsList.ClearItems();

            int num = this.fBase.Tree.RecordsCount;
            for (int i = 0; i < num; i++)
            {
                GEDCOMRecord rec = this.fBase.Tree[i];
                if (rec.RecordType != GEDCOMRecordType.rtIndividual) continue;

                GEDCOMIndividualRecord iRec = (GEDCOMIndividualRecord)rec;
                string nm = GKUtils.GetNameString(iRec, true, false);

                foreach (GEDCOMCustomEvent evt in iRec.Events) {
                    this.PrepareEvent(nm, evt);
                }
            }

            this.fAdrList.ResizeColumn(0);
            this.fAdrList.ResizeColumn(1);
            this.fPhonesList.ResizeColumn(0);
            this.fPhonesList.ResizeColumn(1);
            this.fMailsList.ResizeColumn(0);
            this.fMailsList.ResizeColumn(1);
            this.fWebsList.ResizeColumn(0);
            this.fWebsList.ResizeColumn(1);
        }

        private static void AddItem(GKSheetList list, string name, string value)
        {
            GKListItem item = list.AddItem(name, null);
            item.AddSubItem(value);
        }

        private void PrepareEvent(string iName, GEDCOMCustomEvent ev)
        {
            GEDCOMAddress addr = ev.Detail.Address;
            if (addr == null) return;
            
            string addrStr = addr.Address.Text.Trim();
            if (addrStr != "") {
                AddItem(this.fAdrList, iName, addrStr);
            }

            foreach (GEDCOMTag tag in addr.PhoneNumbers) {
                AddItem(this.fPhonesList, iName, tag.StringValue);
            }

            foreach (GEDCOMTag tag in addr.EmailAddresses) {
                AddItem(this.fMailsList, iName, tag.StringValue);
            }

            foreach (GEDCOMTag tag in addr.WebPages) {
                AddItem(this.fWebsList, iName, tag.StringValue);
            }
        }

        private void OrganizerWin_KeyDown(object sender, KeyEventArgs e)
        {
            switch (e.KeyCode)
            {
                case Keys.Escape:
                    base.Close();
                    break;
            }
        }
    }
}
