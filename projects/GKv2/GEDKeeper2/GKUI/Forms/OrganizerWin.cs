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

using System;
using System.Windows.Forms;
using BSLib;
using GKCore;
using GKCore.Controllers;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.MVP.Views;
using GKUI.Components;

namespace GKUI.Forms
{
    public sealed partial class OrganizerWin : CommonDialog, IOrganizerWin
    {
        private readonly OrganizerController fController;

        private readonly GKSheetList fAdrList;
        private readonly GKSheetList fPhonesList;
        private readonly GKSheetList fMailsList;
        private readonly GKSheetList fWebsList;

        #region View Interface

        ISheetList IOrganizerWin.AdrList
        {
            get { return fAdrList; }
        }

        ISheetList IOrganizerWin.PhonesList
        {
            get { return fPhonesList; }
        }

        ISheetList IOrganizerWin.MailsList
        {
            get { return fMailsList; }
        }

        ISheetList IOrganizerWin.WebsList
        {
            get { return fWebsList; }
        }

        #endregion

        public OrganizerWin(IBaseWindow baseWin)
        {
            InitializeComponent();

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

            Text = LangMan.LS(LSID.LSID_MIOrganizer);
            pageAddresses.Text = LangMan.LS(LSID.LSID_Addresses);
            pageTelephones.Text = LangMan.LS(LSID.LSID_Telephones);
            pageMails.Text = LangMan.LS(LSID.LSID_Mails);
            pageWebs.Text = LangMan.LS(LSID.LSID_Webs);

            fController = new OrganizerController(this);
            fController.Init(baseWin);
        }

        private void OrganizerWin_Load(object sender, EventArgs e)
        {
            fController.UpdateView();
        }

        private void OrganizerWin_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.KeyCode == Keys.Escape) Close();
        }
    }
}
