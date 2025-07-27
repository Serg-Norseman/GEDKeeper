/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih.
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
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Design.Views;
using GKCore.Lists;
using GKUI.Components;

namespace GKUI.Forms
{
    public sealed partial class OrganizerWin : CommonDialog<IOrganizerWin, OrganizerController>, IOrganizerWin
    {
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
            fPhonesList = new GKSheetList(pageTelephones);
            fMailsList = new GKSheetList(pageMails);
            fWebsList = new GKSheetList(pageWebs);

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
