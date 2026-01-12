/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using Eto.Forms;
using Eto.Serialization.Xaml;
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Design.Views;
using GKCore.Lists;
using GKUI.Components;

namespace GKUI.Forms
{
    public sealed partial class OrganizerWin : CommonDialog<IOrganizerWin, OrganizerController>, IOrganizerWin
    {
        #region Design components
#pragma warning disable CS0169, CS0649, IDE0044, IDE0051

        private TabPage pageAddresses;
        private TabPage pageTelephones;
        private TabPage pageMails;
        private TabPage pageWebs;
        private GKSheetList fAdrList;
        private GKSheetList fPhonesList;
        private GKSheetList fMailsList;
        private GKSheetList fWebsList;

#pragma warning restore CS0169, CS0649, IDE0044, IDE0051
        #endregion

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
            XamlReader.Load(this);

            fController = new OrganizerController(this);
            fController.Init(baseWin);
        }

        private void OrganizerWin_Load(object sender, EventArgs e)
        {
            fController.UpdateView();
        }

        private void OrganizerWin_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.Key == Keys.Escape) Close();
        }
    }
}
