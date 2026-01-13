/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Design.Views;
using GKCore.Lists;

namespace GKUI.Forms
{
    public sealed partial class OrganizerWin : CommonDialog<IOrganizerWin, OrganizerController>, IOrganizerWin
    {
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

            fController = new OrganizerController(this);
            fController.Init(baseWin);
        }

        private void OrganizerWin_Load(object sender, EventArgs e)
        {
            fController.UpdateView();
        }
    }
}
