/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using GDModel;
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;

namespace GKUI.Forms
{
    public sealed partial class LocationLinkEditDlg : CommonDialog<ILocationLinkEditDlg, LocationLinkEditDlgController>, ILocationLinkEditDlg
    {
        public GDMLocationLink LocationLink
        {
            get { return fController.LocationLink; }
            set { fController.LocationLink = value; }
        }

        #region View Interface

        ITextBox ILocationLinkEditDlg.TopLevelText
        {
            get { return GetControlHandler<ITextBox>(txtTopLevel); }
        }

        IDateControl ILocationLinkEditDlg.DateCtl
        {
            get { return GetControlHandler<IDateControl>(dateCtl); }
        }

        #endregion

        public LocationLinkEditDlg(IBaseWindow baseWin)
        {
            InitializeComponent();

            fController = new LocationLinkEditDlgController(this);
            fController.Init(baseWin);
        }

        private void btnLocationAdd_Click(object sender, EventArgs e)
        {
            fController.SetLocation();
        }
    }
}
