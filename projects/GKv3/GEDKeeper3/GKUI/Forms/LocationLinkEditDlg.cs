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
using GDModel;
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKUI.Components;

namespace GKUI.Forms
{
    public sealed partial class LocationLinkEditDlg : CommonDialog<ILocationLinkEditDlg, LocationLinkEditDlgController>, ILocationLinkEditDlg
    {
        #region Design components
#pragma warning disable CS0169, CS0649, IDE0044, IDE0051

        private Button btnAccept;
        private Button btnCancel;
        private Label lblLocation;
        private TextBox txtTopLevel;
        private Button btnLocationAdd;
        private Label lblDate;
        private GKDateControl dateCtl;

#pragma warning restore CS0169, CS0649, IDE0044, IDE0051
        #endregion

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
            XamlReader.Load(this);

            fController = new LocationLinkEditDlgController(this);
            fController.Init(baseWin);
        }

        private void btnLocationAdd_Click(object sender, EventArgs e)
        {
            fController.SetLocation();
        }
    }
}
