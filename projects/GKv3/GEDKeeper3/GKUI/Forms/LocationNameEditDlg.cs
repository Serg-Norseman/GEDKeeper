/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

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
    public sealed partial class LocationNameEditDlg : CommonDialog<ILocationNameEditDlg, LocationNameEditDlgController>, ILocationNameEditDlg
    {
        #region Design components
#pragma warning disable CS0169, CS0649, IDE0044, IDE0051

        private Button btnAccept;
        private Button btnCancel;
        private Label lblTitle;
        private TextBox txtTitle;
        private Label lblShortTitle;
        private TextBox txtShortTitle;
        private Label lblDate;
        private GKDateControl dateCtl;

#pragma warning restore CS0169, CS0649, IDE0044, IDE0051
        #endregion

        public GDMLocationName LocationName
        {
            get { return fController.LocationName; }
            set { fController.LocationName = value; }
        }

        #region View Interface

        ITextBox ILocationNameEditDlg.NameText
        {
            get { return GetControlHandler<ITextBox>(txtTitle); }
        }

        ITextBox ILocationNameEditDlg.AbbrText
        {
            get { return GetControlHandler<ITextBox>(txtShortTitle); }
        }

        IDateControl ILocationNameEditDlg.DateCtl
        {
            get { return GetControlHandler<IDateControl>(dateCtl); }
        }

        #endregion

        public LocationNameEditDlg(IBaseWindow baseWin)
        {
            XamlReader.Load(this);

            fController = new LocationNameEditDlgController(this);
            fController.Init(baseWin);
        }
    }
}
