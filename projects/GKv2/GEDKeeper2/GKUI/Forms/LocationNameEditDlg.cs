/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GDModel;
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;

namespace GKUI.Forms
{
    public sealed partial class LocationNameEditDlg : CommonDialog<ILocationNameEditDlg, LocationNameEditDlgController>, ILocationNameEditDlg
    {
        public GDMLocationName LocationName
        {
            get { return fController.LocationName; }
            set { fController.LocationName = value; }
        }

        #region View Interface

        ITextBox ILocationNameEditDlg.NameText
        {
            get { return fController.GetControlHandler<ITextBox>(txtTitle); }
        }

        ITextBox ILocationNameEditDlg.AbbrText
        {
            get { return fController.GetControlHandler<ITextBox>(txtShortTitle); }
        }

        IDateControl ILocationNameEditDlg.DateCtl
        {
            get { return fController.GetControlHandler<IDateControl>(dateCtl); }
        }

        #endregion

        public LocationNameEditDlg(IBaseWindow baseWin)
        {
            InitializeComponent();

            fController = new LocationNameEditDlgController(this);
            fController.InitView(baseWin);
        }
    }
}
