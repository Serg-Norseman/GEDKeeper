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
using GKCore.Design.Controls;
using GKCore.Design.Views;

namespace GKUI.Forms
{
    public sealed partial class TTPlacesManagerDlg : CommonDialog<IPlacesManagerDlg, PlacesManagerController>, IPlacesManagerDlg
    {
        #region View Interface

        ITextBox IPlacesManagerDlg.FilterBox
        {
            get { return GetControlHandler<ITextBox>(txtFilter); }
        }

        IListView IPlacesManagerDlg.PlacesList
        {
            get { return ListPlaces; }
        }

        #endregion

        public TTPlacesManagerDlg(IBaseWindow baseWin)
        {
            InitializeComponent();

            fController = new PlacesManagerController(this);
            fController.Init(baseWin);
        }

        private void btnAnalysePlaces_Click(object sender, EventArgs e)
        {
            fController.CheckPlaces();
        }

        private void btnIntoList_Click(object sender, EventArgs e)
        {
            fController.CreateLocationRecord(ListPlaces.GetSelectedItems());
        }

        private void ListPlaces_DblClick(object sender, EventArgs e)
        {
            fController.CreateLocationRecord(ListPlaces.GetSelectedItems());
        }
    }
}
