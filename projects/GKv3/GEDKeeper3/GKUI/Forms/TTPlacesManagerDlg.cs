/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2024 by Sergey V. Zhdanovskih.
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
using Eto.Forms;
using Eto.Serialization.Xaml;
using GKCore;
using GKCore.Controllers;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Interfaces;
using GKUI.Components;

namespace GKUI.Forms
{
    public sealed partial class TTPlacesManagerDlg : CommonDialog<IPlacesManagerDlg, PlacesManagerController>, IPlacesManagerDlg
    {
        #region Design components
#pragma warning disable CS0169, CS0649, IDE0044, IDE0051

        private Button btnClose;
        private TabPage pagePlaceManage;
        private Panel panPlacesContainer;
        private Button btnAnalysePlaces;
        private Button btnLocExpert;
        private Button btnIntoList;
        private GKListView ListPlaces;
        private Label lblFilter;
        private TextBox txtFilter;
        private ContextMenu contextMenu;

#pragma warning restore CS0169, CS0649, IDE0044, IDE0051
        #endregion

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
            XamlReader.Load(this);

            var miDetails = new ButtonMenuItem();
            miDetails.Text = LangMan.LS(LSID.Details);
            miDetails.Click += miDetails_Click;

            contextMenu = new ContextMenu();
            contextMenu.Items.AddRange(new MenuItem[] { miDetails });

            ListPlaces.ContextMenu = contextMenu;

            fController = new PlacesManagerController(this);
            fController.Init(baseWin);
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                fController.Clear();
            }
            base.Dispose(disposing);
        }

        private void Form_Load(object sender, EventArgs e)
        {
            fController.SyncAll();
        }

        private void miDetails_Click(object sender, EventArgs e)
        {
            fController.ShowDetails();
        }

        private void btnAnalysePlaces_Click(object sender, EventArgs e)
        {
            fController.CheckPlaces();
        }

        private void btnLocExpert_Click(object sender, EventArgs e)
        {
            fController.ShowLocExpert();
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
