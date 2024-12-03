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
using GDModel;
using GKCore;
using GKCore.Controllers;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Interfaces;
using GKCore.Lists;
using GKUI.Components;

namespace GKUI.Forms
{
    public sealed partial class LocationEditDlg : CommonDialog<ILocationEditDlg, LocationEditDlgController>, ILocationEditDlg
    {
        #region Design components
#pragma warning disable CS0169, CS0649, IDE0044, IDE0051

        private Button btnAccept;
        private Button btnCancel;
        private TabControl tabsData;
        private TabPage pageNotes;
        private TabPage pageMultimedia;
        private TabPage pageCommon;
        private Label lblName;
        private TextBox txtName;
        private Label lblLatitude;
        private TextBox txtLatitude;
        private Label lblLongitude;
        private TextBox txtLongitude;
        private GroupBox grpSearch;
        private GKListView ListGeoCoords;
        private Button btnSearch;
        private Button btnSelect;
        private Button btnSelectName;
        private Button btnShowOnMap;
        private GKMapBrowser fMapBrowser;
        private GKSheetList fMediaList;
        private GKSheetList fNamesList;
        private GKSheetList fLinksList;
        private GKSheetList fNotesList;
        private GroupBox pageHistNames;
        private GroupBox pageHistLinks;

#pragma warning restore CS0169, CS0649, IDE0044, IDE0051
        #endregion

        public GDMLocationRecord LocationRecord
        {
            get { return fController.LocationRecord; }
            set { fController.LocationRecord = value; }
        }

        #region View Interface

        IMapBrowser ILocationEditDlg.MapBrowser
        {
            get { return fMapBrowser; }
        }

        ISheetList ILocationEditDlg.NamesList
        {
            get { return fNamesList; }
        }

        ISheetList ILocationEditDlg.LinksList
        {
            get { return fLinksList; }
        }

        ISheetList ILocationEditDlg.MediaList
        {
            get { return fMediaList; }
        }

        ISheetList ILocationEditDlg.NotesList
        {
            get { return fNotesList; }
        }

        IListView ILocationEditDlg.GeoCoordsList
        {
            get { return ListGeoCoords; }
        }

        ITextBox ILocationEditDlg.Name
        {
            get { return GetControlHandler<ITextBox>(txtName); }
        }

        ITextBox ILocationEditDlg.Latitude
        {
            get { return GetControlHandler<ITextBox>(txtLatitude); }
        }

        ITextBox ILocationEditDlg.Longitude
        {
            get { return GetControlHandler<ITextBox>(txtLongitude); }
        }

        #endregion

        public LocationEditDlg(IBaseWindow baseWin)
        {
            XamlReader.Load(this);

            tabsData.SelectedIndexChanged += tabsData_SelectedIndexChanged;

            fController = new LocationEditDlgController(this);
            fController.Init(baseWin);
        }

        private void tabsData_SelectedIndexChanged(object sender, EventArgs e)
        {
            var tabCtl = (TabControl)sender;
            var selectedTab = tabCtl.SelectedIndex;
            if (selectedTab == 1) {
                fController.CheckPrimaryName();
            }
        }

        private void EditName_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.Key == Keys.Down && e.Control) {
                txtName.Text = txtName.Text.ToLower();
            }
        }

        private void btnSearch_Click(object sender, EventArgs e)
        {
            fController.Search();
        }

        private void btnSelect_Click(object sender, EventArgs e)
        {
            fController.SelectCoords();
        }

        private void btnSelectName_Click(object sender, EventArgs e)
        {
            fController.SelectName();
        }

        private void btnSelectCursor_Click(object sender, EventArgs e)
        {
            fController.SelectCursorCoords();
        }

        private void ListGeoCoords_Click(object sender, EventArgs e)
        {
            fController.SelectGeoPoint();
        }

        private void EditName_TextChanged(object sender, EventArgs e)
        {
            Title = string.Format("{0} \"{1}\"", LangMan.LS(LSID.Location), txtName.Text);
        }

        private void btnShowOnMap_Click(object sender, EventArgs e)
        {
            fController.ShowOnMap();
        }
    }
}
