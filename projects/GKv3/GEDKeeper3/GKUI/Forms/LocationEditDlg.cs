/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2022 by Sergey V. Zhdanovskih.
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
using System.ComponentModel;
using BSLib.Design.MVP.Controls;
using Eto.Forms;
using Eto.Serialization.Xaml;
using GDModel;
using GKCore;
using GKCore.Controllers;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.MVP.Controls;
using GKCore.MVP.Views;
using GKUI.Components;

namespace GKUI.Forms
{
    public sealed partial class LocationEditDlg : EditorDialog, ILocationEditDlg
    {
        #region Design components
#pragma warning disable CS0169

        private Button btnAccept;
        private Button btnCancel;
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

#pragma warning restore CS0169
        #endregion

        private readonly LocationEditDlgController fController;

        private readonly GKMapBrowser fMapBrowser;
        private readonly GKSheetList fMediaList;
        private readonly GKSheetList fNotesList;

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

            fController = new LocationEditDlgController(this);
            fController.Init(baseWin);

            fNotesList.ListModel = new NoteLinksListModel(baseWin, fController.LocalUndoman);
            fMediaList.ListModel = new MediaLinksListModel(baseWin, fController.LocalUndoman);
        }

        private void EditName_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.Key == Keys.Down && e.Control) {
                txtName.Text = txtName.Text.ToLower();
            }
        }

        private void btnAccept_Click(object sender, EventArgs e)
        {
            DialogResult = fController.Accept() ? DialogResult.Ok : DialogResult.None;
        }

        private void btnCancel_Click(object sender, EventArgs e)
        {
            DialogResult = fController.Cancel() ? DialogResult.Cancel : DialogResult.None;
        }

        protected override void OnClosing(CancelEventArgs e)
        {
            base.OnClosing(e);
            e.Cancel = fController.CheckChangesPersistence();
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

        private void ListGeoCoords_Click(object sender, EventArgs e)
        {
            fController.SelectGeoPoint();
        }

        private void EditName_TextChanged(object sender, EventArgs e)
        {
            Title = string.Format("{0} \"{1}\"", LangMan.LS(LSID.LSID_Location), txtName.Text);
        }

        private void btnShowOnMap_Click(object sender, EventArgs e)
        {
            fController.ShowOnMap();
        }
    }
}
