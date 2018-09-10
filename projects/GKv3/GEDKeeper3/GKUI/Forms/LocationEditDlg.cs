/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2018 by Sergey V. Zhdanovskih.
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

using BSLib;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Controllers;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.UIContracts;
using GKUI.Components;

namespace GKUI.Forms
{
    /// <summary>
    /// 
    /// </summary>
    public sealed partial class LocationEditDlg : EditorDialog, ILocationEditDlg
    {
        private readonly LocationEditDlgController fController;

        private readonly GKMapBrowser fMapBrowser;
        private readonly GKSheetList fMediaList;
        private readonly GKSheetList fNotesList;

        public GEDCOMLocationRecord LocationRecord
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

        ITextBoxHandler ILocationEditDlg.Name
        {
            get { return fControlsManager.GetControlHandler<ITextBoxHandler>(txtName); }
        }

        ITextBoxHandler ILocationEditDlg.Latitude
        {
            get { return fControlsManager.GetControlHandler<ITextBoxHandler>(txtLatitude); }
        }

        ITextBoxHandler ILocationEditDlg.Longitude
        {
            get { return fControlsManager.GetControlHandler<ITextBoxHandler>(txtLongitude); }
        }

        #endregion

        public LocationEditDlg()
        {
            InitializeComponent();

            btnAccept.Image = UIHelper.LoadResourceImage("Resources.btn_accept.gif");
            btnCancel.Image = UIHelper.LoadResourceImage("Resources.btn_cancel.gif");

            fMapBrowser = new GKMapBrowser();
            fMapBrowser.InitMap();
            fMapBrowser.ShowLines = false;
            panMap.Content = fMapBrowser;

            fNotesList = new GKSheetList(pageNotes);
            fMediaList = new GKSheetList(pageMultimedia);

            // SetLang()
            Title = LangMan.LS(LSID.LSID_Location);
            btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
            btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
            pageCommon.Text = LangMan.LS(LSID.LSID_Common);
            pageNotes.Text = LangMan.LS(LSID.LSID_RPNotes);
            pageMultimedia.Text = LangMan.LS(LSID.LSID_RPMultimedia);
            lblName.Text = LangMan.LS(LSID.LSID_Title);
            lblLatitude.Text = LangMan.LS(LSID.LSID_Latitude);
            lblLongitude.Text = LangMan.LS(LSID.LSID_Longitude);
            ListGeoCoords.SetColumnCaption(0, LangMan.LS(LSID.LSID_Title));
            ListGeoCoords.SetColumnCaption(1, LangMan.LS(LSID.LSID_Latitude));
            ListGeoCoords.SetColumnCaption(2, LangMan.LS(LSID.LSID_Longitude));
            btnShowOnMap.Text = LangMan.LS(LSID.LSID_Show);
            grpSearch.Text = LangMan.LS(LSID.LSID_SearchCoords);
            btnSearch.Text = LangMan.LS(LSID.LSID_Search);
            btnSelect.Text = LangMan.LS(LSID.LSID_SelectCoords);
            btnSelectName.Text = LangMan.LS(LSID.LSID_SelectName);

            btnShowOnMap.ToolTip = LangMan.LS(LSID.LSID_ShowOnMapTip);

            fController = new LocationEditDlgController(this);
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
            }
            base.Dispose(disposing);
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
            try {
                fController.Cancel();
                CancelClickHandler(sender, e);
            } catch (Exception ex) {
                Logger.LogWrite("LocationEditDlg.btnCancel_Click(): " + ex.Message);
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
            if (txtLatitude.Text != "" && txtLongitude.Text != "") {
                fMapBrowser.SetCenter(ConvertHelper.ParseFloat(txtLatitude.Text, 0), ConvertHelper.ParseFloat(txtLongitude.Text, 0), -1);
            }
        }

        public override void InitDialog(IBaseWindow baseWin)
        {
            base.InitDialog(baseWin);
            fController.Init(baseWin);

            fNotesList.ListModel = new NoteLinksListModel(fBase, fController.LocalUndoman);
            fMediaList.ListModel = new MediaLinksListModel(fBase, fController.LocalUndoman);
        }
    }
}
