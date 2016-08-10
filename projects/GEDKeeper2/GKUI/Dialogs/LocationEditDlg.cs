/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
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
using System.Windows.Forms;

using GKCommon;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;
using GKCore.Maps;
using GKUI.Controls;
using GKUI.Sheets;

namespace GKUI.Dialogs
{
    /// <summary>
    /// 
    /// </summary>
    public partial class LocationEditDlg : Form, IBaseEditor
    {
        private readonly IBaseWindow fBase;
        private readonly GKMapBrowser fMapBrowser;
        private readonly GKMediaSheet fMediaList;
        private readonly GKNotesSheet fNotesList;
        private readonly ExtList<GMapPoint> fSearchPoints;

        private GEDCOMLocationRecord fLocationRecord;

        public GEDCOMLocationRecord LocationRecord
        {
            get { return this.fLocationRecord; }
            set { this.SetLocationRecord(value); }
        }

        public IBaseWindow Base
        {
            get { return this.fBase; }
        }

        public LocationEditDlg(IBaseWindow aBase)
        {
            this.InitializeComponent();

            this.btnAccept.Image = global::GKResources.iBtnAccept;
            this.btnCancel.Image = global::GKResources.iBtnCancel;

            this.fBase = aBase;
            this.fSearchPoints = new ExtList<GMapPoint>(true);

            this.fMapBrowser = new GKMapBrowser();
            this.fMapBrowser.InitMap();
            this.fMapBrowser.Dock = DockStyle.Fill;
            this.panMap.Controls.Add(this.fMapBrowser);

            this.fNotesList = new GKNotesSheet(this, this.pageNotes);
            this.fMediaList = new GKMediaSheet(this, this.pageMultimedia);

            // SetLang()
            this.Text = LangMan.LS(LSID.LSID_Location);
            this.btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
            this.btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
            this.pageCommon.Text = LangMan.LS(LSID.LSID_Common);
            this.pageNotes.Text = LangMan.LS(LSID.LSID_RPNotes);
            this.pageMultimedia.Text = LangMan.LS(LSID.LSID_RPMultimedia);
            this.lblName.Text = LangMan.LS(LSID.LSID_Title);
            this.lblLatitude.Text = LangMan.LS(LSID.LSID_Latitude);
            this.lblLongitude.Text = LangMan.LS(LSID.LSID_Longitude);
            this.ListGeoCoords.Columns[0].Text = LangMan.LS(LSID.LSID_Title);
            this.ListGeoCoords.Columns[1].Text = LangMan.LS(LSID.LSID_Latitude);
            this.ListGeoCoords.Columns[2].Text = LangMan.LS(LSID.LSID_Longitude);
            this.btnShowOnMap.Text = LangMan.LS(LSID.LSID_Show);
            this.grpSearch.Text = LangMan.LS(LSID.LSID_SearchCoords);
            this.btnSearch.Text = LangMan.LS(LSID.LSID_Search);
            this.btnSelect.Text = LangMan.LS(LSID.LSID_SelectCoords);
            this.btnSelectName.Text = LangMan.LS(LSID.LSID_SelectName);

            this.toolTip1.SetToolTip(this.btnShowOnMap, LangMan.LS(LSID.LSID_ShowOnMapTip));
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                this.fSearchPoints.Dispose();
                if (this.components != null) this.components.Dispose();
            }
            base.Dispose(disposing);
        }

        private void SetLocationRecord(GEDCOMLocationRecord value)
        {
            this.fLocationRecord = value;
            this.txtName.Text = this.fLocationRecord.LocationName;
            this.txtLatitude.Text = GKMapBrowser.CoordToStr(this.fLocationRecord.Map.Lati);
            this.txtLongitude.Text = GKMapBrowser.CoordToStr(this.fLocationRecord.Map.Long);

            this.fNotesList.DataList = this.fLocationRecord.Notes.GetEnumerator();
            this.fMediaList.DataList = this.fLocationRecord.MultimediaLinks.GetEnumerator();

            this.ActiveControl = this.txtName;
        }

        private void EditName_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.KeyCode == Keys.Down && e.Control)
            {
                this.txtName.Text = this.txtName.Text.ToLower();
            }
        }

        private void btnAccept_Click(object sender, EventArgs e)
        {
            try
            {
                this.fLocationRecord.LocationName = this.txtName.Text;
                this.fLocationRecord.Map.Lati = ConvHelper.ParseFloat(this.txtLatitude.Text, 0.0);
                this.fLocationRecord.Map.Long = ConvHelper.ParseFloat(this.txtLongitude.Text, 0.0);
                this.fBase.ChangeRecord(this.fLocationRecord);
                base.DialogResult = DialogResult.OK;
            }
            catch (Exception ex)
            {
                this.fBase.Host.LogWrite("LocationEditDlg.btnAccept_Click(): " + ex.Message);
                base.DialogResult = DialogResult.None;
            }
        }

        private void btnSearch_Click(object sender, EventArgs e)
        {
            this.fMapBrowser.ShowLines = false;

            this.ListGeoCoords.BeginUpdate();
            this.fMapBrowser.BeginUpdate();
            try
            {
                this.fSearchPoints.Clear();
                GKMapBrowser.RequestGeoCoords(this.txtName.Text, this.fSearchPoints);
                this.ListGeoCoords.Items.Clear();
                this.fMapBrowser.ClearPoints();

                int num = this.fSearchPoints.Count;
                for (int i = 0; i < num; i++)
                {
                    GMapPoint pt = this.fSearchPoints[i];

                    GKListItem item = new GKListItem(pt.Hint, pt);
                    item.AddSubItem(GKMapBrowser.CoordToStr(pt.Latitude));
                    item.AddSubItem(GKMapBrowser.CoordToStr(pt.Longitude));
                    this.ListGeoCoords.Items.Add(item);

                    this.fMapBrowser.AddPoint(pt.Latitude, pt.Longitude, pt.Hint);
                }

                this.fMapBrowser.ZoomToBounds();
            }
            finally
            {
                this.fMapBrowser.EndUpdate();
                this.ListGeoCoords.EndUpdate();
            }
        }

        private GKListItem GetSelectedGeoItem()
        {
            if (this.ListGeoCoords.SelectedItems.Count <= 0) return null;

            GKListItem item = (GKListItem)this.ListGeoCoords.SelectedItems[0];
            return item;
        }

        private void btnSelect_Click(object sender, EventArgs e)
        {
            GKListItem item = this.GetSelectedGeoItem();
            if (item == null) return;

            this.txtLatitude.Text = item.SubItems[1].Text;
            this.txtLongitude.Text = item.SubItems[2].Text;
        }

        private void btnSelectName_Click(object sender, EventArgs e)
        {
            GKListItem item = this.GetSelectedGeoItem();
            if (item == null) return;

            this.txtName.Text = item.Text;
        }

        private void ListGeoCoords_Click(object sender, EventArgs e)
        {
            GKListItem item = this.GetSelectedGeoItem();
            if (item == null) return;

            GMapPoint pt = item.Data as GMapPoint;
            if (pt == null) return;

            this.fMapBrowser.SetCenter(pt.Latitude, pt.Longitude, -1);
        }

        private void EditName_TextChanged(object sender, EventArgs e)
        {
            this.Text = string.Format("{0} \"{1}\"", LangMan.LS(LSID.LSID_Location), this.txtName.Text);
        }

        private void btnShowOnMap_Click(object sender, EventArgs e)
        {
            if (this.txtLatitude.Text != "" && this.txtLongitude.Text != "")
            {
                this.fMapBrowser.SetCenter(ConvHelper.ParseFloat(this.txtLatitude.Text, 0), ConvHelper.ParseFloat(this.txtLongitude.Text, 0), -1);
            }
        }
    }
}
