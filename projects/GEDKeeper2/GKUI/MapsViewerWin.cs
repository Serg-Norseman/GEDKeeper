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
using System.Collections.Generic;
using System.Windows.Forms;

using GKCommon;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;
using GKCore.Maps;
using GKUI.Controls;

namespace GKUI
{
    /// <summary>
    /// 
    /// </summary>
    public sealed partial class MapsViewerWin : Form, ILocalization
    {
        private class PlaceRef
        {
            public readonly DateTime Date;
            public readonly GEDCOMCustomEvent Event;

            public PlaceRef(GEDCOMCustomEvent evt)
            {
                this.Event = evt;
                this.Date = (evt == null) ? new DateTime(0) : evt.Detail.Date.GetDateTime();
            }
        }

        private class MapPlace : IDisposable
        {
            public string Name;
            public readonly ExtList<GMapPoint> Points;
            public readonly ExtList<PlaceRef> PlaceRefs;

            private bool fDisposed;

            public MapPlace()
            {
                this.Points = new ExtList<GMapPoint>(true);
                this.PlaceRefs = new ExtList<PlaceRef>(false);
            }

            public void Dispose()
            {
                if (!this.fDisposed)
                {
                    this.PlaceRefs.Dispose();
                    this.Points.Dispose();
                    this.fDisposed = true;
                }
            }
        }

        private readonly TreeNode fBaseRoot;
        private readonly GKMapBrowser fMapBrowser;
        private readonly ExtList<GMapPoint> fMapPoints;
        private readonly ExtList<MapPlace> fPlaces;
        private readonly List<GEDCOMRecord> fSelectedPersons;
        private readonly IBaseWindow fBase;
        private readonly GEDCOMTree fTree;

        private bool IsSelected(GEDCOMRecord iRec)
        {
            bool res = (this.fSelectedPersons == null || (this.fSelectedPersons != null && this.fSelectedPersons.IndexOf(iRec) >= 0));
            return res;
        }

        private void PlacesLoad()
        {
            try
            {
                this.fMapBrowser.InitMap();

                this.cmbPersons.BeginUpdate();
                this.tvPlaces.BeginUpdate();
                this.fBase.ProgressInit(LangMan.LS(LSID.LSID_LoadingLocations), this.fTree.RecordsCount);
                try
                {
                    this.fPlaces.Clear();
                    this.cmbPersons.Items.Clear();
                    this.cmbPersons.Sorted = false;
                    this.cmbPersons.Items.Add(new GKComboItem(LangMan.LS(LSID.LSID_NotSelected), null));

                    int num = this.fTree.RecordsCount;
                    for (int i = 0; i < num; i++) {
                        GEDCOMRecord rec = this.fTree[i];
                        bool res = rec is GEDCOMIndividualRecord && this.IsSelected(rec);

                        if (res) {
                            GEDCOMIndividualRecord ind = rec as GEDCOMIndividualRecord;
                            int pCnt = 0;

                            int num2 = ind.Events.Count;
                            for (int j = 0; j < num2; j++)
                            {
                                GEDCOMCustomEvent ev = ind.Events[j];
                                if (ev.Detail.Place.StringValue != "") {
                                    AddPlace(ev.Detail.Place, ev);
                                    pCnt++;
                                }
                            }

                            if (pCnt > 0) {
                                this.cmbPersons.Items.Add(new GKComboItem(ind.GetNameString(true, false) + " [" + pCnt.ToString() + "]", ind));
                            }
                        }

                        this.fBase.ProgressStep();
                    }

                    this.fBaseRoot.ExpandAll();
                    this.cmbPersons.Sorted = true;

                    this.btnSelectPlaces.Enabled = true;
                }
                finally
                {
                    this.fBase.ProgressDone();
                    this.tvPlaces.EndUpdate();
                    this.cmbPersons.EndUpdate();
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("MapsViewerWin.PlacesLoad(): " + ex.Message);
            }
        }

        private void PreparePointsList(ExtList<GMapPoint> aPoints, bool byPerson)
        {
            this.fMapBrowser.BeginUpdate();
            try
            {
                this.fMapBrowser.ClearPoints();
                int num = aPoints.Count;
                for (int i = 0; i < num; i++)
                {
                    GMapPoint pt = aPoints[i];
                    string stHint = pt.Hint;
                    if (byPerson)
                    {
                        stHint = stHint + " [" + pt.Date.ToString() + "]";
                    }

                    this.fMapBrowser.AddPoint(pt.Latitude, pt.Longitude, stHint);
                }
                this.fMapBrowser.ZoomToBounds();
            }
            finally
            {
                this.fMapBrowser.EndUpdate();
            }
        }

        private void radTotal_Click(object sender, EventArgs e)
        {
            this.chkBirth.Enabled = this.radTotal.Checked;
            this.chkDeath.Enabled = this.radTotal.Checked;
            this.chkResidence.Enabled = this.radTotal.Checked;
            this.cmbPersons.Enabled = this.radSelected.Checked;
            this.chkLinesVisible.Enabled = this.radSelected.Checked;
        }

        private void TfmMaps_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.KeyCode == Keys.Escape) base.Close();
        }

        // TODO: localize?
        private void btnSaveImage_Click(object sender, EventArgs e)
        {
            string filter1 = "Image files|*.jpg";

            string fileName = UIHelper.GetSaveFile("", "", filter1, 2, "jpg", "");
            if (!string.IsNullOrEmpty(fileName))
            {
                this.fMapBrowser.SaveSnapshot(fileName);
            }
        }

        private void btnSelectPlaces_Click(object sender, EventArgs e)
        {
            GEDCOMIndividualRecord ind = null;

            bool condBirth = false;
            bool condDeath = false;
            bool condResidence = false;

            if (this.radTotal.Checked) {
                condBirth = this.chkBirth.Checked;
                condDeath = this.chkDeath.Checked;
                condResidence = this.chkResidence.Checked;
            } else if (this.radSelected.Checked) {
                if (this.cmbPersons.SelectedIndex >= 0)
                {
                    GKComboItem item = (GKComboItem)this.cmbPersons.Items[this.cmbPersons.SelectedIndex];
                    ind = (item.Data as GEDCOMIndividualRecord);
                }
            }

            this.fMapBrowser.ShowLines = (ind != null && this.chkLinesVisible.Checked);
            this.fMapPoints.Clear();

            int num = this.fPlaces.Count;
            for (int i = 0; i < num; i++)
            {
                MapPlace place = this.fPlaces[i];

                if (place.Points.Count >= 1)
                {
                    int num2 = place.PlaceRefs.Count;
                    for (int j = 0; j < num2; j++)
                    {
                        GEDCOMCustomEvent evt = place.PlaceRefs[j].Event;

                        if ((ind != null && (evt.Parent == ind)) || (condBirth && evt.Name == "BIRT") || (condDeath && evt.Name == "DEAT") || (condResidence && evt.Name == "RESI"))
                        {
                            this.CopyPoint(place.Points[0], place.PlaceRefs[j]);
                        }
                    }
                }
            }

            if (ind != null)
            {
                // sort points by date
                this.fMapPoints.QuickSort(MapPointsCompare);
            }

            this.PreparePointsList(this.fMapPoints, ind != null);
        }

        private static int MapPointsCompare(GMapPoint item1, GMapPoint item2)
        {
            return item1.Date.CompareTo(item2.Date);
        }

        private void TreePlaces_DoubleClick(object sender, EventArgs e)
        {
            GKTreeNode node = this.tvPlaces.SelectedNode as GKTreeNode;
            if (node != null)
            {
                GMapPoint pt = node.Data as GMapPoint;
                if (pt != null)
                {
                    this.fMapBrowser.SetCenter(pt.Latitude, pt.Longitude, -1);
                }
            }
        }

        public void ProcessMap()
        {
            this.Show();
            this.PlacesLoad();
        }

        public MapsViewerWin(IBaseWindow aBase)
        {
            this.InitializeComponent();

            this.fBase = aBase;
            this.fTree = aBase.Tree;
            this.fSelectedPersons = aBase.GetContentList(GEDCOMRecordType.rtIndividual);
            this.fMapBrowser = new GKMapBrowser();
            this.fMapBrowser.Dock = DockStyle.Fill;
            this.Panel1.Controls.Add(this.fMapBrowser);
            this.fMapPoints = new ExtList<GMapPoint>(true);
            this.fPlaces = new ExtList<MapPlace>(true);
            this.fBaseRoot = this.tvPlaces.Nodes.Add(LangMan.LS(LSID.LSID_RPLocations));
            this.radTotal.Checked = true;

            this.SetLang();
        }

        public void SetLang()
        {
            this.Text = LangMan.LS(LSID.LSID_MIMap);
            this.pagePlaces.Text = LangMan.LS(LSID.LSID_RPLocations);
            this.grpSelection.Text = LangMan.LS(LSID.LSID_MapSelection);
            this.radTotal.Text = LangMan.LS(LSID.LSID_MapSelOnAll);
            this.chkBirth.Text = LangMan.LS(LSID.LSID_MSBirthPlaces);
            this.chkDeath.Text = LangMan.LS(LSID.LSID_MSDeathPlaces);
            this.chkResidence.Text = LangMan.LS(LSID.LSID_MSResiPlace);
            this.radSelected.Text = LangMan.LS(LSID.LSID_MapSelOnSelected);
            this.btnSaveImage.Text = LangMan.LS(LSID.LSID_SaveImage);
            this.btnSelectPlaces.Text = LangMan.LS(LSID.LSID_Show);
            this.chkLinesVisible.Text = LangMan.LS(LSID.LSID_LinesVisible);
        }

        private TreeNode FindTreeNode(string place)
        {
            int num = this.fBaseRoot.Nodes.Count;
            for (int i = 0; i < num; i++) {
                TreeNode node = this.fBaseRoot.Nodes[i];

                if (node.Text == place) {
                    return node;
                }
            }

            return null;
        }

        private void AddPlace(GEDCOMPlace place, GEDCOMCustomEvent placeEvent)
        {
            GEDCOMLocationRecord locRec = place.Location.Value as GEDCOMLocationRecord;

            string placeName = (locRec != null) ? locRec.LocationName : place.StringValue;

            TreeNode node = this.FindTreeNode(placeName);
            MapPlace mapPlace;

            if (node == null) {
                mapPlace = new MapPlace();
                mapPlace.Name = placeName;
                this.fPlaces.Add(mapPlace);

                node = new GKTreeNode(placeName, mapPlace);
                this.fBaseRoot.Nodes.Add(node);

                if (locRec == null) {
                    GKMapBrowser.RequestGeoCoords(placeName, mapPlace.Points);

                    int num = mapPlace.Points.Count;
                    for (int i = 0; i < num; i++) {
                        GMapPoint pt = mapPlace.Points[i];
                        string ptTitle = pt.Hint + string.Format(" [{0:0.000000}, {1:0.000000}]", pt.Latitude, pt.Longitude);
                        node.Nodes.Add(new GKTreeNode(ptTitle, pt));
                    }
                } else {
                    GMapPoint pt = new GMapPoint(locRec.Map.Lati, locRec.Map.Long, placeName);
                    mapPlace.Points.Add(pt);

                    string ptTitle = pt.Hint + string.Format(" [{0:0.000000}, {1:0.000000}]", pt.Latitude, pt.Longitude);
                    node.Nodes.Add(new GKTreeNode(ptTitle, pt));
                }
            } else {
                mapPlace = (((GKTreeNode) node).Data as MapPlace);
            }

            mapPlace.PlaceRefs.Add(new PlaceRef(placeEvent));
        }

        private void CopyPoint(GMapPoint aPt, PlaceRef aRef)
        {
            GMapPoint pt;
            int num = this.fMapPoints.Count;
            for (int i = 0; i < num; i++) {
                pt = this.fMapPoints[i];
                if (pt.Hint == aPt.Hint) {
                    return;
                }
            }

            pt = new GMapPoint(aPt.Latitude, aPt.Longitude, aPt.Hint);
            pt.Date = aRef.Date;
            this.fMapPoints.Add(pt);
        }
    }
}
