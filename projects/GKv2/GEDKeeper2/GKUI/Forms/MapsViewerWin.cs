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
using System.Windows.Forms;

using BSLib;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Controllers;
using GKCore.Interfaces;
using GKCore.Maps;
using GKCore.UIContracts;
using GKUI.Components;

namespace GKUI.Forms
{
    /// <summary>
    /// 
    /// </summary>
    public sealed partial class MapsViewerWin : CommonForm, IWindow, IMapsViewerWin
    {
        private readonly MapsViewerWinController fController;

        private readonly TreeNode fBaseRoot;
        private readonly GKMapBrowser fMapBrowser;
        private readonly ExtList<GeoPoint> fMapPoints;
        private readonly IBaseWindow fBase;

        #region View Interface

        IMapBrowser IMapsViewerWin.MapBrowser
        {
            get { return fMapBrowser; }
        }

        object IMapsViewerWin.TreeRoot
        {
            get { return fBaseRoot; }
        }

        IComboBoxHandler IMapsViewerWin.PersonsCombo
        {
            get { return fControlsManager.GetControlHandler<IComboBoxHandler>(cmbPersons); }
        }

        ITreeViewHandler IMapsViewerWin.PlacesTree
        {
            get { return fControlsManager.GetControlHandler<ITreeViewHandler>(tvPlaces); }
        }

        IButtonHandler IMapsViewerWin.SelectPlacesBtn
        {
            get { return fControlsManager.GetControlHandler<IButtonHandler>(btnSelectPlaces); }
        }

        #endregion

        private void radTotal_Click(object sender, EventArgs e)
        {
            chkBirth.Enabled = radTotal.Checked;
            chkDeath.Enabled = radTotal.Checked;
            chkResidence.Enabled = radTotal.Checked;
            cmbPersons.Enabled = radSelected.Checked;

            if (radTotal.Checked) {
                chkLinesVisible.Checked = false;
            }
            chkLinesVisible.Enabled = radSelected.Checked;
        }

        private void MapsViewerWin_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.KeyCode == Keys.Escape) Close();
        }

        private void btnSaveImage_Click(object sender, EventArgs e)
        {
            fController.SaveImage();
        }

        private void btnSelectPlaces_Click(object sender, EventArgs e)
        {
            GEDCOMIndividualRecord ind = null;

            bool condBirth = false;
            bool condDeath = false;
            bool condResidence = false;

            if (radTotal.Checked) {
                condBirth = chkBirth.Checked;
                condDeath = chkDeath.Checked;
                condResidence = chkResidence.Checked;
            } else if (radSelected.Checked && (cmbPersons.SelectedIndex >= 0)) {
                GKComboItem item = (GKComboItem)cmbPersons.Items[cmbPersons.SelectedIndex];
                ind = (item.Tag as GEDCOMIndividualRecord);
            }

            fMapBrowser.ShowLines = (ind != null && chkLinesVisible.Checked);
            fMapPoints.Clear();

            int num = fController.Places.Count;
            for (int i = 0; i < num; i++) {
                MapPlace place = fController.Places[i];
                if (place.Points.Count < 1) continue;

                int num2 = place.PlaceRefs.Count;
                for (int j = 0; j < num2; j++) {
                    GEDCOMCustomEvent evt = place.PlaceRefs[j].Event;

                    if ((ind != null && (evt.Parent == ind)) || (condBirth && evt.Name == "BIRT") || (condDeath && evt.Name == "DEAT") || (condResidence && evt.Name == "RESI")) {
                        PlacesLoader.AddPoint(fMapPoints, place.Points[0], place.PlaceRefs[j]);
                    }
                }
            }

            if (ind != null) {
                // sort points by date
                fMapPoints.QuickSort(MapPointsCompare);
            }

            PlacesLoader.CopyPoints(fMapBrowser, fMapPoints, ind != null);
        }

        private static int MapPointsCompare(GeoPoint item1, GeoPoint item2)
        {
            return item1.Date.CompareTo(item2.Date);
        }

        private void TreePlaces_DoubleClick(object sender, EventArgs e)
        {
            GKTreeNode node = tvPlaces.SelectedNode as GKTreeNode;
            if (node == null) return;

            GeoPoint pt = node.Tag as GeoPoint;
            if (pt == null) return;

            fMapBrowser.ClearPoints();
            fMapBrowser.SetCenter(pt.Latitude, pt.Longitude, -1);
        }

        public void ProcessMap()
        {
            AppHost.Instance.ShowWindow(this);
            fController.LoadPlaces();
            Activate();
        }

        public MapsViewerWin(IBaseWindow baseWin)
        {
            InitializeComponent();

            fMapBrowser = new GKMapBrowser();
            fMapBrowser.Dock = DockStyle.Fill;
            Panel1.Controls.Add(fMapBrowser);

            fBase = baseWin;
            fController = new MapsViewerWinController(this, baseWin.GetContentList(GEDCOMRecordType.rtIndividual));
            fController.Init(baseWin);

            fMapPoints = new ExtList<GeoPoint>(true);
            fBaseRoot = tvPlaces.Nodes.Add(LangMan.LS(LSID.LSID_RPLocations));

            radTotal.Checked = true;
            radTotal_Click(null, null);

            SetLang();
        }

        public void SetLang()
        {
            Text = LangMan.LS(LSID.LSID_MIMap);
            pagePlaces.Text = LangMan.LS(LSID.LSID_RPLocations);
            grpSelection.Text = LangMan.LS(LSID.LSID_MapSelection);
            radTotal.Text = LangMan.LS(LSID.LSID_MapSelOnAll);
            chkBirth.Text = LangMan.LS(LSID.LSID_MSBirthPlaces);
            chkDeath.Text = LangMan.LS(LSID.LSID_MSDeathPlaces);
            chkResidence.Text = LangMan.LS(LSID.LSID_MSResiPlace);
            radSelected.Text = LangMan.LS(LSID.LSID_MapSelOnSelected);
            btnSaveImage.Text = LangMan.LS(LSID.LSID_SaveImage);
            btnSelectPlaces.Text = LangMan.LS(LSID.LSID_Show);
            chkLinesVisible.Text = LangMan.LS(LSID.LSID_LinesVisible);
        }

        private TreeNode FindTreeNode(string place)
        {
            int num = fBaseRoot.Nodes.Count;
            for (int i = 0; i < num; i++) {
                TreeNode node = fBaseRoot.Nodes[i];

                if (node.Text == place) {
                    return node;
                }
            }

            return null;
        }

        public void AddPlace(GEDCOMPlace place, GEDCOMCustomEvent placeEvent)
        {
            try {
                GEDCOMLocationRecord locRec = place.Location.Value as GEDCOMLocationRecord;
                string placeName = (locRec != null) ? locRec.LocationName : place.StringValue;

                TreeNode node = FindTreeNode(placeName);
                MapPlace mapPlace;

                if (node == null) {
                    mapPlace = new MapPlace();
                    mapPlace.Name = placeName;
                    fController.Places.Add(mapPlace);

                    node = new GKTreeNode(placeName, mapPlace);
                    fBaseRoot.Nodes.Add(node);

                    if (locRec == null) {
                        PlacesCache.Instance.GetPlacePoints(placeName, mapPlace.Points);
                    } else {
                        GeoPoint pt = new GeoPoint(locRec.Map.Lati, locRec.Map.Long, placeName);
                        mapPlace.Points.Add(pt);
                    }

                    int num = mapPlace.Points.Count;
                    for (int i = 0; i < num; i++) {
                        GeoPoint pt = mapPlace.Points[i];
                        string ptTitle = pt.Hint + string.Format(" [{0:0.000000}, {1:0.000000}]", pt.Latitude, pt.Longitude);
                        node.Nodes.Add(new GKTreeNode(ptTitle, pt));
                    }
                } else {
                    mapPlace = (((GKTreeNode)node).Tag as MapPlace);
                }

                mapPlace.PlaceRefs.Add(new PlaceRef(placeEvent));
            } catch (Exception ex) {
                Logger.LogWrite("MapsViewerWin.AddPlace(): " + ex.Message);
            }
        }
    }
}
