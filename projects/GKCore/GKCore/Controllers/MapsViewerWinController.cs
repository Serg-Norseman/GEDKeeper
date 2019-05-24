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
using System.Collections.Generic;
using BSLib;
using GDModel;
using GDModel.Providers.GEDCOM;
using GKCore.Interfaces;
using GKCore.Maps;
using GKCore.MVP;
using GKCore.MVP.Controls;
using GKCore.MVP.Views;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class MapsViewerWinController : FormController<IMapsViewerWin>
    {
        private readonly ExtList<GeoPoint> fMapPoints;
        private readonly List<GDMRecord> fSelectedPersons;
        private readonly ExtList<MapPlace> fPlaces;

        private ITVNode fBaseRoot;

        public ExtList<MapPlace> Places
        {
            get { return fPlaces; }
        }

        public ITVNode TreeRoot
        {
            get { return fBaseRoot; }
        }

        public MapsViewerWinController(IMapsViewerWin view, List<GDMRecord> selectedPersons) : base(view)
        {
            fMapPoints = new ExtList<GeoPoint>(true);
            fPlaces = new ExtList<MapPlace>(true);
            fSelectedPersons = selectedPersons;
        }

        public override void UpdateView()
        {
        }

        private bool IsSelected(GDMRecord iRec)
        {
            bool res = (fSelectedPersons == null || (fSelectedPersons.IndexOf(iRec) >= 0));
            return res;
        }

        public void LoadPlaces()
        {
            try {
                PlacesCache.Instance.Load();

                IProgressController progress = AppHost.Progress;
                GDMTree tree = fBase.Context.Tree;

                fView.MapBrowser.InitMap();

                fView.PersonsCombo.BeginUpdate();
                fView.PlacesTree.BeginUpdate();
                progress.ProgressInit(LangMan.LS(LSID.LSID_LoadingLocations), tree.RecordsCount);
                try {
                    fBaseRoot = fView.PlacesTree.AddNode(null, LangMan.LS(LSID.LSID_RPLocations), null);

                    fPlaces.Clear();
                    var personValues = new StringList();

                    int num = tree.RecordsCount;
                    for (int i = 0; i < num; i++) {
                        GDMRecord rec = tree[i];
                        bool res = rec is GDMIndividualRecord && IsSelected(rec);

                        if (res) {
                            GDMIndividualRecord ind = rec as GDMIndividualRecord;
                            int pCnt = 0;

                            int num2 = ind.Events.Count;
                            for (int j = 0; j < num2; j++) {
                                GDMCustomEvent ev = ind.Events[j];
                                if (ev.Place.StringValue != "") {
                                    AddPlace(ev.Place, ev);
                                    pCnt++;
                                }
                            }

                            if (pCnt > 0) {
                                personValues.AddObject(GKUtils.GetNameString(ind, true, false) + " [" + pCnt.ToString() + "]", ind);
                            }
                        }

                        progress.ProgressStep();
                    }

                    fView.PlacesTree.Expand(fBaseRoot);

                    personValues.Sort();
                    fView.PersonsCombo.Clear();
                    fView.PersonsCombo.AddItem(LangMan.LS(LSID.LSID_NotSelected), null);
                    fView.PersonsCombo.AddStrings(personValues);

                    fView.SelectPlacesBtn.Enabled = true;
                } finally {
                    progress.ProgressDone();
                    fView.PlacesTree.EndUpdate();
                    fView.PersonsCombo.EndUpdate();

                    PlacesCache.Instance.Save();
                }
            } catch (Exception ex) {
                Logger.LogWrite("MapsViewerWin.PlacesLoad(): " + ex.Message);
            }
        }

        private void AddPlace(GDMPlace place, GDMCustomEvent placeEvent)
        {
            try {
                GDMLocationRecord locRec = place.Location.Value as GDMLocationRecord;
                string placeName = (locRec != null) ? locRec.LocationName : place.StringValue;

                ITVNode node = fView.FindTreeNode(placeName);
                MapPlace mapPlace;

                if (node == null) {
                    mapPlace = new MapPlace();
                    mapPlace.Name = placeName;
                    fPlaces.Add(mapPlace);

                    node = fView.PlacesTree.AddNode(fBaseRoot, placeName, mapPlace);

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
                        fView.PlacesTree.AddNode(node, ptTitle, pt);
                    }
                } else {
                    mapPlace = (node.Tag as MapPlace);
                }

                mapPlace.PlaceRefs.Add(new PlaceRef(placeEvent));
            } catch (Exception ex) {
                Logger.LogWrite("MapsViewerWin.AddPlace(): " + ex.Message);
            }
        }

        public void SelectPlaces()
        {
            GDMIndividualRecord ind = null;

            bool condBirth = false;
            bool condDeath = false;
            bool condResidence = false;

            if (fView.TotalRadio.Checked) {
                condBirth = fView.BirthCheck.Checked;
                condDeath = fView.DeathCheck.Checked;
                condResidence = fView.ResidenceCheck.Checked;
            } else if (fView.SelectedRadio.Checked && (fView.PersonsCombo.SelectedIndex >= 0)) {
                ind = (fView.PersonsCombo.SelectedTag as GDMIndividualRecord);
            }

            fView.MapBrowser.ShowLines = (ind != null && fView.LinesVisibleCheck.Checked);
            fMapPoints.Clear();

            int num = fPlaces.Count;
            for (int i = 0; i < num; i++) {
                MapPlace place = fPlaces[i];
                if (place.Points.Count < 1) continue;

                int num2 = place.PlaceRefs.Count;
                for (int j = 0; j < num2; j++) {
                    GDMCustomEvent evt = place.PlaceRefs[j].Event;

                    if ((ind != null && (evt.Owner == ind)) || (condBirth && evt.Name == GEDCOMTagType.BIRT) || (condDeath && evt.Name == GEDCOMTagType.DEAT) || (condResidence && evt.Name == GEDCOMTagType.RESI)) {
                        PlacesLoader.AddPoint(fMapPoints, place.Points[0], place.PlaceRefs[j]);
                    }
                }
            }

            if (ind != null) {
                // sort points by date
                fMapPoints.QuickSort(MapPointsCompare);
            }

            PlacesLoader.CopyPoints(fView.MapBrowser, fMapPoints, ind != null);
        }

        private static int MapPointsCompare(GeoPoint item1, GeoPoint item2)
        {
            return item1.Date.CompareTo(item2.Date);
        }

        // TODO: localize?
        public void SaveImage()
        {
            string filter1 = "Image files|*.jpg";

            string fileName = AppHost.StdDialogs.GetSaveFile("", "", filter1, 2, "jpg", "");
            if (!string.IsNullOrEmpty(fileName)) {
                fView.MapBrowser.SaveSnapshot(fileName);
            }
        }
    }
}
