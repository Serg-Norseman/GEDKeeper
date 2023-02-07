/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih.
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
using GKCore.Design.Controls;
using GKCore.Interfaces;
using GKCore.Maps;
using GKCore.Design;
using GKCore.Design.Views;

namespace GKCore.Controllers
{
    /// <summary>
    ///
    /// </summary>
    public sealed class MapsViewerWinController : FormController<IMapsViewerWin>
    {
        private readonly List<GeoPoint> fMapPoints;
        private readonly List<GDMRecord> fSelectedPersons;
        private readonly Dictionary<string, MapPlace> fPlaces;

        private ITVNode fBaseRoot;

        public ITVNode TreeRoot
        {
            get { return fBaseRoot; }
        }

        public MapsViewerWinController(IMapsViewerWin view, List<GDMRecord> selectedPersons) : base(view)
        {
            fMapPoints = new List<GeoPoint>();
            fPlaces = new Dictionary<string, MapPlace>();
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

        private void LoadPlacesInt(StringList personValues, IProgressController progress)
        {
            fPlaces.Clear();

            GDMTree tree = fBase.Context.Tree;
            progress.Begin(LangMan.LS(LSID.LSID_LoadingLocations), tree.RecordsCount);
            try {
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
                            if (ev.HasPlace && !string.IsNullOrEmpty(ev.Place.StringValue)) {
                                AddPlace(ev.Place, ind, ev);
                                pCnt++;
                            }
                        }

                        if (pCnt > 0) {
                            personValues.AddObject(GKUtils.GetNameString(ind, true, false) + " [" + pCnt.ToString() + "]", ind);
                        }
                    }

                    progress.Increment();
                }
            } finally {
                progress.End();
            }
        }

        public void LoadPlaces()
        {
            try {
                PlacesCache.Instance.Load();
                fView.PersonsCombo.BeginUpdate();
                fView.PlacesTree.BeginUpdate();
                fView.PlacesTree.Clear();
                StringList personValues = new StringList();
                fBaseRoot = fView.PlacesTree.AddNode(null, LangMan.LS(LSID.LSID_RPLocations), null);

                AppHost.Instance.ExecuteWork((controller) => {
                    LoadPlacesInt(personValues, controller);
                });

                foreach (var kvp in fPlaces) {
                    string placeName = kvp.Key;
                    MapPlace mapPlace = kvp.Value;

                    ITVNode node = fView.FindTreeNode(placeName);
                    if (node == null) {
                        node = fView.PlacesTree.AddNode(fBaseRoot, placeName, mapPlace);

                        int num = mapPlace.Points.Count;
                        for (int i = 0; i < num; i++) {
                            GeoPoint pt = mapPlace.Points[i];
                            string ptTitle = pt.Hint + string.Format(" [{0:0.000000}, {1:0.000000}]", pt.Latitude, pt.Longitude);
                            fView.PlacesTree.AddNode(node, ptTitle, pt);
                        }
                    }
                }

                fView.PlacesTree.Expand(fBaseRoot);

                personValues.Sort();
                fView.PersonsCombo.Clear();
                fView.PersonsCombo.AddItem<GDMIndividualRecord>(LangMan.LS(LSID.LSID_NotSelected), null);
                for (int i = 0; i < personValues.Count; i++) {
                    fView.PersonsCombo.AddItem(personValues[i], personValues.GetObject(i) as GDMIndividualRecord);
                }

                fView.SelectPlacesBtn.Enabled = true;

                fView.PlacesTree.EndUpdate();
                fView.PersonsCombo.EndUpdate();
                PlacesCache.Instance.Save();
            } catch (Exception ex) {
                Logger.WriteError("MapsViewerWin.PlacesLoad()", ex);
            }
        }

        private void AddPlace(GDMPlace place, GDMRecord owner, GDMCustomEvent placeEvent)
        {
            try {
                var locRec = fBase.Context.Tree.GetPtrValue<GDMLocationRecord>(place.Location);
                string placeName = (locRec != null) ? locRec.LocationName : place.StringValue;

                MapPlace mapPlace;
                if (!fPlaces.TryGetValue(placeName, out mapPlace)) {
                    mapPlace = new MapPlace();
                    mapPlace.Name = placeName;
                    fPlaces.Add(placeName, mapPlace);

                    if (locRec == null) {
                        PlacesCache.Instance.GetPlacePoints(placeName, mapPlace.Points);
                    } else {
                        mapPlace.Points.Add(new GeoPoint(locRec.Map.Lati, locRec.Map.Long, placeName));
                    }
                }

                mapPlace.PlaceRefs.Add(new PlaceRef(owner, placeEvent));
            } catch (Exception ex) {
                Logger.WriteError("MapsViewerWin.AddPlace()", ex);
            }
        }

        public void SetCenter()
        {
            GeoPoint pt = fView.PlacesTree.GetSelectedData() as GeoPoint;
            if (pt == null) return;

            fView.MapBrowser.SetCenter(pt.Latitude, pt.Longitude, -1);
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
                ind = (fView.PersonsCombo.GetSelectedTag<GDMIndividualRecord>());
            }

            fView.MapBrowser.ShowLines = (ind != null && fView.LinesVisibleCheck.Checked);
            fMapPoints.Clear();

            foreach (var kvp in fPlaces) {
                MapPlace place = kvp.Value;
                if (place.Points.Count < 1) continue;

                int num2 = place.PlaceRefs.Count;
                for (int j = 0; j < num2; j++) {
                    var placeRef = place.PlaceRefs[j];
                    GDMCustomEvent evt = placeRef.Event;
                    var evtType = evt.GetTagType();
                    bool checkEventType = (condBirth && evtType == GEDCOMTagType.BIRT) ||
                        (condDeath && evtType == GEDCOMTagType.DEAT) || (condResidence && evtType == GEDCOMTagType.RESI);

                    if ((ind != null && (placeRef.Owner == ind)) || checkEventType) {
                        PlacesLoader.AddPoint(fMapPoints, place.Points[0], placeRef);
                    }
                }
            }

            if (ind != null) {
                // sort points by date
                SortHelper.QuickSort(fMapPoints, MapPointsCompare);
            }

            PlacesLoader.CopyPoints(fView.MapBrowser, fMapPoints, ind != null);
        }

        private static int MapPointsCompare(GeoPoint item1, GeoPoint item2)
        {
            return item1.Date.CompareTo(item2.Date);
        }

        // TODO: localize?
        public void SaveSnapshot()
        {
            try {
                string filter1 = "Image files|*.jpg";

                string fileName = AppHost.StdDialogs.GetSaveFile("", "", filter1, 2, "jpg", "");
                if (!string.IsNullOrEmpty(fileName)) {
                    fView.MapBrowser.SaveSnapshot(fileName);
                }
            } catch (Exception ex) {
                Logger.WriteError("SaveSnapshot()", ex);
                AppHost.StdDialogs.ShowError("Image failed to save: " + ex.Message);
            }
        }

        public override void SetLocale()
        {
            fView.Title = LangMan.LS(LSID.LSID_MIMap);

            GetControl<ITabPage>("pagePlaces").Text = LangMan.LS(LSID.LSID_RPLocations);
            GetControl<IGroupBox>("grpSelection").Text = LangMan.LS(LSID.LSID_MapSelection);
            GetControl<IRadioButton>("radTotal").Text = LangMan.LS(LSID.LSID_MapSelOnAll);
            GetControl<ICheckBox>("chkBirth").Text = LangMan.LS(LSID.LSID_MSBirthPlaces);
            GetControl<ICheckBox>("chkDeath").Text = LangMan.LS(LSID.LSID_MSDeathPlaces);
            GetControl<ICheckBox>("chkResidence").Text = LangMan.LS(LSID.LSID_MSResiPlace);
            GetControl<IRadioButton>("radSelected").Text = LangMan.LS(LSID.LSID_MapSelOnSelected);
            //btnSaveImage.Text = LangMan.LS(LSID.LSID_SaveImage);
            GetControl<IButton>("btnSelectPlaces").Text = LangMan.LS(LSID.LSID_Show);
            GetControl<ICheckBox>("chkLinesVisible").Text = LangMan.LS(LSID.LSID_LinesVisible);
            GetControl<IButtonToolItem>("tbLoadPlaces").Text = LangMan.LS(LSID.LSID_LoadPlaces);
            GetControl<IButtonToolItem>("tbProviders").Text = LangMan.LS(LSID.LSID_Providers);
            GetControl<ITabPage>("pageCoordinates").Text = LangMan.LS(LSID.LSID_Coordinates);
            GetControl<IButtonToolItem>("tbClear").Text = LangMan.LS(LSID.LSID_Clear);
            GetControl<IButton>("btnSearch").Text = LangMan.LS(LSID.LSID_Search);

            SetToolTip("tbSaveSnapshot", LangMan.LS(LSID.LSID_ImageSaveTip));
        }
    }
}
