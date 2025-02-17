/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
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
using System.Globalization;
using BSLib;
using GDModel;
using GDModel.Providers.GEDCOM;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Interfaces;
using GKCore.Maps;
using GKCore.Options;
using GKCore.Types;
using GKMap;
using GKMap.MapProviders;
using GKUI.Themes;

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
        private readonly bool fSearchPlacesWithoutCoords;

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
            fSearchPlacesWithoutCoords = GlobalOptions.Instance.SearchPlacesWithoutCoords;
        }

        public override void Init(IBaseWindow baseWin)
        {
            base.Init(baseWin);

            CultureInfo culture = CultureInfo.CurrentCulture;
            string basePoint = AppHost.Instance.LocalesCollection.GetMapBasePoint(culture.Name);

            // add start location
            PointLatLng? pos = null;
            GeocoderStatusCode status = GeocoderStatusCode.Unknown;

            if (!string.IsNullOrEmpty(basePoint)) {
                //GMapProvider.LanguageStr = culture.TwoLetterISOLanguageName;
                //pos = GMapProviders.GoogleMap.GetPoint(basePoint, out status);

                var pointsList = new List<GeoPoint>();
                PlacesCache.Instance.GetPlacePoints(basePoint, pointsList);
                if (pointsList.Count > 0) {
                    var pt = pointsList[0];
                    pos = new PointLatLng(pt.Latitude, pt.Longitude);
                    status = GeocoderStatusCode.Success;
                }
            }

            if (pos == null || status != GeocoderStatusCode.Success) {
                pos = new PointLatLng(-15.950278, -5.683056);
            }

            fView.MapBrowser.TargetPosition = pos.Value;
            fView.MapBrowser.MapControl.ZoomAndCenterMarkers(null);
        }

        public void ShowFixedPoints(IEnumerable<GeoPoint> points)
        {
            var mapBrowser = fView.MapBrowser;
            mapBrowser.ShowLines = false;
            mapBrowser.BeginUpdate();
            try {
                mapBrowser.ClearPoints();
                foreach (var pt in points) {
                    mapBrowser.AddPoint(pt);
                }
            } finally {
                mapBrowser.EndUpdate();
            }
            mapBrowser.ZoomToBounds();
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
            progress.Begin(LangMan.LS(LSID.LoadingLocations), tree.RecordsCount);
            try {
                for (int i = 0, num = tree.RecordsCount; i < num; i++) {
                    GDMIndividualRecord iRec = tree[i] as GDMIndividualRecord;

                    if (iRec != null && IsSelected(iRec) && iRec.HasEvents) {
                        int pCnt = 0;

                        for (int j = 0, num2 = iRec.Events.Count; j < num2; j++) {
                            GDMCustomEvent ev = iRec.Events[j];
                            if (ev.HasPlace && !string.IsNullOrEmpty(ev.Place.StringValue)) {
                                pCnt += AddPlace(iRec, ev);
                            }
                        }

                        if (pCnt > 0) {
                            personValues.AddObject(GKUtils.GetNameString(iRec, true, false) + " [" + pCnt.ToString() + "]", iRec);
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
                fBaseRoot = fView.PlacesTree.AddNode(null, LangMan.LS(LSID.RPLocations), null);

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
                fView.PersonsCombo.AddItem<GDMIndividualRecord>(LangMan.LS(LSID.NotSelected), null);
                for (int i = 0; i < personValues.Count; i++) {
                    fView.PersonsCombo.AddItem(personValues[i], personValues.GetObject(i) as GDMIndividualRecord);
                }

                fView.SelectPlacesBtn.Enabled = true;

                fView.PlacesTree.EndUpdate();
                fView.PersonsCombo.EndUpdate();
                PlacesCache.Instance.Save();
            } catch (Exception ex) {
                Logger.WriteError("MapsViewerWinController.LoadPlaces()", ex);
            }
        }

        private int AddPlace(GDMRecord owner, GDMCustomEvent placeEvent)
        {
            int result = 0;
            try {
                GDMPlace place = placeEvent.Place;
                var locRec = fBase.Context.Tree.GetPtrValue<GDMLocationRecord>(place.Location);
                string placeName = (locRec != null) ? locRec.LocationName : place.StringValue;

                MapPlace mapPlace = null;
                if (!fPlaces.TryGetValue(placeName, out mapPlace)) {
                    bool hasCoords = locRec != null && !locRec.Map.IsEmpty();

                    if (hasCoords || fSearchPlacesWithoutCoords) {
                        mapPlace = new MapPlace(placeName);
                        fPlaces.Add(placeName, mapPlace);
                    }

                    if (hasCoords) {
                        mapPlace.Points.Add(new GeoPoint(locRec.Map.Lati, locRec.Map.Long, placeName));
                    } else if (fSearchPlacesWithoutCoords) {
                        PlacesCache.Instance.GetPlacePoints(placeName, mapPlace.Points);
                    }
                }

                if (mapPlace != null) {
                    mapPlace.PlaceRefs.Add(new PlaceRef(owner, placeEvent));
                    result = 1;
                }
            } catch (Exception ex) {
                Logger.WriteError("MapsViewerWinController.AddPlace()", ex);
            }
            return result;
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
            return (item1.Date == null || item2.Date == null) ? -1 : item1.Date.CompareTo(item2.Date);
        }

        // TODO: localize?
        public async void SaveSnapshot()
        {
            try {
                string filter1 = "Image files|*.jpg";

                string fileName = await AppHost.StdDialogs.GetSaveFile("", "", filter1, 2, "jpg", "");
                if (!string.IsNullOrEmpty(fileName)) {
                    fView.MapBrowser.SaveSnapshot(fileName);
                }
            } catch (Exception ex) {
                Logger.WriteError("MapsViewerWinController.SaveSnapshot()", ex);
                AppHost.StdDialogs.ShowError("Image failed to save: " + ex.Message);
            }
        }

        public override void SetLocale()
        {
            fView.Title = LangMan.LS(LSID.MIMap);

            GetControl<ITabPage>("pagePlaces").Text = LangMan.LS(LSID.RPLocations);
            GetControl<IGroupBox>("grpSelection").Text = LangMan.LS(LSID.MapSelection);
            GetControl<IRadioButton>("radTotal").Text = LangMan.LS(LSID.MapSelOnAll);
            GetControl<ICheckBox>("chkBirth").Text = LangMan.LS(LSID.MSBirthPlaces);
            GetControl<ICheckBox>("chkDeath").Text = LangMan.LS(LSID.MSDeathPlaces);
            GetControl<ICheckBox>("chkResidence").Text = LangMan.LS(LSID.MSResiPlace);
            GetControl<IRadioButton>("radSelected").Text = LangMan.LS(LSID.MapSelOnSelected);
            GetControl<IButton>("btnSelectPlaces").Text = LangMan.LS(LSID.Show);
            GetControl<ICheckBox>("chkLinesVisible").Text = LangMan.LS(LSID.LinesVisible);
            GetControl<IButtonToolItem>("tbLoadPlaces").Text = LangMan.LS(LSID.LoadPlaces);
            GetControl<IButtonToolItem>("tbProviders").Text = LangMan.LS(LSID.Providers);
            GetControl<ITabPage>("pageCoordinates").Text = LangMan.LS(LSID.Coordinates);
            GetControl<IButtonToolItem>("tbClear").Text = LangMan.LS(LSID.Clear);
            GetControl<IButton>("btnSearch").Text = LangMan.LS(LSID.Search);

            SetToolTip("tbSaveSnapshot", LangMan.LS(LSID.ImageSaveTip));
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            GetControl<IToolItem>("tbSaveSnapshot").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_ImageSave, true);
        }
    }
}
