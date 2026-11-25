/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

#if !TERM

using System;
using System.Collections.Generic;
using System.Globalization;
using BSLib;
using GDModel;
using GDModel.Providers.GEDCOM;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Locales;
using GKCore.Maps;
using GKCore.Options;
using GKMap;
using GKUI.Themes;

namespace GKCore.Controllers
{
    /// <summary>
    ///
    /// </summary>
    public sealed class MapsViewerWinController : FormController<IMapsViewerWin>
    {
        #region Design components
#pragma warning disable CS0649, IDE0044

        private ITabPage _pagePlaces;
        private IGroupBox _grpSelection;
        private ITreeView _tvPlaces;
        private IComboBox _cmbPersons;
        private ICheckBox _chkResidence;
        private ICheckBox _chkDeath;
        private ICheckBox _chkBirth;
        private IButton _btnSelectPlaces;
        private IRadioButton _radTotal;
        private IRadioButton _radSelected;
        private ICheckBox _chkLinesVisible;
        private IToolItem _tbLoadPlaces;
        private IToolItem _tbSaveSnapshot;
        private IToolItem _tbProviders;
        private IToolItem _tbClear;
        private ITabPage _pageCoordinates;
        private IButton _btnSearch;

#pragma warning restore CS0649, IDE0044
        #endregion

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

        public override void InitView(IBaseWindow baseWin)
        {
            base.InitView(baseWin);

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
                _cmbPersons.BeginUpdate();
                _tvPlaces.BeginUpdate();
                _tvPlaces.Clear();
                StringList personValues = new StringList();
                fBaseRoot = _tvPlaces.AddNode(null, LangMan.LS(LSID.RPLocations), null);

                AppHost.Instance.ExecuteWork((controller) => {
                    LoadPlacesInt(personValues, controller);
                });

                foreach (var kvp in fPlaces) {
                    string placeName = kvp.Key;
                    MapPlace mapPlace = kvp.Value;

                    ITVNode node = fView.FindTreeNode(placeName);
                    if (node == null) {
                        node = _tvPlaces.AddNode(fBaseRoot, placeName, mapPlace);

                        int num = mapPlace.Points.Count;
                        for (int i = 0; i < num; i++) {
                            GeoPoint pt = mapPlace.Points[i];
                            string ptTitle = pt.Hint + string.Format(" [{0:0.000000}, {1:0.000000}]", pt.Latitude, pt.Longitude);
                            _tvPlaces.AddNode(node, ptTitle, pt);
                        }
                    }
                }

                _tvPlaces.Expand(fBaseRoot);

                personValues.Sort();
                _cmbPersons.Clear();
                _cmbPersons.AddItem<GDMIndividualRecord>(LangMan.LS(LSID.NotSelected), null);
                for (int i = 0; i < personValues.Count; i++) {
                    _cmbPersons.AddItem(personValues[i], personValues.GetObject(i) as GDMIndividualRecord);
                }

                _btnSelectPlaces.Enabled = true;

                _tvPlaces.EndUpdate();
                _cmbPersons.EndUpdate();
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
            GeoPoint pt = _tvPlaces.GetSelectedData() as GeoPoint;
            if (pt == null) return;

            fView.MapBrowser.SetCenter(pt.Latitude, pt.Longitude, -1);
        }

        public void SelectPlaces()
        {
            GDMIndividualRecord ind = null;

            bool condBirth = false;
            bool condDeath = false;
            bool condResidence = false;

            if (_radTotal.Checked) {
                condBirth = _chkBirth.Checked;
                condDeath = _chkDeath.Checked;
                condResidence = _chkResidence.Checked;
            } else if (_radSelected.Checked && (_cmbPersons.SelectedIndex >= 0)) {
                ind = (_cmbPersons.GetSelectedTag<GDMIndividualRecord>());
            }

            fView.MapBrowser.ShowLines = (ind != null && _chkLinesVisible.Checked);
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

        public async void SaveSnapshot()
        {
            try {
                string filter1 = LangMan.LS(LSID.TreeImagesFilter);
                string fileName = await AppHost.StdDialogs.GetSaveFile("", "", filter1, 2, "jpg", "");
                if (!string.IsNullOrEmpty(fileName)) {
                    fView.MapBrowser.SaveSnapshot(fileName);
                }
            } catch (Exception ex) {
                Logger.WriteError("MapsViewerWinController.SaveSnapshot()", ex);
                await AppHost.StdDialogs.ShowError("Image failed to save: " + ex.Message);
            }
        }

        public override void SetLocale()
        {
            fView.SetTitle(LangMan.LS(LSID.MIMap));

            _pagePlaces.Text = LangMan.LS(LSID.RPLocations);
            _grpSelection.Text = LangMan.LS(LSID.MapSelection);
            _radTotal.Text = LangMan.LS(LSID.MapSelOnAll);
            _chkBirth.Text = LangMan.LS(LSID.MSBirthPlaces);
            _chkDeath.Text = LangMan.LS(LSID.MSDeathPlaces);
            _chkResidence.Text = LangMan.LS(LSID.MSResiPlace);
            _radSelected.Text = LangMan.LS(LSID.MapSelOnSelected);
            _btnSelectPlaces.Text = LangMan.LS(LSID.Show);
            _chkLinesVisible.Text = LangMan.LS(LSID.LinesVisible);
            _tbLoadPlaces.Text = LangMan.LS(LSID.LoadPlaces);
            _tbProviders.Text = LangMan.LS(LSID.Providers);
            _pageCoordinates.Text = LangMan.LS(LSID.Coordinates);
            _tbClear.Text = LangMan.LS(LSID.Clear);
            _btnSearch.Text = LangMan.LS(LSID.Search);

            SetToolTip("tbSaveSnapshot", LangMan.LS(LSID.ImageSaveTip));
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            _tbSaveSnapshot.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_ImageSave, true);
        }
    }
}

#endif
