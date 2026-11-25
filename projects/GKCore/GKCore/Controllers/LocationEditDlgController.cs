/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections.Generic;
using BSLib;
using GDModel;
using GDModel.Providers.GEDCOM;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Lists;
using GKCore.Locales;
using GKCore.Maps;
using GKCore.Options;
using GKUI.Themes;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class LocationEditDlgController : DialogController<ILocationEditDlg>
    {
        #region Design components
#pragma warning disable CS0649, IDE0044
        private IButton _btnAccept;
        private IButton _btnCancel;
        private ITabPage _pageNotes;
        private ITabPage _pageMultimedia;
        private ITabPage _pageCommon;
        private ILabel _lblName;
        private ITextBox _txtName;
        private ILabel _lblLatitude;
        private ITextBox _txtLatitude;
        private ILabel _lblLongitude;
        private ITextBox _txtLongitude;
        private IGroupBox _grpSearch;
        private IListView _ListGeoCoords;
        private IButton _btnSearch;
        private IButton _btnSelect;
        private IButton _btnSelectName;
        private IButton _btnSelectCursor;
        private IButton _btnShowOnMap;
        private IMapBrowser _fMapBrowser;
        private ISheetList _fMediaList;
        private ISheetList _fNamesList;
        private ISheetList _fLinksList;
        private ISheetList _fNotesList;
        private ITabPage _pageHistory;
        private IGroupBox _pageHistNames;
        private IGroupBox _pageHistLinks;
#pragma warning restore CS0649, IDE0044
        #endregion

        private GDMLocationRecord fLocationRecord;
        private GPListModel fPointsModel;

        public GDMLocationRecord LocationRecord
        {
            get { return fLocationRecord; }
            set {
                if (fLocationRecord != value) {
                    fLocationRecord = value;
                    UpdateView();
                }
            }
        }


        public LocationEditDlgController(ILocationEditDlg view) : base(view)
        {
        }

        public override void InitView(IBaseWindow baseWin)
        {
            base.InitView(baseWin);

            _txtName.Activate();

            _fNamesList.ListModel = new LocationNamesListModel(fView, baseWin, fLocalUndoman);
            _fLinksList.ListModel = new LocationLinksListModel(fView, baseWin, fLocalUndoman);

            _fNotesList.ListModel = new NoteLinksListModel(fView, baseWin, fLocalUndoman);
            _fMediaList.ListModel = new MediaLinksListModel(fView, baseWin, fLocalUndoman);

            _fNamesList.OnModify += ModifyNamesSheet;

            fPointsModel = new GPListModel();
            _ListGeoCoords.ListMan = fPointsModel;
            _ListGeoCoords.UpdateContents();
        }

        public override void DoneView()
        {
            _fNamesList.ListModel.SaveSettings();
            _fLinksList.ListModel.SaveSettings();
            _fNotesList.ListModel.SaveSettings();
            _fMediaList.ListModel.SaveSettings();
        }

        private void ModifyNamesSheet(object sender, ModifyEventArgs eArgs)
        {
            _txtName.Text = fLocationRecord.LocationName;
        }

        public void CheckPrimaryName()
        {
            if (!string.IsNullOrEmpty(_txtName.Text) && string.IsNullOrEmpty(fLocationRecord.LocationName)) {
                // new record
                fLocationRecord.LocationName = _txtName.Text;
                _fNamesList.UpdateSheet();
            }
        }

        public override bool Accept()
        {
            try {
                //bool isRenamed = (fLocationRecord.LocationName != _txtName.Text);

                fLocationRecord.LocationName = _txtName.Text;
                fLocationRecord.Map.Lati = ConvertHelper.ParseFloat(_txtLatitude.Text, 0.0);
                fLocationRecord.Map.Long = ConvertHelper.ParseFloat(_txtLongitude.Text, 0.0);

                //bool isChanged = isRenamed || fLocalUndoman.HasChanges();

                fLocalUndoman.Commit();

                /*if (isChanged)*/
                {
                    fBase.Context.Tree.RenameLocationRecord(fLocationRecord);
                }

                fBase.NotifyRecord(fLocationRecord, RecordAction.raEdit);

                return true;
            } catch (Exception ex) {
                Logger.WriteError("LocationEditDlgController.Accept()", ex);
                return false;
            }
        }

        public override void UpdateView()
        {
            _txtName.Text = fLocationRecord.LocationName;
            _txtLatitude.Text = GEDCOMUtils.CoordToStr(fLocationRecord.Map.Lati);
            _txtLongitude.Text = GEDCOMUtils.CoordToStr(fLocationRecord.Map.Long);

            _fNamesList.ListModel.DataOwner = fLocationRecord;
            _fLinksList.ListModel.DataOwner = fLocationRecord;

            _fNotesList.ListModel.DataOwner = fLocationRecord;
            _fMediaList.ListModel.DataOwner = fLocationRecord;

#if !TERM
            ShowOnMap();
#endif
        }

        public GeoPoint GetSelectedGeoPoint()
        {
            return _ListGeoCoords.GetSelectedData() as GeoPoint;
        }

        public void Search()
        {
            string location = _txtName.Text.Trim();
            if (string.IsNullOrEmpty(location)) {
                return;
            }

            var searchPoints = new List<GeoPoint>();
            AppHost.Instance.RequestGeoCoords(location, searchPoints);
            fPointsModel.DataSource = searchPoints;
            _ListGeoCoords.UpdateContents();

#if !TERM
            _fMapBrowser.BeginUpdate();
            try {
                _fMapBrowser.ClearPoints();

                for (int i = 0, num = searchPoints.Count; i < num; i++) {
                    GeoPoint pt = searchPoints[i];

                    _fMapBrowser.AddPoint(pt.Latitude, pt.Longitude, pt.Hint);

                    if (i == 0) {
                        _fMapBrowser.SetCenter(pt.Latitude, pt.Longitude, -1);
                    }
                }
            } finally {
                _fMapBrowser.EndUpdate();
            }
#endif
        }

        public void SelectCoords()
        {
            GeoPoint pt = GetSelectedGeoPoint();
            if (pt == null) return;

            _txtLatitude.Text = GEDCOMUtils.CoordToStr(pt.Latitude);
            _txtLongitude.Text = GEDCOMUtils.CoordToStr(pt.Longitude);
        }

        public void SelectName()
        {
            GeoPoint pt = GetSelectedGeoPoint();
            if (pt == null) return;

            _txtName.Text = pt.Hint;
        }

        public void SelectGeoPoint()
        {
            GeoPoint pt = GetSelectedGeoPoint();
            if (pt == null) return;

#if !TERM
            _fMapBrowser.SetCenter(pt.Latitude, pt.Longitude, -1);
#else
            _txtLatitude.Text = GEDCOMUtils.CoordToStr(pt.Latitude);
            _txtLongitude.Text = GEDCOMUtils.CoordToStr(pt.Longitude);
#endif
        }

#if !TERM
        public void SelectCursorCoords()
        {
            var pos = _fMapBrowser.TargetPosition;

            _txtLatitude.Text = GEDCOMUtils.CoordToStr(pos.Lat);
            _txtLongitude.Text = GEDCOMUtils.CoordToStr(pos.Lng);
        }

        public void ShowOnMap()
        {
            if (_txtLatitude.Text != "" && _txtLongitude.Text != "") {
                _fMapBrowser.SetCenter(ConvertHelper.ParseFloat(_txtLatitude.Text, 0), ConvertHelper.ParseFloat(_txtLongitude.Text, 0), -1);
            }
        }
#endif

        public override void SetLocale()
        {
            fView.SetTitle(LangMan.LS(LSID.Location));

            _btnAccept.Text = LangMan.LS(LSID.DlgAccept);
            _btnCancel.Text = LangMan.LS(LSID.DlgCancel);
            _pageCommon.Text = LangMan.LS(LSID.Common);
            _pageNotes.Text = LangMan.LS(LSID.RPNotes);
            _pageMultimedia.Text = LangMan.LS(LSID.RPMultimedia);
            _lblName.Text = LangMan.LS(LSID.Title);
            _lblLatitude.Text = LangMan.LS(LSID.Latitude);
            _lblLongitude.Text = LangMan.LS(LSID.Longitude);
            _grpSearch.Text = LangMan.LS(LSID.SearchCoords);
            _btnSearch.Text = LangMan.LS(LSID.Search);
            _btnSelect.Text = LangMan.LS(LSID.SelectCoords);
            _btnSelectName.Text = LangMan.LS(LSID.SelectName);

#if !TERM
            _btnSelectCursor.Text = LangMan.LS(LSID.CursorCoords);
            _btnShowOnMap.Text = LangMan.LS(LSID.Show);
            SetToolTip("btnShowOnMap", LangMan.LS(LSID.ShowOnMapTip));
#endif

            _pageHistory.Text = LangMan.LS(LSID.History);
            _pageHistNames.Text = LangMan.LS(LSID.Names);
            _pageHistLinks.Text = LangMan.LS(LSID.TopLevelLinks);
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            _btnAccept.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Accept);
            _btnCancel.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Cancel);
        }


        private sealed class GPListModel : SimpleListModel<GeoPoint>
        {
            public GPListModel() : base(null, CreateListColumns())
            {
            }

            public static ListColumns CreateListColumns()
            {
                var result = new ListColumns(GKListType.ltNone);
                result.AddColumn(LangMan.LS(LSID.Title), 300, false);
                result.AddColumn(LangMan.LS(LSID.Latitude), 80, false);
                result.AddColumn(LangMan.LS(LSID.Longitude), 80, false);
                return result;
            }

            protected override object GetColumnValueEx(int colType, int colSubtype, bool isVisible)
            {
                object result = null;
                switch (colType) {
                    case 0:
                        result = fFetchedRec.Hint;
                        break;
                    case 1:
                        result = GEDCOMUtils.CoordToStr(fFetchedRec.Latitude);
                        break;
                    case 2:
                        result = GEDCOMUtils.CoordToStr(fFetchedRec.Longitude);
                        break;
                }
                return result;
            }
        }
    }
}
