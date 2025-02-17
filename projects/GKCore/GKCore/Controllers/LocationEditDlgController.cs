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
using BSLib;
using GDModel;
using GDModel.Providers.GEDCOM;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.Maps;
using GKCore.Types;
using GKUI.Themes;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class LocationEditDlgController : DialogController<ILocationEditDlg>
    {
        private GDMLocationRecord fLocationRecord;

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
            fView.Name.Activate();
        }

        public override void Init(IBaseWindow baseWin)
        {
            base.Init(baseWin);

            fView.NamesList.ListModel = new LocationNamesListModel(fView, baseWin, fLocalUndoman);
            fView.LinksList.ListModel = new LocationLinksListModel(fView, baseWin, fLocalUndoman);

            fView.NotesList.ListModel = new NoteLinksListModel(fView, baseWin, fLocalUndoman);
            fView.MediaList.ListModel = new MediaLinksListModel(fView, baseWin, fLocalUndoman);

            fView.NamesList.OnModify += ModifyNamesSheet;
        }

        public override void Done()
        {
            fView.NamesList.ListModel.SaveSettings();
            fView.LinksList.ListModel.SaveSettings();
            fView.NotesList.ListModel.SaveSettings();
            fView.MediaList.ListModel.SaveSettings();
        }

        private void ModifyNamesSheet(object sender, ModifyEventArgs eArgs)
        {
            fView.Name.Text = fLocationRecord.LocationName;
        }

        public void CheckPrimaryName()
        {
            if (!string.IsNullOrEmpty(fView.Name.Text) && string.IsNullOrEmpty(fLocationRecord.LocationName)) {
                // new record
                fLocationRecord.LocationName = fView.Name.Text;
                fView.NamesList.UpdateSheet();
            }
        }

        public override bool Accept()
        {
            try {
                //bool isRenamed = (fLocationRecord.LocationName != fView.Name.Text);

                fLocationRecord.LocationName = fView.Name.Text;
                fLocationRecord.Map.Lati = ConvertHelper.ParseFloat(fView.Latitude.Text, 0.0);
                fLocationRecord.Map.Long = ConvertHelper.ParseFloat(fView.Longitude.Text, 0.0);

                //bool isChanged = isRenamed || fLocalUndoman.HasChanges();

                fLocalUndoman.Commit();

                /*if (isChanged)*/ {
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
            fView.Name.Text = fLocationRecord.LocationName;
            fView.Latitude.Text = GEDCOMUtils.CoordToStr(fLocationRecord.Map.Lati);
            fView.Longitude.Text = GEDCOMUtils.CoordToStr(fLocationRecord.Map.Long);

            fView.NamesList.ListModel.DataOwner = fLocationRecord;
            fView.LinksList.ListModel.DataOwner = fLocationRecord;

            fView.NotesList.ListModel.DataOwner = fLocationRecord;
            fView.MediaList.ListModel.DataOwner = fLocationRecord;

            ShowOnMap();
        }

        public GeoPoint GetSelectedGeoPoint()
        {
            return fView.GeoCoordsList.GetSelectedData() as GeoPoint;
        }

        public void Search()
        {
            string location = fView.Name.Text.Trim();
            if (string.IsNullOrEmpty(location)) {
                return;
            }

            fView.GeoCoordsList.BeginUpdate();
            fView.MapBrowser.BeginUpdate();
            try {
                var searchPoints = new List<GeoPoint>();

                AppHost.Instance.RequestGeoCoords(location, searchPoints);
                fView.GeoCoordsList.ClearItems();
                fView.MapBrowser.ClearPoints();

                int num = searchPoints.Count;
                for (int i = 0; i < num; i++) {
                    GeoPoint pt = searchPoints[i];

                    fView.GeoCoordsList.AddItem(pt, pt.Hint,
                        GEDCOMUtils.CoordToStr(pt.Latitude), GEDCOMUtils.CoordToStr(pt.Longitude));

                    fView.MapBrowser.AddPoint(pt.Latitude, pt.Longitude, pt.Hint);

                    if (i == 0) {
                        fView.MapBrowser.SetCenter(pt.Latitude, pt.Longitude, -1);
                    }
                }
            } finally {
                fView.MapBrowser.EndUpdate();
                fView.GeoCoordsList.EndUpdate();
            }
        }

        public void SelectCoords()
        {
            GeoPoint pt = GetSelectedGeoPoint();
            if (pt == null) return;

            fView.Latitude.Text = GEDCOMUtils.CoordToStr(pt.Latitude);
            fView.Longitude.Text = GEDCOMUtils.CoordToStr(pt.Longitude);
        }

        public void SelectName()
        {
            GeoPoint pt = GetSelectedGeoPoint();
            if (pt == null) return;

            fView.Name.Text = pt.Hint;
        }

        public void SelectGeoPoint()
        {
            GeoPoint pt = GetSelectedGeoPoint();
            if (pt == null) return;

            fView.MapBrowser.SetCenter(pt.Latitude, pt.Longitude, -1);
        }

        public void SelectCursorCoords()
        {
            var pos = fView.MapBrowser.TargetPosition;

            fView.Latitude.Text = GEDCOMUtils.CoordToStr(pos.Lat);
            fView.Longitude.Text = GEDCOMUtils.CoordToStr(pos.Lng);
        }

        public void ShowOnMap()
        {
            if (fView.Latitude.Text != "" && fView.Longitude.Text != "") {
                fView.MapBrowser.SetCenter(ConvertHelper.ParseFloat(fView.Latitude.Text, 0), ConvertHelper.ParseFloat(fView.Longitude.Text, 0), -1);
            }
        }

        public override void SetLocale()
        {
            fView.Title = LangMan.LS(LSID.Location);

            GetControl<IButton>("btnAccept").Text = LangMan.LS(LSID.DlgAccept);
            GetControl<IButton>("btnCancel").Text = LangMan.LS(LSID.DlgCancel);
            GetControl<ITabPage>("pageCommon").Text = LangMan.LS(LSID.Common);
            GetControl<ITabPage>("pageNotes").Text = LangMan.LS(LSID.RPNotes);
            GetControl<ITabPage>("pageMultimedia").Text = LangMan.LS(LSID.RPMultimedia);
            GetControl<ILabel>("lblName").Text = LangMan.LS(LSID.Title);
            GetControl<ILabel>("lblLatitude").Text = LangMan.LS(LSID.Latitude);
            GetControl<ILabel>("lblLongitude").Text = LangMan.LS(LSID.Longitude);
            GetControl<IButton>("btnShowOnMap").Text = LangMan.LS(LSID.Show);
            GetControl<IGroupBox>("grpSearch").Text = LangMan.LS(LSID.SearchCoords);
            GetControl<IButton>("btnSearch").Text = LangMan.LS(LSID.Search);
            GetControl<IButton>("btnSelect").Text = LangMan.LS(LSID.SelectCoords);
            GetControl<IButton>("btnSelectName").Text = LangMan.LS(LSID.SelectName);
            GetControl<IButton>("btnSelectCursor").Text = LangMan.LS(LSID.CursorCoords);

            SetToolTip("btnShowOnMap", LangMan.LS(LSID.ShowOnMapTip));

            fView.GeoCoordsList.ClearColumns();
            fView.GeoCoordsList.AddColumn(LangMan.LS(LSID.Title), 300, false);
            fView.GeoCoordsList.AddColumn(LangMan.LS(LSID.Latitude), 80, false);
            fView.GeoCoordsList.AddColumn(LangMan.LS(LSID.Longitude), 80, false);

            GetControl<ITabPage>("pageHistory").Text = LangMan.LS(LSID.History);
            GetControl<IGroupBox>("pageHistNames").Text = LangMan.LS(LSID.Names);
            GetControl<IGroupBox>("pageHistLinks").Text = LangMan.LS(LSID.TopLevelLinks);
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            GetControl<IButton>("btnAccept").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Accept);
            GetControl<IButton>("btnCancel").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Cancel);

            fView.NamesList.ApplyTheme();
            fView.LinksList.ApplyTheme();
            fView.NotesList.ApplyTheme();
            fView.MediaList.ApplyTheme();
        }
    }
}
