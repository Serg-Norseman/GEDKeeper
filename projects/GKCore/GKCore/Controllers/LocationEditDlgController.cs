﻿/*
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
using GKCore.Maps;
using GKCore.MVP;
using GKCore.MVP.Views;
using GKCore.Types;

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

        public override bool Accept()
        {
            try {
                bool isRenamed = (fLocationRecord.LocationName != fView.Name.Text);

                fLocationRecord.LocationName = fView.Name.Text;
                fLocationRecord.Map.Lati = ConvertHelper.ParseFloat(fView.Latitude.Text, 0.0);
                fLocationRecord.Map.Long = ConvertHelper.ParseFloat(fView.Longitude.Text, 0.0);

                fLocalUndoman.Commit();

                if (isRenamed) {
                    fBase.Context.Tree.RenameLocationRecord(fLocationRecord);
                }

                fBase.NotifyRecord(fLocationRecord, RecordAction.raEdit);

                return true;
            } catch (Exception ex) {
                Logger.LogWrite("LocationEditDlgController.Accept(): " + ex.Message);
                return false;
            }
        }

        public override void UpdateView()
        {
            fView.Name.Text = fLocationRecord.LocationName;
            fView.Latitude.Text = PlacesLoader.CoordToStr(fLocationRecord.Map.Lati);
            fView.Longitude.Text = PlacesLoader.CoordToStr(fLocationRecord.Map.Long);

            fView.NotesList.ListModel.DataOwner = fLocationRecord;
            fView.MediaList.ListModel.DataOwner = fLocationRecord;
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
                        PlacesLoader.CoordToStr(pt.Latitude), PlacesLoader.CoordToStr(pt.Longitude));

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

            fView.Latitude.Text = PlacesLoader.CoordToStr(pt.Latitude);
            fView.Longitude.Text = PlacesLoader.CoordToStr(pt.Longitude);
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

        public void ShowOnMap()
        {
            if (fView.Latitude.Text != "" && fView.Longitude.Text != "") {
                fView.MapBrowser.SetCenter(ConvertHelper.ParseFloat(fView.Latitude.Text, 0), ConvertHelper.ParseFloat(fView.Longitude.Text, 0), -1);
            }
        }
    }
}
