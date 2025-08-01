﻿/*
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

using System.Collections.Generic;
using BSLib;
using GDModel;
using GDModel.Providers.GEDCOM;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Lists;
using GKCore.Locales;
using GKCore.Tools;
using GKUI.Themes;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public class PlacesManagerController : DialogController<IPlacesManagerDlg>
    {
        private readonly FlatListModel fPlacesModel;
        private readonly StringList fPlaces;

        public PlacesManagerController(IPlacesManagerDlg view) : base(view)
        {
            fPlaces = new StringList();
            fPlaces.Sorted = true;

            fPlacesModel = new FlatListModel();
            fView.PlacesList.ListMan = fPlacesModel;
        }

        public override void Init(IBaseWindow baseWin)
        {
            base.Init(baseWin);

            fPlacesModel.ListColumns.Clear();
            fPlacesModel.ListColumns.AddColumn(LangMan.LS(LSID.Place), DataType.dtString, 400, false);
            fPlacesModel.ListColumns.AddColumn(LangMan.LS(LSID.LinksCount), DataType.dtInteger, 100, false);
            fPlacesModel.ListColumns.ResetDefaults();
        }

        public override void UpdateView()
        {
        }

        public void SyncAll()
        {
            AppHost.Instance.ExecuteWork((controller) => {
                GEDCOMChecker.SyncTreeLocations(fBase.Context, controller);
            });
        }

        public void Clear()
        {
            TreeTools.SearchPlaces_Clear(fPlaces);
            fPlaces.Dispose();
        }

        public async void ShowLocExpert()
        {
            var placeObj = GetSelectedPlace();
            if (placeObj == null) return;

            string placeName = placeObj.Name;

            var eventDates = new SortedSet<GDMCustomDate>();
            foreach (var evt in placeObj.Facts) {
                if (!evt.Date.IsEmpty())
                    eventDates.Add(evt.Date);
            }

            using (var dlg = AppHost.ResolveDialog<ILocExpertDlg>(fBase, eventDates, placeName)) {
                await AppHost.Instance.ShowModalAsync(dlg, fView);
            }
        }

        private PlaceObj GetSelectedPlace()
        {
            var selObj = fView.PlacesList.GetSelectedData() as FlatItem;
            if (selObj == null) return null;

            var placeObj = selObj.Tag as PlaceObj;
            if (placeObj == null) return null;

            return placeObj;
        }

        public void ShowDetails()
        {
            var placeObj = GetSelectedPlace();
            if (placeObj == null) return;

            var strList = new StringList();
            strList.Add("[u][b][size=+1]" + placeObj.Name + "[/size][/b][/u]");
            strList.Add("");
            strList.Add(LangMan.LS(LSID.Events) + ":");
            for (int i = 0; i < placeObj.Facts.Count; i++) {
                var evt = placeObj.Facts[i];
                strList.Add("");

                string st = GKUtils.GetEventName(evt);
                strList.Add("  " + st + ": " + GKUtils.GetEventDesc(fBase.Context.Tree, evt));
            }
            strList.Add("");

            string result = strList.Text;

            BaseController.ViewTextInfo(fView, fBase, result);
        }

        public void CheckPlaces()
        {
            try {
                string fltText = fView.FilterBox.Text;
                AppHost.Instance.ExecuteWork((controller) => {
                    TreeTools.SearchPlaces(fBase.Context.Tree, fPlaces, controller, fltText);
                });

                fPlacesModel.Clear();
                for (int i = 0, num = fPlaces.Count; i < num; i++) {
                    PlaceObj placeObj = (PlaceObj)fPlaces.GetObject(i);
                    fPlacesModel.AddItem(placeObj, new object[] { fPlaces[i], placeObj.Facts.Count });
                }
            } finally {
                fView.PlacesList.UpdateContents();
            }
        }

        public async void CreateLocationRecord(IList<object> placesList)
        {
            PlaceObj pObj = placesList.Count > 0 ? (PlaceObj) placesList[0] : null;
            if (pObj == null) return;

            // for cases [*] and [**]
            if (pObj.Name.Contains("*]")) {
                AppHost.StdDialogs.ShowMessage(LangMan.LS(LSID.PlaceAlreadyInBook));
            } else {
                GDMLocationRecord locRec = await fBase.Context.SelectRecord(fView, GDMRecordType.rtLocation, new object[] { pObj.Name }) as GDMLocationRecord;
                if (locRec == null) return;

                for (var pi = 0; pi < placesList.Count; pi++) {
                    PlaceObj place = (PlaceObj) placesList[pi];
                    int num = place.Facts.Count;
                    for (int i = 0; i < num; i++) {
                        GDMCustomEvent evt = place.Facts[i];
                        evt.Place.StringValue = GKUtils.GetLocationNameExt(locRec, evt.Date.Value);
                        evt.Place.Location.XRef = locRec.XRef;
                    }
                }

                CheckPlaces();
                fBase.RefreshLists(false);
            }
        }

        public override void SetLocale()
        {
            fView.SetTitle(LangMan.LS(LSID.PlacesManager));

            if (!AppHost.Instance.HasFeatureSupport(Feature.Mobile)) {
                GetControl<ITabPage>("pagePlaceManage").Text = LangMan.LS(LSID.PlacesManager);
                GetControl<IButton>("btnClose").Text = LangMan.LS(LSID.DlgClose);
                GetControl<IButton>("btnLocExpert").Text = LangMan.LS(LSID.LocExpert);
            }

            GetControl<IButton>("btnIntoList").Text = LangMan.LS(LSID.InsertIntoBook);
            GetControl<IButton>("btnAnalysePlaces").Text = LangMan.LS(LSID.Analyze);
            GetControl<ILabel>("lblFilter").Text = LangMan.LS(LSID.MIFilter);
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            GetControl<IButton>("btnClose").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Cancel);
        }
    }
}
