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

using System.Collections.Generic;
using BSLib;
using GDModel;
using GKCore.Design.Controls;
using GKCore.Design;
using GKCore.Design.Views;
using GKCore.Tools;
using GKCore.Types;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public class PlacesManagerController : DialogController<IPlacesManagerDlg>
    {
        private readonly StringList fPlaces;

        public PlacesManagerController(IPlacesManagerDlg view) : base(view)
        {
            fPlaces = new StringList();
            fPlaces.Sorted = true;
        }

        public override void UpdateView()
        {
        }

        public void Clear()
        {
            TreeTools.SearchPlaces_Clear(fPlaces);
            fPlaces.Dispose();
        }

        public void CheckPlaces()
        {
            fView.PlacesList.BeginUpdate();
            try {
                AppHost.Instance.ExecuteWork((controller) => {
                    TreeTools.SearchPlaces(fBase.Context.Tree, fPlaces, controller, fView.FilterBox.Text);
                });

                fView.PlacesList.ClearItems();

                int num4 = fPlaces.Count;
                for (int i = 0; i < num4; i++) {
                    PlaceObj placeObj = (PlaceObj)fPlaces.GetObject(i);

                    fView.PlacesList.AddItem(placeObj, new object[] { fPlaces[i], placeObj.Facts.Count });
                }
            } finally {
                fView.PlacesList.EndUpdate();
            }
        }

        public void CreateLocationRecord(IList<object> placesList)
        {
            PlaceObj pObj = placesList.Count > 0 ? (PlaceObj) placesList[0] : null;
            if (pObj == null) return;

            if (pObj.Name.IndexOf("[*]") == 0) {
                AppHost.StdDialogs.ShowMessage(LangMan.LS(LSID.PlaceAlreadyInBook));
            } else {
                GDMLocationRecord locRec = fBase.Context.SelectRecord(fView, GDMRecordType.rtLocation, new object[] { pObj.Name }) as GDMLocationRecord;
                if (locRec == null) return;

                for (var pi = 0; pi < placesList.Count; pi++) {
                    PlaceObj place = (PlaceObj) placesList[pi];
                    int num = place.Facts.Count;
                    for (int i = 0; i < num; i++) {
                        GDMCustomEvent evt = place.Facts[i];
                        evt.Place.StringValue = locRec.LocationName;
                        evt.Place.Location.XRef = locRec.XRef;
                    }
                }

                CheckPlaces();
                fBase.RefreshLists(false);
            }
        }

        public override void SetLocale()
        {
            fView.Title = LangMan.LS(LSID.ToolOp_9);

            if (!AppHost.Instance.HasFeatureSupport(Feature.Mobile)) {
                GetControl<ITabPage>("pagePlaceManage").Text = LangMan.LS(LSID.ToolOp_9);
                GetControl<IButton>("btnClose").Text = LangMan.LS(LSID.DlgClose);
            }

            GetControl<IButton>("btnIntoList").Text = LangMan.LS(LSID.InsertIntoBook);
            GetControl<IButton>("btnAnalysePlaces").Text = LangMan.LS(LSID.Analyze);
            GetControl<ILabel>("lblFilter").Text = LangMan.LS(LSID.MIFilter);

            fView.PlacesList.AddColumn(LangMan.LS(LSID.Place), 400, false);
            fView.PlacesList.AddColumn(LangMan.LS(LSID.LinksCount), 100, false);
        }
    }
}
