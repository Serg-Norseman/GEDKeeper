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

using BSLib;
using GDModel;
using GKCore.MVP;
using GKCore.MVP.Views;
using GKCore.Tools;

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
                TreeTools.SearchPlaces(fBase.Context.Tree, fPlaces, AppHost.Progress);

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

        public void CreateLocationRecord()
        {
            PlaceObj pObj = fView.PlacesList.GetSelectedData() as PlaceObj;
            if (pObj == null) return;

            if (pObj.Name.IndexOf("[*]") == 0) {
                AppHost.StdDialogs.ShowMessage(LangMan.LS(LSID.LSID_PlaceAlreadyInBook));
            } else {
                GDMLocationRecord loc = fBase.Context.SelectRecord(GDMRecordType.rtLocation, new object[] { pObj.Name }) as GDMLocationRecord;
                if (loc == null) return;

                int num = pObj.Facts.Count;
                for (int i = 0; i < num; i++) {
                    GDMCustomEvent evt = pObj.Facts[i];
                    evt.Place.StringValue = loc.LocationName;
                    evt.Place.Location.Value = loc;
                }

                CheckPlaces();
                fBase.RefreshLists(false);
            }
        }
    }
}
