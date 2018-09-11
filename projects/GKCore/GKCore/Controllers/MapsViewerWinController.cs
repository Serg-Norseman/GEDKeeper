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
using GKCommon.GEDCOM;
using GKCore.Interfaces;
using GKCore.Maps;
using GKCore.Options;
using GKCore.Types;
using GKCore.UIContracts;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class MapsViewerWinController : FormController<IMapsViewerWin>
    {
        private readonly List<GEDCOMRecord> fSelectedPersons;
        private readonly ExtList<MapPlace> fPlaces;

        public ExtList<MapPlace> Places
        {
            get { return fPlaces; }
        }

        public MapsViewerWinController(IMapsViewerWin view, List<GEDCOMRecord> selectedPersons) : base(view)
        {
            fPlaces = new ExtList<MapPlace>(true);
            fSelectedPersons = selectedPersons;
        }

        public override void UpdateView()
        {
        }

        private bool IsSelected(GEDCOMRecord iRec)
        {
            bool res = (fSelectedPersons == null || (fSelectedPersons.IndexOf(iRec) >= 0));
            return res;
        }

        public void LoadPlaces()
        {
            try {
                PlacesCache.Instance.Load();

                IProgressController progress = AppHost.Progress;
                GEDCOMTree tree = fBase.Context.Tree;

                fView.MapBrowser.InitMap();

                fView.PersonsCombo.BeginUpdate();
                fView.PlacesTree.BeginUpdate();
                progress.ProgressInit(LangMan.LS(LSID.LSID_LoadingLocations), tree.RecordsCount);
                try {
                    fPlaces.Clear();
                    fView.PersonsCombo.Clear();
                    //fView.PersonsCombo.Sorted = false;
                    fView.PersonsCombo.AddItem(LangMan.LS(LSID.LSID_NotSelected), null);

                    int num = tree.RecordsCount;
                    for (int i = 0; i < num; i++) {
                        GEDCOMRecord rec = tree[i];
                        bool res = rec is GEDCOMIndividualRecord && IsSelected(rec);

                        if (res) {
                            GEDCOMIndividualRecord ind = rec as GEDCOMIndividualRecord;
                            int pCnt = 0;

                            int num2 = ind.Events.Count;
                            for (int j = 0; j < num2; j++) {
                                GEDCOMCustomEvent ev = ind.Events[j];
                                if (ev.Place.StringValue != "") {
                                    fView.AddPlace(ev.Place, ev);
                                    pCnt++;
                                }
                            }

                            if (pCnt > 0) {
                                fView.PersonsCombo.AddItem(GKUtils.GetNameString(ind, true, false) + " [" + pCnt.ToString() + "]", ind);
                            }
                        }

                        progress.ProgressStep();
                    }

                    fView.PlacesTree.Expand(fView.TreeRoot);
                    fView.PersonsCombo.SortItems();

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
