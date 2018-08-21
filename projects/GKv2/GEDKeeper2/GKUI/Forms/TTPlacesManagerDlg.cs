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
using System.Windows.Forms;

using BSLib;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;
using GKCore.Tools;
using GKUI.Components;

namespace GKUI.Forms
{
    /// <summary>
    /// 
    /// </summary>
    public sealed partial class TTPlacesManagerDlg : Form
    {
        private readonly IBaseWindow fBase;
        private readonly GEDCOMTree fTree;
        private readonly StringList fPlaces;

        private GKListView ListPlaces;

        public TTPlacesManagerDlg(IBaseWindow baseWin)
        {
            InitializeComponent();

            btnClose.Image = UIHelper.LoadResourceImage("Resources.btn_cancel.gif");

            fBase = baseWin;
            fTree = fBase.Context.Tree;

            tabsTools.SelectedIndex = 0;

            fPlaces = new StringList();
            fPlaces.Sorted = true;

            ListPlaces = UIHelper.CreateListView(Panel4);
            ListPlaces.DoubleClick += ListPlaces_DblClick;
            ListPlaces.AddColumn(LangMan.LS(LSID.LSID_Place), 400, false);
            ListPlaces.AddColumn(LangMan.LS(LSID.LSID_LinksCount), 100, false);

            SetLang();
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                TreeTools.SearchPlaces_Clear(fPlaces);
                fPlaces.Dispose();
            }
            base.Dispose(disposing);
        }

        public void SetLang()
        {
            Text = LangMan.LS(LSID.LSID_MITreeTools);
            pagePlaceManage.Text = LangMan.LS(LSID.LSID_ToolOp_9);
            pagePlaceManageOptions.Text = LangMan.LS(LSID.LSID_MIOptions);
            btnClose.Text = LangMan.LS(LSID.LSID_DlgClose);
            btnIntoList.Text = LangMan.LS(LSID.LSID_InsertIntoBook);
            btnSearch.Text = LangMan.LS(LSID.LSID_Search);
        }

        private void CheckPlaces()
        {
            ListPlaces.BeginUpdate();
            try {
                TreeTools.SearchPlaces(fTree, fPlaces, AppHost.Progress);

                ListPlaces.Items.Clear();

                int num4 = fPlaces.Count;
                for (int i = 0; i < num4; i++) {
                    PlaceObj placeObj = (PlaceObj)fPlaces.GetObject(i);

                    ListPlaces.AddItem(placeObj, new object[] { fPlaces[i], placeObj.Facts.Count });
                }
            } finally {
                ListPlaces.EndUpdate();
            }
        }

        private void btnIntoList_Click(object sender, EventArgs e)
        {
            ListPlaces_DblClick(null, null);
        }

        private void ListPlaces_DblClick(object sender, EventArgs e)
        {
            GKListItem item = ListPlaces.GetSelectedItem();
            if (item == null) return;

            PlaceObj pObj = item.Data as PlaceObj;
            if (pObj == null) return;

            if (pObj.Name.IndexOf("[*]") == 0) {
                AppHost.StdDialogs.ShowMessage(LangMan.LS(LSID.LSID_PlaceAlreadyInBook));
            } else {
                GEDCOMLocationRecord loc = fBase.Context.SelectRecord(GEDCOMRecordType.rtLocation, new object[] { pObj.Name }) as GEDCOMLocationRecord;
                if (loc == null) return;

                int num = pObj.Facts.Count;
                for (int i = 0; i < num; i++) {
                    GEDCOMCustomEvent evt = pObj.Facts[i];
                    evt.Place.StringValue = loc.LocationName;
                    evt.Place.Location.Value = loc;
                }

                CheckPlaces();
                fBase.RefreshLists(false);
            }
        }

        private void btnSearch_Click(object sender, EventArgs e)
        {
            CheckPlaces();
        }
    }
}
