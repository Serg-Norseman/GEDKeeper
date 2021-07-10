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
using GKCore;
using GKCore.Controllers;
using GKCore.Interfaces;
using GKCore.MVP.Views;
using GKUI.Components;

namespace GKUI.Forms
{
    public sealed partial class TTPlacesManagerDlg : CommonDialog, IPlacesManagerDlg
    {
        private readonly PlacesManagerController fController;

        private GKListView ListPlaces;

        #region View Interface

        IListViewEx IPlacesManagerDlg.PlacesList
        {
            get { return ListPlaces; }
        }

        #endregion

        public TTPlacesManagerDlg(IBaseWindow baseWin)
        {
            InitializeComponent();

            btnClose.Image = UIHelper.LoadResourceImage("Resources.btn_cancel.gif");

            fController = new PlacesManagerController(this);
            fController.Init(baseWin);

            ListPlaces = UIHelper.CreateListView(Panel4);
            ListPlaces.DoubleClick += ListPlaces_DblClick;
            ListPlaces.AddColumn(LangMan.LS(LSID.LSID_Place), 400, false);
            ListPlaces.AddColumn(LangMan.LS(LSID.LSID_LinksCount), 100, false);

            SetLocale();
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                fController.Clear();
            }
            base.Dispose(disposing);
        }

        public void SetLocale()
        {
            Title = LangMan.LS(LSID.LSID_ToolOp_9);
            pagePlaceManage.Text = LangMan.LS(LSID.LSID_ToolOp_9);
            btnClose.Text = LangMan.LS(LSID.LSID_DlgClose);
            btnIntoList.Text = LangMan.LS(LSID.LSID_InsertIntoBook);
            btnAnalysePlaces.Text = LangMan.LS(LSID.LSID_Analyze);
        }

        private void btnAnalysePlaces_Click(object sender, EventArgs e)
        {
            fController.CheckPlaces();
        }

        private void btnIntoList_Click(object sender, EventArgs e)
        {
            fController.CreateLocationRecord();
        }

        private void ListPlaces_DblClick(object sender, EventArgs e)
        {
            fController.CreateLocationRecord();
        }
    }
}
