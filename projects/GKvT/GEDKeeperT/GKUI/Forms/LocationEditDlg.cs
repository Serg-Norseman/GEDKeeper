/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using GDModel;
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Lists;
using GKCore.Locales;
using GKUI.Components;
using Terminal.Gui;

namespace GKUI.Forms
{
    public sealed partial class LocationEditDlg : CommonDialog<ILocationEditDlg, LocationEditDlgController>, ILocationEditDlg
    {
        private readonly GKSheetList fMediaList;
        private readonly GKSheetList fNamesList;
        private readonly GKSheetList fLinksList;
        private readonly GKSheetList fNotesList;

        public GDMLocationRecord LocationRecord
        {
            get { return fController.LocationRecord; }
            set { fController.LocationRecord = value; }
        }

        #region View Interface

        IMapBrowser ILocationEditDlg.MapBrowser
        {
            get { return null; }
        }

        ISheetList ILocationEditDlg.NamesList
        {
            get { return fNamesList; }
        }

        ISheetList ILocationEditDlg.LinksList
        {
            get { return fLinksList; }
        }

        ISheetList ILocationEditDlg.MediaList
        {
            get { return fMediaList; }
        }

        ISheetList ILocationEditDlg.NotesList
        {
            get { return fNotesList; }
        }

        IListView ILocationEditDlg.GeoCoordsList
        {
            get { return ListGeoCoords; }
        }

        ITextBox ILocationEditDlg.Name
        {
            get { return GetControlHandler<ITextBox>(txtName); }
        }

        ITextBox ILocationEditDlg.Latitude
        {
            get { return GetControlHandler<ITextBox>(txtLatitude); }
        }

        ITextBox ILocationEditDlg.Longitude
        {
            get { return GetControlHandler<ITextBox>(txtLongitude); }
        }

        #endregion

        public LocationEditDlg(IBaseWindow baseWin)
        {
            InitializeComponent();

            tabsData.SelectedTabChanged += tabsData_SelectedIndexChanged;

            fNamesList = new GKSheetList(pageHistNames);
            fLinksList = new GKSheetList(pageHistLinks);

            fNotesList = new GKSheetList(pageNotes);
            fMediaList = new GKSheetList(pageMultimedia);

            fController = new LocationEditDlgController(this);
            fController.Init(baseWin);
        }

        private void tabsData_SelectedIndexChanged(object sender, EventArgs e)
        {
            var tabCtl = (TabView)sender;
            var selectedTab = tabCtl.SelectedIndex;
            if (selectedTab == 1) {
                fController.CheckPrimaryName();
            }
        }

        private void EditName_KeyDown(object sender, KeyEventEventArgs e)
        {
            if (e.KeyEvent.Key == Key.CursorDown && e.KeyEvent.IsCtrl) {
                txtName.Text = txtName.Text.ToLower();
            }
        }

        private void btnSearch_Click(object sender, EventArgs e)
        {
            fController.Search();
        }

        private void btnSelect_Click(object sender, EventArgs e)
        {
            fController.SelectCoords();
        }

        private void btnSelectName_Click(object sender, EventArgs e)
        {
            fController.SelectName();
        }

        private void ListGeoCoords_Click(object sender, EventArgs e)
        {
            fController.SelectGeoPoint();
        }

        private void EditName_TextChanged(object sender, string e)
        {
            SetTitle(string.Format("{0} \"{1}\"", LangMan.LS(LSID.Location), txtName.Text));
        }
    }
}
