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

using System;
using System.Collections.Generic;
using GDModel;
using GKCore;
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Maps;
using GKMap;
using GKMap.MapProviders;
using GKUI.Components;

namespace GKUI.Forms
{
    public sealed partial class MapsViewerWin : CommonWindow, IMapsViewerWin
    {
        private readonly MapsViewerWinController fController;

        public IWindow OwnerWindow
        {
            get { return fController.Base; }
        }

        #region View Interface

        IMapBrowser IMapsViewerWin.MapBrowser
        {
            get { return fMapBrowser; }
        }

        IComboBox IMapsViewerWin.PersonsCombo
        {
            get { return GetControlHandler<IComboBox>(cmbPersons); }
        }

        ITreeView IMapsViewerWin.PlacesTree
        {
            get { return GetControlHandler<ITreeView>(tvPlaces); }
        }

        IButton IMapsViewerWin.SelectPlacesBtn
        {
            get { return GetControlHandler<IButton>(btnSelectPlaces); }
        }

        ICheckBox IMapsViewerWin.BirthCheck
        {
            get { return GetControlHandler<ICheckBox>(chkBirth); }
        }

        ICheckBox IMapsViewerWin.DeathCheck
        {
            get { return GetControlHandler<ICheckBox>(chkDeath); }
        }

        ICheckBox IMapsViewerWin.ResidenceCheck
        {
            get { return GetControlHandler<ICheckBox>(chkResidence); }
        }

        ICheckBox IMapsViewerWin.LinesVisibleCheck
        {
            get { return GetControlHandler<ICheckBox>(chkLinesVisible); }
        }

        IRadioButton IMapsViewerWin.TotalRadio
        {
            get { return GetControlHandler<IRadioButton>(radTotal); }
        }

        IRadioButton IMapsViewerWin.SelectedRadio
        {
            get { return GetControlHandler<IRadioButton>(radSelected); }
        }

        #endregion

        public MapsViewerWin(IBaseWindow baseWin)
        {
            InitializeComponent();

            radTotal.IsChecked = true;

            fController = new MapsViewerWinController(this, baseWin.GetContentList(GDMRecordType.rtIndividual));
            fController.Init(baseWin);

            {
                fMapBrowser.MapControl.OnMapTypeChanged += MainMap_OnMapTypeChanged;
                fMapBrowser.MapControl.OnMapZoomChanged += MainMap_OnMapZoomChanged;

                // get zoom  
                trkZoom.Minimum = fMapBrowser.MapControl.MinZoom * 100;
                trkZoom.Maximum = fMapBrowser.MapControl.MaxZoom * 100;

                if (fMapBrowser.MapControl.Zoom >= fMapBrowser.MapControl.MinZoom && fMapBrowser.MapControl.Zoom <= fMapBrowser.MapControl.MaxZoom) {
                    trkZoom.Value = fMapBrowser.MapControl.Zoom * 100;
                }
            }
        }

        public void ShowFixedPoints(IEnumerable<GeoPoint> points)
        {
            fController.ShowFixedPoints(points);
        }

        private void radTotal_Click(object sender, EventArgs e)
        {
            chkBirth.IsEnabled = radTotal.IsChecked;
            chkDeath.IsEnabled = radTotal.IsChecked;
            chkResidence.IsEnabled = radTotal.IsChecked;
            cmbPersons.IsEnabled = radSelected.IsChecked;

            if (radTotal.IsChecked) {
                chkLinesVisible.IsChecked = false;
            }
            chkLinesVisible.IsEnabled = radSelected.IsChecked;
        }

        private void tbSaveSnapshot_Click(object sender, EventArgs e)
        {
            fController.SaveSnapshot();
        }

        private void btnSelectPlaces_Click(object sender, EventArgs e)
        {
            fController.SelectPlaces();
        }

        private void TreePlaces_DoubleClick(object sender, EventArgs e)
        {
            fController.SetCenter();
        }

        private async void miProviderX_Click(object sender, EventArgs e)
        {
            var provider = await UIHelper.SelectItem(this, GMapProviders.List);
            if (provider != null) {
                fMapBrowser.MapControl.MapProvider = provider;
            }
        }

        public override void SetLocale()
        {
            fController.SetLocale();
        }

        public override void ApplyTheme()
        {
            base.ApplyTheme();
            fController.ApplyTheme();
        }

        public ITVNode FindTreeNode(string place)
        {
            GKTreeNode rootNode = fController.TreeRoot as GKTreeNode;

            int num = rootNode.Children.Count;
            for (int i = 0; i < num; i++) {
                GKTreeNode node = rootNode.Children[i] as GKTreeNode;

                if (node != null && node.Text == place) {
                    return node;
                }
            }

            return null;
        }

        private void tbLoadPlaces_Click(object sender, EventArgs e)
        {
            fController.LoadPlaces();
        }

        private void tbClear_Click(object sender, EventArgs e)
        {
            fMapBrowser.Objects.Clear();
        }

        private void tbZoomCenter_Click(object sender, EventArgs e)
        {
            fMapBrowser.MapControl.ZoomAndCenterMarkers("objects");
        }

        private void btnSearch_Click(object sender, EventArgs e)
        {
            GeocoderStatusCode status = fMapBrowser.MapControl.SetPositionByKeywords(txtPlace.Text);
            if (status != GeocoderStatusCode.Success) {
                AppHost.StdDialogs.ShowError("Geocoder can't find: '" + txtPlace.Text + "', reason: " + status);
            }
        }

        private void MainMap_OnMapTypeChanged(GMapProvider type)
        {
            //cmbMapType.SelectedItem = type;
            trkZoom.Minimum = fMapBrowser.MapControl.MinZoom * 100;
            trkZoom.Maximum = fMapBrowser.MapControl.MaxZoom * 100;
            fMapBrowser.MapControl.ZoomAndCenterMarkers("objects");
        }

        private void MainMap_OnMapZoomChanged()
        {
            trkZoom.Value = (int)(fMapBrowser.MapControl.Zoom * 100.0);
        }

        private void trkZoom_ValueChanged(object sender, EventArgs e)
        {
            fMapBrowser.MapControl.Zoom = (int)Math.Floor(trkZoom.Value / 100.0);
        }

        private void btnZoomUp_Click(object sender, EventArgs e)
        {
            fMapBrowser.MapControl.Zoom = fMapBrowser.MapControl.Zoom + 1;
        }

        private void btnZoomDown_Click(object sender, EventArgs e)
        {
            fMapBrowser.MapControl.Zoom = ((int)(fMapBrowser.MapControl.Zoom + 0.99)) - 1;
        }
    }
}
