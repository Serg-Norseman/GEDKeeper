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
using BSLib.Design.MVP.Controls;
using Eto.Forms;
using GDModel;
using GKCore;
using GKCore.Controllers;
using GKCore.Interfaces;
using GKCore.Maps;
using GKCore.MVP.Controls;
using GKCore.MVP.Views;
using GKUI.Components;

namespace GKUI.Forms
{
    public sealed partial class MapsViewerWin : CommonWindow, IMapsViewerWin
    {
        private readonly MapsViewerWinController fController;

        private readonly GKMapBrowser fMapBrowser;

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

        private void radTotal_Click(object sender, EventArgs e)
        {
            chkBirth.Enabled = radTotal.Checked;
            chkDeath.Enabled = radTotal.Checked;
            chkResidence.Enabled = radTotal.Checked;
            cmbPersons.Enabled = radSelected.Checked;

            if (radTotal.Checked) {
                chkLinesVisible.Checked = false;
            }
            chkLinesVisible.Enabled = radSelected.Checked;
        }

        private void MapsViewerWin_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.Key == Keys.Escape) Close();
        }

        private void btnSaveImage_Click(object sender, EventArgs e)
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

        protected override void OnLoad(EventArgs e)
        {
            base.OnLoad(e);
            fController.LoadPlaces();
            Activate();
        }

        public MapsViewerWin(IBaseWindow baseWin)
        {
            InitializeComponent();

            fMapBrowser = new GKMapBrowser();
            Panel1.Content = fMapBrowser;

            fController = new MapsViewerWinController(this, baseWin.GetContentList(GDMRecordType.rtIndividual));
            fController.Init(baseWin);

            radTotal.Checked = true;

            SetLocale();
        }

        public override void SetLocale()
        {
            Title = LangMan.LS(LSID.LSID_MIMap);
            pagePlaces.Text = LangMan.LS(LSID.LSID_RPLocations);
            grpSelection.Text = LangMan.LS(LSID.LSID_MapSelection);
            radTotal.Text = LangMan.LS(LSID.LSID_MapSelOnAll);
            chkBirth.Text = LangMan.LS(LSID.LSID_MSBirthPlaces);
            chkDeath.Text = LangMan.LS(LSID.LSID_MSDeathPlaces);
            chkResidence.Text = LangMan.LS(LSID.LSID_MSResiPlace);
            radSelected.Text = LangMan.LS(LSID.LSID_MapSelOnSelected);
            btnSaveImage.Text = LangMan.LS(LSID.LSID_SaveImage);
            btnSelectPlaces.Text = LangMan.LS(LSID.LSID_Show);
            chkLinesVisible.Text = LangMan.LS(LSID.LSID_LinesVisible);
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
    }
}
