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
using BSLib.Design.MVP.Controls;
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

        IComboBoxHandler IMapsViewerWin.PersonsCombo
        {
            get { return GetControlHandler<IComboBoxHandler>(cmbPersons); }
        }

        ITreeViewHandler IMapsViewerWin.PlacesTree
        {
            get { return GetControlHandler<ITreeViewHandler>(tvPlaces); }
        }

        IButtonHandler IMapsViewerWin.SelectPlacesBtn
        {
            get { return GetControlHandler<IButtonHandler>(btnSelectPlaces); }
        }

        ICheckBoxHandler IMapsViewerWin.BirthCheck
        {
            get { return GetControlHandler<ICheckBoxHandler>(chkBirth); }
        }

        ICheckBoxHandler IMapsViewerWin.DeathCheck
        {
            get { return GetControlHandler<ICheckBoxHandler>(chkDeath); }
        }

        ICheckBoxHandler IMapsViewerWin.ResidenceCheck
        {
            get { return GetControlHandler<ICheckBoxHandler>(chkResidence); }
        }

        ICheckBoxHandler IMapsViewerWin.LinesVisibleCheck
        {
            get { return GetControlHandler<ICheckBoxHandler>(chkLinesVisible); }
        }

        IRadioButtonHandler IMapsViewerWin.TotalRadio
        {
            get { return GetControlHandler<IRadioButtonHandler>(radTotal); }
        }

        IRadioButtonHandler IMapsViewerWin.SelectedRadio
        {
            get { return GetControlHandler<IRadioButtonHandler>(radSelected); }
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
            if (e.KeyCode == Keys.Escape) Close();
        }

        private void btnSaveImage_Click(object sender, EventArgs e)
        {
            fController.SaveImage();
        }

        private void btnSelectPlaces_Click(object sender, EventArgs e)
        {
            fController.SelectPlaces();
        }

        private void TreePlaces_DoubleClick(object sender, EventArgs e)
        {
            GKTreeNode node = tvPlaces.SelectedNode as GKTreeNode;
            if (node == null) return;

            GeoPoint pt = node.Tag as GeoPoint;
            if (pt == null) return;

            fMapBrowser.SetCenter(pt.Latitude, pt.Longitude, -1);
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
            fMapBrowser.Dock = DockStyle.Fill;
            Panel1.Controls.Add(fMapBrowser);

            fController = new MapsViewerWinController(this, baseWin.GetContentList(GDMRecordType.rtIndividual));
            fController.Init(baseWin);

            radTotal.Checked = true;
            radTotal_Click(null, null);

            SetLang();
        }

        public override void SetLang()
        {
            Text = LangMan.LS(LSID.LSID_MIMap);
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

            int num = rootNode.Nodes.Count;
            for (int i = 0; i < num; i++) {
                TreeNode node = rootNode.Nodes[i];

                if (node.Text == place) {
                    return node as ITVNode;
                }
            }

            return null;
        }
    }
}
