/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2024 by Sergey V. Zhdanovskih.
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
using System.Windows.Forms;
using GDModel;
using GKCore.Controllers;
using GKCore.Design.Views;
using GKCore.Interfaces;
using GKCore.Options;
using GKUI.Components;

namespace GKUI.Forms
{
    public partial class LocExpertDlg : CommonDialog<ILocExpertDlg, LocExpertDlgController>, ILocExpertDlg
    {
        public LocExpertDlg()
        {
            InitializeComponent();
        }

        public LocExpertDlg(IBaseWindow curBase, SortedSet<GDMCustomDate> eventDates, string placeName) : this()
        {
            btnClose.Image = UIHelper.LoadResourceImage("Resources.btn_cancel.gif");
            chkReverseOrder.Checked = GlobalOptions.Instance.ReversePlaceEntitiesOrder;

            fController = new LocExpertDlgController(this);
            fController.Init(curBase);
            fController.InitView(eventDates, placeName);
        }

        private void cmbEventDates_SelectedIndexChanged(object sender, EventArgs e)
        {
            var date = UIHelper.GetSelectedTag<GDMCustomDate>(cmbEventDates);
            if (date == null) return;
            dtlPlaceDate.Date = ((GDMDateValue)date).Value;
        }

        private void dtlPlaceDate_DateChanged(object sender, EventArgs e)
        {
            fController.Analyze();
        }

        private void btnAnalysis_Click(object sender, EventArgs e)
        {
            fController.Analyze();
        }

        private void cmbLocationSearch_KeyUp(object sender, KeyEventArgs e)
        {
            var text = cmbLocationSearch.Text;
            var selPos = cmbLocationSearch.SelectionStart;

            fController.RefreshLocCombo();

            cmbLocationSearch.Text = text;
            cmbLocationSearch.SelectionStart = selPos;
        }

        private void lvEntries_SelectedIndexChanged(object sender, EventArgs e)
        {
            fController.SelectEntry();
        }

        private void btnLocNameAdd_Click(object sender, EventArgs e)
        {
            fController.AddLocName();
        }

        private void btnLocNameEdit_Click(object sender, EventArgs e)
        {
            fController.EditLocName();
        }

        private void btnTopLinkAdd_Click(object sender, EventArgs e)
        {
            fController.AddTopLink();
        }

        private void btnTopLinkEdit_Click(object sender, EventArgs e)
        {
            fController.EditTopLink();
        }
    }
}
