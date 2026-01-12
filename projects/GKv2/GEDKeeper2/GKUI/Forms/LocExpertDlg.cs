/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections.Generic;
using System.Windows.Forms;
using GDModel;
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Design.Views;
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
