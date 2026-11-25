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
using GKCore.Design.Views;
using GKCore.Utilities;

namespace GKUI.Forms
{
    public sealed partial class EventEditDlg : CommonDialog<IEventEditDlg, EventEditDlgController>, IEventEditDlg
    {
        public GDMCustomEvent Event
        {
            get { return fController.Event; }
            set { fController.Event = value; }
        }

        public EventEditDlg(IBaseWindow baseWin)
        {
            InitializeComponent();

            fController = new EventEditDlgController(this);
            fController.InitView(baseWin);
        }

        private void btnAddress_Click(object sender, EventArgs e)
        {
            fController.ModifyAddress();
        }

        private void btnPlaceAdd_Click(object sender, EventArgs e)
        {
            fController.AddPlace();
        }

        private void btnPlaceDelete_Click(object sender, EventArgs e)
        {
            fController.RemovePlace();
        }

        private void EditEventType_SelectedIndexChanged(object sender, EventArgs e)
        {
            if (cmbEventType.Text == FreqCollection<string>.LineItem) {
                cmbEventType.SelectedIndex = 0;
                return;
            }

            fController.ChangeEventType();
        }

        public void SendData(string signature, string data)
        {
            fController.SendData(signature, data);
        }

        private void btnAge_Click(object sender, EventArgs e)
        {
            fController.ModifyAge();
        }
    }
}
