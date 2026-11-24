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

namespace GKUI.Forms
{
    public sealed partial class CommunicationEditDlg : CommonDialog<ICommunicationEditDlg, CommunicationEditDlgController>, ICommunicationEditDlg
    {
        public GDMCommunicationRecord CommunicationRecord
        {
            get { return fController.CommunicationRecord; }
            set { fController.CommunicationRecord = value; }
        }

        public CommunicationEditDlg(IBaseWindow baseWin)
        {
            InitializeComponent();

            fController = new CommunicationEditDlgController(this);
            fController.InitView(baseWin);
        }

        private void btnPersonAdd_Click(object sender, EventArgs e)
        {
            fController.SetPerson();
        }
    }
}
