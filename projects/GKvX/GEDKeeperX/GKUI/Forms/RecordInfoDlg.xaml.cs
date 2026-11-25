/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GDModel;
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Design.Views;

namespace GKUI.Forms
{
    public sealed partial class RecordInfoDlg : CommonDialog<IRecordInfoDlg, RecordInfoDlgController>, IRecordInfoDlg
    {
        public GDMRecord Record
        {
            get { return fController.Record; }
            set { fController.Record = value; }
        }

        string IRecordInfoDlg.Text
        {
            get { return fController.Text; }
            set { fController.Text = value; }
        }

        public RecordInfoDlg(IBaseWindow baseWin)
        {
            InitializeComponent();

            fController = new RecordInfoDlgController(this);
            fController.InitView(baseWin);
            fController.UpdateView();
        }

        private void HyperViewLink(object sender, string linkName)
        {
            fController.SelectLink(linkName);
        }
    }
}
