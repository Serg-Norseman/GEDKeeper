/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Windows.Forms;
using GDModel;
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;

namespace GKUI.Forms
{
    public sealed partial class RecordInfoDlg : CommonDialog, IRecordInfoDlg
    {
        private readonly RecordInfoDlgController fController;

        public GDMRecord Record
        {
            get { return fController.Record; }
            set { fController.Record = value; }
        }

        #region View Interface

        IHyperView IRecordInfoDlg.HyperView
        {
            get { return hyperView1; }
        }

        #endregion

        public RecordInfoDlg(IBaseWindow baseWin)
        {
            InitializeComponent();

            fController = new RecordInfoDlgController(this);
            fController.Init(baseWin);
            fController.UpdateView();
        }

        private void HyperViewLink(object sender, string linkName)
        {
            fController.SelectLink(linkName);
        }

        private void Form_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.KeyCode == Keys.Escape)
                Close();
        }
    }
}
