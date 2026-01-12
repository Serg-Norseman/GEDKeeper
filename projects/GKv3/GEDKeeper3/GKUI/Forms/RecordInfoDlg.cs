/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using Eto.Forms;
using Eto.Serialization.Xaml;
using GDModel;
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKUI.Components;

namespace GKUI.Forms
{
    public sealed partial class RecordInfoDlg : CommonDialog, IRecordInfoDlg
    {
        #region Design components
#pragma warning disable CS0169, CS0649, IDE0044, IDE0051

        private HyperView hyperView1;

#pragma warning restore CS0169, CS0649, IDE0044, IDE0051
        #endregion

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
            XamlReader.Load(this);

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
            if (e.Key == Keys.Escape)
                Close();
        }
    }
}
