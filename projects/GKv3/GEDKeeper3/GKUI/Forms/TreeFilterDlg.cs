/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using Eto.Forms;
using Eto.Serialization.Xaml;
using GKCore.Charts;
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Lists;
using GKUI.Components;

namespace GKUI.Forms
{
    public sealed partial class TreeFilterDlg : CommonDialog<ITreeFilterDlg, TreeFilterDlgController>, ITreeFilterDlg
    {
        #region Design components
#pragma warning disable CS0169, CS0649, IDE0044, IDE0051

        private Button btnAccept;
        private Button btnCancel;
        private Label lblRPSources;
        private ComboBox cmbSource;
        private GroupBox rgBranchCut;
        private RadioButton rbCutNone;
        private RadioButton rbCutYears;
        private RadioButton rbCutPersons;
        private Label lblYear;
        private NumericStepper edYear;
        private GKSheetList fPersonsList;

#pragma warning restore CS0169, CS0649, IDE0044, IDE0051
        #endregion

        public ChartFilter Filter
        {
            get { return fController.Filter; }
            set { fController.Filter = value; }
        }

        #region View Interface

        ISheetList ITreeFilterDlg.PersonsList
        {
            get { return fPersonsList; }
        }

        INumericBox ITreeFilterDlg.YearNum
        {
            get { return GetControlHandler<INumericBox>(edYear); }
        }

        IComboBox ITreeFilterDlg.SourceCombo
        {
            get { return GetControlHandler<IComboBox>(cmbSource); }
        }

        #endregion

        public TreeFilterDlg(IBaseWindow baseWin)
        {
            XamlReader.Load(this);

            UIHelper.FixRadioButtons(this, rgBranchCut);

            fController = new TreeFilterDlgController(this);
            fController.Init(baseWin);
        }

        private void rbCutX_CheckedChanged(object sender, EventArgs e)
        {
            // prevent triggering on incomplete initialization
            if (fController != null) {
                fController.ChangeCutMode();
            }
        }

        private void TreeFilterDlg_Load(object sender, EventArgs e)
        {
            fController.UpdateView();
        }
    }
}
