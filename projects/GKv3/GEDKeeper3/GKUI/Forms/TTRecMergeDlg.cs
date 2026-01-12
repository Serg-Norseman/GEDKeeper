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
using GDModel;
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKUI.Components;

namespace GKUI.Forms
{
    public sealed partial class TTRecMergeDlg : CommonDialog<IRecMergeDlg, RecMergeController>, IRecMergeDlg
    {
        #region Design components
#pragma warning disable CS0169, CS0649, IDE0044, IDE0051

        private TabPage pageMerge;
        private Button btnAutoSearch;
        private Button btnSkip;
        private ProgressBar ProgressBar1;
        private TabPage pageMergeOptions;
        private GroupBox rgMode;
        private GroupBox grpSearchPersons;
        private Label lblNameAccuracy;
        private Label lblYearInaccuracy;
        private NumericStepper edNameAccuracy;
        private NumericStepper edYearInaccuracy;
        private CheckBox chkBirthYear;
        private RadioButton radPersons;
        private RadioButton radNotes;
        private RadioButton radFamilies;
        private RadioButton radSources;
        private CheckBox chkBookmarkMerged;
        private GroupBox grpMergeOther;
        private CheckBox chkIndistinctMatching;

        private Button btnMergeToRight;
        private Button btnMergeToLeft;
        private Button btnRec2Select;
        private Button btnRec1Select;
        private TextBox Edit2;
        private TextBox Edit1;
        private Label Lab2;
        private Label Lab1;
        private HyperView fView1;
        private HyperView fView2;
        private Panel pan1;
        private Panel pan2;

#pragma warning restore CS0169, CS0649, IDE0044, IDE0051
        #endregion

        #region View Interface

        IHyperView IRecMergeDlg.View1
        {
            get { return fView1; }
        }

        IHyperView IRecMergeDlg.View2
        {
            get { return fView2; }
        }

        IButton IRecMergeDlg.SkipBtn
        {
            get { return GetControlHandler<IButton>(btnSkip); }
        }

        IProgressBar IRecMergeDlg.ProgressBar
        {
            get { return GetControlHandler<IProgressBar>(ProgressBar1); }
        }

        ICheckBox IRecMergeDlg.IndistinctMatchingChk
        {
            get { return GetControlHandler<ICheckBox>(chkIndistinctMatching); }
        }

        INumericBox IRecMergeDlg.NameAccuracyNum
        {
            get { return GetControlHandler<INumericBox>(edNameAccuracy); }
        }

        ICheckBox IRecMergeDlg.BirthYearChk
        {
            get { return GetControlHandler<ICheckBox>(chkBirthYear); }
        }

        INumericBox IRecMergeDlg.YearInaccuracyNum
        {
            get { return GetControlHandler<INumericBox>(edYearInaccuracy); }
        }

        #endregion

        public TTRecMergeDlg(IBaseWindow baseWin)
        {
            XamlReader.Load(this);

            UIHelper.FixRadioButtons(this, rgMode);

            fView1 = new HyperView();
            //pan1.Content = TableLayout.AutoSized(fView1);
            pan1.Content = (fView1);

            fView2 = new HyperView();
            //pan2.Content = TableLayout.AutoSized(fView2);
            pan2.Content = (fView2);

            fController = new RecMergeController(this);
            fController.Init(baseWin);
        }

        private void radMergeMode_Click(object sender, EventArgs e)
        {
            // prevent triggering on incomplete initialization
            if (fController != null) {
                fController.ChangeOption();
            }
        }

        private void chkBookmarkMerged_CheckedChanged(object sender, EventArgs e)
        {
            fController.ChangeOption();
        }

        private void btnSkip_Click(object sender, EventArgs e)
        {
            fController.Skip();
        }

        private void btnSearch_Click(object sender, EventArgs e)
        {
            fController.Reset();
            fController.SearchDuplicates();
        }

        private void btnRec1Select_Click(object sender, EventArgs e)
        {
            fController.SelectRec1();
        }

        private void btnRec2Select_Click(object sender, EventArgs e)
        {
            fController.SelectRec2();
        }

        private void btnMergeToLeft_Click(object sender, EventArgs e)
        {
            fController.MergeToLeft();
        }

        private void btnMergeToRight_Click(object sender, EventArgs e)
        {
            fController.MergeToRight();
        }

        public void SetRec1(GDMRecord value)
        {
            fController.SetRec1(value);
        }

        public void SetRec2(GDMRecord value)
        {
            fController.SetRec2(value);
        }

        private void btnEditLeft_Click(object sender, EventArgs e)
        {
            fController.EditLeft();
        }

        private void btnEditRight_Click(object sender, EventArgs e)
        {
            fController.EditRight();
        }
    }
}
