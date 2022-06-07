/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2022 by Sergey V. Zhdanovskih.
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
using BSLib.Design.MVP.Controls;
using Eto.Forms;
using Eto.Serialization.Xaml;
using GDModel;
using GKCore.Controllers;
using GKCore.Interfaces;
using GKCore.MVP.Controls;
using GKCore.MVP.Views;
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

            fController = new RecMergeController(this);
            fController.Init(baseWin);
        }

        private void radMergeMode_Click(object sender, EventArgs e)
        {
            fController.ChangeOption();
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
    }
}
