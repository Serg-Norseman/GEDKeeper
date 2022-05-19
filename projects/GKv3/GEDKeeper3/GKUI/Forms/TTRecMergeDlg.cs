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
using GKCore.Controllers;
using GKCore.Interfaces;
using GKCore.MVP.Controls;
using GKCore.MVP.Views;

namespace GKUI.Forms
{
    public sealed partial class TTRecMergeDlg : CommonDialog, IRecMergeDlg
    {
        #region Design components
#pragma warning disable CS0169

        private TabPage pageMerge;
        private Button btnAutoSearch;
        private Button btnSkip;
        private ProgressBar ProgressBar1;
        private TabPage pageMergeOptions;
        private GroupBox rgMode;
        private GroupBox grpSearchPersons;
        private Label lblNameAccuracy;
        private Label lblYearInaccuracy;
        private NumericUpDown edNameAccuracy;
        private NumericUpDown edYearInaccuracy;
        private CheckBox chkBirthYear;
        private RadioButton radPersons;
        private RadioButton radNotes;
        private RadioButton radFamilies;
        private RadioButton radSources;
        private CheckBox chkBookmarkMerged;
        private GroupBox grpMergeOther;
        private GKUI.Components.GKMergeControl MergeControl;
        private CheckBox chkIndistinctMatching;

#pragma warning restore CS0169
        #endregion

        private readonly RecMergeController fController;

        #region View Interface

        IMergeControl IRecMergeDlg.MergeCtl
        {
            get { return MergeControl; }
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

            MergeControl.Base = baseWin;
            MergeControl.MergeMode = fController.RMMode;
        }

        private void radMergeMode_Click(object sender, EventArgs e)
        {
            fController.ChangeMergeMode();
        }

        private void chkBookmarkMerged_CheckedChanged(object sender, EventArgs e)
        {
            MergeControl.Bookmark = chkBookmarkMerged.Checked.GetValueOrDefault();
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
    }
}
