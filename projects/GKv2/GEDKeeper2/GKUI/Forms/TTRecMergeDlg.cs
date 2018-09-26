/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2018 by Sergey V. Zhdanovskih.
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

using GKCommon.GEDCOM;
using GKCore;
using GKCore.Controllers;
using GKCore.Interfaces;
using GKCore.MVP.Controls;
using GKCore.MVP.Views;

namespace GKUI.Forms
{
    /// <summary>
    /// 
    /// </summary>
    public sealed partial class TTRecMergeDlg : EditorDialog, IRecMergeDlg
    {
        private readonly RecMergeController fController;

        #region View Interface

        IMergeControl IRecMergeDlg.MergeCtl
        {
            get { return MergeControl; }
        }

        IButtonHandler IRecMergeDlg.SkipBtn
        {
            get { return fControlsManager.GetControlHandler<IButtonHandler>(btnSkip); }
        }

        IProgressBarHandler IRecMergeDlg.ProgressBar
        {
            get { return fControlsManager.GetControlHandler<IProgressBarHandler>(ProgressBar1); }
        }

        ICheckBoxHandler IRecMergeDlg.IndistinctMatchingChk
        {
            get { return fControlsManager.GetControlHandler<ICheckBoxHandler>(chkIndistinctMatching); }
        }

        INumericBoxHandler IRecMergeDlg.NameAccuracyNum
        {
            get { return fControlsManager.GetControlHandler<INumericBoxHandler>(edNameAccuracy); }
        }

        ICheckBoxHandler IRecMergeDlg.BirthYearChk
        {
            get { return fControlsManager.GetControlHandler<ICheckBoxHandler>(chkBirthYear); }
        }

        INumericBoxHandler IRecMergeDlg.YearInaccuracyNum
        {
            get { return fControlsManager.GetControlHandler<INumericBoxHandler>(edYearInaccuracy); }
        }

        #endregion

        public TTRecMergeDlg(IBaseWindow baseWin)
        {
            InitializeComponent();

            fController = new RecMergeController(this);
            fController.Init(baseWin);

            MergeControl.Base = baseWin;
            MergeControl.MergeMode = fController.RMMode;

            SetLang();
        }

        public void SetLang()
        {
            Text = LangMan.LS(LSID.LSID_ToolOp_4);
            pageMerge.Text = LangMan.LS(LSID.LSID_RecMerge);
            pageMergeOptions.Text = LangMan.LS(LSID.LSID_MIOptions);
            btnAutoSearch.Text = LangMan.LS(LSID.LSID_RM_Search);
            btnSkip.Text = LangMan.LS(LSID.LSID_RM_Skip);
            rgMode.Text = LangMan.LS(LSID.LSID_RM_Records);
            radPersons.Text = LangMan.LS(LSID.LSID_RPIndividuals);
            radNotes.Text = LangMan.LS(LSID.LSID_RPNotes);
            radFamilies.Text = LangMan.LS(LSID.LSID_RPFamilies);
            radSources.Text = LangMan.LS(LSID.LSID_RPSources);
            grpSearchPersons.Text = LangMan.LS(LSID.LSID_RM_SearchPersons);
            chkIndistinctMatching.Text = LangMan.LS(LSID.LSID_RM_IndistinctMatching);
            chkBirthYear.Text = LangMan.LS(LSID.LSID_RM_BirthYear);
            lblNameAccuracy.Text = LangMan.LS(LSID.LSID_RM_NameAccuracy);
            lblYearInaccuracy.Text = LangMan.LS(LSID.LSID_RM_YearInaccuracy);
            grpMergeOther.Text = LangMan.LS(LSID.LSID_Other);
            chkBookmarkMerged.Text = LangMan.LS(LSID.LSID_BookmarkMerged);
        }

        private void radMergeMode_Click(object sender, EventArgs e)
        {
            if (radPersons.Checked) fController.RMMode = GEDCOMRecordType.rtIndividual;
            if (radNotes.Checked) fController.RMMode = GEDCOMRecordType.rtNote;
            if (radFamilies.Checked) fController.RMMode = GEDCOMRecordType.rtFamily;
            if (radSources.Checked) fController.RMMode = GEDCOMRecordType.rtSource;

            MergeControl.MergeMode = fController.RMMode;
        }

        private void chkBookmarkMerged_CheckedChanged(object sender, EventArgs e)
        {
            MergeControl.Bookmark = chkBookmarkMerged.Checked;
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
