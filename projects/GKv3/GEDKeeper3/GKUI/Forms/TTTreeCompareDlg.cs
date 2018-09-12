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

using GKCore;
using GKCore.Controllers;
using GKCore.Interfaces;
using GKCore.UIContracts;
using GKUI.Components;

namespace GKUI.Forms
{
    /// <summary>
    /// 
    /// </summary>
    public sealed partial class TTTreeCompareDlg : EditorDialog, ITreeCompareDlg
    {
        private readonly TreeCompareController fController;

        #region View Interface

        ITextBoxHandler ITreeCompareDlg.ExternalBase
        {
            get { return fControlsManager.GetControlHandler<ITextBoxHandler>(txtCompareFile); }
        }

        ITextBoxHandler ITreeCompareDlg.CompareOutput
        {
            get { return fControlsManager.GetControlHandler<ITextBoxHandler>(ListCompare); }
        }

        #endregion

        public TTTreeCompareDlg(IBaseWindow baseWin)
        {
            InitializeComponent();

            btnClose.Image = UIHelper.LoadResourceImage("Resources.btn_cancel.gif");

            SetLang();

            fController = new TreeCompareController(this);
            fController.Init(baseWin);
        }

        public void SetLang()
        {
            Title = LangMan.LS(LSID.LSID_MITreeTools);
            pageTreeCompare.Text = LangMan.LS(LSID.LSID_ToolOp_1);
            btnClose.Text = LangMan.LS(LSID.LSID_DlgClose);
            lblFile.Text = LangMan.LS(LSID.LSID_MIFile);
            btnFileChoose.Text = LangMan.LS(LSID.LSID_DlgSelect) + @"...";
            grpMatchType.Text = LangMan.LS(LSID.LSID_MatchType);
            radMatchInternal.Text = LangMan.LS(LSID.LSID_MatchInternal);
            radMathExternal.Text = LangMan.LS(LSID.LSID_MathExternal);
            radAnalysis.Text = LangMan.LS(LSID.LSID_Analysis);
            btnMatch.Text = LangMan.LS(LSID.LSID_Match);
        }

        private void btnFileChoose_Click(object sender, EventArgs e)
        {
            string fileName = AppHost.StdDialogs.GetOpenFile("", "", LangMan.LS(LSID.LSID_GEDCOMFilter), 1, GKData.GEDCOM_EXT);
            if (string.IsNullOrEmpty(fileName)) return;

            txtCompareFile.Text = fileName;
        }

        public TreeMatchType GetTreeMatchType()
        {
            TreeMatchType type =
                ((radMatchInternal.Checked) ?
                 TreeMatchType.tmtInternal :
                 ((radMathExternal.Checked) ? TreeMatchType.tmtExternal : TreeMatchType.tmtAnalysis));

            return type;
        }

        private void btnMatch_Click(object sender, EventArgs e)
        {
            fController.Match();
        }

        private void rbtnMatch_CheckedChanged(object sender, EventArgs e)
        {
            TreeMatchType type = GetTreeMatchType();

            lblFile.Enabled = (type == TreeMatchType.tmtExternal);
            txtCompareFile.Enabled = (type == TreeMatchType.tmtExternal);
            btnFileChoose.Enabled = (type == TreeMatchType.tmtExternal);
        }
    }
}
