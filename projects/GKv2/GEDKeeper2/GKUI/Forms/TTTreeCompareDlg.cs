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
using System.Collections.Generic;
using System.IO;
using System.Windows.Forms;

using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;
using GKCore.Tools;
using GKUI.Components;

namespace GKUI.Forms
{
    /// <summary>
    /// 
    /// </summary>
    public sealed partial class TTTreeCompareDlg : CommonDialog
    {
        private readonly IBaseWindow fBase;
        private readonly GEDCOMTree fTree;

        public TTTreeCompareDlg(IBaseWindow baseWin)
        {
            InitializeComponent();
            btnClose.Image = UIHelper.LoadResourceImage("Resources.btn_cancel.gif");

            fBase = baseWin;
            fTree = fBase.Context.Tree;

            SetLang();
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
            }
            base.Dispose(disposing);
        }

        public void SetLang()
        {
            Text = LangMan.LS(LSID.LSID_MITreeTools);
            pageTreeCompare.Text = LangMan.LS(LSID.LSID_ToolOp_1);
            pageTreeCompareOptions.Text = LangMan.LS(LSID.LSID_MIOptions);
            btnClose.Text = LangMan.LS(LSID.LSID_DlgClose);
            lblFile.Text = LangMan.LS(LSID.LSID_MIFile);
            btnFileChoose.Text = LangMan.LS(LSID.LSID_DlgSelect) + @"...";
            grpMatchType.Text = LangMan.LS(LSID.LSID_MatchType);
            radMatchInternal.Text = LangMan.LS(LSID.LSID_MatchInternal);
            radMathExternal.Text = LangMan.LS(LSID.LSID_MathExternal);
            radAnalysis.Text = LangMan.LS(LSID.LSID_Analysis);
            btnMatch.Text = LangMan.LS(LSID.LSID_Match);
        }

        private string external_match_db;
        private enum TreeMatchType { tmtInternal, tmtExternal, tmtAnalysis }

        private void btnFileChoose_Click(object sender, EventArgs e)
        {
            string fileName = AppHost.StdDialogs.GetOpenFile("", "", LangMan.LS(LSID.LSID_GEDCOMFilter), 1, GKData.GEDCOM_EXT);
            if (string.IsNullOrEmpty(fileName)) return;

            external_match_db = fileName;
            txtCompareFile.Text = Path.GetFileName(external_match_db);
        }

        private void DuplicateFoundFunc(GEDCOMIndividualRecord indivA, GEDCOMIndividualRecord indivB)
        {
            ListCompare.AppendText("    * [" + GKUtils.GetNameString(indivA, true, false) + "]\r\n");
            ListCompare.AppendText("      [" + GKUtils.GetNameString(indivB, true, false) + "]\r\n\r\n");
        }

        private TreeMatchType GetTreeMatchType()
        {
            TreeMatchType type =
                ((radMatchInternal.Checked) ?
                 TreeMatchType.tmtInternal :
                 ((radMathExternal.Checked) ? TreeMatchType.tmtExternal : TreeMatchType.tmtAnalysis));

            return type;
        }

        private void btnMatch_Click(object sender, EventArgs e)
        {
            TreeMatchType type = GetTreeMatchType();

            ListCompare.Clear();

            switch (type) {
                case TreeMatchType.tmtInternal:
                    TreeTools.FindDuplicates(fTree, fTree, 90 /*min: 80-85*/, DuplicateFoundFunc, AppHost.Progress);
                    break;

                case TreeMatchType.tmtExternal:
                    TreeTools.CompareTree(fBase.Context, external_match_db, ListCompare);
                    break;

                case TreeMatchType.tmtAnalysis:
                    {
                        List<TreeTools.ULIndividual> uln = TreeTools.GetUnlinkedNamesakes(fBase);

                        ListCompare.AppendText("  " + LangMan.LS(LSID.LSID_SearchUnlinkedNamesakes) + ":\r\n");
                        if (uln != null && uln.Count > 0)
                        {
                            foreach (TreeTools.ULIndividual indiv in uln)
                            {
                                ListCompare.AppendText("    - [" + indiv.Family + "] " + GKUtils.GetNameString(indiv.IRec, true, false) + "\r\n");
                            }
                        }
                        else
                        {
                            ListCompare.AppendText("    - not found.");
                        }
                        break;
                    }
            }
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
