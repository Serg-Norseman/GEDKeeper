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

using System.Collections.Generic;
using BSLib.Design.MVP.Controls;
using GDModel;
using GKCore.MVP;
using GKCore.MVP.Views;
using GKCore.Tools;

namespace GKCore.Controllers
{
    public enum TreeMatchType { tmtInternal, tmtExternal, tmtAnalysis }

    /// <summary>
    /// 
    /// </summary>
    public class TreeCompareController : DialogController<ITreeCompareDlg>
    {
        private string fExternalFile;

        public TreeCompareController(ITreeCompareDlg view) : base(view)
        {
        }

        public override void UpdateView()
        {
        }

        public void SelectExternalFile()
        {
            string fileName = AppHost.StdDialogs.GetOpenFile("", "", LangMan.LS(LSID.LSID_GEDCOMFilter), 1, GKData.GEDCOM_EXT);
            if (string.IsNullOrEmpty(fileName)) return;

            fExternalFile = fileName;
            fView.ExternalBase.Text = fileName;
        }

        private void DuplicateFoundFunc(GDMIndividualRecord indivA, GDMIndividualRecord indivB)
        {
            fView.CompareOutput.AppendText("    * [" + GKUtils.GetNameString(indivA, true, false) + "]\r\n");
            fView.CompareOutput.AppendText("      [" + GKUtils.GetNameString(indivB, true, false) + "]\r\n\r\n");
        }

        public void Match()
        {
            TreeMatchType type = GetTreeMatchType();

            fView.CompareOutput.Clear();
            var tree = fBase.Context.Tree;

            switch (type) {
                case TreeMatchType.tmtInternal:
                    TreeTools.FindDuplicates(tree, tree, 90 /*min: 80-85*/, DuplicateFoundFunc, AppHost.Progress);
                    break;

                case TreeMatchType.tmtExternal:
                    TreeTools.CompareTree(fBase.Context, fExternalFile, fView.CompareOutput);
                    break;

                case TreeMatchType.tmtAnalysis:
                    {
                        List<TreeTools.ULIndividual> uln = TreeTools.GetUnlinkedNamesakes(fBase);

                        fView.CompareOutput.AppendText("  " + LangMan.LS(LSID.LSID_SearchUnlinkedNamesakes) + ":\r\n");
                        if (uln != null && uln.Count > 0) {
                            foreach (TreeTools.ULIndividual indiv in uln) {
                                fView.CompareOutput.AppendText("    - [" + indiv.Family + "] " + GKUtils.GetNameString(indiv.IRec, true, false) + "\r\n");
                            }
                        } else {
                            fView.CompareOutput.AppendText("    - not found.");
                        }
                        break;
                    }
            }
        }

        public TreeMatchType GetTreeMatchType()
        {
            TreeMatchType type =
                ((GetControl<IRadioButton>("radMatchInternal").Checked) ?
                 TreeMatchType.tmtInternal :
                 ((GetControl<IRadioButton>("radMathExternal").Checked) ? TreeMatchType.tmtExternal : TreeMatchType.tmtAnalysis));

            return type;
        }

        public void ChangeTreeMatchType()
        {
            TreeMatchType type = GetTreeMatchType();

            GetControl<ILabel>("lblFile").Enabled = (type == TreeMatchType.tmtExternal);
            GetControl<ITextBox>("txtCompareFile").Enabled = (type == TreeMatchType.tmtExternal);
            GetControl<IButton>("btnFileChoose").Enabled = (type == TreeMatchType.tmtExternal);
        }

        public override void SetLocale()
        {
            fView.Title = LangMan.LS(LSID.LSID_ToolOp_1);

            GetControl<ITabPage>("pageTreeCompare").Text = LangMan.LS(LSID.LSID_ToolOp_1);
            GetControl<IButton>("btnClose").Text = LangMan.LS(LSID.LSID_DlgClose);
            GetControl<ILabel>("lblFile").Text = LangMan.LS(LSID.LSID_MIFile);
            GetControl<IButton>("btnFileChoose").Text = LangMan.LS(LSID.LSID_DlgSelect) + @"...";
            GetControl<IGroupBox>("grpMatchType").Text = LangMan.LS(LSID.LSID_MatchType);
            GetControl<IRadioButton>("radMatchInternal").Text = LangMan.LS(LSID.LSID_MatchInternal);
            GetControl<IRadioButton>("radMathExternal").Text = LangMan.LS(LSID.LSID_MathExternal);
            GetControl<IRadioButton>("radAnalysis").Text = LangMan.LS(LSID.LSID_Analyze);
            GetControl<IButton>("btnMatch").Text = LangMan.LS(LSID.LSID_Match);
        }
    }
}
