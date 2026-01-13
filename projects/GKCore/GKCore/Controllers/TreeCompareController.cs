/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Collections.Generic;
using GDModel;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Locales;
using GKCore.Tools;
using GKUI.Themes;

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

        public async void SelectExternalFile()
        {
            string fileName = await AppHost.StdDialogs.GetOpenFile("", "", LangMan.LS(LSID.GEDCOMFilter), 1, GKData.GEDCOM_EXT);
            if (string.IsNullOrEmpty(fileName)) return;

            fExternalFile = fileName;
            fView.ExternalBase.Text = fileName;
        }

        private IProgressController fProgressController;

        private void DuplicateFoundFunc(GDMIndividualRecord indivA, GDMIndividualRecord indivB)
        {
            fProgressController.InvokeEx(() => {
                fView.CompareOutput.AppendText("    * [" + GKUtils.GetNameString(indivA, false) + "]\r\n");
                fView.CompareOutput.AppendText("      [" + GKUtils.GetNameString(indivB, false) + "]\r\n\r\n");
            });
        }

        public void Match()
        {
            TreeMatchType type = GetTreeMatchType();

            fView.CompareOutput.Clear();
            var tree = fBase.Context.Tree;

            switch (type) {
                case TreeMatchType.tmtInternal:
                    AppHost.Instance.ExecuteWork((controller) => {
                        fProgressController = controller;
                        TreeTools.FindDuplicates(tree, tree, 90 /*min: 80-85*/, DuplicateFoundFunc, controller);
                        fProgressController = null;
                    });
                    break;

                case TreeMatchType.tmtExternal:
                    TreeTools.CompareTree(fBase.Context, fExternalFile, fView.CompareOutput);
                    break;

                case TreeMatchType.tmtAnalysis:
                    {
                        List<TreeTools.ULIndividual> uln = null;
                        AppHost.Instance.ExecuteWork((controller) => {
                            uln = TreeTools.GetUnlinkedNamesakes(fBase, controller);
                        });

                        fView.CompareOutput.AppendText("  " + LangMan.LS(LSID.SearchUnlinkedNamesakes) + ":\r\n");
                        if (uln != null && uln.Count > 0) {
                            foreach (TreeTools.ULIndividual indiv in uln) {
                                fView.CompareOutput.AppendText("    - [" + indiv.Family + "] " + GKUtils.GetNameString(indiv.IRec, false) + "\r\n");
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
            fView.SetTitle(LangMan.LS(LSID.TreeCompare));

            if (!AppHost.Instance.HasFeatureSupport(Feature.Mobile)) {
                GetControl<ITabPage>("pageTreeCompare").Text = LangMan.LS(LSID.TreeCompare);
                GetControl<IButton>("btnClose").Text = LangMan.LS(LSID.DlgClose);
            }
            GetControl<IGroupBox>("grpMatchType").Text = LangMan.LS(LSID.MatchType);
            GetControl<ILabel>("lblFile").Text = LangMan.LS(LSID.MIFile);
            GetControl<IButton>("btnFileChoose").Text = LangMan.LS(LSID.DlgSelect) + @"...";
            GetControl<IRadioButton>("radMatchInternal").Text = LangMan.LS(LSID.MatchInternal);
            GetControl<IRadioButton>("radMathExternal").Text = LangMan.LS(LSID.MathExternal);
            GetControl<IRadioButton>("radAnalysis").Text = LangMan.LS(LSID.Analyze);
            GetControl<IButton>("btnMatch").Text = LangMan.LS(LSID.Match);
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            GetControl<IButton>("btnClose").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Cancel);
        }
    }
}
