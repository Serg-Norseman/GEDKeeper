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
        #region Design components
#pragma warning disable CS0649, IDE0044
        private ITabPage _pageTreeCompare;
        private ITextBox _ListCompare;
        private IButton _btnClose;
        private IGroupBox _grpMatchType;
        private IRadioButton _radMatchInternal;
        private IRadioButton _radMathExternal;
        private IRadioButton _radAnalysis;
        private ILabel _lblFile;
        private ITextBox _txtCompareFile;
        private IButton _btnFileChoose;
        private IButton _btnMatch;
#pragma warning restore CS0649, IDE0044
        #endregion

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
            _txtCompareFile.Text = fileName;
        }

        private IProgressController fProgressController;

        private void DuplicateFoundFunc(GDMIndividualRecord indivA, GDMIndividualRecord indivB)
        {
            fProgressController.InvokeEx(() => {
                _ListCompare.AppendText("    * [" + GKUtils.GetNameString(indivA, false) + "]\r\n");
                _ListCompare.AppendText("      [" + GKUtils.GetNameString(indivB, false) + "]\r\n\r\n");
            });
        }

        public void Match()
        {
            TreeMatchType type = GetTreeMatchType();

            _ListCompare.Clear();
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
                    TreeTools.CompareTree(fBase.Context, fExternalFile, _ListCompare);
                    break;

                case TreeMatchType.tmtAnalysis:
                    {
                        List<TreeTools.ULIndividual> uln = null;
                        AppHost.Instance.ExecuteWork((controller) => {
                            uln = TreeTools.GetUnlinkedNamesakes(fBase, controller);
                        });

                        _ListCompare.AppendText("  " + LangMan.LS(LSID.SearchUnlinkedNamesakes) + ":\r\n");
                        if (uln != null && uln.Count > 0) {
                            foreach (TreeTools.ULIndividual indiv in uln) {
                                _ListCompare.AppendText("    - [" + indiv.Family + "] " + GKUtils.GetNameString(indiv.IRec, false) + "\r\n");
                            }
                        } else {
                            _ListCompare.AppendText("    - not found.");
                        }
                        break;
                    }
            }
        }

        public TreeMatchType GetTreeMatchType()
        {
            TreeMatchType type =
                ((_radMatchInternal.Checked) ?
                 TreeMatchType.tmtInternal :
                 ((_radMathExternal.Checked) ? TreeMatchType.tmtExternal : TreeMatchType.tmtAnalysis));

            return type;
        }

        public void ChangeTreeMatchType()
        {
            TreeMatchType type = GetTreeMatchType();

            _lblFile.Enabled = (type == TreeMatchType.tmtExternal);
            _txtCompareFile.Enabled = (type == TreeMatchType.tmtExternal);
            _btnFileChoose.Enabled = (type == TreeMatchType.tmtExternal);
        }

        public override void SetLocale()
        {
            fView.SetTitle(LangMan.LS(LSID.TreeCompare));

            if (AppHost.Instance.HasFeatureSupport(Feature.Graphics) && !AppHost.Instance.HasFeatureSupport(Feature.Mobile)) {
                _pageTreeCompare.Text = LangMan.LS(LSID.TreeCompare);
                _btnClose.Text = LangMan.LS(LSID.DlgClose);
            }

            _grpMatchType.Text = LangMan.LS(LSID.MatchType);
            _lblFile.Text = LangMan.LS(LSID.MIFile);
            _btnFileChoose.Text = LangMan.LS(LSID.DlgSelect) + @"...";
            _radMatchInternal.Text = LangMan.LS(LSID.MatchInternal);
            _radMathExternal.Text = LangMan.LS(LSID.MathExternal);
            _radAnalysis.Text = LangMan.LS(LSID.Analyze);
            _btnMatch.Text = LangMan.LS(LSID.Match);
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            _btnClose.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Cancel);
        }
    }
}
