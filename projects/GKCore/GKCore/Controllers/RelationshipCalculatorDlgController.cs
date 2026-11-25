/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GDModel;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Kinships;
using GKCore.Locales;
using GKCore.Options;
using GKUI.Themes;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public class RelationshipCalculatorDlgController : DialogController<IRelationshipCalculatorDlg>
    {
        #region Design components
#pragma warning disable CS0649, IDE0044
        private ILabel _lblKinship;
        private ITextBox _txtResult;
        private IButton _btnRec2Select;
        private IButton _btnRec1Select;
        private ITextBox _Edit2;
        private ITextBox _Edit1;
        private ILabel _Lab2;
        private ILabel _Lab1;
        private IButton _btnClose;
        private IButton _btnSwap;
#pragma warning restore CS0649, IDE0044
        #endregion

        private GDMIndividualRecord fRec1;
        private GDMIndividualRecord fRec2;
        private string fResult;

        public RelationshipCalculatorDlgController(IRelationshipCalculatorDlg view) : base(view)
        {
        }

        public async void SelectRec1()
        {
            GDMIndividualRecord iRec = await BaseController.SelectRecord(fView, fBase, GDMRecordType.rtIndividual, null) as GDMIndividualRecord;
            if (iRec != null) SetRec1(iRec);
        }

        public async void SelectRec2()
        {
            GDMIndividualRecord iRec = await BaseController.SelectRecord(fView, fBase, GDMRecordType.rtIndividual, null) as GDMIndividualRecord;
            if (iRec != null) SetRec2(iRec);
        }

        public void SetRec1(GDMIndividualRecord value)
        {
            fRec1 = value;
            Solve();
        }

        public void SetRec2(GDMIndividualRecord value)
        {
            fRec2 = value;
            Solve();
        }

        public void Swap()
        {
            GDMIndividualRecord fRecTmp = fRec1;
            fRec1 = fRec2;
            fRec2 = fRecTmp;
            Solve();
        }

        private void Solve()
        {
            fResult = "???";

            if (fRec1 != null && fRec2 != null) {
                using (KinshipsGraph kinsGraph = KinshipsGraph.SearchGraph(fBase.Context, fRec1)) {
                    if (kinsGraph.IsEmpty()) {
                        fResult = "Empty graph.";
                    } else if (kinsGraph.FindIndividual(fRec2.XRef) == null) {
                        fResult = "These individuals have no common relatives.";
                    } else {
                        kinsGraph.SetTreeRoot(fRec2);
                        fResult = kinsGraph.DetermineKinship(fRec1, true, GlobalOptions.Instance.ShortKinshipForm);
                    }
                }
            }

            UpdateView();
        }

        public override void UpdateView()
        {
            if (fRec1 == null) {
                _Lab1.Text = @"XXX1";
                _Edit1.Text = "";
            } else {
                _Lab1.Text = fRec1.XRef;
                _Edit1.Text = GKUtils.GetNameString(fRec1, true, false);
            }

            if (fRec2 == null) {
                _Lab2.Text = @"XXX2";
                _Edit2.Text = "";
            } else {
                _Lab2.Text = fRec2.XRef;
                _Edit2.Text = GKUtils.GetNameString(fRec2, true, false);
            }

            _txtResult.Text = fResult;
        }

        public override void SetLocale()
        {
            fView.SetTitle(LangMan.LS(LSID.RelationshipCalculator));

            _btnClose.Text = LangMan.LS(LSID.DlgClose);
            _btnRec1Select.Text = LangMan.LS(LSID.DlgSelect) + @"...";
            _btnRec2Select.Text = LangMan.LS(LSID.DlgSelect) + @"...";
            _lblKinship.Text = LangMan.LS(LSID.Kinship);
            _btnSwap.Text = LangMan.LS(LSID.Swap);
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            _btnClose.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Cancel);
        }
    }
}
