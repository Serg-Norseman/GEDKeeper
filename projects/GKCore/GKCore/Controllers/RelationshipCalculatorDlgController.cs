/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
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

using GDModel;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Kinships;
using GKCore.Options;
using GKCore.Types;
using GKUI.Themes;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public class RelationshipCalculatorDlgController : DialogController<IRelationshipCalculatorDlg>
    {
        private GDMIndividualRecord fRec1;
        private GDMIndividualRecord fRec2;
        private string fResult;

        public RelationshipCalculatorDlgController(IRelationshipCalculatorDlg view) : base(view)
        {
        }

        public async void SelectRec1()
        {
            GDMIndividualRecord iRec = await fBase.Context.SelectRecord(fView, GDMRecordType.rtIndividual, null) as GDMIndividualRecord;
            if (iRec != null) SetRec1(iRec);
        }

        public async void SelectRec2()
        {
            GDMIndividualRecord iRec = await fBase.Context.SelectRecord(fView, GDMRecordType.rtIndividual, null) as GDMIndividualRecord;
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
                    } else if (kinsGraph.FindVertex(fRec2.XRef) == null) {
                        fResult = "These individuals have no common relatives.";
                    } else {
                        kinsGraph.SetTreeRoot(fRec2);
                        fResult = kinsGraph.GetRelationship(fRec1, true, GlobalOptions.Instance.ShortKinshipForm);
                    }
                }
            }

            UpdateView();
        }

        public override void UpdateView()
        {
            if (fRec1 == null) {
                fView.Label1.Text = @"XXX1";
                fView.Person1.Text = "";
            } else {
                fView.Label1.Text = fRec1.XRef;
                fView.Person1.Text = GKUtils.GetNameString(fRec1, true, false);
            }

            if (fRec2 == null) {
                fView.Label2.Text = @"XXX2";
                fView.Person2.Text = "";
            } else {
                fView.Label2.Text = fRec2.XRef;
                fView.Person2.Text = GKUtils.GetNameString(fRec2, true, false);
            }

            fView.Result.Text = fResult;
        }

        public override void SetLocale()
        {
            fView.Title = LangMan.LS(LSID.RelationshipCalculator);

            GetControl<IButton>("btnClose").Text = LangMan.LS(LSID.DlgClose);
            GetControl<IButton>("btnRec1Select").Text = LangMan.LS(LSID.DlgSelect) + @"...";
            GetControl<IButton>("btnRec2Select").Text = LangMan.LS(LSID.DlgSelect) + @"...";
            GetControl<ILabel>("lblKinship").Text = LangMan.LS(LSID.Kinship);
            GetControl<IButton>("btnSwap").Text = LangMan.LS(LSID.Swap);
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            GetControl<IButton>("btnClose").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Cancel);
        }
    }
}
