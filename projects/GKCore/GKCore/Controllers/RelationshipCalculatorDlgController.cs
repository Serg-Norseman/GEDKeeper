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

//#define DEBUG_SOLVE

using System;
using GKCommon.GEDCOM;
using GKCore.Kinships;
using GKCore.UIContracts;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public class RelationshipCalculatorDlgController : DialogController<IRelationshipCalculatorDlg>
    {
        private GEDCOMIndividualRecord fRec1;
        private GEDCOMIndividualRecord fRec2;

        public RelationshipCalculatorDlgController(IRelationshipCalculatorDlg view) : base(view)
        {
        }

        public override bool Accept()
        {
            try {
                return true;
            } catch (Exception ex) {
                Logger.LogWrite("RelationshipCalculatorDlgController.Accept(): " + ex.Message);
                return false;
            }
        }

        public override void UpdateView()
        {
        }

        public void SelectRec1()
        {
            GEDCOMIndividualRecord iRec = fBase.Context.SelectRecord(GEDCOMRecordType.rtIndividual, null) as GEDCOMIndividualRecord;
            if (iRec != null) SetRec1(iRec);
        }

        public void SelectRec2()
        {
            GEDCOMIndividualRecord iRec = fBase.Context.SelectRecord(GEDCOMRecordType.rtIndividual, null) as GEDCOMIndividualRecord;
            if (iRec != null) SetRec2(iRec);
        }

        public void SetRec1(GEDCOMIndividualRecord value)
        {
            fRec1 = value;
            Solve();
        }

        public void SetRec2(GEDCOMIndividualRecord value)
        {
            fRec2 = value;
            Solve();
        }

        private void Solve()
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

            if (fRec1 != null && fRec2 != null) {
                fView.Result.Text = "???";

                using (KinshipsGraph kinsGraph = KinshipsGraph.SearchGraph(fBase.Context, fRec1)) {
                    if (kinsGraph.IsEmpty()) {
                        fView.Result.Text = "Empty graph.";
                        return;
                    }

                    if (kinsGraph.FindVertex(fRec2.XRef) == null) {
                        fView.Result.Text = "These individuals have no common relatives.";
                        return;
                    }

                    kinsGraph.SetTreeRoot(fRec1);
                    fView.Result.Text = kinsGraph.GetRelationship(fRec2, true);

                    #if DEBUG_SOLVE
                    txtResult.Text += "\r\n" + kinsGraph.IndividualsPath;
                    #endif
                }
            }
        }
    }
}
