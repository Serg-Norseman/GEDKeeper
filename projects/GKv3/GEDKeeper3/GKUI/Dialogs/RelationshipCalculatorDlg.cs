/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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
using Eto.Drawing;
using Eto.Forms;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;
using GKCore.Kinships;
using GKCore.Tools;

namespace GKUI.Dialogs
{
    /// <summary>
    /// 
    /// </summary>
    public sealed partial class RelationshipCalculatorDlg : Dialog
    {
        private readonly IBaseWindow fBase;

        private GEDCOMIndividualRecord fRec1;
        private GEDCOMIndividualRecord fRec2;

        public RelationshipCalculatorDlg(IBaseWindow baseWin)
        {
            InitializeComponent();

            fBase = baseWin;

            btnClose.Image = Bitmap.FromResource("Resources.btn_cancel.gif");

            // SetLang()
            Title = LangMan.LS(LSID.LSID_RelationshipCalculator);
            btnClose.Text = LangMan.LS(LSID.LSID_DlgClose);
            btnRec1Select.Text = LangMan.LS(LSID.LSID_DlgSelect) + @"...";
            btnRec2Select.Text = LangMan.LS(LSID.LSID_DlgSelect) + @"...";
            lblKinship.Text = LangMan.LS(LSID.LSID_Kinship);

            SetRec1(null);
            SetRec2(null);
        }

        private void SetRec1(GEDCOMIndividualRecord value)
        {
            fRec1 = value;
            Solve();
        }

        private void SetRec2(GEDCOMIndividualRecord value)
        {
            fRec2 = value;
            Solve();
        }

        private void Solve()
        {
            if (fRec1 == null) {
                Lab1.Text = @"XXX1";
                Edit1.Text = "";
            } else {
                Lab1.Text = fRec1.XRef;
                Edit1.Text = GKUtils.GetNameString(fRec1, true, false);
            }

            if (fRec2 == null) {
                Lab2.Text = @"XXX2";
                Edit2.Text = "";
            } else {
                Lab2.Text = fRec2.XRef;
                Edit2.Text = GKUtils.GetNameString(fRec2, true, false);
            }

            if (fRec1 != null && fRec2 != null) {
                txtResult.Text = "???";

                using (KinshipsGraph kinsGraph = TreeTools.SearchKinshipsGraph(fBase.Context, fRec1)) {
                    if (kinsGraph.IsEmpty()) {
                        txtResult.Text = "Empty graph.";
                        return;
                    }

                    if (kinsGraph.FindVertex(fRec2.XRef) == null) {
                        txtResult.Text = "These individuals have no common relatives.";
                        return;
                    }

                    kinsGraph.SetTreeRoot(fRec1);
                    txtResult.Text = kinsGraph.GetRelationship(fRec2, true);

                    #if DEBUG_SOLVE
                    txtResult.Text += "\r\n" + kinsGraph.IndividualsPath;
                    #endif
                }
            }
        }

        private void btnRec1Select_Click(object sender, EventArgs e)
        {
            GEDCOMIndividualRecord iRec = fBase.Context.SelectRecord(GEDCOMRecordType.rtIndividual, null) as GEDCOMIndividualRecord;
            if (iRec != null) SetRec1(iRec);
        }

        private void btnRec2Select_Click(object sender, EventArgs e)
        {
            GEDCOMIndividualRecord iRec = fBase.Context.SelectRecord(GEDCOMRecordType.rtIndividual, null) as GEDCOMIndividualRecord;
            if (iRec != null) SetRec2(iRec);
        }
    }
}
