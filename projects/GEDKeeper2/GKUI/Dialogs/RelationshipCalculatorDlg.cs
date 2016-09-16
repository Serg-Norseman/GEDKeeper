/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
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
using System.Windows.Forms;

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
    public partial class RelationshipCalculatorDlg : Form
    {
        private readonly IBaseWindow fBase;

        private GEDCOMIndividualRecord fRec1;
        private GEDCOMIndividualRecord fRec2;

        public RelationshipCalculatorDlg(IBaseWindow baseWin)
        {
            InitializeComponent();

            this.fBase = baseWin;

            this.btnClose.Image = global::GKResources.iBtnCancel;

            // SetLang()
            this.Text = LangMan.LS(LSID.LSID_RelationshipCalculator);
            this.btnClose.Text = LangMan.LS(LSID.LSID_DlgClose);
            this.btnRec1Select.Text = LangMan.LS(LSID.LSID_DlgSelect) + @"...";
            this.btnRec2Select.Text = LangMan.LS(LSID.LSID_DlgSelect) + @"...";
            this.lblKinship.Text = LangMan.LS(LSID.LSID_Kinship);

            this.SetRec1(null);
            this.SetRec2(null);
        }

        private void SetRec1(GEDCOMIndividualRecord value)
        {
            this.fRec1 = value;
            this.Solve();
        }

        private void SetRec2(GEDCOMIndividualRecord value)
        {
            this.fRec2 = value;
            this.Solve();
        }

        private void Solve()
        {
            if (this.fRec1 == null) {
                this.Lab1.Text = @"XXX1";
                this.Edit1.Text = "";
            } else {
                this.Lab1.Text = this.fRec1.XRef;
                GEDCOMIndividualRecord iRec = (GEDCOMIndividualRecord)this.fRec1;
                this.Edit1.Text = iRec.GetNameString(true, false);
            }

            if (this.fRec2 == null) {
                this.Lab2.Text = @"XXX2";
                this.Edit2.Text = "";
            } else {
                this.Lab2.Text = this.fRec2.XRef;
                GEDCOMIndividualRecord iRec = (GEDCOMIndividualRecord)this.fRec2;
                this.Edit2.Text = iRec.GetNameString(true, false);
            }

            if (this.fRec1 != null && this.fRec2 != null) {
                txtResult.Text = "???";

                using (KinshipsGraph kinsGraph = TreeTools.SearchKinshipsGraph(this.fRec1)) {
                    if (kinsGraph.IsEmpty()) {
                        txtResult.Text = "Empty graph.";
                        return;
                    }

                    if (kinsGraph.FindVertex(this.fRec2.XRef) == null) {
                        txtResult.Text = "These individuals have no common relatives.";
                        return;
                    }

                    kinsGraph.SetTreeRoot(this.fRec1);
                    txtResult.Text = kinsGraph.GetRelationship(this.fRec2);

                    #if DEBUG_SOLVE
                    txtResult.Text += "\r\n" + kinsGraph.IndividualsPath;
                    #endif
                }
            }
        }

        private void btnRec1Select_Click(object sender, EventArgs e)
        {
            this.SetRec1(this.fBase.SelectRecord(GEDCOMRecordType.rtIndividual, null) as GEDCOMIndividualRecord);
        }

        private void btnRec2Select_Click(object sender, EventArgs e)
        {
            this.SetRec2(this.fBase.SelectRecord(GEDCOMRecordType.rtIndividual, null) as GEDCOMIndividualRecord);
        }
    }
}
