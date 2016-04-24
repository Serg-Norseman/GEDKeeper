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

using System;
using System.Collections.Generic;
using System.Windows.Forms;

using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;
using GKUI.Charts;

namespace GKUI
{
    public partial class TfmAncestorsCircle : Form, IChartWindow
    {
        private readonly AncestorsCircle fAncestorsCircle;
        private readonly IBaseWindow fBase;

        public IBaseWindow Base
        {
            get { return this.fBase; }
        }

        public AncestorsCircleOptions Options
        {
            get { return this.fAncestorsCircle.Options; }
        }

        public TfmAncestorsCircle(IBaseWindow aBase, GEDCOMIndividualRecord startPerson)
        {
            this.InitializeComponent();
            this.MdiParent = TfmGEDKeeper.Instance;
            this.ShowInTaskbar = true;

            this.fBase = aBase;

            this.fAncestorsCircle = new AncestorsCircle(this.fBase.Tree);
            this.fAncestorsCircle.Dock = DockStyle.Fill;
            this.fAncestorsCircle.NavRefresh += Chart_NavRefresh;
            this.fAncestorsCircle.RootChanged += Chart_RootChanged;

            this.fAncestorsCircle.RootPerson = startPerson;

            this.Controls.Add(this.fAncestorsCircle);

            this.SetLang();
        }

        private void miOptions_Click(object sender, EventArgs e)
        {
            /*using (ACOptionsControl dlg = new ACOptionsControl()) {
				dlg.Options = this.fAncestorsCircle.Options;

				if (dlg.ShowDialog() == DialogResult.OK) {
					this.fAncestorsCircle.Invalidate();
				}
			}*/
        }

        private void Chart_NavRefresh(object sender, EventArgs e)
        {
            TfmGEDKeeper.Instance.UpdateControls(false);
        }

        private void Chart_RootChanged(object sender, GEDCOMIndividualRecord person)
        {
            TfmGEDKeeper.Instance.UpdateControls(false);
        }

        private void TfmAncestors_KeyDown(object sender, KeyEventArgs e)
        {
            switch (e.KeyCode)
            {
                case Keys.Escape:
                    base.Close();
                    break;
            }
        }

        #region ILocalization implementation
        
        public void SetLang()
        {
            this.Text = LangMan.LS(LSID.LSID_AncestorsCircle);
        }

        #endregion

        #region IChartWindow implementation
        
        public void GenChart(bool show)
        {
            if (show) base.Show();

            TfmGEDKeeper.Instance.UpdateControls(false);
        }

        public bool AllowPrint()
        {
            return false;
        }

        public void DoPrint()
        {
            // dummy
        }

        public void DoPrintPreview()
        {
            // dummy
        }

        #endregion

        #region IWorkWindow implementation

        public string GetStatusString()
        {
            return string.Format(LangMan.LS(LSID.LSID_TreeIndividualsCount), fAncestorsCircle.IndividualsCount.ToString());
        }

        public bool NavCanBackward()
        {
            return this.fAncestorsCircle.NavCanBackward();
        }

        public bool NavCanForward()
        {
            return this.fAncestorsCircle.NavCanForward();
        }

        public void NavNext()
        {
            this.fAncestorsCircle.NavNext();
        }

        public void NavPrev()
        {
            this.fAncestorsCircle.NavPrev();
        }

        public bool AllowQuickFind()
        {
            return false;
        }

        public IList<ISearchResult> FindAll(string searchPattern)
        {
            return null;
        }

        public void QuickFind()
        {
        }

        public void SelectByRec(GEDCOMIndividualRecord iRec)
        {
        }

        public bool AllowFilter()
        {
            return false;
        }

        public void SetFilter()
        {
        }

        #endregion
    }
}
