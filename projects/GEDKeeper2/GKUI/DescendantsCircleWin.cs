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
    public partial class DescendantsCircleWin : Form, IChartWindow
    {
        private readonly DescendantsCircle fDescendantsCircle;
        private readonly IBaseWindow fBase;

        public IBaseWindow Base
        {
            get { return this.fBase; }
        }

        public DescendantsCircleWin(IBaseWindow aBase, GEDCOMIndividualRecord startPerson)
        {
            this.InitializeComponent();
            this.MdiParent = MainWin.Instance;
            this.ShowInTaskbar = true;

            this.fBase = aBase;

            this.fDescendantsCircle = new DescendantsCircle(this.fBase);
            this.fDescendantsCircle.Dock = DockStyle.Fill;
            this.fDescendantsCircle.NavRefresh += Chart_NavRefresh;
            this.fDescendantsCircle.RootChanged += Chart_RootChanged;
            this.fDescendantsCircle.RootPerson = startPerson;

            this.Controls.Add(this.fDescendantsCircle);

            this.SetLang();
        }

        private void Chart_NavRefresh(object sender, EventArgs e)
        {
            MainWin.Instance.UpdateControls(false);
        }

        private void Chart_RootChanged(object sender, GEDCOMIndividualRecord person)
        {
            MainWin.Instance.UpdateControls(false);
        }

        private void DescendantsCircleWin_KeyDown(object sender, KeyEventArgs e)
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
            this.Text = LangMan.LS(LSID.LSID_DescendantsCircle);
        }

        #endregion

        #region IChartWindow implementation
        
        public void GenChart(bool show)
        {
            if (show) base.Show();

            MainWin.Instance.UpdateControls(false);
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
            return string.Format(LangMan.LS(LSID.LSID_TreeIndividualsCount), fDescendantsCircle.IndividualsCount.ToString());
        }

        public void UpdateView()
        {
            this.fDescendantsCircle.Options.Assign(MainWin.Instance.Options.AncestorsCircleOptions);
            this.fDescendantsCircle.Changed();
        }

        public bool NavCanBackward()
        {
            return this.fDescendantsCircle.NavCanBackward();
        }

        public bool NavCanForward()
        {
            return this.fDescendantsCircle.NavCanForward();
        }

        public void NavNext()
        {
            this.fDescendantsCircle.NavNext();
        }

        public void NavPrev()
        {
            this.fDescendantsCircle.NavPrev();
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
