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

using System;
using System.Windows.Forms;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;

namespace GKUI.Dialogs
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class QuickSearchDlg : Form, ILocalization
    {
        private readonly IWorkWindow fWorkWindow;
        private ISearchStrategy fStrategy;

        private TextBox txtSearchPattern;
        private Button btnPrev;
        private Button btnNext;

        public QuickSearchDlg(IWorkWindow workWindow)
        {
            InitializeComponent();
            fWorkWindow = workWindow;
            SetLang();
        }
        
        private void InitializeComponent()
        {
            this.txtSearchPattern = new TextBox();
            this.btnPrev = new Button();
            this.btnNext = new Button();
            this.SuspendLayout();

            this.txtSearchPattern.Location = new System.Drawing.Point(3, 3);
            this.txtSearchPattern.Width = 150;
            this.txtSearchPattern.Height = 24;
            this.txtSearchPattern.Margin = new Padding(3, 3, 3, 0);
            this.txtSearchPattern.TextChanged += this.SearchPattern_TextChanged;
            this.txtSearchPattern.Name = "txtSearchPattern";
            
            this.btnPrev.Location = new System.Drawing.Point(156, 3);
            this.btnPrev.Margin = new Padding(3);
            this.btnPrev.Height = 24;
            this.btnPrev.Width = 24;
            this.btnPrev.Click += FindPrev_Click;
            this.btnPrev.Image = GKResources.iLeft1;
            this.btnPrev.Name = "btnPrev";
            
            this.btnNext.Location = new System.Drawing.Point(156+27, 3);
            this.btnNext.Margin = new Padding(3);
            this.btnNext.Height = 24;
            this.btnNext.Width = 24;
            this.btnNext.Click += FindNext_Click;
            this.btnNext.Image = GKResources.iRight1;
            this.btnNext.Name = "btnNext";
            
            this.AutoScaleBaseSize = new System.Drawing.Size(5, 14);
            this.ClientSize = new System.Drawing.Size(210, 30);
            this.Controls.Add(this.txtSearchPattern);
            this.Controls.Add(this.btnPrev);
            this.Controls.Add(this.btnNext);
            this.Font = new System.Drawing.Font("Tahoma", 8.25f, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, 204);
            this.FormBorderStyle = FormBorderStyle.FixedToolWindow;
            this.KeyPreview = true;
            this.KeyDown += this.SearchPanel_KeyDown;
            this.Name = "SearchPanel";
            this.ShowInTaskbar = false;
            this.StartPosition = FormStartPosition.Manual;
            this.TopMost = true;

            this.ResumeLayout(false);
        }

        private void SearchPattern_TextChanged(object sender, EventArgs e)
        {
            fStrategy = new BaseSearchStrategy(fWorkWindow, txtSearchPattern.Text);
        }

        private void FindNext_Click(object sender, EventArgs e)
        {
            if (this.fStrategy == null) return;

            if (!this.fStrategy.HasResults()) {
                GKUtils.ShowError(LangMan.LS(LSID.LSID_NoMatchesFound));
                return;
            }

            ISearchResult result = this.fStrategy.FindNext();
            if (result != null) {
                SelectResult(result as SearchResult);
            }
        }

        private void FindPrev_Click(object sender, EventArgs e)
        {
            if (this.fStrategy == null) return;

            if (!this.fStrategy.HasResults()) {
                GKUtils.ShowError(LangMan.LS(LSID.LSID_NoMatchesFound));
                return;
            }

            ISearchResult result = this.fStrategy.FindPrev();
            if (result != null) {
                SelectResult(result as SearchResult);
            }
        }
        
        private void SelectResult(SearchResult result)
        {
            if (result == null || result.Result == null) return;
            
            fWorkWindow.SelectByRec(result.Result as GEDCOMIndividualRecord);
        }
        
        private void SearchPanel_KeyDown(object sender, KeyEventArgs e)
        {
            switch (e.KeyCode) {
                case Keys.Enter:
                    e.Handled = true;
                    if (e.Shift)
                        FindPrev_Click(this, null);
                    else
                        FindNext_Click(this, null);
                    break;

                case Keys.Escape:
                    e.Handled = true;
                    Close();
                    break;
            }
        }

        public void SetLang()
        {
            this.Text = LangMan.LS(LSID.LSID_Search);
            //this.txtSearchPattern.Text = LangMan.LS(LSID.LSID_NoMatchesFound);
            //this.btnPrev.Text = LangMan.LS(LSID.LSID_FindPrevious);
            //this.btnNext.Text = LangMan.LS(LSID.LSID_FindNext);
        }
    }
}
