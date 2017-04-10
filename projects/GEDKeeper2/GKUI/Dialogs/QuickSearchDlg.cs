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
            txtSearchPattern = new TextBox();
            btnPrev = new Button();
            btnNext = new Button();
            SuspendLayout();

            txtSearchPattern.Location = new System.Drawing.Point(3, 3);
            txtSearchPattern.Width = 150;
            txtSearchPattern.Height = 24;
            txtSearchPattern.Margin = new Padding(3, 3, 3, 0);
            txtSearchPattern.TextChanged += SearchPattern_TextChanged;
            txtSearchPattern.Name = "txtSearchPattern";
            
            btnPrev.Location = new System.Drawing.Point(156, 3);
            btnPrev.Margin = new Padding(3);
            btnPrev.Height = 24;
            btnPrev.Width = 24;
            btnPrev.Click += FindPrev_Click;
            btnPrev.Image = GKResources.iLeft1;
            btnPrev.Name = "btnPrev";
            
            btnNext.Location = new System.Drawing.Point(156+27, 3);
            btnNext.Margin = new Padding(3);
            btnNext.Height = 24;
            btnNext.Width = 24;
            btnNext.Click += FindNext_Click;
            btnNext.Image = GKResources.iRight1;
            btnNext.Name = "btnNext";
            
            AutoScaleBaseSize = new System.Drawing.Size(5, 14);
            ClientSize = new System.Drawing.Size(210, 30);
            Controls.Add(txtSearchPattern);
            Controls.Add(btnPrev);
            Controls.Add(btnNext);
            Font = new System.Drawing.Font("Tahoma", 8.25f, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, 204);
            FormBorderStyle = FormBorderStyle.FixedToolWindow;
            KeyPreview = true;
            KeyDown += SearchPanel_KeyDown;
            Name = "QuickSearchDlg";
            ShowInTaskbar = false;
            StartPosition = FormStartPosition.Manual;
            TopMost = true;

            ResumeLayout(false);
        }

        private void SearchPattern_TextChanged(object sender, EventArgs e)
        {
            fStrategy = new BaseSearchStrategy(fWorkWindow, txtSearchPattern.Text);
        }

        private void FindNext_Click(object sender, EventArgs e)
        {
            if (fStrategy == null) return;

            if (!fStrategy.HasResults()) {
                AppHub.StdDialogs.ShowError(LangMan.LS(LSID.LSID_NoMatchesFound));
                return;
            }

            ISearchResult result = fStrategy.FindNext();
            if (result != null) {
                SelectResult(result as SearchResult);
            }
        }

        private void FindPrev_Click(object sender, EventArgs e)
        {
            if (fStrategy == null) return;

            if (!fStrategy.HasResults()) {
                AppHub.StdDialogs.ShowError(LangMan.LS(LSID.LSID_NoMatchesFound));
                return;
            }

            ISearchResult result = fStrategy.FindPrev();
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
            Text = LangMan.LS(LSID.LSID_Search);
            //this.txtSearchPattern.Text = LangMan.LS(LSID.LSID_NoMatchesFound);
            //this.btnPrev.Text = LangMan.LS(LSID.LSID_FindPrevious);
            //this.btnNext.Text = LangMan.LS(LSID.LSID_FindNext);
        }
    }
}
