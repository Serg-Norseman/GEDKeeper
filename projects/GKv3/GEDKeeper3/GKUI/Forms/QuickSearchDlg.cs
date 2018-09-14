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

using System;
using Eto.Drawing;
using Eto.Forms;

using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;
using GKUI.Components;

namespace GKUI.Forms
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class QuickSearchDlg : CommonForm, ILocalization
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
            SuspendLayout();

            txtSearchPattern = new TextBox();
            //txtSearchPattern.Width = 150;
            //txtSearchPattern.Height = 24;
            txtSearchPattern.TextChanged += SearchPattern_TextChanged;

            btnPrev = new Button();
            btnPrev.Size = new Size(26, 26);
            btnPrev.Click += FindPrev_Click;
            btnPrev.Image = UIHelper.LoadResourceImage("Resources.btn_left.gif");

            btnNext = new Button();
            btnNext.Size = new Size(26, 26);
            btnNext.Click += FindNext_Click;
            btnNext.Image = UIHelper.LoadResourceImage("Resources.btn_right.gif");

            Content = TableLayout.Horizontal(10, new TableCell(txtSearchPattern, true), btnPrev, btnNext);

            KeyDown += SearchPanel_KeyDown;
            Maximizable = false;
            Minimizable = false;
            Resizable = false;
            ShowInTaskbar = false;
            Topmost = true;

            UIHelper.SetPredefProperties(this, 210, 30);
            ResumeLayout();
        }

        private void SearchPattern_TextChanged(object sender, EventArgs e)
        {
            fStrategy = new SearchStrategy(fWorkWindow, txtSearchPattern.Text);
        }

        private void FindNext_Click(object sender, EventArgs e)
        {
            if (fStrategy == null) return;

            if (!fStrategy.HasResults()) {
                AppHost.StdDialogs.ShowError(LangMan.LS(LSID.LSID_NoMatchesFound));
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
                AppHost.StdDialogs.ShowError(LangMan.LS(LSID.LSID_NoMatchesFound));
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
            switch (e.Key) {
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
            Title = LangMan.LS(LSID.LSID_Search);
            //txtSearchPattern.Text = LangMan.LS(LSID.LSID_NoMatchesFound);
            SetToolTip(btnPrev, LangMan.LS(LSID.LSID_FindPrevious));
            SetToolTip(btnNext, LangMan.LS(LSID.LSID_FindNext));
        }
    }
}
