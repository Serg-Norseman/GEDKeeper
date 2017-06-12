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
using Eto.Forms;
using Eto.Drawing;

using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;
using GKUI.Components;

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

            txtSearchPattern.Width = 150;
            txtSearchPattern.Height = 24;
            txtSearchPattern.TextChanged += SearchPattern_TextChanged;

            btnPrev.Height = 24;
            btnPrev.Width = 24;
            btnPrev.Click += FindPrev_Click;
            btnPrev.Image = Bitmap.FromResource("Resources.btn_left.gif");

            btnNext.Height = 24;
            btnNext.Width = 24;
            btnNext.Click += FindNext_Click;
            btnNext.Image = Bitmap.FromResource("Resources.btn_right.gif");

            Content = new TableLayout {
                Padding = new Padding(10),
                Spacing = new Size(10, 10),
                Rows = {
                    new TableRow {
                        Cells = { txtSearchPattern, btnPrev, btnNext }
                    }
                }
            };

            ClientSize = new Size(210, 30);
            KeyDown += SearchPanel_KeyDown;
            ShowInTaskbar = false;
            Topmost = true;

            UIHelper.SetControlFont(this, "Tahoma", 8.25f);
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
            //btnPrev.Text = LangMan.LS(LSID.LSID_FindPrevious);
            //btnNext.Text = LangMan.LS(LSID.LSID_FindNext);
        }
    }
}
