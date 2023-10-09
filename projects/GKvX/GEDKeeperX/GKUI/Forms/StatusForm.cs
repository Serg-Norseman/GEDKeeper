/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2018-2023 by Sergey V. Zhdanovskih.
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

using GKCore.Design.Views;
using Xamarin.Forms;

namespace GKUI.Forms
{
    /// <summary>
    /// 
    /// </summary>
    public class StatusForm : CommonWindow, IStatusForm
    {
        public sealed class StatusLinesEx : IStatusLines
        {
            private readonly StatusForm fForm;

            public string this[int index]
            {
                get { return fForm.GetStatusLine(index); }
                set { fForm.SetStatusLine(index, value); }
            }

            internal StatusLinesEx(StatusForm form)
            {
                fForm = form;
            }
        }

        private readonly ContentView fContentRow;
        private readonly StackLayout fStatusBar;
        private readonly StatusLinesEx fStatusLines;

        public IStatusLines StatusLines
        {
            get { return fStatusLines; }
        }

        public new View Content
        {
            get { return fContentRow.Content; }
            set { fContentRow.Content = value; }
        }

        public StatusForm()
        {
            fStatusBar = new StackLayout() {
                HorizontalOptions = LayoutOptions.FillAndExpand,
                VerticalOptions = LayoutOptions.End,
                Orientation = StackOrientation.Horizontal
            };

            fContentRow = new ContentView() {
                HorizontalOptions = LayoutOptions.FillAndExpand,
                VerticalOptions = LayoutOptions.FillAndExpand
            };

            base.Content = new StackLayout() {
                HorizontalOptions = LayoutOptions.FillAndExpand,
                VerticalOptions = LayoutOptions.FillAndExpand,
                Orientation = StackOrientation.Vertical,
                Children = { fContentRow, fStatusBar }
            };

            fStatusLines = new StatusLinesEx(this);
        }

        protected string GetStatusLine(int index)
        {
            if (index < 0 || index >= fStatusBar.Children.Count) {
                return string.Empty;
            } else {
                return ((Label)fStatusBar.Children[index]).Text;
            }
        }

        protected void SetStatusLine(int index, string value)
        {
            Label panel;
            if (index < 0) {
                return;
            } else if (index >= fStatusBar.Children.Count) {
                while (index >= fStatusBar.Children.Count) {
                    panel = new Label();
                    fStatusBar.Children.Add(panel);
                }
            }
            panel = (Label)fStatusBar.Children[index];
            panel.Text = value;
        }
    }
}
