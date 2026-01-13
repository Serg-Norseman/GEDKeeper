/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
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
