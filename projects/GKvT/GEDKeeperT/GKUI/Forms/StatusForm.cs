/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore.Design.Views;
using Terminal.Gui;

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

        private readonly StatusBar fStatusBar;
        private readonly StatusLinesEx fStatusLines;

        public IStatusLines StatusLines
        {
            get { return fStatusLines; }
        }

        public StatusForm()
        {
            fStatusBar = new StatusBar() {
                Visible = true,
                Items = new StatusItem[] {
                    new StatusItem(Key.Null, "test status", null)
                }
            };
            this.StatusBar = fStatusBar;

            fStatusLines = new StatusLinesEx(this);
        }

        protected string GetStatusLine(int index)
        {
            return string.Empty;
        }

        protected void SetStatusLine(int index, string value)
        {
        }
    }
}
