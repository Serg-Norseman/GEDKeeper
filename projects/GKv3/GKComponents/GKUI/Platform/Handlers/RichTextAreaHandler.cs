/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
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

using Eto.Drawing;
using Eto.Forms;
using GKCore;
using GKCore.Design.Controls;
using GKUI.Components;

namespace GKUI.Platform.Handlers
{
    public sealed class RichTextAreaHandler : BaseControlHandler<RichTextArea, RichTextAreaHandler>, ITextBox
    {
        public RichTextAreaHandler(RichTextArea control) : base(control)
        {
        }

        public new bool Enabled
        {
            get { return Control.Enabled; }
            set {
                Control.Enabled = value;
                SetBackColor();
            }
        }

        public string[] Lines
        {
            get { return UIHelper.Convert(Control.Text); }
            set { /* TODO! */ }
        }

        public bool ReadOnly
        {
            get { return Control.ReadOnly; }
            set {
                Control.ReadOnly = value;
                SetBackColor();
            }
        }

        public string SelectedText
        {
            get { return Control.SelectedText; }
            set { Control.SelectedText = value; }
        }

        public string Text
        {
            get { return Control.Text; }
            set { Control.Text = value; }
        }

        public bool Visible
        {
            get { return Control.Visible; }
            set { Control.Visible = value; }
        }

        public void AppendText(string text)
        {
            Control.Append(text, true);
        }

        public void Clear()
        {
            Control.Text = string.Empty;
        }

        private void SetBackColor()
        {
            Control.BackgroundColor = !Control.ReadOnly && Enabled ? SystemColors.WindowBackground : SystemColors.Control;
        }

        public void Copy()
        {
            AppHost.Instance.SetClipboardText(Control.SelectedText);
        }

        public void SelectAll()
        {
            Control.SelectAll();
        }
    }
}
