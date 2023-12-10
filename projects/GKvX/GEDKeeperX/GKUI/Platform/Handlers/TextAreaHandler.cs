/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih.
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
using GKCore;
using GKCore.Design.Controls;
using GKUI.Components;
using Xamarin.Forms;

namespace GKUI.Platform
{
    public sealed class TextAreaHandler : BaseControlHandler<Editor, TextAreaHandler>, ITextBox
    {
        public TextAreaHandler(Editor control) : base(control)
        {
        }

        public new bool Enabled
        {
            get { return Control.IsEnabled; }
            set {
                Control.IsEnabled = value;
                SetBackColor();
            }
        }

        public string[] Lines
        {
            get { return UIHelper.Convert(Control.Text); }
            set { Control.Text = UIHelper.Convert(value); }
        }

        public bool ReadOnly
        {
            get { return Control.IsReadOnly; }
            set {
                Control.IsReadOnly = value;
                SetBackColor();
            }
        }

        public string SelectedText
        {
            get { return /*Control.SelectedText*/ string.Empty; }
            set { /*Control.SelectedText = value;*/ }
        }

        public string Text
        {
            get { return Control.Text; }
            set { Control.Text = value; }
        }

        public void AppendText(string text)
        {
            string strResult = Control.Text;
            if (!string.IsNullOrEmpty(strResult)) {
                strResult += Environment.NewLine;
            }
            strResult += text;
            Control.Text = strResult;
        }

        public void Clear()
        {
            Control.Text = string.Empty;
        }

        private void SetBackColor()
        {
            //Control.BackgroundColor = (!Control.IsReadOnly && Enabled) ? SystemColors.WindowBackground : SystemColors.Control;
        }

        public void Copy()
        {
            AppHost.Instance.SetClipboardText(SelectedText);
        }

        public void SelectAll()
        {
            //Control.CursorPosition = 0;
            //Control.SelectionLength = Control.Text != null ? Control.Text.Length : 0;
        }
    }
}
