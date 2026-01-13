/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
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
