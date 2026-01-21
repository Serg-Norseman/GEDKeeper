/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore.Design.Controls;
using Terminal.Gui;

namespace GKUI.Platform.Handlers
{
    public sealed class TextBoxHandler : BaseControlHandler<TextField, TextBoxHandler>, ITextBox
    {
        public TextBoxHandler(TextField control) : base(control)
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
            get { return new string[] { Control.Text.ToString() }; /*return UIHelper.Convert(Control.Text)*/; }
            set { }
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
            get { return Control.SelectedText.ToString(); }
            set { }
        }

        public string Text
        {
            get { return Control.Text.ToString(); }
            set { Control.Text = value; }
        }

        public void AppendText(string text)
        {
            //Control.Append(text, true);
        }

        public void Clear()
        {
            Control.Text = string.Empty;
        }

        private void SetBackColor()
        {
            //Control.BackgroundColor = (!Control.ReadOnly && Enabled) ? SystemColors.WindowBackground : SystemColors.Control;
        }

        public void Copy()
        {
            //UIHelper.SetClipboardText(Control.SelectedText);
        }

        public void SelectAll()
        {
            Control.SelectAll();
        }
    }
}
