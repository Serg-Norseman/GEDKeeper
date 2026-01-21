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
    public sealed class TextAreaHandler : BaseControlHandler<TextView, TextAreaHandler>, ITextBox
    {
        public TextAreaHandler(TextView control) : base(control)
        {
        }

        public string[] Lines
        {
            get { return /*UIHelper.Convert(Control.Text)*/ new string[0]; }
            set { }
        }

        public bool ReadOnly
        {
            get { return Control.ReadOnly; }
            set { Control.ReadOnly = value; }
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
