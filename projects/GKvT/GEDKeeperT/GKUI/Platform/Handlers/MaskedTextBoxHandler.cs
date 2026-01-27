/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore.Design.Controls;
using GKUI.Components;
using Terminal.Gui.Views;

namespace GKUI.Platform.Handlers
{
    public sealed class MaskedTextBoxHandler : BaseControlHandler<TextValidateField, MaskedTextBoxHandler>, ITextBox
    {
        public MaskedTextBoxHandler(TextValidateField control) : base(control)
        {
        }

        public string[] Lines
        {
            get { return UIHelper.Convert(Control.Text); }
            set { Control.Text = UIHelper.Convert(value); }
        }

        public bool ReadOnly
        {
            get { return false; }
            set { }
        }

        public string SelectedText
        {
            get { return Control.Text; }
            set { Control.Text = value; }
        }

        public string Text
        {
            get { return Control.Text; }
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
        }
    }
}
