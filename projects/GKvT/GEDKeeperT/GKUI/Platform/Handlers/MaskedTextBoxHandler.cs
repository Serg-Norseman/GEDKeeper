/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore;
using GKCore.Design.Controls;
using GKUI.Components;
using Terminal.Gui;

namespace GKUI.Platform.Handlers
{
    public sealed class MaskedTextBoxHandler : BaseControlHandler<TextValidateField, MaskedTextBoxHandler>, ITextBox
    {
        public MaskedTextBoxHandler(TextValidateField control) : base(control)
        {
        }

        public new bool Enabled
        {
            get { return Control.Enabled; }
            set { Control.Enabled = value; }
        }

        public string[] Lines
        {
            get { return UIHelper.Convert(Control.Text.ToString()); }
            set { Control.Text = UIHelper.Convert(value); }
        }

        public bool ReadOnly
        {
            get { return Control.ReadOnly; }
            set { Control.ReadOnly = value; }
        }

        public string SelectedText
        {
            get { return Control.Text.ToString(); }
            set { Control.Text = value; }
        }

        public string Text
        {
            get { return Control.Text.ToString(); }
            set { Control.Text = value; }
        }

        public void AppendText(string text)
        {
        }

        public void Clear()
        {
            Control.Text = string.Empty;
        }

        public void Copy()
        {
            AppHost.Instance.SetClipboardText(Text);
        }

        public void SelectAll()
        {
        }
    }
}
