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

using GKCore.Design.Controls;
using GKUI.Components;
using Xamarin.Forms;

namespace GKUI.Platform
{
    public sealed class TextBoxHandler : BaseControlHandler<Entry, TextBoxHandler>, ITextBox
    {
        public TextBoxHandler(Entry control) : base(control)
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
            get { return /*UIHelper.Convert(Control.Text)*/ null; }
            set { } // TODO
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
            get {
                string selectedText = (Control.Text == null) ? string.Empty : Control.Text.Substring(Control.CursorPosition, Control.SelectionLength);
                return selectedText;
            }
            set { /*Control.SelectedText = value;*/ }
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

        private void SetBackColor()
        {
            //Control.BackgroundColor = (!Control.ReadOnly && Enabled) ? SystemColors.WindowBackground : SystemColors.Control;
        }

        public void Copy()
        {
            UIHelper.SetClipboardText(SelectedText);
        }

        public void SelectAll()
        {
            Control.CursorPosition = 0;
            Control.SelectionLength = Control.Text != null ? Control.Text.Length : 0;
        }
    }

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
            get { /*return UIHelper.Convert(Control.Text);*/ return null; }
            set { } // TODO
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
            get { return /*Control.SelectedText*/ null; }
            set { /*Control.SelectedText = value;*/ }
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

        private void SetBackColor()
        {
            //Control.BackgroundColor = (!Control.IsReadOnly && Enabled) ? SystemColors.WindowBackground : SystemColors.Control;
        }

        public void Copy()
        {
            UIHelper.SetClipboardText(SelectedText);
        }

        public void SelectAll()
        {
            //Control.CursorPosition = 0;
            //Control.SelectionLength = Control.Text != null ? Control.Text.Length : 0;
        }
    }

    /*public sealed class MaskedTextBoxHandler : BaseControlHandler<Entry, MaskedTextBoxHandler>, ITextBox
    {
        public MaskedTextBoxHandler(Entry control) : base(control)
        {
        }

        public new bool Enabled
        {
            get { return Control.IsEnabled; }
            set {
                Control.IsEnabled = value;
                //Control.BackgroundColor = (value) ? SystemColors.WindowBackground : SystemColors.Control;
            }
        }

        public string[] Lines
        {
            get { return UIHelper.Convert(Control.Text); }
            set {  } // TODO
        }

        public bool ReadOnly
        {
            get { return Control.ReadOnly; }
            set { Control.ReadOnly = value; }
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
            UIHelper.SetClipboardText(Control.SelectedText);
        }

        public void SelectAll()
        {
            Control.SelectAll();
        }
    }*/

    /*public sealed class NumericBoxHandler : BaseControlHandler<Stepper, NumericBoxHandler>, INumericBoxHandler
    {
        public NumericBoxHandler(Stepper control) : base(control)
        {
        }

        public bool ReadOnly
        {
            get { return Control.ReadOnly; }
            set { Control.ReadOnly = value; }
        }

        public string Text
        {
            get { return Value.ToString(); }
            set { } // TODO
        }

        public double Value
        {
            get { return Control.Value; }
            set { Control.Value = value; }
        }
    }*/

    /*public sealed class TreeViewHandler : BaseControlHandler<TreeView, TreeViewHandler>, ITreeViewHandler
    {
        private TreeItem fRootNode;

        public TreeViewHandler(TreeView control) : base(control)
        {
        }

        public ITVNode AddNode(ITVNode parent, string name, object tag)
        {
            var node = new GKTreeNode(name, tag);
            if (parent == null) {
                fRootNode.Children.Add(node);
            } else {
                ((GKTreeNode)parent).Children.Add(node);
            }
            return node;
        }

        public void BeginUpdate()
        {
            Control.DataStore = null;
            fRootNode = new TreeItem();
        }

        public void Clear()
        {
        }

        public void EndUpdate()
        {
            Control.DataStore = fRootNode;
            Control.RefreshData();
        }

        public void Expand(ITVNode node)
        {
            GKTreeNode treeNode = node as GKTreeNode;
            if (treeNode != null) {
                treeNode.Expanded = true;
            }
        }
    }*/

    /*public sealed class LogChartHandler : BaseControlHandler<LogChart, LogChartHandler>, ILogChart
    {
        public LogChartHandler(LogChart control) : base(control)
        {
        }

        public void AddFragment(int val)
        {
            Control.AddFragment(val);
        }

        public void Clear()
        {
            Control.Clear();
        }
    }*/

    /*public sealed class MenuItemHandler : ControlHandler<ButtonMenuItem, MenuItemHandler>, IMenuItem
    {
        public MenuItemHandler(ButtonMenuItem control) : base(control)
        {
        }

        public bool Checked
        {
            get { return false; }
            set { }
        }

        public bool Enabled
        {
            get { return Control.Enabled; }
            set { Control.Enabled = value; }
        }

        public object Tag
        {
            get { return Control.Tag; }
            set { Control.Tag = value; }
        }

        public int ItemsCount
        {
            get { return Control.Items.Count; }
        }

        public IMenuItem AddItem(string text, object tag, IImage image, ItemAction action)
        {
            var item = new MenuItemEx(text, tag, image, action);
            Control.Items.Add(item);
            return item;
        }

        public void ClearItems()
        {
            Control.Items.Clear();
        }
    }*/
}
