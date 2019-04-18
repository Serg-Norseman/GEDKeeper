/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2018 by Sergey V. Zhdanovskih.
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

using BSLib;
using GKCore.Interfaces;
using GKCore.MVP;
using GKCore.MVP.Controls;
using GKUI.Components;

namespace GKUI.Providers
{
    public abstract class BaseControlHandler<T, TThis> : ControlHandler<T, TThis>, IBaseControl
        where T : Control
        where TThis : ControlHandler<T, TThis>
    {
        protected BaseControlHandler(T control) : base(control)
        {
        }

        public bool Enabled
        {
            get { return Control.Enabled; }
            set { Control.Enabled = value; }
        }

        public void Activate()
        {
            Control.Focus();
        }
    }


    public sealed class LabelHandler : BaseControlHandler<Label, LabelHandler>, ILabelHandler
    {
        public LabelHandler(Label control) : base(control)
        {
        }

        public string Text
        {
            get { return Control.Text; }
            set { Control.Text = value; }
        }
    }

    public sealed class ButtonHandler : BaseControlHandler<Button, ButtonHandler>, IButtonHandler
    {
        public ButtonHandler(Button control) : base(control)
        {
        }

        public string Text
        {
            get { return Control.Text; }
            set { Control.Text = value; }
        }
    }

    public sealed class CheckBoxHandler : BaseControlHandler<CheckBox, CheckBoxHandler>, ICheckBoxHandler
    {
        public CheckBoxHandler(CheckBox control) : base(control)
        {
        }

        public bool Checked
        {
            get { return Control.Checked.GetValueOrDefault(); }
            set { Control.Checked = value; }
        }

        public string Text
        {
            get { return Control.Text; }
            set { Control.Text = value; }
        }
    }

    public sealed class RadioButtonHandler : BaseControlHandler<RadioButton, RadioButtonHandler>, IRadioButtonHandler
    {
        public RadioButtonHandler(RadioButton control) : base(control)
        {
        }

        public bool Checked
        {
            get { return Control.Checked; }
            set { Control.Checked = value; }
        }

        public string Text
        {
            get { return Control.Text; }
            set { Control.Text = value; }
        }
    }

    public sealed class ComboBoxHandler : BaseControlHandler<ComboBox, ComboBoxHandler>, IComboBoxHandler
    {
        public ComboBoxHandler(ComboBox control) : base(control)
        {
        }

        public new bool Enabled
        {
            get { return Control.Enabled; }
            set {
                Control.Enabled = value;
                //Control.BackgroundColor = (value) ? SystemColors.WindowBackground : SystemColors.Control;
            }
        }

        public bool ReadOnly
        {
            get { return Control.ReadOnly; }
            set { Control.ReadOnly = value; }
        }

        public int SelectedIndex
        {
            get { return Control.SelectedIndex; }
            set { Control.SelectedIndex = value; }
        }

        public object SelectedItem
        {
            get { return Control.SelectedValue; }
            set { Control.SelectedValue = value; }
        }

        public object SelectedTag
        {
            get {
                return ((GKComboItem)Control.SelectedValue).Tag;
            }
            set {
                var ctl = Control;
                foreach (object item in ctl.Items) {
                    GKComboItem comboItem = (GKComboItem)item;
                    if (comboItem.Tag == value) {
                        ctl.SelectedValue = item;
                        return;
                    }
                }
                ctl.SelectedIndex = 0;
            }
        }

        public string Text
        {
            get { return Control.Text; }
            set {
                Control.AutoComplete = true; // FIXME: Wrapper for EtoBug in ComboBox.setText
                Control.Text = value;
                Control.AutoComplete = false;
            }
        }

        public void Add(object item)
        {
            Control.Items.Add((string)item);
        }

        public void AddItem(string caption, object tag, IImage image = null)
        {
            Control.Items.Add(new GKComboItem(caption, tag, image));
        }

        public void AddRange(object[] items, bool sorted = false)
        {
            //Control.Sorted = false;
            Control.Items.AddRange(GKComboItem.Convert((string[])items));
            //Control.Sorted = sorted;
        }

        public void AddStrings(StringList strings)
        {
            int num = strings.Count;
            for (int i = 0; i < num; i++) {
                AddItem(strings[i], strings.GetObject(i));
            }
        }

        public void BeginUpdate()
        {
            //Control.BeginUpdate();
        }

        public void Clear()
        {
            Control.Items.Clear();
        }

        public void EndUpdate()
        {
            //Control.EndUpdate();
        }

        public void SortItems()
        {
            Control.SortItems();
        }
    }

    public sealed class TextBoxHandler : BaseControlHandler<TextBox, TextBoxHandler>, ITextBoxHandler
    {
        public TextBoxHandler(TextBox control) : base(control)
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
            Control.BackgroundColor = (!Control.ReadOnly && Enabled) ? SystemColors.WindowBackground : SystemColors.Control;
        }

        public void Copy()
        {
            UIHelper.SetClipboardText(Control.SelectedText);
        }

        public void SelectAll()
        {
            Control.SelectAll();
        }
    }

    public sealed class TextAreaHandler : BaseControlHandler<TextArea, TextAreaHandler>, ITextBoxHandler
    {
        public TextAreaHandler(TextArea control) : base(control)
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
            Control.BackgroundColor = (!Control.ReadOnly && Enabled) ? SystemColors.WindowBackground : SystemColors.Control;
        }

        public void Copy()
        {
            UIHelper.SetClipboardText(Control.SelectedText);
        }

        public void SelectAll()
        {
            Control.SelectAll();
        }
    }

    public sealed class MaskedTextBoxHandler : BaseControlHandler<MaskedTextBox, MaskedTextBoxHandler>, ITextBoxHandler
    {
        public MaskedTextBoxHandler(MaskedTextBox control) : base(control)
        {
        }

        public new bool Enabled
        {
            get { return Control.Enabled; }
            set {
                Control.Enabled = value;
                //Control.BackgroundColor = (value) ? SystemColors.WindowBackground : SystemColors.Control;
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
    }

    public sealed class DateBoxHandler : BaseControlHandler<GKDateBox, DateBoxHandler>, IDateBoxHandler
    {
        public DateBoxHandler(GKDateBox control) : base(control)
        {
        }

        public string NormalizeDate
        {
            get { return Control.NormalizeDate; }
            set { Control.NormalizeDate = value; }
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
    }

    public sealed class NumericBoxHandler : BaseControlHandler<NumericUpDown, NumericBoxHandler>, INumericBoxHandler
    {
        public NumericBoxHandler(NumericUpDown control) : base(control)
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
            set { /* TODO */ }
        }

        public double Value
        {
            get { return Control.Value; }
            set { Control.Value = value; }
        }
    }

    public sealed class TreeViewHandler : BaseControlHandler<TreeView, TreeViewHandler>, ITreeViewHandler
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

        public object GetSelectedData()
        {
            GKTreeNode node = Control.SelectedItem as GKTreeNode;
            return (node == null) ? null : node.Tag;
        }
    }

    public sealed class ProgressBarHandler : BaseControlHandler<ProgressBar, ProgressBarHandler>, IProgressBarHandler
    {
        public ProgressBarHandler(ProgressBar control) : base(control)
        {
        }

        public int Minimum
        {
            get { return Control.MinValue; }
            set { Control.MinValue = value; }
        }

        public int Maximum
        {
            get { return Control.MaxValue; }
            set { Control.MaxValue = value; }
        }

        public int Value
        {
            get { return Control.Value; }
            set { Control.Value = value; }
        }

        public void Increment(int value)
        {
            Control.Value += value;
        }
    }

    public sealed class LogChartHandler : BaseControlHandler<LogChart, LogChartHandler>, ILogChart
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
    }

    public sealed class TabControlHandler : BaseControlHandler<TabControl, TabControlHandler>, ITabControl
    {
        public TabControlHandler(TabControl control) : base(control)
        {
        }

        public int SelectedIndex
        {
            get { return Control.SelectedIndex; }
            set { Control.SelectedIndex = value; }
        }
    }

    public sealed class MenuItemHandler : ControlHandler<ButtonMenuItem, MenuItemHandler>, IMenuItem
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
    }
}
