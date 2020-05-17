/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2020 by Sergey V. Zhdanovskih.
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
using System.Collections;
using System.Collections.Generic;
using System.Windows.Forms;
using BSLib;
using BSLib.Design.Graphics;
using BSLib.Design.MVP;
using BSLib.Design.MVP.Controls;
using GKCore.MVP.Controls;
using GKUI.Components;

namespace GKUI.Components
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
            Control.Select();
        }
    }


    public sealed class LabelHandler : BaseControlHandler<Label, LabelHandler>, ILabel
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

    public sealed class ButtonHandler : BaseControlHandler<Button, ButtonHandler>, IButton
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

    public sealed class CheckBoxHandler : BaseControlHandler<CheckBox, CheckBoxHandler>, ICheckBox
    {
        public CheckBoxHandler(CheckBox control) : base(control)
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

    public sealed class RadioButtonHandler : BaseControlHandler<RadioButton, RadioButtonHandler>, IRadioButton
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

    public sealed class ComboBoxHandler : BaseControlHandler<ComboBox, ComboBoxHandler>, IComboBoxEx
    {
        public ComboBoxHandler(ComboBox control) : base(control)
        {
        }

        public IList Items
        {
            get { return Control.Items; }
        }

        public bool ReadOnly
        {
            get { return (Control.DropDownStyle == ComboBoxStyle.DropDownList); }
            set { Control.DropDownStyle = (value) ? ComboBoxStyle.DropDownList : ComboBoxStyle.DropDown; }
        }

        public int SelectedIndex
        {
            get { return Control.SelectedIndex; }
            set { Control.SelectedIndex = value; }
        }

        public object SelectedItem
        {
            get { return Control.SelectedItem; }
            set { Control.SelectedItem = value; }
        }

        public string Text
        {
            get { return Control.Text; }
            set { Control.Text = value; }
        }

        public void Add(object item)
        {
            Control.Items.Add(item);
        }

        public void AddItem(string caption, object tag)
        {
            Control.Items.Add(new GKComboItem(caption, tag));
        }

        public void AddItem(string caption, object tag, IImage image)
        {
            Control.Items.Add(new GKComboItem(caption, tag, image));
        }

        public void AddItem<T>(string caption, T tag)
        {
            Control.Items.Add(new GKComboItem(caption, tag));
        }

        public void AddRange(object[] items, bool sorted = false)
        {
            Control.Sorted = false;
            Control.Items.AddRange(items);
            Control.Sorted = sorted;
        }

        public void AddRange(IList<string> items, bool sorted = false)
        {
            Control.Items.Clear();
            Control.Sorted = false;
            int num = items.Count;
            for (int i = 0; i < num; i++) {
                Control.Items.Add(items[i]);
            }
            Control.Sorted = sorted;
        }

        public void AddRange(IEnumerable<object> items, bool sorted = false)
        {
            Control.Items.Clear();
            Control.Sorted = false;
            foreach (object item in items) {
                Control.Items.Add(item);
            }
            Control.Sorted = sorted;
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
            Control.BeginUpdate();
        }

        public void Clear()
        {
            Control.Items.Clear();
        }

        public void EndUpdate()
        {
            Control.EndUpdate();
        }

        public void Sort()
        {
        }

        public T GetSelectedTag<T>()
        {
            return UIHelper.GetSelectedTag<T>(Control);
        }

        public void SetSelectedTag<T>(T tagValue)
        {
            UIHelper.SetSelectedTag<T>(Control, tagValue);
        }
    }

    public sealed class ToolStripComboBoxHandler : ControlHandler<ToolStripComboBox, ToolStripComboBoxHandler>, IComboBoxEx
    {
        public ToolStripComboBoxHandler(ToolStripComboBox control) : base(control)
        {
        }

        public bool Enabled
        {
            get { return Control.Enabled; }
            set { Control.Enabled = value; }
        }

        public IList Items
        {
            get { return Control.Items; }
        }

        public bool ReadOnly
        {
            get { return (Control.DropDownStyle == ComboBoxStyle.DropDownList); }
            set { Control.DropDownStyle = (value) ? ComboBoxStyle.DropDownList : ComboBoxStyle.DropDown; }
        }

        public int SelectedIndex
        {
            get { return Control.SelectedIndex; }
            set { Control.SelectedIndex = value; }
        }

        public object SelectedItem
        {
            get { return Control.SelectedItem; }
            set { Control.SelectedItem = value; }
        }

        public string Text
        {
            get { return Control.Text; }
            set { Control.Text = value; }
        }

        public void Add(object item)
        {
            Control.Items.Add(item);
        }

        public void AddItem(string caption, object tag)
        {
            Control.Items.Add(new GKComboItem(caption, tag));
        }

        public void AddItem(string caption, object tag, IImage image)
        {
            Control.Items.Add(new GKComboItem(caption, tag, image));
        }

        public void AddItem<T>(string caption, T tag)
        {
            Control.Items.Add(new GKComboItem(caption, tag));
        }

        public void AddRange(object[] items, bool sorted = false)
        {
            Control.Sorted = false;
            Control.Items.AddRange(items);
            Control.Sorted = sorted;
        }

        public void AddRange(IEnumerable<object> items, bool sorted = false)
        {
            Control.Items.Clear();
            Control.Sorted = false;
            foreach (object item in items) {
                Control.Items.Add(item);
            }
            Control.Sorted = sorted;
        }

        public void AddRange(IList<string> items, bool sorted = false)
        {
            Control.Items.Clear();
            Control.Sorted = false;
            int num = items.Count;
            for (int i = 0; i < num; i++) {
                Control.Items.Add(items[i]);
            }
            Control.Sorted = sorted;
        }

        public void AddStrings(StringList strings)
        {
            int num = strings.Count;
            for (int i = 0; i < num; i++) {
                Control.Items.Add(strings[i]);
            }
        }

        public void BeginUpdate()
        {
            Control.BeginUpdate();
        }

        public void Clear()
        {
            Control.Items.Clear();
        }

        public void EndUpdate()
        {
            Control.EndUpdate();
        }

        public void Activate()
        {
            Control.Select();
        }

        public void Sort()
        {
        }

        public T GetSelectedTag<T>()
        {
            object selectedItem = Control.SelectedItem;
            GKComboItem comboItem = (GKComboItem)selectedItem;
            T itemTag = (T)comboItem.Tag;
            return itemTag;
        }

        public void SetSelectedTag<T>(T tagValue)
        {
            var ctl = Control;
            foreach (object item in ctl.Items) {
                GKComboItem comboItem = (GKComboItem)item;
                T itemTag = (T)comboItem.Tag;

                if (tagValue.Equals(itemTag)) {
                    ctl.SelectedItem = item;
                    return;
                }
            }
            ctl.SelectedIndex = 0;
        }
    }

    public sealed class TextBoxHandler : BaseControlHandler<TextBox, TextBoxHandler>, ITextBox
    {
        public TextBoxHandler(TextBox control) : base(control)
        {
        }

        public string[] Lines
        {
            get { return Control.Lines; }
            set { Control.Lines = value; }
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
            Control.AppendText(text);
        }

        public void Clear()
        {
            Control.Clear();
        }

        public void Copy()
        {
            Control.Copy();
        }

        public void SelectAll()
        {
            Control.SelectAll();
        }
    }

    public sealed class MaskedTextBoxHandler : BaseControlHandler<MaskedTextBox, MaskedTextBoxHandler>, ITextBox
    {
        public MaskedTextBoxHandler(MaskedTextBox control) : base(control)
        {
        }

        public string[] Lines
        {
            get { return Control.Lines; }
            set { Control.Lines = value; }
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
            Control.AppendText(text);
        }

        public void Clear()
        {
            Control.Clear();
        }

        public void Copy()
        {
            Control.Copy();
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
            Control.Clear();
        }

        public void Copy()
        {
            Control.Copy();
        }

        public void SelectAll()
        {
            Control.SelectAll();
        }
    }

    public sealed class NumericBoxHandler : BaseControlHandler<NumericUpDown, NumericBoxHandler>, INumericBox
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
            get { return Control.Text; }
            set { Control.Text = value; }
        }

        public double Value
        {
            get { return decimal.ToDouble(Control.Value); }
            set { Control.Value = (decimal)value; }
        }
    }

    public sealed class TreeViewHandler : BaseControlHandler<TreeView, TreeViewHandler>, ITreeView
    {
        public TreeViewHandler(TreeView control) : base(control)
        {
        }

        public ITVNode AddNode(ITVNode parent, string name, object tag)
        {
            var node = new GKTreeNode(name, tag);
            if (parent == null) {
                Control.Nodes.Add(node);
            } else {
                ((GKTreeNode)parent).Nodes.Add(node);
            }
            return node;
        }

        public void BeginUpdate()
        {
            Control.BeginUpdate();
        }

        public void Clear()
        {
            Control.Nodes.Clear();
        }

        public void EndUpdate()
        {
            Control.EndUpdate();
        }

        public void Expand(ITVNode node)
        {
            TreeNode treeNode = node as TreeNode;
            if (treeNode != null) {
                treeNode.ExpandAll();
            }
        }

        public object GetSelectedData()
        {
            GKTreeNode node = Control.SelectedNode as GKTreeNode;
            return (node == null) ? null : node.Tag;
        }
    }

    public sealed class ProgressBarHandler : BaseControlHandler<ProgressBar, ProgressBarHandler>, IProgressBar
    {
        public ProgressBarHandler(ProgressBar control) : base(control)
        {
        }

        public int Minimum
        {
            get { return Control.Minimum; }
            set { Control.Minimum = value; }
        }

        public int Maximum
        {
            get { return Control.Maximum; }
            set { Control.Maximum = value; }
        }

        public int Value
        {
            get { return Control.Value; }
            set { Control.Value = value; }
        }

        public void Increment(int value)
        {
            Control.Increment(value);
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

    public sealed class MenuItemHandler : ControlHandler<ToolStripMenuItem, MenuItemHandler>, IMenuItem
    {
        public MenuItemHandler(ToolStripMenuItem control) : base(control)
        {
        }

        public bool Checked
        {
            get { return Control.Checked; }
            set { Control.Checked = value; }
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
            get { return Control.DropDownItems.Count; }
        }

        public IMenuItem AddItem(string text, object tag, IImage image, ItemAction action)
        {
            var item = new MenuItemEx(text, tag, image, action);
            Control.DropDownItems.Add(item);
            return item;
        }

        public void ClearItems()
        {
            Control.DropDownItems.Clear();
        }
    }
}
