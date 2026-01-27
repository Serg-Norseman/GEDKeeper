/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Linq;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Graphics;
using GKUI.Components;
using Terminal.Gui;
using Terminal.Gui.Views;

namespace GKUI.Platform.Handlers
{
    /*public sealed class RichTextAreaHandler : BaseControlHandler<RichTextArea, RichTextAreaHandler>, ITextBox
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
            set {  }
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
    }*/

    /*public sealed class DateBoxHandler : BaseControlHandler<GKDateBox, DateBoxHandler>, IDateBox
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
    }*/

    // NumericUpDown<T> does not exist for v1!
    /*public sealed class NumericBoxHandler : BaseControlHandler<NumericStepper, NumericBoxHandler>, INumericBox
    {
        public NumericBoxHandler(NumericStepper control) : base(control)
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
            set {  }
        }

        public double Value
        {
            get { return Control.Value; }
            set { Control.Value = value; }
        }
    }*/

    /*public sealed class TreeViewHandler : BaseControlHandler<TreeView, TreeViewHandler>, ITreeView
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
    }*/


    /*public sealed class DateControlHandler : BaseControlHandler<GKDateControl, DateControlHandler>, IDateControl
    {
        public GDMCustomDate Date
        {
            get { return Control.Date; }
            set { Control.Date = value; }
        }

        public DateControlHandler(GKDateControl control) : base(control)
        {
        }
    }


    public sealed class ListViewHandler : BaseControlHandler<GKListView, ListViewHandler>, IListView
    {
        public ListViewHandler(GKListView control) : base(control)
        {
        }

        public IListViewItems Items
        {
            get { return ((IListView)Control).Items; }
        }

        public int SelectedIndex
        {
            get { return Control.SelectedIndex; }
            set { Control.SelectedIndex = value; }
        }

        public int SortColumn
        {
            get { return Control.SortColumn; }
            set { Control.SortColumn = value; }
        }

        public void AddCheckedColumn(string caption, int width, bool autoSize = false)
        {
            Control.AddColumn(caption, width, autoSize);
        }

        public void AddColumn(string caption, int width, bool autoSize)
        {
            Control.AddColumn(caption, width, autoSize);
        }

        public void AddColumn(string caption, int width, bool autoSize, BSDTypes.HorizontalAlignment textAlign)
        {
            Control.AddColumn(caption, width, autoSize, textAlign);
        }

        public BSDListItem AddItem(object rowData, bool isChecked, params object[] columnValues)
        {
            return Control.AddItem(rowData, isChecked, columnValues);
        }

        public BSDListItem AddItem(object rowData, params object[] columnValues)
        {
            return Control.AddItem(rowData, columnValues);
        }

        public void BeginUpdate()
        {
            Control.BeginUpdate();
        }

        public void Clear()
        {
            Control.Clear();
        }

        public void ClearColumns()
        {
            Control.ClearColumns();
        }

        public void ClearItems()
        {
            Control.ClearItems();
        }

        public void EndUpdate()
        {
            Control.EndUpdate();
        }

        public object GetSelectedData()
        {
            return Control.GetSelectedData();
        }

        public void SelectItem(object rowData)
        {
            Control.SelectItem(rowData);
        }

        public void SetColumnCaption(int index, string caption)
        {
            Control.SetColumnCaption(index, caption);
        }

        public void SetSortColumn(int sortColumn, bool checkOrder = true)
        {
            Control.SetSortColumn(sortColumn, checkOrder);
        }

        public void Sort(int sortColumn, BSDTypes.SortOrder sortOrder)
        {
            Control.Sort(sortColumn, sortOrder);
        }

        public void UpdateContents(bool columnsChanged = false)
        {
            Control.UpdateContents(columnsChanged);
        }
    }*/


    // RadioButton does not exist!
    /*public sealed class RadioButtonHandler : BaseControlHandler<RadioButton, RadioButtonHandler>, IRadioButton
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
    }*/


    /*public sealed class PasswordBoxHandler : BaseControlHandler<PasswordBox, PasswordBoxHandler>, ITextBox
    {
        public PasswordBoxHandler(PasswordBox control) : base(control)
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
            set {  }
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

        private void SetBackColor()
        {
            Control.BackgroundColor = (!Control.ReadOnly && Enabled) ? SystemColors.WindowBackground : SystemColors.Control;
        }

        public void Copy()
        {
            UIHelper.SetClipboardText(Control.Text);
        }

        public void SelectAll()
        {
        }
    }*/

    // GroupBox does not exist!
    /*public sealed class GroupBoxHandler : BaseControlHandler<GroupBox, GroupBoxHandler>, IGroupBox
    {
        public GroupBoxHandler(GroupBox control) : base(control)
        {
        }

        public string Text
        {
            get { return Control.Text; }
            set { Control.Text = value; }
        }
    }*/
}
