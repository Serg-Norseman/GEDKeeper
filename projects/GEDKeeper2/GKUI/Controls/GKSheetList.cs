/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
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
using System.Drawing;
using System.Windows.Forms;

using GKCommon;
using GKCommon.Controls;
using GKCore;
using GKCore.Types;

namespace GKUI.Controls
{
    public delegate void ModifyEventHandler(object sender, ModifyEventArgs eArgs);
    public delegate void ItemValidatingEventHandler(object sender, ItemValidatingEventArgs e);

    public class ItemValidatingEventArgs : EventArgs
    {
        private bool fIsAvailable;
        private object fItem;

        public bool IsAvailable
        {
            get { return this.fIsAvailable; }
            set { this.fIsAvailable = value; }
        }

        public object Item
        {
            get { return this.fItem; }
            set { this.fItem = value; }
        }

        public ItemValidatingEventArgs() : this(null)
        {
        }

        public ItemValidatingEventArgs(object item)
        {
            this.fItem = item;
        }
    }

    public enum SheetButton
    {
        lbAdd,
        lbEdit,
        lbDelete,
        lbJump,
        lbMoveUp,
        lbMoveDown
    }

    /// <summary>
    /// 
    /// </summary>
    public class GKSheetList : ContainerControl
    {
        private static readonly object EventModify;
        private static readonly object EventItemValidating;

        private readonly ToolStripButton fBtnAdd;
        private readonly ToolStripButton fBtnDelete;
        private readonly ToolStripButton fBtnEdit;
        private readonly ToolStripButton fBtnLinkJump;
        private readonly ToolStripButton fBtnMoveUp;
        private readonly ToolStripButton fBtnMoveDown;
        private readonly ToolStrip fToolBar;
        private readonly GKListView fList;

        private EnumSet<SheetButton> fButtons;
        private bool fReadOnly;

        public event ModifyEventHandler OnModify
        {
            add { base.Events.AddHandler(GKSheetList.EventModify, value); }
            remove { base.Events.RemoveHandler(GKSheetList.EventModify, value); }
        }

        public event ItemValidatingEventHandler OnItemValidating
        {
            add { base.Events.AddHandler(GKSheetList.EventItemValidating, value); }
            remove { base.Events.RemoveHandler(GKSheetList.EventItemValidating, value); }
        }

        public EnumSet<SheetButton> Buttons
        {
            get { return this.fButtons; }
            set { this.SetButtons(value); }
        }

        public bool ReadOnly
        {
            get { return this.fReadOnly; }
            set { this.SetReadOnly(value); }
        }

        static GKSheetList()
        {
            GKSheetList.EventModify = new object();
            GKSheetList.EventItemValidating = new object();
        }

        public GKSheetList(Control owner)
        {
            if (owner == null)
                throw new ArgumentNullException("owner");

            this.fBtnMoveDown = new ToolStripButton();
            this.fBtnMoveDown.Image = global::GKResources.iDown;
            this.fBtnMoveDown.ToolTipText = LangMan.LS(LSID.LSID_RecordMoveDown);
            this.fBtnMoveDown.Click += this.ItemMoveDown;

            this.fBtnMoveUp = new ToolStripButton();
            this.fBtnMoveUp.Image = global::GKResources.iUp;
            this.fBtnMoveUp.ToolTipText = LangMan.LS(LSID.LSID_RecordMoveUp);
            this.fBtnMoveUp.Click += this.ItemMoveUp;

            this.fBtnLinkJump = new ToolStripButton();
            this.fBtnLinkJump.Image = global::GKResources.iToMan;
            this.fBtnLinkJump.ToolTipText = LangMan.LS(LSID.LSID_RecordGoto);
            this.fBtnLinkJump.Click += this.ItemJump;

            this.fBtnDelete = new ToolStripButton();
            this.fBtnDelete.Name = "btnDelete";
            this.fBtnDelete.Image = global::GKResources.iRecDelete;
            this.fBtnDelete.ToolTipText = LangMan.LS(LSID.LSID_MIRecordDelete);
            this.fBtnDelete.Click += this.ItemDelete;

            this.fBtnEdit = new ToolStripButton();
            this.fBtnEdit.Name = "btnEdit";
            this.fBtnEdit.Image = global::GKResources.iRecEdit;
            this.fBtnEdit.ToolTipText = LangMan.LS(LSID.LSID_MIRecordEdit);
            this.fBtnEdit.Click += this.ItemEdit;

            this.fBtnAdd = new ToolStripButton();
            this.fBtnAdd.Name = "btnAdd";
            this.fBtnAdd.Image = global::GKResources.iRecNew;
            this.fBtnAdd.ToolTipText = LangMan.LS(LSID.LSID_MIRecordAdd);
            this.fBtnAdd.Click += this.ItemAdd;

            this.fToolBar = new ToolStrip();
            this.fToolBar.Name = "ToolBar";
            this.fToolBar.Dock = DockStyle.Right;
            //this.fToolBar.Appearance = ToolBarAppearance.Flat;
            this.fToolBar.Items.AddRange(new ToolStripItem[] {
                                             this.fBtnAdd,
                                             this.fBtnEdit,
                                             this.fBtnDelete,
                                             this.fBtnLinkJump,
                                             this.fBtnMoveUp,
                                             this.fBtnMoveDown});
            this.fToolBar.GripStyle = ToolStripGripStyle.Hidden;
            this.fToolBar.ImageScalingSize = new System.Drawing.Size(24, 20);
            //this.fToolBar.AutoSize = true;
            //this.fToolBar.ShowToolTips = true;

            this.fList = new GKListView();
            this.fList.Dock = DockStyle.Fill;
            this.fList.Location = new Point(0, 0);
            this.fList.Size = new Size(500, 290);
            this.fList.HideSelection = false;
            this.fList.LabelEdit = false;
            this.fList.FullRowSelect = true;
            this.fList.View = View.Details;
            this.fList.DoubleClick += this.List_DoubleClick;
            this.fList.KeyDown += this.List_KeyDown;

            this.SuspendLayout();
            this.Controls.Add(this.fList);
            this.Controls.Add(this.fToolBar);
            //this.Controls.SetChildIndex(this.fList, 1);
            //this.Controls.SetChildIndex(this.fToolBar, 0);
            this.ResumeLayout(false);

            this.Dock = DockStyle.Fill;

            owner.SuspendLayout();
            owner.Controls.Add(this);
            owner.ResumeLayout(false);

            this.SetButtons(EnumSet<SheetButton>.Create(SheetButton.lbAdd, SheetButton.lbEdit, SheetButton.lbDelete));
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                this.fList.Dispose();
                this.fBtnLinkJump.Dispose();
                this.fBtnMoveUp.Dispose();
                this.fBtnMoveDown.Dispose();
                this.fBtnDelete.Dispose();
                this.fBtnEdit.Dispose();
                this.fBtnAdd.Dispose();
                this.fToolBar.Dispose();
            }
            base.Dispose(disposing);
        }

        /// <summary>
        /// The library NUnitForms has a bug in the class Finder<T>.
        /// So we need unique names for hierarchical included components.
        /// TODO: Need to fix this bug in NUnitForms.
        /// </summary>
        /// <param name="name"></param>
        public void SetControlName(string name)
        {
            this.Name = name;
            this.fToolBar.Name = name + "_ToolBar";
            this.fBtnAdd.Name = this.fToolBar.Name + "_btnAdd";
            this.fBtnEdit.Name = this.fToolBar.Name + "_btnEdit";
            this.fBtnDelete.Name = this.fToolBar.Name + "_btnDelete";
        }

        private void SetButtons(EnumSet<SheetButton> value)
        {
            this.fButtons = value;
            this.fBtnAdd.Visible = this.fButtons.Contains(SheetButton.lbAdd);
            this.fBtnDelete.Visible = this.fButtons.Contains(SheetButton.lbDelete);
            this.fBtnEdit.Visible = this.fButtons.Contains(SheetButton.lbEdit);
            this.fBtnLinkJump.Visible = this.fButtons.Contains(SheetButton.lbJump);
            this.fBtnMoveUp.Visible = this.fButtons.Contains(SheetButton.lbMoveUp);
            this.fBtnMoveDown.Visible = this.fButtons.Contains(SheetButton.lbMoveDown);
            this.fToolBar.Visible = !this.fButtons.IsEmpty();
        }

        private void SetReadOnly(bool value)
        {
            this.fReadOnly = value;
            this.fBtnAdd.Enabled = !this.fReadOnly;
            this.fBtnDelete.Enabled = !this.fReadOnly;
            this.fBtnEdit.Enabled = !this.fReadOnly;
            this.fBtnMoveUp.Enabled = !this.fReadOnly;
            this.fBtnMoveDown.Enabled = !this.fReadOnly;

            this.fList.BackColor = (this.fReadOnly) ? SystemColors.Control : SystemColors.Window;
        }

        private void List_DoubleClick(object sender, EventArgs e)
        {
            this.ItemEdit(sender, e);
        }

        private void List_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.Control)
            {
                switch (e.KeyCode) {
                    case Keys.I:
                        this.ItemAdd(sender, e);
                        break;
                    case Keys.D:
                        this.ItemDelete(sender, e);
                        break;
                    case Keys.Return:
                        this.ItemEdit(sender, e);
                        break;
                }
            }
        }

        public object GetSelectedData()
        {
            GKListItem item = this.fList.GetSelectedItem();
            return (item != null) ? item.Data : null;
        }

        private void RestoreSelected(object itemData)
        {
            if (itemData != null) this.fList.SelectItem(itemData);
        }

        private void DoModify(ModifyEventArgs eArgs)
        {
            ModifyEventHandler eventHandler = (ModifyEventHandler)base.Events[GKSheetList.EventModify];
            if (eventHandler != null) {
                eventHandler(this, eArgs);
            }
        }

        private bool ValidateItem(object item)
        {
            var args = new ItemValidatingEventArgs(item);

            ItemValidatingEventHandler eventHandler = (ItemValidatingEventHandler)base.Events[GKSheetList.EventItemValidating];
            if (eventHandler != null) {
                eventHandler(this, args);

                return args.IsAvailable;
            } else {
                return true;
            }
        }

        private void ItemAdd(object sender, EventArgs e)
        {
            if (!this.fReadOnly)
            {
                ModifyEventArgs eArgs = new ModifyEventArgs(RecordAction.raAdd, null);
                this.DoModify(eArgs);
                this.RestoreSelected(eArgs.ItemData);
            }
        }

        private void ItemEdit(object sender, EventArgs e)
        {
            object itemData = this.GetSelectedData();
            if (!this.fReadOnly && itemData != null)
            {
                if (!this.ValidateItem(itemData)) return;

                ModifyEventArgs eArgs = new ModifyEventArgs(RecordAction.raEdit, itemData);
                this.DoModify(eArgs);
                this.RestoreSelected(eArgs.ItemData);
            }
        }

        private void ItemDelete(object sender, EventArgs e)
        {
            object itemData = this.GetSelectedData();
            if (!this.fReadOnly && itemData != null)
            {
                if (!this.ValidateItem(itemData)) return;

                ModifyEventArgs eArgs = new ModifyEventArgs(RecordAction.raDelete, itemData);
                this.DoModify(eArgs);
            }
        }

        private void ItemJump(object sender, EventArgs e)
        {
            object itemData = this.GetSelectedData();
            if (itemData != null)
            {
                if (!this.ValidateItem(itemData)) return;

                ModifyEventArgs eArgs = new ModifyEventArgs(RecordAction.raJump, itemData);
                this.DoModify(eArgs);
            }
        }

        private void ItemMoveUp(object sender, EventArgs e)
        {
            object itemData = this.GetSelectedData();
            if (!this.fReadOnly && itemData != null)
            {
                ModifyEventArgs eArgs = new ModifyEventArgs(RecordAction.raMoveUp, itemData);
                this.DoModify(eArgs);
                this.RestoreSelected(eArgs.ItemData);
            }
        }

        private void ItemMoveDown(object sender, EventArgs e)
        {
            object itemData = this.GetSelectedData();
            if (!this.fReadOnly && itemData != null)
            {
                ModifyEventArgs eArgs = new ModifyEventArgs(RecordAction.raMoveDown, itemData);
                this.DoModify(eArgs);
                this.RestoreSelected(eArgs.ItemData);
            }
        }

        public void ClearColumns()
        {
            this.fList.Columns.Clear();
        }

        public void ResizeColumn(int columnIndex)
        {
            this.fList.ResizeColumn(columnIndex);
        }

        public void AddColumn(string caption, int width, bool autoSize)
        {
            this.fList.AddListColumn(caption, width, autoSize);
        }

        public void Columns_BeginUpdate()
        {
        }

        public void Columns_EndUpdate()
        {
        }

        public void BeginUpdate()
        {
            this.fList.BeginUpdate();
        }

        public void EndUpdate()
        {
            this.fList.EndUpdate();
        }

        public GKListItem AddItem(object itemValue, object data)
        {
            return this.fList.AddItem(itemValue, data);
        }

        public void ClearItems()
        {
            this.fList.Items.Clear();
        }

        public void SwitchSorter()
        {
            this.fList.SwitchSorter();
        }

        public void SelectItem(int index)
        {
            this.fList.SelectItem(index);
        }
    }
}
