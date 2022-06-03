/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2022 by Sergey V. Zhdanovskih.
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
using BSLib;
using GKCore;
using GKCore.Lists;
using GKCore.Types;
using BSDListItem = BSLib.Design.MVP.Controls.IListItem;

namespace GKUI.Components
{
    /// <summary>
    /// 
    /// </summary>
    public class GKSheetList : ContainerControl, ISheetList
    {
        private readonly ToolStripButton fBtnAdd;
        private readonly ToolStripButton fBtnDelete;
        private readonly ToolStripButton fBtnEdit;
        private readonly ToolStripButton fBtnLinkJump;
        private readonly ToolStripButton fBtnMoveUp;
        private readonly ToolStripButton fBtnMoveDown;
        private readonly ToolStrip fToolBar;
        private readonly GKListView fList;

        private EnumSet<SheetButton> fButtons;
        private ListModel fListModel;
        private bool fReadOnly;


        public event ModifyEventHandler OnModify;

        public event ItemValidatingEventHandler OnItemValidating;

        public event ModifyEventHandler OnBeforeChange;


        public EnumSet<SheetButton> Buttons
        {
            get { return fButtons; }
            set {
                if (fButtons != value) {
                    fButtons = value;
                    UpdateButtons();
                }
            }
        }

        public ListModel ListModel
        {
            get { return fListModel; }
            set {
                if (fListModel != value) {
                    if (fListModel != null) {
                        fListModel.SheetList = null;
                    }

                    fListModel = value;

                    if (fListModel != null) {
                        fListModel.SheetList = this;
                    }
                }

                UpdateSheet();
            }
        }

        public bool ReadOnly
        {
            get { return fReadOnly; }
            set { SetReadOnly(value); }
        }


        public GKSheetList(Control owner)
        {
            if (owner == null)
                throw new ArgumentNullException("owner");

            fBtnMoveDown = new ToolStripButton();
            fBtnMoveDown.Image = UIHelper.LoadResourceImage("Resources.btn_down.gif");
            fBtnMoveDown.ToolTipText = LangMan.LS(LSID.LSID_RecordMoveDown);
            fBtnMoveDown.Click += ItemMoveDown;

            fBtnMoveUp = new ToolStripButton();
            fBtnMoveUp.Image = UIHelper.LoadResourceImage("Resources.btn_up.gif");
            fBtnMoveUp.ToolTipText = LangMan.LS(LSID.LSID_RecordMoveUp);
            fBtnMoveUp.Click += ItemMoveUp;

            fBtnLinkJump = new ToolStripButton();
            fBtnLinkJump.Image = UIHelper.LoadResourceImage("Resources.btn_jump.gif");
            fBtnLinkJump.ToolTipText = LangMan.LS(LSID.LSID_RecordGoto);
            fBtnLinkJump.Click += ItemJump;

            fBtnDelete = new ToolStripButton();
            fBtnDelete.Name = "btnDelete";
            fBtnDelete.Image = UIHelper.LoadResourceImage("Resources.btn_rec_delete.gif");
            fBtnDelete.ToolTipText = LangMan.LS(LSID.LSID_MIRecordDelete);
            fBtnDelete.Click += ItemDelete;

            fBtnEdit = new ToolStripButton();
            fBtnEdit.Name = "btnEdit";
            fBtnEdit.Image = UIHelper.LoadResourceImage("Resources.btn_rec_edit.gif");
            fBtnEdit.ToolTipText = LangMan.LS(LSID.LSID_MIRecordEdit);
            fBtnEdit.Click += ItemEdit;

            fBtnAdd = new ToolStripButton();
            fBtnAdd.Name = "btnAdd";
            fBtnAdd.Image = UIHelper.LoadResourceImage("Resources.btn_rec_new.gif");
            fBtnAdd.ToolTipText = LangMan.LS(LSID.LSID_MIRecordAdd);
            fBtnAdd.Click += ItemAdd;

            fToolBar = new ToolStrip();
            fToolBar.Name = "ToolBar";
            fToolBar.Dock = DockStyle.Right;
            fToolBar.Items.AddRange(new ToolStripItem[] {
                                        fBtnAdd,
                                        fBtnEdit,
                                        fBtnDelete,
                                        fBtnLinkJump,
                                        fBtnMoveUp,
                                        fBtnMoveDown});
            fToolBar.GripStyle = ToolStripGripStyle.Hidden;
            fToolBar.ImageScalingSize = new Size(24, 20);
            fToolBar.AutoSize = true;
            fToolBar.ShowItemToolTips = true;

            fList = new GKListView();
            fList.Dock = DockStyle.Fill;
            fList.Location = new Point(0, 0);
            fList.Size = new Size(500, 290);
            fList.HideSelection = false;
            fList.LabelEdit = false;
            fList.FullRowSelect = true;
            fList.View = View.Details;
            fList.DoubleClick += List_DoubleClick;
            fList.KeyDown += List_KeyDown;

            SuspendLayout();
            Controls.Add(fList);
            Controls.Add(fToolBar);
            ResumeLayout(false);

            Dock = DockStyle.Fill;

            owner.SuspendLayout();
            owner.Controls.Add(this);
            owner.ResumeLayout(false);

            fButtons = EnumSet<SheetButton>.Create(SheetButton.lbAdd, SheetButton.lbEdit, SheetButton.lbDelete);
            fListModel = null;
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                fList.Dispose();
                fBtnLinkJump.Dispose();
                fBtnMoveUp.Dispose();
                fBtnMoveDown.Dispose();
                fBtnDelete.Dispose();
                fBtnEdit.Dispose();
                fBtnAdd.Dispose();
                fToolBar.Dispose();
            }
            base.Dispose(disposing);
        }

        public void Activate()
        {
            Select();
        }

        /// <summary>
        /// The library NUnitForms has a bug in the class Finder.
        /// So we need unique names for hierarchical included components.
        /// </summary>
        /// <param name="name">name of component</param>
        public void SetControlName(string name)
        {
            Name = name;
            fToolBar.Name = name + "_ToolBar";
            fBtnAdd.Name = fToolBar.Name + "_btnAdd";
            fBtnEdit.Name = fToolBar.Name + "_btnEdit";
            fBtnDelete.Name = fToolBar.Name + "_btnDelete";
            fBtnMoveUp.Name = fToolBar.Name + "_btnMoveUp";
            fBtnMoveDown.Name = fToolBar.Name + "_btnMoveDown";
        }

        #region Private methods

        private void UpdateButtons()
        {
            if (fListModel == null) {
                fBtnAdd.Visible = fButtons.Contains(SheetButton.lbAdd);
                fBtnDelete.Visible = fButtons.Contains(SheetButton.lbDelete);
                fBtnEdit.Visible = fButtons.Contains(SheetButton.lbEdit);
                fBtnLinkJump.Visible = fButtons.Contains(SheetButton.lbJump);
                fBtnMoveUp.Visible = fButtons.Contains(SheetButton.lbMoveUp);
                fBtnMoveDown.Visible = fButtons.Contains(SheetButton.lbMoveDown);
                fToolBar.Visible = !fButtons.IsEmpty();
            } else {
                EnumSet<RecordAction> allowedActions = fListModel.AllowedActions;
                fBtnAdd.Visible = allowedActions.Contains(RecordAction.raAdd);
                fBtnDelete.Visible = allowedActions.Contains(RecordAction.raDelete);
                fBtnEdit.Visible = allowedActions.Contains(RecordAction.raEdit);
                fBtnLinkJump.Visible = allowedActions.Contains(RecordAction.raJump);
                fBtnMoveUp.Visible = allowedActions.Contains(RecordAction.raMoveUp);
                fBtnMoveDown.Visible = allowedActions.Contains(RecordAction.raMoveDown);
                fToolBar.Visible = !allowedActions.IsEmpty();
            }
        }

        private void SetReadOnly(bool value)
        {
            fReadOnly = value;
            fBtnAdd.Enabled = !fReadOnly;
            fBtnDelete.Enabled = !fReadOnly;
            fBtnEdit.Enabled = !fReadOnly;
            fBtnMoveUp.Enabled = !fReadOnly;
            fBtnMoveDown.Enabled = !fReadOnly;

            fList.BackColor = (fReadOnly) ? SystemColors.Control : SystemColors.Window;
        }

        private void List_DoubleClick(object sender, EventArgs e)
        {
            ItemEdit(sender, e);
        }

        private void List_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.Control)
            {
                switch (e.KeyCode) {
                    case Keys.I:
                        ItemAdd(sender, e);
                        break;
                    case Keys.D:
                        ItemDelete(sender, e);
                        break;
                    case Keys.Return:
                        ItemEdit(sender, e);
                        break;
                }
            }
        }

        private void RestoreSelected(object itemData)
        {
            Activate();
            fList.Activate();

            if (itemData != null) fList.SelectItem(itemData);
        }

        private void DoBeforeChange(ModifyEventArgs eArgs)
        {
            var eventHandler = OnBeforeChange;
            if (eventHandler != null) {
                eventHandler(this, eArgs);
            }
        }

        private void DoModify(ModifyEventArgs eArgs)
        {
            DoBeforeChange(eArgs);

            if (fListModel != null) {
                fListModel.Modify(this, eArgs);

                if (eArgs.IsChanged) {
                    UpdateSheet();
                }
            }

            var eventHandler = OnModify;
            if (eventHandler != null) {
                eventHandler(this, eArgs);
            }
        }

        private bool ValidateItem(object item)
        {
            var args = new ItemValidatingEventArgs(item);

            var eventHandler = OnItemValidating;
            if (eventHandler == null) {
                return true;
            }

            eventHandler(this, args);
            return args.IsAvailable;
        }

        private void ItemAdd(object sender, EventArgs e)
        {
            if (fReadOnly) return;

            var eArgs = new ModifyEventArgs(RecordAction.raAdd, null);
            DoModify(eArgs);
            RestoreSelected(eArgs.ItemData);
        }

        private void ItemEdit(object sender, EventArgs e)
        {
            object itemData = GetSelectedData();
            if (fReadOnly || itemData == null) return;

            if (!ValidateItem(itemData)) return;

            var eArgs = new ModifyEventArgs(RecordAction.raEdit, itemData);
            DoModify(eArgs);
            RestoreSelected(eArgs.ItemData);
        }

        private void ItemDelete(object sender, EventArgs e)
        {
            object itemData = GetSelectedData();
            if (fReadOnly || itemData == null) return;

            if (!ValidateItem(itemData)) return;

            var eArgs = new ModifyEventArgs(RecordAction.raDelete, itemData);
            DoModify(eArgs);
        }

        private void ItemJump(object sender, EventArgs e)
        {
            object itemData = GetSelectedData();
            if (itemData == null) return;

            if (!ValidateItem(itemData)) return;

            var eArgs = new ModifyEventArgs(RecordAction.raJump, itemData);
            DoModify(eArgs);
        }

        private void ItemMoveUp(object sender, EventArgs e)
        {
            object itemData = GetSelectedData();
            if (fReadOnly || itemData == null) return;

            var eArgs = new ModifyEventArgs(RecordAction.raMoveUp, itemData);
            DoModify(eArgs);
            RestoreSelected(eArgs.ItemData);
        }

        private void ItemMoveDown(object sender, EventArgs e)
        {
            object itemData = GetSelectedData();
            if (fReadOnly || itemData == null) return;

            var eArgs = new ModifyEventArgs(RecordAction.raMoveDown, itemData);
            DoModify(eArgs);
            RestoreSelected(eArgs.ItemData);
        }

        #endregion

        public void ClearColumns()
        {
            fList.ClearColumns();
        }

        public void ResizeColumn(int columnIndex)
        {
            fList.ResizeColumn(columnIndex);
        }

        public void AddColumn(string caption, int width, bool autoSize)
        {
            fList.AddColumn(caption, width, autoSize);
        }

        public void BeginUpdate()
        {
            fList.BeginUpdate();
        }

        public void EndUpdate()
        {
            fList.EndUpdate();
        }

        public BSDListItem AddItem(object rowData, params object[] columnValues)
        {
            return fList.AddItem(rowData, columnValues);
        }

        public void ClearItems()
        {
            fList.ClearItems();
        }

        public void SelectItem(int index)
        {
            fList.SelectItem(index);
        }

        public void UpdateSheet()
        {
            UpdateButtons();

            if (fListModel != null) {
                if (fList.Columns.Count == 0 || fListModel.ColumnsHaveBeenChanged) {
                    fList.ClearColumns();
                    fListModel.UpdateColumns(fList);
                }

                fListModel.UpdateContents();
            }
        }

        public object GetSelectedData()
        {
            return fList.GetSelectedData();
        }
    }
}
