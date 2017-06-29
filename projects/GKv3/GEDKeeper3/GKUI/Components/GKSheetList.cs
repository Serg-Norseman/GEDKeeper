/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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
using Eto.Drawing;
using Eto.Forms;
using GKCommon;
using GKCore;
using GKCore.Lists;
using GKCore.Types;

namespace GKUI.Components
{
    /// <summary>
    /// 
    /// </summary>
    public class GKSheetList : Panel, ISheetList
    {
        private readonly Button fBtnAdd;
        private readonly Button fBtnDelete;
        private readonly Button fBtnEdit;
        private readonly Button fBtnLinkJump;
        private readonly Button fBtnMoveUp;
        private readonly Button fBtnMoveDown;
        private readonly GKListView fList;

        private EnumSet<SheetButton> fButtons;
        private ListModel fListModel;
        private bool fReadOnly;


        public event ModifyEventHandler OnModify;

        public event ItemValidatingEventHandler OnItemValidating;


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


        public GKSheetList(Panel owner)
        {
            if (owner == null)
                throw new ArgumentNullException("owner");

            fBtnMoveDown = new Button();
            fBtnMoveDown.Image = Bitmap.FromResource("Resources.btn_down.gif");
            fBtnMoveDown.Size = new Size(26, 26);
            fBtnMoveDown.ToolTip = LangMan.LS(LSID.LSID_RecordMoveDown);
            fBtnMoveDown.Click += ItemMoveDown;

            fBtnMoveUp = new Button();
            fBtnMoveUp.Image = Bitmap.FromResource("Resources.btn_up.gif");
            fBtnMoveUp.Size = new Size(26, 26);
            fBtnMoveUp.ToolTip = LangMan.LS(LSID.LSID_RecordMoveUp);
            fBtnMoveUp.Click += ItemMoveUp;

            fBtnLinkJump = new Button();
            fBtnLinkJump.Image = Bitmap.FromResource("Resources.btn_jump.gif");
            fBtnLinkJump.Size = new Size(26, 26);
            fBtnLinkJump.ToolTip = LangMan.LS(LSID.LSID_RecordGoto);
            fBtnLinkJump.Click += ItemJump;

            fBtnDelete = new Button();
            fBtnDelete.Image = Bitmap.FromResource("Resources.btn_rec_delete.gif");
            fBtnDelete.Size = new Size(26, 26);
            fBtnDelete.ToolTip = LangMan.LS(LSID.LSID_MIRecordDelete);
            fBtnDelete.Click += ItemDelete;

            fBtnEdit = new Button();
            fBtnEdit.Image = Bitmap.FromResource("Resources.btn_rec_edit.gif");
            fBtnEdit.Size = new Size(26, 26);
            fBtnEdit.ToolTip = LangMan.LS(LSID.LSID_MIRecordEdit);
            fBtnEdit.Click += ItemEdit;

            fBtnAdd = new Button();
            fBtnAdd.Image = Bitmap.FromResource("Resources.btn_rec_new.gif");
            fBtnAdd.Size = new Size(26, 26);
            fBtnAdd.ToolTip = LangMan.LS(LSID.LSID_MIRecordAdd);
            fBtnAdd.Click += ItemAdd;

            fList = new GKListView();
            //fList.Size = new Size(500, 290);
            fList.MouseDoubleClick += List_DoubleClick;
            fList.KeyDown += List_KeyDown;

            SuspendLayout();
            Content = new StackLayout() {
                Orientation = Orientation.Horizontal,
                Spacing = 5,
                Items = {
                    new StackLayoutItem(fList, true),
                    new StackLayoutItem(
                        new StackLayout() {
                            Orientation = Orientation.Vertical,
                            Spacing = 5,
                            Items = { fBtnAdd, fBtnEdit, fBtnDelete,
                                fBtnLinkJump, fBtnMoveUp, fBtnMoveDown }
                        }, false)
                }
            };
            ResumeLayout();

            owner.SuspendLayout();
            owner.Content = this;
            owner.ResumeLayout();

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
            }
            base.Dispose(disposing);
        }

        #region Private methods

        private void UpdateButtons()
        {
            if (fListModel == null) {
                fBtnAdd.Enabled = fButtons.Contains(SheetButton.lbAdd);
                fBtnDelete.Enabled = fButtons.Contains(SheetButton.lbDelete);
                fBtnEdit.Enabled = fButtons.Contains(SheetButton.lbEdit);
                fBtnLinkJump.Enabled = fButtons.Contains(SheetButton.lbJump);
                fBtnMoveUp.Enabled = fButtons.Contains(SheetButton.lbMoveUp);
                fBtnMoveDown.Enabled = fButtons.Contains(SheetButton.lbMoveDown);
                //fToolBar.Enabled = !fButtons.IsEmpty();
            } else {
                EnumSet<RecordAction> allowedActions = fListModel.AllowedActions;
                fBtnAdd.Enabled = allowedActions.Contains(RecordAction.raAdd);
                fBtnDelete.Enabled = allowedActions.Contains(RecordAction.raDelete);
                fBtnEdit.Enabled = allowedActions.Contains(RecordAction.raEdit);
                fBtnLinkJump.Enabled = allowedActions.Contains(RecordAction.raJump);
                fBtnMoveUp.Enabled = allowedActions.Contains(RecordAction.raMoveUp);
                fBtnMoveDown.Enabled = allowedActions.Contains(RecordAction.raMoveDown);
                //fToolBar.Enabled = !allowedActions.IsEmpty();
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

            fList.BackgroundColor = (fReadOnly) ? SystemColors.Control : SystemColors.WindowBackground;
        }

        private void List_DoubleClick(object sender, EventArgs e)
        {
            ItemEdit(sender, e);
        }

        private void List_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.Control)
            {
                switch (e.Key) {
                    case Keys.I:
                        ItemAdd(sender, e);
                        break;
                    case Keys.D:
                        ItemDelete(sender, e);
                        break;
                    case Keys.Enter:
                        ItemEdit(sender, e);
                        break;
                }
            }
        }

        private void RestoreSelected(object itemData)
        {
            if (itemData != null) fList.SelectItem(itemData);
        }

        private void DoModify(ModifyEventArgs eArgs)
        {
            if (fListModel != null) {
                fListModel.Modify(this, eArgs);

                if (eArgs.IsChanged) {
                    UpdateSheet();
                }
            }

            var eventHandler = (ModifyEventHandler)OnModify;
            if (eventHandler != null) {
                eventHandler(this, eArgs);
            }
        }

        private bool ValidateItem(object item)
        {
            var args = new ItemValidatingEventArgs(item);

            var eventHandler = (ItemValidatingEventHandler)OnItemValidating;
            if (eventHandler == null)
            {
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

        public GKCore.Interfaces.IListItem AddItem(object rowData, params object[] columnValues)
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
