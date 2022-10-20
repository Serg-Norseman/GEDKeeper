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
using BSLib;
using Eto.Drawing;
using Eto.Forms;
using GKCore;
using GKCore.Interfaces;
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
        private readonly Button fBtnCopy;
        private readonly Button fBtnCut;
        private readonly Button fBtnPaste;
        private readonly GKListView fList;

        private EnumSet<SheetButton> fButtons;
        private ISheetModel fListModel;
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

        public ISheetModel ListModel
        {
            get { return fListModel; }
            set {
                if (fListModel != value) {
                    if (fListModel != null) {
                        fListModel.SheetList = null;
                    }

                    fListModel = value;

                    if (fListModel != null) {
                        fList.ListMan = fListModel;
                        fListModel.SheetList = this;
                    }
                }

                UpdateSheet();
            }
        }

        public IListViewEx ListView
        {
            get { return fList; }
        }

        public bool ReadOnly
        {
            get { return fReadOnly; }
            set { SetReadOnly(value); }
        }


        public GKSheetList()
        {
            fBtnPaste = CreateButton("btnPaste", UIHelper.LoadResourceImage("Resources.btn_paste.gif"), LangMan.LS(LSID.LSID_Paste), ItemPaste);
            fBtnCut = CreateButton("btnCut", UIHelper.LoadResourceImage("Resources.btn_cut.gif"), LangMan.LS(LSID.LSID_Cut), ItemCut);
            fBtnCopy = CreateButton("btnCopy", UIHelper.LoadResourceImage("Resources.btn_copy.gif"), LangMan.LS(LSID.LSID_Copy), ItemCopy);
            fBtnMoveDown = CreateButton("btnDown", UIHelper.LoadResourceImage("Resources.btn_down.gif"), LangMan.LS(LSID.LSID_RecordMoveDown), ItemMoveDown);
            fBtnMoveUp = CreateButton("btnUp", UIHelper.LoadResourceImage("Resources.btn_up.gif"), LangMan.LS(LSID.LSID_RecordMoveUp), ItemMoveUp);
            fBtnLinkJump = CreateButton("btnJump",  UIHelper.LoadResourceImage("Resources.btn_jump.gif"), LangMan.LS(LSID.LSID_RecordGoto), ItemJump);
            fBtnDelete = CreateButton("btnDelete", UIHelper.LoadResourceImage("Resources.btn_rec_delete.gif"), LangMan.LS(LSID.LSID_MIRecordDelete), ItemDelete);
            fBtnEdit = CreateButton("btnEdit", UIHelper.LoadResourceImage("Resources.btn_rec_edit.gif"), LangMan.LS(LSID.LSID_MIRecordEdit), ItemEdit);
            fBtnAdd = CreateButton( "btnAdd", UIHelper.LoadResourceImage("Resources.btn_rec_new.gif"), LangMan.LS(LSID.LSID_MIRecordAdd), ItemAdd);

            fList = new GKListView();
            fList.MouseDoubleClick += List_DoubleClick;
            fList.KeyDown += List_KeyDown;

            SuspendLayout();

            var toolbar = new StackLayout() {
                Orientation = Orientation.Vertical,
                Spacing = 4,
                Items = { fBtnAdd, fBtnEdit, fBtnDelete, fBtnLinkJump, fBtnMoveUp, fBtnMoveDown, fBtnCopy, fBtnCut, fBtnPaste }
            };

            Content = new TableLayout() {
                Spacing = new Size(4, 4),
                Rows = {
                    new TableRow() {
                        Cells = {
                            new TableCell(fList, true),
                            new TableCell(toolbar, false)
                        }
                    }
                }
            };

            ResumeLayout();

            fButtons = EnumSet<SheetButton>.Create(SheetButton.lbAdd, SheetButton.lbEdit, SheetButton.lbDelete);
            fListModel = null;
        }

        public GKSheetList(Panel owner) : this()
        {
            if (owner == null)
                throw new ArgumentNullException("owner");

            owner.SuspendLayout();
            owner.Content = this;
            owner.ResumeLayout();
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                fList.Dispose();
                fBtnPaste.Dispose();
                fBtnCut.Dispose();
                fBtnCopy.Dispose();
                fBtnLinkJump.Dispose();
                fBtnMoveUp.Dispose();
                fBtnMoveDown.Dispose();
                fBtnDelete.Dispose();
                fBtnEdit.Dispose();
                fBtnAdd.Dispose();
            }
            base.Dispose(disposing);
        }

        public Button CreateButton(string name, Image image, string toolTip, EventHandler<EventArgs> click)
        {
            var btn = new Button();
            btn.Style = "iconBtn";
            btn.Image = image;
            btn.ToolTip = toolTip;
            btn.Click += click;
            return btn;
        }

        public void Activate()
        {
            Focus();
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
                fBtnCopy.Visible = fButtons.Contains(SheetButton.lbCopy);
                fBtnCut.Visible = fButtons.Contains(SheetButton.lbCut);
                fBtnPaste.Visible = fButtons.Contains(SheetButton.lbPaste);
                //fToolBar.Enabled = !fButtons.IsEmpty();
            } else {
                EnumSet<RecordAction> allowedActions = fListModel.AllowedActions;
                fBtnAdd.Visible = allowedActions.Contains(RecordAction.raAdd);
                fBtnDelete.Visible = allowedActions.Contains(RecordAction.raDelete);
                fBtnEdit.Visible = allowedActions.Contains(RecordAction.raEdit);
                fBtnLinkJump.Visible = allowedActions.Contains(RecordAction.raJump);
                fBtnMoveUp.Visible = allowedActions.Contains(RecordAction.raMoveUp);
                fBtnMoveDown.Visible = allowedActions.Contains(RecordAction.raMoveDown);
                fBtnCopy.Visible = allowedActions.Contains(RecordAction.raCopy);
                fBtnCut.Visible = allowedActions.Contains(RecordAction.raCut);
                fBtnPaste.Visible = allowedActions.Contains(RecordAction.raPaste);
                //fToolBar.Visible = !allowedActions.IsEmpty();
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
            fBtnCopy.Enabled = !fReadOnly;
            fBtnCut.Enabled = !fReadOnly;
            fBtnPaste.Enabled = !fReadOnly;

            fList.BackgroundColor = (fReadOnly) ? SystemColors.Control : SystemColors.WindowBackground;
        }

        private void List_DoubleClick(object sender, EventArgs e)
        {
            ItemEdit(sender, e);
        }

        private void List_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.Control) {
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

                    case Keys.C:
                        ItemCopy(sender, e);
                        break;
                    case Keys.X:
                        ItemCut(sender, e);
                        break;
                    case Keys.V:
                        ItemPaste(sender, e);
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
            object itemData = fList.GetSelectedData();
            if (fReadOnly || itemData == null) return;

            if (!ValidateItem(itemData)) return;

            var eArgs = new ModifyEventArgs(RecordAction.raEdit, itemData);
            DoModify(eArgs);
            RestoreSelected(eArgs.ItemData);
        }

        private void ItemDelete(object sender, EventArgs e)
        {
            object itemData = fList.GetSelectedData();
            if (fReadOnly || itemData == null) return;

            if (!ValidateItem(itemData)) return;

            var eArgs = new ModifyEventArgs(RecordAction.raDelete, itemData);
            DoModify(eArgs);
        }

        private void ItemJump(object sender, EventArgs e)
        {
            object itemData = fList.GetSelectedData();
            if (itemData == null) return;

            if (!ValidateItem(itemData)) return;

            var eArgs = new ModifyEventArgs(RecordAction.raJump, itemData);
            DoModify(eArgs);
        }

        private void ItemMoveUp(object sender, EventArgs e)
        {
            object itemData = fList.GetSelectedData();
            if (fReadOnly || itemData == null) return;

            var eArgs = new ModifyEventArgs(RecordAction.raMoveUp, itemData);
            DoModify(eArgs);
            RestoreSelected(eArgs.ItemData);
        }

        private void ItemMoveDown(object sender, EventArgs e)
        {
            object itemData = fList.GetSelectedData();
            if (fReadOnly || itemData == null) return;

            var eArgs = new ModifyEventArgs(RecordAction.raMoveDown, itemData);
            DoModify(eArgs);
            RestoreSelected(eArgs.ItemData);
        }

        private void ItemCopy(object sender, EventArgs e)
        {
            object itemData = fList.GetSelectedData();
            if (fReadOnly || itemData == null) return;

            var eArgs = new ModifyEventArgs(RecordAction.raCopy, itemData);
            DoModify(eArgs);
            RestoreSelected(eArgs.ItemData);
        }

        private void ItemCut(object sender, EventArgs e)
        {
            object itemData = fList.GetSelectedData();
            if (fReadOnly || itemData == null) return;

            var eArgs = new ModifyEventArgs(RecordAction.raCut, itemData);
            DoModify(eArgs);
        }

        private void ItemPaste(object sender, EventArgs e)
        {
            if (fReadOnly) return;

            var eArgs = new ModifyEventArgs(RecordAction.raPaste, null);
            DoModify(eArgs);
            RestoreSelected(eArgs.ItemData);
        }

        #endregion

        public void UpdateSheet()
        {
            UpdateButtons();
            fList.UpdateContents();
        }
    }
}
