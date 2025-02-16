/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
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
using GKCore.Design.Controls;
using GKCore.Lists;
using GKCore.Types;
using GKUI.Themes;

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
        private readonly ToolStripButton fBtnCopy;
        private readonly ToolStripButton fBtnCut;
        private readonly ToolStripButton fBtnPaste;
        private readonly ToolStrip fToolBar;
        private readonly ContextMenuStrip fContextMenu;
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
                    fList.ContextMenuStrip = null;

                    fListModel = value;

                    if (fListModel != null) {
                        fList.ListMan = fListModel;
                        fListModel.SheetList = this;

                        if (fListModel.AllowedActions.Contains(RecordAction.raDetails)) {
                            fList.ContextMenuStrip = fContextMenu;
                        }
                    }
                }

                UpdateSheet();
            }
        }

        public IListView ListView
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
            fBtnPaste = CreateButton("btnPaste", UIHelper.LoadResourceImage("Resources.btn_paste.gif"), LangMan.LS(LSID.Paste), ItemPaste);
            fBtnCut = CreateButton("btnCut", UIHelper.LoadResourceImage("Resources.btn_cut.gif"), LangMan.LS(LSID.Cut), ItemCut);
            fBtnCopy = CreateButton("btnCopy", UIHelper.LoadResourceImage("Resources.btn_copy.gif"), LangMan.LS(LSID.Copy), ItemCopy);
            fBtnMoveDown = CreateButton("btnDown", UIHelper.LoadResourceImage("Resources.btn_down.gif"), LangMan.LS(LSID.RecordMoveDown), ItemMoveDown);
            fBtnMoveUp = CreateButton("btnUp", UIHelper.LoadResourceImage("Resources.btn_up.gif"), LangMan.LS(LSID.RecordMoveUp), ItemMoveUp);
            fBtnLinkJump = CreateButton("btnJump",  UIHelper.LoadResourceImage("Resources.btn_jump.gif"), LangMan.LS(LSID.RecordGoto), ItemJump);
            fBtnDelete = CreateButton("btnDelete", UIHelper.LoadResourceImage("Resources.btn_rec_delete.gif"), LangMan.LS(LSID.MIRecordDelete), ItemDelete);
            fBtnEdit = CreateButton("btnEdit", UIHelper.LoadResourceImage("Resources.btn_rec_edit.gif"), LangMan.LS(LSID.MIRecordEdit), ItemEdit);
            fBtnAdd = CreateButton( "btnAdd", UIHelper.LoadResourceImage("Resources.btn_rec_new.gif"), LangMan.LS(LSID.MIRecordAdd), ItemAdd);

            fToolBar = new ToolStrip();
            fToolBar.Name = "ToolBar";
            fToolBar.Dock = DockStyle.Right;
            fToolBar.Items.AddRange(new ToolStripItem[] { fBtnAdd, fBtnEdit, fBtnDelete, fBtnLinkJump, fBtnMoveUp, fBtnMoveDown, fBtnCopy, fBtnCut, fBtnPaste });
            fToolBar.GripStyle = ToolStripGripStyle.Hidden;
            fToolBar.ImageScalingSize = new Size(24, 20);
            fToolBar.AutoSize = true;
            fToolBar.ShowItemToolTips = true;

            var miDetails = new ToolStripMenuItem();
            miDetails.Text = LangMan.LS(LSID.Details);
            miDetails.Click += miDetails_Click;

            fContextMenu = new ContextMenuStrip();
            fContextMenu.Items.AddRange(new ToolStripItem[] { miDetails });

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
            fList.SelectedIndexChanged += List_SelectedIndexChanged;

            SuspendLayout();
            Controls.Add(fList);
            Controls.Add(fToolBar);
            ResumeLayout(false);

            Dock = DockStyle.Fill;
            KeyDown += List_KeyDown;

            Buttons = EnumSet<SheetButton>.Create(SheetButton.lbAdd, SheetButton.lbEdit, SheetButton.lbDelete);
            fListModel = null;
        }

        public GKSheetList(Control owner) : this()
        {
            if (owner == null)
                throw new ArgumentNullException("owner");

            owner.SuspendLayout();
            owner.Controls.Add(this);
            owner.ResumeLayout(false);
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
                fToolBar.Dispose();
            }
            base.Dispose(disposing);
        }

        public void Activate()
        {
            Select();
        }

        public void UpdateSheet()
        {
            UpdateButtons();
            fList.UpdateContents();
        }

        public void ApplyTheme()
        {
            UIHelper.SetButtonThemeImage(fBtnPaste, ThemeElement.Glyph_Paste);
            UIHelper.SetButtonThemeImage(fBtnCut, ThemeElement.Glyph_Cut);
            UIHelper.SetButtonThemeImage(fBtnCopy, ThemeElement.Glyph_Copy);
            UIHelper.SetButtonThemeImage(fBtnMoveDown, ThemeElement.Glyph_MoveDown);
            UIHelper.SetButtonThemeImage(fBtnMoveUp, ThemeElement.Glyph_MoveUp);
            UIHelper.SetButtonThemeImage(fBtnLinkJump, ThemeElement.Glyph_LinkJump);
            UIHelper.SetButtonThemeImage(fBtnDelete, ThemeElement.Glyph_ItemDelete);
            UIHelper.SetButtonThemeImage(fBtnEdit, ThemeElement.Glyph_ItemEdit);
            UIHelper.SetButtonThemeImage(fBtnAdd, ThemeElement.Glyph_ItemAdd);
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
            fBtnCopy.Name = fToolBar.Name + "_btnCopy";
            fBtnCut.Name = fToolBar.Name + "_btnCut";
            fBtnPaste.Name = fToolBar.Name + "_btnPaste";
        }

        #region Private methods

        private ToolStripButton CreateButton(string name, Image image, string toolTip, EventHandler click)
        {
            var btn = new ToolStripButton();
            btn.Name = name;
            btn.Image = image;
            btn.ToolTipText = toolTip;
            btn.Click += click;
            return btn;
        }

        public void UpdateButtons()
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
                fToolBar.Visible = !fButtons.IsEmpty();
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
            fBtnCopy.Enabled = !fReadOnly;
            fBtnCut.Enabled = !fReadOnly;
            fBtnPaste.Enabled = !fReadOnly;

            fList.BackColor = (fReadOnly) ? SystemColors.Control : SystemColors.Window;
        }

        private void List_SelectedIndexChanged(object sender, EventArgs e)
        {
            if (fListModel != null) {
                int itemIndex = -1; // fList.SelectedIndex;
                object itemData = fList.GetSelectedData();
                if (itemData == null) return;

                fListModel.OnItemSelected(itemIndex, itemData);
            }
        }

        private void miDetails_Click(object sender, EventArgs e)
        {
            if (fListModel != null) {
                object itemData = fList.GetSelectedData();
                if (itemData != null) {
                    fListModel.ShowDetails(itemData);
                }
            }
        }

        private void List_DoubleClick(object sender, EventArgs e)
        {
            ItemEdit(sender, e);
        }

        private void List_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.Control) {
                switch (e.KeyCode) {
                    case Keys.I:
                        ItemAdd(sender, e);
                        break;
                    case Keys.L:
                        ItemDelete(sender, e);
                        break;
                    case Keys.Return:
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
    }
}
