﻿/*
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

using System;
using BSLib;
using GKCore;
using GKCore.Design.Controls;
using GKCore.Lists;
using GKCore.Types;
using Xamarin.Forms;

namespace GKUI.Components
{
    /// <summary>
    /// 
    /// </summary>
    public class GKSheetList : ContentView, ISheetList
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

        public bool Enabled
        {
            get { return base.IsEnabled; }
            set { base.IsEnabled = value; }
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
            fBtnPaste = CreateButton("btnPaste", UIHelper.LoadResourceImage("Resources.btn_paste.gif"), LangMan.LS(LSID.LSID_Paste), ItemPaste);
            fBtnCut = CreateButton("btnCut", UIHelper.LoadResourceImage("Resources.btn_cut.gif"), LangMan.LS(LSID.LSID_Cut), ItemCut);
            fBtnCopy = CreateButton("btnCopy", UIHelper.LoadResourceImage("Resources.btn_copy.gif"), LangMan.LS(LSID.LSID_Copy), ItemCopy);
            fBtnMoveDown = CreateButton("btnDown", UIHelper.LoadResourceImage("Resources.btn_down.gif"), LangMan.LS(LSID.LSID_RecordMoveDown), ItemMoveDown);
            fBtnMoveUp = CreateButton("btnUp", UIHelper.LoadResourceImage("Resources.btn_up.gif"), LangMan.LS(LSID.LSID_RecordMoveUp), ItemMoveUp);
            fBtnLinkJump = CreateButton("btnJump", UIHelper.LoadResourceImage("Resources.btn_jump.gif"), LangMan.LS(LSID.LSID_RecordGoto), ItemJump);
            fBtnDelete = CreateButton("btnDelete", UIHelper.LoadResourceImage("Resources.btn_rec_delete.gif"), LangMan.LS(LSID.LSID_MIRecordDelete), ItemDelete);
            fBtnEdit = CreateButton("btnEdit", UIHelper.LoadResourceImage("Resources.btn_rec_edit.gif"), LangMan.LS(LSID.LSID_MIRecordEdit), ItemEdit);
            fBtnAdd = CreateButton("btnAdd", UIHelper.LoadResourceImage("Resources.btn_rec_new.gif"), LangMan.LS(LSID.LSID_MIRecordAdd), ItemAdd);

            fList = new GKListView();
            fList.HorizontalOptions = LayoutOptions.FillAndExpand;
            //fList.MouseDoubleClick += List_DoubleClick;

            var toolbar = new StackLayout() {
                Orientation = StackOrientation.Vertical,
                Spacing = 4,
                Children = { fBtnAdd, fBtnEdit, fBtnDelete, fBtnLinkJump, fBtnMoveUp, fBtnMoveDown, fBtnCopy, fBtnCut, fBtnPaste },
                HorizontalOptions = LayoutOptions.End
            };

            Content = new StackLayout() {
                Orientation = StackOrientation.Horizontal,
                Spacing = 4,
                Children = { fList, toolbar }
            };

            Buttons = EnumSet<SheetButton>.Create(SheetButton.lbAdd, SheetButton.lbEdit, SheetButton.lbDelete);
            fListModel = null;
        }

        public void Activate()
        {
            Focus();
        }

        public void UpdateSheet()
        {
            UpdateButtons();
            fList.UpdateContents();
        }

        #region Private methods

        private Button CreateButton(string name, ImageSource image, string toolTip, EventHandler click)
        {
            var btn = new Button();
            //btn.Style = "iconBtn";
            btn.ImageSource = image;
            //btn.ToolTip = toolTip;
            btn.Clicked += click;
            return btn;
        }

        private void UpdateButtons()
        {
            if (fListModel == null) {
                fBtnAdd.IsVisible = fButtons.Contains(SheetButton.lbAdd);
                fBtnDelete.IsVisible = fButtons.Contains(SheetButton.lbDelete);
                fBtnEdit.IsVisible = fButtons.Contains(SheetButton.lbEdit);
                fBtnLinkJump.IsVisible = fButtons.Contains(SheetButton.lbJump);
                fBtnMoveUp.IsVisible = fButtons.Contains(SheetButton.lbMoveUp);
                fBtnMoveDown.IsVisible = fButtons.Contains(SheetButton.lbMoveDown);
                fBtnCopy.IsVisible = fButtons.Contains(SheetButton.lbCopy);
                fBtnCut.IsVisible = fButtons.Contains(SheetButton.lbCut);
                fBtnPaste.IsVisible = fButtons.Contains(SheetButton.lbPaste);
                //fToolBar.Enabled = !fButtons.IsEmpty();
            } else {
                EnumSet<RecordAction> allowedActions = fListModel.AllowedActions;
                fBtnAdd.IsVisible = allowedActions.Contains(RecordAction.raAdd);
                fBtnDelete.IsVisible = allowedActions.Contains(RecordAction.raDelete);
                fBtnEdit.IsVisible = allowedActions.Contains(RecordAction.raEdit);
                fBtnLinkJump.IsVisible = allowedActions.Contains(RecordAction.raJump);
                fBtnMoveUp.IsVisible = allowedActions.Contains(RecordAction.raMoveUp);
                fBtnMoveDown.IsVisible = allowedActions.Contains(RecordAction.raMoveDown);
                fBtnCopy.IsVisible = allowedActions.Contains(RecordAction.raCopy);
                fBtnCut.IsVisible = allowedActions.Contains(RecordAction.raCut);
                fBtnPaste.IsVisible = allowedActions.Contains(RecordAction.raPaste);
                //fToolBar.Visible = !allowedActions.IsEmpty();
            }
        }

        private void SetReadOnly(bool value)
        {
            fReadOnly = value;
            fBtnAdd.IsEnabled = !fReadOnly;
            fBtnDelete.IsEnabled = !fReadOnly;
            fBtnEdit.IsEnabled = !fReadOnly;
            fBtnMoveUp.IsEnabled = !fReadOnly;
            fBtnMoveDown.IsEnabled = !fReadOnly;
            fBtnCopy.IsEnabled = !fReadOnly;
            fBtnCut.IsEnabled = !fReadOnly;
            fBtnPaste.IsEnabled = !fReadOnly;

            //fList.BackgroundColor = (fReadOnly) ? SystemColors.Control : SystemColors.WindowBackground;
        }

        private void List_DoubleClick(object sender, EventArgs e)
        {
            ItemEdit(sender, e);
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