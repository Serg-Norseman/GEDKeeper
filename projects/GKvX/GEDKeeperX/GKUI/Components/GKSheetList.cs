/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2024 by Sergey V. Zhdanovskih.
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
using System.Threading.Tasks;
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
        private readonly StackLayout fToolBar;

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
            fBtnPaste = CreateButton("btnPaste", UIHelper.LoadResourceImage("Resources.btn_paste.gif"), LangMan.LS(LSID.Paste), ItemPaste);
            fBtnCut = CreateButton("btnCut", UIHelper.LoadResourceImage("Resources.btn_cut.gif"), LangMan.LS(LSID.Cut), ItemCut);
            fBtnCopy = CreateButton("btnCopy", UIHelper.LoadResourceImage("Resources.btn_copy.gif"), LangMan.LS(LSID.Copy), ItemCopy);
            fBtnMoveDown = CreateButton("btnDown", UIHelper.LoadResourceImage("Resources.btn_down.gif"), LangMan.LS(LSID.RecordMoveDown), ItemMoveDown);
            fBtnMoveUp = CreateButton("btnUp", UIHelper.LoadResourceImage("Resources.btn_up.gif"), LangMan.LS(LSID.RecordMoveUp), ItemMoveUp);
            fBtnLinkJump = CreateButton("btnJump", UIHelper.LoadResourceImage("Resources.btn_jump.gif"), LangMan.LS(LSID.RecordGoto), ItemJump);
            fBtnDelete = CreateButton("btnDelete", UIHelper.LoadResourceImage("Resources.btn_rec_delete.gif"), LangMan.LS(LSID.MIRecordDelete), ItemDelete);
            fBtnEdit = CreateButton("btnEdit", UIHelper.LoadResourceImage("Resources.btn_rec_edit.gif"), LangMan.LS(LSID.MIRecordEdit), ItemEdit);
            fBtnAdd = CreateButton("btnAdd", UIHelper.LoadResourceImage("Resources.btn_rec_new.gif"), LangMan.LS(LSID.MIRecordAdd), ItemAdd);

            fList = new GKListView();
            fList.HorizontalOptions = LayoutOptions.FillAndExpand;
            fList.VerticalOptions = LayoutOptions.FillAndExpand;
            fList.MouseDoubleClick += ItemEdit;
            fList.ItemSelected += List_SelectedIndexChanged;

            fToolBar = new StackLayout() {
                Orientation = StackOrientation.Vertical,
                Spacing = 4,
                Children = { fBtnAdd, fBtnEdit, fBtnDelete, fBtnLinkJump, fBtnMoveUp, fBtnMoveDown, fBtnCopy, fBtnCut, fBtnPaste },
                HorizontalOptions = LayoutOptions.End,
                VerticalOptions = LayoutOptions.FillAndExpand
            };

            Content = new StackLayout() {
                Orientation = StackOrientation.Horizontal,
                Spacing = 4,
                Children = { fList, fToolBar }
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

        public void UpdateButtons()
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
                fToolBar.IsEnabled = !fButtons.IsEmpty();
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
                fToolBar.IsEnabled = !allowedActions.IsEmpty();
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

        private void List_SelectedIndexChanged(object sender, EventArgs e)
        {
            if (fListModel != null) {
                int itemIndex = -1; // fList.SelectedIndex;
                object itemData = fList.GetSelectedData();
                if (itemData == null) return;

                fListModel.OnItemSelected(itemIndex, itemData);
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

        private async Task DoModify(ModifyEventArgs eArgs)
        {
            DoBeforeChange(eArgs);

            if (fListModel != null) {
                await fListModel.Modify(this, eArgs);

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

        private async void ItemAdd(object sender, EventArgs e)
        {
            if (fReadOnly) return;

            var eArgs = new ModifyEventArgs(RecordAction.raAdd, null);
            await DoModify(eArgs);
            RestoreSelected(eArgs.ItemData);
        }

        private async void ItemEdit(object sender, EventArgs e)
        {
            object itemData = fList.GetSelectedData();
            if (fReadOnly || itemData == null) return;

            if (!ValidateItem(itemData)) return;

            var eArgs = new ModifyEventArgs(RecordAction.raEdit, itemData);
            await DoModify(eArgs);
            RestoreSelected(eArgs.ItemData);
        }

        private async void ItemDelete(object sender, EventArgs e)
        {
            object itemData = fList.GetSelectedData();
            if (fReadOnly || itemData == null) return;

            if (!ValidateItem(itemData)) return;

            var eArgs = new ModifyEventArgs(RecordAction.raDelete, itemData);
            await DoModify(eArgs);
        }

        private async void ItemJump(object sender, EventArgs e)
        {
            object itemData = fList.GetSelectedData();
            if (itemData == null) return;

            if (!ValidateItem(itemData)) return;

            var eArgs = new ModifyEventArgs(RecordAction.raJump, itemData);
            await DoModify(eArgs);
        }

        private async void ItemMoveUp(object sender, EventArgs e)
        {
            object itemData = fList.GetSelectedData();
            if (fReadOnly || itemData == null) return;

            var eArgs = new ModifyEventArgs(RecordAction.raMoveUp, itemData);
            await DoModify(eArgs);
            RestoreSelected(eArgs.ItemData);
        }

        private async void ItemMoveDown(object sender, EventArgs e)
        {
            object itemData = fList.GetSelectedData();
            if (fReadOnly || itemData == null) return;

            var eArgs = new ModifyEventArgs(RecordAction.raMoveDown, itemData);
            await DoModify(eArgs);
            RestoreSelected(eArgs.ItemData);
        }

        private async void ItemCopy(object sender, EventArgs e)
        {
            object itemData = fList.GetSelectedData();
            if (fReadOnly || itemData == null) return;

            var eArgs = new ModifyEventArgs(RecordAction.raCopy, itemData);
            await DoModify(eArgs);
            RestoreSelected(eArgs.ItemData);
        }

        private async void ItemCut(object sender, EventArgs e)
        {
            object itemData = fList.GetSelectedData();
            if (fReadOnly || itemData == null) return;

            var eArgs = new ModifyEventArgs(RecordAction.raCut, itemData);
            await DoModify(eArgs);
        }

        private async void ItemPaste(object sender, EventArgs e)
        {
            if (fReadOnly) return;

            var eArgs = new ModifyEventArgs(RecordAction.raPaste, null);
            await DoModify(eArgs);
            RestoreSelected(eArgs.ItemData);
        }

        #endregion
    }
}
