/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using BSLib;
using GKCore.Design.Controls;
using GKCore.Lists;
using GKCore.Locales;
using Terminal.Gui;

namespace GKUI.Components
{
    public class GKSheetList : View, ISheetList
    {
        private readonly MenuItem fBtnAdd;
        private readonly MenuItem fBtnDelete;
        private readonly MenuItem fBtnEdit;
        private readonly MenuItem fBtnLinkJump;
        private readonly MenuItem fBtnMoveUp;
        private readonly MenuItem fBtnMoveDown;
        private readonly MenuItem fBtnCopy;
        private readonly MenuItem fBtnCut;
        private readonly MenuItem fBtnPaste;
        private readonly MenuItem fBtnDetails;
        private readonly ContextMenu fContextMenu;
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
                    //fList.ContextMenu = null;

                    fListModel = value;

                    if (fListModel != null) {
                        fList.ListMan = fListModel;
                        fListModel.SheetList = this;
                        UpdateActions();
                    }
                }
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
            fBtnDetails = CreateButton("btnView", LangMan.LS(LSID.View), ItemDetails);
            fBtnPaste = CreateButton("btnPaste", LangMan.LS(LSID.Paste), ItemPaste);
            fBtnCut = CreateButton("btnCut", LangMan.LS(LSID.Cut), ItemCut);
            fBtnCopy = CreateButton("btnCopy", LangMan.LS(LSID.Copy), ItemCopy);
            fBtnMoveDown = CreateButton("btnDown", LangMan.LS(LSID.RecordMoveDown), ItemMoveDown);
            fBtnMoveUp = CreateButton("btnUp", LangMan.LS(LSID.RecordMoveUp), ItemMoveUp);
            fBtnLinkJump = CreateButton("btnJump", LangMan.LS(LSID.RecordGoto), ItemJump);
            fBtnDelete = CreateButton("btnDelete", LangMan.LS(LSID.MIRecordDelete), ItemDelete);
            fBtnEdit = CreateButton("btnEdit", LangMan.LS(LSID.MIRecordEdit), ItemEdit);
            fBtnAdd = CreateButton("btnAdd", LangMan.LS(LSID.MIRecordAdd), ItemAdd);

            fContextMenu = new ContextMenu();
            fContextMenu.MenuItems = new MenuBarItem("Actions", new MenuItem[] {
                fBtnAdd, fBtnEdit, fBtnDelete, fBtnLinkJump, fBtnMoveUp, fBtnMoveDown, fBtnCopy, fBtnCut, fBtnPaste, fBtnDetails
            });

            fList = new GKListView();
            fList.Location = new Point(0, 0);
            fList.Width = Dim.Fill();
            fList.Height = Dim.Fill();
            fList.KeyDown += List_KeyDown;
            Add(fList);

            fList.MouseClick += (s, args) => {
                if (args.MouseEvent.Flags.HasFlag(MouseFlags.Button3Clicked)) {
                    fContextMenu.Position = new Point(args.MouseEvent.X, args.MouseEvent.Y);
                    fContextMenu.Show();
                }
            };

            Buttons = EnumSet<SheetButton>.Create(SheetButton.lbAdd, SheetButton.lbEdit, SheetButton.lbDelete);
            fListModel = null;
        }

        public GKSheetList(TabPage tab) : this()
        {
            tab.View = this;
        }

        public GKSheetList(View owner) : this()
        {
            owner.Add(this);
        }

        public void Activate()
        {
            SetFocus();
        }

        public void UpdateSheet()
        {
            UpdateButtons();
            fList.UpdateContents();
        }

        public void ApplyTheme()
        {
        }

        #region Private methods

        private MenuItem CreateButton(string name, string toolTip, EventHandler click)
        {
            var btn = new MenuItem();
            btn.Title = toolTip;
            btn.Action += click;
            btn.CanExecute = () => { return !fReadOnly; };
            return btn;
        }

        public void UpdateButtons()
        {
            /*if (fListModel == null) {
                fBtnAdd.Visible = fButtons.Contains(SheetButton.lbAdd);
                fBtnDelete.Visible = fButtons.Contains(SheetButton.lbDelete);
                fBtnEdit.Visible = fButtons.Contains(SheetButton.lbEdit);
                fBtnLinkJump.Visible = fButtons.Contains(SheetButton.lbJump);
                fBtnMoveUp.Visible = fButtons.Contains(SheetButton.lbMoveUp);
                fBtnMoveDown.Visible = fButtons.Contains(SheetButton.lbMoveDown);
                fBtnCopy.Visible = fButtons.Contains(SheetButton.lbCopy);
                fBtnCut.Visible = fButtons.Contains(SheetButton.lbCut);
                fBtnPaste.Visible = fButtons.Contains(SheetButton.lbPaste);
                fBtnDetails.Visible = fButtons.Contains(SheetButton.lbDetails);
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
                fBtnDetails.Visible = allowedActions.Contains(RecordAction.raDetails);
                //fToolBar.Visible = !allowedActions.IsEmpty();
            }*/

            UpdateActions();
        }

        private void SetReadOnly(bool value)
        {
            fReadOnly = value;
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

        private void List_DoubleClick(object sender, EventArgs e)
        {
            ItemEdit(sender, e);
        }

        private void List_KeyDown(object sender, KeyEventEventArgs e)
        {
            if (e.KeyEvent.IsCtrl) {
                var key = e.KeyEvent.Key & ~Key.CtrlMask;

                bool handled = true;
                switch (key) {
                    case Key.I:
                        ItemAdd(sender, e);
                        break;
                    case Key.L:
                        ItemDelete(sender, e);
                        break;
                    case Key.Enter:
                        ItemEdit(sender, e);
                        break;

                    case Key.C:
                        ItemCopy(sender, e);
                        break;
                    case Key.X:
                        ItemCut(sender, e);
                        break;
                    case Key.V:
                        ItemPaste(sender, e);
                        break;
                    default:
                        handled = false;
                        break;
                }
                e.Handled = handled;
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
            OnBeforeChange?.Invoke(this, eArgs);
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

            OnModify?.Invoke(this, eArgs);
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

        private void ItemDetails(object sender, EventArgs e)
        {
            object itemData = fList.GetSelectedData();
            if (fListModel != null && itemData != null) {
                fListModel.ShowDetails(itemData);
            }
        }

        private void UpdateActions()
        {
            /*if (fListModel == null) return;

            if (fListModel.CustomActions.Count > 0) {
                fContextMenu.Items.Clear();
                foreach (var custAct in fListModel.CustomActions) {
                    var miAction = new ButtonMenuItem();
                    miAction.Text = custAct.Name;
                    miAction.Click += custAct.Handler;
                    fContextMenu.Items.Add(miAction);
                }
            }

            fList.ContextMenu = (fListModel.CustomActions.Count > 0) ? fContextMenu : null;*/
        }

        #endregion
    }
}
