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
using Terminal.Gui.ViewBase;
using Terminal.Gui.Views;

namespace GKUI.Components
{
    public class GKSheetList : View, ISheetList
    {
        private readonly PopoverMenu fContextMenu;
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
            fContextMenu = new PopoverMenu();

            fList = new GKListView();
            fList.Width = Dim.Fill();
            fList.Height = Dim.Fill();
            Add(fList);

            Buttons = EnumSet<SheetButton>.Create(SheetButton.lbAdd, SheetButton.lbEdit, SheetButton.lbDelete);
            fListModel = null;
        }

        public GKSheetList(Tab tab) : this()
        {
            tab.View = this;
        }

        public void Activate()
        {
            //Focus();
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

        private static Button CreateButton(string name, string toolTip, EventHandler<EventArgs> click)
        {
            /*var btn = new Button();
            btn.Style = "iconBtn";
            btn.ToolTip = toolTip;
            btn.Click += click;
            return btn;*/
            return null;
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
            /*fBtnAdd.Enabled = !fReadOnly;
            fBtnDelete.Enabled = !fReadOnly;
            fBtnEdit.Enabled = !fReadOnly;
            fBtnMoveUp.Enabled = !fReadOnly;
            fBtnMoveDown.Enabled = !fReadOnly;
            fBtnCopy.Enabled = !fReadOnly;
            fBtnCut.Enabled = !fReadOnly;
            fBtnPaste.Enabled = !fReadOnly;

            fList.BackgroundColor = (fReadOnly) ? SystemColors.Control : SystemColors.WindowBackground;*/
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

        private void List_KeyDown(object sender, KeyEventArgs e)
        {
            /*if (e.Control) {
                bool handled = true;
                switch (e.Key) {
                    case Keys.I:
                        ItemAdd(sender, e);
                        break;
                    case Keys.L:
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
                    default:
                        handled = false;
                        break;
                }
                e.Handled = handled;
            }*/
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
