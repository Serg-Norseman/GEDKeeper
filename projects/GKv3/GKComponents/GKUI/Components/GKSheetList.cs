/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using BSLib;
using Eto.Drawing;
using Eto.Forms;
using GKCore.Design.Controls;
using GKCore.Lists;
using GKCore.Locales;
using GKUI.Platform;
using GKUI.Themes;

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
        private readonly Button fBtnDetails;
        private readonly ContextMenu fContextMenu;
        private readonly GKListView fList;
        private readonly StackLayout fToolbar;

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
                    fList.ContextMenu = null;

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
            fContextMenu.Items.AddRange(new MenuItem[] { });

            fList = new GKListView();
            fList.MouseDoubleClick += List_DoubleClick;
            fList.SelectedItemsChanged += List_SelectedIndexChanged;

            // empty GridView does not always receive events
            this.KeyDown += List_KeyDown;

            fToolbar = new StackLayout() {
                Orientation = Orientation.Vertical,
                Spacing = EtoAppConsts.ToolButtonSpacing,
                Items = { fBtnAdd, fBtnEdit, fBtnDelete, fBtnLinkJump, fBtnMoveUp, fBtnMoveDown, fBtnCopy, fBtnCut, fBtnPaste, fBtnDetails }
            };

            Content = new TableLayout() {
                Spacing = new Size(2, 0),
                Rows = {
                    new TableRow() {
                        Cells = {
                            new TableCell(fList, true),
                            new TableCell(fToolbar, false)
                        }
                    }
                }
            };

            Buttons = EnumSet<SheetButton>.Create(SheetButton.lbAdd, SheetButton.lbEdit, SheetButton.lbDelete);
            fListModel = null;
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                if (fListModel != null) {
                    fListModel.SheetList = null;
                    fListModel = null;
                }

                fList.ContextMenu = null;
                fContextMenu.Dispose();

                fList.Dispose();
                fBtnDetails.Dispose();
                fBtnPaste.Dispose();
                fBtnCut.Dispose();
                fBtnCopy.Dispose();
                fBtnLinkJump.Dispose();
                fBtnMoveUp.Dispose();
                fBtnMoveDown.Dispose();
                fBtnDelete.Dispose();
                fBtnEdit.Dispose();
                fBtnAdd.Dispose();
                fToolbar.Dispose();
            }
            base.Dispose(disposing);
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

        public void ApplyTheme()
        {
            fToolbar.BackgroundColor = this.BackgroundColor;
            for (int i = 0, num = fToolbar.Items.Count; i < num; i++)
                fToolbar.Items[i].Control.BackgroundColor = this.BackgroundColor;

            UIHelper.SetButtonThemeImage(fBtnDetails, ThemeElement.Glyph_View);
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

        #region Private methods

        private static Button CreateButton(string name, string toolTip, EventHandler<EventArgs> click)
        {
            var btn = new Button();
            btn.Style = "iconBtn";
            btn.ToolTip = toolTip;
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
            }

            UpdateActions();
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
            if (e.Control) {
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
            if (fListModel == null) return;

            if (fListModel.CustomActions.Count > 0) {
                fContextMenu.Items.Clear();
                foreach (var custAct in fListModel.CustomActions) {
                    var miAction = new ButtonMenuItem();
                    miAction.Text = custAct.Name;
                    miAction.Click += custAct.Handler;
                    fContextMenu.Items.Add(miAction);
                }
            }

            fList.ContextMenu = (fListModel.CustomActions.Count > 0) ? fContextMenu : null;
        }

        #endregion
    }
}
