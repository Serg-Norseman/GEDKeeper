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
using System.Collections.ObjectModel;
using Eto.Drawing;
using Eto.Forms;
using GKCore;
using GKCore.Design.Controls;
using GKCore.Filters;
using GKCore.Lists;
using GKCore.Locales;
using GKUI.Platform;
using GKUI.Themes;

namespace GKUI.Components
{
    public class FilterGridView : Panel, IFilterGridView
    {
        private class FilterConditionRow : ColumnConditionExpression
        {
            private readonly FilterGridView fGrid;

            public object ColumnText
            {
                get {
                    return fGrid.fFields[ColumnIndex + 1];
                }
                set {
                    ColumnIndex = fGrid.fListMan.GetFieldColumnId(fGrid.fFields, value.ToString());
                }
            }

            public object ConditionText
            {
                get {
                    return GKData.CondSigns[(int)Operator];
                }
                set {
                    Operator = ListFilter.GetCondByName(value.ToString());
                }
            }

            public string ValueText
            {
                get {
                    return Value.ToString();
                }
                set {
                    Value = value;
                }
            }


            public FilterConditionRow(FilterGridView grid, ColumnConditionExpression filterCondition)
                : base(filterCondition.ColumnIndex, filterCondition.Operator, filterCondition.Value)
            {
                fGrid = grid;
            }
        }


        private readonly Button fBtnAdd;
        private readonly Button fBtnDelete;
        private readonly GKGridView fGridView;

        private IRecordsListModel fListMan;
        private ObservableCollection<FilterConditionRow> fCollection;
        private string[] fFields;


        public int Count
        {
            get { return fCollection.Count; }
        }

        public ColumnConditionExpression this[int index]
        {
            get { return fCollection[index]; }
        }

        public IRecordsListModel ListMan
        {
            get {
                return fListMan;
            }
            set {
                fListMan = value;
                fFields = fListMan.CreateFields();
                InitGrid();
            }
        }

        public Color TextColor { get; set; }


        public FilterGridView()
        {
            GKData.CondSigns[6] = LangMan.LS(LSID.CondContains);
            GKData.CondSigns[7] = LangMan.LS(LSID.CondNotContains);

            fBtnDelete = CreateButton("btnDelete", LangMan.LS(LSID.MIRecordDelete), ItemDelete);
            fBtnAdd = CreateButton("btnAdd", LangMan.LS(LSID.MIRecordAdd), ItemAdd);

            fGridView = new GKGridView();

            SuspendLayout();
            var toolbar = new StackLayout() {
                Orientation = Orientation.Vertical,
                Spacing = EtoAppConsts.ToolButtonSpacing,
                Items = { fBtnAdd, fBtnDelete }
            };
            Content = new TableLayout() {
                Spacing = new Size(4, 4),
                Rows = {
                    new TableRow() {
                        Cells = {
                            new TableCell(fGridView, true),
                            new TableCell(toolbar, false)
                        }
                    }
                }
            };
            ResumeLayout();

            fCollection = new ObservableCollection<FilterConditionRow>();
            fGridView.DataStore = fCollection;

            fGridView.AllowColumnReordering = false;
            fGridView.AllowMultipleSelection = false;
            // [Gtk] Selection of the last (or only) row does not work on left click; EtoForms issue #2443
            fGridView.AllowEmptySelection = false;
        }

        public void AddCondition(ColumnConditionExpression fcond)
        {
            fCollection.Add(new FilterConditionRow(this, fcond));
        }

        public void RemoveCondition(int index)
        {
            if (index >= 0 && index < fCollection.Count) {
                fCollection.RemoveAt(index);
            }
        }

        public void Clear()
        {
            fCollection.Clear();
        }

        public void Activate()
        {
            Focus();
        }

        public void ApplyTheme()
        {
            fGridView.BackgroundColor = this.BackgroundColor;
            fGridView.TextColor = this.TextColor;

            UIHelper.SetButtonThemeImage(fBtnDelete, ThemeElement.Glyph_ItemDelete);
            UIHelper.SetButtonThemeImage(fBtnAdd, ThemeElement.Glyph_ItemAdd);
        }

        #region Private functions

        private Button CreateButton(string name, string toolTip, EventHandler<EventArgs> click)
        {
            var btn = new Button();
            btn.Style = "iconBtn";
            btn.ToolTip = toolTip;
            btn.Click += click;
            return btn;
        }

        private void InitGrid()
        {
            fGridView.Columns.Clear();
            // https://github.com/picoe/Eto/issues/2546
            // WPF ComboBoxCell: only elements within the rows bounding box can be selected
            fGridView.AddComboColumn<FilterConditionRow>(LangMan.LS(LSID.Field), fFields, r => r.ColumnText, 200, false, true);
            fGridView.AddComboColumn<FilterConditionRow>(LangMan.LS(LSID.Condition), GKData.CondSigns, r => r.ConditionText, 150, false, true);
            fGridView.AddTextColumn<FilterConditionRow>(LangMan.LS(LSID.Value), r => r.ValueText, 300, false, true);
        }

        private void ItemAdd(object sender, EventArgs e)
        {
            var fcond = new ColumnConditionExpression(0, ConditionOperator.Contains, "");
            AddCondition(fcond);
        }

        private void ItemDelete(object sender, EventArgs e)
        {
            RemoveCondition(fGridView.SelectedRow);
        }

        #endregion

        protected override void OnMouseDown(MouseEventArgs e)
        {
            // does not receive focus without this handler,
            // without focus does not receive keyboard events
            if (!HasFocus) {
                Focus();
            }
            base.OnMouseDown(e);
        }

        protected override void OnKeyDown(KeyEventArgs e)
        {
            switch (e.Key) {
                case Keys.Enter:
                    if (!fGridView.IsEditing) {
                    }
                    break;

                case Keys.I:
                    if (e.Control) {
                        ItemAdd(null, null);
                    }
                    break;

                case Keys.D:
                    if (e.Control) {
                        ItemDelete(null, null);
                    }
                    break;

                default:
                    base.OnKeyDown(e);
                    break;
            }
        }
    }
}
