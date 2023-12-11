/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih.
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
using GKCore;
using GKCore.Design.Controls;
using GKCore.Interfaces;
using Xamarin.Forms;

namespace GKUI.Components
{
    public class FilterGridView : ContentView, IFilterGridView
    {
        private class FilterConditionRow : FilterCondition
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
                    return GKData.CondSigns[(int)Condition];
                }
                set {
                    Condition = fGrid.fListMan.GetCondByName(value.ToString());
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


            public object[] Values
            {
                get { return new object[] { ColumnText, ConditionText, ValueText }; }
                set { }
            }


            public FilterConditionRow(FilterGridView grid, FilterCondition filterCondition)
                : base(filterCondition.ColumnIndex, filterCondition.Condition, filterCondition.Value)
            {
                fGrid = grid;
            }
        }


        private readonly Button fBtnAdd;
        private readonly Button fBtnDelete;
        private readonly GKListView fGridView;
        private readonly GKComboBox fFieldCombo;
        private readonly GKComboBox fConditionCombo;
        private readonly Entry fValueText;

        private IRecordsListModel fListMan;
        private ObservableCollection<FilterConditionRow> fCollection;
        private string[] fFields;


        public int Count
        {
            get { return fCollection.Count; }
        }

        public FilterCondition this[int index]
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

        public bool Enabled { get; set; }

        public FilterGridView()
        {
            GKData.CondSigns[6] = LangMan.LS(LSID.CondContains);
            GKData.CondSigns[7] = LangMan.LS(LSID.CondNotContains);

            fBtnDelete = CreateButton("btnDelete", UIHelper.LoadResourceImage("Resources.btn_rec_delete.gif"), LangMan.LS(LSID.MIRecordDelete), ItemDelete);
            fBtnAdd = CreateButton("btnAdd", UIHelper.LoadResourceImage("Resources.btn_rec_new.gif"), LangMan.LS(LSID.MIRecordAdd), ItemAdd);

            fGridView = new GKListView();
            fGridView.VerticalOptions = LayoutOptions.FillAndExpand;
            fGridView.ItemSelected += GridView_ItemSelected;

            fFieldCombo = new GKComboBox() { WidthRequest = 200 };

            fConditionCombo = new GKComboBox() { WidthRequest = 150 };
            fConditionCombo.AddRange(GKData.CondSigns);

            fValueText = new Entry() { WidthRequest = 300 };

            var editPanel = new StackLayout() {
                Orientation = StackOrientation.Horizontal,
                Spacing = 4,
                Children = { fFieldCombo, fConditionCombo, fValueText },
                VerticalOptions = LayoutOptions.End
            };

            var mainPanel = new StackLayout() {
                Orientation = StackOrientation.Vertical,
                Spacing = 4,
                Children = { fGridView, editPanel },
                HorizontalOptions = LayoutOptions.FillAndExpand
            };

            var toolbar = new StackLayout() {
                Orientation = StackOrientation.Vertical,
                Spacing = 4,
                Children = { fBtnAdd, fBtnDelete },
                HorizontalOptions = LayoutOptions.End
            };

            Content = new StackLayout() {
                Orientation = StackOrientation.Horizontal,
                Spacing = 4,
                Children = { mainPanel, toolbar }
            };

            fCollection = new ObservableCollection<FilterConditionRow>();
            fGridView.ItemsSource = fCollection;
            fGridView.AllowMultipleSelection = false;
        }

        private void GridView_ItemSelected(object sender, SelectedItemChangedEventArgs e)
        {
            if (e.SelectedItem != null) {
                var row = e.SelectedItem as FilterConditionRow;

                fFieldCombo.Text = row.ColumnText.ToString();
                fConditionCombo.Text = row.ConditionText.ToString();
                fValueText.Text = row.ValueText;
            }
        }

        public void AddCondition(FilterCondition fcond)
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

        #region Private functions

        private Button CreateButton(string name, ImageSource image, string toolTip, EventHandler click)
        {
            var btn = new Button();
            //btn.Style = "iconBtn";
            btn.ImageSource = image;
            btn.Clicked += click;
            return btn;
        }

        private void InitGrid()
        {
            fGridView.ClearColumns();
            fGridView.AddColumn(LangMan.LS(LSID.Field), 200);
            fGridView.AddColumn(LangMan.LS(LSID.Condition), 150);
            fGridView.AddColumn(LangMan.LS(LSID.Value), 300);

            fFieldCombo.AddRange(fFields);

            /*fGridView.AddComboColumn<FilterConditionRow>(LangMan.LS(LSID.Field), fFields, r => r.ColumnText, 200, false, true);
            fGridView.AddComboColumn<FilterConditionRow>(LangMan.LS(LSID.Condition), GKData.CondSigns, r => r.ConditionText, 150, false, true);
            fGridView.AddTextColumn<FilterConditionRow>(LangMan.LS(LSID.Value), r => r.ValueText, 300, false, true);*/
        }

        private void ItemAdd(object sender, EventArgs e)
        {
            var columnId = fListMan.GetFieldColumnId(fFields, fFieldCombo.Text);
            var condition = fListMan.GetCondByName(fConditionCombo.Text);
            var value = fValueText.Text;

            AddCondition(new FilterCondition(columnId, condition, value));
        }

        private void ItemDelete(object sender, EventArgs e)
        {
            RemoveCondition(fGridView.SelectedIndex);
        }

        #endregion
    }
}
