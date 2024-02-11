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
using System.Collections.Generic;
using BSLib;
using GDModel;
using GKCore;
using GKCore.Design.Controls;
using GKCore.Interfaces;
using GKCore.Types;
using Xamarin.Forms;
using XFIKCheckBox = Plugin.InputKit.Shared.Controls.CheckBox;

namespace GKUI.Components
{
    /// <summary>
    /// 
    /// </summary>
    public class GKDateControl : ContentView, IDateControl, ILocalizable
    {
        private GDMDateType fFixedDateType;


        public GDMCustomDate Date
        {
            get { return GetDate(); }
            set { SetDate(value); }
        }

        public bool Enabled
        {
            get { return base.IsEnabled; }
            set { base.IsEnabled = value; }
        }

        public event EventHandler DateChanged;

        public GDMDateType FixedDateType
        {
            get { return fFixedDateType; }
            set { fFixedDateType = value; }
        }


        public GKDateControl()
        {
            fFixedDateType = GDMDateType.None;

            InitializeComponent();
            SetLocale();
        }

        private void DoDateChanged()
        {
            var eventHandler = DateChanged;
            if (eventHandler != null)
                eventHandler(this, new EventArgs());
        }

        public void Activate()
        {
            Focus();
        }

        public void SetLocale()
        {
            int num = GKData.DateKinds.Length;
            for (int i = 0; i < num; i++) {
                cmbDateType.Items.Add(LangMan.LS(GKData.DateKinds[i].Name));
            }

            var calendars = new List<IComboItem>();
            for (GDMCalendar gc = GDMCalendar.dcGregorian; gc <= GDMCalendar.dcLast; gc++) {
                GKData.CalendarStruct cdr = GKData.DateCalendars[(int)gc];
                if (!cdr.HasSupport) continue;

                calendars.Add(new GKComboItem<GDMCalendar>(LangMan.LS(cdr.Name), gc));
                calendars.Add(new GKComboItem<GDMCalendar>(LangMan.LS(cdr.Name), gc));
            }
            cmbDate1Calendar.ItemsSource = calendars;
            cmbDate2Calendar.ItemsSource = calendars;

            cmbDate1Calendar.SelectedIndex = 0;
            cmbDate2Calendar.SelectedIndex = 0;

            txtDate1.Placeholder = txtDate1.RegionalDatePattern;
            txtDate2.Placeholder = txtDate2.RegionalDatePattern;
        }

        public void ChangeDateType()
        {
            int idx = cmbDateType.SelectedIndex;
            if (idx < 0 || idx >= GKData.DateKinds.Length) return;

            byte dates = GKData.DateKinds[idx].Dates;
            bool vis1 = BitHelper.IsSetBit(dates, 0);
            bool vis2 = BitHelper.IsSetBit(dates, 1);

            txtDate1.IsEnabled = vis1;
            cmbDate1Calendar.IsEnabled = vis1;
            chkBC1.IsEnabled = vis1;

            txtDate2.IsEnabled = vis2;
            cmbDate2Calendar.IsEnabled = vis2;
            chkBC2.IsEnabled = vis2;

            DoDateChanged();
        }

        private void cmbDateType_SelectedIndexChanged(object sender, EventArgs e)
        {
            ChangeDateType();
        }

        private GDMCustomDate GetDate()
        {
            GDMCustomDate result = null;

            GDMCalendar cal1 = cmbDate1Calendar.GetSelectedTag<GDMCalendar>();
            GDMDate gcd1 = GDMDate.CreateByFormattedStr(txtDate1.NormalizeDate, cal1, true);
            if (gcd1 == null) throw new ArgumentNullException("gcd1");
            gcd1.YearBC = chkBC1.IsChecked;

            GDMCalendar cal2 = cmbDate2Calendar.GetSelectedTag<GDMCalendar>();
            GDMDate gcd2 = GDMDate.CreateByFormattedStr(txtDate2.NormalizeDate, cal2, true);
            if (gcd2 == null) throw new ArgumentNullException("gcd2");
            gcd2.YearBC = chkBC2.IsChecked;

            switch ((GDMDateType)cmbDateType.SelectedIndex) {
                case GDMDateType.Exact:
                    result = gcd1;
                    break;

                case GDMDateType.Before: // BEF gcd2
                    result = GDMCustomDate.CreateRange(null, gcd2);
                    break;

                case GDMDateType.After: // AFT gcd1
                    result = GDMCustomDate.CreateRange(gcd1, null);
                    break;

                case GDMDateType.Between: // "BET " + gcd1 + " AND " + gcd2
                    result = GDMCustomDate.CreateRange(gcd1, gcd2);
                    break;

                case GDMDateType.PeriodTo: // TO gcd2
                    result = GDMCustomDate.CreatePeriod(null, gcd2);
                    break;

                case GDMDateType.PeriodFrom: // FROM gcd1
                    result = GDMCustomDate.CreatePeriod(gcd1, null);
                    break;

                case GDMDateType.PeriodBetween: // FROM gcd1 TO gcd2
                    result = GDMCustomDate.CreatePeriod(gcd1, gcd2);
                    break;

                case GDMDateType.About: // ABT gcd1
                    result = GDMCustomDate.CreateApproximated(gcd1, GDMApproximated.daAbout);
                    break;

                case GDMDateType.Calculated: // CAL gcd1
                    result = GDMCustomDate.CreateApproximated(gcd1, GDMApproximated.daCalculated);
                    break;

                case GDMDateType.Estimated: // EST gcd1
                    result = GDMCustomDate.CreateApproximated(gcd1, GDMApproximated.daEstimated);
                    break;
            }

            return result;
        }

        private void SetDate(GDMCustomDate date)
        {
            if (date is GDMDateRange) {
                GDMDateRange dtRange = date as GDMDateRange;

                if (dtRange.After.StringValue == "" && dtRange.Before.StringValue != "") {
                    cmbDateType.SelectedIndex = 1; // BEF gcd2
                } else if (dtRange.After.StringValue != "" && dtRange.Before.StringValue == "") {
                    cmbDateType.SelectedIndex = 2; // AFT gcd1
                } else if (dtRange.After.StringValue != "" && dtRange.Before.StringValue != "") {
                    cmbDateType.SelectedIndex = 3; // "BET " + gcd1 + " AND " + gcd2
                }

                FillControls(1, dtRange.After);
                FillControls(2, dtRange.Before);
            } else if (date is GDMDatePeriod) {
                GDMDatePeriod dtPeriod = date as GDMDatePeriod;

                if (dtPeriod.DateFrom.StringValue == "" && dtPeriod.DateTo.StringValue != "") {
                    cmbDateType.SelectedIndex = 4; // TO gcd2
                } else if (dtPeriod.DateFrom.StringValue != "" && dtPeriod.DateTo.StringValue == "") {
                    cmbDateType.SelectedIndex = 5; // FROM gcd1
                } else if (dtPeriod.DateFrom.StringValue != "" && dtPeriod.DateTo.StringValue != "") {
                    cmbDateType.SelectedIndex = 6; // FROM gcd1 TO gcd2
                }

                FillControls(1, dtPeriod.DateFrom);
                FillControls(2, dtPeriod.DateTo);
            } else if (date is GDMDate) {
                var dt = date as GDMDate;

                switch (dt.Approximated) {
                    case GDMApproximated.daExact:
                        cmbDateType.SelectedIndex = 0;
                        break;
                    case GDMApproximated.daAbout:
                        cmbDateType.SelectedIndex = 7;
                        break;
                    case GDMApproximated.daCalculated:
                        cmbDateType.SelectedIndex = 8;
                        break;
                    case GDMApproximated.daEstimated:
                        cmbDateType.SelectedIndex = 9;
                        break;
                }

                FillControls(1, dt);
            } else {
                cmbDateType.SelectedIndex = 0;
                txtDate1.NormalizeDate = "";
                cmbDate1Calendar.SetSelectedTag(GDMCalendar.dcGregorian);
                chkBC1.IsChecked = false;
            }

            if (fFixedDateType != GDMDateType.None) {
                cmbDateType.SelectedIndex = (int)fFixedDateType;
                cmbDateType.IsEnabled = false;
            } else {
                cmbDateType.IsEnabled = true;
            }
        }

        private void FillControls(int dateIndex, GDMDate date)
        {
            switch (dateIndex) {
                case 1:
                    txtDate1.NormalizeDate = date.GetDisplayString(DateFormat.dfDD_MM_YYYY);
                    cmbDate1Calendar.SetSelectedTag(date.DateCalendar);
                    chkBC1.IsChecked = date.YearBC;
                    break;

                case 2:
                    txtDate2.NormalizeDate = date.GetDisplayString(DateFormat.dfDD_MM_YYYY);
                    cmbDate2Calendar.SetSelectedTag(date.DateCalendar);
                    chkBC2.IsChecked = date.YearBC;
                    break;
            }
        }

        public void PasteValue(string value)
        {
            if (txtDate1.IsEnabled) {
                txtDate1.SetYear(value);
            } else if (txtDate2.IsEnabled) {
                txtDate2.SetYear(value);
            }
        }

        private void chkBC_CheckedChanged(object sender, EventArgs e)
        {
            DoDateChanged();
        }

        private void txtDate_TextChanged(object sender, TextChangedEventArgs e)
        {
            DoDateChanged();
        }

        private void cmbDateCalendar_SelectedIndexChanged(object sender, EventArgs e)
        {
            DoDateChanged();
        }

        #region Design

        private Picker cmbDateType;
        private GKDateBox txtDate1;
        private GKDateBox txtDate2;
        private Picker cmbDate1Calendar;
        private Picker cmbDate2Calendar;
        private XFIKCheckBox chkBC1;
        private XFIKCheckBox chkBC2;

        private void InitializeComponent()
        {
            //SuspendLayout();

            cmbDateType = new Picker();
            //cmbDateType.ReadOnly = true;
            cmbDateType.SelectedIndexChanged += cmbDateType_SelectedIndexChanged;

            txtDate1 = new GKDateBox();
            /*txtDate1.AllowDrop = true;
            txtDate1.DragOver += txtDateX_DragOver;
            txtDate1.DragDrop += txtDateX_DragDrop;
            txtDate1.CalcMode = true;*/
            txtDate1.TextChanged += txtDate_TextChanged;

            txtDate2 = new GKDateBox();
            /*txtDate2.AllowDrop = true;
            txtDate2.DragOver += txtDateX_DragOver;
            txtDate2.DragDrop += txtDateX_DragDrop;
            txtDate2.CalcMode = true;*/
            txtDate2.TextChanged += txtDate_TextChanged;

            cmbDate1Calendar = new Picker();
            //cmbDate1Calendar.ReadOnly = true;
            cmbDate1Calendar.SelectedIndexChanged += cmbDateCalendar_SelectedIndexChanged;

            cmbDate2Calendar = new Picker();
            //cmbDate2Calendar.ReadOnly = true;
            cmbDate2Calendar.SelectedIndexChanged += cmbDateCalendar_SelectedIndexChanged;

            chkBC1 = new XFIKCheckBox();
            chkBC1.Text = "BC";
            chkBC1.CheckChanged += chkBC_CheckedChanged;

            chkBC2 = new XFIKCheckBox();
            chkBC2.Text = "BC";
            chkBC2.CheckChanged += chkBC_CheckedChanged;

            var grid = new Grid {
                Padding = new Thickness(0),
                ColumnSpacing = 4,
                RowSpacing = 4,
                ColumnDefinitions = {
                    new ColumnDefinition { Width = GridLength.Auto },
                    new ColumnDefinition { Width = GridLength.Auto },
                    new ColumnDefinition { Width = GridLength.Auto },
                },
            };
            grid.Children.Add(cmbDateType, 0, 0);
            grid.Children.Add(txtDate1, 1, 0);
            grid.Children.Add(txtDate2, 2, 0);
            grid.Children.Add(new StackLayout() { Orientation = StackOrientation.Horizontal, Spacing = 4, Children = { cmbDate1Calendar, chkBC1 } }, 1, 1);
            grid.Children.Add(new StackLayout() { Orientation = StackOrientation.Horizontal, Spacing = 4, Children = { cmbDate2Calendar, chkBC2 } }, 2, 1);
            Content = grid;

            //ResumeLayout();
        }

        #endregion
    }
}
