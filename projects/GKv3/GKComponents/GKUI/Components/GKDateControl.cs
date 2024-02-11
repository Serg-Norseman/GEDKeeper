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
using BSLib;
using Eto.Drawing;
using Eto.Forms;
using GDModel;
using GKCore;
using GKCore.Design.Controls;
using GKCore.Interfaces;
using GKCore.Types;

namespace GKUI.Components
{
    /// <summary>
    /// 
    /// </summary>
    public class GKDateControl : Panel, IDateControl, ILocalizable
    {
        private GDMDateType fFixedDateType;


        public GDMCustomDate Date
        {
            get { return GetDate(); }
            set { SetDate(value); }
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

            for (GDMCalendar gc = GDMCalendar.dcGregorian; gc <= GDMCalendar.dcLast; gc++) {
                GKData.CalendarStruct cdr = GKData.DateCalendars[(int)gc];
                if (!cdr.HasSupport) continue;

                cmbDate1Calendar.Items.Add(new GKComboItem<GDMCalendar>(LangMan.LS(cdr.Name), gc));
                cmbDate2Calendar.Items.Add(new GKComboItem<GDMCalendar>(LangMan.LS(cdr.Name), gc));
            }

            cmbDate1Calendar.SelectedIndex = 0;
            cmbDate2Calendar.SelectedIndex = 0;

            txtDate1.ToolTip = txtDate1.RegionalDatePattern;
            txtDate2.ToolTip = txtDate2.RegionalDatePattern;
        }

        public void ChangeDateType()
        {
            int idx = cmbDateType.SelectedIndex;
            if (idx < 0 || idx >= GKData.DateKinds.Length) return;

            byte dates = GKData.DateKinds[idx].Dates;
            bool vis1 = BitHelper.IsSetBit(dates, 0);
            bool vis2 = BitHelper.IsSetBit(dates, 1);

            txtDate1.Enabled = vis1;
            cmbDate1Calendar.Enabled = vis1;
            chkBC1.Enabled = vis1;

            txtDate2.Enabled = vis2;
            cmbDate2Calendar.Enabled = vis2;
            chkBC2.Enabled = vis2;

            DoDateChanged();
        }

        private void cmbDateType_SelectedIndexChanged(object sender, EventArgs e)
        {
            ChangeDateType();
        }

        private void txtDateX_DragOver(object sender, DragEventArgs e)
        {
            // FIXME: don't work in Eto 2.4.1?
            var data = e.Data.Text;
            e.Effects = !string.IsNullOrEmpty(data) ? DragEffects.Move : DragEffects.None;
        }

        private void txtDateX_DragDrop(object sender, DragEventArgs e)
        {
            // FIXME: don't work in Eto 2.4.1?
            try {
                /*if (e.Data.GetDataPresent(typeof(string))) {
                    string txt = e.Data.GetData(typeof(string)) as string;
                    string[] dt = ((MaskedTextBox)sender).Text.Split('.');
                    ((MaskedTextBox)sender).Text = dt[0] + '.' + dt[1] + '.' + txt;
                }*/
            } catch (Exception ex) {
                Logger.WriteError("GKDateControl.DragDrop()", ex);
            }
        }

        private GDMCustomDate GetDate()
        {
            GDMCustomDate result = null;

            GDMCalendar cal1 = cmbDate1Calendar.GetSelectedTag<GDMCalendar>();
            GDMDate gcd1 = GDMDate.CreateByFormattedStr(txtDate1.NormalizeDate, cal1, true);
            if (gcd1 == null) throw new ArgumentNullException("gcd1");
            gcd1.YearBC = chkBC1.Checked.Value;

            GDMCalendar cal2 = cmbDate2Calendar.GetSelectedTag<GDMCalendar>();
            GDMDate gcd2 = GDMDate.CreateByFormattedStr(txtDate2.NormalizeDate, cal2, true);
            if (gcd2 == null) throw new ArgumentNullException("gcd2");
            gcd2.YearBC = chkBC2.Checked.Value;

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
                chkBC1.Checked = false;
            }

            if (fFixedDateType != GDMDateType.None) {
                cmbDateType.SelectedIndex = (int)fFixedDateType;
                cmbDateType.Enabled = false;
            } else {
                cmbDateType.Enabled = true;
            }
        }

        private void FillControls(int dateIndex, GDMDate date)
        {
            switch (dateIndex) {
                case 1:
                    txtDate1.NormalizeDate = date.GetDisplayString(DateFormat.dfDD_MM_YYYY);
                    cmbDate1Calendar.SetSelectedTag(date.DateCalendar);
                    chkBC1.Checked = date.YearBC;
                    break;

                case 2:
                    txtDate2.NormalizeDate = date.GetDisplayString(DateFormat.dfDD_MM_YYYY);
                    cmbDate2Calendar.SetSelectedTag(date.DateCalendar);
                    chkBC2.Checked = date.YearBC;
                    break;
            }
        }

        public void PasteValue(string value)
        {
            if (txtDate1.Enabled) {
                txtDate1.SetYear(value);
            } else if (txtDate2.Enabled) {
                txtDate2.SetYear(value);
            }
        }

        private void chkBC_CheckedChanged(object sender, EventArgs e)
        {
            DoDateChanged();
        }

        private void txtDate_TextChanged(object sender, EventArgs e)
        {
            DoDateChanged();
        }

        private void cmbDateCalendar_SelectedIndexChanged(object sender, EventArgs e)
        {
            DoDateChanged();
        }

        #region Design

        private ComboBox cmbDateType;
        private GKDateBox txtDate1;
        private GKDateBox txtDate2;
        private ComboBox cmbDate1Calendar;
        private ComboBox cmbDate2Calendar;
        private CheckBox chkBC1;
        private CheckBox chkBC2;

        private void InitializeComponent()
        {
            SuspendLayout();

            cmbDateType = new ComboBox();
            cmbDateType.ReadOnly = true;
            cmbDateType.SelectedIndexChanged += cmbDateType_SelectedIndexChanged;

            txtDate1 = new GKDateBox();
            txtDate1.AllowDrop = true;
            txtDate1.DragOver += txtDateX_DragOver;
            txtDate1.DragDrop += txtDateX_DragDrop;
            txtDate1.CalcMode = true;
            txtDate1.TextChanged += txtDate_TextChanged;

            txtDate2 = new GKDateBox();
            txtDate2.AllowDrop = true;
            txtDate2.DragOver += txtDateX_DragOver;
            txtDate2.DragDrop += txtDateX_DragDrop;
            txtDate2.CalcMode = true;
            txtDate2.TextChanged += txtDate_TextChanged;

            cmbDate1Calendar = new ComboBox();
            cmbDate1Calendar.ReadOnly = true;
            cmbDate1Calendar.SelectedIndexChanged += cmbDateCalendar_SelectedIndexChanged;

            cmbDate2Calendar = new ComboBox();
            cmbDate2Calendar.ReadOnly = true;
            cmbDate2Calendar.SelectedIndexChanged += cmbDateCalendar_SelectedIndexChanged;

            chkBC1 = new CheckBox();
            chkBC1.Text = "BC";
            chkBC1.CheckedChanged += chkBC_CheckedChanged;

            chkBC2 = new CheckBox();
            chkBC2.Text = "BC";
            chkBC2.CheckedChanged += chkBC_CheckedChanged;

            Content = new TableLayout {
                Padding = new Padding(0),
                Spacing = new Size(4, 4),
                Rows = {
                    new TableRow {
                        Cells = { cmbDateType, txtDate1, txtDate2 }
                    },
                    new TableRow {
                        Cells = { null,
                            new StackLayout() { Orientation = Orientation.Horizontal, Spacing = 4, Items = { cmbDate1Calendar, chkBC1 } },
                            new StackLayout() { Orientation = Orientation.Horizontal, Spacing = 4, Items = { cmbDate2Calendar, chkBC2 } }
                        }
                    },
                }
            };

            ResumeLayout();
        }

        #endregion
    }
}
