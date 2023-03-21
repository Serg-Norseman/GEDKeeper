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
        public GDMCustomDate Date
        {
            get { return GetDate(); }
            set { SetDate(value); }
        }


        public GKDateControl()
        {
            InitializeComponent();
            SetLocale();
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

        private void SetYear(MaskedTextBox txtBox, string year)
        {
            try {
                if (txtBox != null) {
                    string[] dt = txtBox.Text.Split('/');
                    txtBox.Text = dt[0] + "/" + dt[1] + "/" + year.PadLeft(4, '_');
                }
            } catch (Exception ex) {
                Logger.WriteError("GKDateControl.SetYear()", ex);
            }
        }

        private GDMCustomDate GetDate()
        {
            GDMCustomDate result = null;

            GDMCalendar cal1 = cmbDate1Calendar.GetSelectedTag<GDMCalendar>();
            GDMCalendar cal2 = cmbDate2Calendar.GetSelectedTag<GDMCalendar>();

            GDMDate gcd1 = GDMDate.CreateByFormattedStr(txtDate1.NormalizeDate, cal1, true);
            if (gcd1 == null) throw new ArgumentNullException("gcd1");

            GDMDate gcd2 = GDMDate.CreateByFormattedStr(txtDate2.NormalizeDate, cal2, true);
            if (gcd2 == null) throw new ArgumentNullException("gcd2");

            gcd1.YearBC = chkBC1.Checked.GetValueOrDefault();
            gcd2.YearBC = chkBC2.Checked.GetValueOrDefault();

            switch (cmbDateType.SelectedIndex) {
                case 0:
                    result = gcd1;
                    break;

                case 1: // BEF gcd2
                    result = GDMCustomDate.CreateRange(null, gcd2);
                    break;

                case 2: // AFT gcd1
                    result = GDMCustomDate.CreateRange(gcd1, null);
                    break;

                case 3: // "BET " + gcd1 + " AND " + gcd2
                    result = GDMCustomDate.CreateRange(gcd1, gcd2);
                    break;

                case 4: // FROM gcd1
                    result = GDMCustomDate.CreatePeriod(gcd1, null);
                    break;

                case 5: // TO gcd2
                    result = GDMCustomDate.CreatePeriod(null, gcd2);
                    break;

                case 6: // FROM gcd1 TO gcd2
                    result = GDMCustomDate.CreatePeriod(gcd1, gcd2);
                    break;

                case 7: // ABT gcd1
                    result = GDMCustomDate.CreateApproximated(gcd1, GDMApproximated.daAbout);
                    break;

                case 8: // CAL gcd1
                    result = GDMCustomDate.CreateApproximated(gcd1, GDMApproximated.daCalculated);
                    break;

                case 9: // EST gcd1
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
                    cmbDateType.SelectedIndex = 1;
                } else if (dtRange.After.StringValue != "" && dtRange.Before.StringValue == "") {
                    cmbDateType.SelectedIndex = 2;
                } else if (dtRange.After.StringValue != "" && dtRange.Before.StringValue != "") {
                    cmbDateType.SelectedIndex = 3;
                }

                txtDate1.NormalizeDate = dtRange.After.GetDisplayString(DateFormat.dfDD_MM_YYYY);
                txtDate2.NormalizeDate = dtRange.Before.GetDisplayString(DateFormat.dfDD_MM_YYYY);
                cmbDate1Calendar.SetSelectedTag<GDMCalendar>(dtRange.After.DateCalendar);
                cmbDate2Calendar.SetSelectedTag<GDMCalendar>(dtRange.Before.DateCalendar);
                chkBC1.Checked = dtRange.After.YearBC;
                chkBC2.Checked = dtRange.Before.YearBC;
            } else if (date is GDMDatePeriod) {
                GDMDatePeriod dtPeriod = date as GDMDatePeriod;

                if (dtPeriod.DateFrom.StringValue != "" && dtPeriod.DateTo.StringValue == "") {
                    cmbDateType.SelectedIndex = 4;
                } else if (dtPeriod.DateFrom.StringValue == "" && dtPeriod.DateTo.StringValue != "") {
                    cmbDateType.SelectedIndex = 5;
                } else if (dtPeriod.DateFrom.StringValue != "" && dtPeriod.DateTo.StringValue != "") {
                    cmbDateType.SelectedIndex = 6;
                }

                txtDate1.NormalizeDate = dtPeriod.DateFrom.GetDisplayString(DateFormat.dfDD_MM_YYYY);
                txtDate2.NormalizeDate = dtPeriod.DateTo.GetDisplayString(DateFormat.dfDD_MM_YYYY);
                cmbDate1Calendar.SetSelectedTag<GDMCalendar>(dtPeriod.DateFrom.DateCalendar);
                cmbDate2Calendar.SetSelectedTag<GDMCalendar>(dtPeriod.DateTo.DateCalendar);
                chkBC1.Checked = dtPeriod.DateFrom.YearBC;
                chkBC2.Checked = dtPeriod.DateTo.YearBC;
            } else if (date is GDMDate) {
                GDMApproximated approximated = (date as GDMDate).Approximated;

                switch (approximated) {
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

                txtDate1.NormalizeDate = (date as GDMDate).GetDisplayString(DateFormat.dfDD_MM_YYYY);
                cmbDate1Calendar.SetSelectedTag<GDMCalendar>((date as GDMDate).DateCalendar);
                chkBC1.Checked = (date as GDMDate).YearBC;
            } else {
                cmbDateType.SelectedIndex = 0;
                txtDate1.NormalizeDate = "";
                cmbDate1Calendar.SetSelectedTag<GDMCalendar>(GDMCalendar.dcGregorian);
                chkBC1.Checked = false;
            }
        }

        public void PasteValue(string value)
        {
            if (txtDate1.Enabled) {
                SetYear(txtDate1, value);
            } else if (txtDate2.Enabled) {
                SetYear(txtDate2, value);
            }
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
            this.cmbDateType = new ComboBox();
            this.txtDate1 = new GKUI.Components.GKDateBox();
            this.txtDate2 = new GKUI.Components.GKDateBox();
            this.cmbDate1Calendar = new ComboBox();
            this.cmbDate2Calendar = new ComboBox();
            this.chkBC2 = new CheckBox();
            this.chkBC1 = new CheckBox();

            this.SuspendLayout();

            this.cmbDateType.ReadOnly = true;
            this.cmbDateType.SelectedIndexChanged += cmbDateType_SelectedIndexChanged;

            this.txtDate1.AllowDrop = true;
            this.txtDate1.DragOver += txtDateX_DragOver;
            this.txtDate1.DragDrop += txtDateX_DragDrop;

            this.txtDate2.AllowDrop = true;
            this.txtDate2.DragOver += txtDateX_DragOver;
            this.txtDate2.DragDrop += txtDateX_DragDrop;

            this.cmbDate1Calendar.ReadOnly = true;

            this.cmbDate2Calendar.ReadOnly = true;

            this.chkBC2.Text = "BC";

            this.chkBC1.Text = "BC";

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

            this.ResumeLayout();
        }

        #endregion
    }
}
