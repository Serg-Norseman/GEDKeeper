/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using GDModel;
using GKCore;
using GKCore.Design.Controls;
using GKCore.Locales;
using Terminal.Gui;

namespace GKUI.Components
{
    /// <summary>
    /// 
    /// </summary>
    public class GKDateControl : FrameView, IDateControl, ILocalizable
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
            DateChanged?.Invoke(this, new EventArgs());
        }

        public void Activate()
        {
            SetFocus();
        }

        public void SetLocale()
        {
            int num = GKData.DateKinds.Length;
            for (int i = 0; i < num; i++) {
                //cmbDateType.Items.Add(LangMan.LS(GKData.DateKinds[i].Name));
            }

            for (GDMCalendar gc = GDMCalendar.dcGregorian; gc <= GDMCalendar.dcLast; gc++) {
                GKData.CalendarStruct cdr = GKData.DateCalendars[(int)gc];
                if (!cdr.HasSupport) continue;

                //cmbDate1Calendar.Items.Add(new GKComboItem<GDMCalendar>(LangMan.LS(cdr.Name), gc));
                //cmbDate2Calendar.Items.Add(new GKComboItem<GDMCalendar>(LangMan.LS(cdr.Name), gc));
            }

            //cmbDate1Calendar.SelectedIndex = 0;
            //cmbDate2Calendar.SelectedIndex = 0;

            //fToolTip.SetToolTip(txtDate1, txtDate1.RegionalDatePattern);
            //fToolTip.SetToolTip(txtDate2, txtDate2.RegionalDatePattern);
        }

        public void ChangeDateType()
        {
            /*int idx = cmbDateType.SelectedIndex;
            if (idx < 0 || idx >= GKData.DateKinds.Length) return;

            byte dates = GKData.DateKinds[idx].Dates;
            bool vis1 = BitHelper.IsSetBit(dates, 0);
            bool vis2 = BitHelper.IsSetBit(dates, 1);

            txtDate1.Enabled = vis1;
            cmbDate1Calendar.Enabled = vis1;
            chkBC1.Enabled = vis1;

            txtDate2.Enabled = vis2;
            cmbDate2Calendar.Enabled = vis2;
            chkBC2.Enabled = vis2;*/

            DoDateChanged();
        }

        private void cmbDateType_SelectedIndexChanged(object sender, EventArgs e)
        {
            ChangeDateType();
        }

        private void SetYear(TextValidateField txtBox, string year)
        {
            try {
                if (txtBox != null) {
                    string[] dt = txtBox.Text.ToString().Split('/');
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
            GDMDate gcd1 = GDMDate.CreateByFormattedStr(txtDate1.NormalizeDate, cal1, true);
            if (gcd1 == null) throw new ArgumentNullException("gcd1");
            gcd1.YearBC = chkBC1.Checked;

            GDMCalendar cal2 = cmbDate2Calendar.GetSelectedTag<GDMCalendar>();
            GDMDate gcd2 = GDMDate.CreateByFormattedStr(txtDate2.NormalizeDate, cal2, true);
            if (gcd2 == null) throw new ArgumentNullException("gcd2");
            gcd2.YearBC = chkBC2.Checked;

            /*switch ((GDMDateType)cmbDateType.SelectedIndex) {
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
            }*/

            return result;
        }

        private void SetDate(GDMCustomDate date)
        {
            /*if (date is GDMDateRange dtRange) {
                if (dtRange.After.StringValue == "" && dtRange.Before.StringValue != "") {
                    cmbDateType.SelectedIndex = 1; // BEF gcd2
                } else if (dtRange.After.StringValue != "" && dtRange.Before.StringValue == "") {
                    cmbDateType.SelectedIndex = 2; // AFT gcd1
                } else if (dtRange.After.StringValue != "" && dtRange.Before.StringValue != "") {
                    cmbDateType.SelectedIndex = 3; // "BET " + gcd1 + " AND " + gcd2
                }

                FillControls(1, dtRange.After);
                FillControls(2, dtRange.Before);
            } else if (date is GDMDatePeriod dtPeriod) {
                if (dtPeriod.DateFrom.StringValue == "" && dtPeriod.DateTo.StringValue != "") {
                    cmbDateType.SelectedIndex = 4; // TO gcd2
                } else if (dtPeriod.DateFrom.StringValue != "" && dtPeriod.DateTo.StringValue == "") {
                    cmbDateType.SelectedIndex = 5; // FROM gcd1
                } else if (dtPeriod.DateFrom.StringValue != "" && dtPeriod.DateTo.StringValue != "") {
                    cmbDateType.SelectedIndex = 6; // FROM gcd1 TO gcd2
                }

                FillControls(1, dtPeriod.DateFrom);
                FillControls(2, dtPeriod.DateTo);
            } else if (date is GDMDate dt) {
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
            }*/
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
                SetYear(txtDate1, value);
            } else if (txtDate2.Enabled) {
                SetYear(txtDate2, value);
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
#pragma warning disable IDE1006 // Naming Styles

        private ComboBox cmbDateType;
        private GKDateBox txtDate1;
        private GKDateBox txtDate2;
        private ComboBox cmbDate1Calendar;
        private ComboBox cmbDate2Calendar;
        private CheckBox chkBC1;
        private CheckBox chkBC2;

        private void InitializeComponent()
        {
            cmbDateType = new ComboBox();
            txtDate1 = new GKDateBox();
            txtDate2 = new GKDateBox();
            cmbDate1Calendar = new ComboBox();
            cmbDate2Calendar = new ComboBox();
            chkBC2 = new CheckBox();
            chkBC1 = new CheckBox();

            cmbDateType.ReadOnly = true;
            cmbDateType.Location = new Point(1, 1);
            cmbDateType.Size = new Size(10, 1);
            cmbDateType.TabIndex = 1;
            //cmbDateType.SelectedIndexChanged += cmbDateType_SelectedIndexChanged;

            txtDate1.Location = new Point(12, 1);
            txtDate1.Size = new Size(14, 1);
            txtDate1.TabIndex = 2;
            //txtDate1.TextChanged += txtDate_TextChanged;

            cmbDate1Calendar.ReadOnly = true;
            cmbDate1Calendar.Location = new Point(12, 3);
            cmbDate1Calendar.Size = new Size(20, 1);
            cmbDate1Calendar.TabIndex = 3;
            //cmbDate1Calendar.SelectedIndexChanged += cmbDateCalendar_SelectedIndexChanged;

            chkBC1.Location = new Point(34, 3);
            chkBC1.Size = new Size(5, 1);
            chkBC1.TabIndex = 4;
            chkBC1.Text = "BC";
            //chkBC1.CheckedChanged += chkBC_CheckedChanged;

            txtDate2.Location = new Point(46, 1);
            txtDate2.Size = new Size(14, 1);
            txtDate2.TabIndex = 5;
            //txtDate2.TextChanged += txtDate_TextChanged;

            cmbDate2Calendar.ReadOnly = true;
            cmbDate2Calendar.Location = new Point(46, 3);
            cmbDate2Calendar.Size = new Size(20, 1);
            cmbDate2Calendar.TabIndex = 6;
            //cmbDate2Calendar.SelectedIndexChanged += cmbDateCalendar_SelectedIndexChanged;

            chkBC2.Location = new Point(68, 3);
            chkBC2.Size = new Size(5, 1);
            chkBC2.TabIndex = 7;
            chkBC2.Text = "BC";
            //chkBC2.CheckedChanged += chkBC_CheckedChanged;

            Add(chkBC2);
            Add(chkBC1);
            Add(cmbDate1Calendar);
            Add(cmbDate2Calendar);
            Add(txtDate1);
            Add(txtDate2);
            Add(cmbDateType);

            Height = 5;
            Width = 20;
        }

#pragma warning restore IDE1006 // Naming Styles
        #endregion
    }
}
