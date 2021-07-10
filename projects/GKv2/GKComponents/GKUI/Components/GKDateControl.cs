/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2021 by Sergey V. Zhdanovskih.
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
using System.Windows.Forms;
using BSLib;
using GDModel;
using GKCore;
using GKCore.Interfaces;
using GKCore.MVP.Controls;
using GKCore.Types;

namespace GKUI.Components
{
    /// <summary>
    /// 
    /// </summary>
    public class GKDateControl : UserControl, IDateControl, ILocalizable
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
            Select();
        }

        public void SetLocale()
        {
            lblDate.Text = LangMan.LS(LSID.LSID_Date);

            int num = GKData.DateKinds.Length;
            for (int i = 0; i < num; i++) {
                cmbDateType.Items.Add(LangMan.LS(GKData.DateKinds[i].Name));
            }

            for (GDMCalendar gc = GDMCalendar.dcGregorian; gc <= GDMCalendar.dcLast; gc++) {
                GKData.CalendarStruct cdr = GKData.DateCalendars[(int)gc];
                if (!cdr.HasSupport) continue;

                cmbDate1Calendar.Items.Add(new GKListItem(LangMan.LS(cdr.Name), gc));
                cmbDate2Calendar.Items.Add(new GKListItem(LangMan.LS(cdr.Name), gc));
            }

            cmbDate1Calendar.SelectedIndex = 0;
            cmbDate2Calendar.SelectedIndex = 0;
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
            e.Effect = e.Data.GetDataPresent(typeof(string)) ? DragDropEffects.Move : DragDropEffects.None;
        }

        private void txtDateX_DragDrop(object sender, DragEventArgs e)
        {
            try {
                IDataObject data = e.Data;
                if (data.GetDataPresent(typeof(string))) {
                    string txt = data.GetData(typeof(string)) as string;

                    MaskedTextBox txtBox = ((MaskedTextBox)sender);
                    string[] dt = txtBox.Text.Split('/');
                    txtBox.Text = dt[0] + "/" + dt[1] + "/" + txt.PadLeft(4, '_');
                }
            } catch (Exception ex) {
                Logger.WriteError("GKDateControl.DragDrop()", ex);
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

            gcd1.YearBC = chkBC1.Checked;
            gcd2.YearBC = chkBC2.Checked;

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

        #region Design

        private Label lblDate;
        private ComboBox cmbDateType;
        private GKDateBox txtDate1;
        private GKDateBox txtDate2;
        private ComboBox cmbDate1Calendar;
        private ComboBox cmbDate2Calendar;
        private CheckBox chkBC1;
        private CheckBox chkBC2;

        private void InitializeComponent()
        {
            this.lblDate = new System.Windows.Forms.Label();
            this.cmbDateType = new System.Windows.Forms.ComboBox();
            this.txtDate1 = new GKUI.Components.GKDateBox();
            this.txtDate2 = new GKUI.Components.GKDateBox();
            this.cmbDate1Calendar = new System.Windows.Forms.ComboBox();
            this.cmbDate2Calendar = new System.Windows.Forms.ComboBox();
            this.chkBC2 = new System.Windows.Forms.CheckBox();
            this.chkBC1 = new System.Windows.Forms.CheckBox();
            this.SuspendLayout();
            // 
            // lblDate
            // 
            this.lblDate.AutoSize = true;
            this.lblDate.Location = new System.Drawing.Point(9, 8);
            this.lblDate.Name = "lblDate";
            this.lblDate.Size = new System.Drawing.Size(49, 17);
            this.lblDate.TabIndex = 10;
            this.lblDate.Text = "lblDate";
            // 
            // cmbDateType
            // 
            this.cmbDateType.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cmbDateType.Location = new System.Drawing.Point(9, 23);
            this.cmbDateType.Name = "cmbDateType";
            this.cmbDateType.Size = new System.Drawing.Size(135, 25);
            this.cmbDateType.TabIndex = 11;
            this.cmbDateType.SelectedIndexChanged += cmbDateType_SelectedIndexChanged;
            // 
            // txtDate1
            // 
            this.txtDate1.AllowDrop = true;
            this.txtDate1.BackColor = System.Drawing.SystemColors.Window;
            this.txtDate1.Culture = new System.Globalization.CultureInfo("");
            this.txtDate1.Location = new System.Drawing.Point(152, 23);
            this.txtDate1.Mask = "00/00/0000";
            this.txtDate1.Name = "txtDate1";
            this.txtDate1.NormalizeDate = "__.__.____";
            this.txtDate1.Size = new System.Drawing.Size(158, 24);
            this.txtDate1.TabIndex = 12;
            this.txtDate1.TextMaskFormat = System.Windows.Forms.MaskFormat.IncludePromptAndLiterals;
            this.txtDate1.DragOver += txtDateX_DragOver;
            this.txtDate1.DragDrop += txtDateX_DragDrop;
            // 
            // txtDate2
            // 
            this.txtDate2.AllowDrop = true;
            this.txtDate2.Culture = new System.Globalization.CultureInfo("");
            this.txtDate2.Location = new System.Drawing.Point(320, 23);
            this.txtDate2.Mask = "00/00/0000";
            this.txtDate2.Name = "txtDate2";
            this.txtDate2.NormalizeDate = "__.__.____";
            this.txtDate2.Size = new System.Drawing.Size(158, 24);
            this.txtDate2.TabIndex = 15;
            this.txtDate2.TextMaskFormat = System.Windows.Forms.MaskFormat.IncludePromptAndLiterals;
            this.txtDate2.DragOver += txtDateX_DragOver;
            this.txtDate2.DragDrop += txtDateX_DragDrop;
            // 
            // cmbDate1Calendar
            // 
            this.cmbDate1Calendar.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cmbDate1Calendar.Location = new System.Drawing.Point(152, 54);
            this.cmbDate1Calendar.Name = "cmbDate1Calendar";
            this.cmbDate1Calendar.Size = new System.Drawing.Size(107, 25);
            this.cmbDate1Calendar.TabIndex = 13;
            // 
            // cmbDate2Calendar
            // 
            this.cmbDate2Calendar.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cmbDate2Calendar.Location = new System.Drawing.Point(320, 54);
            this.cmbDate2Calendar.Name = "cmbDate2Calendar";
            this.cmbDate2Calendar.Size = new System.Drawing.Size(107, 25);
            this.cmbDate2Calendar.TabIndex = 16;
            // 
            // btnBC2
            // 
            this.chkBC2.AutoSize = true;
            this.chkBC2.Location = new System.Drawing.Point(434, 54);
            this.chkBC2.Name = "btnBC2";
            this.chkBC2.Size = new System.Drawing.Size(47, 21);
            this.chkBC2.TabIndex = 17;
            this.chkBC2.Text = "BC";
            this.chkBC2.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            this.chkBC2.UseVisualStyleBackColor = true;
            // 
            // btnBC1
            // 
            this.chkBC1.AutoSize = true;
            this.chkBC1.Location = new System.Drawing.Point(266, 54);
            this.chkBC1.Name = "btnBC1";
            this.chkBC1.Size = new System.Drawing.Size(47, 21);
            this.chkBC1.TabIndex = 14;
            this.chkBC1.Text = "BC";
            this.chkBC1.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            this.chkBC1.UseVisualStyleBackColor = true;
            // 
            // GKDateControl
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(120F, 120F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
            this.Controls.Add(this.chkBC2);
            this.Controls.Add(this.chkBC1);
            this.Controls.Add(this.cmbDate1Calendar);
            this.Controls.Add(this.cmbDate2Calendar);
            this.Controls.Add(this.txtDate1);
            this.Controls.Add(this.txtDate2);
            this.Controls.Add(this.cmbDateType);
            this.Controls.Add(this.lblDate);
            this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
            this.Name = "GKDateControl";
            this.Size = new System.Drawing.Size(491, 94);
            this.ResumeLayout(false);
            this.PerformLayout();
        }

        #endregion
    }
}
