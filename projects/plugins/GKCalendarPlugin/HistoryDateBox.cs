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
using System.Windows.Forms;
using GKCore.Calendar;
using GKCore.Design.Controls;
using GKCore.Interfaces;
using GKCore.Plugins;

namespace GKCalendarPlugin
{
    public enum Calendar
    {
        Gregorian, Julian, Byzantine,
        Hebrew, Islamic,
        Persian, Indian,
        //Bahai,

        First = Gregorian,
        Last = Indian
    }

    public class HistoryDateBox : UserControl
    {
        private static PLS[] fCalendarNames = new PLS[] {
            PLS.Cal_Gregorian, PLS.Cal_Julian, PLS.Cal_Byzantine,
            PLS.Cal_Hebrew, PLS.Cal_Islamic,
            PLS.Cal_Persian, PLS.Cal_Indian,
            //PLS.Cal_Bahai,
        };


        private ILangMan fLangMan;
        private bool fReadOnly;


        public Calendar Calendar
        {
            get { return (Calendar)cmbCalendar.SelectedIndex; }
        }

        public double Date
        {
            get { return GetDate(); }
            set { SetDate(value); }
        }

        public bool ReadOnly
        {
            get { return fReadOnly; }
            set {
                fReadOnly = value;
                txtDay.ReadOnly = fReadOnly;
                cmbMonth.Enabled = !fReadOnly;
                txtYear.ReadOnly = fReadOnly;
                chkBC.Enabled = !fReadOnly;
            }
        }


        public event EventHandler CalendarChanged;

        public event EventHandler DateChanged;


        public HistoryDateBox()
        {
            InitializeComponent();

            SetLocale();

            cmbCalendar.SelectedIndex = (cmbCalendar.Items.Count > 0) ? 0 : -1;
        }

        public void SetLocale()
        {
            try {
                // in designtime calls to GlobalOptions throws exceptions
                fLangMan = PluginsMan.CreateLangMan(this.GetType().Assembly);

                lblCalendar.Text = fLangMan.LS(PLS.Calendar);
                lblDay.Text = fLangMan.LS(PLS.Day);
                lblMonth.Text = fLangMan.LS(PLS.Month);
                lblYear.Text = fLangMan.LS(PLS.Year);
                chkBC.Text = "B.C.";

                for (Calendar hc = Calendar.First; hc <= Calendar.Last; hc++) {
                    cmbCalendar.Items.Add(new ComboItem<Calendar>(fLangMan.LS(fCalendarNames[(int)hc]), hc));
                }
            } catch {
            }
        }

        private double GetDate()
        {
            double result = double.NaN;

            try {
                Calendar hc = (Calendar)cmbCalendar.SelectedIndex;

                int day = int.Parse(txtDay.Text);
                int month = cmbMonth.SelectedIndex + 1;
                int year = int.Parse(txtYear.Text);
                bool bc = chkBC.Checked;

                switch (hc) {
                    case Calendar.Gregorian:
                        result = CalendarConverter.gregorian_to_jd(year, month, day);
                        break;
                    case Calendar.Julian:
                        result = CalendarConverter.julian_to_jd(year, month, day);
                        break;
                    case Calendar.Byzantine:
                        //result = CalendarConverter.byzantine_to_jd(year, month, day);
                        break;
                    case Calendar.Hebrew:
                        result = CalendarConverter.hebrew_to_jd(year, month, day);
                        break;
                    case Calendar.Islamic:
                        result = CalendarConverter.islamic_to_jd(year, month, day);
                        break;
                    case Calendar.Persian:
                        result = CalendarConverter.persian_to_jd(year, month, day);
                        break;
                    case Calendar.Indian:
                        result = CalendarConverter.indian_civil_to_jd(year, month, day);
                        break;
                    //case Calendar.Bahai:
                        //result = CalendarConverter.bahai_to_jd(year, month, day);
                        //break;
                }
            } catch {
            }

            return result;
        }

        private void SetDate(double value)
        {
            try {
                Calendar hc = (Calendar)cmbCalendar.SelectedIndex;
                int day = 0, month = 1, year = 0;
                bool bc = false;

                if (!double.IsNaN(value)) {
                    switch (hc) {
                        case Calendar.Gregorian:
                            CalendarConverter.jd_to_gregorian(value, out year, out month, out day);
                            break;
                        case Calendar.Julian:
                            CalendarConverter.jd_to_julian(value, out year, out month, out day);
                            break;
                        case Calendar.Byzantine:
                            CalendarConverter.jd_to_byzantine(value, out year, out month, out day, CalendarConverter.ByzantineStyle.March);
                            break;
                        case Calendar.Hebrew:
                            CalendarConverter.jd_to_hebrew(value, out year, out month, out day);
                            break;
                        case Calendar.Islamic:
                            CalendarConverter.jd_to_islamic(value, out year, out month, out day);
                            break;
                        case Calendar.Persian:
                            CalendarConverter.jd_to_persian(value, out year, out month, out day);
                            break;
                        case Calendar.Indian:
                            CalendarConverter.jd_to_indian_civil(value, out year, out month, out day);
                            break;
                        //case Calendar.Bahai:
                            //result = CalendarConverter.bahai_to_jd(value, out year, out month, out day);
                            //break;
                    }
                }

                txtDay.Text = string.Format("{0:00}", day);
                cmbMonth.SelectedIndex = month - 1;
                txtYear.Text = string.Format("{0:0000}", year);
                chkBC.Checked = bc;
            } catch {
            }
        }

        private void OnCalendarChanged()
        {
            CalendarChanged?.Invoke(this, new EventArgs());
        }

        private void OnDateChanged()
        {
            DateChanged?.Invoke(this, new EventArgs());
        }

        private void cmbCalendar_SelectedIndexChanged(object sender, EventArgs e)
        {
            int month = cmbMonth.SelectedIndex;

            try {
                Calendar hc = (Calendar)cmbCalendar.SelectedIndex;

                string[] months = null;
                switch (hc) {
                    case Calendar.Gregorian:
                    case Calendar.Julian:
                        months = CalendarData.InitNames(fLangMan.LS(PLS.ClassicMonths));
                        break;
                    case Calendar.Byzantine:
                        months = CalendarData.InitNames(fLangMan.LS(PLS.ByzantineMonths));
                        break;
                    case Calendar.Hebrew:
                        months = CalendarData.InitNames(fLangMan.LS(PLS.HebrewMonths));
                        break;
                    case Calendar.Islamic:
                        months = CalendarData.InitNames(fLangMan.LS(PLS.IslamicMonths));
                        break;
                    case Calendar.Persian:
                        months = CalendarData.InitNames(fLangMan.LS(PLS.PersianMonths));
                        break;
                    case Calendar.Indian:
                        months = CalendarData.InitNames(fLangMan.LS(PLS.IndianCivilMonths));
                        break;
                    //case Calendar.Bahai:
                        //months = CalendarData.InitNames(fLangMan.LS(PLS.BahaiMonths));
                        //break;
                }

                if (months != null) {
                    cmbMonth.Items.Clear();
                    for (int m = 0; m < months.Length; m++) {
                        cmbMonth.Items.Add(new ComboItem<int>(months[m], m));
                    }
                }
            } catch {
            }

            cmbMonth.SelectedIndex = month;

            OnCalendarChanged();
            OnDateChanged();
        }

        private void field_TextChanged(object sender, EventArgs e)
        {
            OnDateChanged();
        }

        #region Design

        private TableLayoutPanel layout;
        private Label lblCalendar;
        private Label lblDay;
        private Label lblMonth;
        private Label lblYear;
        private MaskedTextBox txtDay;
        private MaskedTextBox txtYear;
        private ComboBox cmbMonth;
        private ComboBox cmbCalendar;
        private CheckBox chkBC;

        private void InitializeComponent()
        {
            this.layout = new System.Windows.Forms.TableLayoutPanel();
            this.lblCalendar = new System.Windows.Forms.Label();
            this.lblDay = new System.Windows.Forms.Label();
            this.lblMonth = new System.Windows.Forms.Label();
            this.lblYear = new System.Windows.Forms.Label();
            this.txtDay = new System.Windows.Forms.MaskedTextBox();
            this.txtYear = new System.Windows.Forms.MaskedTextBox();
            this.cmbMonth = new System.Windows.Forms.ComboBox();
            this.cmbCalendar = new System.Windows.Forms.ComboBox();
            this.chkBC = new System.Windows.Forms.CheckBox();
            this.layout.SuspendLayout();
            this.SuspendLayout();
            // 
            // layout
            // 
            this.layout.AutoSize = true;
            this.layout.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
            this.layout.ColumnCount = 4;
            this.layout.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle());
            this.layout.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle());
            this.layout.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle());
            this.layout.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle());
            this.layout.Controls.Add(this.lblCalendar, 0, 0);
            this.layout.Controls.Add(this.lblDay, 0, 2);
            this.layout.Controls.Add(this.lblMonth, 1, 2);
            this.layout.Controls.Add(this.lblYear, 2, 2);
            this.layout.Controls.Add(this.txtDay, 0, 3);
            this.layout.Controls.Add(this.txtYear, 2, 3);
            this.layout.Controls.Add(this.cmbMonth, 1, 3);
            this.layout.Controls.Add(this.cmbCalendar, 0, 1);
            this.layout.Controls.Add(this.chkBC, 3, 3);
            this.layout.Location = new System.Drawing.Point(3, 3);
            this.layout.Margin = new System.Windows.Forms.Padding(0);
            this.layout.Name = "layout";
            this.layout.Padding = new System.Windows.Forms.Padding(2);
            this.layout.RowCount = 4;
            this.layout.RowStyles.Add(new System.Windows.Forms.RowStyle());
            this.layout.RowStyles.Add(new System.Windows.Forms.RowStyle());
            this.layout.RowStyles.Add(new System.Windows.Forms.RowStyle());
            this.layout.RowStyles.Add(new System.Windows.Forms.RowStyle());
            this.layout.Size = new System.Drawing.Size(441, 100);
            this.layout.TabIndex = 0;
            // 
            // lblCalendar
            // 
            this.lblCalendar.AutoSize = true;
            this.layout.SetColumnSpan(this.lblCalendar, 2);
            this.lblCalendar.Location = new System.Drawing.Point(5, 2);
            this.lblCalendar.Name = "lblCalendar";
            this.lblCalendar.Size = new System.Drawing.Size(73, 17);
            this.lblCalendar.TabIndex = 0;
            this.lblCalendar.Text = "lblCalendar";
            // 
            // lblDay
            // 
            this.lblDay.AutoSize = true;
            this.lblDay.Location = new System.Drawing.Point(5, 50);
            this.lblDay.Name = "lblDay";
            this.lblDay.Size = new System.Drawing.Size(45, 17);
            this.lblDay.TabIndex = 1;
            this.lblDay.Text = "lblDay";
            // 
            // lblMonth
            // 
            this.lblMonth.AutoSize = true;
            this.lblMonth.Location = new System.Drawing.Point(56, 50);
            this.lblMonth.Name = "lblMonth";
            this.lblMonth.Size = new System.Drawing.Size(59, 17);
            this.lblMonth.TabIndex = 2;
            this.lblMonth.Text = "lblMonth";
            // 
            // lblYear
            // 
            this.lblYear.AutoSize = true;
            this.lblYear.Location = new System.Drawing.Point(302, 50);
            this.lblYear.Name = "lblYear";
            this.lblYear.Size = new System.Drawing.Size(47, 17);
            this.lblYear.TabIndex = 3;
            this.lblYear.Text = "lblYear";
            // 
            // txtDay
            // 
            this.txtDay.Location = new System.Drawing.Point(5, 70);
            this.txtDay.Mask = "00";
            this.txtDay.Name = "txtDay";
            this.txtDay.Size = new System.Drawing.Size(45, 24);
            this.txtDay.TabIndex = 4;
            this.txtDay.TextChanged += new System.EventHandler(this.field_TextChanged);
            // 
            // txtYear
            // 
            this.txtYear.Location = new System.Drawing.Point(302, 70);
            this.txtYear.Mask = "0000";
            this.txtYear.Name = "txtYear";
            this.txtYear.Size = new System.Drawing.Size(59, 24);
            this.txtYear.TabIndex = 5;
            this.txtYear.ValidatingType = typeof(int);
            this.txtYear.TextChanged += new System.EventHandler(this.field_TextChanged);
            // 
            // cmbMonth
            // 
            this.cmbMonth.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.cmbMonth.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cmbMonth.FormattingEnabled = true;
            this.cmbMonth.Location = new System.Drawing.Point(56, 70);
            this.cmbMonth.Name = "cmbMonth";
            this.cmbMonth.Size = new System.Drawing.Size(240, 25);
            this.cmbMonth.TabIndex = 6;
            this.cmbMonth.SelectedIndexChanged += new System.EventHandler(this.field_TextChanged);
            // 
            // cmbCalendar
            // 
            this.cmbCalendar.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.layout.SetColumnSpan(this.cmbCalendar, 2);
            this.cmbCalendar.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cmbCalendar.FormattingEnabled = true;
            this.cmbCalendar.Location = new System.Drawing.Point(5, 22);
            this.cmbCalendar.Name = "cmbCalendar";
            this.cmbCalendar.Size = new System.Drawing.Size(291, 25);
            this.cmbCalendar.TabIndex = 7;
            this.cmbCalendar.SelectedIndexChanged += new System.EventHandler(this.cmbCalendar_SelectedIndexChanged);
            // 
            // chkBC
            // 
            this.chkBC.AutoSize = true;
            this.chkBC.Location = new System.Drawing.Point(367, 70);
            this.chkBC.Name = "chkBC";
            this.chkBC.Size = new System.Drawing.Size(69, 21);
            this.chkBC.TabIndex = 8;
            this.chkBC.Text = "chkBC";
            this.chkBC.UseVisualStyleBackColor = true;
            this.chkBC.CheckedChanged += new System.EventHandler(this.field_TextChanged);
            // 
            // HistoryDateBox
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(8F, 17F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.AutoSize = true;
            this.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
            this.Controls.Add(this.layout);
            this.Font = new System.Drawing.Font("Tahoma", 8.2F);
            this.Name = "HistoryDateBox";
            this.Padding = new System.Windows.Forms.Padding(2);
            this.Size = new System.Drawing.Size(446, 104);
            this.layout.ResumeLayout(false);
            this.layout.PerformLayout();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion
    }
}
