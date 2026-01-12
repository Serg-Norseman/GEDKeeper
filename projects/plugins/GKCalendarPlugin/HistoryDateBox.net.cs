/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Globalization;
using Eto.Drawing;
using Eto.Forms;
using GKCore.Calendar;
using GKCore.Locales;
using GKCore.Plugins;
using GKUI.Components;

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

    public class HistoryDateBox : Panel
    {
        private static readonly PLS[] fCalendarNames = new PLS[] {
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
                    cmbCalendar.Items.Add(new GKComboItem<Calendar>(fLangMan.LS(fCalendarNames[(int)hc]), hc));
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
                bool bc = chkBC.Checked.Value;

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
                        cmbMonth.Items.Add(new GKComboItem<int>(months[m], m));
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
            SuspendLayout();

            lblCalendar = new Label();
            cmbCalendar = new ComboBox();
            cmbCalendar.ReadOnly = true;
            cmbCalendar.SelectedIndexChanged += cmbCalendar_SelectedIndexChanged;

            lblDay = new Label();
            txtDay = new MaskedTextBox();
            txtDay.Provider = new FixedMaskedTextProvider("00", CultureInfo.InvariantCulture);
            txtDay.TextChanged += field_TextChanged;

            lblMonth = new Label();
            cmbMonth = new ComboBox();
            cmbMonth.ReadOnly = true;
            cmbMonth.SelectedIndexChanged += field_TextChanged;

            lblYear = new Label();
            txtYear = new MaskedTextBox();
            txtYear.Provider = new FixedMaskedTextProvider("0000", CultureInfo.InvariantCulture);
            txtYear.TextChanged += field_TextChanged;

            chkBC = new CheckBox();
            chkBC.CheckedChanged += field_TextChanged;

            Content = new TableLayout() {
                Padding = new Padding(4),
                Spacing = new Size(4, 4),
                Rows = {
                    new TableRow() {
                        Cells = { lblCalendar, null, null, null }
                    },
                    new TableRow() {
                        Cells = { cmbCalendar, null, null, null }
                    },
                    new TableRow() {
                        Cells = { lblDay, lblMonth, lblYear, null }
                    },
                    new TableRow() {
                        Cells = { txtDay, cmbMonth, txtYear, chkBC }
                    }
                }
            };
            Padding = new Padding(2);

            ResumeLayout();
        }

        #endregion
    }
}
