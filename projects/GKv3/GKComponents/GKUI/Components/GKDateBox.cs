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
using System.Globalization;
using Eto.Drawing;
using Eto.Forms;
using GKCore;

namespace GKUI.Components
{
    /// <summary>
    /// 
    /// </summary>
    public class GKDateBox : Panel
    {
        private static readonly string fRegionalDatePattern;
        private static string fSourceYear;

        private bool fPopupShown;


        public new Color BackgroundColor
        {
            get {
                return txtMaskedDate.BackgroundColor;
            }
            set {
                txtMaskedDate.BackgroundColor = value;
            }
        }

        public bool CalcMode
        {
            get { return btnPopup.Visible; }
            set { btnPopup.Visible = value; }
        }

        public string NormalizeDate
        {
            get { return GKUtils.GetNormalizeDate(txtMaskedDate.Text, fRegionalDatePattern); }
            set { txtMaskedDate.Text = GKUtils.GetRegionalDate(value, fRegionalDatePattern); }
        }

        public bool ReadOnly
        {
            get { return txtMaskedDate.ReadOnly; }
            set {
                btnPopup.Enabled = !value;
                txtMaskedDate.ReadOnly = value;
            }
        }

        public string RegionalDatePattern
        {
            get { return fRegionalDatePattern; }
        }

        public string SelectedText
        {
            get { return txtMaskedDate.SelectedText; }
            set { txtMaskedDate.SelectedText = value; }
        }

        public string Text
        {
            get { return txtMaskedDate.Text; }
            set { txtMaskedDate.Text = value; }
        }

        public Color TextColor
        {
            get {
                return txtMaskedDate.TextColor;
            }
            set {
                txtMaskedDate.TextColor = value;
            }
        }

        public event EventHandler<EventArgs> TextChanged
        {
            add {
                txtMaskedDate.Properties.AddHandlerEvent("TextControl.TextChanged", value);
            }
            remove {
                txtMaskedDate.Properties.RemoveEvent("TextControl.TextChanged", value);
            }
        }


        static GKDateBox()
        {
            fRegionalDatePattern = GKUtils.GetShortDatePattern();
            Logger.WriteInfo(string.Format("RegionalDatePattern: {0}", fRegionalDatePattern));
        }

        public GKDateBox()
        {
            InitializeComponent();

            // "/" - Date separator based on the specified Culture for the mask
            var mask = GKUtils.GetDateMask(fRegionalDatePattern).Replace("/", @"\/");
            txtMaskedDate.Provider = new FixedMaskedTextProvider(mask, CultureInfo.InvariantCulture);
        }

        private void txtYear_TextChanged(object sender, EventArgs e)
        {
            int sourceYear, ageAtYear;
            if (!int.TryParse(txtSourceYear.Text, out sourceYear)) {
                sourceYear = 0;
                fSourceYear = string.Empty;
            } else {
                fSourceYear = txtSourceYear.Text;
            }
            if (!int.TryParse(txtAgeAtYear.Text, out ageAtYear)) {
                ageAtYear = 0;
            }

            if (sourceYear > 0 && ageAtYear > 0) {
                int dateYear = sourceYear - ageAtYear;
                SetYear(dateYear.ToString());
            } else {
                SetYear("");
            }
        }

        private void GKDateBox_LostFocus(object sender, EventArgs e)
        {
            if (fPopupShown && !frmPopup.HasFocus) {
                frmPopup.Visible = false;
                fPopupShown = !fPopupShown;
            }
        }

        private void PopupButton_Click(object sender, EventArgs e)
        {
            if (!fPopupShown) {
                txtSourceYear.Text = fSourceYear;

                frmPopup.Width = this.Width;
                //fPopupForm.Height = 100;
                frmPopup.Location = Point.Round(PointToScreen(Content.Bounds.BottomLeft));
                frmPopup.Visible = true;
            } else {
                frmPopup.Visible = false;
            }
            fPopupShown = !fPopupShown;
        }

        public void SetYear(string year)
        {
            try {
                string strVal = NormalizeDate;
                string[] parts = strVal.Split('.');
                NormalizeDate = parts[0] + "." + parts[1] + "." + year.PadLeft(4, '_');
            } catch (Exception ex) {
                Logger.WriteError("GKDateBox.SetYear()", ex);
            }
        }

        public void SelectAll()
        {
            txtMaskedDate.SelectAll();
        }

        #region Design

        private Button btnPopup;
        private Form frmPopup;
        private MaskedTextBox txtMaskedDate;
        private TextBox txtSourceYear;
        private TextBox txtAgeAtYear;

        private void InitializeComponent()
        {
            SuspendLayout();

            txtSourceYear = new TextBox();
            txtSourceYear.TextChanging += txtYear_TextChanged;
            txtSourceYear.MaxLength = 4;
            txtSourceYear.Width = 30;

            txtAgeAtYear = new TextBox();
            txtAgeAtYear.TextChanged += txtYear_TextChanged;
            txtAgeAtYear.MaxLength = 3;
            txtAgeAtYear.Width = 30;

            frmPopup = new Form();
            frmPopup.WindowStyle = WindowStyle.None;
            frmPopup.ShowActivated = false;
            frmPopup.Resizable = false;
            //fPopupForm.CanFocus = false;
            frmPopup.ShowInTaskbar = false;
            frmPopup.Content = new Scrollable() {
                Border = BorderType.Bezel,
                Content = new TabControl() {
                    Pages = {
                        new TabPage() {
                            Text = LangMan.LS(LSID.Source),
                            Content = new TableLayout() {
                                Spacing = new Size(4, 4),
                                Rows = {
                                    new TableRow() {
                                        Cells = {
                                            new Label() { Text = LangMan.LS(LSID.SourceYear) },
                                            txtSourceYear
                                        }
                                    },
                                    new TableRow() {
                                        Cells = {
                                            new Label() { Text = LangMan.LS(LSID.AgeAtYear) },
                                            txtAgeAtYear
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            };

            btnPopup = new Button();
            btnPopup.Text = "⯆";
            btnPopup.Width = 26;
            btnPopup.Click += PopupButton_Click;
            btnPopup.Visible = false;

            txtMaskedDate = new MaskedTextBox();
            txtMaskedDate.SizeChanged += (object sender, EventArgs e) => {
                btnPopup.Height = txtMaskedDate.Height;
            };

            Content = new StackLayout() {
                Orientation = Orientation.Horizontal,
                Items = {
                    new StackLayoutItem(txtMaskedDate, true),
                    new StackLayoutItem(btnPopup, false)
                }
            };
            LostFocus += GKDateBox_LostFocus;

            ResumeLayout();
        }

        #endregion
    }
}
