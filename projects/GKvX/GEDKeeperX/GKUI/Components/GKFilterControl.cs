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
using GKCore.Design.Controls;
using GKCore.Lists;
using GKCore.Locales;
using Xamarin.Forms;

namespace GKUI.Components
{
    public class GKFilterControl : ContentView, IFilterControl, ILocalizable
    {
        private bool fPopupShown;
        private QuickFilterParams fParams;

        public bool Enabled
        {
            get { return base.IsEnabled; }
            set { base.IsEnabled = value; }
        }

        public bool Visible
        {
            get { return base.IsVisible; }
            set { base.IsVisible = value; }
        }

        public QuickFilterParams Params
        {
            get { return fParams; }
            set {
                fParams = value;
                UpdateView();
            }
        }

        public event EventHandler ParamsChanged;

        public GKFilterControl()
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
            /*btnSettings.ToolTip = LangMan.LS(LSID.MIOptions);
            chkIndistinctMatching.Text = LangMan.LS(LSID.RM_IndistinctMatching);*/
        }

        private void Control_LostFocus(object sender, EventArgs e)
        {
            /*if (fPopupShown && !frmPopup.HasFocus) {
                frmPopup.Visible = false;
                fPopupShown = !fPopupShown;
            }*/
        }

        private void PopupButton_Click(object sender, EventArgs e)
        {
            /*if (!fPopupShown) {
                //frmPopup.Width = this.Width;
                //fPopupForm.Height = 100;
                frmPopup.Location = Point.Round(PointToScreen(Content.Bounds.BottomLeft));
                frmPopup.Visible = true;
            } else {
                frmPopup.Visible = false;
            }
            fPopupShown = !fPopupShown;*/
        }

        private void chkIndistinctMatching_CheckedChanged(object sender, EventArgs e)
        {
            //numIndistinctThreshold.Enabled = chkIndistinctMatching.Checked.Value;
            OnParamsChanged();
        }

        private void numIndistinctThreshold_ValueChanged(object sender, EventArgs e)
        {
            OnParamsChanged();
        }

        private void UpdateView()
        {
            if (fParams == null) {
                //chkIndistinctMatching.Checked = false;
                //numIndistinctThreshold.Value = 100.0f;
            } else {
                //chkIndistinctMatching.Checked = (fParams.Type == MatchType.Indistinct);
                //numIndistinctThreshold.Value = Math.Round(fParams.IndistinctThreshold * 100);
            }
        }

        private void OnParamsChanged()
        {
            if (fParams != null) {
                //fParams.Type = !chkIndistinctMatching.Checked.Value ? MatchType.REMask : MatchType.Indistinct;
                //fParams.IndistinctThreshold = (float)(numIndistinctThreshold.Value / 100.0f);
            }

            var handler = ParamsChanged;
            if (handler != null) {
                handler(this, EventArgs.Empty);
            }
        }

        #region Design

        private Button btnSettings;
        //private Form frmPopup;
        //private CheckBox chkIndistinctMatching;
        //private NumericStepper numIndistinctThreshold;

        private void InitializeComponent()
        {
            //SuspendLayout();

            /*chkIndistinctMatching = new CheckBox();
            chkIndistinctMatching.CheckedChanged += chkIndistinctMatching_CheckedChanged;

            numIndistinctThreshold = new NumericStepper();
            numIndistinctThreshold.MinValue = 0;
            numIndistinctThreshold.MaxValue = 100;
            numIndistinctThreshold.Width = 60;
            numIndistinctThreshold.ValueChanged += numIndistinctThreshold_ValueChanged;

            frmPopup = new Form();
            frmPopup.WindowStyle = WindowStyle.None;
            frmPopup.ShowActivated = false;
            frmPopup.Resizable = false;
            //fPopupForm.CanFocus = false;
            frmPopup.ShowInTaskbar = false;
            frmPopup.Content = new Scrollable() {
                Border = BorderType.Bezel,
                Content = new StackLayout() {
                    Padding = 8,
                    Spacing = 8,
                    Orientation = Orientation.Horizontal,
                    Items = {
                        chkIndistinctMatching,
                        numIndistinctThreshold
                    }
                }
            };*/

            btnSettings = new Button();
            //btnSettings.Style = "iconBtn";
            btnSettings.ImageSource = UIHelper.LoadResourceImage("Resources.btn_tools.gif");
            btnSettings.Clicked += PopupButton_Click;

            Content = btnSettings;
            //LostFocus += Control_LostFocus;

            //ResumeLayout();
        }

        #endregion
    }
}
