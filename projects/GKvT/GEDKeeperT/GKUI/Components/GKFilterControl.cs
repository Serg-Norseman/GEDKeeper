/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using GKCore.Design.Controls;
using GKCore.Filters;
using GKCore.Locales;
using Terminal.Gui;

namespace GKUI.Components
{
    public class GKFilterControl : View, IFilterControl, ILocalizable
    {
        private QuickFilterParams fParams;

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

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
            }
            base.Dispose(disposing);
        }

        public void Activate()
        {
            SetFocus();
        }

        public void SetLocale()
        {
            //fToolTip.SetToolTip(btnSettings, LangMan.LS(LSID.MIOptions));
            //chkIndistinctMatching.Text = LangMan.LS(LSID.RM_IndistinctMatching);
        }

        private void UpdateView()
        {
            /*if (fParams == null) {
                chkIndistinctMatching.Checked = false;
                numIndistinctThreshold.Value = 100;
            } else {
                chkIndistinctMatching.Checked = (fParams.Type == MatchType.Indistinct);
                numIndistinctThreshold.Value = (int)Math.Round(fParams.IndistinctThreshold * 100);
            }
            numIndistinctThreshold.Enabled = chkIndistinctMatching.Checked;*/
        }

        private void OnParamsChanged()
        {
            /*if (fParams != null) {
                fParams.Type = !chkIndistinctMatching.Checked ? MatchType.REMask : MatchType.Indistinct;
                fParams.IndistinctThreshold = ((float)numIndistinctThreshold.Value / 100.0f);
            }

            ParamsChanged?.Invoke(this, EventArgs.Empty);*/
        }

        #region Design

        private Button btnSettings;
        private Window frmPopup;
        private CheckBox chkIndistinctMatching;
        private NumericStepper numIndistinctThreshold;

        private void InitializeComponent()
        {
            /*chkIndistinctMatching = new CheckBox();
            chkIndistinctMatching.CheckedChanged += chkIndistinctMatching_CheckedChanged;
            chkIndistinctMatching.AutoSize = true;

            numIndistinctThreshold = new NumericUpDown();
            numIndistinctThreshold.Minimum = 0;
            numIndistinctThreshold.Maximum = 100;
            numIndistinctThreshold.Width = 60;
            numIndistinctThreshold.ValueChanged += numIndistinctThreshold_ValueChanged;

            frmPopup = new Form();
            frmPopup.FormBorderStyle = FormBorderStyle.None;
            frmPopup.AutoSize = true;
            frmPopup.Size = new Size(1, 1);
            frmPopup.Controls.Add(new FlowLayoutPanel() {
                BorderStyle = BorderStyle.FixedSingle,
                Dock = DockStyle.Fill,
                Padding = new Padding(8),
                FlowDirection = FlowDirection.LeftToRight,
                WrapContents = false,
                AutoSizeMode = AutoSizeMode.GrowAndShrink,
                AutoSize = true,
                Controls = { chkIndistinctMatching, numIndistinctThreshold }
            });
            frmPopup.LostFocus += Control_LostFocus;

            btnSettings = new Button();
            btnSettings.Location = new Point(0, 0);
            btnSettings.Size = new Size(36, 32);
            btnSettings.Click += PopupButton_Click;

            Size = btnSettings.Size;
            Controls.Add(btnSettings);
            LostFocus += Control_LostFocus;*/
        }

        #endregion
    }
}
