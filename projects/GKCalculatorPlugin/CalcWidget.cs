/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
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
using System.Drawing;
using System.Windows.Forms;

using GKCommon;
using GKCore.Interfaces;

namespace GKCalculatorPlugin
{
    /// <summary>
    /// 
    /// </summary>
    public partial class CalcWidget : Form, ILocalization
    {
        private readonly Plugin fPlugin;
        private readonly ExpCalculator fCalc;

        public CalcWidget(Plugin plugin) : base()
        {
            this.InitializeComponent();

            this.fPlugin = plugin;

            Screen scr = Screen.PrimaryScreen;
            this.Location = new Point(scr.WorkingArea.Width - this.Width - 10, scr.WorkingArea.Height - this.Height - 10);

            this.fCalc = new ExpCalculator();
            this.lbOutput.Items.Clear();

            this.SetLang();
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                //this.fCalc.Dispose();
            }
            base.Dispose(disposing);
        }

        private void CalcWidget_Load(object sender, EventArgs e)
        {
            this.fPlugin.Host.WidgetShow(this.fPlugin);
        }

        private void CalcWidget_Closed(object sender, EventArgs e)
        {
            this.fPlugin.Host.WidgetClose(this.fPlugin);
        }

        private void edExpression_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.KeyCode == Keys.Return)
            {
                string res;
                try
                {
                    res = this.fCalc.Calc(this.edExpression.Text).ToString();
                    if (this.chkPutToClipboard.Checked)
                    {
                        Clipboard.SetDataObject(res);
                    }
                }
                catch (Exception)
                {
                    res = "[ ??? ]";
                }
                this.lbOutput.Items.Add("> " + this.edExpression.Text);
                this.lbOutput.Items.Add("= " + res);
                this.lbOutput.SelectedIndex = this.lbOutput.Items.Count - 1;
                this.edCalcResult.Text = res;
            }
        }

        private void edCalcResult_MouseMove(object sender, MouseEventArgs e)
        {
            if (e.Button == MouseButtons.Left)
            {
                this.edCalcResult.DoDragDrop(this.edCalcResult.Text, DragDropEffects.Move);
            }
        }

        private void edCalcResult_DragOver(object sender, DragEventArgs e)
        {
            e.Effect = DragDropEffects.None;
        }

        private void lbOutput_DoubleClick(object sender, EventArgs e)
        {
            if (this.lbOutput.SelectedIndex < 0 || this.lbOutput.SelectedIndex >= this.lbOutput.Items.Count) return;

            string line = (string)this.lbOutput.Items[this.lbOutput.SelectedIndex];
            if (line.StartsWith("> ")) {
                line = line.Substring(2);
                this.edExpression.Text = line;
            }
        }

        #region ILocalization support

        public void SetLang()
        {
            this.Text = this.fPlugin.LangMan.LS(PLS.LSID_MICalc);
            this.chkPutToClipboard.Text = this.fPlugin.LangMan.LS(PLS.LSID_CopyResultToClipboard);
        }

        #endregion
    }
}
