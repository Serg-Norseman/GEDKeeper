/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2022 by Sergey V. Zhdanovskih.
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
using GKCore;
using GKCore.Interfaces;
using GKUI.Components;

namespace GKCalculatorPlugin
{
    /// <summary>
    /// 
    /// </summary>
    public partial class CalcWidget : Form, IWidgetForm
    {
        private readonly Plugin fPlugin;
        private readonly ExpCalculator fCalc;

        public CalcWidget(Plugin plugin)
        {
            InitializeComponent();

            fPlugin = plugin;
            fCalc = new ExpCalculator();
            lbOutput.Items.Clear();

            SetLocale();
        }

        private void CalcWidget_Load(object sender, EventArgs e)
        {
            AppHost.Instance.WidgetLocate(this, WidgetHorizontalLocation.Right, WidgetVerticalLocation.Bottom);

            fPlugin.Host.WidgetShow(fPlugin);
        }

        private void CalcWidget_Closed(object sender, EventArgs e)
        {
            fPlugin.Host.WidgetClose(fPlugin);
        }

        private void edExpression_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.KeyCode == Keys.Return) {
                string res;
                try {
                    res = fCalc.Calc(edExpression.Text).ToString();
                    if (chkPutToClipboard.Checked) {
                        Clipboard.SetDataObject(res);
                    }
                } catch (Exception) {
                    res = "[ ??? ]";
                }
                lbOutput.Items.Add("> " + edExpression.Text);
                lbOutput.Items.Add("= " + res);
                lbOutput.SelectedIndex = lbOutput.Items.Count - 1;
                edCalcResult.Text = res;
            }
        }

        private void edCalcResult_MouseMove(object sender, MouseEventArgs e)
        {
            if (e.Button == MouseButtons.Left) {
                edCalcResult.DoDragDrop(edCalcResult.Text, DragDropEffects.Move);
            }
        }

        private void edCalcResult_DragOver(object sender, DragEventArgs e)
        {
            e.Effect = DragDropEffects.None;
        }

        private void lbOutput_DoubleClick(object sender, EventArgs e)
        {
            if (lbOutput.SelectedIndex < 0 || lbOutput.SelectedIndex >= lbOutput.Items.Count)
                return;

            string line = (string)lbOutput.Items[lbOutput.SelectedIndex];
            if (line.StartsWith("> ", StringComparison.Ordinal)) {
                line = line.Substring(2);
                edExpression.Text = line;
            }
        }

        #region ILocalizable support

        public void SetLocale()
        {
            Text = fPlugin.LangMan.LS(PLS.LSID_MICalc);
            chkPutToClipboard.Text = fPlugin.LangMan.LS(PLS.LSID_CopyResultToClipboard);
        }

        #endregion
    }
}
