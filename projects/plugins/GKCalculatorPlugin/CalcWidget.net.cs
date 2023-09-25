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
using Eto.Forms;
using Eto.Serialization.Xaml;
using GKCore;
using GKCore.Interfaces;
using GKUI.Components;

namespace GKCalculatorPlugin
{
    /// <summary>
    /// 
    /// </summary>
    public partial class CalcWidget : FloatingForm, IWidgetForm
    {
        #region Design components
#pragma warning disable CS0169, CS0649, IDE0044, IDE0051

        private ListBox lbOutput;
        private TextBox edExpression;
        private CheckBox chkPutToClipboard;
        private TextBox edCalcResult;
        private CheckBox chkEventsYearCalculation;

#pragma warning restore CS0169, CS0649, IDE0044, IDE0051
        #endregion

        private readonly Plugin fPlugin;
        private readonly ExpCalculator fCalc;

        public CalcWidget(Plugin plugin)
        {
            XamlReader.Load(this);

            fPlugin = plugin;
            fCalc = new ExpCalculator();
            lbOutput.Items.Clear();

            SetLocale();
        }

        private void CalcWidget_Shown(object sender, EventArgs e)
        {
            AppHost.Instance.WidgetLocate(this, WidgetLocation.HRight | WidgetLocation.VBottom);
            fPlugin.Host.WidgetShow(fPlugin);
        }

        private void CalcWidget_Closed(object sender, EventArgs e)
        {
            fPlugin.Host.WidgetClose(fPlugin);
        }

        private void edExpression_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.Key == Keys.Enter) {
                string res;
                try {
                    res = fCalc.Calc(edExpression.Text).ToString();

                    if (chkPutToClipboard.Checked.Value) {
                        AppHost.Instance.SetClipboardText(res);
                    }

                    if (chkEventsYearCalculation.Checked.Value) {
                        fPlugin.Host.Activate();
                        var dataReceiver = fPlugin.Host.GetActiveForm() as IDataReceiver;
                        if (dataReceiver != null) {
                            dataReceiver.SendData("event_year", res);
                        }
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
            if (e.Buttons == MouseButtons.Primary) {
                var dataObj = new DataObject();
                dataObj.SetString(edCalcResult.Text, "");
                edCalcResult.DoDragDrop(dataObj, DragEffects.Move);
            }
        }

        private void edCalcResult_DragOver(object sender, DragEventArgs e)
        {
            e.Effects = DragEffects.None;
        }

        private void lbOutput_DoubleClick(object sender, EventArgs e)
        {
            if (lbOutput.SelectedIndex < 0 || lbOutput.SelectedIndex >= lbOutput.Items.Count)
                return;

            string line = (string)lbOutput.SelectedValue;
            if (line.StartsWith("> ", StringComparison.Ordinal)) {
                line = line.Substring(2);
                edExpression.Text = line;
            }
        }

        #region ILocalizable support

        public void SetLocale()
        {
            Title = fPlugin.LangMan.LS(PLS.Title);
            chkPutToClipboard.Text = fPlugin.LangMan.LS(PLS.CopyResultToClipboard);
            chkEventsYearCalculation.Text = fPlugin.LangMan.LS(PLS.EventsYearCalculation);
        }

        #endregion
    }
}
