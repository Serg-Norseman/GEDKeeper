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
using System.Windows.Forms;
using GDModel;
using GKCore;
using GKCore.Interfaces;
using GKCore.Types;

namespace GKTimeLinePlugin
{
    public partial class TimeLineWidget : Form, IWidgetForm
    {
        private readonly Plugin fPlugin;
        private IBaseWindow fBase;
        private int fYearMin;
        private int fYearMax;
        private int fYearCurrent;

        public TimeLineWidget(Plugin plugin)
        {
            InitializeComponent();
            fPlugin = plugin;
            SetLocale();
        }

        public void SetLocale()
        {
            Text = fPlugin.LangMan.LS(PLS.TimeLine);
            UpdateStatus();
        }

        private void TimeLineWidget_Load(object sender, EventArgs e)
        {
            AppHost.Instance.WidgetLocate(this, WidgetLocation.HLeft | WidgetLocation.VBottom);
            fPlugin.Host.WidgetShow(fPlugin);
            BaseChanged(fPlugin.Host.GetCurrentFile());
        }

        private void TimeLineWidget_Closed(object sender, EventArgs e)
        {
            BaseChanged(null);
            fPlugin.Host.WidgetClose(fPlugin);
        }

        private void UpdateStatus()
        {
            if (fBase != null) {
                StatusBarPanel1.Text = string.Format(fPlugin.LangMan.LS(PLS.TimeScale), fYearMin, fYearMax);
                StatusBarPanel2.Text = string.Format(fPlugin.LangMan.LS(PLS.CurrentYear), fYearCurrent);
            } else {
                StatusBarPanel1.Text = "";
                StatusBarPanel2.Text = "";
            }
        }

        private void UpdateTrack()
        {
            int max = fYearMax + 1;
            int min = fYearMin - 1;
            int cur = fYearCurrent;
            if (min > max) {
                int x = min;
                min = max;
                max = x;
            }
            if (cur < min) cur = min;
            if (cur > max) cur = max;

            tbTimeLine.ValueChanged -= tbTimeLine_ValueChanged;
            tbTimeLine.Maximum = max;
            tbTimeLine.Minimum = min;
            tbTimeLine.Value = cur;
            tbTimeLine.ValueChanged += tbTimeLine_ValueChanged;
        }

        private void tbTimeLine_ValueChanged(object sender, EventArgs e)
        {
            if (fBase != null) {
                fYearCurrent = tbTimeLine.Value;
                GKUtils.SetTimeLineYear(fBase, fYearCurrent);
                fBase.ApplyFilter(GDMRecordType.rtIndividual);
            }
            UpdateStatus();
        }

        public void BaseChanged(IBaseWindow baseWin)
        {
            if (fBase != baseWin) {
                // restore filter's default state
                fYearCurrent = -1;
                GKUtils.SetTimeLineYear(fBase, fYearCurrent);
                GKUtils.SetBaseExternalFilter(fBase, FilterLifeMode.lmAll, null);

                // initialize data
                fBase = baseWin;
                GKUtils.CollectTimeLineData(fBase, out fYearMin, out fYearMax);

                // set new filter parameters
                GKUtils.SetBaseExternalFilter(fBase, FilterLifeMode.lmTimeLocked, FilterHandler);

                // Update controls
                UpdateTrack();
                UpdateStatus();
            }
        }

        private bool FilterHandler(GDMRecord record)
        {
            return GKUtils.FilterTimeLine((GDMIndividualRecord)record, fYearCurrent);
        }
    }
}
