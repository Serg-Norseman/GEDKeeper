/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Windows.Forms;
using GDModel;
using GKCore;
using GKCore.Design;
using GKCore.Lists;
using GKCore.Plugins;
using GKUI.Themes;

namespace GKTimeLinePlugin
{
    public partial class TimeLineWidget : Form, IWidgetForm, IThemedView
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
            fPlugin.Host.WidgetShow(fPlugin);
            if (!DesignMode && AppHost.Instance != null) AppHost.Instance.ApplyTheme(this);
            AppHost.Instance.WidgetLocate(this, WidgetLocation.HLeft | WidgetLocation.VBottom);

            BaseChanged(fPlugin.Host.GetCurrentFile());
        }

        private void TimeLineWidget_Closed(object sender, EventArgs e)
        {
            BaseChanged(null);
            fPlugin.Host.WidgetClose(fPlugin);
        }

        public void ApplyTheme()
        {
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
