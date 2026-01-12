/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Windows.Forms;
using GKCore;
using GKCore.Plugins;
using GKUI.Themes;

namespace GKCalendarPlugin
{
    /// <summary>
    /// 
    /// </summary>
    public partial class CalendarWidget : Form, IWidgetForm, IThemedView
    {
        private readonly Plugin fPlugin;

        public CalendarWidget(Plugin plugin)
        {
            InitializeComponent();

            fPlugin = plugin;

            historyDateBox1.DateChanged += hdb_DateChanged;

            historyDateBox2.ReadOnly = true;
            historyDateBox2.CalendarChanged += hdb_DateChanged;

            SetLocale();
        }

        private void CalendarWidget_Load(object sender, EventArgs e)
        {
            fPlugin.Host.WidgetShow(fPlugin);
            if (!DesignMode && AppHost.Instance != null) AppHost.Instance.ApplyTheme(this);
            AppHost.Instance.WidgetLocate(this, WidgetLocation.HRight | WidgetLocation.VTop);
        }

        private void CalendarWidget_Closed(object sender, EventArgs e)
        {
            fPlugin.Host.WidgetClose(fPlugin);
        }

        public void ApplyTheme()
        {
        }

        private void hdb_DateChanged(object sender, EventArgs e)
        {
            double jd = historyDateBox1.Date;
            historyDateBox2.Date = jd;
        }

        #region ILocalizable support

        public void SetLocale()
        {
            var langMan = fPlugin.LangMan;

            Text = langMan.LS(PLS.Calendar);
            grpSourceDate.Text = langMan.LS(PLS.SourceDate);
            grpConvertedDate.Text = langMan.LS(PLS.ConvertedDate);
        }

        #endregion
    }
}
