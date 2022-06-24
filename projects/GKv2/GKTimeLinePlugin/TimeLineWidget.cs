/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2020 by Sergey V. Zhdanovskih.
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
using GDModel.Providers.GEDCOM;
using GKCore;
using GKCore.Interfaces;
using GKCore.Types;
using GKUI.Components;

namespace GKTimeLinePlugin
{
    /// <summary>
    /// 
    /// </summary>
    public partial class TimeLineWidget : Form, ILocalizable
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

        private void TimeLineWidget_Load(object sender, EventArgs e)
        {
            var loc = AppHost.Instance.WidgetLocate(UIHelper.Rt2Rt(this.Bounds), WidgetHorizontalLocation.Left, WidgetVerticalLocation.Bottom);
            this.Location = new System.Drawing.Point(loc.X, loc.Y);

            fPlugin.Host.WidgetShow(fPlugin);
            BaseChanged(fPlugin.Host.GetCurrentFile());
        }

        private void TimeLineWidget_Closed(object sender, EventArgs e)
        {
            BaseChanged(null);
            fPlugin.Host.WidgetClose(fPlugin);
        }

        public void BaseChanged(IBaseWindow baseWin)
        {
            if (fBase != baseWin)
            {
                // restore filter's default state
                if (fBase != null)
                {
                    IRecordsListModel listMan = fBase.GetRecordsListManByType(GDMRecordType.rtIndividual);
                    if (listMan != null) {
                        listMan.ExternalFilter = null;
                        ((IIndividualListFilter)listMan.Filter).FilterLifeMode = FilterLifeMode.lmAll;
                    }

                    fBase.ApplyFilter(GDMRecordType.rtIndividual);
                }

                fBase = baseWin;
                fYearMin = 10000;
                fYearMax = 0;
                fYearCurrent = -1;

                if (fBase != null)
                {
                    IRecordsListModel listMan = fBase.GetRecordsListManByType(GDMRecordType.rtIndividual);

                    if (listMan != null)
                    {
                        ((IIndividualListFilter)listMan.Filter).FilterLifeMode = FilterLifeMode.lmTimeLocked;
                        listMan.ExternalFilter = FilterHandler;

                        CollectData();

                        fBase.ApplyFilter(GDMRecordType.rtIndividual);
                    }
                }

                UpdateControls();
            }
        }

        private void CollectData()
        {
            int num = fBase.Context.Tree.RecordsCount;
            for (int i = 0; i < num; i++) {
                GDMRecord rec = fBase.Context.Tree[i];
                if (rec.RecordType != GDMRecordType.rtIndividual) continue;

                GDMIndividualRecord iRec = (GDMIndividualRecord)rec;
                if (!iRec.HasEvents) continue;

                for (int k = 0, evNum = iRec.Events.Count; k < evNum; k++) {
                    GDMCustomEvent ev = iRec.Events[k];
                    var evtType = ev.GetTagType();

                    if (evtType == GEDCOMTagType.BIRT || evtType == GEDCOMTagType.DEAT) {
                        int year = ev.GetChronologicalYear();
                        if (year != 0) {
                            if (fYearMin > year) fYearMin = year;
                            if (fYearMax < year) fYearMax = year;
                        }
                    }
                }
            }
        }

        private void tbTimeLine_ValueChanged(object sender, EventArgs e)
        {
            if (fBase != null) {
                fYearCurrent = tbTimeLine.Value;
                fBase.ApplyFilter(GDMRecordType.rtIndividual);
            }
            StatusUpdate();
        }

        private void StatusUpdate()
        {
            if (fBase != null) {
                StatusBarPanel1.Text = string.Format(fPlugin.LangMan.LS(PLS.LSID_TimeScale), fYearMin.ToString(), fYearMax.ToString());
                StatusBarPanel2.Text = string.Format(fPlugin.LangMan.LS(PLS.LSID_CurrentYear), fYearCurrent.ToString());
            } else {
                StatusBarPanel1.Text = "";
                StatusBarPanel2.Text = "";
            }
        }

        private void UpdateControls()
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

            StatusUpdate();
        }

        // FIXME: perhaps it is necessary to define the maximum age by statistics
        private bool FilterHandler(GDMRecord record)
        {
            bool result = true;

            try {
                GDMIndividualRecord iRec = (GDMIndividualRecord)record;
                int bdy = iRec.GetChronologicalYear(GEDCOMTagName.BIRT);
                int ddy = iRec.GetChronologicalYear(GEDCOMTagName.DEAT);

                if (bdy != 0 && ddy == 0) {
                    ddy = bdy + GKData.PROVED_LIFE_LENGTH;
                }

                if (bdy == 0 && ddy != 0) {
                    bdy = ddy - GKData.PROVED_LIFE_LENGTH;
                }

                if (fYearCurrent > 0) {
                    result = (fYearCurrent >= bdy && fYearCurrent <= ddy);
                }
            } catch (Exception ex) {
                Logger.WriteError("TimeLineWidget.FilterHandler()", ex);
            }

            return result;
        }

        #region ILocalizable support

        public void SetLocale()
        {
            Text = fPlugin.LangMan.LS(PLS.LSID_MITimeLine);

            StatusUpdate();
        }

        #endregion
    }
}
