/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;
using GKCore.Types;

namespace GKTimeLinePlugin
{
    /// <summary>
    /// 
    /// </summary>
    public partial class TimeLineWidget : Form, ILocalization
    {
        private readonly Plugin fPlugin;

        private IBaseWindow fBase;
        private int fYearMin;
        private int fYearMax;
        private int fYearCurrent;

        public TimeLineWidget(Plugin plugin)
        {
            InitializeComponent();

            Location = new Point(10, Screen.PrimaryScreen.WorkingArea.Height - Height - 10);

            fPlugin = plugin;

            SetLang();
        }

        private void TimeLineWidget_Load(object sender, EventArgs e)
        {
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
            if (fBase != baseWin && fBase != null)
            {
                IListManager listMan = fBase.GetRecordsListManByType(GEDCOMRecordType.rtIndividual);

                listMan.ExternalFilter = null;
                ((IIndividualListFilter)listMan.Filter).FilterLifeMode = FilterLifeMode.lmAll;

                fBase.ApplyFilter(GEDCOMRecordType.rtIndividual);
            }

            fBase = baseWin;

            fYearMin = 10000;
            fYearMax = 0;
            fYearCurrent = -1;

            if (fBase != null)
            {
                IListManager listMan = fBase.GetRecordsListManByType(GEDCOMRecordType.rtIndividual);

                if (listMan != null)
                {
                    ((IIndividualListFilter)listMan.Filter).FilterLifeMode = FilterLifeMode.lmTimeLocked;
                    listMan.ExternalFilter = FilterHandler;

                    CollectData();

                    fBase.ApplyFilter(GEDCOMRecordType.rtIndividual);
                }
            }

            UpdateControls();
        }

        private void CollectData()
        {
            int num = fBase.Context.Tree.RecordsCount;
            for (int i = 0; i < num; i++)
            {
                GEDCOMRecord rec = fBase.Context.Tree[i];
                if (rec.RecordType != GEDCOMRecordType.rtIndividual) continue;

                GEDCOMIndividualRecord iRec = (GEDCOMIndividualRecord)rec;

                int num2 = iRec.Events.Count;
                for (int k = 0; k < num2; k++)
                {
                    GEDCOMCustomEvent ev = iRec.Events[k];

                    if (ev.Name == "BIRT" || ev.Name == "DEAT") {
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
                fBase.ApplyFilter(GEDCOMRecordType.rtIndividual);
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
        private bool FilterHandler(GEDCOMRecord record)
        {
            bool result = true;

            try
            {
                GEDCOMIndividualRecord iRec = (GEDCOMIndividualRecord)record;
                int bdy = iRec.GetChronologicalYear("BIRT");
                int ddy = iRec.GetChronologicalYear("DEAT");

                if (bdy != 0 && ddy == 0) {
                    ddy = bdy + GKData.PROVED_LIFE_LENGTH;
                }

                if (bdy == 0 && ddy != 0) {
                    bdy = ddy - GKData.PROVED_LIFE_LENGTH;
                }

                if (fYearCurrent > 0) {
                    result = (fYearCurrent >= bdy && fYearCurrent <= ddy);
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("TimeLineWidget.FilterHandler(): " + ex.Message);
            }

            return result;
        }

        #region ILocalization support

        public void SetLang()
        {
            Text = fPlugin.LangMan.LS(PLS.LSID_MITimeLine);

            StatusUpdate();
        }

        #endregion
    }
}
