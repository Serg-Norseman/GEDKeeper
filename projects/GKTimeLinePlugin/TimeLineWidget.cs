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

        public TimeLineWidget(Plugin plugin) : base()
        {
            this.InitializeComponent();

            this.Location = new Point(10, Screen.PrimaryScreen.WorkingArea.Height - this.Height - 10);

            this.fPlugin = plugin;

            this.SetLang();
        }

        private void TimeLineWidget_Load(object sender, EventArgs e)
        {
            this.fPlugin.Host.WidgetShow(this.fPlugin);
            this.BaseChanged(this.fPlugin.Host.GetCurrentFile());
        }

        private void TimeLineWidget_Closed(object sender, EventArgs e)
        {
            this.BaseChanged(null);
            this.fPlugin.Host.WidgetClose(this.fPlugin);
        }

        public void BaseChanged(IBaseWindow aBase)
        {
            if (this.fBase != aBase && this.fBase != null)
            {
                IListManager listMan = this.fBase.GetRecordsListManByType(GEDCOMRecordType.rtIndividual);

                listMan.ExternalFilter = null;
                ((IIndividualListFilter)listMan.Filter).FilterLifeMode = FilterLifeMode.lmAll;

                this.fBase.ApplyFilter(GEDCOMRecordType.rtIndividual);
            }

            this.fBase = aBase;

            this.fYearMin = 10000;
            this.fYearMax = 0;
            this.fYearCurrent = -1;

            if (this.fBase != null)
            {
                IListManager listMan = this.fBase.GetRecordsListManByType(GEDCOMRecordType.rtIndividual);

                ((IIndividualListFilter)listMan.Filter).FilterLifeMode = FilterLifeMode.lmTimeLocked;
                listMan.ExternalFilter = this.FilterHandler;

                this.CollectData();

                this.fBase.ApplyFilter(GEDCOMRecordType.rtIndividual);
            }

            this.UpdateControls();
        }

        private void CollectData()
        {
            GEDCOMTree tree = this.fBase.Tree;

            int num = tree.RecordsCount;
            for (int i = 0; i < num; i++)
            {
                GEDCOMRecord rec = tree[i];

                if (rec.RecordType == GEDCOMRecordType.rtIndividual)
                {
                    GEDCOMIndividualRecord iRec = (GEDCOMIndividualRecord)rec;

                    int num2 = iRec.Events.Count;
                    for (int k = 0; k < num2; k++)
                    {
                        GEDCOMCustomEvent ev = iRec.Events[k];

                        if (ev.Name == "BIRT" || ev.Name == "DEAT")
                        {
                            int year = GEDCOMUtils.GetRelativeYear(ev);
                            if (year != 0)
                            {
                                if (this.fYearMin > year) this.fYearMin = year;
                                if (this.fYearMax < year) this.fYearMax = year;
                            }
                        }
                    }
                }
            }
        }

        private void tbTimeLine_ValueChanged(object sender, EventArgs e)
        {
            if (this.fBase != null) {
                this.fYearCurrent = this.tbTimeLine.Value;
                this.fBase.ApplyFilter(GEDCOMRecordType.rtIndividual);
            }
            this.StatusUpdate();
        }

        private void StatusUpdate()
        {
            if (this.fBase != null) {
                this.StatusBarPanel1.Text = string.Format(this.fPlugin.LangMan.LS(PLS.LSID_TimeScale), this.fYearMin.ToString(), this.fYearMax.ToString());
                this.StatusBarPanel2.Text = string.Format(this.fPlugin.LangMan.LS(PLS.LSID_CurrentYear), this.fYearCurrent.ToString());
            } else {
                this.StatusBarPanel1.Text = "";
                this.StatusBarPanel2.Text = "";
            }
        }

        private void UpdateControls()
        {
            int max = this.fYearMax + 1;
            int min = this.fYearMin - 1;
            int cur = this.fYearCurrent;
            if (min > max) {
                int x = min;
                min = max;
                max = x;
            }
            if (cur < min) cur = min;
            if (cur > max) cur = max;

            this.tbTimeLine.ValueChanged -= this.tbTimeLine_ValueChanged;
            this.tbTimeLine.Maximum = max;
            this.tbTimeLine.Minimum = min;
            this.tbTimeLine.Value = cur;
            this.tbTimeLine.ValueChanged += this.tbTimeLine_ValueChanged;

            this.StatusUpdate();
        }

        // FIXME: perhaps it is necessary to define the maximum age by statistics
        private bool FilterHandler(GEDCOMRecord record)
        {
            bool result = true;

            try
            {
                GEDCOMIndividualRecord iRec = (GEDCOMIndividualRecord)record;
                int bdy = GEDCOMUtils.GetRelativeYear(iRec, "BIRT");
                int ddy = GEDCOMUtils.GetRelativeYear(iRec, "DEAT");

                if (bdy != 0 && ddy == 0) {
                    ddy = bdy + GKConsts.PROVED_LIFE_LENGTH;
                }

                if (bdy == 0 && ddy != 0) {
                    bdy = ddy - GKConsts.PROVED_LIFE_LENGTH;
                }

                if (this.fYearCurrent > 0) {
                    result = (this.fYearCurrent >= bdy && this.fYearCurrent <= ddy);
                }
            }
            catch (Exception ex)
            {
                this.fPlugin.Host.LogWrite("TimeLineWidget.FilterHandler(): " + ex.Message);
            }

            return result;
        }

        #region ILocalization support

        public void SetLang()
        {
            this.Text = this.fPlugin.LangMan.LS(PLS.LSID_MITimeLine);

            this.StatusUpdate();
        }

        #endregion
    }
}
