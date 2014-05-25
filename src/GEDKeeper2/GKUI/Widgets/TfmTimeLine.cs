using System;
using System.Drawing;
using System.Windows.Forms;

using ExtUtils;
using GedCom551;
using GKCore;
using GKCore.Interfaces;
using GKUI.Lists;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKUI.Widgets
{
    public partial class TfmTimeLine : Form, IWidget
	{
    	#region IWidget common

    	private IHost fHost;
    	private MenuItem fMenuItem;

    	IHost IWidget.Host
    	{
    		get { return this.fHost; }
    	}

    	MenuItem IWidget.MenuItem
    	{
    		get { return this.fMenuItem; }
    	}

    	void IWidget.WidgetInit(IHost host, MenuItem menuItem)
    	{
    		this.fHost = host;
    		this.fMenuItem = menuItem;
    	}

    	#endregion

		private IBase fBase;
        private int fYearMin;
        private int fYearMax;
        private int fYearCurrent;

        public TfmTimeLine() : base()
		{
			this.InitializeComponent();
			this.Text = LangMan.LS(LSID.LSID_MITimeLine);
			
            this.Location = new Point(10, Screen.PrimaryScreen.WorkingArea.Height - this.Height - 10);
		}

        private void TfmTimeLine_Load(object sender, EventArgs e)
        {
        	this.fHost.WidgetShow(this);
        	((IWidget)this).BaseChanged(fHost.GetCurrentFile());
        }

        private void TfmTimeLine_Closed(object sender, EventArgs e)
        {
            ((IWidget)this).BaseChanged(null);
            this.fHost.WidgetClose(this);
        }

        void IWidget.BaseChanged(IBase aBase)
        {
            if (this.fBase != aBase && this.fBase != null)
            {
            	IListManager listMan = this.fBase.GetRecordsListManByType(TGEDCOMRecordType.rtIndividual);
            	
                listMan.ExternalFilter = null;
                (listMan.Filter as TIndividualListFilter).LifeMode = TLifeMode.lmAll;
                
                this.fBase.ApplyFilter();
            }

            this.fBase = aBase;

            this.fYearMin = 10000;
            this.fYearMax = 0;
            this.fYearCurrent = -1;

            if (this.fBase != null)
            {
            	IListManager listMan = this.fBase.GetRecordsListManByType(TGEDCOMRecordType.rtIndividual);

                (listMan.Filter as TIndividualListFilter).LifeMode = TLifeMode.lmTimeLocked;
                listMan.ExternalFilter = this.FilterHandler;

                this.CollectData();
                
                this.fBase.ApplyFilter();
            }

            this.UpdateControls();
        }

        private void CollectData()
        {
            TGEDCOMTree tree = this.fBase.Tree;

            int num = tree.RecordsCount - 1;
            for (int i = 0; i <= num; i++)
            {
                TGEDCOMRecord rec = tree[i];

                if (rec.RecordType == TGEDCOMRecordType.rtIndividual)
                {
                    TGEDCOMIndividualRecord iRec = rec as TGEDCOMIndividualRecord;

                    int num2 = iRec.IndividualEvents.Count - 1;
                    for (int k = 0; k <= num2; k++)
                    {
                        TGEDCOMCustomEvent ev = iRec.IndividualEvents[k];

                        if (ev.Name == "BIRT" || ev.Name == "DEAT")
                        {
                            ushort j, d;
                            int year;
                            ev.Detail.Date.aux_GetIndependentDate(out year, out j, out d);
                            if (year > 0)
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
                this.fBase.ApplyFilter();
			}
			this.StatusUpdate();
		}

		private void StatusUpdate()
		{
			if (this.fBase != null) {
				this.StatusBarPanel1.Text = LangMan.LS(LSID.LSID_TimeScale) + ": " + this.fYearMin.ToString() + " - " + this.fYearMax.ToString();
				this.StatusBarPanel2.Text = LangMan.LS(LSID.LSID_CurrentYear) + ": " + this.fYearCurrent.ToString();
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

        // FIXME: возможно необходимо определение максимального возраста по статистике
        private bool FilterHandler(TGEDCOMRecord record)
        {
            bool result = true;

            try
            {
                TIndividualListMan iListMan = this.fBase.GetRecordsListManByType(TGEDCOMRecordType.rtIndividual) as TIndividualListMan;
                //TGEDCOMIndividualRecord iRec = record as TGEDCOMIndividualRecord;

                TGEDCOMCustomEvent buf_bd = iListMan.buf_bd;
                TGEDCOMCustomEvent buf_dd = iListMan.buf_dd;

                ushort j, d;

                int bdy = -1;
                if (buf_bd != null) buf_bd.Detail.Date.aux_GetIndependentDate(out bdy, out j, out d);

                int ddy = -1;
                if (buf_dd != null) buf_dd.Detail.Date.aux_GetIndependentDate(out ddy, out j, out d);

                if (bdy > 0 && ddy <= 0) {
                    ddy = bdy + GKUtils.ProvedLifeLength;
                }

                if (bdy <= 0 && ddy > 0) {
                    bdy = ddy - GKUtils.ProvedLifeLength;
                }

                if (this.fYearCurrent > 0) {
                    result = (this.fYearCurrent >= bdy && this.fYearCurrent <= ddy);
                } else {
                    result = true;
                }
            }
            catch (Exception ex)
            {
                SysUtils.LogWrite("TfmTimeLine.FilterHandler(): " + ex.Message);
            }

            return result;
        }
	}
}
