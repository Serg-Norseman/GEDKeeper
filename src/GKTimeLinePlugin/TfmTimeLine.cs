using System;
using System.Drawing;
using System.Windows.Forms;
using GKCommon.GEDCOM;
using GKCommon.GEDCOM.Enums;
using GKCore;
using GKCore.Interfaces;
using GKCore.Types;

/// <summary>
/// 
/// </summary>

namespace GKTimeLinePlugin
{
    public partial class TfmTimeLine : Form
	{
    	private Plugin fPlugin;
		private IBaseWindow fBase;
        private int fYearMin;
        private int fYearMax;
        private int fYearCurrent;

        public TfmTimeLine(Plugin plugin) : base()
		{
			this.InitializeComponent();
			
			this.fPlugin = plugin;
			
			this.Text = this.fPlugin.LangMan.LS(PLS.LSID_MITimeLine);
			
            this.Location = new Point(10, Screen.PrimaryScreen.WorkingArea.Height - this.Height - 10);
		}

        private void TfmTimeLine_Load(object sender, EventArgs e)
        {
        	this.fPlugin.Host.WidgetShow(this.fPlugin);
        	this.BaseChanged(this.fPlugin.Host.GetCurrentFile());
        }

        private void TfmTimeLine_Closed(object sender, EventArgs e)
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
                (listMan.Filter as IIndividualListFilter).FilterLifeMode = FilterLifeMode.lmAll;
                
                this.fBase.ApplyFilter();
            }

            this.fBase = aBase;

            this.fYearMin = 10000;
            this.fYearMax = 0;
            this.fYearCurrent = -1;

            if (this.fBase != null)
            {
            	IListManager listMan = this.fBase.GetRecordsListManByType(GEDCOMRecordType.rtIndividual);

                (listMan.Filter as IIndividualListFilter).FilterLifeMode = FilterLifeMode.lmTimeLocked;
                listMan.ExternalFilter = this.FilterHandler;

                this.CollectData();
                
                this.fBase.ApplyFilter();
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
                    GEDCOMIndividualRecord iRec = rec as GEDCOMIndividualRecord;

                    int num2 = iRec.Events.Count;
                    for (int k = 0; k < num2; k++)
                    {
                        GEDCOMCustomEvent ev = iRec.Events[k];

                        if (ev.Name == "BIRT" || ev.Name == "DEAT")
                        {
                            AbsDate evDate = GEDCOMUtils.GetAbstractDate(ev);
                            if (evDate.IsValid())
                            {
                            	int year = evDate.Year;
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
				this.StatusBarPanel1.Text = this.fPlugin.LangMan.LS(PLS.LSID_TimeScale) + ": " + this.fYearMin.ToString() + " - " + this.fYearMax.ToString();
				this.StatusBarPanel2.Text = this.fPlugin.LangMan.LS(PLS.LSID_CurrentYear) + ": " + this.fYearCurrent.ToString();
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
        private bool FilterHandler(GEDCOMRecord record)
        {
            bool result = true;

            try
            {               
            	GEDCOMIndividualRecord iRec = record as GEDCOMIndividualRecord;
            	GEDCOMCustomEvent buf_bd = iRec.FindEvent("BIRT");
                GEDCOMCustomEvent buf_dd = iRec.FindEvent("DEAT");

                AbsDate birthDate = GEDCOMUtils.GetAbstractDate(buf_bd);
                AbsDate deathDate = GEDCOMUtils.GetAbstractDate(buf_dd);
                
                int bdy = birthDate.Year;
                int ddy = deathDate.Year;

                if (birthDate.IsValid() && !deathDate.IsValid()) {
                    ddy = bdy + GKConsts.ProvedLifeLength;
                }

                if (!birthDate.IsValid() && deathDate.IsValid()) {
                    bdy = ddy - GKConsts.ProvedLifeLength;
                }

                if (this.fYearCurrent > 0) {
                    result = (this.fYearCurrent >= bdy && this.fYearCurrent <= ddy);
                } else {
                    result = true;
                }
            }
            catch (Exception ex)
            {
                this.fPlugin.Host.LogWrite("TfmTimeLine.FilterHandler(): " + ex.Message);
            }

            return result;
        }
	}
}
