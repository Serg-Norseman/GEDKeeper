using GedCom551;
using GKCore;
using GKCore.Sys;
using System;
using System.Runtime.CompilerServices;

namespace GKUI.Lists
{
	public class TPersonsFilter : TFilter
	{
		public enum TListFilterMode : byte
		{
			flCommon,
			flSelector
		}

		private string Back_AliveBeforeDate;
		private TFilter.TGroupMode Back_GroupMode;
		private string Back_GroupRef;
		private TGenEngine.TLifeMode Back_LifeMode;
		private string Back_Name;
		private bool Back_PatriarchOnly;
		private string Back_Residence;
		private TGEDCOMSex Back_Sex;
		private TFilter.TGroupMode Back_SourceMode;
		private string Back_SourceRef;
		private string Back_EventVal;

		public string AliveBeforeDate;
		public TFilter.TGroupMode GroupMode;
		public string GroupRef;
		public TGenEngine.TLifeMode LifeMode;
		public string Name;
		public bool PatriarchOnly;
		public string Residence;
		public TGEDCOMSex Sex;
		public TFilter.TGroupMode SourceMode;
		public string SourceRef;
		public string EventVal;
		public TPersonsFilter.TListFilterMode List;
		public bool ChildSelector;
		public int TimeLineYear;

		public TPersonsFilter()
		{
			this.Clear();
		}

		public void Clear()
		{
			this.GroupMode = TFilter.TGroupMode.gmAll;
			this.GroupRef = "";
			if (this.LifeMode != TGenEngine.TLifeMode.lmTimeLine)
			{
				this.LifeMode = TGenEngine.TLifeMode.lmAll;
				this.TimeLineYear = -1;
			}
			this.Name = "*";
			this.AliveBeforeDate = "";
			this.PatriarchOnly = false;
			this.Residence = "*";
			this.Sex = TGEDCOMSex.svNone;
			this.SourceMode = TFilter.TGroupMode.gmAll;
			this.SourceRef = "";
			this.EventVal = "*";
		}

		public void Backup()
		{
			this.Back_AliveBeforeDate = this.AliveBeforeDate;
			this.Back_GroupMode = this.GroupMode;
			this.Back_GroupRef = this.GroupRef;
			this.Back_LifeMode = this.LifeMode;
			this.Back_Name = this.Name;
			this.Back_PatriarchOnly = this.PatriarchOnly;
			this.Back_Residence = this.Residence;
			this.Back_Sex = this.Sex;
			this.Back_SourceMode = this.SourceMode;
			this.Back_SourceRef = this.SourceRef;
			this.Back_EventVal = this.EventVal;
		}

		public void Restore()
		{
			this.AliveBeforeDate = this.Back_AliveBeforeDate;
			this.GroupMode = this.Back_GroupMode;
			this.GroupRef = this.Back_GroupRef;
			this.LifeMode = this.Back_LifeMode;
			this.Name = this.Back_Name;
			this.PatriarchOnly = this.Back_PatriarchOnly;
			this.Residence = this.Back_Residence;
			this.Sex = this.Back_Sex;
			this.SourceMode = this.Back_SourceMode;
			this.SourceRef = this.Back_SourceRef;
			this.EventVal = this.Back_EventVal;
		}

	}
}
