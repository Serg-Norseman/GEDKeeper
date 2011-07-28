using GKSys;
using GKUI.Lists;
using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GKUI.Charts
{
	public class TChartFilter : TFilter
	{

		public enum TBranchCut : byte
		{
			bcNone,
			bcYears,
			bcPersons
		}

		internal TFilter.TGroupMode Back_SourceMode;
		internal string Back_SourceRef;
		internal TChartFilter.TBranchCut Back_BranchCut;
		internal int Back_BranchYear;
		internal string Back_BranchPersons;
		public TFilter.TGroupMode SourceMode;
		public string SourceRef;
		public TChartFilter.TBranchCut BranchCut;
		public int BranchYear;
		public string BranchPersons;

		public TChartFilter()
		{
			this.Clear();
		}

		public void Clear()
		{
			this.SourceMode = TFilter.TGroupMode.gmAll;
			this.BranchCut = TChartFilter.TBranchCut.bcNone;
		}

		public void Backup()
		{
			this.Back_SourceMode = this.SourceMode;
			this.Back_SourceRef = this.SourceRef;
			this.Back_BranchCut = this.BranchCut;
			this.Back_BranchYear = this.BranchYear;
			this.Back_BranchPersons = this.BranchPersons;
		}

		public void Restore()
		{
			this.SourceMode = this.Back_SourceMode;
			this.SourceRef = this.Back_SourceRef;
			this.BranchCut = this.Back_BranchCut;
			this.BranchYear = this.Back_BranchYear;
			this.BranchPersons = this.Back_BranchPersons;
		}

	}
}
