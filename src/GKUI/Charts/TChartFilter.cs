using System;

using GKUI.Lists;

/// <summary>
/// Localization: clean
/// </summary>

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

		private TFilter.TGroupMode Back_SourceMode;
		private string Back_SourceRef;
		private TChartFilter.TBranchCut Back_BranchCut;
		private int Back_BranchYear;
		private string Back_BranchPersons;

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
