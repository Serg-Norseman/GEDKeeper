using System;

namespace GKCore.Stats
{
	/// <summary>
	/// 
	/// </summary>
	public sealed class StatsItem
	{
		public string Caption;
		public int Value;

		public StatsItem(string caption, int value)
		{
			this.Caption = caption;
			this.Value = value;
		}

		public override string ToString()
		{
			return this.Caption;
		}
	}
}
