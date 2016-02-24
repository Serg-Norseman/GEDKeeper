namespace GKCore.Stats
{
	/// <summary>
	/// 
	/// </summary>
	public sealed class StatsItem
	{
		public string Caption;
		public int Value;

		public bool IsCombo;
		public int ValF;
		public int ValM;

		public StatsItem(string caption, bool isCombo)
		{
			this.Caption = caption;
			this.Value = 0;
			this.IsCombo = isCombo;
		}

		public StatsItem(string caption, int value)
		{
			this.Caption = caption;
			this.Value = value;
			this.IsCombo = false;
		}

		public override string ToString()
		{
			return this.Caption;
		}
	}
}
